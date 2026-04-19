namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

module Measure =
    [<RequireQualifiedAccess>]
    type MeasureAux = | PowerOperand of neg: SyntaxToken voption * intTok: SyntaxToken

    type MeasureOperatorParser() =
        static let productPrecedence = BindingPower.fromLevel 1 // * and /
        static let juxtapositionPrecedence = BindingPower.fromLevel 2 // Implicit (whitespace)
        static let powerPrecedence = BindingPower.fromLevel 3 // ^
        static let reciprocalPrecedence = BindingPower.fromLevel 4 // / (Reciprocal)
        static let parenPrecedence = BindingPower.fromLevel 10 // ( ... )

        // --- Completion Functions ---

        static let completeInfix (l: Measure<SyntaxToken>) (op: SyntaxToken) (r: Measure<SyntaxToken>) =
            match op.Token with
            | Token.OpMultiply -> Measure.Product(l, op, r)
            | Token.OpDivision -> Measure.Quotient(l, op, r)
            | _ -> failwithf "Unexpected infix measure operator: %A" op

        static let completePrefix (op: SyntaxToken) (e: Measure<SyntaxToken>) =
            match op.Token with
            | Token.OpDivision -> Measure.Reciprocal(op, e)
            | _ -> failwithf "Unexpected prefix measure operator: %A" op

        static let completePower (l: Measure<SyntaxToken>) (op: SyntaxToken) (aux: MeasureAux) =
            match aux with
            | MeasureAux.PowerOperand(neg, exponentToken) -> Measure.Power(l, op, neg, exponentToken)

        static let completeJuxtaposition (elements: ResizeArray<Measure<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
            Measure.Juxtaposition(ImmutableArray.CreateRange(elements), ImmutableArray.CreateRange(ops))

        static let completeParen (l: SyntaxToken) (m: Measure<SyntaxToken>) (r: SyntaxToken) = Measure.Paren(l, m, r)

        // --- Aux Parsers ---

        static let pPowerRhs =
            parser {
                // First token may be `-` (from a split `^-N` operator) or the numeric exponent.
                let! firstTok = nextNonTriviaToken

                if firstTok.Token = Token.OpSubtraction then
                    let! intToken =
                        nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsNumeric) "Expected integer exponent after '-'"

                    return MeasureAux.PowerOperand(ValueSome firstTok, intToken)
                elif firstTok.Token.IsNumeric then
                    return MeasureAux.PowerOperand(ValueNone, firstTok)
                else
                    return! fail (Message "Expected integer exponent")
            }

        static let pJuxtapositionOp =
            parser {
                let! pos = getPosition
                let! token = satisfyL (fun (t: PositionedToken) -> t.Token = Token.Whitespace) "Whitespace"

                // Lookahead to confirm we are adjacent to a measure atom
                let! _ =
                    lookAhead (
                        nextNonTriviaTokenSatisfiesL
                            (fun t ->
                                t.Token.IsIdentifier
                                || t.Token = Token.OpLessThan
                                || t.Token = Token.KWSingleQuote
                                || t.Token = Token.Wildcard
                                || t.Token = Token.KWLParen
                                || t.Token.IsNumeric
                            )
                            "Measure Atom or Literal"
                    )

                return syntaxToken token pos.Index
            }

        // --- Parsers ---

        static let lhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                | Token.OpDivision ->
                    let p = preturn token
                    // Reciprocal: / s
                    let op = Prefix(token, p, reciprocalPrecedence, completePrefix)
                    preturn op
                | Token.KWLParen ->
                    // Parenthesized measure
                    let p = preturn token
                    let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, 0))
                    let op = Enclosed(token, p, parenPrecedence, rParen, pRParen, completeParen)

                    preturn op
                | _ -> fail (Message "Not a prefix measure operator")

        /// Detects a fused `^-` operator token (the lexer merges `^-` into a single custom
        /// operator at Append precedence when there is no whitespace between them). Sets
        /// SplitPowerMinus so the `-` half is presented as OpSubtraction on the next read,
        /// and returns a virtual `^` token so the enclosing rhsParser dispatches normally
        /// to the power operator path. The underlying `^-` token is not consumed here; it
        /// is rewritten and advanced past by the subsequent `pPowerRhs` read.
        static let pSplitPowerOp =
            parser {
                let! peeked = peekNextNonTriviaToken
                let! state = getUserState

                if ParseState.tokenStringIs "^-" peeked state then
                    do!
                        updateUserState (fun s ->
                            s.Trace.SplitPowerMinusSet peeked.StartIndex
                            { s with SplitPowerMinus = true }
                        )

                    return virtualToken (PositionedToken.Create(Token.OpConcatenate, peeked.StartIndex))
                else
                    return! fail (Message "Not a split ^- power operator")
            }

        static let rhsParser =
            // Try the fused `^-` operator split first, then whitespace (juxtaposition),
            // then standard operator tokens.
            (pSplitPowerOp <|> pJuxtapositionOp <|> nextNonTriviaToken)
            >>= fun token ->
                match token.Token with
                | Token.OpMultiply
                | Token.OpDivision ->
                    // Product: * or /
                    let op = InfixLeft(token, preturn token, productPrecedence, completeInfix)
                    preturn op

                | Token.Whitespace ->
                    // Juxtaposition: implicit multiplication
                    let op =
                        InfixNary(token, pJuxtapositionOp, juxtapositionPrecedence, false, completeJuxtaposition)

                    preturn op

                | Token.OpConcatenate ->
                    // Power: ^
                    // InfixMapped handles the parsing of the integer operand (Aux)
                    let op =
                        InfixMapped(token, preturn token, powerPrecedence, pPowerRhs, completePower)

                    preturn op

                | _ -> fail (Message "Not a valid RHS measure operator")

        interface Operators<
            SyntaxToken,
            MeasureAux,
            Measure<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser

    let pOneLiteral =
        parser {
            let! state = getUserState

            let! t =
                nextNonTriviaTokenSatisfiesL
                    (fun t -> t.Token = Token.NumInt32 && tokenStringIs "1" t state)
                    "Expected '1'"

            return Measure.One t
        }

    let pAnonymous = pWildcard |>> Measure.Anonymous
    let pTypar = Typar.parse |>> Measure.Typar
    let pNamed = LongIdent.parse |>> Measure.Named

    let atomMeasureParser: Parser<Measure<SyntaxToken>, _, _, _, _> =
        choiceL [ pOneLiteral; pAnonymous; pTypar; pNamed ] "Measure Atom"

    let measureOperatorParser = MeasureOperatorParser()

    let parse: Parser<Measure<SyntaxToken>, _, _, _, _> =
        Operator.parser atomMeasureParser measureOperatorParser

    do refMeasure.Set parse
