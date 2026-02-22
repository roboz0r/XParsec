namespace XParsec.FSharp.Parser

open XParsec
open XParsec.OperatorParsing
open XParsec.Parsers
open XParsec.FSharp.Lexer

/// Reader state for #if expression parsers.
/// Carries the absolute token index of the slice start so that
/// SyntaxToken.Index can be set to the correct absolute position
/// for later text extraction via ParseState.tokenString.
[<Struct>]
type IfExprState = { AbsoluteStart: int }

[<RequireQualifiedAccess>]
module IfExpr =
    open SyntaxToken
    // Operator precedence parser for #if expressions

    let isTriviaToken (token: PositionedToken) =
        if token.InComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Whitespace
            | Token.Tab -> true
            | _ -> false

    /// Ignores trivia tokens and returns the next non-trivia token, or fails if the end of input is reached.
    /// Only for use on #if directive lines.
    let rec nextNonTriviaIfToken (reader: Reader<PositionedToken, IfExprState, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when isTriviaToken token ->
            reader.Skip()
            nextNonTriviaIfToken reader
        | ValueSome token ->
            // AbsoluteStart + slice-relative index = absolute token index in the full Lexed.Tokens array.
            let absoluteIndex = reader.State.AbsoluteStart + reader.Index
            let t = syntaxToken token absoluteIndex
            reader.Skip()
            preturn t reader

    [<RequireQualifiedAccess>]
    type IfExprAux = unit

    // The main expression type for #if expressions, with auxiliary data for operators.
    // We use a generic type here to allow the parser to be generic over the input type (e.g. ReadableImmutableArray vs ReadableImmutableArraySlice).
    type IfExprParser<'Input, 'InputSlice
        when 'Input :> IReadable<PositionedToken, 'InputSlice>
        and 'InputSlice :> IReadable<PositionedToken, 'InputSlice>>() =
        static let orPrecedence = BindingPower.fromLevel 1 // ||
        static let andPrecedence = BindingPower.fromLevel 2 // &&
        static let notPrecedence = BindingPower.fromLevel 3 // !
        static let parenPrecedence = BindingPower.fromLevel 4 // ( )
        // --- Completion Functions ---

        static let completeInfix (l: IfExpr<SyntaxToken>) (op: SyntaxToken) (r: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpBarBar -> IfExpr.Or(l, op, r)
            | Token.OpAmpAmp -> IfExpr.And(l, op, r)
            | _ -> failwithf "Unexpected infix if operator: %A" op

        static let completePrefix (op: SyntaxToken) (e: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpDereference -> IfExpr.Not(op, e)
            | _ -> failwithf "Unexpected prefix if operator: %A" op

        static let completeParen (l: SyntaxToken) (m: IfExpr<SyntaxToken>) (r: SyntaxToken) = IfExpr.Paren(l, m, r)

        // --- Operator Parsers ---

        static let lhsParser: Parser<_, _, _, 'Input, 'InputSlice> =
            nextNonTriviaIfToken
            >>= fun token ->
                match token.Token with
                | Token.OpDereference ->
                    // Parenthesized measure
                    let p = preturn token
                    let op = Prefix(token, p, notPrecedence, completePrefix)
                    preturn op
                | Token.KWLParen ->
                    // Parenthesized expression
                    let p = preturn token

                    let rParen =
                        virtualToken (PositionedToken.Create(Token.KWRParen, token.StartIndex + 1))

                    let op = Enclosed(token, p, parenPrecedence, rParen, p, completeParen)
                    preturn op
                | _ -> fail (Message "Not a valid LHS #if operator")

        static let rhsParser: Parser<_, _, _, 'Input, 'InputSlice> =
            nextNonTriviaIfToken
            >>= fun token ->
                match token.Token with
                | Token.OpBarBar ->
                    let p = preturn token
                    let op = InfixLeft(token, p, orPrecedence, completeInfix)
                    preturn op
                | Token.OpAmpAmp ->
                    let p = preturn token
                    let op = InfixLeft(token, p, andPrecedence, completeInfix)
                    preturn op
                | _ -> fail (Message "Not a valid RHS #if operator")

        static let atomParser: Parser<_, _, _, 'Input, 'InputSlice> =
            nextNonTriviaIfToken
            >>= fun token ->
                match token.Token with
                | Token.Identifier -> preturn (IfExpr.Term token)
                | t when t.IsKeyword -> preturn (IfExpr.Term token)
                | _ -> fail (Message "Not a valid #if term")

        static let ifDirectiveParser: Parser<_, _, _, 'Input, 'InputSlice> =
            satisfyL (fun token -> token.Token = Token.IfDirective) "Not a #if directive"

        interface Operators<
            SyntaxToken,
            IfExprAux,
            IfExpr<SyntaxToken>,
            PositionedToken,
            IfExprState,
            'Input,
            'InputSlice
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

        static member AtomParser = atomParser
        static member IfDirectiveParser = ifDirectiveParser

    let parse: Parser<_, _, _, _, _> =
        IfExprParser<ReadableImmutableArray<PositionedToken>, _>.IfDirectiveParser
        >>. Operator.parser
                IfExprParser<ReadableImmutableArray<PositionedToken>, _>.AtomParser
                (IfExprParser<ReadableImmutableArray<PositionedToken>, _>())

    let parseSlice: Parser<_, _, _, _, _> =
        IfExprParser<ReadableImmutableArraySlice<PositionedToken>, _>.IfDirectiveParser
        >>. Operator.parser
                IfExprParser<ReadableImmutableArraySlice<PositionedToken>, _>.AtomParser
                (IfExprParser<ReadableImmutableArraySlice<PositionedToken>, _>())

    let evaluate (expr: IfExpr<SyntaxToken>) (isDefined: SyntaxToken -> bool) : bool =
        let rec eval e =
            match e with
            | IfExpr.Term t -> isDefined t
            | IfExpr.And(l, _, r) -> eval l && eval r
            | IfExpr.Or(l, _, r) -> eval l || eval r
            | IfExpr.Not(_, r) -> not (eval r)
            | IfExpr.Paren(_, m, _) -> eval m

        eval expr

    let evaluateStateful (expr: IfExpr<SyntaxToken>) (state: ParseState) : bool =
        let isDefined token = ParseState.isDefined state token
        evaluate expr isDefined
