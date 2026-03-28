namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

module Pat =

    [<RequireQualifiedAccess>]
    type PatAux =
        | Type of Type<SyntaxToken>
        | AsIdent of SyntaxToken

    type PatOperatorParser() =
        // --- Precedence Definitions ---
        static let tuplePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Comma)
        static let asPrecedence = BindingPower.fromLevel (int PrecedenceLevel.As)
        static let semiPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Semicolon)
        static let pipePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Pipe)
        static let andPrecedence = BindingPower.fromLevel (int PrecedenceLevel.LogicalAnd)
        static let colonPrecedence = BindingPower.fromLevel (int PrecedenceLevel.TypeTest)
        static let consPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Cons)
        static let parenPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Parens)
        static let structPrecedence = BindingPower.fromLevel (int PrecedenceLevel.HighApplication)

        // --- Completion Functions ---

        static let completeInfix (l: Pat<SyntaxToken>) (op: SyntaxToken) (r: Pat<SyntaxToken>) =
            match op.Token with
            | Token.OpBar -> Pat.Or(l, op, r)
            | Token.OpAmp -> Pat.And(l, op, r)
            | Token.KWColonColon -> Pat.Cons(l, op, r)
            | _ -> failwithf "Unexpected infix pattern operator: %A" op

        static let completeTuple (elements: ResizeArray<Pat<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
            Pat.Tuple(List.ofSeq elements, List.ofSeq ops)

        static let completeTyped (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
            match aux with
            | PatAux.Type t -> Pat.Typed(l, op, t)
            | _ -> failwith "Expected Type aux for Typed pattern"

        static let completeAs (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
            match aux with
            | PatAux.AsIdent ident -> Pat.As(l, op, ident)
            | _ -> failwith "Expected Ident aux for As pattern"

        static let completeParen (l: SyntaxToken) (p: Pat<SyntaxToken>) (r: SyntaxToken) =
            Pat.EnclosedBlock(ParenKind.Paren l, p, r)

        static let completeStruct (op: SyntaxToken) (r: Pat<SyntaxToken>) =
            match r with
            | Pat.EnclosedBlock(ParenKind.Paren l, Pat.Tuple(elements, ops), r) ->
                Pat.StructTuple(op, l, elements, ops, r)
            | _ ->
                // TODO: Error - struct must be followed by tuple in parens
                Pat.Struct(op, r)


        static let completeElems (exprs: ResizeArray<Pat<_>>) ops =
            Pat.Elems(List.ofSeq exprs, List.ofSeq ops)

        // --- Aux Parsers ---

        static let pTypeRhs = Type.parse |>> PatAux.Type

        static let pAsRhs =
            parser {
                // NOTE: the 'as' token was already consumed by rhsParser's nextNonTriviaToken.
                // We only need to consume the identifier that follows.
                let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "identifier after 'as'"
                return PatAux.AsIdent ident
            }

        // --- Main Parsers ---
        static let lhsParser =
            parser {
                let! token = nextNonTriviaToken

                match token.Token with
                | Token.KWLParen ->
                    // Start of tuple pattern ( ... )
                    // This is a Prefix Operator on a pattern
                    let p = preturn token
                    let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, 0))
                    let op = Enclosed(token, p, parenPrecedence, rParen, pRParen, completeParen)
                    return op
                | Token.KWStruct ->
                    let p = preturn token
                    // Create Prefix operator
                    // This will parse the immediate next pattern (e.g. Paren, or erroneously Literal/List)
                    let op = Prefix(token, p, structPrecedence, completeStruct)
                    return op
                | _ -> return! fail (Message "Not a prefix pattern operator")
            }

        static let rhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                // Infix Left: | (Or), & (And)
                | Token.OpBar ->
                    let op = InfixLeft(token, preturn token, pipePrecedence, completeInfix)
                    preturn op

                | Token.OpAmp ->
                    let op = InfixLeft(token, preturn token, andPrecedence, completeInfix)
                    preturn op

                // Infix Right: :: (Cons)
                | Token.KWColonColon ->
                    let op = InfixRight(token, preturn token, consPrecedence, completeInfix)
                    preturn op

                // Infix Mapped: : (Typed)
                | Token.OpColon ->
                    let op = InfixMapped(token, preturn token, colonPrecedence, pTypeRhs, completeTyped)
                    preturn op

                // Infix Mapped: as (As)
                | Token.KWAs ->
                    let op = InfixMapped(token, preturn token, asPrecedence, pAsRhs, completeAs)
                    preturn op

                // Infix N-ary: , (Tuple)
                | Token.OpComma ->
                    let op = InfixNary(token, preturn token, tuplePrecedence, false, completeTuple)
                    preturn op

                | Token.OpSemicolon ->
                    let op = InfixNary(token, preturn token, semiPrecedence, false, completeElems)
                    preturn op

                | _ -> fail (Message "Not a valid RHS pattern operator")

        interface Operators<
            SyntaxToken,
            PatAux,
            Pat<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let private refPat = FSRefParser<Pat<SyntaxToken>>()
    let private refFieldPat = FSRefParser<Pat<SyntaxToken>>()
    let private refUnionFieldPat = FSRefParser<Pat<SyntaxToken>>()
    let private refPatAtomic = FSRefParser<Pat<SyntaxToken>>()

    let private pEnclosed =
        let completeEmpty l r = Pat.EmptyBlock(l, r)
        let completeEnclosed l e r = Pat.EnclosedBlock(l, e, r)
        let skipsTokens toks = Pat.SkipsTokens(toks)
        pEnclosed completeEmpty completeEnclosed Pat.Missing skipsTokens

    let pParenPat =
        pEnclosed
            pLParen
            Token.KWRParen
            ParenKind.Paren
            OffsideContext.Paren
            DiagnosticCode.ExpectedRParen
            refPat.Parser

    let pListPat =
        pEnclosed
            pLBracket
            Token.KWRBracket
            ParenKind.List
            OffsideContext.Bracket
            DiagnosticCode.ExpectedRBracket
            refPat.Parser

    let pArrayPat =
        pEnclosed
            pLArrayBracket
            Token.KWRArrayBracket
            ParenKind.Array
            OffsideContext.BracketBar
            DiagnosticCode.ExpectedRArrayBracket
            refPat.Parser

    let pFieldPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! p = withContext OffsideContext.SeqBlock refFieldPat.Parser
            return FieldPat(lid, eq, p)
        }

    let pRecordPat: Parser<Pat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        fun reader ->
            match pLBrace reader with
            | Error e -> Error e
            | Ok l ->
                let savedState = reader.State

                // Push Brace context (indent 0) so closing } is not blocked by offside check
                let braceEntry: Offside =
                    {
                        Context = OffsideContext.Brace
                        Indent = 0
                        Token = l.PositionedToken
                    }

                reader.State <- ParseState.pushOffside braceEntry reader.State

                let innerParser =
                    parser {
                        let! fields, _ = withContext OffsideContext.SeqBlock (sepEndBy1 pFieldPat pRecordFieldSep)
                        let! r = pRBrace
                        return Pat.Record(l, List.ofSeq fields, r)
                    }

                match innerParser reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside braceEntry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message "Record pattern") reader

    /// Parse a single field in a named union case pattern: fieldName = pat (excludes comma)
    let private pUnionFieldPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! p = withContext OffsideContext.SeqBlock refUnionFieldPat.Parser
            return FieldPat(lid, eq, p)
        }

    /// Parse named field patterns: UnionCase(field1 = pat1, field2 = pat2)
    /// Accepts both commas and semicolons as separators (F# allows both).
    let private pNamedFieldPats =
        parser {
            let! lid = LongIdent.parse
            let! lParen = pLParen
            let! fields, commas = sepBy1 pUnionFieldPat (pComma <|> pSemi)
            let! rParen = pRParen
            return Pat.NamedFieldPats(lid, lParen, List.ofSeq fields, List.ofSeq commas, rParen)
        }

    let pNamed =
        // Try named field patterns first (backtracking on failure),
        // then fall back to standard named pattern parsing.
        choiceL
            [
                pNamedFieldPats
                parser {
                    let! lid = LongIdent.parse
                    let! param = opt refPatAtomic.Parser
                    let! arg = opt refPat.Parser

                    match lid, param, arg with
                    | [ name ], ValueNone, ValueNone ->
                        // Simple named pattern (variable)
                        return Pat.NamedSimple(name)
                    | _ ->
                        // Full named pattern
                        return Pat.Named(lid, param, arg)
                }
            ]
            "Named pattern"

    let pTypeTestPat =
        parser {
            let! op = pTypeTest
            let! t = Type.parseAtomic
            // Check optional 'as ident'
            let! asClause =
                opt (
                    parser {
                        let! asTok = pAs
                        let! id = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "identifier"
                        return struct (asTok, id)
                    }
                )

            match asClause with
            | ValueSome(asTok, id) -> return Pat.TypeTestAs(op, t, asTok, id)
            | ValueNone -> return Pat.TypeTest(op, t)
        }

    let pAttributesPat =
        parser {
            let! attrs = Attributes.parse
            let! pat = refPat.Parser
            return Pat.Attributed(attrs, pat)
        }

    let pConstPat =
        parser {
            let! c = Constant.parse
            return Pat.Const c
        }

    let pWildcardPat = pWildcard |>> Pat.Wildcard
    let pNullPat = pNull |>> Pat.Null

    let private pStructPat =
        parser {
            let! structTok = pStruct
            let! pat = pParenPat

            match pat with
            | Pat.EnclosedBlock(ParenKind.Paren l, Pat.Tuple(elements, ops), r) ->
                return Pat.StructTuple(structTok, l, elements, ops, r)
            | _ -> return Pat.Struct(structTok, pat)
        }

    let private pOptionalPat =
        parser {
            let! qmark = pQuestionMark
            let! pat = pNamed
            return Pat.Optional(qmark, pat)
        }

    let parseAtomic =
        dispatchNextNonTriviaTokenFallback
            [
                Token.Wildcard, pWildcardPat
                Token.KWNull, pNullPat
                Token.OpTypeTest, pTypeTestPat
                Token.OpDynamic, pOptionalPat
                Token.Identifier, pNamed
                Token.BacktickedIdentifier, pNamed
                Token.UnterminatedBacktickedIdentifier, pNamed
                Token.KWLParen, pParenPat
                Token.KWLBracket, pListPat
                Token.KWLArrayBracket, pArrayPat
                Token.KWLBrace, pRecordPat
                Token.KWLAttrBracket, pAttributesPat
                Token.KWStruct, pStructPat
            ]
            pConstPat

    do refPatAtomic.Set parseAtomic

    let parse = Operator.parser parseAtomic (PatOperatorParser())
    // For record field patterns, we want to allow the same operators as the top-level, but not semicolon since that separates fields.
    let private parseFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Semicolon + 1)) parseAtomic (PatOperatorParser())

    // For named union case field patterns (comma-separated), exclude both comma and semicolon.
    let private parseUnionFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Comma + 1)) parseAtomic (PatOperatorParser())

    let parseMany1 = many1 parse
    let parseAtomicMany1 = many1 parseAtomic

    let parseAtomicOrTuple =
        parser {
            let! firstPat = parseAtomic

            // Support unparenthesized tuple patterns: let a, b = expr
            let! extraPats =
                many (
                    parser {
                        let! comma = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) "Expected ','"

                        let! pat = parseAtomic
                        return (comma, pat)
                    }
                )

            let pat =
                if extraPats.IsEmpty then
                    firstPat
                else
                    Pat.Tuple(
                        firstPat :: (extraPats |> Seq.map snd |> List.ofSeq),
                        extraPats |> Seq.map fst |> List.ofSeq
                    )

            return pat
        }

    do refPat.Set parse
    do refFieldPat.Set parseFieldPat
    do refUnionFieldPat.Set parseUnionFieldPat


[<RequireQualifiedAccess>]
module PatternGuard =
    let parse: Parser<PatternGuard<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! w = pWhen
            // Use refExprGuard (bounded at Arrow precedence) so '->' is not consumed
            // as part of the guard expression and remains for Rule.parse's pArrowRight.
            let! e =
                refExprGuard.Parser
                |> recoverWith
                    StoppingTokens.afterPattern
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingExpression
                    (fun toks ->
                        if toks.IsEmpty then
                            Expr.Missing
                        else
                            Expr.SkipsTokens(toks)
                    )

            return PatternGuard(w, e)
        }

[<RequireQualifiedAccess>]
module Rule =
    let parse: Parser<Rule<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! pat =
                Pat.parse
                |> recoverWith
                    StoppingTokens.afterPattern
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingPattern
                    (fun toks -> if toks.IsEmpty then Pat.Missing else Pat.SkipsTokens(toks))

            let! guard = opt PatternGuard.parse
            let! arrow = pArrowRight

            let! expr =
                refExprSeqBlock.Parser
                |> recoverWith
                    StoppingTokens.afterRule
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingExpression
                    (fun toks ->
                        if toks.IsEmpty then
                            Expr.Missing
                        else
                            Expr.SkipsTokens(toks)
                    )

            return Rule.Rule(pat, guard, arrow, expr)
        }

[<RequireQualifiedAccess>]
module Rules =
    let private pRule =
        recoverWith
            StoppingTokens.afterRule
            DiagnosticSeverity.Error
            DiagnosticCode.MissingRule
            (fun toks ->
                if toks.IsEmpty then
                    Rule.Missing
                else
                    Rule.SkipsTokens(toks)
            )
            Rule.parse


    let parse: Parser<Rules<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! firstBar = opt pBar
            let! rules, bars = sepBy1 pRule pBar
            return Rules(firstBar, List.ofSeq rules, List.ofSeq bars)
        }
