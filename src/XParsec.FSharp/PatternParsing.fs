namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

[<RequireQualifiedAccess>]
module PatParam =
    // PatParam is a restricted subset of patterns used for active pattern arguments
    let private refPatParam = FSRefParser<PatParam<SyntaxToken>>()

    let private pAtom =
        choiceL
            [
                nextNonTriviaTokenIsL Token.KWNull "null" |>> PatParam.Null

                nextNonTriviaTokenSatisfiesL
                    (fun synTok ->
                        let t = synTok.Token
                        t.IsLiteral || t = Token.KWTrue || t = Token.KWFalse
                    )
                    "Literal"
                |>> PatParam.Const

                // Identifiers / App
                parser {
                    let! lid = LongIdent.parse
                    // Check if it's an Application (App)
                    // This is recursive, e.g. Case(x) or Case x
                    // Simplified: Just consume one param if present?
                    // For PatParam, App usually implies `Ident(Param)`.
                    let! param = opt refPatParam.Parser

                    match param with
                    | ValueSome p -> return PatParam.App(lid, p)
                    | ValueNone -> return PatParam.LongIdent(lid)
                }

                // List [ ... ]
                parser {
                    let! l = pLBracket
                    let! parms, _ = sepBy refPatParam.Parser pSemi
                    let! r = pRBracket
                    return PatParam.List(l, List.ofSeq parms, r)
                }

                // Tuple ( ... )
                parser {
                    let! l = pLParen
                    let! parms, _ = sepBy refPatParam.Parser pComma
                    let! r = pRParen
                    return PatParam.Tuple(l, List.ofSeq parms, r)
                }

                // Quoted
                parser {
                    let! l = nextNonTriviaTokenIsL Token.OpQuotationTypedLeft "<@"
                    let! e = refExpr.Parser
                    let! r = nextNonTriviaTokenIsL Token.OpQuotationTypedRight "@>"
                    return PatParam.Quoted(l, e, r)
                }
            ]
            "PatParam Atom"

    let parse =
        parser {
            let! atom = pAtom
            // Check for Typed: atom : Type
            let! typed = opt pColon

            match typed with
            | ValueSome colon ->
                let! t = Type.parse
                return PatParam.Typed(atom, colon, t)
            | ValueNone -> return atom
        }

    do refPatParam.Set parse

module Pat =

    [<RequireQualifiedAccess>]
    type PatAux =
        | Type of Type<SyntaxToken>
        | AsIdent of SyntaxToken

    type PatOperatorParser() =
        // --- Precedence Definitions ---
        static let tuplePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Comma)
        static let asPrecedence = BindingPower.fromLevel (int PrecedenceLevel.As)
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

        static let completeParen (l: SyntaxToken) (p: Pat<SyntaxToken>) (r: SyntaxToken) = Pat.Paren(l, p, r)

        static let completeStruct (op: SyntaxToken) (r: Pat<SyntaxToken>) =
            match r with
            | Pat.Paren(l, Pat.Tuple(elements, ops), r) -> Pat.StructTuple(op, l, elements, ops, r)
            | _ ->
                // TODO: Error - struct must be followed by tuple
                Pat.Struct(op, r)


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
                    let op = InfixNary(token, preturn token, tuplePrecedence, completeTuple)
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

    let pListPat: Parser<ListPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                // Empty list `[]` is lexed as a single OpNil token
                parser {
                    let! t = nextNonTriviaTokenIsL Token.OpNil "[]"
                    return ListPat.ListPat(t, [], t)
                }
                // Non-empty list `[x; y]` with explicit brackets
                parser {
                    let! l = pLBracket
                    let! pats, _ = sepEndBy refPat.Parser pSemi
                    let! r = pRBracket
                    return ListPat.ListPat(l, List.ofSeq pats, r)
                }
            ]
            "ListPat"

    let pArrayPat: Parser<ArrayPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLArrayBracket "[|"
            let! pats, _ = sepEndBy refPat.Parser pSemi
            let! r = nextNonTriviaTokenIsL Token.KWRArrayBracket "|]"
            return ArrayPat.ArrayPat(l, List.ofSeq pats, r)
        }

    let pFieldPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! p = refPat.Parser
            return FieldPat.FieldPat(lid, eq, p)
        }

    let pRecordPat: Parser<RecordPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = pLBrace
            let! fields, _ = sepEndBy1 pFieldPat pSemi
            let! r = pRBrace
            return RecordPat.RecordPat(l, List.ofSeq fields, r)
        }

    let pNamed =
        parser {
            let! lid = LongIdent.parse
            let! param = opt PatParam.parse
            let! arg = opt refPat.Parser

            match lid, param, arg with
            | [ name ], ValueNone, ValueNone ->
                // Simple named pattern (variable)
                return Pat.NamedSimple(name)
            | _ ->
                // Full named pattern
                return Pat.Named(lid, param, arg)
        }

    let pTypeTest =
        parser {
            let! op = nextNonTriviaTokenIsL Token.OpTypeTest ":?"
            let! t = Type.parse
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

    let pPatAtom =
        choiceL
            [
                pWildcard |>> Pat.Wildcard
                nextNonTriviaTokenIsL Token.KWNull "null" |>> Pat.Null
                Constant.parse |>> Pat.Const
                pTypeTest
                pNamed
                pListPat |>> Pat.List
                pArrayPat |>> Pat.Array
                pRecordPat |>> Pat.Record
                pAttributesPat
            ]
            "Pattern Atom"

    let parse = Operator.parser pPatAtom (PatOperatorParser())
    let parseMany1 = many1 parse

    do refPat.Set parse


[<RequireQualifiedAccess>]
module PatternGuard =
    let parse: Parser<PatternGuard<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! w = pWhen
            // Use refExprGuard (bounded at Arrow precedence) so '->' is not consumed
            // as part of the guard expression and remains for Rule.parse's pArrowRight.
            let! e = refExprGuard.Parser
            return PatternGuard(w, e)
        }

[<RequireQualifiedAccess>]
module Rule =

    let parse: Parser<Rule<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! pat = Pat.parse
            let! guard = opt PatternGuard.parse
            let! arrow = pArrowRight
            let! expr = withContext OffsideContext.SeqBlock (pSeqBlock refExpr.Parser)
            return Rule.Rule(pat, guard, arrow, expr)
        }

[<RequireQualifiedAccess>]
module Rules =
    let parse: Parser<Rules<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            // Optional leading bar
            let! firstBar = opt pBar
            let! rules, bars = sepBy1 Rule.parse pBar
            return Rules(firstBar, List.ofSeq rules, List.ofSeq bars)
        }

[<RequireQualifiedAccess>]
module SimplePat =
    let parse: Parser<SimplePat<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = pIdent
            let! typeAnnotation = opt pColon

            match typeAnnotation with
            | ValueSome colon ->
                let! t = Type.parse
                return SimplePat.Typed(SimplePat.Ident ident, colon, t)
            | ValueNone -> return SimplePat.Ident ident
        }
