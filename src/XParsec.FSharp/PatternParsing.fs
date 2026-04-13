namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.OperatorParsing
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken

module Pat =

    [<RequireQualifiedAccess>]
    type PatAux =
        | Type of Type<SyntaxToken>
        | AsIdent of SyntaxToken

    // --- Shared Precedence / Helpers for pattern Pratt parsing ---
    // These are referenced by both `PatOperatorParser` (which models F#'s
    // `parenPattern` grammar, including `:` as a type-annotation operator) and
    // `PatHeadOperatorParser` (which models `headBindingPattern`, where `:` is
    // not part of the pattern and belongs to the binding's `optReturnType`).

    let private tuplePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Comma)
    let private asPrecedence = BindingPower.fromLevel (int PrecedenceLevel.As)
    let private semiPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Semicolon)
    let private pipePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Pipe)
    let private andPrecedence = BindingPower.fromLevel (int PrecedenceLevel.LogicalAnd)
    let private colonPrecedence = BindingPower.fromLevel (int PrecedenceLevel.TypeTest)
    let private consPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Cons)
    let private parenPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Parens)

    let private structPrecedence =
        BindingPower.fromLevel (int PrecedenceLevel.HighApplication)

    let private completeInfix (l: Pat<SyntaxToken>) (op: SyntaxToken) (r: Pat<SyntaxToken>) =
        match op.Token with
        | Token.OpBar -> Pat.Or(l, op, r)
        | Token.OpAmp -> Pat.And(l, op, r)
        | Token.KWColonColon -> Pat.Cons(l, op, r)
        | _ -> failwithf "Unexpected infix pattern operator: %A" op

    let private completeTuple (elements: ResizeArray<Pat<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
        Pat.Tuple(ImmutableArray.CreateRange(elements), ImmutableArray.CreateRange(ops))

    let private completeTyped (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
        match aux with
        | PatAux.Type t -> Pat.Typed(l, op, t)
        | _ -> failwith "Expected Type aux for Typed pattern"

    let private completeAs (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
        match aux with
        | PatAux.AsIdent ident -> Pat.As(l, op, ident)
        | _ -> failwith "Expected Ident aux for As pattern"

    let private completeParen (l: SyntaxToken) (p: Pat<SyntaxToken>) (r: SyntaxToken) =
        Pat.EnclosedBlock(ParenKind.Paren l, p, r)

    let private completeStruct (op: SyntaxToken) (r: Pat<SyntaxToken>) =
        match r with
        | Pat.EnclosedBlock(ParenKind.Paren l, Pat.Tuple(elements, ops), r) -> Pat.StructTuple(op, l, elements, ops, r)
        | _ ->
            // TODO: Error - struct must be followed by tuple in parens
            Pat.Struct(op, r)

    let private completeElems (exprs: ResizeArray<Pat<_>>) ops =
        Pat.Elems(ImmutableArray.CreateRange(exprs), ImmutableArray.CreateRange(ops))

    /// Emits a virtual `;` in pattern SeqBlock contexts when the next non-trivia token
    /// is at the current offside indent and can start a pattern. Mirrors `pSepVirt` in
    /// ExpressionParsing.fs so list/array patterns accept newline-separated elements.
    let private pSepVirtPat: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        let failSep =
            fail (Message "Expected ';' or newline at the same indent for pattern sequencing")

        parser {
            match! peekNextNonTriviaToken with
            | t when t.Token = Token.EOF -> return! failSep
            | t when t.Token = Token.OpSemicolon -> return! failSep
            | t ->
                if TokenInfo.canStartPattern t.Token then
                    let! indent = currentIndent
                    let! state = getUserState

                    let atContextIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                        | [] -> indent = 0

                    if atContextIndent then
                        return virtualToken (PositionedToken.Create(Token.OpSemicolon, t.StartIndex))
                    else
                        return! failSep
                else
                    return! failSep
        }

    let private pTypeRhs = Type.parse |>> PatAux.Type

    let private pAsRhs =
        parser {
            // NOTE: the 'as' token was already consumed by rhsParser's nextNonTriviaToken.
            // We only need to consume the identifier that follows.
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "identifier after 'as'"
            return PatAux.AsIdent ident
        }

    let private patLhsParser =
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

    /// Matches F#'s `parenPattern` grammar rule: includes `:` as a type-annotation
    /// operator so that `(x : int)`, `(x : int, y : float)`, etc. parse as
    /// per-element `Pat.Typed` inside the paren.
    type PatOperatorParser() =
        static let tokenToOp (token: SyntaxToken) =
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

        static let rhsParser =
            choiceL [ pSepVirtPat >>= tokenToOp; nextNonTriviaToken >>= tokenToOp ] "RHS pattern operator"

        interface Operators<
            SyntaxToken,
            PatAux,
            Pat<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = patLhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    /// Matches F#'s `headBindingPattern` grammar rule: same operators as
    /// `PatOperatorParser` except `:` is not consumed. In the F# grammar `:`
    /// is a production of `parenPattern` (and `simplePat` for primary-ctor
    /// arg lists), not of `headBindingPattern`; a top-level `let x : int = 5`
    /// puts the `: int` into the binding's `optReturnType`, not the head
    /// pattern. Used by `Pat.parseHead` for let-value binding heads.
    type PatHeadOperatorParser() =
        static let tokenToOp (token: SyntaxToken) =
            match token.Token with
            | Token.OpBar ->
                let op = InfixLeft(token, preturn token, pipePrecedence, completeInfix)
                preturn op

            | Token.OpAmp ->
                let op = InfixLeft(token, preturn token, andPrecedence, completeInfix)
                preturn op

            | Token.KWColonColon ->
                let op = InfixRight(token, preturn token, consPrecedence, completeInfix)
                preturn op

            | Token.KWAs ->
                let op = InfixMapped(token, preturn token, asPrecedence, pAsRhs, completeAs)
                preturn op

            | Token.OpComma ->
                let op = InfixNary(token, preturn token, tuplePrecedence, false, completeTuple)
                preturn op

            | Token.OpSemicolon ->
                let op = InfixNary(token, preturn token, semiPrecedence, false, completeElems)
                preturn op

            | _ -> fail (Message "Not a valid RHS pattern operator")

        static let rhsParser =
            choiceL [ pSepVirtPat >>= tokenToOp; nextNonTriviaToken >>= tokenToOp ] "RHS pattern operator"

        interface Operators<
            SyntaxToken,
            PatAux,
            Pat<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = patLhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let private refPat = FSRefParser<Pat<SyntaxToken>>()
    let private refPatSeqBlock = FSRefParser<Pat<SyntaxToken>>()
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
            refPatSeqBlock.Parser

    let pArrayPat =
        pEnclosed
            pLArrayBracket
            Token.KWRArrayBracket
            ParenKind.Array
            OffsideContext.BracketBar
            DiagnosticCode.ExpectedRArrayBracket
            refPatSeqBlock.Parser

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
                        let! fields, seps = withContext OffsideContext.SeqBlock (sepEndBy1 pFieldPat pRecordFieldSep)
                        let! r = pRBrace
                        return Pat.Record(l, fields, seps, r)
                    }

                match innerParser reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside braceEntry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message "Record pattern") reader

    // Shared '|' / 'as' chain handling for union case arg patterns.
    // The Pratt parser for union fields excludes 'as' and '|' (below Comma+1 cutoff),
    // so we reapply them manually here around a parsed base pattern.
    let private pBarToken =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "'|'"

    let private pOrAsChain (basePat: Pat<SyntaxToken>) =
        parser {
            let! orAlts =
                many (
                    parser {
                        let! barTok = pBarToken
                        let! altPat = refUnionFieldPat.Parser
                        return struct (barTok, altPat)
                    }
                )

            let p =
                if orAlts.IsEmpty then
                    basePat
                else
                    orAlts
                    |> Seq.fold (fun acc struct (barTok, altPat) -> Pat.Or(acc, barTok, altPat)) basePat

            let! asClause =
                opt (
                    parser {
                        let! asTok = pAs

                        let! ident =
                            nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "identifier after 'as'"

                        return struct (asTok, ident)
                    }
                )

            return
                match asClause with
                | ValueSome(asTok, ident) -> Pat.As(p, asTok, ident)
                | ValueNone -> p
        }

    /// Parse a single named field in a union case pattern: fieldName = pat (excludes comma).
    let private pUnionNamedArgPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! basePat = withContext OffsideContext.SeqBlock refUnionFieldPat.Parser
            let! p = pOrAsChain basePat
            return UnionArgPat.Named(lid, eq, p)
        }

    /// Parse a single positional argument in a union case pattern: pat (excludes comma).
    let private pUnionPositionalArgPat =
        parser {
            let! basePat = withContext OffsideContext.SeqBlock refUnionFieldPat.Parser
            let! p = pOrAsChain basePat
            return UnionArgPat.Positional p
        }

    /// Parse a union case argument: either a named field (tried first) or a positional pattern.
    let private pUnionArgPat = pUnionNamedArgPat <|> pUnionPositionalArgPat

    /// Parse named field patterns: UnionCase(field1 = pat1, _, field2 = pat2, ...).
    /// Accepts an arbitrary mix of named and positional args in any order, separated by
    /// commas, semicolons, or newline-at-indent. Commits to this AST shape only when at
    /// least one argument is a named field; otherwise fails so `pNamed`'s fallback can
    /// handle the positional-only case with the standard `Pat.Named(lid, param, arg)`.
    let private pNamedFieldPats =
        parser {
            let! lid = LongIdent.parse
            let! lParen = pLParen

            let! args, seps = withContext OffsideContext.SeqBlock (sepBy1 pUnionArgPat (pComma <|> pRecordFieldSep))

            let! rParen = pRParen

            let hasNamed =
                args
                |> Seq.exists (
                    function
                    | UnionArgPat.Named _ -> true
                    | _ -> false
                )

            if hasNamed then
                return Pat.NamedFieldPats(lid, lParen, args, seps, rParen)
            else
                return! fail (Message "positional-only ctor pattern")
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

                    match lid.Length, param, arg with
                    | 1, ValueNone, ValueNone ->
                        // Simple named pattern (variable)
                        return Pat.NamedSimple(lid[0])
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
            // Check optional 'as pat' — accepts any pattern, not just an identifier
            let! asClause =
                opt (
                    parser {
                        let! asTok = pAs
                        let! pat = pNamed
                        return struct (asTok, pat)
                    }
                )

            match asClause with
            | ValueSome(asTok, pat) -> return Pat.TypeTestAs(op, t, asTok, pat)
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

    let private isStringClose (tok: Token) =
        match tok with
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal -> true
        | _ -> false

    let private isStringTextFragment (tok: Token) =
        match tok with
        | Token.StringFragment
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> true
        | _ -> false

    let private stringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | _ -> invalidOp $"Not a string open token: {t.Token}"

    let pStringPat =
        let rec loop (parts: ResizeArray<StringPart<SyntaxToken>>) reader =
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok t when isStringTextFragment t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fragment ->
                    parts.Add(StringPart.Text fragment)
                    loop parts reader
            | Ok t when t.Token = Token.EscapeSequence ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok esc ->
                    parts.Add(StringPart.EscapeSequence esc)
                    loop parts reader
            | Ok t when t.Token = Token.FormatPlaceholder ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fmt ->
                    parts.Add(StringPart.FormatSpecifier fmt)
                    loop parts reader
            | Ok t when
                t.Token = Token.InvalidFormatPlaceholder
                || t.Token = Token.InvalidFormatPercents
                ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok _ -> loop parts reader
            | Ok _ -> Ok parts

        parser {
            let! opening =
                nextNonTriviaTokenSatisfiesL
                    (fun t ->
                        match t.Token with
                        | Token.StringOpen
                        | Token.VerbatimStringOpen
                        | Token.String3Open -> true
                        | _ -> false
                    )
                    "Expected string literal open"

            let kind = stringKindOfToken opening
            let! parts = loop (ResizeArray())

            let! closing = nextNonTriviaTokenSatisfiesL (fun t -> isStringClose t.Token) "Expected string close"

            return Pat.String(kind, ImmutableArray.CreateRange(parts), closing)
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
                Token.StringOpen, pStringPat
                Token.VerbatimStringOpen, pStringPat
                Token.String3Open, pStringPat
            ]
            pConstPat

    do refPatAtomic.Set parseAtomic

    let parse = Operator.parser parseAtomic (PatOperatorParser())

    let parseSeqBlock = withContext OffsideContext.SeqBlock parse

    /// Matches F#'s `headBindingPattern` grammar rule. Same operators as `parse`
    /// except `:` is not consumed (the surrounding binding's `optReturnType`
    /// owns any trailing `: type`). Used by `Binding.parseValue` for let-value
    /// binding heads.
    let parseHead = Operator.parser parseAtomic (PatHeadOperatorParser())

    // For record field patterns, we want to allow the same operators as the top-level, but not semicolon since that separates fields.
    let private parseFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Semicolon + 1)) parseAtomic (PatOperatorParser())

    // For named union case field patterns (comma-separated), exclude both comma and semicolon.
    let private parseUnionFieldPat =
        Operator.parserAt (BindingPower.fromLevel (int PrecedenceLevel.Comma + 1)) parseAtomic (PatOperatorParser())

    let parseMany1 = many1 parse
    let parseAtomicMany1 = many1 parseAtomic

    do refPat.Set parse
    do refPatSeqBlock.Set parseSeqBlock
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

            // Grammar: patternAndGuard RARROW typedSeqExprBlock
            let! expr =
                refTypedSeqExprBlock.Parser
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
            return Rules(firstBar, rules, bars)
        }
