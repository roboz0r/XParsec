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
open XParsec.FSharp.Parser.ParseState

module ElifBranches =
    [<Struct; RequireQualifiedAccess>]
    type private ElIfTok =
        | Elif of el: SyntaxToken
        | Else of el: SyntaxToken
        | ElseIf of el: SyntaxToken * ifTok: SyntaxToken

    let private pElifOrElseIf =
        dispatchNextSyntaxTokenL
            [
                Token.KWElif,
                parser {
                    let! elifTok = pElif
                    return ElIfTok.Elif elifTok
                }
                Token.KWElse,
                parser {
                    let! elseTok = pElse

                    match! peekNextSyntaxToken with
                    | t when t.Token = Token.KWIf ->
                        // `else if` is collapsed into a single chain arm (matching
                        // F#'s LexFilter: `ELSE` immediately followed by `IF` becomes
                        // `ELIF`) only when it's clearly sugar for flat chaining:
                        //   (a) same source line (classic inline `else if`), or
                        //   (b) `if` at or left of the `else` column (multi-line form
                        //       where the `if` is undentated to align with `else`).
                        // When the `if` is strictly to the right of `else`, the user
                        // has written a nested `if` inside the else body — let the
                        // else branch parse the whole body as an expression so
                        // subsequent sibling statements at the inner col join via
                        // the else body's own SeqBlock.
                        let! state = getUserState

                        let collapse =
                            match elseTok.Index, t.Index with
                            | TokenIndex.Regular elseIdx, TokenIndex.Regular ifIdx ->
                                let elseCol = ParseState.getIndent state elseIdx
                                let ifCol = ParseState.getIndent state ifIdx
                                let elseLine = ParseState.findLineNumber state elseIdx
                                let ifLine = ParseState.findLineNumber state ifIdx
                                elseLine = ifLine || ifCol <= elseCol
                            | _ -> true

                        if collapse then
                            let! ifTok = consumePeeked t
                            return ElIfTok.ElseIf(elseTok, ifTok)
                        else
                            return ElIfTok.Else elseTok
                    | _ -> return ElIfTok.Else elseTok
                }
            ]
            "Expected 'elif' or 'else'"

    // Anchor the arm's If and Then contexts at (indent + 1). `indent` is the
    // chain's effective offside column — the leftmost of the arm's keywords:
    //   - `elif`     — elif's column
    //   - `else if`  — min(else_col, if_col). `else if` is F# LexFilter sugar
    //                  that collapses to a single chain arm; the arm aligns at
    //                  the leftmost keyword. For same-line `else if` that's
    //                  `else`; for multi-line `else\nif` (where collapse requires
    //                  `if_col <= else_col`, e.g. `if` starts the next line after
    //                  a trailing `else`) it's `if`.
    // Anchoring at +1 (rather than the peeked expression token's column, which
    // is what plain `withContext` would use) is needed so multi-line conditions
    // that undent onto the following line remain within the If context.
    let pConditionThen (indent: int) (armKeyword: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        let pCond =
            withContextAt OffsideContext.If (indent + 1) armKeyword.PositionedToken refExpr.Parser
        // Grammar: THEN typedSeqExprBlock
        let pThenExpr =
            withContextAt OffsideContext.Then (indent + 1) armKeyword.PositionedToken refTypedSeqExprBlock.Parser

        match pCond reader with
        | Ok condition ->
            match pThen reader with
            | Ok thenTok ->
                match pThenExpr reader with
                | Ok expr -> Ok(condition, thenTok, expr)
                | Error e -> Error e
            | Error e -> Error e
        | Error e -> Error e

    // Grammar: ELSE typedSeqExprBlock
    let private pElseExpr = withContext OffsideContext.Else refTypedSeqExprBlock.Parser

    let private getIndent (reader: Reader<PositionedToken, ParseState, _>) (tok: SyntaxToken) =
        match tok.Index with
        | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
        | TokenIndex.Virtual -> 0

    let rec private parseBranches (acc: ResizeArray<_>) (reader: Reader<PositionedToken, ParseState, _>) =
        match pElifOrElseIf reader with
        | Ok(ElIfTok.Elif elifTok) ->
            let indent = getIndent reader elifTok

            match pConditionThen indent elifTok reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.Elif(elifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.ElseIf(elseTok, ifTok)) ->
            // Leftmost of the pair is the chain's alignment column — see comment on pConditionThen.
            let indent = min (getIndent reader elseTok) (getIndent reader ifTok)

            match pConditionThen indent elseTok reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.ElseIf(elseTok, ifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.Else elseTok) ->
            match pElseExpr reader with
            | Ok expr -> Ok struct (ImmutableArray.CreateRange acc, ValueSome(ElseBranch.ElseBranch(elseTok, expr)))
            | Error e -> Error e

        | Error e -> Ok struct (ImmutableArray.CreateRange acc, ValueNone)

    let parse: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        fun reader -> parseBranches (ResizeArray()) reader

module Binding =
    let private errNoTrailingTypeAnnotation: ErrorType<PositionedToken, ParseState> =
        Message "no trailing type annotation"

    let private errNotNamedFieldCtorHead: ErrorType<PositionedToken, ParseState> =
        Message "not a named-field ctor head"

    let private pMutableTok =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    /// Build the Pat head from an IdentOrOp
    let private headPatOfIdentOrOp (identOrOp: IdentOrOp<SyntaxToken>) : Pat<SyntaxToken> =
        match identOrOp with
        | IdentOrOp.Ident t -> Pat.NamedSimple t
        | _ -> Pat.Op identOrOp

    // Parses a single static-optimization constraint: `^T : Type` or `^T : struct`.
    // Distinct from general type-parameter constraints (Constraint.pTyparConstraints)
    // because the RHS is an arbitrary named type, not a keyword like 'equality'.
    let private pStaticOptimizationConstraint =
        parser {
            let! typar = Typar.parse

            match! peekNextSyntaxToken with
            | t when t.Token = Token.KWStruct ->
                // Bare `'T struct` — no colon
                let! structTok = consumePeeked t
                return StaticOptimizationConstraint.WhenTyparIsStruct(typar, ValueNone, structTok)
            | _ ->
                let! colon = pColon

                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWStruct ->
                    let! structTok = consumePeeked t
                    return StaticOptimizationConstraint.WhenTyparIsStruct(typar, ValueSome colon, structTok)
                | _ ->
                    let! rhsType = Type.parse
                    return StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar, colon, rhsType)
        }

    let private pAndConstraint =
        parser {
            let! andTok = pAnd
            let! c = pStaticOptimizationConstraint
            return struct (andTok, c)
        }

    // Parses one `when c1 and c2 ... = optimizedExpr` clause, wrapping the given baseExpr.
    let private pStaticOptimizationClause (baseExpr: Expr<SyntaxToken>) =
        parser {
            let! whenTok = pWhen
            let! firstC = pStaticOptimizationConstraint
            let! restPairs = many pAndConstraint

            let constraints =
                ImmutableArray.CreateRange(
                    seq {
                        yield firstC

                        for struct (_, c) in restPairs do
                            yield c
                    }
                )

            let ands =
                ImmutableArray.CreateRange(
                    seq {
                        for struct (a, _) in restPairs do
                            yield a
                    }
                )

            let! equalsTok = pEquals
            let! optExpr = refExprSeqBlock.Parser

            return Expr.LibraryOnlyStaticOptimization(baseExpr, whenTok, constraints, ands, equalsTok, optExpr)
        }

    // Left-fold any trailing `when` clauses onto the binding body. Outermost node
    // ends up holding the last `when` clause in source order, matching F# compiler semantics.
    // Uses `choice` so that a peek failure (EndOfInput / offside) after the body cleanly
    // falls back to returning the base expression unchanged. Without the fallback, every
    // binding body would need to guarantee a following token, which is not true at
    // end-of-file or at the tail of a member list inside a type definition.
    let rec private pChainStaticOptimizations (baseExpr: Expr<SyntaxToken>) =
        let pClause =
            parser {
                let! wrapped = pStaticOptimizationClause baseExpr
                return! pChainStaticOptimizations wrapped
            }

        choice [ pClause; preturn baseExpr ]

    let private pBindingBody =
        // Captures the colon's column before consumption so the following type
        // can be parsed inside a stricter offside context (col_of_colon + 1).
        // Without this, Type.parse would greedily slurp suffix identifiers from
        // following lines as type suffixes (`'T array` + `res` -> `res<'T array>`).
        let pColonPeek (reader: Reader<PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>>) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.OpColon ->
                let col = ParseState.getIndent reader.State (reader.Index * 1<token>)

                match consumePeeked t reader with
                | Ok colon -> Ok(struct (colon, col))
                | Error e -> Error e
            | Ok _ -> fail errNoTrailingTypeAnnotation reader
            | Error e -> Error e

        parser {
            let! expr = refExprSeqBlock.Parser
            let! maybeColon = opt pColonPeek

            let! annotated =
                parser {
                    match maybeColon with
                    | ValueSome(colon, colonCol) ->
                        let! typ = withContextAt OffsideContext.Let (colonCol + 1) colon.PositionedToken Type.parse

                        return Expr.TypeAnnotation(expr, colon, typ)
                    | ValueNone -> return expr
                }

            return! pChainStaticOptimizations annotated
        }

    // Detects a named-field destructure head: `( Identifier = ...`. Used only as a
    // lookahead guard in `parseFunction`; the shape is unambiguously a value-binding
    // pattern (F# let bindings don't support default-valued parameters), so letting
    // parseFunction commit here would cause pEnclosed to recover with a virtual `)`
    // and bury the real parse.
    let private pNamedFieldCtorArgHead =
        lookAhead (
            parser {
                let! t1 = nextSyntaxToken
                let! t2 = nextSyntaxToken
                let! t3 = nextSyntaxToken

                if
                    t1.Token = Token.KWLParen
                    && (t2.Token = Token.Identifier || t2.Token = Token.BacktickedIdentifier)
                    && t3.Token = Token.OpEquality
                then
                    return ()
                else
                    return! fail errNotNamedFieldCtorHead
            }
        )

    /// Parse a function-style binding: [inline] [access] identOrOp [typar-defns] pat+ [: returnType] = expr
    let parseFunction attrs =
        let pBody = pBindingBody

        parser {
            let! inlineTok = opt pInline
            let! access = opt pAccessModifier
            let! identOrOp = IdentOrOp.parse
            do! notFollowedByL pNamedFieldCtorArgHead "named-field destructure"
            let! typarDefns = opt TyparDefns.parse
            // Parse argument patterns (atomic to avoid consuming return type annotations).
            // Operator definitions (e.g., `let (|PointFree|) = expr`) allow zero arguments.
            // Generic value definitions (e.g., `let inline f<'T> : Type = expr`) also allow zero arguments.
            // Named function definitions require at least one argument.
            let! argumentPats =
                match identOrOp with
                | IdentOrOp.ParenOp _
                | IdentOrOp.StarOp _ -> many Pat.parseAtomicBindingArg
                | IdentOrOp.Ident _ when typarDefns.IsSome -> many Pat.parseAtomicBindingArg
                | IdentOrOp.Ident _ -> Pat.parseAtomicBindingArgMany1

            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pBody

            return
                {
                    attributes = attrs
                    inlineToken = inlineTok
                    mutableToken = ValueNone
                    fixedToken = ValueNone
                    access = access
                    headPat = headPatOfIdentOrOp identOrOp
                    typarDefns = typarDefns
                    argumentPats = argumentPats
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse a value-style binding: [mutable] [access] pat [typar-defns] [: returnType] = expr
    let parseValue attrs =
        let pBody = pBindingBody

        parser {
            let! mut = opt pMutableTok
            let! access = opt pAccessModifier
            let! pat = Pat.parseHead
            let! typarDefns = opt TyparDefns.parse
            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pBody

            return
                {
                    attributes = attrs
                    inlineToken = ValueNone
                    mutableToken = mut
                    fixedToken = ValueNone
                    access = access
                    headPat = pat
                    typarDefns = typarDefns
                    argumentPats = ImmutableArray.Empty
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse either a function or a value binding
    let parse attrs : Parser<Binding<_>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        choiceL [ parseFunction attrs; parseValue attrs ] "Binding"

    let parseSepByAnd1 attrs =
        let pAndBinding =
            parser {
                let! andTok = pAnd
                let! andAttrs = opt Attributes.parse
                let! binding = parse andAttrs
                return struct (andTok, binding)
            }

        fun (reader: Reader<_, _, _>) ->
            match parse attrs reader with
            | Error e -> Error e
            | Ok first ->
                match many pAndBinding reader with
                | Error e -> Error e
                | Ok rest ->
                    let bindings = ImmutableArray.CreateBuilder(1 + rest.Length)
                    let ands = ImmutableArray.CreateBuilder(rest.Length)
                    bindings.Add(first)

                    for i = 0 to rest.Length - 1 do
                        let struct (andTok, b) = rest.[i]
                        ands.Add(andTok)
                        bindings.Add(b)

                    preturn (struct (bindings.ToImmutable(), ands.ToImmutable())) reader

[<AutoOpen>]
module private MemberHelpers =
    // Forward reference for MemberDefn to avoid circular dependency issues
    // and provide a stub for ObjectMembers
    let refMemberDefn = FSRefParser<MemberDefn<SyntaxToken>>()

[<RequireQualifiedAccess>]
module FieldInitializer =
    let parse: Parser<FieldInitializer<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! id = LongIdent.parse
            let! equals = pEquals
            let! expr = withContext OffsideContext.SeqBlock refExprInRecords.Parser
            return FieldInitializer(id, equals, expr)
        }

    let parseSepBySemi1 = sepBy1 parse pSemi

[<RequireQualifiedAccess>]
module ObjectConstruction =
    let private errNoConstructorArguments: ErrorType<PositionedToken, ParseState> =
        Message "No constructor arguments"

    // Dummy function to ensure the static constructor runs and initializes refObjectConstruction
    let mutable private x = 0
    let init () = x <- x + 1

    let parse: Parser<ObjectConstruction<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! typ = Type.parse

            // Check for constructor arguments: parens, literals, or other expression starters.
            // Attributes can use `[<Attr "str">]` or `[<Attr(args)>]` syntax.
            let! argExpr =
                opt (
                    parser {
                        let! token = lookAhead nextSyntaxToken

                        if
                            token.Token = Token.KWLParen
                            || TokenInfo.isLiteral token.Token
                            || token.Token = Token.StringOpen
                            || token.Token = Token.VerbatimStringOpen
                            || token.Token = Token.String3Open
                        then
                            return! refExpr.Parser
                        else
                            return! fail errNoConstructorArguments
                    }
                )

            match argExpr with
            | ValueSome expr -> return ObjectConstruction.ObjectConstruction(typ, expr)
            | ValueNone -> return ObjectConstruction.InterfaceConstruction(typ)
        }

    do refObjectConstruction.Set parse

[<RequireQualifiedAccess>]
module BaseCall =
    let parse: Parser<BaseCall<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            // Note: This parser expects 'inherit' to be handled by the caller,
            // as the BaseCall AST type does not contain the 'inherit' token.

            let! construction = ObjectConstruction.parse

            let! asPart =
                opt (
                    parser {
                        let! asTok = pAs
                        let! ident = pIdent
                        return (asTok, ident)
                    }
                )

            match asPart with
            | ValueSome(asTok, ident) -> return BaseCall.NamedBaseCall(construction, asTok, ident)
            | ValueNone -> return BaseCall.AnonBaseCall(construction)
        }

[<RequireQualifiedAccess>]
module ObjectMembers =
    let parse: Parser<ObjectMembers<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! withTok = pWith

            // Parse list of members until 'end'
            // We use `many` combined with a check for the `end` token to terminate
            let! members, endTok = manyTill refMemberDefn.Parser pEnd

            return ObjectMembers.ObjectMembers(withTok, members, endTok)
        }

[<RequireQualifiedAccess>]
module InterfaceImpl =
    let parse: Parser<InterfaceImpl<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        parser {
            let! interfaceTok = pInterface
            let! typ = Type.parse
            let! members = opt ObjectMembers.parse
            return InterfaceImpl.InterfaceImpl(interfaceTok, typ, members)
        }

/// Used by ExprOperatorParser to carry auxiliary information from operator-specific parsers back to the main expression parser for completion.
[<RequireQualifiedAccess>]
type ExprAux =
    | Ident of SyntaxToken
    | TypeApp of SyntaxToken * ImArr<Type<SyntaxToken>> * ImArr<SyntaxToken> * SyntaxToken
    | DotIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // .[ expr ]
    | DotParenOp of IdentOrOp<SyntaxToken> // .( op ) — qualified operator access
    | PostfixDynamic of SyntaxToken * Type<SyntaxToken> // :? Type (DynamicTypeTest)
    | HighPrecApp of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // ( expr ) for f(x, y)
    | HighPrecIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // [ expr ] for arr[i] (F# 6+ dot-less indexing)
    | TypeCast of Type<SyntaxToken> // :> Type
    /// Wraps a fully-parsed Expr for use as 'Aux in PrefixMapped keyword operators (if, match, let, etc.)
    | KeywordExpr of (SyntaxToken -> Expr<SyntaxToken>)
    | ForExpr of Expr<SyntaxToken>
    | SliceAll
    | SliceFrom
    | Range of Expr<SyntaxToken>

[<RequireQualifiedAccess>]
module Expr =
    let private errNoTrailingTypeAnnotation: ErrorType<PositionedToken, ParseState> =
        Message "no trailing type annotation"

    let private errExpectedInAtLetIndent: ErrorType<PositionedToken, ParseState> =
        Message "Expected 'in' at the same indent as 'let'"

    let private errExpectedRhsOperator: ErrorType<PositionedToken, ParseState> =
        Message "Expected RHS operator"

    let private errUnexpectedBarOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected '|' operator in expression context"

    let private errUnexpectedDereferenceOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected dereference operator '!' in infix context"

    let private errUnexpectedArrowOp: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected arrow operator '->' in infix context"

    let private errUnexpectedPrefixKeywordRhs: ErrorType<PositionedToken, ParseState> =
        Message "Unexpected prefix keyword in RHS expression context"

    let private errLhsOnlyOpInInfix: ErrorType<PositionedToken, ParseState> =
        Message "LHS-only operator cannot appear in infix position"

    let private errExpectedSliceAll: ErrorType<PositionedToken, ParseState> =
        Message "Expected ',' or ']' for slice all syntax"

    let private errNotPrefixOp: ErrorType<PositionedToken, ParseState> =
        Message "Not a prefix operator"

    let private errNotCEBody: ErrorType<PositionedToken, ParseState> =
        Message "Not a computation expression body"

    let bp x =
        LanguagePrimitives.ByteWithMeasure<bp> x

    let pl x : PrecedenceLevel = LanguagePrimitives.EnumOfValue x

    let private refExprRange = FSRefParser<Expr<SyntaxToken>>()

    // Grammar: typedSeqExprBlock = seqExpr [':' type]
    // Captures the colon's column before consumption so the following type
    // parses inside a stricter offside context (colonCol + 1), preventing
    // Type.parse from greedily slurping suffix identifiers on following lines.
    let pTypedSeqExprBlock =
        let pColonPeek (reader: Reader<PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>>) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.OpColon ->
                let col = ParseState.getIndent reader.State (reader.Index * 1<token>)

                match consumePeeked t reader with
                | Ok colon -> Ok(struct (colon, col))
                | Error e -> Error e
            | Ok _ -> fail errNoTrailingTypeAnnotation reader
            | Error e -> Error e

        parser {
            let! expr = refExprSeqBlock.Parser
            let! maybeColon = opt pColonPeek

            match maybeColon with
            | ValueSome(colon, colonCol) ->
                let! typ = withContextAt OffsideContext.Let (colonCol + 1) colon.PositionedToken Type.parse

                return Expr.TypeAnnotation(expr, colon, typ)
            | ValueNone -> return expr
        }

    /// Completion functions for operator parsing — pure transformations from the
    /// parsed pieces (LHS / operator / RHS or Aux) into an Expr. These are
    /// independent of any ExprOperatorParser state, so they live at module scope.
    module private Complete =
        let infix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)

        let range (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) =
            match l with
            | Expr.Range(l, dotdot1, step) ->
                // We already have a range on the left, so this must be a stepped range
                // with the new step in the middle and the new end on the right
                Expr.SteppedRange(l, dotdot1, step, op, r)
            | _ -> Expr.Range(l, op, r)

        // Shared PrefixNoConsume completion for fun, function, if, match, try, while, for —
        // each wraps its fully-parsed body into ExprAux.ForExpr (historical name).
        let forE _forTok (expr: ExprAux) =
            match expr with
            | ExprAux.ForExpr forExpr -> forExpr
            | _ -> failwith "Unexpected Aux type for For expression completion"

        let sequence (exprs: ResizeArray<Expr<_>>) ops =
            Expr.Sequential(ImmutableArray.CreateRange exprs, ImmutableArray.CreateRange ops)

        let assignment (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.Assignment(l, op, r)
        let prefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let lazyE (op: SyntaxToken) (e: Expr<_>) = Expr.Lazy(op, e)
        let assertE (op: SyntaxToken) (e: Expr<_>) = Expr.Assert(op, e)
        let upcastE (op: SyntaxToken) (e: Expr<_>) = Expr.Upcast(op, e)
        let downcastE (op: SyntaxToken) (e: Expr<_>) = Expr.Downcast(op, e)
        let sliceTo (op: SyntaxToken) (e: Expr<_>) = Expr.SliceTo(op, e)

        let tuple (elements: ResizeArray<Expr<_>>) (ops: ResizeArray<SyntaxToken>) =
            Expr.Tuple(ImmutableArray.CreateRange elements, ImmutableArray.CreateRange ops)

        let app (elements: ResizeArray<Expr<_>>) ops =
            let args = ImmutableArray.CreateBuilder(elements.Count - 1)

            for i in 1 .. elements.Count - 1 do
                args.Add(elements[i])

            Expr.App(elements[0], args.ToImmutable())

        let sliceAll (op: SyntaxToken) (x: ExprAux) =
            match x with
            | ExprAux.SliceAll -> Expr.SliceAll(op)
            | _ -> failwith "Unexpected Aux type for SliceAll completion"

        let rangeOrSliceFrom (l: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.SliceFrom -> Expr.SliceFrom(l, op)
            | ExprAux.Range r -> range l op r
            | _ -> failwith "Unexpected Aux type for RangeOrSliceFrom completion"

        let dot (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.Ident ident ->
                match expr with
                | Expr.Ident firstIdent ->
                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ImmutableArray.Create(firstIdent, ident)))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdentOrOp) ->
                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(longIdentOrOp.Add(ident)))
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.LongIdent(ImmutableArray.Create(ident)))
            | ExprAux.DotParenOp identOrOp ->
                match expr with
                | Expr.Ident firstIdent ->
                    Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(ImmutableArray.Create(firstIdent), op, identOrOp))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdent) ->
                    Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent, op, identOrOp))
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.Op identOrOp)
            | ExprAux.DotIndex(lBracket, indexExpr, rBracket) ->
                Expr.IndexedLookup(expr, ValueSome op, lBracket, indexExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for dot completion"

        let typeApp (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeApp(lAngle, types, commas, rAngle) -> Expr.TypeApp(expr, lAngle, types, commas, rAngle)
            | _ -> failwith "Unexpected Aux type for type application completion"

        let highPrec (funcExpr: Expr<_>) (_op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.HighPrecApp(lParen, argExpr, rParen) -> Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen)
            | ExprAux.HighPrecIndex(lBracket, argExpr, rBracket) ->
                Expr.IndexedLookup(funcExpr, ValueNone, lBracket, argExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for high-precedence application/index completion"

        let typeCast (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeCast(typ) ->
                match op.Token with
                | Token.OpColon -> Expr.TypeAnnotation(expr, op, typ)
                | Token.OpUpcast -> Expr.StaticUpcast(expr, op, typ)
                | Token.OpDowncast -> Expr.DynamicDowncast(expr, op, typ)
                | Token.OpTypeTest -> Expr.DynamicTypeTest(expr, op, typ)
                | _ -> failwith "Unexpected operator for type cast completion"
            | _ -> failwith "Unexpected Aux type for type cast completion"

        let keyword (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.KeywordExpr e -> e op
            | _ -> failwith "Unexpected Aux type for keyword expression"

    /// Keyword-expression body parsers (if/match/fun/function/try/while/for/let/use/yield/return/do).
    /// Each parser consumes its own keyword via `assertKeywordToken` and uses `withContextAt` to
    /// set the offside indent based on the keyword's column; lhsParser peeks but does NOT consume.
    /// Lifted from ExprOperatorParser so they're initialized once at module load rather than
    /// per-instance.
    module private KWBody =
        let recoverExprMissing p =
            recoverWith
                StoppingTokens.afterExpr
                DiagnosticSeverity.Error
                DiagnosticCode.MissingExpression
                (fun toks ->
                    if toks.IsEmpty then
                        Expr.Missing
                    else
                        Expr.SkipsTokens(toks)
                )
                p

        // Used for for-to and use identifiers (not the dot-access pIdent above)
        let pIdentTok = nextSyntaxIdentifierLMsg "identifier"

        let pDoneVirt reader =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.KWDone -> consumePeeked t reader
            | Ok t ->
                let doneTok =
                    virtualToken (PositionedToken.Create(Token.VirtualDone, t.PositionedToken.StartIndex))

                Ok doneTok
            | Error e ->
                match reader.Current with
                | ValueSome t ->
                    let doneTok = virtualToken (PositionedToken.Create(Token.VirtualDone, t.StartIndex))
                    Ok doneTok
                | ValueNone ->
                    // No more tokens means something else skipped past the EOF marker token
                    // So, we throw here
                    failwith "Unexpected end of input while looking for 'done' or virtual 'done'"

        let pIfExpr =
            parser {
                let! (ifTok, indent) = assertKeywordToken Token.KWIf
                // Condition at if_col + 1
                let! cond =
                    recoverExprMissing (
                        withContextAt OffsideContext.If (indent + 1) ifTok.PositionedToken refExpr.Parser
                    )
                // then permitted undentation at if_col via contextPermitsToken
                let! thenTok = recoverWithVirtualToken Token.KWThen "Expected 'then' after condition" pThen
                // Body anchored to if_col + 1, NOT then_col + 1
                // Grammar: THEN typedSeqExprBlock
                let! thenExpr =
                    recoverExprMissing (
                        withContextAt OffsideContext.Then (indent + 1) ifTok.PositionedToken refTypedSeqExprBlock.Parser
                    )

                let! elifs, elseBranch = ElifBranches.parse

                return ExprAux.ForExpr(Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, elifs, elseBranch))
            }

        let pMatchRules =
            withContext OffsideContext.MatchClauses Rules.parse
            |> recoverWith
                StoppingTokens.afterRule
                DiagnosticSeverity.Error
                DiagnosticCode.MissingRule
                (fun toks ->
                    let missing =
                        Rule.Rule(
                            Pat.Missing,
                            ValueNone,
                            virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                            Expr.Missing
                        )

                    if toks.IsEmpty then
                        Rules(ValueNone, ImmutableArray.Create(missing), ImmutableArray.Empty)
                    else
                        let missingWithSkips =
                            Rule.Rule(
                                Pat.SkipsTokens(toks),
                                ValueNone,
                                virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                Expr.Missing
                            )

                        Rules(ValueNone, ImmutableArray.Create(missingWithSkips), ImmutableArray.Empty)
                )

        let pMatchExpr =
            let parseMatchBody
                (matchTok: SyntaxToken)
                (indent: int)
                (reader: Reader<PositionedToken, ParseState, _>)
                : Result<ExprAux, _> =
                let savedState = reader.State

                // Push Match context at match_col — stays active for with/| undentation
                let matchEntry =
                    {
                        Context = OffsideContext.Match
                        Indent = indent
                        Token = matchTok.PositionedToken
                    }

                reader.State <- ParseState.pushOffside matchEntry reader.State

                // Match expression inner content at match_col + 1
                match
                    recoverExprMissing
                        (withContextAt OffsideContext.SeqBlock (indent + 1) matchTok.PositionedToken refExpr.Parser)
                        reader
                with
                | Error e ->
                    reader.State <- savedState
                    Error e
                | Ok e ->

                    // with permitted at match_col via contextPermitsToken (Match context still active)
                    match
                        recoverWithVirtualToken Token.KWWith "Expected 'with' after match expression" pWith reader
                    with
                    | Error e ->
                        reader.State <- savedState
                        Error e
                    | Ok w ->

                        // | permitted at match_col via contextPermitsToken (Match context still active)
                        match pMatchRules reader with
                        | Error e ->
                            reader.State <- savedState
                            Error e
                        | Ok rules ->

                            reader.State <- ParseState.popOffside matchEntry reader.State
                            Ok(ExprAux.ForExpr(Expr.Match(matchTok, e, w, rules)))

            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                match assertKeywordTokens Token.KWMatch Token.KWMatchBang reader with
                | Error e -> Error e
                | Ok(matchTok, indent) -> parseMatchBody matchTok indent reader

        let pFunctionExpr =
            parser {
                let! (funcTok, indent) = assertKeywordToken Token.KWFunction
                // Function context at function_col for | undentation
                let! rules = withContextAt OffsideContext.Function indent funcTok.PositionedToken pMatchRules
                return ExprAux.ForExpr(Expr.Function(funcTok, rules))
            }

        let pFunExpr =
            // Grammar: FUN atomicPatterns RARROW typedSeqExprBlock
            let pBody = pTypedSeqExprBlock

            parser {
                let! (funTok, indent) = assertKeywordToken Token.KWFun
                // params, arrow, and body all at fun_col + 1
                let! result =
                    withContextAt
                        OffsideContext.Fun
                        (indent + 1)
                        funTok.PositionedToken
                        (parser {
                            let! pats = many1 Pat.parse

                            let! arrow =
                                recoverWithVirtualToken
                                    Token.OpArrowRight
                                    "Expected '->' after fun parameters"
                                    pArrowRight

                            let! expr = recoverExprMissing pBody
                            return Expr.Fun(funTok, pats, arrow, expr)
                        })

                return ExprAux.ForExpr result
            }

        let pTryExpr =
            let pTryMatchRules indent tryTok =
                withContextAt OffsideContext.MatchClauses indent tryTok Rules.parse
                |> recoverWith
                    StoppingTokens.afterRule
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingRule
                    (fun toks ->
                        let missing =
                            Rule.Rule(
                                Pat.Missing,
                                ValueNone,
                                virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                Expr.Missing
                            )

                        if toks.IsEmpty then
                            Rules(ValueNone, ImmutableArray.Create(missing), ImmutableArray.Empty)
                        else
                            let missingWithSkips =
                                Rule.Rule(
                                    Pat.SkipsTokens(toks),
                                    ValueNone,
                                    virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                    Expr.Missing
                                )

                            Rules(ValueNone, ImmutableArray.Create(missingWithSkips), ImmutableArray.Empty)
                    )

            // Manually manage the Try context so it stays active through with/finally parsing,
            // similar to how parseMatchBody keeps Match context active for | undentation.
            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                match assertKeywordToken Token.KWTry reader with
                | Error e -> Error e
                | Ok(tryTok, indent) ->

                    let savedState = reader.State

                    // Push Try context at try_col — stays active for with/finally/| undentation
                    let tryEntry =
                        {
                            Context = OffsideContext.Try
                            Indent = indent
                            Token = tryTok.PositionedToken
                        }

                    reader.State <- ParseState.pushOffside tryEntry reader.State

                    // Try body — SeqBlock set by refExprSeqBlock at first expression's indent.
                    // The Try context below it permits with/finally undentation to try_col.
                    // Grammar: TRY typedSeqExprBlock WITH|FINALLY ...
                    match recoverExprMissing refTypedSeqExprBlock.Parser reader with
                    | Error e ->
                        reader.State <- savedState
                        Error e
                    | Ok tryExpr ->

                        // with/finally permitted at try_col via contextPermitsToken (Try context still active)
                        let pWith' =
                            parser {
                                let! withTok = pWith
                                // | permitted at try_col via contextPermitsToken (Try context still active)
                                // MatchClauses at try_col so clause bodies can be at try_col
                                let! rules = pTryMatchRules indent tryTok.PositionedToken
                                return Expr.TryWith(tryTok, tryExpr, withTok, rules)
                            }

                        let pFinally' =
                            parser {
                                let! finTok = pFinally
                                // Grammar: FINALLY typedSeqExprBlock
                                let! finExpr = refTypedSeqExprBlock.Parser
                                return Expr.TryFinally(tryTok, tryExpr, finTok, finExpr)
                            }

                        match
                            dispatchNextSyntaxTokenL
                                [ Token.KWWith, pWith'; Token.KWFinally, pFinally' ]
                                "Expected 'with' or 'finally'"
                                reader
                        with
                        | Error e ->
                            reader.State <- savedState
                            Error e
                        | Ok result ->
                            reader.State <- ParseState.popOffside tryEntry reader.State
                            Ok(ExprAux.ForExpr result)

        let pWhileExpr =
            parser {
                let! (whileTok, indent) = assertKeywordToken Token.KWWhile
                // Condition at while_col + 1 (uses refExprNoSeq to prevent `do` from being consumed
                // as a keyword expression via sequential composition)
                let! cond =
                    recoverExprMissing (
                        withContextAt OffsideContext.While (indent + 1) whileTok.PositionedToken refExprNoSeq.Parser
                    )
                // do keyword also at while_col + 1 (NOT permitted undentation)
                let! doTok =
                    recoverWithVirtualToken
                        Token.KWDo
                        "Expected 'do' after while condition"
                        (withContextAt OffsideContext.While (indent + 1) whileTok.PositionedToken pDo)
                // Body at while_col
                // Grammar: WHILE _ DO typedSeqExprBlock
                let! body =
                    recoverExprMissing (
                        withContextAt OffsideContext.Do indent whileTok.PositionedToken refTypedSeqExprBlock.Parser
                    )

                let! doneTok = pDoneVirt
                return ExprAux.ForExpr(Expr.While(whileTok, cond, doTok, body, doneTok))
            }

        let pForExpr =
            let pForHeader =
                choiceL
                    [
                        // for ident = start to end do (also accepts _ as wildcard)
                        parser {
                            let! ident = pIdentTok <|> pWildcard
                            let! eq = pEquals
                            let! startExpr = refExprGuard.Parser
                            let! toTok = pToOrDownTo
                            let! endExpr = refExprGuard.Parser
                            let! doTok = pDo <|> pArrowRight

                            return
                                fun forTok body doneTok ->
                                    Expr.ForTo(forTok, ident, eq, startExpr, toTok, endExpr, doTok, body, doneTok)
                        }
                        // for pat in expr do
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pIn
                            let! iterable = refExprGuard.Parser
                            let! doTok = pDo <|> pArrowRight

                            return
                                fun forTok body doneTok ->
                                    Expr.ForIn(forTok, pat, inTok, iterable, doTok, body, doneTok)
                        }
                    ]
                    "Expected 'for-to' or 'for-in' loop header"

            let assertFor reader =
                // We throw here as `pForExpr` should only be called if we've already peeked and confirmed we have a 'for' token.
                match peekNextSyntaxToken reader with
                | Ok t when t.Token = Token.KWFor ->
                    (consumePeeked t
                     |>> fun forTok ->
                         let state = reader.State

                         let indent =
                             match forTok.Index with
                             | TokenIndex.Regular iT -> ParseState.getIndent state iT
                             | TokenIndex.Virtual ->
                                 invalidOp "Virtual tokens should not be used for 'for' keyword in pForBody"

                         struct (forTok, indent))
                        reader

                | Ok t -> invalidOp $"Expected 'for' keyword. Got {t.Token} at position {t.StartIndex}"
                | Error e -> invalidOp $"Expected 'for' keyword. Failed to peek next token: {e}"

            parser {
                let! (forTok, indent) = assertFor
                // The pattern and iterator (or variable and range) must be indented past the 'for' keyword
                // After do, the body can be at the same indent as 'for' or indented further
                let headerMinIndent = indent + 1
                let bodyMinIndent = indent
                let! forBuilder = withContextAt OffsideContext.For headerMinIndent forTok.PositionedToken pForHeader
                // Grammar: FOR _ IN _ DO typedSeqExprBlock (and FOR _ = _ TO _ DO typedSeqExprBlock)
                let! body =
                    withContextAt OffsideContext.Do bodyMinIndent forTok.PositionedToken refTypedSeqExprBlock.Parser

                let! doneTok = pDoneVirt
                return ExprAux.ForExpr(forBuilder forTok body doneTok)
            }

        let pLetOrUseIn letIndent (reader: Reader<PositionedToken, ParseState, _>) =
            match peekNextSyntaxToken reader with
            | Ok t when t.Token = Token.KWIn -> consumePeeked t reader
            | Ok t ->
                // Not 'in', but maybe we emit a virtual 'in' for offside rule in let bindings without 'in'.
                // We can only emit VirtualIn if the next token is indented less than or equal to the 'let' and we're at the correct context indent.
                let indent = ParseState.getIndent reader.State (reader.Index * 1<token>)
                let state = reader.State

                let atContextIndent =
                    match state.Context with
                    // Inside paren-like contexts (Indent = 0), the offside rule is relaxed.
                    // Always emit virtual 'in' since the paren delimiter governs scope, not indentation.
                    | { Indent = 0 } :: _ -> true
                    | { Indent = ctxIndent } :: _ -> indent = ctxIndent || indent = letIndent
                    | [] -> indent = 0 || indent = letIndent

                if atContextIndent then
                    Ok(virtualToken (PositionedToken.Create(Token.VirtualIn, t.StartIndex)))
                else
                    // TODO: Consider parser recovery here instead of hard failure,
                    // e.g. skip tokens until we find one that is at the correct indent
                    // or a valid separator (e.g. semicolon, newline with correct indent, or closing delimiter).
                    // Maybe still emit a virtual 'in' as well as a diagnostic error for the missing 'in' to allow parsing to continue and produce a more complete AST with error nodes.
                    fail errExpectedInAtLetIndent reader
            | Error _ ->
                // Peek failed (e.g., EOF offside or end of input).
                // Emit a virtual 'in' — this handles `use _ = expr` at the end of a scope.
                let pos = reader.Index * 1<token>

                let startIndex =
                    if pos < reader.State.Lexed.Tokens.Length * 1<token> then
                        reader.State.Lexed.Tokens[pos].StartIndex
                    else
                        0

                Ok(virtualToken (PositionedToken.Create(Token.VirtualIn, startIndex)))

        // Shared definition parser for let/use bindings:
        // Parses [rec] binding [and binding ...] inside a Let offside context.
        // 'use rec' and 'use ... and ...' are syntactically accepted here; semantic
        // validation is responsible for rejecting those forms with diagnostics.
        let pLetOrUseDefn indent token =
            withContextAt
                OffsideContext.Let
                indent
                token
                (parser {
                    let! recTok = opt pRec
                    let! bindings, ands = Binding.parseSepByAnd1 ValueNone
                    return struct (recTok, bindings, ands)
                })

        /// Build nested `Expr.LetOrUse` from an accumulated list of bindings (innermost last)
        /// and a body expression. The fold runs backwards so that the first binding wraps the outermost scope.
        let buildNestedLetOrUse
            (bindings: ResizeArray<struct (SyntaxToken * SyntaxToken voption * _ * _ * SyntaxToken)>)
            (body: Expr<SyntaxToken> voption)
            =
            let mutable result = body

            for i in bindings.Count - 1 .. -1 .. 0 do
                let struct (kwTok, recTok, defs, ands, inTok) = bindings[i]

                let kw =
                    match kwTok.Token with
                    | Token.KWLet -> LetOrUseKeyword.Let kwTok
                    | Token.KWUse -> LetOrUseKeyword.Use kwTok
                    | Token.KWLetBang -> LetOrUseKeyword.LetBang kwTok
                    | Token.KWUseBang -> LetOrUseKeyword.UseBang kwTok
                    | t -> failwith $"Unexpected keyword token for let/use body {t}"

                result <- ValueSome(Expr.LetOrUse(kw, recTok, defs, ands, ValueSome inTok, result))

            match result with
            | ValueSome expr -> expr
            | ValueNone -> Expr.Missing

        /// Single body parser used by both `let/let!` and `use/use!`.
        /// The keyword token has been peeked but NOT consumed by lhsParser.
        ///
        /// Sequential let/use bindings (e.g. `let x = 1\nlet y = 2\nexpr`) are collected
        /// iteratively to avoid deep recursion — each `let` in a sequence would otherwise
        /// add ~30 stack frames via the Pratt parser → SeqBlock → pLetOrUseBody chain,
        /// overflowing the stack on moderately nested code.
        let pLetOrUseBody =
            fun (reader: Reader<PositionedToken, ParseState, _>) ->
                let bindings = ResizeArray()
                let mutable cont = true
                let mutable error = Unchecked.defaultof<ParseResult<ExprAux, _, _>>

                while cont do
                    match peekNextSyntaxToken reader with
                    | Error e ->
                        if bindings.Count = 0 then
                            error <- Error e
                        // If we already have bindings, stop iterating; the body parse will report the error
                        cont <- false
                    | Ok peeked ->
                        match peeked.Token with
                        | Token.KWLet
                        | Token.KWLetBang
                        | Token.KWUse
                        | Token.KWUseBang ->
                            match consumePeeked peeked reader with
                            | Error e ->
                                error <- Error e
                                cont <- false
                            | Ok kwTok ->
                                let indent =
                                    match kwTok.Index with
                                    | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                                    | TokenIndex.Virtual -> 0

                                match pLetOrUseDefn indent kwTok.PositionedToken reader with
                                | Error e ->
                                    error <- Error e
                                    cont <- false
                                | Ok(recTok, defs, ands) ->
                                    match pLetOrUseIn indent reader with
                                    | Error e ->
                                        error <- Error e
                                        cont <- false
                                    | Ok inTok -> bindings.Add(struct (kwTok, recTok, defs, ands, inTok))
                        | _ ->
                            if bindings.Count = 0 then
                                error <-
                                    Parsers.fail (Message $"Expected let/use keyword but got {peeked.Token}") reader

                            cont <- false

                if bindings.Count = 0 then
                    error
                else
                    // Committed after consuming let/use bindings — recover body expression if it fails
                    // Grammar: LET bindings IN typedSeqExprBlock
                    let body =
                        match refTypedSeqExprBlock.Parser reader with
                        | Ok body -> ValueSome body
                        | Error err ->
                            let struct (lastKw, _, _, _, _) = bindings[bindings.Count - 1]

                            match lastKw.Token with
                            | Token.KWUse
                            | Token.KWUseBang ->
                                // use/use! with no body is valid F# (e.g. `use _ = expr` at end of scope)
                                ValueNone
                            | _ ->
                                reader.State <-
                                    ParseState.addErrorDiagnosticWithError
                                        DiagnosticCode.MissingExpression
                                        (match peekNextSyntaxToken reader with
                                         | Ok tok -> tok.PositionedToken
                                         | Error _ -> PositionedToken.Create(Token.EOF, 0))
                                        err
                                        reader.State

                                ValueSome Expr.Missing

                    let result = buildNestedLetOrUse bindings body
                    preturn (ExprAux.KeywordExpr(fun _opTok -> result)) reader

        let pYieldReturnDoBody =
            // Grammar: YIELD|RETURN typedSeqExprBlock (and DO typedSeqExprBlock in CE context)
            let pBody =
                recoverWith
                    StoppingTokens.afterExpr
                    DiagnosticSeverity.Error
                    DiagnosticCode.MissingExpression
                    (fun toks ->
                        if toks.IsEmpty then
                            Expr.Missing
                        else
                            Expr.SkipsTokens(toks)
                    )
                    refTypedSeqExprBlock.Parser

            parser {
                // Committed after do/return/yield keyword
                let! expr = pBody

                return
                    ExprAux.KeywordExpr(fun kwTok ->
                        let kw =
                            match kwTok.Token with
                            | Token.KWDo -> ControlFlowKeyword.Do kwTok
                            | Token.KWDoBang -> ControlFlowKeyword.DoBang kwTok
                            | Token.KWReturn -> ControlFlowKeyword.Return kwTok
                            | Token.KWReturnBang -> ControlFlowKeyword.ReturnBang kwTok
                            | Token.KWYield -> ControlFlowKeyword.Yield kwTok
                            | Token.KWYieldBang -> ControlFlowKeyword.YieldBang kwTok
                            | t -> failwith $"Unexpected keyword token for yield/return body {t}"

                        Expr.ControlFlow(kw, expr)
                    )
            }

    /// High-precedence-application / type-application lookaheads — the `f(x)`, `f[i]`,
    /// and `f<T>` forms that bind tighter than whitespace application. Lifted from
    /// ExprOperatorParser to avoid re-evaluating their bodies on each generic
    /// instantiation of an enclosing class member.
    module private HighPrec =
        // Shared once-allocated ErrorType for pTypeApplication's failure paths. Kept as a
        // separate module-level binding (with explicit type) so the value-restriction rule
        // can't generalize pTypeApplication into a thunk that re-allocates the Message.
        let private pTypeApplicationErr: ErrorType<PositionedToken, ParseState> =
            Message "Expected '<' for type application"

        /// Direct-style type-application lookahead. Moved to module scope so the binding
        /// is initialized once as a static field (rather than re-evaluated for each generic
        /// instantiation of an enclosing class member).
        let pTypeApplication: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>> =
            fun reader ->
                if not (isPrevTokenSyntax reader) then
                    fail pTypeApplicationErr reader
                else
                    match reader.Peek() with
                    | ValueSome t when t.Token = Token.OpLessThan ->
                        let st = syntaxToken t reader.Index

                        if tokenStringIs "<" st reader.State then
                            let typeAngle =
                                { st with
                                    PositionedToken = PositionedToken.Create(Token.VirtualTyApp, t.StartIndex)
                                }

                            preturn typeAngle reader
                        else
                            fail pTypeApplicationErr reader
                    | _ -> fail pTypeApplicationErr reader

        let pTypeAppRhs =
            parser {
                let! lAngle = nextSyntaxTokenIsLMsg Token.OpLessThan "Expected '<' for type application"
                let! types, commas = sepBy Type.parse pComma

                let! state = getUserState

                let! rAngle = pCloseTypeParams

                return ExprAux.TypeApp(lAngle, types, commas, rAngle)
            }

        let pHighPrec token char =
            // Use satisfy to check the raw token is '(' AND the previous raw token is not trivia,
            // confirming true adjacency (e.g. f(x), not f (x) or a newline-separated expression).
            // Hoist the error message into a single ErrorType.Message value so the failure
            // paths below don't allocate per invocation.
            let errMsg = Message $"Expected '{char}' for high precedence"

            let pSatisfy =
                fun (reader: Reader<PositionedToken, ParseState, _>) ->
                    match reader.Peek() with
                    | ValueNone -> fail EndOfInput reader
                    | ValueSome t ->
                        if t.Token = token then
                            reader.Skip()
                            preturn t reader
                        else
                            fail errMsg reader

            let pFail (reader: Reader<PositionedToken, ParseState, _>) = fail errMsg reader

            parser {
                let! canBeHighPrec = isPrevTokenSyntax >> Ok

                if canBeHighPrec then

                    return! pSatisfy
                else
                    return! pFail
            }

        let pHighPrecLParen = pHighPrec Token.KWLParen '('

        let pHighPrecLBracket = pHighPrec Token.KWLBracket '['

        let peekHighPrecApp =
            lookAhead (choiceL [ pHighPrecLParen; pHighPrecLBracket ] "( or [ for high-precedence")
            |>> fun (pt: PositionedToken) ->
                match pt.Token with
                | Token.KWLParen -> virtualToken (PositionedToken.Create(Token.OpHighPrecedenceApp, pt.StartIndex))
                | Token.KWLBracket ->
                    virtualToken (PositionedToken.Create(Token.OpHighPrecedenceIndexApp, pt.StartIndex))
                | _ -> failwith "Unexpected token in peekHighPrecApp"

        let parseHighPrecIndex: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>> =
            // Parse [ expr ] for F# 6+ dot-less indexing arr[i] — no whitespace before '['
            parser {
                let! pos = getPosition
                let! lBracket = pHighPrecLBracket
                let lBracket = syntaxToken lBracket pos.Index
                // Special case: f[] with empty brackets → treat as f [] (application to empty list)
                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWRBracket ->
                    let! rBracket = consumePeeked t
                    return ExprAux.HighPrecApp(lBracket, Expr.EmptyBlock(ParenKind.List lBracket, rBracket), rBracket)
                | _ ->
                    let! argExpr = refExpr.Parser
                    let! rBracket = pRBracket
                    return ExprAux.HighPrecIndex(lBracket, argExpr, rBracket)
            }

        let parseHighPrecApp: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>> =
            // Parse ( expr ) for high-precedence application f(x, y) — no whitespace before '('
            parser {
                let! pos = getPosition
                let! lParen = pHighPrecLParen
                let lParen = syntaxToken lParen pos.Index

                // Handle f() — empty argument list
                match! peekNextSyntaxToken with
                | t when t.Token = Token.KWRParen ->
                    let! rParen = consumePeeked t
                    return ExprAux.HighPrecApp(lParen, Expr.EmptyBlock(ParenKind.Paren lParen, rParen), rParen)
                | _ ->
                    let! argExpr = refExpr.Parser

                    let! argExpr =
                        parser {
                            match! peekNextSyntaxToken with
                            | t when t.Token = Token.OpColon ->
                                let! colon = consumePeeked t
                                let! typ = Type.parse
                                return Expr.TypeAnnotation(argExpr, colon, typ)
                            | _ -> return argExpr
                        }

                    let! rParen = pRParen
                    return ExprAux.HighPrecApp(lParen, argExpr, rParen)
            }

    module private Application =
        let parse =
            /// Checks whether the operator at the given raw token index is an ADJACENT_PREFIX_OP:
            /// adjacent to the following token (no whitespace after) but with whitespace before.
            /// See F# spec 3.8.1 "Post-filtering of Adjacent Prefix Tokens".
            let isAdjacentPrefixOp (state: ParseState) (rawIndex: int<token>) =
                let tokens = state.Lexed.Tokens

                // Check right-adjacency: next raw token must NOT be trivia (whitespace/newline/comment)
                let rightAdjacent =
                    rawIndex + 1<token> < tokens.Length * 1<token>
                    && not (ParseState.isTriviaToken state tokens[rawIndex + 1<token>])

                // Check left-separation: previous raw token must be trivia (whitespace/newline/comment)
                // or the operator must be at the start of input
                let leftSeparated =
                    rawIndex = 0<token>
                    || (rawIndex > 0<token>
                        && ParseState.isTriviaToken state tokens[rawIndex - 1<token>])

                rightAdjacent && leftSeparated

            let isAtomicExprToken (state: ParseState) (t: SyntaxToken) =
                match t.Token with
                | Token.Identifier
                | Token.BacktickedIdentifier
                | Token.UnterminatedBacktickedIdentifier
                | Token.KWLParen
                | Token.KWLBracket
                | Token.KWLArrayBracket
                | Token.KWBegin
                | Token.KWStruct
                | Token.KWLBrace
                | Token.KWLBraceBar
                | Token.KWLHashParen
                | Token.OpQuotationTypedLeft
                | Token.OpQuotationUntypedLeft
                | Token.OpDereference
                | Token.InterpolatedStringOpen
                | Token.VerbatimInterpolatedStringOpen
                | Token.Interpolated3StringOpen
                | Token.StringOpen
                | Token.VerbatimStringOpen
                | Token.String3Open
                | Token.Wildcard -> true
                | _ ->
                    if Constant.isLiteralToken t.Token then
                        true
                    else
                        match OperatorInfo.TryCreate t.PositionedToken with
                        | ValueSome opInfo when opInfo.CanBePrefix ->
                            // Prefix-only operators (e.g. !, ~~~) always start application args.
                            // Dual-use operators (e.g. -, +) only start application args when they
                            // are ADJACENT_PREFIX_OPs: adjacent to the right token with whitespace before.
                            // See F# spec 3.8.1 "Post-filtering of Adjacent Prefix Tokens".
                            opInfo.Precedence = PrecedenceLevel.Prefix
                            || (
                                match t.Index with
                                | TokenIndex.Regular rawIndex -> isAdjacentPrefixOp state rawIndex
                                | TokenIndex.Virtual -> false
                            )
                        | _ -> false

            let failApp = fail (Message "Expected expression after application")
            // Application is *juxtaposition* of two expressions with trivia in between, e.g. `f x` or `f (g y)` or `f(*comment*)x`.
            parser {
                match! peekNextSyntaxToken with
                | t when t.Token = Token.EOF -> return! failApp
                | t ->
                    let! indent = currentIndent
                    let! state = getUserState

                    if not (isAtomicExprToken state t) then
                        return! failApp
                    else
                        let atAppIndent =
                            match state.Context with
                            | { Indent = ctxIndent } :: _ ->
                                if indent > ctxIndent then
                                    true
                                elif indent < ctxIndent then
                                    // Token is undented from the SeqBlock but might still be a valid
                                    // application argument inside a paren-like context (rule 15.1.10.4).
                                    // Walk past SeqBlock+Paren pairs to find the enclosing non-paren
                                    // context and check indent > that context's indent (strictly greater
                                    // to preserve sequential-expression semantics at the same indent).
                                    match state.Context with
                                    | { Context = OffsideContext.SeqBlock } :: ctx :: deeper when
                                        ctx.Context = OffsideContext.Paren
                                        || ctx.Context = OffsideContext.Bracket
                                        || ctx.Context = OffsideContext.BracketBar
                                        || ctx.Context = OffsideContext.BraceBar
                                        || ctx.Context = OffsideContext.Brace
                                        || ctx.Context = OffsideContext.Begin
                                        ->
                                        let rec walkPastParens stack =
                                            match stack with
                                            | [] -> indent > 0
                                            | (ctx: Offside) :: rest ->
                                                match ctx.Context with
                                                | OffsideContext.SeqBlock
                                                | OffsideContext.Paren
                                                | OffsideContext.Bracket
                                                | OffsideContext.BracketBar
                                                | OffsideContext.BraceBar
                                                | OffsideContext.Brace
                                                | OffsideContext.Begin -> walkPastParens rest
                                                | _ -> indent > ctx.Indent

                                        walkPastParens deeper
                                    | _ -> false
                                else
                                    false // indent = ctxIndent → sequential expression, not application
                            | [] -> indent > 0

                        if atAppIndent then
                            return virtualToken (PositionedToken.Create(Token.VirtualApp, t.StartIndex))
                        else
                            return! failApp
            }

    /// Parses a subsequent tuple separator `,` for the Pratt parser's InfixNary
    /// loop. Unlike the first comma (which is identified by the full RHS
    /// operator dispatcher), this parser only needs to recognise another `,`.
    let private pTupleComma: Parser<SyntaxToken, _, _, _> =
        nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.OpComma) "','"

    /// Matches a real `;` or emits a layout-sensitive `VirtualSep` when the
    /// next token can start an expression at the enclosing context's indent.
    /// Used as the subsequent-separator parser for both the expression-level
    /// RHS dispatcher and the InfixNary nary-loop for sequences.
    let private pSepVirt =
        let failSep =
            fail (Message "Expected ';' or newline at the same indent for expression sequencing")

        parser {
            match! peekNextSyntaxToken with
            | t when t.Token = Token.EOF -> return! failSep
            | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
            | t ->
                // Not a semicolon, but maybe we emit a virtual semicolon for newline-separated expressions.
                // Only emit VirtualSep when the next token can actually start an expression.
                // Tokens that cannot start expressions (closing delimiters, block-continuation keywords
                // like `with`/`finally`/`then`/`else`, pure infix operators, etc.) must NOT trigger
                // VirtualSep — doing so causes infinite loops in the Pratt parser's InfixNary handler
                // because the zero-width virtual token never advances the reader.
                if TokenInfo.canStartExpression t.Token then
                    let! indent = currentIndent
                    let! state = getUserState

                    let atContextIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                        | [] -> indent = 0

                    if atContextIndent then
                        return virtualToken (PositionedToken.Create(Token.VirtualSep, t.StartIndex))
                    else
                        return! failSep
                else
                    return! failSep
        }

    type ExprOperatorParser() =
        let printOpInfo (op: OperatorInfo) =
            printfn
                $"Operator: {op.PositionedToken}({op.StartIndex}), Precedence: {op.Precedence}, Associativity: %A{op.Associativity}"

        let pIdentAfterDot = nextSyntaxIdentifierLMsg "Expected identifier after '.'"

        let parseDotRhs: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<_>> =
            // Note: cannot use module-level pIdent here as that maps to Expr.Ident
            choiceL
                [
                    // .[expr] — indexed access (e.g. xs.[0])
                    parser {
                        let! lBracket = pLBracket
                        let! indexExpr = refExpr.Parser
                        let! rBracket = pRBracket
                        return ExprAux.DotIndex(lBracket, indexExpr, rBracket)
                    }
                    // .(op) — qualified operator access (e.g. Module.(+))
                    parser {
                        let! lParen = pLParen
                        let! op = OpName.parse
                        let! rParen = pRParen
                        return ExprAux.DotParenOp(IdentOrOp.ParenOp(lParen, op, rParen))
                    }
                    // .ident — field/member access (e.g. x.Name)
                    parser {
                        let! ident = pIdentAfterDot
                        return ExprAux.Ident ident
                    }
                    // .N — positional DU field access (e.g. cons.( :: ).1)
                    parser {
                        let! intTok =
                            nextSyntaxTokenSatisfiesLMsg (fun t -> t.Token = Token.NumInt32) "integer field index"

                        return ExprAux.Ident intTok
                    }
                ]
                "Dot RHS (identifier or index)"

        let parseRangeRhs reader =
            match peekNextSyntaxToken reader with
            | Ok t ->
                match t.Token with
                | Token.KWRBracket
                | Token.OpComma
                | Token.EOF ->
                    // This is likely a slice like arr.[1..] or a range with an omitted end like [1..]
                    // We return an Aux to indicate this and handle it in the completion function.
                    Ok ExprAux.SliceFrom
                | _ ->
                    // Otherwise, we expect a full range expression on the right
                    (refExprRange.Parser |>> ExprAux.Range) reader
            | Error e ->
                // If we fail to peek, treat it as an omitted end (e.g. [1..EOF)
                Ok ExprAux.SliceFrom

        let parseTypeCastRhs = Type.parse |>> ExprAux.TypeCast


        let rhsOperators
            : (SyntaxToken
                  -> RHSOperator<
                      SyntaxToken,
                      ExprAux,
                      Expr<SyntaxToken>,
                      PositionedToken,
                      ParseState,
                      ReadableImmutableArray<PositionedToken>
                   >) array =
            Array.init
                30
                (fun i ->
                    let pl = pl i
                    let power = BindingPower.fromLevel i

                    match pl with
                    // Pattern only keywords
                    // | PrecedenceLevel.As -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    // | PrecedenceLevel.When -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.Pipe -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Semicolon ->
                        // (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeSemicolon))
                        (fun op -> InfixNary(op, pSepVirt, power, true, Complete.sequence))
                    // | PrecedenceLevel.RArrow -> `->` is listed in the operator precedence table but never used as an operator in expressions
                    //     (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    // LHS keywords
                    // | PrecedenceLevel.Let -> (fun op -> InfixNonAssociative(op, preturn op, power, Complete.infix))
                    // | PrecedenceLevel.Function -> (fun op -> InfixNonAssociative(op, preturn op, power, Complete.infix))
                    // | PrecedenceLevel.If -> (fun op -> InfixNonAssociative(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Assignment ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.assignment))
                    | PrecedenceLevel.Comma -> (fun op -> InfixNary(op, pTupleComma, power, false, Complete.tuple))
                    | PrecedenceLevel.Range ->
                        (fun op ->
                            // The binary '..' operator. Non-associative. The ternary '.. ..' form or the postfix form are special grammar rules.
                            // Parse as Left-associative and disambiguate if we see another '..' at the same precedence level on the right.
                            InfixMapped(op, preturn op, power, parseRangeRhs, Complete.rangeOrSliceFrom)
                        )
                    | PrecedenceLevel.LogicalOr -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.LogicalAnd -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Cast ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, Complete.typeCast))
                    | PrecedenceLevel.ComparisonAndBitwise ->
                        (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Append ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.Cons ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.TypeTest ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, Complete.typeCast))
                    | PrecedenceLevel.InfixAdd -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.InfixMultiply -> (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                    | PrecedenceLevel.Power ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, Complete.infix))
                    | PrecedenceLevel.Application ->
                        // (fun op -> InfixLeft(op, preturn op, power, Complete.infix))
                        (fun op -> InfixNary(op, Application.parse, power, false, Complete.app))
                    // | PrecedenceLevel.PatternMatchBar -> Pattern only operator
                    // | PrecedenceLevel.Prefix -> LHS only
                    | PrecedenceLevel.Dot -> (fun op -> InfixMapped(op, preturn op, power, parseDotRhs, Complete.dot))
                    | PrecedenceLevel.HighIndexApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.parseHighPrecIndex, Complete.highPrec))
                    | PrecedenceLevel.HighApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.parseHighPrecApp, Complete.highPrec))
                    | PrecedenceLevel.HighTypeApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, HighPrec.pTypeAppRhs, Complete.typeApp))
                    // | PrecedenceLevel.Parens -> LHS only
                    | pl ->
                        (fun op ->
                            invalidOp
                                $"No operator handler for precedence level {pl}. Got op {op.Token} at position {op.StartIndex}"
                        )
                )

        let getRhsOperatorHandler (opInfo: OperatorInfo) token =
            // Note: Precedence gets bit-packed into the token and converted directly
            // to RHS handler index for efficiency.
            let pl = opInfo.Precedence
            let handler = rhsOperators[LanguagePrimitives.EnumToValue pl]
            handler token

        let rhsParser =
            let handleToken token =
                match OperatorInfo.TryCreate token.PositionedToken with
                | ValueNone -> fail errExpectedRhsOperator
                | ValueSome opInfo ->
                    match opInfo.Token with
                    | Token.OpBar -> fail errUnexpectedBarOp
                    | Token.OpDereference -> fail errUnexpectedDereferenceOp
                    | Token.OpArrowRight -> fail errUnexpectedArrowOp
                    | Token.KWLet
                    | Token.KWLetBang
                    | Token.KWUse
                    | Token.KWUseBang
                    | Token.KWMatch
                    | Token.KWMatchBang
                    | Token.KWDo
                    | Token.KWDoBang
                    | Token.KWReturn
                    | Token.KWReturnBang
                    | Token.KWYield
                    | Token.KWYieldBang
                    | Token.KWIf
                    | Token.KWFor
                    | Token.KWWhile
                    | Token.KWTry
                    | Token.KWFun
                    | Token.KWFunction -> fail errUnexpectedPrefixKeywordRhs
                    | _ when
                        // These precedence levels are LHS-only (no RHS handler registered).
                        // When encountered in RHS position, fail so the Pratt parser stops
                        // and lets function application or outer parsers handle them.
                        (match opInfo.Precedence with
                         | PrecedenceLevel.Prefix
                         | PrecedenceLevel.Function
                         | PrecedenceLevel.If
                         | PrecedenceLevel.Let
                         | PrecedenceLevel.As
                         | PrecedenceLevel.When
                         | PrecedenceLevel.PatternMatchBar
                         | PrecedenceLevel.Parens -> true
                         | _ -> false)
                        ->
                        fail errLhsOnlyOpInInfix
                    | _ -> preturn (getRhsOperatorHandler opInfo token)

            // First try type application, then whitespace application, then explicit operator.
            // Using choiceL ensures that if nextSyntaxToken consumes a token
            // (e.g. '->') but handleToken fails (no expression-level handler for Arrow),
            // XParsec backtracks the consumed token so outer parsers (e.g. Rule.parse) see it.
            choiceL
                [
                    reprocessedOperatorAfterTypeParams >>= handleToken
                    HighPrec.pTypeApplication >>= handleToken
                    HighPrec.peekHighPrecApp >>= handleToken
                    Application.parse >>= handleToken
                    pSepVirt >>= handleToken
                    nextSyntaxToken >>= handleToken
                ]
                "RHS operator"

        let peekIsSliceAll =
            parser {
                let! token = peekNextSyntaxToken
                // We are in the middle of parsing a slice like A[0..1,*], and we've just parsed the '*' token.
                // Now we peek to see if the next token is a ',' or ']' which would indicate slice-all syntax.
                // Otherwise '*' is just a regular operator for multiplication.
                match token.Token with
                | Token.KWRBracket
                | Token.EOF
                | Token.OpComma -> return ExprAux.SliceAll
                | _ -> return! fail errExpectedSliceAll
            }

        let pOperatorPrefix (token: SyntaxToken) =
            parser {
                match OperatorInfo.TryCreate(token.PositionedToken) with
                // Note: Spec shows lazy and assert keywords as having same precedence as function application,
                // so they are parsed as prefix operators with the same precedence level.
                | ValueSome opInfo when opInfo.Token = Token.KWLazy ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Lazy(kwTok, e))),
                            Complete.keyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWAssert ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Assert(kwTok, e))),
                            Complete.keyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWFixed ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Fixed(kwTok, e))),
                            Complete.keyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWUpcast ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Upcast(kwTok, e))),
                            Complete.keyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWDowncast ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Downcast(kwTok, e))),
                            Complete.keyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.OpRange ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    // A[..5]
                    return Prefix(tok, preturn tok, power, Complete.sliceTo)
                | ValueSome opInfo when opInfo.Token = Token.OpMultiply ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    // A[0..1,*]
                    return PrefixMapped(token, preturn tok, peekIsSliceAll, Complete.sliceAll)
                // & and && are address-of operators when used as prefix.
                // Must be checked before the generic CanBePrefix handler to use
                // PrecedenceLevel.Prefix instead of their infix precedence (LogicalAnd).
                | ValueSome opInfo when opInfo.Token = Token.OpAmp || opInfo.Token = Token.OpAmpAmp ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int PrecedenceLevel.Prefix)
                    return Prefix(tok, preturn tok, power, Complete.prefix)
                | ValueSome opInfo when opInfo.CanBePrefix ->
                    let! tok = consumePeeked token
                    // TODO: PrecedenceLevel.Prefix has higher precedence than function application, which means `-f x` parses as `(-f) x` instead of `-(f x)`.
                    // The F# parser treats `-f x` as `-(f x)`, so we add 1 to the precedence to allow prefix operators to bind tighter than infix operators of the same precedence, while still being below application.
                    // This doesn't seem right. Prefix precedence may need to be revised as a whole, but for now this is a pragmatic solution to allow existing code to parse without ambiguity errors.
                    // Dual-use operators (e.g., `-`, `+`) use their infix precedence + 1 for prefix.
                    // This avoids "Ambiguous operator associativity" when the same operator follows
                    // (e.g., `-x - 1` must parse as `(-x) - 1`), while staying below Application
                    // so `-f x` still parses as `-(f x)`.
                    let power = BindingPower.fromLevel (int opInfo.Precedence) + 1uy<bp>
                    return Prefix(tok, preturn tok, power, Complete.prefix)
                | _ -> return! fail errNotPrefixOp
            }

        // Keyword-prefix dispatch table. Flattened to one entry per token for a simple
        // linear scan (same pattern as `dispatchNextSyntaxTokenFallback`). Most entries
        // do NOT consume the keyword here — the body parser peeks the keyword to establish
        // offside indent, then consumes. The `do/do!/return/return!/yield/yield!` forms
        // consume up front because their body expects to start past the keyword.
        let kwPrefixNoConsume body completer (token: SyntaxToken) =
            preturn (PrefixMapped(token, preturn token, body, completer))

        let kwPrefixConsume body completer (token: SyntaxToken) =
            parser {
                let! tok = consumePeeked token
                return PrefixMapped(tok, preturn tok, body, completer)
            }

        let kwPrefixRoutes: struct (Token * (SyntaxToken -> Parser<_, _, _, _>))[] =
            [|
                struct (Token.KWLet, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.keyword)
                struct (Token.KWMatch, kwPrefixNoConsume KWBody.pMatchExpr Complete.forE)
                struct (Token.KWIf, kwPrefixNoConsume KWBody.pIfExpr Complete.forE)
                struct (Token.KWFun, kwPrefixNoConsume KWBody.pFunExpr Complete.forE)
                struct (Token.KWDo, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWFor, kwPrefixNoConsume KWBody.pForExpr Complete.forE)
                struct (Token.KWYieldBang, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWYield, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWReturn, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWTry, kwPrefixNoConsume KWBody.pTryExpr Complete.forE)
                struct (Token.KWFunction, kwPrefixNoConsume KWBody.pFunctionExpr Complete.forE)
                struct (Token.KWUse, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.keyword)
                struct (Token.KWLetBang, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.keyword)
                struct (Token.KWDoBang, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWWhile, kwPrefixNoConsume KWBody.pWhileExpr Complete.forE)
                struct (Token.KWReturnBang, kwPrefixConsume KWBody.pYieldReturnDoBody Complete.keyword)
                struct (Token.KWMatchBang, kwPrefixNoConsume KWBody.pMatchExpr Complete.forE)
                struct (Token.KWUseBang, kwPrefixNoConsume KWBody.pLetOrUseBody Complete.keyword)
            |]

        let lhsParser =
            parser {
                let! token = peekNextSyntaxToken

                let mutable handler = ValueNone
                let mutable i = 0

                while handler.IsNone && i < kwPrefixRoutes.Length do
                    let struct (tok, h) = kwPrefixRoutes.[i]

                    if token.Token = tok then
                        handler <- ValueSome h

                    i <- i + 1

                match handler with
                | ValueSome h -> return! h token
                | ValueNone -> return! pOperatorPrefix token
            }

        interface Operators<
            SyntaxToken,
            ExprAux,
            Expr<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser

    let pConst = Constant.parse |>> Expr.Const

    let private isStringTextFragment (tok: Token) =
        match tok with
        // Plain string fragments
        | Token.StringFragment
        | Token.EscapeSequence
        // Interpolated string fragments
        | Token.InterpolatedStringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.Interpolated3StringFragment
        | Token.EscapeLBrace
        | Token.EscapeRBrace
        // Shared fragment tokens
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> true
        | _ -> false

    let private isStringInvalidText (tok: Token) =
        match tok with
        | Token.UnmatchedInterpolatedRBrace
        | Token.InvalidFormatPlaceholder
        | Token.InvalidFormatPercents
        | Token.TooManyLBracesInInterpolated3String
        | Token.TooManyRBracesInInterpolated3String -> true
        | _ -> false

    let private isStringClose (tok: Token) =
        match tok with
        // Plain string close
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal
        // Interpolated string close
        | Token.InterpolatedStringClose
        | Token.VerbatimInterpolatedStringClose
        | Token.Interpolated3StringClose
        | Token.UnterminatedInterpolatedString -> true
        | _ -> false

    let private stringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | Token.InterpolatedStringOpen -> StringKind.InterpolatedString t
        | Token.VerbatimInterpolatedStringOpen -> StringKind.VerbatimInterpolatedString t
        | Token.Interpolated3StringOpen -> StringKind.Interpolated3String t
        | _ -> invalidOp $"Not a string open token: {t.Token}"

    let private isStringOpen (tok: Token) =
        match tok with
        | Token.StringOpen
        | Token.VerbatimStringOpen
        | Token.String3Open
        | Token.InterpolatedStringOpen
        | Token.VerbatimInterpolatedStringOpen
        | Token.Interpolated3StringOpen -> true
        | _ -> false

    let pString =
        let rec loop (isInterpolated: bool) (parts: ResizeArray<StringPart<SyntaxToken>>) reader =
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            // Text fragments (plain and interpolated)
            | Ok t when isStringTextFragment t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fragment ->
                    parts.Add(StringPart.Text fragment)
                    loop isInterpolated parts reader
            // Escape sequences (plain strings only)
            | Ok t when t.Token = Token.EscapeSequence ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok esc ->
                    parts.Add(StringPart.EscapeSequence esc)
                    loop isInterpolated parts reader
            // Format specifier (%d etc.) — may be followed by expression hole in interpolated strings
            | Ok t when t.Token = Token.FormatPlaceholder ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok formatSpec ->
                    match peekNextSyntaxToken reader with
                    | Ok next when next.Token = Token.InterpolatedExpressionOpen ->
                        match consumePeeked next reader with
                        | Error e -> Error e
                        | Ok lBrace ->
                            match refExpr.Parser reader with
                            | Error e -> Error e
                            | Ok expr ->
                                let formatClause =
                                    match peekNextSyntaxToken reader with
                                    | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                        consumePeeked fc reader |> ignore
                                        ValueSome fc
                                    | _ -> ValueNone

                                match nextSyntaxTokenIsLMsg Token.InterpolatedExpressionClose "Expected }" reader with
                                | Error e -> Error e
                                | Ok rBrace ->
                                    parts.Add(StringPart.Expr(ValueSome formatSpec, lBrace, expr, formatClause, rBrace))

                                    loop isInterpolated parts reader
                    | _ ->
                        if isInterpolated then
                            parts.Add(StringPart.OrphanFormatSpecifier formatSpec)
                        else
                            parts.Add(StringPart.FormatSpecifier formatSpec)

                        loop isInterpolated parts reader
            // Expression hole without format specifier (interpolated strings only)
            | Ok t when t.Token = Token.InterpolatedExpressionOpen ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok lBrace ->
                    match refExpr.Parser reader with
                    | Error e -> Error e
                    | Ok expr ->
                        let formatClause =
                            match peekNextSyntaxToken reader with
                            | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                consumePeeked fc reader |> ignore
                                ValueSome fc
                            | _ -> ValueNone

                        match nextSyntaxTokenIsLMsg Token.InterpolatedExpressionClose "Expected }" reader with
                        | Error e -> Error e
                        | Ok rBrace ->
                            parts.Add(StringPart.Expr(ValueNone, lBrace, expr, formatClause, rBrace))
                            loop isInterpolated parts reader
            // Invalid text tokens (interpolated strings only)
            | Ok t when isStringInvalidText t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok invalid ->
                    parts.Add(StringPart.InvalidText invalid)
                    loop isInterpolated parts reader
            | Ok _ -> Ok parts

        parser {
            let! opening = nextSyntaxTokenSatisfiesLMsg (fun t -> isStringOpen t.Token) "Expected string open"

            let kind = stringKindOfToken opening

            let isInterpolated =
                match kind with
                | StringKind.InterpolatedString _
                | StringKind.VerbatimInterpolatedString _
                | StringKind.Interpolated3String _ -> true
                | _ -> false

            let! parts = loop isInterpolated (ResizeArray())

            let! closing = nextSyntaxTokenSatisfiesLMsg (fun t -> isStringClose t.Token) "Expected string close"

            return Expr.String(kind, ImmutableArray.CreateRange parts, closing)
        }

    let pIdentExpr = nextSyntaxIdentifierLMsg "Expected identifier" |>> Expr.Ident

    let private pEnclosed =
        let completeEmpty l r = Expr.EmptyBlock(l, r)
        let completeEnclosed l e r = Expr.EnclosedBlock(l, e, r)
        let skipsTokens toks = Expr.SkipsTokens(toks)
        pEnclosed completeEmpty completeEnclosed Expr.Missing skipsTokens

    let private pExprOrTypedPat =
        let pInnerExpr = refExprSeqBlock.Parser

        parser {
            let! expr = pInnerExpr

            match! peekNextSyntaxToken with
            | t when t.Token = Token.OpColon ->
                let! colon = consumePeeked t
                let! typ = Type.parse
                return Expr.TypeAnnotation(expr, colon, typ)
            | _ -> return expr
        }

    let private pParenOpExpr =
        parser {
            let! l = pLParen
            let! op = OpName.parse
            let! r = pRParen
            return Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(l, op, r)))
        }

    // SRTP trait call: (^T : (member Name : sig) expr)
    let private pStaticMemberInvocation =
        parser {
            let! lParen = pLParen

            let pBody =
                parser {
                    let! staticTypars = StaticTypars.parse
                    let! colon = pColon
                    let! lParenMember = pLParen
                    let! staticTok = opt pStatic
                    let! memberTok = pMember
                    let! membersig = Constraint.pConstraintMemberSig
                    let! rParenMember = pRParen
                    let! expr = refExprSeqBlock.Parser
                    let! rParen = pRParen

                    return
                        struct (staticTypars,
                                colon,
                                lParenMember,
                                staticTok,
                                memberTok,
                                membersig,
                                rParenMember,
                                expr,
                                rParen)
                }

            let! struct (staticTypars, colon, lParenMember, staticTok, memberTok, membersig, rParenMember, expr, rParen) =
                withContextAt OffsideContext.Paren 0 lParen.PositionedToken pBody

            return
                Expr.StaticMemberInvocation(
                    lParen,
                    staticTypars,
                    colon,
                    lParenMember,
                    staticTok,
                    memberTok,
                    membersig,
                    rParenMember,
                    expr,
                    rParen
                )
        }

    let pParen =
        choiceL
            [
                pStaticMemberInvocation
                pParenOpExpr
                pEnclosed
                    pLParen
                    Token.KWRParen
                    ParenKind.Paren
                    OffsideContext.Paren
                    DiagnosticCode.ExpectedRParen
                    pExprOrTypedPat
            ]
            "pParen"

    let pBeginEnd =
        pEnclosed
            pBegin
            Token.KWEnd
            ParenKind.BeginEnd
            OffsideContext.Begin
            DiagnosticCode.ExpectedEnd
            pTypedSeqExprBlock

    let pList =
        pEnclosed
            pLBracket
            Token.KWRBracket
            ParenKind.List
            OffsideContext.Bracket
            DiagnosticCode.ExpectedRBracket
            refExprSeqBlock.Parser

    let pArray =
        pEnclosed
            pLArrayBracket
            Token.KWRArrayBracket
            ParenKind.Array
            OffsideContext.BracketBar
            DiagnosticCode.ExpectedRArrayBracket
            refExprSeqBlock.Parser

    let pQuoteTyped =
        pEnclosed
            pQuotationTypedLeft
            Token.OpQuotationTypedRight
            ParenKind.Quoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationTypedRight
            refExprSeqBlock.Parser

    let pQuoteUntyped =
        pEnclosed
            pQuotationUntypedLeft
            Token.OpQuotationUntypedRight
            ParenKind.DoubleQuoted
            OffsideContext.Quote
            DiagnosticCode.ExpectedQuotationUntypedRight
            refExprSeqBlock.Parser

    let pStructTuple =
        parser {
            let! kw = pStruct
            let! l = pLParen
            let! e = refExpr.Parser
            let! r = pRParen

            let struct (es, commas) =
                match e with
                | Expr.Tuple(es, commas) -> struct (es, commas)
                | e -> struct (ImmutableArray.Create(e), ImmutableArray.Empty)

            return Expr.StructTuple(kw, l, es, commas, r)
        }

    let pNewExpr =
        parser {
            let! newTok = pNew
            let! typ = Type.parse
            // Constructor args are a parenthesised expression: new T(args)
            let! argExpr = pParen
            return Expr.New(newTok, typ, argExpr)
        }

    /// Consumes the closing delimiter for a record/anonymous record expression.
    /// Accepts the matching close token, or the mismatched one with a diagnostic.
    /// `expectedClose` is the correct close token; `mismatchedClose` is the wrong one.
    let private pRecordClose
        (openTok: SyntaxToken)
        (expectedClose: Token)
        (mismatchedClose: Token)
        : Parser<SyntaxToken, PositionedToken, ParseState, _> =
        fun reader ->
            match peekNextSyntaxToken reader with
            | Ok tok when tok.Token = expectedClose -> consumePeeked tok reader
            | Ok tok when tok.Token = mismatchedClose ->
                reader.State <-
                    ParseState.addErrorDiagnostic
                        (DiagnosticCode.UnclosedDelimiter(openTok, expectedClose))
                        tok.PositionedToken
                        reader.State

                consumePeeked tok reader
            | _ -> fail (Message $"Expected closing delimiter") reader

    /// Parses record field initializers with optional trailing semicolon,
    /// then consumes the close delimiter (with mismatch recovery).
    let private pRecordFieldsAndClose (openTok: SyntaxToken) (expectedClose: Token) (mismatchedClose: Token) =
        parser {
            let! fields, seps = withContext OffsideContext.SeqBlock (sepBy1 FieldInitializer.parse pRecordFieldSep)

            let! trailingSep = opt pSemi

            let seps =
                match trailingSep with
                | ValueSome sep -> seps.Add(sep)
                | ValueNone -> seps

            let! rClose = pRecordClose openTok expectedClose mismatchedClose
            return struct (fields, seps, rClose)
        }

    /// Parses the inner content of a record or anonymous record expression,
    /// given the opening token and its ParenKind.
    let private pRecordInner
        (lParen: ParenKind<SyntaxToken>)
        (openTok: SyntaxToken)
        (expectedClose: Token)
        (mismatchedClose: Token)
        =
        choiceL
            [
                // { expr with Field = val; ... } — record clone/update
                parser {
                    let! baseExpr = refExprInRecords.Parser
                    let! withTok = pWith

                    let! struct (fields, seps, rClose) = pRecordFieldsAndClose openTok expectedClose mismatchedClose

                    return Expr.RecordClone(lParen, baseExpr, withTok, fields, seps, rClose)
                }
                // { Field = val; ... } — record literal
                parser {
                    let! struct (fields, seps, rClose) = pRecordFieldsAndClose openTok expectedClose mismatchedClose

                    return Expr.Record(lParen, fields, seps, rClose)
                }
            ]
            "Record fields"

    /// Shared boilerplate: consume open delimiter, push offside context, run inner parser,
    /// pop context, and handle failure/restore.
    let private pBracedExpr
        (pOpen: Parser<SyntaxToken, PositionedToken, ParseState, _>)
        (offsideCtx: OffsideContext)
        (innerParser: SyntaxToken -> Parser<Expr<SyntaxToken>, PositionedToken, ParseState, _>)
        (label: string)
        : Parser<_, PositionedToken, ParseState, _> =
        fun reader ->
            match pOpen reader with
            | Error e -> Error e
            | Ok openTok ->
                let savedState = reader.State

                let entry: Offside =
                    {
                        Context = offsideCtx
                        Indent = 0
                        Token = openTok.PositionedToken
                    }

                reader.State <- ParseState.pushOffside entry reader.State

                match (innerParser openTok) reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside entry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message label) reader

    let pRecordOrObjectExpr: Parser<_, PositionedToken, ParseState, _> =
        pBracedExpr
            pLBrace
            OffsideContext.Brace
            (fun lBrace ->
                choiceL
                    [
                        // { CE keywords... } — computation expression body
                        // Detected by first token being a CE-specific keyword (not a valid record field name)
                        parser {
                            let! peekTok = peekNextSyntaxToken

                            match peekTok.Token with
                            | Token.KWLet
                            | Token.KWLetBang
                            | Token.KWUse
                            | Token.KWUseBang
                            | Token.KWDo
                            | Token.KWDoBang
                            | Token.KWYield
                            | Token.KWYieldBang
                            | Token.KWReturn
                            | Token.KWReturnBang
                            | Token.KWMatch
                            | Token.KWMatchBang
                            | Token.KWIf
                            | Token.KWTry
                            | Token.KWWhile
                            | Token.KWFor ->
                                let! body = refExprSeqBlock.Parser

                                let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                                return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                            | _ -> return! fail errNotCEBody
                        }
                        // '{' new base-call object-members interface-impls '}' -- object expression
                        parser {
                            let! newTok = pNew
                            let! construction = ObjectConstruction.parse

                            let! baseCall =
                                opt (
                                    parser {
                                        let! asTok = pAs
                                        let! ident = pIdent
                                        return struct (asTok, ident)
                                    }
                                )

                            let baseCall =
                                match baseCall with
                                | ValueSome struct (asTok, ident) -> BaseCall.NamedBaseCall(construction, asTok, ident)
                                | ValueNone -> BaseCall.AnonBaseCall(construction)

                            let! withTok = pWith
                            let! members = withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                            // Parse additional interface implementations
                            let! interfaceImpls =
                                many (
                                    parser {
                                        let! interfaceTok = pInterface
                                        let! typ = Type.parse
                                        let! intfWithTok = opt pWith

                                        match intfWithTok with
                                        | ValueSome wTok ->
                                            let! intfMembers =
                                                withContext OffsideContext.WithAugment (many refMemberDefn.Parser)

                                            let! intfNextTokOpt = opt peekNextSyntaxToken

                                            let intfVirtualEnd =
                                                {
                                                    PositionedToken =
                                                        mkVirtualPT
                                                            Token.KWEnd
                                                            (match intfNextTokOpt with
                                                             | ValueSome tok -> tok.StartIndex
                                                             | ValueNone -> 0)
                                                    Index = TokenIndex.Virtual
                                                }

                                            let objMembers =
                                                ObjectMembers.ObjectMembers(wTok, intfMembers, intfVirtualEnd)

                                            return InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueSome objMembers)
                                        | ValueNone -> return InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueNone)
                                    }
                                )

                            // Synthesize virtual end for the main ObjectMembers
                            let! nextTokOpt = opt peekNextSyntaxToken

                            let virtualEnd =
                                {
                                    PositionedToken =
                                        mkVirtualPT
                                            Token.KWEnd
                                            (match nextTokOpt with
                                             | ValueSome tok -> tok.StartIndex
                                             | ValueNone -> 0)
                                    Index = TokenIndex.Virtual
                                }

                            let objMembers = ObjectMembers.ObjectMembers(withTok, members, virtualEnd)

                            let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                            return Expr.Object(lBrace, ValueSome newTok, baseCall, objMembers, interfaceImpls, rBrace)
                        }
                        // Record literal or clone
                        pRecordInner (ParenKind.Brace lBrace) lBrace Token.KWRBrace Token.KWRBraceBar
                        // Fallback: { expr-seq } — computation expression body without leading CE keyword
                        // (e.g., seq { someExpr }, task { callAsync() })
                        parser {
                            let! body = refExprSeqBlock.Parser

                            let! rBrace = nextSyntaxTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                            return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                        }
                    ]
                    "Record or RecordClone"
            )
            "Record or RecordClone"

    let pAnonRecordExpr: Parser<_, PositionedToken, ParseState, _> =
        pBracedExpr
            pLBraceBar
            OffsideContext.BraceBar
            (fun lBraceBar -> pRecordInner (ParenKind.BraceBar lBraceBar) lBraceBar Token.KWRBraceBar Token.KWRBrace)
            "Anonymous record"

    let private pILIntrinsic =
        // Structured IL intrinsic parser: (# "instr" type('T) args : retType #)
        let pAnyToken = nextSyntaxTokenSatisfiesLMsg (fun _ -> true) "IL intrinsic token"

        let pTypeArg (reader: Reader<PositionedToken, ParseState, _>) =
            // Parse type('T) or type ('T) — balanced parens after 'type' keyword
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok tok when tok.Token = Token.KWType ->
                match consumePeeked tok reader with
                | Error e -> Error e
                | Ok typeKw ->
                    match nextSyntaxTokenIsLMsg Token.KWLParen "(" reader with
                    | Error e -> Error e
                    | Ok lParen ->
                        let tokens = ResizeArray()
                        let mutable depth = 1
                        let mutable rParen = Unchecked.defaultof<SyntaxToken>
                        let mutable error = ValueNone

                        while depth > 0 && error = ValueNone do
                            match pAnyToken reader with
                            | Error e -> error <- ValueSome e
                            | Ok t ->
                                if t.Token = Token.KWLParen then
                                    depth <- depth + 1
                                    tokens.Add(t)
                                elif t.Token = Token.KWRParen then
                                    depth <- depth - 1

                                    if depth > 0 then tokens.Add(t) else rParen <- t
                                else
                                    tokens.Add(t)

                        match error with
                        | ValueSome e -> Error e
                        | ValueNone ->
                            preturn
                                (ValueSome(ILTypeArg(typeKw, lParen, ImmutableArray.CreateRange tokens, rParen)))
                                reader
            | _ -> preturn ValueNone reader

        fun (reader: Reader<PositionedToken, ParseState, _>) ->
            match nextSyntaxTokenIsLMsg Token.KWLHashParen "(#" reader with
            | Error e -> Error e
            | Ok lHashParen ->
                // Instruction string
                match parsePlainStringLiteral "IL instruction string" reader with
                | Error e -> Error e
                | Ok(instrKind, instrParts, instrClose) ->
                    // Optional type argument: type('T)
                    match pTypeArg reader with
                    | Error e -> Error e
                    | Ok typeArg ->
                        // Collect args, stopping at ':' or '#)'. Each arg is parsed at min binding power
                        // just above Application so dot chains, indexers, and type applications bind in,
                        // but juxtaposition (whitespace application) stops so each space-separated arg is distinct.
                        let args = ResizeArray()
                        let mutable finished = false
                        let mutable error = ValueNone

                        while not finished && error = ValueNone do
                            match peekNextSyntaxToken reader with
                            | Error e -> error <- ValueSome e
                            | Ok tok ->
                                match tok.Token with
                                | Token.KWRHashParen
                                | Token.OpColon -> finished <- true
                                | _ ->
                                    match refExprILArg.Parser reader with
                                    | Error e -> error <- ValueSome e
                                    | Ok arg -> args.Add(arg)

                        match error with
                        | ValueSome e -> Error e
                        | ValueNone ->
                            // Optional return type annotation (: Type)
                            match opt ReturnType.parse reader with
                            | Error e -> Error e
                            | Ok returnType ->
                                match nextSyntaxTokenIsLMsg Token.KWRHashParen "#)" reader with
                                | Error e -> Error e
                                | Ok rHashParen ->
                                    preturn
                                        (Expr.ILIntrinsic(
                                            lHashParen,
                                            instrKind,
                                            instrParts,
                                            instrClose,
                                            typeArg,
                                            ImmutableArray.CreateRange args,
                                            returnType,
                                            rHashParen
                                        ))
                                        reader

    let private recoverExpr p =
        recoverWith
            StoppingTokens.afterExpr
            DiagnosticSeverity.Error
            DiagnosticCode.MissingExpression
            (fun toks ->
                if toks.IsEmpty then
                    Expr.Missing
                else
                    Expr.SkipsTokens(toks)
            )
            p


    /// Parses `?ident` — optional argument expression in function calls.
    /// e.g., `f(?x=value)` passes `value` as optional parameter `x`.
    let private pOptionalArgExpr =
        parser {
            let! qmark = pQuestionMark
            let! ident = nextSyntaxIdentifierLMsg "Expected identifier after '?'"
            return Expr.OptionalArgExpr(qmark, ident)
        }

    let parseAtomic =
        dispatchNextSyntaxTokenFallback
            [
                // Ordered by approximate token frequency in idiomatic F# source (see
                // keyword/token histogram over the F# compiler corpus). `dispatchNextSyntaxTokenFallback`
                // does a linear scan, so common cases should come first.
                // Note: KWLet, KWIf, KWMatch, KWFunction, KWFun, KWTry, KWWhile, KWFor, KWUse
                // are handled as PrefixMapped operators in ExprOperatorParser.lhsParser,
                // not here.
                Token.Identifier, pIdentExpr
                Token.KWLParen, recoverExpr pParen
                Token.StringOpen, pString
                Token.KWLBracket, recoverExpr pList
                Token.VerbatimStringOpen, pString
                Token.KWLBrace, recoverExpr pRecordOrObjectExpr
                Token.String3Open, pString
                Token.InterpolatedStringOpen, pString
                Token.KWLArrayBracket, recoverExpr pArray
                Token.KWNew, recoverExpr pNewExpr
                Token.KWLHashParen, pILIntrinsic
                Token.Wildcard, (nextSyntaxTokenIsLMsg Token.Wildcard "_" |>> Expr.Wildcard)
                Token.KWStruct, recoverExpr pStructTuple
                Token.KWLBraceBar, recoverExpr pAnonRecordExpr
                Token.VerbatimInterpolatedStringOpen, pString
                Token.Interpolated3StringOpen, pString
                Token.BacktickedIdentifier, pIdentExpr
                Token.OpDynamic, pOptionalArgExpr
                Token.KWBegin, recoverExpr pBeginEnd
                Token.OpQuotationTypedLeft, recoverExpr pQuoteTyped
                Token.OpQuotationUntypedLeft, recoverExpr pQuoteUntyped
                Token.KWBase, (nextSyntaxTokenIsLMsg Token.KWBase "base" |>> Expr.Ident)
                Token.UnterminatedBacktickedIdentifier, pIdentExpr
            ]
            pConst

    let operators = ExprOperatorParser()

    let parse = Operator.parser parseAtomic operators

    let parseSeqBlock =
        withContext OffsideContext.SeqBlock parse
        |> recoverWith
            StoppingTokens.afterExpr
            DiagnosticSeverity.Error
            DiagnosticCode.MissingExpression
            (fun toks ->
                if toks.IsEmpty then
                    Expr.Missing
                else
                    Expr.SkipsTokens(toks)
            )

    do
        refExprAtomic.Set parseAtomic
        refExpr.Set parse
        refExprSeqBlock.Set parseSeqBlock
        refTypedSeqExprBlock.Set pTypedSeqExprBlock

        // Single expression without sequential composition — used for while conditions
        // and other contexts where the next keyword (e.g. `do`) must not be consumed as
        // a keyword expression via newline-triggered virtual semicolons.
        refExprNoSeq.Set(
            Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // IL intrinsic arguments: parse at just above Application so dot chains, indexers,
        // and type applications bind in, but juxtaposition stops so each whitespace-separated
        // arg in `(# "op" a.b c : ty #)` is captured as a separate arg.
        refExprILArg.Set(
            Operator.parserAt (int PrecedenceLevel.Application + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Semicolon has special handling in F# records, and object expressions
        // where it is used as a separator between elements rather than an operator, and it
        // should not be treated as an operator in those contexts.
        // So, we create a separate parser for expressions in records
        // and set the starting precedence one level higher so it will be parsed in `pRecordOrObjectExpr`
        refExprInRecords.Set(
            Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Pattern guards (when <expr>) must stop before '->' (Arrow) so the arrow
        // remains available for Rule.parse to consume with pArrowRight.
        // Arrow is right-associative so its LBP = base+1; using Arrow+1 as the level
        // makes minBp = base+3, which is just above Arrow's LBP, excluding it.
        refExprGuard.Set(
            Operator.parserAt (int PrecedenceLevel.RArrow + 1 |> BindingPower.fromLevel) parseAtomic operators
        )

        // Range operator '..' may be a postfix operator in slice syntax (e.g. A[1..]) or an infix operator in range expressions (e.g. 1..10).
        // So, we treat it effectively an a left-infix operator that special cases to prefix with the right terminator.
        refExprRange.Set(
            Operator.parserAt (int PrecedenceLevel.Range + 1 |> BindingPower.fromLevel) parseAtomic operators
        )
