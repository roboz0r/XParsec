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

module ElifBranches =
    [<Struct; RequireQualifiedAccess>]
    type private ElIfTok =
        | Elif of el: SyntaxToken
        | Else of el: SyntaxToken
        | ElseIf of el: SyntaxToken * ifTok: SyntaxToken

    let private pElifOrElseIf =
        dispatchNextNonTriviaTokenL
            [
                Token.KWElif,
                parser {
                    let! elifTok = pElif
                    return ElIfTok.Elif elifTok
                }
                Token.KWElse,
                parser {
                    let! elseTok = pElse

                    match! peekNextNonTriviaToken with
                    | t when t.Token = Token.KWIf ->
                        let! ifTok = consumePeeked t
                        return ElIfTok.ElseIf(elseTok, ifTok)
                    | _ -> return ElIfTok.Else elseTok
                }
            ]
            "Expected 'elif' or 'else'"

    let pConditionThen =
        parser {
            let! condition = withContext OffsideContext.If refExpr.Parser
            let! thenTok = pThen
            let! expr = withContext OffsideContext.Then refExprSeqBlock.Parser
            return (condition, thenTok, expr)
        }

    let rec private parseBranches (acc: ResizeArray<_>) (reader: Reader<PositionedToken, ParseState, _, _>) =
        match pElifOrElseIf reader with
        | Ok(ElIfTok.Elif elifTok) ->
            match pConditionThen reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.Elif(elifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.ElseIf(elseTok, ifTok)) ->
            match pConditionThen reader with
            | Ok(condition, thenTok, expr) ->
                acc.Add(ElifBranch.ElseIf(elseTok, ifTok, condition, thenTok, expr))
                parseBranches acc reader
            | Error e -> Error e

        | Ok(ElIfTok.Else elseTok) ->
            match withContext OffsideContext.Else refExprSeqBlock.Parser reader with
            | Ok expr -> Ok struct (List.ofSeq acc, ValueSome(ElseBranch.ElseBranch(elseTok, expr)))
            | Error e -> Error e

        | Error e -> Ok struct (List.ofSeq acc, ValueNone)

    let parse: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        fun reader -> parseBranches (ResizeArray()) reader

module Binding =
    let private pInlineTok =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWInline) "Expected 'inline'"

    let private pMutableTok =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    /// Build the Pat head from an IdentOrOp
    let private headPatOfIdentOrOp (identOrOp: IdentOrOp<SyntaxToken>) : Pat<SyntaxToken> =
        match identOrOp with
        | IdentOrOp.Ident t -> Pat.NamedSimple t
        | _ -> Pat.Op identOrOp

    /// Parse a function-style binding: [inline] [access] identOrOp [typar-defns] pat+ [: returnType] = expr
    let parseFunction attrs =
        parser {
            let! inlineTok = opt pInlineTok
            let! access = opt pAccessModifier
            let! identOrOp = IdentOrOp.parse
            let! typarDefns = opt TyparDefns.parse
            // Parse one or more argument patterns
            // Note: Must be atomic patterns to avoid consuming tokens that belong to the parent.
            // e.g. `let f x : int = 1` should parse `x` as an argument pattern, not `x : int` which would consume the return type annotation.
            let! argumentPats = Pat.parseAtomicMany1
            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = refExprSeqBlock.Parser

            return
                {
                    attributes = attrs
                    inlineToken = inlineTok
                    mutableToken = ValueNone
                    fixedToken = ValueNone
                    access = access
                    headPat = headPatOfIdentOrOp identOrOp
                    typarDefns = typarDefns
                    argumentPats = List.ofSeq argumentPats
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse a value-style binding: [mutable] [access] pat [typar-defns] [: returnType] = expr
    let parseValue attrs =
        parser {
            let! mut = opt pMutableTok
            let! access = opt pAccessModifier
            let! pat = Pat.parseAtomicOrTuple
            let! typarDefns = opt TyparDefns.parse
            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = refExprSeqBlock.Parser

            return
                {
                    attributes = attrs
                    inlineToken = ValueNone
                    mutableToken = mut
                    fixedToken = ValueNone
                    access = access
                    headPat = pat
                    typarDefns = typarDefns
                    argumentPats = []
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse either a function or a value binding
    let parse attrs : Parser<Binding<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ parseFunction attrs; parseValue attrs ] "Binding"

    let parseSepByAnd1 attrs =
        sepBy1 (parse attrs) pAnd |>> fun struct (bindings, _) -> List.ofSeq bindings

[<AutoOpen>]
module private MemberHelpers =
    // Forward reference for MemberDefn to avoid circular dependency issues
    // and provide a stub for ObjectMembers
    let refMemberDefn = FSRefParser<MemberDefn<SyntaxToken>>()

[<RequireQualifiedAccess>]
module FieldInitializer =
    let parse: Parser<FieldInitializer<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! id = LongIdent.parse
            let! equals = pEquals
            let! expr = refExprInRecords.Parser
            return FieldInitializer(id, equals, expr)
        }

    let parseSepBySemi1 = sepBy1 parse pSemi

[<RequireQualifiedAccess>]
module ObjectConstruction =
    let parse: Parser<ObjectConstruction<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! typ = Type.parse

            // Heuristic: If followed by '(', it's likely a constructor call (ObjectConstruction).
            // If not, it might be an Interface construction (inherit IMyInterface).
            // F# optional () logic is subtle, but for AST parsing:

            // Check for explicit unit '()' or paren '(' start
            let! argExpr =
                opt (
                    // Logic to detect if the next token starts an expression argument
                    // Usually looks for '(' or 'unit'
                    parser {
                        let! token = lookAhead nextNonTriviaToken

                        if token.Token = Token.KWLParen then
                            return! refExpr.Parser
                        else
                            return! fail (Message "No constructor arguments")
                    }
                )

            match argExpr with
            | ValueSome expr -> return ObjectConstruction.ObjectConstruction(typ, expr)
            | ValueNone -> return ObjectConstruction.InterfaceConstruction(typ)
        }

    do refObjectConstruction.Set parse

[<RequireQualifiedAccess>]
module BaseCall =
    let parse: Parser<BaseCall<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
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
    let parse: Parser<ObjectMembers<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! withTok = pWith

            // Parse list of members until 'end'
            // We use `many` combined with a check for the `end` token to terminate
            let! members, endTok = manyTill refMemberDefn.Parser pEnd

            return ObjectMembers.ObjectMembers(withTok, List.ofSeq members, endTok)
        }

[<RequireQualifiedAccess>]
module InterfaceImpl =
    let parse: Parser<InterfaceImpl<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
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
    | TypeApp of SyntaxToken * Type<SyntaxToken> list * SyntaxToken
    | DotIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // .[ expr ]
    | PostfixDynamic of SyntaxToken * Type<SyntaxToken> // :? Type (DynamicTypeTest)
    | HighPrecApp of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // ( expr ) for f(x, y)
    | HighPrecIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // [ expr ] for arr[i] (F# 6+ dot-less indexing)
    | TypeCast of Type<SyntaxToken> // :> Type
    /// Wraps a fully-parsed Expr for use as 'Aux in PrefixMapped keyword operators (if, match, let, etc.)
    | KeywordExpr of (SyntaxToken -> Expr<SyntaxToken>)
    | SliceAll
    | SliceFrom
    | Range of Expr<SyntaxToken>

[<RequireQualifiedAccess>]
module Expr =
    let bp x =
        LanguagePrimitives.ByteWithMeasure<bp> x

    let pl x : PrecedenceLevel = LanguagePrimitives.EnumOfValue x

    let private refExprRange = FSRefParser<Expr<SyntaxToken>>()

    type ExprOperatorParser() =
        let completeInfix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)

        let completeRange (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) =
            match l with
            | Expr.Range(l, dotdot1, step) ->
                // We already have a range on the left, so this must be a stepped range
                // with the new step in the middle and the new end on the right
                Expr.SteppedRange(l, dotdot1, step, op, r)
            | _ -> Expr.Range(l, op, r)

        let completeSequence (exprs: ResizeArray<Expr<_>>) ops =
            Expr.Sequential(List.ofSeq exprs, List.ofSeq ops)

        let completeAssignment (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.Assignment(l, op, r)
        let completePrefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let completeLazy (op: SyntaxToken) (e: Expr<_>) = Expr.Lazy(op, e)
        let completeAssert (op: SyntaxToken) (e: Expr<_>) = Expr.Assert(op, e)
        let completeSliceTo (op: SyntaxToken) (e: Expr<_>) = Expr.SliceTo(op, e)
        let completeTuple (elements: ResizeArray<Expr<_>>) ops = Expr.Tuple(List.ofSeq elements)

        let completeApp (elements: ResizeArray<Expr<_>>) ops =
            Expr.App(elements[0], elements |> Seq.skip 1 |> List.ofSeq)

        let completeSliceAll (op: SyntaxToken) (x: ExprAux) =
            match x with
            | ExprAux.SliceAll -> Expr.SliceAll(op)
            | _ -> failwith "Unexpected Aux type for SliceAll completion"

        let completeRangeOrSliceFrom (l: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.SliceFrom -> Expr.SliceFrom(l, op)
            | ExprAux.Range r -> completeRange l op r
            | _ -> failwith "Unexpected Aux type for RangeOrSliceFrom completion"

        let printOpInfo (op: OperatorInfo) =
            printfn
                $"Operator: {op.PositionedToken}({op.StartIndex}), Precedence: {op.Precedence}, Associativity: %A{op.Associativity}"

        let pIdent = nextNonTriviaTokenIsL Token.Identifier "Expected identifier after '.'"

        let parseDotRhs: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
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
                    // .ident — field/member access (e.g. x.Name)
                    parser {
                        let! ident = pIdent
                        return ExprAux.Ident ident
                    }
                ]
                "Dot RHS (identifier or index)"

        let completeDot (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.Ident ident ->
                match expr with
                | Expr.Ident firstIdent -> Expr.LongIdentOrOp(LongIdentOrOp.LongIdent [ firstIdent; ident ])
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdentOrOp) ->
                    let newLongIdent =
                        match longIdentOrOp with
                        | [] -> [ ident ]
                        | _ -> longIdentOrOp @ [ ident ]

                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent newLongIdent)
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.LongIdent [ ident ])
            | ExprAux.DotIndex(lBracket, indexExpr, rBracket) ->
                Expr.IndexedLookup(expr, ValueSome op, lBracket, indexExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for dot completion"


        let pTypeAppRhs =
            parser {
                let! lAngle = nextNonTriviaTokenIsL Token.OpLessThan "Expected '<' for type application"
                let! types, _ = sepBy Type.parse pComma

                let! state = getUserState

                let! rAngle = pCloseTypeParams

                return ExprAux.TypeApp(lAngle, List.ofSeq types, rAngle)
            }

        let completeTypeApp (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeApp(lAngle, types, rAngle) -> Expr.TypeApp(expr, lAngle, types, rAngle)
            | _ -> failwith "Unexpected Aux type for type application completion"

        let pHighPrecLParen =
            // Use satisfy instead of nextNonTriviaToken to ensure we only match '(' if
            // it's immediately after the function expression with no trivia in between.
            satisfyL (fun (t: PositionedToken) -> t.Token = Token.KWLParen) "'(' high precedence application"

        let pHighPrecLBracket =
            // Use satisfy instead of nextNonTriviaToken to ensure we only match '[' if
            // it's immediately after the expression with no trivia in between (F# 6+ dot-less indexing).
            satisfyL (fun (t: PositionedToken) -> t.Token = Token.KWLBracket) "'[' high precedence indexing"

        let peekHighPrecApp =
            lookAhead (choiceL [ pHighPrecLParen; pHighPrecLBracket ] "( or [ for high-precedence")
            |>> fun (pt: PositionedToken) ->
                match pt.Token with
                | Token.KWLParen -> virtualToken (PositionedToken.Create(Token.OpHighPrecedenceApp, pt.StartIndex))
                | Token.KWLBracket ->
                    virtualToken (PositionedToken.Create(Token.OpHighPrecedenceIndexApp, pt.StartIndex))
                | _ -> failwith "Unexpected token in peekHighPrecApp"

        let parseHighPrecIndex: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>, _> =
            // Parse [ expr ] for F# 6+ dot-less indexing arr[i] — no whitespace before '['
            parser {
                let! pos = getPosition
                let! lBracket = pHighPrecLBracket
                let lBracket = syntaxToken lBracket pos.Index
                let! argExpr = refExpr.Parser
                let! rBracket = pRBracket
                return ExprAux.HighPrecIndex(lBracket, argExpr, rBracket)
            }

        let parseHighPrecApp: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>, _> =
            // Parse ( expr ) for high-precedence application f(x, y) — no whitespace before '('
            parser {
                let! pos = getPosition
                let! lParen = pHighPrecLParen
                let lParen = syntaxToken lParen pos.Index

                // Handle f() — empty argument list
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.KWRParen ->
                    let! rParen = consumePeeked t
                    return ExprAux.HighPrecApp(lParen, Expr.EmptyBlock(ParenKind.Paren lParen, rParen), rParen)
                | _ ->
                    let! argExpr = refExpr.Parser
                    let! rParen = pRParen
                    return ExprAux.HighPrecApp(lParen, argExpr, rParen)
            }

        let completeHighPrec (funcExpr: Expr<_>) (_op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.HighPrecApp(lParen, argExpr, rParen) -> Expr.HighPrecedenceApp(funcExpr, lParen, argExpr, rParen)
            | ExprAux.HighPrecIndex(lBracket, argExpr, rBracket) ->
                Expr.IndexedLookup(funcExpr, ValueNone, lBracket, argExpr, rBracket)
            | _ -> failwith "Unexpected Aux type for high-precedence application/index completion"

        let parseRangeRhs reader =
            match peekNextNonTriviaToken reader with
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

        let completeTypeCast (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeCast(typ) ->
                match op.Token with
                | Token.OpColon -> Expr.TypeAnnotation(expr, op, typ)
                | Token.OpUpcast -> Expr.StaticUpcast(expr, op, typ)
                | Token.OpDowncast -> Expr.DynamicDowncast(expr, op, typ)
                | Token.OpTypeTest -> Expr.DynamicTypeTest(expr, op, typ)
                | _ -> failwith "Unexpected operator for type cast completion"
            | _ -> failwith "Unexpected Aux type for type cast completion"

        let rhsOperators
            : (SyntaxToken
                  -> RHSOperator<
                      SyntaxToken,
                      ExprAux,
                      Expr<SyntaxToken>,
                      PositionedToken,
                      ParseState,
                      ReadableImmutableArray<PositionedToken>,
                      ReadableImmutableArraySlice<PositionedToken>
                   >) array =
            Array.init
                30
                (fun i ->
                    let pl = pl i
                    let power = BindingPower.fromLevel i

                    match pl with
                    // Pattern only keywords
                    // | PrecedenceLevel.As -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // | PrecedenceLevel.When -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Pipe -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Semicolon ->
                        // (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeSemicolon))
                        (fun op -> InfixNary(op, preturn op, power, true, completeSequence))
                    | PrecedenceLevel.RArrow ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // LHS keywords
                    // | PrecedenceLevel.Let -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.Function -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.If -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Assignment ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeAssignment))
                    | PrecedenceLevel.Comma -> (fun op -> InfixNary(op, preturn op, power, false, completeTuple))
                    | PrecedenceLevel.Range ->
                        (fun op ->
                            // The binary '..' operator. Non-associative. The ternary '.. ..' form or the postfix form are special grammar rules.
                            // Parse as Left-associative and disambiguate if we see another '..' at the same precedence level on the right.
                            InfixMapped(op, preturn op, power, parseRangeRhs, completeRangeOrSliceFrom)
                        )
                    | PrecedenceLevel.LogicalOr -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.LogicalAnd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Cast ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, completeTypeCast))
                    | PrecedenceLevel.ComparisonAndBitwise ->
                        (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Append ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Cons ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.TypeTest ->
                        (fun op -> InfixMapped(op, preturn op, power, parseTypeCastRhs, completeTypeCast))
                    | PrecedenceLevel.InfixAdd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.InfixMultiply -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Power ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Application ->
                        // (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                        (fun op -> InfixNary(op, preturn op, power, false, completeApp))
                    // | PrecedenceLevel.PatternMatchBar -> Pattern only operator
                    // | PrecedenceLevel.Prefix -> LHS only
                    | PrecedenceLevel.Dot -> (fun op -> InfixMapped(op, preturn op, power, parseDotRhs, completeDot))
                    | PrecedenceLevel.HighIndexApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, parseHighPrecIndex, completeHighPrec))
                    | PrecedenceLevel.HighApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, parseHighPrecApp, completeHighPrec))
                    | PrecedenceLevel.HighTypeApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, pTypeAppRhs, completeTypeApp))
                    // | PrecedenceLevel.Parens -> LHS only
                    | pl ->
                        (fun op ->
                            invalidOp
                                $"No operator handler for precedence level {pl}. Got op {op.Token} at position {op.StartIndex}"
                        )
                )

        let pSepVirt =
            let failSep =
                fail (Message "Expected ';' or newline at the same indent for expression sequencing")

            parser {
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.EOF -> return! failSep
                | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
                | t ->
                    // Not a semicolon, but maybe we emit a virtual semicolon for newline-separated expressions.
                    // Closing delimiters (}, ], |], ), end) cannot start an expression, so never emit VirtualSep for them —
                    // they belong to an enclosing parser (e.g. the paren/brace block that called us).
                    // Pure infix operators (e.g. ||, &&, |>, >>) cannot start an expression either —
                    // they must be continuations of the previous expression (F# spec permits infix continuation).
                    match t.Token with
                    | Token.KWRBrace
                    | Token.KWRBracket
                    | Token.KWRArrayBracket
                    | Token.KWRParen
                    | Token.KWEnd -> return! failSep
                    | _ when TokenInfo.isOperator t.Token && not (TokenInfo.canBePrefix t.Token) -> return! failSep
                    | _ ->
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
            }

        let pApplication =
            let isAtomicExprToken (t: SyntaxToken) =
                match t.Token with
                | Token.Identifier
                | Token.KWLParen
                | Token.KWLBracket
                | Token.KWLArrayBracket
                | Token.KWBegin
                | Token.KWStruct
                | Token.KWLBrace
                | Token.OpQuotationTypedLeft
                | Token.OpQuotationUntypedLeft
                | Token.OpDereference -> true
                | _ ->
                    if Constant.isLiteralToken t.Token then
                        true
                    else
                        match OperatorInfo.TryCreate t.PositionedToken with
                        | ValueSome opInfo -> false //TODO: opInfo.CanBePrefix
                        | ValueNone -> false

            let failApp = fail (Message "Expected expression after application")
            // Application is *juxtaposition* of two expressions with trivia in between, e.g. `f x` or `f (g y)` or `f(*comment*)x`.
            parser {
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.EOF -> return! failApp
                | t when isAtomicExprToken t ->
                    let! indent = currentIndent
                    let! state = getUserState
                    // printfn "Application check: indent=%d, context=%A" indent state.Context
                    let atAppIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent > ctxIndent
                        | [] -> indent > 0

                    if atAppIndent then
                        // We have a valid argument token coming up, so this is an application
                        return virtualToken (PositionedToken.Create(Token.VirtualApp, t.StartIndex))
                    else
                        return! failApp
                | _ ->
                    // Not a valid argument token, fail to avoid consuming tokens that should be parsed by outer parsers
                    return! failApp
            }

        let pTypeApplication =
            // Treat '<' followed by a type and '>' as type application
            parser {

                let! state = getUserState
                let! pos = getPosition

                let! lAngle =
                    satisfyL
                        (fun (t: PositionedToken) ->
                            match t.Token with
                            | Token.OpLessThan -> true
                            | _ -> false
                        )
                        "Expected '<' for type application"
                    |> lookAhead
                    >>= fun t ->
                        let st = syntaxToken t pos.Index

                        if tokenStringIs "<" st state then
                            preturn st
                        else
                            fail (Message "Expected '<' for type application")


                let typeAngle =
                    { lAngle with
                        PositionedToken = PositionedToken.Create(Token.VirtualTyApp, lAngle.StartIndex)
                    }

                return typeAngle
            }

        let getRhsOperatorHandler (opInfo: OperatorInfo) token =
            // Note: Precedence gets bit-packed into the token and converted directly
            // to RHS handler index for efficiency.
            let pl = opInfo.Precedence
            let handler = rhsOperators[LanguagePrimitives.EnumToValue pl]
            handler token

        let rhsParser =
            let handleToken token =
                match OperatorInfo.TryCreate token.PositionedToken with
                | ValueNone -> fail (Message "Expected RHS operator")
                | ValueSome opInfo ->
                    match opInfo.Token with
                    | Token.OpBar -> fail (Message "Unexpected '|' operator in expression context")
                    | Token.OpDereference -> fail (Message "Unexpected dereference operator '!' in infix context")
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
                    | Token.KWFunction -> fail (Message "Unexpected prefix keyword in RHS expression context")
                    | _ -> preturn (getRhsOperatorHandler opInfo token)

            // First try type application, then whitespace application, then explicit operator.
            // Using choiceL ensures that if nextNonTriviaToken consumes a token
            // (e.g. '->') but handleToken fails (no expression-level handler for Arrow),
            // XParsec backtracks the consumed token so outer parsers (e.g. Rule.parse) see it.
            choiceL
                [
                    reprocessedOperatorAfterTypeParams >>= handleToken
                    pTypeApplication >>= handleToken
                    peekHighPrecApp >>= handleToken
                    pApplication >>= handleToken
                    pSepVirt >>= handleToken
                    nextNonTriviaToken >>= handleToken
                ]
                "RHS operator"

        // Used for for-to and use identifiers (not the dot-access pIdent above)
        let pIdentTok = nextNonTriviaTokenIsL Token.Identifier "identifier"

        // Shared complete function for all PrefixMapped keyword forms.
        let completeKeyword (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.KeywordExpr e -> e op
            | _ -> failwith "Unexpected Aux type for keyword expression"

        // Body parsers for PrefixMapped keyword forms.
        // Each parser consumes the keyword itself (inside withContext for correct offside behaviour).
        // They are called AFTER lhsParser has peeked (but NOT consumed) the keyword token.

        let pIfBody =
            let pCond = withContext OffsideContext.If refExpr.Parser

            parser {
                let! cond = pCond
                let! thenTok = pThen
                let! thenExpr = withContext OffsideContext.Then refExprSeqBlock.Parser
                let! elifs, elseBranch = ElifBranches.parse

                return
                    ExprAux.KeywordExpr(fun ifTok ->
                        Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, List.ofSeq elifs, elseBranch)
                    )
            }

        let pMatchBody =
            let pMatchExpr = withContext OffsideContext.Match refExpr.Parser

            parser {
                let! e = pMatchExpr
                let! w = pWith
                let! rules = withContext OffsideContext.MatchClauses Rules.parse
                return ExprAux.KeywordExpr(fun m -> Expr.Match(m, e, w, rules))
            }

        let pFunctionBody =
            withContext
                OffsideContext.Function
                (parser {
                    let! rules = withContext OffsideContext.MatchClauses Rules.parse
                    return ExprAux.KeywordExpr(fun funTok -> Expr.Function(funTok, rules))
                })

        let pFunBody =
            withContext
                OffsideContext.Fun
                (parser {
                    // let! funTok = pFun
                    let! pats = many1 Pat.parse
                    let! arrow = pArrowRight
                    let! expr = refExprSeqBlock.Parser
                    return ExprAux.KeywordExpr(fun funTok -> Expr.Fun(funTok, List.ofSeq pats, arrow, expr))
                })

        let pTryBody =
            let pTryExpr = withContext OffsideContext.Try refExprSeqBlock.Parser

            let pWith =
                parser {
                    let! withTok = pWith
                    let! rules = withContext OffsideContext.MatchClauses Rules.parse

                    return
                        fun tryExpr -> ExprAux.KeywordExpr(fun tryTok -> Expr.TryWith(tryTok, tryExpr, withTok, rules))
                }

            let pFinally =
                parser {
                    let! finTok = pFinally
                    let! finExpr = refExprSeqBlock.Parser

                    return
                        fun tryExpr ->
                            ExprAux.KeywordExpr(fun tryTok -> Expr.TryFinally(tryTok, tryExpr, finTok, finExpr))
                }

            let pWithOrFinally =
                dispatchNextNonTriviaTokenL
                    [ Token.KWWith, pWith; Token.KWFinally, pFinally ]
                    "Expected 'with' or 'finally'"

            parser {
                let! tryExpr = pTryExpr
                let! withOrFinally = pWithOrFinally
                return withOrFinally tryExpr
            }

        let pDoneVirt reader =
            match peekNextNonTriviaToken reader with
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

        let pWhileBody =
            // Note: Both the condition and the do must be indented past the 'while'
            // but they may be at different indents from each other.
            let pCond = withContext OffsideContext.While refExpr.Parser
            let pDo = withContext OffsideContext.While pDo

            parser {
                let! cond = pCond
                let! doTok = pDo
                let! body = withContext OffsideContext.Do refExprSeqBlock.Parser
                let! doneTok = pDoneVirt
                return ExprAux.KeywordExpr(fun whileTok -> Expr.While(whileTok, cond, doTok, body, doneTok))
            }

        let pForBody =
            let pIterDefn =
                choiceL
                    [
                        // for ident = start to end do
                        parser {
                            let! ident = pIdentTok
                            let! eq = pEquals
                            let! startExpr = refExprGuard.Parser
                            let! toTok = pToOrDownTo
                            let! endExpr = refExprGuard.Parser

                            return
                                fun doTok body doneTok forTok ->
                                    Expr.ForTo(forTok, ident, eq, startExpr, toTok, endExpr, doTok, body, doneTok)
                        }
                        // for pat in expr do
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pIn
                            let! range = refExprGuard.Parser

                            return
                                fun doTok body doneTok forTok ->
                                    Expr.ForIn(forTok, pat, inTok, range, doTok, body, doneTok)
                        }
                    ]
                    "Expected for-to or for-in iterator definition"

            let pForBody =
                withContext
                    OffsideContext.For
                    (parser {
                        let! iterDefn = pIterDefn
                        let! doTok = pDo <|> pArrowRight
                        let! body = withContext OffsideContext.Do refExprSeqBlock.Parser
                        return iterDefn doTok body
                    })

            parser {
                // let! forTok = pFor
                let! forBody = pForBody
                let! doneTok = pDoneVirt
                return ExprAux.KeywordExpr(forBody doneTok)
            }

        let pLetOrUseIn =
            parser {
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.KWIn -> return! consumePeeked t
                | t ->
                    // Not 'in', but maybe we emit a virtual 'in' for offside rule in let bindings without 'in'.
                    // We can only emit VirtualIn if the next token is indented less than or equal to the 'let' and we're at the correct context indent.
                    let! indent = currentIndent
                    let! state = getUserState

                    let atContextIndent =
                        match state.Context with
                        | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                        | [] -> indent = 0

                    if atContextIndent then
                        return virtualToken (PositionedToken.Create(Token.VirtualIn, t.StartIndex))
                    else
                        // TODO: Consider parser recovery here instead of hard failure,
                        // e.g. skip tokens until we find one that is at the correct indent
                        // or a valid separator (e.g. semicolon, newline with correct indent, or closing delimiter).
                        // Maybe still emit a virtual 'in' as well as a diagnostic error for the missing 'in' to allow parsing to continue and produce a more complete AST with error nodes.
                        return! fail (Message "Expected 'in' at the same indent as 'let'")
            }

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
                    let! bindings = Binding.parseSepByAnd1 ValueNone
                    return struct (recTok, bindings)
                })

        /// Single body parser used by both `let/let!` and `use/use!`.
        /// The keyword token has been peeked but NOT consumed by lhsParser.
        let pLetOrUseBody =
            // The keyword is still unconsumed. We peek it to get its column for the Let
            // offside context (F# spec 15.1.7: "The start of a let ... token"), then consume.
            // The definition gets parsed in a Let context, with another SeqBlock after the `=`
            // for correct offside behaviour. After the definition, we check for 'in' or emit
            // a virtual 'in' for the offside rule, then parse the body expression.
            parser {
                // TODO: Move this indent-peeking logic into a separate parser combinator that runs before the main body parser to set up the correct offside context,
                // so we don't have to repeat it for let and use and potentially other keywords with similar offside rules in the future (e.g. maybe 'do' in computation expressions).
                // Apply the same logic to other LHS keywords as well (e.g. if, match) that have offside rules for their bodies.
                let! kwTok = peekNextNonTriviaToken

                match kwTok.Token with
                | Token.KWLet
                | Token.KWLetBang
                | Token.KWUse
                | Token.KWUseBang ->
                    let! kwTok = consumePeeked kwTok
                    let! state = getUserState

                    let indent =
                        match kwTok.Index with
                        | TokenIndex.Regular iT -> ParseState.getIndent state iT
                        | TokenIndex.Virtual -> 0

                    let! (recTok, bindings) = pLetOrUseDefn indent kwTok.PositionedToken
                    let! inTok = pLetOrUseIn
                    let! expr = refExprSeqBlock.Parser

                    return
                        ExprAux.KeywordExpr(fun _opTok ->
                            let kw =
                                match kwTok.Token with
                                | Token.KWLet -> LetOrUseKeyword.Let kwTok
                                | Token.KWUse -> LetOrUseKeyword.Use kwTok
                                | Token.KWLetBang -> LetOrUseKeyword.LetBang kwTok
                                | Token.KWUseBang -> LetOrUseKeyword.UseBang kwTok
                                | t -> failwith $"Unexpected keyword token for let/use body {t}"

                            Expr.LetOrUse(kw, recTok, bindings, ValueSome inTok, ValueSome expr)
                        )
                | t -> return! fail (Message $"Expected let/use keyword but got {t}")
            }

        let pYieldReturnDoBody =
            parser {
                let! expr = refExprSeqBlock.Parser

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

        let peekIsSliceAll =
            parser {
                let! token = peekNextNonTriviaToken
                // We are in the middle of parsing a slice like A[0..1,*], and we've just parsed the '*' token.
                // Now we peek to see if the next token is a ',' or ']' which would indicate slice-all syntax.
                // Otherwise '*' is just a regular operator for multiplication.
                match token.Token with
                | Token.KWRBracket
                | Token.EOF
                | Token.OpComma -> return ExprAux.SliceAll
                | _ -> return! fail (Message "Expected ',' or ']' for slice all syntax")
            }

        let lhsParser =
            parser {
                let! token = peekNextNonTriviaToken

                // Keyword-prefix forms: these tokens are not in isOperatorKeyword, so OperatorInfo.TryCreate
                // returns ValueNone for them. Match directly on the token type instead.
                // We peek but do NOT consume — the body parser handles consumption inside withContext.
                match token.Token with
                | Token.KWIf ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pIfBody, completeKeyword)
                | Token.KWMatch
                | Token.KWMatchBang ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pMatchBody, completeKeyword)
                | Token.KWFunction ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pFunctionBody, completeKeyword)
                | Token.KWFun ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pFunBody, completeKeyword)
                | Token.KWTry ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pTryBody, completeKeyword)
                | Token.KWWhile ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pWhileBody, completeKeyword)
                | Token.KWFor ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pForBody, completeKeyword)
                | Token.KWLet
                | Token.KWLetBang
                | Token.KWUse
                | Token.KWUseBang ->
                    // Do NOT consume here — pLetOrUseBody peeks the keyword to set the Let
                    // context indent at the keyword's column (F# spec 15.1.7), then consumes.
                    return PrefixMapped(token, preturn token, pLetOrUseBody, completeKeyword)
                | Token.KWDo
                | Token.KWDoBang
                | Token.KWReturn
                | Token.KWReturnBang
                | Token.KWYield
                | Token.KWYieldBang ->
                    let! token = consumePeeked token
                    return PrefixMapped(token, preturn token, pYieldReturnDoBody, completeKeyword)
                | _ ->
                    match OperatorInfo.TryCreate(token.PositionedToken) with
                    // Note: Spec shows lazy and assert keywords as having same precedence as function application,
                    // so they are parsed as prefix operators with the same precedence level.
                    | ValueSome opInfo when opInfo.Token = Token.KWLazy ->
                        // printOpInfo opInfo
                        let! tok = consumePeeked token
                        let power = BindingPower.fromLevel (int opInfo.Precedence)
                        return Prefix(tok, preturn tok, power, completeLazy)
                    | ValueSome opInfo when opInfo.Token = Token.KWAssert ->
                        // printOpInfo opInfo
                        let! tok = consumePeeked token
                        let power = BindingPower.fromLevel (int opInfo.Precedence)
                        return Prefix(tok, preturn tok, power, completeAssert)
                    | ValueSome opInfo when opInfo.Token = Token.OpRange ->
                        // printOpInfo opInfo
                        let! tok = consumePeeked token
                        let power = BindingPower.fromLevel (int opInfo.Precedence)
                        // A[..5]
                        return Prefix(tok, preturn tok, power, completeSliceTo)
                    | ValueSome opInfo when opInfo.Token = Token.OpMultiply ->
                        // printOpInfo opInfo
                        let! tok = consumePeeked token
                        let power = BindingPower.fromLevel (int opInfo.Precedence)
                        // A[0..1,*]
                        return PrefixMapped(token, preturn tok, peekIsSliceAll, completeSliceAll)
                    | ValueSome opInfo when opInfo.CanBePrefix ->
                        // printOpInfo opInfo
                        let! tok = consumePeeked token
                        let power = BindingPower.fromLevel (int opInfo.Precedence)
                        return Prefix(tok, preturn tok, power, completePrefix)
                    | _ -> return! fail (Message "Not a prefix operator")
            }

        interface Operators<
            SyntaxToken,
            ExprAux,
            Expr<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let pConst = Constant.parse |>> Expr.Const

    let private isInterpolatedFragment (tok: Token) =
        match tok with
        | Token.InterpolatedStringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.Interpolated3StringFragment
        | Token.EscapeLBrace
        | Token.EscapeRBrace
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> true
        | _ -> false

    let private isInterpolatedInvalidText (tok: Token) =
        match tok with
        | Token.UnmatchedInterpolatedRBrace
        | Token.InvalidFormatPlaceholder
        | Token.InvalidFormatPercents
        | Token.TooManyLBracesInInterpolated3String
        | Token.TooManyRBracesInInterpolated3String -> true
        | _ -> false

    let private isInterpolatedClose (tok: Token) =
        match tok with
        | Token.InterpolatedStringClose
        | Token.VerbatimInterpolatedStringClose
        | Token.Interpolated3StringClose
        | Token.UnterminatedInterpolatedString -> true
        | _ -> false

    let pInterpolatedString =
        let rec loop (parts: ResizeArray<InterpolatedStringPart<SyntaxToken>>) reader =
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok t when isInterpolatedFragment t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok fragment ->
                    parts.Add(InterpolatedStringPart.Text fragment)
                    loop parts reader
            | Ok t when t.Token = Token.FormatPlaceholder ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok formatSpec ->
                    match peekNextNonTriviaToken reader with
                    | Error e -> Error e
                    | Ok next when next.Token = Token.InterpolatedExpressionOpen ->
                        match consumePeeked next reader with
                        | Error e -> Error e
                        | Ok lBrace ->
                            match refExpr.Parser reader with
                            | Error e -> Error e
                            | Ok expr ->
                                match nextNonTriviaTokenIsL Token.InterpolatedExpressionClose "Expected }" reader with
                                | Error e -> Error e
                                | Ok rBrace ->
                                    parts.Add(InterpolatedStringPart.Expr(ValueSome formatSpec, lBrace, expr, rBrace))
                                    loop parts reader
                    | _ ->
                        parts.Add(InterpolatedStringPart.OrphanFormatSpecifier formatSpec)
                        loop parts reader
            | Ok t when t.Token = Token.InterpolatedExpressionOpen ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok lBrace ->
                    match refExpr.Parser reader with
                    | Error e -> Error e
                    | Ok expr ->
                        match nextNonTriviaTokenIsL Token.InterpolatedExpressionClose "Expected }" reader with
                        | Error e -> Error e
                        | Ok rBrace ->
                            parts.Add(InterpolatedStringPart.Expr(ValueNone, lBrace, expr, rBrace))
                            loop parts reader
            | Ok t when isInterpolatedInvalidText t.Token ->
                match consumePeeked t reader with
                | Error e -> Error e
                | Ok invalid ->
                    parts.Add(InterpolatedStringPart.InvalidText invalid)
                    loop parts reader
            | Ok _ -> Ok parts

        parser {
            let! opening =
                nextNonTriviaTokenSatisfiesL
                    (fun t ->
                        match t.Token with
                        | Token.InterpolatedStringOpen
                        | Token.VerbatimInterpolatedStringOpen
                        | Token.Interpolated3StringOpen -> true
                        | _ -> false
                    )
                    "Expected interpolated string open"

            let! parts = loop (ResizeArray())

            let! closing =
                nextNonTriviaTokenSatisfiesL (fun t -> isInterpolatedClose t.Token) "Expected interpolated string close"

            return Expr.InterpolatedString(opening, Seq.toList parts, closing)
        }

    let pIdent =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.Identifier) "Expected identifier"
        |>> Expr.Ident

    let private pEnclosed =
        let completeEmpty l r = Expr.EmptyBlock(l, r)
        let completeEnclosed l e r = Expr.EnclosedBlock(l, e, r)
        let skipsTokens toks missing = Expr.SkipsTokens(toks, missing)
        pEnclosed completeEmpty completeEnclosed Expr.Missing skipsTokens

    let private pExprOrTypedPat =
        parser {
            let! expr = refExprSeqBlock.Parser

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.OpColon ->
                let! colon = consumePeeked t
                let! typ = Type.parse
                return Expr.TypeAnnotation(expr, colon, typ)
            | _ -> return expr
        }

    let pParen =
        pEnclosed
            pLParen
            Token.KWRParen
            ParenKind.Paren
            OffsideContext.Paren
            DiagnosticCode.ExpectedRParen
            pExprOrTypedPat

    let pBeginEnd =
        pEnclosed
            pBegin
            Token.KWEnd
            ParenKind.BeginEnd
            OffsideContext.Begin
            DiagnosticCode.ExpectedEnd
            refExprSeqBlock.Parser

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

            let e =
                match e with
                | Expr.Tuple(es) -> es
                | e -> [ e ]

            return Expr.StructTuple(kw, l, e, r)
        }

    let pNewExpr =
        parser {
            let! newTok = pNew
            let! typ = Type.parse
            // Constructor args are a parenthesised expression: new T(args)
            let! argExpr = pParen
            return Expr.New(newTok, typ, argExpr)
        }

    let pRecordOrObjectExpr: Parser<_, PositionedToken, ParseState, _, _> =
        fun reader ->
            match pLBrace reader with
            | Error e -> Error e
            | Ok lBrace ->

                // Push Brace offside context so collection/CE undentation rules can find it
                let savedState = reader.State

                let entry: Offside =
                    {
                        Context = OffsideContext.Brace
                        Indent = 0
                        Token = lBrace.PositionedToken
                    }

                reader.State <- ParseState.pushOffside entry reader.State
                let stackDepth = reader.State.Context.Length

                reader.State.Trace.Invoke(
                    TraceEvent.ContextPush(OffsideContext.Brace, 0, lBrace.PositionedToken, stackDepth)
                )

                let innerParser =
                    choiceL
                        [
                            // { CE keywords... } — computation expression body
                            // Detected by first token being a CE-specific keyword (not a valid record field name)
                            parser {
                                let! peekTok = peekNextNonTriviaToken

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

                                    let! rBrace =
                                        nextNonTriviaTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                                    return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                                | _ -> return! fail (Message "Not a computation expression body")
                            }
                            // TODO: '{' new base-call object-members interface-impls '}' -- object expression
                            // { expr with Field = val; ... } — record clone/update
                            parser {
                                let! baseExpr = refExprInRecords.Parser
                                let! withTok = pWith
                                let! fields, _ = sepBy1 FieldInitializer.parse pSemi
                                let! rBrace = pRBrace
                                return Expr.RecordClone(lBrace, baseExpr, withTok, List.ofSeq fields, rBrace)
                            }
                            // { Field = val; ... } — record literal
                            parser {
                                let! fields, _ = sepBy1 FieldInitializer.parse pSemi
                                let! rBrace = pRBrace
                                return Expr.Record(lBrace, List.ofSeq fields, rBrace)
                            }
                        ]
                        "Record or RecordClone"

                match innerParser reader with
                | Ok result ->
                    reader.State.Trace.Invoke(TraceEvent.ContextPop(OffsideContext.Brace, stackDepth - 1))
                    reader.State <- ParseState.popOffside reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message "Record or RecordClone") reader

    let private recoverExpr p =
        recoverWith
            StoppingTokens.afterExpr
            DiagnosticSeverity.Error
            DiagnosticCode.MissingExpression
            (fun toks ->
                if toks.IsEmpty then
                    Expr.Missing
                else
                    Expr.SkipsTokens(toks, Expr.Missing)
            )
            p


    let parseAtomic =
        dispatchNextNonTriviaTokenFallback
            [
                // TODO: Performance, sort this by frequency and put most common cases first
                Token.Identifier, pIdent
                //Token.Unit, pConst
                // Note: KWLet, KWIf, KWMatch, KWFunction, KWFun, KWTry, KWWhile, KWFor, KWUse
                // are handled as PrefixMapped operators in ExprOperatorParser.lhsParser.
                Token.KWLParen, recoverExpr pParen
                //Token.OpNil, recoverExpr pList
                Token.KWLBracket, recoverExpr pList
                Token.KWLArrayBracket, recoverExpr pArray
                Token.KWBegin, recoverExpr pBeginEnd
                Token.KWStruct, recoverExpr pStructTuple
                Token.KWNew, recoverExpr pNewExpr
                Token.KWLBrace, recoverExpr pRecordOrObjectExpr
                Token.OpQuotationTypedLeft, recoverExpr pQuoteTyped
                Token.OpQuotationUntypedLeft, recoverExpr pQuoteUntyped
                Token.InterpolatedStringOpen, pInterpolatedString
                Token.VerbatimInterpolatedStringOpen, pInterpolatedString
                Token.Interpolated3StringOpen, pInterpolatedString
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
                    Expr.SkipsTokens(toks, Expr.Missing)
            )

    do
        refExpr.Set parse
        refExprSeqBlock.Set parseSeqBlock

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
