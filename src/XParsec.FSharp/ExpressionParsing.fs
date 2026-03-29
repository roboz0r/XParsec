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
        let pCond = withContext OffsideContext.If refExpr.Parser
        let pThenExpr = withContext OffsideContext.Then refExprSeqBlock.Parser

        parser {
            let! condition = pCond
            let! thenTok = pThen
            let! expr = pThenExpr
            return (condition, thenTok, expr)
        }

    let private pElseExpr = withContext OffsideContext.Else refExprSeqBlock.Parser

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
            match pElseExpr reader with
            | Ok expr -> Ok struct (List.ofSeq acc, ValueSome(ElseBranch.ElseBranch(elseTok, expr)))
            | Error e -> Error e

        | Error e -> Ok struct (List.ofSeq acc, ValueNone)

    let parse: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        fun reader -> parseBranches (ResizeArray()) reader

module Binding =
    let private pMutableTok =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    /// Build the Pat head from an IdentOrOp
    let private headPatOfIdentOrOp (identOrOp: IdentOrOp<SyntaxToken>) : Pat<SyntaxToken> =
        match identOrOp with
        | IdentOrOp.Ident t -> Pat.NamedSimple t
        | _ -> Pat.Op identOrOp

    /// Parse a function-style binding: [inline] [access] identOrOp [typar-defns] pat+ [: returnType] = expr
    let parseFunction attrs =
        let pBody = refExprSeqBlock.Parser

        parser {
            let! inlineTok = opt pInline
            let! access = opt pAccessModifier
            let! identOrOp = IdentOrOp.parse
            let! typarDefns = opt TyparDefns.parse
            // Parse argument patterns (atomic to avoid consuming return type annotations).
            // Operator definitions (e.g., `let (|PointFree|) = expr`) allow zero arguments.
            // Generic value definitions (e.g., `let inline f<'T> : Type = expr`) also allow zero arguments.
            // Named function definitions require at least one argument.
            let! argumentPats =
                match identOrOp with
                | IdentOrOp.ParenOp _
                | IdentOrOp.StarOp _ -> many Pat.parseAtomic
                | IdentOrOp.Ident _ when typarDefns.IsSome -> many Pat.parseAtomic
                | IdentOrOp.Ident _ -> Pat.parseAtomicMany1

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
                    argumentPats = List.ofSeq argumentPats
                    returnType = returnType
                    equals = equals
                    expr = expr
                }
        }

    /// Parse a value-style binding: [mutable] [access] pat [typar-defns] [: returnType] = expr
    let parseValue attrs =
        let pBody = refExprSeqBlock.Parser

        parser {
            let! mut = opt pMutableTok
            let! access = opt pAccessModifier
            let! pat = Pat.parseAtomicOrTuple
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
            let! expr = withContext OffsideContext.SeqBlock refExprInRecords.Parser
            return FieldInitializer(id, equals, expr)
        }

    let parseSepBySemi1 = sepBy1 parse pSemi

[<RequireQualifiedAccess>]
module ObjectConstruction =
    // Dummy function to ensure the static constructor runs and initializes refObjectConstruction
    let mutable private x = 0
    let init () = x <- x + 1

    let parse: Parser<ObjectConstruction<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! typ = Type.parse

            // Check for constructor arguments: parens, literals, or other expression starters.
            // Attributes can use `[<Attr "str">]` or `[<Attr(args)>]` syntax.
            let! argExpr =
                opt (
                    parser {
                        let! token = lookAhead nextNonTriviaToken

                        if
                            token.Token = Token.KWLParen
                            || TokenInfo.isLiteral token.Token
                            || token.Token = Token.StringOpen
                            || token.Token = Token.VerbatimStringOpen
                            || token.Token = Token.String3Open
                        then
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

        let completeFor _forTok (expr: ExprAux) =
            match expr with
            | ExprAux.ForExpr forExpr -> forExpr
            | _ -> failwith "Unexpected Aux type for For expression completion"

        let completeSequence (exprs: ResizeArray<Expr<_>>) ops =
            Expr.Sequential(List.ofSeq exprs, List.ofSeq ops)

        let completeAssignment (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.Assignment(l, op, r)
        let completePrefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let completeLazy (op: SyntaxToken) (e: Expr<_>) = Expr.Lazy(op, e)
        let completeAssert (op: SyntaxToken) (e: Expr<_>) = Expr.Assert(op, e)
        let completeUpcast (op: SyntaxToken) (e: Expr<_>) = Expr.Upcast(op, e)
        let completeDowncast (op: SyntaxToken) (e: Expr<_>) = Expr.Downcast(op, e)
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

        let pIdentAfterDot = nextNonTriviaIdentifierL "Expected identifier after '.'"

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
            | ExprAux.DotParenOp identOrOp ->
                match expr with
                | Expr.Ident firstIdent -> Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp([ firstIdent ], op, identOrOp))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdent) ->
                    Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent, op, identOrOp))
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.Op identOrOp)
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

        let pApplication =
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
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.EOF -> return! failApp
                | t ->
                    let! indent = currentIndent
                    let! state = getUserState

                    if not (isAtomicExprToken state t) then
                        return! failApp
                    else
                        let atAppIndent =
                            match state.Context with
                            | { Indent = ctxIndent } :: _ -> indent > ctxIndent
                            | [] -> indent > 0

                        if atAppIndent then
                            return virtualToken (PositionedToken.Create(Token.VirtualApp, t.StartIndex))
                        else
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
                        fail (Message "LHS-only operator cannot appear in infix position")
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
        let pIdentTok = nextNonTriviaIdentifierL "identifier"

        // Shared complete function for all PrefixMapped keyword forms.
        let completeKeyword (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.KeywordExpr e -> e op
            | _ -> failwith "Unexpected Aux type for keyword expression"

        // Keyword expression parsers. Each parser consumes its own keyword via assertKeywordToken
        // and uses withContextAt to set the offside indent based on the keyword's column.
        // lhsParser peeks but does NOT consume — the parser handles consumption.

        let pIfExpr =
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
                let! thenExpr =
                    recoverExprMissing (
                        withContextAt OffsideContext.Then (indent + 1) ifTok.PositionedToken refExprSeqBlock.Parser
                    )

                let! elifs, elseBranch = ElifBranches.parse

                return ExprAux.ForExpr(Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, List.ofSeq elifs, elseBranch))
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
                        Rules(ValueNone, [ missing ], [])
                    else
                        let missingWithSkips =
                            Rule.Rule(
                                Pat.SkipsTokens(toks),
                                ValueNone,
                                virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                Expr.Missing
                            )

                        Rules(ValueNone, [ missingWithSkips ], [])
                )

        let pMatchExpr =
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

            let parseMatchBody
                (matchTok: SyntaxToken)
                (indent: int)
                (reader: Reader<PositionedToken, ParseState, _, _>)
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

            fun (reader: Reader<PositionedToken, ParseState, _, _>) ->
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
            let pBody = refExprSeqBlock.Parser

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
                            return Expr.Fun(funTok, List.ofSeq pats, arrow, expr)
                        })

                return ExprAux.ForExpr result
            }

        let pTryExpr =
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
                            Rules(ValueNone, [ missing ], [])
                        else
                            let missingWithSkips =
                                Rule.Rule(
                                    Pat.SkipsTokens(toks),
                                    ValueNone,
                                    virtualToken (PositionedToken.Create(Token.OpArrowRight, 0)),
                                    Expr.Missing
                                )

                            Rules(ValueNone, [ missingWithSkips ], [])
                    )

            // Manually manage the Try context so it stays active through with/finally parsing,
            // similar to how parseMatchBody keeps Match context active for | undentation.
            fun (reader: Reader<PositionedToken, ParseState, _, _>) ->
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
                    match recoverExprMissing refExprSeqBlock.Parser reader with
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
                                let! finExpr = refExprSeqBlock.Parser
                                return Expr.TryFinally(tryTok, tryExpr, finTok, finExpr)
                            }

                        match
                            dispatchNextNonTriviaTokenL
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

        let pWhileExpr =
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
                let! body =
                    recoverExprMissing (
                        withContextAt OffsideContext.Do indent whileTok.PositionedToken refExprSeqBlock.Parser
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
                match peekNextNonTriviaToken reader with
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
                let! body = withContextAt OffsideContext.Do bodyMinIndent forTok.PositionedToken refExprSeqBlock.Parser
                let! doneTok = pDoneVirt
                return ExprAux.ForExpr(forBuilder forTok body doneTok)
            }

        let pLetOrUseIn letIndent (reader: Reader<PositionedToken, ParseState, _, _>) =
            match peekNextNonTriviaToken reader with
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
                    fail (Message "Expected 'in' at the same indent as 'let'") reader
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
                    let! bindings = Binding.parseSepByAnd1 ValueNone
                    return struct (recTok, bindings)
                })

        /// Build nested `Expr.LetOrUse` from an accumulated list of bindings (innermost last)
        /// and a body expression. The fold runs backwards so that the first binding wraps the outermost scope.
        let buildNestedLetOrUse
            (bindings: ResizeArray<struct (SyntaxToken * SyntaxToken voption * _ * SyntaxToken)>)
            (body: Expr<SyntaxToken> voption)
            =
            let mutable result = body

            for i in bindings.Count - 1 .. -1 .. 0 do
                let struct (kwTok, recTok, defs, inTok) = bindings[i]

                let kw =
                    match kwTok.Token with
                    | Token.KWLet -> LetOrUseKeyword.Let kwTok
                    | Token.KWUse -> LetOrUseKeyword.Use kwTok
                    | Token.KWLetBang -> LetOrUseKeyword.LetBang kwTok
                    | Token.KWUseBang -> LetOrUseKeyword.UseBang kwTok
                    | t -> failwith $"Unexpected keyword token for let/use body {t}"

                result <- ValueSome(Expr.LetOrUse(kw, recTok, defs, ValueSome inTok, result))

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
            fun (reader: Reader<PositionedToken, ParseState, _, _>) ->
                let bindings = ResizeArray()
                let mutable cont = true
                let mutable error = Unchecked.defaultof<ParseResult<ExprAux, _, _>>

                while cont do
                    match peekNextNonTriviaToken reader with
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
                                | Ok(recTok, defs) ->
                                    match pLetOrUseIn indent reader with
                                    | Error e ->
                                        error <- Error e
                                        cont <- false
                                    | Ok inTok -> bindings.Add(struct (kwTok, recTok, defs, inTok))
                        | _ ->
                            if bindings.Count = 0 then
                                error <-
                                    Parsers.fail (Message $"Expected let/use keyword but got {peeked.Token}") reader

                            cont <- false

                if bindings.Count = 0 then
                    error
                else
                    // Committed after consuming let/use bindings — recover body expression if it fails
                    let body =
                        match refExprSeqBlock.Parser reader with
                        | Ok body -> ValueSome body
                        | Error err ->
                            let struct (lastKw, _, _, _) = bindings[bindings.Count - 1]

                            match lastKw.Token with
                            | Token.KWUse
                            | Token.KWUseBang ->
                                // use/use! with no body is valid F# (e.g. `use _ = expr` at end of scope)
                                ValueNone
                            | _ ->
                                reader.State <-
                                    ParseState.addDiagnostic
                                        DiagnosticCode.MissingExpression
                                        DiagnosticSeverity.Error
                                        (match peekNextNonTriviaToken reader with
                                         | Ok tok -> tok.PositionedToken
                                         | Error _ -> PositionedToken.Create(Token.EOF, 0))
                                        None
                                        (Some err)
                                        reader.State

                                ValueSome Expr.Missing

                    let result = buildNestedLetOrUse bindings body
                    preturn (ExprAux.KeywordExpr(fun _opTok -> result)) reader

        let pYieldReturnDoBody =
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
                    refExprSeqBlock.Parser

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
                            completeKeyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWAssert ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Assert(kwTok, e))),
                            completeKeyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWUpcast ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Upcast(kwTok, e))),
                            completeKeyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.KWDowncast ->
                    let! tok = consumePeeked token

                    return
                        PrefixMapped(
                            tok,
                            preturn tok,
                            (refExprSeqBlock.Parser
                             |>> fun e -> ExprAux.KeywordExpr(fun kwTok -> Expr.Downcast(kwTok, e))),
                            completeKeyword
                        )
                | ValueSome opInfo when opInfo.Token = Token.OpRange ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    // A[..5]
                    return Prefix(tok, preturn tok, power, completeSliceTo)
                | ValueSome opInfo when opInfo.Token = Token.OpMultiply ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    // A[0..1,*]
                    return PrefixMapped(token, preturn tok, peekIsSliceAll, completeSliceAll)
                // & and && are address-of operators when used as prefix.
                // Must be checked before the generic CanBePrefix handler to use
                // PrecedenceLevel.Prefix instead of their infix precedence (LogicalAnd).
                | ValueSome opInfo when opInfo.Token = Token.OpAmp || opInfo.Token = Token.OpAmpAmp ->
                    let! tok = consumePeeked token
                    let power = BindingPower.fromLevel (int PrecedenceLevel.Prefix)
                    return Prefix(tok, preturn tok, power, completePrefix)
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
                    return Prefix(tok, preturn tok, power, completePrefix)
                | _ -> return! fail (Message "Not a prefix operator")
            }

        let lhsParser =
            parser {
                let! token = peekNextNonTriviaToken

                // Keyword-prefix forms: these tokens are not in isOperatorKeyword, so OperatorInfo.TryCreate
                // returns ValueNone for them. Match directly on the token type instead.
                // We peek but do NOT consume — the body parser handles consumption inside withContext.
                match token.Token with
                | Token.KWIf ->
                    // Do NOT consume here — pIfExpr peeks the 'if' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pIfExpr, completeFor)
                | Token.KWMatch
                | Token.KWMatchBang ->
                    // Do NOT consume here — pMatchExpr peeks the 'match' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pMatchExpr, completeFor)
                | Token.KWFunction ->
                    // Do NOT consume here — pFunctionExpr peeks the 'function' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pFunctionExpr, completeFor)
                | Token.KWFun ->
                    // Do NOT consume here — pFunExpr peeks the 'fun' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pFunExpr, completeFor)
                | Token.KWTry ->
                    // Do NOT consume here — pTryExpr peeks the 'try' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pTryExpr, completeFor)
                | Token.KWWhile ->
                    // Do NOT consume here — pWhileExpr peeks the 'while' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pWhileExpr, completeFor)
                | Token.KWFor ->
                    // Do NOT consume here — pForBody peeks the 'for' keyword to set the context indent, then consumes.
                    return PrefixMapped(token, preturn token, pForExpr, completeFor)
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
                | _ -> return! pOperatorPrefix token
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
            match peekNextNonTriviaToken reader with
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
                    match peekNextNonTriviaToken reader with
                    | Ok next when next.Token = Token.InterpolatedExpressionOpen ->
                        match consumePeeked next reader with
                        | Error e -> Error e
                        | Ok lBrace ->
                            match refExpr.Parser reader with
                            | Error e -> Error e
                            | Ok expr ->
                                let formatClause =
                                    match peekNextNonTriviaToken reader with
                                    | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                        consumePeeked fc reader |> ignore
                                        ValueSome fc
                                    | _ -> ValueNone

                                match nextNonTriviaTokenIsL Token.InterpolatedExpressionClose "Expected }" reader with
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
                            match peekNextNonTriviaToken reader with
                            | Ok fc when fc.Token = Token.InterpolatedFormatClause ->
                                consumePeeked fc reader |> ignore
                                ValueSome fc
                            | _ -> ValueNone

                        match nextNonTriviaTokenIsL Token.InterpolatedExpressionClose "Expected }" reader with
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
            let! opening = nextNonTriviaTokenSatisfiesL (fun t -> isStringOpen t.Token) "Expected string open"

            let kind = stringKindOfToken opening

            let isInterpolated =
                match kind with
                | StringKind.InterpolatedString _
                | StringKind.VerbatimInterpolatedString _
                | StringKind.Interpolated3String _ -> true
                | _ -> false

            let! parts = loop isInterpolated (ResizeArray())

            let! closing = nextNonTriviaTokenSatisfiesL (fun t -> isStringClose t.Token) "Expected string close"

            return Expr.String(kind, Seq.toList parts, closing)
        }

    let pIdentExpr = nextNonTriviaIdentifierL "Expected identifier" |>> Expr.Ident

    let private pEnclosed =
        let completeEmpty l r = Expr.EmptyBlock(l, r)
        let completeEnclosed l e r = Expr.EnclosedBlock(l, e, r)
        let skipsTokens toks = Expr.SkipsTokens(toks)
        pEnclosed completeEmpty completeEnclosed Expr.Missing skipsTokens

    let private pExprOrTypedPat =
        let pInnerExpr = refExprSeqBlock.Parser

        parser {
            let! expr = pInnerExpr

            match! peekNextNonTriviaToken with
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

    let pParen =
        choiceL
            [
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
                            // '{' new base-call object-members interface-impls '}' -- object expression
                            parser {
                                let! newTok = pNew
                                let! construction = ObjectConstruction.parse

                                let! baseCall =
                                    opt (
                                        parser {
                                            let! asTok = pAs
                                            let! ident = nextNonTriviaIdentifierL "identifier"
                                            return struct (asTok, ident)
                                        }
                                    )

                                let baseCall =
                                    match baseCall with
                                    | ValueSome struct (asTok, ident) ->
                                        BaseCall.NamedBaseCall(construction, asTok, ident)
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

                                                let! intfNextTokOpt = opt peekNextNonTriviaToken

                                                let intfVirtualEnd =
                                                    {
                                                        PositionedToken =
                                                            PositionedToken.Create(
                                                                Token.ofUInt16 (
                                                                    uint16 Token.KWEnd ||| TokenRepresentation.IsVirtual
                                                                ),
                                                                match intfNextTokOpt with
                                                                | ValueSome tok -> tok.StartIndex
                                                                | ValueNone -> 0
                                                            )
                                                        Index = TokenIndex.Virtual
                                                    }

                                                let objMembers =
                                                    ObjectMembers.ObjectMembers(
                                                        wTok,
                                                        List.ofSeq intfMembers,
                                                        intfVirtualEnd
                                                    )

                                                return
                                                    InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueSome objMembers)
                                            | ValueNone ->
                                                return InterfaceImpl.InterfaceImpl(interfaceTok, typ, ValueNone)
                                        }
                                    )

                                // Synthesize virtual end for the main ObjectMembers
                                let! nextTokOpt = opt peekNextNonTriviaToken

                                let virtualEnd =
                                    {
                                        PositionedToken =
                                            PositionedToken.Create(
                                                Token.ofUInt16 (uint16 Token.KWEnd ||| TokenRepresentation.IsVirtual),
                                                match nextTokOpt with
                                                | ValueSome tok -> tok.StartIndex
                                                | ValueNone -> 0
                                            )
                                        Index = TokenIndex.Virtual
                                    }

                                let objMembers =
                                    ObjectMembers.ObjectMembers(withTok, List.ofSeq members, virtualEnd)

                                let! rBrace = nextNonTriviaTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                                return
                                    Expr.Object(
                                        lBrace,
                                        ValueSome newTok,
                                        baseCall,
                                        objMembers,
                                        List.ofSeq interfaceImpls,
                                        rBrace
                                    )
                            }
                            // { expr with Field = val; ... } — record clone/update
                            parser {
                                let! baseExpr = refExprInRecords.Parser
                                let! withTok = pWith

                                let! fields, _ =
                                    withContext OffsideContext.SeqBlock (sepBy1 FieldInitializer.parse pRecordFieldSep)

                                let! rBrace = pRBrace
                                return Expr.RecordClone(lBrace, baseExpr, withTok, List.ofSeq fields, rBrace)
                            }
                            // { Field = val; ... } — record literal
                            parser {
                                let! fields, _ =
                                    withContext OffsideContext.SeqBlock (sepBy1 FieldInitializer.parse pRecordFieldSep)

                                let! rBrace = pRBrace
                                return Expr.Record(lBrace, List.ofSeq fields, rBrace)
                            }
                            // Fallback: { expr-seq } — computation expression body without leading CE keyword
                            // (e.g., seq { someExpr }, task { callAsync() })
                            parser {
                                let! body = refExprSeqBlock.Parser

                                let! rBrace = nextNonTriviaTokenVirtualWithDiagnostic (ValueSome lBrace) Token.KWRBrace

                                return Expr.EnclosedBlock(ParenKind.Brace lBrace, body, rBrace)
                            }
                        ]
                        "Record or RecordClone"

                match innerParser reader with
                | Ok result ->
                    reader.State <- ParseState.popOffside entry reader.State
                    Ok result
                | Error _ ->
                    reader.State <- savedState
                    fail (Message "Record or RecordClone") reader

    let private pILIntrinsic =
        // Structured IL intrinsic parser: (# "instr" type('T) args : retType #)
        let pAnyToken = nextNonTriviaTokenSatisfiesL (fun _ -> true) "IL intrinsic token"

        let pTypeArg (reader: Reader<PositionedToken, ParseState, _, _>) =
            // Parse type('T) or type ('T) — balanced parens after 'type' keyword
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok tok when tok.Token = Token.KWType ->
                match consumePeeked tok reader with
                | Error e -> Error e
                | Ok typeKw ->
                    match nextNonTriviaTokenIsL Token.KWLParen "(" reader with
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
                        | ValueNone -> preturn (ValueSome(ILTypeArg(typeKw, lParen, List.ofSeq tokens, rParen))) reader
            | _ -> preturn ValueNone reader

        fun (reader: Reader<PositionedToken, ParseState, _, _>) ->
            match nextNonTriviaTokenIsL Token.KWLHashParen "(#" reader with
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
                        // Collect args as atomic expressions, stopping at ':' or '#)'
                        let args = ResizeArray()
                        let mutable finished = false
                        let mutable error = ValueNone

                        while not finished && error = ValueNone do
                            match peekNextNonTriviaToken reader with
                            | Error e -> error <- ValueSome e
                            | Ok tok ->
                                match tok.Token with
                                | Token.KWRHashParen
                                | Token.OpColon -> finished <- true
                                | _ ->
                                    match refExprAtomic.Parser reader with
                                    | Error e -> error <- ValueSome e
                                    | Ok arg -> args.Add(arg)

                        match error with
                        | ValueSome e -> Error e
                        | ValueNone ->
                            // Optional return type annotation (: Type)
                            match opt ReturnType.parse reader with
                            | Error e -> Error e
                            | Ok returnType ->
                                match nextNonTriviaTokenIsL Token.KWRHashParen "#)" reader with
                                | Error e -> Error e
                                | Ok rHashParen ->
                                    preturn
                                        (Expr.ILIntrinsic(
                                            lHashParen,
                                            instrKind,
                                            instrParts,
                                            instrClose,
                                            typeArg,
                                            List.ofSeq args,
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
            let! ident = nextNonTriviaIdentifierL "Expected identifier after '?'"
            return Expr.OptionalArgExpr(qmark, ident)
        }

    let parseAtomic =
        dispatchNextNonTriviaTokenFallback
            [
                // TODO: Performance, sort this by frequency and put most common cases first
                Token.Identifier, pIdentExpr
                Token.BacktickedIdentifier, pIdentExpr
                Token.UnterminatedBacktickedIdentifier, pIdentExpr
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
                Token.InterpolatedStringOpen, pString
                Token.VerbatimInterpolatedStringOpen, pString
                Token.Interpolated3StringOpen, pString
                Token.StringOpen, pString
                Token.VerbatimStringOpen, pString
                Token.String3Open, pString
                Token.KWLHashParen, pILIntrinsic
                Token.KWBase, (nextNonTriviaTokenIsL Token.KWBase "base" |>> Expr.Ident)
                Token.Wildcard, (nextNonTriviaTokenIsL Token.Wildcard "_" |>> Expr.Wildcard)
                Token.OpDynamic, pOptionalArgExpr
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

        // Single expression without sequential composition — used for while conditions
        // and other contexts where the next keyword (e.g. `do`) must not be consumed as
        // a keyword expression via newline-triggered virtual semicolons.
        refExprNoSeq.Set(
            Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) parseAtomic operators
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
