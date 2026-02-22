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


module ElifBranch =
    let private pElif = nextNonTriviaTokenIsL Token.KWElif "Expected 'elif' keyword"

    let parse: Parser<ElifBranch<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! elifTok = pElif
            let! condition = refExpr.Parser
            let! thenTok = pThen
            let! expr = pSeqBlock refExpr.Parser
            return ElifBranch.ElifBranch(elifTok, condition, thenTok, expr)
        }

module ElseBranch =
    let parse: Parser<ElseBranch<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! elseTok = pElse
            let! expr = pSeqBlock refExpr.Parser
            return ElseBranch.ElseBranch(elseTok, expr)
        }

module FunctionDefn =
    let parse: Parser<FunctionDefn<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! inlineTok = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWInline) "Expected 'inline'")
            let! access = opt pAccessModifier
            let! identOrOp = IdentOrOp.parse
            let! typarDefns = opt TyparDefns.parse

            // Parse one or more argument patterns
            let! argumentPats = many1 (parser { return! Pat.parse })

            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pSeqBlock refExpr.Parser

            return
                FunctionDefn.FunctionDefn(
                    inlineTok,
                    access,
                    identOrOp,
                    typarDefns,
                    List.ofSeq argumentPats,
                    returnType,
                    equals,
                    expr
                )
        }

module ValueDefn =
    let pMutable: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    let parse =
        parser {
            let! mut = opt pMutable
            let! access = opt pAccessModifier
            let! pat = Pat.parse
            let! typarDefns = opt TyparDefns.parse
            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = pSeqBlock refExpr.Parser
            return ValueDefn.ValueDefn(mut, access, pat, typarDefns, returnType, equals, expr)
        }

module FunctionOrValueDefn =
    let parse: Parser<FunctionOrValueDefn<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                FunctionDefn.parse |>> FunctionOrValueDefn.Function
                ValueDefn.parse |>> FunctionOrValueDefn.Value
            ]
            "FunctionOrValueDefn"

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
            let! equals = pEquals // Defined in previous keywords module
            let! expr = refExpr.Parser
            return FieldInitializer.FieldInitializer(id, equals, expr)
        }

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

                        if token.Token = Token.KWLParen || token.Token = Token.Unit then
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


[<RequireQualifiedAccess>]
module SliceRange =

    let private pAll = pOpMultiply |>> SliceRange.All

    let private pTo =
        parser {
            let! dd = pRange
            let! e = refExpr.Parser
            return SliceRange.To(dd, e)
        }

    let private pFromOrSingleOrFromTo =
        parser {
            let! start = refExpr.Parser

            // Check for '..'
            let! dd = opt pRange

            match dd with
            | ValueNone -> return SliceRange.Single(start)
            | ValueSome dotdot ->
                // We have 'start ..', check if there is an end expression.
                // In slice syntax like a[1..], the end is optional.
                // We try to parse an expression. If it fails (e.g. hits ']'), it's a From.
                // Note: We use 'opt refExpr.Parser'. This relies on refExpr.Parser failing gracefully
                // if it encounters a closing delimiter immediately.
                let! endExpr = opt refExpr.Parser

                match endExpr with
                | ValueSome finish -> return SliceRange.FromTo(start, dotdot, finish)
                | ValueNone -> return SliceRange.From(start, dotdot)
        }

    let parse: Parser<SliceRange<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pAll; pTo; pFromOrSingleOrFromTo ] "SliceRange"


[<RequireQualifiedAccess>]
module RangeExpr =
    // Parses a full range expression: start .. end OR start .. step .. end
    // Assumes the caller has NOT parsed the start expression yet.
    let parse: Parser<RangeExpr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! start = refExpr.Parser
            let! dd1 = pRange
            let! middle = refExpr.Parser

            // Check for second '..'
            let! dd2 = opt pRange

            match dd2 with
            | ValueSome dotdot2 ->
                // start .. step .. end
                let step = middle
                let! finish = refExpr.Parser
                return RangeExpr.SteppedRange(start, dd1, step, dotdot2, finish)
            | ValueNone ->
                // start .. end
                let finish = middle
                return RangeExpr.SimpleRange(start, dd1, finish)
        }

[<RequireQualifiedAccess>]
module ExprOrRange =
    // Parses either a RangeExpr or a standard Expr.
    // This is commonly used in list/array constructors e.g. [ 1..10 ] or [ 1; 2; 3 ]
    let parse: Parser<ExprOrRange<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! start = refExpr.Parser

            // Check for '..' indicating the start of a range
            let! dd1 = opt pRange

            match dd1 with
            | ValueNone -> return ExprOrRange.Expr(start)
            | ValueSome dotdot1 ->
                // It is a range
                let! middle = refExpr.Parser

                let! dd2 = opt pRange

                match dd2 with
                | ValueSome dotdot2 ->
                    let step = middle
                    let! finish = refExpr.Parser
                    return ExprOrRange.Range(RangeExpr.SteppedRange(start, dotdot1, step, dotdot2, finish))
                | ValueNone ->
                    let finish = middle
                    return ExprOrRange.Range(RangeExpr.SimpleRange(start, dotdot1, finish))
        }

[<RequireQualifiedAccess>]
module CompExpr =

    // Recursive reference for CompExpr
    let private refCompExpr = FSRefParser<CompExpr<SyntaxToken>>()

    // --- Specific Clause Parsers ---


    let private pLetBangCE =
        parser {
            let! letBang = pLetBang
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.LetBang(letBang, pat, eq, expr, inTok, comp)
        }

    let private pLetCE =
        parser {
            let! letTok = pLet // defined in global keywords
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Let(letTok, pat, eq, expr, inTok, comp)
        }

    let private pUseBangCE =
        parser {
            let! useBang = pUseBang
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.UseBang(useBang, pat, eq, expr, inTok, comp)
        }

    let private pUseCE =
        parser {
            let! useTok = pUse
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Use(useTok, pat, eq, expr, inTok, comp)
        }

    let private pDoBangCE =
        parser {
            let! doBang = pDoBang
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.DoBang(doBang, expr, inTok, comp)
        }

    let private pDoCE =
        parser {
            let! doTok = pDo
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Do(doTok, expr, inTok, comp)
        }

    let private pYieldBangCE =
        parser {
            let! t = pYieldBang
            let! e = refExpr.Parser
            return CompExpr.YieldBang(t, e)
        }

    let private pYieldCE =
        parser {
            let! t = pYield
            let! e = refExpr.Parser
            return CompExpr.Yield(t, e)
        }

    let private pReturnBangCE =
        parser {
            let! t = pReturnBang
            let! e = refExpr.Parser
            return CompExpr.ReturnBang(t, e)
        }

    let private pReturnCE =
        parser {
            let! t = pReturn
            let! e = refExpr.Parser
            return CompExpr.Return(t, e)
        }

    let private pMatchBangCE =
        parser {
            let! m = pMatchBang
            let! e = refExpr.Parser
            let! w = pWith
            let! r = Rules.parse
            return CompExpr.MatchBang(m, e, w, r)
        }

    let private pMatchCE =
        parser {
            let! m = pMatch
            let! e = refExpr.Parser
            let! w = pWith
            let! r = Rules.parse
            return CompExpr.Match(m, e, w, r)
        }

    let private pIfCE =
        parser {
            let! ifTok = pIf
            let! cond = refExpr.Parser
            let! thenTok = pThen
            // Check for else
            // Per AST: IfThenElse takes Expr for 'then' branch, CompExpr for 'else' branch
            // IfThen takes CompExpr for 'then' branch

            // We attempt to parse an else block to decide
            // This logic assumes we can look ahead or backtrack

            return!
                choice
                    [
                        parser {
                            let! thenExpr = refExpr.Parser
                            let! elseTok = pElse
                            let! elseComp = refCompExpr.Parser
                            return CompExpr.IfThenElse(ifTok, cond, thenTok, thenExpr, elseTok, elseComp)
                        }
                        parser {
                            let! thenComp = refCompExpr.Parser
                            return CompExpr.IfThen(ifTok, cond, thenTok, thenComp)
                        }
                    ]
        }

    let private pTryCE =
        parser {
            let! tryTok = pTry
            let! comp = refCompExpr.Parser

            return!
                choice
                    [
                        parser {
                            let! withTok = pWith
                            let! rules = Rules.parse
                            return CompExpr.TryWith(tryTok, comp, withTok, rules)
                        }
                        parser {
                            let! finTok = pFinally
                            let! finExpr = refExpr.Parser
                            return CompExpr.TryFinally(tryTok, comp, finTok, finExpr)
                        }
                    ]
        }

    let private pWhileCE =
        parser {
            let! w = pWhile
            let! cond = refExpr.Parser
            let! d = pDo
            let! body = refCompExpr.Parser
            let! doneTok = pDone
            return CompExpr.While(w, cond, d, body, doneTok)
        }

    // Distinguish ForTo vs ForIn
    // for ident = ... to ...
    // for pat in ...
    let private pForCE =
        parser {
            let! forTok = pFor

            return!
                choice
                    [
                        // ForTo: for i = 1 to 10 do
                        (parser {
                            let! ident = pIdent
                            let! eq = pEquals
                            let! start = refExpr.Parser
                            let! toTok = pToOrDownTo
                            let! endExpr = refExpr.Parser
                            let! doTok = Keywords.pDo
                            let! comp = refCompExpr.Parser
                            let! doneTok = pDone
                            return CompExpr.ForTo(forTok, ident, eq, start, toTok, endExpr, doTok, comp, doneTok)
                        })
                        // ForIn: for x in xs do
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pInVirt
                            let! exprOrRange = ExprOrRange.parse
                            let! doTok = Keywords.pDo
                            let! comp = refCompExpr.Parser
                            let! doneTok = pDone
                            return CompExpr.ForIn(forTok, pat, inTok, exprOrRange, doTok, comp, doneTok)
                        }
                    ]
        }

    let private pBaseExpr = refExpr.Parser |>> CompExpr.BaseExpr

    let private pAtomComp =
        choiceL
            [
                pLetBangCE
                pUseBangCE
                pDoBangCE
                pYieldBangCE
                pReturnBangCE
                pMatchBangCE
                // Check specific keywords before checking BaseExpr
                // Note: 'let', 'use', 'do' are in the keyword list but parsed specifically here for CE structure
                pLetCE
                pUseCE
                pDoCE
                pYieldCE
                pReturnCE
                pIfCE
                pMatchCE
                pTryCE
                pWhileCE
                pForCE
                // Fallback
                pBaseExpr
            ]
            "Computation Expression Atom"

    // Top Level: Handles Sequential composition
    // comp := atom ; comp | atom
    let parse =
        parser {
            let! head = pAtomComp

            // Check for semicolon
            let! semi = opt pSemi

            match semi with
            | ValueSome s ->
                let! tail = refCompExpr.Parser // Recursive
                return CompExpr.Sequential(head, s, tail)
            | ValueNone -> return head
        }

    do refCompExpr.Set parse

[<RequireQualifiedAccess>]
module ShortCompExpr =
    // for pat in expr -> expr
    let parse =
        parser {
            let! forTok = pFor
            let! pat = Pat.parse
            let! inTok = pInVirt
            let! range = ExprOrRange.parse
            let! arrow = pArrowRight
            let! expr = refExpr.Parser
            return ShortCompExpr.ShortCompExpr(forTok, pat, inTok, range, arrow, expr)
        }

[<RequireQualifiedAccess>]
module CompOrRangeExpr =
    let parse: Parser<CompOrRangeExpr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                // ShortComp starts with 'for' ... '->'
                // Standard Comp 'ForIn' starts with 'for' ... 'do'
                // We use attempt on ShortComp because they share 'for pat in expr' prefix
                (ShortCompExpr.parse |>> CompOrRangeExpr.ShortComp)

                // Range: expr .. expr
                // CompExpr: includes BaseExpr(expr)
                // Ambiguity: [ 1 .. 10 ] (Range) vs [ 1 ] (Comp->BaseExpr)
                // RangeExpr parser expects 'start ..', it doesn't parse just 'start'.
                // However, CompExpr includes BaseExpr which parses 'start'.
                // If we use RangeExpr.parse, it internally calls refExpr.Parser then looks for '..'.
                // If '..' is not found, RangeExpr.parse fails (based on previous implementation).
                // So we can try RangeExpr first.
                (RangeExpr.parse |>> CompOrRangeExpr.Range)

                // Fallback to full Computation Expression
                (CompExpr.parse |>> CompOrRangeExpr.Comp)
            ]
            "CompOrRangeExpr"

[<RequireQualifiedAccess>]
type ExprAux =
    | Ident of SyntaxToken
    | TypeApp of Type<SyntaxToken> list * SyntaxToken
    | DotIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // .[ expr ]
    | DotSlice of SyntaxToken * SliceRange<SyntaxToken> list * SyntaxToken // .[ 1.. ]
    | ComputationBlock of SyntaxToken * CompOrRangeExpr<SyntaxToken> * SyntaxToken // { ... }
    | PostfixDynamic of SyntaxToken * Type<SyntaxToken> // :? Type (DynamicTypeTest)

[<RequireQualifiedAccess>]
module Expr =
    let bp x =
        LanguagePrimitives.ByteWithMeasure<bp> x

    let pl x : PrecedenceLevel = LanguagePrimitives.EnumOfValue x

    type ExprOperatorParser() =
        let completeInfix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)
        let completeSemicolon (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.Sequential(l, op, r)
        let completePrefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let completeTuple (elements: ResizeArray<Expr<_>>) ops = Expr.Tuple(List.ofSeq elements)

        let printOpInfo (op: OperatorInfo) =
            printfn
                $"Operator: {op.PositionedToken}({op.StartIndex}), Precedence: {op.Precedence}, Associativity: %A{op.Associativity}"

        let pIdent = nextNonTriviaTokenIsL Token.Identifier "Expected identifier after '.'"

        let parseDotRhs: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
            parser {
                // For now we just parse an identifier after the dot
                // Note: cannot use module-level pIdent here as that maps to Expr.Ident
                let! ident = pIdent

                return ExprAux.Ident ident
            }

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
            | _ -> failwith "Unexpected Aux type for dot completion"

        let pTypeAppRhs =
            parser {
                let! types, _ = sepBy Type.parse pComma

                let! state = getUserState

                let! rAngle =
                    nextNonTriviaTokenSatisfiesL
                        (fun t ->
                            match t.Token with
                            | Token.OpGreaterThan -> tokenStringIs ">" t state
                            | _ -> false
                        )
                        "Expected '>' for type application"
                    <|> parser {
                        let! (pos: Position<_>) = getPosition
                        let! token = nextNonTriviaToken
                        let! state = getUserState

                        if token.Token = Token.OpGreaterThan && tokenStringStartsWith ">" token state then
                            // We have an operator like '>>' or '>>=' that needs to be reprocessed
                            // after the type application that takes the first '>' as its left operand
                            let pos =
                                { pos with
                                    State =
                                        { pos.State with
                                            ReprocessOpAfterTypeDeclaration = true
                                        }
                                }

                            do! setPosition pos
                            return token
                        else
                            return! fail (Message "Expected '>' for type application") // Could be a different operator
                    }

                return ExprAux.TypeApp(List.ofSeq types, rAngle)
            }

        let completeTypeApp (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeApp(types, rAngle) -> Expr.TypeApp(expr, op, types, rAngle)
            | _ -> failwith "Unexpected Aux type for type application completion"

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
                29
                (fun i ->
                    let pl = pl i
                    let power = BindingPower.fromLevel i

                    match pl with
                    // Pattern only keywords
                    // | PrecedenceLevel.As -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // | PrecedenceLevel.When -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Pipe -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Semicolon ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeSemicolon))
                    // Atom keywords
                    // | PrecedenceLevel.Let -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.Function -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.If -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Arrow ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Assignment ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Comma -> (fun op -> InfixNary(op, preturn op, power, completeTuple))
                    | PrecedenceLevel.LogicalOr -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.LogicalAnd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    // TODO: Implement mapped infix operator and Aux type
                    // | PrecedenceLevel.Cast -> (fun op -> InfixMapped(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.LogicalAndBitwise -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Caret ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Cons ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // | PrecedenceLevel.TypeTest -> Pattern only operator :?
                    | PrecedenceLevel.InfixAdd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.InfixMultiply -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Power ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Application -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.PatternMatchBar -> Pattern only operator
                    // | PrecedenceLevel.Prefix -> LHS only
                    | PrecedenceLevel.Dot -> (fun op -> InfixMapped(op, preturn op, power, parseDotRhs, completeDot))
                    | PrecedenceLevel.HighApplication -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.HighTypeApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, pTypeAppRhs, completeTypeApp))
                    // | PrecedenceLevel.Parens -> LHS only
                    | _ -> Unchecked.defaultof<_>
                )

        let pApplication =
            // Treat whitespace followed by an atom as application
            // Technically also need to handle line breaks with proper indentation here and interstitial comments
            // This parser also need to handle high-precedence application f(x) and f<x>
            parser {
                let! i = getPosition

                let! token =
                    satisfyL (fun (t: PositionedToken) -> t.Token = Token.Whitespace) "Whitespace for application"

                do!
                    followedBy (fun reader ->
                        match reader.Peek() with
                        | ValueNone -> fail EndOfInput reader
                        | ValueSome t ->
                            if t.Token.IsIdentifier then
                                preturn () reader
                            elif t.Token.IsLiteral then
                                preturn () reader
                            else
                                match t.Token with
                                | Token.Unit
                                | Token.KWLParen
                                | Token.KWLBracket -> preturn () reader
                                | _ -> fail (Message "Expected expression after application whitespace") reader
                    )

                let t = syntaxToken token i.Index
                return t
            }

        let pTypeApplication =
            // Treat '<' followed by a type and '>' as type application
            parser {
                let! state = getUserState

                let! lAngle =
                    nextNonTriviaTokenSatisfiesL
                        (fun synTok ->
                            match synTok.Token with
                            | Token.OpLessThan -> tokenStringIs "<" synTok state
                            | _ -> false
                        )
                        "Expected '<' for type application"

                do!
                    followedBy (fun reader ->
                        match reader.Peek() with
                        | ValueNone -> fail EndOfInput reader
                        | ValueSome t ->
                            if t.Token.IsIdentifier then
                                preturn () reader
                            elif t.Token.IsLiteral then
                                preturn () reader
                            else
                                fail (Message "Expected type after '<' for type application") reader
                    )

                let typeAngle =
                    { lAngle with
                        PositionedToken =
                            PositionedToken.Create(
                                Token.ofUInt16 (
                                    TokenRepresentation.KindOperator
                                    ||| TokenRepresentation.Precedence.HighTypeApplication
                                ),
                                lAngle.PositionedToken.StartIndex
                            )
                    }

                return typeAngle
            }

        let rhsParser =
            // First try to parse application operator (whitespace) or high-precedence application
            // then try to parse explicit operator
            (pTypeApplication <|> pApplication <|> nextNonTriviaToken)
            >>= fun token ->
                match OperatorInfo.TryCreate token.PositionedToken with
                | ValueNone -> fail (Message "Expected RHS operator")
                | ValueSome opInfo ->
                    parser {
                        let! state = getUserState

                        if state.ReprocessOpAfterTypeDeclaration then
                            // We have an operator like '>>' or '>>=' that needs to be reprocessed
                            // after the type application that takes the first '>' to close the type application
                            do!
                                updateUserState (fun state ->
                                    { state with
                                        ReprocessOpAfterTypeDeclaration = false
                                    }
                                )

                            let reprocessedToken =
                                let newStart = token.StartIndex + 1 // Adjust start index to account for consumed '>'

                                let tokenString =
                                    match token.Index with
                                    | TokenIndex.Virtual -> failwith "Cannot re-lex virtual token"
                                    | TokenIndex.Regular iT ->
                                        let t1 = state.Lexed.Tokens[iT + 1<token>] // Next token after operator always exists as EOF is present
                                        state.Input.[newStart .. (t1.StartIndex - 1)]
                                // printfn "Re-lexing operator after type declaration: '%s'" tokenString
                                let t =
                                    match tokenString with
                                    | "" ->
                                        failwith "Unexpected empty operator string in ReprocessOpAfterTypeDeclaration"
                                    | "." ->
                                        // Special case for dot operator as it is common after type declarations
                                        Token.OpDot
                                    | _ ->
                                        // Need to re-lex to discover the correct operator token (mostly for precedence)
                                        match Lexing.lexString tokenString with
                                        | Error e -> failwithf "Failed to re-lex operator after type declaration %A" e
                                        | Ok lexed ->
                                            if lexed.Tokens.Length <> 2 then
                                                // Expect exactly two tokens: the operator and EOF
                                                failwithf
                                                    "Re-lexed operator did not produce exactly one token: %A"
                                                    lexed.Tokens
                                            else
                                                let relexedToken = lexed.Tokens[0<token>]
                                                relexedToken.Token

                                { token with
                                    PositionedToken = PositionedToken.Create(t, newStart)
                                }

                            match OperatorInfo.TryCreate reprocessedToken.PositionedToken with
                            | ValueNone -> return! fail (Message "Not a valid RHS operator after type declaration")
                            | ValueSome opInfo ->
                                // printOpInfo opInfo
                                let pl = opInfo.Precedence
                                let handler = rhsOperators[LanguagePrimitives.EnumToValue pl]

                                if obj.ReferenceEquals(handler, null) then
                                    return! fail (Message "Not a valid RHS operator after type declaration")
                                else
                                    let x = handler reprocessedToken

                                    if obj.ReferenceEquals(x, null) then
                                        return! fail (Message "Not a valid RHS operator after type declaration")
                                    else
                                        return x
                        else
                            // printOpInfo opInfo
                            let pl = opInfo.Precedence
                            let handler = rhsOperators[LanguagePrimitives.EnumToValue pl]

                            if obj.ReferenceEquals(handler, null) then
                                return! fail (Message "Not a valid RHS operator")
                            else
                                let x = handler token

                                if obj.ReferenceEquals(x, null) then
                                    return! fail (Message "Not a valid RHS operator")
                                else
                                    return x
                    }

        let lhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match OperatorInfo.TryCreate(token.PositionedToken) with
                | ValueSome opInfo when opInfo.CanBePrefix ->
                    // printOpInfo opInfo
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    let p = preturn token
                    let op = Prefix(token, p, power, completePrefix)
                    preturn op
                | _ -> fail (Message "Not a prefix operator")

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

    let private refExprInCollection = FSRefParser<Expr<SyntaxToken>>()

    let pConst = Constant.parse |>> Expr.Const

    let pIdent =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.Identifier) "Expected identifier"
        |>> Expr.Ident

    let pLetValue =
        parser {
            let! letTok = pLet
            let! valueDefn = ValueDefn.parse
            let! inTok = pInVirt
            let! expr = pSeqBlock refExpr.Parser
            return Expr.LetValue(letTok, valueDefn, inTok, expr)
        }

    let pParen =
        parser {
            let! l = pLParen
            let! e = refExpr.Parser
            let! r = pRParen
            return Expr.ParenBlock(l, e, r)
        }

    let pBeginEnd =
        parser {
            let! l = pBegin
            let! e = pSeqBlock refExpr.Parser
            let! r = pEnd
            return Expr.BeginEndBlock(l, e, r)
        }

    let pCollection openTok closeTok complete =
        parser {
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = openTok) $"Expected '{openTok}'"

            let! elems, seps = sepEndBy refExprInCollection.Parser pSemi

            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = closeTok) $"Expected '{closeTok}'"
            return complete l elems r
        }

    let pList =
        pCollection Token.KWLBracket Token.KWRBracket (fun l elems r -> Expr.List(l, List.ofSeq elems, r))

    let pArray =
        pCollection Token.KWLArrayBracket Token.KWRArrayBracket (fun l elems r -> Expr.Array(l, List.ofSeq elems, r))

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

    let pIfExpr =
        parser {
            let! ifTok = pIf
            let! cond = refExpr.Parser
            let! thenTok = pThen
            let! thenExpr = pSeqBlock refExpr.Parser
            let! elifs = many ElifBranch.parse
            let! elseBranch = opt ElseBranch.parse
            return Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, List.ofSeq elifs, elseBranch)
        }

    let pMatchExpr =
        parser {
            let! m = pMatch
            let! e = refExpr.Parser
            let! w = pWith
            let! rules = Rules.parse
            return Expr.Match(m, e, w, rules)
        }

    let pFunExpr =
        parser {
            let! funTok = pFun
            let! pats = many1 Pat.parse
            let! arrow = pArrowRight
            let! expr = pSeqBlock refExpr.Parser
            return Expr.Fun(funTok, List.ofSeq pats, arrow, expr)
        }

    let pTryExpr =
        parser {
            let! tryTok = pTry
            let! tryExpr = pSeqBlock refExpr.Parser

            return!
                choiceL
                    [
                        parser {
                            let! withTok = pWith
                            let! rules = Rules.parse
                            return Expr.TryWith(tryTok, tryExpr, withTok, rules)
                        }
                        parser {
                            let! finTok = pFinally
                            let! finExpr = pSeqBlock refExpr.Parser
                            return Expr.TryFinally(tryTok, tryExpr, finTok, finExpr)
                        }
                    ]
                    "Expected 'with' or 'finally'"
        }

    let pWhileExpr =
        parser {
            let! whileTok = pWhile
            let! cond = refExpr.Parser
            let! doTok = pDo
            let! body = pSeqBlock refExpr.Parser
            let! doneTok = pDone
            return Expr.While(whileTok, cond, doTok, body, doneTok)
        }

    let pIdentTok = nextNonTriviaTokenIsL Token.Identifier "identifier"

    let pForExpr =
        parser {
            let! forTok = pFor

            return!
                choiceL
                    [
                        // ForTo: for ident = start to/downto end do body done
                        parser {
                            let! ident = pIdentTok
                            let! eq = pEquals
                            let! startExpr = refExpr.Parser
                            let! toTok = pToOrDownTo
                            let! endExpr = refExpr.Parser
                            let! doTok = pDo
                            let! body = pSeqBlock refExpr.Parser
                            let! doneTok = pDone
                            return Expr.ForTo(forTok, ident, eq, startExpr, toTok, endExpr, doTok, body, doneTok)
                        }

                        // ForIn: for pat in expr do body done
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pInVirt
                            let! range = ExprOrRange.parse
                            let! doTok = pDo
                            let! body = pSeqBlock refExpr.Parser
                            let! doneTok = pDone
                            return Expr.ForIn(forTok, pat, inTok, range, doTok, body, doneTok)
                        }
                    ]
                    "Expected for-to or for-in"
        }

    let pUseExpr =
        parser {
            let! useTok = pUse
            let! ident = pIdentTok
            let! eq = pEquals
            let! expr = refExpr.Parser
            let! inTok = pInVirt
            let! body = pSeqBlock refExpr.Parser
            return Expr.Use(useTok, ident, eq, expr, inTok, body)
        }

    let atomExpr =
        choiceL
            [
                pConst
                pIdent
                pLetValue
                pUseExpr
                pParen
                pList
                pArray
                pStructTuple
                pBeginEnd
                pIfExpr
                pMatchExpr
                pFunExpr
                pTryExpr
                pWhileExpr
                pForExpr
            ]
            "atom expression"

    let operators = ExprOperatorParser()

    let parse = Operator.parser atomExpr operators

    refExpr.Set parse
    // Semicolon has special handling in F# lists
    // so we create a separate parser for expressions in lists
    // and set the starting precedence one level higher so it will be parsed in `pList`
    refExprInCollection.Set(
        Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) atomExpr operators
    )
