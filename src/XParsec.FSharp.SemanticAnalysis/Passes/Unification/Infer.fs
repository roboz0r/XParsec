namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload

module UnificationInfer =

    let rec infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp ctx key fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp ctx key fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix ctx key left right
            | Expr.PrefixApp(_, operand) -> inferPrefix ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> inferLet ctx key kw bindings body
            | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                inferListLikeLiteral ctx key inner false
            | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                inferListLikeLiteral ctx key inner true
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.StaticUpcast(expr = inner; typ = t) -> inferStaticUpcast ctx key inner t
            | Expr.DynamicTypeTest(expr = inner; typ = t) -> inferDynamicTypeTest ctx key inner t
            | Expr.DynamicDowncast(expr = inner; typ = t) -> inferDynamicDowncast ctx key inner t
            | Expr.EmptyBlock(lParen = ParenKind.List _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRBracket "]"
                emptyListLikeLiteral ctx key false
            | Expr.EmptyBlock(lParen = ParenKind.Array _; rParen = rTok) ->
                checkLiteralClose ctx key rTok Token.KWRArrayBracket "|]"
                emptyListLikeLiteral ctx key true
            | Expr.EmptyBlock _ -> BuiltinTypes.tyUnit
            | Expr.While(condition = cond; body = body) -> inferWhile ctx key cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo ctx key ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn ctx key pat src body
            | Expr.String(parts = parts) -> inferString ctx key parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) -> inferMatch ctx key scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction ctx key rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith ctx key body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> inferTryFinally ctx key body finallyE
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment ctx key left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange ctx key a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) -> inferRange ctx key a (ValueSome s) b
            | Expr.Null _ ->
                // No reference-type bound yet — free TypeVar so surrounding
                // context can pin it.
                TyVar(freshTyVar ctx)
            | Expr.Record(fieldInitializers = inits) -> inferRecord ctx key inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone ctx key src inits
            | Expr.DotLookup(expr = recv; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                // A type-name receiver (`EqualityComparer<int>.Default`) resolves
                // its static member through the provider — probed once here, ahead
                // of the field-access fallback so the receiver isn't `infer`d as a
                // value. Instance access (`value.Member`) takes the fallback.
                match tryExternalTypeReceiver ctx recv with
                | ValueSome(metaName, typeArgsCst) ->
                    let args = [ for t in typeArgsCst -> translateType ctx t ]
                    inferExternalStaticMember ctx key metaName args li.Idents.[0]
                | ValueNone -> inferFieldAccess ctx key recv li.Idents.[0]
            | Expr.IndexedLookup(expr = recv; indexExpr = idx) -> inferIndexedLookup ctx key recv idx
            | Expr.New(typ = t; expr = argExpr) -> inferNew ctx key t argExpr
            | Expr.ILIntrinsic(args = args; returnType = rt) -> inferILIntrinsic ctx args rt
            | Expr.LibraryOnlyStaticOptimization(expr = baseE; constraints = cs; optimizedExpr = optE) ->
                inferLibraryOnlyStaticOptimization ctx key baseE cs optE
            | Expr.TypeApp(expr = inner; types = typeArgs) -> inferTypeApp ctx key inner typeArgs
            | _ ->
                // Surface the unhandled case loudly rather than fabricating a
                // free TyVar and silently producing a broken type for every
                // use site. Matches the precedent in
                // `Freeze.translateExpr` (file: Freeze.fs).
                failwithf "infer: TODO %A" e

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        // A multi-segment LongIdent whose head is a local binding is a
        // record-field access chain (`r.X.Y`), not a qualified name — the
        // parser rides these inside a single `Expr.LongIdentOrOp` rather
        // than emitting `Expr.DotLookup`.
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through
        // the provider, instantiating its scheme like any external symbol.
        // Freeze projects this to `External("op_Addition", …)`.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                // The ambient prelude leg resolves a contract's `[<AutoOpen>]`
                // operator module.
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
                | ValueNone -> errorTy ctx key (sprintf "Operator '%s' is not available from the symbol provider" name)
            | ValueNone -> TyVar(freshTyVar ctx)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        // Two-segment qualified reference whose head is *not* a local binding:
        // `Math.Pi` / `Lst.Empty` / `Result2.Ok`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2 && not (ctx.Bindings.Binding.ContainsKey key)
            ->
            let headName = ctx.NameOf li.Idents.[0]
            let tailName = ctx.NameOf li.Idents.[1]

            let tryStaticMember (typeParams: EqArray<string * TypeVar>) (members: TypeMemberInfo[]) =
                match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = tailName) with
                | Some m ->
                    let _, subst = freshNamedInstance ctx typeParams
                    ValueSome(substituteWith subst m.Type)
                | None -> ValueNone

            // Class static member takes priority over union static member which
            // takes priority over a union ctor — preserves the original cascade
            // order so a static member shadows the not-a-case diagnostic.
            let classHit =
                match ctx.Types.Class.TryGetValue headName with
                | true, info -> tryStaticMember info.TypeParams info.Members
                | false, _ -> ValueNone

            match classHit with
            | ValueSome ty -> ty
            | ValueNone ->
                match ctx.Types.Union.TryGetValue headName with
                | true, info ->
                    match tryStaticMember info.TypeParams info.Members with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        // Qualified ctor reference `Result2.Ok` — via the union
                        // registry, bypassing the CtorIndex ambiguity check.
                        match resolveQualifiedCtor ctx headName tailName with
                        | ValueSome info -> ctorType ctx info
                        | ValueNone -> errorTy ctx key (sprintf "Union '%s' has no case '%s'" headName tailName)
                | false, _ ->
                    // Qualified external union case (`Option.Some`) — the head is
                    // an external union, not a local one (Gap 2 Layer B).
                    match tryExternalCtorType ctx (ValueSome headName) tailName with
                    | ValueSome t -> t
                    | ValueNone -> inferIdentDefault ctx e key
        | _ -> inferIdentDefault ctx e key

    /// Resolution order: local binding map, then provider, then `Class`-name
    /// and `Union`-case registries (the latter two only for single-segment names).
    and private inferIdentDefault (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> instantiateBinding ctx rb
        | ValueNone ->
            // Provider first — provider hits beat ctor-name resolution
            // when both exist (a let-bound `Ok` would have a Binding entry
            // and never reach here). Bare single-segment idents absent
            // from the provider fall to the ctor registry.
            let name = qualifiedNameOf ctx e

            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
            | ValueNone ->

                match tryExternalStaticLongIdent ctx key e with
                | ValueSome ty -> ty
                | ValueNone ->
                    let singleSegName =
                        match e with
                        | Expr.Ident t -> ValueSome(ctx.NameOf t)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            ValueSome(ctx.NameOf li.Idents.[0])
                        | _ -> ValueNone

                    match singleSegName with
                    | ValueSome n ->
                        let info, count = resolveCtorName ctx n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 ->
                            errorTy
                                ctx
                                key
                                (sprintf
                                    "Ambiguous constructor '%s'; declared in %d union types — add a qualifier or annotation"
                                    n
                                    count)
                        | ValueNone ->
                            // External union case ctor (`Some` / `None` from a
                            // referenced package, in scope via `open`): typed as
                            // `field… → TyUnion(union, …)` so `inferApp` flows the
                            // application through the normal function arm and the
                            // bare nullary form (`None`) lands as the union value
                            // (vesper-lib-test-plan Gap 2 Layer B).
                            match tryExternalCtorType ctx ValueNone n with
                            | ValueSome t -> t
                            | ValueNone ->
                                // Class-name-as-function: `Point(3, 4)` parses as
                                // `Expr.App (Expr.Ident "Point", ...)`. Return the
                                // ctor as a function value so `inferApp` types the
                                // call through the normal function arm.
                                classCtorAsFunction ctx n
                    | ValueNone ->
                        // A multi-segment qualified name that resolved to nothing.
                        // If its qualifier names a known external union/record, the
                        // tail is a missing member (`Result.Nope` / `Option.Nope`):
                        // diagnose it rather than minting a fresh TyVar that unifies
                        // with anything and hides the typo deep in codegen — the
                        // symmetric front-end miss to `resolveFieldStep`'s instance-
                        // member arm.
                        match tryQualifiedExternalMemberMiss ctx e with
                        | ValueSome(qual, memberName) ->
                            errorTy ctx key (sprintf "Type '%s' has no value or member '%s'" qual memberName)
                        | ValueNone -> TyVar(freshTyVar ctx)

    and private qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    and private inferApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        match tryInferPrintfApp ctx key fn args with
        | ValueSome ty -> ty
        | ValueNone ->
            // A .NET static method is tupled: `String.Concat ("a", "b")` is one
            // tuple argument. Resolve a multi-overload static method by its arg
            // types at the call site before the generic curried application path.
            match
                (if args.Length = 1 then
                     tryInferExternalStaticMethodCall ctx key fn args.[0]
                 else
                     ValueNone)
            with
            | ValueSome ty -> ty
            | ValueNone ->

                // `new`-less ctor-as-function sugar on an external class (`Exn "x"`).
                // Probed before the generic application path so the head resolves to the
                // external constructor instead of leaking a fresh TyVar (the latter only
                // generalises when it reaches the binding's type — buried as an argument
                // it dangles, which `ResolvedTypes` flags as an unresolved TyVar).
                match tryInferExternalCtorApp ctx key fn args with
                | ValueSome ty -> ty
                | ValueNone ->

                    match
                        (if args.Length = 1 then
                             tryInferExternalGenericCtorApp ctx key fn args.[0]
                         else
                             ValueNone)
                    with
                    | ValueSome ty -> ty
                    | ValueNone ->

                        let mutable currTy = infer ctx fn

                        for a in args do
                            let argTy = infer ctx a
                            let resultTy = TyVar(freshTyVar ctx)
                            unify ctx key currTy (TyFun(argTy, resultTy))
                            currTy <- resultTy

                        currTy

    /// Printf-family typing rule (front-end-gaps-plan §B). For a recognised
    /// printf entry point with a plain-literal format argument, the format spec
    /// — not the literal's apparent `string` type — drives the call's curried
    /// result type. The format argument types as `PrintfFormat<printer, …>`.
    and private tryInferPrintfApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        let fnKey = CstKeys.ofExpr fn

        // A local binding shadowing a printf name is an ordinary function —
        // don't apply the special rule.
        if ctx.Bindings.Binding.ContainsKey fnKey then
            ValueNone
        else
            match fn with
            | Expr.Ident _
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
                match PrintfSpec.tryFamily (qualifiedNameOf ctx fn) with
                | ValueNone -> ValueNone
                | ValueSome fam ->
                    let idx = fam.FormatArgIndex

                    if args.Length <= idx then
                        // Format argument not supplied (e.g. partially-applied
                        // `fprintf writer`); defer to standard inference.
                        ValueNone
                    else
                        match formatSpecifiers ctx args.[idx] with
                        | ValueNone -> ValueNone
                        | ValueSome specs ->
                            let fresh () = TyVar(freshTyVar ctx)

                            match PrintfSpec.appliedTypeOf fresh specs fam with
                            // A specifier we don't type in v1 (`%a` / `%t`);
                            // defer to standard inference.
                            | ValueNone -> ValueNone
                            | ValueSome(fnTy, fmtTy, _) ->
                                // Stamp the function node so Freeze threads the
                                // curried result type through the App chain.
                                (freshTv ctx fnKey).Link <- ValueSome fnTy

                                let mutable currTy = fnTy

                                for i in 0 .. args.Length - 1 do
                                    let a = args.[i]

                                    let argTy =
                                        if i = idx then
                                            // The format literal types as the PrintfFormat, not `string`.
                                            (freshTv ctx (CstKeys.ofExpr a)).Link <- ValueSome fmtTy
                                            fmtTy
                                        else
                                            infer ctx a

                                    let resultTy = TyVar(freshTyVar ctx)
                                    unify ctx key currTy (TyFun(argTy, resultTy))
                                    currTy <- resultTy

                                // P1 happy-path lowering marker: fully-applied literal call, a
                                // StdOut/StdErr/StringResult sink, and every specifier lowerable
                                // → Freeze mints a `TExpr.Format`. Otherwise the FSharp.Core path
                                // stands (additive — `%A`, partial application, etc. unaffected).
                                match PrintfSpec.sinkOf (qualifiedNameOf ctx fn) with
                                | ValueSome sink when
                                    idx = 0
                                    && args.Length = specs.Length + 1
                                    && lowerablePlaceholders ctx args.[idx]
                                    ->
                                    ctx.PrintfApp.Set(key, sink)
                                | _ -> ()

                                ValueSome currTy
            | _ -> ValueNone

    and private inferHighPrecApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — same shape as `Expr.App fn [|arg|]`, a separate CST case. A
        // no-space method call (`String.Concat("a", "b")`) is a HighPrecedenceApp,
        // so the call-site overload resolver is checked here too.
        match tryInferExternalStaticMethodCall ctx key fn arg with
        | ValueSome ty -> ty
        | ValueNone ->
            match tryInferExternalGenericCtorApp ctx key fn arg with
            | ValueSome ty -> ty
            | ValueNone ->
                let fnTy = infer ctx fn
                let argTy = infer ctx arg
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key fnTy (TyFun(argTy, resultTy))
                resultTy

    and private inferRange
        (ctx: PassContext)
        (key: NodeKey)
        (fromE: Expr<SyntaxToken>)
        (stepE: Expr<SyntaxToken> voption)
        (toE: Expr<SyntaxToken>)
        : SemType =
        // Tiny subset: endpoints (and step) constrained to int, result the
        // `seq<int>` placeholder. Real F# is generic over the `..` overload.
        let fromTy = infer ctx fromE
        unify ctx key fromTy BuiltinTypes.tyInt

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx key stepTy BuiltinTypes.tyInt
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx key toTy BuiltinTypes.tyInt
        BuiltinTypes.tySeqInt

    and private inferInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match tryMeasuredArith ctx key name leftTy rightTy with
            | Some resultTy -> resultTy
            | None ->
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(leftTy, TyFun(rightTy, resultTy)))
                    resultTy
                | ValueNone -> errorTy ctx key (sprintf "Unknown operator symbol: %s" name)
        | ValueSome DesugaredForm.ConsExpr ->
            // `h :: t` builds the list union directly (not a provider operator):
            // `h`'s type is the element, `t` is unified to the same list type,
            // and the result is that list type — exactly a one-cell `[h]` literal
            // consed onto `t`.
            let listTy = listLiteralTy ctx key leftTy
            unify ctx key rightTy listTy
            listTy
        | ValueSome _
        | ValueNone ->
            // Desugar didn't recognise the operator (non-OpName can't happen
            // for an InfixApp key) — leave the result free.
            TyVar(freshTyVar ctx)

    and private inferPrefix (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone -> errorTy ctx key (sprintf "Unknown prefix operator: %s" name)
        | ValueSome _
        | ValueNone -> TyVar(freshTyVar ctx)

    and private inferIfThenElse
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy BuiltinTypes.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else): the then-branch must be `unit` and the whole
            // expression is `unit` (F# spec — a missing else is `else ()`). The elif
            // branches above were already unified with `thenTy`, so this one `unify`
            // forces all branches to `unit`.
            unify ctx key thenTy BuiltinTypes.tyUnit
            BuiltinTypes.tyUnit

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferTuple (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple(EqArray.ofSeq (seq { for e in items -> infer ctx e }))

    and private inferSequential (ctx: PassContext) (key: NodeKey) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        if items.Length = 0 then
            BuiltinTypes.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty BuiltinTypes.tyUnit

            infer ctx items.[items.Length - 1]

    /// `pEnclosed` virtual-inserts a missing/mismatched close token with a
    /// parser-side diagnostic that isn't visible to semantic-analysis consumers,
    /// so surface the breakage on `ctx.Diagnostics` too — otherwise the malformed
    /// literal types successfully and Freeze emits a well-shaped TAST.
    and private checkLiteralClose
        (ctx: PassContext)
        (key: NodeKey)
        (rTok: SyntaxToken)
        (expected: Token)
        (display: string)
        : unit =
        match rTok.Index with
        | TokenIndex.Virtual -> ctx.Error(key, sprintf "Mismatched or missing closing delimiter: expected '%s'" display)
        | TokenIndex.Regular _ when rTok.Token <> expected ->
            // Defensive: pEnclosed only emits a real rParen when the peeked
            // token matched, so this can't trigger today — guards against a
            // future parser change letting a mismatched close-token through.
            ctx.Error(key, sprintf "Mismatched closing delimiter: expected '%s'" display)
        | TokenIndex.Regular _ -> ()

    /// The list type a `[…]` literal carries. Two cases:
    ///   1. A program that declares its own `'T list` abbreviation (the self-host
    ///      shape — `list.fs`'s `and 'T list = List<'T>`) resolves eagerly to its
    ///      RHS union.
    ///   2. A bare program (R3): the container is left *flexible* — a fresh
    ///      `TypeVar` registered in `ctx.ListLiterals`. This is the consumer-driven
    ///      typing handoff R3 calls for: `List.fold`'s `Vesper.Collections.List`
    ///      parameter flips it to the Vesper list (so the literal emits BCL-only),
    ///      while a literal nothing else pins (`printfn "%A" [1;2;3]`) defaults
    ///      back to FSharp.Core's `list` in `resolveListLiterals`.
    and private listLiteralTy (ctx: PassContext) (key: NodeKey) (elemTy: SemType) : SemType =
        match ctx.Types.Abbreviation.TryGetValue "list" with
        | true, info ->
            forceFill ctx info
            expandAbbreviation ctx key info (EqArray.singleton elemTy)
        | false, _ ->
            let tv = freshTyVar ctx
            ctx.ListLiterals.Add(UnionFind.find tv, elemTy)
            TyVar tv

    and private inferListLikeLiteral
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (isArray: bool)
        : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        let items =
            match body with
            | Expr.Sequential(exprs = items) -> items
            | single -> ImmutableArray.Create(single)

        for i = 0 to items.Length - 1 do
            let itemTy = infer ctx items.[i]
            unify ctx key itemTy elemTy

        if isArray then
            TyConst(RuntimeNames.arrayName 1, EqArray.singleton elemTy)
        else
            listLiteralTy ctx key elemTy

    /// Element type stays free so context can pin it (`let xs : int list = []`).
    and private emptyListLikeLiteral (ctx: PassContext) (key: NodeKey) (isArray: bool) : SemType =
        let elemTy = TyVar(freshTyVar ctx)

        if isArray then
            TyConst(RuntimeNames.arrayName 1, EqArray.singleton elemTy)
        else
            listLiteralTy ctx key elemTy

    and private inferWhile
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy BuiltinTypes.tyBool
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and private inferForTo
        (ctx: PassContext)
        (key: NodeKey)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx key startTy BuiltinTypes.tyInt
        let endTy = infer ctx endE
        unify ctx key endTy BuiltinTypes.tyInt
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome BuiltinTypes.tyInt
        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    /// The §4.4 duck-typed enumerator probe: C#'s pattern-based `foreach` accepts
    /// any source exposing a public parameterless `GetEnumerator()` whose return
    /// type `E` exposes `MoveNext(): bool` and a `Current` property — no
    /// `IEnumerable<'T>` required (`List<'T>` hands back its non-boxing
    /// `struct Enumerator` this way). Returns the element type (`Current`'s type)
    /// and the resolved `DuckTyped` descriptor so codegen can pick value-receiver
    /// emission. `srcArgs` are the source class's type arguments — the substitution
    /// for `GetEnumerator`'s (and thereby `E`'s) typars.
    and private tryDuckTypedEnumerator
        (ctx: PassContext)
        (shape: ExternalClassShape)
        (srcArgs: SemType[])
        : (SemType * ForInEnumerator) voption =
        match
            shape.Members
            |> Array.tryFind (fun m -> m.Name = "GetEnumerator" && not m.IsStatic && not m.IsProperty)
        with
        | None -> ValueNone
        | Some ge ->
            // `GetEnumerator` reads as `unit → E`; `E` carries the enumerator type's
            // own instantiation (`List`1+Enumerator` over the source's `'T`).
            match ExternalSymbols.openSignature ge srcArgs with
            | TyFun(_, (TyClass(enumKey, enumArgsEq) as enumTy)) ->
                match ExternalSymbols.tryLookupType ctx.Provider enumKey with
                | ValueSome(ExternalTypeShape.Class enumShape) ->
                    let enumArgs = enumArgsEq.AsSpan().ToArray()

                    let moveNext =
                        enumShape.Members
                        |> Array.tryFind (fun m -> m.Name = "MoveNext" && not m.IsStatic && not m.IsProperty)

                    let current =
                        enumShape.Members
                        |> Array.tryFind (fun m -> m.Name = "Current" && not m.IsStatic && m.IsProperty)

                    match moveNext, current with
                    | Some mn, Some cur ->
                        match ExternalSymbols.openSignature mn enumArgs with
                        | TyFun(_, TyConst("bool", _)) ->
                            let elemTy = ExternalSymbols.openSignature cur enumArgs

                            // The enumerator only needs disposing — and the `finally`
                            // region only exists — when it is `IDisposable` (C# parity).
                            let dispose =
                                if
                                    ExternalSymbols.instantiateInterfaces enumShape enumArgs
                                    |> Array.exists (fun (n, _) -> n = "System.IDisposable")
                                then
                                    ValueSome(
                                        SymbolKey.MemberKey(
                                            SymbolKey.TypeKey(None, "System", "IDisposable"),
                                            "Dispose",
                                            EqArray.empty,
                                            MemberKind.Method
                                        )
                                    )
                                else
                                    ValueNone

                            ValueSome(
                                elemTy,
                                ForInEnumeratorG.DuckTyped(
                                    enumTy,
                                    ge.Key,
                                    mn.Key,
                                    cur.Key,
                                    enumShape.Flags.IsValueType,
                                    dispose
                                )
                            )
                        | _ -> ValueNone
                    | _ -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone

    /// The element-type + enumerator-shape probe for `for x in src do …` (B-6 /
    /// §4.4). `srcTy` is either `IEnumerable<'T>` itself, an external class that
    /// implements it (the directly-implemented interface set the metadata layer
    /// surfaces through `ExternalClassShape.Interfaces`), or — as the §4.4
    /// fallback — a source exposing a pattern-based `GetEnumerator()`. Returns the
    /// `'T` so `inferForIn` can pin the loop pattern's type, plus the
    /// `ForInEnumerator` codegen reads off the frozen node.
    and private tryForInEnumerator (ctx: PassContext) (srcTy: SemType) : (SemType * ForInEnumerator) voption =
        let ienumName = "System.Collections.Generic.IEnumerable`1"

        match zonk srcTy with
        | TyClass(nameKey, args) when SymbolKeyOps.qualifiedName nameKey = ienumName && args.Length = 1 ->
            ValueSome(args.[0], ForInEnumeratorG.Interface)
        | TyClass(nameKey, args) ->
            match ExternalSymbols.tryLookupType ctx.Provider nameKey with
            | ValueSome(ExternalTypeShape.Class shape) ->
                let argArr = args.AsSpan().ToArray()

                // C# precedence: a pattern-based `GetEnumerator()` wins over the
                // `IEnumerable<'T>` interface, so `List<'T>` walks its non-boxing
                // struct `Enumerator` (§4.4) rather than the boxing interface
                // enumerator (now that value-type member-call emission has landed).
                // Fall back to the interface shape (§4.2) for a source that only
                // implements `IEnumerable<'T>` and exposes no usable pattern
                // `GetEnumerator()`.
                match tryDuckTypedEnumerator ctx shape argArr with
                | ValueSome r -> ValueSome r
                | ValueNone ->
                    match
                        ExternalSymbols.instantiateInterfaces shape argArr
                        |> Array.tryPick (fun (n, ta) -> if n = ienumName && ta.Length = 1 then Some ta.[0] else None)
                    with
                    | Some elem -> ValueSome(elem, ForInEnumeratorG.Interface)
                    | None -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    and private inferForIn
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Int-range source: element type is int. Any other source must be an
        // `IEnumerable<'T>` (a BCL collection in v1, B-6) — the element type is
        // recovered from its interface set and the loop pattern unified with it.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key srcTy BuiltinTypes.tySeqInt
            unify ctx key patTy BuiltinTypes.tyInt
        else
            match tryForInEnumerator ctx srcTy with
            | ValueSome(elemTy, shape) ->
                unify ctx key patTy elemTy
                ctx.Resolution.ForInShape.Set(key, shape)
            | ValueNone ->
                ctx.Error(
                    key,
                    "for-in: source is not a supported enumerable (expected IEnumerable<'T> or a pattern-based GetEnumerator())"
                )

        let bodyTy = infer ctx body
        unify ctx key bodyTy BuiltinTypes.tyUnit
        BuiltinTypes.tyUnit

    and private inferRules
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy scrutineeTy

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy BuiltinTypes.tyBool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

    and private inferMatch
        (ctx: PassContext)
        (key: NodeKey)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key scrutineeTy resultTy rules
        resultTy

    and private inferFunction (ctx: PassContext) (key: NodeKey) (rules: ImmutableArray<Rule<SyntaxToken>>) : SemType =
        // `function … ` ~ `fun x -> match x with …`. The synthesised
        // parameter's TypeVar IS the scrutinee's — every arm's pattern
        // unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and private inferTryWith
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // Until a real `exn` type lands, pin the scrutinee to placeholder
        // `TyConst "exn"`. A fresh TyVar would let wildcard / variable arm
        // patterns carry an unresolved TyVar into the TAST, which
        // `ResolvedTypes` correctly flags.
        let resultTy = infer ctx body
        let exnTy = TyConst("exn", EqArray.empty)
        inferRules ctx key exnTy resultTy rules
        resultTy

    and private inferTryFinally
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx key finallyTy BuiltinTypes.tyUnit
        resultTy

    and private inferAssignment
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        // Mutability of the LHS is a Validation concern; here we only typecheck.
        let leftTy = infer ctx left
        let rightTy = infer ctx right
        unify ctx key leftTy rightTy
        BuiltinTypes.tyUnit

    and private inferRecord
        (ctx: PassContext)
        (key: NodeKey)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        let candidate =
            match qualifier with
            | Some typeName ->
                match ctx.Types.Record.TryGetValue typeName with
                | true, info -> ValueSome info
                | false, _ ->
                    ctx.Error(key, sprintf "Unknown record type qualifier: %s" typeName)
                    ValueNone
            | None ->
                let cand, count = findUniqueRecordByFieldSet ctx names

                match cand with
                | ValueSome _ -> cand
                | ValueNone ->
                    if count = 0 then
                        ctx.Error(key, sprintf "No record type matches the field set: %s" (String.concat ", " names))
                    else
                        ctx.Error(
                            key,
                            sprintf
                                "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                count
                        )

                    ValueNone

        match candidate with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome info ->
            // Fresh typars per literal so independent literals get independent
            // vars; each initialiser unifies against the field type *under this
            // substitution*, pinning a `'a` field to the initialiser's type.
            let args, subst = freshNamedInstance ctx info.TypeParams

            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                | None -> ctx.Error(CstKeys.ofExpr e, sprintf "Type '%s' has no field '%s'" info.Name fieldName)

            TyRecord(info.Key, args)

    and private inferRecordClone
        (ctx: PassContext)
        (key: NodeKey)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep srcTy with
        | TyRecord(recKey, srcArgs) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                // Clone preserves the source's arg list — overrides unify
                // against the substituted field type (`'a` → source's arg).
                let subst = mkNamedTypeSubst info.TypeParams srcArgs

                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofExpr e) eTy (substituteWith subst field.Type)
                    | None -> ctx.Error(CstKeys.ofExpr e, sprintf "Type '%s' has no field '%s'" info.Name fieldName)

                TyRecord(recKey, srcArgs)
            | ValueNone ->
                ctx.Error(key, sprintf "Unknown record type '%s'" (SymbolKeyOps.simpleName recKey))

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord(recKey, srcArgs)
        | _ ->
            ctx.Error(key, "Record clone requires the source expression to be a record")

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// One step of dot-access resolution; deferred when the receiver is a free
    /// TyVar. For a generic receiver `(b : Box<int>).Value`, the declared field /
    /// member type `'a` is substituted against the receiver's arg list so `Value`
    /// types as `int`, not a free typar.
    /// Resolve an instance member on a project-local class/union, or emit a
    /// static-hint-aware diagnostic. Shared by the `TyClass` / `TyUnion` arms of
    /// `resolveFieldStep` — the only thing that differs between them is the
    /// registry consulted and the "Unknown … type" wording on a registry miss.
    and private resolveLocalInstanceMember
        (ctx: PassContext)
        (diagKey: NodeKey)
        (typeName: string)
        (typeParams: EqArray<string * TypeVar>)
        (args: EqArray<SemType>)
        (members: TypeMemberInfo[])
        (memberName: string)
        : SemType =
        match members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
        | Some m -> instantiateMember (typeParams, args) m.Type
        | None ->
            if members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic) then
                errorTy
                    ctx
                    diagKey
                    (sprintf
                        "Member '%s' on type '%s' is static; access it via '%s.%s'"
                        memberName
                        typeName
                        typeName
                        memberName)
            else
                errorTy ctx diagKey (sprintf "Type '%s' has no instance member '%s'" typeName memberName)

    and private resolveFieldStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (memberName: string) : SemType =
        match resolveStep rTy with
        | TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = memberName) with
                | Some field -> instantiateMember (info.TypeParams, args) field.Type
                | None -> errorTy ctx diagKey (sprintf "Type '%s' has no field '%s'" info.Name memberName)
            | ValueNone -> errorTy ctx diagKey (sprintf "Unknown record type '%s'" (SymbolKeyOps.simpleName recKey))
        | TyClass(clsKey, args) ->
            // Local lookup by the bare simple name; the external provider by the
            // qualified compiled name (an external `TyClass` carries a qualified key).
            let clsSimple = SymbolKeyOps.simpleName clsKey

            match TypeRegistry.tryClass ctx.Types clsSimple with
            | ValueSome info ->
                // Walk the inheritance chain (derived members shadow inherited).
                // On a total miss, fall back to the single-class diagnostic so
                // the static-access hint still references the receiver's own
                // class rather than some ancestor.
                match tryClassChainMember ctx clsSimple args memberName with
                | ValueSome ty -> ty
                | ValueNone ->
                    resolveLocalInstanceMember ctx diagKey clsSimple info.TypeParams args info.Members memberName
            | ValueNone ->
                // Not a project-local class — an *external* type (e.g. a BCL
                // `TyClass("…EqualityComparer`1", [int])` produced by a prior static
                // access). Resolve the instance member through the provider and
                // record it for Freeze.
                let clsQual = SymbolKeyOps.qualifiedName clsKey

                match ctx.Provider.TryLookupMember(clsQual, memberName) with
                | ValueSome m when not m.IsStatic ->
                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            IsProperty = m.IsProperty
                        }
                    )

                    ExternalSymbols.openSignature m (args.AsSpan().ToArray())
                | _ -> errorTy ctx diagKey (sprintf "Unknown class type '%s'" clsQual)
        | TyUnion(unionKey, args) ->
            // Union instance member access (P3d.3) — mirrors the `TyClass` arm
            // against the union's augmentation members.
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                resolveLocalInstanceMember
                    ctx
                    diagKey
                    (SymbolKeyOps.simpleName unionKey)
                    info.TypeParams
                    args
                    info.Members
                    memberName
            | ValueNone ->
                // Not a project-local union — an *external* one (e.g. a referenced
                // `Vesper.Option` whose `IsSome`/`Value`/`IsNone` augmentation
                // members the contract provider publishes). Resolve through the
                // provider and record it for Freeze, exactly as the external
                // `TyClass` arm does (vesper-lib-test-plan Gap 2 Layer A).
                let unionQual = SymbolKeyOps.qualifiedName unionKey

                match ctx.Provider.TryLookupMember(unionQual, memberName) with
                | ValueSome m when not m.IsStatic ->
                    ctx.Resolution.ExternalAccess.Set(
                        diagKey,
                        {
                            Key = m.Key
                            IsStatic = false
                            IsProperty = m.IsProperty
                        }
                    )

                    ExternalSymbols.openSignature m (args.AsSpan().ToArray())
                | _ ->
                    // The provider knows the union but not this member → a real
                    // member miss; otherwise the type itself is unknown.
                    match ctx.Provider.TryLookupType unionQual with
                    | ValueSome(ExternalTypeShape.Union _) ->
                        errorTy ctx diagKey (sprintf "Type '%s' has no instance member '%s'" unionQual memberName)
                    | _ -> errorTy ctx diagKey (sprintf "Unknown union type '%s'" unionQual)
        | TyVar tv ->
            let root = UnionFind.find tv
            let resultTv = freshTyVar ctx

            let access =
                {
                    MemberName = memberName
                    UseKey = diagKey
                    ResultTv = resultTv
                }

            root.PendingDotAccess <- access :: root.PendingDotAccess
            TyVar resultTv
        // `arr.Length` on a rank-1 intrinsic array resolves to the core
        // `GetArrayLength` inline function (scheme `'T[] -> int`), grounding the
        // call so `InlineExpansion` can splice the source `ldlen` — the same path as
        // `arr.[i]`/`GetArray`. No member metadata on the intrinsic `'T[]`.
        | TyConst(name, _) when name = RuntimeNames.arrayName 1 && memberName = "Length" ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetArrayLength" with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx diagKey (sym.Instantiate ctx.CurrentLevel) (TyFun(rTy, resultTy))
                resultTy
            | ValueNone ->
                errorTy ctx diagKey "Array 'Length' intrinsic 'GetArrayLength' is not in scope (Vesper.Core missing?)"
        | _ -> errorTy ctx diagKey (sprintf "Cannot read member '%s' from non-record non-class type" memberName)

    /// Application-site overload resolution for a static external method call
    /// (`String.Concat("a", "b")`). Fires only when the member name has >1 mapped
    /// overload — single-candidate access keeps the existing single-pick path, so
    /// behaviour is unchanged everywhere it already worked. Commits the chosen
    /// `SymbolKey` to `ExternalAccess` keyed on the member node where Freeze reads it.
    /// The chosen member's method-owned typars (`Take<TSource>`) are freshened to
    /// inference vars by `ExternalSymbols.instantiateSignature` so the argument
    /// types drive their solution (superseding the former `BuildSignature` +
    /// `instantiateMethodTypars` pair).
    and private tryInferExternalStaticMethodCall
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match tryResolveExternalStaticMemberRef ctx fn with
        | ValueNone -> ValueNone
        | ValueSome(metaName, memberTok) ->
            let memberName = ctx.NameOf memberTok
            let candidates = ctx.Provider.TryLookupMembers(metaName, memberName)

            // A folded LongIdent names a non-generic type (generics need `<>`), so
            // the declaring type has no type arguments to instantiate.
            let typeArgs: SemType[] = [||]

            if candidates.Length <= 1 then
                // 0 / 1 candidate: defer to the eager single-pick path unchanged.
                ValueNone
            else
                let argTy = infer ctx argExpr

                match pickStaticOverload typeArgs candidates (argElemsOf argTy) with
                | ValueSome chosen ->
                    let fnKey = CstKeys.ofExpr fn

                    ctx.Resolution.ExternalAccess.Set(
                        fnKey,
                        {
                            Key = chosen.Key
                            IsStatic = chosen.IsStatic
                            IsProperty = chosen.IsProperty
                        }
                    )

                    // Instantiate the method-owned typars (`Take<TSource>`) to fresh
                    // vars so the argument types drive their solution; a non-generic
                    // overload is unchanged.
                    let memberSig =
                        ExternalSymbols.instantiateSignature chosen typeArgs ctx.CurrentLevel

                    (freshTv ctx fnKey).Link <- ValueSome memberSig
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key memberSig (TyFun(argTy, resultTy))
                    ValueSome resultTy
                | ValueNone ->
                    ValueSome(
                        errorTy
                            ctx
                            key
                            (sprintf
                                "No applicable (or no unique best) overload of '%s' on type '%s' for the given arguments"
                                memberName
                                metaName)
                    )

    and private inferFieldAccess
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let fieldName = ctx.NameOf fieldTok
        let rTy = infer ctx receiver
        resolveFieldStep ctx key rTy fieldName

    /// `arr.[i]` — the receiver is a rank-1 array `'T[]` and the index an `int`;
    /// the result is the element type. The element stays a fresh var unified
    /// against the receiver so an as-yet-unresolved receiver (a bare `[]`) is
    /// pinned from context the same way an array literal is.
    and private inferIndexedLookup
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (index: Expr<SyntaxToken>)
        : SemType =
        let recvTy = infer ctx receiver
        let idxTy = infer ctx index

        // `arr.[i]` resolves to the core `GetArray` inline function, exactly as an
        // operator resolves through `inferInfix`: instantiate its scheme
        // (`'T[] -> int -> 'T`) and unify against `arr -> idx -> result`. That pins
        // the array element type, the `int` index, and the result — and (like every
        // resolved call) grounds the types so `InlineExpansion` can splice the
        // source `ldelem` at the use site. The mnemonic never originates here.
        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "GetArray" with
        | ValueSome sym ->
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(recvTy, TyFun(idxTy, resultTy)))
            resultTy
        | ValueNone -> errorTy ctx key "Array indexing intrinsic 'GetArray' is not in scope (Vesper.Core missing?)"

    /// `new T(args)`. Mirrors a single application against the ctor, kept inline
    /// so a bare `Expr.New` doesn't need to fabricate an `Expr.App` first.
    and private inferNew
        (ctx: PassContext)
        (key: NodeKey)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let receiverTy = translateType ctx t

        match resolveStep receiverTy with
        | TyClass(clsKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info ->
                let subst = mkNamedTypeSubst info.TypeParams args

                let expected =
                    info.CtorParams
                    |> Array.map (fun p -> substituteWith subst p.Type)
                    |> Array.toList
                    |> tupleOrSingle

                let argTy = infer ctx argExpr
                unify ctx (CstKeys.ofExpr argExpr) argTy expected
                receiverTy
            | ValueNone ->
                // Fall through to the external-class path: `new System.Exception(msg)`
                // inside an inline body (the `failwith` body, `raise (System.Exception
                // message)`) — the type was named through `tryResolveExternalType` so
                // `name` is the metadata full name, and the symbol provider already
                // owns the ctor catalogue (`MetadataSymbols.extractMembers` /
                // `computeMembers` surfaces them under `.ctor`).
                let name = SymbolKeyOps.qualifiedName clsKey

                match ctx.Provider.TryLookupType name with
                | ValueSome(ExternalTypeShape.Class _) -> inferExternalCtorOn ctx key name args receiverTy argExpr
                | _ ->
                    ctx.Error(key, sprintf "Unknown class type '%s'" name)
                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        | _ ->
            ctx.Error(key, "'new' requires a class type")
            infer ctx argExpr |> ignore
            TyVar(freshTyVar ctx)

    /// Resolve a constructor application on an external (BCL / referenced) class —
    /// shared by `new T(args)` (`inferNew`) and the *sugar* form `T args` (a ctor
    /// treated as a first-class function, routed here from `inferApp` via
    /// `tryInferExternalCtorApp`). `name` is the resolved metadata full name and
    /// `receiverTy` the `TyClass(name, args)` the call yields; the provider owns the
    /// `.ctor` catalogue. Overload-resolves on the argument types, then unifies the
    /// chosen ctor signature `(p1 * … * pN) → declTy` against `TyFun(argTy, result)`
    /// so each parameter constrains the call's arguments — the same shape as
    /// `tryInferExternalStaticMethodCall`.
    and private inferExternalCtorOn
        (ctx: PassContext)
        (key: NodeKey)
        (name: string)
        (args: EqArray<SemType>)
        (receiverTy: SemType)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let ctors = ctx.Provider.TryLookupMembers(name, ".ctor")

        if ctors.Length = 0 then
            ctx.Error(key, sprintf "External type '%s' has no accessible constructor" name)
            infer ctx argExpr |> ignore
            receiverTy
        else
            let argTy = infer ctx argExpr
            let typeArgs = args |> EqArray.toList |> List.toArray

            match pickStaticOverload typeArgs ctors (argElemsOf argTy) with
            | ValueSome chosen ->
                let ctorSig = ExternalSymbols.openSignature chosen typeArgs
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key ctorSig (TyFun(argTy, resultTy))
                unify ctx key resultTy receiverTy
                receiverTy
            | ValueNone ->
                ctx.Error(key, sprintf "No applicable constructor on '%s' for the given arguments" name)
                receiverTy

    /// The `new`-less constructor-as-function sugar: `InvalidOperationException "x"`,
    /// `ArgumentException(message, name)`. `inferApp` reaches here only after the
    /// head fails to resolve as a value / static method / union case / *user* class
    /// ctor — exactly the slot that previously fell to a fresh, unconstrained TyVar
    /// (the head's type leaked when buried in an argument, e.g. `raise (Exn "x")`,
    /// surfacing as a stray unresolved TyVar). The head must name an external class
    /// (resolved through the active `open`s) and not be a local binding (a real
    /// call). Multi-argument ctors arrive as one tupled arg, matching `inferNew`.
    and private tryInferExternalCtorApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        if args.Length <> 1 then
            ValueNone
        else
            let headName =
                match fn with
                | Expr.Ident tok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent)) ->
                    ValueSome(ctx.NameOf tok)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                    ->
                    ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
                | _ -> ValueNone

            match headName with
            | ValueNone -> ValueNone
            | ValueSome name ->
                match OpenScope.tryQualify ctx.Resolution.OpenScope (isExternalClass ctx) name with
                | ValueSome resolved ->
                    // Mint the ctor's result class with the resolved type's home
                    // assembly (its provider shape's `origin`) so it unifies with the
                    // same type resolved elsewhere.
                    let classKey =
                        match ctx.Provider.TryLookupType resolved with
                        | ValueSome(ExternalTypeShape.Class info) -> SymbolKeyOps.externalTypeKey info.Origin resolved 0
                        | _ -> SymbolKeyOps.qualifiedTypeKey resolved 0

                    ValueSome(
                        inferExternalCtorOn ctx key resolved EqArray.empty (TyClass(classKey, EqArray.empty)) args.[0]
                    )
                | ValueNone -> ValueNone

    /// Construction of an external *generic* class through an explicit type
    /// application: `ResizeArray<int>()`, `List<string>(cap)` — the no-`new`
    /// sugar whose CST is `App`/`HighPrecedenceApp(TypeApp(head, tyArgs), valueArgs)`.
    /// The generic sibling of `tryInferExternalCtorApp`: the head's explicit type
    /// arguments pin the element type up front (`ResizeArray<int>` →
    /// `TyClass(System.Collections.Generic.List`1, [int])`, an abbreviation expanded
    /// to its underlying class) so the constructed node carries `TyClass(List, [int])`
    /// rather than a free TyVar the value-args alone can't resolve for a
    /// parameterless ctor. The pinned class then drives `inferExternalCtorOn`'s
    /// overload pick (so `List()` vs `List(IEnumerable<int>)` resolves) and gives
    /// Freeze/codegen the `tyArgs` to emit `newobj List`1<!!T>::.ctor()`. A *local*
    /// generic class (`Box<int>(x)`) isn't an in-scope external type, so the resolver
    /// returns `ValueNone` and this declines — the local path (`inferTypeApp`'s
    /// nominal-unify arm) handles it.
    and private tryInferExternalGenericCtorApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match fn with
        | Expr.TypeApp(expr = headExpr; types = tyArgs) ->
            let headName =
                match headExpr with
                | Expr.Ident tok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent)) ->
                    ValueSome(ctx.NameOf tok)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                    ->
                    ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
                | _ -> ValueNone

            match headName with
            | ValueNone -> ValueNone
            | ValueSome name ->
                let explicit = EqArray.ofSeq (seq { for t in tyArgs -> translateType ctx t })

                match tryResolveExternalNominal ctx name explicit with
                | ValueSome(TyClass(clsKey, args) as receiverTy) ->
                    ValueSome(inferExternalCtorOn ctx key (SymbolKeyOps.qualifiedName clsKey) args receiverTy argExpr)
                | _ -> ValueNone
        | _ -> ValueNone

    /// Explicit type application on a value/constructor head: `Set<'T>(args)`
    /// (`set.fs` construction sites), `Box<int>(x)`, etc. The CST shape is
    /// `HighPrecedenceApp(TypeApp(head, [tyArgs]), valueArgs)`, so this types the
    /// `TypeApp` node to the head's curried ctor / function type — the enclosing
    /// App then unifies the value args as usual. The supplied type arguments are
    /// unified pairwise against the head's *nominal result* type arguments so the
    /// instantiation is pinned even when the value args alone wouldn't determine
    /// it (e.g. `ResizeArray<int>()`). A non-nominal result (a bare generic
    /// *function*, `id<int>`) carries its typars scattered through the function
    /// type rather than in a single nominal result; v1 leaves those to value-arg
    /// inference — the explicit args are a no-op there, matching eliding `<…>`.
    /// External generic-static *member* receivers (`EqualityComparer<int>.Default`)
    /// never reach here — they are a `DotLookup` over the `TypeApp`, handled by
    /// `tryExternalTypeReceiver` upstream.
    and private inferTypeApp
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (typeArgs: ImmutableArray<Type<SyntaxToken>>)
        : SemType =
        let innerTy = infer ctx inner
        let explicit = [ for t in typeArgs -> translateType ctx t ]

        let rec resultOf t =
            match resolveStep t with
            | TyFun(_, r) -> resultOf r
            | other -> other

        match resultOf innerTy with
        | TyClass(_, freshArgs)
        | TyUnion(_, freshArgs)
        | TyRecord(_, freshArgs) when freshArgs.Length = List.length explicit ->
            List.iter2 (fun fresh ex -> unify ctx key fresh ex) (EqArray.toList freshArgs) explicit
        | _ -> ()

        innerTy

    /// Value-level inline IL `(# "op" args : retTy #)`. The instruction string is
    /// opaque to the type-checker (the IL contract is the platform author's
    /// responsibility); we only type each operand so its own subtree is solved,
    /// and take the node's type from the declared result annotation (no annotation
    /// → `unit`). Value-level analogue of the type-level `Type.ILIntrinsic`.
    and private inferILIntrinsic
        (ctx: PassContext)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (returnType: ReturnType<SyntaxToken> voption)
        : SemType =
        for a in args do
            infer ctx a |> ignore

        match returnType with
        | ValueSome(ReturnType(typ = t)) -> translateType ctx t
        | ValueNone -> BuiltinTypes.tyUnit

    /// `expr when ^T : Type [and ^U : Type]* = optimizedExpr` — one clause of an
    /// F# library-only static optimization. Type the default `baseE` (its type is
    /// the node's type — the operator's declared result, e.g. `bool` for the
    /// equality family, `^T` for `(+)`) and type this clause's `optimizedExpr` so
    /// its own subtree (operands, nested inline IL) is solved.
    ///
    /// The clause body is **NOT** cross-unified with the base. F#'s static-opt
    /// rule is per-clause — "assume the constraint, then check the body against the
    /// return type": under `when ^T : int` the body's `int` matches the (then-also
    /// -`int`) declared result `^T`. The earlier blanket `unify baseTy optTy` only
    /// happens to work when every clause shares one concrete type (the equality
    /// family's `bool`); it wrongly fuses the distinct clause results of an
    /// `^T`-returning op — `byte`/`int16`/`^T` for `(+)` — and fails to unify them.
    /// We omit that check (a fully sound version would speculatively unify under
    /// the assumed constraint and undo — out of scope, by the
    /// no-speculative-unification stop); soundness rides on the clause being
    /// selected (and its body substituted) at expansion, where `^T` is concrete.
    ///
    /// The `when ^T : Type` constraints are a *compile-time dispatch*, NOT
    /// unification constraints, so the typar is **not** unified with its required
    /// type; it is translated only to record the verdict for `Inline.inlineExpand`
    /// to resolve at the call site. The typar resolves through `ctx.Resolution.TyparScope` —
    /// already seeded by the enclosing binding's parameters (`(x: ^T)`) — so the
    /// recorded `SemType` carries the binding's quantified root. See
    /// docs/operators-plan.md (the arithmetic/bitwise/unary task).
    and private inferLibraryOnlyStaticOptimization
        (ctx: PassContext)
        (key: NodeKey)
        (baseE: Expr<SyntaxToken>)
        (constraints: ImmutableArray<StaticOptimizationConstraint<SyntaxToken>>)
        (optimizedExpr: Expr<SyntaxToken>)
        : SemType =
        let baseTy = infer ctx baseE
        infer ctx optimizedExpr |> ignore

        let resolved =
            EqArray.ofSeq (
                seq {
                    for c in constraints do
                        match c with
                        | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar = tp; rhsType = rhs) ->
                            TStaticOptConstraint.TyconEquals(translateType ctx (Type.VarType tp), translateType ctx rhs)
                        | StaticOptimizationConstraint.WhenTyparIsStruct(typar = tp) ->
                            TStaticOptConstraint.IsStruct(translateType ctx (Type.VarType tp))
                }
            )

        ctx.StaticOpt.Set(key, resolved)
        baseTy

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`, whose
    /// head segment NameResolution resolved as a local binding; the remaining
    /// segments are a field-access chain.
    and private inferLongIdentFieldChain (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Bindings.Binding.TryGetValue headKey with
            | ValueSome rb -> instantiateBinding ctx rb
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Diagnose against the LongIdent's overall key — there's no
            // separate sub-expression NodeKey for an intermediate segment.
            currTy <- resolveFieldStep ctx key currTy segName

        currTy

    and private inferString
        (ctx: PassContext)
        (_key: NodeKey)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : SemType =
        // Freeze lowers interpolated strings to a `TExpr.Format` (D9) and reads
        // each hole's computed type back to emit `AppendFormatted<T>`; a `%d{x}`
        // specifier additionally constrains the hole.
        for part in parts do
            match part with
            | StringPart.Expr(formatSpecifier = fs; expr = e) ->
                let holeTy = infer ctx e

                match fs with
                | ValueSome ft ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
                    | ValueSome p ->
                        match PrintfSpec.argType (fun () -> TyVar(freshTyVar ctx)) p.Type with
                        | ValueSome t -> unify ctx (CstKeys.ofExpr e) holeTy t
                        | ValueNone -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
            | _ -> ()

        BuiltinTypes.tyString

    and private inferTypeAnnotation
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

    /// `obj` is the top of every reference hierarchy. `subsumes` doesn't model
    /// it (the BCL `System.Object` class isn't in `ctx.Types.Class`), so the
    /// coercion arms special-case it: a downcast / type-test from `obj` to any
    /// known type is statically admissible and resolved at runtime. The
    /// `set.fs:988` `(that :?> Set<'T>).Tree` site relies on this.
    and private isObjTy (t: SemType) : bool =
        match resolveStep t with
        | TyConst("obj", _) -> true
        | _ -> false

    /// `e :> T` — explicit upcast. `subsumes src tgt` must be `Equal`
    /// (redundant but legal) or `Subtype`; the result type is the target.
    and private inferStaticUpcast
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        match subsumes ctx srcTy tgtTy with
        | SubsumeOutcome.Equal
        | SubsumeOutcome.Subtype -> ()
        | SubsumeOutcome.Unrelated ->
            ctx.Error(
                key,
                sprintf "Cannot upcast type '%A' to '%A' — no inheritance relationship" (zonk srcTy) (zonk tgtTy)
            )

        tgtTy

    /// `e :? T` — type test. v1 requires the static types to be related in
    /// either direction (an unrelated test is statically always-false); the
    /// result is always `bool`.
    and private inferDynamicTypeTest
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t
        // The node's own type is `bool`; stash the tested-against type so Freeze
        // can carry it into `TExpr.TypeTest.testTy` for the `isinst` operand.
        ctx.Resolution.TypeTestTargets.Set(key, tgtTy)

        let related =
            isObjTy srcTy
            || subsumes ctx srcTy tgtTy <> SubsumeOutcome.Unrelated
            || subsumes ctx tgtTy srcTy <> SubsumeOutcome.Unrelated

        if not related then
            ctx.Warn(
                key,
                sprintf "Type test of '%A' against unrelated type '%A' is always false" (zonk srcTy) (zonk tgtTy)
            )

        BuiltinTypes.tyBool

    /// `e :?> T` — explicit downcast. The target must be a strict descendant of
    /// the source (`subsumes tgt src = Subtype`); an equal static type warns
    /// (redundant), an unrelated one errors. A downcast from `obj` is always
    /// admissible (checked at runtime).
    and private inferDynamicDowncast
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        if not (isObjTy srcTy) then
            match subsumes ctx tgtTy srcTy with
            | SubsumeOutcome.Subtype -> ()
            | SubsumeOutcome.Equal ->
                ctx.Warn(key, sprintf "Downcast is redundant — the static type '%A' already matches" (zonk srcTy))
            | SubsumeOutcome.Unrelated ->
                ctx.Error(key, sprintf "Cannot downcast type '%A' to unrelated type '%A'" (zonk srcTy) (zonk tgtTy))

        tgtTy

    /// Resolve a keyed `Dispose` for a `use` binder of *external* (BCL) type
    /// (vesper-set-sprint-phase-4 §4.3). Prefer the type's *own* declared `Dispose`
    /// — a duck-typed pattern dispose, including a non-`IDisposable` ref struct —
    /// then fall back to `System.IDisposable::Dispose` when the type implements the
    /// interface (the common BCL case: `Dispose` is declared on a base, so
    /// `TryLookupMember` — `DeclaredOnly` — misses it, but `GetInterfaces` surfaces
    /// `IDisposable` transitively). `ValueNone` ⇒ the type exposes no `Dispose`.
    and private tryExternalDispose (ctx: PassContext) (name: string) (args: EqArray<SemType>) : SymbolKey voption =
        match ctx.Provider.TryLookupMember(name, "Dispose") with
        | ValueSome m when not m.IsStatic && not m.IsProperty -> ValueSome m.Key
        | _ ->
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) when
                ExternalSymbols.instantiateInterfaces shape (args.AsSpan().ToArray())
                |> Array.exists (fun (n, _) -> n = "System.IDisposable")
                ->
                ValueSome(
                    SymbolKey.MemberKey(
                        SymbolKey.TypeKey(None, "System", "IDisposable"),
                        "Dispose",
                        EqArray.empty,
                        MemberKind.Method
                    )
                )
            | _ -> ValueNone

    /// Resolve the disposal target for one `use` binding (§4.3). A *project-local*
    /// binder keeps the duck-typed direct `Dispose()` call (codegen resolves it via
    /// the local member table), recorded as nothing so Freeze leaves
    /// `TExpr.Use.dispose = ValueNone`. An *external* binder's keyed `Dispose` is
    /// stashed in `UseDispose` for Freeze. A binder with no `Dispose` is a
    /// `use`-over-non-disposable error (C# parity); an unresolved binder type is
    /// left alone (pre-existing behaviour).
    and private resolveUseDispose (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.headPat with
        | Pat.NamedSimple _ ->
            let patKey = CstKeys.ofPat b.headPat
            let binderTy = zonk (TyVar(tvOf ctx patKey))

            match resolveStep binderTy with
            | TyClass(clsKey, args) ->
                let simple = SymbolKeyOps.simpleName clsKey

                match TypeRegistry.tryClass ctx.Types simple with
                | ValueSome _ ->
                    match tryClassChainMember ctx simple args "Dispose" with
                    | ValueSome _ -> ()
                    | ValueNone ->
                        ctx.Error(
                            patKey,
                            sprintf "The type '%s' has no 'Dispose' member; it cannot be used with 'use'" simple
                        )
                | ValueNone ->
                    let qual = SymbolKeyOps.qualifiedName clsKey

                    match tryExternalDispose ctx qual args with
                    | ValueSome key -> ctx.Resolution.UseDispose.Set(patKey, key)
                    | ValueNone ->
                        ctx.Error(
                            patKey,
                            sprintf "The type '%s' has no 'Dispose' member; it cannot be used with 'use'" qual
                        )
            | _ -> ()
        | _ -> ()

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (keyword: LetOrUseKeyword<SyntaxToken>)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings

        // `use` binds a disposable: resolve each binder's `Dispose` so an external
        // (BCL) disposal can be keyed for codegen and a non-disposable diagnosed
        // (§4.3). `let` skips this.
        match keyword with
        | LetOrUseKeyword.Use _
        | LetOrUseKeyword.UseBang _ ->
            for b in bindings do
                resolveUseDispose ctx b
        | LetOrUseKeyword.Let _
        | LetOrUseKeyword.LetBang _ -> ()

        infer ctx (CstWalk.requireLetBody body)

    and inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        // One typar scope per binding signature: explicit `<'a>` typars seed
        // it first so later implicit `'a` mentions share the same TyVar.
        let savedScope = ctx.Resolution.TyparScope
        ctx.Resolution.TyparScope <- Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

        match b.typarDefns with
        | ValueSome(TyparDefns(defns = ds; constraints = bindingConstraints)) ->
            for TyparDefn(typar = t) in ds do
                match t with
                | Typar.Named(ident = id)
                | Typar.Static(ident = id) ->
                    let n = ctx.NameOf id

                    if not (ctx.Resolution.TyparScope.ContainsKey n) then
                        // Reuse the member's prototype typar (B-12) when the seed
                        // names it, so the inferred signature shares roots with
                        // `TypeMemberInfo.MethodTypeParams`; otherwise mint fresh.
                        let tv =
                            match ctx.Resolution.BindingTyparSeed with
                            | ValueSome seed ->
                                match seed.TryGetValue n with
                                | true, proto -> proto
                                | _ ->
                                    let tv = TypeVar()
                                    tv.Level <- ctx.CurrentLevel
                                    tv
                            | ValueNone ->
                                let tv = TypeVar()
                                tv.Level <- ctx.CurrentLevel
                                tv

                        ctx.Resolution.TyparScope.[n] <- tv
                | Typar.Anon _ -> ()

            match bindingConstraints with
            | ValueSome cs -> translateConstraints ctx cs
            | ValueNone -> ()
        | ValueNone -> ()

        // The member-typar seed (B-12) is for this binding's own typars only;
        // clear it so a nested `let`-binding in the body mints fresh typars
        // rather than reusing the member's prototypes.
        ctx.Resolution.BindingTyparSeed <- ValueNone

        try
            let patTy = inferPat ctx b.headPat

            // Typar order is explicit `<'T>` → args → return → body, all sharing
            // one TyparScope. The return annotation is translated *before* the
            // body so a return-only typar (`let f () : 'T list = …`) seeds the
            // scope first; otherwise the body would mint a fresh `'T` and the
            // return would translate into a different one.
            let rhsTy =
                if b.argumentPats.IsEmpty then
                    match b.returnType with
                    | ValueSome(ReturnType(typ = t)) ->
                        let annTy = translateType ctx t
                        let bodyTy = infer ctx b.expr
                        unify ctx (CstKeys.ofBinding b) bodyTy annTy
                        annTy
                    | ValueNone -> infer ctx b.expr
                else
                    let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]

                    let bodyTy =
                        match b.returnType with
                        | ValueSome(ReturnType(typ = t)) ->
                            let annTy = translateType ctx t
                            let bodyTy = infer ctx b.expr
                            unify ctx (CstKeys.ofBinding b) bodyTy annTy
                            annTy
                        | ValueNone -> infer ctx b.expr

                    List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

            unify ctx (CstKeys.ofBinding b) patTy rhsTy
        finally
            ctx.Resolution.TyparScope <- savedScope

    /// Type a `let` / `let rec` group with Rémy-level discipline. Key
    /// subtlety: pre-allocate single-name sibling headPat TyVars (step 2) so
    /// forward references from inside one RHS (or a nested let) find the
    /// sibling's TyVar at this group's level rather than lazy-minting at a
    /// deeper one — which would let a nested let generalise a var that
    /// actually belongs to an un-typed outer sibling. RHSes type at the
    /// pushed level (sibling lookups stay monomorphic — no scheme written
    /// yet); generalisation happens against the outer level after popping.
    and inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.headPat with
            | Pat.NamedSimple _
            | Pat.Op _ -> tvOf ctx (CstKeys.ofPat b.headPat) |> ignore
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.headPat
                let headTv = tvOf ctx key
                let zonked = zonk (TyVar headTv)

                if not (hasPendingDotAccess zonked) then
                    // Settle flexible list-literal containers first (R3), then
                    // re-zonk so the (now-linked) FSharpList element generalises.
                    prepareListLiterals ctx zonked outerLevel
                    let scheme = generalise (zonk zonked) outerLevel
                    ctx.Bindings.Scheme.Set(key, scheme)
