namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferLiteralExpr
open UnificationInferExternalCall
open UnificationInferCtor
open UnificationInferIdentExpr

module internal UnificationInferApp =

    /// Record the node-keyed `Fun`-arity verdict (and its
    /// result-typar position) for each source-lambda argument of an application.
    /// Walk the head's curried domains in lockstep with the source arguments; when a
    /// SOURCE lambda lands on a parameter whose typar bound is `:> Fun<a,b>`/`:> Fun<a,b,c>`
    /// (`funSlotArityOf`, the same nominal the `subsumes` arm matches), key the
    /// lambda's node → that flat arity so codegen sizes its value-struct `Invoke`.
    /// Must run BEFORE `inferGenericAppFrom` links the domain to the arrow (which
    /// would erase the bound). A non-lambda argument or a non-`Fun` slot records
    /// nothing. Pure side-effect into `ctx.FunVerdicts`.
    let private recordFunArityVerdicts (ctx: PassContext) (args: ImmutableArray<Expr<SyntaxToken>>) (fnTy: SemType) =
        let mutable currTy = fnTy
        // The lambda verdicts recorded in the spine walk, paired with the typar `dom`
        // (its union-find root) the lambda landed on — so a SECOND pass over the spine
        // RESULT can record `lambda → result-typar position` once `currTy`
        // reaches the tail. Recording the position in the loop is premature: `currTy`
        // is still the residual arrow, not the result nominal.
        let lambdaSlots = ResizeArray<NodeKey * SemType>()

        for i in 0 .. args.Length - 1 do
            match resolveStep currTy with
            | TyFun(dom, cod) ->
                // An argument lambda is usually parenthesised (`apply2 (fun … )`), so
                // peel `EnclosedBlock` / `TypeAnnotation` wrappers — `Freeze` strips
                // them transparently, anchoring the frozen `Lambda` on the inner
                // `Expr.Fun`'s FIRST parameter pattern's token (NOT the `fun` keyword).
                // Key the verdict on the SAME `(firstTokenOfPat arg0, ExprLambda)` the
                // frozen node carries so codegen's lookup matches.
                let rec peelLambda e =
                    match e with
                    | Expr.EnclosedBlock(expr = inner)
                    | Expr.TypeAnnotation(expr = inner) -> peelLambda inner
                    | Expr.Fun(argumentPats = argPats) when argPats.Length > 0 -> ValueSome argPats.[0]
                    | _ -> ValueNone

                match peelLambda args.[i] with
                | ValueSome arg0Pat ->
                    match funSlotArityOf dom with
                    | ValueSome arity ->
                        let lamKey = NodeKey.ofToken (CstKeys.firstTokenOfPat arg0Pat) NodeKind.ExprLambda
                        // Arity now; the result-typar position (if any) is filled in by
                        // the second pass below, once `currTy` reaches the result nominal.
                        ctx.FunVerdicts.Set(
                            lamKey,
                            {
                                Arity = arity
                                ResultTyparPos = ValueNone
                            }
                        )

                        lambdaSlots.Add(lamKey, dom)
                    | ValueNone -> ()
                | ValueNone -> ()

                currTy <- cod
            | _ -> currTy <- TyVar(freshTyVar ctx)

        // Record the result-typar POSITION for each verdict lambda.
        // `currTy` is now the spine's result type; a *transformer* combinator's result
        // is a nominal (`Holder<'TF>`, `MapSeq<…,'TF,…>`) carrying the lambda's typar
        // at some top-level arg index. Match by typar IDENTITY (the arg's union-find
        // root equals `dom`'s root), NOT by shape — a genuine function-valued arg of
        // the same arrow shape would otherwise be conflated. A *terminal* combinator
        // (`fold`/`apply2`, result `'State`/`int`) records nothing, so its stored
        // bindings are never rewritten.
        if lambdaSlots.Count > 0 then
            match resolveStep currTy with
            | TyConst(_, resArgs)
            | TyRecord(_, resArgs)
            | TyUnion(_, resArgs)
            | TyClass(_, resArgs)
            | TyTuple resArgs ->
                // Typar identity = the union-find ROOT (reference-stable); a free
                // `TyVar`'s `resolveStep` re-wraps a fresh `TyVar` each call, so compare
                // the underlying roots, not the wrappers.
                let rootOf (t: SemType) : TypeVar voption =
                    match resolveStep t with
                    | TyVar tv -> ValueSome(UnionFind.find tv)
                    | _ -> ValueNone

                for (lamKey, dom) in lambdaSlots do
                    match rootOf dom with
                    | ValueSome domRoot ->
                        let mutable found = ValueNone

                        for i in 0 .. resArgs.Length - 1 do
                            if ValueOption.isNone found then
                                match rootOf resArgs.[i] with
                                | ValueSome r when System.Object.ReferenceEquals(r, domRoot) -> found <- ValueSome i
                                | _ -> ()

                        match found with
                        | ValueSome idx ->
                            // Upgrade the arity-only verdict recorded in the first pass
                            // with the result-typar position (keys are a guaranteed subset).
                            match ctx.FunVerdicts.TryGetValue lamKey with
                            | ValueSome v ->
                                ctx.FunVerdicts.Set(
                                    lamKey,
                                    { v with
                                        ResultTyparPos = ValueSome idx
                                    }
                                )
                            | ValueNone -> ()
                        | ValueNone -> ()
                    | ValueNone -> ()
            | _ -> ()

    /// DIRECTIONAL constant admission at the external-arg seam (design §"a syntactic
    /// string/number CONSTANT argument admits by set membership … a plain `string`-typed
    /// NON-constant expression does NOT admit"). Returns `true` when it HANDLED the
    /// position — either the constant is in the literal set (admitted; the runtime value
    /// already IS the literal, so no unify and no wrapper) or it is NOT (a type error
    /// naming the allowed set). Returns `false` for a non-literal slot OR a non-constant
    /// argument, so those fall through to the ordinary `unifyArg` (which rejects a plain
    /// `string` into a literal union via the directional `subsumes` layer — item 3). The
    /// arg EXPRESSION (not just its `string` type) is what lets the constant be seen —
    /// the printf-format precedent for call-site constant propagation.
    let private tryAdmitLiteralConstArg
        (ctx: PassContext)
        (key: NodeKey)
        (argExpr: Expr<SyntaxToken>)
        (dom: SemType)
        : bool =
        // Syntactic check FIRST: it is a cheap peel, while the slot check
        // ground-folds the parameter type (`evalTypeLevel` is a deep rebuild) —
        // ordering confines the fold to the rare constant-argument case instead of
        // running it for every argument of every application. Result-identical:
        // both orders return `false` unless BOTH succeed.
        match constStringArg ctx argExpr with
        | ValueNone -> false
        | ValueSome s ->
            let lit = LiteralConst.String s

            // Ground-fold a carried node first: a `keyof Events` parameter (`TyKeyOf`)
            // folds to its literal-name union here, so a syntactic string constant
            // admits into it by the same set-membership rule as an explicit literal
            // union (a bare `TyLiteral` slot is a singleton set).
            match tryLiteralMembers (evalTypeLevel ctx (resolveStep dom)) with
            | ValueNone -> false
            | ValueSome members ->
                if List.contains lit members then
                    true
                else
                    let allowed = members |> List.map (fun v -> v.Render) |> String.concat " | "

                    ctx.Error(key, sprintf "%s is not one of the allowed literal values: %s" lit.Render allowed)

                    true

    let rec inferApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        // The tupled, .NET-style probes (static method, generic/local ctor,
        // instance method) only apply to a single tupled argument; `single` runs
        // one of them iff there is exactly one arg, declining otherwise.
        let single probe =
            if args.Length = 1 then
                probe ctx key fn args.[0]
            else
                ValueNone

        // The generic curried-application fallback: the head is a function, each
        // arg unifies against the next domain. Reached only when no specialised
        // probe claims the call. `fn` is inferred by the caller and threaded in so
        // the optional-argument fill (which needs `fn`'s recorded external member)
        // and this loop share one inference of the head.
        let inferGenericAppFrom (fnTy: SemType) (argTys: SemType[]) =
            let mutable currTy = fnTy

            for i in 0 .. argTys.Length - 1 do
                let argTy = argTys.[i]

                match resolveStep currTy with
                | TyFun(dom, cod) ->
                    // A literal / literal-union parameter consults the argument
                    // EXPRESSION for a syntactic constant (directional admission); when
                    // it handles the slot, skip `unifyArg` (which would reject the
                    // `string`-typed constant). Otherwise:
                    // allow an implicit class→interface / class→base upcast on the
                    // argument: a `Comparer<'T>` value flows into an `IComparer<'T>`
                    // parameter. `unifyArg` accepts a ground subtype and otherwise falls
                    // back to plain unification (which links vars and reports a genuine
                    // mismatch — including a plain `string` into a literal union).
                    if not (tryAdmitLiteralConstArg ctx key args.[i] dom) then
                        unifyArg ctx key argTy dom

                    currTy <- cod
                | _ ->
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key currTy (TyFun(argTy, resultTy))
                    currTy <- resultTy

            currTy

        // Specialised resolution probes, tried in order; the first `ValueSome`
        // wins. The order is load-bearing — a non-spaced external ctor must reach
        // `tryInferExternalCtorApp` before the generic fallback types its class
        // name as a function and leaks a fresh, unpinned result TyVar (which only
        // generalises when it reaches the binding's type; buried as an argument it
        // dangles, and `ResolvedTypes` flags it). A .NET static method is tupled
        // (`String.Concat ("a", "b")` is one tuple arg), resolved by its arg types
        // at the call site before the curried path.
        tryInferPrintfApp infer ctx key fn args
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalStaticMethodCall infer))
        |> ValueOption.orElseWith (fun () -> tryInferExternalCtorApp infer ctx key fn args)
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalGenericCtorApp infer))
        |> ValueOption.orElseWith (fun () -> single (tryInferLocalCtorApp infer))
        |> ValueOption.orElseWith (fun () -> single (tryInferExternalInstanceMethodCall infer))
        |> ValueOption.defaultWith (fun () ->
            // Infer the head and each argument once, then either fill omitted trailing
            // optional arguments (an external method call short of its full arity) or
            // run the generic curried-application loop. Both consumers need the args
            // inferred, and the optional-fill probe may inspect the single arg's arity
            // and then decline — so sharing one inference avoids re-inferring it here.
            let fnTy = infer ctx fn
            let argTys = [| for a in args -> infer ctx a |]

            // Record the node-keyed `Fun`-arity + result-typar
            // verdicts for any source-lambda arguments, BEFORE the curried-application
            // loop below links each domain to its arrow (which would erase the `:> Fun`
            // bound the verdict reads).
            recordFunArityVerdicts ctx args fnTy

            tryFillOptionalCall ctx key fn args argTys
            |> ValueOption.defaultWith (fun () -> inferGenericAppFrom fnTy argTys)
        )

    /// Printf-family typing rule. For a recognised
    /// printf entry point with a plain-literal format argument, the format spec
    /// — not the literal's apparent `string` type — drives the call's curried
    /// result type. The format argument types as `PrintfFormat<printer, …>`.
    and tryInferPrintfApp
        (infer: Infer)
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

    and inferHighPrecApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — the same call as `Expr.App fn [|arg|]`, only a separate CST case
        // (the parser splits on the space before `(`). Associativity is already
        // resolved; the inference rule must not differ between the two. Route through
        // `inferApp` so the non-spaced form gets the *same* probe chain — printf,
        // external static method, external ctor sugar, instance-method overload,
        // generic/local ctor — as the spaced form, instead of skipping straight to
        // the generic-application fallback (which typed an external ctor head as a
        // function and leaked a fresh, unpinned result TyVar under a non-pinning sink
        // like `raise`).
        inferApp infer ctx key fn (ImmutableArray.Create arg)

    and inferRange
        (infer: Infer)
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

    and inferInfix
        (infer: Infer)
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

                    unify
                        ctx
                        key
                        (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                        (TyFun(leftTy, TyFun(rightTy, resultTy)))

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

    /// `recv?name` — the dynamic-access operator (F# spec 6.4.5: `x ? ident`
    /// desugars to `(?) x "ident"`). Resolve `op_Dynamic` and instantiate it exactly
    /// as `inferInfix` does an operator, unifying against `recv -> string -> ^TResult`.
    /// The operator's declared `target: dynamic` parameter enforces the STRICT dynamic
    /// receiver (a non-`dynamic` receiver fails the unify), and its `default ^TResult :
    /// dynamic` rides on the instantiated result var — so an unconstrained context keeps
    /// the result `dynamic` (chains stay dynamic) while a pinned context unifies it
    /// first and the default never fires (the principled escape back to static).
    and inferDynamicLookup (infer: Infer) (ctx: PassContext) (key: NodeKey) (recv: Expr<SyntaxToken>) : SemType =
        let recvTy = infer ctx recv

        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "op_Dynamic" with
        | ValueSome sym ->
            let resultTy = TyVar(freshTyVar ctx)

            unify
                ctx
                key
                (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                (TyFun(recvTy, TyFun(BuiltinTypes.tyString, resultTy)))

            resultTy
        | ValueNone -> errorTy ctx key "dynamic-access operator '?' (op_Dynamic) is not in scope (Vesper.Core missing?)"

    /// `recv?name <- value` — the dynamic-set operator (`(?<-) recv "name" value`).
    /// Resolve `op_DynamicAssignment` and unify against `recv -> string -> value ->
    /// unit`. Result is `unit` (an assignment).
    and inferDynamicSet
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (recv: Expr<SyntaxToken>)
        (value: Expr<SyntaxToken>)
        : SemType =
        let recvTy = infer ctx recv
        let valueTy = infer ctx value

        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup "op_DynamicAssignment" with
        | ValueSome sym ->
            unify
                ctx
                key
                (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                (TyFun(recvTy, TyFun(BuiltinTypes.tyString, TyFun(valueTy, BuiltinTypes.tyUnit))))

            BuiltinTypes.tyUnit
        | ValueNone ->
            errorTy ctx key "dynamic-set operator '?<-' (op_DynamicAssignment) is not in scope (Vesper.Core missing?)"

    and inferPrefix (infer: Infer) (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName "op_AddressOf") ->
            // `&local` (managed address-of) is the byref intrinsic, not a
            // provider operator — `op_AddressOf` has no Vesper.Core / BCL symbol.
            // Type it directly as `TyConst("&", [operandTy])` (mirroring
            // `inferIndexedLookup`'s byref wrapping) so it matches a BCL method's
            // byref/`out` parameter (`Int32.TryParse(string, int&)`). The operand
            // must be an addressable mutable local; that is enforced at codegen
            // (a `Var` bound to a slot), deferred here per the relax-then-reject
            // convention.
            //
            // TODO(byref-producer): this is the *consume* side only — `&local` as an
            // argument into an external method. Vesper source cannot yet *declare* a
            // byref parameter / return, nor use the `byref<'T>` / `inref<'T>` /
            // `outref<'T>` type aliases as annotations (no alias → byref-intrinsic
            // resolution is wired; only the `&` prefix is). `Formatter`/printf needs
            // neither (its byref returns come from BCL `Span.get_Item`), so the
            // producer side is unbuilt.
            TyConst(RuntimeNames.byrefName, EqArray.singleton operandTy)
        | ValueSome(DesugaredForm.OpName name) ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone -> errorTy ctx key (sprintf "Unknown prefix operator: %s" name)
        | ValueSome _
        | ValueNone -> TyVar(freshTyVar ctx)
