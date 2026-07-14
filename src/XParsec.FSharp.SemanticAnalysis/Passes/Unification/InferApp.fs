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

    /// An operator whose symbol name resolution never resolved: no contract in the
    /// referenced set declares it. Spell it as the user WROTE it — the compiled name
    /// (`op_LessThan`) is an implementation detail they never typed. `sourceSymbol`
    /// inverts the lexer's own table, so the spelling cannot drift from the name;
    /// an operator outside that table (spelled out per-character by
    /// `generateOperatorName`) has no inverse and keeps its compiled name.
    ///
    /// The hint names no package deliberately. The failure IS that the declaring
    /// contract is absent from the referenced set, so nothing the compiler can see
    /// knows the operator exists — the only way to name `Vesper.Comparison` here
    /// would be a hardcoded operator→package table, i.e. exactly the by-name coupling
    /// that recognising operators by name string cost us everywhere else.
    let private unresolvedOperator (ctx: PassContext) (key: NodeKey) (name: string) : SemType =
        let spelling =
            match OperatorNames.sourceSymbol name with
            | ValueSome symbol -> symbol
            | ValueNone -> name

        errorTy
            ctx
            key
            (sprintf "No definition for '%s' found — is the package that defines it referenced and opened?" spelling)

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
                // peel `EnclosedBlock` / `TypeAnnotation` wrappers — `Elaborate` strips
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
                    // Resolve the family's external sink slot (`fprintf`'s `TextWriter`
                    // / `bprintf`'s `StringBuilder`) to the provider's `TyClass`
                    // through the SAME path a real sink arg (`System.Console.Out` /
                    // `StringBuilder()`) takes (`tryResolveExternalTypeKey`, arity 0),
                    // so both carry the identical `TypeKey` and a leading sink arg
                    // unifies with the slot directly. A slot whose name the provider
                    // doesn't surface keeps its by-name `TyConst` (no regression; in
                    // practice the resolving provider is the same one that types the
                    // real sink argument).
                    let fam =
                        fam
                        |> PrintfSpec.resolveExternalSlots (fun name ->
                            // The sink slot names (`System.IO.TextWriter`,
                            // `System.Text.StringBuilder`, `System.IO.StringWriter`) are
                            // FIXED and fully qualified, so the declaring type resolves by
                            // KEY on the store face — no opens-aware resolver call. The
                            // minted `TyClass` key matches a real sink argument's
                            // (`Console.Out`) exactly: the qualified name IS the identity.
                            match ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedTypeKey name 0) with
                            | ValueSome(ExternalTypeShape.Class info) when info.Arity = 0 ->
                                ValueSome(TyClass(SymbolKeyOps.qualifiedTypeKeyOfT name 0, EqArray.empty))
                            | _ -> ValueNone
                        )

                    let idx = fam.FormatArgIndex

                    if args.Length <= idx then
                        // Format argument not supplied (e.g. partially-applied
                        // `fprintf writer`); defer to standard inference.
                        ValueNone
                    else
                        // E1(b): the format position may hold not a syntactic literal
                        // but an `Ident` bound to one (`let fmt : Fmt = "%d" in sprintf
                        // fmt …`). Recover that literal and drive the SAME classify /
                        // typing / marker path a direct literal takes — the gate builds
                        // its own `PrintfFormat` shape and never consults the binding's
                        // (Vesper-faced) type, so there is no cold fallback needed and
                        // no face conflict. `formatRecovered` gates the 4a *partial*
                        // marker off (its heap-closure synthesis reads a literal at the
                        // format slot, which a bound `Ident` is not).
                        let recoveredFormat = ctx.TryRecoverFormatLiteral args.[idx]
                        let formatArg = ValueOption.defaultValue args.[idx] recoveredFormat
                        let formatRecovered = recoveredFormat.IsSome

                        match formatSpecifiers ctx formatArg with
                        | ValueNone -> ValueNone
                        | ValueSome specs ->
                            let fresh () = TyVar(freshTyVar ctx)

                            match PrintfSpec.appliedTypeOf fresh specs fam with
                            // A specifier we don't type in v1 (`%a` / `%t`);
                            // defer to standard inference.
                            | ValueNone -> ValueNone
                            | ValueSome(fnTy, fmtTy, _) ->
                                // Stamp the function node so Elaborate threads the
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

                                    match resolveStep currTy with
                                    | TyFun(dom, cod) ->
                                        // Uniform over every arg, leading writer included: the
                                        // writer slot is now the provider-resolved
                                        // `TyClass(TextWriter)`, so a real writer arg unifies with
                                        // it directly.
                                        unify ctx key argTy dom
                                        currTy <- cod
                                    | _ ->
                                        let resultTy = TyVar(freshTyVar ctx)
                                        unify ctx key currTy (TyFun(argTy, resultTy))
                                        currTy <- resultTy

                                // P1 happy-path lowering marker: fully-applied literal call, a
                                // lowerable sink, and every specifier lowerable → Elaborate mints a
                                // `TExpr.Format`. Full application is `idx` leading args + the
                                // format + one arg per hole (`specs.Length + idx + 1`). The
                                // console/string sinks put the format at arg 0 (`idx = 0`);
                                // `fprintf`/`fprintfn` (`Writer`) and `bprintf` (`Builder`) put a
                                // `TextWriter` / `StringBuilder` at arg 0 and the format at arg 1
                                // (`idx = 1`), threaded into the `Format` node as
                                // `FormatSink.ToWriter` / `FormatSink.ToBuilder`. Otherwise the
                                // FSharp.Core path stands (additive — `%A`, partial application,
                                // etc. unaffected).
                                // `%a`/`%t` callback holes lower only where THIS target's
                                // provider surfaces the family's sink type (see
                                // `PrintfSpec.callbackSinkAvailable`, reading the
                                // `resolveExternalSlots`-rewritten `fam`). No cold fallback once
                                // FSharp.Core is dropped, and the sink type this target can't name
                                // — reject rather than silently mis-lower. The marker guards below
                                // read `not rejectCallback`, so neither marker sets and the call
                                // stays cold (App path) today.
                                let hasCallbackHole =
                                    specs |> List.exists (fun p -> PrintfSpec.isCallbackHole p.Type)

                                let rejectCallback = hasCallbackHole && not (PrintfSpec.callbackSinkAvailable fam)

                                if rejectCallback then
                                    ctx.Diagnostics.Add
                                        {
                                            Key = key
                                            Message =
                                                "printf %a/%t requires a sink type (System.IO.TextWriter / System.Text.StringBuilder) not available on this target"
                                            Code = ""
                                            Severity = Severity.Error
                                        }

                                // Cold residuals — a specifier no backend renders faithfully
                                // (`%0*d`, `%0*.Nf`, `%0*A` — runtime-width zero-pad forms with no
                                // native handler, plus the F# `0`-flag quirk on `%*A`; and the
                                // forced-sign zero-pad floats `%+08.2f` / `% 08.2f`, whose only
                                // faithful lowering rounds half-away-from-zero rather than the
                                // half-to-even the engine uses elsewhere). There is no FSharp.Core
                                // cold fallback once the family lowers natively, so diagnose rather
                                // than route silently to a path that is being removed. `%a`/`%t`
                                // classify fine (Callback) and are handled by `rejectCallback`
                                // above, so any specifier `tryClassify` rejects here is a true
                                // residual. Additive — the marker guards below read
                                // `lowerablePlaceholders`, so a residual sets no marker regardless.
                                match specs |> List.tryFind (fun p -> (PrintfHoleForm.tryClassify p).IsNone) with
                                | Some p ->
                                    ctx.Diagnostics.Add
                                        {
                                            Key = key
                                            Message =
                                                sprintf
                                                    "printf format specifier %s cannot be lowered on this target"
                                                    (PrintfHoleForm.renderPlaceholder p)
                                            Code = ""
                                            Severity = Severity.Error
                                        }
                                | None -> ()

                                match PrintfSpec.sinkOf (qualifiedNameOf ctx fn) with
                                | ValueSome sink when
                                    not rejectCallback
                                    && args.Length = PrintfSpec.totalArity specs + idx + 1
                                    && lowerablePlaceholders specs
                                    && (idx = 0
                                        || (idx = 1
                                            && (
                                                match sink with
                                                | PrintfSpec.PrintfSink.Writer _
                                                | PrintfSpec.PrintfSink.Builder -> true
                                                | _ -> false
                                            )))
                                    ->
                                    ctx.PrintfApp.Set(key, sink)

                                    // Capture-first `%a`/`%t` on a *writer/builder family* lowers to
                                    // a residue block `{ let s = new Scratch() in cb s [v];
                                    // s.ToString() }`. A family needs a scratch iff it is not
                                    // `sprintf` (`PrintfSpec.familyNeedsScratch`) — keyed on the FAMILY
                                    // (writer families → `StringWriter`, `bprintf` → `StringBuilder`),
                                    // NOT the sink kind: `printf`/`eprintf` are writer families with a
                                    // `StdOut`/`StdErr` sink and still need a scratch. `sprintf`
                                    // (`ScratchSink = unit`) splices the callback's returned string, so
                                    // it gets NO entry — and that absence is Elaborate's sole signal to
                                    // take the sprintf path. Resolve the scratch + its *parameterless*
                                    // `ToString` ONCE here (the gate owns `ctx.Provider`) and stash it
                                    // for Elaborate, which has none.
                                    //
                                    // A scratch-needing family is only reached with `not
                                    // rejectCallback`, i.e. `callbackSinkAvailable` already saw its
                                    // `State` resolve to a provider `TyClass`; a `ScratchSink` that then
                                    // fails to resolve — or a resolved scratch class with no
                                    // parameterless `ToString` (impossible for `StringWriter` /
                                    // `StringBuilder`) — is a broken invariant, NOT a silent fall
                                    // through to sprintf, which would pass `unit` where the callback's
                                    // `State` sink is required.
                                    if hasCallbackHole && PrintfSpec.familyNeedsScratch fam then
                                        match fam.ScratchSink with
                                        | TyClass(scratchKey, _) as scratchTy ->
                                            let scratchName = SymbolKeyOps.typeMetaName scratchKey

                                            // `ToString` is overloaded (`StringBuilder.ToString(int,
                                            // int)`); pick the parameterless override, not the
                                            // most-params one `TryLookupMember` would return.
                                            let toString =
                                                ctx.Provider.TryLookupMembers(SymbolKey.Type scratchKey, "ToString")
                                                |> Array.tryFind (fun m -> m.Key.ArgSig.Length = 0)

                                            match toString with
                                            | Some m ->
                                                ctx.PrintfCallbackScratch.Set(
                                                    key,
                                                    {
                                                        ScratchClassName = scratchName
                                                        ScratchTy = scratchTy
                                                        ToStringKey = SymbolKey.Member m.Key
                                                    }
                                                )
                                            | None ->
                                                failwithf
                                                    "InferApp: writer/builder %%a/%%t scratch sink %s resolved to a class with no parameterless ToString — cannot lower capture-first"
                                                    scratchName
                                        | other ->
                                            failwithf
                                                "InferApp: writer/builder %%a/%%t scratch sink is unresolved (%A) though callbackSinkAvailable passed the State gate — resolveExternalSlots and the gate disagree"
                                                other
                                // 4a partial-application marker: a *fully-unapplied* lowerable
                                // literal partial (`printfn "%d"`, `printf "%d %s"`) — only the
                                // format is supplied (`args.Length = idx + 1`), `1..K` holes,
                                // none `%A`/`%O` (an unapplied `%A` hole is an unpinned typar,
                                // so out of scope for 4a). Elaborate synthesises a Vesper heap
                                // closure over the `EmitFormat` unroll instead of the FSharp.Core
                                // `PrintfFormat` cold path. Mutually exclusive with `PrintfApp`
                                // (which needs full application).
                                | ValueSome sink when
                                    idx = 0
                                    && not formatRecovered
                                    && args.Length = idx + 1
                                    && specs.Length >= 1
                                    && lowerablePlaceholders specs
                                    && specs |> List.forall PrintfSpec.isUnaryConcreteHole
                                    ->
                                    ctx.PrintfPartial.Set(key, sink)
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
        // A range's endpoints (and step) are constrained to int. Its result type is
        // NOT modelled: a range materialises no seq value in this compiler, so it is
        // legal ONLY as a `for … in` source (which types its own source and lowers to
        // a counted `ForTo` — `InferControlFlow.inferForIn`). Reaching this function
        // at all is therefore a range in VALUE position; the type is left `TyUnknown`
        // (concrete, so a surviving `TExpr.Range` freezes cleanly) and the unsupported
        // use is rejected at the lowering choke point, where position is known
        // (`ElaborateExpr.translateExpr`'s `Range` arms). `range-operators-plan.md`
        // tracks making `(..)` a real seq operator so a range becomes a first-class value.
        let fromTy = infer ctx fromE
        unify ctx key fromTy ctx.Intrinsics.Int

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx key stepTy ctx.Intrinsics.Int
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx key toTy ctx.Intrinsics.Int
        TyUnknown "range"

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
                match ctx.Resolution.ExternalSymbolStamp.TryGetValue key with
                | ValueSome sym ->
                    // Record the resolved identity so Elaborate stamps it onto the
                    // `TExpr.External(name, …)` it mints for this operator and
                    // `InlineExpansion` splices the contract's `let inline` body by KEY.
                    // EVERY resolved operator is stamped — there is no builtin-operator
                    // exclusion. A primitive `1 + 2` splices `ops-platform.fs`'s `(+)`
                    // exactly like a referenced package's operator does; the static-opt
                    // clause selection at splice time is what turns it into `add`.
                    ctx.Resolution.IntrinsicKey.Set(key, SymbolKey.Binding sym.Key)
                    let resultTy = TyVar(freshTyVar ctx)

                    unify
                        ctx
                        key
                        (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                        (TyFun(leftTy, TyFun(rightTy, resultTy)))

                    resultTy
                | ValueNone -> unresolvedOperator ctx key name
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

        match ctx.Resolution.ExternalSymbolStamp.TryGetValue key with
        | ValueSome sym ->
            // Thread the resolved `op_Dynamic` identity to Elaborate's `External` mint
            // (`translateDynamicLookup`, same `DynamicLookup` key) so the `$0[$1]`
            // body splices by KEY.
            ctx.Resolution.IntrinsicKey.Set(key, SymbolKey.Binding sym.Key)
            let resultVar = freshTyVar ctx
            let resultTy = TyVar resultVar

            unify
                ctx
                key
                (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                (TyFun(recvTy, TyFun(ctx.Intrinsics.String, resultTy)))

            // Record for the post-settle escape sweep: if context pins `resultVar` to a
            // concrete non-`dynamic` type the `default : dynamic` never fires — an
            // unchecked assertion `DynamicEscape.run` warns on (unless ascribed here).
            ctx.DynamicEscapes.Add { Root = resultVar; Key = key }
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

        match ctx.Resolution.ExternalSymbolStamp.TryGetValue key with
        | ValueSome sym ->
            // Thread the resolved `op_DynamicAssignment` identity to Elaborate's
            // `External` mint (`translateAssignment`'s `DynamicLookup` arm, keyed by
            // the enclosing `Assignment` node) so the `$0[$1] = $2` body splices by KEY.
            ctx.Resolution.IntrinsicKey.Set(key, SymbolKey.Binding sym.Key)

            unify
                ctx
                key
                (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel)
                (TyFun(recvTy, TyFun(ctx.Intrinsics.String, TyFun(valueTy, ctx.Intrinsics.Unit))))

            ctx.Intrinsics.Unit
        | ValueNone ->
            errorTy ctx key "dynamic-set operator '?<-' (op_DynamicAssignment) is not in scope (Vesper.Core missing?)"

    and inferPrefix (infer: Infer) (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName "op_AddressOf") ->
            // `&local` (managed address-of) is the byref intrinsic, not a
            // provider operator — `op_AddressOf` has no Vesper.Core / BCL symbol.
            // Type it directly as `TyConst("byref", [operandTy])` (mirroring
            // `inferIndexedLookup`'s byref wrapping) so it matches a BCL method's
            // byref/`out` parameter (`Int32.TryParse(string, int&)`). The operand
            // must be an addressable mutable local; that is enforced at codegen
            // (a `Var` bound to a slot), deferred here per the relax-then-reject
            // convention.
            //
            // This is the *consume* side only — `&local` as an argument into an
            // external method. Vesper source cannot yet *declare* a byref parameter /
            // return, nor write `byref<'T>` / `inref<'T>` / `outref<'T>` annotations,
            // and `&` stays a front-end special-case here rather than a resolved
            // `(~&)` contract operator with real lvalue analysis. Turning `&` / `~&`
            // into that operator (and general lvalue address-of beyond a mutable
            // local) is deferred — see `docs/byref-address-of-plan.md`. No consumer
            // needs the producer side today (`Formatter`/printf byref returns come
            // from BCL `Span.get_Item`).
            TyConst(RuntimeNames.byrefKey, EqArray.singleton operandTy)
        | ValueSome(DesugaredForm.OpName name) ->
            match ctx.Resolution.ExternalSymbolStamp.TryGetValue key with
            | ValueSome sym ->
                // Thread the resolved identity to Elaborate's `TExpr.External` mint (see
                // the infix twin above) so the prefix operator splices by KEY.
                ctx.Resolution.IntrinsicKey.Set(key, SymbolKey.Binding sym.Key)
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (ExternalSymbols.instantiateSymbol sym ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone -> unresolvedOperator ctx key name
        | ValueSome _
        | ValueNone -> TyVar(freshTyVar ctx)
