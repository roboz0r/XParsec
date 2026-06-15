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
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferLiteralExpr
open UnificationInferExternalCall
open UnificationInferCtor
open UnificationInferIdentExpr

module internal UnificationInferApp =

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

            for argTy in argTys do
                match resolveStep currTy with
                | TyFun(dom, cod) ->
                    // Allow an implicit class→interface / class→base upcast on the
                    // argument (G19): a `Comparer<'T>` value flows into an
                    // `IComparer<'T>` parameter. `unifyArg` accepts a ground subtype
                    // and otherwise falls back to plain unification (which links vars
                    // and reports a genuine mismatch).
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

            tryFillOptionalCall ctx key fn args argTys
            |> ValueOption.defaultWith (fun () -> inferGenericAppFrom fnTy argTys)
        )

    /// Printf-family typing rule (front-end-gaps-plan §B). For a recognised
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
        // like `raise`). Infer-resolution-gaps-plan.md Gap A.
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
            // convention (PP5d).
            //
            // TODO(byref-producer): this is the *consume* side only — `&local` as an
            // argument into an external method. Vesper source cannot yet *declare* a
            // byref parameter / return, nor use the `byref<'T>` / `inref<'T>` /
            // `outref<'T>` type aliases as annotations (no alias → byref-intrinsic
            // resolution is wired; only the `&` prefix is). `Formatter`/printf needs
            // neither (its byref returns come from BCL `Span.get_Item`, PP2b), so the
            // producer side is unbuilt.
            TyConst(RuntimeNames.byrefName, EqArray.singleton operandTy)
        | ValueSome(DesugaredForm.OpName name) ->
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone -> errorTy ctx key (sprintf "Unknown prefix operator: %s" name)
        | ValueSome _
        | ValueNone -> TyVar(freshTyVar ctx)
