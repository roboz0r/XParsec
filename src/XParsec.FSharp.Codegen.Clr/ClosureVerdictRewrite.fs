namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// `map f src` freezes as `MapSeq<…, (int->int), …>` but the call produces a `<closure>$`
/// value-struct: rewrite that argument to the value-struct so the `constrained.` token matches
/// the value. Keyed by lambda NODE, so two `int->int` transformers are never conflated.
module internal ClosureVerdictRewrite =

    type Rewrite =
        {
            /// Retype a body so a reference to a verdict binding (`Var h` / `h.F`) or an
            /// inline transformer call carries the value-struct, not the function type.
            RetypeBody: TastAccessor.ExprId -> TastAccessor.ExprId
            RetypeDecl: TastAccessor.DeclId -> TastAccessor.DeclId
            /// The rewritten container if the key is a verdict binding, else the declared
            /// type unchanged.
            ModuleValueSlotType: BoundVarId -> FrozenType -> FrozenType
        }

    /// * `closureValueTypeByNode` — value-struct closure's Lambda node → its `<closure>$` type.
    /// * `funVerdicts` — source-lambda node → the type-arg POSITION its `'TFunc` occupies in
    ///   the producing transformer's result nominal (`ResultTyparPos`, the only field read).
    let build
        (closureValueTypeByNode: IReadOnlyDictionary<TastAccessor.ExprId, FrozenType>)
        (funVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>)
        (enumeratorOf: FrozenType -> FrozenType voption)
        (moduleValues: (BoundVarId * FrozenType * TastAccessor.ExprId) seq)
        : Rewrite =

        // Lambda node → its `<closure>$` value-struct + the result-typar POSITION its
        // `'TFunc` occupies. `closureValueTypeByNode`'s keys ARE exactly the value-struct
        // closure nodes, so iterate it rather than re-testing a closure's shape.
        let closureNodeVerdict =
            let d = Dictionary<TastAccessor.ExprId, struct (FrozenType * int)>()

            for KeyValue(node, closureFt) in closureValueTypeByNode do
                match funVerdicts.TryGetValue node with
                | true, { ResultTyparPos = ValueSome idx } -> d.[node] <- struct (closureFt, idx)
                | _ -> ()

            d

        // Each verdict module value's rewritten field type + its `function → closure` argument
        // replacements. Filled in DECLARATION order, so a chained `let s2 = map g s1` can
        // read the ALREADY-rewritten type of `s1`.
        let verdictBindings =
            Dictionary<BoundVarId, FrozenType * (FrozenType * FrozenType) list>()

        // Replace a stored binding's `'TFunc` argument with the value-struct closure its
        // initialiser produces, matched by POSITION, so a genuine function-valued field of
        // the same shape is untouched. Returns the container type + the argument replacements.
        let substituteVerdictClosures
            (ty: FrozenType)
            (init: TastAccessor.ExprId)
            : FrozenType * (FrozenType * FrozenType) list =
            // This initialiser's verdict lambdas, by result-typar position → `<closure>$` type.
            let slots = Dictionary<int, FrozenType>()

            // `let s2 = map g s1` froze before any closure was minted, so the positions `s2`
            // inherited from `s1` (its `'S` source slot, its `'E` enumerator) still spell
            // function types where `s1`'s rewritten field lays out `<closure>$`. Map old→new.
            let nestedSubst = Dictionary<FrozenType, FrozenType>()

            // An OLD nominal maps to exactly ONE NEW one. Re-recording the same target is
            // idempotent (the lockstep walk revisits shared subtrees); a different target
            // means two closures reached one nominal, so fail rather than corrupt the signature.
            let record (oldT: FrozenType) (newT: FrozenType) =
                match nestedSubst.TryGetValue oldT with
                | true, existing when existing <> newT ->
                    failwithf
                        "ClosureVerdictRewrite: nested-substitution collision — one OLD nominal mapped to two NEW ones. Only linear chains are supported; a multi-source combinator (`zip s1 s2`, two same-typed arguments bound to different closures) reaches this.\n  old: %A\n  new1: %A\n  new2: %A"
                        oldT
                        existing
                        newT
                | _ -> nestedSubst.[oldT] <- newT

            // Lockstep walk of a referenced binding's (old, rewritten) pair: record every
            // differing NOMINAL subtree, then recurse pairwise into its args so a deeper
            // binding's enumerator is caught too. Equal subtrees and bare leaves are skipped.
            let recordArgs recurse (ao: EqArray<FrozenType>) (an: EqArray<FrozenType>) =
                for i in 0 .. ao.Length - 1 do
                    recurse ao.[i] an.[i]

            let rec recordNominalDiff (oldT: FrozenType) (newT: FrozenType) =
                if oldT <> newT then
                    match oldT, newT with
                    | FTClass(ko, ao), FTClass(kn, an) when ko = kn && ao.Length = an.Length ->
                        record oldT newT
                        recordArgs recordNominalDiff ao an
                    | FTUnion(ko, ao), FTUnion(kn, an) when ko = kn && ao.Length = an.Length ->
                        record oldT newT
                        recordArgs recordNominalDiff ao an
                    | FTRecord(ko, ao), FTRecord(kn, an) when ko = kn && ao.Length = an.Length ->
                        record oldT newT
                        recordArgs recordNominalDiff ao an
                    | FTConst(no, ao), FTConst(nn, an) when no = nn && ao.Length = an.Length ->
                        record oldT newT
                        recordArgs recordNominalDiff ao an
                    | FTTuple ao, FTTuple an when ao.Length = an.Length ->
                        record oldT newT
                        recordArgs recordNominalDiff ao an
                    // A differing argument (`FTFun` → `<closure>$`) or a swapped type
                    // constructor: the caller already recorded the nominal enclosing it.
                    | _ -> ()

            let rec collect (e: TastAccessor.ExprId) =
                match closureNodeVerdict.TryGetValue e with
                | true, struct (closureFt, idx) -> slots.[idx] <- closureFt
                | false, _ -> ()

                match e with
                | TastAccessor.EVar k ->
                    let varTy = TastAccessor.exprTy e

                    match verdictBindings.TryGetValue k with
                    | true, (newTy, _) ->
                        // The referenced binding contributes two nested positions: its `'S`
                        // source slot (`varTy`→`newTy`) and its `'E` enumerator, derived by
                        // the witness from that SAME pair rather than from an `FTFun` argument.
                        recordNominalDiff varTy newTy

                        match enumeratorOf varTy, enumeratorOf newTy with
                        | ValueSome eOld, ValueSome eNew -> recordNominalDiff eOld eNew
                        | _ -> ()
                    | false, _ -> ()
                | _ -> ()

                TastAccessor.iterChildren collect e

            collect init

            if slots.Count = 0 then
                ty, []
            else
                // The replaced `FTFun` leaves paired with their closures: a projection `h.F`
                // is typed as the function that occupied the slot, so the body must retype
                // that projection to the value-struct.
                let replaced = ResizeArray<FrozenType * FrozenType>()

                // A recorded earlier-binding nominal is replaced WHOLESALE and not descended
                // into, because its own buried `FTFun` already rode in via the recorded NEW nominal.
                let rec deep (t: FrozenType) : FrozenType =
                    match nestedSubst.TryGetValue t with
                    | true, newTy -> newTy
                    | false, _ -> TastLower.mapFrozenArgs (EqArray.map deep) t

                // Each recorded position takes this binding's own closure; every other arg is
                // deep-rewritten, so a nested nominal keeps ITS `'TFunc` slot rather than
                // being clobbered with this binding's closure.
                let rwArgs (args: EqArray<FrozenType>) : EqArray<FrozenType> =
                    args
                    |> EqArray.mapi (fun i a ->
                        match slots.TryGetValue i with
                        | true, closureFt ->
                            replaced.Add(a, closureFt)
                            closureFt
                        | false, _ -> deep a
                    )

                TastLower.mapFrozenArgs rwArgs ty, List.ofSeq replaced

        for (key, ty, init) in moduleValues do
            match substituteVerdictClosures ty init with
            | _, [] -> ()
            | newTy, replaced -> verdictBindings.[key] <- (newTy, replaced)

        // The gate that keeps the `App`-result rewrite a no-op when no stored verdict binding
        // exists but a nested temp does (`fold f 0 (map …)`).
        let hasTransformerVerdict = closureNodeVerdict.Count > 0

        // The verdict of the lambda a single application feeds: walk this `App` chain's own
        // arguments for a value-struct closure node. A combinator takes at most one
        // `Fun`2`/`Fun`3`-bounded lambda argument, so at most one verdict is found.
        let appOwnVerdict (e: TastAccessor.ExprId) : struct (FrozenType * int) voption =
            let rec scan (e: TastAccessor.ExprId) : struct (FrozenType * int) voption =
                match e with
                | TastAccessor.EApp app ->
                    match closureNodeVerdict.TryGetValue app.Arg with
                    | true, v -> ValueSome v
                    | false, _ -> scan app.Fn
                | _ -> ValueNone

            scan e

        // In a producing transformer's result (`MapSeq<…,fn,…>`), replace the `'TFunc`
        // argument with THIS call's own `<closure>$`. A same-shaped function type at any other
        // position is untouched, because the nested source slot is rewritten by its own producing site.
        let rewriteAppResultByVerdict (resultTy: FrozenType) (verdict: struct (FrozenType * int)) : FrozenType =
            let struct (closureFt, pos) = verdict
            // Position-only, not recursive: one application rewrites its own slot. An
            // out-of-range `pos` matches no index and leaves the type unchanged.
            resultTy
            |> TastLower.mapFrozenArgs (EqArray.mapi (fun i a -> if i = pos then closureFt else a))

        // A `FieldGet` off a verdict binding has its own type, the projected function,
        // mapped to the closure via that binding's recorded replacements; the object arg is
        // retyped to the container so its field `TypeSpec` matches the instantiated field row.
        let retypeBody (e: TastAccessor.ExprId) : TastAccessor.ExprId =
            if verdictBindings.Count = 0 && not hasTransformerVerdict then
                e
            else
                // The verdict binding a (possibly nested-field) object arg bottoms out in,
                // for mapping a projection's function type to its closure.
                let rec objArgBinding (r: TastAccessor.ExprId) : BoundVarId voption =
                    match r with
                    | TastAccessor.EVar k ->
                        if verdictBindings.ContainsKey k then
                            ValueSome k
                        else
                            ValueNone
                    | TastAccessor.EFieldGet fg -> objArgBinding fg.ObjArg
                    | _ -> ValueNone

                // Re-author ONLY the affected nodes: closure discovery keys lambdas by node
                // id, so a blanket rebuild would give every lambda beneath a rewritten node a
                // fresh id the verdict tables no longer recognise.
                let rec rw (e: TastAccessor.ExprId) : TastAccessor.ExprId =
                    match e with
                    | TastAccessor.EVar k ->
                        match verdictBindings.TryGetValue k with
                        | true, (newTy, _) -> TastAccessor.retype e newTy
                        | false, _ -> e
                    | TastAccessor.EFieldGet fg ->
                        let ty = TastAccessor.exprTy e
                        let objArg' = rw fg.ObjArg

                        let ty' =
                            match objArgBinding fg.ObjArg with
                            | ValueSome k ->
                                let _, replaced = verdictBindings.[k]

                                replaced
                                |> List.tryPick (fun (funTy, closureTy) -> if funTy = ty then Some closureTy else None)
                                |> Option.defaultValue ty
                            | ValueNone -> ty

                        TastAccessor.retypeWithChildren e [| objArg' |] ty'
                    | TastAccessor.EApp app ->
                        let ty = TastAccessor.exprTy e
                        // `fold f 0 (map g src)` — a transformer argument with no stored
                        // `let s1`. The fold call's `'S` MethodSpec reads this `App`'s type,
                        // so its `'TFunc` slot must already spell the value-struct.
                        let ty' =
                            match appOwnVerdict e with
                            | ValueSome verdict -> rewriteAppResultByVerdict ty verdict
                            | ValueNone -> ty

                        TastAccessor.retypeWithChildren e [| rw app.Fn; rw app.Arg |] ty'
                    | _ -> TastAccessor.mapChildren rw e

                rw e

        let retypeDecl (d: TastAccessor.DeclId) : TastAccessor.DeclId = TastAccessor.mapDeclExpr retypeBody d

        {
            RetypeBody = retypeBody
            RetypeDecl = retypeDecl
            ModuleValueSlotType =
                fun key declared ->
                    match verdictBindings.TryGetValue key with
                    | true, (newTy, _) -> newTy
                    | false, _ -> declared
        }
