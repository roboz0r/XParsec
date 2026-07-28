namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// The closure-verdict TAST type rewrite, lifted out of `Assembler`.
///
/// A *transformer* combinator (`map`, `mk : ('TF:>Fun) -> Holder<'TF>`) returns a
/// nominal carrying the lambda's function typar; the front end freezes that result
/// with `'TFunc := arrow`, which `encodeType` lowers to the `Vesper.Fun`2`/`Fun`3`
/// INTERFACE — but the call actually produces a `<closure>$` value-struct, so the
/// stored slot / `constrained.` dispatch token disagrees with the value (corruption /
/// `EntryPointNotFoundException`). Given the value-struct closures' minted nominal
/// types (`closureValueTypeByNode`) and the front end's result-typar verdicts
/// (`FunVerdict.ResultTyparPos`), this pass rewrites the *producing* `'TFunc`-position
/// leaf — in stored module-value field types and in body references to them
/// (`substituteVerdictClosures` / `ModuleValueSlotType`), and in the inline `App`-result
/// type of a transformer call used directly as an argument (`appOwnVerdict` /
/// `rewriteAppResultByVerdict`) — to the value-struct nominal, so token and value agree.
/// Both paths are node-identity keyed.
///
/// The CONSUMING combinator's `for-in` enumerator (`fold`'s `for y in source`) is NO
/// LONGER rewritten here: that body is genuinely generic over
/// the phantom enumerator typar `'E`, so the closure rides in as a real `MethodSpec`
/// type-argument the call site solves (`EmitCall`) — there is no grounded arrow leaf to
/// patch, and the old collision-prone arrow-equality rewrite is gone.
///
/// Pure `TastAccessor.ExprId` / `FrozenType` / `BinderId` traffic — the same family
/// `TastLower` already shares between backends. It depends on NOTHING CLR-specific (no
/// provider, layout, or `Emit.Closure`): the only backend input is the already-minted
/// node→nominal map, which any backend that lowers a captureless lambda to a nominal
/// can populate. A later move to `Codegen.Common` (so the JS backend can opt in) is
/// therefore a file relocation, not a rewrite.
module internal ClosureVerdictRewrite =

    /// The product: the body / decl retype closures every emission entry point routes
    /// through, plus the field-slot type lookup the field-table pass needs. The
    /// `verdictBindings` table itself stays private — both consumers go through these.
    type Rewrite =
        {
            /// Retype a body expression so a reference to a verdict binding (`Var h` /
            /// `h.F`) or an inline transformer call dispatches on the value-struct
            /// nominal rather than the frozen arrow. A no-op when there are no verdicts.
            RetypeBody: TastAccessor.ExprId -> TastAccessor.ExprId
            /// `RetypeBody` lifted to a top-level `TDecl` (the Main path) — the single
            /// place that knows how to reach the expressions inside a `TDecl`.
            RetypeDecl: TastAccessor.DeclId -> TastAccessor.DeclId
            /// The field-slot type for a module value: the rewritten container if it is
            /// a verdict binding, else the declared type unchanged.
            ModuleValueSlotType: BinderId -> FrozenType -> FrozenType
        }

    /// Build the rewrite from the backend-neutral inputs:
    /// * `closureValueTypeByNode` — each value-struct closure's Lambda NODE (a pool id
    ///   with the pool that issued it) → its minted `<closure>$` value-struct `FrozenType`.
    /// * `funVerdicts` — a source-lambda NODE → its `FunVerdict`; the
    ///   `ResultTyparPos` names the type-arg POSITION its `'TFunc` occupies in the
    ///   producing transformer's result nominal (the only field this pass reads).
    /// * `enumeratorOf` — the seq→enumerator witness: given a (rewritten) seq nominal,
    ///   the enumerator type its seq-interface impl produces, or `ValueNone` for a
    ///   non-seq nominal. The structural relationship the type system defines (the
    ///   codegen analog of `EmitResolve.tryInterfaceWitness`); used to rewrite a chained
    ///   binding's nested `'E` slot NODE-KEYED, never by matching an arrow leaf.
    /// * `moduleValues` — each stored module value as `(key, declared type, initialiser)`.
    let build
        (closureValueTypeByNode: IReadOnlyDictionary<TastAccessor.ExprId, FrozenType>)
        (funVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>)
        (enumeratorOf: FrozenType -> FrozenType voption)
        (moduleValues: (BinderId * FrozenType * TastAccessor.ExprId) seq)
        : Rewrite =

        // Per-value-struct-closure VERDICT, keyed by the closure's Lambda node's POOL ID
        // → its `<closure>$` value-struct + the RESULT-typar POSITION its `'TFunc`
        // occupies in the producing transformer's result nominal.
        // `closureValueTypeByNode`'s keys ARE exactly the value-struct closure nodes, so
        // iterate it directly (no `Emit.Closure`/`IsValueStruct` dependency — the seam
        // that keeps this module backend-neutral). Node identity is what stops two
        // *structurally identical* transformer arrows (two `int->int` maps) from ever
        // being conflated: each transformer-call site is rewritten with the closure THAT
        // call produced, found by walking that call's OWN argument spine
        // (`appOwnVerdict`). Built first so every downstream consumer (`collect`,
        // `appOwnVerdict`, the for-in leaf set) shares it.
        let closureNodeVerdict =
            let d = Dictionary<TastAccessor.ExprId, struct (FrozenType * int)>()

            for KeyValue(node, closureFt) in closureValueTypeByNode do
                match funVerdicts.TryGetValue node with
                | true, { ResultTyparPos = ValueSome idx } -> d.[node] <- struct (closureFt, idx)
                | _ -> ()

            d

        // Replace a stored binding's `'TFunc`-position type leaf with the value-struct
        // closure its initialiser produces. The verdict (`closureNodeVerdict`) names which
        // top-level type-arg POSITION the lambda's typar occupies — matched by POSITION,
        // not arrow shape, so a genuine function-valued field of the same shape is never
        // miscoerced. Recursive so a future nested result (`MapSeq<MapSeq<…>,…>`) is also
        // rewritten. A no-op when the init carries no verdict lambda or the typar appears
        // at no recorded position (a terminal combinator).
        //
        // Returns the rewritten container type (the slot the field table encodes) and the
        // `arrow → closure` leaf replacements it made (so a projection `h.F : arrow` off
        // this binding can be retyped to the closure value-struct in the body).
        // Each verdict module value's rewritten field type + its `arrow → closure` leaf
        // replacements (consumed by `ModuleValueSlotType` + `retypeBody`). Empty unless a
        // stored binding's initialiser feeds a value-struct lambda into a typar-carrying
        // result. Built in DECLARATION order so a chained binding (`let s2 = map g s1`)
        // can read the ALREADY-rewritten type of an earlier source binding (`s1`).
        let verdictBindings =
            Dictionary<BinderId, FrozenType * (FrozenType * FrozenType) list>()

        let substituteVerdictClosures
            (ty: FrozenType)
            (init: TastAccessor.ExprId)
            : FrozenType * (FrozenType * FrozenType) list =
            // The verdict lambdas in this initialiser, by result-typar position → its
            // `<closure>$` value-type. Each value-struct lambda node carrying a
            // result-typar verdict is already in `closureNodeVerdict` (keyed by
            // reference), so consult it directly rather than re-deriving the binder.
            let slots = Dictionary<int, FrozenType>()

            // A chained source binding (`let s2 = map g s1`) references an EARLIER verdict
            // binding `s1` by `Var`. Inference froze `s2`'s type BEFORE any closure was
            // minted, so every nested position that came from `s1` — its `'S` source slot
            // (`s1`'s whole seq type) AND its `'E` enumerator slot (`s1`'s enumerator,
            // which buries `s1`'s `'TFunc` arrow inside a `MapEnumerator<…, arrow, …>`) —
            // still carries arrows where `s1`'s ALREADY-rewritten field lays out the
            // `<closure>$` value-struct. Left stale, the consuming combinator's recovered
            // `'E` (the constrained `GetEnumerator` interface instantiation) mismatches
            // `s1`'s actual impl → `EntryPointNotFoundException`.
            //
            // `nestedSubst` maps each such nested OLD nominal subtree to its NEW one. It is
            // populated NODE-KEYED, never by arrow shape: for the referenced binding `s1`
            // we have, by node identity, both its old frozen type (`varTy`) and its
            // already-rewritten type (`verdictBindings.[k]`), and we record the structural
            // correspondence between the two by a LOCKSTEP walk (`recordNominalDiff`). Each
            // recorded key is a WHOLE NOMINAL (`FTClass`/`FTConst`/…), so its full nesting
            // depth + nominal head is part of the key — two structurally-identical `'TFunc`
            // arrows at different chain depths live inside DIFFERENT enclosing nominals and
            // therefore never collide. Bare arrow leaves are deliberately NOT keyed (an
            // `int->int` leaf is ambiguous across the chain); they are only ever rewritten
            // (a) at this binding's own `'TFunc` slot via the node-keyed `slots`, or
            // (b) inside a matched whole nominal, which `deep` returns WITHOUT descending.
            //
            // BOUNDARY (linear chains only): collision-freedom relies on each nested
            // closure sitting at a structurally-distinct depth, which holds for a LINEAR
            // pipeline (`map |> map |> fold`). A multi-source combinator (a hypothetical
            // `zip s1 s2` / `combine` taking TWO same-typed seq args bound to DIFFERENT
            // closures) would record one OLD nominal against two different NEW ones → a
            // collision. No such combinator exists or is planned (the struct-seq design is
            // single-source); the true fix would be node-tagged frozen types. The
            // `record` guard below makes that collision a LOUD failure (not silent
            // last-write-wins corruption surfacing later as `EntryPointNotFoundException`),
            // naming this boundary — revisit here if a multi-source seq combinator is added.
            let nestedSubst = Dictionary<FrozenType, FrozenType>()

            // Guarded write: an OLD nominal must map to exactly ONE NEW nominal. A repeat
            // with the same target is idempotent (the lockstep walk can revisit a shared
            // subtree); a repeat with a DIFFERENT target is the multi-source collision the
            // BOUNDARY note forbids — fail loud rather than corrupt the stored signature.
            let record (oldT: FrozenType) (newT: FrozenType) =
                match nestedSubst.TryGetValue oldT with
                | true, existing when existing <> newT ->
                    failwithf
                        "ClosureVerdictRewrite: nested-substitution collision — one OLD nominal mapped to two NEW ones (multi-source seq combinator? see the BOUNDARY note):\n  old: %A\n  new1: %A\n  new2: %A"
                        oldT
                        existing
                        newT
                | _ -> nestedSubst.[oldT] <- newT

            // Lockstep walk of a referenced binding's (old, rewritten) type pair: record
            // every differing NOMINAL subtree as an old→new pair, then recurse pairwise
            // into its args so inner differing nominals (a deeper binding's enumerator) are
            // captured too. Stops at equal subtrees and never records a bare arrow leaf —
            // a leaf is disambiguated only by the whole nominal that encloses it. Heads are
            // compared by case + key/name + arity; a shape mismatch means the rewrite
            // changed the head (it doesn't here) and is left to the outer-nominal pair.
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
                    // A differing leaf (`FTFun` arrow → `<closure>$`, or a head swap):
                    // record the whole nominal pair at THIS level only (the caller has the
                    // enclosing nominal); do NOT key a bare arrow leaf on its own — that is
                    // the structural collision this redesign removes.
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
                        // The referenced binding `s_{n-1}` contributes TWO nested positions
                        // to THIS binding's frozen type: its `'S` source slot (its whole seq
                        // type, `varTy`→`newTy`) and its `'E` enumerator slot (its enumerator,
                        // which buries `s_{n-1}`'s `'TFunc` arrow). Both are recorded by a
                        // node-keyed lockstep diff of the old/new pair. The enumerator pair is
                        // derived from the seq→enumerator witness applied to the SAME old/new
                        // seq types — never from the arrow leaf — so `s_{n-1}`'s closure rides
                        // into the enumerator at its OWN depth, distinct from any other
                        // chain level's structurally-identical arrow.
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
                // The replaced arrow leaves, paired with their closures — the projection
                // off this binding (`h.F`) is typed as the arrow that occupied the slot,
                // so the body must retype that projection to the closure value-struct.
                let replaced = ResizeArray<FrozenType * FrozenType>()

                // Deep rewrite of a non-`'TFunc` arg: replace it WHOLESALE if it is a
                // recorded earlier-binding nominal (`nestedSubst`, keyed by the whole
                // depth-carrying nominal — collision-free); otherwise recurse into its
                // children. A matched whole nominal is returned as-is WITHOUT descending,
                // so an arrow leaf buried inside it is never reached by a (collision-prone)
                // leaf lookup — its closure already rode in via the recorded NEW nominal.
                let rec deep (t: FrozenType) : FrozenType =
                    match nestedSubst.TryGetValue t with
                    | true, newTy -> newTy
                    | false, _ -> TastLower.mapFrozenArgs (EqArray.map deep) t

                // Replace the arg at each recorded position with this binding's own
                // closure value-type (node-keyed `slots`); for a non-recorded position,
                // deep-rewrite it — substituting any referenced earlier-binding nominal
                // (`'S` source / `'E` enumerator) wholesale with its already-laid-out
                // rewritten form. NO position-keyed recursion and NO arrow-shape lookup
                // that would clobber a nested nominal's own `'TFunc` slot with THIS
                // binding's closure (the multi-map nested-layout collision).
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

        // True iff ANY value-struct closure carries a transformer verdict — the cheap
        // gate that keeps the inline `App`-result rewrite (`retypeBody`'s `App` arm) a
        // no-op on the green named-struct / terminal-only paths, even when no stored
        // verdict binding exists (the nested-temp `fold f 0 (map …)` shape).
        let hasTransformerVerdict = closureNodeVerdict.Count > 0

        // The transformer verdict the lambda argument of a single application produces:
        // walk the spine of `fn (arg)` collecting each direct argument, and look up the
        // value-struct closure (by node identity) among them. Returns the closure's
        // `(value-struct type, result-typar position)`. A combinator takes at most one
        // `Fun`2`/`Fun`3`-bounded lambda argument, so at most one verdict is found per call.
        let appOwnVerdict (e: TastAccessor.ExprId) : struct (FrozenType * int) voption =
            let rec scan (e: TastAccessor.ExprId) : struct (FrozenType * int) voption =
                match e with
                | TastAccessor.EApp app ->
                    match closureNodeVerdict.TryGetValue app.Arg with
                    | true, v -> ValueSome v
                    | false, _ -> scan app.Fn
                | _ -> ValueNone

            scan e

        // Replace, in a producing transformer's RESULT type (`MapSeq<…,arrow,…>`), the
        // `'TFunc`-position leaf with THIS call's own `<closure>$` value-struct. `verdict`
        // = the `(closure value-struct, FunResultTypar position)` of the lambda this very
        // application fed (resolved by node identity, `appOwnVerdict`), so a same-shaped
        // arrow at a NON-recorded position is left untouched — the nested source slot is
        // rewritten by ITS OWN producing site, never by a first-matching arrow guess.
        let rewriteAppResultByVerdict (resultTy: FrozenType) (verdict: struct (FrozenType * int)) : FrozenType =
            let struct (closureFt, pos) = verdict
            // Position-only (not recursive): one application rewrites exactly one slot,
            // its own. An out-of-range `pos` matches no index, leaving the type unchanged.
            resultTy
            |> TastLower.mapFrozenArgs (EqArray.mapi (fun i a -> if i = pos then closureFt else a))

        // Retype a body expression so a reference to a verdict binding (and its field
        // projections) carries the `<closure>$` value-struct type rather than the frozen
        // arrow. Rebuilds only the affected nodes (a no-op deep copy elsewhere). A
        // `FieldGet` whose receiver resolves to a verdict binding has its own type — the
        // projected arrow — mapped to the closure via that binding's recorded
        // replacements; the receiver itself is retyped to the rewritten container so its
        // field-`MemberRef` `TypeSpec` matches the value-struct-instantiated field row.
        let retypeBody (e: TastAccessor.ExprId) : TastAccessor.ExprId =
            if verdictBindings.Count = 0 && not hasTransformerVerdict then
                e
            else
                // The verdict binding a (possibly nested-field) receiver bottoms out in,
                // for mapping a projection's arrow type to its closure.
                let rec receiverBinding (r: TastAccessor.ExprId) : BinderId voption =
                    match r with
                    | TastAccessor.EVar k ->
                        if verdictBindings.ContainsKey k then
                            ValueSome k
                        else
                            ValueNone
                    | TastAccessor.EFieldGet fg -> receiverBinding fg.Receiver
                    | _ -> ValueNone

                // Re-author ONLY the affected nodes. Closure discovery keys its lambdas
                // by node id, so a blanket rebuild would give every lambda beneath a
                // rewritten node a fresh id the verdict tables no longer recognise; the
                // row copies below return the ORIGINAL id whenever nothing moved, which
                // is what keeps an untouched subtree the very node discovery found.
                let rec rw (e: TastAccessor.ExprId) : TastAccessor.ExprId =
                    match e with
                    | TastAccessor.EVar k ->
                        match verdictBindings.TryGetValue k with
                        | true, (newTy, _) -> TastAccessor.retype e newTy
                        | false, _ -> e
                    | TastAccessor.EFieldGet fg ->
                        let ty = TastAccessor.exprTy e
                        let recv' = rw fg.Receiver

                        let ty' =
                            match receiverBinding fg.Receiver with
                            | ValueSome k ->
                                let _, replaced = verdictBindings.[k]

                                replaced
                                |> List.tryPick (fun (arrowTy, closureTy) ->
                                    if arrowTy = ty then Some closureTy else None
                                )
                                |> Option.defaultValue ty
                            | ValueNone -> ty

                        TastAccessor.retypeWithChildren e [| recv' |] ty'
                    | TastAccessor.EApp app ->
                        let ty = TastAccessor.exprTy e
                        // A *transformer* call (`map (fun x -> x+1) src`)
                        // whose result type carries the lambda's `'TFunc`
                        // (`MapSeq<…,arrow,…>`), used directly as an argument to a consuming
                        // combinator (`fold f 0 (map …)`) WITHOUT a stored `let s1`. The
                        // fold call's `'S` MethodSpec reads `typeOfExpr` of this App, so its
                        // result type must lay the `'TFunc` slot out as the value-struct.
                        //
                        // COLLISION-SAFE: the rewrite is keyed on THIS application's
                        // own produced closure — `appOwnVerdict` walks this `App`'s argument
                        // spine, finds the value-struct lambda node it feeds (by node
                        // id), and rewrites ONLY that closure's recorded
                        // `FunResultTypar` POSITION. It never consults a program-wide
                        // arrow-type table, so two transformer calls whose lambdas share the
                        // SAME frozen arrow (`int->int`) are each rewritten with the closure
                        // THEY produced — a type-keyed collision is impossible by
                        // construction. A terminal call (`fold …`, result `int`) produces no
                        // value-struct transformer verdict, so it is left unchanged.
                        let ty' =
                            match appOwnVerdict e with
                            | ValueSome verdict -> rewriteAppResultByVerdict ty verdict
                            | ValueNone -> ty

                        TastAccessor.retypeWithChildren e [| rw app.Fn; rw app.Arg |] ty'
                    // Recurse without forcing a rebuild: the row copy appends only when a
                    // child actually moved, so an untouched subtree keeps its id.
                    | _ -> TastAccessor.mapChildren rw e

                rw e

        // No `Type` arm: a `type` decl surfaces no `DeclExprChildren` (its member bodies
        // are named by id inside the payload), so the mapping is already the identity on
        // one and returns the decl's own id.
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
