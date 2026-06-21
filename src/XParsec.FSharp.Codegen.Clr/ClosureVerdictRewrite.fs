namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// rung-4 M6: the closure-verdict TAST type rewrite, lifted out of `Assembler`.
///
/// A *transformer* combinator (`map`, `mk : ('TF:>Fun) -> Holder<'TF>`) returns a
/// nominal carrying the lambda's function typar; the front end freezes that result
/// with `'TFunc := arrow`, which `encodeType` lowers to the `Vesper.Fun`/`Fun2`
/// INTERFACE — but the call actually produces a `<closure>$` value-struct, so the
/// stored slot / `constrained.` dispatch token disagrees with the value (corruption /
/// `EntryPointNotFoundException`). Given the value-struct closures' minted nominal
/// types (`closureValueTypeByNode`) and the front end's result-typar verdicts
/// (`FunVerdict.ResultTyparPos`), this pass rewrites the `'TFunc`-position leaf — in stored
/// module-value field types, in body references to them, and in the `'TFunc`-as-arrow
/// leaves of consuming-combinator `for-in` enumerator descriptors — to the
/// value-struct nominal, so token and value agree.
///
/// Pure `Frozen.TExpr` / `FrozenType` / `NodeKey` traffic — the same family
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
            RetypeBody: Frozen.TExpr -> Frozen.TExpr
            /// `RetypeBody` lifted to a top-level `TDecl` (the Main path) — the single
            /// place that knows how to reach the expressions inside a `TDecl`.
            RetypeDecl: Frozen.TDecl -> Frozen.TDecl
            /// The field-slot type for a module value: the rewritten container if it is
            /// a verdict binding, else the declared type unchanged.
            ModuleValueSlotType: NodeKey -> FrozenType -> FrozenType
        }

    /// Build the rewrite from the backend-neutral inputs:
    /// * `closureValueTypeByNode` — each value-struct closure's Lambda node (reference
    ///   identity) → its minted `<closure>$` value-struct `FrozenType`.
    /// * `funVerdicts` — a source-lambda node's `NodeKey` → its `FunVerdict`; the
    ///   `ResultTyparPos` names the type-arg POSITION its `'TFunc` occupies in the
    ///   producing transformer's result nominal (the only field this pass reads).
    /// * `moduleValues` — each stored module value as `(key, declared type, initialiser)`.
    let build
        (closureValueTypeByNode: IReadOnlyDictionary<Frozen.TExpr, FrozenType>)
        (funVerdicts: Map<NodeKey, FunVerdict>)
        (moduleValues: (NodeKey * FrozenType * Frozen.TExpr) seq)
        : Rewrite =

        // rung-4 M6 P-d: per-value-struct-closure VERDICT, keyed by the closure's
        // Lambda node (reference identity) → its `<closure>$` value-struct + the
        // RESULT-typar POSITION its `'TFunc` occupies in the producing transformer's
        // result nominal. `closureValueTypeByNode`'s keys ARE exactly the value-struct
        // closure nodes, so iterate it directly (no `Emit.Closure`/`IsValueStruct`
        // dependency — the seam that keeps this module backend-neutral). The
        // node-identity discipline so two *structurally identical* transformer arrows
        // (two `int->int` maps) are NEVER conflated — each transformer-call site is
        // rewritten with the closure THAT call produced, found by walking that call's
        // OWN argument spine (`appOwnVerdict`). Built first so every downstream
        // consumer (`collect`, `appOwnVerdict`, the for-in leaf set) shares it.
        let closureNodeVerdict =
            let d = Dictionary<Frozen.TExpr, struct (FrozenType * int)>(HashIdentity.Reference)

            for KeyValue(node, closureFt) in closureValueTypeByNode do
                let k = NodeKey.ofToken (TastWalk.exprTok node) NodeKind.ExprLambda

                match Map.tryFind k funVerdicts with
                | Some { ResultTyparPos = ValueSome idx } -> d.[node] <- struct (closureFt, idx)
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
        let substituteVerdictClosures
            (ty: FrozenType)
            (init: Frozen.TExpr)
            : FrozenType * (FrozenType * FrozenType) list =
            // The verdict lambdas in this initialiser, by result-typar position → its
            // `<closure>$` value-type. Each value-struct lambda node carrying a
            // result-typar verdict is already in `closureNodeVerdict` (keyed by
            // reference), so consult it directly rather than re-deriving the NodeKey.
            let slots = Dictionary<int, FrozenType>()

            let rec collect (e: Frozen.TExpr) =
                match closureNodeVerdict.TryGetValue e with
                | true, struct (closureFt, idx) -> slots.[idx] <- closureFt
                | false, _ -> ()

                TastLower.iterChildren collect e

            collect init

            if slots.Count = 0 then
                ty, []
            else
                // The replaced arrow leaves, paired with their closures — the projection
                // off this binding (`h.F`) is typed as the arrow that occupied the slot,
                // so the body must retype that projection to the closure value-struct.
                let replaced = ResizeArray<FrozenType * FrozenType>()

                // Replace the arg at each recorded position with its closure value-type;
                // recurse into the others so a nested transformer result is also rewritten.
                let rec rw (t: FrozenType) : FrozenType = TastLower.mapFrozenArgs rwArgs t

                and rwArgs (args: EqArray<FrozenType>) : EqArray<FrozenType> =
                    args
                    |> EqArray.mapi (fun i a ->
                        match slots.TryGetValue i with
                        | true, closureFt ->
                            replaced.Add(a, closureFt)
                            closureFt
                        | false, _ -> rw a
                    )

                rw ty, List.ofSeq replaced

        // Each verdict module value's rewritten field type + its `arrow → closure` leaf
        // replacements (consumed by `ModuleValueSlotType` + `retypeBody`). Empty unless a
        // stored binding's initialiser feeds a value-struct lambda into a typar-carrying
        // result.
        let verdictBindings =
            Dictionary<NodeKey, FrozenType * (FrozenType * FrozenType) list>()

        for (key, ty, init) in moduleValues do
            match substituteVerdictClosures ty init with
            | _, [] -> ()
            | newTy, replaced -> verdictBindings.[key] <- (newTy, replaced)

        // True iff ANY value-struct closure carries a transformer verdict — the cheap
        // gate that keeps the consuming-combinator rewrite (`retypeBody`'s `App` + `ForIn`
        // arms) a no-op on the green named-struct / terminal-only paths, even when no
        // stored verdict binding exists (the nested-temp `fold f 0 (map …)` shape).
        let hasTransformerVerdict = closureNodeVerdict.Count > 0

        // The transformer verdict the lambda argument of a single application produces:
        // walk the spine of `fn (arg)` collecting each direct argument, and look up the
        // value-struct closure (by node identity) among them. Returns the closure's
        // `(value-struct type, result-typar position)`. A combinator takes at most one
        // `Fun`/`Fun2`-bounded lambda argument, so at most one verdict is found per call.
        let appOwnVerdict (e: Frozen.TExpr) : struct (FrozenType * int) voption =
            let rec scan (e: Frozen.TExpr) : struct (FrozenType * int) voption =
                match e with
                | TExprG.App(fn, arg, _, _) ->
                    match closureNodeVerdict.TryGetValue arg with
                    | true, v -> ValueSome v
                    | false, _ -> scan fn
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

        // The consuming combinator's `for-in` enumerator descriptor (`fold`'s `for y in
        // source`) carries the SOURCE seq's frozen types with the producing transformer's
        // `'TFunc` STILL the arrow (encoded to the `Fun`/`Fun2` INTERFACE). The receiver
        // is the value-struct-instantiated seq, so the `constrained. callvirt
        // GetEnumerator` interface-map lookup misses (impl keyed on the closure, token on
        // the arrow) → `EntryPointNotFoundException`. Rewriting those nested `'TFunc`
        // arrow leaves to the `<closure>$` value-struct aligns the token with the
        // receiver's actual impl.
        //
        // KNOWN LIMITATION (design doc M6): unlike the App-result path (`appOwnVerdict`,
        // node-identity keyed), this match is necessarily by arrow EQUALITY. The for-in
        // being rewritten lives in the GROUNDED consuming-combinator body (`fold`'s
        // `for y in source`), which does NOT lexically contain the producing lambda —
        // that lambda is in the caller's `let s1 = map …` / nested-temp initialiser. So
        // there is no node in this body to key on; the leaf↔closure link can only be the
        // arrow type. Two same-typed transformer lambdas (two `int->int` maps) whose
        // grounded arrows are structurally equal therefore collide here (first wins) — the
        // `ptest "M6 P-d: multi-map chain"` case. Removing the collision needs call-site→
        // body verdict propagation (thread which closure each `fold` instantiation uses
        // into the rewrite of that grounded body); deferred future work, and today's
        // shipping pipeline does not surface it.
        let verdictArrowToClosure =
            [
                for KeyValue(node, struct (closureFt, _)) in closureNodeVerdict ->
                    (TastLower.typeOfExpr node, closureFt)
            ]

        let rec rewriteClosureLeaves (t: FrozenType) : FrozenType =
            match
                verdictArrowToClosure
                |> List.tryPick (fun (a, c) -> if a = t then Some c else None)
            with
            | Some closureFt -> closureFt
            | None -> TastLower.mapFrozenArgs (EqArray.map rewriteClosureLeaves) t

        let rewriteForInEnumerator (en: Frozen.ForInEnumerator) : Frozen.ForInEnumerator =
            match en with
            | ForInEnumeratorG.Interface -> en
            | ForInEnumeratorG.Pattern(enumeratorTy, getEnum, members, isValueType, dispose) ->
                let getEnum' =
                    match getEnum with
                    | ForInGetEnumG.ConstrainedInterface(k, args) ->
                        ForInGetEnumG.ConstrainedInterface(k, EqArray.map rewriteClosureLeaves args)
                    | _ -> getEnum

                let members' =
                    match members with
                    | ForInEnumMembersG.ConstrainedInterface(k, args) ->
                        ForInEnumMembersG.ConstrainedInterface(k, EqArray.map rewriteClosureLeaves args)
                    | _ -> members

                ForInEnumeratorG.Pattern(rewriteClosureLeaves enumeratorTy, getEnum', members', isValueType, dispose)

        // Retype a body expression so a reference to a verdict binding (and its field
        // projections) carries the `<closure>$` value-struct type rather than the frozen
        // arrow. Rebuilds only the affected nodes (a no-op deep copy elsewhere). A
        // `FieldGet` whose receiver resolves to a verdict binding has its own type — the
        // projected arrow — mapped to the closure via that binding's recorded
        // replacements; the receiver itself is retyped to the rewritten container so its
        // field-`MemberRef` `TypeSpec` matches the value-struct-instantiated field row.
        let retypeBody (e: Frozen.TExpr) : Frozen.TExpr =
            if verdictBindings.Count = 0 && not hasTransformerVerdict then
                e
            else
                // The verdict binding a (possibly nested-field) receiver bottoms out in,
                // for mapping a projection's arrow type to its closure.
                let rec receiverBinding (r: Frozen.TExpr) : NodeKey voption =
                    match r with
                    | TExprG.Var(k, _, _) when verdictBindings.ContainsKey k -> ValueSome k
                    | TExprG.FieldGet(inner, _, _, _) -> receiverBinding inner
                    | _ -> ValueNone

                // Rebuild ONLY the affected nodes — closure discovery keyed lambdas by
                // reference identity (`HashIdentity.Reference`), so a blanket `mapChildren`
                // rebuild would mint fresh Lambda nodes the verdict tables no longer
                // recognise. Each arm returns the SAME `e` when nothing beneath changed.
                let rec rw (e: Frozen.TExpr) : Frozen.TExpr =
                    match e with
                    | TExprG.Var(k, _, tok) ->
                        match verdictBindings.TryGetValue k with
                        | true, (newTy, _) -> TExprG.Var(k, newTy, tok)
                        | false, _ -> e
                    | TExprG.FieldGet(recv, name, ty, tok) ->
                        let recv' = rw recv

                        let ty' =
                            match receiverBinding recv with
                            | ValueSome k ->
                                let _, replaced = verdictBindings.[k]

                                replaced
                                |> List.tryPick (fun (arrowTy, closureTy) ->
                                    if arrowTy = ty then Some closureTy else None
                                )
                                |> Option.defaultValue ty
                            | ValueNone -> ty

                        if
                            System.Object.ReferenceEquals(recv', recv)
                            && System.Object.ReferenceEquals(ty', ty)
                        then
                            e
                        else
                            TExprG.FieldGet(recv', name, ty', tok)
                    | TExprG.ForIn(pat, src, body, enumerator, ty, tok) ->
                        // The `for y in source` of a consuming combinator (`fold`)
                        // iterates a verdict-typed seq; its enumerator descriptor carries
                        // the source's frozen `'TFunc`-as-arrow leaves. Rewrite them to the
                        // `<closure>$` value-struct so the `constrained. callvirt
                        // GetEnumerator` token matches the receiver's value-struct impl.
                        let src' = rw src
                        let body' = rw body
                        let enumerator' = rewriteForInEnumerator enumerator

                        if
                            System.Object.ReferenceEquals(src', src)
                            && System.Object.ReferenceEquals(body', body)
                            && System.Object.ReferenceEquals(enumerator', enumerator)
                        then
                            e
                        else
                            TExprG.ForIn(pat, src', body', enumerator', ty, tok)
                    | TExprG.App(fn, arg, ty, tok) ->
                        // rung-4 M6 P-d: a *transformer* call (`map (fun x -> x+1) src`)
                        // whose result type carries the lambda's `'TFunc`
                        // (`MapSeq<…,arrow,…>`), used directly as an argument to a consuming
                        // combinator (`fold f 0 (map …)`) WITHOUT a stored `let s1`. The
                        // fold call's `'S` MethodSpec reads `typeOfExpr` of this App, so its
                        // result type must lay the `'TFunc` slot out as the value-struct.
                        //
                        // COLLISION-SAFE (P-d): the rewrite is keyed on THIS application's
                        // own produced closure — `appOwnVerdict` walks this `App`'s argument
                        // spine, finds the value-struct lambda node it feeds (by reference
                        // identity), and rewrites ONLY that closure's recorded
                        // `FunResultTypar` POSITION. It never consults a program-wide
                        // arrow-type table, so two transformer calls whose lambdas share the
                        // SAME frozen arrow (`int->int`) are each rewritten with the closure
                        // THEY produced — a type-keyed collision is impossible by
                        // construction. A terminal call (`fold …`, result `int`) produces no
                        // value-struct transformer verdict, so it is left unchanged.
                        let fn' = rw fn
                        let arg' = rw arg

                        let ty' =
                            match appOwnVerdict e with
                            | ValueSome verdict -> rewriteAppResultByVerdict ty verdict
                            | ValueNone -> ty

                        if
                            System.Object.ReferenceEquals(fn', fn)
                            && System.Object.ReferenceEquals(arg', arg)
                            && System.Object.ReferenceEquals(ty', ty)
                        then
                            e
                        else
                            TExprG.App(fn', arg', ty', tok)
                    | _ ->
                        // Recurse without forcing a rebuild: rebuild only if a child node
                        // actually changed identity (preserving Lambda reference identity).
                        let mutable changed = false

                        let rebuilt =
                            TastLower.mapChildren
                                (fun c ->
                                    let c' = rw c

                                    if not (System.Object.ReferenceEquals(c', c)) then
                                        changed <- true

                                    c'
                                )
                                e

                        if changed then rebuilt else e

                rw e

        let retypeDecl (d: Frozen.TDecl) : Frozen.TDecl =
            match d with
            | TDeclG.Expression(e, tok) -> TDeclG.Expression(retypeBody e, tok)
            | TDeclG.Let(p, v, isInline, tok) -> TDeclG.Let(p, retypeBody v, isInline, tok)
            | TDeclG.Type _ -> d

        {
            RetypeBody = retypeBody
            RetypeDecl = retypeDecl
            ModuleValueSlotType =
                fun key declared ->
                    match verdictBindings.TryGetValue key with
                    | true, (newTy, _) -> newTy
                    | false, _ -> declared
        }
