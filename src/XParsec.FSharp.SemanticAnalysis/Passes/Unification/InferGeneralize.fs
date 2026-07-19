namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module internal UnificationInferGeneralize =

    /// Visit every `TyVar` leaf of `t`, resolving it to its union-find `root` and
    /// invoking `onRoot` — the structural skeleton shared by `instantiate`,
    /// `generalise`, and `generaliseMemberTypars`, each of which supplies its own
    /// root predicate and dedup. A plain structural walk: it assumes `t` is already
    /// zonked and does *not* follow `Link`s, unlike `applyDefaults` /
    /// `prepareListLiterals`, which chase the link/default graph and so keep their
    /// own bespoke walks. A thin wrapper now: it delegates the traversal to the
    /// shared `SemTypeWalk.iterSemTypeVars`, supplying only the `find`-then-`onRoot`
    /// leaf policy.
    let iterTypeVarRoots (onRoot: TypeVar -> unit) (t: SemType) : unit =
        t |> SemTypeWalk.iterSemTypeVars (fun tv -> onRoot (UnionFind.find tv))

    /// Non-quantified TyVars are left alone — they're free w.r.t. the
    /// surrounding scope and must keep their identity. `scheme.Body` is
    /// already zonked by `generalise`, so we don't follow Links here.
    let instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        let freshOf = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = ctx.NewTypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- TyVar fresh
            freshOf.[qRoot] <- fresh

        // EVERY quantified root is freshened per call in the
        // constraint substitution, INCLUDING purely PHANTOM quantified roots (e.g.
        // the enumerator `'E` in `fold`'s `'S :> IStructSeq<'T,'E>`, absent from the
        // surface type). Previously phantom roots were left verbatim so the body
        // grounded them at the binding — but that baked one call's concrete
        // enumerator (carrying a function-arrow where a value-struct closure belongs)
        // into a method that is supposed to be generic over `'E`, forcing the
        // collision-prone arrow-equality rewrite in ClosureVerdictRewrite. Now `'E`
        // stays free in the body (freezes as `FTTypar(Method, idx_E)`), becomes a
        // real generic method slot, and the call site solves it from the bound. The
        // remapping of SURFACE quantified roots (e.g. `'U` in `map`'s
        // `… -> MapSeq<…,'U>` return) is still required so the dependent-typar
        // inference in `drainConstraints` does not ground the ORIGINAL surface var
        // and leave the FRESH return copy un-instantiated → an unresolved TyVar at
        // freeze. Seeding from the FULL `subst` covers both.
        let constraintSubst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)

        for kv in subst do
            constraintSubst.[kv.Key] <- kv.Value

        // A `Coercion` target may ALSO reference still-free roots that are NOT
        // quantified at all: a placeholder typar that leaked into the bound when
        // the combinator's param typar unified with a CONSTRUCTED type's own
        // declared typar (the `WSeq`/`MapSeq` ctor's `'S :> ISeq<…>` bound migrating
        // onto `wrap`/`map`'s `'S` via `migrateBounds`). Its root sits at the
        // registry/outer level, so `generalise`'s level test never quantified it,
        // yet it rides the constraint. Left verbatim it is SHARED across every
        // instantiation of the scheme, so the FIRST call grounds it (its
        // dependent-typar inference pins it to the inner arg's witness) and the
        // SECOND call inherits that ground bound → a spurious subtype check against
        // an unrelated nominal (`MapSeq`5 does not support subtype of IStructSeq`2`).
        // Freshen each such non-quantified free root per call, sharing one fresh
        // instance across all constraints that mention it. (Quantified roots —
        // surface AND phantom — are already in `constraintSubst` from the full
        // `subst` seed above, so the `quantifiedRoots` guard skips them here.)
        let quantifiedRoots = HashSet<TypeVar>(freshOf.Keys, HashIdentity.Reference)

        for (_, c) in scheme.Constraints do
            match c.Kind with
            | SemanticConstraintKind.Coercion target ->
                target
                |> zonk
                |> iterTypeVarRoots (fun root ->
                    if
                        root.Link.IsNone
                        && not (quantifiedRoots.Contains root)
                        && not (constraintSubst.ContainsKey root)
                    then
                        let fresh = ctx.NewTypeVar()
                        fresh.Level <- ctx.CurrentLevel
                        constraintSubst.[root] <- TyVar fresh
                )
            | _ -> ()

        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find qTv

            match freshOf.TryGetValue qRoot with
            | true, fresh ->
                let c =
                    match c.Kind with
                    | SemanticConstraintKind.Coercion target ->
                        { c with
                            Kind = SemanticConstraintKind.Coercion(substituteWith constraintSubst target)
                        }
                    | _ -> c

                addConstraintByKind fresh c
            | false, _ -> ()

        substituteWith subst scheme.Body

    /// Resolve a bound name to its type: instantiate its generalised scheme if
    /// one was written, else take the monomorphic binding-site TyVar (a sibling
    /// in the same `let rec` group, not yet generalised — which is what forbids
    /// polymorphic recursion).
    let instantiateBinding (ctx: PassContext) (rb: ResolvedBinding) : SemType =
        match ctx.Bindings.Scheme.TryGetValue rb.BindingSite with
        | ValueSome scheme -> instantiate ctx scheme
        | ValueNone -> TyVar(tvOf ctx rb.BindingSite)

    /// True if `t` contains a TyVar whose root carries a deferred
    /// `PendingDotAccess` constraint. Such a binding cannot be safely
    /// generalised in v1 — quantifying a TyVar with pending dot accesses
    /// would freeze the constraint into the scheme, and a use site that
    /// pins the receiver would only resolve a fresh instantiation, leaving
    /// the original (still-quantified) constraint dangling. Keeping the
    /// binding monomorphic lets the first use site unify directly with the
    /// pre-instantiation TyVar, which drains the constraint normally.
    let rec hasPendingDotAccess (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if not (List.isEmpty root.PendingDotAccess) then
                true
            else
                match root.Link with
                | ValueSome target -> hasPendingDotAccess target
                | ValueNone -> false
        // A compound carries pending dot access iff a child does; leaves hold none.
        | t -> SemType.existsChild hasPendingDotAccess t

    /// A chained default like `default ^T3 : ^T1 ; default ^T1 : int` needs
    /// two passes, hence the fixpoint iteration.
    ///
    /// Defaults walked here are *consumed*: once a fire happens (or once
    /// all candidates fail), the `Defaults` list is cleared so subsequent
    /// passes don't re-walk dead targets. A TyVar generalised at a use-site
    /// instantiation is re-stamped with fresh defaults on the next call to
    /// its `Instantiate` closure.
    let applyDefaults (zonkedTy: SemType) (outerLevel: int) : unit =
        let visited = HashSet<TypeVar>(HashIdentity.Reference)

        let rec collect (t: SemType) : ResizeArray<TypeVar> =
            let acc = ResizeArray<TypeVar>()

            let rec go (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if visited.Add root then
                        if root.Level > outerLevel && root.Link.IsNone && not (List.isEmpty root.Defaults) then
                            acc.Add root
                            // Follow the default-target graph: a chained default
                            // (`default ^T3 : ^T1`) names another TyVar that may be
                            // an *intermediate* result var (the inner `a + b` of
                            // `a + b + c`) not reachable from the binding's surface
                            // type. Without this it never becomes a candidate and the
                            // tail of the chain never grounds.
                            for target in root.Defaults do
                                go target

                        match root.Link with
                        | ValueSome target -> go target
                        | ValueNone -> ()
                | t -> SemType.iterChildren go t

            go t
            acc

        let candidates = collect zonkedTy

        let rec resolveTarget (t: SemType) : SemType voption =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> resolveTarget target
                | ValueNone -> ValueNone
            | _ -> ValueSome t

        let tryDefault (tv: TypeVar) : bool =
            let mutable fired = false
            let defaults = tv.Defaults
            // A target that resolves only to a still-free TyVar is *deferrable*:
            // a chained default like `default ^T2 : ^T3` can't fire until ^T3 is
            // itself defaulted (e.g. to `int`) on a later pass. We must keep such
            // a default alive rather than discard it, or the fixpoint loses the
            // tail of the chain — `let g a b = a + b` would ground `a`/result to
            // `int` but leak `b` as a free typar.
            let mutable anyDeferrable = false

            for target in defaults do
                if not fired then
                    match resolveTarget target with
                    | ValueSome concrete when not (occursAndAdjust tv concrete) ->
                        // Occurs guard: a chain like `default ^T3 : ^T1`
                        // with a structural target (`^T1 list`) could build
                        // a `concrete` transitively containing tv; linking
                        // through would create an infinite type. Skip on
                        // occurs — the default is unsatisfiable.
                        tv.Link <- ValueSome concrete
                        fired <- true
                    | ValueSome _ -> () // resolved but occurs-unsafe — permanently dead
                    | ValueNone -> anyDeferrable <- true // target still free — retry next pass

            // Clear once discharged, or once nothing is left to chase. A deferrable
            // default stays so the fixpoint can re-evaluate it after its target
            // links; `while changed` only re-iterates while some default *fires*,
            // so each TyVar is retried a bounded number of times.
            if fired || not anyDeferrable then
                tv.Defaults <- []

            fired

        let mutable changed = true

        while changed do
            changed <- false

            for tv in candidates do
                if tv.Link.IsNone && not (List.isEmpty tv.Defaults) then
                    if tryDefault tv then
                        changed <- true

    /// The element type of the bare list-literal registered against union-find `root`
    /// in `ctx.ListLiterals` (`ValueNone` if none). The shared "look up a registered
    /// literal by its root" primitive behind both `prepareListLiterals` and the for-in
    /// `pinListLiteralToVesper` — the differing flip *policy* stays at each call site.
    let tryListLiteralElem (ctx: PassContext) (root: TypeVar) : SemType voption =
        let mutable result = ValueNone

        for (lv, elem) in ctx.ListLiterals do
            if result.IsNone && System.Object.ReferenceEquals(UnionFind.find lv, root) then
                result <- ValueSome elem

        result

    /// Settle the flexible list-literal containers reachable from a binding's
    /// type *before* it generalises, so the bare container `TypeVar` is never
    /// quantified as `∀L. L`:
    ///   - element still free (`let xs = []`) → link the container to FSharp.Core's
    ///     `list` now, so the *element* generalises normally (`'a list`);
    ///   - element already concrete (`let nums = [1;2;3]`) → leave the container
    ///     free but drop its level to the outer scope so generalisation skips it,
    ///     deferring the FSharpList-vs-Vesper choice to `resolveListLiterals` (a
    ///     later consumer like `List.fold` can still flip it to the Vesper list).
    let prepareListLiterals (ctx: PassContext) (ty: SemType) (outerLevel: int) : unit =
        if ctx.ListLiterals.Count = 0 then
            ()
        else
            let seen = HashSet<TypeVar>(HashIdentity.Reference)

            let rec walk (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if seen.Add root then
                        match root.Link with
                        | ValueSome target -> walk target
                        | ValueNone ->
                            match tryListLiteralElem ctx root with
                            | ValueSome elemTy when root.Level > outerLevel ->
                                match zonk elemTy with
                                | TyVar _ ->
                                    // Self-host (no FSharp.Core) defaults the bare
                                    // container to the Vesper cons-list, mirroring
                                    // `resolveListLiterals`.
                                    let listTy =
                                        if ctx.DefaultListIsVesper then
                                            TyUnion(RuntimeNames.vesperListKey, EqArray.singleton elemTy)
                                        else
                                            TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy)

                                    root.Link <- ValueSome listTy
                                | _ -> root.Level <- outerLevel
                            | _ -> ()
                | t -> SemType.iterChildren walk t

            walk ty

    let generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        // Apply defaults before quantifying: a default that resolves links
        // its source TyVar, which the quantifier walk then skips. Without
        // this, `let x = 1 + 2` would generalise as `∀'a. 'a` instead of
        // `int` (the unbound `^T3` from external-symbol Instantiate).
        applyDefaults zonkedTy outerLevel

        let quantified = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let addRoot (root: TypeVar) =
            if root.Level > outerLevel && root.Link.IsNone && seen.Add(root) then
                quantified.Add(root)

        zonkedTy |> iterTypeVarRoots addRoot

        // Dependent typars: a quantified typar's `Coercion` bound may name *further*
        // typars that appear ONLY in constraints, never in the binding type itself
        // (`let f (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator>)` — `'E`
        // is in no parameter/return position). F# generalises these phantom parameters
        // too; without them they leak as un-ground `TyVar`s → `?free-typar` at
        // the freeze cut (a constrained `for … in` over `'S`). Walk each quantified
        // typar's `Coercion` targets to a fixpoint (a bound may itself reference a typar
        // with its own bounds), `ResizeArray` growth driving the worklist.
        let mutable i = 0

        while i < quantified.Count do
            for c in quantified.[i].Constraints do
                match c.Kind with
                | SemanticConstraintKind.Coercion target -> iterTypeVarRoots addRoot (zonk target)
                | _ -> ()

            i <- i + 1

        // `instantiate` swaps these onto fresh substitutions per use site
        // so satisfaction is re-evaluated independently.
        let constraints =
            [
                for tv in quantified do
                    for c in tv.Constraints -> tv, c
            ]

        TypeScheme(List.ofSeq quantified, zonkedTy, constraints)

    /// The value restriction: a *parameterless* binding may only generalise when
    /// its RHS is a syntactic value — a non-expansive expression. An *expansive*
    /// RHS (a function/method application or an allocation, e.g.
    /// `let res = ResizeArray<'T>()`) must NOT generalise: doing so quantifies the
    /// binding's own free typar (`res`'s element), so every use site instantiates
    /// a *fresh* element that unifies with its context while the binding's typar is
    /// left dangling — exactly the unsound generalisation the restriction forbids,
    /// and which `ResolvedTypes` flags as a stray unresolved TyVar. Keeping such a
    /// binding monomorphic lets a use site (`res.Add(e.Current)`) unify the
    /// binding's own typar into the enclosing function's, where it generalises
    /// soundly. A binding *with* parameters is a function — itself a syntactic
    /// value — so it always generalises regardless of its body.
    let rec private isExpansive (e: Expr<SyntaxToken>) : bool =
        match e with
        | Expr.App _
        | Expr.HighPrecedenceApp _
        | Expr.New _ -> true
        | Expr.TypeAnnotation(expr = inner) -> isExpansive inner
        | _ -> false

    /// Single-name `let` generalises unless the binding is `mutable`.
    /// Mutable bindings stay monomorphic: every use of the name unifies
    /// against the binding's own TyVar (no instantiation), so a free TyVar
    /// in a mutable binding's type can be pinned later by any use or
    /// assignment — but the binding is never made polymorphic at the
    /// scheme level, which would re-introduce the classic value-
    /// restriction soundness hole. Compound destructuring heads and
    /// bindings whose head is something other than `Pat.NamedSimple`
    /// don't get schemes either — they bind values, not function
    /// abstractions, and the scheme table is keyed by a single NodeKey.
    let shouldGeneralise (b: Binding<SyntaxToken>) : bool =
        if b.mutableToken.IsSome then
            false
        // A parameterless binding with an expansive RHS is value-restricted
        // (above); only function bindings and non-expansive values generalise.
        elif b.argumentPats.IsEmpty && isExpansive b.expr then
            false
        else
            match b.headPat with
            | Pat.NamedSimple _ -> true
            // An operator-named binding (`let inline (=) …`) is a single-name
            // head; generalise it like any other function value.
            | Pat.Op _ -> true
            | _ -> false
