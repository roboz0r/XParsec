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

    /// Visit every `TyVar` leaf of `t`, resolving it to its union-find `root` and invoking
    /// `onRoot`. A plain structural walk: it assumes `t` is already zonked and does *not*
    /// follow `Link`s, so a caller needing the link/default graph walks it itself.
    let iterTypeVarRoots (store: TypeStore) (onRoot: Rep -> unit) (t: SemType) : unit =
        t |> SemTypeWalk.iterSemTypeVars (fun tv -> onRoot (UnionFind.find store tv))

    /// Non-quantified TyVars are left alone, because they're free w.r.t. the
    /// surrounding scope and must keep their identity. `scheme.Body` is
    /// already zonked by `generalise`, so we don't follow Links here.
    let instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TyVarId, SemType>()
        let freshOf = Dictionary<TyVarId, TyVarId>()

        for q in scheme.Quantified do
            let qRoot = UnionFind.find ctx.Store q
            let fresh = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store fresh, ctx.CurrentLevel)
            subst.[qRoot.Id] <- TyVar fresh
            freshOf.[qRoot.Id] <- fresh

        // EVERY quantified root is freshened per call in the constraint substitution,
        // INCLUDING purely PHANTOM roots absent from the surface type (the enumerator `'E`
        // in `fold`'s `'S :> IStructSeq<'T,'E>`), so `'E` stays a free generic method slot.
        let constraintSubst = Dictionary<TyVarId, SemType>()

        for kv in subst do
            constraintSubst.[kv.Key] <- kv.Value

        // A `Coercion` target may ALSO reference still-free roots that are NOT quantified at
        // all, such as an outer-level placeholder that joined the bound when two roots unified. Left
        // verbatim it is SHARED, so the first call's grounding leaks into every later one.
        let quantifiedRoots = HashSet<TyVarId>(freshOf.Keys)

        for (_, c) in scheme.Constraints do
            match c.Kind with
            | SemanticConstraintKind.Coercion target ->
                target
                |> zonk ctx.Store
                |> iterTypeVarRoots
                    ctx.Store
                    (fun root ->
                        if
                            (ctx.Store.Link root).IsNone
                            && not (quantifiedRoots.Contains root.Id)
                            && not (constraintSubst.ContainsKey root.Id)
                        then
                            let fresh = ctx.NewTypeVar()
                            ctx.Store.SetLevel(UnionFind.find ctx.Store fresh, ctx.CurrentLevel)
                            constraintSubst.[root.Id] <- TyVar fresh
                    )
            | _ -> ()

        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find ctx.Store qTv

            match freshOf.TryGetValue qRoot.Id with
            | true, fresh ->
                let c =
                    match c.Kind with
                    | SemanticConstraintKind.Coercion target ->
                        { c with
                            Kind = SemanticConstraintKind.Coercion(substituteWith ctx.Store constraintSubst target)
                        }
                    | _ -> c

                addConstraintByKind ctx.Store fresh c
            | false, _ -> ()

        substituteWith ctx.Store subst scheme.Body

    /// Resolve a bound name to its type: instantiate its generalised scheme if one was
    /// written, else take the monomorphic binding-site TyVar (a sibling in the same
    /// `let rec` group, not yet generalised, which is what forbids polymorphic recursion).
    let instantiateBinding (ctx: PassContext) (rb: ResolvedBinding) : SemType =
        match ctx.Bindings.Scheme.TryGetValue rb.BindingSite with
        | ValueSome scheme -> instantiate ctx scheme
        | ValueNone -> TyVar(tvOf ctx rb.BindingSite)

    /// True if `t` contains a TyVar whose root carries a deferred `PendingDotAccess`
    /// constraint. Such a binding must stay monomorphic in v1: quantifying freezes the
    /// constraint into the scheme, where a use site pinning the object argument never discharges it.
    let rec hasPendingDotAccess (store: TypeStore) (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            if not (List.isEmpty (store.Pda.Live root)) then
                true
            else
                match store.Link root with
                | ValueSome target -> hasPendingDotAccess store target
                | ValueNone -> false
        // A compound carries pending dot access iff a child does; leaves hold none.
        | t -> SemType.existsChild (hasPendingDotAccess store) t

    /// A chained default like `default ^T3 : ^T1 ; default ^T1 : int` needs two passes,
    /// hence the fixpoint. Defaults walked here are *consumed*: once one fires (or all
    /// candidates fail) the list is cleared, so later passes don't re-walk dead targets.
    let applyDefaults (store: TypeStore) (zonkedTy: SemType) (outerLevel: int) : unit =
        let visited = HashSet<TyVarId>()

        let rec collect (t: SemType) : ResizeArray<TyVarId> =
            let acc = ResizeArray<TyVarId>()

            let rec go (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find store tv

                    if visited.Add root.Id then
                        if
                            store.Level root > outerLevel
                            && (store.Link root).IsNone
                            && not (store.Defaults.IsEmpty root)
                        then
                            acc.Add root.Id
                            // Follow the default-target graph: `default ^T3 : ^T1` references
                            // another TyVar that may be an *intermediate* result var (the
                            // inner `a + b` of `a + b + c`), off the binding's surface type.
                            for target in store.Defaults.Items root do
                                go target

                        match store.Link root with
                        | ValueSome target -> go target
                        | ValueNone -> ()
                | t -> SemType.iterChildren go t

            go t
            acc

        let candidates = collect zonkedTy

        let rec resolveTarget (t: SemType) : SemType voption =
            match t with
            | TyVar tv ->
                let root = UnionFind.find store tv

                match store.Link root with
                | ValueSome target -> resolveTarget target
                | ValueNone -> ValueNone
            | _ -> ValueSome t

        let tryDefault (tv: TyVarId) : bool =
            let root = UnionFind.find store tv
            let mutable fired = false
            let defaults = store.Defaults.Items root
            // A target resolving only to a still-free TyVar is *deferrable*: `default ^T2 :
            // ^T3` can't fire until ^T3 itself defaults on a later pass. Discarding it loses
            // the rest of the chain: `let g a b = a + b` grounds `a` but leaks `b` as a typar.
            let mutable anyDeferrable = false

            for target in defaults do
                if not fired then
                    match resolveTarget target with
                    | ValueSome concrete when not (occursAndAdjust store tv concrete) ->
                        // Occurs guard: a structural target (`^T1 list`) can resolve to a
                        // `concrete` transitively containing `tv`, and linking through it
                        // would build an infinite type.
                        store.SetLink(root, ValueSome concrete)
                        fired <- true
                    | ValueSome _ -> () // resolved but occurs-unsafe, so permanently dead
                    | ValueNone -> anyDeferrable <- true // target still free, so retry next pass

            // Clear once discharged, or once nothing is left to chase. A deferrable default
            // stays so the fixpoint re-evaluates it after its target links; `while changed`
            // re-iterates only while some default *fires*, bounding the retries.
            if fired || not anyDeferrable then
                store.Defaults.Set(root, [])

            fired

        let mutable changed = true

        while changed do
            changed <- false

            for tv in candidates do
                let root = UnionFind.find store tv

                if (store.Link root).IsNone && not (store.Defaults.IsEmpty root) then
                    if tryDefault tv then
                        changed <- true

    /// The element type of the bare list-literal registered against union-find `root` in
    /// `ctx.ListLiterals` (`ValueNone` if none). A look-up only: whether to flip the
    /// container to the Vesper or the FSharp.Core list stays with each caller.
    let tryListLiteralElem (ctx: PassContext) (root: TyVarId) : SemType voption =
        let mutable result = ValueNone

        for lit in ctx.ListLiterals do
            if result.IsNone && (UnionFind.find ctx.Store lit.Var).Id = root then
                result <- ValueSome lit.Elem

        result

    /// Settle the flexible list-literal containers reachable from a binding's type *before*
    /// it generalises, so the bare container var is never quantified as `∀L. L`: `let xs = []`
    /// links the container now; `let nums = [1;2;3]` drops its level so quantification skips it.
    let prepareListLiterals (ctx: PassContext) (ty: SemType) (outerLevel: int) : unit =
        if ctx.ListLiterals.Count = 0 then
            ()
        else
            let seen = HashSet<TyVarId>()

            let rec walk (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find ctx.Store tv

                    if seen.Add root.Id then
                        match ctx.Store.Link root with
                        | ValueSome target -> walk target
                        | ValueNone ->
                            match tryListLiteralElem ctx root.Id with
                            | ValueSome elemTy when ctx.Store.Level root > outerLevel ->
                                match zonk ctx.Store elemTy with
                                | TyVar _ ->
                                    // Out of scope, there is nothing to link to: leave the
                                    // container free and let it reach the whole-file sweep,
                                    // which blames each literal once, by its own token.
                                    if ctx.ConsListInScope then
                                        ctx.Store.SetLink(root, ValueSome(RuntimeNames.consListTy elemTy))
                                | _ -> ctx.Store.SetLevel(root, outerLevel)
                            | _ -> ()
                | t -> SemType.iterChildren walk t

            walk ty

    let generalise (store: TypeStore) (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        // Apply defaults before quantifying: a default that resolves links its source TyVar,
        // which the quantifier walk then skips. Without this, `let x = 1 + 2` would
        // generalise as `∀'a. 'a` instead of `int`.
        applyDefaults store zonkedTy outerLevel

        let quantified = ResizeArray<TyVarId>()
        let seen = HashSet<TyVarId>()

        let addRoot (root: Rep) =
            if store.Level root > outerLevel && (store.Link root).IsNone && seen.Add(root.Id) then
                quantified.Add(root.Id)

        zonkedTy |> iterTypeVarRoots store addRoot

        // Dependent typars: a `Coercion` bound may reference *further* typars that appear ONLY in
        // constraints (`'S :> IStructSeq<'T,'E>`, where `'E` is in no parameter/return position).
        // Un-quantified they leak as un-ground `TyVar`s, degraded at freeze to an `FTUnknown`.
        let mutable i = 0

        while i < quantified.Count do
            for c in store.Constraints.Items(UnionFind.find store quantified.[i]) do
                match c.Kind with
                | SemanticConstraintKind.Coercion target -> iterTypeVarRoots store addRoot (zonk store target)
                | _ -> ()

            i <- i + 1

        for tv in quantified do
            store.MarkQuantified(UnionFind.find store tv)

        // `instantiate` swaps these onto fresh substitutions per use site
        // so satisfaction is re-evaluated independently.
        let constraints =
            [
                for tv in quantified do
                    for c in store.Constraints.Items(UnionFind.find store tv) -> tv, c
            ]

        TypeScheme(List.ofSeq quantified, zonkedTy, constraints)

    /// The value restriction: a *parameterless* binding generalises only when its RHS is a
    /// syntactic value. Generalising an expansive one (`let res = ResizeArray<'T>()`)
    /// quantifies its own free element typar, leaving it dangling at every use site.
    let rec private isExpansive (e: Expr<SyntaxToken>) : bool =
        match e with
        | Expr.App _
        | Expr.HighPrecedenceApp _
        | Expr.New _ -> true
        | Expr.TypeAnnotation(expr = inner) -> isExpansive inner
        | _ -> false

    /// Single-name `let` generalises unless the binding is `mutable`. A mutable binding stays
    /// monomorphic: every use unifies against the binding's own TyVar, so a free TyVar can
    /// still be pinned by a later use or assignment, without a polymorphic scheme.
    let shouldGeneralise (b: Binding<SyntaxToken>) : bool =
        if b.mutableToken.IsSome then
            false
        // A parameterless binding with an expansive RHS is value-restricted; only function
        // bindings and non-expansive values generalise.
        elif b.argumentPats.IsEmpty && isExpansive b.expr then
            false
        else
            match b.pattern with
            | Pat.NamedSimple _ -> true
            // An operator-named binding (`let inline (=) …`) is a single-name
            // function; generalise it like any other function value.
            | Pat.Op _ -> true
            | _ -> false
