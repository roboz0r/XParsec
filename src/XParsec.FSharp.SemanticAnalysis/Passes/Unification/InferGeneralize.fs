namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate

module UnificationInferGeneralize =

    /// Non-quantified TyVars are left alone — they're free w.r.t. the
    /// surrounding scope and must keep their identity. `scheme.Body` is
    /// already zonked by `generalise`, so we don't follow Links here.
    let instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        let freshOf = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- TyVar fresh
            freshOf.[qRoot] <- fresh

        // Re-stamp constraints onto the fresh instance TyVars so each use
        // site re-evaluates satisfaction against its own substitution; the
        // original quantified TyVars stay constraint-bearing for the next call.
        for (qTv, c) in scheme.Constraints do
            let qRoot = UnionFind.find qTv

            match freshOf.TryGetValue qRoot with
            | true, fresh ->
                if not (fresh.Constraints |> List.exists (fun e -> e.Kind = c.Kind)) then
                    fresh.Constraints <- c :: fresh.Constraints
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
        | TyConst(_, args) -> EqArray.exists hasPendingDotAccess args
        | TyFun(a, r) -> hasPendingDotAccess a || hasPendingDotAccess r
        | TyTuple xs -> EqArray.exists hasPendingDotAccess xs
        | TyRecord(_, args) -> EqArray.exists hasPendingDotAccess args
        | TyUnion(_, args) -> EqArray.exists hasPendingDotAccess args
        | TyClass(_, args) -> EqArray.exists hasPendingDotAccess args
        | TyUnknown _ -> false

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

                        match root.Link with
                        | ValueSome target -> go target
                        | ValueNone -> ()
                | TyConst(_, args) ->
                    for a in args do
                        go a
                | TyFun(a, r) ->
                    go a
                    go r
                | TyTuple xs ->
                    for x in xs do
                        go x
                | TyRecord(_, args) ->
                    for a in args do
                        go a
                | TyUnion(_, args) ->
                    for a in args do
                        go a
                | TyClass(_, args) ->
                    for a in args do
                        go a
                | TyUnknown _ -> ()

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
                    | _ -> ()

            // Clear regardless — discharged, or not worth chasing further.
            tv.Defaults <- []
            fired

        let mutable changed = true

        while changed do
            changed <- false

            for tv in candidates do
                if tv.Link.IsNone && not (List.isEmpty tv.Defaults) then
                    if tryDefault tv then
                        changed <- true

    /// Settle the flexible list-literal containers (R3) reachable from a binding's
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
            let flexElem (root: TypeVar) : SemType voption =
                let mutable result = ValueNone

                for (lv, elem) in ctx.ListLiterals do
                    if result.IsNone && System.Object.ReferenceEquals(UnionFind.find lv, root) then
                        result <- ValueSome elem

                result

            let seen = HashSet<TypeVar>(HashIdentity.Reference)

            let rec walk (t: SemType) =
                match t with
                | TyVar tv ->
                    let root = UnionFind.find tv

                    if seen.Add root then
                        match root.Link with
                        | ValueSome target -> walk target
                        | ValueNone ->
                            match flexElem root with
                            | ValueSome elemTy when root.Level > outerLevel ->
                                match zonk elemTy with
                                | TyVar _ ->
                                    root.Link <-
                                        ValueSome(TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy))
                                | _ -> root.Level <- outerLevel
                            | _ -> ()
                | TyFun(a, b) ->
                    walk a
                    walk b
                | TyConst(_, xs)
                | TyTuple xs
                | TyRecord(_, xs)
                | TyUnion(_, xs)
                | TyClass(_, xs) ->
                    for x in xs do
                        walk x
                | TyUnknown _ -> ()

            walk ty

    let generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        // Apply defaults before quantifying: a default that resolves links
        // its source TyVar, which the quantifier walk then skips. Without
        // this, `let x = 1 + 2` would generalise as `∀'a. 'a` instead of
        // `int` (the unbound `^T3` from external-symbol Instantiate).
        applyDefaults zonkedTy outerLevel

        let quantified = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if root.Level > outerLevel && root.Link.IsNone && seen.Add(root) then
                    quantified.Add(root)
            | TyConst(_, args) ->
                for a in args do
                    walk a
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs ->
                for x in xs do
                    walk x
            | TyRecord(_, args) ->
                for a in args do
                    walk a
            | TyUnion(_, args) ->
                for a in args do
                    walk a
            | TyClass(_, args) ->
                for a in args do
                    walk a
            | TyUnknown _ -> ()

        walk zonkedTy

        // `instantiate` swaps these onto fresh substitutions per use site
        // so satisfaction is re-evaluated independently.
        let constraints =
            [
                for tv in quantified do
                    for c in tv.Constraints -> tv, c
            ]

        TypeScheme(List.ofSeq quantified, zonkedTy, constraints)

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
        else
            match b.headPat with
            | Pat.NamedSimple _ -> true
            // An operator-named binding (`let inline (=) …`) is a single-name
            // head; generalise it like any other function value.
            | Pat.Op _ -> true
            | _ -> false
