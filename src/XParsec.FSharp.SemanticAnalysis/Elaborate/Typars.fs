namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// The typar axis of the Elaborate pass: the declaring / method typar envs a decl
// quantifies, and the deferred `TyVar -> TyTypar` cut applied once the whole decl is
// surfaced. Everything the surfacers build stays metavar-shaped until that cut, so a
// member signature, a local and a case field all flip on the same indices.

module internal ElaborateTypars =

    /// Rewrite open typars (free `TyVar`s, by zonked root) to their frozen
    /// `TyTypar` nodes: `env` pairs each typar's zonked root
    /// with its target `TyTypar(axis, index)`. Anything else passes through
    /// unchanged — a leftover inference `TyVar` not in `env` stays a `TyVar`, which
    /// the backend rejects loudly (an unresolved-typar bug).
    let remapDeclTypars (store: TypeStore) (env: (TyVarId * SemType) list) (t: SemType) : SemType =
        let rec go t =
            match t with
            | TyVar tv ->
                match env |> List.tryPick (fun (r, target) -> if r = tv then Some target else None) with
                | Some target -> target
                | None -> t
            | t -> SemType.mapChildren go t

        go (Unification.zonk store t)

    /// Rewrite every `SemType` embedded in a member body via `f`. Used to push a
    /// generic union's declaring-typar remap (`remapDeclTypars`) through the whole
    /// member body, so a typar-typed local / scrutinee / bound variable carries the
    /// `TyConst "'T"` marker the backend's generic-member encoder consumes — just as
    /// the case-field types do (generalised to member bodies).
    let private mapExprTypes (f: SemType -> SemType) (e: TExpr) : TExpr =
        TastWalk.mapExpr
            { TastWalk.identityMapper with
                MapType = f
            }
            e

    /// Pair each declared typar's *zonked* root TyVar with the frozen
    /// `TyTypar(Declaring, i)` it remaps to; the index is the typar's position in its
    /// declaration list — the same index the backend's `GenericParam` rows use. Pinned
    /// typars (collapsed to a non-`TyVar`) are dropped (nothing to remap), but the loop
    /// index still tracks declaration position so a surviving typar keeps its correct
    /// slot. Shared by every `try*Type` surfacer and the interface / abstract-method
    /// projections. The method axis is never minted from a declaration list — a
    /// method's typars come from its generalised carrier (`GeneralizedTypars.methodEnv`)
    /// or, for a module `let`, from `mkMethodQuantEnv`.
    let mkDeclTyparEnv (store: TypeStore) (typeParams: EqArray<string * TyVarId>) : (TyVarId * SemType) list =
        [
            for i in 0 .. typeParams.Length - 1 do
                let (_, ptv) = typeParams.[i]

                match Unification.zonk store (TyVar ptv) with
                | TyVar root -> yield (root, TyTypar(TyparAxis.Declaring, i))
                | _ -> ()
        ]

    /// Quantify a module-`let`'s free type parameters into `TyTypar(Method, i)` in
    /// CANONICAL order via the one shared `GeneralizedTypars.canonical` — the F#
    /// rule: explicitly-declared `<'b,'a>` typars first in source order (`declared`,
    /// threaded from the `TDecl.Let` site), then the remaining inferred roots by
    /// first-left-to-right appearance (params left-to-right, then return). A *linked*
    /// root (pinned to a concrete type, or a measure carrier whose `Link` points at
    /// its carrier) is followed, not collected, so measures and pinned vars stay out
    /// of the typar list. After the canonical order, the dependent-typar fixpoint
    /// (constraint-only `Coercion` targets, absent from the type) is preserved and
    /// appended, mirroring `InferGeneralize.generalise`. The resulting env feeds
    /// `remapDeclTypars`, exactly like the declaring-typar env in 2A. Caller
    /// restricts this to function bindings (a non-function value's free var is a
    /// value-restriction case, not a method typar).
    let mkMethodQuantEnv
        (store: TypeStore)
        (declared: (string * TyVarId) list)
        (declTy: SemType)
        : (TyVarId * SemType) list =
        // The canonical F# order — declared typars first in source order, then the
        // remaining free roots by first-left-to-right-appearance — is computed by the
        // ONE shared `GeneralizedTypars.canonical`. Free functions have no enclosing
        // class typars, so `fixedRoots` is empty. A declared typar that inference
        // pinned to a concrete type (its root is `Link`ed) is NOT a real method typar;
        // drop it so this stays identical to the old appearance-only walk for the
        // no-typar / pinned-declared cases (only the genuinely-reordered case changes).
        let declaredFree =
            declared
            |> List.filter (fun (_, tv) -> (store.Link(UnionFind.find store tv)).IsNone)

        let zonked = Unification.zonk store declTy

        // Free-fn inferred typars have no source names, so an empty `knownNames`
        // preserves today's all-`M%d` synthesis for the appearance tail.
        let knownNames =
            System.Collections.Generic.Dictionary<TyVarId, string>()
            :> System.Collections.Generic.IReadOnlyDictionary<_, _>

        let gt =
            GeneralizedTypars.canonical
                store
                declaredFree
                (System.Collections.Generic.HashSet<TyVarId>())
                knownNames
                zonked

        // The canonical roots, in ABI order, become the seed of the dependent-typar
        // worklist below.
        let acc = ResizeArray<TyVarId>(GeneralizedTypars.toArray gt |> Array.map snd)
        let seen = System.Collections.Generic.HashSet<TyVarId>()

        for r in acc do
            seen.Add r |> ignore

        // Dependent typars (mirrors `InferGeneralize.generalise`): a collected typar's
        // `Coercion` bound may name further typars absent from the declared (curried)
        // type — `let f (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator>)`
        // has `'E` in no parameter/return position. F# generalises these phantom
        // parameters too, so they are genuine method typars; fold each collected
        // typar's `Coercion` targets in to a fixpoint (a bound may itself reference a
        // typar with bounds), `ResizeArray` growth driving the worklist. Without this a
        // constrained `for … in` over `'S` leaks `'E` as `?free-typar` at the freeze cut.
        let mutable depIdx = 0

        while depIdx < acc.Count do
            for c in store.Constraints.Items(UnionFind.find store acc.[depIdx]) do
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    // Append the first-appearance roots of the coercion-bound target
                    // (link-following, deduped against the seed). The shared collector.
                    SemTypeWalk.collectLinkedRoots store acc seen (Unification.zonk store target)
                | _ -> ()

            depIdx <- depIdx + 1

        [ for i in 0 .. acc.Count - 1 -> acc.[i], TyTypar(TyparAxis.Method, i) ]

    /// The declaring-type typars as `SemType` args, for a member's `ThisTy` and
    /// the body's synthesised `this` self-type: each declared typar zonked to its
    /// root `TyVar`. `elaborate` keeps these in `TyVar` form (not `TyTypar`) so
    /// the whole tree stays metavar-shaped until the `freezeTypars` cut, which
    /// remaps each root to `TyTypar(Declaring, i)`. The
    /// index `i` is the typar's declaration position — the same index
    /// `mkDeclTyparEnv` pairs the root with — so the round-trip is faithful.
    let declTyparArgs (store: TypeStore) (typeParams: EqArray<string * TyVarId>) : EqArray<SemType> =
        EqArray.ofSeq (seq { for (_, ptv) in typeParams -> Unification.zonk store (TyVar ptv) })

    /// Elaborate one type member: stamp its `ThisTy` with the `TyVar`-rooted
    /// `selfTy` and surface its *method-axis* typar roots so the caller folds them
    /// into the decl's freeze env. The signature / body / return types stay
    /// verbatim — the `TyVar → TyTypar` cut is deferred to `freezeTypars`. Shared
    /// by the union / class member surfacers (they differ only in `selfTy`'s
    /// `TyUnion` vs `TyClass` head). `MethodTypeParams` rides `'ty` and is cut by
    /// `freezeTypars` alongside the body; here we only READ each entry's root
    /// (`TyVar root`) to key the `env` marker `TyTypar(Method, i)` on it.
    let elaborateMember (selfTy: SemType) (m: TTypeMember) : TTypeMember * (TyVarId * SemType) list =
        let methodMarkers =
            [
                for i in 0 .. m.MethodTypeParams.Length - 1 do
                    match snd m.MethodTypeParams.[i] with
                    | TyVar root -> (root, TyTypar(TyparAxis.Method, i))
                    | _ -> ()
            ]

        { m with ThisTy = selfTy }, methodMarkers

    /// The per-member elaborator every host surfacer (union / record / class /
    /// intrinsic-abbrev) folds over its members — they differ only in `selfTy`'s
    /// head. Surface a member when the declaring type is generic (declaring axis) *or*
    /// the member itself is generic (method axis): stamp its self-type and fold its
    /// method typars into the decl `env`, so `freezeTypars` later flips both axes. A
    /// generic method on a *monomorphic* host still needs its `'C` cut to
    /// `TyTypar(Method, i)`, so it can't be skipped. For a mono host with a mono member,
    /// `selfTy` equals the member's existing `ThisTy`, so leaving it verbatim is
    /// byte-identical.
    let mkMemberElaborator
        (selfTy: SemType)
        (declTypars: string list)
        (env: ResizeArray<TyVarId * SemType>)
        : TTypeMember -> TTypeMember =
        fun m ->
            if List.isEmpty declTypars && m.MethodTypeParams.Length = 0 then
                m
            else
                let m, methodMarkers = elaborateMember selfTy m
                env.AddRange methodMarkers
                m

    /// The deferred typar cut. Walk every `SemType` in a
    /// decl through `remapDeclTypars env`, rewriting the decl's open `TyVar` typars
    /// to their `TyTypar(axis, index)` nodes. `env` is the decl's own quantified
    /// typar roots, collected by `elaborate` (the single index-minting point).
    /// `remapDeclTypars` zonks as it recurses, so an empty `env` is a pure
    /// zonk-rebuild — exactly the old monomorphic `remapDeclTypars []` path every
    /// surfacer applied inline.
    ///
    /// A type declaration's slots are NOT enumerated here: `TastWalk.mapTypeDecl` carries
    /// the cut through the declaration shape's single enumeration, which is how the cut
    /// reaches a member signature, a preamble initialiser and a secondary ctor's chain args
    /// alike — including `MethodTypeParams`, which rides `'ty` like every other slot, so `f`
    /// flips each entry's `TyVar root` to `TyTypar(Method, i)` and the `GenericParam` rows key
    /// on the marker rather than on a cell. A binding's own pattern and value are the term
    /// axis and stay here; the shape rebuild has no pattern slot to give them.
    let freezeTypars (store: TypeStore) (env: (TyVarId * SemType) list) (d: TDecl) : TDecl =
        let f = remapDeclTypars store env

        match d with
        | TDecl.Let(binding, value, isInline, ty) ->
            let binding =
                TastWalk.mapPat
                    { TastWalk.identityMapper with
                        MapType = f
                    }
                    binding

            TDecl.Let(binding, mapExprTypes f value, isInline, f ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(mapExprTypes f e, f ty)
        | TDecl.Type td -> TDecl.Type(TastWalk.mapTypeDecl f (mapExprTypes f) td)
