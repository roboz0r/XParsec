namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// The declaring / method typar envs a decl quantifies, and the `TyVar -> TyTypar` cut
// deferred until the whole decl is surfaced, so a member signature, a local and a case
// field all flip on the same indices.

module internal ElaborateTypars =

    /// Rewrite open typars to their frozen `TyTypar` nodes: `env` pairs each typar's
    /// zonked root with its target `TyTypar(axis, index)`. A `TyVar` whose root is not
    /// in `env` stays a `TyVar`, and degrades to `FTUnknown` at the freeze cut.
    let remapDeclTypars (store: TypeStore) (env: (TyVarId * SemType) list) (t: SemType) : SemType =
        let rec go t =
            match t with
            | TyVar tv ->
                match env |> List.tryPick (fun (r, target) -> if r = tv then Some target else None) with
                | Some target -> target
                | None -> t
            | t -> SemType.mapChildren go t

        go (Unification.zonk store t)

    let private mapExprTypes (f: SemType -> SemType) (e: TExpr) : TExpr =
        TastWalk.mapExpr
            { TastWalk.identityMapper with
                MapType = f
            }
            e

    /// Pair each declared typar's zonked root with `TyTypar(Declaring, i)`, `i` being its
    /// position in the declaration list. A typar pinned to a non-`TyVar` is dropped, but
    /// the index still counts it, so a surviving typar keeps its declared slot.
    let mkDeclTyparEnv (store: TypeStore) (protos: EqArray<TyVarId>) : (TyVarId * SemType) list =
        [
            for i in 0 .. protos.Length - 1 do
                match Unification.zonk store (TyVar protos.[i]) with
                | TyVar root -> yield (root, TyTypar(TyparAxis.Declaring, i))
                | _ -> ()
        ]

    /// The bounds the store holds on `root`, indexed `i`; a `Coercion` target stays
    /// `TyVar`-rooted for the deferred cut.
    let private boundsAt (store: TypeStore) (i: int) (root: TyVarId) : TyparConstraintG<SemType> list =
        [
            for sc in store.Constraints.Items(UnionFind.find store root) do
                match TyparConstraint.ofSemantic i id sc.Kind with
                | ValueSome c -> c
                | ValueNone -> ()
        ]

    /// The bounds on each typar of `env`, at the index its `TyTypar` marker carries.
    let boundsOfEnv (store: TypeStore) (env: (TyVarId * SemType) list) : EqSet<TyparConstraintG<SemType>> =
        EqSet.ofSeq
            [
                for (root, target) in env do
                    match target with
                    | TyTypar(_, i) -> yield! boundsAt store i root
                    | _ -> ()
            ]

    /// Quantify a module-`let`'s free type parameters into `TyTypar(Method, i)` in the F#
    /// canonical order: `declared` typars first in source order (`<'b,'a>` stays `'b,'a`),
    /// then the remaining free roots by first appearance, then the constraint-only typars.
    let mkMethodQuantEnv
        (store: TypeStore)
        (declared: DeclaredTypar list)
        (declTy: SemType)
        : (TyVarId * SemType) list =
        // A declared typar that inference pinned to a concrete type (its root is `Link`ed)
        // is not a method typar; drop it. A free function has no enclosing class typars, so
        // the `fixedRoots` set passed below is empty.
        let declaredFree =
            declared
            |> List.filter (fun tp -> (store.Link(UnionFind.find store tp.TyVar)).IsNone)

        let zonked = Unification.zonk store declTy

        // Free-fn inferred typars have no source names, so an empty `knownNames` leaves every
        // appearance-ordered typar to be synthesised as `M0`, `M1`, ….
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

        // The canonical roots seed the dependent-typar worklist below.
        let acc =
            ResizeArray<TyVarId>(GeneralizedTypars.toArray gt |> Array.map (fun tp -> tp.TyVar))

        let seen = System.Collections.Generic.HashSet<TyVarId>()

        for r in acc do
            seen.Add r |> ignore

        // A `Coercion` bound may reference typars absent from the declared type: in
        // `let f (s: 'S when 'S :> IStructSeq<'T,'E>)`, `'E` is in no parameter/return position.
        // F# generalises those too, so fold the bounds in to a fixpoint (a bound may add more).
        let mutable depIdx = 0

        while depIdx < acc.Count do
            for c in store.Constraints.Items(UnionFind.find store acc.[depIdx]) do
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    SemTypeWalk.collectLinkedRoots store acc seen (Unification.zonk store target)
                | _ -> ()

            depIdx <- depIdx + 1

        [ for i in 0 .. acc.Count - 1 -> acc.[i], TyTypar(TyparAxis.Method, i) ]

    /// The env's typar roots at the position a type argument for each is supplied: declaring
    /// axis first, then method axis, each in index order. A consumer's thaw of a template
    /// frozen with this env recovers this array as `InlineThaw.ThawedTemplate.Typars`.
    let quantifiedRoots (env: (TyVarId * SemType) list) : TyVarId[] =
        let rank (target: SemType) : (int * int) voption =
            match target with
            | TyTypar(TyparAxis.Declaring, i) -> ValueSome(0, i)
            | TyTypar(TyparAxis.Method, j) -> ValueSome(1, j)
            | _ -> ValueNone

        env
        |> Seq.choose (fun (root, target) ->
            match rank target with
            | ValueSome r -> Some(r, root)
            | ValueNone -> None
        )
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Array.ofSeq

    /// The declaring-type typars as `SemType` args, for a member's `ThisTy` and the body's
    /// synthesised `this` self-type: each declared typar zonked to its root `TyVar`. They
    /// stay `TyVar`-shaped until `freezeTypars` remaps them to `TyTypar(Declaring, i)`.
    let declTyparArgs (store: TypeStore) (typeParams: EqArray<DeclaredTypar>) : EqArray<SemType> =
        EqArray.ofSeq (seq { for tp in typeParams -> Unification.zonk store (TyVar tp.TyVar) })

    /// Elaborate one type member: stamp its `ThisTy` with the `TyVar`-rooted `selfTy` and
    /// surface its method-axis typar roots so the caller folds them into the decl's freeze
    /// env. Signature / body / return types stay verbatim, because the cut is deferred.
    let elaborateMember (selfTy: SemType) (m: TTypeMember) : TTypeMember * (TyVarId * SemType) list =
        let methodMarkers =
            [
                for i in 0 .. m.MethodTypeParams.Length - 1 do
                    match snd m.MethodTypeParams.[i] with
                    | TyVar root -> (root, TyTypar(TyparAxis.Method, i))
                    | _ -> ()
            ]

        { m with ThisTy = selfTy }, methodMarkers

    /// The per-member elaborator each host surfacer folds over its members. Host elaborators
    /// differ only in `selfTy`'s type constructor. Surface a member when the declaring type is generic
    /// (declaring axis) OR the member itself is generic (method axis); else leave it as is.
    let mkMemberElaborator
        (selfTy: SemType)
        (declTypars: EqArray<string>)
        (env: ResizeArray<TyVarId * SemType>)
        : TTypeMember -> TTypeMember =
        fun m ->
            if declTypars.Length = 0 && m.MethodTypeParams.Length = 0 then
                m
            else
                let m, methodMarkers = elaborateMember selfTy m
                env.AddRange methodMarkers
                m

    /// Walk every `SemType` in `p` through `remapDeclTypars env` (see `freezeTypars`).
    let freezeTyparsPat (store: TypeStore) (env: (TyVarId * SemType) list) (p: TPat) : TPat =
        TastWalk.mapPat
            { TastWalk.identityMapper with
                MapType = remapDeclTypars store env
            }
            p

    /// Walk every `SemType` in `e` through `remapDeclTypars env` (see `freezeTypars`).
    let freezeTyparsExpr (store: TypeStore) (env: (TyVarId * SemType) list) (e: TExpr) : TExpr =
        mapExprTypes (remapDeclTypars store env) e

    /// The deferred typar cut: walk every `SemType` in `d` through `remapDeclTypars env`,
    /// whose `env` holds the decl's own quantified typar roots. An empty `env` is then a
    /// pure zonk-rebuild. A type declaration's own slots are enumerated by `mapTypeDecl`.
    let freezeTypars (store: TypeStore) (env: (TyVarId * SemType) list) (d: TDecl) : TDecl =
        let f = remapDeclTypars store env

        match d with
        | TDecl.Let(binding, value, isInline, ty) ->
            TDecl.Let(freezeTyparsPat store env binding, mapExprTypes f value, isInline, f ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(mapExprTypes f e, f ty)
        | TDecl.Type td -> TDecl.Type(TastWalk.mapTypeDecl f (mapExprTypes f) td)
