namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// The typar envs a decl quantifies, one per scope, and the `TyVar -> TyTypar` cut deferred
// until the whole decl is surfaced, so a member signature, a local and a case field all flip
// on the same indices.

/// The typar roots a declaration quantifies, paired with their `TyTypar` targets.
[<RequireQualifiedAccess>]
type DeclEnv =
    | One of (TyVarId * SemType) list
    /// One env per `LetGroup` member, in member order: two members of one recursion
    /// component share roots at different method-typar indices.
    | PerMember of EqArray<(TyVarId * SemType) list>

    /// Every pair, across the members of a group.
    member this.All: (TyVarId * SemType) list =
        match this with
        | DeclEnv.One env -> env
        | DeclEnv.PerMember envs ->
            [
                for env in envs do
                    yield! env
            ]

module internal ElaborateTypars =

    /// Rewrite open typars to their frozen `TyTypar` nodes: `env` pairs each typar's
    /// zonked root with its target `TyTypar(scope, index)`. A `TyVar` whose root is not
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

    /// Pair each declared typar's zonked root with `TyTypar(scope, i)`, `i` being its
    /// position in the declaration list. A typar pinned to a non-`TyVar` is dropped, but
    /// the index still counts it, so a surviving typar keeps its declared slot.
    let mkDeclTyparEnv (store: TypeStore) (scope: TyparScope) (protos: EqArray<TyVarId>) : (TyVarId * SemType) list =
        [
            for i in 0 .. protos.Length - 1 do
                match Unification.zonk store (TyVar protos.[i]) with
                | TyVar root -> yield (root, TyTypar(scope, i))
                | _ -> ()
        ]

    /// The constraints the store holds on `root`, indexed `i`; a `Coercion` target stays
    /// `TyVar`-rooted for the deferred cut.
    let private constraintsAt (store: TypeStore) (i: int) (root: TyVarId) : TyparConstraintG<SemType> list =
        [
            for sc in store.Constraints.Items(UnionFind.find store root) do
                match TyparConstraint.ofSemantic i id sc.Kind with
                | ValueSome c -> c
                | ValueNone -> ()
        ]

    /// The constraints on each typar of `env`, at the index its `TyTypar` marker carries.
    let constraintsOfEnv (store: TypeStore) (env: (TyVarId * SemType) list) : EqSet<TyparConstraintG<SemType>> =
        EqSet.ofSeq
            [
                for (root, target) in env do
                    match target with
                    | TyTypar(_, i) -> yield! constraintsAt store i root
                    | _ -> ()
            ]

    /// Quantify a module-`let`'s free type parameters into `TyTypar(scope, i)` in the F#
    /// canonical order: `declared` typars first in source order (`<'b,'a>` stays `'b,'a`),
    /// then the remaining free roots by first appearance, then the constraint-only typars.
    let mkMethodQuantEnv
        (store: TypeStore)
        (scope: TyparScope)
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

        // A constraint's embedded type may reference typars absent from the declared type: in
        // `let f (s: 'S when 'S :> IStructSeq<'T,'E>)`, `'E` is in no parameter/return position.
        // F# generalises those too, so fold the constraints in to a fixpoint (a constraint may add more).
        let mutable depIdx = 0

        while depIdx < acc.Count do
            for c in store.Constraints.Items(UnionFind.find store acc.[depIdx]) do
                c.Kind
                |> SemanticConstraintKind.iterTypes (fun t ->
                    SemTypeWalk.collectLinkedRoots store acc seen (Unification.zonk store t)
                )

            depIdx <- depIdx + 1

        [ for i in 0 .. acc.Count - 1 -> acc.[i], TyTypar(scope, i) ]

    /// The env's typar roots in type-argument order: the declaring type's first, then the
    /// member's or module function's own, each by index. A thaw of a template frozen with this
    /// env recovers the same order.
    let quantifiedRoots (env: (TyVarId * SemType) list) : TyVarId[] =
        let rank (target: SemType) : (int * int) voption =
            match target with
            | TyTypar(TyparScope.Type _, i) -> ValueSome(0, i)
            | TyFunctionTypar j -> ValueSome(1, j)
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
    /// stay `TyVar`-shaped until `freezeTypars` remaps them to `TyTypar(Type _, i)`.
    let declTyparArgs (store: TypeStore) (typeParams: EqArray<DeclaredTypar>) : EqArray<SemType> =
        EqArray.ofSeq (seq { for tp in typeParams -> Unification.zonk store (TyVar tp.TyVar) })

    /// Elaborate one member of `declKey`: stamp `ThisTy` with the `TyVar`-rooted `selfTy` and
    /// pair its own typar roots with markers under the member's scope, for the decl's freeze
    /// env. Signature / body / return types stay verbatim until the deferred cut.
    let elaborateMember (declKey: TypeKey) (selfTy: SemType) (m: TTypeMember) : TTypeMember * (TyVarId * SemType) list =
        let scope = TyparScope.Member declKey

        let methodMarkers =
            [
                for i in 0 .. m.MethodTypeParams.Length - 1 do
                    match snd m.MethodTypeParams.[i] with
                    | TyVar root -> (root, TyTypar(scope, i))
                    | _ -> ()
            ]

        { m with ThisTy = selfTy }, methodMarkers

    /// The per-member elaborator each host surfacer folds over the members of `declKey`.
    /// Host elaborators differ only in `selfTy`'s type constructor. Surface a member when
    /// the declaring type is generic OR the member itself is generic; else leave it as is.
    let mkMemberElaborator
        (declKey: TypeKey)
        (selfTy: SemType)
        (declTypars: EqArray<string>)
        (env: ResizeArray<TyVarId * SemType>)
        : TTypeMember -> TTypeMember =
        fun m ->
            if declTypars.Length = 0 && m.MethodTypeParams.Length = 0 then
                m
            else
                let m, methodMarkers = elaborateMember declKey selfTy m
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
    let freezeTypars (store: TypeStore) (env: DeclEnv) (d: TDecl) : TDecl =
        let f = remapDeclTypars store env.All

        let freezeMember (m: TLetMember) (env: (TyVarId * SemType) list) : TLetMember =
            let f = remapDeclTypars store env

            {
                Pattern = freezeTyparsPat store env m.Pattern
                Value = mapExprTypes f m.Value
                Tok = m.Tok
            }

        match d, env with
        | TDecl.Let(binding, isInline, isRec), _ -> TDecl.Let(freezeMember binding env.All, isInline, isRec)
        | TDecl.LetGroup(members, components), DeclEnv.PerMember envs ->
            TDecl.LetGroup(EqArray.map2 freezeMember members envs, components)
        | TDecl.LetGroup(members, _), DeclEnv.One _ ->
            failwithf
                "ElaborateTypars.freezeTypars: a group of %d members was paired with one env rather than one per member"
                members.Length
        | TDecl.Expression(e, ty), _ -> TDecl.Expression(mapExprTypes f e, f ty)
        | TDecl.Type td, _ -> TDecl.Type(TastWalk.mapTypeDecl f (mapExprTypes f) td)
