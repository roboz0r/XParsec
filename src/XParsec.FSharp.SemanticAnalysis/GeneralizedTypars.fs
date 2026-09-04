namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

module SemTypeWalk =

    /// `onVar` sees the RAW (un-`find`ed) typar, so each caller decides its own
    /// find / link / dedup policy. The walk does NOT recurse past a `TyVar`.
    let iterSemTypeVars (onVar: TyVarId -> unit) (t: SemType) : unit =
        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv -> onVar tv
            | t -> SemType.iterChildren walk t

        walk t

    /// Appends every still-free root reachable from `t`, FOLLOWING links: a `Link`ed root
    /// is a measure / pinned carrier, not a typar, so recurse into its target instead of
    /// collecting it.
    let collectLinkedRoots (store: TypeStore) (acc: ResizeArray<TyVarId>) (seen: HashSet<TyVarId>) (t: SemType) : unit =
        let rec onVar (tv: TyVarId) =
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome target -> iterSemTypeVars onVar target
            | ValueNone ->
                if seen.Add root.Id then
                    acc.Add root.Id

        iterSemTypeVars onVar t

/// Typars in CANONICAL order: declared typars first in source order, then the remaining
/// free roots in first-left-to-right-appearance order (`TyFun` domain before range; tuple
/// and nominal args left-to-right). Array position IS the ABI method-typar index.
type GeneralizedTypars = private | GeneralizedTypars of DeclaredTypar[]

module internal GeneralizedTypars =

    /// `zonkedTy` must already be zonked by the caller. A root already declared, or in
    /// `fixedRoots`, is skipped. A declared entry keeps its kind; an inferred root is
    /// type-kinded.
    let canonical
        (store: TypeStore)
        (declared: DeclaredTypar list)
        (fixedRoots: HashSet<TyVarId>)
        (knownNames: IReadOnlyDictionary<TyVarId, string>)
        (zonkedTy: SemType)
        : GeneralizedTypars =
        let result = ResizeArray<DeclaredTypar>()
        let seen = HashSet<TyVarId>()

        for tp in declared do
            let root = UnionFind.find store tp.TyVar

            if seen.Add root.Id then
                result.Add { tp with TyVar = root.Id }

        let mutable inferredCount = 0

        let addInferred (name: string) (root: TyVarId) =
            result.Add
                {
                    Name = name
                    TyVar = root
                    Kind = TyparKind.Type
                }

        zonkedTy
        |> SemTypeWalk.iterSemTypeVars (fun tv ->
            let root = UnionFind.find store tv

            if
                (store.Link root).IsNone
                && not (fixedRoots.Contains root.Id)
                && seen.Add root.Id
            then
                // A registered source name (a real `'a`) wins; the `M%d` index bumps only
                // when one is minted, so synthetic indices stay dense.
                match knownNames.TryGetValue root.Id with
                | true, n -> addInferred n root.Id
                | _ ->
                    addInferred (sprintf "M%d" inferredCount) root.Id
                    inferredCount <- inferredCount + 1
        )

        GeneralizedTypars(result.ToArray())

    let methodEnv (GeneralizedTypars roots) : (TyVarId * SemType) list =
        [
            for i in 0 .. roots.Length - 1 -> (roots.[i].TyVar, TyTypar(TyparAxis.Method, i))
        ]

    /// ORDER-PRESERVING root refresh: `f` returns an entry's CURRENT union-find / link
    /// representative, or `ValueNone` if it pinned to a concrete type. Those entries are
    /// DROPPED, since a kept-but-linked one would inflate the method's GenericParam arity.
    let refreshRoots (f: TyVarId -> TyVarId voption) (GeneralizedTypars roots) : GeneralizedTypars =
        GeneralizedTypars(
            roots
            |> Array.choose (fun tp ->
                match f tp.TyVar with
                | ValueSome r -> Some { tp with TyVar = r }
                | ValueNone -> None
            )
        )

    let toArray (GeneralizedTypars roots) : DeclaredTypar[] = roots

    let names (GeneralizedTypars roots) : string[] = roots |> Array.map (fun tp -> tp.Name)

    let count (GeneralizedTypars roots) : int = roots.Length

    let empty: GeneralizedTypars = GeneralizedTypars [||]
