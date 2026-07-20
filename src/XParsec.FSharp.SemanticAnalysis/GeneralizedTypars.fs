namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

/// THE one SemType structural pre-order skeleton, plus the link-following root
/// collector built on it. Lives here (compile-unit #14, after `UnionFind` and
/// before every consumer — `Inline`, `InferGeneralize`, `Elaborate`) so the
/// four historically-duplicated "first-appearance typar" walks share a single
/// traversal. Free of any Unification-pass dependency: needs only `SemType` /
/// `TyVarId` / `UnionFind`, all earlier compile units.
module SemTypeWalk =

    /// THE one SemType typar-collector: invoke `onVar` at every `TyVar` leaf with
    /// the RAW (un-`find`ed) typar — each caller's leaf decides its own
    /// `UnionFind.find` / `Link` / dedup policy. A `TyVar` is a leaf: this does
    /// NOT recurse past it (a link-following caller re-enters via `onVar`). All
    /// composite forms recurse via the shared `SemType.iterChildren` skeleton,
    /// left-to-right — the type-level computations included (a fresh method var
    /// can live in any child).
    let iterSemTypeVars (onVar: TyVarId -> unit) (t: SemType) : unit =
        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv -> onVar tv
            | t -> SemType.iterChildren walk t

        walk t

    /// Append every still-free root reachable from `t` into `acc` (deduped by
    /// `seen`), FOLLOWING links: a `Link`ed root is a measure / pinned carrier, not
    /// a typar, so recurse into its target instead of collecting it. Built on
    /// `iterSemTypeVars`. Shared by `Inline.quantifiedTypars` and
    /// `Elaborate.mkMethodQuantEnv`'s dependent-typar fixpoint.
    let collectLinkedRoots (store: TypeStore) (acc: ResizeArray<TyVarId>) (seen: HashSet<TyVarId>) (t: SemType) : unit =
        let rec onVar (tv: TyVarId) =
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome target -> iterSemTypeVars onVar target
            | ValueNone ->
                if seen.Add root.Id then
                    acc.Add root.Id

        iterSemTypeVars onVar t

/// Generalized typars in CANONICAL order — the F# rule (verbatim): explicitly
/// declared typars first in source-declaration order, then the remaining free
/// roots in first-left-to-right-appearance order over the value/member type
/// (`TyFun` domain before range; tuple / nominal / union args left-to-right).
///
/// INVARIANT: array position IS the ABI method-typar index (`TyTypar(Method, i)`).
/// The constructor is private and `canonical` is its only builder, so no call
/// site can mint an ad-hoc order — the one ordering implementation is
/// correct-by-construction. WHY: the GenericParam table, every MethodSpec, the
/// open signature, and the type scheme must all derive their typar order from a
/// single source, because F# treats this order as an ABI surface (the data analog
/// of F#'s `PlaceTyparsInDeclarationOrder` + `freeInTypeLeftToRight`).
type GeneralizedTypars = private | GeneralizedTypars of (string * TyVarId)[]

/// Same-file companion so it can construct the private case. `canonical` is the
/// sole producer; everything else is read-only projection.
module GeneralizedTypars =

    /// THE single ordering implementation — the data analog of F#'s
    /// `PlaceTyparsInDeclarationOrder` + `freeInTypeLeftToRight`.
    ///
    /// `declared` (source order, `[]` when none) come first, keeping their given
    /// names; then every still-free root reachable in `zonkedTy` is appended in
    /// first-appearance order, EXCLUDING any root already declared or in
    /// `fixedRoots` (the enclosing class typars). Inferred roots get the synthetic
    /// `Mn` name — same scheme as `Unification.generaliseMemberTypars` — indexed
    /// over the inferred tail only, so emitted names stay consistent.
    ///
    /// Only UNLINKED roots are collected (a `TyVar` whose union-find root has no
    /// `.Link`), mirroring `InferGeneralize.iterTyVarIdRoots` /
    /// `generaliseMemberTypars`. `zonkedTy` is ALREADY zonked by the caller — this
    /// pass never zonks, keeping the file free of any Unification-pass dependency.
    ///
    /// NO FS0664 analog (and why none is possible): F#'s
    /// `tcTypeParametersInferredAreNotStable` (FS0664) warns when a generic value's
    /// inferred typar order would shift if type abbreviations were erased — i.e. when
    /// a reordering/dropping alias (`type Pair<'a,'b> = 'b * 'a`) sits between the
    /// declared order and the appearance order. That divergence CANNOT arise here:
    /// abbreviations carry no distinct `SemType` form (there is no `TyAbbrev` — eager
    /// erasure is a deliberate design choice) and are fully substituted inside
    /// `translateType`, long before inference. `zonkedTy` is therefore already
    /// abbreviation-ERASED, so the appearance walk only ever sees the expanded body;
    /// the post-erasure order is the one and only order, taken silently. Restoring the
    /// F# warning would require RE-introducing the rejected `TyAbbrev` form to even
    /// have two orders to compare — out of scope and against the erasure design.
    let canonical
        (store: TypeStore)
        (declared: (string * TyVarId) list)
        (fixedRoots: HashSet<TyVarId>)
        (knownNames: IReadOnlyDictionary<TyVarId, string>)
        (zonkedTy: SemType)
        : GeneralizedTypars =
        let result = ResizeArray<string * TyVarId>()
        // Roots are compared / deduped / excluded by union-find representative id —
        // dense per-file ids, so structural int equality IS representative identity.
        let seen = HashSet<TyVarId>()

        // 1. Declared typars first, in given order, by their union-find roots.
        for (name, tv) in declared do
            let root = UnionFind.find store tv

            if seen.Add root.Id then
                result.Add(name, root.Id)

        // 2. Remaining free roots of `zonkedTy` in first-left-to-right-appearance
        //    order — the shared `SemTypeWalk.iterSemTypeVars` skeleton with this
        //    pass's leaf policy.
        let mutable inferredCount = 0

        zonkedTy
        |> SemTypeWalk.iterSemTypeVars (fun tv ->
            let root = UnionFind.find store tv

            if
                (store.Link root).IsNone
                && not (fixedRoots.Contains root.Id)
                && seen.Add root.Id
            then
                // Prefer the registered source name (a real `'a` F# keeps in the
                // emitted GenericParam); synthesise a method-scoped `M%d` only for a
                // genuinely body-inferred root, bumping the index only when minted so
                // synthetic indices stay dense.
                match knownNames.TryGetValue root.Id with
                | true, n -> result.Add(n, root.Id)
                | _ ->
                    result.Add(sprintf "M%d" inferredCount, root.Id)
                    inferredCount <- inferredCount + 1
        )

        GeneralizedTypars(result.ToArray())

    /// The ONE position→index materialization: `roots.[i]`'s `TyVarId` ↦
    /// `TyTypar(TyparAxis.Method, i)`. Callers use this to rewrite a body's typar
    /// identities to frozen method markers.
    let methodEnv (GeneralizedTypars roots) : (TyVarId * SemType) list =
        [
            for i in 0 .. roots.Length - 1 -> (snd roots.[i], TyTypar(TyparAxis.Method, i))
        ]

    /// ORDER-PRESERVING root refresh + drop: re-zonk each entry's root to its
    /// CURRENT union-find / link representative (`f` returns the live root), and
    /// DROP any entry whose root pinned to a concrete type (`f` returns `ValueNone`)
    /// — that typar is no longer real, exactly as the pre-split Elaborate helper's
    /// per-entry `zonk`+drop did (a kept-but-linked entry would inflate the method's
    /// GenericParam arity and break call-site `recoverOpenTypars`). A sub-sequence in
    /// the SAME relative order is still canonical — this mints no new order — so the
    /// invariant holds. Used by Elaborate to align the carrier's roots with the roots
    /// the member's frozen signature / body actually reference.
    let refreshRoots (f: TyVarId -> TyVarId voption) (GeneralizedTypars roots) : GeneralizedTypars =
        GeneralizedTypars(
            roots
            |> Array.choose (fun (n, tv) ->
                match f tv with
                | ValueSome r -> Some(n, r)
                | ValueNone -> None
            )
        )

    let toArray (GeneralizedTypars roots) : (string * TyVarId)[] = roots

    let names (GeneralizedTypars roots) : string[] = Array.map fst roots

    let count (GeneralizedTypars roots) : int = roots.Length

    /// The zero-typar value (a non-generic value/member).
    let empty: GeneralizedTypars = GeneralizedTypars [||]

    /// UNSAFE, DESERIALIZATION-ONLY carrier rebuild — do not use in analysis.
    ///
    /// It mints a FRESH `TypeVar` per name, so it does NOT preserve union-find identity
    /// and does NOT establish a canonical order: it merely TRANSPORTS an already-canonical
    /// name sequence (`names` of a carrier `canonical` produced) back into the type, in the
    /// given order. Passing a non-canonical `ns` silently breaks the array-position-is-ABI-
    /// index invariant `canonical` exists to guarantee — hence `unsafe`. The one legitimate
    /// caller is the frozen serializer (`FrozenCodec`), thawing a `MethodTypeParams` whose
    /// live roots cannot survive a byte round-trip and whose post-freeze consumers read only
    /// `names` + `count`. `[||]` reproduces `empty` exactly. See
    /// `docs/frozen-tree-semtype-residue-plan.md` for the residue this works around.
    let unsafeOfNames (ns: string[]) : GeneralizedTypars =
        GeneralizedTypars(Array.map (fun n -> n, TypeVar()) ns)
