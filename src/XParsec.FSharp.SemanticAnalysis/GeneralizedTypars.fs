namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

/// THE one SemType structural pre-order skeleton, plus the link-following root
/// collector built on it. Lives here (compile-unit #14, after `UnionFind` and
/// before every consumer — `Inline`, `InferGeneralize`, `Elaborate`) so the
/// four historically-duplicated "first-appearance typar" walks share a single
/// traversal. Free of any Unification-pass dependency: needs only `SemType` /
/// `TypeVar` / `UnionFind`, all earlier compile units.
module SemTypeWalk =

    /// THE one SemType structural pre-order skeleton: invoke `onVar` at every
    /// `TyVar` leaf with the RAW (un-`find`ed) typar — each caller's leaf decides
    /// its own `UnionFind.find` / `Link` / dedup policy. A `TyVar` is a leaf: this
    /// does NOT recurse past it (a link-following caller re-enters via `onVar`).
    /// `TyTypar` / `TyUnknown` are no-ops; all composite forms recurse into their
    /// args left-to-right.
    let iterSemTypeVars (onVar: TypeVar -> unit) (t: SemType) : unit =
        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv -> onVar tv
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
            | TyOr members ->
                for m in members.Members do
                    walk m
            | TyUnknown _ -> ()
            | TyTypar _ -> ()
            // A nominal enum / a structural literal has no args and no typars — a
            // leaf, like `TyTypar`.
            | TyEnum _
            | TyLiteral _ -> ()

        walk t

    /// Append every still-free root reachable from `t` into `acc` (deduped by
    /// `seen`), FOLLOWING links: a `Link`ed root is a measure / pinned carrier, not
    /// a typar, so recurse into its target instead of collecting it. Built on
    /// `iterSemTypeVars`. Shared by `Inline.quantifiedTypars` and
    /// `Elaborate.mkMethodQuantEnv`'s dependent-typar fixpoint.
    let collectLinkedRoots (acc: ResizeArray<TypeVar>) (seen: HashSet<TypeVar>) (t: SemType) : unit =
        let rec onVar (tv: TypeVar) =
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> iterSemTypeVars onVar target
            | ValueNone ->
                if seen.Add root then
                    acc.Add root

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
type GeneralizedTypars = private | GeneralizedTypars of (string * TypeVar)[]

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
    /// `.Link`), mirroring `InferGeneralize.iterTypeVarRoots` /
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
        (declared: (string * TypeVar) list)
        (fixedRoots: HashSet<TypeVar>)
        (knownNames: IReadOnlyDictionary<TypeVar, string>)
        (zonkedTy: SemType)
        : GeneralizedTypars =
        let result = ResizeArray<string * TypeVar>()
        // Reference identity: roots are compared / deduped / excluded by union-find
        // representative, never by structural equality.
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        // 1. Declared typars first, in given order, by their union-find roots.
        for (name, tv) in declared do
            let root = UnionFind.find tv

            if seen.Add root then
                result.Add(name, root)

        // 2. Remaining free roots of `zonkedTy` in first-left-to-right-appearance
        //    order — the shared `SemTypeWalk.iterSemTypeVars` skeleton with this
        //    pass's leaf policy.
        let mutable inferredCount = 0

        zonkedTy
        |> SemTypeWalk.iterSemTypeVars (fun tv ->
            let root = UnionFind.find tv

            if root.Link.IsNone && not (fixedRoots.Contains root) && seen.Add root then
                // Prefer the registered source name (a real `'a` F# keeps in the
                // emitted GenericParam); synthesise a method-scoped `M%d` only for a
                // genuinely body-inferred root, bumping the index only when minted so
                // synthetic indices stay dense.
                match knownNames.TryGetValue root with
                | true, n -> result.Add(n, root)
                | _ ->
                    result.Add(sprintf "M%d" inferredCount, root)
                    inferredCount <- inferredCount + 1
        )

        GeneralizedTypars(result.ToArray())

    /// The ONE position→index materialization: `roots.[i]`'s `TypeVar` ↦
    /// `TyTypar(TyparAxis.Method, i)`. Callers use this to rewrite a body's typar
    /// identities to frozen method markers.
    let methodEnv (GeneralizedTypars roots) : (TypeVar * SemType) list =
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
    let refreshRoots (f: TypeVar -> TypeVar voption) (GeneralizedTypars roots) : GeneralizedTypars =
        GeneralizedTypars(
            roots
            |> Array.choose (fun (n, tv) ->
                match f tv with
                | ValueSome r -> Some(n, r)
                | ValueNone -> None
            )
        )

    let toArray (GeneralizedTypars roots) : (string * TypeVar)[] = roots

    let names (GeneralizedTypars roots) : string[] = Array.map fst roots

    let count (GeneralizedTypars roots) : int = roots.Length

    /// The zero-typar value (a non-generic value/member).
    let empty: GeneralizedTypars = GeneralizedTypars [||]
