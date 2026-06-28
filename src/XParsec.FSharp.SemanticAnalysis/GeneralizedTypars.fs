namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

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
/// single source, because F# treats this order as an ABI surface (see
/// `docs/typar-ordering-unification-plan.md`).
type GeneralizedTypars = private GeneralizedTypars of (string * TypeVar)[]

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
    let canonical
        (declared: (string * TypeVar) list)
        (fixedRoots: HashSet<TypeVar>)
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
        //    order. A local pre-order fold over `SemType` — deliberately a copy of
        //    `iterTypeVarRoots`'s skeleton, since this file must not depend on the
        //    Unification pass where that helper lives.
        let mutable inferredCount = 0

        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if root.Link.IsNone && not (fixedRoots.Contains root) && seen.Add root then
                    // Synthetic method-scoped name; can't collide with class typars.
                    result.Add(sprintf "M%d" inferredCount, root)
                    inferredCount <- inferredCount + 1
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

        walk zonkedTy
        GeneralizedTypars(result.ToArray())

    /// The ONE position→index materialization: `roots.[i]`'s `TypeVar` ↦
    /// `TyTypar(TyparAxis.Method, i)`. Callers use this to rewrite a body's typar
    /// identities to frozen method markers.
    let methodEnv (GeneralizedTypars roots) : (TypeVar * SemType) list =
        [ for i in 0 .. roots.Length - 1 -> (snd roots.[i], TyTypar(TyparAxis.Method, i)) ]

    let toArray (GeneralizedTypars roots) : (string * TypeVar)[] = roots

    let names (GeneralizedTypars roots) : string[] = Array.map fst roots

    let count (GeneralizedTypars roots) : int = roots.Length

    /// The zero-typar value (a non-generic value/member).
    let empty: GeneralizedTypars = GeneralizedTypars [||]
