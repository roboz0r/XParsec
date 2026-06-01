namespace XParsec.FSharp.SemanticAnalysis

/// Single source of truth for the well-known runtime type identities that flow
/// through the pipeline under more than one string form (symbol-key-refactor.md
/// Phase 3a). Before 3a these names + the arity-strip that recognises them were
/// duplicated across `ClrEnv.isVesperListName` (codegen), `FreezeExpr`'s list-
/// retarget (front end), and `RefCellPromotion` — each had independently re-derived
/// "is this string the Vesper cons-list / the ref cell?". This module collapses
/// that to one place.
///
/// The cons-list legitimately arrives in three forms, all of which must be
/// accepted: the bare union name `Vesper.Collections.List` (self-host
/// `'T list = List<'T>` expansion), the arity-suffixed compiled name
/// `Vesper.Collections.List`1` (the contract layer arity-suffixes generic
/// compiled names), and the lowercase abbreviation `Vesper.Collections.list`
/// (the `'T list` convention, FSharp.Core-style). These forms are irreducible —
/// the abbreviation name is load-bearing for contract extraction
/// (symbol-resolution-handoff.md) — so the normalisation here cannot be pushed
/// onto the producers; it lives in `isVesperList` and every consumer routes
/// through it. Phase 3b can swap this predicate's internals to `SymbolKey`
/// equality at this one site.
[<RequireQualifiedAccess>]
module RuntimeNames =

    /// The cons-list union's fully-qualified compiled name (no arity suffix).
    [<Literal>]
    let vesperListUnion = "Vesper.Collections.List"

    /// The cons-list's lowercase abbreviation name (the `'T list` convention).
    [<Literal>]
    let vesperListAbbrev = "Vesper.Collections.list"

    /// FSharp.Core's list — the nominal a non-retargeted literal / parameter
    /// renders as (the `Cons`/`Nil` default in `FreezeExpr`'s list lowering).
    [<Literal>]
    let fsharpCoreList = "Microsoft.FSharp.Collections.list"

    /// The heap ref-cell record's compiled name (no arity suffix). Lives in
    /// `Vesper.Core.dll`; `RefCellPromotion` wraps escaping mutables in it.
    [<Literal>]
    let vesperRef = "Vesper.Ref"

    /// Strip a trailing `` `N `` generic-arity suffix, returning the bare compiled
    /// name (namespace kept). The contract layer arity-suffixes generic compiled
    /// names, so a name may arrive as either `Vesper.Collections.List` or
    /// `Vesper.Collections.List`1`; recognition must accept both.
    let bareName (name: string) : string =
        let tick = name.IndexOf '`'
        if tick < 0 then name else name.Substring(0, tick)

    /// True iff `name` denotes the Vesper cons-list in any of its forms — the bare
    /// or arity-suffixed union name, or the lowercase abbreviation.
    let isVesperList (name: string) : bool =
        let bare = bareName name
        bare = vesperListUnion || bare = vesperListAbbrev
