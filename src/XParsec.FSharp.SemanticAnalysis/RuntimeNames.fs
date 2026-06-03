namespace XParsec.FSharp.SemanticAnalysis

/// Single source of truth for the well-known runtime type identities that flow
/// through the pipeline. Before the
/// SymbolKey refactor these names + the arity-strip that recognises them were
/// duplicated across `ClrEnv.isVesperListName` (codegen), `FreezeExpr`'s list-
/// retarget (front end), and `RefCellPromotion` — each had independently re-derived
/// "is this the Vesper cons-list / the ref cell?". This module collapses that to
/// one place: the canonical `*Key` constants below, which the producers stamp and
/// the recognisers match.
///
/// Identity is the `SymbolKey`, not a string. Each singleton has exactly one
/// canonical key (the cons-list additionally has its lowercase abbreviation key —
/// the abbreviation name is load-bearing for contract extraction
/// — so it's the one type with two accepted nominal
/// forms). Recognition is asm-blind structural field comparison against those keys
/// (`sameTypeAsmBlind`): same namespace + same bare simple name, home assembly
/// ignored, no qualified-string rebuild. The former parallel fully-qualified string
/// constants + the `isVesperList` string recogniser are gone — the key constants are
/// now the sole representation.
///
/// Scope: this module owns ONLY the well-known
/// singleton key constants + the recognisers built over them. The generic
/// `SymbolKey` ↔ string projection / minting helpers (`bareName`, `simpleName`,
/// `qualifiedName`, `keyAsm`, `externalTypeKey`, …) are NOT runtime-name-specific,
/// so they live with `valueKeyOf` in `module ExternalSymbols`.
[<RequireQualifiedAccess>]
module RuntimeNames =

    // --- Canonical SymbolKey identities --------------------------------------------
    //
    // The well-known runtime singletons above flow through the pipeline as bare
    // string `SemType` names; Phase 5 (sub-step 4) puts a `SymbolKey` on the
    // nominal `SemType` cases, at which point these consumers compare *key*
    // identity instead of normalising strings (3b: `isVesperList name` becomes
    // `key = vesperListKey`). These constants are the one canonical key each
    // singleton's producers stamp and its consumers match — minted here, next to
    // the string forms, so the identity lives in exactly one place. Nothing reads
    // them yet; sub-step 4 wires the producers/consumers onto them.
    //
    // Shape conventions (so a key here equals the key the rest of the pipeline
    // mints for the same type):
    //   * `asm = Some <home>` — the type's **home assembly** (Phase 6), invariant
    //     per type. When the cons-list / ref cell are compiled *locally*
    //     (self-hosting `Vesper.List` / `Vesper.Core`) the stamped `info.Key`
    //     carries the same home (`PassContext.AssemblyName`), and a *consumer*
    //     resolves the cross-package reference to the same home via its
    //     `SymbolOrigin.Assembly` — so this one key recognises the local
    //     definition and the cross-package reference alike. The home is the
    //     assembly's *simple name* as referenced (`Vesper.List`, not the
    //     namespace `Vesper.Collections`).
    //   * `name` is the **arity-qualified** simple name (`` List`1 ``), following
    //     `SymbolKeyOps.arityName`.

    /// Canonical identity for the Vesper cons-list `List` union. Home assembly
    /// `Vesper.List` (the simple name the cons-list is referenced by — `ProjectInfo`
    /// `Vesper.List` owns `Vesper.Collections.List\`1`), arity-qualified `List`1` to
    /// match the locally compiled `UnionTypeInfo.Key` (`TypeKey(Some "Vesper.List",
    /// "Vesper.Collections", "List`1")`). The producers' canonical key.
    let vesperListKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.List", "Vesper.Collections", "List`1")

    /// The cons-list's lowercase `list` abbreviation (the `'T list` convention) —
    /// the cons-list's *second* accepted nominal form, sharing the union's namespace.
    /// Recogniser-only (no producer mints the abbreviation; `isVesperListKey` matches
    /// it alongside `vesperListKey`), hence `private`.
    let private vesperListAbbrevKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.List", "Vesper.Collections", "list")

    /// Canonical identity for FSharp.Core's `list` — the non-retargeted default
    /// `FreezeExpr` / `Unification` fall back to. Home `FSharp.Core`, arity 1
    /// (`` list`1 ``); never project-local.
    let fsharpCoreListKey: SymbolKey =
        SymbolKey.TypeKey(Some "FSharp.Core", "Microsoft.FSharp.Collections", "list`1")

    /// Canonical identity for the heap ref-cell record (`Ref<'T>`, arity 1 ⇒
    /// `` Ref`1 ``). Home `Vesper.Core`, matching the locally compiled
    /// `Vesper.Core` `RecordTypeInfo.Key`.
    let vesperRefKey: SymbolKey =
        SymbolKey.TypeKey(Some "Vesper.Core", "Vesper", "Ref`1")

    /// Canonical identity for `PrintfFormat<'Printer,'State,'Residue,'Result>`
    /// (arity 4 ⇒ `` PrintfFormat`4 ``) — the type a format literal freezes to
    /// (`PrintfSpec.printfFormatName`). Home `FSharp.Core`. The printf *entry
    /// points* are already key-based (`PrintfSpec.canonicalPrintfShortName`, a
    /// `ValueKey`); this is the format *type* identity.
    let printfFormatKey: SymbolKey =
        SymbolKey.TypeKey(Some "FSharp.Core", "Microsoft.FSharp.Core", "PrintfFormat`4")

    /// Canonical identity for the BCL `System.Object` — recognised at the unify
    /// boundary (an empty `TyClass` whose key denotes `System.Object` satisfies the
    /// equality/derives predicates). Non-generic. Recogniser-only (no producer mints
    /// it; `System.Object` arrives via external resolution), and the home assembly is
    /// a don't-care here (`isSystemObjectKey` is asm-blind), so `private`.
    let private systemObjectKey: SymbolKey =
        SymbolKey.TypeKey(Some "System.Runtime", "System", "Object")

    /// The canonical identity name for a rank-`rank` array, sourced from the
    /// `prim-types-min.fs` declaration `type 'T ``[]`` ` (rank 1 → `"[]"`;
    /// rank N → `"[" + (N-1) commas + "]"`, e.g. `"[,]"` for 2-D). Arrays are a
    /// generic intrinsic carried as `TyConst(arrayName rank, [elem])`
    /// — this single name replaces the former
    /// `"array"` / `"arrayN"` / `"Microsoft.FSharp.Core.[]"` triple-naming.
    let arrayName (rank: int) : string =
        if rank <= 1 then
            "[]"
        else
            "[" + System.String(',', rank - 1) + "]"

    // --- Well-known-singleton recognition by key (Phase 5.4) ---
    //
    // Recognition is asm-blind structural field comparison against the canonical
    // `*Key` constants above: same namespace + same bare (arity-stripped) simple
    // name, home assembly ignored. Asm-blind because a bare-named / origin-less mint
    // (a test helper, an asm-blind codegen path) carries no home assembly but still
    // denotes the singleton; allocation-free (no qualified-string rebuild) so it's
    // cheap on the hot unify / codegen paths. The canonical keys are the single
    // identity source — the former parallel FQ string constants + `isVesperList`
    // string recogniser are gone. (Where a *local* definition of
    // the same type must win — codegen's self-host cons-list — the caller checks the
    // project-local table first, then falls to these.)

    /// Asm-blind field match against a canonical `TypeKey`: same namespace and same
    /// bare (arity-stripped) simple name, home assembly ignored. Keys are always
    /// well-formed — every mint path (`externalTypeKey` / `qualifiedTypeKeyOf` /
    /// `LocalSymbolKey.ofType`) splits the namespace into `ns`, so the `name` segment
    /// never carries dots and one `bareName` strip suffices.
    let private sameTypeAsmBlind (canonical: SymbolKey) (k: SymbolKey) : bool =
        match canonical, k with
        | SymbolKey.TypeKey(_, cns, cn), SymbolKey.TypeKey(_, ns, n) ->
            cns = ns && SymbolKeyOps.bareName cn = SymbolKeyOps.bareName n
        | _ -> false

    /// True iff `k` denotes the Vesper cons-list in either of its nominal forms —
    /// the `List` union or its lowercase `list` abbreviation (both in
    /// `Vesper.Collections`).
    let isVesperListKey (k: SymbolKey) : bool =
        sameTypeAsmBlind vesperListKey k || sameTypeAsmBlind vesperListAbbrevKey k

    let isFsharpCoreListKey (k: SymbolKey) : bool = sameTypeAsmBlind fsharpCoreListKey k

    /// True iff `k` denotes the BCL `System.Object`. Asm-blind (the consumers — the
    /// unify equality/derives predicates — never compared the home assembly).
    let isSystemObjectKey (k: SymbolKey) : bool = sameTypeAsmBlind systemObjectKey k

    /// True iff `k` denotes `PrintfFormat<'Printer,'State,'Residue,'Result>` — the
    /// format type a `printf` / `sprintf` literal freezes to. Asm-blind, matching the
    /// list/object recognisers; replaces the inline `bareName (qualifiedName key) =
    /// PrintfSpec.printfFormatName` rebuild at the codegen / FreezeExpr consumer sites.
    let isPrintfFormatKey (k: SymbolKey) : bool = sameTypeAsmBlind printfFormatKey k
