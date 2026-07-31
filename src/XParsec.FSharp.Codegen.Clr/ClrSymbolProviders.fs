namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// The BCL-defaulting symbol-provider conveniences: `Codegen.Common.SymbolProviders`
/// composes a layer-1 contract stack over an injected layer-2 leaf FACTORY but names
/// no concrete leaf; this module supplies the .NET (`MetadataSymbols`) tail. A
/// `System.Reflection`-backed leaf is .NET-specific, so it lives in `Codegen.Clr` —
/// the JS backend injects its own tail (`JsNativeSymbols`) and never sees this.
module ClrSymbolProviders =

    /// BCL reflection over the host runtime. The factory is invoked once per package
    /// extraction AND once per final composite (see `composeProviders`), so for a
    /// K-package closure it fires ~K times — and every firing whose deps include
    /// Vesper.Core gets the SAME 57-entry reverse map over the SAME runtime paths.
    /// A seeded leaf is a pure function of `(reverseCanon, paths)`, so we memoise by
    /// reverse-map content: the whole process builds one `MetadataLoadContext` per
    /// distinct reverse map (in practice: Core's), not one per package per manifest
    /// set. The empty-reverse case reuses the shared singleton.
    let bclMetaTail: SymbolProviders.MetaTailFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<Map<string, SymbolKey list>, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        fun reverseCanon ->
            if reverseCanon.IsEmpty then
                [ MetadataSymbols.provider ]
            else
                seeded.GetOrAdd(
                    reverseCanon,
                    fun rc -> [ MetadataSymbols.createWith rc (MetadataSymbols.runtimeAssemblyPaths ()) ]
                )

    /// `bclMetaTail` over an EXPLICIT reference set rather than the host TPA
    /// (`runtimeAssemblyPaths ()`) — a compilation's own BCL surface (a TFM ref pack
    /// + `<Reference>`s). Each call returns a FRESH factory with its OWN per-instance
    /// memo keyed on the reverse map: a driver builds one factory per compilation and
    /// the factory fires ~K times over a K-package closure with (mostly) the same
    /// reverse map, so per-instance memoisation still collapses that to one
    /// `MetadataLoadContext`. The global `seeded` memo of `bclMetaTail` keys only on
    /// the reverse map (sound only because the host TPA is constant), so a path-taking
    /// leaf must NOT route through it. The empty-reverse case builds over the SAME
    /// explicit paths (memoised per-instance) — NOT the host-TPA singleton
    /// `MetadataSymbols.provider`, whose fallback is correct only for `bclMetaTail`.
    let bclMetaTailWith (dllPaths: string list) : SymbolProviders.MetaTailFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<Map<string, SymbolKey list>, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        // `createWith Map.empty` IS `create`, so the empty-reverse case needs no branch.
        fun reverseCanon -> seeded.GetOrAdd(reverseCanon, fun rc -> [ MetadataSymbols.createWith rc dllPaths ])

    /// Content-derived cache tag for an explicit reference set. `buildContractCached`
    /// keys on `cacheTag|target|manifests`, so two compilations with different ref
    /// sets but identical manifests would alias unless the path set enters the key.
    /// ORDER-PRESERVING (resolution scans paths in order — do not sort) and normalised.
    let private refsCacheTag (dllPaths: string list) : string =
        "bcl-refs:"
        + (dllPaths |> List.map System.IO.Path.GetFullPath |> String.concat ";")

    /// Layer-1 contract stack over the BCL metadata leaf. Uncached.
    let build (manifestPaths: string list) : IExternalSymbolProvider =
        SymbolProviders.buildWith bclMetaTail manifestPaths

    /// Provider stack for a manifest set (BCL leaf), including cross-package inline bodies.
    let buildContract (manifestPaths: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail None manifestPaths).Provider

    /// `buildContract` for a specific target (`Some "js"` selects `inline-bodies-js`
    /// overrides). `None` is identical to `buildContract`.
    let buildContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail target manifestPaths).Provider

    /// The compiling package's own intrinsic reverse axis
    /// The reverse `{ platform-repr -> [canon] }` axis of the package being compiled, as
    /// a consumer of it would see it. Read off that package's own composed contract
    /// sources.
    let selfReverseCanon (target: string option) (selfManifest: string option) : Map<string, SymbolKey list> =
        match selfManifest with
        | None -> Map.empty
        | Some manifestPath -> (buildContractFor target [ manifestPath ]).IntrinsicReverseCanon

    let private seeded
        (seed: Map<string, SymbolKey list>)
        (tail: SymbolProviders.MetaTailFactory)
        : SymbolProviders.MetaTailFactory =
        if seed.IsEmpty then
            tail
        else
            fun referenced ->
                (referenced, seed)
                ||> Map.fold (fun acc platform canons ->
                    match Map.tryFind platform acc with
                    | Some existing -> Map.add platform (canons @ existing |> List.distinct) acc
                    | None -> Map.add platform canons acc
                )
                |> tail

    let private seedTag (seed: Map<string, SymbolKey list>) : string =
        if seed.IsEmpty then
            ""
        else
            "|self:"
            + (seed
               |> Map.toList
               |> List.map (fun (platform, canons) ->
                   platform
                   + "="
                   + (canons |> List.map SymbolKeyOps.qualifiedName |> String.concat ",")
               )
               |> String.concat ";")

    /// `buildContractFor` for a compilation that IS a package — the host-TPA mirror of
    /// `buildContractWithRefs`, for a caller with no explicit reference set (the package
    /// fixtures). `selfManifest` seeds the leaf; it is NOT added to the resolution stack,
    /// which would declare every one of its types twice.
    let buildContractForSelf
        (selfManifest: string option)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        let seed = selfReverseCanon target selfManifest

        (SymbolProviders.buildContractWith ("bcl" + seedTag seed) (seeded seed bclMetaTail) target manifestPaths)
            .Provider

    /// Raw cross-package inline bodies by source name — introspection seam for tests.
    /// Production code reads a body off the resolved entry that owns its key
    /// (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`); a simple name is not a
    /// resolution channel.
    let contractInlineBodies (manifestPaths: string list) : Map<string, InlineBody> =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail None manifestPaths).BodiesByName

    /// `contractInlineBodies` for a specific target — introspection seam for target tests.
    let contractInlineBodiesFor (target: string option) (manifestPaths: string list) : Map<string, InlineBody> =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail target manifestPaths).BodiesByName

    /// The cached contract for one compilation: an EXPLICIT reference set (a
    /// per-compilation BCL surface — a TFM ref pack + `<Reference>`s — not the host TPA),
    /// seeded with the compiling package's own intrinsic reverse axis.
    ///
    /// Both halves of the identity are in the tag: the ref path set (`refsCacheTag`), so
    /// two compilations with different ref sets but identical manifests never alias, and
    /// the seed (`seedTag`), so two packages compiling THEMSELVES — each with an empty
    /// manifest list — never alias either.
    let private compilationContract
        (selfManifest: string option)
        (dllPaths: string list)
        (target: string option)
        (manifestPaths: string list)
        : SymbolProviders.Contract =
        let seed = selfReverseCanon target selfManifest

        SymbolProviders.buildContractWith
            (refsCacheTag dllPaths + seedTag seed)
            (seeded seed (bclMetaTailWith dllPaths))
            target
            manifestPaths

    /// `buildContractFor` over a compilation's own reference set and self package.
    /// `selfManifest` is `None` for a consumer — every compilation that does not itself
    /// declare primitives.
    let buildContractWithRefs
        (selfManifest: string option)
        (dllPaths: string list)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        (compilationContract selfManifest dllPaths target manifestPaths).Provider

    /// The inline-body half of the SAME cached `buildContractWith` call
    /// `buildContractWithRefs` takes the provider of: a driver needs the provider AND the
    /// bodies for one compilation, and the shared cache tag means the second call hits the
    /// built entry rather than rebuilding.
    let contractInlineBodiesWithRefs
        (selfManifest: string option)
        (dllPaths: string list)
        (target: string option)
        (manifestPaths: string list)
        : Map<string, InlineBody> =
        (compilationContract selfManifest dllPaths target manifestPaths).BodiesByName
