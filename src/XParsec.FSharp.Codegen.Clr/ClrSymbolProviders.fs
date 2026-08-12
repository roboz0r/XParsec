namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// `Codegen.Common.SymbolProviders` composes a contract stack over an injected leaf
/// FACTORY and holds no concrete leaf; this module supplies the .NET reflection tail.
module ClrSymbolProviders =

    /// BCL reflection over the host runtime. A seeded leaf is a pure function of
    /// `(reverseCanon, paths)` and the paths are constant here, so memoising on the reverse
    /// map gives one `MetadataLoadContext` per distinct map rather than one per firing.
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

    /// `bclMetaTail` over an EXPLICIT reference set (a TFM ref pack + `<Reference>`s) instead
    /// of the host TPA. Each call returns a FRESH factory with its own memo, because
    /// `bclMetaTail`'s process-wide one keys on the reverse map alone and so assumes the paths.
    let bclMetaTailWith (dllPaths: string list) : SymbolProviders.MetaTailFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<Map<string, SymbolKey list>, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        // `createWith Map.empty` IS `create`, so the empty-reverse case needs no branch.
        fun reverseCanon -> seeded.GetOrAdd(reverseCanon, fun rc -> [ MetadataSymbols.createWith rc dllPaths ])

    /// Content-derived cache tag for an explicit reference set: without it two compilations
    /// with different ref sets but identical manifests would share a contract entry.
    /// ORDER-PRESERVING because resolution scans paths in order, so do not sort.
    let private refsCacheTag (dllPaths: string list) : string =
        "bcl-refs:"
        + (dllPaths |> List.map System.IO.Path.GetFullPath |> String.concat ";")

    /// Layer-1 contract stack over the BCL metadata leaf. Uncached.
    let build (packageDirs: string list) : IExternalSymbolProvider =
        SymbolProviders.buildWith bclMetaTail Target.Clr packageDirs

    /// Provider stack for a package set (BCL leaf), including cross-package inline bodies.
    let buildContract (packageDirs: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail Target.Clr packageDirs).Provider

    /// `buildContract` for another backend's collection of the same packages: an INTROSPECTION
    /// seam, how a CLR-side test reads what the JS contract makes of a package. A compilation
    /// never calls this; its target is the one its own backend resolves under.
    let buildContractFor (target: string) (packageDirs: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail target packageDirs).Provider

    /// The `{ platform-repr -> [canon] }` axis of the package being compiled, as a consumer
    /// of it would see it, because it is read off that package's own composed contract sources.
    let selfReverseCanon (selfPackage: string option) : Map<string, SymbolKey list> =
        match selfPackage with
        | None -> Map.empty
        | Some dir -> (buildContract [ dir ]).IntrinsicReverseCanon

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

    /// `buildContract` for a compilation that IS a package, over the host TPA rather than
    /// an explicit reference set. `selfPackage` seeds the leaf AND joins the resolution stack.
    let buildContractForSelf (selfPackage: string option) (packageDirs: string list) : IExternalSymbolProvider =
        let seed = selfReverseCanon selfPackage

        (SymbolProviders.buildContractWith
            ("bcl" + seedTag seed)
            (seeded seed bclMetaTail)
            Target.Clr
            (SymbolProviders.selfStack selfPackage packageDirs))
            .Provider

    /// An introspection seam for tests: raw cross-package inline bodies by source name. A
    /// simple name is not a resolution channel; production reads a body off its resolved entry.
    let contractInlineBodies (packageDirs: string list) : Map<string, InlineBody> =
        (SymbolProviders.buildContractWith "bcl" bclMetaTail Target.Clr packageDirs).BodiesByName

    /// The cached contract for one compilation: an explicit reference set, seeded with the
    /// compiling package's own reverse axis. Both halves are in the cache tag, the seed included,
    /// because two packages compiling THEMSELVES with empty package lists would otherwise alias.
    let private compilationContract
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : SymbolProviders.Contract =
        let seed = selfReverseCanon selfPackage

        SymbolProviders.buildContractWith
            (refsCacheTag dllPaths + seedTag seed)
            (seeded seed (bclMetaTailWith dllPaths))
            Target.Clr
            (SymbolProviders.selfStack selfPackage packageDirs)

    /// `buildContract` over a compilation's own reference set and self package.
    /// `selfPackage` is `None` for a compilation that declares no primitives of its own.
    let buildContractWithRefs
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        (compilationContract selfPackage dllPaths packageDirs).Provider

    /// The inline-body half of the same cached contract `buildContractWithRefs` takes the
    /// provider of, so a driver needing both makes two calls and one build.
    let contractInlineBodiesWithRefs
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : Map<string, InlineBody> =
        (compilationContract selfPackage dllPaths packageDirs).BodiesByName
