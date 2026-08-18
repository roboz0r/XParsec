namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// The .NET reflection reader that fills `Codegen.Common.SymbolProviders`'s injected
/// layer-2 seam.
module ClrSymbolProviders =

    /// Reflection over the host runtime's assemblies. A seeded reader is a pure function of
    /// `(intrinsics, paths)` and the paths are constant here, so memoising on the axis gives
    /// one `MetadataLoadContext` per distinct axis rather than one per firing.
    let dotnetMetadata: SymbolProviders.PlatformMetadataFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<IntrinsicTypeMap, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        fun intrinsics ->
            if IntrinsicTypeMap.isEmpty intrinsics then
                [ MetadataSymbols.provider ]
            else
                seeded.GetOrAdd(
                    intrinsics,
                    fun m -> [ MetadataSymbols.createWith m (MetadataSymbols.runtimeAssemblyPaths ()) ]
                )

    /// `dotnetMetadata` over an EXPLICIT reference set (a TFM ref pack + `<Reference>`s)
    /// instead of the host TPA. Each call returns a FRESH factory with its own memo, because
    /// `dotnetMetadata`'s process-wide one keys on the axis alone and so assumes the paths.
    let dotnetMetadataWith (dllPaths: string list) : SymbolProviders.PlatformMetadataFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<IntrinsicTypeMap, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        // `createWith` over an empty axis IS `create`, so the empty case needs no branch.
        fun intrinsics -> seeded.GetOrAdd(intrinsics, fun m -> [ MetadataSymbols.createWith m dllPaths ])

    /// One package read at most once per contract build: the seed compose and the stack
    /// compose share one set of trees, and a `preloaded` package's trees are served over a
    /// read of its directory.
    let private memoisedRead
        (preloaded: PackageSource.ParsedPackage list)
        : SymbolProviders.Manifest -> PackageSource.ParsedPackage =
        let cache =
            System.Collections.Generic.Dictionary<SymbolProviders.ManifestPath, PackageSource.ParsedPackage>(
                HashIdentity.Structural
            )

        for p in preloaded do
            cache[p.Manifest.Path] <- p

        fun m ->
            match cache.TryGetValue m.Path with
            | true, p -> p
            | _ ->
                let p = PackageSource.readPackage m
                cache[m.Path] <- p
                p

    /// Provider stack for a package set, including cross-package inline bodies.
    let buildContract (packageDirs: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContract dotnetMetadata Target.Clr packageDirs).Provider

    /// `buildContract` for another backend's collection of the same packages: an INTROSPECTION
    /// seam, how a CLR-side test reads what the JS contract makes of a package. A compilation
    /// never calls this; its target is the one its own backend resolves under.
    let buildContractFor (target: string) (packageDirs: string list) : IExternalSymbolProvider =
        (SymbolProviders.buildContract dotnetMetadata target packageDirs).Provider

    /// The intrinsic axis of the package being compiled, as a consumer of it would see it,
    /// because it is read off that package's own composed contract sources.
    let private selfSeed
        (readPackage: SymbolProviders.Manifest -> PackageSource.ParsedPackage)
        (selfPackage: string option)
        : IntrinsicTypeMap =
        match selfPackage with
        | None -> IntrinsicTypeMap.empty
        | Some dir ->
            (SymbolProviders.buildContractWith readPackage dotnetMetadata Target.Clr [ dir ]).Provider.IntrinsicTypeMap

    /// `selfSeed` as an introspection seam for tests, reading from disk.
    let selfIntrinsics (selfPackage: string option) : IntrinsicTypeMap = selfSeed (memoisedRead []) selfPackage

    /// The compiling package's own declarations SHADOW the referenced ones the reader is
    /// otherwise seeded with: a package declaring `string` is served its own, not a dependency's.
    let private seeded
        (seed: IntrinsicTypeMap)
        (platformMetadata: SymbolProviders.PlatformMetadataFactory)
        : SymbolProviders.PlatformMetadataFactory =
        if IntrinsicTypeMap.isEmpty seed then
            platformMetadata
        else
            fun referenced -> platformMetadata (IntrinsicTypeMap.shadow seed referenced)

    /// The contract for one compilation: `selfPackage` seeds the reader AND joins the
    /// resolution stack, and `platformMetadata` is the compilation's own layer 2. The seed
    /// axis always reads over the host TPA, whatever layer 2 the bodies get.
    let private selfContractWith
        (readPackage: SymbolProviders.Manifest -> PackageSource.ParsedPackage)
        (platformMetadata: SymbolProviders.PlatformMetadataFactory)
        (selfPackage: string option)
        (packageDirs: string list)
        : PackageProviders.AnalyzedManifest =
        SymbolProviders.buildContractWith
            readPackage
            (seeded (selfSeed readPackage selfPackage) platformMetadata)
            Target.Clr
            (SymbolProviders.selfStack selfPackage packageDirs)

    /// `buildContract` for a compilation that IS a package, over the host TPA rather than
    /// an explicit reference set.
    let contractForSelf (selfPackage: string option) (packageDirs: string list) : PackageProviders.AnalyzedManifest =
        selfContractWith (memoisedRead []) dotnetMetadata selfPackage packageDirs

    /// `contractForSelf` for a caller that already READ the self package: `self`'s trees back
    /// both the seed axis and the contract stack, so it serves exactly the trees it was given.
    let contractForSelfParsed
        (selfPackageDir: string)
        (packageDirs: string list)
        (self: PackageSource.ParsedPackage)
        : PackageProviders.AnalyzedManifest =
        selfContractWith (memoisedRead [ self ]) dotnetMetadata (Some selfPackageDir) packageDirs

    /// `contractForSelf`'s provider with its diagnostics DROPPED: an introspection seam for a
    /// test that reads what the stack resolves and compiles nothing.
    let buildContractForSelf (selfPackage: string option) (packageDirs: string list) : IExternalSymbolProvider =
        (contractForSelf selfPackage packageDirs).Provider

    /// An introspection seam for tests: raw cross-package inline bodies by source name. A
    /// simple name is not a resolution channel; production reads a body off its resolved entry.
    let contractInlineBodies (packageDirs: string list) : Map<string, InlineBody> =
        (SymbolProviders.buildContract dotnetMetadata Target.Clr packageDirs).InlineBodies
        |> InlineBodies.valuesByName

    /// The contract for one compilation: an explicit reference set, seeded with the
    /// compiling package's own intrinsic axis.
    let compilationContract
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : PackageProviders.AnalyzedManifest =
        selfContractWith (memoisedRead []) (dotnetMetadataWith dllPaths) selfPackage packageDirs

    /// `compilationContract`'s provider alone.
    /// `selfPackage` is `None` for a compilation that declares no primitives of its own.
    let buildContractWithRefs
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        (compilationContract selfPackage dllPaths packageDirs).Provider
