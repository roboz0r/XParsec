namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// `Codegen.Common.SymbolProviders` composes a contract stack over an injected leaf
/// FACTORY and holds no concrete leaf; this module supplies the .NET reflection tail.
module ClrSymbolProviders =

    /// BCL reflection over the host runtime. A seeded leaf is a pure function of
    /// `(intrinsics, paths)` and the paths are constant here, so memoising on the axis gives
    /// one `MetadataLoadContext` per distinct axis rather than one per firing.
    let bclMetaTail: SymbolProviders.MetaTailFactory =
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

    /// `bclMetaTail` over an EXPLICIT reference set (a TFM ref pack + `<Reference>`s) instead
    /// of the host TPA. Each call returns a FRESH factory with its own memo, because
    /// `bclMetaTail`'s process-wide one keys on the axis alone and so assumes the paths.
    let bclMetaTailWith (dllPaths: string list) : SymbolProviders.MetaTailFactory =
        let seeded =
            System.Collections.Concurrent.ConcurrentDictionary<IntrinsicTypeMap, IExternalSymbolProvider list>(
                HashIdentity.Structural
            )

        // `createWith` over an empty axis IS `create`, so the empty case needs no branch.
        fun intrinsics -> seeded.GetOrAdd(intrinsics, fun m -> [ MetadataSymbols.createWith m dllPaths ])

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

    /// The intrinsic axis of the package being compiled, as a consumer of it would see it,
    /// because it is read off that package's own composed contract sources.
    let selfIntrinsics (selfPackage: string option) : IntrinsicTypeMap =
        match selfPackage with
        | None -> IntrinsicTypeMap.empty
        | Some dir -> (buildContract [ dir ]).IntrinsicTypeMap

    /// The compiling package's own declarations SHADOW the referenced ones the leaf is
    /// otherwise seeded with: a package declaring `string` is served its own, not a dependency's.
    let private seeded
        (seed: IntrinsicTypeMap)
        (tail: SymbolProviders.MetaTailFactory)
        : SymbolProviders.MetaTailFactory =
        if IntrinsicTypeMap.isEmpty seed then
            tail
        else
            fun referenced -> tail (IntrinsicTypeMap.shadow seed referenced)

    let private seedTag (seed: IntrinsicTypeMap) : string =
        if IntrinsicTypeMap.isEmpty seed then
            ""
        else
            "|self:" + IntrinsicTypeMap.cacheTag seed

    /// `buildContract` for a compilation that IS a package, over the host TPA rather than
    /// an explicit reference set. `selfPackage` seeds the leaf AND joins the resolution stack.
    let buildContractForSelf (selfPackage: string option) (packageDirs: string list) : IExternalSymbolProvider =
        let seed = selfIntrinsics selfPackage

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
    /// compiling package's own intrinsic axis. Both halves are in the cache tag, the seed included,
    /// because two packages compiling THEMSELVES with empty package lists would otherwise alias.
    let private compilationContract
        (selfPackage: string option)
        (dllPaths: string list)
        (packageDirs: string list)
        : SymbolProviders.Contract =
        let seed = selfIntrinsics selfPackage

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
