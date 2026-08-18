namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 FACTORY over the intrinsic axis composed from the layer-1 providers, which is
    /// what `type int = (# "System.Int32" #)` declares, both directions. A factory, not a
    /// fixed list, so that axis can seed it.
    type PlatformMetadataFactory = PackageProviders.PlatformMetadataFactory

    /// A package's manifest for the compiling target, as resolved from its directory.
    type ManifestPath = ReferencedProject.ManifestPath

    /// A parsed package manifest.
    type Manifest = ReferencedProject.Manifest

    /// A whole-set fault as an unpositioned diagnostic, there being no file to anchor it to.
    let private setFaultDiagnostics (fault: PackageSetFault) : AssemblyFiles.AnchoredDiagnostic list =
        AssemblyFiles.unpositionedDiagnostics AssemblyFileId.nowhere [ Diagnostic.nowhere (Kind.PackageSet fault) ]

    /// The package stack for a compilation that IS a package: its declared references, then the
    /// package's OWN directory last. That `.fsi` route is the only channel a prior file's
    /// `type int32 = int` reaches a later one through: freezing carries no abbreviation.
    let selfStack (selfPackage: string option) (packageDirs: string list) : string list =
        match selfPackage with
        | Some p -> packageDirs @ [ p ]
        | None -> packageDirs

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 FACTORY. Common
    /// is platform-neutral; each backend injects its own reader. Uncached.
    let buildWith
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : PackageProviders.ComposedContract =
        match ReferencedProject.resolveAll target packageDirs with
        | Result.Ok resolved -> PackageProviders.composeContract platformMetadata resolved
        | Result.Error fault ->
            {
                Provider = ExternalSymbolProviders.nullProvider
                Diagnostics = setFaultDiagnostics fault
            }

    /// One pass over a manifest set's splice sources: the templates published, and the
    /// producer file each was declared in.
    type CollectedInlineBodies =
        {
            Bodies: InlineBodies.FileInlineBodies
            Origins: OriginSources
        }

    /// Load cross-package inline bodies from the packages' implementation files as READ,
    /// type-checked and frozen once against `provider`. Manifest/decl order, so a later body
    /// wins a clash.
    let inlineBodies
        (target: string)
        (provider: IExternalSymbolProvider)
        (packages: PackageSource.ParsedPackage list)
        : CollectedInlineBodies =
        let acc = ResizeArray<InlineBodies.KeyedInlineBody>()
        let memberAcc = ResizeArray<InlineBodies.KeyedInlineBody>()
        let mutable origins = OriginSources.empty

        let collect
            (manifest: ReferencedProject.Manifest)
            (file: PackageSource.ReadFile<ParseChain.ParsedFile>)
            (impl: ParseChain.ParsedFile)
            =
            let origin =
                Hashing.originSource
                    {
                        BucketName = manifest.Name
                        Relative = file.Id
                    }
                    impl.Lexed

            origins <- OriginSources.add origin origins

            // `manifest.Name` is the home assembly the published keys are rooted at, the same
            // one the package's own symbols are stamped with, so a served key and a resolved
            // one agree.
            let ctx, sem =
                Pipeline.analyseSemWithContextFor
                    {
                        Name = manifest.Name
                        Target = target
                    }
                    provider
                    origin
                    impl.File

            // Freezing an errored tree prunes the failed declarations; pooling then
            // trips on side-table entries that outlived them, blaming the file's FIRST
            // binding. Errors alone are not fatal: an implementation file may reference spare types.
            let frozen =
                try
                    Freeze.run ctx sem
                with e ->
                    let pruned =
                        match sem.Diagnostics |> Diagnostic.errors with
                        | [] -> " (none, so the fault is in the freeze itself)"
                        | errors -> errors |> List.map (fun d -> "\n  " + Kind.message d.Kind) |> String.concat ""

                    failwithf
                        "internal error: freezing package '%s' impl file '%s' failed: %s\nits analysis errors, which the freeze pruned:%s"
                        manifest.Name
                        file.Relative
                        e.Message
                        pruned

            let bodies = InlineBodies.collect origin frozen

            acc.AddRange bodies.Values
            memberAcc.AddRange bodies.Members

        for pkg in packages do
            for entry in pkg.Implementations do
                match entry.Implementation.Outcome with
                | Ok parsed -> collect pkg.Manifest entry.Implementation parsed
                // A file the read could not deliver splices nothing. Its fault is reported by
                // the provider build, which reads the same package value.
                | Error _ -> ()

        {
            Bodies =
                {
                    Values = List.ofSeq acc
                    Members = List.ofSeq memberAcc
                }
            Origins = origins
        }

    /// One manifest set's composed contract. A backend takes the WHOLE value: a provider from
    /// one manifest set beside an anchor domain from another resolves a served body's position
    /// against a file that was never retained, and the wrong answer is in range.
    type Contract =
        {
            /// The `[core] runtime` assets of the whole `depends-on` closure, which back a
            /// compiled program's imports, keyed by package name and in manifest order.
            RuntimeAssets: Map<string, RuntimeAsset list>
            Provider: IExternalSymbolProvider
            /// Simple name → body. NOT a resolution channel (the provider folds a body onto
            /// the entry that owns its key); the introspection seam tests assert against.
            BodiesByName: Map<string, InlineBody>
            /// The producer files the collected bodies were unpooled from, retained so their
            /// anchors stay readable. Re-parsing to recover them would give a second answer.
            Origins: OriginSources
            /// What resolving the referenced contracts found: a manifest listing a file it has
            /// not got, a declaration a contract could not publish.
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
        }

    module Contract =

        /// The contract of the EMPTY manifest set: nothing resolves, nothing is served, nothing
        /// is anchored but the compiling file. A real value, so no consumer carries a
        /// "there is no contract" arm.
        let empty: Contract =
            {
                RuntimeAssets = Map.empty
                Provider = ExternalSymbolProviders.nullProvider
                BodiesByName = Map.empty
                Origins = OriginSources.empty
                Diagnostics = []
            }

        /// The contract, refused if resolving it failed. Ungated, a contract that publishes LESS
        /// than its `.fsi` files say surfaces as an unresolved name in the CONSUMING file, which
        /// blames the wrong file for it.
        let gate (contract: Contract) : Result<Contract, AssemblyFiles.AnchoredDiagnostic list> =
            match
                contract.Diagnostics
                |> List.filter (fun d -> d.Diagnostic.Severity = Severity.Error)
            with
            | [] -> Ok contract
            | errors -> Error errors

    /// Cache keyed by resolved manifest set + target + metadata tag.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Lazy<Contract>>(System.StringComparer.Ordinal)

    /// Cached contract for a package set, over a caller-supplied layer-2 FACTORY: the seam
    /// each backend wraps with its own platform reader.
    let buildContractWith
        (cacheTag: string)
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : Contract =
        match ReferencedProject.resolveAll target packageDirs with
        | Result.Error fault ->
            { Contract.empty with
                Diagnostics = setFaultDiagnostics fault
            }
        | Result.Ok normalised ->

            // The tag distinguishes each backend's collection of one package set: they freeze
            // different bodies over different platform metadata. The target is in the key in
            // its own right because an EMPTY set contributes no path that could carry it.
            let key =
                cacheTag
                + "|"
                + target
                + "|"
                + (normalised |> List.map (fun m -> m.Path) |> String.concat ";")

            contractCache
                .GetOrAdd(
                    key,
                    fun _ ->
                        lazy
                            (match ReferencedProject.buildClosureWithDeps normalised with
                             | Result.Error e ->
                                 { Contract.empty with
                                     Diagnostics = setFaultDiagnostics (PackageSetFault.UnresolvedDependency e)
                                 }
                             | Result.Ok(ordered, transitiveDeps) ->

                                 // Read once: the contracts below resolve, and the bodies freeze,
                                 // off these same trees.
                                 let packages = ordered |> List.map PackageSource.readPackage

                                 let composed =
                                     PackageProviders.composeOrdered platformMetadata packages transitiveDeps

                                 let collected = inlineBodies target composed.Provider packages

                                 // A later body wins a clash (the list is in manifest/decl order).
                                 let byName =
                                     (Map.empty, collected.Bodies.Values)
                                     ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                                 let served =
                                     composed.Provider
                                     |> ExternalSymbolProviders.withInlineBodies (InlineBodies.index collected.Bodies)

                                 {
                                     RuntimeAssets = ReferencedProject.runtimeModules ordered
                                     Provider = served
                                     BodiesByName = byName
                                     Origins = collected.Origins
                                     Diagnostics = composed.Diagnostics
                                 })
                )
                .Value

    /// `buildContractWith` over a FIXED provider list, wrapped as a constant factory: for a
    /// backend whose platform metadata reads nothing from the intrinsic axis.
    let buildContractWithMetadata
        (cacheTag: string)
        (platformMetadata: IExternalSymbolProvider list)
        (target: string)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        (buildContractWith cacheTag (fun _ -> platformMetadata) target packageDirs).Provider
