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
                Diagnostics = AssemblyFiles.setFaultDiagnostics fault
                InlineBodies = InlineBodies.empty
                Origins = OriginSources.empty
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
                Diagnostics = AssemblyFiles.setFaultDiagnostics fault
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
                             | Result.Error fault ->
                                 { Contract.empty with
                                     Diagnostics = AssemblyFiles.setFaultDiagnostics fault
                                 }
                             | Result.Ok(ordered, transitiveDeps) ->

                                 // Read once: the contracts resolve, and the bodies freeze, off
                                 // these same trees, inside one fold per package.
                                 let composed =
                                     PackageProviders.composeOrdered
                                         platformMetadata
                                         (ordered |> List.map PackageSource.readPackage)
                                         transitiveDeps

                                 // A later body wins a clash (the list is in manifest/decl order).
                                 let byName =
                                     (Map.empty, composed.InlineBodies.Values)
                                     ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                                 let served =
                                     composed.Provider
                                     |> ExternalSymbolProviders.withInlineBodies (
                                         InlineBodies.index composed.InlineBodies
                                     )

                                 {
                                     RuntimeAssets = ReferencedProject.runtimeModules ordered
                                     Provider = served
                                     BodiesByName = byName
                                     Origins = composed.Origins
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
