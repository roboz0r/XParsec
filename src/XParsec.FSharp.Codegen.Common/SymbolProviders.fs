namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 FACTORY over the intrinsic axis composed from the layer-1 providers, which is
    /// what `type int = (# "System.Int32" #)` declares, both directions. A factory, not a
    /// fixed list, so that axis can seed it.
    type PlatformMetadataFactory = ReferencedProject.PlatformMetadataFactory

    /// A package's manifest for the compiling target, as resolved from its directory.
    type ManifestPath = ReferencedProject.ManifestPath

    /// A parsed package manifest.
    type Manifest = ReferencedProject.Manifest

    /// Dependency-ordered manifests, PARSED, and each package's transitive `depends-on` closure.
    let private orderedManifestsWithDeps
        (manifests: ManifestPath list)
        : Manifest list * (ManifestPath -> ManifestPath list) =
        match ReferencedProject.buildClosureWithDeps manifests with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// The package stack for a compilation that IS a package: its declared references, then the
    /// package's OWN directory last. That `.fsi` route is the only channel a prior file's
    /// `type int32 = int` reaches a later one through: freezing carries no abbreviation.
    let selfStack (selfPackage: string option) (packageDirs: string list) : string list =
        match selfPackage with
        | Some p -> packageDirs @ [ p ]
        | None -> packageDirs

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 FACTORY. Common
    /// names no platform; each backend injects its own reader. Uncached.
    let buildWith
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        ReferencedProject.composeContract platformMetadata (ReferencedProject.resolveAll target packageDirs)

    /// One pass over a manifest set's splice sources: the templates published, and the
    /// producer file each was declared in.
    type CollectedInlineBodies =
        {
            Bodies: InlineBodies.FileInlineBodies
            Origins: OriginSources
        }

    /// Load cross-package inline bodies from manifests' `impl` files, type-checked and
    /// frozen once against `provider`. Manifest/decl order, so a later body wins a clash.
    let inlineBodies (provider: IExternalSymbolProvider) (manifests: Manifest list) : CollectedInlineBodies =
        let acc = ResizeArray<InlineBodies.KeyedInlineBody>()
        let memberAcc = ResizeArray<InlineBodies.KeyedInlineBody>()
        let mutable origins = OriginSources.empty

        for manifest in manifests do
            for rel in manifest.Impl do
                let file = VesperLib.libFile manifest.Name manifest.Dir rel

                match VesperLib.parseFileFull file with
                | Result.Error _ -> ()
                | Result.Ok parsed ->
                    let origin = Hashing.originSource parsed.File parsed.Lexed

                    origins <- OriginSources.add origin origins

                    let implFile =
                        match parsed.Ast with
                        | FSharpAst.ImplementationFile f -> Some f
                        | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
                            Some(ImplementationFile.AnonymousModule elems)
                        | _ -> None

                    match implFile with
                    | None -> ()
                    | Some f ->
                        // `manifest.Name` is the home assembly the published keys are rooted
                        // at, the same one the package's own symbols are stamped with, so a
                        // served key and a resolved one agree.
                        let ctx, sem = Pipeline.analyseSemWithContextFor manifest.Name provider origin f

                        // Freezing an errored tree prunes the failed declarations; pooling then
                        // trips on side-table entries that outlived them, blaming the file's FIRST
                        // binding. Errors alone are not fatal: a contract file may name spare types.
                        let frozen =
                            try
                                Freeze.run ctx sem
                            with e ->
                                let pruned =
                                    match sem.Diagnostics |> Diagnostic.errors with
                                    | [] -> " (none — the fault is in the freeze itself)"
                                    | errors ->
                                        errors |> List.map (fun d -> "\n  " + Kind.message d.Kind) |> String.concat ""

                                failwithf
                                    "SymbolProviders: freezing package '%s' impl file '%s' failed: %s\nits analysis errors, which the freeze pruned:%s"
                                    manifest.Name
                                    rel
                                    e.Message
                                    pruned

                        let bodies = InlineBodies.collect origin frozen

                        acc.AddRange bodies.Values
                        memberAcc.AddRange bodies.Members

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
            /// compiled program's imports, keyed by package name.
            RuntimeAssets: Map<string, RuntimeAsset>
            Provider: IExternalSymbolProvider
            /// Simple name → body. NOT a resolution channel (the provider folds a body onto
            /// the entry that owns its key); the introspection seam tests assert against.
            BodiesByName: Map<string, InlineBody>
            /// The producer files the collected bodies were unpooled from, retained so their
            /// anchors stay readable. Re-parsing to recover them would give a second answer.
            Origins: OriginSources
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
            }

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
        let normalised = ReferencedProject.resolveAll target packageDirs

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
                        (let ordered, transitiveDeps = orderedManifestsWithDeps normalised

                         let provider =
                             ReferencedProject.composeOrdered platformMetadata ordered transitiveDeps

                         let collected = inlineBodies provider ordered

                         // A later body wins a clash (the list is in manifest/decl order).
                         let byName =
                             (Map.empty, collected.Bodies.Values)
                             ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                         let served =
                             provider
                             |> ExternalSymbolProviders.withInlineBodies (InlineBodies.index collected.Bodies)

                         {
                             RuntimeAssets = ReferencedProject.runtimeModules ordered
                             Provider = served
                             BodiesByName = byName
                             Origins = collected.Origins
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
