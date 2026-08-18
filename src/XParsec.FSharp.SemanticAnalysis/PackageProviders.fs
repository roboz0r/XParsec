namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// Layer 1 of the symbol-resolution stack: each referenced package's `[core] files` signature
/// files RESOLVED one at a time, through the front end an in-assembly `.fsi` goes through, and
/// composed into one provider. A symbol's namespace is its FILE's `namespace` header.
module PackageProviders =

    /// One built package. `DeclaredTypeNames` are the qualified compiled names of the NOMINAL
    /// types it OWNS: those that would first-hit-shadow a peer package's same-named type.
    /// `InlineBodies` are its splice templates in manifest/decl order, anchored in the
    /// producer files `Origins` retains.
    [<NoEquality; NoComparison>]
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
            HomeAssembly: string
            DeclaredTypeNames: string list
            InlineBodies: InlineBodies.FileInlineBodies
            Origins: OriginSources
        }

    /// The nominal types a surface declares, for the composition-time duplicate sweep. Every
    /// package's `int` is THE `int`, so intrinsics and capability interfaces repeat
    /// legitimately and are excluded.
    let private declaredTypeNames (surface: PublishedSurface) : string list =
        [
            for entry in surface.ShapesByKey do
                match entry.Value with
                | ExternalTypeShape.Class _
                | ExternalTypeShape.Record _
                | ExternalTypeShape.Union _
                | ExternalTypeShape.Enum _
                | ExternalTypeShape.Abbrev _
                | ExternalTypeShape.Unmodelled _ -> yield SymbolKeyOps.typeMetaName entry.Key
                | ExternalTypeShape.Intrinsic _
                | ExternalTypeShape.IntrinsicInterface _ -> ()
        ]

    /// Analyse+freeze one package implementation file. A freeze fault here is an internal
    /// error, rethrown naming the package and file with the analysis errors the freeze pruned,
    /// because those are usually its cause.
    let private analysePackageFile: AssemblyFiles.AnalyseFile =
        fun assembly provider origin file ->
            let ctx, sem = Pipeline.analyseSemWithContextFor assembly provider origin file

            try
                Freeze.run ctx sem
            with e ->
                let pruned =
                    match sem.Diagnostics |> Diagnostic.errors with
                    | [] -> " (none, so the fault is in the freeze itself)"
                    | errors -> errors |> List.map (fun d -> "\n  " + Kind.message d.Kind) |> String.concat ""

                failwithf
                    "internal error: freezing package '%s' impl file '%s' failed: %s\nits analysis errors, which the freeze pruned:%s"
                    assembly.Name
                    origin.File.Path.Relative.Name
                    e.Message
                    pruned

    /// Fold the package's units in manifest order against its dependency providers, nearest
    /// first — the same fold an assembly's units go through. Every `.fsi` publishes its
    /// declarations, a `.fs` without one publishes the surface its analysis infers, and a file
    /// the read could not deliver yields a diagnostic and contributes nothing.
    /// `platformMetadata` seeds the signatures with the dependencies' intrinsic axis and each
    /// BODY with the axis published before it as well, the way its own compile presents its
    /// primitives to itself.
    let buildProviderSeeded
        (platformMetadata: IntrinsicTypeMap -> IExternalSymbolProvider list)
        (depProviders: IExternalSymbolProvider list)
        (pkg: PackageSource.ParsedPackage)
        : BuiltPackage =
        let manifest = pkg.Manifest

        let depIntrinsics = ExternalSymbolProviders.mergeIntrinsics depProviders

        let bodyExternal (ownIntrinsics: IntrinsicTypeMap) =
            ExternalSymbolProviders.composite (
                depProviders
                @ platformMetadata (
                    IntrinsicTypeMap.ofSeq (
                        Seq.append (IntrinsicTypeMap.entries depIntrinsics) (IntrinsicTypeMap.entries ownIntrinsics)
                    )
                )
            )

        let folded =
            AssemblyFiles.foldUnits
                analysePackageFile
                {
                    Name = manifest.Name
                    Target = manifest.Target
                }
                (ExternalSymbolProviders.composite (depProviders @ platformMetadata depIntrinsics))
                (AssemblyFiles.Publication.AcrossAssemblies bodyExternal)
                pkg.Units

        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()
        let surfaces = ResizeArray<PublishedSurface>()
        let published = ResizeArray<IExternalSymbolProvider>()
        let analysed = ResizeArray<AssemblyFiles.AnalysedUnit>()

        for unit in folded do
            match unit with
            | AssemblyFiles.FoldedUnit.Failed faults ->
                for fault in faults do
                    diagnostics.AddRange(AssemblyFiles.failureDiagnostics fault)
            | AssemblyFiles.FoldedUnit.SignatureOnly r ->
                surfaces.Add r.Surface
                published.Add r.Published
                diagnostics.AddRange(AssemblyFiles.anchorDiagnostics r.Source (r.ParseDiagnostics @ r.Diagnostics))
            | AssemblyFiles.FoldedUnit.Analysed u ->
                surfaces.Add u.Surface
                published.Add u.Published
                analysed.Add u

                match u.File.Signature with
                | ValueSome s ->
                    diagnostics.AddRange(AssemblyFiles.anchorDiagnostics s.Source (s.ParseDiagnostics @ s.Diagnostics))
                | ValueNone ->
                    // The unit's inferred surface IS its contract, so its analysis errors are
                    // findings about what this package publishes. A unit with a `.fsi` keeps
                    // today's tolerance: its analysis feeds splice templates alone.
                    diagnostics.AddRange(
                        AssemblyFiles.anchorDiagnostics
                            u.File.Source
                            (u.File.ParseDiagnostics @ u.File.Frozen.Residue.Diagnostics)
                        |> List.filter (fun d -> d.Diagnostic.Severity = Severity.Error)
                    )

        // What a consumer resolves through: this package's `[<AutoOpen>]` modules (most
        // specific, e.g. `Vesper.ArithmeticOperators`) ahead of the language prelude.
        let ambient =
            [
                for s in surfaces do
                    yield! s.AmbientOpenPrefixes
            ]
            |> List.distinct
            |> (fun prefixes -> prefixes @ RuntimeNames.preludeNamespaces)

        // The units' published views, NEAREST first, as the fold stacked them.
        let own = List.rev (List.ofSeq published)

        {
            // Every resolved descriptor carries the package as its home: the surfaces record
            // `SymbolOrigin.Empty`, and a package spans as many namespaces as its files declare.
            Provider =
                ExternalSymbolProviders.stack
                    (ValueSome(Origin.InAssembly(AssemblyName manifest.Name)))
                    ambient
                    [ ExternalSymbolProviders.composite own ]
            Diagnostics = List.ofSeq diagnostics
            HomeAssembly = manifest.Name
            DeclaredTypeNames = surfaces |> Seq.collect declaredTypeNames |> List.ofSeq |> List.distinct
            InlineBodies = InlineBodies.concat [ for u in analysed -> u.Bodies ]
            Origins = OriginSources.ofSeq [ for u in analysed -> u.File.Source ]
        }

    /// `buildProviderSeeded` with no platform metadata, over one already-composed dependency
    /// provider.
    let buildProviderWith (dependencies: IExternalSymbolProvider) (pkg: PackageSource.ParsedPackage) : BuiltPackage =
        buildProviderSeeded (fun _ -> []) [ dependencies ] pkg

    /// Stand up a referenced project in isolation: nothing in scope but the package's own
    /// files. For a package with no `depends-on`.
    let buildProvider
        (mp: ReferencedProject.ManifestPath)
        : Result<IExternalSymbolProvider * AssemblyFiles.AnchoredDiagnostic list, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (fun manifest ->
            let bp =
                buildProviderWith ExternalSymbolProviders.nullProvider (PackageSource.readPackage manifest)

            bp.Provider, bp.Diagnostics
        )

    /// Layer-2: given the intrinsic axis of the layer-1 providers composed so far, produce the
    /// target platform's metadata providers. A backend injects its .NET reflection reader or
    /// its JS-native stubs here.
    type PlatformMetadataFactory = IntrinsicTypeMap -> IExternalSymbolProvider list

    /// No layer-2: the layer-1 contracts alone, for an in-assembly caller that needs no
    /// platform metadata.
    let noPlatformMetadata: PlatformMetadataFactory = fun _ -> []

    /// One package set composed: the provider a consumer resolves through, and everything
    /// resolving the contracts found.
    [<NoEquality; NoComparison>]
    type ComposedContract =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
            /// Every package's splice templates, package then declaration order, so a later
            /// body wins a clash.
            InlineBodies: InlineBodies.FileInlineBodies
            /// The producer files the templates were unpooled from, retained so their anchors
            /// stay readable.
            Origins: OriginSources
        }

    /// Compose layer-1 providers in dependency (topological) order ahead of `platformMetadata`.
    /// Each package resolves against its transitive `depends-on` closure, so a cross-package
    /// nominal type constructor kinds at bake time.
    let composeOrdered
        (platformMetadata: PlatformMetadataFactory)
        (orderedPackages: PackageSource.ParsedPackage list)
        (transitiveDeps: ReferencedProject.ManifestPath -> ReferencedProject.ManifestPath list)
        : ComposedContract =
        // `byPath` indexes each built provider by its manifest, so a package's dependency
        // providers resolve in O(closure).
        let builtPackages = ResizeArray<BuiltPackage>()
        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()

        let byPath =
            Dictionary<ReferencedProject.ManifestPath, IExternalSymbolProvider>(HashIdentity.Structural)

        // A qualified type key declared twice in the referenced set resolves as a silent
        // first-hit shadow, so the loser is unreachable by lookup; refuse it here, a
        // CS0433-equivalent. The overlap with the platform metadata is diagnosed downstream.
        let seenTypeHomes = Dictionary<string, string>(System.StringComparer.Ordinal)

        for pkg in orderedPackages do
            let manifest = pkg.Manifest

            let depProviders =
                transitiveDeps manifest.Path
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            // The signatures resolve seeded with the deps' intrinsic axis, so a dependency's
            // BCL member sigs canonicalize while this package resolves; the bodies re-seed
            // with the package's own axis as well.
            let bp = buildProviderSeeded platformMetadata depProviders pkg
            diagnostics.AddRange bp.Diagnostics

            for typeName in bp.DeclaredTypeNames do
                match seenTypeHomes.TryGetValue typeName with
                | true, otherHome ->
                    diagnostics.AddRange(
                        AssemblyFiles.setFaultDiagnostics (
                            PackageSetFault.DuplicateType(typeName, otherHome, bp.HomeAssembly)
                        )
                    )
                | false, _ -> seenTypeHomes.[typeName] <- bp.HomeAssembly

            builtPackages.Add bp
            byPath.[manifest.Path] <- bp.Provider

        // The final composite's platform metadata IS seeded with the full resolved axis, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = [ for bp in builtPackages -> bp.Provider ]

        {
            Provider =
                ExternalSymbolProviders.composite (
                    builtList @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics builtList)
                )
            Diagnostics = List.ofSeq diagnostics
            InlineBodies = InlineBodies.concat [ for bp in builtPackages -> bp.InlineBodies ]
            Origins =
                (OriginSources.empty, builtPackages)
                ||> Seq.fold (fun acc bp -> OriginSources.addAll bp.Origins acc)
        }

    /// A whole-set fault, raised before the set got as far as having a package or a file.
    let private setFault (fault: PackageSetFault) : ComposedContract =
        {
            Provider = ExternalSymbolProviders.nullProvider
            Diagnostics = AssemblyFiles.setFaultDiagnostics fault
            InlineBodies = InlineBodies.empty
            Origins = OriginSources.empty
        }

    /// `composeOrdered` over a raw, unordered manifest set: the ordering is taken here and not
    /// handed back.
    let composeContract
        (platformMetadata: PlatformMetadataFactory)
        (manifests: ReferencedProject.ManifestPath list)
        : ComposedContract =
        match ReferencedProject.buildClosureWithDeps manifests with
        | Ok(ordered, transitiveDeps) ->
            composeOrdered platformMetadata (List.map PackageSource.readPackage ordered) transitiveDeps
        | Error fault -> setFault fault
