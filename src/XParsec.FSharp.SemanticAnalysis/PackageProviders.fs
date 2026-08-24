namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// Layer 1 of the symbol-resolution stack: each referenced package's `[core] files` signature
/// files RESOLVED one at a time, through the front end an in-assembly `.fsi` goes through, and
/// composed into one provider. A symbol's namespace is its FILE's `namespace` header.
module PackageProviders =

    /// One analysed manifest set: a single built package, or a `depends-on` closure composed
    /// into one. A backend takes the WHOLE value: a provider from one manifest set beside an
    /// anchor domain from another resolves a served body's position against a file that was
    /// never retained, and the wrong answer is in range.
    [<NoEquality; NoComparison>]
    type AnalysedManifest =
        {
            /// The `[core] runtime` assets of the set, which back a compiled program's
            /// imports, keyed by package name.
            RuntimeAssets: Map<string, RuntimeAsset list>
            /// What a consumer resolves through. For a composed set, `InlineBodies` is
            /// served through it.
            Provider: IExternalSymbolProvider
            /// Qualified compiled name of each OWNED nominal type → its home assembly:
            /// the types that would first-hit-shadow a peer package's same-named type.
            TypeHomes: Map<string, string>
            /// The splice templates in manifest then declaration order, so a later body
            /// wins a clash, anchored in the declaring files `Retained` holds.
            InlineBodies: InlineBodies.FileInlineBodies
            /// The declaring files the collected bodies were unpooled from, retained so their
            /// anchors stay readable. Re-parsing to recover them would give a second answer.
            Retained: LexedFiles
            /// What resolving found: a manifest listing a file it has not got, a declaration
            /// a contract could not publish.
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
        }

    [<RequireQualifiedAccess>]
    module AnalysedManifest =

        /// The EMPTY manifest set: nothing resolves, nothing is served, nothing is anchored
        /// but the compiling file. A real value, so no consumer carries a
        /// "there is no contract" arm.
        let empty: AnalysedManifest =
            {
                RuntimeAssets = Map.empty
                Provider = ExternalSymbolProviders.nullProvider
                TypeHomes = Map.empty
                InlineBodies = InlineBodies.empty
                Retained = LexedFiles.empty
                Diagnostics = []
            }

        /// A whole-set fault, raised before the set got as far as having a package or a file.
        let ofSetFault (fault: PackageSetFault) : AnalysedManifest =
            { empty with
                Diagnostics = AssemblyFiles.setFaultDiagnostics fault
            }

        /// The manifest, refused if resolving it failed. Ungated, a set that publishes LESS
        /// than its `.fsi` files say surfaces as an unresolved name in the COMPILING file,
        /// which blames the wrong file for it.
        let gate (m: AnalysedManifest) : Result<AnalysedManifest, AssemblyFiles.AnchoredDiagnostic list> =
            match AssemblyFiles.AnchoredDiagnostic.errors m.Diagnostics with
            | [] -> Ok m
            | errors -> Error errors

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
        fun assembly provider retained impl ->
            let ctx, sem = Pipeline.analyseSemWithContextFor assembly provider retained impl

            try
                Freeze.run ctx sem
            with e ->
                let pruned =
                    match sem.Diagnostics |> Diagnostic.errors with
                    | [] -> " (none, so the fault is in the freeze itself)"
                    | errors -> errors |> List.map (fun d -> "\n  " + Kind.message d.Kind) |> String.concat ""

                failwithf
                    "internal error: freezing package '%O' impl file '%s' failed: %s\nits analysis errors, which the freeze pruned:%s"
                    assembly.Name
                    retained.Path.Relative.Name
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
        (parsed: ParsedManifest)
        : AnalysedManifest =
        let manifest = parsed.Manifest

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

        let analysedUnits =
            AssemblyAnalysis.analyseUnits
                analysePackageFile
                {
                    Name = AssemblyName manifest.Name
                    Target = manifest.Target
                }
                (ExternalSymbolProviders.composite (depProviders @ platformMetadata depIntrinsics))
                (AssemblyAnalysis.Publication.AcrossAssemblies bodyExternal)
                (parsed.Units |> List.map AssemblyFiles.AssemblyUnit.ofReadUnit)

        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()
        let surfaces = ResizeArray<PublishedSurface>()
        let analysed = ResizeArray<AssemblyAnalysis.AnalysedUnit>()

        for unit in analysedUnits.Units do
            diagnostics.AddRange(AssemblyAnalysis.UnitOutcome.surfaced unit)

            match unit with
            | AssemblyAnalysis.UnitOutcome.Failed _ -> ()
            | AssemblyAnalysis.UnitOutcome.Analysed u ->
                surfaces.Add u.Surface
                analysed.Add u

        // What a consumer resolves through: this package's `[<AutoOpen>]` modules (most
        // specific, e.g. `Vesper.ArithmeticOperators`) ahead of the language prelude.
        let ambient =
            [
                for s in surfaces do
                    yield! s.AmbientOpenPrefixes
            ]
            |> List.distinct
            |> (fun prefixes -> prefixes @ RuntimeNames.preludeNamespaces)

        let runtimeAssets =
            match ReferencedProject.runtimeModules [ manifest ] with
            | Ok assets -> assets
            | Error fault ->
                diagnostics.AddRange(AssemblyFiles.setFaultDiagnostics fault)
                Map.empty

        {
            RuntimeAssets = runtimeAssets
            // Every resolved descriptor carries the package as its home: the surfaces record
            // `SymbolOrigin.Empty`, and a package spans as many namespaces as its files declare.
            Provider =
                ExternalSymbolProviders.stack
                    (ValueSome(SymbolHome.InAssembly(AssemblyName manifest.Name)))
                    ambient
                    [ ExternalSymbolProviders.composite analysedUnits.Published ]
            Diagnostics = List.ofSeq diagnostics
            TypeHomes =
                Map.ofList
                    [
                        for s in surfaces do
                            for typeName in declaredTypeNames s -> typeName, manifest.Name
                    ]
            InlineBodies = InlineBodies.concat [ for u in analysed -> u.Bodies ]
            Retained = AssemblyFiles.retainedDomain [ for u in analysed -> u.File ]
        }

    /// `buildProviderSeeded` with no platform metadata, over one already-composed dependency
    /// provider.
    let buildProviderWith (dependencies: IExternalSymbolProvider) (parsed: ParsedManifest) : AnalysedManifest =
        buildProviderSeeded (fun _ -> []) [ dependencies ] parsed

    /// Stand up a referenced project in isolation: nothing in scope but the package's own
    /// files. For a package with no `depends-on`.
    let buildProvider
        (mp: ReferencedProject.ManifestPath)
        : Result<IExternalSymbolProvider * AssemblyFiles.AnchoredDiagnostic list, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (fun manifest ->
            let bp =
                buildProviderWith ExternalSymbolProviders.nullProvider (ParsedManifest.ofManifest manifest)

            bp.Provider, bp.Diagnostics
        )

    /// Layer-2: given the intrinsic axis of the layer-1 providers composed so far, produce the
    /// target platform's metadata providers. A backend injects its .NET reflection reader or
    /// its JS-native stubs here.
    type PlatformMetadataFactory = IntrinsicTypeMap -> IExternalSymbolProvider list

    /// No layer-2: the layer-1 contracts alone, for an in-assembly caller that needs no
    /// platform metadata.
    let noPlatformMetadata: PlatformMetadataFactory = fun _ -> []

    /// Compose layer-1 providers in dependency (topological) order ahead of `platformMetadata`.
    /// Each package resolves against its transitive `depends-on` closure, so a cross-package
    /// nominal type constructor kinds at bake time.
    let composeOrdered
        (platformMetadata: PlatformMetadataFactory)
        (orderedManifests: ParsedManifest list)
        (transitiveDeps: ReferencedProject.ManifestPath -> ReferencedProject.ManifestPath list)
        : AnalysedManifest =
        // `byPath` indexes each built provider by its manifest, so a package's dependency
        // providers resolve in O(closure).
        let builtPackages = ResizeArray<AnalysedManifest>()
        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()

        let byPath =
            Dictionary<ReferencedProject.ManifestPath, IExternalSymbolProvider>(HashIdentity.Structural)

        // A qualified type key declared twice in the referenced set resolves as a silent
        // first-hit shadow, so the loser is unreachable by lookup; refuse it here, a
        // CS0433-equivalent. The overlap with the platform metadata is diagnosed downstream.
        let seenTypeHomes = Dictionary<string, string>(System.StringComparer.Ordinal)

        for parsed in orderedManifests do
            let manifest = parsed.Manifest

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
            let bp = buildProviderSeeded platformMetadata depProviders parsed
            diagnostics.AddRange bp.Diagnostics

            for KeyValue(typeName, home) in bp.TypeHomes do
                match seenTypeHomes.TryGetValue typeName with
                | true, otherHome ->
                    diagnostics.AddRange(
                        AssemblyFiles.setFaultDiagnostics (PackageSetFault.DuplicateType(typeName, otherHome, home))
                    )
                | false, _ -> seenTypeHomes.[typeName] <- home

            builtPackages.Add bp
            byPath.[manifest.Path] <- bp.Provider

        // The final composite's platform metadata IS seeded with the full resolved axis, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = [ for bp in builtPackages -> bp.Provider ]

        // Every package's splice templates, package then declaration order, so a later body
        // wins a clash.
        let inlineBodies =
            InlineBodies.concat [ for bp in builtPackages -> bp.InlineBodies ]

        let runtimeAssets =
            match ReferencedProject.runtimeModules [ for parsed in orderedManifests -> parsed.Manifest ] with
            | Ok assets -> assets
            | Error fault ->
                diagnostics.AddRange(AssemblyFiles.setFaultDiagnostics fault)
                Map.empty

        {
            RuntimeAssets = runtimeAssets
            Provider =
                ExternalSymbolProviders.composite (
                    builtList @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics builtList)
                )
                |> ExternalSymbolProviders.withInlineBodies (InlineBodies.index inlineBodies)
            TypeHomes = Map.ofSeq [ for KeyValue(typeName, home) in seenTypeHomes -> typeName, home ]
            InlineBodies = inlineBodies
            Diagnostics = List.ofSeq diagnostics
            Retained =
                (LexedFiles.empty, builtPackages)
                ||> Seq.fold (fun acc bp -> LexedFiles.addAll bp.Retained acc)
        }

    /// `composeOrdered` over a raw, unordered manifest set: the ordering is taken here and not
    /// handed back. `readManifest` is the per-manifest read, so a caller already holding one
    /// package's trees hands them over instead of reading them again.
    let composeContractWith
        (readManifest: ReferencedProject.Manifest -> ParsedManifest)
        (platformMetadata: PlatformMetadataFactory)
        (manifests: ReferencedProject.ManifestPath list)
        : AnalysedManifest =
        match ReferencedProject.buildClosureWithDeps manifests with
        | Ok(ordered, transitiveDeps) -> composeOrdered platformMetadata (List.map readManifest ordered) transitiveDeps
        | Error fault -> AnalysedManifest.ofSetFault fault

    /// `composeContractWith`, reading every package from disk.
    let composeContract
        (platformMetadata: PlatformMetadataFactory)
        (manifests: ReferencedProject.ManifestPath list)
        : AnalysedManifest =
        composeContractWith ParsedManifest.ofManifest platformMetadata manifests
