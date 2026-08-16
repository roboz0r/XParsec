namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

/// Layer 1 of the symbol-resolution stack: each referenced package's `[core] files` `.fsi`
/// contracts RESOLVED, one at a time, through the same front end an in-assembly `.fsi` goes
/// through, and composed into one provider. A symbol's namespace is its FILE's `namespace`
/// header.
module PackageProviders =

    /// One built package. `DeclaredTypeNames` are the qualified compiled names of the NOMINAL
    /// types it OWNS: those that would first-hit-shadow a peer package's same-named type.
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
            HomeAssembly: string
            DeclaredTypeNames: string list
        }

    /// The intrinsic reprs the package's `.fs` bodies bind (`type exn = (# "System.Exception" #)`),
    /// short name ⇒ repr. Read for the WHOLE package before any `.fsi` is resolved, so the
    /// `extern` arm of a contract that commits `type exn = extern` picks `IntrinsicPlatform.Repr`
    /// over `Unsupported`.
    let private implementationReprs (manifest: ReferencedProject.Manifest) : Dictionary<string, string> =
        let reprs = Dictionary<string, string>(System.StringComparer.Ordinal)

        for rel in manifest.Impl do
            let abs = Path.Combine(manifest.Dir, rel)

            if File.Exists abs then
                match Pipeline.parse (File.ReadAllText abs) with
                | Error _ -> ()
                | Ok parsed -> IntrinsicReprs.ofImplementationInto reprs (SyntaxToken.nameIn parsed.Lexed) parsed.File

        reprs

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

    /// The language prelude as a SOURCE: it resolves nothing, and publishes the prefixes every
    /// contract is written against, so a `.fsi` in `namespace Vesper.Collections` names `unit`
    /// exactly as a consumer of the package would. Fixed, not manifest-declared.
    let private prelude =
        ExternalSymbolProviders.stack ValueNone RuntimeNames.preludeNamespaces []

    /// Resolve each contract `.fsi` in `files` order against `dependencies` plus the package's
    /// OWN earlier files, nearest first — the same fold an assembly's units go through. A file
    /// that fails to parse yields a diagnostic and contributes nothing, not an aborted build.
    let buildProviderWith
        (dependencies: IExternalSymbolProvider)
        (manifest: ReferencedProject.Manifest)
        : BuiltPackage =
        let inputs: SignatureResolutionContext.SignatureInputs =
            {
                Target = manifest.Target
                Reprs = implementationReprs manifest
            }

        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()
        // The package's own files, NEAREST first: what it publishes, and the top of the
        // visibility stack each later file resolves through, over the dependencies.
        let mutable own: IExternalSymbolProvider list = []
        let surfaces = ResizeArray<PublishedSurface>()

        for rel in manifest.Files do
            let id = AssemblyFileId.ofPathUnder manifest.Dir rel

            match Pipeline.parseSignature (File.ReadAllText(Path.Combine(manifest.Dir, rel))) with
            | Error failure ->
                match failure.Lexed with
                | ValueSome lexed ->
                    diagnostics.AddRange(
                        AssemblyFiles.anchorDiagnostics
                            (AssemblyFiles.fileSource manifest.Name id lexed)
                            failure.Diagnostics
                    )
                | ValueNone -> diagnostics.AddRange(AssemblyFiles.unpositionedDiagnostics id failure.Diagnostics)
            | Ok parsed ->
                let source = AssemblyFiles.fileSource manifest.Name id parsed.Lexed

                let ctx =
                    PassContext(ExternalSymbolProviders.composite (own @ [ dependencies; prelude ]), source)

                ctx.AssemblyName <- manifest.Name

                let surface = SignatureResolution.run ctx inputs parsed.File
                surfaces.Add surface
                own <- PublishedSurface.toProvider surface :: own

                diagnostics.AddRange(
                    AssemblyFiles.anchorDiagnostics source (parsed.Diagnostics @ List.ofSeq ctx.Diagnostics)
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
        }

    /// Stand up a referenced project in isolation: nothing in scope but the package's own
    /// files. For a package with no `depends-on`.
    let buildProvider
        (mp: ReferencedProject.ManifestPath)
        : Result<IExternalSymbolProvider * AssemblyFiles.AnchoredDiagnostic list, string> =
        ReferencedProject.loadManifest mp
        |> Result.map (fun manifest ->
            let bp = buildProviderWith ExternalSymbolProviders.nullProvider manifest
            bp.Provider, bp.Diagnostics
        )

    /// Layer-2: given the intrinsic axis of the layer-1 providers composed so far, produce the
    /// target platform's metadata providers. A backend injects its .NET reflection reader or
    /// its JS-native stubs here.
    type PlatformMetadataFactory = IntrinsicTypeMap -> IExternalSymbolProvider list

    /// No layer-2: the layer-1 `.fsi` contracts alone, for an in-assembly caller that
    /// resolves no platform metadata.
    let noPlatformMetadata: PlatformMetadataFactory = fun _ -> []

    /// Compose layer-1 providers in dependency (topological) order ahead of `platformMetadata`.
    /// Each package resolves against its transitive `depends-on` closure, so a cross-package
    /// nominal type constructor kinds at bake time.
    let composeOrdered
        (platformMetadata: PlatformMetadataFactory)
        (orderedManifests: ReferencedProject.Manifest list)
        (transitiveDeps: ReferencedProject.ManifestPath -> ReferencedProject.ManifestPath list)
        : IExternalSymbolProvider =
        // `byPath` indexes each built provider by its manifest, so a package's dependency
        // providers resolve in O(closure).
        let built = ResizeArray<IExternalSymbolProvider>()

        let byPath =
            Dictionary<ReferencedProject.ManifestPath, IExternalSymbolProvider>(HashIdentity.Structural)

        // A qualified type key declared twice in the referenced set resolves as a silent
        // first-hit shadow, so the loser is unreachable by lookup; refuse it here, a
        // CS0433-equivalent. The overlap with the platform metadata is diagnosed downstream.
        let seenTypeHomes = Dictionary<string, string>(System.StringComparer.Ordinal)

        for manifest in orderedManifests do
            let depProviders =
                transitiveDeps manifest.Path
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            // Seeded with the axis of the deps built so far, so a dependency's BCL member
            // sigs canonicalize while this package resolves.
            let depComposite =
                ExternalSymbolProviders.composite (
                    depProviders
                    @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics depProviders)
                )

            let bp = buildProviderWith depComposite manifest

            for typeName in bp.DeclaredTypeNames do
                match seenTypeHomes.TryGetValue typeName with
                | true, otherHome when otherHome <> bp.HomeAssembly ->
                    failwithf
                        "The type '%s' exists in both '%s' and '%s'. A referenced package set must declare each type once; reference only one of the two packages."
                        typeName
                        otherHome
                        bp.HomeAssembly
                | true, _ ->
                    failwithf
                        "The type '%s' is declared twice by package '%s' — the referenced set contains two copies (or versions) of it. Reference the package once."
                        typeName
                        bp.HomeAssembly
                | false, _ -> seenTypeHomes.[typeName] <- bp.HomeAssembly

            built.Add bp.Provider
            byPath.[manifest.Path] <- bp.Provider

        // The final composite's platform metadata IS seeded with the full resolved axis, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = List.ofSeq built

        ExternalSymbolProviders.composite (
            builtList @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics builtList)
        )

    /// `composeOrdered` over a raw, unordered manifest set. A cycle or missing dependency is
    /// a hard error. A caller that also needs the ordered list should order it itself.
    let composeContract
        (platformMetadata: PlatformMetadataFactory)
        (manifests: ReferencedProject.ManifestPath list)
        : IExternalSymbolProvider =
        match ReferencedProject.buildClosureWithDeps manifests with
        | Ok(ordered, transitiveDeps) -> composeOrdered platformMetadata ordered transitiveDeps
        | Error e -> failwithf "Failed to order referenced project manifests: %s" e
