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
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
            HomeAssembly: string
            DeclaredTypeNames: string list
        }

    /// Short name ⇒ intrinsic repr, read off the signature file's PAIRED implementation
    /// (`type exn = (# "System.Exception" #)`), so `type exn = extern` picks `Repr` over
    /// `Unsupported`. A signature this target ships no implementation for stays unsupported.
    let private companionReprs (entry: PackageSource.SignatureEntry) : Dictionary<string, string> =
        let reprs = Dictionary<string, string>(System.StringComparer.Ordinal)

        match entry.Companion with
        | ValueSome { Outcome = Ok implementation } ->
            IntrinsicReprs.ofImplementationInto reprs (SyntaxToken.nameIn implementation.Lexed) implementation.File
        | _ -> ()

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
    /// signature file is written against, so a `.fsi` in `namespace Vesper.Collections` names
    /// `unit` exactly as a consumer of the package would. Fixed, not manifest-declared.
    let private prelude =
        ExternalSymbolProviders.stack ValueNone RuntimeNames.preludeNamespaces []

    /// What a path the read could not deliver reads as, anchored. The parser's own findings
    /// carry positions in the text; the rest have no place in any file to point at.
    let private faultDiagnostics
        (packageName: string)
        (file: PackageSource.ReadFile<'Tree>)
        (fault: PackageSource.FileFault)
        : AssemblyFiles.AnchoredDiagnostic list =
        let failure = PackageSource.FileFault.toFailure packageName file.Relative fault

        match failure.Lexed with
        | ValueSome lexed ->
            AssemblyFiles.anchorDiagnostics (AssemblyFiles.fileSource packageName file.Id lexed) failure.Diagnostics
        | ValueNone -> AssemblyFiles.unpositionedDiagnostics file.Id failure.Diagnostics

    /// Resolve each signature file in `files` order against `dependencies` plus the package's
    /// OWN earlier files, nearest first, which is the fold an assembly's units go through. A file
    /// the read could not deliver yields a diagnostic and contributes nothing.
    let buildProviderWith (dependencies: IExternalSymbolProvider) (pkg: PackageSource.ParsedPackage) : BuiltPackage =
        let manifest = pkg.Manifest
        let diagnostics = ResizeArray<AssemblyFiles.AnchoredDiagnostic>()

        // The package's own files, NEAREST first: what it publishes, and the top of the
        // visibility stack each later file resolves through, over the dependencies.
        let mutable own: IExternalSymbolProvider list = []
        let surfaces = ResizeArray<PublishedSurface>()

        for entry in pkg.Signatures do
            match entry.Signature.Outcome with
            | Error fault -> diagnostics.AddRange(faultDiagnostics manifest.Name entry.Signature fault)
            | Ok parsed ->
                let source = AssemblyFiles.fileSource manifest.Name entry.Signature.Id parsed.Lexed

                let surface, resolutionDiagnostics =
                    SignatureResolution.resolveFile
                        (ExternalSymbolProviders.composite (own @ [ dependencies; prelude ]))
                        source
                        {
                            Assembly = manifest.Name
                            Target = manifest.Target
                            Reprs = companionReprs entry
                        }
                        parsed.File

                surfaces.Add surface
                own <- PublishedSurface.toProvider surface :: own

                diagnostics.AddRange(
                    AssemblyFiles.anchorDiagnostics source (parsed.Diagnostics @ resolutionDiagnostics)
                )

        // An implementation file's fault, reported from the list that NAMES it: off the pairing
        // a companion's would appear twice, and one with no signature file not at all.
        for entry in pkg.Implementations do
            match entry.Implementation.Outcome with
            | Error fault -> diagnostics.AddRange(faultDiagnostics manifest.Name entry.Implementation fault)
            | Ok _ -> ()

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
    type ComposedContract =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: AssemblyFiles.AnchoredDiagnostic list
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
        let built = ResizeArray<IExternalSymbolProvider>()
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

            // Seeded with the axis of the deps built so far, so a dependency's BCL member
            // sigs canonicalize while this package resolves.
            let depComposite =
                ExternalSymbolProviders.composite (
                    depProviders
                    @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics depProviders)
                )

            let bp = buildProviderWith depComposite pkg
            diagnostics.AddRange bp.Diagnostics

            for typeName in bp.DeclaredTypeNames do
                match seenTypeHomes.TryGetValue typeName with
                | true, otherHome ->
                    diagnostics.AddRange(
                        AssemblyFiles.unpositionedDiagnostics
                            (AssemblyFileId.ofRelative manifest.Name)
                            [
                                Diagnostic.nowhere (
                                    Kind.PackageSet(PackageSetFault.DuplicateType(typeName, otherHome, bp.HomeAssembly))
                                )
                            ]
                    )
                | false, _ -> seenTypeHomes.[typeName] <- bp.HomeAssembly

            built.Add bp.Provider
            byPath.[manifest.Path] <- bp.Provider

        // The final composite's platform metadata IS seeded with the full resolved axis, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = List.ofSeq built

        {
            Provider =
                ExternalSymbolProviders.composite (
                    builtList @ platformMetadata (ExternalSymbolProviders.mergeIntrinsics builtList)
                )
            Diagnostics = List.ofSeq diagnostics
        }

    /// A whole-set fault, raised before the set got as far as having a package or a file.
    let private setFault (fault: PackageSetFault) : ComposedContract =
        {
            Provider = ExternalSymbolProviders.nullProvider
            Diagnostics =
                AssemblyFiles.unpositionedDiagnostics
                    AssemblyFileId.nowhere
                    [ Diagnostic.nowhere (Kind.PackageSet fault) ]
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
        | Error e -> setFault (PackageSetFault.UnresolvedDependency e)
