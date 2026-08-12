namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// Layer 1 of the symbol-resolution stack: a *referenced project*, a package DIRECTORY resolved
/// against a target to `manifest.<target>.toml`. Parses its `[core]` `.fsi` files, in order, into
/// one `ExtractCtx` provider. A symbol's namespace is its FILE's `namespace` header.
module ReferencedProject =

    /// A package's manifest FOR one target: the resolved file, and the target it was resolved
    /// under. Minted only by `resolveManifest`, so a file and a target that disagree is not a
    /// value that exists.
    [<Struct>]
    type ManifestPath =
        private
            {
                FullPath: string
                TargetTag: string
            }

        /// The absolute `manifest.<target>.toml` path.
        member this.Path = this.FullPath
        /// The target this manifest was resolved for.
        member this.Target = this.TargetTag
        /// The package directory the manifest sits in; its name is the package identity.
        member this.PackageDir = Path.GetDirectoryName this.FullPath

    /// The manifest `packageDir` publishes for `target`, absent when the package does not build
    /// for it. `src/Vesper.Core` + `"js"` ⇒ `src/Vesper.Core/manifest.js.toml`.
    let resolveManifest (target: string) (packageDir: string) : Result<ManifestPath, string> =
        let path = Path.Combine(packageDir, "manifest." + target + ".toml")

        if File.Exists path then
            Ok
                {
                    FullPath = Path.GetFullPath path
                    TargetTag = target
                }
        else
            Error(
                sprintf
                    "Package '%s' does not build for target `%s`: no %s"
                    (Path.GetFileName(Path.TrimEndingDirectorySeparator packageDir))
                    target
                    (Path.GetFileName path)
            )

    /// Every package directory resolved against `target`. A package that does not build for it
    /// is a hard error: dropping one silently would resolve, and CACHE, against a smaller
    /// package set than the caller named.
    let resolveAll (target: string) (packageDirs: string list) : ManifestPath list =
        packageDirs
        |> List.map (fun dir ->
            match resolveManifest target dir with
            | Ok mp -> mp
            | Error e -> failwith e
        )

    /// A parsed package `manifest.<target>.toml`: one flat `[core]` table, each list already
    /// in compile order for this manifest's target. A package that builds for two targets
    /// writes two files.
    type Manifest =
        {
            /// The file this was parsed from, and the target it was resolved under.
            Path: ManifestPath
            /// Package / assembly simple name: `[core] name` when present, else
            /// the manifest's directory name (`src/Vesper.Core` ⇒ `"Vesper.Core"`).
            Name: string
            /// Other packages this one depends on (`[core] depends-on`): the
            /// package names whose DLLs/contracts must be built/referenced first.
            DependsOn: string list
            /// Contract `.fsi` files in compile order (`[core] files`).
            Files: string list
            /// The `.fs` bodies compiled into the package DLL, and the splice sources those
            /// same bodies publish (`[core] impl`).
            Impl: string list
            /// Contract `.fsi` files that are DELIBERATELY impl-free (`[core] sig-only`): a
            /// front-end intrinsic lowered inline (`printf.fsi`), or a BCL-resolved contract
            /// (`exceptions.fsi`). An impl-free `.fsi` NOT listed is a hard error.
            SigOnly: string list
            /// `.fs` bodies that implement NO contract (`[core] impl-only`), publishing their
            /// whole public surface. Naming one here keeps the pairing rule from marrying it
            /// to a `.fsi` of the same key.
            ImplOnly: string list
            /// Hand-authored runtime *asset* modules: not sources the front end parses, but
            /// platform-support artifacts (the JS `.mjs`) the backend ships beside its output.
            Runtime: string list
        }

        /// The target this manifest is the package's contract for, read off the file name
        /// (`manifest.js.toml` ⇒ `"js"`).
        member this.Target = this.Path.Target
        /// The directory the manifest sits in, which every list entry is relative to.
        member this.Dir = this.Path.PackageDir

    let private asString (v: TomlValue) : string option =
        match v with
        | TomlValue.String s -> Some s
        | _ -> None

    let private asTable (v: TomlValue) : TomlTable option =
        match v with
        | TomlValue.Table t
        | TomlValue.InlineTable t -> Some t
        | _ -> None

    let private findString (t: TomlTable) (key: string) : string option =
        Map.tryFind key t |> Option.bind asString

    let private findStringList (t: TomlTable) (key: string) : string list option =
        match Map.tryFind key t with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Some
        | _ -> None

    /// The pairing key of a manifest-listed source: its name minus the extension, minus a
    /// trailing `.<target>` segment. In `manifest.js.toml`, `prim-types-int.js.fs` and
    /// `prim-types-int.fsi` both key on `prim-types-int`.
    let pairingKey (m: Manifest) (rel: string) : string =
        let noExt = Path.ChangeExtension(rel, null)
        let suffix = "." + m.Target

        if noExt.EndsWith(suffix, System.StringComparison.Ordinal) then
            noExt.Substring(0, noExt.Length - suffix.Length)
        else
            noExt

    /// Every path the provider build may READ for this manifest, relative to the manifest's
    /// own directory; a path named here need not exist. `Runtime` is omitted: an asset is
    /// never parsed, so determines no frozen tree.
    let sourceInputs (m: Manifest) : string list =
        m.Files @ m.Impl @ m.SigOnly @ m.ImplOnly |> List.distinct

    /// The `[core]` keys a manifest may carry. An unknown one is a parse ERROR: read as
    /// silence, it would resolve a stale manifest to a plausible wrong file set.
    let private coreKeys =
        set
            [
                "name"
                "description"
                "depends-on"
                "files"
                "impl"
                "sig-only"
                "impl-only"
                "runtime"
            ]

    let private unknownKey (path: string) (t: TomlTable) : string option =
        t
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.tryFind (coreKeys.Contains >> not)
        |> Option.map (fun key ->
            sprintf
                "%s: [core] unknown key `%s` (expected one of: %s)"
                path
                key
                (coreKeys |> Set.toList |> String.concat ", ")
        )

    /// Parse the manifest document read from `mp`, which carries both halves of a manifest's
    /// identity: its DIRECTORY name is the assembly name when `[core]` declares no `name`, and
    /// the target it was resolved under is the one its lists are read for.
    let parseManifest (mp: ManifestPath) (doc: TomlDocument) : Result<Manifest, string> =
        let path = mp.Path
        let dirName = Path.GetFileName(Path.TrimEndingDirectorySeparator mp.PackageDir)

        // A manifest is `[core]` and nothing else. A second table read as silence would resolve
        // a stale manifest to a file set missing everything that table held.
        match doc |> Map.toSeq |> Seq.map fst |> Seq.tryFind ((<>) "core") with
        | Some other -> Error(sprintf "%s: unknown table [%s] (a manifest carries [core] alone)" path other)
        | None ->

            match Map.tryFind "core" doc |> Option.bind asTable with
            | None -> Error(sprintf "%s: missing [core] table" path)
            | Some core ->
                match unknownKey path core with
                | Some e -> Error e
                | None ->
                    match findStringList core "files" with
                    | None -> Error(sprintf "%s: [core] missing `files = [...]`" path)
                    | Some files ->
                        // An omitted `name` trivially matches via the directory-name fallback.
                        match findString core "name" with
                        | Some explicit when explicit <> dirName ->
                            Error(
                                sprintf
                                    "%s: [core] name \"%s\" must match the package directory name \"%s\" — the directory name is the package identity that `depends-on` resolves against"
                                    path
                                    explicit
                                    dirName
                            )
                        | nameOpt ->
                            let list key =
                                findStringList core key |> Option.defaultValue []

                            Ok
                                {
                                    Path = mp
                                    Name = nameOpt |> Option.defaultValue dirName
                                    DependsOn = list "depends-on"
                                    Files = files
                                    Impl = list "impl"
                                    SigOnly = list "sig-only"
                                    ImplOnly = list "impl-only"
                                    Runtime = list "runtime"
                                }

    /// Read + parse a resolved manifest.
    let loadManifest (mp: ManifestPath) : Result<Manifest, string> =
        match Toml.parse (File.ReadAllText mp.Path) with
        | Error e -> Error(sprintf "Manifest parse error (%s): %s" mp.Path e)
        | Ok doc -> parseManifest mp doc

    /// Resolve a `depends-on` package name against the dependent's own target. A package's
    /// directory name *is* its identity, so `"Vesper.Core"` named by a manifest in
    /// `src/Vesper.List` resolves in `src/Vesper.Core`, at the same target.
    let private dependencyManifest (dependent: ManifestPath) (dependencyName: string) : Result<ManifestPath, string> =
        let srcDir = Path.GetDirectoryName dependent.PackageDir
        resolveManifest dependent.Target (Path.Combine(srcDir, dependencyName))

    /// Close `rootManifests` over `[core] depends-on`: every reachable manifest PARSED, in
    /// **dependency order** (de-duplicated, stable over discovery order), plus each one's DIRECT
    /// dependencies. A cycle or an absent manifest is a hard error.
    let private closeAndOrder
        (rootManifests: ManifestPath list)
        : Result<Manifest list * System.Collections.Generic.Dictionary<ManifestPath, ManifestPath list>, string> =
        // `discovered` is the order nodes were first reached (roots, then their deps);
        // the topo sort below walks it, so an already-ordered input comes back unchanged.
        let dependencies =
            System.Collections.Generic.Dictionary<ManifestPath, ManifestPath list>(HashIdentity.Structural)

        // Every consumer below needs the parsed manifest, so the closure hands its own back.
        let loaded =
            System.Collections.Generic.Dictionary<ManifestPath, Manifest>(HashIdentity.Structural)

        let discovered = ResizeArray<ManifestPath>()
        let mutable error = None

        let rec load (mp: ManifestPath) =
            if error.IsSome || dependencies.ContainsKey mp then
                ()
            else
                match loadManifest mp with
                | Error e -> error <- Some(sprintf "buildClosure: %s" e)
                | Ok manifest ->
                    let rec resolveDeps acc names =
                        match names with
                        | [] -> Ok(List.rev acc)
                        | name :: rest ->
                            match dependencyManifest mp name with
                            | Error e -> Error e
                            | Ok dep -> resolveDeps (dep :: acc) rest

                    match resolveDeps [] manifest.DependsOn with
                    | Error e -> error <- Some(sprintf "buildClosure: %s" e)
                    | Ok depPaths ->

                        dependencies.[mp] <- depPaths
                        loaded.[mp] <- manifest
                        discovered.Add mp

                        for depPath in depPaths do
                            load depPath

        for root in rootManifests do
            load root

        match error with
        | Some e -> Error e
        | None ->
            // Post-order DFS over the discovery order, so dependencies are emitted first.
            // The gray/black colouring (1 = on the stack, 2 = emitted) rejects a cycle.
            let ordered = ResizeArray<Manifest>()

            let state =
                System.Collections.Generic.Dictionary<ManifestPath, int>(HashIdentity.Structural)

            let mutable cycle = None

            let rec visit (node: ManifestPath) =
                if cycle.IsSome then
                    ()
                else
                    match state.TryGetValue node with
                    | true, 2 -> ()
                    | true, _ ->
                        cycle <- Some(sprintf "buildClosure: dependency cycle through package '%s'" loaded.[node].Name)
                    | _ ->
                        state.[node] <- 1

                        for dep in dependencies.[node] do
                            visit dep

                        state.[node] <- 2
                        ordered.Add loaded.[node]

            for node in discovered do
                visit node

            match cycle with
            | Some e -> Error e
            | None -> Ok(List.ofSeq ordered, dependencies)

    /// Every manifest reachable over `[core] depends-on`, parsed, in dependency order.
    let buildClosure (rootManifests: ManifestPath list) : Result<Manifest list, string> =
        closeAndOrder rootManifests |> Result.map fst

    /// Like `buildClosure`, but also returns each package's **transitive** `depends-on`
    /// closure: manifest → the manifests it depends on, directly or transitively (excluding
    /// itself). An unknown manifest maps to the empty list.
    let buildClosureWithDeps
        (rootManifests: ManifestPath list)
        : Result<Manifest list * (ManifestPath -> ManifestPath list), string> =
        match closeAndOrder rootManifests with
        | Error e -> Error e
        | Ok(ordered, adjacency) ->
            // Each package's closure is the union of its direct deps and those deps' already-
            // computed closures, emitted dep-closure-before-dep so each closure is itself
            // topologically ordered.
            let transitive =
                System.Collections.Generic.Dictionary<ManifestPath, ManifestPath list>(HashIdentity.Structural)

            for manifest in ordered do
                let key = manifest.Path
                let acc = ResizeArray<ManifestPath>()

                let seen = System.Collections.Generic.HashSet<ManifestPath>(HashIdentity.Structural)

                let add p =
                    if seen.Add p then
                        acc.Add p

                match adjacency.TryGetValue key with
                | true, directDeps ->
                    for dep in directDeps do
                        match transitive.TryGetValue dep with
                        | true, depClosure -> List.iter add depClosure
                        | _ -> ()

                        add dep
                | _ -> ()

                transitive.[key] <- List.ofSeq acc

            let lookup (key: ManifestPath) =
                match transitive.TryGetValue key with
                | true, v -> v
                | _ -> []

            Ok(ordered, lookup)

    /// The `[core] runtime` assets of an already-closed manifest set, read off disk: package
    /// name → `(fileName, source)`, the `.mjs` the backend imports by `./<fileName>`. One per
    /// package: the first listed.
    let runtimeModules (manifests: Manifest list) : Map<string, string * string> =
        let mutable acc = Map.empty

        for manifest in manifests do
            match manifest.Runtime with
            | rel :: _ ->
                let abs = Path.Combine(manifest.Dir, rel)

                if File.Exists abs then
                    acc <- Map.add manifest.Name (Path.GetFileName rel, File.ReadAllText abs) acc
            | [] -> ()

        acc

    /// Wrap the extractor's provider so every resolved descriptor carries the package's home
    /// assembly (the extractor records `SymbolOrigin.Empty`) and `ambient` is published as
    /// the provider's `AmbientOpenPrefixes`.
    let private wrap (home: Origin) (ambient: string list) (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        ExternalSymbolProviders.stack (ValueSome home) ambient [ inner ]

    /// One built package. `DeclaredTypeNames` are the qualified compiled names of the NOMINAL
    /// types it OWNS: those that would first-hit-shadow a peer package's same-named type.
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: (VesperLib.LibFile * string) list
            HomeAssembly: string
            DeclaredTypeNames: string list
        }

    /// Parse each contract `.fsi` in `files` order into one accumulating `ExtractCtx` seeded
    /// with `ambientShapes` (its dependencies' already-built type shapes), then expose it as
    /// a provider. A file that fails to parse yields a diagnostic, not an aborted build.
    let buildProviderWith
        (ambientShapes: string -> ExternalTypeShape voption)
        (dependencyAmbientPrefixes: string list)
        (manifest: Manifest)
        : BuiltPackage =
        let dir = manifest.Dir
        let ctx = VesperLib.ExtractCtx.empty manifest.Target
        ctx.AmbientShapes <- ambientShapes
        // Ambient open prefixes (`Vesper`, …) so this package's extraction resolves a
        // dependency's type by bare name (`Fun`2` / `Fun`3`).
        ctx.DependencyAmbientPrefixes <- dependencyAmbientPrefixes

        // Extract intrinsic reprs from the `.fs` bodies FIRST, so the `extern` arm of the
        // `.fsi` extraction below can pick `IntrinsicPlatform.Repr` over `Unsupported`.
        // The `.fsi` commits `type exn = extern`, no repr.
        for rel in manifest.Impl do
            let abs = Path.Combine(dir, rel)

            if File.Exists abs then
                let fsFile: VesperLib.LibFile =
                    {
                        Path =
                            {
                                BucketName = manifest.Name
                                Relative = rel
                            }
                        Absolute = abs
                    }

                match VesperLib.parseFileFull fsFile with
                | Error _ -> ()
                | Ok parsed ->
                    let reprs =
                        System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

                    VesperLib.extractIntrinsicReprsInto reprs parsed

                    for KeyValue(k, v) in reprs do
                        ctx.IntrinsicReprs.[k] <- v

        // In declared order, so a later contract's RHS (`Vesper.disposable`) is already
        // in the registry.
        for rel in manifest.Files do
            let file: VesperLib.LibFile =
                {
                    Path =
                        {
                            BucketName = manifest.Name
                            Relative = rel
                        }
                    Absolute = Path.Combine(dir, rel)
                }

            match VesperLib.parseFileFull file with
            | Error e -> ctx.Diagnostics.Add(file, e)
            | Ok parsed -> VesperLib.extractSymbols ctx parsed

        let home = Origin.InAssembly(AssemblyName manifest.Name)

        // The contract's implicit prelude: its `[<AutoOpen>]` modules (most specific,
        // e.g. `Vesper.ArithmeticOperators`) ahead of the language prelude (`Vesper`,
        // so `int` finds `Vesper.int`). The prelude is fixed, not manifest-declared.
        let ambient = List.ofSeq ctx.AutoOpenPrefixes @ RuntimeNames.preludeNamespaces

        // The nominal types this package declares, for the composition-time duplicate
        // sweep. Every package's `int` is THE `int`, so intrinsics and capability
        // interfaces repeat legitimately and are excluded.
        let declaredTypeNames =
            [
                for kv in ctx.TypeShapes do
                    match kv.Value with
                    | ExternalTypeShape.Class _
                    | ExternalTypeShape.Record _
                    | ExternalTypeShape.Union _
                    | ExternalTypeShape.Enum _
                    | ExternalTypeShape.Abbrev _
                    | ExternalTypeShape.Unmodelled _ -> yield kv.Key
                    | ExternalTypeShape.Intrinsic _
                    | ExternalTypeShape.IntrinsicInterface _ -> ()
            ]

        {
            Provider = wrap home ambient (VesperLib.ExtractCtx.toProvider ctx)
            Diagnostics = List.ofSeq ctx.Diagnostics
            HomeAssembly = manifest.Name
            DeclaredTypeNames = declaredTypeNames
        }

    /// Stand up a referenced project in isolation: no dependency shapes in scope, so
    /// `ambientShapes` resolves nothing. For a package with no `depends-on`.
    let buildProvider (mp: ManifestPath) : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        loadManifest mp
        |> Result.map (fun manifest ->
            let bp = buildProviderWith (fun _ -> ValueNone) [] manifest
            bp.Provider, bp.Diagnostics
        )

    /// A layer-2 metadata-tail factory: given the extracted `{ platform-repr → [canon] }`
    /// reverse map of the layer-1 providers composed so far, produce the trailing leaf
    /// providers. A backend injects its BCL metadata / JS-native tail here.
    type MetaTailFactory = Map<string, SymbolKey list> -> IExternalSymbolProvider list

    /// The empty layer-2 tail: the layer-1 `.fsi` contracts alone, for an in-assembly
    /// caller that resolves no BCL/native metadata.
    let noMetaTail: MetaTailFactory = fun _ -> []

    /// Compose layer-1 providers in dependency (topological) order ahead of the `metaTail`
    /// leaf. Each package is extracted with read access to its transitive `depends-on`
    /// closure's shapes, so a cross-package nominal type constructor kinds at bake time.
    let composeOrdered
        (metaTail: MetaTailFactory)
        (orderedManifests: Manifest list)
        (transitiveDeps: ManifestPath -> ManifestPath list)
        : IExternalSymbolProvider =
        // `byPath` indexes each built provider by its manifest, so a package's dependency
        // providers resolve in O(closure).
        let built = ResizeArray<IExternalSymbolProvider>()

        let byPath =
            System.Collections.Generic.Dictionary<ManifestPath, IExternalSymbolProvider>(HashIdentity.Structural)

        // A qualified type key declared twice in the referenced set resolves as a silent
        // first-hit shadow, so the loser is unreachable by lookup; refuse it here, a
        // CS0433-equivalent. The overlap with the metadata tail is diagnosed downstream.
        let seenTypeHomes =
            System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

        for manifest in orderedManifests do
            let depProviders =
                transitiveDeps manifest.Path
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            // Seeded with the reverse map of the deps built so far, so a dependency's BCL
            // member sigs canonicalize during extraction.
            let depComposite =
                ExternalSymbolProviders.composite (
                    depProviders @ metaTail (ExternalSymbolProviders.mergeReverseCanon depProviders)
                )

            let ambientShapes =
                (fun (name: string) -> depComposite.TryLookupType name |> ExternalSymbols.typeShapeOf)

            // The dependency providers' implicit open prefixes (`Vesper` from Core, where
            // `Fun`2`/`Fun`3`/`Ref` live). Deduped, in dependency order.
            let depAmbientPrefixes =
                depProviders |> List.collect (fun p -> p.AmbientOpenPrefixes) |> List.distinct

            let bp = buildProviderWith ambientShapes depAmbientPrefixes manifest

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

        // The final composite's leaf IS seeded with the full extracted reverse map, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = List.ofSeq built
        ExternalSymbolProviders.composite (builtList @ metaTail (ExternalSymbolProviders.mergeReverseCanon builtList))

    /// `composeOrdered` over a raw, unordered manifest set. A cycle or missing dependency is
    /// a hard error. A caller that also needs the ordered list should order it itself.
    let composeContract (metaTail: MetaTailFactory) (manifests: ManifestPath list) : IExternalSymbolProvider =
        match buildClosureWithDeps manifests with
        | Ok(ordered, transitiveDeps) -> composeOrdered metaTail ordered transitiveDeps
        | Error e -> failwithf "Failed to order referenced project manifests: %s" e
