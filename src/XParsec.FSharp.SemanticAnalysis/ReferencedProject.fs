namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// Layer 1 of the symbol-resolution stack: a
/// *referenced project*, declared by its `manifest.toml`. A package's `[core]`
/// table names the namespace and lists its contract `.fsi` files in compile
/// order; this module parses each into one accumulating `ExtractCtx` (reusing
/// `VesperLib`'s extractor) and exposes the result as an `IExternalSymbolProvider`
/// whose symbols carry the package `Origin` (assembly simple name + namespace).
///
/// The `.fsi` is the *target-agnostic contract* (`type int = extern`); the
/// matching `.fs` is the *per-target binding* (`type int = (# "System.Int32" #)`).
/// Resolution needs only the `.fsi`; the absent `.fs` is a codegen-side concern
/// not a resolution failure here.
module ReferencedProject =

    /// A parsed package `manifest.toml`'s `[core]` table. Mirrors the schema used
    /// across `src/Vesper.*` (and consumed today by the parser golden tests).
    type Manifest =
        {
            /// Package / assembly simple name — `[core] name` when present, else
            /// the manifest's directory name (`src/Vesper.Core` ⇒ `"Vesper.Core"`).
            /// `Vesper.Core`/`Vesper.Printf` omit `name`; the dir name is the
            /// package identity in both cases.
            Name: string
            /// `[core] namespace` — the namespace the package's symbols live in
            /// (and the implicit auto-open prefix for short-name resolution).
            Namespace: string
            /// Other packages this one depends on (`[core] depends-on`) — the
            /// package names whose DLLs/contracts must be built/referenced first.
            /// Drives the package-build harness's recursive dependency resolution.
            DependsOn: string list
            /// Contract `.fsi` files in compile order (`[core] files`).
            Files: string list
            /// The `.fs` files compiled into the package DLL (`[core] impl`) — the
            /// compile target. For most packages these are also the inline-body
            /// source (see `InlineBodies`); they diverge for signature-only operator
            /// packages (Vesper.Core's DLL is the prim-types/`Ref` bodies, its inline
            /// bodies live in `ops-platform.fs`; Vesper.Comparison has no DLL at all).
            Impl: string list
            /// The `.fs` files whose module-level `let inline` bindings are spliced
            /// across the package boundary at consumer use sites (`[core]
            /// inline-bodies`), consumed by `SymbolProviders.inlineBodies`. Defaults
            /// to `Impl` when the key is absent — the common case where the impl
            /// files are themselves the inline-body source.
            InlineBodies: string list
        }

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

    /// Parse a package `manifest.toml` document. `dirName` is the manifest's
    /// directory name, used as the assembly name when `[core]` carries no `name`.
    let parseManifest (dirName: string) (doc: TomlDocument) : Result<Manifest, string> =
        match Map.tryFind "core" doc |> Option.bind asTable with
        | None -> Error "manifest.toml: missing [core] table"
        | Some core ->
            match findString core "namespace", findStringList core "files" with
            | None, _ -> Error "manifest.toml: [core] missing `namespace`"
            | _, None -> Error "manifest.toml: [core] missing `files = [...]`"
            | Some ns, Some files ->
                // The directory name *is* the package identity — it is what a
                // sibling's `depends-on` resolves against (`dependencyManifestPath`)
                // and what `buildClosure` would otherwise report via `Name`. If an
                // explicit `[core] name` diverged from the directory, a `depends-on`
                // would be resolved by directory but reported by `Name` (and a
                // `depends-on` written against `Name` would silently miss). Reject
                // the divergence at parse time so the two identities can't drift
                // unnoticed; an omitted `name` trivially matches via the fallback.
                match findString core "name" with
                | Some explicit when explicit <> dirName ->
                    Error(
                        sprintf
                            "manifest.toml: [core] name \"%s\" must match the package directory name \"%s\" — the directory name is the package identity that `depends-on` resolves against"
                            explicit
                            dirName
                    )
                | nameOpt ->
                    let impl = findStringList core "impl" |> Option.defaultValue []

                    Ok
                        {
                            Name = nameOpt |> Option.defaultValue dirName
                            Namespace = ns
                            DependsOn = findStringList core "depends-on" |> Option.defaultValue []
                            Files = files
                            Impl = impl
                            // `inline-bodies` defaults to the impl files: the common case
                            // is that a package's implementation *is* its inline-body
                            // source. Operator packages override it (their DLL compile
                            // target and inline-splice source differ).
                            InlineBodies = findStringList core "inline-bodies" |> Option.defaultValue impl
                        }

    /// Read + parse the manifest at `manifestPath` (the path to a `manifest.toml`).
    let loadManifest (manifestPath: string) : Result<Manifest, string> =
        if not (File.Exists manifestPath) then
            Error(sprintf "Manifest not found: %s" manifestPath)
        else
            let dirName =
                Path.GetFileName(Path.TrimEndingDirectorySeparator(Path.GetDirectoryName manifestPath))

            match Toml.parse (File.ReadAllText manifestPath) with
            | Error e -> Error(sprintf "Manifest parse error (%s): %s" manifestPath e)
            | Ok doc -> parseManifest dirName doc

    /// Resolve a `depends-on` package name to its `manifest.toml` path, relative
    /// to a dependent manifest's location. By the package-split-plan convention a
    /// package's directory name *is* its identity (`src/Vesper.Core` ⇒
    /// `"Vesper.Core"`, the `Manifest.Name` fallback), so a dependency
    /// `"Vesper.Core"` of the manifest at `src/Vesper.List/manifest.toml` lives at
    /// the sibling `src/Vesper.Core/manifest.toml`. Mirrors the package-build
    /// harness's `srcManifest`.
    let private dependencyManifestPath (dependentManifestPath: string) (dependencyName: string) : string =
        let packageDir = Path.GetDirectoryName dependentManifestPath
        let srcDir = Path.GetDirectoryName packageDir
        Path.Combine(srcDir, dependencyName, "manifest.toml")

    /// Close `rootManifests` over `[core] depends-on` and return every reachable
    /// manifest path in **dependency order** — each package appears *after* all the
    /// packages it depends on. Paths are
    /// normalised (`Path.GetFullPath`) and de-duplicated, so a dependency named by
    /// several roots (every Vesper package's `Vesper.Core`) is processed once. A
    /// `depends-on` cycle is a hard error — contract packages may not be mutually
    /// recursive — as is a `depends-on` naming a package whose manifest is absent.
    ///
    /// The returned order is the priority order callers stack into the composite
    /// provider; building bottom-up is what later lets each package's extraction
    /// read its dependencies' already-built type shapes. For an
    /// input that is already dependency-ordered the order is returned unchanged
    /// (the sort is stable over the discovery order).
    ///
    /// Core closure: returns the dependency-ordered paths **and** the direct
    /// `depends-on` adjacency (normalised path → its direct dependency paths) so a
    /// caller can scope a package's ambient to its declared dependencies rather
    /// than all topological predecessors. `buildClosure` /
    /// `buildClosureWithDeps` are the public projections.
    let private closeAndOrder
        (rootManifests: string list)
        : Result<string list * System.Collections.Generic.Dictionary<string, string list>, string> =
        let norm (p: string) = Path.GetFullPath p

        // key (normalised path) → its dependency keys (adjacency) and package name.
        // `discovered` is the order nodes were first reached (roots, then their
        // deps), which the topo sort below walks so an already-ordered input is
        // returned unchanged.
        let dependencies =
            System.Collections.Generic.Dictionary<string, string list>(System.StringComparer.Ordinal)

        let names =
            System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

        let discovered = ResizeArray<string>()
        let mutable error = None

        let rec load (path: string) =
            if error.IsSome then
                ()
            else
                let key = norm path

                if dependencies.ContainsKey key then
                    ()
                else
                    match loadManifest key with
                    | Error e -> error <- Some(sprintf "buildClosure: %s" e)
                    | Ok manifest ->
                        let depPaths =
                            manifest.DependsOn
                            |> List.map (fun dep -> norm (dependencyManifestPath key dep))

                        dependencies.[key] <- depPaths
                        names.[key] <- manifest.Name
                        discovered.Add key

                        for depPath in depPaths do
                            load depPath

        for root in rootManifests do
            load root

        match error with
        | Some e -> Error e
        | None ->
            // Post-order DFS over the discovery order: a node is emitted only after
            // its dependencies, so the result lists dependencies before dependents.
            // The gray/black colouring (1 = on the current stack, 2 = emitted)
            // rejects a `depends-on` cycle.
            let ordered = ResizeArray<string>()

            let state =
                System.Collections.Generic.Dictionary<string, int>(System.StringComparer.Ordinal)

            let mutable cycle = None

            let rec visit (node: string) =
                if cycle.IsSome then
                    ()
                else
                    match state.TryGetValue node with
                    | true, 2 -> ()
                    | true, _ ->
                        cycle <- Some(sprintf "buildClosure: dependency cycle through package '%s'" names.[node])
                    | _ ->
                        state.[node] <- 1

                        for dep in dependencies.[node] do
                            visit dep

                        state.[node] <- 2
                        ordered.Add node

            for node in discovered do
                visit node

            match cycle with
            | Some e -> Error e
            | None -> Ok(List.ofSeq ordered, dependencies)

    /// Close `rootManifests` over `[core] depends-on` and return every reachable
    /// manifest path in dependency order (see `closeAndOrder`).
    let buildClosure (rootManifests: string list) : Result<string list, string> =
        closeAndOrder rootManifests |> Result.map fst

    /// Like `buildClosure`, but also returns each package's **transitive**
    /// `depends-on` closure: a function from a normalised manifest path to the
    /// normalised paths it depends on, directly or transitively (excluding itself).
    /// Lets a caller scope a package's extraction ambient to exactly its declared
    /// dependencies instead of every topological predecessor. A path with no
    /// dependencies (or an unknown path) maps to the empty list.
    let buildClosureWithDeps (rootManifests: string list) : Result<string list * (string -> string list), string> =
        match closeAndOrder rootManifests with
        | Error e -> Error e
        | Ok(ordered, adjacency) ->
            // Transitive closure computed in dependency order: each package's
            // closure is the union of its direct deps and those deps' already-
            // computed closures. Each closure is itself **topologically ordered**
            // (a dependency precedes anything that depends on it) — for a direct
            // dep we emit that dep's own closure *before* the dep, so the indexed
            // lookup in `composeProviders` yields a deterministic, dependency-first
            // provider list. Each path appears once.
            let transitive =
                System.Collections.Generic.Dictionary<string, string list>(System.StringComparer.Ordinal)

            for key in ordered do
                let acc = ResizeArray<string>()
                let seen = System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)

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

            let lookup (key: string) =
                match transitive.TryGetValue key with
                | true, v -> v
                | _ -> []

            Ok(ordered, lookup)

    /// Wrap the extractor's provider so (a) every resolved descriptor carries
    /// the package `Origin` (the extractor records `SymbolOrigin.Empty`; the
    /// manifest knows the assembly + namespace),
    /// and (b) the package's implicit prelude — its `[<AutoOpen>]` modules plus
    /// the namespace itself — is surfaced as `IAmbientOpenScope`. Short-name
    /// resolution is *not* a provider-internal retry any more: the pipeline
    /// seeds these `ambient` prefixes into the open scope and probes them
    /// BEHIND explicit `open`s, so an explicit `open` can shadow a prelude
    /// name. The actual
    /// composition / stamping / `IAmbientOpenScope` plumbing is the shared
    /// `ExternalSymbols.stack` primitive — `wrap` is a 1-source instantiation
    /// of it with origin stamping.
    let private wrap
        (origin: SymbolOrigin)
        (ambient: string list)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        ExternalSymbols.stack (ValueSome origin) ambient [ inner ]

    /// Stand up a referenced project (layer 1) from its `manifest.toml`, with
    /// read access to its dependencies' already-built type shapes
    /// (`ambientShapes`). Parse each
    /// contract `.fsi` in `files` order into one accumulating `ExtractCtx` seeded
    /// with `ambientShapes`, then expose it as a provider whose symbols carry the
    /// package `Origin`. The ambient is consulted during extraction (through
    /// `ExtractCtx.shapeOf`) to kind a cross-package nominal head; it MUST be set
    /// before the file walk, since signature translation runs inside it.
    /// Returns per-file parse diagnostics alongside the provider (a file that
    /// fails to parse contributes no symbols but does not abort the build).
    let buildProviderWith
        (ambientShapes: string -> ExternalTypeShape voption)
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        match loadManifest manifestPath with
        | Error e -> Error e
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let ctx = VesperLib.ExtractCtx.empty ()
            ctx.AmbientShapes <- ambientShapes
            // The package's own home assembly, so `mkNominal` stamps it onto own-type
            // keys whose extraction-time origin is still Empty (Phase 6) — matching
            // the `Some manifest.Name` origin the `wrap` below stamps for consumers.
            ctx.HomeAssembly <- Some manifest.Name

            // Pair `.fsi` extern + `.fs` `(# … #)`: harvest the per-target
            // intrinsic reprs from each contract's sibling `.fs` companion FIRST,
            // so the `extern` arm of the `.fsi` extraction below publishes a
            // matched primitive as `ExternalTypeShape.Intrinsic repr` rather than
            // an opaque `Class`. The `.fs` is the only place the repr lives
            // (the `.fsi` commits `type exn = extern`, no repr) —
            // Moved here from the codegen-layer harvest.
            for rel in manifest.Files do
                let fsRel = Path.ChangeExtension(rel, ".fs")
                let abs = Path.Combine(dir, fsRel)

                if File.Exists abs then
                    let fsFile: VesperLib.LibFile =
                        {
                            BucketName = manifest.Name
                            Relative = fsRel
                            Absolute = abs
                        }

                    match VesperLib.parseFileFull fsFile with
                    | Error _ -> ()
                    | Ok parsed -> VesperLib.harvestIntrinsicReprs ctx parsed

            for rel in manifest.Files do
                let file: VesperLib.LibFile =
                    {
                        BucketName = manifest.Name
                        Relative = rel
                        Absolute = Path.Combine(dir, rel)
                    }

                match VesperLib.parseFileFull file with
                | Error e -> ctx.Diagnostics.Add(file, e)
                | Ok parsed -> VesperLib.extractSymbols ctx parsed

            let origin =
                {
                    Assembly = Some manifest.Name
                    Namespace = manifest.Namespace
                    DeclaringType = None
                }

            // The contract's implicit prelude: its `[<AutoOpen>]` modules (most
            // specific, e.g. `Vesper.ArithmeticOperators`) ahead of the package
            // namespace itself (`Vesper`, so `int` finds `Vesper.int`). Both are
            // probed behind explicit `open`s.
            let ambient =
                List.ofSeq ctx.AutoOpenPrefixes
                @ (if manifest.Namespace.Length > 0 then
                       [ manifest.Namespace ]
                   else
                       [])

            Ok(wrap origin ambient (VesperLib.ExtractCtx.toProvider ctx), List.ofSeq ctx.Diagnostics)

    /// Stand up a referenced project in isolation — no dependency shapes in scope
    /// (`AmbientShapes` defaults to "resolve nothing"). The dependency-free path:
    /// a package with no `depends-on`, and the entry point tests build a single
    /// package from. Dependency-aware composition uses `buildProviderWith`
    /// directly (`SymbolProviders.composeProviders`).
    let buildProvider
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        buildProviderWith (fun _ -> ValueNone) manifestPath

    /// Lazy cache keyed by the (normalised) manifest path so repeated callers
    /// parse a package's `.fsi` set at most once. Mirrors `VesperLib.defaultProvider`.
    let private cached =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string>>
         >(
            System.StringComparer.Ordinal
        )

    /// Production-path entry point: caches `buildProvider` per manifest path.
    /// Tests that need a fresh provider should call `buildProvider`.
    let provider (manifestPath: string) : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        let normalised = Path.GetFullPath manifestPath
        cached.GetOrAdd(normalised, (fun p -> lazy (buildProvider p))).Value
