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
            /// Per-target `impl` overrides: every `impl-<t>` key (e.g. `impl-js`),
            /// keyed by the bare suffix `<t>` ("js"). SemanticAnalysis stores these
            /// inertly and never enumerates target names — each *backend* asks for
            /// its own suffix via `resolveImpl`, falling back to the base `Impl` when
            /// absent. A target's `.fs` bodies diverge from the CLR ones only where
            /// they carry platform IL (Vesper.Core / .Comparison / .Array); the rest
            /// of the library is target-neutral and needs no override.
            ImplOverrides: Map<string, string list>
            /// Per-target `inline-bodies` overrides: every `inline-bodies-<t>` key,
            /// keyed by the bare suffix `<t>`. Mirror of `ImplOverrides` for the
            /// inline-splice source; resolved by `resolveInlineBodies`, falling back
            /// to the base `InlineBodies`.
            InlineBodiesOverrides: Map<string, string list>
            /// Per-target EXTRA contract `.fsi` files APPENDED to `Files` for a target
            /// (`files-<t>`), keyed by suffix. Unlike `ImplOverrides` (which REPLACE), these
            /// APPEND after the base contract. Used by the JS capability compat shim
            /// (`capabilities-compat.js.fsi`). Resolved by `resolveExtraFiles`.
            FilesOverrides: Map<string, string list>
            /// Per-target runtime *asset* modules: every `runtime-<t>` key (e.g.
            /// `runtime-js`), keyed by the bare suffix `<t>`. Unlike `Impl` /
            /// `InlineBodies` these are NOT `.fsi`/`.fs` sources the front end parses
            /// — they are hand-authored platform-support artifacts (the JS `.mjs`
            /// runtime, the analogue of Vesper.Printf's committed DLL) the backend
            /// ships beside its output and resolves via `runtimeModules`. There is no
            /// base `runtime` key (a runtime asset is inherently target-specific), so
            /// an absent key yields nothing (`resolveRuntime`).
            RuntimeOverrides: Map<string, string list>
            /// Contract `.fsi` files that are DELIBERATELY impl-free on the base
            /// (CLR) target (`[core] sig-only`) — a front-end intrinsic lowered
            /// inline (`printf.fsi`), an FSharp.Core-interop type whose self-host is
            /// sequenced later (`printf-format.fsi`), or a per-target/BCL-resolved
            /// contract (`exceptions.fsi`). The conformance pass treats a `SigOnly`
            /// `.fsi` listed here as an accepted exemption; one NOT listed is the
            /// FS0240 analogue — a hard error (T8 Step 5). Per-target overrides
            /// (`sig-only-<t>`) REPLACE the base, like `impl`.
            SigOnly: string list
            /// Per-target `sig-only` overrides, keyed by suffix; resolved by
            /// `resolveSigOnly`, falling back to the base `SigOnly`.
            SigOnlyOverrides: Map<string, string list>
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

    /// Collect every `<prefix>-<t>` key from a `[core]` table into a
    /// `suffix -> string list` map (`impl-js` -> "js"). Target-agnostic: the parser
    /// records whatever suffixes are present without knowing the target set.
    let private collectOverrides (core: TomlTable) (prefix: string) : Map<string, string list> =
        let dash = prefix + "-"

        core
        |> Map.toSeq
        |> Seq.choose (fun (key, value) ->
            if key.StartsWith dash then
                match value with
                | TomlValue.Array xs -> Some(key.Substring dash.Length, xs |> List.choose asString)
                | _ -> None
            else
                None
        )
        |> Map.ofSeq

    /// Resolve the `impl` file list for an optional target suffix: the target's
    /// override if present, else the base `impl`. `None` (and any suffix with no
    /// override) yields the base list — the CLR path is `resolveImpl None`.
    let resolveImpl (target: string option) (m: Manifest) : string list =
        match target with
        | Some t -> m.ImplOverrides |> Map.tryFind t |> Option.defaultValue m.Impl
        | None -> m.Impl

    /// Resolve the `inline-bodies` file list for an optional target suffix; mirror
    /// of `resolveImpl`. The future JS backend calls `resolveInlineBodies (Some "js")`;
    /// the CLR backend keeps reading `m.InlineBodies` (i.e. `resolveInlineBodies None`).
    let resolveInlineBodies (target: string option) (m: Manifest) : string list =
        match target with
        | Some t -> m.InlineBodiesOverrides |> Map.tryFind t |> Option.defaultValue m.InlineBodies
        | None -> m.InlineBodies

    /// Resolve the per-target EXTRA `.fsi` files appended to `Files` (`files-<t>`).
    /// APPEND semantics (contrast `resolveImpl`'s REPLACE), so a shim may reference a
    /// base-declared type — the JS compat abbreviations name `Vesper.disposable` from
    /// the base `capabilities.fsi`. `None` / no `files-<t>` key appends nothing.
    let resolveExtraFiles (target: string option) (m: Manifest) : string list =
        match target with
        | Some t -> m.FilesOverrides |> Map.tryFind t |> Option.defaultValue []
        | None -> []

    /// Resolve the `runtime-<t>` asset-module file list for a target suffix. Unlike
    /// `resolveImpl` / `resolveInlineBodies` there is NO base list — a runtime asset
    /// (the JS `.mjs`) is inherently target-specific — so an absent key (or `None`)
    /// yields the empty list.
    let resolveRuntime (target: string option) (m: Manifest) : string list =
        match target with
        | Some t -> m.RuntimeOverrides |> Map.tryFind t |> Option.defaultValue []
        | None -> []

    /// Resolve the `sig-only` impl-free exemption `.fsi` list for a target suffix;
    /// mirror of `resolveImpl` (REPLACE). The conformance pass (`ConformancePass.enforce`)
    /// reads this to distinguish a legitimately impl-free contract from a missing
    /// implementation (the FS0240 hard error).
    let resolveSigOnly (target: string option) (m: Manifest) : string list =
        match target with
        | Some t -> m.SigOnlyOverrides |> Map.tryFind t |> Option.defaultValue m.SigOnly
        | None -> m.SigOnly

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
                            // Per-target overrides are inert here: any `impl-<t>` /
                            // `inline-bodies-<t>` key is captured by suffix and resolved
                            // by the *backend* (`resolveImpl`/`resolveInlineBodies`).
                            ImplOverrides = collectOverrides core "impl"
                            InlineBodiesOverrides = collectOverrides core "inline-bodies"
                            FilesOverrides = collectOverrides core "files"
                            RuntimeOverrides = collectOverrides core "runtime"
                            SigOnly = findStringList core "sig-only" |> Option.defaultValue []
                            SigOnlyOverrides = collectOverrides core "sig-only"
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
    /// to a dependent manifest's location. By convention a
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

    /// Resolve the per-target runtime *asset* modules (`runtime-<t>`) for a manifest
    /// set, closed over `depends-on`, reading each file's contents from disk. Maps
    /// each package/assembly name (`Manifest.Name`) to its `(fileName, source)` —
    /// the hand-authored platform-support module (the JS `.mjs`) the backend
    /// materialises beside the output and imports by `./<fileName>`. A package with
    /// no `runtime-<t>` key contributes nothing; one module per package (only the
    /// first listed is taken — the import specifier keys one file per assembly); a
    /// later package wins a name clash. This is the seam Route B (a backend-compiled
    /// runtime, the `--compiling-fslib` bootstrap) later slots into — it generates
    /// the same module the backend ships, leaving import + materialise unchanged.
    let runtimeModules (target: string) (rootManifests: string list) : Map<string, string * string> =
        match buildClosure rootManifests with
        | Error _ -> Map.empty
        | Ok ordered ->
            let mutable acc = Map.empty

            for manifestPath in ordered do
                match loadManifest manifestPath with
                | Error _ -> ()
                | Ok manifest ->
                    match resolveRuntime (Some target) manifest with
                    | rel :: _ ->
                        let abs = Path.Combine(Path.GetDirectoryName manifestPath, rel)

                        if File.Exists abs then
                            acc <- Map.add manifest.Name (Path.GetFileName rel, File.ReadAllText abs) acc
                    | [] -> ()

            acc

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
    /// `ExternalSymbolProviders.stack` primitive — `wrap` is a 1-source instantiation
    /// of it with origin stamping.
    let private wrap
        (origin: SymbolOrigin)
        (ambient: string list)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        ExternalSymbolProviders.stack (ValueSome origin) ambient [ inner ]

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
    /// The `.fs` companion to harvest a contract `.fsi`'s intrinsic repr from, for
    /// an optional backend target. A target's `<base>.<target>.fs` (`prim-types-exn.js.fs`,
    /// `exn → Error`) wins over the base `<base>.fs` (`exn → System.Exception`) when it
    /// exists — the intrinsic-repr analogue of the manifest's `inline-bodies-<t>`
    /// override. `None`, or a target with no override file,
    /// falls back to the base companion.
    /// The base `.fs` companion (`prim-types-exn.fsi` ⇒ `prim-types-exn.fs`) — the
    /// primitive *marker* + the CLR platform repr.
    let private baseFs (dir: string) (fsiRel: string) : string =
        Path.Combine(dir, Path.ChangeExtension(fsiRel, ".fs"))

    /// The per-target override companion (`prim-types-exn.fsi`, target `js` ⇒
    /// `prim-types-exn.js.fs`) — `Some` ONLY when a distinct file exists, so the
    /// caller harvests the override without re-parsing the base as a fallback. `None`
    /// on the base target (CLR) or when a primitive ships no companion for this target.
    let private targetOverrideFs (target: string option) (dir: string) (fsiRel: string) : string option =
        match target with
        | Some t ->
            // `ChangeExtension("prim-types-exn.fsi", "js.fs")` → `prim-types-exn.js.fs`.
            let abs = Path.Combine(dir, Path.ChangeExtension(fsiRel, t + ".fs"))
            if File.Exists abs then Some abs else None
        | None -> None

    /// A package built by `buildProviderWith`, plus the census the composition-time
    /// duplicate sweep reads. `DeclaredTypeNames` are the qualified compiled names of the
    /// NOMINAL types this package OWNS (Class/Record/Union/Enum — the shapes that mint a
    /// lookup key and would silently first-hit-shadow a peer package's same-named type).
    /// Intrinsics and capability faces are asm-blind by design (`sameTypeAsmBlind`) and so
    /// are deliberately excluded — a shared canon there is not a collision.
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: (VesperLib.LibFile * string) list
            HomeAssembly: string
            DeclaredTypeNames: string list
        }

    let buildProviderWith
        (target: string option)
        (ambientShapes: string -> ExternalTypeShape voption)
        (dependencyAmbientPrefixes: string list)
        (manifestPath: string)
        : Result<BuiltPackage, string> =
        match loadManifest manifestPath with
        | Error e -> Error e
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let ctx = VesperLib.ExtractCtx.empty ()
            ctx.AmbientShapes <- ambientShapes
            // The dependency composite's ambient open prefixes (`Vesper`, …) so this
            // package's own extraction resolves a dependency's ambiently-available
            // type by bare name (`Fun`2` / `Fun`3`), the way the consumer front end does.
            ctx.DependencyAmbientPrefixes <- dependencyAmbientPrefixes
            // The package's own home assembly, so `mkNominal` stamps it onto own-type
            // keys whose extraction-time origin is still Empty — matching
            // the `Some manifest.Name` origin the `wrap` below stamps for consumers.
            ctx.HomeAssembly <- Some manifest.Name

            // Pair `.fsi` extern + `.fs` `(# … #)`: harvest the intrinsic reprs
            // from each contract's sibling `.fs` companion FIRST, so the `extern`
            // arm of the `.fsi` extraction below publishes a matched primitive as
            // `ExternalTypeShape.Intrinsic` rather than an opaque `Class`. The `.fs`
            // is the only place the repr lives (the `.fsi` commits `type exn =
            // extern`, no repr) — moved here from the codegen-layer harvest.
            //
            // Two repr faces:
            //  - the BASE `.fs` ⇒ `IntrinsicBaseReprs`: the primitive *marker* (its
            //    presence is what publishes the `extern` as an `Intrinsic`, not a
            //    `Class`) and, on CLR, the platform repr itself.
            //  - the per-target `<base>.<target>.fs` override ⇒ `IntrinsicReprs`: the
            //    `platform` face for THIS target (`prim-types-int.js.fs` ⇒ `number`).
            //    On CLR there is no override, so the base repr also feeds `IntrinsicReprs`.
            // A primitive the target OMITS (`decimal` ships no `.js.fs`) is in
            // `IntrinsicBaseReprs` but NOT `IntrinsicReprs`, so it stays an `Intrinsic`
            // with `platform = None` rather than falling back to a BCL repr that has no
            // JS runtime. The `canon` face is the `.fsi` name itself (set at the `extern`
            // arm), so the override never moves the unifier's identity key.
            let harvestCompanion (dest: System.Collections.Generic.Dictionary<string, string>) (abs: string) =
                let fsFile: VesperLib.LibFile =
                    {
                        BucketName = manifest.Name
                        Relative = Path.GetFileName abs
                        Absolute = abs
                    }

                match VesperLib.parseFileFull fsFile with
                | Error _ -> ()
                | Ok parsed -> VesperLib.harvestIntrinsicReprsInto dest parsed

            for rel in manifest.Files do
                let baseAbs = baseFs dir rel

                if File.Exists baseAbs then
                    harvestCompanion ctx.IntrinsicBaseReprs baseAbs

                    match targetOverrideFs target dir rel with
                    | Some overrideAbs -> harvestCompanion ctx.IntrinsicReprs overrideAbs
                    | None ->
                        // Base target (CLR), or no per-target companion: the base repr
                        // IS the platform face. Reuse the just-harvested base marker
                        // rather than re-parsing the file.
                        ()

            // Target-only EXTRA contracts (`files-<t>`) may declare an intrinsic that
            // exists ONLY on that target (`undefined`/`null` — JS-only, no CLR analog and
            // so no base `.fs`). Such a type has no base/override split: its single
            // `<base>.<t>.fs` companion is BOTH the marker (→ `IntrinsicBaseReprs`, so the
            // `extern` publishes as an `Intrinsic` not an opaque `Class`) AND the JS
            // platform face (→ `IntrinsicReprs`). `File.Exists` skips a shim `.fsi` with
            // no `.fs` companion (`capabilities-compat.js.fsi`, `ops-platform-runtime.js.fsi`).
            for rel in resolveExtraFiles target manifest do
                let companionAbs = baseFs dir rel

                if File.Exists companionAbs then
                    harvestCompanion ctx.IntrinsicBaseReprs companionAbs
                    harvestCompanion ctx.IntrinsicReprs companionAbs

            // CLR (and any target whose primitive has no override): the base repr is the
            // platform face. Seed `IntrinsicReprs` from the base markers WITHOUT a
            // second parse; a real per-target override (harvested above) already shadows
            // its entry, so this only fills the gaps.
            match target with
            | None ->
                for KeyValue(k, v) in ctx.IntrinsicBaseReprs do
                    if not (ctx.IntrinsicReprs.ContainsKey k) then
                        ctx.IntrinsicReprs.[k] <- v
            | Some _ -> ()

            // Base contract files, then this target's APPENDED shim files (`files-<t>`),
            // so a shim's RHS (`Vesper.disposable`) is already in the registry. Base / CLR
            // appends nothing; harvest (above) is unaffected — a compat `.fsi` has no `.fs`.
            for rel in manifest.Files @ resolveExtraFiles target manifest do
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

            // The nominal types this package declares (own shapes only — `shapeOf`
            // consults dependency `AmbientShapes` as a fallback but never inserts them
            // into `TypeShapes`), for the composition-time duplicate sweep. EXHAUSTIVE
            // over the shape cases so a NEW shape forces an include/exclude decision
            // here: intrinsics and capability interfaces are asm-blind by design (every
            // package's `int` is THE `int`, so cross-package repetition is the norm,
            // not a collision); an `Abbrev` shadow silently re-points an alias and an
            // `Opaque` shadow hides a residue, so both ARE swept.
            let declaredTypeNames =
                [
                    for kv in ctx.TypeShapes do
                        match kv.Value with
                        | ExternalTypeShape.Class _
                        | ExternalTypeShape.Record _
                        | ExternalTypeShape.Union _
                        | ExternalTypeShape.Enum _
                        | ExternalTypeShape.Abbrev _
                        | ExternalTypeShape.Opaque _ -> yield kv.Key
                        | ExternalTypeShape.Intrinsic _
                        | ExternalTypeShape.IntrinsicInterface _ -> ()
                ]

            Ok
                {
                    Provider = wrap origin ambient (VesperLib.ExtractCtx.toProvider ctx)
                    Diagnostics = List.ofSeq ctx.Diagnostics
                    HomeAssembly = manifest.Name
                    DeclaredTypeNames = declaredTypeNames
                }

    /// Stand up a referenced project in isolation — no dependency shapes in scope
    /// (`AmbientShapes` defaults to "resolve nothing"). The dependency-free path:
    /// a package with no `depends-on`, and the entry point tests build a single
    /// package from. Dependency-aware composition uses `composeOrdered`.
    let buildProvider
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        buildProviderWith None (fun _ -> ValueNone) [] manifestPath
        |> Result.map (fun bp -> bp.Provider, bp.Diagnostics)

    /// A layer-2 metadata-tail factory: given the harvested `{ platform-repr →
    /// [canon] }` reverse map of the layer-1 providers composed so far, produce the
    /// trailing leaf providers. `composeOrdered` is leaf-AGNOSTIC — a backend injects
    /// its BCL `MetadataSymbols` / JS-native tail; an in-assembly caller that needs no
    /// metadata passes `noMetaTail`.
    type MetaTailFactory = Map<string, SymbolKey list> -> IExternalSymbolProvider list

    /// The empty layer-2 tail: the layer-1 `.fsi` contracts alone, no metadata leaf.
    /// For an in-assembly caller (a test fixture front-ending source against the real
    /// `Vesper.*` contracts) that resolves no BCL/native metadata.
    let noMetaTail: MetaTailFactory = fun _ -> []

    /// Compose layer-1 providers in dependency (topological) order ahead of the
    /// `metaTail` leaf. Each package is extracted with read access to its transitive
    /// `depends-on` closure's shapes (and that closure's ambient open prefixes), so a
    /// cross-package nominal head kinds at bake time. `orderedManifestPaths` /
    /// `transitiveDeps` come from `buildClosureWithDeps`.
    ///
    /// This is the single dependency-order wiring shared by the codegen
    /// `SymbolProviders` stack and the in-assembly test fixtures — do not re-implement
    /// the ambient-shape loop at a call site.
    let composeOrdered
        (metaTail: MetaTailFactory)
        (target: string option)
        (orderedManifestPaths: string list)
        (transitiveDeps: string -> string list)
        : IExternalSymbolProvider =
        // Final stack, in build (topological) order; `byPath` indexes each built
        // provider by its normalised manifest path so a package's dependency providers
        // resolve in O(closure).
        let built = ResizeArray<IExternalSymbolProvider>()

        let byPath =
            System.Collections.Generic.Dictionary<string, IExternalSymbolProvider>(System.StringComparer.Ordinal)

        // Composition-time duplicate sweep: a qualified type key declared twice in the
        // referenced set resolves as a silent first-hit shadow (`composite` → `firstHit`),
        // so the loser's type is minted a correct key but is unreachable by lookup. Refuse
        // the ambiguity here — a CS0433-equivalent. ANY second sighting is a collision:
        // one package never declares a key twice (`TypeShapes` is a map), so a repeat is
        // either two peer packages sharing a namespace+name, or two copies/versions of
        // one package (same `manifest.Name` at different paths) — both are exactly the
        // shadowing this sweep exists to refuse, so no home-assembly comparison waives
        // either. The package-vs-metadata-tail overlap is NOT swept (the BCL/native
        // tail is not enumerable) — that case is diagnosed lazily downstream.
        let seenTypeHomes =
            System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

        for path in orderedManifestPaths do
            let key = Path.GetFullPath path

            let depProviders =
                transitiveDeps key
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            // The per-package extraction leaf is the SAME injected `metaTail` factory as
            // the final composite — this layer never names a concrete provider. Seeded
            // with the reverse map of the deps built so far so a dependency's BCL member
            // sigs canonicalize during extraction.
            let depComposite =
                ExternalSymbolProviders.composite (
                    depProviders @ metaTail (ExternalSymbolProviders.mergeReverseCanon depProviders)
                )

            let ambientShapes = (fun (name: string) -> depComposite.TryLookupType name)

            // The dependency providers' implicit open prefixes (`Vesper` from Core,
            // where `Fun`2`/`Fun`3`/`Ref` live), so this package's extraction resolves a
            // dependency's ambiently-available type by bare name — mirroring the consumer
            // composite's `AmbientOpenPrefixes`. Dedup, dependency order.
            let depAmbientPrefixes =
                depProviders |> List.collect (fun p -> p.AmbientOpenPrefixes) |> List.distinct

            match buildProviderWith target ambientShapes depAmbientPrefixes path with
            | Ok bp ->
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
                byPath.[key] <- bp.Provider
            | Error e -> failwithf "Failed to load referenced project manifest '%s': %s" path e

        // The final composite's leaf IS seeded with the full harvested reverse map, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = List.ofSeq built
        ExternalSymbolProviders.composite (builtList @ metaTail (ExternalSymbolProviders.mergeReverseCanon builtList))

    /// `composeOrdered` over a raw manifest set, ordering it (and computing each
    /// package's transitive `depends-on` closure) via `buildClosureWithDeps`. A cycle
    /// or missing dependency is a hard error. Callers that also need the ordered list
    /// for a second pass (e.g. inline bodies) should call `buildClosureWithDeps` +
    /// `composeOrdered` directly to avoid ordering twice.
    let composeContract
        (metaTail: MetaTailFactory)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        match buildClosureWithDeps manifestPaths with
        | Ok(ordered, transitiveDeps) -> composeOrdered metaTail target ordered transitiveDeps
        | Error e -> failwithf "Failed to order referenced project manifests: %s" e

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
