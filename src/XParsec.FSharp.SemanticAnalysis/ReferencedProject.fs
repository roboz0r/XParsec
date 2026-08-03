namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// Layer 1 of the symbol-resolution stack: a
/// *referenced project*, declared by its `manifest.toml`. A package's `[core]`
/// table lists its contract `.fsi` files in compile
/// order; this module parses each into one accumulating `ExtractCtx` (reusing
/// `VesperLib`'s extractor) and exposes the result as an `IExternalSymbolProvider`
/// whose symbols carry the package's home assembly.
///
/// A manifest declares no namespace: a symbol's namespace is its FILE's `namespace`
/// header, which the extractor already reads into every key it mints, and a package
/// may declare as many as it has files (`Vesper.Core` declares three).
///
/// The `.fsi` is the *target-agnostic contract* (`type int = extern`); the
/// matching `.fs` is the *per-target binding* (`type int = (# "System.Int32" #)`).
/// Resolution needs only the `.fsi`; the absent `.fs` is a codegen-side concern
/// not a resolution failure here.
module ReferencedProject =

    /// The `[core]` lists — what a package declares once for EVERY target. No `Runtime`
    /// field: a runtime asset is a per-target artifact, and a record that cannot spell a
    /// shared one says so better than a conventionally-empty field would.
    type SharedLists =
        {
            /// Target-neutral contract `.fsi` files in compile order (`[core] files`).
            Files: string list
            /// Target-neutral `.fs` bodies compiled into the package DLL (`[core] impl`).
            Impl: string list
            /// Target-neutral `.fs` bodies whose module-level `let inline` bindings are
            /// spliced across the package boundary (`[core] inline-bodies`). Absent
            /// EVERYWHERE in a manifest means "the impl files are themselves the splice
            /// source"; the two lists diverge only where the CLR backend cannot yet
            /// COMPILE a splice source (Vesper.Core's operator files: the emitter cannot
            /// reference `Vesper.Fun` from inside the assembly that defines it, and a
            /// prior file's type abbreviations are not exported to a later one).
            InlineBodies: string list
            /// Contract `.fsi` files that are DELIBERATELY impl-free (`[core] sig-only`)
            /// — a front-end intrinsic lowered inline (`printf.fsi`), an
            /// FSharp.Core-interop type whose self-host is sequenced later
            /// (`printf-format.fsi`), or a BCL-resolved contract (`exceptions.fsi`). The
            /// conformance pass accepts a `SigOnly` `.fsi` listed here; one NOT listed is
            /// the FS0240 analogue — a hard error.
            SigOnly: string list
        }

    module SharedLists =
        let empty: SharedLists =
            {
                Files = []
                Impl = []
                InlineBodies = []
                SigOnly = []
            }

    /// One `[targets.<t>]` table. Every list here is APPENDED to its `SharedLists` peer —
    /// uniformly, with no REPLACE anywhere, because no target overrides a base that was
    /// secretly another target's.
    type TargetLists =
        {
            /// Target-only EXTRA contracts, after the shared ones (the JS capability
            /// compat shim, the JS-only `undefined`/`dynamic` intrinsics).
            Files: string list
            /// Target-only `.fs` bodies, after the shared ones.
            Impl: string list
            /// Target-only splice sources, after the shared ones.
            InlineBodies: string list
            /// Target-only impl-free contract exemptions, after the shared ones.
            SigOnly: string list
            /// Hand-authored runtime *asset* modules — NOT `.fsi`/`.fs` sources the front
            /// end parses, but platform-support artifacts (the JS `.mjs`) the backend
            /// ships beside its output and resolves via `runtimeModules`.
            Runtime: string list
        }

    module TargetLists =
        let empty: TargetLists =
            {
                Files = []
                Impl = []
                InlineBodies = []
                SigOnly = []
                Runtime = []
            }

    /// A parsed package `manifest.toml`: the `[core]` table's target-neutral lists plus
    /// one `[targets.<t>]` table per target the package participates in. The CLR is an
    /// ordinary key here, not an unnamed base.
    type Manifest =
        {
            /// Package / assembly simple name — `[core] name` when present, else
            /// the manifest's directory name (`src/Vesper.Core` ⇒ `"Vesper.Core"`).
            /// `Vesper.Core`/`Vesper.Printf` omit `name`; the dir name is the
            /// package identity in both cases.
            Name: string
            /// Other packages this one depends on (`[core] depends-on`) — the
            /// package names whose DLLs/contracts must be built/referenced first.
            /// Drives the package-build harness's recursive dependency resolution.
            DependsOn: string list
            Shared: SharedLists
            /// `[targets.<t>]` by target name. The keys ARE the target set this package
            /// participates in — there is no registry of target names elsewhere.
            Targets: Map<string, TargetLists>
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

    /// The `[targets.<t>]` table for `target`, or an all-empty one — a target a manifest
    /// says nothing about contributes nothing, so it resolves to exactly the shared lists.
    let private listsFor (target: string) (m: Manifest) : TargetLists =
        m.Targets |> Map.tryFind target |> Option.defaultValue TargetLists.empty

    /// The contract `.fsi` files for `target`: shared first (so a target extra may name a
    /// type the shared contract declares), then the target's own.
    let resolveFiles (target: string) (m: Manifest) : string list =
        m.Shared.Files @ (listsFor target m).Files

    /// The `.fs` bodies the package DLL compiles for `target`. Shared, then the target's own.
    let resolveImpl (target: string) (m: Manifest) : string list =
        m.Shared.Impl @ (listsFor target m).Impl

    /// The `.fs` bodies whose `let inline` templates are spliced across the package
    /// boundary for `target`. A manifest that names NO splice source anywhere says the
    /// impl files ARE the splice source — the common case — so the fallback is on the
    /// whole manifest, never per target (a target that deliberately splices nothing then
    /// says so with an explicit empty list, which is indistinguishable from silence and is
    /// why the fallback cannot be per-target).
    let resolveInlineBodies (target: string) (m: Manifest) : string list =
        let declared =
            not (List.isEmpty m.Shared.InlineBodies)
            || m.Targets |> Map.exists (fun _ t -> not (List.isEmpty t.InlineBodies))

        if declared then
            m.Shared.InlineBodies @ (listsFor target m).InlineBodies
        else
            resolveImpl target m

    /// The impl-free contract exemptions for `target`. Shared, then the target's own.
    let resolveSigOnly (target: string) (m: Manifest) : string list =
        m.Shared.SigOnly @ (listsFor target m).SigOnly

    /// The runtime *asset* modules for `target`. No shared peer: a runtime asset is
    /// inherently target-specific (the CLR builds a DLL rather than committing one).
    let resolveRuntime (target: string) (m: Manifest) : string list = (listsFor target m).Runtime

    /// The pairing key of a manifest-listed source: its name minus the extension, minus a
    /// trailing `.<t>` segment for a target this manifest declares. `prim-types-int.js.fs`,
    /// `prim-types-int.fs` and `prim-types-int.fsi` all key on `prim-types-int`.
    ///
    /// ONE rule, shared by the conformance pass and the intrinsic-repr extraction: the two
    /// pair the same `.fsi` with the same `.fs`, or one of them is checking a pair the other
    /// never built.
    let pairingStem (m: Manifest) (rel: string) : string =
        let noExt = Path.ChangeExtension(rel, null)

        m.Targets
        |> Map.toSeq
        |> Seq.tryPick (fun (t, _) ->
            let suffix = "." + t

            if noExt.EndsWith(suffix, System.StringComparison.Ordinal) then
                Some(noExt.Substring(0, noExt.Length - suffix.Length))
            else
                None
        )
        |> Option.defaultValue noExt

    /// Every path the provider build may READ for this manifest, relative to the manifest's
    /// own directory — and so the coverage set the compile cache's dependency hash folds
    /// (`Hashing.dependencySignatureHash`). A path named here need not exist; the hash
    /// records its absence.
    ///
    /// TARGET-BLIND: every target's lists UNIONED, never `resolve*`'s selection for one. The
    /// cache key is computed with no target in hand, and the two errors are not symmetric —
    /// folding too much costs a rebuild when an unrelated target changes, folding too little
    /// serves a WRONG blob.
    ///
    /// `Runtime` is the one deliberate omission. A runtime asset (the JS `.mjs`) is shipped
    /// beside the backend's output and is never parsed, so it determines no frozen tree.
    let sourceInputs (m: Manifest) : string list =
        [
            yield! m.Shared.Files
            yield! m.Shared.Impl
            yield! m.Shared.InlineBodies
            yield! m.Shared.SigOnly

            for KeyValue(_, t) in m.Targets do
                yield! t.Files
                yield! t.Impl
                yield! t.InlineBodies
                yield! t.SigOnly
        ]
        |> List.distinct

    /// The `[core]` keys a manifest may carry. An unknown one is a parse ERROR: the old
    /// schema's dashed-suffix keys (`impl-js`, `inline-bodies`) would otherwise be read as
    /// silence, and a stale manifest would resolve to a plausible wrong file set.
    let private coreKeys =
        set
            [
                "name"
                "description"
                "depends-on"
                "files"
                "impl"
                "inline-bodies"
                "sig-only"
            ]

    /// The keys a `[targets.<t>]` table may carry — same rule, same reason.
    let private targetKeys =
        set [ "files"; "impl"; "inline-bodies"; "sig-only"; "runtime" ]

    let private unknownKey (tableName: string) (allowed: Set<string>) (t: TomlTable) : string option =
        t
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.tryFind (allowed.Contains >> not)
        |> Option.map (fun key ->
            sprintf
                "manifest.toml: [%s] unknown key `%s` (expected one of: %s)"
                tableName
                key
                (allowed |> Set.toList |> String.concat ", ")
        )

    let private parseTargets (targets: TomlTable) : Result<Map<string, TargetLists>, string> =
        (Ok Map.empty, targets |> Map.toList)
        ||> List.fold (fun acc (name, value) ->
            match acc with
            | Error e -> Error e
            | Ok map ->
                match asTable value with
                | None -> Error(sprintf "manifest.toml: [targets.%s] must be a table" name)
                | Some t ->
                    match unknownKey ("targets." + name) targetKeys t with
                    | Some e -> Error e
                    | None ->
                        let list key =
                            findStringList t key |> Option.defaultValue []

                        Ok(
                            Map.add
                                name
                                {
                                    Files = list "files"
                                    Impl = list "impl"
                                    InlineBodies = list "inline-bodies"
                                    SigOnly = list "sig-only"
                                    Runtime = list "runtime"
                                }
                                map
                        )
        )

    /// Parse a package `manifest.toml` document. `dirName` is the manifest's
    /// directory name, used as the assembly name when `[core]` carries no `name`.
    let parseManifest (dirName: string) (doc: TomlDocument) : Result<Manifest, string> =
        let targetsTable =
            Map.tryFind "targets" doc
            |> Option.bind asTable
            |> Option.defaultValue Map.empty

        match Map.tryFind "core" doc |> Option.bind asTable with
        | None -> Error "manifest.toml: missing [core] table"
        | Some core ->
            match unknownKey "core" coreKeys core with
            | Some e -> Error e
            | None ->
                match findStringList core "files" with
                | None -> Error "manifest.toml: [core] missing `files = [...]`"
                | Some files ->
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
                        parseTargets targetsTable
                        |> Result.map (fun targets ->
                            {
                                Name = nameOpt |> Option.defaultValue dirName
                                DependsOn = findStringList core "depends-on" |> Option.defaultValue []
                                Shared =
                                    {
                                        Files = files
                                        Impl = findStringList core "impl" |> Option.defaultValue []
                                        InlineBodies = findStringList core "inline-bodies" |> Option.defaultValue []
                                        SigOnly = findStringList core "sig-only" |> Option.defaultValue []
                                    }
                                Targets = targets
                            }
                        )

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

    /// Resolve the per-target runtime *asset* modules (`[targets.<t>] runtime`) for a
    /// manifest set, closed over `depends-on`, reading each file's contents from disk. Maps
    /// each package/assembly name (`Manifest.Name`) to its `(fileName, source)` —
    /// the hand-authored platform-support module (the JS `.mjs`) the backend
    /// materialises beside the output and imports by `./<fileName>`. A package with
    /// no `runtime` key for the target contributes nothing; one module per package (only the
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
                    match resolveRuntime target manifest with
                    | rel :: _ ->
                        let abs = Path.Combine(Path.GetDirectoryName manifestPath, rel)

                        if File.Exists abs then
                            acc <- Map.add manifest.Name (Path.GetFileName rel, File.ReadAllText abs) acc
                    | [] -> ()

            acc

    /// Wrap the extractor's provider so (a) every resolved descriptor carries
    /// the package's home assembly (the extractor records `SymbolOrigin.Empty` — it
    /// knows the namespace it extracted from, not which assembly it will be read as),
    /// and (b) the package's implicit prelude — its `[<AutoOpen>]` modules plus
    /// `RuntimeNames.preludeNamespaces` — is surfaced as `IAmbientOpenScope`. Short-name
    /// resolution is *not* a provider-internal retry any more: the pipeline
    /// seeds these `ambient` prefixes into the open scope and probes them
    /// BEHIND explicit `open`s, so an explicit `open` can shadow a prelude
    /// name. The actual
    /// composition / stamping / `IAmbientOpenScope` plumbing is the shared
    /// `ExternalSymbolProviders.stack` primitive — `wrap` is a 1-source instantiation
    /// of it with origin stamping.
    let private wrap (home: Origin) (ambient: string list) (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        ExternalSymbolProviders.stack (ValueSome home) ambient [ inner ]

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
    ///
    /// A package built by `buildProviderWith`, plus the census the composition-time
    /// duplicate sweep reads. `DeclaredTypeNames` are the qualified compiled names of the
    /// NOMINAL types this package OWNS (Class/Record/Union/Enum — the shapes that mint a
    /// lookup key and would silently first-hit-shadow a peer package's same-named type).
    /// Intrinsics and capability interfaces share one canon across packages by design, and
    /// so are deliberately excluded — a shared canon there is not a collision.
    type BuiltPackage =
        {
            Provider: IExternalSymbolProvider
            Diagnostics: (VesperLib.LibFile * string) list
            HomeAssembly: string
            DeclaredTypeNames: string list
        }

    let buildProviderWith
        (target: string)
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

            // Pair `.fsi` extern + `.fs` `(# … #)`: extract the intrinsic reprs from the
            // manifest's `.fs` bodies FIRST, so the `extern` arm of the `.fsi` extraction
            // below publishes a matched primitive as `ExternalTypeShape.Intrinsic` rather
            // than an opaque `Class`. The `.fs` is the only place the repr lives (the
            // `.fsi` commits `type exn = extern`, no repr).
            //
            // Two repr tables, filled from two different body sets:
            //  - `IntrinsicBaseReprs`, the primitive *marker*: every target's `impl`. A
            //    primitive is a primitive of the language on every target, so `decimal`
            //    (which ships no JS repr) still publishes as an `Intrinsic` with
            //    `platform = None` there rather than as a silently opaque class. Only key
            //    PRESENCE is read, so a key several targets bind is not a conflict. A
            //    SPLICE-only body does not mark: it publishes members onto a contract some
            //    `impl` decides the species of.
            //  - `IntrinsicReprs`, the `platform` name: THIS target's bodies, spliced ones
            //    included (`prim-types-int.js.fs` ⇒ `number`).
            // `canon` is the `.fsi` name itself (set at the `extern` arm), so a target's
            // repr never moves the unifier's identity key.
            let targetBodies =
                resolveImpl target manifest @ resolveInlineBodies target manifest |> Set.ofList

            let everyBody =
                [
                    yield! manifest.Shared.Impl
                    yield! manifest.Shared.InlineBodies

                    for KeyValue(_, t) in manifest.Targets do
                        yield! t.Impl
                        yield! t.InlineBodies
                ]
                |> List.distinct

            let markers =
                [
                    yield! manifest.Shared.Impl

                    for KeyValue(_, t) in manifest.Targets do
                        yield! t.Impl
                ]
                |> Set.ofList

            for rel in everyBody do
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
                        // ONE parse feeds both tables — a second read is a second answer
                        // for what the file says.
                        let reprs =
                            System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

                        VesperLib.extractIntrinsicReprsInto reprs parsed

                        for KeyValue(k, v) in reprs do
                            if markers.Contains rel then
                                ctx.IntrinsicBaseReprs.[k] <- v

                            if targetBodies.Contains rel then
                                ctx.IntrinsicReprs.[k] <- v

            // Shared contracts, then this target's APPENDED extras, so an extra's RHS
            // (`Vesper.disposable`) is already in the registry.
            for rel in resolveFiles target manifest do
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

            // The contract's implicit prelude: its `[<AutoOpen>]` modules (most
            // specific, e.g. `Vesper.ArithmeticOperators`) ahead of the language prelude
            // (`Vesper`, so `int` finds `Vesper.int`). Both are probed behind explicit
            // `open`s. The prelude is fixed, not manifest-declared — see
            // `RuntimeNames.preludeNamespaces`.
            let ambient = List.ofSeq ctx.AutoOpenPrefixes @ RuntimeNames.preludeNamespaces

            // The nominal types this package declares (own shapes only — `shapeOf`
            // consults dependency `AmbientShapes` as a fallback but never inserts them
            // into `TypeShapes`), for the composition-time duplicate sweep. EXHAUSTIVE
            // over the shape cases so a NEW shape forces an include/exclude decision
            // here: intrinsics and capability interfaces share one canon by design (every
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
                    Provider = wrap home ambient (VesperLib.ExtractCtx.toProvider ctx)
                    Diagnostics = List.ofSeq ctx.Diagnostics
                    HomeAssembly = manifest.Name
                    DeclaredTypeNames = declaredTypeNames
                }

    /// Stand up a referenced project in isolation — no dependency shapes in scope
    /// (`AmbientShapes` defaults to "resolve nothing"). The dependency-free path:
    /// a package with no `depends-on`, and the entry point tests build a single
    /// package from. Dependency-aware composition uses `composeOrdered`.
    let buildProvider
        (target: string)
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        buildProviderWith target (fun _ -> ValueNone) [] manifestPath
        |> Result.map (fun bp -> bp.Provider, bp.Diagnostics)

    /// A layer-2 metadata-tail factory: given the extracted `{ platform-repr →
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
        (target: string)
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

            let ambientShapes =
                (fun (name: string) -> depComposite.TryLookupType name |> ExternalSymbols.typeShapeOf)

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

        // The final composite's leaf IS seeded with the full extracted reverse map, so a
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
        (target: string)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        match buildClosureWithDeps manifestPaths with
        | Ok(ordered, transitiveDeps) -> composeOrdered metaTail target ordered transitiveDeps
        | Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// Lazy cache keyed by target + (normalised) manifest path so repeated callers
    /// parse a package's `.fsi` set at most once. Mirrors `VesperLib.defaultProvider`.
    let private cached =
        System.Collections.Concurrent.ConcurrentDictionary<
            string * string,
            Lazy<Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string>>
         >(
            HashIdentity.Structural
        )

    /// Production-path entry point: caches `buildProvider` per target + manifest path.
    /// Tests that need a fresh provider should call `buildProvider`.
    let provider
        (target: string)
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        cached.GetOrAdd((target, Path.GetFullPath manifestPath), (fun (t, p) -> lazy (buildProvider t p))).Value
