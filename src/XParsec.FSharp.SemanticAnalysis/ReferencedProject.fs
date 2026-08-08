namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// Layer 1 of the symbol-resolution stack: a *referenced project*, declared by its
/// `manifest.toml`. Parses the `[core]` contract `.fsi` files, in compile order, into one
/// `ExtractCtx` exposed as a provider. A symbol's namespace is its FILE's `namespace` header.
module ReferencedProject =

    /// The `[core]` lists — what a package declares once for EVERY target. No `Runtime`
    /// field: a runtime asset is a per-target artifact.
    type SharedLists =
        {
            /// Target-neutral contract `.fsi` files in compile order (`[core] files`).
            Files: string list
            /// Target-neutral `.fs` bodies compiled into the package DLL, and the splice
            /// sources those same bodies publish (`[core] impl`).
            Impl: string list
            /// Contract `.fsi` files that are DELIBERATELY impl-free (`[core] sig-only`) —
            /// a front-end intrinsic lowered inline (`printf.fsi`) or a BCL-resolved
            /// contract (`exceptions.fsi`). An impl-free `.fsi` NOT listed is a hard error.
            SigOnly: string list
            /// `.fs` bodies that implement NO contract (`[core] impl-only`), publishing their
            /// whole public surface. Naming one here keeps the pairing rule from marrying it
            /// to a `.fsi` of the same key.
            ImplOnly: string list
        }

    module SharedLists =
        let empty: SharedLists =
            {
                Files = []
                Impl = []
                SigOnly = []
                ImplOnly = []
            }

    /// One `[targets.<t>]` table. Every list here is APPENDED to its `SharedLists` peer.
    type TargetLists =
        {
            /// Target-only EXTRA contracts, after the shared ones (the JS capability
            /// compat shim, the JS-only `undefined`/`dynamic` intrinsics).
            Files: string list
            /// Target-only `.fs` bodies, after the shared ones.
            Impl: string list
            /// Target-only impl-free contract exemptions, after the shared ones.
            SigOnly: string list
            /// Target-only contract-less bodies, after the shared ones.
            ImplOnly: string list
            /// Hand-authored runtime *asset* modules — not sources the front end parses, but
            /// platform-support artifacts (the JS `.mjs`) the backend ships beside its output.
            Runtime: string list
        }

    module TargetLists =
        let empty: TargetLists =
            {
                Files = []
                Impl = []
                SigOnly = []
                ImplOnly = []
                Runtime = []
            }

    /// A parsed package `manifest.toml`: the `[core]` table's target-neutral lists plus
    /// one `[targets.<t>]` table per target the package participates in.
    type Manifest =
        {
            /// Package / assembly simple name — `[core] name` when present, else
            /// the manifest's directory name (`src/Vesper.Core` ⇒ `"Vesper.Core"`).
            Name: string
            /// Other packages this one depends on (`[core] depends-on`) — the
            /// package names whose DLLs/contracts must be built/referenced first.
            DependsOn: string list
            Shared: SharedLists
            /// `[targets.<t>]` by target name. The keys ARE the target set this package
            /// participates in.
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

    /// The `.fs` bodies for `target`: what the package DLL compiles AND what it publishes
    /// as splice sources. Shared, then the target's own.
    let resolveImpl (target: string) (m: Manifest) : string list =
        m.Shared.Impl @ (listsFor target m).Impl

    let resolveSigOnly (target: string) (m: Manifest) : string list =
        m.Shared.SigOnly @ (listsFor target m).SigOnly

    let resolveImplOnly (target: string) (m: Manifest) : string list =
        m.Shared.ImplOnly @ (listsFor target m).ImplOnly

    /// The runtime *asset* modules for `target`. No shared peer: a runtime asset is
    /// inherently target-specific (the CLR builds a DLL rather than committing one).
    let resolveRuntime (target: string) (m: Manifest) : string list = (listsFor target m).Runtime

    /// The pairing key of a manifest-listed source: its name minus the extension, minus a
    /// trailing `.<t>` segment for a target this manifest declares. `prim-types-int.js.fs`,
    /// `prim-types-int.clr.fs` and `prim-types-int.fsi` all key on `prim-types-int`.
    let pairingKey (m: Manifest) (rel: string) : string =
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
    /// own directory; a path named here need not exist. TARGET-BLIND — every target's lists
    /// UNIONED. `Runtime` is omitted: an asset is never parsed, so determines no frozen tree.
    let sourceInputs (m: Manifest) : string list =
        [
            yield! m.Shared.Files
            yield! m.Shared.Impl
            yield! m.Shared.SigOnly
            yield! m.Shared.ImplOnly

            for KeyValue(_, t) in m.Targets do
                yield! t.Files
                yield! t.Impl
                yield! t.SigOnly
                yield! t.ImplOnly
        ]
        |> List.distinct

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
            ]

    /// The keys a `[targets.<t>]` table may carry — same rule, same reason.
    let private targetKeys = set [ "files"; "impl"; "sig-only"; "impl-only"; "runtime" ]

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
                                    SigOnly = list "sig-only"
                                    ImplOnly = list "impl-only"
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
                    // An omitted `name` trivially matches via the directory-name fallback.
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
                                        SigOnly = findStringList core "sig-only" |> Option.defaultValue []
                                        ImplOnly = findStringList core "impl-only" |> Option.defaultValue []
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

    /// Resolve a `depends-on` package name to its `manifest.toml` path. A package's
    /// directory name *is* its identity, so a dependency `"Vesper.Core"` of the manifest
    /// at `src/Vesper.List/manifest.toml` lives at `src/Vesper.Core/manifest.toml`.
    let private dependencyManifestPath (dependentManifestPath: string) (dependencyName: string) : string =
        let packageDir = Path.GetDirectoryName dependentManifestPath
        let srcDir = Path.GetDirectoryName packageDir
        Path.Combine(srcDir, dependencyName, "manifest.toml")

    /// Close `rootManifests` over `[core] depends-on`: every reachable manifest path in
    /// **dependency order** (normalised, de-duplicated, stable over discovery order), plus
    /// each path's DIRECT dependency paths. A cycle or an absent manifest is a hard error.
    let private closeAndOrder
        (rootManifests: string list)
        : Result<string list * System.Collections.Generic.Dictionary<string, string list>, string> =
        let norm (p: string) = Path.GetFullPath p

        // `discovered` is the order nodes were first reached (roots, then their deps);
        // the topo sort below walks it, so an already-ordered input comes back unchanged.
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
            // Post-order DFS over the discovery order, so dependencies are emitted first.
            // The gray/black colouring (1 = on the stack, 2 = emitted) rejects a cycle.
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

    /// Every manifest reachable over `[core] depends-on`, in dependency order.
    let buildClosure (rootManifests: string list) : Result<string list, string> =
        closeAndOrder rootManifests |> Result.map fst

    /// Like `buildClosure`, but also returns each package's **transitive** `depends-on`
    /// closure: normalised manifest path → the normalised paths it depends on, directly or
    /// transitively (excluding itself). An unknown path maps to the empty list.
    let buildClosureWithDeps (rootManifests: string list) : Result<string list * (string -> string list), string> =
        match closeAndOrder rootManifests with
        | Error e -> Error e
        | Ok(ordered, adjacency) ->
            // Each package's closure is the union of its direct deps and those deps' already-
            // computed closures, emitted dep-closure-before-dep so each closure is itself
            // topologically ordered.
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

    /// The per-target runtime *asset* modules (`[targets.<t>] runtime`) for a manifest set
    /// closed over `depends-on`, read off disk: package name → `(fileName, source)`, the
    /// `.mjs` the backend imports by `./<fileName>`. One per package — the first listed.
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

    /// Wrap the extractor's provider so every resolved descriptor carries the package's home
    /// assembly (the extractor records `SymbolOrigin.Empty`) and `ambient` is published as
    /// the provider's `AmbientOpenPrefixes`.
    let private wrap (home: Origin) (ambient: string list) (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        ExternalSymbolProviders.stack (ValueSome home) ambient [ inner ]

    /// One built package. `DeclaredTypeNames` are the qualified compiled names of the NOMINAL
    /// types it OWNS — those that would first-hit-shadow a peer package's same-named type.
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
        (target: string)
        (ambientShapes: string -> ExternalTypeShape voption)
        (dependencyAmbientPrefixes: string list)
        (manifestPath: string)
        : Result<BuiltPackage, string> =
        match loadManifest manifestPath with
        | Error e -> Error e
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let ctx = VesperLib.ExtractCtx.empty target
            ctx.AmbientShapes <- ambientShapes
            // Ambient open prefixes (`Vesper`, …) so this package's extraction resolves a
            // dependency's type by bare name (`Fun`2` / `Fun`3`).
            ctx.DependencyAmbientPrefixes <- dependencyAmbientPrefixes

            // Extract intrinsic reprs from the `.fs` bodies FIRST, so the `extern` arm of
            // the `.fsi` extraction below publishes a matched primitive as `Intrinsic`
            // rather than an opaque `Class`. The `.fsi` commits `type exn = extern`, no repr.
            let targetBodies = resolveImpl target manifest |> Set.ofList

            // `IntrinsicMarkers` takes every target's bodies, so `decimal` (which ships no
            // JS repr) still publishes there as an `Intrinsic` with `platform = None`;
            // `IntrinsicReprs` takes THIS target's (`prim-types-int.js.fs` ⇒ `number`).
            let everyBody =
                [
                    yield! manifest.Shared.Impl

                    for KeyValue(_, t) in manifest.Targets do
                        yield! t.Impl
                ]
                |> List.distinct

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
                        let reprs =
                            System.Collections.Generic.Dictionary<string, string>(System.StringComparer.Ordinal)

                        VesperLib.extractIntrinsicReprsInto reprs parsed

                        for KeyValue(k, v) in reprs do
                            ctx.IntrinsicMarkers.Add k |> ignore

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

            Ok
                {
                    Provider = wrap home ambient (VesperLib.ExtractCtx.toProvider ctx)
                    Diagnostics = List.ofSeq ctx.Diagnostics
                    HomeAssembly = manifest.Name
                    DeclaredTypeNames = declaredTypeNames
                }

    /// Stand up a referenced project in isolation — no dependency shapes in scope, so
    /// `ambientShapes` resolves nothing. For a package with no `depends-on`.
    let buildProvider
        (target: string)
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (VesperLib.LibFile * string) list, string> =
        buildProviderWith target (fun _ -> ValueNone) [] manifestPath
        |> Result.map (fun bp -> bp.Provider, bp.Diagnostics)

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
        (target: string)
        (orderedManifestPaths: string list)
        (transitiveDeps: string -> string list)
        : IExternalSymbolProvider =
        // `byPath` indexes each built provider by its normalised manifest path, so a
        // package's dependency providers resolve in O(closure).
        let built = ResizeArray<IExternalSymbolProvider>()

        let byPath =
            System.Collections.Generic.Dictionary<string, IExternalSymbolProvider>(System.StringComparer.Ordinal)

        // A qualified type key declared twice in the referenced set resolves as a silent
        // first-hit shadow, so the loser is unreachable by lookup; refuse it here, a
        // CS0433-equivalent. The overlap with the metadata tail is diagnosed downstream.
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

    /// `composeOrdered` over a raw, unordered manifest set. A cycle or missing dependency is
    /// a hard error. A caller that also needs the ordered list should order it itself.
    let composeContract
        (metaTail: MetaTailFactory)
        (target: string)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        match buildClosureWithDeps manifestPaths with
        | Ok(ordered, transitiveDeps) -> composeOrdered metaTail target ordered transitiveDeps
        | Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// Lazy cache keyed by target + (normalised) manifest path, so repeated callers parse
    /// a package's `.fsi` set at most once.
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
