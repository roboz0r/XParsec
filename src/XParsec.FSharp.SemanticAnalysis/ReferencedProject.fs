namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// A committed file a package's `[core] runtime` key lists, read off disk.
type RuntimeAsset = { FileName: string; Source: string }

/// A *referenced project*: a package DIRECTORY resolved against a target to
/// `manifest.<target>.toml`, parsed, and closed over `depends-on` into a build order. What
/// the file lists NAME is read here; what they DECLARE is resolved by `PackageProviders`.
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
    let resolveManifest (target: string) (packageDir: string) : Result<ManifestPath, PackageSetFault> =
        let path = Path.Combine(packageDir, "manifest." + target + ".toml")

        if File.Exists path then
            Ok
                {
                    FullPath = Path.GetFullPath path
                    TargetTag = target
                }
        else
            Error(
                PackageSetFault.NoManifestForTarget(
                    Path.GetFileName(Path.TrimEndingDirectorySeparator packageDir),
                    target
                )
            )

    /// Every package directory resolved against `target`. A package that does not build for it
    /// is a REFUSAL, not a silent drop.
    let resolveAll (target: string) (packageDirs: string list) : Result<ManifestPath list, PackageSetFault> =
        let rec go acc dirs =
            match dirs with
            | [] -> Ok(List.rev acc)
            | dir :: rest ->
                match resolveManifest target dir with
                | Ok mp -> go (mp :: acc) rest
                | Error fault -> Error fault

        go [] packageDirs

    /// One `[core] files` entry: the path as the manifest spells it, and the kind of source
    /// file its extension makes it.
    type ManifestFile =
        {
            Relative: string
            Kind: SourceFileKind
        }

    /// One `[core] files` compilation unit: an implementation entry, under the signature
    /// entry listed immediately ahead of it. `ValueNone` where the manifest lists an
    /// implementation entry alone.
    type ManifestUnit = SourceUnit<ManifestFile, ManifestFile>

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
            /// Other packages this one depends on (`[core] depends-on`), each a path
            /// RELATIVE TO THIS PACKAGE'S DIRECTORY (`"../Vesper.Core"`), because a package
            /// outside `src/` must be able to reference one inside it.
            DependsOn: string list
            /// Every `[core] files` entry in compile order, PAIRED: the parse rejects a
            /// duplicate entry, a `.fsi` listed other than immediately ahead of its companion,
            /// and a `.fsi` with no companion. The implementations are the DLL's bodies.
            Units: ManifestUnit list
            /// Hand-authored runtime *asset* modules: not sources the front end parses, but
            /// platform-support artifacts (the JS `.mjs`) the backend ships beside its output.
            Runtime: string list
        }

        /// The target this manifest declares the package's file set for, read off the file name
        /// (`manifest.js.toml` ⇒ `"js"`).
        member this.Target = this.Path.Target
        /// The directory the manifest sits in, which every list entry is relative to.
        member this.Dir = this.Path.PackageDir

        /// `Units` flattened back to the `[core] files` order the manifest spells.
        member this.Files: ManifestFile list =
            [
                for u in this.Units do
                    match u.Signature with
                    | ValueSome s -> s
                    | ValueNone -> ()

                    u.Implementation
            ]

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
    /// trailing `.<target>` segment. Under the `js` target, `prim-types-int.js.fs` and
    /// `prim-types-int.fsi` both key on `prim-types-int`.
    let pairingKey (target: string) (rel: string) : string =
        let noExt = Path.ChangeExtension(rel, null)
        let suffix = "." + target

        if noExt.EndsWith(suffix, System.StringComparison.Ordinal) then
            noExt.Substring(0, noExt.Length - suffix.Length)
        else
            noExt

    /// Every path the provider build may READ for this manifest, relative to the manifest's
    /// own directory; a path named here need not exist. `Runtime` is omitted: an asset is
    /// never parsed, so determines no frozen tree.
    let sourceInputs (m: Manifest) : string list =
        m.Files |> List.map (fun f -> f.Relative)

    /// The `.fsi` entries, in list order.
    let signatureFiles (m: Manifest) : string list =
        [
            for u in m.Units do
                match u.Signature with
                | ValueSome s -> s.Relative
                | ValueNone -> ()
        ]

    /// The `.fs` entries, in list order: the package's compile order.
    let implementationFiles (m: Manifest) : string list =
        [ for u in m.Units -> u.Implementation.Relative ]

    /// The `[core]` keys a manifest may carry. An unknown one is a parse ERROR: read as
    /// silence, it would resolve a stale manifest to a plausible wrong file set.
    let private coreKeys =
        set [ "name"; "description"; "depends-on"; "files"; "runtime" ]

    let private unknownKey (t: TomlTable) : string option =
        t
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.tryFind (coreKeys.Contains >> not)
        |> Option.map (fun key ->
            sprintf "[core] unknown key `%s` (expected one of: %s)" key (coreKeys |> Set.toList |> String.concat ", ")
        )

    /// Each `files` entry classified by extension and paired into units: no entry twice, and
    /// each `.fsi` immediately ahead of the one companion `.fs` it keys with. Any violation is
    /// a parse ERROR, and every later stage takes this pairing rather than re-deriving it.
    let private classifyFiles (target: string) (files: string list) : Result<ManifestUnit list, string> =
        let rec classify acc rest =
            match rest with
            | [] -> Ok(List.rev acc)
            | (rel: string) :: rest ->
                match SourceFileKind.tryOfPath rel with
                | ValueSome kind -> classify ({ Relative = rel; Kind = kind } :: acc) rest
                | ValueNone -> Error(sprintf "[core] files entry `%s` is neither a `.fsi` nor a `.fs`" rel)

        let duplicated =
            files
            |> List.countBy id
            |> List.tryPick (fun (rel, n) -> if n > 1 then Some rel else None)

        // Adjacency makes key pairing and list layout provably agree: a `.fsi` whose sole
        // key-mate is the next entry pairs with it under either reading.
        let pairUp (classified: ManifestFile list) : Result<ManifestUnit list, string> =
            let implementations =
                classified
                |> List.filter (fun f -> f.Kind = SourceFileKind.Implementation)
                |> List.groupBy (fun f -> pairingKey target f.Relative)
                |> Map.ofList

            let rec go acc entries =
                match entries with
                | [] -> Ok(List.rev acc)
                | (f: ManifestFile) :: rest ->
                    match f.Kind with
                    | SourceFileKind.Implementation ->
                        go
                            ({
                                Signature = ValueNone
                                Implementation = f
                             }
                             :: acc)
                            rest
                    | SourceFileKind.Signature ->
                        match Map.tryFind (pairingKey target f.Relative) implementations with
                        | None ->
                            // A `.fsi` alone declares a surface with no body compiled for this
                            // target, so every later stage would have to invent one.
                            Error(
                                sprintf
                                    "[core] files: `%s` has no companion implementation (expected `%s.fs` or `%s.%s.fs`)"
                                    f.Relative
                                    (pairingKey target f.Relative)
                                    (pairingKey target f.Relative)
                                    target
                            )
                        | Some [ companion ] ->
                            match rest with
                            | next :: rest when next.Relative = companion.Relative ->
                                go
                                    ({
                                        Signature = ValueSome f
                                        Implementation = next
                                     }
                                     :: acc)
                                    rest
                            | _ ->
                                Error(
                                    sprintf
                                        "[core] files: `%s` must sit immediately ahead of its companion `%s`"
                                        f.Relative
                                        companion.Relative
                                )
                        | Some claimants ->
                            Error(
                                sprintf
                                    "[core] files: `%s` pairs with %s"
                                    f.Relative
                                    (claimants
                                     |> List.map (fun c -> sprintf "`%s`" c.Relative)
                                     |> String.concat " and ")
                            )

            go [] classified

        match duplicated with
        | Some rel -> Error(sprintf "[core] files lists `%s` twice" rel)
        | None -> classify [] files |> Result.bind pairUp

    /// The `[core]` table, gated: the document's sole table, carrying known keys alone. A
    /// second table or an unknown key read as silence would resolve a stale manifest to a
    /// plausible wrong file set.
    let private coreTable (doc: TomlDocument) : Result<TomlTable, string> =
        match doc |> Map.toSeq |> Seq.map fst |> Seq.tryFind ((<>) "core") with
        | Some other -> Error(sprintf "unknown table [%s] (a manifest carries [core] alone)" other)
        | None ->
            match Map.tryFind "core" doc |> Option.bind asTable with
            | None -> Error "missing [core] table"
            | Some core ->
                match unknownKey core with
                | Some e -> Error e
                | None -> Ok core

    /// `[core] name` when declared, else `dirName`. A declared name must match `dirName`,
    /// because the directory name is the package identity, and the assembly name it emits
    /// under; an omitted `name` trivially matches via the fallback.
    let private packageName (core: TomlTable) (dirName: string) : Result<string, string> =
        match findString core "name" with
        | Some explicit when explicit <> dirName ->
            Error(
                sprintf
                    "[core] name \"%s\" must match the package directory name \"%s\", because the directory name is the package identity, and the assembly name it emits under"
                    explicit
                    dirName
            )
        | Some explicit -> Ok explicit
        | None -> Ok dirName

    /// Parse the manifest document read from `mp`, which carries both halves of a manifest's
    /// identity: its DIRECTORY name is the assembly name when `[core]` declares no `name`, and
    /// the target it was resolved under is the one its file list is read for.
    let parseManifest (mp: ManifestPath) (doc: TomlDocument) : Result<Manifest, PackageSetFault> =
        let malformed detail =
            Error(PackageSetFault.MalformedManifest(mp.Path, detail))

        let dirName = Path.GetFileName(Path.TrimEndingDirectorySeparator mp.PackageDir)

        match coreTable doc with
        | Error e -> malformed e
        | Ok core ->
            match findStringList core "files" with
            | None -> malformed "[core] missing `files = [...]`"
            | Some files ->
                match classifyFiles mp.Target files, packageName core dirName with
                | Error e, _
                | _, Error e -> malformed e
                | Ok units, Ok name ->
                    let list key =
                        findStringList core key |> Option.defaultValue []

                    Ok
                        {
                            Path = mp
                            Name = name
                            DependsOn = list "depends-on"
                            Units = units
                            Runtime = list "runtime"
                        }

    /// Read + parse a resolved manifest.
    let loadManifest (mp: ManifestPath) : Result<Manifest, PackageSetFault> =
        match Toml.parse (File.ReadAllText mp.Path) with
        | Error e -> Error(PackageSetFault.MalformedManifest(mp.Path, sprintf "TOML parse error: %s" e))
        | Ok doc -> parseManifest mp doc

    /// Resolve a `depends-on` entry against the dependent's own DIRECTORY and target:
    /// `"../Vesper.Core"` named by a manifest in `src/Vesper.List` resolves in `src/Vesper.Core`,
    /// canonicalised, so two spellings of one package are one.
    let private dependencyManifest
        (dependent: ManifestPath)
        (dependencyPath: string)
        : Result<ManifestPath, PackageSetFault> =
        resolveManifest dependent.Target (Path.Combine(dependent.PackageDir, dependencyPath))

    /// Close `rootManifests` over `[core] depends-on`: every reachable manifest PARSED, in
    /// **dependency order** (de-duplicated, stable over discovery order), plus each one's DIRECT
    /// dependencies. A cycle or an absent manifest is a hard error.
    let private closeAndOrder
        (rootManifests: ManifestPath list)
        : Result<Manifest list * System.Collections.Generic.Dictionary<ManifestPath, ManifestPath list>, PackageSetFault> =
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
                | Error e -> error <- Some e
                | Ok manifest ->
                    let rec resolveDeps acc names =
                        match names with
                        | [] -> Ok(List.rev acc)
                        | name :: rest ->
                            match dependencyManifest mp name with
                            | Error e -> Error e
                            | Ok dep -> resolveDeps (dep :: acc) rest

                    match resolveDeps [] manifest.DependsOn with
                    | Error e -> error <- Some e
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
                        cycle <-
                            Some(
                                PackageSetFault.UnresolvedDependency(
                                    sprintf "dependency cycle through package '%s'" loaded.[node].Name
                                )
                            )
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
    let buildClosure (rootManifests: ManifestPath list) : Result<Manifest list, PackageSetFault> =
        closeAndOrder rootManifests |> Result.map fst

    /// Like `buildClosure`, but also returns each package's **transitive** `depends-on`
    /// closure: manifest → the manifests it depends on, directly or transitively (excluding
    /// itself). An unknown manifest maps to the empty list.
    let buildClosureWithDeps
        (rootManifests: ManifestPath list)
        : Result<Manifest list * (ManifestPath -> ManifestPath list), PackageSetFault> =
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

    /// The `[core] runtime` assets of an already-closed manifest set, read off disk, keyed by
    /// package name. In manifest order, and the FIRST is the package's runtime entry: the file
    /// a backend-synthesised import (a structural helper, a format helper) resolves to.
    let runtimeModules (manifests: Manifest list) : Map<string, RuntimeAsset list> =
        let mutable acc = Map.empty

        for manifest in manifests do
            let assets =
                [
                    for rel in manifest.Runtime do
                        let abs = Path.Combine(manifest.Dir, rel)

                        if File.Exists abs then
                            {
                                FileName = Path.GetFileName rel
                                Source = File.ReadAllText abs
                            }
                ]

            match assets with
            | [] -> ()
            | _ -> acc <- Map.add manifest.Name assets acc

        acc
