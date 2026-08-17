namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// A committed file a package's `[core] runtime` key names, read off disk.
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
            /// outside `src/` must be able to name one inside it.
            DependsOn: string list
            /// Signature files in compile order (`[core] files`).
            Files: string list
            /// The `.fs` bodies compiled into the package DLL, and the splice sources those
            /// same bodies publish (`[core] impl`).
            Impl: string list
            /// Signature files that are DELIBERATELY impl-free (`[core] sig-only`): a
            /// front-end intrinsic lowered inline (`printf.fsi`), or one whose declarations the
            /// BCL resolves (`exceptions.fsi`).
            SigOnly: string list
            /// Hand-authored runtime *asset* modules: not sources the front end parses, but
            /// platform-support artifacts (the JS `.mjs`) the backend ships beside its output.
            Runtime: string list
        }

        /// The target this manifest declares the package's file set for, read off the file name
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
        m.Files @ m.Impl @ m.SigOnly |> List.distinct

    /// The `[core]` keys a manifest may carry. An unknown one is a parse ERROR: read as
    /// silence, it would resolve a stale manifest to a plausible wrong file set.
    let private coreKeys =
        set [ "name"; "description"; "depends-on"; "files"; "impl"; "sig-only"; "runtime" ]

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
                                    "%s: [core] name \"%s\" must match the package directory name \"%s\", because the directory name is the package identity, and the assembly name it emits under"
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
                                    Runtime = list "runtime"
                                }

    /// Read + parse a resolved manifest.
    let loadManifest (mp: ManifestPath) : Result<Manifest, string> =
        match Toml.parse (File.ReadAllText mp.Path) with
        | Error e -> Error(sprintf "Manifest parse error (%s): %s" mp.Path e)
        | Ok doc -> parseManifest mp doc

    /// Resolve a `depends-on` entry against the dependent's own DIRECTORY and target:
    /// `"../Vesper.Core"` named by a manifest in `src/Vesper.List` resolves in `src/Vesper.Core`,
    /// canonicalised, so two spellings of one package are one. The fault renders to prose here.
    let private dependencyManifest (dependent: ManifestPath) (dependencyPath: string) : Result<ManifestPath, string> =
        resolveManifest dependent.Target (Path.Combine(dependent.PackageDir, dependencyPath))
        |> Result.mapError PackageSetFault.describe

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
