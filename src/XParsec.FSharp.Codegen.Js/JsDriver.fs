namespace XParsec.FSharp.Codegen.Js

open System.IO
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// One EMITTING source file of a package build, compiled to its own `.mjs`.
type JsPackageModule =
    {
        /// The file this was compiled from (`a/one.fs`), and the file a diagnostic is blamed on.
        Source: AssemblyFileId
        Path: JsModulePath
        Artifact: JsArtifact
    }

/// A compiled JS package: `<Name>/<file>.mjs` per emitting source, each with its own 1:1
/// source map, plus the `index.mjs` barrel a consumer imports the whole package through.
type JsPackage =
    {
        Name: string
        Modules: JsPackageModule list
        /// The committed runtime files the modules import, each written into its own
        /// package's directory.
        RuntimeAssets: JsRuntimeModule list
    }

    /// The generated `index.mjs`: `export * from "./shapes.mjs";` per emitting module, in
    /// file order, then this package's OWN committed runtime files, so a consumer entering by
    /// the barrel reaches the hand-authored half too. A package whose files all lower to
    /// nothing gets a barrel over its runtime files alone, which still resolves.
    member this.Barrel: string =
        let ownAssets =
            this.RuntimeAssets
            |> List.filter (fun a -> a.Path.Package = ValueSome this.Name)

        (this.Modules |> List.map (fun m -> m.Path.FileName))
        @ (ownAssets |> List.map (fun a -> a.Path.FileName))
        |> List.map (sprintf "export * from \"./%s\";\n")
        |> String.concat ""

/// The production JS driver: an ordered multi-file assembly in, a directory of one `.mjs`
/// per emitting file out.
module JsDriver =

    let BarrelFileName = JsModulePath.BarrelFileName

    /// The resolution contract for a compilation that IS a package, over the JS-native stubs.
    let contractForSelf (selfPackage: string) (references: string list) : PackageProviders.AnalyzedManifest =
        JsNativeSymbols.jsNativeContract (SymbolProviders.selfStack (Some selfPackage) references)

    /// The sources that claim one `.mjs`, blamed individually. A module path is a base name,
    /// so `a/one.fs` and `b/one.fs` both claim `one.mjs`; the second write would win silently.
    let private modulePathCollisions
        (packageName: string)
        (modules: JsPackageModule list)
        : AssemblyFiles.AnchoredDiagnostic list =
        [
            for path, claimants in modules |> List.groupBy (fun m -> m.Path) do
                match claimants with
                | _ :: _ :: _ ->
                    let message =
                        sprintf
                            "JS package '%s': sources %s all emit '%s'; a module path is the source's base name, so they cannot share a package directory"
                            packageName
                            (claimants
                             |> List.map (fun m -> sprintf "'%s'" m.Source.Name)
                             |> String.concat ", ")
                            path.FileName

                    for m in claimants do
                        yield!
                            AssemblyFiles.unpositionedDiagnostics m.Source [ Diagnostic.nowhere (Kind.Driver message) ]
                | _ -> ()
        ]

    /// Every import of everything this build writes, against what it writes: the emitted
    /// modules' and the runtime files' alike, since `Vesper.Seq.mjs` imports `Vesper.Core`'s
    /// runtime file and a build shipping the first must ship the second.
    let private checkResolvable
        (packageName: string)
        (modules: JsPackageModule list)
        (assets: JsRuntimeModule list)
        : unit =
        let written =
            (modules |> List.map (fun m -> m.Path)) @ (assets |> List.map (fun a -> a.Path))
            |> Set.ofList

        let check (from: JsModulePath) (target: JsModulePath) =
            if not (written.Contains target) then
                failwithf
                    "JS package '%s': module '%s' imports '%s', which this build does not write"
                    packageName
                    from.FileName
                    (JsModulePath.specifierFrom from.Package target)

        for m in modules do
            for target in m.Artifact.ImportedModules do
                check m.Path target

        for a in assets do
            for target in a.Imports do
                check a.Path target

    /// Compile an ordered source-file list as ONE assembly named `packageName`.
    let compileAssemblyWith
        (contract: PackageProviders.AnalyzedManifest)
        (packageName: string)
        (units: Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list)
        : Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list> =
        let assembly: CompilingAssembly =
            {
                Name = packageName
                Target = Target.Js
            }

        PackageProviders.AnalyzedManifest.gate contract
        |> Result.bind (fun gated -> AssemblyFiles.analyseGated Pipeline.analyseFor assembly gated.Provider units)
        |> Result.bind (fun analysed ->
            // A spliced node reads only against its declaring file's own text, so the
            // assembly's own sources join the references' before any file is emitted.
            let origins = LexedFiles.addAll analysed.Retained contract.Retained

            // Each file's compile keeps its source identity, which is both the module path it
            // emits to and the file a refusal is blamed on.
            let compiled =
                [
                    for file in analysed.Files do
                        let fileId = file.Retained.Path.Relative
                        let relative = fileId.Name

                        let project =
                            { JsProjectInfo.defaults (JsModulePath.baseName relative) with
                                Package = ValueSome packageName
                                Kind = Library
                                GeneratedFrom = Some relative
                                Source =
                                    Some
                                        {
                                            Path = relative
                                            Content = file.Retained.Input
                                            Lexed = file.Retained.Lexed
                                        }
                            }

                        {|
                            Source = fileId
                            Path = JsModulePath.ofSource packageName relative
                            Artifact =
                                Codegen.compileWith
                                    { contract with
                                        Provider = file.Scoped
                                        Retained = origins
                                    }
                                    project
                                    file.Frozen
                        |}
                ]

            // Codegen's gate firing on a tree `analyseGated` already passed means the two
            // disagree; the finding is re-filed against the file that produced it.
            let refusals =
                [
                    for c in compiled do
                        match c.Artifact with
                        | Error errors ->
                            yield!
                                AssemblyFiles.unpositionedDiagnostics
                                    c.Source
                                    [ for d in errors -> Diagnostic.nowhere (Kind.Driver d.Message) ]
                        | Ok _ -> ()
                ]

            match refusals with
            | _ :: _ -> Error refusals
            | [] ->
                // A file that lowers to no statements writes no module, so it joins neither
                // the barrel nor the collision check.
                let emitted =
                    [
                        for c in compiled do
                            match c.Artifact with
                            | Ok artifact when not artifact.IsEmpty ->
                                {
                                    Source = c.Source
                                    Path = c.Path
                                    Artifact = artifact
                                }
                            | _ -> ()
                    ]

                let assets =
                    emitted
                    |> List.collect (fun m -> m.Artifact.RuntimeModules)
                    |> List.distinctBy (fun (a: JsRuntimeModule) -> a.Path)

                match modulePathCollisions packageName emitted with
                | _ :: _ as errors -> Error errors
                | [] ->
                    checkResolvable packageName emitted assets

                    Ok
                        {
                            Name = packageName
                            Modules = emitted
                            RuntimeAssets = assets
                        }
        )

    /// Write `package` under the output `root`: its modules and barrel into `<root>/<Name>/`,
    /// each committed runtime file into its own package's directory.
    ///
    /// KNOWN DEFECT: when a module imports a runtime asset of its OWN package, the contract's
    /// asset barrel rides along in `RuntimeAssets` and `materialiseAssets` writes it over the
    /// generated barrel written here.
    let materialise (root: string) (package: JsPackage) : unit =
        // Creating the package directory creates the root it sits in.
        let dir = Path.Combine(root, package.Name)
        Directory.CreateDirectory dir |> ignore

        for m in package.Modules do
            File.WriteAllText(Path.Combine(dir, m.Path.FileName), m.Artifact.Source)

            match m.Artifact.Map with
            | Some map -> File.WriteAllText(Path.Combine(dir, m.Path.FileName + ".map"), map)
            | None -> ()

        File.WriteAllText(Path.Combine(dir, BarrelFileName), package.Barrel)
        Codegen.materialiseAssets root package.RuntimeAssets
