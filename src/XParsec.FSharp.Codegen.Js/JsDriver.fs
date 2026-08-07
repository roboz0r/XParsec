namespace XParsec.FSharp.Codegen.Js

open System.IO
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// One EMITTING source file of a package build, compiled to its own `.mjs`.
type JsPackageModule =
    {
        /// The path this was compiled from (`a/one.fs`) — what a diagnostic is blamed on.
        Source: string
        Path: JsModulePath
        Artifact: JsArtifact
    }

/// A compiled JS package: `<Name>/<file>.mjs` per emitting source, each with its own 1:1
/// source map, plus the `index.mjs` barrel a consumer names the whole package by.
type JsPackage =
    {
        Name: string
        Modules: JsPackageModule list
        /// The runtime assets the modules import, written to the output ROOT:
        /// `<root>/Vesper.Core.mjs` beside `<root>/<Name>/`.
        RuntimeAssets: JsRuntimeModule list
    }

    /// The generated `index.mjs`: `export * from "./shapes.mjs";` per emitting module, in
    /// file order. A package whose files all lower to nothing gets an empty barrel, which
    /// still resolves.
    member this.Barrel: string =
        this.Modules
        |> List.map (fun m -> sprintf "export * from \"./%s\";\n" m.Path.FileName)
        |> String.concat ""

/// The production JS driver: an ordered multi-file assembly in, a directory of one `.mjs`
/// per emitting file out.
module JsDriver =

    [<Literal>]
    let BarrelFileName = "index.mjs"

    /// The resolution contract for a compilation that IS a package, over the JS-native leaf.
    let contractForSelf (target: string) (selfManifest: string) (references: string list) : SymbolProviders.Contract =
        JsNativeSymbols.jsNativeContractFor target (SymbolProviders.selfStack (Some selfManifest) references)

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
                            (claimants |> List.map (fun m -> sprintf "'%s'" m.Source) |> String.concat ", ")
                            path.FileName

                    for m in claimants do
                        yield!
                            AssemblyFiles.unpositionedDiagnostics m.Source [ Diagnostic.nowhere (Kind.Driver message) ]
                | _ -> ()
        ]

    /// The EMITTED modules' imports, each against what this build writes. An asset's own
    /// imports are NOT checked, nor closed over: `Vesper.Seq.mjs` imports `./Vesper.Core.mjs`,
    /// which is written only if the program reached Core directly too.
    let private checkResolvable
        (packageName: string)
        (modules: JsPackageModule list)
        (assets: JsRuntimeModule list)
        : unit =
        let written =
            (modules |> List.map (fun m -> m.Path))
            @ (assets |> List.map (fun a -> JsModulePath.asset a.FileName))
            |> Set.ofList

        for m in modules do
            for target in m.Artifact.ImportedModules do
                if not (written.Contains target) then
                    failwithf
                        "JS package '%s': module '%s' imports '%s', which this build does not write"
                        packageName
                        m.Path.FileName
                        (JsModulePath.specifierFrom m.Path.Package target)

    /// Compile an ordered `(path, source)` list as ONE assembly named `packageName`.
    let compileAssemblyWith
        (analyse: AssemblyFiles.AnalyseFile)
        (contract: SymbolProviders.Contract)
        (packageName: string)
        (files: (string * string) list)
        : Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list> =
        AssemblyFiles.analyseGated analyse packageName contract.Provider files
        |> Result.bind (fun analysed ->
            // A spliced node reads only against its declaring file's own text, so the
            // assembly's own sources join the references' before any file is emitted.
            let origins = OriginSources.addAll analysed.Origins contract.Origins

            let emitted =
                [
                    for file in analysed.Files do
                        let relative = file.Source.File.Path.Relative

                        let project =
                            { JsProjectInfo.defaults (JsModulePath.baseName relative) with
                                Package = ValueSome packageName
                                Kind = Library
                                GeneratedFrom = Some relative
                                Source =
                                    Some
                                        {
                                            Path = relative
                                            Content = file.Source.Input
                                            Lexed = file.Source.Lexed
                                        }
                            }

                        let artifact =
                            Codegen.compileWith
                                { contract with
                                    Provider = file.Scoped
                                    Origins = origins
                                }
                                project
                                file.Frozen

                        if not artifact.IsEmpty then
                            {
                                Source = relative
                                Path = JsModulePath.ofSource packageName relative
                                Artifact = artifact
                            }
                ]

            let assets =
                emitted
                |> List.collect (fun m -> m.Artifact.RuntimeModules)
                |> List.distinctBy (fun a -> a.FileName)

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
    /// the runtime assets into `<root>` itself.
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
