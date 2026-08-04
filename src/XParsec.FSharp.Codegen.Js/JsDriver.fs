namespace XParsec.FSharp.Codegen.Js

open System.IO
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// One EMITTING source file of a package build, compiled to its own ESM module. A source
/// that lowers to no statements produces none of these — it has no module, rather than an
/// empty one.
type JsPackageModule =
    {
        /// The source file this was compiled from, as the manifest names it.
        Source: string
        /// Where the module sits in the output tree. THE identity an importer's specifier
        /// is rendered against, so the writer and the importer name one value rather than
        /// two strings that have to agree.
        Path: JsModulePath
        Artifact: JsArtifact
    }

/// A compiled JS package: one `.mjs` per emitting source file, all inside a directory
/// named for the package, plus the `index.mjs` barrel that re-exports them so a consumer
/// has one specifier to name the package by. Pools are never merged and each module keeps
/// its own 1:1 source map — a package is a DIRECTORY of per-file modules, not one module
/// fused from many files.
type JsPackage =
    {
        Name: string
        Modules: JsPackageModule list
        /// The generated `index.mjs` source.
        Barrel: string
        /// The committed runtime ASSETS the package's modules import — copied to the
        /// output ROOT, beside the package directory, since a consumer names them from
        /// there too.
        RuntimeAssets: JsRuntimeModule list
    }

/// The library-level production JS driver: analyse an ordered multi-file assembly, gate on
/// its diagnostics, and emit ONE `.mjs` per emitting file. The per-assembly counterpart of
/// `Codegen.compileWith`, and the JS twin of `ClrDriver.compileAssemblyWith` — which emits
/// one PE for the same input, because the CLR's artifact is per-ASSEMBLY where this one is
/// per-FILE.
module JsDriver =

    /// The barrel's file name — the specifier a consumer names the whole package by.
    [<Literal>]
    let BarrelFileName = "index.mjs"

    /// The resolution contract for a compilation that IS a package: its declared
    /// references plus the package's own manifest last, over the JS-native leaf. The JS
    /// tail resolves no BCL metadata and canonicalizes nothing, so — unlike the CLR's —
    /// nothing has to be seeded from the package's own reverse axis.
    let contractForSelf (target: string) (selfManifest: string) (references: string list) : SymbolProviders.Contract =
        JsNativeSymbols.jsNativeContractFor target (SymbolProviders.selfStack (Some selfManifest) references)

    /// `export * from "./<stem>.mjs";` per emitting module, in file order — the barrel
    /// body. A package whose files all lower to nothing gets an empty barrel, which is
    /// still a resolvable specifier.
    let private barrelOf (modules: JsPackageModule list) : string =
        modules
        |> List.map (fun m -> sprintf "export * from \"./%s\";\n" m.Path.FileName)
        |> String.concat ""

    /// Every specifier the emitted modules name must resolve to something this build
    /// writes: a surviving per-file module, or a runtime asset copied to the output root.
    /// A file dropped for emitting nothing is decided per FILE while the reference to it is
    /// decided per CONSUMER, so nothing but this ties the two — and a dangling ESM
    /// specifier otherwise surfaces only when Node loads the package.
    let private checkResolvable
        (packageName: string)
        (modules: JsPackageModule list)
        (assets: JsRuntimeModule list)
        : unit =
        // A module path is a STEM, which drops directories, so two sources whose names
        // differ only by directory claim one `.mjs` — and the second write would replace
        // the first with no other sign.
        for path, claimants in modules |> List.groupBy (fun m -> m.Path) do
            match claimants with
            | _ :: _ :: _ ->
                failwithf
                    "JS package '%s': sources %s all emit '%s'; a module path is a file stem, so they cannot share a package directory"
                    packageName
                    (claimants |> List.map (fun m -> sprintf "'%s'" m.Source) |> String.concat ", ")
                    path.FileName
            | _ -> ()

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

    /// Compile an ordered `(path, source)` list as ONE assembly named `packageName`,
    /// through `analyse` (`Pipeline.analyseFor` for a package consumer,
    /// `analyseForSelfHost` for a BCL-free package), emitting one module per emitting file.
    ///
    /// Each file is emitted against the provider the front end CARRIED back from analysing
    /// it, so an `External` node resolves at emission to the same symbol the front end
    /// resolved it to, and a cross-file reference names the declaring file's module through
    /// the origin that view stamped.
    let compileAssemblyWith
        (analyse: AssemblyFiles.AnalyseFile)
        (contract: SymbolProviders.Contract)
        (packageName: string)
        (files: (string * string) list)
        : Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list> =
        AssemblyFiles.analyseGated analyse packageName contract.Provider files
        |> Result.map (fun analysed ->
            // The package's own files are producers like any referenced package: a node
            // spliced out of one stays readable only against that file's own text. A file
            // can only splice from one BEFORE it, so the whole assembly's retention covers
            // every emission.
            let origins = OriginSources.addAll analysed.Origins contract.Origins

            let emitted =
                [
                    for file in analysed.Files do
                        let relative = file.Source.File.Path.Relative

                        let project =
                            { JsProjectInfo.defaults (JsModulePath.stem relative) with
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

                        // A source that lowers to no statements (an intrinsic-repr-only file)
                        // gets no module at all — an absent `.mjs` says what an empty one
                        // would not.
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

            checkResolvable packageName emitted assets

            {
                Name = packageName
                Modules = emitted
                Barrel = barrelOf emitted
                RuntimeAssets = assets
            }
        )

    /// Write `package` under the output `root`: its modules and barrel into
    /// `<root>/<Name>/`, and the referenced runtime assets into `<root>` itself. The split
    /// is the model — a package BUILD produces a directory, a referenced asset is copied
    /// beside the consumer that names it, as a `.dll` sits beside the app referencing it.
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
