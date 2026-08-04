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
        /// The module's file name inside the package directory (`<stem>.mjs`).
        FileName: string
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
        |> List.map (fun m -> sprintf "export * from \"./%s\";\n" m.FileName)
        |> String.concat ""

    /// Compile an ordered `(path, source)` list as ONE assembly named `packageName`,
    /// through `analyse` (`Pipeline.analyseFor` for a package consumer,
    /// `analyseForSelfHost` for a BCL-free package), emitting one module per emitting file.
    ///
    /// Each file is EMITTED against exactly the provider it was ANALYSED against — the
    /// prior files' views nearest-first ahead of the contract — so an `External` node
    /// resolves at emission to the same symbol the front end resolved it to, and a
    /// cross-file reference names the declaring file's module through the origin that view
    /// stamped.
    let compileAssemblyWith
        (analyse: AssemblyFiles.AnalyseFile)
        (contract: SymbolProviders.Contract)
        (packageName: string)
        (files: (string * string) list)
        : Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list> =
        AssemblyFiles.analyseGated analyse packageName contract.Provider files
        |> Result.map (fun analysed ->
            // Prior views in FILE order; the head is the nearest file after each push, so
            // reversing before composing puts the NEAREST first — the same layering the
            // analysis ran under.
            let mutable priorViews: IExternalSymbolProvider list = []
            // The retention grows WITH the views, because they are halves of one contract: a
            // prior file that serves a body is a producer like any package, and a node spliced
            // out of it stays readable only against that file's own text.
            let mutable origins = contract.Origins
            let emitted = ResizeArray<JsPackageModule>()

            for file in analysed do
                let relative = file.Source.File.Path.Relative

                let scoped =
                    { contract with
                        Provider = ExternalSymbolProviders.composite ((List.rev priorViews) @ [ contract.Provider ])
                        Origins = origins
                    }

                let project =
                    { JsProjectInfo.defaults (JsModulePath.stem relative) with
                        Package = Some packageName
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

                let artifact = Codegen.compileWith scoped project file.Frozen

                // A source that lowers to no statements (an intrinsic-repr-only file) gets
                // no module at all — an absent `.mjs` says what an empty one would not.
                if not artifact.IsEmpty then
                    emitted.Add
                        {
                            Source = relative
                            FileName = JsModulePath.stem relative + ".mjs"
                            Artifact = artifact
                        }

                priorViews <- file.View :: priorViews
                origins <- OriginSources.add file.Source origins

            let modules = List.ofSeq emitted

            {
                Name = packageName
                Modules = modules
                Barrel = barrelOf modules
                RuntimeAssets =
                    modules
                    |> List.collect (fun m -> m.Artifact.RuntimeModules)
                    |> List.distinctBy (fun a -> a.FileName)
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
            File.WriteAllText(Path.Combine(dir, m.FileName), m.Artifact.Source)

            match m.Artifact.Map with
            | Some map -> File.WriteAllText(Path.Combine(dir, m.FileName + ".map"), map)
            | None -> ()

        File.WriteAllText(Path.Combine(dir, BarrelFileName), package.Barrel)
        Codegen.materialiseAssets root package.RuntimeAssets
