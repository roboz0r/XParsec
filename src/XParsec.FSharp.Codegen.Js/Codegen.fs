namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

type JsSource =
    {
        /// The file name recorded in the map's `"sources"` array, e.g. `program.fsx`.
        Path: string
        /// The original source text, embedded verbatim as the map's `sourcesContent`.
        Content: string
        /// The tokens `Content` lexed to.
        Lexed: Lexed
    }

/// `Library` emits a top-level `let` as `export const name = …`, so another module can
/// `import { name }` from it; `Script` emits a bare `const name = …`.
type JsCompileKind =
    | Script
    | Library

type JsProjectInfo =
    {
        ModuleName: string
        /// The directory this module is emitted into, `<out>/<Package>/`; `ValueNone` is a
        /// program at the output root. Import specifiers are rendered relative to it.
        Package: string voption
        /// `None` keeps the emitted source in-memory only.
        OutputPath: string option
        /// `Some` turns on V3 source-map emission.
        Source: JsSource option
        Kind: JsCompileKind
        /// `Some src` prepends `// Generated from <src>` as the emitted file's first line.
        GeneratedFrom: string option
    }

module JsProjectInfo =
    let defaults (moduleName: string) : JsProjectInfo =
        {
            ModuleName = moduleName
            Package = ValueNone
            OutputPath = None
            Source = None
            Kind = Script
            GeneratedFrom = None
        }

type JsArtifact =
    {
        Source: string
        OutputPath: string option
        /// The V3 source-map JSON, `None` when no source text was supplied.
        Map: string option
        /// `<OutputPath>.map`, `None` when not writing to disk or no map.
        MapPath: string option
        /// The committed JS runtime assets this program imports, copied beside the
        /// output file by `materialise`.
        RuntimeModules: JsRuntimeModule list
        ImportedModules: JsModulePath list
        /// `true` when the file lowered to no statements, as an intrinsic-repr-only source does.
        IsEmpty: bool
    }

module Codegen =

    let private jsFileName (project: JsProjectInfo) : string =
        match project.OutputPath with
        | Some path -> System.IO.Path.GetFileName path
        | None -> project.ModuleName + ".mjs"

    /// `contract.Provider` served the inline bodies whose nodes the tree carries; `contract.Origins`
    /// is the anchor domain those nodes index. Mixed from two contracts, a served body's file has
    /// no retained source and emission throws rather than reporting a plausible wrong position.
    let private emit
        (contract: PackageProviders.AnalysedManifest)
        (project: JsProjectInfo)
        (tast: FrozenPools)
        : JsArtifact =
        let runtimeAssets = contract.RuntimeAssets |> Map.map JsPackageOutput.ofAssets

        // A node's anchor is an index into the token table, so a position needs `src.Lexed`, the
        // very table the anchors were numbered against, not just the line starts.
        let resolver: EmitJsContext.Resolver =
            match project.Source with
            | Some src ->
                ValueSome
                    {
                        Lexed = src.Lexed
                        Lines = JsMapSources.LineIndex.build src.Content
                        Retained = contract.Retained
                    }
            | None -> ValueNone

        let pool = TastPoolBuilder.openOver tast

        let imports = JsImports.createIn project.Package runtimeAssets

        let inputs =
            EmitJsContext.EmissionInputs.create
                resolver
                pool
                contract.Provider
                imports
                (match project.Kind with
                 | Library -> true
                 | Script -> false)

        let program = EmitJs.buildProgram inputs
        let result = JsPrint.print program
        let jsFile = jsFileName project

        let runtimeModules = JsImports.assets imports

        // The header occupies generated line 0, so every mapping shifts down one.
        let header =
            match project.GeneratedFrom with
            | Some src -> sprintf "// Generated from %s\n" src
            | None -> ""

        let mappings =
            if header = "" then
                result.Mappings
            else
                result.Mappings |> List.map (fun m -> { m with GenLine = m.GenLine + 1 })

        let body = header + result.Source

        // The compiling file must land at index 0: its own nodes were resolved against source
        // index 0, and each published declaring file numbered itself from 1.
        let map =
            project.Source
            |> Option.map (fun src ->
                let compiling: JsMapSource =
                    {
                        Path = src.Path
                        Content = src.Content
                    }

                let sources = compiling :: JsMapSources.MapSources.published inputs.MapSources

                JsSourceMap.build jsFile sources mappings
            )

        {
            Source =
                match map with
                | Some _ -> body + sprintf "//# sourceMappingURL=%s.map\n" jsFile
                | None -> body
            OutputPath = project.OutputPath
            Map = map
            MapPath =
                match map, project.OutputPath with
                | Some _, Some path -> Some(path + ".map")
                | _ -> None
            RuntimeModules = runtimeModules
            ImportedModules = JsImports.importedModules imports
            IsEmpty = List.isEmpty program.Body
        }

    /// Every file of a gated assembly lowered to its own module, in manifest order, each
    /// against the provider it was analysed under.
    let emitAssembly
        (contract: PackageProviders.AnalysedManifest)
        (assembly: EmittableAssembly)
        : (AssemblyFileId * JsArtifact) list =
        // A module is written into its own assembly's directory, which is what a sibling
        // import's specifier resolves against.
        let packageName = assembly.Assembly.Name.Name

        // A spliced node resolves its positions against its declaring file's own text, so the
        // assembly's sources join the references' before any file is emitted.
        let origins = LexedFiles.addAll assembly.Retained contract.Retained

        [
            for file in assembly.Files do
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

                fileId,
                emit
                    { contract with
                        Provider = file.Scoped
                        Retained = origins
                    }
                    project
                    file.Frozen
        ]

    /// `emit`, GATED: a tree carrying an error-severity diagnostic has no defined lowering,
    /// so emission is refused and the findings come back.
    let compileWith
        (contract: PackageProviders.AnalysedManifest)
        (project: JsProjectInfo)
        (tast: FrozenPools)
        : Result<JsArtifact, Diagnostic list> =
        match FrozenPools.blockingErrors tast with
        | _ :: _ as errors -> Error errors
        | [] -> Ok(emit contract project tast)

    /// `compileWith` over the empty contract: a program that references no external
    /// union/record and imports no package runtime, so its map has the one source.
    let compile (project: JsProjectInfo) (tast: FrozenPools) : Result<JsArtifact, Diagnostic list> =
        compileWith PackageProviders.AnalysedManifest.empty project tast

    let toSource (artifact: JsArtifact) : string = artifact.Source
    let toSourceMap (artifact: JsArtifact) : string option = artifact.Map

    let materialiseAssets (root: string) (assets: JsRuntimeModule list) : unit =
        for rt in assets do
            let dir =
                match rt.Path.Package with
                | ValueSome package -> System.IO.Path.Combine(root, package)
                | ValueNone -> root

            System.IO.Directory.CreateDirectory dir |> ignore
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, rt.Path.FileName), rt.Source)

    /// Assets land in `OutputPath`'s own directory, which for a program IS the output root
    /// every emitted specifier resolves from.
    let materialise (artifact: JsArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            System.IO.File.WriteAllText(path, artifact.Source)
            materialiseAssets (System.IO.Path.GetDirectoryName path) artifact.RuntimeModules
        | None -> ()

        match artifact.MapPath, artifact.Map with
        | Some path, Some map -> System.IO.File.WriteAllText(path, map)
        | _ -> ()
