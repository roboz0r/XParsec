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
    let compileWith (contract: SymbolProviders.Contract) (project: JsProjectInfo) (tast: FrozenPools) : JsArtifact =
        let runtimeAssets =
            ReferencedProject.runtimeModules Target.Js contract.ManifestPaths
            |> Map.map (fun _ (fileName, source) -> { FileName = fileName; Source = source })

        // A node's anchor is an index into the token table, so a position needs `src.Lexed`, the
        // very table the anchors were numbered against, not just the line starts.
        let resolver: EmitJsContext.Resolver =
            match project.Source with
            | Some src ->
                ValueSome
                    {
                        Lexed = src.Lexed
                        Lines = JsMapSources.LineIndex.build src.Content
                        Origins = contract.Origins
                    }
            | None -> ValueNone

        let pool = TastPoolBuilder.openOver tast

        let ctx =
            EmitJsContext.WalkCtx.create
                resolver
                pool
                contract.Provider
                (JsImports.createIn project.Package runtimeAssets)
                (match project.Kind with
                 | Library -> true
                 | Script -> false)

        let program = EmitJs.buildProgram ctx
        let result = JsPrint.print program
        let jsFile = jsFileName project

        let runtimeModules = JsImports.assets ctx.Imports

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

        // The consuming file must land at index 0: its own nodes were resolved against source
        // index 0, and each published producer numbered itself from 1.
        let map =
            project.Source
            |> Option.map (fun src ->
                let consuming: JsMapSource =
                    {
                        Path = src.Path
                        Content = src.Content
                    }

                let sources = consuming :: JsMapSources.MapSources.published ctx.MapSources

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
            ImportedModules = JsImports.importedModules ctx.Imports
            IsEmpty = List.isEmpty program.Body
        }

    /// `compileWith` over the empty contract: a program that references no external
    /// union/record and imports no package runtime, so its map has the one source.
    let compile (project: JsProjectInfo) (tast: FrozenPools) : JsArtifact =
        compileWith SymbolProviders.Contract.empty project tast

    let toSource (artifact: JsArtifact) : string = artifact.Source
    let toSourceMap (artifact: JsArtifact) : string option = artifact.Map

    let materialiseAssets (root: string) (assets: JsRuntimeModule list) : unit =
        for rt in assets do
            System.IO.File.WriteAllText(System.IO.Path.Combine(root, rt.FileName), rt.Source)

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
