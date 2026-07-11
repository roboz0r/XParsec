namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// The input source text plus the file name to record for it. Its presence is
/// what turns on source-map emission, so the two travel as one unit rather than
/// as a string whose emptiness doubles as an on/off flag.
type JsSource =
    {
        /// The source file name recorded in the map's `sources` array
        /// (e.g. `program.fsx`). Cosmetic — the resolver works off offsets.
        Path: string
        /// The original source text. Embedded verbatim as the map's
        /// `sourcesContent` and used to resolve each node's `SyntaxToken`
        /// offset to (line, column).
        Content: string
    }

/// Whether top-level `let` bindings emit as module `export`s or plain `const`s.
/// `Script` keeps them as `const`; `Library` exports them so a consumer's
/// `import { name as $… } from "./<asm>.mjs"` resolves.
type JsCompileKind =
    | Script
    | Library

/// Per-build configuration for the JS backend. `OutputPath = None` keeps the emitted
/// source in-memory only.
type JsProjectInfo =
    {
        ModuleName: string
        /// `None` keeps the emitted source in-memory only.
        OutputPath: string option
        /// `Some` turns on V3 source-map emission; `None` keeps the output
        /// map-free.
        Source: JsSource option
        /// `Library` exports top-level bindings; `Script` keeps them as `const`.
        Kind: JsCompileKind
        /// `Some sourceFile` prepends a `// Generated from <sourceFile>` provenance
        /// line as the emitted file's first line — set when compiling a committed
        /// runtime asset (`Vesper.List.mjs` etc.) so the artifact records the Vesper
        /// source it was generated from. `None` emits no header (the default for an
        /// ordinary program). The header shifts every source-map mapping down one
        /// generated line, handled in `compileWith`.
        GeneratedFrom: string option
    }

module JsProjectInfo =
    let defaults (moduleName: string) : JsProjectInfo =
        {
            ModuleName = moduleName
            OutputPath = None
            Source = None
            Kind = Script
            GeneratedFrom = None
        }

/// The emitted artifact: the ESM source, its V3 source map (when source text was
/// supplied), and the resolved output paths (if any).
type JsArtifact =
    {
        Source: string
        OutputPath: string option
        /// The V3 source-map JSON, `None` when no source text was supplied.
        Map: string option
        /// `<OutputPath>.map`, `None` when not writing to disk or no map.
        MapPath: string option
        /// The JS runtime modules this program imports, written beside the output by
        /// `materialise` so Node can resolve the emitted `import … from "./<FileName>"`.
        RuntimeModules: JsRuntimeModule list
    }

module Codegen =

    /// Basename of the generated `.js` file — drives the map's `file` field.
    let private jsFileName (project: JsProjectInfo) : string =
        match project.OutputPath with
        | Some path -> System.IO.Path.GetFileName path
        | None -> project.ModuleName + ".mjs"

    /// Frozen TAST → in-memory JS artifact. When `Source` is `Some`, a V3 source map
    /// is produced. `provider` resolves external union/record shapes; pass
    /// `ExternalSymbolProviders.nullProvider` for a program that touches none. `manifestPaths`
    /// is the package set whose `runtime-js` assets back the program's runtime imports.
    /// A `Default`-shaped TS export (mitt's factory) lowers to a default import with no
    /// extra wiring here: the fact rides the resolved symbol (`ExternalSymbol.ImportForm`,
    /// stamped by the TS-manifest provider) and is read at the `JsImports.addRef` site.
    let compileWith
        (provider: IExternalSymbolProvider)
        (manifestPaths: string list)
        (project: JsProjectInfo)
        (tast: Frozen.TastFile)
        : JsArtifact =
        // Resolve `runtime-js` assets from the manifest set; only the referenced subset
        // is materialised. `"js"` is the JS backend's target suffix.
        let runtimeAssets =
            ReferencedProject.runtimeModules "js" manifestPaths
            |> Map.map (fun _ (fileName, source) -> { FileName = fileName; Source = source })

        let resolver: EmitJsContext.Resolver =
            match project.Source with
            | Some src -> ValueSome(EmitJsContext.LineIndex.build src.Content)
            | None -> ValueNone

        // Source text drives variable naming (recovering source identifiers from binder
        // offsets) independently of whether maps are emitted.
        let ctx =
            EmitJsContext.WalkCtx.create
                resolver
                (match project.Source with
                 | Some src -> ValueSome src.Content
                 | None -> ValueNone)
                provider
                (JsImports.create runtimeAssets)
                (match project.Kind with
                 | Library -> true
                 | Script -> false)

        let result = JsPrint.print (EmitJs.buildProgram ctx tast)
        let jsFile = jsFileName project

        let runtimeModules = JsImports.modules ctx.Imports

        // Optional provenance header as the file's first line; it shifts every
        // source-map mapping down one generated line so the map stays aligned.
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

        let map =
            project.Source
            |> Option.map (fun src -> JsSourceMap.build jsFile src.Path src.Content mappings)

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
        }

    /// `compileWith` with the null provider and empty manifest set — for a program
    /// that references no external union/record and imports no package runtime.
    let compile (project: JsProjectInfo) (tast: Frozen.TastFile) : JsArtifact =
        compileWith ExternalSymbolProviders.nullProvider [] project tast

    let toSource (artifact: JsArtifact) : string = artifact.Source
    let toSourceMap (artifact: JsArtifact) : string option = artifact.Map

    /// Write the `.js` to `OutputPath`, runtime modules alongside it, and the map to `MapPath`.
    let materialise (artifact: JsArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            System.IO.File.WriteAllText(path, artifact.Source)
            let dir = System.IO.Path.GetDirectoryName path

            for rt in artifact.RuntimeModules do
                System.IO.File.WriteAllText(System.IO.Path.Combine(dir, rt.FileName), rt.Source)
        | None -> ()

        match artifact.MapPath, artifact.Map with
        | Some path, Some map -> System.IO.File.WriteAllText(path, map)
        | _ -> ()
