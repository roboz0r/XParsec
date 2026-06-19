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
    }

module JsProjectInfo =
    let defaults (moduleName: string) : JsProjectInfo =
        {
            ModuleName = moduleName
            OutputPath = None
            Source = None
            Kind = Script
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
    /// `ExternalSymbols.nullProvider` for a program that touches none. `manifestPaths`
    /// is the package set whose `runtime-js` assets back the program's runtime imports.
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

        let resolver: EmitJs.Resolver =
            match project.Source with
            | Some src -> ValueSome(EmitJs.LineIndex.build src.Content)
            | None -> ValueNone

        // Source text drives variable naming (recovering source identifiers from binder
        // offsets) independently of whether maps are emitted.
        let ctx: EmitJs.WalkCtx =
            {
                Resolver = resolver
                Source =
                    match project.Source with
                    | Some src -> ValueSome src.Content
                    | None -> ValueNone
                Records = System.Collections.Generic.Dictionary()
                Unions = System.Collections.Generic.Dictionary()
                Provider = ValueSome provider
                ExternalUnions = System.Collections.Generic.Dictionary()
                Imports = JsImports.create runtimeAssets
                ExportTopLevel =
                    match project.Kind with
                    | Library -> true
                    | Script -> false
                // Populated by `buildProgram` from the lowered decls.
                CompiledFns = System.Collections.Generic.Dictionary()
            }

        let result = JsPrint.print (EmitJs.buildProgram ctx tast)
        let jsFile = jsFileName project

        let runtimeModules = JsImports.modules ctx.Imports

        let map =
            project.Source
            |> Option.map (fun src -> JsSourceMap.build jsFile src.Path src.Content result.Mappings)

        {
            Source =
                match map with
                | Some _ -> result.Source + sprintf "//# sourceMappingURL=%s.map\n" jsFile
                | None -> result.Source
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
        compileWith ExternalSymbols.nullProvider [] project tast

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
