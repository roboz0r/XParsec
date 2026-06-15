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

/// Per-build configuration for the JS backend — far thinner than the CLR
/// `ProjectInfo` (no assembly references, no TFM): a JS module is a single `.js`
/// file. `OutputPath = None` keeps the emitted source in-memory.
type JsProjectInfo =
    {
        ModuleName: string
        /// `None` keeps the emitted source in-memory only.
        OutputPath: string option
        /// `Some` turns on V3 source-map emission; `None` keeps the output
        /// map-free.
        Source: JsSource option
    }

module JsProjectInfo =
    let defaults (moduleName: string) : JsProjectInfo =
        {
            ModuleName = moduleName
            OutputPath = None
            Source = None
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
        /// The JS runtime modules the program imports (Step 5b — `Vesper.List`),
        /// each written beside the output by `materialise` so Node can resolve the
        /// emitted `import … from "./<FileName>"`. Empty for a program that imports
        /// no package runtime.
        RuntimeModules: JsRuntimeModule list
    }

/// Entry point mirroring `Codegen.Clr.Codegen.compile`. `compileWith` takes the
/// `IExternalSymbolProvider` (Step 5: it resolves the case shapes of external
/// union types — `Option`, `List` — the file references but does not declare, so
/// they can be emitted as honest nominal JS classes); `compile` is the
/// null-provider convenience for the earlier scalar / `printfn` slices that touch
/// no library type.
module Codegen =

    /// The generated `.js` file name (basename) — drives the map's `file` field
    /// and the `sourceMappingURL` comment. Falls back to `<ModuleName>.mjs` when
    /// emitting in-memory only.
    let private jsFileName (project: JsProjectInfo) : string =
        match project.OutputPath with
        | Some path -> System.IO.Path.GetFileName path
        | None -> project.ModuleName + ".mjs"

    /// Frozen TAST → in-memory JS artifact. Deterministic given the same input.
    /// When `Source` is `Some` a V3 source map is produced and the emitted JS
    /// gains a trailing `//# sourceMappingURL` comment. The `provider` resolves
    /// external union/record shapes (`Option`, `List`) the file references but does
    /// not declare — they are emitted as honest nominal JS classes (Step 5); pass
    /// `ExternalSymbols.nullProvider` for a program that touches none.
    let compileWith (provider: IExternalSymbolProvider) (project: JsProjectInfo) (tast: Frozen.TastFile) : JsArtifact =
        let resolver: EmitJs.Resolver =
            match project.Source with
            | Some src -> ValueSome(EmitJs.LineIndex.build src.Content)
            | None -> ValueNone

        // The raw source text also drives `let`-bound variable naming (recovering
        // the source identifier from its binder token offset), independent of
        // whether maps are emitted.
        // The record table is filled by `buildProgram` from the file's type
        // declarations; the context starts with an empty one.
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
                ExternalUnionDecls = ResizeArray()
                Imports = JsImports.create ()
            }

        let result = JsPrint.print (EmitJs.buildProgram ctx tast)
        let jsFile = jsFileName project

        // The runtime modules the walk imported (`ctx.Imports` is the same shared
        // accumulator the walker populated): each home assembly's `JsRuntimeModule`,
        // resolved once at first reference, materialised beside the output so Node
        // resolves the `./<file>.mjs` imports.
        let runtimeModules = JsImports.modules ctx.Imports

        // Source text supplied ⇒ emit the V3 map and append the
        // `sourceMappingURL` comment pointing the runtime at `<jsFile>.map`.
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
            // The `.map` is written beside the `.js`, so it needs both an output
            // path and a map to exist.
            MapPath =
                match map, project.OutputPath with
                | Some _, Some path -> Some(path + ".map")
                | _ -> None
            RuntimeModules = runtimeModules
        }

    /// `compileWith` against the empty provider — for a program that references no
    /// external union/record (the `printfn`/scalar/source-map slices). A program
    /// that touches `Option` / `List` must use `compileWith`.
    let compile (project: JsProjectInfo) (tast: Frozen.TastFile) : JsArtifact =
        compileWith ExternalSymbols.nullProvider project tast

    /// The emitted ESM source text.
    let toSource (artifact: JsArtifact) : string = artifact.Source

    /// The V3 source-map JSON, when one was produced.
    let toSourceMap (artifact: JsArtifact) : string option = artifact.Map

    /// The side effects: write the `.js` to `OutputPath` and the `.js.map` to
    /// `MapPath` when those are set.
    let materialise (artifact: JsArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            System.IO.File.WriteAllText(path, artifact.Source)

            // Runtime modules sit next to the output `.js` so the emitted
            // `import … from "./<FileName>"` resolves under Node.
            let dir = System.IO.Path.GetDirectoryName path

            for rt in artifact.RuntimeModules do
                System.IO.File.WriteAllText(System.IO.Path.Combine(dir, rt.FileName), rt.Source)
        | None -> ()

        match artifact.MapPath, artifact.Map with
        | Some path, Some map -> System.IO.File.WriteAllText(path, map)
        | _ -> ()
