namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// The input source as the map needs it: its text, its tokens, and the file name to record
/// for it. Its presence is what turns on source-map emission, so the three travel as one
/// value rather than as a string whose emptiness doubles as an on/off flag.
type JsSource =
    {
        /// The source file name recorded in the map's `sources` array
        /// (e.g. `program.fsx`). Cosmetic — the resolver works off offsets.
        Path: string
        /// The original source text, embedded verbatim as the map's `sourcesContent`.
        Content: string
        /// The tokens `Content` lexed to.
        Lexed: Lexed
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
        /// The package directory this module is emitted INTO — one member of a package
        /// build, which sits at `<out>/<Package>/`. `None` is a program at the output
        /// root. Import specifiers are rendered relative to it, so a sibling module, a
        /// root-level runtime asset and another package's module each get the right
        /// number of `../` hops.
        Package: string option
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
            Package = None
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
        /// The committed JS runtime ASSETS this program imports, written to the output
        /// ROOT by `materialise` so Node can resolve the emitted specifiers.
        RuntimeModules: JsRuntimeModule list
        /// `true` when the file lowered to NO statements — an intrinsic-repr-only source
        /// that contributes no module at all. A package build writes no `.mjs` for it and
        /// leaves it out of the barrel.
        IsEmpty: bool
    }

module Codegen =

    /// Basename of the generated `.js` file — drives the map's `file` field.
    let private jsFileName (project: JsProjectInfo) : string =
        match project.OutputPath with
        | Some path -> System.IO.Path.GetFileName path
        | None -> project.ModuleName + ".mjs"

    /// Frozen TAST → in-memory JS artifact. When `Source` is `Some`, a V3 source map
    /// is produced.
    ///
    /// ONE contract, never a provider beside a manifest set: `contract.Provider` resolves
    /// external union/record shapes AND served the inline bodies whose nodes the tree carries,
    /// and `contract.Origins` is the domain those nodes' positions index. Handed separately
    /// they can disagree, and the disagreement is invisible — a producer file missing from the
    /// domain is not a fault but a fall back to the CALL SITE's position, which is in range,
    /// plausible, and names the wrong file. `SymbolProviders.Contract.empty` compiles a program
    /// that touches no package: nothing retained, single-source map.
    ///
    /// A `Default`-shaped TS export (mitt's factory) lowers to a default import with no
    /// extra wiring here: the fact rides the resolved symbol (`ExternalSymbol.ImportForm`,
    /// stamped by the TS-manifest provider) and is read at the `JsImports.addRef` site.
    let compileWith (contract: SymbolProviders.Contract) (project: JsProjectInfo) (tast: FrozenPools) : JsArtifact =
        // Resolve `[targets.js] runtime` assets from the CONTRACT's manifest set; only the
        // referenced subset is materialised.
        let runtimeAssets =
            ReferencedProject.runtimeModules Target.Js contract.ManifestPaths
            |> Map.map (fun _ (fileName, source) -> { FileName = fileName; Source = source })

        // A node's anchor is an index into the file's token table, so a map needs the table
        // as well as the line starts. Both come from `project.Source` — the table is the
        // front end's own, the one the anchors were numbered against.
        //
        // The producer files are the contract's own — the collection whose bodies the provider
        // below serves — so a body an entry serves and the file its anchors index are the same
        // read by construction. An empty contract retains nothing and gets the single-source
        // map a one-file program always got.
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

        // The file's trees as columns, with an append-only overlay stacked over them:
        // every node this emission derives (the `substVar` splice of an inlinable `let`)
        // is appended to the overlay mid-walk, and every id the canonical pool already
        // handed out keeps naming the same node.
        let pool = TastPoolBuilder.openOver tast

        let selfPackage =
            match project.Package with
            | Some p -> ValueSome p
            | None -> ValueNone

        let ctx =
            EmitJsContext.WalkCtx.create
                resolver
                pool
                contract.Provider
                (JsImports.createIn selfPackage runtimeAssets)
                (match project.Kind with
                 | Library -> true
                 | Script -> false)

        let program = EmitJs.buildProgram ctx
        let result = JsPrint.print program
        let jsFile = jsFileName project

        let runtimeModules = JsImports.assets ctx.Imports

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

        // The consuming file is index 0 and the producers `buildProgram` published follow, in
        // the slot order it assigned them — the same order every mapping's `SrcIndex` was
        // resolved against.
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
            // Imports are recorded WHILE the body is built, so a program with no
            // statements imported nothing either — emptiness is the one test.
            IsEmpty = List.isEmpty program.Body
        }

    /// `compileWith` over the empty contract — for a program that references no external
    /// union/record and imports no package runtime: nothing retained, single-source map.
    let compile (project: JsProjectInfo) (tast: FrozenPools) : JsArtifact =
        compileWith SymbolProviders.Contract.empty project tast

    let toSource (artifact: JsArtifact) : string = artifact.Source
    let toSourceMap (artifact: JsArtifact) : string option = artifact.Map

    /// Copy the referenced runtime ASSETS into `root` — the output root, where every
    /// specifier that names one resolves from.
    let materialiseAssets (root: string) (assets: JsRuntimeModule list) : unit =
        for rt in assets do
            System.IO.File.WriteAllText(System.IO.Path.Combine(root, rt.FileName), rt.Source)

    /// Write the `.js` to `OutputPath`, runtime assets alongside it, and the map to
    /// `MapPath`. A program is emitted AT the output root, so its own directory is that
    /// root; a package member is written by the package driver instead.
    let materialise (artifact: JsArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            System.IO.File.WriteAllText(path, artifact.Source)
            materialiseAssets (System.IO.Path.GetDirectoryName path) artifact.RuntimeModules
        | None -> ()

        match artifact.MapPath, artifact.Map with
        | Some path, Some map -> System.IO.File.WriteAllText(path, map)
        | _ -> ()
