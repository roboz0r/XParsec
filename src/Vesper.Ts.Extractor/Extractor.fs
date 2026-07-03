/// TS → manifest extractor (Fable-compiled to JS, run under Node) — the DRIVER:
/// program construction, module resolution, and manifest emission. The PRODUCER
/// end of the slice: drives the TypeScript compiler API over a `.d.ts` surface
/// (single file or a package's cross-file closure) and emits a
/// `Vesper.Ts.Manifest` JSON file the F# `TsManifestProvider` consumes. The .NET
/// analog is `MetadataSymbols` reading assemblies through `MetadataLoadContext`;
/// here the oracle is `ts.TypeChecker`.
///
/// Wired against the vendored bindings (`vendor/TypeScript.fs`, from Glutinum,
/// MIT). The walk itself lives in the sibling modules: `TypeMap` (ts.Type →
/// `Schema.TypeRef`), `ExportMap` (symbols → `Schema.Export`), `Diagnostics`
/// (the degradation channel), `TsInterop` (node/ts primitives).
module Vesper.Ts.Extractor.Extractor

open Fable.Core
open Fable.Core.JsInterop

open TypeScript
open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.TsInterop
open Vesper.Ts.Extractor.Diagnostics
open Vesper.Ts.Extractor.ExportMap

let private moduleSymbolOf (checker: Ts.TypeChecker) (sf: Ts.SourceFile) (label: string) : Ts.Symbol =
    match checker.getSymbolAtLocation (unbox sf) with
    | None -> failwithf "'%s' is not a module (no exports found)" label
    | Some moduleSym -> moduleSym

// Compiler options shared by both paths. `strict` keeps `T | null` from collapsing
// to `T` (strictNullChecks); `skipLibCheck`/`noEmit` keep the run lib-agnostic and
// side-effect-free. The package path additionally needs module resolution wired (it
// resolves a bare specifier through node's algorithm), but adding those options to
// the single-file program would not change its exports — so for safety the
// single-file builder is left byte-for-byte as before and the package builder layers
// the resolution options on top.
let private baseOptions () : Ts.CompilerOptions =
    jsOptions<Ts.CompilerOptions> (fun o ->
        o.strict <- Some true
        o.skipLibCheck <- Some true
        o.noEmit <- Some true
    )

let extractFile (dtsPath: string) (packageName: string) : Schema.PackageManifest =
    let options = baseOptions ()
    let program = ts.createProgram (ResizeArray [ dtsPath ], options)
    let checker = program.getTypeChecker ()

    match program.getSourceFile dtsPath with
    | None -> failwithf "could not load source file '%s'" dtsPath
    | Some sf ->
        let moduleSym = moduleSymbolOf checker sf dtsPath
        let diags = ResizeArray<Schema.Diagnostic>()
        let refs = ResizeArray<string * Schema.RefEntry>()
        let exports = extractModuleExports checker program diags refs moduleSym

        {
            SchemaVersion = Schema.SchemaVersion
            Package = packageName
            // A single local `.d.ts` carries no package version (no resolving
            // `package.json`), so the stamp stays `null` — preserved exactly.
            Version = None
            Exports = exports
            // Per-type degradations recorded during the walk: a faithful
            // representation was impossible but the type could be named, so it was
            // degraded + diagnosed rather than aborting the extraction. Spans are
            // relativized against the `.d.ts`'s directory so the manifest is portable.
            Diagnostics = drainDiagnostics (pathDirname dtsPath) diags
            // Foreign named references homed at extraction (identity only): a
            // default-lib type → `es2015`, an external package → its specifier, a LOCAL
            // type → no entry. Deduped by bare name; empty stays codec-omitted.
            Refs = drainRefs refs
        }

/// The package version stamp (item 18). Preference order:
///   1. the resolver's `packageId.version` — populated when the entry was resolved
///      out of `node_modules` (an installed `@types/*` package);
///   2. the nearest `package.json`'s `version` walking up from the resolved entry —
///      the local / relative case the resolver leaves `packageId`-less.
/// Returns `None` only when neither yields a string (the stamp is genuinely absent).
let private packageVersionOf (resolvedModule: Ts.ResolvedModuleFull) (resolvedFileName: string) : string option =
    let fromPackageId =
        match resolvedModule.packageId with
        | Some pid when not (System.String.IsNullOrEmpty pid.version) -> Some pid.version
        | _ -> None

    match fromPackageId with
    | Some _ -> fromPackageId
    | None ->
        // Walk up from the entry file's directory to the filesystem root, taking the
        // FIRST `package.json` found — the package's own manifest sits closest, so it
        // wins over any ancestor (a monorepo root, the repo itself).
        let rec walk (dir: string) : string option =
            let pj = pathJoin dir "package.json"

            if existsSync pj then
                jsonVersionField (JS.JSON.parse (readFileSyncUtf8 pj "utf8"))
            else
                let parent = pathDirname dir

                if parent = dir then None else walk parent

        walk (pathDirname resolvedFileName)

/// Pull a package's full `.d.ts` module-graph closure via the synthetic-entry-file
/// approach (item 18). A throwaway entry module that `export *`-s the requested
/// `specifier` is written into `resolveFromDir` (so BOTH relative specifiers and
/// `node_modules` resolution anchor there); `ts.createProgram` over it pulls the entry
/// plus everything it re-exports/imports across files. We then resolve the specifier to
/// the package ENTRY source file and walk ITS exports (following its cross-file
/// re-exports) — not the synthetic entry's — and stamp the package version.
let extractPackage (specifier: string) (resolveFromDir: string) (packageName: string) : Schema.PackageManifest =
    let options = baseOptions ()
    // Module resolution must be wired for the bare/relative specifier to resolve and
    // for the closure to be pulled. Node10 is the classic node algorithm (honours a
    // package's `types`/`typings` and `index.d.ts`); ESNext module keeps `export *`
    // an ES re-export. Set via the typed enum constants, never raw numerics.
    options.moduleResolution <- Some Ts.ModuleResolutionKind.Node10
    options.``module`` <- Some Ts.ModuleKind.ESNext

    let host = ts.createCompilerHost options
    // The synthetic entry lives in `resolveFromDir` under a reserved name; a `.ts`
    // (not `.d.ts`) so its `export *` is an ordinary module re-export. Removed in the
    // `finally` so a fixture directory is never left polluted, even on a throw.
    let entryPath = pathJoin resolveFromDir "__vesper_synthetic_entry__.ts"
    writeFileSync entryPath (sprintf "export * from \"%s\";\n" specifier)

    try
        let program = ts.createProgram (ResizeArray [ entryPath ], options, host)
        let checker = program.getTypeChecker ()

        // Resolve the specifier to the package's entry `.d.ts` (+ version) through the
        // SAME host/options the program used, so the resolved path matches a program
        // source file. `host` is a `CompilerHost`, a subtype of the `ModuleResolutionHost`
        // the resolver wants.
        let resolution = ts.resolveModuleName (specifier, entryPath, options, host)

        match resolution.resolvedModule with
        | None -> failwithf "could not resolve package '%s' from '%s'" specifier resolveFromDir
        | Some resolvedModule ->
            let resolvedFileName = resolvedModule.resolvedFileName

            // The program loaded the closure keyed by TS's normalised file names
            // (forward slashes). `getSourceFile` keys on the same normalisation, but the
            // resolver's path can differ by slash direction on Windows — so fall back to
            // a slash-normalised scan of the program's source files before giving up.
            let sf =
                match program.getSourceFile resolvedFileName with
                | Some sf -> sf
                | None ->
                    let target = normalizeSlashes resolvedFileName

                    match
                        program.getSourceFiles ()
                        |> Seq.tryFind (fun f -> normalizeSlashes f.fileName = target)
                    with
                    | Some sf -> sf
                    | None -> failwithf "resolved entry '%s' is not in the program" resolvedFileName

            let moduleSym = moduleSymbolOf checker sf resolvedFileName
            let diags = ResizeArray<Schema.Diagnostic>()
            let refs = ResizeArray<string * Schema.RefEntry>()
            let exports = extractModuleExports checker program diags refs moduleSym

            {
                SchemaVersion = Schema.SchemaVersion
                Package = packageName
                Version = packageVersionOf resolvedModule resolvedFileName
                Exports = exports
                // Spans relativized against the package resolve dir for portability.
                Diagnostics = drainDiagnostics resolveFromDir diags
                // Foreign named references homed at extraction (see `extractFile`): the
                // package's OWN cross-file types stay LOCAL (relative-resolved, not
                // external), so only default-lib / external-package refs land here.
                Refs = drainRefs refs
            }
    finally
        if existsSync entryPath then
            unlinkSync entryPath

let run (dtsPath: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractFile dtsPath packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length

let runPackage (specifier: string) (resolveFromDir: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractPackage specifier resolveFromDir packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length
