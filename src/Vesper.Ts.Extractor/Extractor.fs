/// TS → manifest extractor (Fable-compiled to JS, run under Node): drives the
/// TypeScript compiler API over a `.d.ts` surface and emits the JSON manifest the
/// F# `TsManifestProvider` consumes. Bindings vendored from Glutinum, MIT.
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

// `strict` keeps `T | null` from collapsing to `T` (strictNullChecks);
// `skipLibCheck`/`noEmit` keep the run lib-agnostic and side-effect-free.
let private baseOptions () : Ts.CompilerOptions =
    jsOptions<Ts.CompilerOptions> (fun o ->
        o.strict <- Some true
        o.skipLibCheck <- Some true
        o.noEmit <- Some true
    )

/// `baseOptions` + `noLib`. To extract `lib.es*.d.ts` AS CONTENT they must be explicit
/// inputs AND `noLib` set — otherwise TS loads them as the default lib and the
/// `isSourceFileDefaultLibrary` filter drops every one, yielding an empty manifest.
let private libOptions () : Ts.CompilerOptions =
    let o = baseOptions ()
    o.noLib <- Some true
    o

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
        // A single-file `.d.ts` is ONE module: no sibling-module home override.
        let exports =
            extractModuleExports checker program diags refs (fun _ -> None) moduleSym

        {
            SchemaVersion = Schema.SchemaVersion
            Package = packageName
            // A single local `.d.ts` has no resolving `package.json`, so no version.
            Version = None
            Exports = exports
            // Spans relativized against the `.d.ts`'s directory, so the manifest is portable.
            Diagnostics = finalizeDiagnostics (pathDirname dtsPath) diags
            Refs = finalizeRefs refs
        }

/// The resolver's `packageId.version` — populated only when the entry resolved out of
/// `node_modules` — else the nearest `package.json`'s `version` walking up from the
/// resolved entry, which covers the local / relative case.
let private packageVersionOf (resolvedModule: Ts.ResolvedModuleFull) (resolvedFileName: string) : string option =
    let fromPackageId =
        match resolvedModule.packageId with
        | Some pid when not (System.String.IsNullOrEmpty pid.version) -> Some pid.version
        | _ -> None

    match fromPackageId with
    | Some _ -> fromPackageId
    | None ->
        // The FIRST `package.json` walking up wins: the package's own manifest sits
        // closest, so it beats an ancestor's (a monorepo root, the repo itself).
        let rec walk (dir: string) : string option =
            let pj = pathJoin dir "package.json"

            if existsSync pj then
                jsonVersionField (JS.JSON.parse (readFileSyncUtf8 pj "utf8"))
            else
                let parent = pathDirname dir

                if parent = dir then None else walk parent

        walk (pathDirname resolvedFileName)

/// Pull a package's full `.d.ts` closure: a throwaway entry module that `export *`-s
/// `specifier` is written into `resolveFromDir` (so BOTH relative specifiers and
/// `node_modules` resolution anchor there), then the RESOLVED entry's exports are walked.
let extractPackage (specifier: string) (resolveFromDir: string) (packageName: string) : Schema.PackageManifest =
    let options = baseOptions ()
    // Node10 is the classic node algorithm (honours a package's `types`/`typings` and
    // `index.d.ts`); ESNext keeps the entry's `export *` an ES re-export.
    options.moduleResolution <- Some Ts.ModuleResolutionKind.Node10
    options.``module`` <- Some Ts.ModuleKind.ESNext

    let host = ts.createCompilerHost options
    // A `.ts`, not a `.d.ts`, so its `export *` is an ordinary module re-export.
    let entryPath = pathJoin resolveFromDir "__vesper_synthetic_entry__.ts"
    writeFileSync entryPath (sprintf "export * from \"%s\";\n" specifier)

    try
        let program = ts.createProgram (ResizeArray [ entryPath ], options, host)
        let checker = program.getTypeChecker ()

        // Resolve through the SAME host/options the program used, so the resolved path
        // matches a program source file.
        let resolution = ts.resolveModuleName (specifier, entryPath, options, host)

        match resolution.resolvedModule with
        | None -> failwithf "could not resolve package '%s' from '%s'" specifier resolveFromDir
        | Some resolvedModule ->
            let resolvedFileName = resolvedModule.resolvedFileName

            // Program source files are keyed by TS's normalised name (forward slashes),
            // but the resolver's path can differ by slash direction on Windows — so fall
            // back to a slash-normalised scan before giving up.
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

            let exports =
                extractModuleExports checker program diags refs (fun _ -> None) moduleSym

            {
                SchemaVersion = Schema.SchemaVersion
                Package = packageName
                Version = packageVersionOf resolvedModule resolvedFileName
                Exports = exports
                Diagnostics = finalizeDiagnostics resolveFromDir diags
                // The package's OWN cross-file types stay LOCAL (relative-resolved, not
                // external), so only default-lib / external-package refs land here.
                Refs = finalizeRefs refs
            }
    finally
        if existsSync entryPath then
            unlinkSync entryPath

/// `mapGlobalSymbol` with a per-symbol backstop: a throw is diagnosed
/// (`SymbolWalkFailed`) and the symbol DROPPED, so one exotic lib symbol never aborts
/// a whole real-scale extraction.
let private mapGlobalSymbolResilient (ctx0: MapCtx) (sym: Ts.Symbol) : Schema.Export option =
    try
        mapGlobalSymbol ctx0 sym
    with ex ->
        let span = tryDeclOf sym |> Option.map spanOfNode

        emitWarning
            ctx0
            Schema.DiagCode.SymbolWalkFailed
            (sym.getName ())
            span
            (sprintf "global symbol '%s' could not be extracted and was dropped: %s" (sym.getName ()) ex.Message)

        None

/// Enumerates the checker's GLOBAL scope rather than a module's exports. `noLib` = the
/// passed files ARE the lib and extract as content; without it the real default lib is
/// implicit and filtered out.
let private extractGlobalsCore (noLib: bool) (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    let options = if noLib then libOptions () else baseOptions ()
    let program = ts.createProgram (ResizeArray dtsPaths, options)
    let checker = program.getTypeChecker ()

    // The fixture's OWN source files: a global script references default-lib types
    // (`string`, `Array`), which must NOT be re-extracted here.
    let fixtureSources =
        program.getSourceFiles ()
        |> Seq.filter (fun sf -> not (program.isSourceFileDefaultLibrary sf))
        |> List.ofSeq

    // Global scope is program-wide and MERGED, so ONE query anchored at any fixture
    // file sees every global (including declarations merged in from the sibling files).
    let anchor =
        match fixtureSources with
        | sf :: _ -> unbox<Ts.Node> sf
        | [] -> failwithf "globals extraction loaded no fixture source files from %A" dtsPaths

    // `meaning` spans the type + value + function namespaces so a fused class-like pair
    // (type-side interface + value-side ctor var) surfaces as its one merged symbol.
    let meaning =
        Ts.SymbolFlags.Type ||| Ts.SymbolFlags.Value ||| Ts.SymbolFlags.Function

    // Fixture-declared = ANY declaration sits in a non-default-lib file, so a merged
    // interface with a half in each fixture file still qualifies.
    let isFixtureDeclared (sym: Ts.Symbol) : bool =
        match sym.declarations with
        | Some ds ->
            ds
            |> Seq.exists (fun d -> not (program.isSourceFileDefaultLibrary ((unbox<Ts.Node> d).getSourceFile ())))
        | None -> false

    // The FULL fixture-declared set, BEFORE the name skip-list: a skip-listed fused type
    // (`Object`) still has to mark its carrier (`ObjectConstructor`) consumed, else the
    // carrier survives standalone and double-represents the dropped intrinsic.
    let fixtureGlobals =
        checker.getSymbolsInScope (anchor, meaning)
        |> Seq.filter isFixtureDeclared
        |> List.ofSeq

    // A fused class-like global (`interface Map` + `declare var Map: MapConstructor`)
    // becomes ONE `Export.Class` that consumes the `MapConstructor` carrier, so the
    // carrier must not ALSO be emitted standalone.
    let consumedCarriers =
        fixtureGlobals
        |> List.choose (fun sym ->
            if isFusedClassLike sym then
                fusedCarrierSymbol checker sym
            else
                None
        )

    let isConsumed (sym: Ts.Symbol) : bool =
        consumedCarriers |> List.exists (fun c -> jsRefEq (box c) (box sym))

    let diags = ResizeArray<Schema.Diagnostic>()
    let refs = ResizeArray<string * Schema.RefEntry>()
    let ctx0 = MapCtx.Root checker program diags refs

    let exports =
        fixtureGlobals
        |> List.filter (fun sym -> not (isConsumed sym))
        // A TS-lib intrinsic-overlap interface (`Array`, `String`, `Object`, …) is NOT
        // emitted: Vesper already represents those values intrinsically. `Map`/`Set`/…
        // are absent from the list and extract normally.
        |> List.filter (fun sym -> not (intrinsicOverlapNames.Contains(sym.getName ())))
        |> List.choose (mapGlobalSymbolResilient ctx0)

    // Spans/refs relativize against the FIRST input's directory (the files are siblings).
    let baseDir = pathDirname (List.head dtsPaths)

    {
        SchemaVersion = Schema.SchemaVersion
        Package = packageName
        Version = None
        Exports = exports
        Diagnostics = finalizeDiagnostics baseDir diags
        Refs = finalizeRefs refs
    }

/// Ambient-global entry mode over a global-scope (script) `.d.ts`, which declares
/// GLOBALS and is NOT a module. Passing several files lets the checker MERGE their
/// cross-file interface declarations into one symbol before the walk.
let extractGlobals (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    extractGlobalsCore false dtsPaths packageName

/// The real-scale lib extraction: `noLib` + an explicit lib file set. The full
/// `lib.es2015.*.d.ts` closure plus `lib.es5.d.ts` go into ONE program and flatten into
/// ONE `Package = "es2015"` home — the home a default-lib ref is recorded against.
let extractLibGlobals (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    extractGlobalsCore true dtsPaths packageName

// ─── ambient-module entry mode ─── `@types/node` is neither a single module nor a
// global script: it declares DOZENS of quoted ambient modules (`declare module "fs"
// { … }`), each getting its own manifest homed `<pkg>/<module>` — its own refs home.

/// Filesystem-safe basename for a per-module manifest (`node:fs` → `node_fs`). Only the
/// FILENAME is sanitized; the manifest's `Package` home keeps `<pkg>/<module>`.
let private manifestBaseName (moduleName: string) : string =
    moduleName.Replace(":", "_").Replace("/", "_").Replace("\\", "_")

/// The unquoted name of an ambient module symbol: prefer the string literal off its
/// `declare module "…"` declaration, else de-quote the symbol name — TS stores an
/// ambient module symbol under its QUOTED name, `"fs"`.
let private ambientModuleName (sym: Ts.Symbol) : string =
    let fromDecl =
        match sym.declarations with
        | Some ds -> ds |> Seq.tryPick (fun d -> quotedModuleNameOf (unbox<Ts.Node> d))
        | None -> None

    match fromDecl with
    | Some n -> n
    | None ->
        let raw = sym.getName ()
        let n = raw.Length

        if n >= 2 && (raw.[0] = '"' || raw.[0] = '\'') then
            raw.Substring(1, n - 2)
        else
            raw

/// One `(artifactBaseName, manifest)` pair per quoted ambient module the fixture
/// declares. A module declared ENTIRELY in the default lib is TS's own (the lib's `"*"`
/// wildcard), not the fixture's, and is excluded.
let extractAmbientModules (dtsPaths: string list) (packageName: string) : (string * Schema.PackageManifest) list =
    let options = baseOptions ()
    let program = ts.createProgram (ResizeArray dtsPaths, options)
    let checker = program.getTypeChecker ()

    let fixtureAmbient =
        checker.getAmbientModules ()
        |> Seq.filter (fun m ->
            match m.declarations with
            | Some ds ->
                ds
                |> Seq.exists (fun d -> not (program.isSourceFileDefaultLibrary ((unbox<Ts.Node> d).getSourceFile ())))
            | None -> false
        )
        |> List.ofSeq

    // The set a cross-module ref is homed against: a ref to a module outside it is not ours.
    let ambientNames = fixtureAmbient |> List.map ambientModuleName |> Set.ofList

    let baseDir = pathDirname (List.head dtsPaths)

    fixtureAmbient
    |> List.map (fun moduleSym ->
        let moduleName = ambientModuleName moduleSym

        // A ref into a SIBLING ambient module homes to `<pkg>/<module>`; a same-module or
        // non-ambient ref returns `None`, leaving the default file-origin homing.
        let moduleHome (sym: Ts.Symbol) : string option =
            match tryDeclOf sym |> Option.bind enclosingQuotedModuleName with
            | Some m when m <> moduleName && ambientNames.Contains m -> Some(packageName + "/" + m)
            | _ -> None

        let diags = ResizeArray<Schema.Diagnostic>()
        let refs = ResizeArray<string * Schema.RefEntry>()
        let exports = extractModuleExports checker program diags refs moduleHome moduleSym

        manifestBaseName moduleName,
        {
            SchemaVersion = Schema.SchemaVersion
            Package = packageName + "/" + moduleName
            Version = None
            Exports = exports
            Diagnostics = finalizeDiagnostics baseDir diags
            Refs = finalizeRefs refs
        }
    )

let run (dtsPath: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractFile dtsPath packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length

let runPackage (specifier: string) (resolveFromDir: string) (packageName: string) (outPath: string) : unit =
    let manifest = extractPackage specifier resolveFromDir packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length

let runGlobals (dtsPaths: string list) (packageName: string) (outPath: string) : unit =
    let manifest = extractGlobals dtsPaths packageName
    writeFileSync outPath (Codec.serialize manifest)
    eprintfn "Wrote %s (%d exports)" outPath manifest.Exports.Length

let runLibGlobals (dtsPaths: string list) (packageName: string) (outPath: string) : unit =
    let manifest = extractLibGlobals dtsPaths packageName
    writeFileSync outPath (Codec.serialize manifest)

    eprintfn "Wrote %s (%d exports, %d diagnostics)" outPath manifest.Exports.Length manifest.Diagnostics.Length

/// Writes `<outDir>/<sanitized-module>.manifest.json` per quoted ambient module.
/// `outDir` must already exist.
let runAmbientModules (dtsPaths: string list) (packageName: string) (outDir: string) : unit =
    let manifests = extractAmbientModules dtsPaths packageName

    for (baseName, man) in manifests do
        let outPath = pathJoin outDir (baseName + ".manifest.json")
        writeFileSync outPath (Codec.serialize man)
        eprintfn "Wrote %s (%d exports)" outPath man.Exports.Length

    eprintfn "Extracted %d ambient module(s)" manifests.Length
