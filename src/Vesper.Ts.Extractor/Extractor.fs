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

/// `baseOptions` + `noLib` (Step 3's lib-extraction path). CRITICAL: to extract the
/// `lib.es*.d.ts` files AS CONTENT (rather than as the program's implicit DEFAULT
/// library) they MUST be passed as EXPLICIT inputs AND `noLib` must be set — otherwise
/// TS loads them as the default lib and `isSourceFileDefaultLibrary` filters EVERY one
/// out, yielding an empty manifest. With `noLib` the explicit lib files are ordinary
/// (non-default-library) source files, so the globals walk enumerates their globals.
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

/// Ambient-global entry mode. A global-scope (script) `.d.ts` — `lib.es*.d.ts`'s
/// shape — declares GLOBALS, it is NOT a module: `moduleSymbolOf` throws "not a
/// module" there (correctly fatal for the module entries), so this entry BYPASSES it
/// and enumerates the checker's GLOBAL scope directly. It takes a LIST of `.d.ts`
/// (the merge fixture spans two files) so `ts.createProgram` over all of them lets the
/// checker MERGE cross-file interface declarations into one symbol — the merged
/// symbol's declared type is the truth, so we enumerate by SYMBOL, never per-file
/// statement (which would emit duplicate/partial interfaces and MISS the merges).
/// Route ONE enumerated global through `mapGlobalSymbol`, but with a per-symbol
/// RESILIENCE backstop (Step 3): an unforeseen construct that makes the walk THROW is
/// caught, diagnosed (`SymbolWalkFailed`), and the symbol DROPPED — so one exotic lib
/// symbol never aborts the whole real-scale extraction. The known in-place degrades
/// (accessor / enum / heritage / intersection / structural) fire BELOW this and keep
/// their symbol; this only catches what those did not anticipate.
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

/// `extractGlobals` shared core. `noLib` selects the compiler options: the plain
/// ambient-globals fixture (`--globals`) runs WITHOUT it (its own files are non-lib,
/// the real default lib is implicit + filtered); the real `lib.es2015` pack
/// (`--lib-globals`) runs WITH it so the passed lib files extract as content.
let private extractGlobalsCore (noLib: bool) (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    let options = if noLib then libOptions () else baseOptions ()
    let program = ts.createProgram (ResizeArray dtsPaths, options)
    let checker = program.getTypeChecker ()

    // The fixture's OWN source files (everything the program loaded that is NOT the
    // default lib): a global script references default-lib types (`string`, `Array`),
    // which must NOT be re-extracted here — they are the ref pack's / Step 3's concern.
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

    // Keep only symbols the FIXTURE declares — a symbol is fixture-declared when ANY of
    // its declarations sits in a non-default-lib source file (a merged interface with a
    // half in each fixture file still qualifies on its first fixture declaration).
    let isFixtureDeclared (sym: Ts.Symbol) : bool =
        match sym.declarations with
        | Some ds ->
            ds
            |> Seq.exists (fun d -> not (program.isSourceFileDefaultLibrary ((unbox<Ts.Node> d).getSourceFile ())))
        | None -> false

    // The FULL fixture-declared set, BEFORE the name skip-list. `consumedCarriers` must
    // be computed over THIS set: a skip-listed fused type (e.g. `Object` = `interface
    // Object` + `declare var Object: ObjectConstructor`) is still fused class-like, and
    // its constructor-INTERFACE carrier (`ObjectConstructor`) must be marked consumed so
    // it is suppressed too — else the carrier survives as a standalone `Export.Interface`
    // that double-represents the very intrinsic the skip-list exists to drop. So compute
    // carriers first, THEN apply the name skip-list to the emit set.
    let fixtureGlobals =
        checker.getSymbolsInScope (anchor, meaning)
        |> Seq.filter isFixtureDeclared
        |> List.ofSeq

    // A fused class-like global is THREE symbols → ONE `Export.Class`: the type-side
    // interface, the value-side ctor var (already MERGED into the same symbol), and the
    // SEPARATE constructor-interface (`MapConstructor` / `ObjectConstructor`) the var's
    // type resolves to. That carrier is CONSUMED (its construct sigs → ctors, its other
    // members → statics), so it must not ALSO stand alone as an `Export.Interface`.
    // Collected over `fixtureGlobals` (pre-skip-list) so a skip-listed fused type's
    // carrier is included here even though the type itself is dropped from the emit set.
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
        // Primitive-overlap skip-list (Step 3): a TS-lib intrinsic-overlap interface
        // (`Array`, `String`, `Object`, …) is NOT emitted as an export — the ref pack
        // does not REGISTER the names Vesper already represents intrinsically (they ride
        // `IntrinsicRepr` / native JS arrays). `Map`/`Set`/… are absent from the list and
        // extract normally. Applied AFTER the consumed-carrier pass so both the type AND
        // its ctor-interface carrier are dropped. Consistent with `recordForeignRef`.
        |> List.filter (fun sym -> not (intrinsicOverlapNames.Contains(sym.getName ())))
        |> List.choose (mapGlobalSymbolResilient ctx0)

    // Spans/refs relativize against the FIRST input's directory (all fixture files are
    // siblings), mirroring `extractFile`.
    let baseDir = pathDirname (List.head dtsPaths)

    {
        SchemaVersion = Schema.SchemaVersion
        Package = packageName
        // The fixture carries no version; the reserved-home `es2015` version stamp is
        // Step 3's concern (this entry must not hardcode it).
        Version = None
        Exports = exports
        Diagnostics = drainDiagnostics baseDir diags
        Refs = drainRefs refs
    }

/// Ambient-global entry mode (see the header). The plain fixture form: no `noLib`, the
/// real default lib is implicit and filtered out.
let extractGlobals (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    extractGlobalsCore false dtsPaths packageName

/// The real-scale `lib.es2015` extraction (Step 3): `noLib` + the explicit lib file
/// set. Decision recorded here: the pack is "es2015 FLAT, INCLUDING es5" — the full
/// `lib.es2015.*.d.ts` closure plus `lib.es5.d.ts` are fed to ONE program (they
/// cross-`/// <reference>` each other) and flattened into ONE `Package = "es2015"`
/// home. That reserved home is exactly what Step-1B's refs point at (e.g. mitt's `Map`
/// homes to `es2015`), so a later stacked es2015 provider registers `Map` under
/// `es2015` and the refs resolve. The intrinsic-overlap names (`Array`, `String`, …)
/// are SKIPPED (see `intrinsicOverlapNames`); residue is EXPECTED and rides the
/// burndown diagnostics (this path never aborts — `mapGlobalSymbolResilient`).
let extractLibGlobals (dtsPaths: string list) (packageName: string) : Schema.PackageManifest =
    extractGlobalsCore true dtsPaths packageName

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
