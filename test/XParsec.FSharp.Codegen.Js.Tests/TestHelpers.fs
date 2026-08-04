module XParsec.FSharp.Codegen.Js.Tests.TestHelpers

open System
open System.Diagnostics
open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js

/// Lex + parse a source string; a script fragment wraps as `AnonymousModule`. Through
/// `Pipeline.parseUnrecovered`, the same gate the driver compiles behind, so a source that
/// parses only because RECOVERY patched it raises here rather than being analysed as though
/// it had been written that way.
/// `Result.Ok`/`Result.Error` are qualified because `open …SemanticAnalysis` brings
/// `Severity.Error` into scope, which would otherwise shadow them.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match Pipeline.parseUnrecovered input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed -> parsed.Lexed, parsed.File

/// `src/<pkg>/manifest.toml`.
let srcManifest (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

/// `src/Vesper.Core/manifest.toml`.
let vesperCoreManifest: string = srcManifest "Vesper.Core"

/// A file beside a package's `manifest.toml` (e.g. `srcFile "Vesper.List" "list.fs"`).
let srcFile (pkg: string) (file: string) : string =
    IO.Path.Combine(IO.Path.GetDirectoryName(srcManifest pkg), file)

/// `src/Vesper.Printf/manifest.toml`.
let vesperPrintfManifest: string = srcManifest "Vesper.Printf"

/// `<repo-root>/tmp/<name>`, created on demand.
let tmpDir (name: string) : string =
    let rec up (dir: string) =
        if isNull dir then
            failwith "repo root not found"
        elif IO.File.Exists(IO.Path.Combine(dir, "claude_tools.cmd")) then
            dir
        else
            up (IO.Path.GetDirectoryName dir)

    let d = IO.Path.Combine(up AppContext.BaseDirectory, "tmp", name)
    IO.Directory.CreateDirectory d |> ignore
    d

/// Run a `.mjs` file under Node. Returns `None` when `node` is absent (exec tests skip).
let runNode (jsPath: string) : (int * string) option =
    let psi = ProcessStartInfo "node"
    psi.ArgumentList.Add jsPath
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    try
        use p = Process.Start psi
        let out = p.StandardOutput.ReadToEnd()
        let err = p.StandardError.ReadToEnd()
        p.WaitForExit()
        Some(p.ExitCode, (if p.ExitCode = 0 then out else out + err))
    with :? System.ComponentModel.Win32Exception ->
        None

/// The manifests the JS-target program resolves against.
let jsManifests: string list =
    [
        vesperCoreManifest
        srcManifest "Vesper.Comparison"
        vesperPrintfManifest
        // BCL exceptions as Vesper contracts (inherit exn → Error); must precede Vesper.Option.
        srcManifest "Vesper.Exceptions"
        srcManifest "Vesper.Option"
        srcManifest "Vesper.List"
        srcManifest "Vesper.Array"
    ]

/// The JS-target contract for `jsManifests` (BCL-free; resolves exceptions through
/// Vesper.Exceptions) — the provider a program is analysed against, the producer files its
/// served inline bodies are anchored in, and the manifest set backing its runtime imports.
///
/// ONE value, and every helper below draws from it rather than composing its own: a compile
/// handed a provider from one manifest set and an anchor domain from another emits a source map
/// that attributes producer code to a consuming line, in range and wrong.
let jsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContractFor Target.Js jsManifests

/// `jsContract`'s provider, for the front-end helpers — analysis resolves symbols and reads no
/// position. A PROJECTION of the contract, never a second build.
let jsProvider: Lazy<IExternalSymbolProvider> = lazy jsContract.Value.Provider

/// Front-end a program to its frozen `FrozenPools`. Fails on any error diagnostic.
/// Resolves through the real JS-native contract stack (`jsProvider`) — the
/// superset that replaced the value-only `MockBuiltins` fixture.
let frozenOf (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContext jsProvider.Value (Hashing.originSourceOfText input lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// Compile `input` to JS source text (in-memory).
let emit (input: string) : string =
    Codegen.compile (JsProjectInfo.defaults "Test") (frozenOf input)
    |> Codegen.toSource

/// Front-end a program through the JS-target provider. Fails on any error diagnostic.
let frozenOfJs (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemForSelfHostWithContext jsProvider.Value (Hashing.originSourceOfText input lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// The map's view of a source: its text plus the tokens a frozen node's anchor indexes into.
/// A production driver hands the backend the `Lexed` the front end already built; a test
/// that only has the text lexes here, which is the same table by construction.
let jsSource (path: string) (input: string) : JsSource =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        {
            Path = path
            Content = input
            Lexed = lexed
        }

/// Emit a named program's JS from an ALREADY-frozen tree, through the exact project shape the
/// byte-identity gates pin (assembly `name`, `name + ".fsx"` source), stripping the trailing
/// `//# sourceMappingURL` line. Single-sourced so the direct, round-trip, and cache-parity gates
/// judge the round-tripped/cached tree against the identical emit path — they cannot drift apart.
let emitFrozenJs (name: string) (src: string) (frozen: FrozenPools) : string =
    let project =
        { JsProjectInfo.defaults name with
            Source = Some(jsSource (name + ".fsx") src)
        }

    let source = Codegen.compileWith jsContract.Value project frozen |> Codegen.toSource

    let idx = source.IndexOf "//# sourceMappingURL"
    if idx >= 0 then source.Substring(0, idx) else source

/// Compile a JS-target `input` to JS source text (strips `//# sourceMappingURL`).
let emitJs (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some(jsSource "test.fsx" input)
        }

    let src =
        Codegen.compileWith jsContract.Value project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Like `emitJs` but in library mode (top-level `let` → `export const`).
let emitJsLibrary (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some(jsSource "test.fsx" input)
            Kind = Library
        }

    let src =
        Codegen.compileWith jsContract.Value project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Deps-only JS contract for compiling a package impl (the package's own contract is absent, to
/// avoid colliding with the in-file types the impl declares).
let coreDepsJsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContractFor Target.Js [ vesperCoreManifest; srcManifest "Vesper.Exceptions" ]

/// As `coreDepsJsContract`, plus `Vesper.Array`'s OWN manifest — `array.fs` splices
/// `NewArray` out of the per-target `array-prelude.js.fs`, so the package's inline bodies
/// have to be in the contract that compiles it. Safe here for the reason the exclusion
/// exists: the collision it guards against is over in-file TYPES, and `Vesper.Array`
/// declares none. A package that declares types (`Vesper.List`) still takes the deps-only
/// contract above.
let arrayDepsJsContract: Lazy<SymbolProviders.Contract> =
    lazy
        JsNativeSymbols.jsNativeContractFor
            Target.Js
            [
                vesperCoreManifest
                srcManifest "Vesper.Exceptions"
                srcManifest "Vesper.Array"
            ]

/// Front-end + freeze a JS-target package impl. The provider carries only the package's
/// dependencies — the impl's own in-file types are the resolution authority.
let frozenImplJs (provider: IExternalSymbolProvider) (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemForSelfHostWithContext provider (Hashing.originSourceOfText input lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "impl analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// Compile a package impl in library mode to runtime-module source text (strips
/// sourceMappingURL). `sourceFile` is the Vesper source basename (`list.fs`),
/// recorded both in the source map and in the emitted `// Generated from …` header.
/// The impl is analysed against the same contract it is emitted through, so a body spliced
/// out of a dependency resolves against the file it was written in.
let compileLibrary
    (contract: SymbolProviders.Contract)
    (moduleName: string)
    (sourceFile: string)
    (input: string)
    : string =
    let project =
        { JsProjectInfo.defaults moduleName with
            Source = Some(jsSource sourceFile input)
            Kind = Library
            GeneratedFrom = Some sourceFile
        }

    let src =
        Codegen.compileWith contract project (frozenImplJs contract.Provider input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Write `files` to a tmp dir and run the first as entry point under Node.
let runNodeFiles (name: string) (files: (string * string) list) : (int * string) option =
    let dir = tmpDir name

    for (fileName, source) in files do
        IO.File.WriteAllText(IO.Path.Combine(dir, fileName), source)

    match files with
    | (entry, _) :: _ ->
        runNode (IO.Path.Combine(dir, entry))
        |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())
    | [] -> failwith "runNodeFiles: no files"

/// Compile `input` and run under Node. Returns `None` when `node` is absent (test skips).
let runJs (name: string) (input: string) : (int * string) option =
    let outDir = tmpDir name
    let jsPath = IO.Path.Combine(outDir, name + ".mjs")

    let project =
        { JsProjectInfo.defaults name with
            OutputPath = Some jsPath
            Source = Some(jsSource (name + ".fsx") input)
        }

    Codegen.compileWith jsContract.Value project (frozenOfJs input)
    |> Codegen.materialise

    runNode jsPath
    |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())

// ─── TS-provider test scaffolding (shared by the provider tests) ─────────

/// Aggregate the sources' ambient `open` prefixes, exactly as the production composite
/// (`ExternalSymbolProviders.composite` via `collectAmbient`) does. `stackTs`/`stackTsMany` must
/// surface this (not `[]`): the JS-native provider carries `Vesper` ambient, and
/// `canonName`'s forward intrinsic resolution reaches `Vesper.undefined` (a JS-only
/// intrinsic registered under its qualified name) only through it — dropping ambient
/// would let the reverse-canon map collapse `undefined` onto `unit`.
/// The ONE builder for a JS-target test provider stack: composes `sources` (no home
/// assembly) then applies the covariant `number → float` resolution and the per-lookup
/// cache in the SAME order as production (`TsManifestProvider.buildContractFor`). EVERY
/// hand-built front-end test stack MUST go through here so the `NumberCovariance.wrap` /
/// `ExternalSymbolProviders.memoize` steps can never be silently dropped at one site and quietly
/// diverge from production behaviour.
let stackJs (ambient: string list) (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
    ExternalSymbolProviders.stack ValueNone ambient sources
    |> NumberCovariance.wrap
    |> ExternalSymbolProviders.memoize

/// `stackJs` with the ambient prefix set AGGREGATED from the sources (mirroring
/// production `TsManifestProvider.buildContractFor`). A hand-built stack MUST use
/// this rather than `stackJs []` whenever it layers over a Vesper contract source:
/// dropping ambient hides the `Vesper` open-prefix the intrinsic resolver needs to
/// find `Vesper.unit`/`Vesper.int` (`ctx.Intrinsics`) beneath the TS manifest.
let stackWithAmbient (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
    let ambient = sources |> List.collect (fun s -> s.AmbientOpenPrefixes)
    stackJs ambient sources

/// The provider-stack one-liner: a TS-manifest provider layered over the standard
/// JS-native provider (so the manifest's primitive/`int`/`string` argument types still
/// resolve). `ValueNone` = no home-assembly identity; ambient is aggregated from the
/// sources (mirroring production) so JS-only intrinsics resolve by bare name.
let stackTs (manifest: Schema.PackageManifest) : IExternalSymbolProvider =
    stackWithAmbient [ TsManifestProvider.providerOfManifest manifest; jsProvider.Value ]

/// Like `stackTs` but layers SEVERAL TS-manifest providers (order preserved) over the
/// JS-native provider — for a program driving more than one external package.
let stackTsMany (manifests: Schema.PackageManifest list) : IExternalSymbolProvider =
    stackWithAmbient
        [
            yield! manifests |> List.map TsManifestProvider.providerOfManifest
            jsProvider.Value
        ]

/// The EMIT contract of a `stackTsMany` stack: that stack as the provider, re-seated in
/// `jsContract` — the contract of the JS-native leaf it layers over — so it carries that leaf's
/// retention as its anchor domain. A TS manifest is declaration data: it carries no F# source and
/// so serves no inline body, which makes the leaf the only layer a served body can come from and
/// its retained producer files the only domain such a body's anchors index.
///
/// Layering and re-seating are ONE step so a caller cannot take the stack and leave the domain
/// behind: a provider from one manifest set beside an anchor domain from another resolves a
/// served body's position against a file that was never retained (`SymbolProviders.Contract`).
let contractTsMany (manifests: Schema.PackageManifest list) : SymbolProviders.Contract =
    { jsContract.Value with
        Provider = stackTsMany manifests
    }

/// `contractTsMany` for one manifest — the emit contract behind `stackTs`.
let contractTs (manifest: Schema.PackageManifest) : SymbolProviders.Contract = contractTsMany [ manifest ]

/// Analyse `input` through `provider` (the self-host front end) and return the ERROR
/// diagnostics — the shared body of the per-package `analyse`/`analyseErrors` wrappers.
let analyseWith (provider: IExternalSymbolProvider) (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast =
        Pipeline.analyseSemForSelfHost provider (Hashing.originSourceOfText input lexed) file

    tast.Diagnostics |> Diagnostic.errors

/// The newline-joined messages of `ds` (for `stringContains` assertions on the set of
/// allowed values a directional-admission error names).
let errorText (ds: Diagnostic list) : string =
    ds |> List.map (fun d -> d.Message) |> String.concat "\n"

/// The ONE shared `EmitJsContext.WalkCtx` builder, matching production wiring
/// (`Codegen.compileWith`): a real resolver over the source — its token table AND its line
/// starts, since an anchor is an index into the former (the hand-built test copies wrongly
/// left `Resolver = ValueNone`) — and all lowering tables empty for `buildProgram` to fill.
/// `runtime` is the injected package → `.mjs` map; `exportTopLevel` selects script
/// (`false`) vs library (`true`).
///
/// Takes the whole `contract`, exactly as `Codegen.compileWith` does, so the provider that
/// serves an inline body and the retention its producer positions index are one value here too.
let private jsWalkCtx
    (contract: SymbolProviders.Contract)
    (runtime: Map<string, JsRuntimeModule>)
    (exportTopLevel: bool)
    (input: string)
    (frozen: FrozenPools)
    : EmitJsContext.WalkCtx =
    let resolver: EmitJsContext.Resolver =
        match Lexing.lexString input with
        | Result.Ok lexed ->
            ValueSome
                {
                    Lexed = lexed
                    Lines = JsMapSources.LineIndex.build input
                    Origins = contract.Origins
                }
        | Result.Error _ -> ValueNone

    EmitJsContext.WalkCtx.create
        resolver
        (TastPoolBuilder.openOver frozen)
        contract.Provider
        (JsImports.create runtime)
        exportTopLevel

/// Front-end + freeze `input` through `contract`, then emit JS with the injected
/// `runtime` modules — the shared body of the per-package `emitWithX` helpers. Routes
/// through `frozenImplJs` (analyse-for-self-host + freeze) and the one `jsWalkCtx`
/// builder, so every provider test emits through identical, production-matched wiring.
///
/// The program is analysed and emitted through the SAME contract, so a body spliced out of a
/// dependency resolves its position against the file it was written in (`contractTs` /
/// `JsNativeSymbols.jsNativeContractFor` are how one is built).
let emitWith
    (contract: SymbolProviders.Contract)
    (runtime: Map<string, JsRuntimeModule>)
    (exportTopLevel: bool)
    (input: string)
    : string =
    let frozen = frozenImplJs contract.Provider input
    let ctx = jsWalkCtx contract runtime exportTopLevel input frozen
    (JsPrint.print (EmitJs.buildProgram ctx)).Source

/// The Node round-trip assertion, documented ONCE: write `files` to a tmp dir, run the
/// first under Node, and require the trimmed stdout to EXACTLY equal `expected`. When
/// `node` is absent `runNodeFiles` yields `None` and the exec check is SKIPPED — the
/// caller's emit-time assertions already ran, so the test still exercises codegen.
let expectNodeOutput (name: string) (files: (string * string) list) (expected: string) : unit =
    match runNodeFiles name files with
    | None -> () // node absent — exec check skips; the caller's emit-time asserts still ran
    | Some(code, out) ->
        Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
        Expect.equal out expected (sprintf "round-trip output, got:\n%s" out)

/// The vendored es2015 ref pack (`../ts-fixtures/es2015/es2015.manifest.json`) — the
/// GLOBAL lib manifest mounted under `Js` (its `Package = "es2015"` is a
/// `TsGlobalHomes.globalLibHomes` entry). Shared by the `Js.Map` gate and by
/// `MittFixture` (mitt's `all: Map<…>` is a homed ref into es2015). Committed golden;
/// the Node extractor is never run.
let es2015Manifest: Schema.PackageManifest =
    let path =
        IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures", "es2015", "es2015.manifest.json")

    match Codec.deserialize (IO.File.ReadAllText path) with
    | Error e -> failwithf "es2015 manifest does not parse: %s" e
    | Ok man -> man

/// The mitt TS fixture (golden manifest + vendored runtime under `../ts-fixtures/mitt`),
/// shared by `MittE2ETests` and `UnannotatedMittTests`. Reads ONLY committed files; the
/// Node extractor is never run.
module MittFixture =

    let dir = IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures", "mitt")

    /// The golden `mitt.manifest.json`, deserialised.
    let manifest: Schema.PackageManifest =
        match Codec.deserialize (IO.File.ReadAllText(IO.Path.Combine(dir, "mitt.manifest.json"))) with
        | Error e -> failwithf "mitt manifest does not parse: %s" e
        | Ok man -> man

    /// The vendored `dist/mitt.mjs` runtime source.
    let runtimeSource: string =
        IO.File.ReadAllText(IO.Path.Combine(dir, "dist", "mitt.mjs"))

    /// The base provider — mitt STACKED OVER es2015 (so mitt's `all: Map<…>` homed ref
    /// resolves as a real `Js.Map` and its members can be called) over the JS-native
    /// provider. es2015 mounts under `Js` and emits no import (global pack).
    let provider: IExternalSymbolProvider = stackTsMany [ manifest; es2015Manifest ]
