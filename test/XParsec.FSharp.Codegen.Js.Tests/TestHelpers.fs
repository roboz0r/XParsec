module XParsec.FSharp.Codegen.Js.Tests.TestHelpers

open System
open System.Diagnostics
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js

/// Lex + parse a source string; a script fragment wraps as `AnonymousModule`.
/// `Result.Ok`/`Result.Error` are qualified because `open …SemanticAnalysis` brings
/// `Severity.Error` into scope, which would otherwise shadow them.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parse reader with
        | Result.Error e -> failwithf "parse failed: %A" e
        | Result.Ok(FSharpAst.ImplementationFile f) -> lexed, f
        | Result.Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
            lexed, ImplementationFile.AnonymousModule elems
        | Result.Ok ast -> failwithf "unexpected AST: %A" ast

/// Front-end a program to a `Frozen.TastFile`, the JS backend's input. Resolves
/// through `MockBuiltins.provider` (printf is special-cased in the front end, so no
/// real `Vesper.Printf` contract is needed) and fails on any error diagnostic so a
/// degraded TAST never reaches the walker.
let frozenOf (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSem MockBuiltins.provider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile `input` to JS source text (in-memory).
let emit (input: string) : string =
    Codegen.compile (JsProjectInfo.defaults "Test") (frozenOf input)
    |> Codegen.toSource

/// `src/<pkg>/manifest.toml` — a layer-1 contract manifest.
let srcManifest (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

/// `src/Vesper.Core/manifest.toml` — the layer-1 contract manifest the F1 JS
/// inline-body tests resolve against (its `inline-bodies-js` key swaps in
/// `ops-platform.js.fs`).
let vesperCoreManifest: string = srcManifest "Vesper.Core"

/// A file beside a package's `manifest.toml` (e.g. `srcFile "Vesper.List" "list.js.fs"`).
let srcFile (pkg: string) (file: string) : string =
    IO.Path.Combine(IO.Path.GetDirectoryName(srcManifest pkg), file)

/// `src/Vesper.Printf/manifest.toml` — gives `printfn`/`eprintfn`/… a resolvable
/// symbol. The actual lowering to a `Format` node is front-end special-casing
/// (no inline body is emitted), so no `inline-bodies-js` is needed here.
let vesperPrintfManifest: string = srcManifest "Vesper.Printf"

/// `<repo-root>/tmp/<name>`, created. Walks up to the repo root (holding
/// `claude_tools.cmd`) so artifacts land somewhere stable and inspectable.
let tmpDir (name: string) : string =
    let rec up (dir: string) =
        if isNull dir then
            failwith "repo root not found (no claude_tools.cmd above the test binary)"
        elif IO.File.Exists(IO.Path.Combine(dir, "claude_tools.cmd")) then
            dir
        else
            up (IO.Path.GetDirectoryName dir)

    let d = IO.Path.Combine(up AppContext.BaseDirectory, "tmp", name)
    IO.Directory.CreateDirectory d |> ignore
    d

/// Run a `.mjs` file under Node, returning `Some(exitCode, output)`. Returns
/// `None` when `node` is absent (the exec test then skips rather than fails), so a
/// CI box without Node still passes the golden-text half of the suite.
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

// ---- Step 1: JS-target front end (operator templates spliced from F1) --------

/// The manifests a Step-1 program resolves against: `Vesper.Core` owns the
/// primitives (`int`/`float`/…) plus the arithmetic / equality operators whose
/// `inline-bodies-js` `.fs` bodies (`ops-platform.js.fs`, Step F1) carry the `$N`
/// JS templates; `Vesper.Printf` gives `printfn` a resolvable symbol;
/// `Vesper.Comparison` owns the ordering operators (`< > <= >=`), whose
/// `inline-bodies-js` `comparison.js.fs` + `runtime-js` `Vesper.Comparison.mjs`
/// landed in Step 6's compare half.
let jsManifests: string list =
    [
        vesperCoreManifest
        srcManifest "Vesper.Comparison"
        vesperPrintfManifest
        // Step 5: `Option` (`Some`/`None`) and `List` (`[]`/`::`) are external
        // union types the backend emits as honest nominal JS classes — their case
        // shapes are read off the provider. `Vesper.List`'s contract forward-refs
        // `int option`, so `Vesper.Option` precedes it.
        srcManifest "Vesper.Option"
        srcManifest "Vesper.List"
    ]

/// The JS-target provider: built with `Some Target.Js` so its inline-body channel
/// splices the JS operator templates at the consumer's use site (a ground
/// `2 + 2` freezes to `ILIntrinsic("($0 + $1) | 0", …)`). Lazily built once — the
/// `SymbolProviders` cache also memoises the contract per target.
let jsProvider: Lazy<IExternalSymbolProvider> =
    lazy SymbolProviders.buildContractFor (Some Target.Js) jsManifests

/// Front-end a program to a `Frozen.TastFile` through the **JS-target** contract
/// provider, so operator use sites carry the JS templates (vs `frozenOf`'s
/// `MockBuiltins`, which has no JS bodies). Fails on any error diagnostic.
let frozenOfJs (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    // Self-host list default: the JS target has no FSharp.Core, so an unpinned
    // `[]`/`::` resolves to the Vesper cons-list (`Vesper.Collections.List`1`),
    // emitted as honest nominal JS classes via the same provider path as `Option`.
    let tast = Pipeline.analyseSemForSelfHost jsProvider.Value input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile a Step-1 `input` (JS-target front end) to JS source text. The source
/// is supplied so `let`-bound names are the real source identifiers; the trailing
/// `//# sourceMappingURL` comment that supplying `Source` also emits is stripped
/// so golden assertions see just the code (map emission itself is covered by the
/// Step 0b / exec tests).
let emitJs (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some { Path = "test.fsx"; Content = input }
        }

    let src =
        Codegen.compileWith jsProvider.Value jsManifests project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Compile a Step-1 `input` in **library** mode (top-level `let`s become
/// `export const …`, Step 5b Phase 3) to JS source text. Same source-driven naming
/// + `sourceMappingURL` stripping as `emitJs`; only `Kind = Library` differs.
let emitJsLibrary (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some { Path = "test.fsx"; Content = input }
            Kind = Library
        }

    let src =
        Codegen.compileWith jsProvider.Value jsManifests project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

// ---- Step 5b Phase 3: compile a package *impl* (`list.js.fs`) to a runtime .mjs --

/// Deps-only JS provider for compiling a package impl whose only dependency is
/// `Vesper.Core` (both `Vesper.List` and `Vesper.Option`). The package's OWN contract
/// must be ABSENT — the impl declares its type in-file, so a provider that also
/// declared it would collide — mirroring CLR `buildPackage`'s "provider = `depends-on`
/// only". `Vesper.Core` carries the primitives + `Fun` + the `+` JS inline body; built
/// with `Some Target.Js` so use sites carry the JS templates.
let coreDepsJsProvider: Lazy<IExternalSymbolProvider> =
    lazy SymbolProviders.buildContractFor (Some Target.Js) [ vesperCoreManifest ]

/// Front-end + freeze a JS-target package *impl* through a deps-only `provider`.
/// Like `frozenOfJs` (self-host list default on, JS target) but the provider carries
/// only the package's dependencies, so the impl's own in-file type declarations are
/// the resolution authority (the `buildPackage` self-compile shape).
let frozenImplJs (provider: IExternalSymbolProvider) (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost provider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "impl analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile a package impl in **library** mode (top-level `let`s → `export const …`)
/// to its runtime-module source text. `Source` is supplied so the exported bindings
/// take their real source names (`length`, `map`, …); the trailing
/// `//# sourceMappingURL` comment that enables is stripped (a committed runtime asset
/// ships map-free, like the hand-authored predecessor). No runtime imports are
/// resolved (`manifestPaths = []`) — the List subset imports nothing.
let compileLibrary (provider: IExternalSymbolProvider) (moduleName: string) (input: string) : string =
    let project =
        { JsProjectInfo.defaults moduleName with
            Source =
                Some
                    {
                        Path = moduleName + ".fs"
                        Content = input
                    }
            Kind = Library
        }

    let src =
        Codegen.compileWith provider [] project (frozenImplJs provider input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Run a `.mjs` source string under Node by writing it (plus any sibling modules) to
/// a fresh tmp dir and executing `entry`. `files` is `(fileName, source)` pairs; the
/// first is the entry point. Returns `Some(exitCode, trimmed-output)` or `None` when
/// `node` is absent.
let runNodeFiles (name: string) (files: (string * string) list) : (int * string) option =
    let dir = tmpDir name

    for (fileName, source) in files do
        IO.File.WriteAllText(IO.Path.Combine(dir, fileName), source)

    match files with
    | (entry, _) :: _ ->
        runNode (IO.Path.Combine(dir, entry))
        |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())
    | [] -> failwith "runNodeFiles: no files"

/// Compile a Step-1 `input` and run it under Node, returning `Some(exitCode,
/// trimmed-stdout)` or `None` when `node` is absent (the test then skips).
let runJs (name: string) (input: string) : (int * string) option =
    let outDir = tmpDir name
    let jsPath = IO.Path.Combine(outDir, name + ".mjs")

    let project =
        { JsProjectInfo.defaults name with
            OutputPath = Some jsPath
            Source =
                Some
                    {
                        Path = name + ".fsx"
                        Content = input
                    }
        }

    Codegen.compileWith jsProvider.Value jsManifests project (frozenOfJs input)
    |> Codegen.materialise

    runNode jsPath
    |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())
