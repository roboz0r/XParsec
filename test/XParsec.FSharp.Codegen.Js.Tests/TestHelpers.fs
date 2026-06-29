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

/// `src/<pkg>/manifest.toml`.
let srcManifest (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

/// `src/Vesper.Core/manifest.toml`.
let vesperCoreManifest: string = srcManifest "Vesper.Core"

/// A file beside a package's `manifest.toml` (e.g. `srcFile "Vesper.List" "list.js.fs"`).
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
    ]

/// The JS-target provider (BCL-free; resolves exceptions through Vesper.Exceptions).
let jsProvider: Lazy<IExternalSymbolProvider> =
    lazy JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) jsManifests

/// Front-end a program to a `Frozen.TastFile`. Fails on any error diagnostic.
/// Resolves through the real JS-native contract stack (`jsProvider`) — the
/// superset that replaced the value-only `MockBuiltins` fixture.
let frozenOf (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSem jsProvider.Value input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile `input` to JS source text (in-memory).
let emit (input: string) : string =
    Codegen.compile (JsProjectInfo.defaults "Test") (frozenOf input)
    |> Codegen.toSource

/// Front-end a program through the JS-target provider. Fails on any error diagnostic.
let frozenOfJs (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost jsProvider.Value input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile a JS-target `input` to JS source text (strips `//# sourceMappingURL`).
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

/// Like `emitJs` but in library mode (top-level `let` → `export const`).
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

/// Deps-only JS provider for compiling a package impl (own contract absent to avoid collision).
let coreDepsJsProvider: Lazy<IExternalSymbolProvider> =
    lazy
        JsNativeSymbols.buildJsNativeContractFor
            (Some Target.Js)
            [ vesperCoreManifest; srcManifest "Vesper.Exceptions" ]

/// Front-end + freeze a JS-target package impl. The provider carries only the package's
/// dependencies — the impl's own in-file types are the resolution authority.
let frozenImplJs (provider: IExternalSymbolProvider) (input: string) : Frozen.TastFile =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost provider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "impl analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run tast

/// Compile a package impl in library mode to runtime-module source text (strips
/// sourceMappingURL). `sourceFile` is the Vesper source basename (`list.js.fs`),
/// recorded both in the source map and in the emitted `// Generated from …` header.
let compileLibrary
    (provider: IExternalSymbolProvider)
    (moduleName: string)
    (sourceFile: string)
    (input: string)
    : string =
    let project =
        { JsProjectInfo.defaults moduleName with
            Source = Some { Path = sourceFile; Content = input }
            Kind = Library
            GeneratedFrom = Some sourceFile
        }

    let src =
        Codegen.compileWith provider [] project (frozenImplJs provider input)
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
