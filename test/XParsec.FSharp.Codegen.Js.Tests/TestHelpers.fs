module XParsec.FSharp.Codegen.Js.Tests.TestHelpers

open System
open System.Diagnostics
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
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

/// `src/Vesper.Core/manifest.toml` — the layer-1 contract manifest the F1 JS
/// inline-body tests resolve against (its `inline-bodies-js` key swaps in
/// `ops-platform.js.fs`).
let vesperCoreManifest: string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Core", "manifest.toml")

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
