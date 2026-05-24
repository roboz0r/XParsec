module XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

open System
open System.Reflection
open System.Runtime.Loader
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

/// Lex + parse a source string into Lexed + an ImplementationFile (script
/// fragments wrap as AnonymousModule). Raises on failure. Mirrors the
/// semantic-analysis test helper.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    // `Result.Ok`/`Result.Error` are qualified because `open ...SemanticAnalysis`
    // brings `Severity.Error` into scope, which would otherwise shadow them.
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

/// Analyse a source string to a `TastFile` with the mock provider.
let analyse (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

/// Compile a source string straight to a `ClrArtifact`.
let compileSource (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    let lexed, file = parseFile input
    let tast = Pipeline.analyse MockBuiltins.provider input lexed file

    let artifact =
        Codegen.compile MockBuiltins.provider (ProjectInfo.defaults assemblyName) tast

    tast, artifact

/// Compile a source string against a caller-supplied `ProjectInfo` (e.g. an
/// on-disk app build via `ProjectInfo.app`).
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    let lexed, file = parseFile input
    let tast = Pipeline.analyse MockBuiltins.provider input lexed file
    Codegen.compile MockBuiltins.provider project tast

/// `<repo-root>/tmp/<name>`, created. Walks up from the test binary to the
/// repo root (the directory holding `claude_tools.cmd`) so emitted artifacts
/// land somewhere stable and inspectable rather than the OS temp dir.
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

/// Run a materialised app via the `dotnet` host (`dotnet <dll>`), capturing
/// exit code + stdout. The out-of-process counterpart to `runEntryPoint`: it
/// proves the emitted bundle runs as a real `dotnet` app, not just an
/// in-process `Assembly.Load`. On a non-zero exit, stderr is appended so host
/// failures (missing runtimeconfig, unresolved reference) surface in the
/// assertion message.
let runOnDisk (dllPath: string) : int * string =
    let psi = Diagnostics.ProcessStartInfo "dotnet"
    psi.ArgumentList.Add dllPath
    psi.WorkingDirectory <- IO.Path.GetDirectoryName dllPath
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    use p = Diagnostics.Process.Start psi
    let out = p.StandardOutput.ReadToEnd()
    let err = p.StandardError.ReadToEnd()
    p.WaitForExit()
    (p.ExitCode, (if p.ExitCode = 0 then out else out + err))

/// Load emitted PE bytes into a *fresh* `AssemblyLoadContext`, returning the
/// loaded assembly. Each load gets its own context, so an emitted assembly's
/// type identities are isolated per test: loading the *same* bytes a second time
/// (e.g. into the default context) produces a *distinct* assembly, and
/// cross-`Invoke`ing a value built by one into a method reflected from the other
/// throws "Object of type X cannot be converted to type X". A reflection
/// round-trip must therefore reflect every member + construct every value
/// through the single `Assembly` this returns. Framework / already-loaded
/// dependencies (FSharp.Core, Vesper.Printf) resolve via the default context's
/// fallback, so a custom context still runs printf-bearing programs.
let loadAssembly (bytes: byte[]) : Assembly =
    let alc = AssemblyLoadContext("xparsec-codegen-test", isCollectible = true)
    use ms = new IO.MemoryStream(bytes)
    alc.LoadFromStream ms

/// Serialises the `Console.Out` capture below. Expecto runs tests in
/// parallel, but `Console.Out` is process-global — without this lock,
/// concurrent `runEntryPoint`s redirect each other's output (and can write to
/// an already-disposed `StringWriter`).
let private consoleLock = obj ()

/// Load emitted PE bytes into an isolated context, invoke the entry point with
/// empty args, and capture both the exit code and anything written to
/// `Console.Out`.
let runEntryPoint (bytes: byte[]) : int * string =
    let asm = loadAssembly bytes
    let entry = asm.EntryPoint

    if isNull entry then
        failwith "emitted assembly has no entry point"

    lock
        consoleLock
        (fun () ->
            let original = Console.Out
            use captured = new IO.StringWriter()
            Console.SetOut captured

            try
                let result = entry.Invoke(null, [| box (Array.empty<string>) |])
                Console.Out.Flush()
                (result :?> int), captured.ToString()
            finally
                Console.SetOut original
        )
