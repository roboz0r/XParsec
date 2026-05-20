module XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

open System
open System.Reflection
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

/// Serialises the `Console.Out` capture below. Expecto runs tests in
/// parallel, but `Console.Out` is process-global — without this lock,
/// concurrent `runEntryPoint`s redirect each other's output (and can write to
/// an already-disposed `StringWriter`).
let private consoleLock = obj ()

/// Load emitted PE bytes in-process, invoke the entry point with empty args,
/// and capture both the exit code and anything written to `Console.Out`.
let runEntryPoint (bytes: byte[]) : int * string =
    let asm = Assembly.Load bytes
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
