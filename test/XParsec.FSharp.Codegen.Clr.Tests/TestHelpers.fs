module XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

open System
open System.Reflection
open System.Runtime.Loader
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

/// Lex + parse a source string; script fragments wrap as `AnonymousModule`.
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

let analyse (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

/// `<repo-root>/tmp/<name>`, created. Walks up to the repo root (holding
/// `claude_tools.cmd`) so artifacts land somewhere stable and inspectable
/// rather than the OS temp dir.
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

/// `src/Vesper.Core/<fileName>`, relative to this test file.
let vesperCoreSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Core", fileName)

/// `src/Vesper.List/<fileName>`, relative to this test file.
let vesperListSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.List", fileName)

/// Compile `Vesper.Core.dll` from `prim-types-min.fs` (the `Vesper.Fun\`2`
/// interface + primitive intrinsics — R1), load it into the *Default*
/// `AssemblyLoadContext`, and return its path. An in-process user PE loaded into a
/// fresh context resolves `Fun` through that context's fallback to Default, exactly
/// how `Vesper.Printf` already resolves. Forced once; later compiles inject the
/// path so their function values reference this DLL. Compiled with **no** core
/// injected — `Vesper.Core` *defines* `Fun`. The cons-list is its own package now
/// (`vesperListDll` → `Vesper.List.dll`, package-split-plan PS2), not concatenated
/// here.
let vesperCoreDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-core"
         let corePath = IO.Path.Combine(outDir, "Vesper.Core.dll")

         let project =
             { ProjectInfo.library "Vesper.Core" with
                 OutputPath = Some corePath
             }

         let src = IO.File.ReadAllText(vesperCoreSource "prim-types-min.fs")
         let lexed, file = parseFile src
         let tast = Pipeline.analyse MockBuiltins.provider src lexed file
         let artifact = Codegen.compile MockBuiltins.provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath corePath |> ignore
         corePath)

/// Compile `Vesper.List.dll` from `src/Vesper.List/list-min.fs` — the
/// `Vesper.Collections.List\`1` cons-list (`Cons`/`Nil` + `IsEmpty`/`Head`/`Tail`)
/// as its own package (package-split-plan PS2) — load it into the *Default*
/// `AssemblyLoadContext`, and return its path. Compiled **standalone**: the
/// minimal list forms no function value and uses no `Fun`, so it is BCL-only and
/// needs neither an external core nor (being the list itself) an external list.
let vesperListDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-list"
         let listPath = IO.Path.Combine(outDir, "Vesper.List.dll")

         let project =
             { ProjectInfo.library "Vesper.List" with
                 OutputPath = Some listPath
             }

         let src = IO.File.ReadAllText(vesperListSource "list-min.fs")
         let lexed, file = parseFile src
         let tast = Pipeline.analyse MockBuiltins.provider src lexed file
         let artifact = Codegen.compile MockBuiltins.provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath listPath |> ignore
         listPath)

/// Point a project at the compiled `Vesper.Core.dll` (for `Vesper.Fun`, R1) and
/// `Vesper.List.dll` (for `Vesper.Collections.List`, package-split-plan PS2), so a
/// program's function values + list literals resolve. Each path is injected only
/// when absent, and never into the package that *defines* the type (a package must
/// not reference itself): `Vesper.Core` gets no core path, `Vesper.List` no list
/// path. Forcing each lazy loads the DLL into the Default ALC before any in-process
/// run.
let withCore (project: ProjectInfo) : ProjectInfo =
    let withCorePath p =
        if p.VesperCorePath.IsNone && p.AssemblyName <> "Vesper.Core" then
            { p with
                VesperCorePath = Some vesperCoreDll.Value
            }
        else
            p

    let withListPath p =
        if p.VesperListPath.IsNone && p.AssemblyName <> "Vesper.List" then
            { p with
                VesperListPath = Some vesperListDll.Value
            }
        else
            p

    project |> withCorePath |> withListPath

let compileSource (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    let lexed, file = parseFile input
    let tast = Pipeline.analyse MockBuiltins.provider input lexed file

    let artifact =
        Codegen.compile MockBuiltins.provider (withCore (ProjectInfo.defaults assemblyName)) tast

    tast, artifact

/// Like `compileSource` but against a caller-supplied `ProjectInfo` (e.g. an
/// on-disk app build via `ProjectInfo.app`). `withCore` injects the compiled
/// `Vesper.Core.dll` unless the project is `Vesper.Core` itself.
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    let lexed, file = parseFile input
    let tast = Pipeline.analyse MockBuiltins.provider input lexed file
    Codegen.compile MockBuiltins.provider (withCore project) tast

/// Run a materialised app out-of-process via the `dotnet` host, the counterpart
/// to the in-process `runEntryPoint`. On a non-zero exit, stderr is appended so
/// host failures (missing runtimeconfig, unresolved reference) surface in the
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
