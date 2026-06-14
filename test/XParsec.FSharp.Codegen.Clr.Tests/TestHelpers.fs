module XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

open System
open System.Reflection
open System.Runtime.Loader
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common

// The nominal `SemType` cases carry a
// `SymbolKey`; these shadow the constructors + expose name-projecting active
// patterns so codegen tests keep constructing / matching them by string name.
// See the SemanticAnalysis.Tests `TestHelpers` twin for the rationale.
let TyUnion (name: string, args: EqArray<SemType>) =
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

let TyRecord (name: string, args: EqArray<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

let TyClass (name: string, args: EqArray<SemType>) =
    SemType.TyClass(SymbolKeyOps.qualifiedTypeKey name args.Length, args)

let private nominalDisplayName (k: SymbolKey) : string = SymbolKeyOps.qualifiedName k

let (|TyUnion|_|) (t: SemType) =
    match t with
    | SemType.TyUnion(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

let (|TyRecord|_|) (t: SemType) =
    match t with
    | SemType.TyRecord(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

let (|TyClass|_|) (t: SemType) =
    match t with
    | SemType.TyClass(k, args) -> Some(nominalDisplayName k, args)
    | _ -> None

/// Project an `EqArray<'T>` as a plain `'T list` inside a pattern match — lets
/// tests written against the pre-EqArray TAST keep their list-literal arms
/// (`| [ TDecl.Let _ ] -> …`, `| [ x; y ] -> …`) verbatim across the flip
/// (docs/tast-eqarray-list.md Stage 2).
let inline (|EqList|) (xs: EqArray<'T>) : 'T list = EqArray.toList xs

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
    Pipeline.analyseSem MockBuiltins.provider input lexed file

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

/// `src/Vesper.Core/manifest.toml` — the Vesper.Core layer-1 referenced-project
/// manifest. Declared up here (above the
/// `vesperListDll` fixture, which references it) rather than in the
/// downstream manifest block.
let vesperCoreManifest: string = vesperCoreSource "manifest.toml"

/// Compile `Vesper.Core.dll` from `prim-types-min.fs` + `core-types.fs` (the
/// `Vesper.Fun\`2` interface, the primitive intrinsics, and the `Vesper.Ref\`1`
/// captured-mutable cell), load it into the *Default* `AssemblyLoadContext`, and
/// return its path. An in-process user PE loaded into a fresh context resolves
/// `Fun` / `Ref` through that context's fallback to Default, exactly how
/// `Vesper.Printf` already resolves. Forced once; later compiles inject the path
/// so their function values + promoted-mutable cells reference this DLL.
/// Compiled with **no** core injected — `Vesper.Core` *defines* `Fun` and
/// `Ref`. The cons-list is its own package now (`vesperListDll` →
/// `Vesper.List.dll`, package-split-plan PS2), not concatenated here.
let vesperCoreDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-core"
         let corePath = IO.Path.Combine(outDir, "Vesper.Core.dll")

         let project =
             { ProjectInfo.library "Vesper.Core" with
                 OutputPath = Some corePath
             }

         // Both files declare `namespace Vesper` and contribute disjoint types
         // (Fun + intrinsics in prim-types-min.fs, Ref in core-types.fs). Joined
         // with two blank lines so the second `namespace Vesper` starts a fresh
         // top-level block.
         let src =
             [
                 IO.File.ReadAllText(vesperCoreSource "prim-types-min.fs")
                 IO.File.ReadAllText(vesperCoreSource "core-types.fs")
                 // The `%A` structural-format interfaces (`IFormatSink` /
                 // `IStructuralFormattable`, P3 step 3.2) — Core-owned so a
                 // synthesised record/DU `Format` implements a Core type.
                 IO.File.ReadAllText(vesperCoreSource "structural-format.fs")
             ]
             |> String.concat "\n\n"

         let lexed, file = parseFile src

         let tast =
             Pipeline.analyseFor project.AssemblyName MockBuiltins.provider src lexed file

         let artifact = Codegen.compile MockBuiltins.provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath corePath |> ignore
         corePath)

/// Compile `Vesper.List.dll` from `src/Vesper.List/list.fs` — the
/// `Vesper.Collections.List\`1` cons-list (`Cons`/`Empty` + `IsEmpty`/`Head`/`Tail`)
/// **and** the `Vesper.Collections.ListModule::fold` static method (R3 deferred:
/// `fold` is compiled into the DLL now) — as its own package (package-split-plan
/// PS2), load it into the *Default* `AssemblyLoadContext`, and return its path.
/// Compiled with the core injected: `fold`'s folder parameter is a `Vesper.Fun`,
/// so the DLL now carries a `Vesper.Core` `AssemblyRef` (it was BCL-only while only
/// the list type shipped). It needs no external *list* (it defines the list
/// itself), so only `Vesper.Core` is in `References` — not via `withCore` (defined
/// below), just the core path directly, since `vesperCoreDll` is forced here too.
let vesperListDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-list"
         let listPath = IO.Path.Combine(outDir, "Vesper.List.dll")

         let project =
             { ProjectInfo.library "Vesper.List" with
                 OutputPath = Some listPath
                 References = [ vesperCoreDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperListSource "list.fs")
         // Vesper.List's compiled impl is `list.fs` (post-cutover): the verbatim
         // `[]`/`::` cons-list. A `[1; 2; 3]` consumer literal binds to it by arity
         // (nullary terminator + binary cons), not by case name, so the driver
         // stack is unaffected by the `Nil`/`Cons` → `Empty`/`Cons` rename. It uses
         // `failwith` (a real inline operator in `Vesper.Core/ops-platform.fs`, not
         // a name-suffix probe), so the build must run through the Vesper.Core
         // contract — `MockBuiltins` alone leaves the call head un-inlined.
         // Self-manifest (`Vesper.List`'s own) is excluded; the package is
         // *defining* its types here.
         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath listPath |> ignore
         listPath)

/// Add the compiled `Vesper.Core.dll` (for `Vesper.Fun`, R1) and `Vesper.List.dll`
/// (for `Vesper.Collections.List`, package-split-plan PS2) to a project's
/// `References`, so a program's function values + list literals resolve. Each path
/// is added only when absent, and never into the package that *defines* the type (a
/// package must not reference itself): `Vesper.Core` gets no core ref, `Vesper.List`
/// no list ref. Forcing each lazy loads the DLL into the Default ALC before any
/// in-process run.
let withCore (project: ProjectInfo) : ProjectInfo =
    let ensure (asmName: string) (dll: Lazy<string>) (refs: string list) =
        if
            project.AssemblyName = asmName
            || refs |> List.exists (fun p -> IO.Path.GetFileNameWithoutExtension p = asmName)
        then
            refs
        else
            refs @ [ dll.Value ]

    { project with
        References =
            project.References
            |> ensure "Vesper.Core" vesperCoreDll
            |> ensure "Vesper.List" vesperListDll
    }

/// The other contract packages that round out the default resolution stack.
let vesperListManifest: string = vesperListSource "manifest.toml"

let private srcManifest (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

let vesperComparisonManifest: string = srcManifest "Vesper.Comparison"

/// `src/Vesper.Printf/manifest.toml` — the printf family (`printf`/`printfn`/
/// `sprintf`) as its own `[<AutoOpen>] module Printf` contract, so a `printfn`
/// call resolves from source rather than the `MockBuiltins` `printfOps` crutch.
let vesperPrintfManifest: string = srcManifest "Vesper.Printf"

/// The default contract stack the demoted compile path resolves through
/// `MockBuiltins` stays the lowest-priority backstop inside `SymbolProviders.build`
/// for anything the contract does not yet own (operators still *emit* via
/// `Emit.BuiltinOps` regardless — emission is resolution-source-agnostic).
///
/// Vesper.Core (primitives + arithmetic/equality operators + `hash` + `failwith`),
/// Vesper.List (`List.fold` over the cons-list), Vesper.Comparison (the ordering
/// operators) and Vesper.Printf (the printf family) make up the default stack.
let defaultManifests: string list =
    [
        vesperCoreManifest
        vesperListManifest
        vesperComparisonManifest
        vesperPrintfManifest
    ]

/// The load context the package-build harness (`buildPackage`) loads its own DLLs
/// into. Its `Load` override resolves sibling `Vesper.*` packages it has built from
/// an internal registry, so a package loaded here binds against *this harness's*
/// copy of its dependencies — not whatever the Default context holds. (The
/// `vesperCoreDll`/`vesperListDll` lazies load a *different* `Vesper.Core` into the
/// Default context; resolving the harness's `Vesper.List` against that one would
/// trip the same-name / distinct-identity trap.) Everything else — FSharp.Core,
/// `Vesper.Printf`, the BCL — returns `null` to fall through to Default.
type private PackageLoadContext() =
    inherit AssemblyLoadContext("xparsec-package-build", isCollectible = false)

    let built =
        Collections.Concurrent.ConcurrentDictionary<string, Assembly>(StringComparer.Ordinal)

    member _.Register(name: string, asm: Assembly) = built.[name] <- asm

    override _.Load(name: AssemblyName) : Assembly =
        match built.TryGetValue name.Name with
        | true, asm -> asm
        | _ -> null

let private packageAlc = PackageLoadContext()

let private packageBuildCache =
    Collections.Concurrent.ConcurrentDictionary<string, Lazy<Assembly * ClrArtifact>>(StringComparer.Ordinal)

/// Pre-1: compile `src/<package>/`'s `impl` `.fs` files (in manifest order) to a
/// DLL through our own backend, resolving `depends-on`
/// recursively — each dependency is built + loaded first, its DLL added to
/// `References` and its `manifest.toml` to the contract stack. Caches per package
/// (`Lazy`), generalizing the hand-written `vesperCoreDll`/`vesperListDll` fixtures
/// into one manifest-driven function. Returns the loaded `Assembly` (in the shared
/// `packageAlc`) and the `ClrArtifact` (so a caller can assert
/// `FSharpCoreDependencies` is empty — the BCL-only bar).
let rec buildPackage (package: string) : Lazy<Assembly * ClrArtifact> =
    packageBuildCache.GetOrAdd(
        package,
        fun pkg ->
            lazy
                (let manifestPath = srcManifest pkg

                 let manifest =
                     match ReferencedProject.loadManifest manifestPath with
                     | Result.Ok m -> m
                     | Result.Error e -> failwithf "buildPackage %s: %s" pkg e

                 // Force each dependency's build first (recursively, shared cache):
                 // this loads + registers it in `packageAlc`, so the current package
                 // resolves against it at load time. Collect each dep's on-disk DLL
                 // for `References` (the emit-time AssemblyRef) and its manifest for
                 // the contract provider / inline bodies.
                 let depArtifacts =
                     manifest.DependsOn |> List.map (fun d -> (buildPackage d).Value |> snd)

                 let depDlls = depArtifacts |> List.choose (fun art -> art.OutputPath)
                 let depManifests = manifest.DependsOn |> List.map srcManifest

                 let provider = SymbolProviders.buildContract depManifests

                 let dir = IO.Path.GetDirectoryName manifestPath

                 let src =
                     manifest.Impl
                     |> List.map (fun rel -> IO.File.ReadAllText(IO.Path.Combine(dir, rel)))
                     |> String.concat "\n\n"

                 let outDir = tmpDir (sprintf "pkg-%s" pkg)
                 let outPath = IO.Path.Combine(outDir, manifest.Name + ".dll")

                 let project =
                     { ProjectInfo.library manifest.Name with
                         OutputPath = Some outPath
                         References = depDlls
                     }

                 let lexed, file = parseFile src
                 // Self-host: a BCL-only package has no FSharp.Core, so a bare
                 // `[]`/`::` defaults to the Vesper cons-list, not FSharp.Core's.
                 let tast = Pipeline.analyseForSelfHost project.AssemblyName provider src lexed file

                 // A package that doesn't type-check hasn't built: `Pipeline.analyse`
                 // collects diagnostics rather than throwing, so surface any
                 // error-severity ones here instead of emitting a degraded DLL.
                 let analysisErrors =
                     tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

                 if not (List.isEmpty analysisErrors) then
                     failwithf
                         "buildPackage %s: %d analysis error(s):\n%s"
                         pkg
                         (List.length analysisErrors)
                         (analysisErrors |> List.map (fun d -> d.Message) |> String.concat "\n")

                 let artifact = Codegen.compile provider project tast
                 Codegen.materialise artifact

                 use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)

                 // A contract-only package (`impl = []`: Vesper.Printf, whose runtime
                 // `Vesper.Formatter` is the C#-built `Vesper.Printf.dll`; Vesper.Comparison,
                 // whose operators are inlined) compiles to an *empty* DLL here — it
                 // carries no runtime types. Registering it in `packageAlc` would
                 // shadow the real, host-loaded assembly: a driver `printfn` would bind
                 // `Vesper.Printf` to this empty stub and fail to load `Vesper.Formatter`.
                 // So load such a package into a throwaway context and leave `packageAlc`
                 // without it — the driver's `Vesper.Printf` reference then falls through
                 // to the Default ALC (where the test project's C# `Vesper.Printf.dll` is
                 // loaded). The on-disk path stays in `References` for emit-time identity.
                 if List.isEmpty manifest.Impl then
                     let throwaway = AssemblyLoadContext("xparsec-contract-only", isCollectible = true)

                     throwaway.LoadFromStream ms, artifact
                 else
                     let asm = packageAlc.LoadFromStream ms
                     packageAlc.Register(manifest.Name, asm)
                     asm, artifact)
    )

/// Build the symbol-resolution stack + its cross-package inline bodies once
/// (cached per manifest set by `SymbolProviders.buildContract`) and run *both*
/// phases against it: a use-site
/// `External(name)` whose body lives in a referenced `.fs` (today: `hash` from
/// `ops-platform.fs`) is spliced in pre-freeze by `Passes.InlineExpansion` (via the
/// provider's `IInlineBodyProvider` channel) rather than served by a
/// codegen stopgap. `[]` manifests ⇒ `composite [MetadataSymbols; MockBuiltins]`
/// (the pre-demotion wiring), for callers that must stay off the contract.
let private compileContract
    (manifestPaths: string list)
    (project: ProjectInfo)
    (input: string)
    : TastFile * ClrArtifact =
    let provider = SymbolProviders.buildContract manifestPaths
    let lexed, file = parseFile input
    // 3B-4 checkpoint: callers assert on the returned `SemType` tast, but the real
    // `analyse` output is frozen — return the SemType tree, compile the frozen one.
    let tast = Pipeline.analyseSemFor project.AssemblyName provider input lexed file
    let artifact = Codegen.compile provider (withCore project) (Freeze.run tast)
    tast, artifact

/// The default compile path — now resolved through the contract stack
/// (`defaultManifests`) with `MockBuiltins` only as the backstop. This is the
/// contract-as-provider demotion: the same source
/// types the same way, but `int`/`hash`/the operators now resolve from the
/// `Vesper.Core` `.fsi` contract rather than the hand-curated mock.
let compileSource (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract defaultManifests (ProjectInfo.defaults assemblyName) input

/// Like `compileSource` but against a caller-supplied `ProjectInfo` (e.g. an
/// on-disk app build via `ProjectInfo.app`). `withCore` injects the compiled
/// `Vesper.Core.dll` unless the project is `Vesper.Core` itself.
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    compileContract defaultManifests project input |> snd

/// Explicit-manifest variant: stand the given layer-1 manifests up at the head of
/// the stack and share that one provider + inline bodies across both phases.
let compileSourceWith (manifestPaths: string list) (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract manifestPaths (ProjectInfo.defaults assemblyName) input

/// Contract-backed compile against the real `Vesper.Core` manifest (milestone M).
let compileSourceContract (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract [ vesperCoreManifest ] (ProjectInfo.defaults assemblyName) input

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

/// Invoke an already-loaded assembly's entry point under the shared console lock,
/// returning its exit code + captured stdout. Split out of `runEntryPoint` so a
/// driver loaded into a *specific* `AssemblyLoadContext` (e.g. the package-build
/// `packageAlc`, where a multi-dependency graph already resolves) can run through
/// the same capture path as the fresh-ALC `runEntryPoint`.
let runLoadedEntryPoint (asm: Assembly) : int * string =
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
                try
                    let result = entry.Invoke(null, [| box (Array.empty<string>) |])
                    Console.Out.Flush()
                    (result :?> int), captured.ToString()
                with :? System.Reflection.TargetInvocationException as e when not (isNull e.InnerException) ->
                    // Unwrap to the deepest cause: a runtime failure inside a static
                    // initializer surfaces as `TypeInitializationException` wrapping
                    // the real exception, which itself may wrap further. Report the
                    // whole chain so the root is legible.
                    let rec deepest (ex: exn) =
                        if isNull ex.InnerException then
                            ex
                        else
                            deepest ex.InnerException

                    let inner = e.InnerException
                    let root = deepest inner
                    let captured = captured.ToString()

                    failwithf
                        "Entry-point threw %s: %s\n--- root cause %s: %s ---\n%s\n--- captured stdout ---\n%s"
                        (inner.GetType().FullName)
                        inner.Message
                        (root.GetType().FullName)
                        root.Message
                        root.StackTrace
                        captured
            finally
                Console.SetOut original
        )

let runEntryPoint (bytes: byte[]) : int * string =
    // `MethodBase.Invoke` wraps any user-code exception in a
    // `TargetInvocationException`; `runLoadedEntryPoint` surfaces the inner
    // exception's type, message, and stack trace so a runtime IL bug
    // (`InvalidProgramException` from a malformed method body, a
    // `NullReferenceException`, a typed `ArithmeticException`) is *legible* in
    // the test failure instead of a single line of "Exception has been thrown
    // by the target of an invocation".
    runLoadedEntryPoint (loadAssembly bytes)

// ---- Layer 1 behavioral corpus helpers --------------------------------------
// The one-liners the suite was missing (docs/codegen-test-strategy-plan.md):
// the dominant assertion — "run this source, get this stdout, exit 0" — had no
// short form, so the cheap broad cases never got written. These wrap the
// existing `compileSource` + `runEntryPoint` machinery and carry `src` in every
// failure message so a red row inside a table is self-identifying. They raise
// (via `failwithf`) rather than depend on `Expecto.Expect`, which Expecto still
// reports as an ordinary test failure — keeping `TestHelpers` free of an Expecto
// reference.

/// Compile `src` as a bare program, run it in-process, and assert exit 0 and
/// that trimmed, CRLF-normalised stdout equals `expected`.
let runs (expected: string) (src: string) : unit =
    let _, artifact = compileSource "Layer1Corpus" src
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// Like `runs` but for a multi-line expected block (joined with "\n"); spares
/// callers the `\n` plumbing in the table.
let runsLines (expected: string list) (src: string) : unit = runs (String.concat "\n" expected) src

/// Compile `src` as a bare program, run it in-process, and assert it threw a
/// runtime exception whose type-name contains `expectedTypeFragment` (e.g.
/// `"DivideByZero"`). `runEntryPoint` surfaces a target-invocation failure as a
/// `failwithf` whose message embeds the inner exception's full type name, so the
/// fragment match keys off that. Used to prove an argument WAS evaluated (a
/// strict, non-short-circuiting parameter).
let runtimeThrows (expectedTypeFragment: string) (src: string) : unit =
    let _, artifact = compileSource "Layer1Corpus" src

    let thrown =
        try
            runEntryPoint (Codegen.toBytes artifact) |> ignore
            None
        with ex ->
            Some ex.Message

    match thrown with
    | Some msg when msg.Contains expectedTypeFragment -> ()
    | Some msg ->
        failwithf "expected a runtime %s but got a different failure:\n%s\nfor:\n%s" expectedTypeFragment msg src
    | None -> failwithf "expected a runtime %s but the program completed for:\n%s" expectedTypeFragment src

// ---- Vesper.Option runtime harness -----------
// `Vesper.Option` is *not* in `defaultManifests` (adding `Some`/`None`/`Option`
// to the global stack would shadow resolution in every other test), so it gets
// its own opt-in harness: build `Vesper.Option.dll` from `option.fs` once, load
// it into the *Default* ALC (like `vesperCoreDll`/`vesperListDll`), and compile
// driver programs against the default stack PLUS the Option contract, with the
// DLL in `References`. The emitted DLL is not BCL-only yet (it pulls
// `FSharp.Core.Unit` via the shared `unit` codegen cut — see PackageBuildTriage),
// which is harmless for an in-process `runEntryPoint`: FSharp.Core is present in
// the test process, so the program still loads and runs.

let vesperOptionSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Option", fileName)

let vesperOptionManifest: string = srcManifest "Vesper.Option"

/// Compile `Vesper.Option.dll` from `src/Vesper.Option/option.fs` against the
/// Vesper.Core contract (so `Fun` / `unit` / `raise` resolve from source), load
/// it into the Default `AssemblyLoadContext`, and return its path. Depends on
/// `Vesper.Core` only, so just the core DLL is in `References`.
let vesperOptionDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-option"
         let optionPath = IO.Path.Combine(outDir, "Vesper.Option.dll")

         let project =
             { ProjectInfo.library "Vesper.Option" with
                 OutputPath = Some optionPath
                 References = [ vesperCoreDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperOptionSource "option.fs")
         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath optionPath |> ignore
         optionPath)

/// Compile a driver program that `open`s `Vesper` and exercises the `Option`
/// type/module, run it in-process, and assert exit 0 with trimmed stdout equal
/// to `expected`. The Option contract is stacked on the default manifests and
/// `Vesper.Option.dll` is added to `References` (alongside the `withCore`
/// Core/List DLLs). The `Option`-module counterpart of `runs`.
let runsOption (expected: string) (src: string) : unit =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperOptionManifest ])

    let baseProject = withCore (ProjectInfo.defaults "OptionCorpus")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperOptionDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsOption` for a multi-line expected block.
let runsOptionLines (expected: string list) (src: string) : unit =
    runsOption (String.concat "\n" expected) src

/// Analyse `src` through the default contract stack (no codegen) and return the
/// error-severity diagnostics — the front-end-only half of the corpus.
/// `Pipeline.analyse` collects diagnostics rather than throwing, so both
/// `failsWith` and `typeChecks` read off the returned `TastFile.Diagnostics`.
let private analyseErrors (src: string) : Diagnostic list =
    let provider = SymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src`; assert it produced an error diagnostic whose message contains
/// `fragment`. The negative direction the suite was missing — pins that bad
/// input is *rejected*, and rejected for the stated reason.
let failsWith (fragment: string) (src: string) : unit =
    match analyseErrors src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

/// Analyse `src`; assert it produced NO error diagnostics, without running it —
/// for front-end-only coverage where codegen is deferred (cf. the duck-typed
/// for-in test in ForInTests.fs).
let typeChecks (src: string) : unit =
    match analyseErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// `analyseErrors` against the default contract stack PLUS the `Vesper.Option`
/// contract — the front-end-only probe for cross-package Option use.
/// No codegen, so it exercises type resolution + member access without the
/// backend B/C/D paths.
let private analyseOptionErrors (src: string) : Diagnostic list =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperOptionManifest ])

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against the Option contract; assert NO error diagnostics —
/// `typeChecks`'s Option-aware twin.
let typeChecksOption (src: string) : unit =
    match analyseOptionErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// Analyse `src` against the Option contract; assert an error diagnostic whose
/// message contains `fragment`. `failsWith`'s Option-aware twin.
let failsWithOption (fragment: string) (src: string) : unit =
    match analyseOptionErrors src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

// ---- Vesper.Result runtime harness -----------
// Mirrors the Vesper.Option harness above. `Vesper.Result` is *not* in
// `defaultManifests` (its `Ok`/`Error`/`Result` would shadow resolution in every
// other test), so driver programs opt in by stacking the Result contract and
// referencing a once-built `Vesper.Result.dll`. Unlike Option, Result is BCL-only
// (Gap 1 closed), so the emitted DLL pulls no FSharp.Core — but the harness is
// otherwise identical (the in-process run resolves any dep against the test
// process regardless).

let vesperResultSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Result", fileName)

let vesperResultManifest: string = srcManifest "Vesper.Result"

/// Compile `Vesper.Result.dll` from `src/Vesper.Result/result.fs` against the
/// Vesper.Core contract (so `Fun` / `unit` / `raise` resolve from source), load
/// it into the Default `AssemblyLoadContext`, and return its path. Depends on
/// `Vesper.Core` only, so just the core DLL is in `References`. (Loaded into the
/// Default ALC — not the `buildPackage` `packageAlc` — so a `runEntryPoint` driver
/// program, which runs in a fresh ALC that falls back to Default, can resolve it.)
let vesperResultDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-result"
         let resultPath = IO.Path.Combine(outDir, "Vesper.Result.dll")

         let project =
             { ProjectInfo.library "Vesper.Result" with
                 OutputPath = Some resultPath
                 References = [ vesperCoreDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperResultSource "result.fs")
         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath resultPath |> ignore
         resultPath)

/// Compile a `Vesper.Result` consumer through the full backend and return the
/// `ClrArtifact` (no run) — for assertions on `FSharpCoreDependencies`, e.g. that
/// a `%A` of an external Vesper union lowers on the structural engine (the use-set
/// stays clear of `PrintfModule.PrintFormatLine`) rather than the cold path.
let compileResultArtifact (src: string) : ClrArtifact =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperResultManifest ])

    let baseProject = withCore (ProjectInfo.defaults "ResultDeps")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperResultDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    Codegen.compile provider project tast

/// Compile a driver program that `open`s `Vesper` and exercises the `Result`
/// type/module, run it in-process, and assert exit 0 with trimmed stdout equal to
/// `expected`. The `Result`-module counterpart of `runsOption`.
let runsResult (expected: string) (src: string) : unit =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperResultManifest ])

    let baseProject = withCore (ProjectInfo.defaults "ResultCorpus")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperResultDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsResult` for a multi-line expected block.
let runsResultLines (expected: string list) (src: string) : unit =
    runsResult (String.concat "\n" expected) src

/// `analyseErrors` against the default contract stack PLUS the `Vesper.Result`
/// contract — the front-end-only probe for cross-package Result use.
let private analyseResultErrors (src: string) : Diagnostic list =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperResultManifest ])

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against the Result contract; assert NO error diagnostics —
/// `typeChecks`'s Result-aware twin.
let typeChecksResult (src: string) : unit =
    match analyseResultErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// Analyse `src` against the Result contract; assert an error diagnostic whose
/// message contains `fragment`. `failsWith`'s Result-aware twin.
let failsWithResult (fragment: string) (src: string) : unit =
    match analyseResultErrors src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

// ---- Vesper.Choice runtime harness -----------
// Mirrors the Vesper.Option / Vesper.Result harnesses above. `Vesper.Choice` is
// *not* in `defaultManifests` (its `Choice`/`Choice1Of2`/`Choice2Of2` would shadow
// resolution in every other test), so driver programs opt in by stacking the
// Choice contract and referencing a once-built `Vesper.Choice.dll`. Choice is a
// pure-data struct union with NO module (its sole consumer `set.fs` uses only the
// constructors + pattern matching), so there is no `runs…`-via-module-call surface
// — only construction (Layer B) and `match` (Layer C). Like Result it is BCL-only.

let vesperChoiceSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Choice", fileName)

let vesperChoiceManifest: string = srcManifest "Vesper.Choice"

/// Compile `Vesper.Choice.dll` from `src/Vesper.Choice/choice.fs` against the
/// Vesper.Core contract (so `unit` / `bool` / `int` resolve from source), load it
/// into the Default `AssemblyLoadContext`, and return its path. Depends on
/// `Vesper.Core` only, so just the core DLL is in `References`. (Loaded into the
/// Default ALC — not the `buildPackage` `packageAlc` — so a `runEntryPoint` driver
/// program, which runs in a fresh ALC that falls back to Default, can resolve it.)
let vesperChoiceDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-choice"
         let choicePath = IO.Path.Combine(outDir, "Vesper.Choice.dll")

         let project =
             { ProjectInfo.library "Vesper.Choice" with
                 OutputPath = Some choicePath
                 References = [ vesperCoreDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperChoiceSource "choice.fs")
         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath choicePath |> ignore
         choicePath)

/// Compile a driver program that `open`s `Vesper` and exercises the `Choice` type,
/// run it in-process, and assert exit 0 with trimmed stdout equal to `expected`.
/// The `Choice`-type counterpart of `runsResult`.
let runsChoice (expected: string) (src: string) : unit =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperChoiceManifest ])

    let baseProject = withCore (ProjectInfo.defaults "ChoiceCorpus")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperChoiceDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsChoice` for a multi-line expected block.
let runsChoiceLines (expected: string list) (src: string) : unit =
    runsChoice (String.concat "\n" expected) src

/// `analyseErrors` against the default contract stack PLUS the `Vesper.Choice`
/// contract — the front-end-only probe for cross-package Choice use.
let private analyseChoiceErrors (src: string) : Diagnostic list =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperChoiceManifest ])

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against the Choice contract; assert NO error diagnostics —
/// `typeChecks`'s Choice-aware twin.
let typeChecksChoice (src: string) : unit =
    match analyseChoiceErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// Analyse `src` against the Choice contract; assert an error diagnostic whose
/// message contains `fragment`. `failsWith`'s Choice-aware twin.
let failsWithChoice (fragment: string) (src: string) : unit =
    match analyseChoiceErrors src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

// ---- Vesper.Array runtime harness ------------
// Mirrors the Option/Result/Choice harnesses. `Vesper.Array` is *not* in
// `defaultManifests` (its `Array` module would shadow resolution elsewhere), so
// driver programs opt in by stacking the Array contract and referencing a
// once-built `Vesper.Array.dll`. The DLL is BCL-only (proven by
// `PackageBuildTriage`): `arr.[i]`/`arr.Length`/`Array.zeroCreate` lower to the
// `ldelem`/`ldlen`/`newarr` IL intrinsics, no FSharp.Core. (Array *literals*
// `[| … |]` in a driver still route through FSharp.Core's `ArrayModule.OfList`,
// which is harmless in-process — but the rows here build arrays through our own
// `zeroCreate` so they stay on the BCL-only path.)

let vesperArraySource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Array", fileName)

let vesperArrayManifest: string = srcManifest "Vesper.Array"

/// Compile `Vesper.Array.dll` from `src/Vesper.Array/array.fs` against the
/// Vesper.Core contract (so `Fun` / `int` / the `'T[]` intrinsic resolve from
/// source), load it into the Default `AssemblyLoadContext`, and return its path.
/// Depends on `Vesper.Core` only. (Loaded into the Default ALC so a
/// `runEntryPoint` driver program — which runs in a fresh ALC that falls back to
/// Default — can resolve it.)
let vesperArrayDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-array"
         let arrayPath = IO.Path.Combine(outDir, "Vesper.Array.dll")

         let project =
             { ProjectInfo.library "Vesper.Array" with
                 OutputPath = Some arrayPath
                 References = [ vesperCoreDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperArraySource "array.fs")
         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath arrayPath |> ignore
         arrayPath)

/// Compile a driver program that `open`s `Vesper.Collections` and exercises the
/// `Array` module, run it in-process, and assert exit 0 with trimmed stdout equal
/// to `expected`. The Array contract is stacked on the default manifests and
/// `Vesper.Array.dll` is added to `References`. The `Array`-module counterpart of
/// `runs`.
let runsArray (expected: string) (src: string) : unit =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperArrayManifest ])

    let baseProject = withCore (ProjectInfo.defaults "ArrayCorpus")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperArrayDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsArray` for a multi-line expected block.
let runsArrayLines (expected: string list) (src: string) : unit =
    runsArray (String.concat "\n" expected) src

/// `analyseErrors` against the default contract stack PLUS the `Vesper.Array`
/// contract — the front-end-only probe for cross-package Array use.
let private analyseArrayErrors (src: string) : Diagnostic list =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperArrayManifest ])

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against the Array contract; assert NO error diagnostics —
/// `typeChecks`'s Array-aware twin.
let typeChecksArray (src: string) : unit =
    match analyseArrayErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

// ---- Vesper.Seq runtime harness --------------
// Mirrors the Array harness. `Vesper.Seq` is *not* in `defaultManifests` (its
// `Seq` module would shadow resolution elsewhere), so driver programs opt in by
// stacking the Seq contract and referencing a once-built `Vesper.Seq.dll`. The
// DLL is BCL-only (proven by `PackageBuildTriage`): the explicit-enumerator
// terminals `fold`/`reduce`/`toArray` drive `source.GetEnumerator()` /
// `MoveNext` / `Current` over `IEnumerator<'T>`, and `truncate` delegates to the
// generic external `System.Linq.Enumerable.Take<TSource>`.
//
// Depends on Vesper.Core (`Fun`, `int`, `'T[]`) AND Vesper.List (the `seq<'T>` /
// `ResizeArray<'T>` abbreviations declared in `list.fsi`), so both DLLs are in
// `References` and both manifests in the contract stack — the same `[core, list]`
// stack `buildPackage "Vesper.Seq"` resolves through.
//
// A driver's `seq<'T>` source is `System.Linq.Enumerable.Range(start, count)` (a
// real BCL `IEnumerable<int>`) — the Vesper cons-list declares `IEnumerable<'T>`
// in its `.fsi` but does not implement it in `list.fs`, so a list value is not a
// runtime seq (get-enumerator-gaps.md Gap 2). `Range` sidesteps that entirely.

let vesperSeqSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Seq", fileName)

let vesperSeqManifest: string = srcManifest "Vesper.Seq"

/// Compile `Vesper.Seq.dll` from `src/Vesper.Seq/seq.fs` against the Vesper.Core +
/// Vesper.List contracts (so `Fun` / `'T[]` / the `seq<'T>` + `ResizeArray<'T>`
/// abbreviations resolve from source), load it into the Default
/// `AssemblyLoadContext`, and return its path. Depends on both `Vesper.Core` and
/// `Vesper.List`, so both DLLs are in `References`. (Loaded into the Default ALC so
/// a `runEntryPoint` driver program — which runs in a fresh ALC that falls back to
/// Default — can resolve it.)
let vesperSeqDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-seq"
         let seqPath = IO.Path.Combine(outDir, "Vesper.Seq.dll")

         let project =
             { ProjectInfo.library "Vesper.Seq" with
                 OutputPath = Some seqPath
                 References = [ vesperCoreDll.Value; vesperListDll.Value ]
             }

         let src = IO.File.ReadAllText(vesperSeqSource "seq.fs")

         let provider =
             SymbolProviders.buildContract [ vesperCoreManifest; vesperListManifest ]

         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath seqPath |> ignore
         seqPath)

/// Compile a driver program that `open`s `Vesper.Collections` and exercises the
/// `Seq` module, run it in-process, and assert exit 0 with trimmed stdout equal to
/// `expected`. The Seq contract is stacked on the default manifests and
/// `Vesper.Seq.dll` is added to `References`. The `Seq`-module counterpart of
/// `runs`.
let runsSeq (expected: string) (src: string) : unit =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperSeqManifest ])

    let baseProject = withCore (ProjectInfo.defaults "SeqCorpus")

    let project =
        { baseProject with
            References = baseProject.References @ [ vesperSeqDll.Value ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsSeq` for a multi-line expected block.
let runsSeqLines (expected: string list) (src: string) : unit =
    runsSeq (String.concat "\n" expected) src

/// `analyseErrors` against the default contract stack PLUS the `Vesper.Seq`
/// contract — the front-end-only probe for cross-package Seq use.
let private analyseSeqErrors (src: string) : Diagnostic list =
    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ vesperSeqManifest ])

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against the Seq contract; assert NO error diagnostics —
/// `typeChecks`'s Seq-aware twin.
let typeChecksSeq (src: string) : unit =
    match analyseSeqErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

// ---- Vesper.Set runtime harness -------
// `Vesper.Set` (the immutable AVL-tree set + the `Set` module) is the capstone
// self-host package. Its DLL builds + links + loads BCL-only — proven by
// `PackageBuildTriage` "Vesper.Set builds BCL-only". This harness adds the
// *runtime round-trip* the §9.7 gate calls for (`Set.add`/`contains`/`toList`/
// `union`/`intersect`/`fold` and the wider Phase-9-exit operation set).
//
// `Set` is NOT in `defaultManifests` (its `Set` module would shadow resolution
// everywhere), so a driver opts in. Unlike the Option/Choice/Seq harnesses —
// which load each dependency DLL into the *Default* ALC — a Set driver has eight
// transitive `Vesper.*` deps, all already built + registered in `packageAlc` by
// `buildPackage "Vesper.Set"`. So the driver is loaded into `packageAlc` itself
// (where every `Vesper.*` dep resolves off the registry, and FSharp.Core / the
// BCL fall through to Default) rather than re-loading the whole graph into
// Default. The driver's assembly name is uniquified per call so successive loads
// into the persistent `packageAlc` get distinct identities.
//
// NOTE on driver shape: HOF arguments (`Set.fold`/`partition`'s folder) are
// written *curried* (`fun s -> fun x -> …`) per the same Freeze multi-arg-lambda
// posture the Seq harness documents.

/// Transitive package names for `root` (dependencies before dependents,
/// deduplicated, `root` last) — drives both the contract stack and the
/// `References` DLL list. Reads each package's `depends-on` off its manifest.
let private transitivePackages (root: string) : string list =
    let acc = System.Collections.Generic.List<string>()

    let rec go (pkg: string) =
        if not (acc.Contains pkg) then
            match ReferencedProject.loadManifest (srcManifest pkg) with
            | Result.Ok m ->
                m.DependsOn |> List.iter go

                if not (acc.Contains pkg) then
                    acc.Add pkg
            | Result.Error e -> failwithf "transitivePackages %s: %s" pkg e

    go root
    List.ofSeq acc

/// Uniquifies the per-call driver assembly name (Expecto runs tests in parallel;
/// `packageAlc` is process-persistent, so two `SetSmoke` loads would collide on
/// identity).
let private setDriverCounter = ref 0

/// Compile a driver program that `open`s `Vesper.Collections` and exercises the
/// `Set` type/module, run it inside `packageAlc` (so `Set` + its eight transitive
/// `Vesper.*` deps resolve off the package-build registry), and assert exit 0
/// with trimmed stdout equal to `expected`. The `Set` counterpart of `runsSeq`,
/// but routed through `packageAlc` rather than the Default ALC.
let runsSet (expected: string) (src: string) : unit =
    // Force the whole graph (Set + every transitive dep) — registers them all in
    // `packageAlc` and yields each one's on-disk DLL for the driver's References.
    let packages = transitivePackages "Vesper.Set"

    let depDlls =
        packages |> List.choose (fun p -> ((buildPackage p).Value |> snd).OutputPath)

    let provider = SymbolProviders.buildContract (packages |> List.map srcManifest)

    let n = System.Threading.Interlocked.Increment setDriverCounter

    let project =
        { ProjectInfo.defaults (sprintf "SetSmoke%d" n) with
            References = depDlls
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file

    let analysisErrors =
        tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty analysisErrors) then
        failwithf
            "runsSet: %d analysis error(s) for:\n%s\n--- errors ---\n%s"
            (List.length analysisErrors)
            src
            (analysisErrors |> List.map (fun d -> d.Message) |> String.concat "\n")

    let artifact = Codegen.compile provider project tast

    use ms = new IO.MemoryStream(Codegen.toBytes artifact)
    let asm = packageAlc.LoadFromStream ms
    let exitCode, output = runLoadedEntryPoint asm
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsSet` for a multi-line expected block.
let runsSetLines (expected: string list) (src: string) : unit =
    runsSet (String.concat "\n" expected) src

// ---- PE inspection helpers (deep introspection for codegen tests) -----------
// Reach beyond `loadAssembly`'s reflection view: open the emitted PE through
// `System.Reflection.Metadata` so a test can read raw metadata (Method/Field
// tokens, AssemblyRef table, IL bytes) without going through the runtime
// loader. Useful when debugging a malformed IL emission (`InvalidProgramException`)
// or asserting the *structure* of an emitted PE — e.g., "method X references
// AssemblyRef Vesper.Core", "field F has signature Y" — rather than the
// behaviour of its execution.

open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// Open a PE byte stream as a metadata reader. The caller must dispose the
/// returned `PEReader`; the `MetadataReader` it yields stays valid for the
/// reader's lifetime.
let openPe (bytes: byte[]) : PEReader =
    new PEReader(System.Collections.Immutable.ImmutableArray.Create<byte>(bytes))

/// List every type-def's full name (`Namespace.TypeName`) in the PE. Anonymous
/// `<Module>` is excluded so a "no user type" assertion can be punctual.
let peTypeDefNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for h in md.TypeDefinitions do
            let td = md.GetTypeDefinition h
            let name = md.GetString td.Name

            if name <> "<Module>" then
                let ns = md.GetString td.Namespace

                if System.String.IsNullOrEmpty ns then
                    name
                else
                    sprintf "%s.%s" ns name
    ]

/// List every method-def's `(declaringType, methodName)` in the PE. The
/// declaring type's name comes through `peTypeDefNames`'s formatting.
let peMethodNames (bytes: byte[]) : (string * string) list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for tdHandle in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdHandle
            let typeName = md.GetString td.Name

            if typeName <> "<Module>" then
                let ns = md.GetString td.Namespace

                let qualified =
                    if System.String.IsNullOrEmpty ns then
                        typeName
                    else
                        sprintf "%s.%s" ns typeName

                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh
                    yield qualified, md.GetString m.Name
    ]

/// Every AssemblyRef name in the PE's reference table — the dependency surface
/// the loader resolves at load. Symmetric to `Assembly.GetReferencedAssemblies`
/// but works directly off PE bytes (no `AssemblyLoadContext` needed).
let peAssemblyRefs (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for h in md.AssemblyReferences do
            let r = md.GetAssemblyReference h
            md.GetString r.Name
    ]

/// Total number of `InterfaceImpl` rows across every type-def in the PE — the
/// count of `: IFace` entries the metadata carries (one per implemented
/// interface). Reflection's `GetInterfaces` folds in transitively-inherited
/// interfaces, so this raw count is what distinguishes "emitted both
/// `IEnumerable<int>` and `IEnumerable`" from "emitted only the generic one
/// and inherited the non-generic".
let peInterfaceImplCount (bytes: byte[]) : int =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.sumBy (fun h -> (md.GetTypeDefinition h).GetInterfaceImplementations().Count)

/// Read the IL byte stream of a method by `(declaringType, methodName)` —
/// useful for asserting a specific opcode sequence (e.g., "the closure body
/// emits stfld, ldnull, ret") or printing a hex dump in a failing test. Returns
/// an empty array for an abstract method (no body). Throws if the method is
/// not found.
let peMethodIl (bytes: byte[]) (declaringType: string) (methodName: string) : byte[] =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let typeMatches (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        let qualified =
            if System.String.IsNullOrEmpty ns then
                name
            else
                sprintf "%s.%s" ns name

        qualified = declaringType

    let methodHandle =
        md.TypeDefinitions
        |> Seq.tryPick (fun tdh ->
            let td = md.GetTypeDefinition tdh

            if typeMatches td then
                td.GetMethods()
                |> Seq.tryFind (fun mdh -> md.GetString(md.GetMethodDefinition(mdh).Name) = methodName)
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodIl: no method '%s' on type '%s'" methodName declaringType
    | Some mdh ->
        let m = md.GetMethodDefinition mdh

        if m.RelativeVirtualAddress = 0 then
            [||]
        else
            let body = peReader.GetMethodBody m.RelativeVirtualAddress
            let ilReader = body.GetILReader()
            let buf = Array.zeroCreate ilReader.RemainingBytes
            ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
            buf

/// The *return type's* element-type tag in a method's MethodDef signature, found
/// by `(declaringType, methodName)`. Parses the signature blob through a
/// `BlobReader` — skipping the calling-convention header + compressed param count
/// — and returns the first byte of the return type, i.e. its `ELEMENT_TYPE_*` tag.
/// For a method returning a user/referenced nominal type that is the encoder's
/// `VALUETYPE`-vs-`CLASS` decision point: `0x11` (ELEMENT_TYPE_VALUETYPE) vs `0x12`
/// (ELEMENT_TYPE_CLASS). Read straight off the emitted metadata — no loader, no
/// referenced assembly needed. Throws if the method is not found.
let peMethodReturnElementType (bytes: byte[]) (declaringType: string) (methodName: string) : byte =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let qualifiedOf (td: TypeDefinition) =
        let name = md.GetString td.Name
        let ns = md.GetString td.Namespace

        if System.String.IsNullOrEmpty ns then
            name
        else
            sprintf "%s.%s" ns name

    let methodHandle =
        md.TypeDefinitions
        |> Seq.tryPick (fun tdh ->
            let td = md.GetTypeDefinition tdh

            if qualifiedOf td = declaringType then
                td.GetMethods()
                |> Seq.tryFind (fun mdh -> md.GetString(md.GetMethodDefinition(mdh).Name) = methodName)
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodReturnElementType: no method '%s' on type '%s'" methodName declaringType
    | Some mdh ->
        let mutable r = md.GetBlobReader((md.GetMethodDefinition mdh).Signature)
        r.ReadSignatureHeader() |> ignore // calling convention (HASTHIS etc.)
        r.ReadCompressedInteger() |> ignore // parameter count
        r.ReadByte() // return type's ELEMENT_TYPE_* tag

/// Format a PE byte array as a hex string (`"02 00 01 …"`), capped so a test
/// failure message stays readable.
let formatIlBytes (bytes: byte[]) : string =
    bytes
    |> Array.truncate 64
    |> Array.map (sprintf "%02x")
    |> String.concat " "
    |> fun s ->
        if bytes.Length > 64 then
            s + sprintf " ... (%d bytes total)" bytes.Length
        else
            s
