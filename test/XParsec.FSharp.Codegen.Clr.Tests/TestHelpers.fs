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
    SemType.TyUnion(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyRecord (name: string, args: EqArray<SemType>) =
    SemType.TyRecord(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let TyClass (name: string, args: EqArray<SemType>) =
    SemType.TyClass(SymbolKeyOps.qualifiedTypeKeyOf name args.Length, args)

let private nominalDisplayName (k: TypeKey) : string = SymbolKeyOps.typeMetaName k

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
/// (`| [ TDecl.Let _ ] -> …`, `| [ x; y ] -> …`) verbatim across the flip.
let inline (|EqList|) (xs: EqArray<'T>) : 'T list = EqArray.toList xs

/// A frozen file's declarations as pool handles — what `Layout.buildUnit` opens before
/// anything else, so a test that drives a lowering / discovery pass directly starts from
/// the same representation the backend does.
let pooledDecls (frozen: FrozenPools) : TastAccessor.DeclId list =
    TastAccessor.roots (TastPoolBuilder.openOver frozen) |> List.ofArray

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

/// `src/Vesper.Printf/<fileName>`, relative to this test file.
let vesperPrintfSource (fileName: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Printf", fileName)

/// `src/Vesper.Core/manifest.toml` — the Vesper.Core layer-1 referenced-project
/// manifest. Declared up here (above the
/// `vesperListDll` fixture, which references it) rather than in the
/// downstream manifest block.
let vesperCoreManifest: string = vesperCoreSource "manifest.toml"

/// Render a multi-file driver's anchored diagnostics (`path: message`, one per line)
/// for a fixture's failure message.
let private anchoredDiagText (diags: AssemblyUnits.AnchoredDiagnostic list) : string =
    diags
    |> List.map (fun d -> sprintf "%s: %s" d.Path d.Diagnostic.Message)
    |> String.concat "\n"

/// Compile `Vesper.Core.dll` from `prim-types-min.fs` + `core-types.fs` (the
/// `Vesper.Fun\`2` interface, the primitive intrinsics, and the `Vesper.Ref\`1`
/// captured-mutable cell), load it into the *Default* `AssemblyLoadContext`, and
/// return its path. An in-process user PE loaded into a fresh context resolves
/// `Fun` / `Ref` through that context's fallback to Default, exactly how
/// `Vesper.Printf` already resolves. Forced once; later compiles inject the path
/// so their function values + promoted-mutable cells reference this DLL.
/// Compiled with **no** core injected — `Vesper.Core` *defines* `Fun` and
/// `Ref`. The cons-list is its own package now (`vesperListDll` →
/// `Vesper.List.dll`), not concatenated here.
let vesperCoreDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-core"
         let corePath = IO.Path.Combine(outDir, "Vesper.Core.dll")

         let project =
             { ProjectInfo.library "Vesper.Core" with
                 OutputPath = Some corePath
             }

         // Compile every `impl` file the manifest lists as its OWN unit through the
         // shared multi-file seam (`ClrDriver.compileAssemblyWith`) — the fixture and the
         // package build share ONE source list (no fixture/manifest drift), and each file
         // is analysed against the composed prior-unit views rather than fused into one
         // `String.concat` blob. The intrinsic-only prim-types files complete channel-1, so
         // primitive reprs (`string`, …) resolve from Core's own `.fs`.
         let implFiles =
             match ReferencedProject.loadManifest vesperCoreManifest with
             | Ok m -> ReferencedProject.resolveImpl None m
             | Error e -> failwithf "vesperCoreDll: cannot load Vesper.Core manifest: %s" e

         let files =
             implFiles
             |> List.map (fun rel -> vesperCoreSource rel, IO.File.ReadAllText(vesperCoreSource rel))

         // Vesper.Core *defines* its own primitives + operators, so it compiles
         // against the empty contract stack (just the BCL metadata leaf for the
         // `(# "System.Int32" #)` reprs) — the same provider `buildPackage
         // "Vesper.Core"` uses (Core has no `depends-on`).
         let provider = ClrSymbolProviders.buildContract []

         let artifact =
             match ClrDriver.compileAssemblyWith Pipeline.analyseFor [] provider project files with
             | Ok artifact -> artifact
             | Error diags ->
                 failwithf "vesperCoreDll: %d analysis error(s):\n%s" (List.length diags) (anchoredDiagText diags)

         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath corePath |> ignore
         corePath)

/// Compile `Vesper.List.dll` from `src/Vesper.List/list.fs` — the
/// `Vesper.Collections.List\`1` cons-list (`Cons`/`Empty` + `IsEmpty`/`Head`/`Tail`)
/// **and** the `Vesper.Collections.ListModule::fold` static method
/// (`fold` is compiled into the DLL now) — as its own package,
/// load it into the *Default* `AssemblyLoadContext`, and return its path.
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
         // contract for the call head to inline.
         // Self-manifest (`Vesper.List`'s own) is excluded; the package is
         // *defining* its types here.
         let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile src
         let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file
         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath listPath |> ignore
         listPath)

/// The other contract packages that round out the default resolution stack.
let vesperListManifest: string = vesperListSource "manifest.toml"

let private srcManifest (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg, "manifest.toml")

let vesperComparisonManifest: string = srcManifest "Vesper.Comparison"

/// `src/Vesper.Printf/manifest.toml` — the printf family (`printf`/`printfn`/
/// `sprintf`) as its own `[<AutoOpen>] module Printf` contract, so a `printfn`
/// call resolves from the real contract source.
let vesperPrintfManifest: string = srcManifest "Vesper.Printf"

/// The default contract stack the compile path resolves through. Everything a
/// bare program needs now comes from real `Vesper.*` `.fsi` contracts, and an
/// operator *emits* from the matching `.fs` contract body too — spliced by
/// `Passes.InlineExpansion`, whether applied (`1 + 2`) or used as a value
/// (`List.fold (+) 0 xs`, which the same pass eta-reifies first). Codegen owns no
/// per-operator dispatch at all.
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

/// Front-end a program to a (SemType) `TastFile` through the default contract
/// stack. The generic front-end-only helper — was the value-only `MockBuiltins`
/// fixture; now the real `Vesper.*` contracts (a superset).
let analyse (input: string) : TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSem (ClrSymbolProviders.buildContract defaultManifests) input lexed file

/// `analyse`, keeping the `PassContext`. `Freeze.run` needs it: the binder a residual
/// typar root belongs to is recorded in `ctx.Bindings.Scheme`, not recoverable from the
/// TAST alone.
let analyseWithCtx (input: string) : PassContext * TastFile =
    let lexed, file = parseFile input
    Pipeline.analyseSemWithContext (ClrSymbolProviders.buildContract defaultManifests) input lexed file

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

    override _.Load(name: System.Reflection.AssemblyName) : Assembly =
        match built.TryGetValue name.Name with
        | true, asm -> asm
        | _ -> null

let private packageAlc = PackageLoadContext()

let private packageBuildCache =
    Collections.Concurrent.ConcurrentDictionary<string, Lazy<Assembly * ClrArtifact>>(StringComparer.Ordinal)

/// Compile `src/<package>/`'s `impl` `.fs` files (in manifest order) to a
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

                 // T8 Step 5: `.fsi`↔`.fs` conformance is a HARD gate on the build. A
                 // contract binding with no implementation (and not declared `sig-only`
                 // in the manifest) is an FS0240-style error — no codegen substitution
                 // may stand in for a missing `.fs`.
                 match ConformancePass.checkManifest None manifestPath with
                 | Result.Error e -> failwithf "buildPackage %s: conformance: %s" pkg e
                 | Result.Ok outcome ->
                     match ConformancePass.enforce outcome with
                     | [] -> ()
                     | ds ->
                         failwithf
                             "buildPackage %s: %d conformance error(s):\n%s"
                             pkg
                             (List.length ds)
                             (ds |> List.map (fun d -> d.Message) |> String.concat "\n")

                 // Force each dependency's build first (recursively, shared cache):
                 // this loads + registers it in `packageAlc`, so the current package
                 // resolves against it at load time. Collect each dep's on-disk DLL
                 // for `References` (the emit-time AssemblyRef) and its manifest for
                 // the contract provider / inline bodies.
                 let depArtifacts =
                     manifest.DependsOn |> List.map (fun d -> (buildPackage d).Value |> snd)

                 let depDlls = depArtifacts |> List.choose (fun art -> art.OutputPath)
                 let depManifests = manifest.DependsOn |> List.map srcManifest

                 let provider = ClrSymbolProviders.buildContract depManifests

                 let dir = IO.Path.GetDirectoryName manifestPath

                 // Each `impl` file is analysed as its OWN unit through the shared multi-file
                 // seam (`ClrDriver.compileAssemblyWith Pipeline.analyseForSelfHost`) rather
                 // than fused into one `String.concat` blob — self-host front end, so a bare
                 // `[]`/`::` in a BCL-only package defaults to the Vesper cons-list, not
                 // FSharp.Core's. The seam gates on error-severity front-end diagnostics (a
                 // package that doesn't type-check hasn't built), returning `Error` rather than
                 // emitting a degraded DLL.
                 let files =
                     manifest.Impl
                     |> List.map (fun rel ->
                         let p = IO.Path.Combine(dir, rel)
                         p, IO.File.ReadAllText p
                     )

                 let outDir = tmpDir (sprintf "pkg-%s" pkg)
                 let outPath = IO.Path.Combine(outDir, manifest.Name + ".dll")

                 let project =
                     { ProjectInfo.library manifest.Name with
                         OutputPath = Some outPath
                         References = depDlls
                     }

                 let artifact =
                     match ClrDriver.compileAssemblyWith Pipeline.analyseForSelfHost [] provider project files with
                     | Ok artifact -> artifact
                     | Error diags ->
                         failwithf
                             "buildPackage %s: %d analysis error(s):\n%s"
                             pkg
                             (List.length diags)
                             (anchoredDiagText diags)

                 Codegen.materialise artifact

                 use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)

                 // A contract-only package (`impl = []`: Vesper.Comparison, whose
                 // operators are inlined) compiles to an *empty* DLL here — it carries
                 // no runtime types. `Vesper.Printf` is a special case: its runtime
                 // peer — the Vesper-compiled `Vesper.Formatter` (`formatter.fs`) and
                 // `StructuralPrinter` (`structural-printer.fs`, the `%A` engine), plus
                 // the printf module surface — is loaded separately into the Default ALC
                 // by `vesperPrintfDll` for the in-process driver path. Registering it in
                 // `packageAlc` too would make a second copy: a driver `printfn` would
                 // bind `Vesper.Formatter` / `StructuralPrinter` to the wrong one and
                 // fail. So load it into a throwaway context and leave `packageAlc`
                 // without it — the driver's `Vesper.Printf` reference then falls through
                 // to the Default ALC copy. The on-disk path stays in `References` for
                 // emit-time identity. (The `buildsBclOnly "Vesper.Printf"` test only
                 // needs the build to succeed; it does not drive the emitted handler.)
                 if List.isEmpty manifest.Impl || manifest.Name = "Vesper.Printf" then
                     let throwaway = AssemblyLoadContext("xparsec-contract-only", isCollectible = true)

                     throwaway.LoadFromStream ms, artifact
                 else
                     let asm = packageAlc.LoadFromStream ms
                     packageAlc.Register(manifest.Name, asm)
                     asm, artifact)
    )

/// The Vesper-compiled `Vesper.Printf.dll` (`structural-printer.fs` + `formatter.fs`),
/// built by the `buildPackage` harness and loaded into the *Default*
/// `AssemblyLoadContext` — the in-process runtime printf/`%A` handler a driver binds
/// (the only `Vesper.Printf` in Default, so a fresh-ALC
/// `runEntryPoint` driver resolves it through the Default fall-through). Forcing it builds `Vesper.Printf` (and its `Vesper.Core` /
/// `Vesper.List` deps) and loads the on-disk DLL into Default; the printf assembly's
/// `Vesper.Core` / `Vesper.List` references resolve by simple name to the
/// `vesperCoreDll` / `vesperListDll` copies (forced first), the same value-identity
/// unification `withPrintfAlc` relies on. (`buildPackage` itself loads `Vesper.Printf`
/// only into a throwaway context — see its `manifest.Name = "Vesper.Printf"` case — so
/// this is the Default-ALC copy the driver path needs.)
let vesperPrintfDll: Lazy<string> =
    lazy
        (vesperCoreDll.Value |> ignore
         vesperListDll.Value |> ignore

         let path =
             match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
             | Some p -> p
             | None -> failwith "buildPackage Vesper.Printf produced no OutputPath"

         AssemblyLoadContext.Default.LoadFromAssemblyPath path |> ignore
         path)

/// Add the compiled `Vesper.Core.dll` (for `Vesper.Fun`), `Vesper.List.dll` (for
/// `Vesper.Collections.List`), and the Vesper-compiled
/// `Vesper.Printf.dll` (for `Vesper.Formatter`, the happy-path printf/`%A` handler)
/// to a project's `References`, so a program's function
/// values, list literals, and `printf` calls resolve. Each path is added only when
/// absent, and never into the package that *defines* the type (a package must not
/// reference itself): `Vesper.Core` gets no core ref, `Vesper.List` no list ref,
/// `Vesper.Printf` no printf ref. Forcing each lazy loads the DLL into the Default ALC
/// before any in-process run. The printf ref is `lazy`-forced and unused-by-the-PE
/// when the program has no `printf` (an unforced `AssemblyRef` emits nothing), so a
/// non-printf program neither gains a `Vesper.Printf` `AssemblyRef` nor ships the DLL.
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
            |> ensure "Vesper.Printf" vesperPrintfDll
    }

/// Build the symbol-resolution stack + its cross-package inline bodies once
/// (cached per manifest set by `ClrSymbolProviders.buildContract`) and run *both*
/// phases against it: a use-site
/// `External(name)` whose body lives in a referenced `.fs` (today: `hash` from
/// `ops-platform.fs`) is spliced in pre-freeze by `Passes.InlineExpansion` (off the
/// resolved symbol's own `InlineBody`) rather than served by a codegen stopgap. `[]` manifests ⇒ the BCL metadata leaf alone, for callers
/// that must stay off the Vesper contracts.
let private compileContract
    (manifestPaths: string list)
    (project: ProjectInfo)
    (input: string)
    : TastFile * ClrArtifact =
    let provider = ClrSymbolProviders.buildContract manifestPaths
    let lexed, file = parseFile input
    // Callers assert on the returned `SemType` tast, but the real
    // `analyse` output is frozen — return the SemType tree, compile the frozen one.
    let ctx, tast =
        Pipeline.analyseSemWithContextFor project.AssemblyName provider input lexed file

    let artifact = Codegen.compile provider (withCore project) (Freeze.run ctx tast)
    tast, artifact

/// The default compile path — resolved through the contract stack
/// (`defaultManifests`). This is the contract-as-provider demotion: `int`/`hash`/
/// the operators resolve from the `Vesper.Core` `.fsi` contract (no hand-curated
/// mock).
let compileSource (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract defaultManifests (ProjectInfo.defaults assemblyName) input

/// The CLR artifacts a frozen-tree round-trip must reconcile against the DIRECT
/// codegen: the frozen-cache `thaw (flatten frozen)` and the DU round-trip
/// `TastPools.toPools (TastPools.ofPools frozen)`. Both are codegen-INVARIANT
/// obligations over the same tree, so they share the whole parse → analyse → freeze
/// prefix and differ only by the round-trip applied.
type ConformanceRoundTripArtifacts =
    {
        /// Codegen from the direct frozen tree.
        Direct: ClrArtifact
        /// Codegen from `thaw (flatten frozen)` (the serialization round-trip).
        ThawRoundTripped: ClrArtifact
        /// Codegen from `toPools (ofPools frozen)` — the columns drained to the DU and
        /// re-derived from it, which is what proves the columns are tree-sufficient now
        /// that the freeze emits them directly.
        PoolRoundTripped: ClrArtifact
    }

/// Produce the round-trip artifacts a frozen-tree gate reconciles. The freeze runs
/// ONCE and `Codegen.compile` runs against the SAME provider / `withCore` project /
/// `defaultManifests` as `compileSource` for every variant, so an artifact differs from
/// `Direct` only by the round-trip it went through. `compileContract` fuses freeze +
/// compile and hides the frozen tree, so this reaches past it.
let compileConformanceDirectAndRoundTripped (assemblyName: string) (input: string) : ConformanceRoundTripArtifacts =
    let project = ProjectInfo.defaults assemblyName
    let provider = ClrSymbolProviders.buildContract defaultManifests
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContextFor project.AssemblyName provider input lexed file

    let frozen = Freeze.run ctx tast
    let thawRoundTripped = FrozenCodec.thaw (FrozenCodec.flatten frozen)
    let poolRoundTripped = TastPools.toPools (TastPools.ofPools frozen)
    let cored = withCore project

    {
        Direct = Codegen.compile provider cored frozen
        ThawRoundTripped = Codegen.compile provider cored thawRoundTripped
        PoolRoundTripped = Codegen.compile provider cored poolRoundTripped
    }

/// The conformance corpus names programs with hyphens (`arith-byte`); an assembly name
/// has to be an identifier the emitted module can carry. Single-sourced (rather than
/// duplicated into the corpus runner and the byte-identity gate) so the gate's digest is
/// provably of the SAME PE the corpus run judges — the two cannot drift apart.
let conformanceAssemblyName (program: string) : string =
    "Conformance_" + program.Replace("-", "_")

/// Like `compileSource` but drives the **self-host** front end
/// (`analyseForSelfHost`): a bare-program `[]` / `::` defaults to the Vesper
/// cons-list, not FSharp.Core's `list` — the same posture a BCL-only package
/// build (`buildPackage`) uses. Needed when a probe mixes `'T list`-annotated
/// state (which resolves to the Vesper list via the `list` abbreviation) with
/// bare `::` / `[]` construction: under the default (FSharp.Core) pipeline the two
/// disagree on the list representation, but a real self-host package resolves both
/// to the Vesper list consistently.
let compileSourceSelfHost (assemblyName: string) (input: string) : ClrArtifact =
    let provider = ClrSymbolProviders.buildContract defaultManifests
    let project = ProjectInfo.defaults assemblyName
    let lexed, file = parseFile input

    let tast =
        Pipeline.analyseForSelfHost project.AssemblyName provider input lexed file

    Codegen.compile provider (withCore project) tast

/// Like `compileSource` but against a caller-supplied `ProjectInfo` (e.g. an
/// on-disk app build via `ProjectInfo.app`). `withCore` injects the compiled
/// `Vesper.Core.dll` unless the project is `Vesper.Core` itself.
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    compileContract defaultManifests project input |> snd

/// Explicit-manifest variant: stand the given layer-1 manifests up at the head of
/// the stack and share that one provider + inline bodies across both phases.
let compileSourceWith (manifestPaths: string list) (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract manifestPaths (ProjectInfo.defaults assemblyName) input

/// Contract-backed compile against the real `Vesper.Core` manifest.
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

// ---- ALC-separable `Vesper.Printf` (differential-testing foundation) ----
// This block makes the runtime handler choice explicit: a driver PE is
// loaded into a dedicated *collectible* ALC whose `Load` override resolves
// `Vesper.Printf` to a CHOSEN copy — the committed C# DLL or the
// `buildPackage`-produced Vesper one — while everything else (`Vesper.Core`,
// `Vesper.List`, FSharp.Core, the BCL) falls through to Default.
//
// Identity unification: the driver's synthesised `IStructuralFormattable.Format`
// takes a `Vesper.IFormatSink`, and the chosen `Vesper.Printf`'s
// `RuntimeFormatState` implements that same Core interface. Both reference
// `Vesper.Core` by SIMPLE NAME, so in the child ALC both resolve (via the null
// fall-through) to the single Default-ALC `vesperCoreDll` — the same runtime
// identity. The Vesper-compiled `Vesper.Printf` was built (`buildPackage`)
// against its own `packageAlc` `Vesper.Core`, but that copy shares
// `vesperCoreDll`'s source (`prim-types-min.fs` + `core-types.fs` +
// `structural-format.fs`), so the surface matches and the simple-name bind is
// sound. (Same for `Vesper.List`.) The driver compile (`compileSource` →
// `withCore`) forces `vesperCoreDll`/`vesperListDll` into Default first, so the
// child ALC's fall-through finds them loaded.

/// The Vesper-compiled `Vesper.Printf.dll` path (built + materialised to disk by
/// the `buildPackage` harness). Forcing the lazy also builds its `Vesper.Core` /
/// `Vesper.List` / `Vesper.Comparison` deps into `packageAlc`.
let private vesperPrintfPath () : string =
    match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
    | Some p -> p
    | None -> failwith "buildPackage Vesper.Printf produced no OutputPath"

/// A collectible ALC that resolves `Vesper.Printf` to a chosen on-disk DLL and
/// delegates everything else to Default (where `Vesper.Core` / `Vesper.List` and
/// the BCL live). Loaded from a byte copy (not a file handle) so the on-disk DLL
/// stays unlocked and the context owns its copy — required for a clean `Unload`.
type private PrintfLoadContext(printfPath: string) as this =
    inherit AssemblyLoadContext("xparsec-printf-diff", isCollectible = true)

    let printf =
        lazy (use ms = new IO.MemoryStream(IO.File.ReadAllBytes printfPath) in this.LoadFromStream ms)

    override _.Load(name: System.Reflection.AssemblyName) : Assembly =
        if name.Name = "Vesper.Printf" then printf.Value else null

/// Create a fresh collectible ALC bound to the Vesper-compiled `Vesper.Printf.dll`,
/// run `run` against it, then unload. The result must hold no `Type`/`Assembly` from
/// the context (return captured stdout / scalars), so `Unload` can collect it.
let withPrintfAlc (run: AssemblyLoadContext -> 'a) : 'a =
    let alc = PrintfLoadContext(vesperPrintfPath ())

    try
        run (alc :> AssemblyLoadContext)
    finally
        alc.Unload()

/// Uniquifies a per-call driver assembly name (Expecto runs in parallel; even
/// across distinct ALCs a unique name keeps failures legible).
let private diffDriverCounter = ref 0

/// Compile a bare driver program (default contract stack + `withCore`) and run
/// its entry point inside `alc`, returning exit code + stdout. The driver's
/// `Vesper.Printf` reference binds to whatever `alc` resolves it to.
let runDriverInAlc (alc: AssemblyLoadContext) (src: string) : int * string =
    let n = Threading.Interlocked.Increment diffDriverCounter
    let _, artifact = compileSource (sprintf "DiffDriver%d" n) src
    use ms = new IO.MemoryStream(Codegen.toBytes artifact)
    let asm = alc.LoadFromStream ms
    runLoadedEntryPoint asm

/// Run `src` through the Vesper-compiled printf handler in a dedicated ALC and
/// return its (CRLF-normalised, trailing-newline-trimmed) stdout, asserting exit 0.
let runsPrintf (src: string) : string =
    withPrintfAlc (fun alc ->
        let exitCode, output = runDriverInAlc alc src
        // Strip CR + trailing newlines only (not all whitespace): a `%5d`
        // right-justify ("   42") carries meaningful LEADING spaces, and a
        // broken `%A` group carries embedded newlines + indent — both must
        // survive so the oracle can pin them.
        let actual = output.Replace("\r", "").TrimEnd('\n')

        if exitCode <> 0 then
            failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

        actual
    )

/// `runsPrintf` plus an assertion that the output equals `expected` (the
/// structural spec oracle) — pins the handler to the spec.
let runsEq (expected: string) (src: string) : unit =
    let actual = runsPrintf src

    if actual <> expected then
        failwithf "expected %A but the handler produced %A for:\n%s" expected actual src

// ---- Drive the `%A` golden oracle on the VESPER engine ------------
// Exercise the *Vesper-compiled* engine (`structural-printer.fs`, including its
// cons-list `Object.ReferenceEquals` cycle scan) by resolving `StructuralPrinter` by
// reflection from the `buildPackage`-produced `Vesper.Printf.dll`. It is loaded into
// a dedicated long-lived (non-collectible) ALC with no `Load` override, so its
// `Vesper.Core` dependency resolves through the runtime's Default fall-through — the
// same value-identity unification `withPrintfAlc` does for drivers. The hand-written `Point`/`Opt` `IStructuralFormattable` impls
// (bound to the test's `Vesper.Core`) and the engine's `RuntimeFormatState`
// (`IFormatSink`) then meet on the single Default `Vesper.Core`, so the engine's
// `value :? IStructuralFormattable` test succeeds across the ALC boundary.

/// The Vesper-compiled `StructuralPrinter::Print(obj, int, int)` bound once.
/// Forcing it builds `Vesper.Printf` (and its deps into Default) via
/// `vesperPrintfPath`, then loads that DLL into its own ALC.
let private vesperStructuralPrintMethod: Lazy<MethodInfo> =
    lazy
        (let path = vesperPrintfPath ()
         // The engine's `RuntimeFormatState` references `Vesper.Core` (the `%A`
         // interfaces) and `Vesper.List` (the cons-list its cycle scan walks). The
         // dedicated ALC below has no `Load` override, so those resolve through the
         // Default fall-through — force both Default-ALC copies first (exactly what a
         // driver compile's `withCore` does for `withPrintfAlc`), or the cross-ALC
         // load throws `FileNotFoundException`.
         vesperCoreDll.Value |> ignore
         vesperListDll.Value |> ignore
         let alc = AssemblyLoadContext("xparsec-structural-printer", isCollectible = false)

         let asm =
             use ms = new IO.MemoryStream(IO.File.ReadAllBytes path)
             alc.LoadFromStream ms

         let sp = asm.GetType("Vesper.StructuralPrinter", true)
         let m = sp.GetMethod("Print", [| typeof<obj>; typeof<int>; typeof<int> |])

         if isNull m then
             failwith "Vesper.StructuralPrinter has no Print(obj, int, int)"

         m)

/// Render `value` through the Vesper-compiled `StructuralPrinter` with an explicit
/// column budget and PrintSize node budget (the `%.NA` mode).
let structuralPrintSized (value: obj) (widthBudget: int) (sizeBudget: int) : string =
    vesperStructuralPrintMethod.Value.Invoke(null, [| value; box widthBudget; box sizeBudget |]) :?> string

/// Render `value` through the Vesper-compiled `StructuralPrinter` at the default
/// node budget (F#'s 10000 — plain `%A`).
let structuralPrint (value: obj) (widthBudget: int) : string =
    structuralPrintSized value widthBudget 10000

/// Compile a STANDALONE `%A` structural-engine source string (defining
/// `Vesper.StructuralPrinter`, depending only on Vesper.Core/List/Comparison) through
/// THIS repo's backend and bind its `Print(obj, int, int)` as a typed `Func` delegate —
/// the real Codegen.Clr-emitted IL, callable with no per-call reflection. Used by the
/// `Codegen.Clr` structural-format benchmark to measure the live engine vs the frozen
/// pre-buffer baseline on the *emitted* output (not the fsc rendering). Same compile
/// path as `buildPackage`; only the source string + assembly name differ, so the live
/// and baseline engines go through an identical backend for an apples-to-apples ratio.
/// Each is loaded into its own dedicated (non-collectible) ALC whose `Vesper.Core` /
/// `Vesper.List` dependencies resolve through the Default fall-through (forced first).
let compileStructuralEngine (asmName: string) (source: string) : Func<obj, int, int, string> =
    vesperCoreDll.Value |> ignore
    vesperListDll.Value |> ignore

    let deps = [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison" ]

    let depDlls =
        deps |> List.choose (fun d -> ((buildPackage d).Value |> snd).OutputPath)

    let provider = ClrSymbolProviders.buildContract (deps |> List.map srcManifest)

    let outDir = tmpDir (sprintf "engine-%s" asmName)
    let outPath = IO.Path.Combine(outDir, asmName + ".dll")

    let project =
        { ProjectInfo.library asmName with
            OutputPath = Some outPath
            References = depDlls
        }

    let lexed, file = parseFile source

    let tast =
        Pipeline.analyseForSelfHost project.AssemblyName provider source lexed file

    let errs =
        tast.Residue.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errs) then
        failwithf
            "compileStructuralEngine %s: %d analysis error(s):\n%s"
            asmName
            (List.length errs)
            (errs |> List.map (fun d -> d.Message) |> String.concat "\n")

    let artifact = Codegen.compile provider project tast
    Codegen.materialise artifact

    let alc =
        AssemblyLoadContext(sprintf "xparsec-engine-%s" asmName, isCollectible = false)

    let asm =
        use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)
        alc.LoadFromStream ms

    let sp = asm.GetType("Vesper.StructuralPrinter", true)
    let m = sp.GetMethod("Print", [| typeof<obj>; typeof<int>; typeof<int> |])

    if isNull m then
        failwithf "compileStructuralEngine %s: no Vesper.StructuralPrinter.Print(obj, int, int)" asmName

    m.CreateDelegate(typeof<Func<obj, int, int, string>>) :?> Func<obj, int, int, string>

/// Compile a `<None Include>` Vesper source FILE (read from disk, `fileName` relative
/// to this test project directory) through this repo's backend against the
/// Vesper.Core / Vesper.List / Vesper.Comparison contract, load it into its own
/// long-lived (non-collectible) ALC, and return the loaded `Assembly` — so the
/// fixture's `obj`-returning nullary functions can be reflected + invoked. The
/// fixture binds the Core-owned `%A` interfaces (`IStructuralFormattable` /
/// `IFormatSink`) and the Vesper cons-list, so `Vesper.Core` / `Vesper.List` are
/// forced into the Default ALC first (exactly as `compileStructuralEngine` does) and
/// the fixture's simple-name references to them resolve through the fall-through — the
/// same value-identity unification the Vesper-compiled `%A` engine relies on, so a
/// fixture value's `IStructuralFormattable` impl and the engine's sink meet on ONE
/// `Vesper.Core`. Front end: `analyseForSelfHost`, so a bare `[]` / `::` is the Vesper
/// cons-list (rendered `[…]` by the engine's `IEnumerable` arm), matching a package build.
let compileFixtureFile (asmName: string) (fileName: string) : Assembly =
    vesperCoreDll.Value |> ignore
    vesperListDll.Value |> ignore

    let deps = [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison" ]

    let depDlls =
        deps |> List.choose (fun d -> ((buildPackage d).Value |> snd).OutputPath)

    let provider = ClrSymbolProviders.buildContract (deps |> List.map srcManifest)

    let outDir = tmpDir (sprintf "fixture-%s" asmName)
    let outPath = IO.Path.Combine(outDir, asmName + ".dll")

    let project =
        { ProjectInfo.library asmName with
            OutputPath = Some outPath
            References = depDlls
        }

    let source = IO.File.ReadAllText(IO.Path.Combine(__SOURCE_DIRECTORY__, fileName))

    let lexed, file = parseFile source

    let tast =
        Pipeline.analyseForSelfHost project.AssemblyName provider source lexed file

    let errs =
        tast.Residue.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errs) then
        failwithf
            "compileFixtureFile %s: %d analysis error(s):\n%s"
            asmName
            (List.length errs)
            (errs |> List.map (fun d -> d.Message) |> String.concat "\n")

    let artifact = Codegen.compile provider project tast
    Codegen.materialise artifact

    let alc =
        AssemblyLoadContext(sprintf "xparsec-fixture-%s" asmName, isCollectible = false)

    use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)
    alc.LoadFromStream ms

// ---- Layer 1 behavioral corpus helpers --------------------------------------
// The one-liners the suite was missing:
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

/// `runs` against the **self-host** front end (`compileSourceSelfHost`): bare
/// `[]` / `::` default to the Vesper cons-list, matching a BCL-only package build.
/// Use for probes that mix `'T list`-typed state with bare cons construction.
let runsSelfHost (expected: string) (src: string) : unit =
    let artifact = compileSourceSelfHost "Layer1SelfHost" src
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsSelfHost` for a multi-line expected block (joined with "\n").
let runsSelfHostLines (expected: string list) (src: string) : unit =
    runsSelfHost (String.concat "\n" expected) src

// ---- Externalised program sources (`data/*.fs`) ------------------------------
// The struct / self-host probes' to-be-compiled programs live as standalone `.fs`
// files under `data/`, read as TEXT and compiled through this repo's backend (never
// fsc — they use Vesper self-host primitives, so they are `<None Include>`, not
// `<Compile>`). A `//#include <frag>.fs` line splices in a shared fragment file, so
// the self-hosted `%A` sink protocol (`_layout-core.fs` / `_frame-sem-types.fs` /
// `_sink-frame-plumbing.fs` / `_sink-finish-protocol.fs`) stays single-sourced across
// the layout / structural-format probes rather than hand-copied into each program.

let private dataDir = IO.Path.Combine(__SOURCE_DIRECTORY__, "data")

/// Read `data/<name>.fs`, expanding each `//#include <file>` line (resolved against
/// `data/`, recursively) into the referenced fragment's lines. Fragments are authored
/// at column 0 and re-indented to the directive's own column, so a `member`-block
/// fragment splices cleanly at any nesting (`    //#include …` lands its lines at
/// 4-space indent inside a type body). A missing target or an include cycle fails with
/// a pointed message rather than an opaque `FileNotFoundException` / stack overflow.
let dataSource (name: string) : string =
    let includePrefix = "//#include "

    let rec expand (indent: string) (ancestors: string list) (fileName: string) : string list =
        if List.contains fileName ancestors then
            failwithf "data source %s: include cycle %s -> %s" name (String.concat " -> " (List.rev ancestors)) fileName

        let path = IO.Path.Combine(dataDir, fileName)

        if not (IO.File.Exists path) then
            failwithf "data source %s: included file not found: %s" name fileName

        IO.File.ReadAllLines path
        |> Array.toList
        |> List.collect (fun line ->
            let trimmed = line.TrimStart()

            if trimmed.StartsWith includePrefix then
                // The directive's own leading whitespace shifts the whole fragment,
                // so a `member`-block fragment authored at column 0 lands at the
                // directive's indent inside the enclosing type body.
                let directiveIndent = indent + line.Substring(0, line.Length - trimmed.Length)
                expand directiveIndent (fileName :: ancestors) (trimmed.Substring(includePrefix.Length).Trim())
            elif trimmed.Length = 0 then
                [ line ] // keep blank / whitespace-only lines un-indented
            else
                [ indent + line ]
        )

    expand "" [] (name + ".fs") |> String.concat "\n"

/// `compileSource` with the program read from `data/<name>.fs`; the file's base name
/// doubles as the assembly name.
let compileSourceData (name: string) : TastFile * ClrArtifact = compileSource name (dataSource name)

/// `runsLines` with the program read from `data/<name>.fs`.
let runsDataLines (expected: string list) (name: string) : unit = runsLines expected (dataSource name)

/// `runsSelfHostLines` with the program read from `data/<name>.fs`.
let runsSelfHostDataLines (expected: string list) (name: string) : unit =
    runsSelfHostLines expected (dataSource name)

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

// ---- Declarative package harness (one core, many wrappers) -------------------
// The Option / Result / Choice / Array / Seq / Set harnesses below were seven
// verbatim copies of ONE recipe: stack a package's contract on the default
// manifests, append its DLL to `References`, parse/analyse/compile, then run (or
// just analyse). They are now thin wrappers over a single DECLARATIVE core: name
// the Vesper packages a snippet links against, and `buildPackage` +
// `transitivePackages` derive the contract stack, the reference DLLs, and the
// whole `depends-on` graph (built once + loaded into `packageAlc`) — the
// `runsSet` model generalised to an arbitrary package set, with the default stack
// (Core/List/Comparison/Printf) always unioned in so a driver can use the
// operators and `printfn`.

/// The packages every driver implicitly links — the package-name spelling of
/// `defaultManifests` (language core, cons-list, ordering operators, printf). A
/// declarative reference set is unioned with these.
let private defaultPackageNames =
    [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison"; "Vesper.Printf" ]

/// Transitive `depends-on` closure of `roots`: dependencies before dependents,
/// deduplicated, each root after its deps. Drives both the contract stack and the
/// `References` DLL list. Reads each package's `depends-on` off its manifest.
let private transitivePackages (roots: string list) : string list =
    let acc = System.Collections.Generic.List<string>()

    let rec go (pkg: string) =
        if not (acc.Contains pkg) then
            match ReferencedProject.loadManifest (srcManifest pkg) with
            | Result.Ok m ->
                m.DependsOn |> List.iter go

                if not (acc.Contains pkg) then
                    acc.Add pkg
            | Result.Error e -> failwithf "transitivePackages %s: %s" pkg e

    roots |> List.iter go
    List.ofSeq acc

/// Uniquifies a per-call driver assembly name (Expecto runs tests in parallel and
/// `packageAlc` is process-persistent, so two identically-named loads would
/// collide on identity).
let private driverCounter = ref 0

/// Compile `src` as a bare program against the declarative package set `packages`
/// (unioned with the default Core/List/Comparison/Printf stack). Every package in
/// the transitive `depends-on` closure is built once + registered in `packageAlc`;
/// its `.fsi` joins the contract stack and its DLL the `References`. Returns the
/// `ClrArtifact` (compile only — the run path adds printf + the `packageAlc`
/// load). Front end: `analyseFor` — a driver is a FSharp.Core-front-end consumer
/// of the packages, exactly as the hand-written `runsX` harnesses were.
let compilePackages (packages: string list) (src: string) : ClrArtifact =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let depDlls =
        allPackages |> List.choose (fun p -> ((buildPackage p).Value |> snd).OutputPath)

    let provider =
        ClrSymbolProviders.buildContract (allPackages |> List.map srcManifest)

    let n = System.Threading.Interlocked.Increment driverCounter

    let project =
        { ProjectInfo.defaults (sprintf "PkgDriver%d" n) with
            References = depDlls
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file

    let analysisErrors =
        tast.Residue.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty analysisErrors) then
        failwithf
            "compilePackages %A: %d analysis error(s) for:\n%s\n--- errors ---\n%s"
            packages
            (List.length analysisErrors)
            src
            (analysisErrors |> List.map (fun d -> d.Message) |> String.concat "\n")

    Codegen.compile provider project tast

/// The Vesper-compiled `Vesper.Printf`, loaded + registered in `packageAlc` once
/// (built against the packageAlc `Vesper.Core`/`Vesper.List`). `buildPackage`
/// loads Printf into a *throwaway* ALC (so it can't shadow a host C# peer) and
/// never registers it, so a driver run in `packageAlc` would otherwise resolve
/// printf via the Default fall-through to a DIFFERENT `Vesper.Core` identity than
/// the one its package types implement — breaking a `%A` of an external Vesper
/// union (`value :? Vesper.IStructuralFormattable` then tests the wrong Core's
/// interface). Registering Printf in `packageAlc` puts the driver, its package
/// types, and printf on ONE `Vesper.Core` identity — the packageAlc analogue of
/// the Default-ALC unification the old per-package harnesses got for free.
let private packageAlcPrintf: Lazy<unit> =
    lazy
        (let path =
            match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
            | Some p -> p
            | None -> failwith "buildPackage Vesper.Printf produced no OutputPath"

         use ms = new IO.MemoryStream(IO.File.ReadAllBytes path)
         packageAlc.Register("Vesper.Printf", packageAlc.LoadFromStream ms))

/// Compile `src` against `packages`, run its entry point inside `packageAlc` (so
/// the driver, every `Vesper.*` dependency, and printf all resolve off the build
/// registry under ONE `Vesper.Core` identity), and return (exitCode, stdout) plus
/// the emitted bytes — the bytes let a caller assert on the emitted IL
/// (constrained./no-box dispatch) in the same pass as the run.
let runPackagesInspect (packages: string list) (src: string) : (int * string) * byte[] =
    packageAlcPrintf.Value
    let artifact = compilePackages packages src
    let bytes = Codegen.toBytes artifact
    use ms = new IO.MemoryStream(bytes)
    let asm = packageAlc.LoadFromStream ms
    runLoadedEntryPoint asm, bytes

/// `runPackagesInspect` without the bytes — compile + run, returning
/// (exitCode, stdout).
let runPackages (packages: string list) (src: string) : int * string = runPackagesInspect packages src |> fst

/// Compile + run `src` against `packages`; assert exit 0 and trimmed,
/// CRLF-normalised stdout equals `expected`. The declarative generalisation of
/// `runsOption` / `runsResult` / … / `runsSet`.
let runsPackages (packages: string list) (expected: string) (src: string) : unit =
    let exitCode, output = runPackages packages src
    let actual = output.Replace("\r", "").Trim()

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// `runsPackages` for a multi-line expected block (joined with "\n").
let runsPackagesLines (packages: string list) (expected: string list) (src: string) : unit =
    runsPackages packages (String.concat "\n" expected) src

/// Analyse `src` against `packages` (default stack + the set), no codegen, and
/// return the error-severity diagnostics. Backs `typeChecksPackages` /
/// `failsWithPackages`.
let private analysePackagesErrors (packages: string list) (src: string) : Diagnostic list =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let provider =
        ClrSymbolProviders.buildContract (allPackages |> List.map srcManifest)

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider src lexed file
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

/// Analyse `src` against `packages`; assert NO error diagnostics, without running
/// it — the front-end-only probe.
let typeChecksPackages (packages: string list) (src: string) : unit =
    match analysePackagesErrors packages src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// Analyse `src` against `packages`; assert an error diagnostic whose message
/// contains `fragment`.
let failsWithPackages (packages: string list) (fragment: string) (src: string) : unit =
    match analysePackagesErrors packages src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

// ---- Per-package wrappers over the declarative core --------------------------
// Each `Vesper.X` package's old bespoke harness collapses to a one-liner naming
// the package(s). New package? Add a wrapper line — no DLL lazy, no contract
// plumbing.

/// Vesper.Option — the option type + `Option` module (counterpart of `runs`).
let runsOption (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Option" ] expected src

/// `runsOption` for a multi-line expected block.
let runsOptionLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Option" ] expected src

/// Analyse `src` through the default contract stack (no codegen) and return the
/// error-severity diagnostics — the front-end-only half of the corpus.
/// `Pipeline.analyse` collects diagnostics rather than throwing, so both
/// `failsWith` and `typeChecks` read off the returned `TastFile.Diagnostics`.
let private analyseErrors (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultManifests
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

/// Vesper.Option front-end-only probes (`typeChecks` / `failsWith` twins).
let typeChecksOption (src: string) : unit =
    typeChecksPackages [ "Vesper.Option" ] src

let failsWithOption (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Option" ] fragment src

// ---- Vesper.Result wrappers --------------------------------------------------

/// Compile a `Vesper.Result` consumer through the full backend and return the
/// `ClrArtifact` (no run) — for assertions on `FSharpCoreDependencies`, e.g. that
/// a `%A` of an external Vesper union lowers on the structural engine (the use-set
/// stays clear of `PrintfModule.PrintFormatLine`) rather than the cold path.
let compileResultArtifact (src: string) : ClrArtifact = compilePackages [ "Vesper.Result" ] src

/// Vesper.Result — the result type + `Result` module (counterpart of `runsOption`).
let runsResult (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Result" ] expected src

let runsResultLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Result" ] expected src

let typeChecksResult (src: string) : unit =
    typeChecksPackages [ "Vesper.Result" ] src

let failsWithResult (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Result" ] fragment src

// ---- Vesper.Choice wrappers --------------------------------------------------
// Choice is a pure-data struct union with NO module (its sole consumer `set.fs`
// uses only constructors + pattern matching), so `runsChoice` exercises
// construction (Layer B) + `match` (Layer C), not a module call.

let runsChoice (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Choice" ] expected src

let runsChoiceLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Choice" ] expected src

let typeChecksChoice (src: string) : unit =
    typeChecksPackages [ "Vesper.Choice" ] src

let failsWithChoice (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Choice" ] fragment src

// ---- Vesper.Array wrappers ---------------------------------------------------
// `Vesper.Array` is BCL-only: `arr.[i]`/`arr.Length`/`Array.zeroCreate` lower to
// the `ldelem`/`ldlen`/`newarr` IL intrinsics. (Array *literals* `[| … |]` in a
// driver still route through FSharp.Core's `ArrayModule.OfList`, harmless
// in-process — rows that must stay BCL-only build via `zeroCreate`.)

let runsArray (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Array" ] expected src

let runsArrayLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Array" ] expected src

let typeChecksArray (src: string) : unit =
    typeChecksPackages [ "Vesper.Array" ] src

// ---- Vesper.Seq wrappers -----------------------------------------------------
// A driver's `seq<'T>` source is `System.Linq.Enumerable.Range(start, count)` (a
// real BCL `IEnumerable<int>`) — the Vesper cons-list declares `IEnumerable<'T>`
// in its `.fsi` but does not implement it in `list.fs`, so a list value is not a
// runtime seq. `Range` sidesteps that entirely.

let runsSeq (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Seq" ] expected src

let runsSeqLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Seq" ] expected src

let typeChecksSeq (src: string) : unit = typeChecksPackages [ "Vesper.Seq" ] src

// ---- Vesper.Set wrappers -----------------------------------------------------
// `Vesper.Set` (the immutable AVL-tree set + the `Set` module) is the capstone
// self-host package, with eight transitive `Vesper.*` deps. The declarative core
// already builds the whole graph into `packageAlc` and runs the driver there, so
// the once-bespoke routing is just the default behaviour now.
//
// NOTE on driver shape: HOF arguments (`Set.fold`/`partition`'s folder) are
// written *curried* (`fun s -> fun x -> …`) per the same Elaborate multi-arg-lambda
// posture the struct-seq pipeline documents.

let runsSet (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Set" ] expected src

let runsSetLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Set" ] expected src

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

/// The base-type full name (`Namespace.Name`) of the first type-def whose simple
/// name satisfies `nameMatches` — resolving the `BaseType` handle through either a
/// `TypeReference` (BCL, e.g. `System.ValueType` / `System.Object`) or a sibling
/// `TypeDefinition`. `ValueNone` if no type matches or the base handle is nil.
/// Distinguishes a value-type (`System.ValueType`) closure from a heap one
/// (`System.Object`).
let peTypeBaseTypeName (bytes: byte[]) (nameMatches: string -> bool) : string voption =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    let nameOf (ns: string) (n: string) =
        if System.String.IsNullOrEmpty ns then
            n
        else
            sprintf "%s.%s" ns n

    md.TypeDefinitions
    |> Seq.tryPick (fun tdh ->
        let td = md.GetTypeDefinition tdh

        if nameMatches (md.GetString td.Name) then
            let bt = td.BaseType

            if bt.IsNil then
                Some ValueNone
            else
                match bt.Kind with
                | HandleKind.TypeReference ->
                    let r = md.GetTypeReference(TypeReferenceHandle.op_Explicit bt)
                    Some(ValueSome(nameOf (md.GetString r.Namespace) (md.GetString r.Name)))
                | HandleKind.TypeDefinition ->
                    let d = md.GetTypeDefinition(TypeDefinitionHandle.op_Explicit bt)
                    Some(ValueSome(nameOf (md.GetString d.Namespace) (md.GetString d.Name)))
                | _ -> Some ValueNone
        else
            None
    )
    |> Option.defaultValue ValueNone

/// The base-type SIMPLE name (`ValueType` / `Object`, not the qualified form)
/// of every synthesised `<closure>$…` type-def in the PE — one entry per closure,
/// so a struct-seq test can assert "all source-lambda closures are value types"
/// (`= "ValueType"`). `<none>` for a closure whose base handle isn't a
/// `TypeReference` (it never is for a real closure; surfaced rather than dropped
/// so an unexpected shape fails loudly). Unlike `peTypeBaseTypeName` this lists
/// ALL closure type-defs, not just the first match.
let peClosureBaseTypeNames (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.choose (fun tdh ->
        let td = md.GetTypeDefinition tdh

        if (md.GetString td.Name).StartsWith "<closure>$" then
            match td.BaseType.Kind with
            | HandleKind.TypeReference ->
                Some(md.GetString (md.GetTypeReference(TypeReferenceHandle.op_Explicit td.BaseType)).Name)
            | _ -> Some "<none>"
        else
            None
    )
    |> Seq.toList

/// List every method-def's `(declaringType, methodName)` in the PE, the declaring type
/// named `Namespace.Name`. For the full CLR spelling of a NESTED type (`Ns.Outer+Inner`)
/// and the rows each type's range claims, use `MetadataStructure.emittedTypes`.
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
/// Raw IL bytes of the first method on `declaringType` whose name satisfies
/// `nameMatches`. Use when the emitted method name is synthetic (a holder-less
/// top-level function lands on "Program" as `fn$<n>`) so an exact name can't be
/// pinned. Throws if no matching method is found.
let peMethodIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[] =
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
                |> Seq.tryFind (fun mdh -> nameMatches (md.GetString(md.GetMethodDefinition(mdh).Name)))
            else
                None
        )

    match methodHandle with
    | None -> failwithf "peMethodIlWhere: no matching method on type '%s'" declaringType
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

/// Like `peMethodIlWhere` but returns the IL of EVERY method on `declaringType`
/// whose name matches — for when several methods share a synthetic naming scheme
/// (`fn$<n>` for top-level functions) and the caller picks the right one by
/// inspecting the IL (e.g. "the one containing a `constrained.` prefix").
let peMethodsIlWhere (bytes: byte[]) (declaringType: string) (nameMatches: string -> bool) : byte[][] =
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

    [|
        for tdh in md.TypeDefinitions do
            let td = md.GetTypeDefinition tdh

            if typeMatches td then
                for mdh in td.GetMethods() do
                    let m = md.GetMethodDefinition mdh

                    if nameMatches (md.GetString m.Name) then
                        if m.RelativeVirtualAddress = 0 then
                            yield [||]
                        else
                            let body = peReader.GetMethodBody m.RelativeVirtualAddress
                            let ilReader = body.GetILReader()
                            let buf = Array.zeroCreate ilReader.RemainingBytes
                            ilReader.ReadBytes(ilReader.RemainingBytes, buf, 0)
                            yield buf
    |]

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
