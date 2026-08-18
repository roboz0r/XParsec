module XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

open System
open System.Reflection
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests

// `SemType`'s nominal cases carry a `SymbolKey`; these shadow the constructors and
// project the name back out, so tests construct and match by string name.
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

/// Project an `EqArray<'T>` as a `'T list` inside a pattern match, so a test arm can be a
/// list literal: `| EqList [ TDecl.Let _ ] -> …`.
let inline (|EqList|) (xs: EqArray<'T>) : 'T list = EqArray.toList xs

/// `project` as the compiling identity the pipeline takes; this suite always compiles clr.
let compilingClr (project: ProjectInfo) : CompilingAssembly =
    {
        Name = project.AssemblyName
        Target = Target.Clr
    }

/// A frozen file's declarations as pool handles with the specialization graph expanded,
/// which is the representation the backend starts from. Until this runs, an inline body
/// sits behind an edge rather than in the tree.
let pooledDecls (frozen: FrozenPools) : TastAccessor.DeclId list =
    let pool = TastPoolBuilder.openOver frozen
    (InlineExpand.expand pool (TastAccessor.roots pool |> List.ofArray)).Decls

/// The lambda a specialization entry binds, read out of the file's own table.
let specializationValue (tast: TastFile) (spec: SpecializationId) : TExpr =
    let (SpecializationId i) = spec

    match tast.Specializations.[i].Decl with
    | TDecl.Let(_, value, _, _) -> value
    | other -> failwithf "a specialization entry is a `TDecl.Let` of lambdas; got %A" other

/// Read THROUGH an `InlineCall` edge to the body it points to; the identity on anything else.
/// A resolved inline body sits in the specialization table, not spliced into the consumer.
let rec throughEdge (tast: TastFile) (e: TExpr) : TExpr =
    match e with
    | TExpr.InlineCall(spec = spec) -> throughEdge tast (specializationValue tast spec)
    | _ -> e

/// Run `it` over `e` and, transitively, over every specialization entry its edges name.
let rec iterThroughEdges (it: TastWalk.Iter) (tast: TastFile) (e: TExpr) : unit =
    TastWalk.iterExpr
        { it with
            VisitExpr =
                fun m n ->
                    let descend = it.VisitExpr m n

                    match n with
                    | TExpr.InlineCall(spec = spec) -> iterThroughEdges it tast (specializationValue tast spec)
                    | _ -> ()

                    descend
        }
        e

/// Run `it` over every expression the file carries: its declarations AND its
/// specialization entries. Walking `Decls` alone misses the entries.
let iterFileExprs (it: TastWalk.Iter) (tast: TastFile) : unit =
    let ofDecl (d: TDecl) =
        match d with
        | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
        | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
        | TDecl.Type _ -> ()

    for d in tast.Decls do
        ofDecl d

    for entry in tast.Specializations do
        ofDecl entry.Decl

/// Lex + parse a source string; a script fragment wraps as `AnonymousModule`. A source that
/// parses only because recovery patched it raises here, as it does in the driver, rather
/// than being analysed as though it had been written that way.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parseUnrecovered Set.empty input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed -> parsed.Lexed, parsed.File

/// `<repo-root>/tmp/<name>`, created, where the repo root is the directory holding
/// `claude_tools.cmd`. Artifacts stay inspectable rather than landing in the OS temp dir.
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

/// `src/<pkg>` — the package DIRECTORY. The CLR backend resolves it to `manifest.clr.toml`;
/// a test that wants another target's collection asks for it by target, not by path.
let srcPackage (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg)

let vesperCorePackage: string = srcPackage "Vesper.Core"

/// The last segment of a `depends-on` path (`"../Vesper.Core"` ⇒ `"Vesper.Core"`), which is how
/// this harness keys packages under `src/`.
let dependencyName (entry: string) : string =
    IO.Path.GetFileName(IO.Path.TrimEndingDirectorySeparator entry)

/// A multi-file driver's anchored diagnostics as `path: message`, one per line.
let private anchoredDiagText (diags: AssemblyFiles.AnchoredDiagnostic list) : string =
    diags
    |> List.map (fun d -> sprintf "%s: %s" d.Path.Name d.Diagnostic.Message)
    |> String.concat "\n"

/// The contract, GATED, so a test that miswires its packages fails with the contract error
/// and not an unresolved name three files later.
let private gatedContract
    (label: string)
    (contract: PackageProviders.AnalyzedManifest)
    : PackageProviders.AnalyzedManifest =
    match PackageProviders.AnalyzedManifest.gate contract with
    | Ok contract -> contract
    | Error ds -> failwithf "%s: %d contract error(s):\n%s" label (List.length ds) (anchoredDiagText ds)

/// The self-package contract, GATED.
let private gatedContractForSelf
    (label: string)
    (selfPackage: string)
    (packageDirs: string list)
    : PackageProviders.AnalyzedManifest =
    gatedContract label (ClrSymbolProviders.contractForSelf (Some selfPackage) packageDirs)

/// The `.fs` files a package's CLR manifest lists, in manifest order, relative to the
/// package directory.
let manifestImplFiles (package: string) : string list =
    ReferencedProject.resolveManifest Target.Clr package
    |> Result.bind ReferencedProject.loadManifest
    |> PackageFaults.okOrFail (sprintf "manifestImplFiles %s" package)
    |> ReferencedProject.implementationFiles

/// Compile `Vesper.Core.dll` from its manifest's units, load it into the *Default*
/// `AssemblyLoadContext`, and return its path. *Default* because a PE loaded into a fresh
/// context resolves `Vesper.Fun\`2` / `Vesper.Ref\`1` through that context's fallback to it.
let vesperCoreDll: Lazy<string> =
    lazy
        (let outDir = tmpDir "vesper-core"
         let corePath = IO.Path.Combine(outDir, "Vesper.Core.dll")

         let project =
             { ProjectInfo.library "Vesper.Core" with
                 OutputPath = Some corePath
             }

         // Each unit the manifest lists is analysed as its own file against the composed
         // prior-file views, so a primitive repr (`string`, …) resolves from Core's own `.fs`.
         let files =
             ReferencedProject.resolveManifest Target.Clr vesperCorePackage
             |> Result.bind PackageUnits.ofManifest
             |> PackageFaults.okOrFail "vesperCoreDll"

         // Core defines its own primitives, so it references nothing and declares ITSELF as
         // the self manifest. That seeds the platform metadata with its own `{ platform -> canon }`
         // axis, so a BCL signature presents `System.String` as `Vesper.string` here too.
         let contract = gatedContractForSelf "vesperCoreDll" vesperCorePackage []

         let artifact =
             match ClrDriver.compileAssemblyWith [] contract.Provider project files with
             | Ok artifact -> artifact
             | Error diags ->
                 failwithf "vesperCoreDll: %d analysis error(s):\n%s" (List.length diags) (anchoredDiagText diags)

         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath corePath |> ignore
         corePath)

/// Compile `src/Vesper.List/list.fs` (the `Vesper.Collections.List\`1` cons-list plus
/// `ListModule::fold`), load it into the *Default* `AssemblyLoadContext`, and return its
/// path. `fold`'s folder is a `Vesper.Fun`, so it references `Vesper.Core` and nothing else.
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
         // A `[1; 2; 3]` consumer literal binds to this list by ARITY (nullary terminator
         // + binary cons), not by case name. `list.fs` calls `failwith`, an inline operator
         // in the Vesper.Core contract, so that contract must be in the stack to inline it.
         let provider = ClrSymbolProviders.buildContract [ vesperCorePackage ]
         let lexed, file = parseFile src

         let tast =
             Pipeline.analyseFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath listPath |> ignore
         listPath)

let vesperListPackage: string = srcPackage "Vesper.List"

let vesperComparisonPackage: string = srcPackage "Vesper.Comparison"

/// `src/Vesper.Printf` — `printf` / `printfn` / `sprintf` as an
/// `[<AutoOpen>] module Printf` contract.
let vesperPrintfPackage: string = srcPackage "Vesper.Printf"

/// The default contract stack. An operator emits from its `.fs` implementation body, spliced
/// whether applied (`1 + 2`) or used as a value (`List.fold (+) 0 xs`, eta-reified first).
let defaultPackages: string list =
    [
        vesperCorePackage
        vesperListPackage
        vesperComparisonPackage
        vesperPrintfPackage
    ]

/// Front-end a program to a (SemType) `TastFile` through the default contract stack.
let analyse (input: string) : TastFile =
    let lexed, file = parseFile input

    Pipeline.analyseSem (ClrSymbolProviders.buildContract defaultPackages) (Hashing.originSourceOfText lexed) file

/// `analyse`, keeping the `PassContext`. `Freeze.run` reads `ctx.Bindings.Scheme` for the
/// bound variable a residual typar root belongs to; the TAST alone does not carry it.
let analyseWithCtx (input: string) : PassContext * TastFile =
    let lexed, file = parseFile input

    Pipeline.analyseSemWithContext
        (ClrSymbolProviders.buildContract defaultPackages)
        (Hashing.originSourceOfText lexed)
        file

/// Load context for the package-build harness. `Load` resolves a sibling `Vesper.*`
/// package from the registry below, so a package binds against THIS harness's copy of its
/// dependencies; everything else returns `null` and falls through to Default.
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

/// Compile `src/<package>/`'s `impl` files in manifest order, resolving `depends-on`
/// recursively: each dependency is built + loaded first, its DLL joins `References` and its
/// manifest the contract stack. Returns the loaded `Assembly` and the `ClrArtifact`.
let rec buildPackage (package: string) : Lazy<Assembly * ClrArtifact> =
    packageBuildCache.GetOrAdd(
        package,
        fun pkg ->
            lazy
                (let manifestPath =
                    ReferencedProject.resolveManifest Target.Clr (srcPackage pkg)
                    |> PackageFaults.okOrFail (sprintf "buildPackage %s" pkg)

                 let manifest =
                     ReferencedProject.loadManifest manifestPath
                     |> PackageFaults.okOrFail (sprintf "buildPackage %s" pkg)

                 // Read ONCE: the conformance gate and the unit list below both work off these
                 // trees, and off the pairing taken with them, so no file of the package is
                 // parsed or paired twice.
                 let parsedPackage = PackageSource.readPackage manifest

                 // `.fsi`↔`.fs` conformance gates the build: a signature binding with no
                 // implementation is an error.
                 match ConformancePass.enforce (ConformancePass.check parsedPackage) with
                 | [] -> ()
                 | ds ->
                     failwithf
                         "buildPackage %s: %d conformance error(s):\n%s"
                         pkg
                         (List.length ds)
                         (ds |> List.map (fun d -> d.Message) |> String.concat "\n")

                 // Force each dependency's build first: that registers it in `packageAlc`,
                 // so this package resolves against it at load time. Its DLL goes to
                 // `References` (the emit-time AssemblyRef), its manifest to the provider.
                 let depNames = manifest.DependsOn |> List.map dependencyName

                 let depArtifacts = depNames |> List.map (fun d -> (buildPackage d).Value |> snd)

                 let depDlls = depArtifacts |> List.choose (fun art -> art.OutputPath)
                 let depManifests = depNames |> List.map srcPackage

                 // The package declares ITSELF as self, so a BCL signature presents the primitives
                 // this compilation declares: `prim-types-string.clr.fs`'s `String.Concat(x, y)`
                 // takes two `Vesper.string`s and must still find the `(String, String)` overload.
                 // The self entry folds off the trees read above, not a second read.
                 let contract =
                     gatedContract
                         (sprintf "buildPackage %s" pkg)
                         (ClrSymbolProviders.contractForSelfParsed (srcPackage pkg) depManifests parsedPackage)

                 // Self-host front end, so a bare `[]` / `::` in a BCL-only package defaults
                 // to the Vesper cons-list rather than FSharp.Core's. The seam returns `Error`
                 // on any error-severity diagnostic instead of emitting a degraded DLL.
                 let files = PackageUnits.ofPackage parsedPackage

                 let outDir = tmpDir (sprintf "pkg-%s" pkg)
                 let outPath = IO.Path.Combine(outDir, manifest.Name + ".dll")

                 let project =
                     { ProjectInfo.library manifest.Name with
                         OutputPath = Some outPath
                         References = depDlls
                     }

                 let artifact =
                     match ClrDriver.compileAssemblyWith [] contract.Provider project files with
                     | Ok artifact -> artifact
                     | Error diags ->
                         failwithf
                             "buildPackage %s: %d analysis error(s):\n%s"
                             pkg
                             (List.length diags)
                             (anchoredDiagText diags)

                 Codegen.materialise artifact

                 use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)

                 // A package with no `impl` files carries no runtime types, and `Vesper.Printf`
                 // is loaded into Default separately, so registering a second copy here would
                 // bind a driver's `Vesper.Formatter` to the wrong one. Throwaway ALC for both.
                 if List.isEmpty files || manifest.Name = "Vesper.Printf" then
                     let throwaway = AssemblyLoadContext("xparsec-contract-only", isCollectible = true)

                     throwaway.LoadFromStream ms, artifact
                 else
                     let asm = packageAlc.LoadFromStream ms
                     packageAlc.Register(manifest.Name, asm)
                     asm, artifact)
    )

/// The Vesper-compiled `Vesper.Printf.dll` loaded into the *Default* `AssemblyLoadContext`
/// as the only copy there, so a fresh-ALC driver resolves it by fall-through, onto the
/// `vesperCoreDll` / `vesperListDll` copies forced first.
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

/// Add `Vesper.Core.dll` / `Vesper.List.dll` / `Vesper.Printf.dll` to `References` so a
/// program's function values, list literals and `printf` calls resolve, but not when the
/// project IS that package. An unused reference emits no `AssemblyRef`.
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

/// Build the symbol stack + its cross-package inline bodies once (cached per manifest set)
/// and run both phases against it: an `External(name)` whose body lives in a referenced
/// `.fs` splices in pre-freeze. `[]` manifests ⇒ the .NET metadata reader alone.
let private compileContract
    (manifestPaths: string list)
    (project: ProjectInfo)
    (input: string)
    : TastFile * ClrArtifact =
    let provider = ClrSymbolProviders.buildContract manifestPaths
    let lexed, file = parseFile input
    // Callers assert on the SemType tree; codegen takes the frozen one.
    let ctx, tast =
        Pipeline.analyseSemWithContextFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

    let artifact = Codegen.compile provider (withCore project) (Freeze.run ctx tast)
    tast, artifact

/// The default compile path: `int` / `hash` / the operators all resolve from the
/// `Vesper.Core` contract.
let compileSource (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract defaultPackages (ProjectInfo.defaults assemblyName) input

/// The CLR artifacts a frozen-tree round-trip must reconcile against the DIRECT codegen.
/// All three share one parse → analyse → freeze prefix and differ only by the round-trip.
type ConformanceRoundTripArtifacts =
    {
        /// Codegen from the direct frozen tree.
        Direct: ClrArtifact
        /// Codegen from `thaw (flatten frozen)` (the serialization round-trip).
        ThawRoundTripped: ClrArtifact
        /// Codegen from `rePool (ofPools frozen)` (the columns unpooled to the DU and
        /// re-derived from it), proving the columns are tree-sufficient.
        PoolRoundTripped: ClrArtifact
    }

/// Produce the round-trip artifacts a frozen-tree gate reconciles. One freeze, one provider
/// and one project, so an artifact differs from `Direct` only by its round-trip.
let compileConformanceDirectAndRoundTripped (assemblyName: string) (input: string) : ConformanceRoundTripArtifacts =
    let project = ProjectInfo.defaults assemblyName
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContextFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

    let frozen = Freeze.run ctx tast
    let thawRoundTripped = FrozenCodec.thaw (FrozenCodec.flatten frozen)
    let poolRoundTripped = TastPools.rePool frozen (TastUnpool.ofPools frozen)
    let cored = withCore project

    {
        Direct = Codegen.compile provider cored frozen
        ThawRoundTripped = Codegen.compile provider cored thawRoundTripped
        PoolRoundTripped = Codegen.compile provider cored poolRoundTripped
    }

/// The conformance corpus names programs with hyphens (`arith-byte`); an assembly name
/// must be an identifier the emitted module can carry.
let conformanceAssemblyName (program: string) : string =
    "Conformance_" + program.Replace("-", "_")

/// `compileSource` against a caller-supplied `ProjectInfo` (e.g. an on-disk app build via
/// `ProjectInfo.app`).
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    compileContract defaultPackages project input |> snd

/// `compileSource` against an explicit manifest stack instead of `defaultPackages`.
let compileSourceWith (manifestPaths: string list) (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract manifestPaths (ProjectInfo.defaults assemblyName) input

/// Contract-backed compile against the real `Vesper.Core` manifest.
let compileSourceContract (assemblyName: string) (input: string) : TastFile * ClrArtifact =
    compileContract [ vesperCorePackage ] (ProjectInfo.defaults assemblyName) input

/// Run a materialised app out-of-process via the `dotnet` host. On a non-zero exit stderr
/// is appended, so a host failure (missing runtimeconfig, unresolved reference) is visible.
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

/// Load emitted PE bytes into a FRESH `AssemblyLoadContext`. Loading the same bytes twice
/// gives two assemblies, and mixing them throws "Object of type X cannot be converted to
/// type X", so reflect every member through the ONE `Assembly` this returns.
let loadAssembly (bytes: byte[]) : Assembly =
    let alc = AssemblyLoadContext("xparsec-codegen-test", isCollectible = true)
    use ms = new IO.MemoryStream(bytes)
    alc.LoadFromStream ms

/// Serialises the `Console.Out` capture below: Expecto runs tests in parallel and
/// `Console.Out` is process-global, so concurrent captures would redirect each other.
let private consoleLock = obj ()

/// Invoke an already-loaded assembly's entry point under the shared console lock, returning
/// exit code + captured stdout. Takes the loaded `Assembly`, so the caller chooses the
/// `AssemblyLoadContext` the driver runs in.
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
                    // Unwrap to the deepest cause: a failure in a static initializer arrives
                    // as a `TypeInitializationException` wrapping the real exception, which
                    // may itself wrap further.
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

/// `runLoadedEntryPoint` on a fresh ALC, the usual driver-run path.
let runEntryPoint (bytes: byte[]) : int * string =
    runLoadedEntryPoint (loadAssembly bytes)

/// The emitted PE binds no `FSharp.Core`, asserted on its `AssemblyRef` table — the
/// artefact itself, not a use-set the emitter maintains alongside it.
let expectNoFSharpCore (artifact: ClrArtifact) (context: string) : unit =
    Expect.isFalse
        (List.contains "FSharp.Core" artifact.ReferencedAssemblies)
        (sprintf "%s: no FSharp.Core AssemblyRef (refs: %A)" context artifact.ReferencedAssemblies)

// ---- ALC-separable `Vesper.Printf` (choose the runtime handler) --------------
// A driver PE loads into a collectible ALC whose `Load` returns a CHOSEN `Vesper.Printf`;
// everything else falls to Default, so driver and handler meet on ONE `Vesper.Core`.

/// Path to the `buildPackage`-produced `Vesper.Printf.dll`; the call also builds its
/// `Vesper.Core` / `Vesper.List` / `Vesper.Comparison` deps into `packageAlc`.
let private vesperPrintfPath () : string =
    match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
    | Some p -> p
    | None -> failwith "buildPackage Vesper.Printf produced no OutputPath"

/// A collectible ALC resolving `Vesper.Printf` to a chosen on-disk DLL, delegating
/// everything else to Default. Loaded from a byte COPY, not a file handle, so the DLL
/// stays unlocked and `Unload` can collect the context.
type private PrintfLoadContext(printfPath: string) as this =
    inherit AssemblyLoadContext("xparsec-printf-diff", isCollectible = true)

    let printf =
        lazy (use ms = new IO.MemoryStream(IO.File.ReadAllBytes printfPath) in this.LoadFromStream ms)

    override _.Load(name: System.Reflection.AssemblyName) : Assembly =
        if name.Name = "Vesper.Printf" then printf.Value else null

/// Create a fresh collectible ALC bound to the Vesper-compiled `Vesper.Printf.dll`, run
/// `run` against it, then unload. `run` must return no `Type`/`Assembly` from the context.
let withPrintfAlc (run: AssemblyLoadContext -> 'a) : 'a =
    let alc = PrintfLoadContext(vesperPrintfPath ())

    try
        run (alc :> AssemblyLoadContext)
    finally
        alc.Unload()

/// Uniquifies a per-call driver assembly name (Expecto runs in parallel).
let private diffDriverCounter = ref 0

/// Compile a bare driver program and run its entry point inside `alc`, returning exit code
/// + stdout. The driver's `Vesper.Printf` reference binds to whatever `alc` resolves.
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
        // CR + trailing newlines only, not all whitespace: a `%5d` right-justify
        // ("   42") carries meaningful LEADING spaces and a `%A` group carries indent.
        let actual = output.Replace("\r", "").TrimEnd('\n')

        if exitCode <> 0 then
            failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

        actual
    )

/// `runsPrintf` plus an equality assertion against the structural spec oracle.
let runsEq (expected: string) (src: string) : unit =
    let actual = runsPrintf src

    if actual <> expected then
        failwithf "expected %A but the handler produced %A for:\n%s" expected actual src

// ---- Drive the `%A` golden oracle on the VESPER engine -----------------------
// `StructuralPrinter` is reflected out of the `buildPackage` `Vesper.Printf.dll`, in an ALC
// with no `Load` override, so it and the test's fixtures share the Default `Vesper.Core`.

/// The Vesper-compiled `StructuralPrinter::Print(obj, int, int)`, bound once and loaded
/// into its own ALC.
let private vesperStructuralPrintMethod: Lazy<MethodInfo> =
    lazy
        (let path = vesperPrintfPath ()
         // The engine references `Vesper.Core` (the `%A` interfaces) and `Vesper.List`
         // (the cons-list its cycle scan walks), and the ALC below has no `Load` override,
         // so force both Default copies first or the load throws `FileNotFoundException`.
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

/// Render `value` through the Vesper-compiled `StructuralPrinter` at plain `%A`'s default
/// node budget (F#'s 10000).
let structuralPrint (value: obj) (widthBudget: int) : string =
    structuralPrintSized value widthBudget 10000

/// Compile a standalone `%A` engine source (defining `Vesper.StructuralPrinter`) and bind
/// its `Print(obj, int, int)` as a `Func` delegate, so calls run emitted IL, not
/// reflection. Its own non-collectible ALC; `Vesper.Core` / `Vesper.List` come from Default.
let compileStructuralEngine (asmName: string) (source: string) : Func<obj, int, int, string> =
    vesperCoreDll.Value |> ignore
    vesperListDll.Value |> ignore

    let deps = [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison" ]

    let depDlls =
        deps |> List.choose (fun d -> ((buildPackage d).Value |> snd).OutputPath)

    let provider = ClrSymbolProviders.buildContract (deps |> List.map srcPackage)

    let outDir = tmpDir (sprintf "engine-%s" asmName)
    let outPath = IO.Path.Combine(outDir, asmName + ".dll")

    let project =
        { ProjectInfo.library asmName with
            OutputPath = Some outPath
            References = depDlls
        }

    let lexed, file = parseFile source

    let tast =
        Pipeline.analyseFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

    let errs = tast.Residue.Diagnostics |> Diagnostic.errors

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

/// Compile a `<None Include>` Vesper source file (`fileName` relative to this test project)
/// against the Core / List / Comparison contracts and load it into its own ALC, so a
/// fixture value's `%A` interfaces meet the engine's sink on the ONE Default `Vesper.Core`.
let compileFixtureFile (asmName: string) (fileName: string) : Assembly =
    vesperCoreDll.Value |> ignore
    vesperListDll.Value |> ignore

    let deps = [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison" ]

    let depDlls =
        deps |> List.choose (fun d -> ((buildPackage d).Value |> snd).OutputPath)

    let provider = ClrSymbolProviders.buildContract (deps |> List.map srcPackage)

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
        Pipeline.analyseFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

    let errs = tast.Residue.Diagnostics |> Diagnostic.errors

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
// "run this source, get this stdout, exit 0". Every failure message carries `src`, and
// they `failwithf` rather than reference `Expecto.Expect`.

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

/// `runs` for a multi-line expected block (joined with "\n").
let runsLines (expected: string list) (src: string) : unit = runs (String.concat "\n" expected) src

// ---- Externalised program sources (`data/*.fs`) ------------------------------
// Probe programs live under `data/` as `<None Include>` text, compiled through this
// backend, never fsc, since they use Vesper self-host primitives.

let private dataDir = IO.Path.Combine(__SOURCE_DIRECTORY__, "data")

/// Read `data/<name>.fs`, expanding each `//#include <unit>` line recursively against
/// `data/`. A fragment is authored at column 0 and re-indented to the directive's own
/// column, so `    //#include …` lands its lines at 4-space indent inside a type body.
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

/// Compile + run `src` and assert it threw a runtime exception whose type name contains
/// `expectedTypeFragment` (e.g. `"DivideByZero"`), matched against the `failwithf` message
/// `runEntryPoint` builds, which embeds the inner exception's full type name.
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
// Name the Vesper packages a snippet links against; the contract stack, the reference
// DLLs and the whole `depends-on` graph are derived, with the default stack unioned in.

/// `defaultPackages` by package name: what every driver implicitly links.
let private defaultPackageNames =
    [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison"; "Vesper.Printf" ]

/// Transitive `depends-on` closure of `roots`, dependencies before dependents, deduped.
/// Drives both the contract stack and the `References` DLL list.
let private transitivePackages (roots: string list) : string list =
    let acc = System.Collections.Generic.List<string>()

    let rec go (pkg: string) =
        if not (acc.Contains pkg) then
            let m =
                ReferencedProject.resolveManifest Target.Clr (srcPackage pkg)
                |> Result.bind ReferencedProject.loadManifest
                |> PackageFaults.okOrFail (sprintf "transitivePackages %s" pkg)

            m.DependsOn |> List.map dependencyName |> List.iter go

            if not (acc.Contains pkg) then
                acc.Add pkg

    roots |> List.iter go
    List.ofSeq acc

/// Uniquifies a per-call driver assembly name: Expecto runs in parallel and `packageAlc`
/// is process-persistent, so two identically-named loads would collide on identity.
let private driverCounter = ref 0

/// Compile `src` against `packages` unioned with the default stack. Every package in the
/// transitive `depends-on` closure is built once and registered in `packageAlc`; its `.fsi`
/// joins the contract stack, its DLL the `References`. Front end: `analyseFor`, a consumer.
let compilePackages (packages: string list) (src: string) : ClrArtifact =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let depDlls =
        allPackages |> List.choose (fun p -> ((buildPackage p).Value |> snd).OutputPath)

    let provider = ClrSymbolProviders.buildContract (allPackages |> List.map srcPackage)

    let n = System.Threading.Interlocked.Increment driverCounter

    let project =
        { ProjectInfo.defaults (sprintf "PkgDriver%d" n) with
            References = depDlls
        }

    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseFor (compilingClr project) provider (Hashing.originSourceOfText lexed) file

    let analysisErrors = tast.Residue.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty analysisErrors) then
        failwithf
            "compilePackages %A: %d analysis error(s) for:\n%s\n--- errors ---\n%s"
            packages
            (List.length analysisErrors)
            src
            (analysisErrors |> List.map (fun d -> d.Message) |> String.concat "\n")

    Codegen.compile provider project tast

/// The Vesper-compiled `Vesper.Printf`, registered in `packageAlc` once. `buildPackage`
/// deliberately leaves it out, so without this a driver run here would reach printf through
/// Default, which implements a DIFFERENT `Vesper.Core` identity than its own package types.
let private packageAlcPrintf: Lazy<unit> =
    lazy
        (let path =
            match ((buildPackage "Vesper.Printf").Value |> snd).OutputPath with
            | Some p -> p
            | None -> failwith "buildPackage Vesper.Printf produced no OutputPath"

         use ms = new IO.MemoryStream(IO.File.ReadAllBytes path)
         packageAlc.Register("Vesper.Printf", packageAlc.LoadFromStream ms))

/// Compile `src` against `packages` and run its entry point inside `packageAlc`, so driver,
/// dependencies and printf share ONE `Vesper.Core` identity. Returns (exitCode, stdout)
/// plus the emitted bytes, for a caller asserting on the IL in the same pass as the run.
let runPackagesInspect (packages: string list) (src: string) : (int * string) * byte[] =
    packageAlcPrintf.Value
    let artifact = compilePackages packages src
    let bytes = Codegen.toBytes artifact
    use ms = new IO.MemoryStream(bytes)
    let asm = packageAlc.LoadFromStream ms
    runLoadedEntryPoint asm, bytes

/// `runPackagesInspect` without the bytes.
let runPackages (packages: string list) (src: string) : int * string = runPackagesInspect packages src |> fst

/// Compile + run `src` against `packages`; assert exit 0 and trimmed, CRLF-normalised
/// stdout equals `expected`.
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

/// Analyse `src` against the default stack plus `packages`, no codegen, and return the
/// error-severity diagnostics.
let private analysePackagesErrors (packages: string list) (src: string) : Diagnostic list =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let provider = ClrSymbolProviders.buildContract (allPackages |> List.map srcPackage)

    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file
    tast.Diagnostics |> Diagnostic.errors

/// The front-end-only probe: analyse `src` against `packages` and assert NO error
/// diagnostics, without running it.
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
// A new package needs one wrapper line: no DLL lazy, no contract plumbing.

/// Vesper.Option — the option type + `Option` module (counterpart of `runs`).
let runsOption (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Option" ] expected src

/// `runsOption` for a multi-line expected block.
let runsOptionLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Option" ] expected src

/// Analyse `src` through the default contract stack (no codegen) and return the
/// error-severity diagnostics, because analysis collects them rather than throwing.
let private analyseErrors (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile src
    let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file
    tast.Diagnostics |> Diagnostic.errors

/// Analyse `src`; assert an error diagnostic whose message contains `fragment`: bad input
/// is rejected, and rejected for the stated reason.
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

/// Analyse `src`; assert NO error diagnostics, without running it, for front-end-only
/// coverage where codegen is deferred.
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

/// Compile a `Vesper.Result` consumer and return the `ClrArtifact` without running it,
/// for `expectNoFSharpCore` assertions, e.g. that a `%A` of an external Vesper union
/// lowers on the structural engine.
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
// Choice is a pure-data struct union with NO module, so `runsChoice` exercises
// construction + `match`, never a module call.

let runsChoice (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Choice" ] expected src

let runsChoiceLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Choice" ] expected src

let typeChecksChoice (src: string) : unit =
    typeChecksPackages [ "Vesper.Choice" ] src

let failsWithChoice (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Choice" ] fragment src

// ---- Vesper.Array wrappers ---------------------------------------------------
// BCL-only: `arr.[i]` / `arr.Length` / `Array.zeroCreate` / `[| … |]` lower to `ldelem` /
// `ldlen` / `newarr`.

let runsArray (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Array" ] expected src

let runsArrayLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Array" ] expected src

let typeChecksArray (src: string) : unit =
    typeChecksPackages [ "Vesper.Array" ] src

// ---- Vesper.Seq wrappers -----------------------------------------------------
// A driver's `seq<'T>` source is `System.Linq.Enumerable.Range(start, count)`, a real BCL
// `IEnumerable<int>`.

let runsSeq (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Seq" ] expected src

let runsSeqLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Seq" ] expected src

let typeChecksSeq (src: string) : unit = typeChecksPackages [ "Vesper.Seq" ] src

// ---- Vesper.Set wrappers -----------------------------------------------------
// A driver's HOF argument (`Set.fold` / `partition`'s folder) must be written CURRIED:
// `fun s -> fun x -> …`.

let runsSet (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Set" ] expected src

let runsSetLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Set" ] expected src

// ---- PE inspection helpers ---------------------------------------------------
// Read the emitted PE through `System.Reflection.Metadata` (method/field tokens, the
// AssemblyRef table, raw IL) without going through the runtime loader.

open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// Open a PE byte stream as a metadata reader. The caller disposes the `PEReader`; the
/// `MetadataReader` it yields is valid only for that lifetime.
let openPe (bytes: byte[]) : PEReader =
    new PEReader(System.Collections.Immutable.ImmutableArray.Create<byte>(bytes))

/// The base-type full name of the FIRST type-def whose simple name satisfies `nameMatches`,
/// resolving the handle through a `TypeReference` or a sibling `TypeDefinition`.
/// `System.ValueType` vs `System.Object` distinguishes a struct closure from a heap one.
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

/// The base-type SIMPLE name (`ValueType` / `Object`) of EVERY `<closure>$…` type-def in
/// the PE, one entry per closure, so a test can assert all closures are value types.
/// `<none>` when the base handle is not a `TypeReference`.
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

/// Every method-def's `(declaringType, methodName)`, the declaring type named
/// `Namespace.Name`, so a NESTED type appears under its bare simple name, not `Outer+Inner`.
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

/// Every AssemblyRef name in the PE, the dependency surface the loader resolves, read
/// straight off the bytes with no `AssemblyLoadContext`.
let peAssemblyRefs (bytes: byte[]) : string list =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    [
        for h in md.AssemblyReferences do
            let r = md.GetAssemblyReference h
            md.GetString r.Name
    ]

/// Total `InterfaceImpl` rows across every type-def, one per `: IFace` entry actually
/// emitted. `GetInterfaces` folds in inherited ones, so only this count separates
/// "emitted both `IEnumerable<int>` and `IEnumerable`" from "emitted just the generic".
let peInterfaceImplCount (bytes: byte[]) : int =
    use peReader = openPe bytes
    let md = peReader.GetMetadataReader()

    md.TypeDefinitions
    |> Seq.sumBy (fun h -> (md.GetTypeDefinition h).GetInterfaceImplementations().Count)

/// Raw IL bytes of the FIRST method on `declaringType` whose name satisfies `nameMatches`,
/// for when the caller cannot pin an exact name (a top-level function on "Program",
/// `n <> "Main"`). `[||]` for a body-less method; throws if none matches.
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

/// `peMethodIlWhere` for EVERY matching method, when the caller picks the right one by
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

/// Every static method on the anonymous "Program" class (where a binding that declares no
/// module lands), minus the synthesised entry point; `[||]` if there is no Program class.
/// Takes the loaded `Assembly` so a caller reflecting types out of the same PE holds ONE.
let programClassMethodsOf (asm: Assembly) : MethodInfo[] =
    match asm.GetType "Program" with
    | null -> [||]
    | program ->
        program.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
        |> Array.filter (fun m -> m.Name <> "Main")

/// `programClassMethodsOf` for a caller that reflects nothing else out of the PE.
let programClassMethods (bytes: byte[]) : MethodInfo[] =
    programClassMethodsOf (loadAssembly bytes)

/// Raw IL bytes of the method named `methodName` on `declaringType`, for asserting an
/// opcode sequence. `[||]` for an abstract method (no body); throws if not found.
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

/// The return type's `ELEMENT_TYPE_*` tag from a method's MethodDef signature blob. For a
/// nominal return type that is the encoder's value-vs-class decision: `0x11`
/// (ELEMENT_TYPE_VALUETYPE) vs `0x12` (ELEMENT_TYPE_CLASS). Throws if not found.
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
