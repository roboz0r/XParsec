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
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

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

/// The assembly name a helper that builds no `ProjectInfo` of its own compiles under.
let testAsm = AssemblyName "Test"

/// `testAsm` compiled for the CLR target.
let testCompiling: CompilingAssembly = { Name = testAsm; Target = Target.Clr }

/// A frozen file's declarations as pool handles with the specialization graph expanded,
/// which is the representation the backend starts from. Until this runs, an inline body
/// sits behind an edge rather than in the tree.
let pooledDecls (frozen: FrozenPools) : TastAccessor.DeclId list =
    let pool = TastPoolBuilder.openOver frozen
    (InlineExpand.expand pool (TastAccessor.roots pool |> List.ofArray)).Decls

/// The lambda a specialization entry binds, read out of the file's own table.
let specializationValue (tast: TastFile) (spec: SpecializationId) : TExpr =
    let (SpecializationId i) = spec
    tast.Specializations.[i].Value

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
        | TDecl.Let(_, value, _, _, _) -> TastWalk.iterExpr it value
        | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
        | TDecl.Type _ -> ()

    for d in tast.Decls do
        ofDecl d

    for entry in tast.Specializations do
        TastWalk.iterExpr it entry.Value

/// Lex + parse a source string; a script fragment wraps as `AnonymousModule`. A source that
/// parses only because recovery patched it raises here, as it does in the driver, rather
/// than being analysed as though it had been written that way.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match ParseChain.parseUnrecovered Set.empty input with
    | Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Ok parsed -> parsed.Lexed, parsed.Tree

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

let vesperOptionPackage: string = srcPackage "Vesper.Option"

/// The last segment of a `depends-on` path (`"../Vesper.Core"` ⇒ `"Vesper.Core"`), which is how
/// this harness keys packages under `src/`.
let dependencyName (entry: string) : string =
    IO.Path.GetFileName(IO.Path.TrimEndingDirectorySeparator entry)

/// The `src/` package directories listed under `depends-on` in `package`'s CLR manifest, in
/// manifest order: the contract stack its own impl analyses against.
let dependencyPackages (package: string) : string list =
    ReferencedProject.resolveManifest Target.Clr package
    |> Result.bind ReferencedProject.loadManifest
    |> PackageFaults.okOrFail (sprintf "dependencyPackages %s" package)
    |> fun manifest -> manifest.DependsOn |> List.map (dependencyName >> srcPackage)

/// The contract, GATED, so a test that miswires its packages fails with the contract error
/// and not an unresolved name three files later.
let private gatedContract
    (label: string)
    (contract: PackageProviders.AnalysedManifest)
    : PackageProviders.AnalysedManifest =
    match PackageProviders.AnalysedManifest.gate contract with
    | Ok contract -> contract
    | Error ds ->
        failwithf "%s: %d contract error(s):\n%s" label (List.length ds) (AssemblyFiles.AnchoredDiagnostic.renderAll ds)

/// `input` as the ONE unit of an assembly named `assemblyName`. The unit carries that name as
/// its file name, which is what a failure message prints beside each finding. Compilation
/// defines are empty: a source here carries no `#if`.
let oneSource (assemblyName: string) (input: string) : AssemblyFiles.SourceUnit list =
    [
        AssemblyFiles.SourceFile.ofText (assemblyName + ".fs") input
        |> AssemblyFiles.SourceUnit.ofImplementation
    ]

/// `oneSource` as a CLR assembly's inputs.
let private oneSources (assemblyName: string) (input: string) : AssemblySources =
    AssemblySources.synthetic assemblyName Target.Clr Set.empty (oneSource assemblyName input)

/// The artifact, or a test failure carrying the findings that refused it. `label` identifies
/// the compile in that message.
let private emitted (label: string) (result: Result<ClrArtifact, AssemblyFiles.AnchoredDiagnostic list>) : ClrArtifact =
    match result with
    | Ok artifact -> artifact
    | Error diagnostics ->
        failwithf
            "%s: the front end refused the assembly over %d error diagnostic(s):\n%s"
            label
            (List.length diagnostics)
            (AssemblyFiles.AnchoredDiagnostic.renderAll diagnostics)

/// Compile one in-memory source as a whole assembly against `external`, through the production
/// driver. The emitted `AssemblyRef` identities come from `project.References` alone.
let compileAgainst (external: IExternalSymbolProvider) (project: ProjectInfo) (input: string) : ClrArtifact =
    ClrDriver.compileWith [] external project (oneSources project.AssemblyName input)
    |> emitted project.AssemblyName

/// The self-package contract, GATED.
let private gatedContractForSelf
    (label: string)
    (selfPackage: string)
    (packageDirs: string list)
    : PackageProviders.AnalysedManifest =
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
         let sources =
             ReferencedProject.resolveManifest Target.Clr vesperCorePackage
             |> Result.bind AssemblySources.ofManifest
             |> PackageFaults.okOrFail "vesperCoreDll"

         // Core defines its own primitives, so it references nothing and declares ITSELF as
         // the self manifest. That seeds the platform metadata with its own `{ platform -> canon }`
         // axis, so a BCL signature presents `System.String` as `Vesper.string` here too.
         let contract = gatedContractForSelf "vesperCoreDll" vesperCorePackage []

         let artifact =
             match ClrDriver.compileWith [] contract.Provider project sources with
             | Ok artifact -> artifact
             | Error diags ->
                 failwithf
                     "vesperCoreDll: %d analysis error(s):\n%s"
                     (List.length diags)
                     (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath corePath |> ignore
         corePath)

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
        vesperOptionPackage
        vesperListPackage
        vesperComparisonPackage
        vesperPrintfPackage
    ]

/// Front-end a program to a (SemType) `TastFile` through the default contract stack.
let analyse (input: string) : TastFile =
    let lexed, file = parseFile input

    Pipeline.analyseSemFor
        testCompiling
        (ClrSymbolProviders.buildContract defaultPackages)
        (LexedFile.ofText lexed)
        file

/// `analyse`, keeping the `PassContext`. `Freeze.run` reads `ctx.Bindings.Scheme` for the
/// bound variable a residual typar root belongs to; the TAST alone does not carry it.
let analyseWithCtx (input: string) : PassContext * TastFile =
    let lexed, file = parseFile input

    Pipeline.analyseSemWithContextFor
        testCompiling
        (ClrSymbolProviders.buildContract defaultPackages)
        (LexedFile.ofText lexed)
        file

/// Load context for the package-build harness. `Load` resolves a sibling `Vesper.*`
/// package from the registry below, so a package binds against THIS harness's copy of its
/// dependencies; everything else returns `null` and falls through to Default.
type PackageLoadContext() =
    inherit AssemblyLoadContext("xparsec-package-build", isCollectible = false)

    let built =
        Collections.Concurrent.ConcurrentDictionary<string, Assembly>(StringComparer.Ordinal)

    member _.Register(name: string, asm: Assembly) = built.[name] <- asm

    override _.Load(name: System.Reflection.AssemblyName) : Assembly =
        match built.TryGetValue name.Name with
        | true, asm -> asm
        | _ -> null

/// The ONE load context every `buildPackage` artifact and every package-harness driver run
/// shares, so driver, dependencies and printf resolve a single `Vesper.Core` identity.
let packageAlc = PackageLoadContext()

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

                 // The trees and the pairing both the contract fold and the unit list below
                 // work off. `.fsi`↔`.fs` conformance runs inside analysis, so the compile
                 // seam below returns `Error` on a mismatch.
                 let parsedManifest = ParsedManifest.ofManifest manifest

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
                         (ClrSymbolProviders.contractForSelfParsed (srcPackage pkg) depManifests parsedManifest)

                 // Self-host front end, so a bare `[]` / `::` in a BCL-only package defaults
                 // to the Vesper cons-list rather than FSharp.Core's. The seam returns `Error`
                 // on any error-severity diagnostic instead of emitting a degraded DLL.
                 let sources = AssemblySources.ofParsedManifest parsedManifest

                 let outDir = tmpDir (sprintf "pkg-%s" pkg)
                 let outPath = IO.Path.Combine(outDir, manifest.Name + ".dll")

                 let project =
                     { ProjectInfo.library manifest.Name with
                         OutputPath = Some outPath
                         References = depDlls
                     }

                 let artifact =
                     match ClrDriver.compileWith [] contract.Provider project sources with
                     | Ok artifact -> artifact
                     | Error diags ->
                         failwithf
                             "buildPackage %s: %d analysis error(s):\n%s"
                             pkg
                             (List.length diags)
                             (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

                 Codegen.materialise artifact

                 use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)

                 // A package with no `impl` files carries no runtime types, and `Vesper.Printf`
                 // is loaded into Default separately, so registering a second copy here would
                 // bind a driver's `Vesper.Formatter` to the wrong one. Throwaway ALC for both.
                 if List.isEmpty sources.Units || manifest.Name = "Vesper.Printf" then
                     let throwaway = AssemblyLoadContext("xparsec-contract-only", isCollectible = true)

                     throwaway.LoadFromStream ms, artifact
                 else
                     let asm = packageAlc.LoadFromStream ms
                     packageAlc.Register(manifest.Name, asm)
                     asm, artifact)
    )

/// The on-disk DLL `buildPackage` produced for `package`, building it on first use.
let packageOutputPath (package: string) : string =
    match ((buildPackage package).Value |> snd).OutputPath with
    | Some p -> p
    | None -> failwithf "buildPackage %s produced no OutputPath" package

/// The Vesper-compiled DLL `buildPackage` produces for `package`, loaded into the *Default*
/// `AssemblyLoadContext` and returned by path, so a fresh-ALC driver resolves it by
/// fall-through. `deps` are the Default copies its references bind to, forced first.
let private defaultLoadedPackageDll (deps: Lazy<string> list) (package: string) : Lazy<string> =
    lazy
        (for dep in deps do
            dep.Value |> ignore

         let path = packageOutputPath package
         AssemblyLoadContext.Default.LoadFromAssemblyPath path |> ignore
         path)

/// `Vesper.Option.dll`: the `Vesper.Option\`1` struct union plus `OptionModule`.
let vesperOptionDll: Lazy<string> =
    defaultLoadedPackageDll [ vesperCoreDll ] "Vesper.Option"

/// `Vesper.List.dll`: the `Vesper.Collections.List\`1` cons-list plus `ListModule`. A
/// `[1; 2; 3]` consumer literal binds to this list by ARITY (nullary terminator + binary
/// cons), not by case name.
let vesperListDll: Lazy<string> =
    defaultLoadedPackageDll [ vesperCoreDll; vesperOptionDll ] "Vesper.List"

let vesperPrintfDll: Lazy<string> =
    defaultLoadedPackageDll [ vesperCoreDll; vesperListDll ] "Vesper.Printf"

/// Add `Vesper.Core.dll` / `Vesper.Option.dll` / `Vesper.List.dll` / `Vesper.Printf.dll` to
/// `References` so a program's function values, list literals and `printf` calls resolve, but
/// not when the project IS that package. An unused reference emits no `AssemblyRef`.
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
            |> ensure "Vesper.Option" vesperOptionDll
            |> ensure "Vesper.List" vesperListDll
            |> ensure "Vesper.Printf" vesperPrintfDll
    }

/// `input` analysed against `manifestPaths`' symbol stack and its cross-package inline bodies,
/// built once and cached per manifest set: an `External(name)` whose body lives in a referenced
/// `.fs` splices in pre-freeze. `[]` manifests ⇒ the .NET metadata reader alone.
let private analyseContract (manifestPaths: string list) (assemblyName: string) (input: string) : AnalysedAssembly =
    Frontend.analyse (ClrSymbolProviders.buildContract manifestPaths) (oneSources assemblyName input)

/// What the front end may report about a compile.
type private Expected =
    | Silent
    /// At least one finding, every one of them a WARNING mentioning `fragment`.
    | Warning of fragment: string

/// The findings that refuse the compile, given what it expects.
let private unexpected
    (expected: Expected)
    (diagnostics: AssemblyFiles.AnchoredDiagnostic list)
    : AssemblyFiles.AnchoredDiagnostic list =
    match expected with
    | Silent -> diagnostics
    | Warning fragment ->
        match diagnostics with
        | [] -> failwithf "expected a warning mentioning '%s'; the front end reported none" fragment
        // An ERROR carrying `fragment` is unexpected too: it would otherwise pass here and die
        // at the gate, whose message mentions neither the fragment nor the test's intent.
        | ds ->
            ds
            |> List.filter (fun d -> Diagnostic.isError d.Diagnostic || not (d.Diagnostic.Message.Contains fragment))

/// `input` compiled against `manifestPaths`' contract, with `withCore`'s DLLs in the emitted
/// `AssemblyRef` set. Refused where a finding falls outside `expected`.
let private compileContract
    (expected: Expected)
    (manifestPaths: string list)
    (project: ProjectInfo)
    (input: string)
    : ClrArtifact =
    let analysed = analyseContract manifestPaths project.AssemblyName input

    match unexpected expected analysed.Diagnostics with
    | [] ->
        ClrDriver.emitAnalysed [] (withCore project) analysed
        |> emitted project.AssemblyName
    | diagnostics ->
        failwithf
            "%s: the front end reported %d unexpected diagnostic(s):\n%s"
            project.AssemblyName
            (List.length diagnostics)
            (AssemblyFiles.AnchoredDiagnostic.renderAll diagnostics)

/// The default compile path: `int` / `hash` / the operators all resolve from the `Vesper.Core`
/// contract, and the front end reports nothing.
let compileSource (assemblyName: string) (input: string) : ClrArtifact =
    compileContract Silent defaultPackages (ProjectInfo.defaults assemblyName) input

/// `compileSource` for a source that warns, where every warning mentions `fragment`.
let compileSourceWarning (fragment: string) (assemblyName: string) (input: string) : ClrArtifact =
    compileContract (Warning fragment) defaultPackages (ProjectInfo.defaults assemblyName) input

/// `compileSource`'s front end alone, stopping before emission: every finding the assembly
/// surfaced, anchored in its own file. `analyse` differs in compiling under `testAsm`, which
/// changes how a locally declared type resolves.
let diagnoseSource (assemblyName: string) (input: string) : AssemblyFiles.AnchoredDiagnostic list =
    (analyseContract defaultPackages assemblyName input).Diagnostics

/// The error-severity half of `diagnoseSource`.
let diagnoseSourceErrors (assemblyName: string) (input: string) : AssemblyFiles.AnchoredDiagnostic list =
    diagnoseSource assemblyName input |> AssemblyFiles.AnchoredDiagnostic.errors

/// The findings whose message mentions `fragment`.
let mentioning
    (fragment: string)
    (diagnostics: AssemblyFiles.AnchoredDiagnostic list)
    : AssemblyFiles.AnchoredDiagnostic list =
    diagnostics |> List.filter (fun d -> d.Diagnostic.Message.Contains fragment)

/// Every finding's message.
let diagnosticMessages (diagnostics: AssemblyFiles.AnchoredDiagnostic list) : string list =
    diagnostics |> List.map (fun d -> d.Diagnostic.Message)

/// The CLR artifacts a frozen-tree round-trip must reconcile against the DIRECT codegen.
/// All three come off one analysed assembly and differ only by the round-trip.
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

/// `input` analysed as one assembly named `assemblyName` against `manifestPaths` and gated,
/// or a test failure rendering the error diagnostics.
let analysedEmittable (manifestPaths: string list) (assemblyName: string) (input: string) : EmittableAssembly =
    match AnalysedAssembly.gate RuntimeModules.unsupported (analyseContract manifestPaths assemblyName input) with
    | Ok emittable -> emittable
    | Error ds ->
        failwithf
            "%s: %d error diagnostic(s):\n%s"
            assemblyName
            (List.length ds)
            (AssemblyFiles.AnchoredDiagnostic.renderAll ds)

/// The codegen symbol view used when emitting an `analysedEmittable` assembly, with its
/// pooled declarations.
let analysedSymbols
    (manifestPaths: string list)
    (assemblyName: string)
    (input: string)
    : ICodegenSymbols * TastAccessor.DeclId list =
    let emittable = analysedEmittable manifestPaths assemblyName input
    let decls = emittable.Files |> List.collect (fun f -> pooledDecls f.Frozen)
    CodegenSymbols.ofProvider emittable.Visibility, decls

/// Produce the round-trip artifacts a frozen-tree gate reconciles. One analysis, one gate and
/// one project, so an artifact differs from `Direct` only by its round-trip.
let compileConformanceDirectAndRoundTripped (assemblyName: string) (input: string) : ConformanceRoundTripArtifacts =
    let project = ProjectInfo.defaults assemblyName
    let emittable = analysedEmittable defaultPackages assemblyName input
    let file = List.exactlyOne emittable.Files
    let cored = withCore project

    let over (frozen: FrozenPools) =
        Codegen.emitAssembly
            []
            cored
            { emittable with
                Files = [ { file with Frozen = frozen } ]
            }

    {
        Direct = over file.Frozen
        ThawRoundTripped = over (FrozenCodec.thaw (FrozenCodec.flatten file.Frozen))
        PoolRoundTripped = over (TastPools.rePool file.Frozen (TastUnpool.ofPools file.Frozen))
    }

/// The conformance corpus names programs with hyphens (`arith-byte`); an assembly name
/// must be an identifier the emitted module can carry.
let conformanceAssemblyName (program: string) : string =
    "Conformance_" + program.Replace("-", "_")

/// `compileSource` against a caller-supplied `ProjectInfo` (e.g. an on-disk app build via
/// `ProjectInfo.app`).
let compileSourceTo (project: ProjectInfo) (input: string) : ClrArtifact =
    compileContract Silent defaultPackages project input

/// Contract-backed compile against the real `Vesper.Core` manifest.
let compileSourceContract (assemblyName: string) (input: string) : ClrArtifact =
    compileContract Silent [ vesperCorePackage ] (ProjectInfo.defaults assemblyName) input

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

/// The reduction applied to captured stdout before an equality assertion. CRs are
/// removed under every rule.
type StdoutTrim =
    /// `Trim()`: surrounding whitespace is discarded.
    | Whole
    /// Trailing newlines only: leading and embedded alignment spaces survive, as a `%5d`
    /// right-justify ("   42") or a `%A` indent requires.
    | TrailingNewlines

let private trimStdout (trim: StdoutTrim) (output: string) : string =
    let text = output.Replace("\r", "")

    match trim with
    | Whole -> text.Trim()
    | TrailingNewlines -> text.TrimEnd '\n'

/// Assert that a run exited 0 and that its stdout, reduced by `trim`, equals `expected`.
/// `src` goes into the failure message.
let expectRan (trim: StdoutTrim) (expected: string) (src: string) (run: int * string) : unit =
    let exitCode, output = run
    let actual = trimStdout trim output

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for:\n%s\n--- stdout ---\n%s" exitCode src actual

    if actual <> expected then
        failwithf "expected %A but got %A for:\n%s" expected actual src

/// The emitted PE binds no `FSharp.Core`, asserted on its `AssemblyRef` table — the
/// artefact itself, not a use-set the emitter maintains alongside it.
let expectNoFSharpCore (artifact: ClrArtifact) (context: string) : unit =
    Expect.isFalse
        (List.contains "FSharp.Core" artifact.ReferencedAssemblies)
        (sprintf "%s: no FSharp.Core AssemblyRef (refs: %A)" context artifact.ReferencedAssemblies)

/// The emitted PE carries no `AssemblyRef` to its own name `asmName`. A cross-file
/// reference that failed to re-home to its local definition emits a self-`AssemblyRef`,
/// which faults the loader.
let expectNoSelfAssemblyRef (asmName: string) (bytes: byte[]) : unit =
    let refs = peAssemblyRefs bytes

    Expect.isFalse
        (refs |> List.contains asmName)
        (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

/// Materialise `artifact` as a `dotnet <dll>` bundle, run it under the host, and assert exit
/// 0 and that trimmed stdout equals `expected`. Returns the bundle's DLL path for a caller
/// inspecting what was written beside it.
let ranOnDisk (artifact: ClrArtifact) (expected: string) (src: string) : string =
    Codegen.materialiseApp artifact

    let dllPath =
        match artifact.OutputPath with
        | Some p -> p
        | None -> failwithf "%s: the artifact carries no OutputPath; build it through ProjectInfo.app" src

    runOnDisk dllPath |> expectRan Whole expected src
    dllPath

/// Compile `src` as the app `project`, then `ranOnDisk`.
let runsOnDisk (project: ProjectInfo) (expected: string) (src: string) : string =
    ranOnDisk (compileSourceTo project src) expected src

// ---- ALC-separable `Vesper.Printf` (choose the runtime handler) --------------
// A driver PE loads into a collectible ALC whose `Load` returns a CHOSEN `Vesper.Printf`;
// everything else falls to Default, so driver and handler meet on ONE `Vesper.Core`.

/// Path to the `buildPackage`-produced `Vesper.Printf.dll`; the call also builds its
/// `Vesper.Core` / `Vesper.List` / `Vesper.Comparison` deps into `packageAlc`.
let private vesperPrintfPath () : string = packageOutputPath "Vesper.Printf"

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

/// Compile `src` as the driver assembly `name` and run its entry point inside `alc`,
/// returning exit code + stdout. The driver's `Vesper.Printf` reference binds to whatever
/// `alc` resolves.
let runNamedDriverInAlc (alc: AssemblyLoadContext) (name: string) (src: string) : int * string =
    let artifact = compileSource name src
    use ms = new IO.MemoryStream(Codegen.toBytes artifact)
    let asm = alc.LoadFromStream ms
    runLoadedEntryPoint asm

/// Uniquifies a per-call driver assembly name (Expecto runs in parallel).
let private diffDriverCounter = ref 0

/// `runNamedDriverInAlc` under a generated `DiffDriver<n>` name.
let runDriverInAlc (alc: AssemblyLoadContext) (src: string) : int * string =
    let n = Threading.Interlocked.Increment diffDriverCounter
    runNamedDriverInAlc alc (sprintf "DiffDriver%d" n) src

/// Compile `src` as the driver assembly `name`, run it against the Vesper-compiled
/// `Vesper.Printf` in a fresh ALC, and assert exit 0 and that stdout, reduced by `trim`,
/// equals `expected`.
let printsUnder (trim: StdoutTrim) (name: string) (expected: string) (src: string) : unit =
    withPrintfAlc (fun alc -> runNamedDriverInAlc alc name src)
    |> expectRan trim expected src

/// `printsUnder TrailingNewlines` for the structural spec oracle, under a generated
/// driver name.
let runsEq (expected: string) (src: string) : unit =
    withPrintfAlc (fun alc -> runDriverInAlc alc src)
    |> expectRan TrailingNewlines expected src

// ---- Drive the `%A` golden oracle on the VESPER engine -----------------------
// `StructuralPrinter` is reflected out of the `buildPackage` `Vesper.Printf.dll`, in an ALC
// with no `Load` override, so it and the test's fixtures share the Default `Vesper.Core`.

/// `asm`'s `Vesper.StructuralPrinter::Print(obj, int, int)`, or a test failure labelled
/// `label`.
let private structuralPrintMethodOf (label: string) (asm: Assembly) : MethodInfo =
    let sp = asm.GetType("Vesper.StructuralPrinter", true)
    let m = sp.GetMethod("Print", [| typeof<obj>; typeof<int>; typeof<int> |])

    if isNull m then
        failwithf "%s: no Vesper.StructuralPrinter.Print(obj, int, int)" label

    m

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

         structuralPrintMethodOf "Vesper.Printf" asm)

/// Render `value` through the Vesper-compiled `StructuralPrinter` with an explicit
/// column budget and PrintSize node budget (the `%.NA` mode).
let structuralPrintSized (value: obj) (widthBudget: int) (sizeBudget: int) : string =
    vesperStructuralPrintMethod.Value.Invoke(null, [| value; box widthBudget; box sizeBudget |]) :?> string

/// Render `value` through the Vesper-compiled `StructuralPrinter` at plain `%A`'s default
/// node budget (F#'s 10000).
let structuralPrint (value: obj) (widthBudget: int) : string =
    structuralPrintSized value widthBudget 10000

/// Compile `source` as the library `asmName` against the Core / List / Comparison
/// contracts, materialise it under `tmp/selfhost-<asmName>/`, and load it into its own
/// non-collectible ALC. `Vesper.Core` / `Vesper.List` resolve to the Default copies, so
/// the assembly's `%A` interfaces meet the engine's sink on ONE `Vesper.Core`.
let compileSelfHostAssembly (asmName: string) (source: string) : Assembly =
    vesperCoreDll.Value |> ignore
    vesperListDll.Value |> ignore

    let deps = [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison" ]
    let depDlls = deps |> List.map packageOutputPath
    let provider = ClrSymbolProviders.buildContract (deps |> List.map srcPackage)

    let outDir = tmpDir (sprintf "selfhost-%s" asmName)
    let outPath = IO.Path.Combine(outDir, asmName + ".dll")

    let project =
        { ProjectInfo.library asmName with
            OutputPath = Some outPath
            References = depDlls
        }

    let artifact = compileAgainst provider project source
    Codegen.materialise artifact

    let alc =
        AssemblyLoadContext(sprintf "xparsec-selfhost-%s" asmName, isCollectible = false)

    use ms = new IO.MemoryStream(IO.File.ReadAllBytes outPath)
    alc.LoadFromStream ms

/// Compile a standalone `%A` engine source (defining `Vesper.StructuralPrinter`) through
/// `compileSelfHostAssembly` and bind its `Print(obj, int, int)` as a `Func` delegate, so
/// calls run emitted IL, not reflection.
let compileStructuralEngine (asmName: string) (source: string) : Func<obj, int, int, string> =
    let m =
        compileSelfHostAssembly asmName source
        |> structuralPrintMethodOf (sprintf "compileStructuralEngine %s" asmName)

    m.CreateDelegate(typeof<Func<obj, int, int, string>>) :?> Func<obj, int, int, string>

/// `compileSelfHostAssembly` over a `<None Include>` Vesper source file, `fileName`
/// relative to this test project.
let compileFixtureFile (asmName: string) (fileName: string) : Assembly =
    compileSelfHostAssembly asmName (IO.File.ReadAllText(IO.Path.Combine(__SOURCE_DIRECTORY__, fileName)))

// ---- Layer 1 behavioral corpus helpers --------------------------------------
// "run this source, get this stdout, exit 0". Every failure message carries `src`, and
// they `failwithf` rather than reference `Expecto.Expect`.

/// Run `artifact` in-process and assert exit 0 and that trimmed, CRLF-normalised stdout
/// equals `expected`. `src` goes into the failure message.
let private ranPrinting (artifact: ClrArtifact) (expected: string) (src: string) : unit =
    runEntryPoint (Codegen.toBytes artifact) |> expectRan Whole expected src

/// Compile `src` as a bare program, run it in-process, and assert exit 0 and
/// that trimmed, CRLF-normalised stdout equals `expected`.
let runs (expected: string) (src: string) : unit =
    ranPrinting (compileSource "Layer1Corpus" src) expected src

/// `runs` for a multi-line expected block (joined with "\n").
let runsLines (expected: string list) (src: string) : unit = runs (String.concat "\n" expected) src

/// `runsLines` for a program whose every finding mentions `fragment`.
let runsLinesWarning (fragment: string) (expected: string list) (src: string) : unit =
    ranPrinting (compileSourceWarning fragment "Layer1Corpus" src) (String.concat "\n" expected) src

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
let compileSourceData (name: string) : ClrArtifact = compileSource name (dataSource name)

/// `runsLines` with the program read from `data/<name>.fs`.
let runsDataLines (expected: string list) (name: string) : unit = runsLines expected (dataSource name)

/// Compile + run `src` and assert it threw a runtime exception whose type name contains
/// `expectedTypeFragment` (e.g. `"DivideByZero"`), matched against the `failwithf` message
/// `runEntryPoint` builds, which embeds the inner exception's full type name.
let runtimeThrows (expectedTypeFragment: string) (src: string) : unit =
    let artifact = compileSource "Layer1Corpus" src

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

// ---- The struct-union data corpus ---------------------------------------------

/// One `StructUnion*` data program and the `[<Struct>]` union it declares.
type StructUnionProgram =
    {
        Program: string
        /// The union's source name.
        Union: string
        Arity: int
    }

    /// The union's metadata name (`` GBox`1 ``).
    member this.MetaName: string = SymbolKeyOps.arityName this.Union this.Arity

/// Every `StructUnion*` data program with its union. The census tables are in this order.
let structUnionCorpus: StructUnionProgram list =
    [
        {
            Program = "StructUnionShape"
            Union = "Shape"
            Arity = 0
        }
        {
            Program = "StructUnionGenericShape"
            Union = "GBox"
            Arity = 1
        }
        {
            Program = "StructUnionSameNameFields"
            Union = "Mixed"
            Arity = 0
        }
        {
            Program = "StructUnionExternalPayload"
            Union = "External"
            Arity = 0
        }
        {
            Program = "StructUnionLocalRefPayload"
            Union = "Holder"
            Arity = 1
        }
        {
            Program = "StructUnionMixedStorage"
            Union = "Storage"
            Arity = 0
        }
        {
            Program = "StructUnionGenericOverlay"
            Union = "GShape"
            Arity = 1
        }
        {
            Program = "StructUnionReaderNameClash"
            Union = "Readers"
            Arity = 0
        }
    ]

type AnalysedUnion =
    {
        Key: TypeKey
        Regime: UnionRegime
        Cases: Frozen.TUnionCase list
    }

/// The union `unionName` among the analysed declarations. Raises when the program declares
/// no such union.
let unionOf (decls: TastAccessor.DeclId list) (unionName: string) : AnalysedUnion =
    let found =
        [
            for d in decls do
                match TastAccessor.declKind d with
                | DeclShape.Type ->
                    let td = TastAccessor.declType d

                    match td.Kind with
                    | TTypeKindG.Union u when td.Name = unionName ->
                        {
                            Key = td.TypeKey
                            Regime =
                                UnionRegime.classify
                                    u.ValueKind
                                    u.Cases.Length
                                    (u.Cases |> EqArray.exists (fun c -> not c.Fields.IsEmpty))
                            Cases = EqArray.toList u.Cases
                        }
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ one ] -> one
    | _ -> failwithf "no union '%s' among the analysed declarations" unionName

/// The flat placements of `u`. Raises when the union's regime is a hierarchy one.
let flatPlacementsOf (symbols: ICodegenSymbols) (u: AnalysedUnion) : FlatUnionPlacements =
    match FlatUnionPlacements.ofCases symbols u.Key u.Regime u.Cases with
    | ValueSome p -> p
    | ValueNone -> failwithf "%A is a hierarchy regime" u.Regime

let analysedStructUnion (entry: StructUnionProgram) : ICodegenSymbols * AnalysedUnion =
    let symbols, decls =
        analysedSymbols defaultPackages entry.Program (dataSource entry.Program)

    symbols, unionOf decls entry.Union
