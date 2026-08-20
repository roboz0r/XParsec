/// Fixtures + provider wiring for the semantic-analysis benchmark: the realistic
/// multi-file chain Vesper.Core → Vesper.List → Vesper.Set → a synthetic consumer.
///
/// Each package is analysed as its OWN assembly against a provider composed from its
/// `depends-on` closure (self EXCLUDED — the package defines its own types here),
/// mirroring the package-build wiring (`Codegen.Clr.Tests` `buildPackage`/`vesperListDll`).
/// Why this shape: the CROSS-STAGE re-thaw of
/// shared upstream contracts — Core's `.fsi` thawed by List's provider AND again by Set's
/// AND again by the synthetic consumer's — is the interning ceiling a single-file bench
/// cannot see. The chain prefix is the size axis, not synthetic sizes.
module XParsec.FSharp.Benchmarks.SemanticAnalysisFixtures

open System.IO
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr

/// repo `src/` dir, resolved from this bench file (`bench/XParsec.FSharp.Benchmarks`).
let private srcDir =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src"))

let packageDir (pkg: string) = Path.Combine(srcDir, pkg)

let private loadManifest (pkg: string) =
    match ReferencedProject.resolveManifest Target.Clr (packageDir pkg) with
    | Error e -> failwithf "SemanticAnalysisFixtures: cannot resolve '%s' manifest: %s" pkg (PackageSetFault.describe e)
    | Ok mp ->
        match ReferencedProject.loadManifest mp with
        | Ok m -> m
        | Error e ->
            failwithf "SemanticAnalysisFixtures: cannot load '%s' manifest: %s" pkg (PackageSetFault.describe e)

/// One assembly's worth of analysable input: its home name, the external provider its
/// files resolve against, and its ordered impl files.
type Stage =
    {
        Name: string
        Provider: IExternalSymbolProvider
        Files: AssemblyFiles.SourceFile list
    }

/// A contract provider over a set of package names, WITH host .NET reflection: without it
/// Core's `(# "System.Int32" #)` reprs and Set's `ICollection` impls do not resolve and the
/// bench times the error path. The transitive closure is taken here, so pass DIRECT deps.
let composeProvider (pkgs: string list) : IExternalSymbolProvider =
    pkgs |> List.map packageDir |> ClrSymbolProviders.buildContract

/// A package analysed as its own assembly: provider = its `depends-on` closure (self
/// EXCLUDED), files = its `impl` `.fs` in manifest order, read relative to the manifest dir.
let packageStage (pkg: string) : Stage =
    let m = loadManifest pkg
    let dir = packageDir pkg

    let files =
        ReferencedProject.implementationFiles m
        |> List.map (AssemblyFiles.SourceFile.read dir)

    {
        Name = m.Name
        Provider = composeProvider m.DependsOn
        Files = files
    }

/// A small hand-written consumer of Vesper.Set, the adversarial last stage of the chain,
/// kept CONSERVATIVE (list/primitive-based `Set` ops only; the `seq`-based members pull BCL
/// enumerables unresolvable under `noPlatformMetadata`). This is the one stage NOT proven green by
/// an existing suite — the setup guard fails loudly if it regresses. Expand it toward the
/// caching-adversarial shape (repeated same-type generic instantiations / SRTP dispatch)
/// once the baseline is captured.
let private syntheticSource =
    """module Bench.Synthetic

open Vesper.Collections

let build (xs: int list) : Set<int> = Set.ofList xs

let has (x: int) (s: Set<int>) : bool = Set.contains x s

let size (s: Set<int>) : int = Set.count s

let combine (a: Set<int>) (b: Set<int>) : Set<int> = Set.union a b

let sum (s: Set<int>) : int = Set.fold (fun acc x -> acc + x) 0 s
"""

/// The synthetic consumer as its own assembly, resolved against Set's FULL contract
/// (Set itself INCLUDED — it is external to this assembly), which pulls Set's transitive
/// closure (Core, List, …) via `composeContract`.
let syntheticStage () : Stage =
    {
        Name = "Bench.Synthetic"
        Provider = composeProvider [ "Vesper.Set" ]
        Files = [ AssemblyFiles.SourceFile.ofText "synthetic.fs" syntheticSource ]
    }

/// The chain-prefix size axis: how many packages of the chain to analyse in one run.
type ChainDepth =
    | Core = 0
    | CoreList = 1
    | CoreListSet = 2
    | CoreListSetSynthetic = 3

/// The stages to analyse (in dependency order) for a chain depth. Building a stage forces
/// its provider composition, so call this in `[<GlobalSetup>]`, never in the measured body.
let stagesFor (depth: ChainDepth) : Stage list =
    match depth with
    | ChainDepth.Core -> [ packageStage "Vesper.Core" ]
    | ChainDepth.CoreList -> [ packageStage "Vesper.Core"; packageStage "Vesper.List" ]
    | ChainDepth.CoreListSet ->
        [
            packageStage "Vesper.Core"
            packageStage "Vesper.List"
            packageStage "Vesper.Set"
        ]
    | ChainDepth.CoreListSetSynthetic ->
        [
            packageStage "Vesper.Core"
            packageStage "Vesper.List"
            packageStage "Vesper.Set"
            syntheticStage ()
        ]
    | other -> failwithf "SemanticAnalysisFixtures: unknown chain depth %A" other

/// Analyse one stage, returning every file's result. `analyse` is a seam so the probe can
/// inject a timing wrapper.
let analyseStage (analyse: AssemblyFiles.AnalyseFile) (s: Stage) =
    AssemblyFiles.analyseAssemblyWith
        analyse
        {
            Name = AssemblyName s.Name
            Target = Target.Clr
        }
        s.Provider
        Set.empty
        (s.Files |> List.map AssemblyFiles.SourceUnit.ofImplementation)

/// Count error-severity diagnostics across a stage's results (parse failures + analysis
/// errors). The green-workload guard: a bench on an erroring workload measures the error
/// path, so a non-zero count is a setup crash, not a silent number.
let stageErrorCount (results: Result<AssemblyFiles.FrozenFile, AssemblyFiles.UnparsedFile> list) : int =
    results
    |> List.sumBy (
        function
        | Error e ->
            e.Failure.Diagnostics
            |> List.filter (fun d -> d.Severity = Severity.Error)
            |> List.length
        // A file that parsed only because RECOVERY patched it is not a green workload
        // either, so its parse diagnostics count the same as the analysis residue.
        | Ok u ->
            u.ParseDiagnostics @ u.Frozen.Residue.Diagnostics
            |> List.filter (fun d -> d.Severity = Severity.Error)
            |> List.length
    )
