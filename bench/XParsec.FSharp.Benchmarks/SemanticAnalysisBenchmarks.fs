/// The macro-benchmark — the GATE for interning/caching.
/// Baseline the current engine over the realistic multi-file chain
/// (`SemanticAnalysisFixtures`), then re-run after interning+caching land: a cache that
/// does not measurably move wall/alloc here does not stay. `MemoryDiagnoser` because the
/// interning thesis is primarily an ALLOCATION-reduction thesis (re-walked / re-thawed
/// immutable structure), so gen0/1/2 + bytes are the load-bearing signal.
///
/// `[<Config InProcessConfig>]` is REQUIRED, not decorative: an out-of-process runner
/// regenerates a csproj that inherits this repo's CPM FSharp.Core pin, which conflicts with
/// FCS's transitive requirement (NU1109). In-process side-steps that (see `BenchConfig`).
module XParsec.FSharp.Benchmarks.SemanticAnalysisBenchmarks

open BenchmarkDotNet.Attributes

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Benchmarks.SemanticAnalysisFixtures

[<MemoryDiagnoser>]
[<Config(typeof<BenchConfig.InProcessConfig>)>]
type SemanticAnalysisBenchmarks() =

    let mutable stages: Stage list = []

    [<Params(ChainDepth.Core, ChainDepth.CoreList, ChainDepth.CoreListSet, ChainDepth.CoreListSetSynthetic)>]
    member val Depth = ChainDepth.Core with get, set

    [<GlobalSetup>]
    member this.Setup() =
        // Provider composition + source reads are heavy and MUST be out of the measured
        // body — only the `analyseAssemblyWith` chain is timed.
        stages <- stagesFor this.Depth

        // Green-workload guard: a bench on an erroring workload measures the error path.
        // Any error-severity diagnostic in the active prefix is a setup crash. (Because the
        // guard scopes to the current `Depth`, iterating on the not-yet-proven synthetic
        // tail never blocks the Core/List/Set baselines — just omit that param.)
        for s in stages do
            let errs = analyseStage Pipeline.analyseForSelfHost s |> stageErrorCount

            if errs > 0 then
                failwithf
                    "SemanticAnalysisBenchmarks: stage '%s' produced %d error(s) — not a green workload"
                    s.Name
                    errs

    /// The whole compilation through the SA-only multi-file seam (`AssemblyFiles`), through
    /// Freeze, no codegen. Parse is inside the seam and thus in the measured body — it is a
    /// fixed cost that cancels in the before/after diff; the `InternProbe` timing wrapper
    /// isolates SA-from-parse when the per-pass split is needed.
    [<Benchmark>]
    member _.FullChain() =
        let mutable acc = 0

        for s in stages do
            acc <- acc + List.length (analyseStage Pipeline.analyseForSelfHost s)

        acc
