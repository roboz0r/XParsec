/// Dev-only instrumentation for the interning/caching go-no-go (docs/engine-rewrite-plan.md
/// "benchmarking"). NOT a shipped benchmark — a spike to measure the CEILINGS before any
/// cache is built, so a cache that cannot be shown to pay never lands:
///
///   * Pure analyse+freeze wall time per stage, ISOLATED from parse. `analyseAssemblyWith`
///     parses each unit before calling the front end, so wrapping the front-end seam
///     (`timed`) times SA only — the fair denominator for "how much could SA-internal
///     caching save".
///   * Redundancy ceilings via shadow counters (`hit`) keyed EXACTLY as a real cache would
///     key. A query family's max hit-rate is `1 - distinct/total`; if that is low on this
///     workload, no cache implementation for it pays. The invasive `find`/`zonk`/`subsumes`/
///     `matchTypes`/member-lookup probes are one `InternProbe.hit "<family>" <key>` call
///     each at those sites — wire them (behind this dev flag) when measuring the caching
///     ceiling; they are intentionally NOT applied here so the invasive edits are reviewed
///     against the current engine deliberately, not scattered blind.
module XParsec.FSharp.Benchmarks.InternProbe

open System.Collections.Generic
open System.Diagnostics
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Benchmarks.SemanticAnalysisFixtures

/// A shadow-cache hit counter for one query family: total calls and the DISTINCT keys
/// seen. `1 - distinct/total` is the ceiling on what memoising this family could save.
type private Bucket =
    {
        mutable Calls: int
        Distinct: HashSet<string>
    }

let private buckets = Dictionary<string, Bucket>()
let private stageMs = Dictionary<string, float>()
let private stageUnits = Dictionary<string, int>()

let private bump (d: Dictionary<string, 'v>) key (zero: 'v) (f: 'v -> 'v) =
    d.[key] <-
        f (
            match d.TryGetValue key with
            | true, v -> v
            | _ -> zero
        )

/// Record one query of `family` on `key` — call this at a candidate cache site (`find`,
/// `zonk`, `subsumes`, `matchTypes`, `tryClassByKey`, `TryLookupMemberByKey`). `key` must
/// spell the SAME identity the real cache would key on (interned id / structural key), or
/// the measured ceiling is meaningless.
let hit (family: string) (key: string) =
    let b =
        match buckets.TryGetValue family with
        | true, b -> b
        | _ ->
            let b =
                {
                    Calls = 0
                    Distinct = HashSet<string>()
                }

            buckets.[family] <- b
            b

    b.Calls <- b.Calls + 1
    b.Distinct.Add key |> ignore

/// Wrap an `AnalyseUnit` so each per-file front-end call (POST-parse) accumulates into
/// `family` — the parse cost is excluded, isolating SA.
let timed (family: string) (analyse: AssemblyUnits.AnalyseUnit) : AssemblyUnits.AnalyseUnit =
    fun asmName provider input lexed file ->
        let sw = Stopwatch.StartNew()
        let r = analyse asmName provider input lexed file
        sw.Stop()
        bump stageMs family 0.0 (fun v -> v + sw.Elapsed.TotalMilliseconds)
        bump stageUnits family 0 (fun v -> v + 1)
        r

let reset () =
    buckets.Clear()
    stageMs.Clear()
    stageUnits.Clear()

let report () =
    printfn ""
    printfn "== SA analyse+freeze time (parse excluded) =="

    for kv in stageMs do
        let units =
            match stageUnits.TryGetValue kv.Key with
            | true, u -> u
            | _ -> 0

        printfn "  %-22s %9.2f ms  %3d units" kv.Key kv.Value units

    if buckets.Count > 0 then
        printfn ""
        printfn "== Cache ceiling per query family (1 - distinct/total) =="
        printfn "  %-22s %10s %10s %9s" "family" "calls" "distinct" "ceiling"

        for kv in buckets do
            let b = kv.Value

            let ceiling =
                if b.Calls = 0 then
                    0.0
                else
                    1.0 - float b.Distinct.Count / float b.Calls

            printfn "  %-22s %10d %10d %8.1f%%" kv.Key b.Calls b.Distinct.Count (ceiling * 100.0)
    else
        printfn ""
        printfn "(no cache-ceiling counters wired — add InternProbe.hit calls at the candidate sites)"

/// Run the chain once (after a warm-up pass) through the timing wrapper and print the
/// probe report. The non-invasive baseline: total wall (incl parse) + per-stage SA time.
/// Wire `hit` at the engine sites to populate the cache-ceiling table.
let runChain (depth: ChainDepth) =
    let stages = stagesFor depth

    // Warm-up so JIT / tiered compilation settles before the measured pass. Not counted.
    for s in stages do
        analyseStage Pipeline.analyseForSelfHost s |> ignore

    reset ()
    let sw = Stopwatch.StartNew()
    let errorsByStage = ResizeArray<string * int>()

    for s in stages do
        let results = analyseStage (timed s.Name Pipeline.analyseForSelfHost) s
        errorsByStage.Add(s.Name, stageErrorCount results)

    sw.Stop()
    report ()

    printfn ""
    printfn "== Green-workload check (error-severity diagnostics) =="

    for (name, errs) in errorsByStage do
        printfn "  %-22s %d error(s)%s" name errs (if errs = 0 then "" else "  <-- NOT GREEN")

    printfn ""
    printfn "Total wall (incl parse): %.2f ms  [depth=%A]" sw.Elapsed.TotalMilliseconds depth
