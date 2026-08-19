/// The frozen WIRE format's load/store cost — the gate for whether an mmap'd blob is worth
/// building.
///
/// The claim under test is narrow and falsifiable: the frozen form is flat 4-byte columns
/// (`TypeId`, `Anchor`, CSR child ids), so a consumer reading a blob "pays for the ids at load"
/// and an mmap'd tier would stop paying. That is only worth the format churn — a raw, aligned,
/// section-headed blob in place of today's Brotli-wrapped byte-packed one — if column decode is
/// a visible share of what a load actually costs. So the four stages are timed APART rather
/// than as one round-trip number:
///
///   store = `flatten` + `compress`
///   load  = `decompress` + `thaw`      (what mmap would attack)
///
/// `thaw` is the only stage an mmap tier changes, and only the blittable part of it: the
/// payload columns, the type rows, the `DenseTable`s and the whole `Residue` decode
/// element-by-element either way.
///
/// **Measured, and the answer was no** (ShortRun, in-process, `CoreListSet`; `FullChain` from
/// `SemanticAnalysisBenchmarks` is the same workload's analysis, for scale):
///
///     FullChain           26418 us   34.8 MB
///     Flatten             ~1478 us    1587 KB   (noisy - nothing is read from it)
///     Compress             2112 us      96 KB
///     Decompress            599 us     585 KB
///     Thaw                  661 us     605 KB
///     LoadRoundTrip        1399 us    1191 KB
///
/// `Thaw` is **1.6% of the analysis a loaded tree stands in for**, and mmap improves only a
/// fraction of that — so no mmap'd tier is worth a raw, aligned, section-headed blob.
///
/// The one visible cost is `Compress`, on the STORE side: five times `Thaw`, the largest single
/// stage anywhere, ~8% on top of the analysis, and pure Brotli — untouched by the column work
/// and untouchable by an mmap tier. If this path is ever worth more work the lever is the
/// `Compression` seam (which documents itself as swappable), not the id encoding. That is a
/// trade and not a free win: storing raw deletes `Compress` AND `Decompress` (~2.7 ms of the
/// ~4.6 ms round trip) at the cost of the blob size the hash-consing and CSR work earned.
/// Measure that before taking it.
///
/// Real self-host files, not toy programs: `FrozenBlobSizeTests` already covers small shapes
/// for SIZE, and a 400-byte blob cannot tell you anything about load. `[<MemoryDiagnoser>]`
/// because `thaw` is an allocation story before it is a time story — it materialises every
/// column array and every payload object.
///
/// `[<Config InProcessConfig>]` is REQUIRED for the same reason as
/// `SemanticAnalysisBenchmarks` — see `BenchConfig`.
module XParsec.FSharp.Benchmarks.FrozenCodecBenchmarks

open BenchmarkDotNet.Attributes

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Benchmarks.SemanticAnalysisFixtures

[<MemoryDiagnoser>]
[<Config(typeof<BenchConfig.InProcessConfig>)>]
type FrozenCodecBenchmarks() =

    /// Every file of the chain, frozen once in setup. The front end is what this format
    /// exists to AVOID re-running, so it must not be in any measured body.
    let mutable frozen: FrozenPools list = []

    /// The same files already flattened, and already compressed — so the load-side
    /// benchmarks measure a decode and not the encode that produced their input.
    let mutable raw: byte[] list = []
    let mutable compressed: byte[] list = []

    [<Params(ChainDepth.Core, ChainDepth.CoreListSet)>]
    member val Depth = ChainDepth.Core with get, set

    [<GlobalSetup>]
    member this.Setup() =
        frozen <-
            [
                for s in stagesFor this.Depth do
                    for r in analyseStage Pipeline.analyseFor s do
                        match r with
                        | Ok u -> u.Frozen
                        // A file that did not parse has no frozen tree to encode. The
                        // green-workload guard belongs to the analysis benchmark; here an
                        // unparsed file is simply not a codec input.
                        | Error _ -> ()
            ]

        // A codec benchmark over nothing measures nothing — fail loudly in setup rather
        // than report a fast zero.
        if List.isEmpty frozen then
            failwithf "FrozenCodecBenchmarks: chain depth %A froze no files" this.Depth

        raw <- frozen |> List.map FrozenCodec.flatten
        compressed <- raw |> List.map Compression.compress

    /// The store side, encode half: pools → bytes. Interns every payload type into the
    /// file's tables on the way (`writePools`' two-pass buffer), so this is not a pure
    /// serialise.
    [<Benchmark>]
    member _.Flatten() =
        let mutable acc = 0

        for f in frozen do
            acc <- acc + (FrozenCodec.flatten f).Length

        acc

    /// The store side, compress half. Only a MISS pays this.
    [<Benchmark>]
    member _.Compress() =
        let mutable acc = 0

        for b in raw do
            acc <- acc + (Compression.compress b).Length

        acc

    /// The load side, decompress half — the stage an mmap tier DELETES rather than speeds
    /// up, since a mapped blob cannot be Brotli.
    [<Benchmark>]
    member _.Decompress() =
        let mutable acc = 0

        for b in compressed do
            acc <- acc + (Compression.decompress b).Length

        acc

    /// The load side, decode half — the ONLY stage an mmap'd blob improves, and only for
    /// its blittable columns. This number against `Decompress` is the whole question.
    [<Benchmark>]
    member _.Thaw() =
        let mutable acc = 0

        for b in raw do
            acc <- acc + (FrozenCodec.thaw b).ExprPayloads.Length

        acc

    /// Both load stages together, for scale against `FullChain`: a wire format is worth
    /// reading from only where it lands far below the analysis that would rebuild the tree.
    [<Benchmark>]
    member _.LoadRoundTrip() =
        let mutable acc = 0

        for b in compressed do
            acc <- acc + (FrozenCodec.thaw (Compression.decompress b)).ExprPayloads.Length

        acc
