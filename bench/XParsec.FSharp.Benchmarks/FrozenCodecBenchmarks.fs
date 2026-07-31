/// The frozen-format LOAD/STORE benchmark — the gate for whether an mmap'd spine is worth
/// building.
///
/// The claim under test is narrow and falsifiable: the frozen spine is flat 4-byte columns
/// (`TypeId`, `Anchor`, CSR child ids), so a cached compile "pays for the ids at load" and an
/// mmap'd tier would stop paying. That is only worth the format churn — a raw, aligned,
/// section-headed blob in place of today's Brotli-wrapped byte-packed one — if column decode
/// is a visible share of what a HIT actually costs. So the four stages a hit and a miss are
/// made of are timed APART rather than as one `freeze` number:
///
///   miss = `flatten` + `compress`      (the store side)
///   hit  = `decompress` + `thaw`       (the load side — what mmap would attack)
///
/// `thaw` is the only stage an mmap tier changes, and only the blittable part of it: the
/// payload columns, the type rows, the `DenseTable`s and the whole `Residue` decode
/// element-by-element either way.
///
/// **Measured, and the answer was no** (ShortRun, in-process, `CoreListSet`; `FullChain` from
/// `SemanticAnalysisBenchmarks` is the same workload's analysis, the thing a hit replaces):
///
///     FullChain           26418 us   34.8 MB
///     Flatten             ~1478 us    1587 KB   (noisy - nothing is read from it)
///     Compress             2112 us      96 KB
///     Decompress            599 us     585 KB
///     Thaw                  661 us     605 KB
///     LoadHit              1399 us    1191 KB
///     CompilationDigest    3359 us    1582 KB   <- the KEY, per COMPILATION
///     FileKey                23 us      89 KB   <- the key, per file (~0.25 us each)
///
/// `Thaw` is **1.6% of the analysis it replaces**, and mmap improves only a fraction of that
/// — so no mmap'd spine is worth a raw, aligned, section-headed blob. A hit is ~24x faster
/// and ~29x leaner than analysing, which is what makes the cache worth having at all.
///
/// The one visible cost is `Compress`, on the MISS path: five times `Thaw`, the largest single
/// stage anywhere, ~8% on top of the analysis, and pure Brotli — untouched by the column work
/// and untouchable by an mmap tier. If this path is ever worth more work the lever is the
/// `Compression` seam (which documents itself as swappable), not the id encoding. That is a
/// trade and not a free win: storing raw deletes `Compress` AND `Decompress` (~2.7 ms of the
/// ~4.6 ms round trip) at the cost of the blob size the hash-consing and CSR work earned.
/// Measure that before taking it.
///
/// **The KEY is timed here too, and it is why `Hashing` splits into two lifetimes.** A hit is
/// only a saving net of what it costs to ask for one, and asking is not free: a cache key
/// covers every source file in the referenced-package closure, so computing one reads them.
///
/// The measurement above settles it. **`CompilationDigest` is 3359 us — 2.4x `LoadHit`, and it
/// allocates as much as `Flatten` does.** Folded per FILE it would cost more than twice the hit
/// it enables and 13% of the analysis a hit elides, turning a 19x saving into a 5x one. Folded
/// per COMPILATION it is amortised to nothing, and what a file then pays is `FileKey`:
/// **~0.25 us, four orders of magnitude below the digest**, because it touches no disk at all.
///
/// So the split is not tidiness. It is the difference between a cache that pays for itself per
/// file and one that spends most of what it saves re-reading its dependencies to ask whether it
/// may. `CompilationDigest` is flat across `Depth` — it is the same `Vesper.Set` closure either
/// way — which is exactly the shape of a cost that belongs to the compilation and not the file.
///
/// Real self-host units, not toy programs: `FrozenBlobSizeTests` already covers small shapes
/// for SIZE, and a 400-byte blob cannot tell you anything about load. `[<MemoryDiagnoser>]`
/// because `thaw` is an allocation story before it is a time story — it materialises every
/// column array and every payload object.
///
/// `[<Config InProcessConfig>]` is REQUIRED for the same reason as
/// `SemanticAnalysisBenchmarks` — see `BenchConfig`.
module XParsec.FSharp.Benchmarks.FrozenCodecBenchmarks

open BenchmarkDotNet.Attributes

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Benchmarks.SemanticAnalysisFixtures

[<MemoryDiagnoser>]
[<Config(typeof<BenchConfig.InProcessConfig>)>]
type FrozenCodecBenchmarks() =

    /// Every unit of the chain, frozen once in setup. The front end is what this format
    /// exists to AVOID re-running, so it must not be in any measured body.
    let mutable frozen: FrozenPools list = []

    /// The same units already flattened, and already compressed — so the load-side
    /// benchmarks measure a decode and not the encode that produced their input.
    let mutable raw: byte[] list = []
    let mutable compressed: byte[] list = []

    /// The compilation the key benchmarks fold. `Vesper.Set` closes over List and Core, so
    /// this is the same package closure the chain's stages resolve against — the digest reads
    /// the sources this workload actually depends on rather than a synthetic set.
    ///
    /// Not `Depth`-dependent and so not in `[<GlobalSetup>]`: the closure is what a driver
    /// configures a compilation with, and it does not vary with how many of its packages this
    /// run happens to analyse.
    let keyInputs: Hashing.CompilationInputs =
        {
            HomeAssembly = "Bench"
            Target = None
            ReferenceAssemblies = []
            Manifests = [ manifestPath "Vesper.Set" ]
            SelfManifest = None
        }

    /// `keyInputs` already folded — what `FileKey` keys against, and what a driver holds for
    /// the life of a compilation.
    let keyDigest = Hashing.compilationDigest keyInputs

    /// Every unit's source text — what `FileKey` folds against the digest, one per file, which
    /// is how a driver actually asks the cache a question.
    let mutable sources: string list = []

    [<Params(ChainDepth.Core, ChainDepth.CoreListSet)>]
    member val Depth = ChainDepth.Core with get, set

    [<GlobalSetup>]
    member this.Setup() =
        frozen <-
            [
                for s in stagesFor this.Depth do
                    for r in analyseStage Pipeline.analyseForSelfHost s do
                        match r with
                        | Ok u -> u.Frozen
                        // A unit that did not parse has no frozen tree to encode. The
                        // green-workload guard belongs to the analysis benchmark; here an
                        // unparsed unit is simply not a codec input.
                        | Error _ -> ()
            ]

        // A codec benchmark over nothing measures nothing — fail loudly in setup rather
        // than report a fast zero.
        if List.isEmpty frozen then
            failwithf "FrozenCodecBenchmarks: chain depth %A froze no units" this.Depth

        raw <- frozen |> List.map FrozenCodec.flatten
        compressed <- raw |> List.map Compression.compress

        sources <-
            [
                for s in stagesFor this.Depth do
                    for (_, text) in s.Files -> text
            ]

    /// The store side, encode half: pools → bytes. Interns every payload type into the
    /// unit's tables on the way (`writePools`' two-pass buffer), so this is not a pure
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

    /// The load side, decode half — the ONLY stage an mmap'd spine improves, and only for
    /// its blittable columns. This number against `Decompress` is the whole question.
    [<Benchmark>]
    member _.Thaw() =
        let mutable acc = 0

        for b in raw do
            acc <- acc + (FrozenCodec.thaw b).ExprPayloads.Length

        acc

    /// The whole HIT path as `FrozenCache.freeze` runs it, for scale against `FullChain` —
    /// a hit is only worth having if it is far below the analysis it replaces.
    [<Benchmark>]
    member _.LoadHit() =
        let mutable acc = 0

        for b in compressed do
            acc <- acc + (FrozenCodec.thaw (Compression.decompress b)).ExprPayloads.Length

        acc

    /// The key's PER-COMPILATION half: stat + read + hash every source file the package
    /// closure names. Paid ONCE per compilation — and the number that says why it must be:
    /// against `FileKey` below it is the whole argument for `Hashing`'s two lifetimes, and
    /// against `LoadHit` it says how many files a compilation needs before a cache pays.
    [<Benchmark>]
    member _.CompilationDigest() = Hashing.compilationDigest keyInputs

    /// The key's PER-FILE half, against a digest already folded. Touches no disk — this is
    /// what a driver actually pays per file to ask the cache a question.
    [<Benchmark>]
    member _.FileKey() =
        let mutable acc = 0

        for s in sources do
            acc <- acc + (Hashing.fileInputHash (Hashing.textOriginPath s) s keyDigest).Hex.Length

        acc
