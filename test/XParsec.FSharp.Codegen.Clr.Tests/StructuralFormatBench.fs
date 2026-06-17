module XParsec.FSharp.Codegen.Clr.Tests.StructuralFormatBench

open System
open System.Diagnostics
open System.IO

// Real-IL before/after for the `%A` structural-printer render pass. Invoked off the
// test runner's entry point (`Program.fs`, `--bench-structural`) so it is NOT an
// Expecto test (no CI cost). BOTH engines are compiled THROUGH this repo's Codegen.Clr
// backend (`TestHelpers.compileStructuralEngine`) and driven via a bound `Func`
// delegate over the emitted `Vesper.StructuralPrinter.Print` — so the numbers reflect
// the ACTUAL emitted IL, not the fsc rendering:
//
//   * PooledBuffer (new) = the live `src/Vesper.Printf/structural-printer.fs`.
//   * StringConcat (old) = the frozen pre-rewrite `StructuralFormatBaseline.fs`.
//
// Run:  dotnet run -c Release --project test/XParsec.FSharp.Codegen.Clr.Tests -- --bench-structural

let private fixtures () : (string * obj) list =
    let deep =
        let rec nest n =
            if n <= 0 then box 0 else box [ nest (n - 1) ]

        nest 90

    [
        "wide", box [ for i in 1..100 -> i ]
        "deep", deep
        "largeMixed", box [ for i in 1..60 -> box (i, sprintf "s%d" i, [ i; i + 1; i + 2 ]) ]
    ]

/// (ns/op, bytes/op). Time = min over `batches` runs of `iters` (min is robust to GC
/// / scheduling noise); allocation = single clean batch via per-thread alloc counter.
let private measure (f: unit -> string) : float * int64 =
    let iters = 2000
    let batches = 8

    for _ in 1..500 do
        f () |> ignore

    let mutable best = Double.MaxValue

    for _ in 1..batches do
        let sw = Stopwatch.StartNew()

        for _ in 1..iters do
            f () |> ignore

        sw.Stop()
        best <- min best (sw.Elapsed.TotalNanoseconds / float iters)

    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
    let allocBefore = GC.GetAllocatedBytesForCurrentThread()

    for _ in 1..iters do
        f () |> ignore

    let allocAfter = GC.GetAllocatedBytesForCurrentThread()
    best, (allocAfter - allocBefore) / int64 iters

let run () : int =
    let here = __SOURCE_DIRECTORY__

    let liveSrc =
        File.ReadAllText(Path.Combine(here, "..", "..", "src", "Vesper.Printf", "structural-printer.fs"))

    let baseSrc = File.ReadAllText(Path.Combine(here, "StructuralFormatBaseline.fs"))

    printfn "Compiling both engines through Codegen.Clr (emitted IL) ..."
    let concat = TestHelpers.compileStructuralEngine "VesperEngineConcat" baseSrc
    let pooled = TestHelpers.compileStructuralEngine "VesperEnginePooled" liveSrc

    // Sanity: confirm the two emitted engines produce byte-identical output before
    // comparing their cost — a divergence would make the ratio meaningless.
    let mutable mismatch = false

    for name, v in fixtures () do
        for w in [ 0; 80 ] do
            if concat.Invoke(v, w, 10000) <> pooled.Invoke(v, w, 10000) then
                mismatch <- true
                printfn "  OUTPUT MISMATCH: shape=%s width=%d" name w

    if mismatch then
        printfn "Aborting: engines disagree."
        1
    else

        printfn ""

        printfn
            "%-11s %5s | %14s %12s | %14s %12s | %7s %7s"
            "Shape"
            "Width"
            "old ns/op"
            "old B/op"
            "new ns/op"
            "new B/op"
            "time"
            "alloc"

        printfn "%s" (String('-', 92))

        for name, v in fixtures () do
            for w in [ 0; 80 ] do
                let tOld, aOld = measure (fun () -> concat.Invoke(v, w, 10000))
                let tNew, aNew = measure (fun () -> pooled.Invoke(v, w, 10000))

                printfn
                    "%-11s %5d | %11.0f ns %10d B | %11.0f ns %10d B | %6.2fx %6.2fx"
                    name
                    w
                    tOld
                    aOld
                    tNew
                    aNew
                    (tNew / tOld)
                    (float aNew / float aOld)

        0
