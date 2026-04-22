module XParsec.FSharp.Benchmarks.Program

open System.Diagnostics
open System.IO
open BenchmarkDotNet.Running

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let private parseSize (arg: string) =
    match arg.ToLowerInvariant() with
    | "small" -> Fixtures.FixtureSize.Small
    | "medium" -> Fixtures.FixtureSize.Medium
    | "large" -> Fixtures.FixtureSize.Large
    | s -> failwithf "Unknown size: %s (expected small|medium|large)" s

/// The loop that runs under `dotnet-trace collect`. Isolated as `--trace-child` so the
/// outer `--trace` command can spawn itself with tracing wrapped around only this work.
let private runTraceChild (size: Fixtures.FixtureSize) (iterations: int) =
    let source = Fixtures.load size

    let lexed =
        match Lexing.lexString source with
        | Ok l -> l
        | Error e -> failwithf "Lexing failed: %A" e

    // Warm-up so JIT / tiered compilation settles before the trace window.
    for _ in 1..5 do
        let reader = Reader.ofLexed lexed source Set.empty
        FSharpAst.parse reader |> ignore

    printfn "TRACE_BEGIN size=%A iterations=%d" size iterations
    let sw = Stopwatch.StartNew()

    for i in 1..iterations do
        let reader = Reader.ofLexed lexed source Set.empty
        FSharpAst.parse reader |> ignore

        if i % 50 = 0 then
            printfn "  iter %d / %d  (%dms elapsed)" i iterations sw.ElapsedMilliseconds

    sw.Stop()
    printfn "TRACE_END total=%dms avg=%.2fms" sw.ElapsedMilliseconds (float sw.ElapsedMilliseconds / float iterations)

/// Runs `dotnet <exe> <args>` inheriting this process's stdio, returns the exit code.
let private runTool (fileName: string) (args: string list) =
    let psi = ProcessStartInfo(fileName)
    psi.UseShellExecute <- false

    for a in args do
        psi.ArgumentList.Add(a)

    use proc = Process.Start(psi)
    proc.WaitForExit()
    proc.ExitCode

/// Outer `--trace`: wraps `dotnet-trace collect --profile gc-verbose` around a
/// `--trace-child` invocation of this same assembly, then calls the AllocAggregate
/// module in-process over the resulting .nettrace.
let private runTraceAndAggregate (size: Fixtures.FixtureSize) (iterations: int) =
    let selfDll = typeof<Fixtures.FixtureSize>.Assembly.Location

    // Put the trace file under the working directory — typically repo root when
    // the user invokes `dotnet run -- --trace`. Overrideable via TRACE_OUT env var.
    let outDir =
        match System.Environment.GetEnvironmentVariable("TRACE_OUT") with
        | null
        | "" -> Path.Combine(System.Environment.CurrentDirectory, "tmp")
        | p -> p

    Directory.CreateDirectory(outDir) |> ignore
    let sizeArg = size.ToString().ToLowerInvariant()
    let nettrace = Path.Combine(outDir, $"parse-alloc-{sizeArg}.nettrace")

    printfn "Collecting allocation trace → %s" nettrace

    let collectExit =
        runTool
            "dotnet"
            [
                "trace"
                "collect"
                "--profile"
                "gc-verbose"
                "--show-child-io"
                "--format"
                "Speedscope"
                "-o"
                nettrace
                "--"
                "dotnet"
                selfDll
                "--trace-child"
                sizeArg
                string iterations
            ]

    if collectExit <> 0 then
        failwithf "dotnet-trace collect exited with code %d" collectExit

    printfn ""
    printfn "Aggregating allocations ..."
    AllocAggregate.aggregate nettrace "XParsec"

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--trace"; sizeArg; iterArg |] ->
        runTraceAndAggregate (parseSize sizeArg) (int iterArg)
        0
    | [| "--trace-child"; sizeArg; iterArg |] ->
        runTraceChild (parseSize sizeArg) (int iterArg)
        0
    | [| "--aggregate-callers"; tracePath; substring |] ->
        AllocAggregate.aggregateCallers tracePath substring 20
        0
    | [| "--dump-stacks"; tracePath; substring; count |] ->
        AllocAggregate.dumpStacks tracePath substring (int count)
        0
    | [| "--aggregate-types"; tracePath; substring |] ->
        AllocAggregate.aggregateTypesAt tracePath substring 20
        0
    | [| "--aggregate-cpu"; tracePath |] ->
        AllocAggregate.aggregateCpu tracePath "XParsec"
        0
    | [| "--aggregate-cpu"; tracePath; filter |] ->
        AllocAggregate.aggregateCpu tracePath filter
        0
    | [| "--aggregate"; tracePath |] ->
        AllocAggregate.aggregate tracePath "XParsec"
        0
    | [| "--aggregate"; tracePath; filter |] ->
        AllocAggregate.aggregate tracePath filter
        0
    | _ ->
        BenchmarkSwitcher.FromAssembly(typeof<LexingBenchmarks.LexingBenchmarks>.Assembly).Run(argv)
        |> ignore

        0
