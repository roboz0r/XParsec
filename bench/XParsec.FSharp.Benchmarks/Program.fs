module XParsec.FSharp.Benchmarks.Program

open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    BenchmarkSwitcher.FromAssembly(typeof<LexingBenchmarks.LexingBenchmarks>.Assembly).Run(argv)
    |> ignore

    0
