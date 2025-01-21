module XParsec.Json.Benchmarks.Program

open BenchmarkDotNet.Running
open System.Runtime.InteropServices

open Json

[<EntryPoint>]
let main _ =
    let _ = BenchmarkRunner.Run<ParsingLargeJson>()
    0
