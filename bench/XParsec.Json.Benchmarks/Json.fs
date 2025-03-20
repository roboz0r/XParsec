module XParsec.Json.Benchmarks.Json

open System
open System.Net.Http

open BenchmarkDotNet.Attributes

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Json


[<MemoryDiagnoser>]
type ParsingLargeJson() =

    let client = new HttpClient()

    let source =
        "https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json"

    let content = (client.GetStringAsync source).Result

    [<Benchmark>]
    member __.XParsecJson() =
        let reader = Reader.ofString content ()
        let p = JsonParsers.Parser
        let result = p reader
        ()

    [<Benchmark>]
    member __.FParsecJson() =
        let p = FParsecJsonParsers.Parser
        let result = FParsec.CharParsers.runParserOnString p () "Name" content
        ()

    interface IDisposable with
        member __.Dispose() = client.Dispose()
