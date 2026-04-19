module XParsec.FSharp.Benchmarks.EndToEndBenchmarks

open BenchmarkDotNet.Attributes

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

[<MemoryDiagnoser>]
type EndToEndBenchmarks() =

    let mutable source: string = null
    let mutable sourceText: ISourceText = Unchecked.defaultof<_>
    let mutable fcsChecker: FSharpChecker = Unchecked.defaultof<_>
    let mutable fcsOptions: FSharpParsingOptions = Unchecked.defaultof<_>

    let fileName = "bench.fs"

    [<Params(Fixtures.FixtureSize.Small, Fixtures.FixtureSize.Medium, Fixtures.FixtureSize.Large)>]
    member val Size = Fixtures.FixtureSize.Small with get, set

    [<GlobalSetup>]
    member this.Setup() =
        source <- Fixtures.load this.Size
        sourceText <- SourceText.ofString source
        fcsChecker <- FSharpChecker.Create()

        fcsOptions <-
            { FSharpParsingOptions.Default with
                SourceFiles = [| fileName |]
            }

    [<Benchmark(Baseline = true)>]
    member _.XParsec() =
        match Lexing.lexString source with
        | Error _ -> false
        | Ok lexed ->
            let reader = Reader.ofLexed lexed source Set.empty

            match FSharpAst.parse reader with
            | Ok _ -> true
            | Error _ -> false

    [<Benchmark>]
    member _.FCS() =
        let result =
            fcsChecker.ParseFile(fileName, sourceText, fcsOptions, cache = false)
            |> Async.RunSynchronously

        not result.ParseHadErrors
