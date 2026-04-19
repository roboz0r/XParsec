module XParsec.FSharp.Benchmarks.LexingBenchmarks

open BenchmarkDotNet.Attributes

open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

open XParsec.FSharp.Lexer

[<MemoryDiagnoser>]
type LexingBenchmarks() =

    let mutable source: string = null
    let mutable sourceText: ISourceText = Unchecked.defaultof<_>

    [<Params(Fixtures.FixtureSize.Small, Fixtures.FixtureSize.Medium, Fixtures.FixtureSize.Large)>]
    member val Size = Fixtures.FixtureSize.Small with get, set

    [<GlobalSetup>]
    member this.Setup() =
        source <- Fixtures.load this.Size
        sourceText <- SourceText.ofString source

    [<Benchmark(Baseline = true)>]
    member _.XParsec() =
        match Lexing.lexString source with
        | Ok lexed -> lexed.Tokens.Length
        | Error _ -> -1

    [<Benchmark>]
    member _.FCS() =
        let mutable count = 0
        FSharpLexer.Tokenize(sourceText, (fun _ -> count <- count + 1))
        count
