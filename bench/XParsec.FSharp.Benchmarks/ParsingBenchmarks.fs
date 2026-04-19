module XParsec.FSharp.Benchmarks.ParsingBenchmarks

open BenchmarkDotNet.Attributes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Measures parsing in isolation, starting from a pre-lexed token stream.
/// FCS has no public "parse pre-tokenized" entry point, so this benchmark
/// is XParsec-only (useful for tracking parser regressions across commits).
[<MemoryDiagnoser>]
type ParsingBenchmarks() =

    let mutable source: string = null
    let mutable lexed: Lexed = Unchecked.defaultof<_>

    [<Params(Fixtures.FixtureSize.Small, Fixtures.FixtureSize.Medium, Fixtures.FixtureSize.Large)>]
    member val Size = Fixtures.FixtureSize.Small with get, set

    [<GlobalSetup>]
    member this.Setup() =
        source <- Fixtures.load this.Size

        match Lexing.lexString source with
        | Ok l -> lexed <- l
        | Error e -> failwithf "Lexing failed during setup: %A" e

    [<Benchmark>]
    member _.XParsec() =
        let reader = Reader.ofLexed lexed source Set.empty
        FSharpAst.parse reader
