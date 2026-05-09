<div align="center">
  <h1 align="center">
    <img alt="XParsec logo" src="docs/images/logo.svg" title="Logo" align="center"/>
    XParsec
  </h1>
</div>
<br/>

[![Documentation](https://img.shields.io/badge/see_the-docs-blue?style=flat)][DocsLink]
[![NuGet Version](https://img.shields.io/nuget/v/XParsec)][NugetLink]

XParsec is a parser combinator library for F#, with several important differences from [FParsec](https://github.com/stephan-tolksdorf/fparsec):

- Generalization over collection and token types

With XParsec all common contiguous collections `string` `'T array` `ResizeArray<'T>` `ImmutableArray<'T>` and `ReadOnlyMemory<'T>` can be parsed with essentially the same code.

- Pure F# implementation

F# is a great .NET language, and with the [Fable compiler](https://fable.io/), a powerful JavaScript language too. XParsec's pure F# implementation provides a robust, easy to use parsing library for Fable target languages.

- More Performant

XParsec uses newer F# & .NET features like `[<InlineIfLambda>]`, `Span<'T>`, and `struct` unions to compete with imperative parsing libraries while remaining terse and easy to reason about. Parsing a single large JSON file takes roughly half the time of FParsec with ~1/6 the allocations.

| Method      | Mean     | Error    | StdDev   | Gen0      | Gen1     | Gen2     | Allocated |
|------------ |---------:|---------:|---------:|----------:|---------:|---------:|----------:|
| XParsecJson | 30.61 ms | 0.196 ms | 0.164 ms |  718.7500 | 625.0000 |        - |  35.61 MB |
| FParsecJson | 61.09 ms | 0.267 ms | 0.208 ms | 4555.5556 | 777.7778 | 222.2222 | 208.37 MB |

- Simplified operator precedence parsing
- No line number tracking by default. A separate line ending parser is available for generating detailed error messages.
- `<|>`, `choice`, and `choiceL` always backtrack. There is no `attempt` combinator because every alternative already behaves like one. See [Migrating from FParsec](#migrating-from-fparsec) below.

## Detailed Error Messages

```log
The quick brown fox jumps over the lazy dog.
    ^ At index 4 (Ln 1, Col 5)
All choices failed.
├───Expected 'a'
└───All choices failed.
    ├───Unexpected 'q'
    └───Expected 'c'
```

## Real-world usage

XParsec is capable of parsing extremely complex grammars, including F# itself. I'm documenting building a complete F# language parser with XParsec in an ongoing blog series, starting with [the prologue](https://robertlenders.com/blog/xparsec-01-prologue).

## Migrating from FParsec

XParsec is API-shaped to be familiar to FParsec users, but a few semantics are deliberately different. Two are worth calling out:

- **`<|>`, `choice`, and `choiceL` always backtrack.** FParsec's alternative combinators only continue to the next branch if the previous branch failed without consuming input; you wrap branches in `attempt` to force backtracking after partial consumption. XParsec's alternatives save the reader position before running each branch and restore it on any failure. **There is no `attempt` combinator** — drop `attempt` calls when migrating; every alternative already behaves like one. The save is a struct copy, so the always-backtrack default is cheap.
- **`pzero` produces a structural `Empty` error**. Aggregating combinators (`<|>`, `choice`, `manyTill`, …) filter `Empty` siblings before constructing nested errors, so `pzero <|> p` propagates only `p`'s error rather than wrapping a blank stub. The default formatter renders nothing for `Empty`.

If you need "fail in place when input was consumed" behaviour for a specific alternative, use `notFollowedBy`/`<?>` to gate or relabel rather than reaching for `attempt`.

## Running Tests

### .NET

```pwsh
dotnet test
```

### Fable JS

```pwsh
npm run test
```

[NugetLink]: https://www.nuget.org/packages/XParsec
[DocsLink]: https://roboz0r.github.io/XParsec/
