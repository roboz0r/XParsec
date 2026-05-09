<div align="center">
  <h1 align="center">
    <img alt="XParsec logo" src="docs/images/logo.svg" title="Logo" align="center"/>
    XParsec
  </h1>
</div>
<br/>

[![Documentation](https://img.shields.io/badge/see_the-docs-blue?style=flat)][DocsLink]
[![NuGet Version](https://img.shields.io/nuget/v/XParsec)][NugetLink]

XParsec is a parser combinator library for F#

It aims to be a successor to the popular [FParsec](https://github.com/stephan-tolksdorf/fparsec) library with several important differences:

- Generalization over collection and token types

With XParsec all common contiguous collections `string` `'T array` `ResizeArray<'T>` `ImmutableArray<'T>` and `ReadOnlyMemory<'T>` can be parsed with essentially the same code.

- Pure F# implementation

F# is a great .NET language but with the [Fable compiler](https://fable.io/), a powerful JavaScript language too. By implementing XParsec completely in F#, I aim to provide an equally robust and easy to use parsing library for Fable target languages.

- More Performant

By making use of newer F# & .NET technologies like `[<InlineIfLambda>]` `Span<'T>` and `struct` unions I aim to make XParsec competitive with imperative parsing libraries while remaining terse and easy to reason about.

Initial results are encoraging with roughly half the execution time and ~1/6 the allocations for the equivalent parser code parsing a single large json file.

| Method      | Mean     | Error    | StdDev   | Gen0      | Gen1     | Gen2     | Allocated |
|------------ |---------:|---------:|---------:|----------:|---------:|---------:|----------:|
| XParsecJson | 30.61 ms | 0.196 ms | 0.164 ms |  718.7500 | 625.0000 |        - |  35.61 MB |
| FParsecJson | 61.09 ms | 0.267 ms | 0.208 ms | 4555.5556 | 777.7778 | 222.2222 | 208.37 MB |

- Simplified operator precedence parsing
- No line number tracking by default. A separate line ending parser is available for generating detailed error messages.

```log
The quick brown fox jumps over the lazy dog.
    ^ At index 4 (Ln 1, Col 5)
All choices failed.
├───Expected 'a'
└───All choices failed.
    ├───Unexpected 'q'
    └───Expected 'c'
```

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
