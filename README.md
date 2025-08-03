<div align="center">
  <h1 align="center">
    <img alt="XParsec logo" src="docs/images/logo.svg" title="Logo" align="center"/>
    XParsec 
  </h1>

  [<img hspace="10px" alt="Static Badge" src="https://img.shields.io/badge/see_the-docs-blue?style=flat">](https://roboz0r.github.io/XParsec/) 
  ![NuGet Version](https://img.shields.io/nuget/v/XParsec?style=flat)

</div>
<br/>


XParsec is a parser combinator library for F#

It aims to be a successor to the popular [FParsec](https://github.com/stephan-tolksdorf/fparsec) library with several important differences:

- Generalization over collection and token types

With XParsec all common contiguous collections `string` `'T array` `ResizeArray<'T>` `ImmutableArray<'T>` and `Stream` can be parsed with essentially the same code.

- Pure F# implementation

F# is a great .NET language but with the power of Fable, a powerful JavaScript language too. By implementing XParsec in completely in F#, I aim to provide an equally robust and easy to use parsing library for Fable target languages.

- More Performant

By making use of newer F# & .NET technologies like `[<InlineIfLambda>]` `Span<'T>` and `struct` unions I aim to make XParsec competitive with imperative parsing libraries while remaining terse and easy to reason about.

Initial results are encoraging with roughly 2/3 the execution time and 1/4 the allocations for the equivalent parser code parsing a single large json file.

| Method      | Mean     | Error    | StdDev   | Gen0      | Gen1     | Gen2     | Allocated |
|------------ |---------:|---------:|---------:|----------:|---------:|---------:|----------:|
| XParsecJson | 41.40 ms | 0.205 ms | 0.182 ms | 1000.0000 | 916.6667 |        - |  50.91 MB |
| FParsecJson | 67.38 ms | 1.247 ms | 1.106 ms | 4375.0000 | 875.0000 | 250.0000 | 200.98 MB |

- Simplified operator precedence parsing
- No line number tracking by default

## Running Tests

### .NET

```pwsh
dotnet test
```

### Fable JS

```pwsh
npm run test
```

## TODO

- [x] `ByteParsers` module
- [x] Fable JS compatibility
- [ ] Other Fable targets compatibility
- [x] Multi-token operator parsing
- [ ] Performance benchmarks and optimization
- [ ] Complete FParsec API coverage
- [ ] Tests with complex grammars
- [ ] Improvements to error messages
- [ ] Release to NuGet
