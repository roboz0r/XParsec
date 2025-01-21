# XParsec

XParsec is a parser combinator library for F#

It aims to be a successor to the popular [FParsec](https://github.com/stephan-tolksdorf/fparsec) library with several important differences:

- Generalization over collection and token types

With XParsec all common contiguous collections `string` `'T array` `ResizeArray<'T>` `ImmutableArray<'T>` and `Stream` can be parsed with essentially the same code.

- Pure F# implementation

F# is a great .NET language but with the power of Fable, a powerful JavaScript language too. By implementing XParsec in completely in F#, I aim to provide an equally robust and easy to use parsing library for Fable target languages.

- More Performant

By making use of newer F# & .NET technologies like `[<InlineIfLambda>]` `Span<'T>` and `struct` unions I aim to make XParsec competitive with imperative parsing libraries while remaining terse and easy to reason about.

- Simplified operator precedence parsing
- No line number tracking by default

## TODO

- [ ] Fable compatibility
- [ ] Performance benchmarks and optimization
- [ ] Complete FParsec API coverage
- [ ] Tests with complex grammars
- [ ] Improvements to error messages
- [ ] `ByteParsers` module
- [ ] Release to NuGet
