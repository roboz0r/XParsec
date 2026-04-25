# Writing an F# Parser in F#

A blog series tracing the development of `XParsec.FSharp`: an F#-language parser built in F# on top of the [XParsec](../README.md) parser combinator library.

The series follows the `fsharp` branch's commit history — roughly two hundred and fifty commits — and stops at each of the genuine inflection points: the decisions, surprises, and refactors that shaped the parser.

Each post is a self-contained outline: a hook, the key commits and files, the code to feature, and the takeaway. They're written to be readable linearly, but each can stand on its own.

## Posts

1. [Prologue: Why build an F# parser with XParsec?](01-prologue.md)
2. [Building the lexer: tokens, strings, fused operators](02-lexer.md)
3. [Lexing the awkward bits: interpolated strings](03-string-lexing.md)
4. [The lexical filter: one invisible pass behind the parsers](04-lexical-filter.md)
5. [Pratt parsing for F#'s operator zoo](05-pratt.md)
6. [The offside rule, part 1 — context stacks](06-offside-context-stacks.md)
7. [The offside rule, part 2 — allowed undentations](07-offside-undentations.md)
8. [AST design through consolidation](08-ast-consolidation.md)
9. [Error recovery you can actually ship](09-error-recovery.md)
10. [The corpus test](10-corpus-test.md)
11. [Taming the stack](11-taming-the-stack.md)
12. [LLM-assisted parser development](12-llm-assisted-development.md)
13. Performance Tuning

## Bonus

- [Surprises in the F# grammar](bonus-grammar-surprises.md) — the greatest hits of "wait, *that's* legal syntax?"
