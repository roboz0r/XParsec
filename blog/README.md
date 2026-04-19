# Writing an F# Parser in F#

A blog series tracing the development of `XParsec.FSharp`: an F#-language parser built in F# on top of the [XParsec](../README.md) parser combinator library.

The series follows the `fsharp` branch's commit history — roughly two hundred and fifty commits — and stops at each of the genuine inflection points: the decisions, surprises, and refactors that shaped the parser.

Each post is a self-contained outline: a hook, the key commits and files, the code to feature, and the takeaway. They're written to be readable linearly, but each can stand on its own.

## Posts

1. [Prologue: Why build an F# parser with XParsec?](01-prologue.md)
2. [Building the lexer: tokens, strings, fused operators](02-lexer.md)
3. [The lexical filter: one invisible pass behind the parsers](03-lexical-filter.md)
4. [Pratt parsing for F#'s operator zoo](04-pratt.md)
5. [The offside rule, part 1 — context stacks](05-offside-context-stacks.md)
6. [The offside rule, part 2 — allowed undentations](06-offside-undentations.md)
7. [Interpolated strings and contextual lexing](07-interpolated-strings.md)
8. [AST design through consolidation](08-ast-consolidation.md)
9. [Error recovery you can actually ship](09-error-recovery.md)
10. [The corpus test: where theory meets reality](10-corpus-test.md)
11. [Taming the stack: tracing, probing, and a second operator rewrite](11-taming-the-stack.md)
12. [Hardening the ParserCE](12-parser-ce.md)
13. [LLM-assisted parser development: a harness that pays off](13-llm-assisted-development.md)

## Bonus

- [Surprises in the F# grammar](bonus-grammar-surprises.md) — the greatest hits of "wait, *that's* legal syntax?"
