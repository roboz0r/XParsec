# 1. Prologue: Why build an F# parser with XParsec?

Every parser combinator library starts with a JSON example. XParsec did too: [a one-page recursive-descent parser](https://github.com/roboz0r/XParsec/blob/main/src/XParsec.Json/JsonParser.fs) small enough to fit on a slide.

[XParsec](https://github.com/roboz0r/XParsec) is my library. I'd spent more than a year on it, designing and redesigning the types and combinators until everything did exactly one thing, and a small but real F# community had started using it. The next milestone was 1.0, and I wasn't going to release 1.0 against a JSON example. Whether all that careful design held up to anything bigger than a configuration format was a question I had to answer for myself. Elegant I had. Real I wasn't sure of.

So the bar I set was F#. Not "some F#". The actual source of the F# compiler, 249 production files, clean, no diagnostics, no crashes. F# is written in F#; if a parser can hold up to the language's own self-hosting source, it can hold up to anything I would ever throw at it. Layout-sensitive grammar. Contextual lexing. Fused operators that split based on syntactic context. Preprocessor directives. Error recovery that keeps the rest of the file parseable after a broken declaration. A published specification the language has already outgrown.

Six months and over two hundred and fifty commits on the `fsharp` branch later, that bar got cleared. This series is what it took.

The short answer: parser combinators can parse real languages. If the project had failed, this series wouldn't exist. The longer answer, the one the series is actually about, is that **the combinators you start with are not the combinators you finish with**. Halfway through, I had to rewrite the operator parser inside XParsec itself; the rewrite shipped as XParsec 0.3, because F#'s operators are not a high school algebra problem. Months later I rewrote it a second time, same feature set but stack-bounded, after the first version blew a megabyte of stack on deeply chained expressions. The grammar pushed back into the library at every layer, and that pushback is what the series is for. Every post is also a note on which design assumption bent under the weight of a real language and which ones held.

If you came here for the parser, you'll get it: the lexer, the lexical filter, the offside rule, [Pratt](./Vaughan.Pratt.TDOP.pdf)-style operator precedence, interpolated strings, error recovery, AST design, a stack probe, a set of grammar surprises, and the LLM-assisted harness that closed out the long tail of bugs. If you came here for the library story, it runs underneath all of it.

## What's a parser combinator?

A parser combinator is a function with a contract: given an input position, it either succeeds with a parsed value (and a new position) or fails with an error. Once the shape is fixed, parsers compose. `parseDigit` followed by `parseDigit` becomes `parseTwoDigitNumber`. `parseToken "if"` followed by `parseExpression` followed by `parseToken "then"` followed by `parseExpression` becomes `parseIfThen`. Each combinator (`andThen`, `orElse`, `many`, `optional`) takes parsers and returns a new parser. The grammar of your language ends up looking like a transcription of its EBNF, written in your host language and type-checked by your host language's compiler.

That's the appeal compared to the alternatives. Regex is too weak; it can't recurse, can't track context, can't return a structured value. Parser generators (yacc, bison, ANTLR, FsLexYacc) are powerful but live in a separate file with a separate language and a separate build step, and their error messages point at generated code rather than your grammar. Hand-rolled recursive-descent parsers (what production compilers usually ship) are fast and flexible but every grammar rule is a new function, every backtrack is manual, and the only library helping you is the one you write yourself. Combinators sit in the middle: the expressive power of recursive descent, library-level support for the common patterns, no separate build step.

The price is performance. Naive combinator libraries allocate a closure per `bind` and a list per `many`, which is fine for configuration files and painful for source code. A lot of XParsec's design pressure is about paying that price down without losing the composition.

## Why another parser combinator library?

F# already has [FParsec](https://github.com/stephan-tolksdorf/fparsec), and FParsec is excellent. It was the inspiration for XParsec and the library I'd still point most F# developers to first. I started XParsec because I wanted four things FParsec wasn't going to give me without a substantial rewrite:

- **Generic input, not just characters.** FParsec parses characters from a `CharStream`. XParsec's reader is generic over the collection (`string`, `'T array`, `ResizeArray<'T>`, `ImmutableArray<'T>`, `ReadOnlyMemory<'T>`) and over the element type. The same combinators that parse characters can parse tokens, which is exactly what a real compiler pipeline needs, and what XParsec.FSharp does throughout.
- **Pure F#, Fable-compatible.** FParsec ships a mixture of F# and C#. XParsec is F# top to bottom, which means it can also be compiled by Fable: JavaScript compatibility is what I wanted. The same beautiful parsing code should run in your browser or node just as well as on .NET.
- **Performance from the type design.** On a large JSON file, XParsec runs in about 2/3 the time of FParsec and allocates roughly 1/5 as much. Most of the choices that look strange in isolation (struct readers, inlined lambdas, no implicit error materialization on the hot path) exist to keep it.
- **Pratt-style operator parsing as a first-class concept.** FParsec ships an `OperatorPrecedenceParser` that you build up imperatively. XParsec treats the operator table as data the parser is parameterized over. This is the piece that bent first under F#'s operator zoo and forced the 0.3 rewrite. Post 5.

A fifth thing, smaller but worth flagging: no line-number tracking by default. Source positions are indices; line and column are computed on demand from a `LineStarts` array only when a diagnostic actually needs to be rendered. For a parser that succeeds 99% of the time, this is free.

The honest answer underneath all of this is that I wanted to know if I could build it. FParsec is the work of one person, so is most of what I admire in this corner of the .NET ecosystem.

It’s also worth contrasting them with FsLexYacc.

## FsLexYacc

[FsLexYacc](https://fsprojects.github.io/FsLexYacc/) is the F# port of the venerable `lex` and `yacc` tools. Token rules live in a `.fsl` file. Grammar productions live in a `.fsy` file. A build-time tool reads both and emits the F# code that actually runs. A grammar fragment looks like this:

```fsy
typedSequentialExpr:
  | sequentialExpr COLON typeWithTypeConstraints
      { SynExpr.Typed($1, $3, unionRanges $1.Range $3.Range) }

  | sequentialExpr COLON recover
    { let mColon = rhs parseState 2
      let ty = SynType.FromParseError(mColon.EndRange)
      SynExpr.Typed($1, ty, unionRanges $1.Range mColon) }

  | sequentialExpr
    { $1 }
```

The generator produces a table-driven LALR(1) parser, the parsing strategy that powers most of the world's compiled languages. For a grammar that fits cleanly into LALR(1), the result is fast, deterministic, and well-understood.

The price is everything else. The `.fsl` and `.fsy` files are not F#; they're a domain-specific language with its own syntax, its own scoping rules, and almost no IDE support. There's no autocomplete in the action blocks, no go-to-definition through the generated code, and the official documentation hasn't substantially changed since the original Microsoft Research releases. When something breaks, you're typically debugging machine-generated code without a stable line mapping back to the grammar you wrote.

For the F# compiler team this is a reasonable tradeoff. The grammar is largely stable, the team is small and experienced, and the generator was the realistic option when the project started. For me, choosing FsLexYacc would have meant writing in two languages I didn't want to write in, and skipping the only test that actually mattered for XParsec.

## Success criteria

The bar I held the parser to, in concrete terms:

- **Coverage.** All 249 production files of the F# compiler source parse end-to-end. No file silently skipped, no file partially consumed.
- **No crashes.** No exceptions, no infinite loops, no stack overflows. The parser must run to completion on every file in the corpus.
- **Faithful AST.** The output preserves the syntactic information needed to reconstruct the source: tokens, trivia, accurate source ranges. Range accuracy in particular is a known historical pain point for F# Compiler Service (FCS) backed tooling, and one I wanted XParsec.FSharp to get right from the start. No silent normalization, no information loss the user didn't ask for.
- **No diagnostics on valid input.** Zero parse errors against source the F# compiler itself accepts. If the compiler is happy with a file, so must this parser.
- **Error recovery.** When given invalid input, the parser produces an error node and continues, so the rest of the file remains analyzable rather than discarded at the first broken declaration.

Performance, what counts as "fast enough", I deliberately left out of the success bar. Correctness first, then numbers. The benchmarks below are the starting line, not the goal.

**Parser combinators can parse real languages**. The trick is knowing which parts of the grammar deserve their own primitives, and that's the rest of the series.

Next, in Part 2, we dive deep into the lexer.

---

## Benchmarks at the starting line

> **First-pass benchmarks.** On a large F# source file, XParsec.FSharp's lexer runs in roughly half the time of the F# Compiler Service's lexer (32 ms vs 65 ms) while allocating about 4× as much memory. End-to-end (lex + parse), XParsec is ~2× slower than FCS and allocates ~12× more. No profiling or tuning has been done yet. The lexer's lead is essentially free, and closing the parser's allocation gap is what the perf post will report on. Raw BenchmarkDotNet output below.

Run with BenchmarkDotNet v0.15.8 on .NET 10, ShortRun job (3 iterations, 3 warmup, in-process). `Large` is the F# compiler's `prim-types.fs`: a 7000+ line file that leans heavily on internal and obscure F# language features and is a worst-case stress test for any F# parser.

### End-to-end (lex + parse)

| Method  | Size   | Mean         | Ratio | Allocated    | Alloc Ratio |
|-------- |------- |-------------:|------:|-------------:|------------:|
| XParsec | Small  |     904.3 us |  1.00 |   1784.26 KB |        1.00 |
| FCS     | Small  |   1,035.6 us |  1.15 |    408.70 KB |        0.23 |
| XParsec | Medium |   5,896.2 us |  1.00 |  12328.83 KB |        1.00 |
| FCS     | Medium |   5,204.5 us |  0.88 |   2302.58 KB |        0.19 |
| XParsec | Large  | 112,236.0 us |  1.00 | 172308.18 KB |        1.00 |
| FCS     | Large  |  57,821.2 us |  0.52 |  13734.39 KB |        0.08 |

### Lexing only

| Method  | Size   | Mean        | Ratio | Allocated   | Alloc Ratio |
|-------- |------- |------------:|------:|------------:|------------:|
| XParsec | Small  |    261.6 us |  1.00 |   574.53 KB |        1.00 |
| FCS     | Small  |    693.5 us |  2.65 |   194.51 KB |        0.34 |
| XParsec | Medium |  2,703.9 us |  1.00 |  6258.93 KB |        1.00 |
| FCS     | Medium |  3,820.8 us |  1.41 |  1162.23 KB |        0.19 |
| XParsec | Large  | 32,160.0 us |  1.00 | 54631.74 KB |        1.00 |
| FCS     | Large  | 64,596.2 us |  2.01 | 12485.21 KB |        0.23 |

### Parsing only (XParsec)

| Method  | Size   | Mean        | Allocated |
|-------- |------- |------------:|----------:|
| XParsec | Small  |    636.7 us |   1.18 MB |
| XParsec | Medium |  3,784.7 us |   5.93 MB |
| XParsec | Large  | 83,242.3 us | 114.92 MB |
