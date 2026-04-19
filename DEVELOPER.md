# XParsec Developer Guide

## Project Overview

**XParsec** is a parser combinator library for .NET. The core `Parser` type is a function `Reader -> ParseResult`.
**XParsec.FSharp** (`src/XParsec.FSharp/`) is an F# language parser/lexer built on XParsec. It lexes source into tokens, processes compiler directives, applies offside (indentation) rules, and parses the stream into an AST.

```text
Source ──> [Lexing] ──> [Preprocessing (#if)] ──> [Parsing (AST)] ──> [Debug Output]
```

The **xparsec-dev** skill is provided to ease everyday development.

## Architecture & Compilation Order

The parser forms a strict **Directed Acyclic Graph (DAG)**. There are no recursive namespaces (`namespace rec`). Files in `XParsec.FSharp.fsproj` MUST remain in exact compilation order to satisfy F#'s strict top-to-bottom type resolution. The architectural layers are strictly:

1. **Foundation & Lexing:** Utilities, Tokens, Lexer (`Lexing.fs`)
2. **AST Data Types:** `Expr.fs`, `Declarations.fs`, etc.
3. **Parsing State:** `ParsingTypes.fs`, `Preprocessing.fs`, `ParsingHelpers.fs`
4. **Bottom-up Combinators:** Leaf parsers -> Expressions -> Type Definitions -> Declarations

## Key Patterns & Conventions

### Breaking Cycles with `ParserRefs.fs`

F# is mutually recursive, but our files are a strict DAG. We use the **RefParser** pattern to break dependencies:

1. `ParserRefs.fs` creates empty references early: `let refExpr = RefParser<Expr<...>>()`
2. Lower-level parsers use `refExpr.Parser` before the actual expression parser exists.
3. Higher-level parsers implement the logic and bind it at the bottom of their file: `do refExpr.Set Expr.parse`

### ParsingHelpers & Token Consumption

Because fetching the next token is computationally expensive (due to trivia skipping and `#if` preprocessing), use the patterns in `ParsingHelpers.fs`:

- **Peek and Consume:** Use `peekNextNonTriviaToken` to evaluate the upcoming token. If matching, use `consumePeeked` to advance the reader.
- **Fast Dispatch:** Use `dispatchNextNonTriviaTokenL` instead of large `choice` combinators for O(1) branching on the next token.
- **Virtual Tokens:** F# offside rules allow omitted delimiters. Use `nextNonTriviaTokenVirtualIfNot` (synthesizes missing structural tokens like `in`) or `nextNonTriviaTokenVirtualWithDiagnostic` (synthesizes missing required tokens like `)` while emitting an error).
- **Context Management:** Use `withContext` to establish new indentation boundaries. It manages the `OffsideContext` stack safely.
- **Error Recovery:** Wrap fallible constructs in `recoverWith`. On failure, it backtracks, emits a diagnostic, and skips tokens until a safe synchronization point (e.g., `;;` or `end`).

### Prefer Statically-Allocated Parser Values

If a parser does not close over local variables, define it as a **value** rather than a function to allocate the closure once at startup.
**Prefer:** `let peekNonTriviaIndent = lookAhead (fun r -> ...)`
**Avoid:** `let peekNonTriviaIndent reader = lookAhead (fun r -> ...) reader`

### Operator Precedence Parsing (Pratt Parsing)

Used for expressions, patterns, measures, and types. `PrecedenceLevel` converts to `BindingPower`. Handled via the `Operators` interface.

### Automatic Backtracking

`XParsec` performs automatic backtracking on `choice` and `choiceL` after failed alternatives. There is typically no need for an `attempt` combinator.

## ParseState & Indentation (Offside Rule)

`ParseState` tracks crucial context:

- `ConditionalCompilationStack` / `DefinedSymbols`: For nested `#if` evaluation.
- `Context`: A stack of `OffsideContext` values tracking the current indentation context (e.g., inside a `Let`, `Match`, or `Type`).
- `IndentationMode`: `Syntax.Light` or `Syntax.Verbose`.

## Testing

- **Framework**: [Expecto](https://github.com/haf/expecto) (`test/XParsec.FSharp.Tests/`)
- **Golden File Testing**: Test data lives in `data/`. Source `.fs` files are lexed/parsed and asserted against `.fs.lexed` and `.fs.parsed` golden output files.
- **Trace Debugging**: Test "Trace Debugging Test" in `test/XParsec.FSharp.Tests/ParserTests.fs` should be set to `ftest` and used to understand challenging parsing issues using the `.fs.trace` and `.fs.stack` output.

## Benchmarking

[BenchmarkDotNet](https://benchmarkdotnet.org/) benchmarks live in `bench/XParsec.FSharp.Benchmarks/`. Each benchmark class compares XParsec against `FSharp.Compiler.Service` (FCS) on `fixtures/{small,medium,large}.fs` and reports mean time and managed allocations via `[<MemoryDiagnoser>]`.

**Benchmark classes:**

- `LexingBenchmarks` — tokenization only (`Lexing.lexString`)
- `ParsingBenchmarks` — parse from already-lexed input (isolates the parser layer)
- `EndToEndBenchmarks` — source-to-AST (full pipeline)

**Running:**

```bash
./claude_tools.cmd -Action Benchmark -Filter '*Lexing*'
```

The `-Filter` argument is **required** and accepts BDN's wildcard syntax (e.g. `*Lexing*`, `*Parsing*`, `*EndToEnd*`, or `*` for all). The script runs `dotnet run -c Release -- -i -j short --filter <pattern>` under the hood — `-j short` keeps iteration count low so a pass takes ~45 s per class. Full BDN output is streamed to stdout and also saved to `claude_tools_output.log`; a GitHub-flavoured markdown summary lands in `BenchmarkDotNet.Artifacts/results/`.

**Reading the results:** focus on the `Allocated` column (managed bytes per op) and `Alloc Ratio` (vs. FCS baseline). Mean-time columns also appear but BDN's short-run confidence intervals are wide — allocation numbers are far more stable than mean times. When changing hot paths, always run the relevant benchmark before and after and compare both.

**In-process toolchain:** `BenchConfig.fs` forces `InProcessEmitToolchain` because the default out-of-process runner generates a fresh csproj that inherits this repo's CPM pin (`FSharp.Core 8.0.300`) and trips `NU1109` against FCS's `10.1.202` transitive requirement. The benchmark fsproj pins `FSharp.Core` via `VersionOverride` for the same reason.
