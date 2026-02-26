# XParsec Developer Guide

## Project Overview

**XParsec** is a parser combinator library for .NET. The core `Parser` type is a function `Reader -> ParseResult`.
**XParsec.FSharp** (`src/XParsec.FSharp/`) is an F# language parser/lexer built on XParsec. It lexes source into tokens, processes compiler directives, applies offside (indentation) rules, and parses the stream into an AST.

```text
Source ──> [Lexing] ──> [Preprocessing (#if)] ──> [Parsing (AST)] ──> [Debug Output]
```

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
- **Golden File Testing**: Test data lives in `data/` and `data/blocks/`. Source `.fs` files are lexed/parsed and asserted against `.fs.lexed` and `.fs.parsed` golden output files.

## Build & Run

You are running on Windows. **DO NOT** use raw `dotnet` commands or Bash tools. Use the PowerShell wrapper.

```powershell
# Build entire solution
pwsh -File "./claude_tools.ps1" -Action Build

# Build F# parser only
pwsh -File "./claude_tools.ps1" -Action Build -SourceProject "XParsec.FSharp"

# Run tests (Truncates output to 30 lines. Full logs in claude_tools_output.log)
pwsh -File "./claude_tools.ps1" -Action Test -TestProject "XParsec.FSharp.Tests"
```
