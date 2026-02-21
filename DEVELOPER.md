# XParsec Developer Guide

## Project Overview

**XParsec** is a parser combinator library for .NET, designed as a modern successor to FParsec. The core `Parser` type is a function `Reader -> ParseResult`, generic over `'Parsed`, `'T` (token type), `'State`, `'Input`, and `'InputSlice`.

**XParsec.FSharp** (`src/XParsec.FSharp/`) is an F# language parser/lexer built on XParsec. It lexes F# source code into tokens, processes compiler directives, applies offside (indentation) rules, and parses the token stream into an AST.

## Architecture & Pipeline

```
Source string
    |
    v
[Lexing] ──> Lexed { Tokens, Blocks, LineStarts }
    |
    v
[Preprocessing] ──> #if directive evaluation (during parsing)
    |
    v
[Parsing] ──> FSharpAst<SyntaxToken>
    |
    v
[Debug] ──> Pretty-printed output (for testing)
```

- **Lexing** converts raw source into `Lexed`: an array of `PositionedToken` with block structure and line-start indices.
- **Preprocessing** evaluates `#if`/`#else`/`#endif` directives using operator precedence parsing during the parse phase.
- **Parsing** consumes the token stream, tracking indentation context (`OffsideContext`), and produces an AST (`FSharpAst<SyntaxToken>`).

## File Compilation Order & Responsibilities

Files in `src/XParsec.FSharp/XParsec.FSharp.fsproj` (in compilation order):

| File | Purpose |
|---|---|
| `MoreParsers.fs` | XParsec utility extensions (`peekAnyOf`, `skipN`, `skipNOf`, `allEqual`, `tryLastV`) |
| `UtilTypes.fs` | `ImmutableArrayM<'T,'M>` — immutable array wrapper with unit-of-measure index safety |
| `Token.fs` | `Token` enum (200+ cases), `PositionedToken` struct, bit-packed `uint16` representation, `TokenKind` enum, `PrecedenceLevel`, operator info |
| `Lexing.fs` | `Lexed` record (`Tokens`, `Blocks`, `LineStarts`), `BlockInfo` struct, measurement types (`token`, `block`, `line`), lexer implementation |
| `Expr.fs` | Expression AST types: `Expr<'T>`, `Pat<'T>`, `Type<'T>`, `Constant<'T>`, `Measure<'T>`, `LongIdent`, `Typar`, etc. |
| `Declarations.fs` | Declaration AST: `ModuleElem`, `ModuleDefn`, `TypeDefn`, `MemberDefn`, `FunctionOrValueDefn`, `Access` |
| `Signatures.fs` | Signature file AST (`.fsi`): `ModuleSignature`, `TypeSignature`, `ValSig`, `TypeSignatureElement` |
| `ProgramStructure.fs` | Top-level types: `FSharpAst<'T>`, `ImplementationFile`, `ScriptFile`, `SignatureFile`, `NamedModule`, `AnonymousModule` |
| `ParsingTypes.fs` | Parser state: `ParseState`, `SyntaxToken`, `TokenIndex` (Regular/Virtual), `OffsideContext` (24 cases), `Syntax` (Light/Verbose), `IfExpr`, `Diagnostic` |
| `Preprocessing.fs` | `#if` directive expression parser using Pratt/operator-precedence parsing, `IfExprParser` |
| `Parsing.fs` | Main parser — all grammar rules as modules (one per AST node), `namespace rec` for forward references, operator precedence parsing for expressions, patterns, measures, and types |
| `Debug.fs` | AST pretty-printing functions (`printExpr`, `printPat`, `printType`, `printConstant`, etc.) for test output |

## Key Patterns & Conventions

### Naming
- **PascalCase** for types, modules, DU cases
- **camelCase** for values, functions, let bindings
- **`KW` prefix** for keyword tokens (e.g., `KWLet`, `KWMatch`, `KWIf`)
- **`Op` prefix** for operator tokens (e.g., `OpAddition`, `OpPipeRight`)
- **`Num` prefix** for numeric literal tokens (e.g., `NumInt32`, `NumIEEE64`)
- **`Virtual` prefix** for compiler-inserted offside tokens (e.g., `VirtualIn`, `VirtualSep`)

### Units of Measure for Type-Safe Indices
```fsharp
[<Measure>] type token   // index into Lexed.Tokens
[<Measure>] type block   // index into Lexed.Blocks
[<Measure>] type line    // index into Lexed.LineStarts
```
These prevent accidentally mixing token, block, and line indices.

### Bit-Packed Token Representation
Tokens are stored as `uint16` values with fields packed via bit masks:
- `TokenKind` (3 bits): Identifier, Keyword, Operator, NumericLiteral, TextLiteral, Special, Invalid
- Flags: `InComment`, `IsVirtual`, `CanBePrefix`, `IsDeprecated`
- Payload bits vary by kind (e.g., `PrecedenceLevel` for operators, `NumericKind`/`NumericBase` for numerics)

### Parser Computation Expression
```fsharp
parser {
    let! x = someParser
    let! y = anotherParser
    return (x, y)
}
```

### Prefer Statically-Allocated Parser Values

When a parser does not close over any local variables, define it as a **value** (no explicit `reader` parameter) rather than a function. This allocates the combinator closure once at module initialisation instead of on every invocation.

**Prefer (allocated once at startup):**
```fsharp
let peekNonTriviaIndent =
    lookAhead (fun r -> ...)
```

**Avoid (new closure allocation on every call):**
```fsharp
let peekNonTriviaIndent reader =
    lookAhead (fun r -> ...) reader
```

The same principle applies to any parser built entirely from other combinators (`many`, `opt`, `choice`, `lookAhead`, etc.) that does not capture a local variable. Compare:
- `let pLet = nextNonTriviaTokenIsL Token.KWLet "Expected 'let'"` — static value ✓
- `let pInVirt = nextNonTriviaTokenVirtualIfNot Token.KWIn` — static value ✓
- `let pSeqBlock pElem = parser { ... }` — parameterised, so each unique `pElem` produces a distinct closure; this is expected and correct

The key rule: if the definition body does not reference any enclosing `let`-bound local variables (only top-level definitions or its own parameters), remove the trailing `reader` application and drop the explicit `reader` parameter so F# computes the result once.

### Operator Precedence Parsing (Pratt Parsing)
Used for expressions, patterns, measures, and types. Key types:
- `Operators<'Parsed, 'T, 'State, 'Input, 'InputSlice>` interface with `LhsParser`, `RhsParser`, `OpComparer`
- `BindingPower.fromLevel` converts precedence levels to binding power values
- `PrecedenceLevel` enum defines relative precedence (e.g., `Let`, `Semicolon`, `Pipe`, `InfixAdd`, `Application`)

### Recursive Namespaces
`Parsing.fs` uses `namespace rec XParsec.FSharp.Parser` to allow forward references between parser modules (e.g., `Expr` referencing `Pat` and vice versa). Ref parsers (`refExpr`, `refPat`, `refType`) break cycles.

### Struct Types
`[<Struct>]` is used on performance-critical types:
- `PositionedToken`, `BlockInfo`, `ParseError`
- Most DU cases in AST types (e.g., `Expr`, `Pat`, `Type`)
- `TokenIndex` (Regular/Virtual)

### `[<RequireQualifiedAccess>]`
Applied to discriminated unions and utility modules to avoid name collisions and enforce explicit qualification.

## ParseState & Indentation (Offside Rule)

`ParseState` tracks parser state beyond token position:

| Field | Purpose |
|---|---|
| `Lexed` | Reference to the `Lexed` token/block/line data |
| `Input` | Source string |
| `DefinedSymbols` | Set of defined preprocessor symbols |
| `ConditionalCompilationStack` | Stack for nested `#if` directives |
| `IndentationMode` | `Syntax.Light` or `Syntax.Verbose` |
| `Context` | Stack of `OffsideContext` values |
| `Diagnostics` | Accumulated parse diagnostics |
| `LastLine` | Last observed line number |
| `ReprocessOpAfterTypeDeclaration` | Flag for operator reprocessing |

### OffsideContext (24 cases)
Tracks what indentation context the parser is inside:
`Let`, `Fun`, `Match`, `MatchClauses`, `If`, `Then`, `Else`, `For`, `While`, `Do`, `Try`, `Lazy`, `Function`, `Begin`, `Paren`, `Bracket`, `BracketBar`, `Brace`, `Quote`, `Struct`, `Sig`, `Module`, `Namespace`, `WithAugment`, `WithLet`, `Member`, `Type`, `Vanilla`

### Virtual Tokens
`TokenIndex.Virtual` represents compiler-inserted tokens for the offside rule (e.g., `VirtualIn` after `let` bindings, `VirtualSep` for semicolons, `VirtualEnd` for block endings).

## Testing

- **Framework**: [Expecto](https://github.com/haf/expecto)
- **Test project**: `test/XParsec.FSharp.Tests/`

### Test Files
| File | Purpose |
|---|---|
| `TestHelpers.fs` | Shared utilities (`testParseFile`, `testLexed`, `testLexedBlocks`, `printLexed`, etc.) |
| `LexingTests.fs` | Lexer golden-file tests |
| `ParserTests.fs` | Parser golden-file tests |
| `BlocksTests.fs` | Block structure tests |
| `KeywordTests.fs` | Keyword token tests |
| `NumericTests.fs` | Numeric literal tests |

### Golden File Testing
Test data lives in `test/XParsec.FSharp.Tests/data/` (and `data/blocks/`).

Pattern: `.fs` source files are lexed/parsed and compared against expected output:
- `*.fs` → source input
- `*.fs.lexed` → expected lexer output
- `*.fs.parsed` → expected parser output

Helpers: `testLexed` compares lexer output, `testParseFile` compares parser output, `testLexedBlocks` compares block structure.

## Build & Run

Requires .NET SDK 10.0 (see `global.json`, `rollForward: latestFeature`).

```bash
# Build the F# parser library
dotnet build src/XParsec.FSharp/

# Run all F# parser tests
dotnet test test/XParsec.FSharp.Tests/

# Build entire solution
dotnet build
```

## Solution Structure

```
src/
  XParsec/              # Core parser combinator library
  XParsec.FSharp/       # F# language parser (this project)
  XParsec.CLArgs/       # Command-line argument parser
test/
  XParsec.FSharp.Tests/ # F# parser tests (Expecto)
```
