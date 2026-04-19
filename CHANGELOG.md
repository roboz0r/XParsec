# Changelog

## 0.4.1 - 2026-04-19

### 0.4.1 Behaviour Changes

- **`ParserCE.While` and `ParserCE.For` now use plain F# imperative semantics.** The guard is evaluated normally, the body runs for its side effects on each iteration, and mutations to enclosing `let mutable` bindings propagate as expected. This supersedes the 0.4.0 behaviour where `ParserCE.While` raised `InfiniteLoopException` when the body made no progress. Termination is now the user's responsibility (enforce it via the guard).
- **`ParserCE.Zero` now returns `Ok ()` instead of `pzero`.** This fixes `if cond then stmt` (with no `else`) and the implicit trailing `Zero` at the end of `while`/`for` bodies from short-circuiting the rest of the CE via `Combine`.
- **`ParserCE.Using` generalised to any return type.** Previously constrained to `Parser<unit, …>`; now `Parser<'A, …>`. Existing callers remain source-compatible. Implementation also switched to `use` binding to avoid a null-check boxing allocation.

### 0.4.1 Fixes

- Fix `OperatorParser.InfixNary` to roll back the reader position when the right-hand side parse fails under `allowTrailingOp`, and add a same-position guard before the trailing `parseRhsInternal` call to prevent infinite recursion when virtual tokens can repeatedly fire at the same position. Follow-up to the 0.4.0 `InfixNary` infinite-loop fix.
- Fix bounds-check bugs in `ReadableStringSlice` and related slice types:
  - `Slice` was double-applying the `start` offset when computing the new slice's start.
  - `AsSpan` used `index > length` where it should have been `index >= length`, and did not clamp the returned length to this slice's remaining bytes.
  - `Item` and `TryItem` index checks tightened via `uint` cast (single comparison for `0 <= index < length`).
- Fix an escape bug where a parent `Readable` could leak through slice operations.

### 0.4.1 Performance

- `ParserCE.Bind`, `Return`, `ReturnFrom`, and `BindReturn` are now inlined directly instead of delegating to `>>=`, `preturn`, and `|>>`. Eliminates closure allocations in CE bodies.

### 0.4.1 Tests

- New `ReadableTests.fs` covering slicing edge cases.
- New `ParserCE` tests for imperative `while`/`for` semantics and `if`-without-`else`.

## 0.4.0 - 2026-03-28

### 0.4.0 Breaking Changes

- **`ParseSuccess` struct removed:** Parsers now return `Result<'Parsed, _>` directly instead of wrapping the result in a `ParseSuccess` struct.
  - *Migration:* Replace `result.Parsed` with `result`, and `ParseSuccess.create x` with `Ok x`.
- **`RefParser` default constructor now throws:** An uninitialized `RefParser` now raises `InvalidOperationException` instead of returning a parser error.
  - *Migration:* Ensure `RefParser.Set` is called before the parser is used.

### 0.4.0 Added

#### 0.4.0 Combinators

- `dispatch` and `dispatchWithState`: New combinators for token-dispatch based parsing, allowing efficient branching on the current token without backtracking.

#### 0.4.0 Operator Parsing

- `LHSOperator.PrefixMapped`: New operator type for prefix operators where the result is not an expression (analogous to `RHSOperator.InfixMapped`).
- `RHSOperator.InfixNary.allowTrailingOp`: New option to allow a trailing operator at the end of an nary sequence.
- Operator parsing now accumulates errors from all attempted branches, improving error messages on failure.

### 0.4.0 Fixes

- Fix `skipManyTill` to ensure `pEnd` is tried first, consistent with `manyTill` behaviour.
- Fix `Reader.Slice` offset calculation (`index - newStart` → `index + newStart`).
- Fix infinite loop detection in `OperatorParser.InfixNary`.
- Fix excessive stack consumption in operator parsing.
- Fix `ParserCE.While` to detect and raise `InfiniteLoopException` when the body parser makes no progress.
- Fix `LineIndex.GetLineCol` to tolerate an index equal to `input.Length`.

## 0.3.1 - 2026-01-15

### 0.3.1 Fixes

- Ensure `pEnd` is tried first in `manyTill` so that if `p` conflicts with `pEnd` then `pEnd` will cause the parser to stop.

## 0.3.0 - 2026-01-01

### 0.3.0 Added

#### 0.3.0 Operator Parsing Features

A significant update to the operator precedence parsing API to allow for dynamic operator precedence calculations and custom operators.

- `RHSOperator` and `LHSOperator` now have `public` case constructors.
- **New Operator Types:**
  - `RHSOperator.InfixNary`: Added to support operators like tuple commas (`,`).
  - `RHSOperator.InfixMapped`: Added to support operators like member access (`.`) where the RHS is not an expression.
  - `LHSOperator.LHSTernary`: Added to support constructs like "if `expr` then `expr`" or "while `expr` do `expr`".
- Operator parsing failures now produce improved error messages.
- Operator binding power is now exposed via the `byte<bp>` type.

### 0.3.0 Breaking Changes

#### 0.3.0 Core / IReadable

- **Indices changed from 64-bit to 32-bit:**
  - `IReadable` interface and derived types now use 32-bit indices.
  - *Context:* This significantly simplifies the common case of parsing 32-bit indexed types like strings and arrays.
  - *Migration:* For parsing very long inputs (>2GB), an external buffering and resumption strategy is now expected.
- **Stream support removed:**
  - `ReadableStream` and `ReadableStreamSlice` types have been removed.
  - *Migration:* Users should employ an external buffering strategy when parsing `Stream` types.

#### 0.3.0 Operator Parsing API

- The `'Index` type parameter was renamed to `'Aux` in `RHSOperator` and related types to reflect that it can hold auxiliary data other than indices (refer to the additions above).
- The `equality` constraint was removed from the `'Op` token type parameter.
  - *Migration:* Equality is now handled via a provided `IEqualityComparer<'Op>` or `EqualityComparer<'Op>.Default`.
- `Operators` changed from an internal record type to a **public interface**.
- Functions in the `Operator` module have been updated to accommodate the above signature changes.

### 0.3.0 Additions

- Core `IReadable` implementations now expose `IReadable` methods on their type as well as the interface.

### 0.3.0 Maintenance

- **Internal Tests:** Test projects updated to .NET 10. (No effect on the .NET target for published packages).

## 0.2.5 - 2025-12-09

### 0.2.5 Additions

- Adds to `CharParsers` `anyString`, `anyStringCI`, `anyStringBy` and more variants
- Adds to `Parsers` `skip`, `fold`, `fold1`, `foldUserState`, `folderUserState1`

### 0.2.5 Fixes

- Make `sepEndBy` allow main parser to succeed without consuming input - by @bisgardo
- Make JSON number parser locale-independent - by @bisgardo
- Improve docs for Combinators `sepBy` variants

## 0.2.4 - 2025-11-23

### 0.2.4 Bugfixes

- Correctly sort operators by precedence in `Operator.create` to handle overlapping token definitions.

## 0.2.3 - 2025-10-12

### 0.2.3 Features

- Adds combinators `countManySatisfies` `countMany1Satisfies` `skipManySatisfies` `skipMany1Satisfies`
- Adds operations to `ParserCE`: `Using`, `While`, `For`, `Combine`.

### 0.2.3 Bugfixes

- Fix inlining of the bind operator `>>=`
