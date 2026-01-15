# Changelog

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
