# Changelog

## 1.0.0 - 2026-05-09

### 1.0.0 API Removals

- **`Reader.Current` removed.** It was an exact duplicate of `Reader.Peek()`. *Migration:* call `reader.Peek()` (or `reader.TryRead()` if you also want to advance).
- **`OperatorParsing.brackets` removed.** Deprecated since the `enclosedBy` rename. *Migration:* call `Operator.enclosedBy` instead.

### 1.0.0 Error Model

- **`ErrorType.Empty` added.** Analogous to FParsec's "empty error message list" — produced by `pzero` and any combinator that filters all of its children. *Migration:* exhaustive matches on `ErrorType` need an `Empty` arm. Most consumers don't pattern-match exhaustively and will be unaffected.
- **`ParseError.zero` is now `Empty` instead of `Message ""`.** Renders as nothing (no prefix, no content) in the default formatter, instead of as a blank stub line.
- **Aggregating combinators filter `Empty` children before constructing `Nested`.** `<|>`, `choice`, `manyTill`, `skipManyTill`, `many1Till`, and `skipMany1Till` now drop `Empty` siblings; if every side was `Empty`, the result is itself `Empty` rather than a `Nested(bothFailed, [])` / `Nested(allChoicesFailed, [])` stub. So `pzero <|> pzero` collapses to `Empty`, and `pzero <|> p` propagates only `p`'s error.
- `ParseError.isEmpty: ErrorType<'T, 'State> -> bool` exposed for downstream combinators that want the same filtering semantics.

### 1.0.0 Behaviour Changes

- **`pseq`, `pseqReturn`, `pstring`, `stringReturn`, and `stringCIReturn` now succeed on an empty needle.** Previously, passing an empty array/string would always fail with `EndOfInput`, even at the start of a non-empty input. The empty needle now vacuously matches at any position (including end-of-input) and consumes nothing — matching FParsec semantics. *Migration:* if any caller depended on the old `EndOfInput` failure for empty inputs, guard the empty case explicitly.
- **`manyChars` / `many1Chars` now raise `InfiniteLoopException` instead of returning a soft `Error`.** This matches `many`, `sepBy*`, `manyTill`, `chainl1`, and `Parsers.fold` / `foldUserState`, which already throw on a zero-progress inner success. *Migration:* `try`/`with :? InfiniteLoopException<'State>` if you previously matched on `Error ParseError.infiniteLoop`. The latter `Message` value is still exported but no longer produced by the library.

### 1.0.0 Fixes

- `manyTill` / `skipManyTill` now report errors at the position where both `p` and `pEnd` failed, instead of the start of the combinator. Matches `many1Till`'s behaviour.
- `LineIndex.OfString(input, maxLength)` now passes `"maxLength"` as the `ParamName` of the thrown `ArgumentOutOfRangeException` (previously the descriptive message was leaked into `.ParamName`). Matches the single-arg pattern used elsewhere in the library and stays Fable-compatible.
- `puint8` / `puint16` / `puint32` / `puint64` XML docs now reference the matching unsigned bound instead of `Int64.MaxValue`/`MinValue`. `pbigint` no longer claims to fail on out-of-range values — it returns a `bigint` and cannot overflow.
- `Reader.SkipN` now rejects a negative `count` with `ArgumentException` instead of silently rewinding. To rewind, set `Reader.Index` directly.
- Both `Reader.Slice` overloads (the 2-arg state-resetting form and the 3-arg explicit-state form) now have XML docs explaining the slicing-with-state intent and the independent `ReaderId` of the child reader.

### 1.0.0 Breaking Changes

#### 1.0.0 Core Types — Self-Slicing `IReadable`

- **`'InputSlice` generic parameter removed from `Reader<>` and `Parser<>`.** `IReadable<'T, 'Slice>` is now self-slicing: an input's slice type is itself. Type signatures collapse from `Reader<'T, 'State, 'Input, 'InputSlice>` to `Reader<'T, 'State, 'Input>` and from `Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>` to `Parser<'Parsed, 'T, 'State, 'Input>`. The constraint becomes `'Input :> IReadable<'T, 'Input>`.
  - *Migration:* Drop the trailing `'InputSlice` type argument wherever `Reader<>` or `Parser<>` is named explicitly. Custom `IReadable` implementations must declare themselves as their own slice type.
- **`Readable*Slice` types collapsed into their non-slice counterparts.** Each `Readable<X>` / `Readable<X>Slice` pair is replaced by a single self-slicing type:
  - `ReadableString` + `ReadableStringSlice` → `ReadableString(s, start, length)`
  - `ReadableArray<'T>` + `ReadableArraySlice<'T>` → `ReadableArray<'T>(arr, start, length)`
  - `ReadableImmutableArray<'T>` + `ReadableImmutableArraySlice<'T>` → `ReadableImmutableArray<'T>(arr, start, length)`
  - `ReadableResizeArray<'T>` + `ReadableResizeArraySlice<'T>` → `ReadableResizeArray<'T>(arr, start, length)`
  - *Migration:* Replace any `Readable*Slice` type name with the non-`Slice` form. The whole-input single-arg constructors (`ReadableString(s)`, `ReadableArray(a)`, `ReadableImmutableArray(a)`, `ReadableResizeArray(a)`) are retained as secondary constructors that delegate to the three-arg primary, so existing call sites compile unchanged. The `Reader.of*` helpers are unaffected. `ReadableMemory<'T>` is unchanged.
- **`IReadable.SpanSlice(start, length)` renamed to `AsSpan`, with three overloads, and switched to BCL bounds semantics.** The interface now exposes `AsSpan()`, `AsSpan(start)`, and `AsSpan(start, length)` matching the BCL `string.AsSpan` / `T[].AsSpan` shape. Bounds checks throw `ArgumentOutOfRangeException` instead of silently clamping (the previous `SpanSlice` clamped `count` to remaining and returned an empty span when `index >= length`).
  - *Migration:* Rename `SpanSlice` calls to `AsSpan`. Audit any caller that relied on clamping (e.g. passing `Int32.MaxValue` for "everything from here"); replace with `AsSpan(start)` (whole tail) or `AsSpan(start, min count (input.Length - start))`. `Reader.PeekN(count)` continues to clamp internally — it is the recommended primitive for "peek up to N items" lookups, so most parser-level code is unaffected. Only direct `IReadable.AsSpan` callers (rare — primarily error-formatting and custom `IReadable` consumers) need attention.
  - *Fable note:* JS classes can't dispatch by arity, so under `#if FABLE_COMPILER` the interface declares a single `AsSpan: ?start: int * ?length: int -> ReadOnlySpan<'T>` member instead of three overloads. Call sites are unchanged on both targets — `AsSpan()`, `AsSpan(start)`, and `AsSpan(start, length)` resolve correctly under both compilers.

#### 1.0.0 Operator Parsing

- **`Operators.OpComparer` member removed.** The runtime equality-based dispatch that consulted it is gone too: `rhsInfixNary` now calls each operator's own `parseOp` directly to detect a subsequent occurrence, so no `IEqualityComparer<'Op>` is consulted at parse time. Implementations of the public `Operators<>` interface no longer need to expose `OpComparer` — and the parser doesn't compare `'Op` values for equality at runtime.
  - *Migration:* Remove the `OpComparer` member from any custom `Operators<>` implementation. (Note: the existing `'Op: equality` constraint on `OperatorsCollection` / `OperatorLookup` is unchanged — it was always there for internal storage indexing in `Operator.create`, not for runtime dispatch.)

### 1.0.0 Added

- **`ReadableArrayBuilder<'T>`** — a class-based growable builder for `ReadableArray<'T>` with a `List<T>`-style fast-path append + non-inlined grow path. Use when materialising parser output into a `ReadableArray` without intermediate allocations. Single-shot: `ToReadableArray()` releases the buffer and the builder must not be reused.
- **`SmallArrayBuilder<'T>`** — a stack-only `[<Struct; IsByRefLike>]` builder with four inline slots that spill to an `ImmutableArray.Builder` on the fifth `Add`. `ToImmutable()` dispatches to the exact-size `ImmutableArray.Create` overload for counts ≤ 4, avoiding both the builder allocation and the size-fitting copy. Now backs `many`, `many1`, the `sepBy*`/`sepEndBy*` family, `manyTill`, `many1Till`, and `chainl1`.
- **Readable view API surface expanded** across `ReadableString`, `ReadableArray<'T>`, `ReadableImmutableArray<'T>`, `ReadableResizeArray<'T>`, and `ReadableMemory<'T>`:
  - `static member Empty` and instance `IsEmpty` on every type.
  - Materialisation: `ReadableString.ToString()` returns the substring; `ReadableArray<'T>` / `ReadableImmutableArray<'T>` / `ReadableResizeArray<'T>` / `ReadableMemory<'T>` get `ToImmutableArray()` (`ReadableImmutableArray<'T>` is zero-copy when the view spans the whole backing array, otherwise allocates).
  - All five views implement `IEnumerable<_>`, `IReadOnlyCollection<_>`, and `IReadOnlyList<_>` so they work directly with `for … in`, LINQ, and indexed-collection consumers. (Boxes the struct on interface dispatch, as expected.)
  - `[<MethodImpl(AggressiveInlining)>]` applied to hot members (`Item`, `TryItem`, `Length`, `IsEmpty`, `Slice`, `AsSpan`).
  - All views retain default struct-field equality (reference of the backing buffer + `start` + `length`). No custom element-wise equality contract.
- Internal `IsByRefLikeAttribute` polyfill on `netstandard2.0` (declared in `FableTypes.fs`) so the attribute is recognised by name on that target — required for `SmallArrayBuilder<'T>`.

### 1.0.0 Performance

- Closure allocation eliminated in `sepBy1` (the `pOneThen p (fun s -> …)` body is manually inlined; the binder lambda was previously emitted as a closure class because `sepBy1` itself is not `inline`).
- Allocation and dispatch reductions across `OperatorParsing`:
  - `rhsInfixNary` calls the operator's own `parseOp` directly instead of re-dispatching through `RhsParser` + `OpComparer.Equals` per iteration.
  - `parseRhsInternal` extracts `leftPower` once and runs a single `< / = / >` minBinding ladder, instead of re-running the ladder inside each operator-variant arm.
  - `mergeSoftErrors` reordered hot-path-first (the steady-state `ValueNone, ValueNone` case wins out before any `ParseError` allocation).
  - `inline` added to `PrattParsed.success` / `withError`, `mergeSoftErrors`, `ensureAdvanced`, and `rhsInfix`.
  - `rhsInfixNary` now hints `ResizeArray(4)` initial capacity for the typical tuple/app-chain/sequence size, eliminating the first grow allocation.
- Stack-allocated `SmallArrayBuilder<'T>` replaces `ImmutableArray.CreateBuilder()` in the small-result combinators listed in Added — no builder heap allocation, and counts ≤ 4 skip the size-fitting copy.
- Primitive parsers in `Parsers.fs` (`preturn`, `pzero`, `fail`, `getUserState`, `setUserState`, `updateUserState`, `getPosition`, `setPosition`, `skip`) are now `inline` at their call sites, reducing thunk overhead in tight parser combinations. `updateUserState`'s mapper is annotated `[<InlineIfLambda>]`.
- `ReadableArrayBuilder<'T>.ToReadableArray()` always hands the backing buffer off without copying. The previous "right-size when below half-full" branch was removed — retaining some unused capacity is cheaper than the copy, and avoids round-tripping LOH-sized buffers through the Gen2 heap.
- `spaces` / `spaces1` no longer build a `StringBuilder` they immediately discard — they now drive `skipManySatisfies` / `skipMany1Satisfies` directly. `spaces1` keeps its labelled error via `<?>` for both the wrong-first-char and end-of-input failure paths.
- `pchar`, `skipChar`, `charReturn`, `anyChar`, `skipAnyChar`, `pstring`, `stringReturn`, and `stringCIReturn` are now `inline`, so they don't discard the inlining work of the primitives they delegate to.
- `Pratt.lhsTernary` computes `mergeSoftErrors errCond errBody` once and reuses it across the success and failure branches instead of recomputing it three times.
- `parray` builds its result via `SmallArrayBuilder<'T>` — no `ImmutableArray.Builder` allocation for `n ≤ 4`.
- `Pratt` module's soft-error accumulator now uses `ErrorType.Empty` directly instead of wrapping every `ParseError` in a `voption`. Removes one struct-of-struct layer through `mergeSoftErrors`, `mergeWithError`, `PrattParsed.Error`, and the per-case helpers (`rhsInfix`, `rhsInfixNary`, `rhsInfixMapped`, `rhsIndexer`, `rhsTernary`, `lhsPrefix`, `lhsEnclosed`, `lhsTernary`, `parseRhsInternal`, `parseLhsInternal`).
- Fable workaround in `Operator.create` removed: `(rhsParseOp op >>% op)` and `(lhsParseOp op >>% op)` are no longer wrapped in lambdas to dodge fable-compiler/Fable#4031, which is fixed in Fable 5. Repo's Fable tool minimum bumped from 4.25.0 → 5.0.0.

### 1.0.0 Tooling

- Fable tool requirement raised to **5.0.0**. Fable 5 forbids generic-argument runtime type tests (e.g. `:? ('T array)`); these have been split per-target so that .NET keeps the fast-path dispatch and Fable always materialises to an array via `Seq.toArray`. Affects `pseq`, `pseqReturn`, `anyOf`, `skipAnyOf`, `noneOf`, `skipNoneOf`. No public-API changes.
- `System.Collections.Immutable` PackageReference is now conditional on `netstandard2.0` only (`net8+` ships it in-box; Fable 5 promotes the `NU1510` warning to an error).

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
