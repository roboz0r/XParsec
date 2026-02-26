# Offside Infrastructure Gaps

Status as of 2026-02-26. 510 tests passing.

## Background

The offside (indentation) infrastructure lives in `ParsingHelpers.fs` and `ParsingTypes.fs`:

- **`OffsideContext`** (`ParsingTypes.fs:48`): An enum of ~25 contexts (`Let`, `If`, `Match`, `Type`, `Member`, `SeqBlock`, etc.) that describe what indentation block the parser is currently inside.
- **`Offside`** (`ParsingTypes.fs:88`): A record `{ Context; Indent; Token }` stored as a stack in `ParseState.Context`.
- **`withContext`** (`ParsingHelpers.fs:427`): Pushes a new offside entry onto the context stack before running an inner parser, and pops it afterwards. The indent level is taken from the first non-trivia token inside the new context.
- **Offside check** (`ParsingHelpers.fs:147`): Every call to `peekNextNonTriviaToken` / `nextNonTriviaToken` checks (in Light syntax mode) whether the token's column is to the left of the current context's indent. If so, the parser fails with `"Offside"`, which naturally terminates loops like `many`, `manyTill`, and `sepBy`.

The offside check works well for expression-level constructs (let bodies, match clauses, if-then-else, etc.) because those parsers already use `withContext OffsideContext.SeqBlock ...` to establish the indentation boundary.

## Problem

Type definition body parsing currently uses **explicit delimiters** (`class`/`end`, `{`/`}`, `|`) or **`nextNonTriviaTokenVirtualIfNot`** (which synthesizes a virtual token unconditionally without checking indentation) to determine where a construct ends. Neither approach works correctly in Light syntax when the delimiter is implied by dedent.

### What `nextNonTriviaTokenVirtualIfNot` actually does

```fsharp
// ParsingHelpers.fs:181
let rec nextNonTriviaTokenVirtualIfNot t reader =
    match peekNextNonTriviaToken reader with
    | Ok token ->
        if token.Token = t then
            consumePeeked token reader   // real token present: consume it
        else
            preturn { virtualToken }     // NOT present: synthesize immediately
```

This is an **unconditional fallback** — if the next token isn't exactly `t`, it creates a virtual token on the spot. It does **not** check whether the next token is at a valid indentation level. This means:

- If the terminator is `end` and the next token is `member` (at a valid indent), it still synthesizes `end` immediately, terminating the loop with zero iterations.
- The synthesized virtual token carries position info from the next real token, but no indentation awareness.

---

## Gap #1: `TypeDefn.Anon` body is always empty

**File:** `TypeDefnParsing.fs:836-837`

```fsharp
let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
let! body, endTok = ClassTypeBody.parse (nextNonTriviaTokenVirtualIfNot Token.KWEnd)
```

**What happens:** For `type T(args) = member this.X = ...`, there is no explicit `begin`/`end`. The parser:
1. Synthesizes a virtual `begin` (correct).
2. Calls `ClassTypeBody.parse` with `nextNonTriviaTokenVirtualIfNot Token.KWEnd` as the terminator.
3. Inside `ClassTypeBody.parse`, it calls `TypeDefnElements.parseTill terminator`, which is `manyTill TypeDefnElement.parse terminator`.
4. Before `manyTill` tries its first element, it checks the terminator — `nextNonTriviaTokenVirtualIfNot Token.KWEnd` sees `member` (not `end`), synthesizes a virtual `end`, and the loop terminates with **zero elements**.

**Fix needed:** Replace the unconditional virtual `end` terminator with an indentation-aware terminator. Two approaches:

### Approach A: Use `withContext` + offside failure as terminator

Wrap the class body in `withContext OffsideContext.Type (...)`. The body elements are parsed with `many TypeDefnElement.parse` (no explicit terminator). When the indentation drops below the context indent, `peekNextNonTriviaToken` fails with `"Offside"`, and `many` stops naturally. Then synthesize a virtual `end` unconditionally after the loop.

```fsharp
// Sketch:
let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
let! inh, lets, elems =
    withContext OffsideContext.Type (
        parser {
            let! inh = opt ClassInheritsDecl.parse
            let! lets = many ClassFunctionOrValueDefn.parse
            let! elems = many TypeDefnElement.parse
            return (inh, lets, elems)
        }
    )
let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
```

This relies on the offside check in `nextNonTriviaTokenImpl` failing when the next token (e.g., a new top-level `let` or `type`) is at a lower indent than the class body's first token.

### Approach B: Explicit indent-checking terminator

Create a helper like `isOffsideOrToken`:

```fsharp
let isOffsideOrToken t reader =
    match peekNextNonTriviaToken reader with
    | Ok tok when tok.Token = t -> consumePeeked tok reader  // real end
    | Ok _ -> fail (Message "Not terminated") reader         // still indented: keep going
    | Error _ -> preturn { virtualToken } reader             // offside failure: synthesize end
```

Use this as the terminator for `manyTill`. The offside failure from `peekNextNonTriviaToken` signals dedent, at which point we synthesize the virtual token.

---

## Gap #6: Record fields separated by newlines (no `;`)

**File:** `TypeDefnParsing.fs:776`

```fsharp
let! fields, _ = sepBy1 RecordField.parse pSemi
```

**What happens:** `type T = { X: int\n  Y: int }` fails because `sepBy1` requires an explicit `;` between fields. In F# Light syntax, newlines at the same indentation level serve as implicit separators.

**Fix needed:** Replace `sepBy1 ... pSemi` with a separator that accepts either `;` or an offside-aware newline. Options:

### Approach A: `sepBy1` with virtual separator

Create `pSemiOrOffsideNewline` that first tries `pSemi`, and if that fails, checks whether the next token is at the same indentation level as the record's opening `{`. If so, succeed (acting as a virtual separator).

### Approach B: Use `many1` inside a context

Push `withContext OffsideContext.Brace` after consuming `{`. Then use `many1 RecordField.parse` — fields keep parsing as long as they're at a valid indent. When `}` appears (or dedent), `many1` stops.

```fsharp
// Sketch:
let! lBrace = pLBrace
let! fields =
    withContext OffsideContext.Brace (
        many1 (parser {
            let! field = RecordField.parse
            let! _ = opt pSemi  // consume optional semicolon
            return field
        })
    )
let! rBrace = pRBrace
```

---

## Gap: `TypeExtensionElements.parse` requires explicit `end`

**File:** `TypeDefnParsing.fs:702`

```fsharp
let! elems, endTok = TypeDefnElements.parseTill pEnd
```

**What happens:** `type Foo with member this.Bar = ...` (no `end`) fails because `parseTill pEnd` requires a literal `end` token. In Light syntax, the extension body ends at dedent.

**Fix needed:** Same pattern as Gap #1 — use `withContext OffsideContext.WithAugment` to establish indent, parse with `many`, and synthesize a virtual `end` after the offside check terminates the loop.

---

## General Pattern for Fixes

All three gaps share the same root cause: using `nextNonTriviaTokenVirtualIfNot` or `pEnd` as a terminator where the construct's end is signaled by dedent, not by an explicit token. The general fix pattern is:

1. **Push an offside context** (`withContext`) with the appropriate `OffsideContext` value before parsing the body.
2. **Parse elements with `many`** (not `manyTill`). The offside check built into `peekNextNonTriviaToken` will fail with `"Offside"` when the next token is dedented, causing `many` to stop.
3. **Synthesize a virtual closing token** (`end`, `}`, etc.) unconditionally after the loop completes.

The offside check infrastructure already exists and works correctly for expressions. The gap is that type definition parsing hasn't been wired up to use it.
