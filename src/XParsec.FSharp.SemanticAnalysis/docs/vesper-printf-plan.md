# Vesper.Printf plan

`printf` as its own library (minimal-core-lib-plan **D4**), depending on
`Vesper.Core` (`Fun`, `List`). This plan **resolves D4's open fork** —
compile-time lowering, *not* a closure-factory `PrintfModule` engine — and
**unifies the happy path with string interpolation** (minimal-core-lib-plan
**D9**): `printfn "%d" x` and `$"{x:d}"` differ only in surface syntax for the
format spec; once parsed they are the same `(literal | hole)*` sequence and lower
the same way. One mechanism, two surface syntaxes.

Today the backend mints `Microsoft.FSharp.Core.PrintfFormat\`4` + `PrintfModule`
(`ClrProvider.fs:29,32`); this replaces that.

## Goals

- **No allocation on the happy path** — no `PrintfFormat` object, no closure, no
  boxing.
- **Target-typed, two senses:** each hole is statically typed
  (`AppendFormatted<int>`, no box); and the *sink* (string / `TextWriter` /
  `StringBuilder`) picks the flush (D9's "target-typed lowering").
- **Through .NET InterpolatedStringHandler-style infra** in the typical case.

It is the `Fun` story retold ([function-representation-plan](function-representation-plan.md)):
value-struct / inline on the happy path, a heap/spec fallback for what escapes.

## One spec, one parser, one handler

`PrintfSpec.fs` already parses a format literal into a typed-hole sequence (it
drives type-checking today). That same sequence — `(literal | hole(type, spec))*`
— is the **target-agnostic IR** that drives lowering (a universal canonicalisation,
[backend-design-plan](backend-design-plan.md) §"Lowering split"). The CLR target
lowers it to handler calls; a JS target lowers the *same* sequence to a template
literal. Handlers are CLR-specific; the IR is not.

## The three lowerings

One spec, one parser (`PrintfSpec.fs`), one handler — three lowerings that share
them:

| Case | Lowering | Runtime touched |
|---|---|---|
| fully-applied literal (**v1**) | inline `AppendLiteral`/`AppendFormatted<T>` at the call site + flush | handler only |
| partially-applied literal (later) | stateless/captured `Fun` struct(s) + static parsed-spec field; `Invoke` runs spec → handler | handler + spec-runner |
| format-as-value / non-literal (cold) | runtime parse → spec-runner → handler | handler + parser + spec-runner |

`%A` is orthogonal — a dispatch *inside* `AppendFormatted`/the runner (P3).
`PrintfFormat` exists only as the parsed-spec object for rows 2–3 (P4).

## P1 — One custom write-through handler (a ref struct)

Start with one; add a second only if a sink demands it.

- Buffers (stackalloc small, `ArrayPool` spill); flushes via
  `TextWriter.Write(ReadOnlySpan<char>)` (Core 2.1+) — **no result string** for
  `printfn`/`fprintf`.
- Members: `AppendLiteral`, `AppendFormatted` (+ alignment / format overloads).
  **No box:** `ISpanFormattable.TryFormat` writes straight into the buffer span.
- Sinks: write-through to `TextWriter` (`printf`/`printfn`/`eprintf`/`fprintf`);
  `ToStringAndClear` (`sprintf`); `StringBuilder` (`bprintf`, later).
- **Shared with D9's default `$"..."` handler** — author it once.
- **Why custom, not the BCL `DefaultInterpolatedStringHandler`:** there is no
  `TextWriter.Write(handler)` overload in the BCL, so a zero-string `printfn`
  needs our own; and we want one type shared with interpolation. (`sprintf` may
  still delegate to BCL `DefaultInterpolatedStringHandler` if cheaper.)

## P2 — v1 is the fully-applied literal happy path only

`printfn` / `sprintf` / `printf` with a **literal** format, **fully applied**, no
`%A` → inline `AppendLiteral` / `AppendFormatted<T>` at the call site + flush.
Closes minimal-core-lib gap ① for the canonical sample
(`printfn "%d" (sum nums)`).

Deferred, all additive: the `PrintfFormat` object, partial application,
format-as-value, the runtime spec-runner, `%A`.

## Format spec → handler

- `%d %s %f %b %O` → `AppendFormatted<T>(value, spec?)`.
- `%5.2f` → alignment `5`, format `"F2"`; `%x` → format `"x"`. Width / precision /
  flags map onto the alignment + format arguments.
- `%A` → structural dispatch (P3).

## P3 — `%A` is reflection-free, compiler-synthesized (deferred)

Dispatch by static hole type:

```
ISpanFormattable        → TryFormat into the span        (no box)
generated IStructFormat → FormatTo(&handler, indent)     (records / DUs)
otherwise               → AppendFormatted (ToString)
```

Reflection-free ⇒ the backend **synthesizes `IStructFormat.FormatTo` per record /
DU** that reaches `%A` (the way structural equality is synthesized) — there is no
runtime walker. Output is an **indented tree**; it deliberately does **not** match
F#'s `[1; 2; 3]`. Heavy tail (self-host rung 3); out of v1.

## P4 — Partial application: a `Fun` value struct over a static parsed spec (deferred)

`let p = printfn "%d"` becomes a value struct `S : Fun<int, unit>`:

- Its only "state" is the format, a compile-time constant → a **`static readonly`
  parsed-spec field**, so `S` is **stateless ⇒ `default(S)`, zero heap
  allocation**, and `constrained.callvirt Fun::Invoke` devirtualises (rides the
  `Fun` work directly — [[project_function_representation_ifunc]]).
- The static field holds the **parsed spec** (parsed once at type init), not the
  raw string — `Invoke` drives it through the same handler, no reparse, no box.
  **This is the one place D4's `PrintfFormat` survives** — demoted to the
  static-field / cold representation, never on the happy path.
- **Multi-hole currying:** `printfn "%d %s"` partially applied is a chain — outer
  `Fun<int, Fun<string, unit>>` whose `Invoke(n)` yields an inner
  `Fun<string, unit>` **capturing `n`** + the shared static spec; the final
  `Invoke` runs spec → handler. Intermediate stages capture bound args (not
  stateless), still value structs where they don't escape — the slice-5 capture
  story over `Fun`.
- **Escape caveat:** zero-alloc only while `p` flows into `Fun`-bounded generic
  positions or is invoked directly; an interface-typed `Fun<int, unit>` slot boxes
  it — inherits whatever the `Fun` escape analysis decides.

The runtime spec-runner that `Invoke` calls is the same engine the cold path
(format-as-value / non-literal) uses; both reuse the P1 handler.

## Type checking

`printfn : PrintfFormat<'Printer, unit, string, unit> -> 'Printer`; the literal is
typed as `PrintfFormat` and `PrintfSpec.fs` derives `'Printer` (the curried hole
types). That derivation feeds **both** type-checking and lowering, so the
front-end work is shared across all three lowerings and across `printf` / `$"..."`.
The `PrintfFormat` *type* is in the contract even in v1 — it types the literal —
but on the happy path no `PrintfFormat` *value* is ever constructed.

## Cross-target

The format → hole IR is universal; only the lowering is per-target (CLR → handler;
JS → template literal), consistent with
[fsi-target-brainstorm](fsi-target-brainstorm.md)'s contract / per-target split.

## Surface (v1 contract)

`src/Vesper.Printf/` — no `.fsproj`, like `Vesper.Core`:

- `printf-format.fsi` — `PrintfFormat<_,_,_,_>` (+ 5-typar) and the `Format` abbreviation.
- `formatter.fsi` — the P1 write-through ref-struct handler.
- `printf.fsi` — `[<AutoOpen>]` `printf` / `printfn` / `sprintf` (others additive).

Impl `.fs` (the handler body, the spec-runner, the `%A` synthesis) lands with the
self-host rungs; until then `ClrProvider` lowers the happy path against these
names — like the arithmetic operators, the printf functions are signature-only
and lowered as intrinsics, no runtime body on the happy path.

## Cross-references

- [minimal-core-lib-plan](minimal-core-lib-plan.md) — **D4** (printf is its own
  library; this resolves its compile-time-lowering fork) and **D9** (the shared
  interpolation handler). `Vesper.Printf` is the gap-① closer for the canonical sample.
- [function-representation-plan](function-representation-plan.md) — the `Fun`
  value-struct + escape analysis the P4 partial-application structs ride on.
- [backend-design-plan](backend-design-plan.md) §"Lowering split" — format-spec
  resolution as a universal canonicalisation feeding per-target lowering.
- `PrintfSpec.fs` — the existing format parser that derives the typed holes.
- [codegen-clr-part-5](codegen-clr-part-5.md) / `RunnableAppTests.fs` — slice 1
  (`printfn "hi"`) and slice 4 (`printfn "%A" [1;2;3]`); the `%A` cutover (P3)
  gates re-greening slice 4 against `Vesper.Printf`.
