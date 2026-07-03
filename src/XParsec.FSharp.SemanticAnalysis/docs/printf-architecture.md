# Printf & `%A` architecture

Durable record of the architectural decisions behind `Vesper.Printf` and the `%A`
structural formatter — the *why* behind choices the code cannot state for itself.
For the one piece not yet built (zero-alloc partial application) see
[printf-partial-app-plan](printf-partial-app-plan.md).

`Vesper.Printf` is printf as its own contract library (depends on `Vesper.Core`),
**lowered at compile time** — not a closure-factory `PrintfModule` engine, and with
no `PrintfFormat` object on the happy path. `printf` and string interpolation are
one mechanism, two surface syntaxes: `printfn "%d" x` and `$"{x:d}"` differ only in
how the format spec is written; once parsed they are the same `(literal | hole)*`
sequence and lower identically.

## One spec, one parser, one handler

`PrintfSpec.fs` (SemanticAnalysis) parses a format literal into a typed-hole
sequence. That same sequence — `(literal | hole(type, spec))*` — is the
**target-agnostic IR** that drives *both* type-checking and lowering, shared across
`printf` and `$"..."`. The CLR target lowers it to handler calls; a JS target lowers
the same sequence to a template literal. Handlers are CLR-specific; the IR is not.

## The three lowerings

| Case | Lowering | Status |
|---|---|---|
| fully-applied literal | inline `AppendLiteral` / `AppendFormatted<T>` at the call site + flush | **built** |
| partially-applied literal | stateless/captured `Fun` struct over a static parsed-spec field | deferred → [printf-partial-app-plan](printf-partial-app-plan.md) |
| format-as-value / non-literal / `%a` `%t` | runtime parse → spec-runner → handler | falls back to FSharp.Core `PrintfModule` |

Anything not on the fully-applied-literal happy path currently routes to FSharp.Core's
cold printf (`FreezeExpr.fs` leaves the `App printfn` intact). That path is correct
but allocates the `PrintfFormat` object + closures the happy path avoids; the
partial-app plan replaces it for the lowerable case.

## The write-through handler (`formatter.fs`)

`Vesper.Formatter` is a `[<Struct; IsByRefLike>]` stack-only handler the backend
constructs and drives inline; users never name it. It rents a pooled `char[]` from
`ArrayPool` (256-char floor, doubling growth) and flushes through to a `TextWriter`,
so `printfn`/`fprintf` produce **no result string**. `sprintf` uses
`ToStringAndClear` on a string sink instead.

Members: `AppendLiteral`, `AppendFormatted` (value / +format / +alignment /
+alignment+format), plus F#-semantics helpers (`AppendBool`, `AppendUnsigned`,
`AppendOctal`, `AppendZeroPaddedFloat`) and `AppendStructured` for `%A`.

Two deliberate deviations from a pure no-alloc handler, each **byte-identical** in
output and documented at the head of `formatter.fs`:

- `AppendFormatted` takes the `IFormattable.ToString(format, provider)` path rather
  than the no-alloc `ISpanFormattable.TryFormat` span fast-path — so it does box the
  value. Same culture (Invariant), same format string.
- `Flush` uses `TextWriter.Write(string)` rather than `Write(ReadOnlySpan<char>)` —
  F# has no implicit `Span<char>` → `ReadOnlySpan<char>` conversion.

**Why a custom handler, not the BCL `DefaultInterpolatedStringHandler`:** there is no
`TextWriter.Write(handler)` overload in the BCL, so a zero-string `printfn` needs our
own; and we want one type shared with string interpolation. (`sprintf` may still
delegate to the BCL handler where cheaper.)

Sinks built: write-through `TextWriter` and string/`ToStringAndClear`. `StringBuilder`
(`bprintf`) is not built — `EmitFormat.fs` errors on `ToBuilder`.

## Happy-path lowering

- **Recognition:** `Passes/Unification/InferApp.fs` (`tryInferPrintfApp`) marks a call
  for inline lowering only when it is a literal format, **fully applied**
  (`args.Length = specs.Length + 1`), with lowerable placeholders. Partial
  application, `%a`/`%t`, format-as-value, and `fprintf`/shadowing cases are left
  unmarked and fall through to FSharp.Core.
- **Freeze:** `FreezeExpr.fs` diverts a marked call to a `TExpr.Format` node — no
  `PrintfFormat` value, no closure.
- **Emit:** `Codegen.Clr/EmitFormat.fs` materialises the ref-struct local in place:
  ctor, one `AppendLiteral`/`AppendFormatted<T>` per segment (arg evaluated at its
  position), a trailing `"\n"` for `printfn`, then `Flush` (writers) or
  `ToStringAndClear` (`sprintf`).

## Spec → handler mapping

`Codegen.Clr/ClrHoleFormat.fs`: `%d`/`%s` → verbatim/formatted; `%5.2f` → alignment
`5` + format `"F2"`; `%x`/`%X` → `"x"`/`"X"`; width/precision/flags map onto the
alignment + format arguments; `%A` routes to `AppendStructured`.

## `%A`: reflection-free structural formatting

The interfaces live in `Vesper.Core/structural-format.fs(i)`:
`IStructuralFormattable` (types implement `Format(sink)`) and `IFormatSink` (the sink
the body drives). A record/DU that reaches `%A` gets a **compiler-synthesized**
`Format` body (`Codegen.Clr/EmitStructuralFormat.fs`); the layout engine +
built-in walking live in the runtime (`Vesper.Printf/structural-printer.fs`,
`RuntimeFormatState`). Reflection-free means no `System.Reflection` over fields —
but there *is* a runtime layout walker for built-ins (collections, tuples,
primitives). Output is **copy-pasteable Vesper source** with a **group-based**
layout: small values render identically to F# (`[1; 2; 3]`); divergence is only in
the multi-line break regime.

### Semantic ops, not layout replay

`IFormatSink` carries two vocabularies:

- **Layout ops** (`Text`, `Line`, `SoftBreak`, `BeginGroup`/`EndGroup`,
  `BeginNest`/`EndNest`) — for hand-written `IStructuralFormattable` implementors,
  who thereby pin their own layout (their choice, documented).
- **Semantic ops** (`BeginRecord`, `Field name`, `EndRecord`, `BeginCase name`,
  `EndCase`, `Child obj`) — the *only* thing synthesized bodies call. They encode
  what a record/union *is*, not how it looks.

Three forces drove the semantic vocabulary over replaying the layout grammar as
literal sink calls:

1. **Versioning.** Replaying the grammar compiles the `%A` output *policy* (the
   `{ F = · }` spacing, the `+2` hang, when parens appear) into every assembly,
   forever. Semantic ops encode the stable *structure* and leave the policy in the
   runtime, where it can be patched. This is disqualifying-or-not for the upstream
   pitch (FSharp.Core under `--reflectionfree`, netstandard2.0, no default interface
   methods — a frozen surface that must be right the first time).
2. **Emitted-body size.** An *n*-field record is `BeginRecord; (Field; Child)×n;
   EndRecord` = 2+2n calls, versus ~4+6n replaying layout ops; a payload-*k* union
   arm drops from ~5+6k to 2+k.
3. **Fidelity under erasure.** `Child(obj)` erases the static type. On the CLR the
   box preserves the runtime type, so the dispatcher recovers it — obj-only is
   *correct* here. On JS the box cannot (char vs 1-char string, int vs integral
   float are indistinguishable). That is why the interface is designed with room
   for typed `Child` atoms it does not yet carry (see Deferred seams).

### Protocol decisions

- **`Field(name)` is a marker, not `Field(name, value)`.** The value arrives in the
  next `Child` call. Invariant the sink must honour: `Child`, on entry, flushes the
  pending label into the `Doc` *before* recursing into the value — otherwise a nested
  record's first `Field` clobbers the outer pending label.
- **Frame context replaces the old `FormatChild`/`FormatArg` split.** The enclosing
  frame (`BeginRecord` vs `BeginCase`) says which position a `Child` is in; because
  the sink builds a deferred `Doc` tree, the 1-vs-n payload form is decided at
  `EndCase` from the observed child count. `Child` is the sole recursion entry.
- **Single-payload parenthesisation** (`Some (Some 3)` parens; `Some [1; 2]` /
  `Some 3` do not) is *not* settled by child count — the rule is "parenthesise the
  lone child iff it is itself a DU application", a property of the child's form. A
  payload-bearing case frame is marked *application-shaped* (`LastAppShaped` /
  `ChildAppShaped`); at a 1-child `EndCase` the lone child parenthesises iff it
  carries that mark.

`Child(obj)` is the only child entry: `box` on a reference type is a no-op
(ECMA-335 III.4.1), so every field takes one uniform `box`. No generic `Child<'T>` —
a generic virtual method is its own AOT hazard.

### Where the knowledge lives, and the single-source-of-truth cost

The record/union output forms live independently in `structural-printer.fs` (CLR)
and `structural-printer.js.fs` (JS), tied only by the cross-target differential
test. This is not a pure win: it trades one shared definition for two copies and
promotes the differential test from backstop to the thing keeping the targets in
sync. `Codegen.Common/StructuralFormatRecipe.fs` is kept as the shared **oracle** for
that test (its only remaining consumer is the JS `StructuralFormatRecipeTests`); the
CLR emitter no longer consumes it. Acceptable — the JS walker already duplicated the
forms — but stated rather than folded into "the grammar moved into the runtime".

## Cross-target

The format → hole IR is universal; only the lowering is per-target. On JS, `%A`
holes import `structuralFormat` from `Vesper.Printf.mjs` — the committed runtime
asset generated from `structural-printer.js.fs` (a shape-keyed semantic walker, not
an `IFormatSink` impl). The plain printf family is front-end special-cased on JS
(lowered to `Format` nodes, template-literal style), not spliced from the CLR
handler.

## Surface (contract)

`src/Vesper.Printf/`, no `.fsproj` (Vesper-compiled like `Vesper.Core`):

- `printf-format.fsi` — `PrintfFormat<_,_,_,_>` (+ 5-typar) and the `Format`
  abbreviation. The *type* is in the contract to type the literal; on the happy path
  no `PrintfFormat` *value* is constructed.
- `formatter.fsi` — the write-through ref-struct handler.
- `printf.fsi` — `[<AutoOpen>]` `printf` / `printfn` / `sprintf`.
- `structural-printer.fs(i)` — the `%A` engine + `RuntimeFormatState`.
- `formatter.fs` — the handler body.

The `.fs` are compiled into `Vesper.Printf.dll` by this repo's own backend;
`structural-printer.fs` must precede `formatter.fs` (declaration-ordered single
package — `Formatter.AppendStructured` calls `StructuralPrinter.Print`).

The handler and `%A` engine were bootstrapped in C# (`Formatter.cs`,
`StructuralFormat.cs`, a throwaway `.csproj`) and that scaffolding has been retired:
the Vesper-compiled `formatter.fs` + `structural-printer.fs` are the sole CLR runtime.

## Deferred seams (designed for, not built)

- **Typed `Child` atoms** for JS fidelity — overloads for the IntrinsicRepr-encodable
  primitives (bool, char, string, sbyte…uint64, float32, float, decimal). Enumerate
  from `IntrinsicRepr`, *not* `DocLayout.formatPrimitive` (they disagree in ways that
  matter for a set you cannot cheaply extend once frozen). Use **suffixed names**
  (`ChildInt32`), not overloads: JS has no overload resolution, and this is a frozen
  cross-target interface. They buy nothing load-bearing on the CLR (the box
  round-trips the runtime type), so they wait for the per-type JS `Format` emitter.
- **Per-type JS `Format` emitter** — the consumer the typed atoms serve.
- **Zero-alloc partial application** — [printf-partial-app-plan](printf-partial-app-plan.md).
- **Upstream RFC questions** (a `Case(name)` nullary shortcut, a `kind` enum arg on
  `BeginRecord` for anonymous records, `%+A`) — only if the FSharp.Core pitch
  proceeds. On a no-default-interface-method frozen surface, prefer an extensible
  enum arg over a new method.
