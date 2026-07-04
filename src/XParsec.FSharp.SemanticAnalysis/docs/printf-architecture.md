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
| partially-applied literal | stateless/captured `Fun` value struct whose `Invoke` is statically unrolled through the same `EmitFormat` lowering as the happy path — no runtime spec, no `PrintfFormat` | deferred → [printf-partial-app-plan](printf-partial-app-plan.md) |
| format-as-value / non-literal / `%a` `%t` | runtime parse → spec-runner → handler | falls back to FSharp.Core `PrintfModule` |

Anything not on the fully-applied-literal happy path currently routes to FSharp.Core's
cold printf (`FreezeExpr.fs` leaves the `App printfn` intact). That path is correct
but allocates the `PrintfFormat` object + closures the happy path avoids; the
partial-app plan replaces it for the lowerable case.

The partial-app row updated (2026-07-04): the earlier design drove `Invoke` through a
runtime spec-runner over a `static readonly` parsed-spec field. That is superseded — for a
*literal* format the spec is a compile-time constant, so `Invoke` is statically unrolled
through the same `EmitFormat.fs` lowering as the happy path. `PrintfFormat` and the runtime
spec-runner survive only on the **cold** (format-as-value / non-literal) row.

**Star-width holes (`%*d`, `%*.*f`).** The `*` width/precision consumes the dimension as an
extra `int` argument *preceding* the value: `sprintf "%*.*f" 12 1 x` applies width, then
precision, then the value. Lexing (`FormatDim`), the typing seam (`argTypes`), and native
lowering of the width-star forms below are **implemented**; star-*precision* is designed
(see the Star-precision bullet) but still rides the cold path, as do the tracked residuals
(`%0*d`, flagged star-`%A`).

Semantics verified against F# (fsi, 2026-07-04):
- **Arg order** is width, precision, value (`%*.*f` above).
- **Every flag combines with `*`** — `%-*d`, `%0*d`, `%+*d` all type-check.
- **A negative runtime width throws** `ArgumentOutOfRangeException` (`totalWidth`, from the
  underlying `PadLeft`/`PadRight`) — there is **no** C-style negative-width-means-left-justify.
- **`%*A` is legal**: the star feeds the `%A` print-*width budget* at runtime
  (`printf "%*A" 1 [1;2;3]` breaks at column 1).
- **Interpolated strings reject star** (F# emits a misleading FS3371). Under our relaxed
  lexer the rejection moves to the typing layer, which should emit an *accurate* diagnostic.

The model, per layer:
- **Lexer**: `FormatPlaceholder.Width`/`.Precision` become a three-state
  `FormatDim = Absent | Literal of bigint | Star` — not `voption` + a star bool, which
  admits `Literal ∧ Star`; the shape change deliberately breaks every consumer so each
  decides what `Star` means for it. The grammar edit is local to `lFormatPlaceholder`, so
  `parseFormatSpecifierView` inherits it. Once this lexes, `%*d` stops being a compile error.
- **Typing**: the per-hole seam becomes `PrintfSpec.argTypes : FormatPlaceholder ->
  SemType list voption` — `[star-width int; star-precision int; value]`, holes that don't
  type (`%a`/`%t`) still `ValueNone`. With the lexer + this layer alone, `%*d` runs
  correctly via the cold FSharp.Core path (a degrade, not an error — and *silent*, unlike
  `%a`/`%t`: a star hole types fine, so it warrants no diagnostic, same as `%+05d` today).
  The application fold in `tryInferPrintfApp` is already arity-agnostic (it unifies
  `TyFun` per *arg* against the curried printer type), so mid-hole partials
  (`printfn "%*d" 5` : `int -> unit`) type correctly with no extra work. Two gates DO bake
  in holes = args and must be rewritten against the seam: the happy-path marker's
  `args.Length = specs.Length + 1` becomes `totalArity + 1`, and the partial-app (4a)
  marker gains an **explicit per-hole-arity-1 predicate on `PrintfSpec`** (star holes are
  all-concrete, so `hasConcreteArgType` alone would silently admit them to the Fun-K peel —
  which assumes one arg per hole — the moment `tryClassify` starts accepting star; the
  named predicate turns that coincidence into a stated invariant, and extending the peel to
  multi-arg holes becomes a deliberate later decision). Interpolated strings are the one
  *new* diagnostic: `$"%*d{x}"` (`InferLiteralExpr.inferString`) errors accurately — the
  star has no argument to consume — instead of F#'s misleading FS3371.
- **`FormatType` is a field, never a currency.** It earns its keep as the case-collapsed
  identity of the type letter *inside* `FormatPlaceholder` (the big matches in `tryClassify`
  / `argType` dispatch on it; `TypeChar` keeps the case) — but no cross-pass signature
  carries a bare `FormatType list`: star-ness lives on the placeholder, so
  `InferLiterals.formatSpecifiers` returns `FormatPlaceholder list voption` and
  `appliedTypeOf` takes placeholders, projecting `.Type` only where the letter is genuinely
  all that's needed. Consequence: `lowerablePlaceholders` collapses to a fold over
  `formatSpecifiers`' result (they walk the same parts with the same rejections today, each
  re-parsing every placeholder — one parse, one walk, and the two can no longer drift).
- **Lowering** (native width-star):
  - `HoleForm.Field`'s alignment slot becomes `Alignment = None | Const of int (signed,
    negative = left-justify, today's convention) | Star of leftJustify: bool`. Star is
    admitted exactly where a literal width already lands in the alignment slot (incl. the
    `ForcedSign` arm, so `%+*d` / `% *d` classify); the zero-pad star forms (`%0*d` — width
    rides *inside* `FieldFormat`) and **star-precision** stay cold: a legal degrade only
    until the cold recipes are deleted (the coverage-plan capstone sequences after them).
  - `FormatSegG` gains a width-carrying hole case (width expr + spec + value expr) — the
    `Hole(spec, arg)` 1:1 shape is the lowering-side holes = args pin.
    `translatePrintfFormat` walks the (marker-guaranteed fully-applied) args by per-hole
    arity instead of one per hole. The spec's `Alignment.Star` and the segment's width expr
    are constructed together from the same placeholder — consumers may assume they agree.
  - **No new generic handler members.** Emission must *not* fold the `-` flag into a signed
    alignment (F# *throws* on a negative runtime width; a negated negative would silently
    right-justify). Instead a single width-guard helper on the handler
    (`int -> int`, throwing `PadLeft`'s exact `ArgumentOutOfRangeException("totalWidth")`
    on negative) feeds the *existing* signed-alignment members — negate after the guard for
    `leftJustify`. All alignment-capable members (incl. `AppendBool`/`AppendUnsigned`/
    `AppendOctal`) get star support from this one helper.
  - **Evaluation order**: F# evaluates the width argument *before* the value (curried
    application order), but the handler members take `(value, alignment, …)` — so the
    emitter must spill the guarded width to a local *before* emitting the value expression,
    not push it in parameter order. Same for `%*A`'s `widthBudget` (parameter follows the
    value in `AppendStructured`).
  - JS backend: pads via `padStart`/`padEnd` with the equivalent negative-width guard;
    the thrown error's *type* diverges from the CLR (documented, like the `%e`/`%g`
    approximations) but throws-vs-pads agrees.
- **Star-precision** (design settled against `XParsec.FSharp.Lib/Printf/printf.fs`; not yet
  implemented — the cold path remains correct meanwhile):
  - FSharp.Core builds the .NET format string *per call*
    (`getFormatForFloat ch prec = ch.ToString() + prec.ToString()`, `printf.fs:606`) — an
    allocation the native handler avoids: a dedicated dynamic-precision member builds the
    format in a `stackalloc` span and renders via `ISpanFormattable.TryFormat`. Build it
    from the **source type char + raw digits**, exactly mirroring `getFormatForFloat`:
    byte parity with FSharp.Core's quirks then emerges for free (see next).
  - **Clamp asymmetry is load-bearing, verified in fsi (2026-07-04)**:
    `normalizePrecision` (clamp 0..99, `printf.fs:608`) is applied on the
    width=\*+prec=\* path (`:632`) but NOT the prec=\*-only paths (`:649-657`). So
    `sprintf "%.*f" -1 3.14` = `"f-1"` (the invalid standard format falls back to .NET
    *custom*-format interpretation and echoes its literals) and precision 105 yields 105
    digits — while `%*.*f` clamps the same inputs to 0 / 99. Mirror both paths as-is;
    `runParity` (oracle = the process's own sprintf) enforces the quirk.
  - Scope: `Fixed`/`Exponential`/`Compact`/`ForcedSign` precision slots become a two-state
    dim (`Const of int | Star`), like `Alignment`; `PercentA`'s size budget likewise —
    `%.*A` is a runtime `sizeBudget`, already a parameter of `AppendStructured`, so it
    rides along free. `FixedZeroPad` stays literal-only (zero-pad star remains cold).
  - The width-carrying format segment generalizes to one dynamic-hole record
    (`{ Width: voption; Precision: voption; Spec; Value }`) rather than a case per
    combination; Freeze constructs dim-presence and spec agreement together. Emission
    spills width then precision locals before the value (evaluation order, as for width).
  - JS: `toFixed`/`toExponential`/`toPrecision` accept runtime precision natively; the
    existing approximation caveats carry over.
  - `%0A`'s zero-width flag **wins** over a star width (`printf.fs:1117` — the width arg
    is consumed but ignored), settling the flagged-`%0*A` semantics if it ever goes
    native; it stays cold for now.
  - **Accepted deviation (maintainer, 2026-07-04): `%*%` / `%5%` are rejected.** F# accepts a
    width on the percent *escape* (`StepPercentStar1`, `printf.fs:418`), consuming the
    width argument and writing a bare unpadded `%`. Our lexer rejects both forms
    (compile error). Left rejected: an arity-affecting no-op even FSharp.Core doesn't
    render.
  - `%*A` (verified in fsi, 2026-07-04): a negative runtime width does **not** throw — it
    renders flat (`-1` never breaks where `1` breaks per element), so the native emission
    clamps negative to `0` (= `PrintWidth.Never`) rather than guard-throwing; the same
    width-spill preserves evaluation order (`AppendStructured`'s budget parameter follows
    the value). Only *bare* `%*A` lowers natively: the flagged star forms
    (`%-*A`/`%+*A`/`%0*A`) type-check in F# (the star int is consumed) but their
    flag-vs-star layout interaction is unverified, so they stay cold residuals alongside
    star-precision.

Because of star-width, **arity is computed per hole, never as a hole count.** Today every
hole yields exactly one arg, so arity = hole count and `args.Length = specs.Length + 1`
holds — but no lowering should assume holes = args: every arity consumer routes through the
seam above — the happy path and the partial-app arity peel
([printf-partial-app-plan](printf-partial-app-plan.md)) count `argTypes(p).Length` — and
absorbs the length-2/3 holes without a structural change.

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
