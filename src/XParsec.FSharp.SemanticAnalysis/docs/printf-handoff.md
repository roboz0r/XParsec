# Printf — completing coverage (handoff)

Hand-off for **everything `printf` still leaves on the FSharp.Core cold path,
except `%A`.** Two phases have shipped:

- **P1** — the additive happy path: fully-applied literal
  `printf`/`printfn`/`eprintf`/`eprintfn`/`sprintf`, lowered to inline
  `Vesper.Formatter` calls. Specifiers `%s` / `%d` / `%f` only.
- **P2** — widened the literal happy path to `%O` / `%x` / `%X` / `%B` / `%e` /
  `%E` / `%u` / `%b` / `%o`, the `-` (left-align) and `0` (zero-pad on the integer
  bases) flags, and the `%%` escape. See "P2 as-built" below — that is the seam
  this work extends.

What's left (this handoff): the last literal specifiers (~~`%c` / `%M`~~ **done —
see A1**, `%g` / `%G`), the last flags (~~`+` / space~~ **done — see B1**,
`0`-on-float), the callback specifiers (`%a` / `%t`), and the lowerings *beyond*
fully-applied literals — partial application, format-as-value, and finally
cutting FSharp.Core off the printf path. ~~Plus the orthogonal `$"..."`
interpolation track that reuses the same node.~~ **(interpolation done — see E.)**

Read first: the design ([vesper-printf-plan.md](vesper-printf-plan.md)) and "P2
as-built". Pull the memories `project_printf_p1_implemented`,
`project_printf_p1_csharp_dll`, `project_printf_p2_implemented`,
`project_eqarray_tast`.

**Out of scope (one deferral only):**
- `%A` / structural formatting → **design-doc P3** (reflection-free
  `IStructFormat.FormatTo` synthesis per record/DU). `%A` keeps its FSharp.Core
  cold path until then. Everything else is in scope here.

## North star (unchanged)

Maximise the fraction of real-world printf that takes the zero-alloc fast path,
**without ever shipping output that differs from F#'s `printf`.** The cold path
is correct (it *is* FSharp.Core), so anything we can't render byte-for-byte stays
deferred — additive, never a regression. **Parity is the gate, not coverage.**
Three of the items below (`%g`/`%G`, `+`/space, `%a`/`%t`) are parity- or
complexity-traps where "keep cold" may be the right long-term answer; each says
so explicitly.

## P2 as-built (the seam this work extends)

The literal happy-path machinery is mature; most of what follows *feeds it more
cases* or extends the same nodes. Component map (current state):

| Piece | Location | Role |
|---|---|---|
| `tryHoleFormat` | `PrintfSpec.fs` | **The gate.** `FormatPlaceholder -> (HoleKind * string option * int option) voption`. `ValueNone` ⇒ cold path. Covers `%s`/`%d`/`%i`/`%f`/`%O`/`%x`/`%X`/`%B`/`%e`/`%E` (`Formatted`), `%u` (`Unsigned`), `%b` (`BoolText`), `%o` (`Octal`); the `-` flag → negative alignment, `0` → a width-bearing format on the integer bases. Defers `+`/space, `0`-on-float, `%c`/`%M`/`%g`/`%G`/`%a`/`%t`/`%A`. |
| `HoleKind` | `PrintfSpec.fs` | `Formatted | BoolText | Unsigned | Octal`. **Lives in `PrintfSpec.fs`, not `Tast.fs`** — `PrintfSpec` compiles first, and `tryHoleFormat` (the one place that classifies a specifier) must name the kind it returns. `Tast.HoleSpec.Kind` references it. |
| `FormatPlaceholder.TypeChar` | `Lexing.fs` | the raw type letter (`'x'` vs `'X'`, `'e'` vs `'E'`), so `tryHoleFormat` can pick the case the `FormatType` collapses. |
| `sinkOf` | `PrintfSpec.fs` | entry-point name → `PrintfSink` (StdOut/StdErr/StringResult). |
| `argType` | `PrintfSpec.fs` | `FormatType -> SemType voption`. Already maps `Char→char`, `Decimal→decimal`, `Object`/`Structured→fresh`; returns `ValueNone` for `%a`/`%t` (so they don't yet type — see C). |
| `lowerablePlaceholders` | `Passes/Unification.fs` | every specifier lowerable per `tryHoleFormat`; `%%` is lowerable; only interpolation `Expr` / orphan / lexer-error parts defer. |
| `tryInferPrintfApp` | `Passes/Unification.fs` | sets the `PassContext.PrintfApp` marker when fully-applied + arg-0 sink + `lowerablePlaceholders`. **Assumes one arg per specifier** (`args.Length = specs.Length + 1`) — `%a` breaks this (see C). |
| `translatePrintfFormat` | `Freeze.fs` | marker ⇒ builds `TExpr.Format`: walks `StringPart`s, re-parses each specifier via `tryHoleFormat`, pairs holes with args in order, threads the `HoleKind`. Collapses `%%`→`%` in raw literal runs. Hole `Ty` = the arg's zonked type. |
| `TExpr.Format` / `FormatSink` / `FormatSeg` / `HoleSpec` | `Tast.fs` | the node. `HoleSpec = { Ty; Kind: PrintfSpec.HoleKind; Format: string option; Alignment: int option }`. |
| `emitFormat` | `Codegen.Clr/Emit.fs` | ref-struct local + ctor, fold segments, trailing `\n` for `…n` sinks, `Flush` / `ToStringAndClear`. Dispatches on `hole.Kind`: `Formatted`→`AppendFormatted<Ty>(v[,align][,fmt])`; `BoolText`/`Octal`/`Unsigned`→the matching handler member (value + alignment always pushed, 0 ⇒ no padding). |
| `FormatHandles` | `Codegen.Clr/ICodegenProvider.fs` + `ClrProvider.fs` | the `AppendFormatted(ty,hasAlign,hasFmt)` factory + `AppendBool`/`AppendOctal`/`AppendUnsigned` member refs + `Console.get_Out/Error`. |
| `Formatter` | `src/Vesper.Printf/Formatter.cs` | the write-through ref struct (InvariantCulture). `AppendLiteral`, `AppendFormatted` (+ align/format overloads), `AppendBool(bool,int)`, `AppendOctal(int,int)` (`Convert.ToString(v,8)`), `AppendUnsigned(uint,int)`, `Flush`, `ToStringAndClear`. C# DLL, copied beside the PE in `materialiseApp`. |

Two shapes the new specifiers reuse:
- **A `Formatted` hole** is anything that maps to "ToString/TryFormat of `T`
  under a .NET format string + a field-width." `%c` / `%M` and the `+`/space
  flags are all *just* `tryHoleFormat` additions on this shape (plus, for
  `%c`/`%M`, the const-subset prerequisite) — **no new `HoleKind`.**
- **A dedicated handler member** (`AppendBool`/`AppendOctal`/`AppendUnsigned`)
  is how P2 handled the cases with no `AppendFormatted<T>(…, fmt)` shape. `%g`,
  `0`-on-float, and (if pursued) `%a`/`%t` follow this pattern: a new
  `Formatter` member + `FormatHandles` field + `emitFormat` arm.

## A. Remaining literal specifiers

### A1. `%c` / `%M` — blocked on the const subset (not printf-specific)

> **Status: DONE (2026-05-23).** The TAST const subset now carries
> `TConstValue.Char` / `TConstValue.Decimal`; `literalCarrier` types
> `CharLiteral → char` and `NumDecimal* → decimal`; `Freeze.parseLiteral`
> decodes both (a `parseCharLiteral` helper mirrors the lexer's `pCharChar`
> escape set; decimal strips the `M`/`m` suffix and parses invariant);
> `encodeType` maps `char → te.Char()` and `decimal → System.Decimal` (value
> type); `emitExpr` emits `char` as `ldc.i4 <codepoint>` and `decimal` via
> `System.Decimal::.ctor(lo,mid,hi,sign,scale)` (new `ICodegenProvider.DecimalCtor`
> handle). Both `tryHoleFormat` arms are now `Formatted` holes (no format
> string). `%M` with a precision (`%.2M`) and zero-pad on either stay cold.
> Shape + runtime-parity + local-slot round-trip tests in
> `PrintfHappyPathTests.fs`.

`argType` already types these (`Char→char`, `Decimal→decimal`). The block is the
**TAST const subset**: char and decimal *literals* don't round-trip yet, so the
arg can't be frozen.

- `literalCarrier` (`Unification.fs:1047`) types a `Token.CharLiteral` /
  `Token.NumDecimal*` token as **`int`** (the `_ -> tyInt` fallback) — a
  mistype.
- `Freeze.parseConst` → `parseLiteral` falls through to `Int32.Parse` for those
  tokens — a char/decimal literal **crashes Freeze** today.
- `TConstValue` (`Tast.fs`) has no `Char` / `Decimal`.

Work (each is its own small task; **unblocks char/decimal literals everywhere,
not just printf**):

1. `TConstValue.Char of char` + `TConstValue.Decimal of decimal` (`Tast.fs`).
2. `literalCarrier`: `Token.CharLiteral → char`; `Token.NumDecimal*` → `decimal`.
3. `Freeze.parseLiteral`: parse the char-literal token → `Char`; the decimal
   token (strip the `M`/`m` suffix) → `Decimal`.
4. `encodeType` (`ClrProvider.fs`): `TyConst "char" → te.Char()`;
   `TyConst "decimal"` → the `System.Decimal` value-type ref. (`encodeType "char"`
   was added then removed in P1 — re-add it here.)
5. `emitExpr`'s `TExpr.Const` arm (`Emit.fs`): `char` → `ldc.i4 <code point>`;
   `decimal` → the `System.Decimal::.ctor(int32,int32,int32,bool,uint8)`
   (lo/mid/hi/sign/scale) the same way F#/Roslyn emit a decimal constant (for a
   whole number that fits, `System.Decimal::.ctor(int32)` is simpler).

Then flip the `tryHoleFormat` arms (both become `Formatted`, no new kind):

- **`%c`** → `Formatted`, `Ty = char`, `format = None`. `char` is **not**
  `IFormattable`, so `AppendFormatted<char>` falls to `value.ToString()` → the
  one-char string. Alignment via the `(T, alignment)` overload. Zero-pad on
  `%c`: defer.
- **`%M`** → `Formatted`, `Ty = decimal`, `format = None` — `decimal` is
  `ISpanFormattable`; `TryFormat` under Invariant matches F# `%M` (invariant).
  If a precision is present (`%.2M`) verify against `sprintf` before mapping it to
  `"F<prec>"` — F# `%M` precision semantics are unusual; defer if unsure.

### A2. `%g` / `%G` — compact float (parity trap)

F# `%g`: the shorter of `%e`/`%f`, default 6 *significant* digits, **lowercase**
`e` exponent, trailing zeros stripped. .NET `"G<n>"`: `n` significant digits but
(a) **uppercases the exponent `E`**, (b) a different exponential threshold, and
(c) different trailing-zero rules than C-style `%g`. So `"G6"` ≠ F# `%g`
byte-for-byte.

To ship it parity-clean you need a dedicated handler member, e.g.
`AppendCompactFloat(double value, bool upper, int precision, int alignment)`,
that **reproduces F#'s `%g`** — either by mirroring FSharp.Core's float-`%g` path,
or `value.ToString("G<prec>")` then post-processing (`E`→`e` for `%g`, exponent
digit-count, trailing zeros). **Verify against a wide value sweep** (many
magnitudes, near-thresholds, subnormals, `0.0`/`-0.0`, ∞, NaN) — if any case
diverges from `sprintf "%g"`, **keep `%g`/`%G` cold.** This is the trap the P1/P2
docs flagged; don't ship "close enough".

## B. Remaining flags

### B1. `+` / space (forced sign) — a `Formatted` addition

> **Status: DONE (2026-05-23).** `tryHoleFormat`'s `plusSign || spaceSign` arm
> now lowers the signed decimal-integer (`%+d`/`% d`) and fixed-point-float
> (`%+.2f`/`% .2f`) specifiers to a `Formatted` hole carrying a custom .NET
> *section* format string (`"+0;-0"` / `" 0;-0"`; the float body is built from
> the precision, e.g. `"+0.00;-0.00"`). A width rides as a handler alignment
> (the `AppendFormatted<T>(value, alignment, format)` overload already existed —
> no codegen change). Deferred (cold): `+`/space on any other type (`%e`'s
> custom-exponent width diverges from `"e6"`; `%M` scale would be lost), and
> `+`/space combined with `0` (zero-pad). Shape + runtime-parity tests
> (negative/zero/positive for int; positive + precision + width-alignment for
> float — a negative *float* can't be produced in the codegen subset, so its
> section is covered by the shape test) in `PrintfHappyPathTests.fs`.

`%+d` → `"+42"`; `% d` → `" 42"`. Map via a custom .NET *section* format string;
the hole stays `Formatted` (no new kind), so this is a `tryHoleFormat` change plus
verification:

- int: `"+0;-0"` (force `+`; the negative section keeps `-`). Verify: `42`→`"+42"`,
  `-42`→`"-42"`, `0`→`"+0"` (positive section — matches F# `%+d 0 = "+0"`). Space:
  `" 0;-0"` → `" 42"` / `"-42"` / `" 0"`.
- float `%+.2f`: build `"+0.00;-0.00"` from the precision.

Caveats: a custom section format and an *alignment* don't compose cleanly, and
neither does `+`/space *with* the `0` flag. **Support `+`/space alone (or with
width-as-alignment); defer the `0`+sign combination.** Verify across
negative/zero/positive and (for floats) with precision.

### B2. `0`-on-float — a handler member

> **Status: DONE (2026-05-23).** `%08.2f` (and `%08f`, `%010.3f`, `%08.0f`)
> lower to a new `HoleKind.ZeroPaddedFloat` driven by a `Formatter`
> `AppendZeroPaddedFloat(double value, string format, int width)` member: it
> appends the `"F<prec>"` body (the existing `AppendFormatted<T>(value, format)`
> path), then — if the body is shorter than `width` — inserts `0`s **after any
> leading `-` sign** to reach the field width (ground-truthed against `sprintf`:
> `%08.2f -3.14159 = "-0003.14"`, `%08.2f 3.14159 = "00003.14"`,
> `%08.0f 3.0 = "00000003"`, body-wider-than-field left untouched). The hole
> reuses `HoleSpec` with no new field — the `"F<prec>"` body rides in `Format`,
> the field width in `Alignment`; `tryHoleFormat`'s `FloatDecimal` arm returns
> `ZeroPaddedFloat` when `zeroPad` (else the prior `Formatted "F<prec>"`). Touched:
> `PrintfSpec.fs` (`HoleKind` + `tryHoleFormat`), `Formatter.cs`/`formatter.fsi`,
> `FormatHandles` (`ICodegenProvider.fs` + `ClrProvider.fs`), `Emit.emitFormat`
> (new arm: push value, format, width; `argc = 4`). **Cold (deferred):** `0`-on-
> `%e` (exponent zero-pad parity is subtle — only `%f` is lowered), and `0`+sign
> (already cold from B1). A negative *float* can't be produced in the codegen
> subset (no float arithmetic / unary negation), so the sign-then-zeros path is
> covered by the C# handler logic + the `%08.2f` shape test; positive / zero /
> wider-than-field run as `runParity` tests in `PrintfHappyPathTests.fs`.

`%08.2f`: F# zero-pads the *whole field* after the sign; .NET has no float format
that zero-pads to a total width. Add a handler member (e.g.
`AppendZeroPaddedFloat(double value, int precision, int width, int alignment)`)
that formats `F<prec>` then inserts `0`s **after** the sign to reach `width`.
Verify sign/zero placement against `sprintf`. Rare; defer-acceptable.

## C. `%a` / `%t` — callback specifiers (recommendation: keep cold)

These are structurally unlike every other specifier — they consume a **callback
that is handed the output sink (`'State`)**, not a value:

- `%t` consumes **one** arg: `'State -> 'Residue` (writer families:
  `TextWriter -> unit`). `printf "%t" f` calls `f writer`.
- `%a` consumes **two** args: `('State -> 'T -> 'Residue)` + a `'T` (writer
  families: `(TextWriter -> 'T -> unit)` + `'T`). `printf "%a" f x` calls
  `f writer x`.

Why they're hard on the happy path:

1. **Typing.** `argType` returns `ValueNone` for `FormatFunction` (`%a`) /
   `Text` (`%t`), so they never type on the happy path → cold today. Modelling
   them means typing the callback against the `State`/`Residue` typars **and**
   teaching `appliedTypeOf` + the `tryInferPrintfApp` marker that `%a` adds *two*
   args and `%t` *one* — the `args.Length = specs.Length + 1` invariant is wrong
   for them.
2. **Lowering.** The callback wants the *sink*. The handler buffers, so you'd
   `Flush` it to the underlying `TextWriter`, invoke the callback (which writes
   directly), then resume the handler — a new `FormatSeg.Callback` variant, not a
   value hole. For the writer families `'State = TextWriter` and the sink writer
   is to hand (`Console.Out`/`.Error`). For `sprintf` `'State = unit` — there is
   no `TextWriter`; you'd wrap a `StringWriter` (heavy) or keep `sprintf`'s
   `%a`/`%t` cold.

**Recommendation: keep `%a`/`%t` cold.** They are rare, the cold path renders
them correctly, and the happy-path version is a lot of structural work for little
real-world payoff. Implement only on demand, scoped to the writer families, via
`FormatSeg.Callback` + a flush-invoke-resume `emitFormat` arm.

## D. Beyond fully-applied literals (design-doc P4)

The remaining *lowerings* (the design doc's "three lowerings" rows 2–3, plus the
FSharp.Core cut). These are heavier and largely specified in
[vesper-printf-plan.md](vesper-printf-plan.md) §P4 — covered here for completeness
with pointers, not re-derived.

### D1. Partial application — a `Fun` value struct over a static spec

`let p = printfn "%d"` (fewer args than holes). Detect: literal format + sink +
`args.Length < specs.Length + 1`. Synthesise a `Fun<…>` value struct whose only
state is the format — a **`static readonly` parsed-spec field** ⇒ stateless
`default(S)`, zero heap, devirtualised `Invoke`. Multi-hole currying is a `Fun`
chain capturing the bound args. Rides the slice-5 closure work and the `Fun`
escape analysis ([function-representation-plan](function-representation-plan.md)).
Heavy.

### D2. Format-as-value / non-literal — the runtime spec-runner

A `PrintfFormat` flowing as a value, or a non-literal format string. Needs a
**runtime spec-runner**: parse the format at runtime → drive the same P1 handler.
This is the one place `PrintfFormat` survives — demoted to the parsed-spec object,
never on the happy path. The runner is the same engine D1's `Invoke` calls. Heavy.

### D3. Cut FSharp.Core off the printf path (the endgame)

Once D1/D2's runner exists: flip `printfFormatName` to `Vesper.PrintfFormat`,
delete the cold-path recipes (`emitPrintfn`, `emitPrintfFormatCtor`, the
`encodeType` `PrintfFormat` case, `TryEmitCall "printfn"`), and `Vesper.Printf`
is self-contained. Until D3, **everything stays additive** — don't flip
`printfFormatName` or touch those recipes; they serve everything the happy path
doesn't yet cover.

> **Tooling for the cut (2026-05-23):** `ClrArtifact.FSharpCoreDependencies` (from
> `ICodegenProvider.FSharpCoreDependencies`) reports, per build, exactly which
> FSharp.Core constructs the emitted PE still references — `ClrProvider`'s
> `markFSharpCoreDep` records each use-site, so it's the authoritative cut-list.
> `materialiseApp` already copies `FSharp.Core.dll` *conditionally* on it being
> non-empty, so the happy path (printf/interpolation/arithmetic) already ships
> apps with **no FSharp.Core**. D3 is "drive that set empty for the printf
> constructs"; watch `PrintfModule.PrintFormatLine` / `PrintfFormat`4` drop out.

## E. `$"..."` interpolation (D9) — parallel track

> **Status: DONE (2026-05-23).** An interpolated string with holes lowers to a
> `TExpr.Format` with a `ToString` sink — the same node the printf happy path
> produces. Front-end map:
> - `CstWalk.iterExpr` was a no-op on `Expr.String` (a leaf), so NameResolution /
>   Unification never descended into hole exprs — a `{name}` ident never resolved.
>   `iterExpr` now recurses into each `StringPart.Expr` hole (sharing the
>   enclosing scope). **This was the load-bearing fix; without it variable holes
>   silently fall back to the literal stub.**
> - `Unification.inferString` infers every hole expr (so Freeze can read its type
>   back) and unifies a `%d{x}` printf-typed hole against `PrintfSpec.argType`.
> - `Freeze.translateString` calls a new `tryTranslateInterpolation`: builds
>   `Lit` / `Hole` segments, classifying each hole via `tryInterpHoleSpec` — a
>   printf `%d{x}` reuses `tryHoleFormat`; a plain `{x}` / `{x:fmt}` is a
>   `Formatted` hole whose `:fmt` clause becomes the .NET format. Returns `None`
>   (keeping the literal-stitch fallback, extracted to `stitchLiteralString`)
>   when a hole has a free type, an unsupported `%spec`, or the string carries an
>   orphan/standalone `%spec` part. Interpolation alignment (`{x,n}`) isn't
>   representable (the parser folds `x,n` into a tuple), so alignment is `None`
>   for the plain forms.
>
> Shape + runtime-parity tests (plain hole, bound hole, multi-hole, `:X`/`:F2`
> clauses, typed `%d{x}`, sprintf-result reuse; parity oracle = the test
> process's own `$"..."`) in `PrintfHappyPathTests.fs`. Escape-sequence
> unescaping in literal runs stays the same pre-existing gap `translatePrintfFormat`
> carries (tests avoid escapes); `printfn $"…"` (an interpolated string *as a
> printf format*) stays cold — `formatSpecifiers` returns `ValueNone` for it.

Orthogonal but covered by the same node. An interpolated string `$"{x:d}"` lowers
to the **same** `TExpr.Format` (the `emitFormat` codegen half is already done —
it's literally the same handler). Remaining work is front-end: recognise
`Expr.String` with interpolation `Expr` parts, build `FormatSeg.Hole`s from the
holes (the `:format` clause → the `HoleSpec.Format`/`Alignment`), and pick the
sink (string-typed context → `ToString`). Reuses everything P1/P2 built; can land
any time. See [minimal-core-lib-plan](minimal-core-lib-plan.md) D9.

## Where each change lands

| Workstream | Files |
|---|---|
| **A1** `%c`/`%M` | `Tast.fs` (`TConstValue`), `Unification.fs` (`literalCarrier`), `Freeze.fs` (`parseLiteral`), `ClrProvider.fs` (`encodeType`), `Emit.fs` (`emitExpr` const), then `PrintfSpec.tryHoleFormat` (flip the two arms). |
| **A2** `%g`/`%G` | `Formatter.cs`+`.fsi`, `FormatHandles` (`ICodegenProvider.fs`+`ClrProvider.fs`), `Emit.emitFormat`, `PrintfSpec.tryHoleFormat`; new `HoleKind` if a member is added. |
| **B1** `+`/space | `PrintfSpec.tryHoleFormat` only (custom format string; stays `Formatted`). |
| **B2** `0`-float | `Formatter.cs`+`.fsi`, `FormatHandles`, `Emit.emitFormat`, `PrintfSpec.tryHoleFormat` (+`HoleKind`). |
| **C** `%a`/`%t` | `PrintfSpec.argType`/`appliedTypeOf`, `Unification.tryInferPrintfApp` (arg-count), `Tast` (`FormatSeg.Callback`), `Freeze`, `Emit.emitFormat`. (Recommend: skip.) |
| **D1–D3** | per [vesper-printf-plan.md](vesper-printf-plan.md) §P4 + `ClrProvider`/`Emit` cold-path recipes. |
| **E** interpolation | `Unification`/`Freeze` (recognise interpolation parts → `Format`). |

## Tests

`test/XParsec.FSharp.Codegen.Clr.Tests/PrintfHappyPathTests.fs` is the home. Per
specifier add a **shape** test (Format node with the right `HoleKind`/`Format`/
`Alignment`) and a **runtime parity** test — `runParity` compiles + runs
in-process and asserts against `sprintf`-of-the-same-format **evaluated in the
test process**, so it fails the instant output drifts from real F#. Keep the
**cold-path fallback** tests for whatever stays deferred (`%A`, and `%g`/`%a`/`%t`
if you keep them cold), asserting the TAST is *not* a `Format` node and the cold
path still prints correctly. The full suite (codegen + semantic-analysis) must
stay green; design-doc P3 (`%A`) stays on the cold path.

The const-subset work (A1) also wants its own non-printf tests: char/decimal
literals binding, typing, and round-tripping through codegen.

## Build / test / format

Use the **`xparsec-dev`** skill (`./claude_tools.cmd`), not raw `dotnet`:

```
./claude_tools.cmd -Action Build
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests"
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.SemanticAnalysis.Tests"
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Tests"   # lexer/parser
```

Then format only your edited F# files with Fantomas (whole-repo format can churn
unrelated in-flight files on this branch). The C# DLL is referenced by the
codegen project, so a clean codegen build pulls it in; `materialiseApp` copies it
beside the PE.

> Note: 3 `VesperCoreContract.Parsing` tests in `XParsec.FSharp.Tests` fail
> pre-existing on this branch (a `RefParser was not initialized` issue in `.fsi`
> attribute parsing, unrelated to printf). Don't chase them.

## Gotchas

- **Parity over coverage.** The cold path is correct F#; never lower a case you
  can't render identically. `%g`/`%G`, `0`-on-float, `+`/space, and `%a`/`%t` are
  the traps — defer rather than ship "close enough".
- **`%c`/`%M` are blocked on the const subset, not on printf.** A char/decimal
  *literal* crashes Freeze today (`literalCarrier`+`parseLiteral`). Land the TAST
  const extension first; the specifiers then fall out as `Formatted` holes.
- **`%g` ≠ .NET "G".** .NET uppercases the exponent and uses different
  threshold/trailing-zero rules. Verify exhaustively or keep cold.
- **`%a`/`%t` hand the callback the sink** (`'State`), so they need a
  flush-invoke-resume lowering and break the one-arg-per-spec invariant. The
  realistic call is: keep cold.
- **`%O` is `Formatted` but culture-subtle.** The handler uses InvariantCulture;
  F# `%O` calls the boxed value's culture-sensitive `ToString()`. For `int`/
  `string` they coincide; for `float`/`decimal` `%O` they can diverge under a
  non-invariant culture. P2 accepts this under the project's invariant-culture
  stance — keep it in mind if `%O` parity is ever questioned.
- **`0` flag ≠ alignment.** Alignment space-pads; zero-pad needs a width-bearing
  format string (`"D5"`/`"x8"`). They're mutually exclusive on one hole — P2's
  `tryHoleFormat` already enforces this; new flag work must too.
- **Still additive (until D3).** Don't flip `printfFormatName`, don't touch the
  cold-path recipes (`emitPrintfn`, `emitPrintfFormatCtor`, `encodeType`
  PrintfFormat, `TryEmitCall "printfn"`) — they serve everything the happy path
  doesn't cover, and D3 owns their removal.
- `EqArray` is `NoComparison`; nothing orders TAST/`SemType`.
- The handler formats with `CultureInfo.InvariantCulture` — keep every new member
  on it (F# numeric `printf` is culture-invariant).
