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
cold printf (`ElaborateExpr.fs` leaves the `App printfn` intact). That path is correct
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
lowering of **both** the width-star and precision-star forms below are **implemented** (the
width-carrying `StarWidthHole` generalised to the one dynamic-hole record `DynHole`); the
tracked residuals (`%0*d`, `%0*.Nf`, flagged star-`%A`, `% A`) still ride the cold path.

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
- **Star-precision** (settled against `XParsec.FSharp.Lib/Printf/printf.fs`; **implemented**):
  - FSharp.Core builds the .NET format string *per call*
    (`getFormatForFloat ch prec = ch.ToString() + prec.ToString()`, `printf.fs:606`). The
    native handler's dynamic-precision members (`Formatter.AppendDynamicPrecisionFloat` /
    `AppendDynamicPrecisionSignedFloat`) build the same string from the **source type char +
    raw digits** and render through the existing `IFormattable.ToString(format, provider)`
    path (the `AppendZeroPaddedFloat` precedent), so byte parity with FSharp.Core's quirks
    emerges for free (see next). (Deviation from the earlier `stackalloc` +
    `ISpanFormattable.TryFormat` sketch: the Vesper-compiled runtime has no precedent for
    `Span`-from-pointer / `TryFormat`, and the `ToString` path is already the proven
    byte-parity one; the small format-string allocation matches what the handler and the
    port already do.)
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

Sinks built: write-through `TextWriter` (`fprintf`/`fprintfn`), write-through
`StringBuilder` (`bprintf` — `Flush` appends the buffer to the builder), and
string/`ToStringAndClear` (`sprintf`).

## Happy-path lowering

- **Recognition:** `Passes/Unification/InferApp.fs` (`tryInferPrintfApp`) marks a call
  for inline lowering only when it is a literal format, **fully applied**
  (`args.Length = specs.Length + 1`), with lowerable placeholders. Partial
  application, `%a`/`%t`, format-as-value, and `fprintf`/shadowing cases are left
  unmarked and fall through to FSharp.Core.
- **Freeze:** `ElaborateExpr.fs` diverts a marked call to a `TExpr.Format` node — no
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

## The sink model (designed, not built): contract-declared, provider-resolved

The printf machinery has two name-based couplings where it should have identity-based
ones, and they are the same coupling seen twice: **what the sink type IS** (`PrintfSpec`
mints `TyConst(RuntimeNames.opaqueKey "System.IO.TextWriter")` — an unresolved name in
the global namespace, `PrintfSpec.fs:28-35`) and **which functions are the family**
(a `Map<string, Family>` keyed on the last dotted segment of the written name,
`PrintfSpec.fs:291-313`). `RuntimeNames.fs:109-121` already names the intended exit —
"the deferred target-independent model resolves the sink type through the provider per
target". This section is that model, and the probes that decide it.

### What is actually true today (probed 2026-07-14, not inferred)

**A plain `fprintf` on JS does not diagnose. It crashes the compiler.** Compiling
`let go (w: System.IO.TextWriter) = fprintf w "%d" 42` through the JS provider
(`Pipeline.analyseSemForSelfHost` + `Codegen.compileWith`, the `PrintfSpecifierTests`
idiom) yields **zero diagnostics**, marks the happy path, freezes a
`TExpr.Format(ToWriter …)`, and then dies in the backend:
`failwithf "EmitJs: unsupported format sink %A"` (`Codegen.Js/EmitJs.fs:532` — the JS
`Format` arm handles only `ToStdOut true`, `ToStdErr true`, `ToString`). `bprintf`
crashes identically on `ToBuilder`. So the load-bearing premise is *worse* than
"silently emits": there is no diagnostic anywhere, and the failure surfaces as an
unhandled exception in codegen.

*(Adjacent, separate cause, and — under the rule below — squarely in scope: `printf` /
`eprintf`, the **no-newline** console forms, crash the same way on JS (`ToStdOut false` /
`ToStdErr false`, `EmitJs.fs:527-531`). That sink is perfectly expressible on JS
(`process.stdout.write`); it is simply unimplemented. Same failure *mode* — the gate marks
a sink for native lowering without ever asking the target whether it can lower it — but a
different axis. Because these calls are UNOBSERVABLE they must keep working, so emitting
them is part of this work. See "Two capability axes" below.)*

**The user's annotation does not unify by string coincidence — it unifies because it
is a free type variable.** `Translate.fs:539-571` (`resolveQualifiedTypeName`) returns
`TyVar(freshTyVar ctx)` for an unresolved **dotted** name under a non-local qualifier
(`:571`), and its own comment names the case: *"a stack with no BCL tail cannot resolve
`System.IO.TextWriter` yet must still type a body that mentions it"*. The `opaqueKey`
fallbacks at `Translate.fs:518` / `:673` apply only to **bare, single-segment** names,
which `System.IO.TextWriter` never is. So on a BCL-free stack the annotation is a free
typar, and a free typar unifies with anything. Probe:
`let go (w: Foo.Bar.Baz) = fprintf w "%d" 42` — a wholly unrelated nonsense type —
produces **zero diagnostics**, and the parameter freezes to
`FTConst(System.IO.TextWriter)`. Annotations that *do* resolve (`int`, `string`)
correctly error, so it is precisely the unresolvable ones that get laundered into the
sink. `PrintfTests.fs:373`'s `(w: System.IO.TextWriter)` is therefore decorative — the
test would pass with any spelling at all.

**The sink is already provider-resolved; the mechanism exists and works.**
`InferApp.fs:313-326` hands `PrintfSpec.resolveExternalSlots` a callback that does
`ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedTypeKey name 0)` and rewrites the
by-name `TyConst` slot to a real `TyClass`. `ctx.Provider` **is** in scope at
`PrintfSpec`'s only call site. Probed: the CLR stack
(`ClrSymbolProviders.buildContract` over the `Vesper.*` manifests) resolves
`System.IO.TextWriter`, `System.Text.StringBuilder`, `System.IO.StringWriter` all to
`Class(arity=0)`; the JS stack (`JsNativeSymbols.buildJsNativeContractFor`) returns
`ValueNone` for all three. **The feasibility crux is answered YES.** What is missing is
not resolution — it is that `resolveExternalSlots` treats `ValueNone` as *keep the
placeholder* ("a slot whose name `resolve` can't map keeps its by-name `TyConst` (no
regression)", `PrintfSpec.fs:322-325`) instead of *this target has no such sink*. That
one `ValueNone` arm is the whole bug: it manufactures a type nothing on the target
denotes, which then unifies with the free typar the annotation became.

**A contract declaration is already a per-target sink resolver.**
`Vesper.Printf/formatter.fsi:17,21` declares `Formatter` ctors taking `TextWriter` /
`StringBuilder`. The *same* contract extracts to
`(int * int * FTClass(System.IO.TextWriter))` under the CLR provider and to
`(int * int * FTUnknown)` under the JS provider (probed). `FTUnknown` is already the
type system's "this target cannot name this type" leaf. So per-target sink resolution
needs **no new machinery** — it is what contract extraction already does.

**The family is not declared.** `Vesper.Printf/printf.fsi` declares only `printf`,
`printfn`, `sprintf`. `fprintf` / `fprintfn` / `bprintf` / `eprintf` / `eprintfn` are
declared **nowhere** in the self-host contract — `Scope.fs:202-207` suppresses
"Unresolved identifier" for them via `PrintfSpec.tryFamily`, and says so verbatim
("the writer families are not in the `Vesper.Printf` contract"). The FSharp.Core port
does declare the whole family with real signatures
(`XParsec.FSharp.Lib/Printf/printf.fsi:165,200,238` — `val fprintf: textWriter:
TextWriter -> format: TextWriterFormat<'T> -> 'T`), but it is a different package and
is not in the self-host stack. Meanwhile the three that *are* declared already carry a
`SymbolKey` at every use site: `PassContext.Resolution.ExternalValue :
SideTable<SymbolKey>` (`PassContext.fs:225-231`), whose own doc says it exists so a
consumer can ask *"is this exactly `Vesper.Printf.printfn`?"* rather than suffix-match
a name. Probed: `TryLookupByKey` finds `Vesper.Printf.printf/printfn/sprintf`,
`ValueNone` for the writer/builder family. **The identity seam is already built and
already stamped — the family recogniser simply doesn't read it, because three of the
eight names it knows have no declaration to key on.**

### `printf.fsi`'s `State = unit` is a BUG, not a competing model

`printf.fsi` declares `printf` / `printfn : PrintfFormat<'Printer, unit, string, unit>
-> 'Printer` — **`State = unit`, `Residue = string`** — while `PrintfSpec.writerFamily`
(`:256-265`, applied to `printf`/`printfn`/`eprintf`/`eprintfn` at `:292-296`) gives them
**`State = TextWriter`, `Residue = unit`**. That is not a fork. In F# `printf fmt` **is**
`fprintf Console.Out fmt`: the state IS a `TextWriter`, which is precisely why `%a`'s
callback has somewhere to write. `PrintfSpec` has the right answer and the contract has
the wrong one; the disagreement is invisible today only because the compiler's table
overrides the contract at every call site.

**So the contract is corrected to `State = TextWriter`, and the family is sourced from
it.** Contract-sourcing the family *without* fixing the contract would enshrine the bug.

### The observability rule

Naively setting `State = TextWriter` regresses working JS. `printfn "%d" 42` lowers today
to `console.log(…)`: no sink object exists anywhere in the emitted program, and nothing in
the source can observe one. If the format's `'State` merely *mentions* `TextWriter` and the
JS provider cannot resolve it, that program would start diagnosing — and it must not.

The rule:

> **A sink type must resolve on a target IFF the program can OBSERVE it.**

Resolution is therefore **demand-driven**, not eager:

| Call | Sink is… | Resolve? | JS today |
|---|---|---|---|
| `fprintf w …` / `bprintf sb …` | a real **argument** | **yes** | **diagnoses** (intended) |
| `printf "%d"` / `printfn` / `eprintf` | never named, never passed | **no** | keeps working (`console.log`) |
| `printf "%a"` / `"%t"` | **received by the callback** | **yes** | **diagnoses** (unchanged) |

Both halves of the prize survive, `State = TextWriter` is true on every target (matching
F#), and the JS diagnostic becomes a statement about *the target*, not about printf: it
says "this target has no `TextWriter` **yet**". The day someone writes a JS `TextWriter`
shim into a contract, `fprintf` starts working on JS **with no compiler change**. That
property — the target's capability decides, not a compiler special case — is the one worth
designing for, and it is the reason to prefer this over any table of what-works-where.

**Where the rule is enforced, concretely.** Observability is **not** a function of the sink
kind. `printf "%d"` and `printf "%a"` share a sink kind (`PrintfSink.StdOut`) and differ in
observability; that is the whole difficulty, and any design keyed on `FormatSink` alone is
wrong. Observability is a function of **the family's arity shape and the hole set**:

```fsharp
/// True iff THIS call can observe the family's `State` sink — the sink is a real
/// leading ARGUMENT (`fprintf`/`bprintf`, `FormatArgIndex > 0`), or a `%a`/`%t`
/// callback RECEIVES it. Otherwise `State` appears only inside the synthesised
/// `PrintfFormat<_,'State,_,_>` type args, which nothing in the program unifies
/// against — so the sink need not exist on this target at all.
let observesSink (fam: Family) (specs: FormatPlaceholder list) : bool =
    fam.FormatArgIndex > 0 || specs |> List.exists (fun p -> isCallbackHole p.Type)
```

Both inputs are in scope at the gate — `fam.FormatArgIndex` (`InferApp.fs:328`) and the
`hasCallbackHole` fold the gate already computes (`InferApp.fs:407-408`). **Validated: the
`State` slot really is inert when `observesSink` is false.** `PrintfSpec.argTypes`
(`:185-205`) consumes `state`/`residue` on the `FormatFunction` / `Text` arms and **nowhere
else**; with no callback hole, `State` reaches only `formatType` (`:390-391`), i.e. the type
args of the `PrintfFormat` the gate synthesises for the format literal — and the gate
*builds and then unifies that shape against itself* (`fmtTy` is stamped onto the format arg
and unified with the domain of the `fnTy` derived from the same `fmtTy`, `InferApp.fs:357-386`),
never against anything the user wrote. So an unresolved `State` on an unobservable call
touches no unification the program can see. The honest filler for that slot is `TyUnknown`
(the SemType twin of the `FTUnknown` the JS contract extraction already produces), not
`TyConst(opaqueKey "System.IO.TextWriter")` — a name-shaped placeholder is exactly what let
a free typar launder into the sink in the first place.

**One real ordering change.** `resolveExternalSlots` runs at `InferApp.fs:313-326`, *before*
`formatSpecifiers` produces `specs` at `:348`. Demand-driven resolution must run *after* the
specifiers are known. Small, but it is a genuine reordering, not a rename.

### The design

**1. The family is declared; the declaration carries the SIGNAL and the SINK, never the
typing.** `Vesper.Printf/printf.fsi` grows the missing five `val`s, the corrected
`'State`, and the state-carrying format abbreviations
(`TextWriterFormat<'T> = Format<'T, TextWriter, unit, unit>`,
`BuilderFormat<'T> = Format<'T, StringBuilder, unit, unit>`) — the same shapes the
FSharp.Core port already declares (`XParsec.FSharp.Lib/Printf/printf.fsi:110-130`). printf's
typing is FORMAT-DIRECTED — `'Printer` is a function of the holes in the literal — so no
ordinary signature can type it, and the format-directed elaboration stays in the compiler
exactly where it is (`PrintfSpec.appliedTypeOf`). This is not a new liberty: the gate
*already* ignores the declared scheme of `printf`/`printfn`/`sprintf` and builds its own
`fnTy` from the literal (`InferApp.fs:353-386`). What the declaration adds is what a
signature genuinely *can* say — **this identity is the writer family, and its sink is this
type** — and, for free, says it per target (`FTClass(System.IO.TextWriter)` on CLR,
`FTUnknown` on JS).

**2. `PrintfSpec.Family` stops holding sink placeholders.** `State`, `ScratchSink` and
`LeadingArgTypes` are no longer `TyConst(opaqueKey "System.IO.TextWriter")` constants
(`PrintfSpec.fs:28-35`, `:256-301`). The gate reads them off the declaration it already has
stamped (`Resolution.ExternalSymbolStamp`, `PassContext.fs:232-249`): the leading parameter
types up to `FormatArgIndex`, and the `'State`/`'Residue` args of the format parameter's
`PrintfFormat`. `PrintfSpec` stays a **provider-free pure SemType module** (the property
`:320-326` deliberately protects) — the gate still owns `ctx.Provider` and hands in the
resolution; it just hands in a *resolved* answer rather than a name to look up.

**3. An unresolved sink is a DIAGNOSTIC exactly when `observesSink` holds.** The
`ValueNone -> keep the by-name TyConst` arm (`PrintfSpec.fs:322-325`, "no regression") is
deleted and replaced by the rule above: unobservable ⇒ the slot is `TyUnknown` and nobody
cares; observable ⇒ the call errors, in Unification, at the call site:

> `fprintf` requires a sink of type `System.IO.TextWriter`, which this target does not provide.

Not a `failwith` in codegen. `EmitJs.fs:532`'s `failwithf` then becomes the unreachable
codegen invariant it always claimed to be.

**4. The family is recognised by `SymbolKey`.** `PrintfSpec.tryFamily` (`:306-313`) and
`PrintfSpec.sinkOf` (`:61-75`) — two independent re-derivations of the last dotted segment
of a written name — collapse into one lookup keyed on `Resolution.ExternalValue` at the
head's `NodeKey`. Consequences that fall out rather than being engineered:
- `Scope.fs:202-207` (the suppression that lets an *undeclared* `fprintf` resolve at all)
  is **deleted**: the family is declared, so it resolves like any other value.
- `InferApp.fs:295`'s `ctx.Bindings.Binding.ContainsKey fnKey` shadow guard is **deleted**:
  a local `let fprintf` has no external key, so it cannot be a family member by
  construction. (`PrintfTests.fs:431`'s shadowing test then passes for the structural
  reason instead of the guarded one.)
- The recogniser must accept **either** package's key — `Vesper.Printf.fprintf` (self-host)
  or the FSharp.Core port's, for a stack that still references it. A two-faced identity,
  modelled exactly as `RuntimeNames.isPrintfFormatKey` already models `PrintfFormat`'s two
  faces (`RuntimeNames.fs:343-348`). This is the one place the "one key" story does not
  hold, and it should not pretend to.

**5. The `%a` capability gate stops being a special case — it becomes one clause of the
rule.** `callbackSinkAvailable` (`PrintfSpec.fs:356-359`) and the `rejectCallback`
diagnostic (`InferApp.fs:407-420`) are the observability rule already, asked about one hole
letter and about half the families. `observesSink` subsumes it: the callback clause *is*
`callbackSinkAvailable`'s job, and the argument clause is the half it was missing. The
`%a`-specific predicate is deleted; the JS `%a` diagnostic it produced survives unchanged,
now issued by the general rule. `familyNeedsScratch` (`:368`) likewise stays sound by
construction: a scratch is resolved only on a callback call, which is by definition
observable, so the scratch is never demanded on a target that cannot name the sink.

### The JS `TextWriter` shim — and the two things that block it

**Ruled (2026-07-14): `fprintf` / `bprintf` STAY in the shared contract.** They are not split
into per-target contracts. Vesper source is written identically for every target; the *sink
type* resolves per target through the provider — CLR binds it to `System.IO.TextWriter`, JS
binds it to a **Vesper-authored shim**. The JS diagnostic is therefore not a permanent
property of the target; it is the "no shim written yet" state, and the shim is the thing that
flips it off — **with no compiler change**, which is the property the whole design exists to
buy.

That settles *what* the shim is. Three questions decide whether it is **declarable**, not
merely writable. Probed 2026-07-14; two of the three answers are blockers.

**1. What the lowering ACTUALLY CALLS on the sink: `Write(string)`. Nothing else. So the
shim needs NO overloads, and the mangling dependency EVAPORATES.**

This is the question that sets the scope of the whole piece of work, so it is answered from
the code, not from the BCL's shape.

*CLR.* The sink is touched in exactly two places. It is passed as a **ctor argument** to the
`Vesper.Formatter` ref-struct (`EmitFormat.fs:58-63`: `ToWriter` → `CtorWriter`, `ToBuilder` →
`CtorBuilder`), and it is written to **once, in `Flush`** (`formatter.fs:350-357`):

```fsharp
match this.Writer with
| null -> match this.Builder with
          | null -> ()
          | sb -> sb.Append(this.Chars.Slice(0, this.Pos).ToString()) |> ignore
| w -> w.Write(this.Chars.Slice(0, this.Pos).ToString())
```

`Write(string)` and `Append(string)`. **Every hole has already been rendered to characters by
the handler** (`AppendFormatted` / `AppendBool` / `AppendZeroPaddedFloat` / …) *before* the
sink is ever touched, and `printfn`'s newline rides as a trailing literal segment inside the
same buffer. `System.IO.TextWriter`'s typed `Write(int)` / `Write(double)` / `WriteLine(…)`
overloads are **never called by the printf lowering at all**.

*JS.* The same conclusion, more directly. `EmitJsFormat.buildFormatArg` (`:465-497`) composes
**a string** — a `+`-concatenation of per-hole rendered `JsRawSeg`s — which the existing arms
hand to `console.log` (`EmitJs.fs:527-531`). So the `ToWriter` / `ToBuilder` arms that today
`failwithf` (`:532`) need to emit precisely `w.Write(<that string expr>)` and
`sb.Append(<that string expr>)`. One member each.

**Three consequences, and they change the scope of the work.**

- **The shim's declared surface has NO overloads.** `Write : string -> unit` (plus `Flush`,
  and `ToString : unit -> string` on the `StringWriter` scratch). Nothing overloaded, so
  nothing to mangle.
- **The dependency on `js-overload-mangling-plan.md` does not exist for this work.** That plan
  is real, fully specified, and unimplemented — and its *own* first consumer is the typed
  `IFormatSink.Child` overloads (`Child$int` / `Child$string` / `Child$obj`), which is also
  printf work (the "Typed `Child` atoms" deferred seam below). The two efforts are entangled at
  the root, but **not on this path**: the sink shim does not need it. If the shim's surface
  ever grows overloads — i.e. if the goal becomes a BCL-shaped `TextWriter` that *user* code
  can call as `w.Write 42` — then it is **hard-blocked on that plan landing first**, and that
  is a prerequisite with its own scope, not a detail of "write the shim". Do not conflate the
  two, and do not invent a second mangling: the scheme is already designed
  (`Type__member$<paramtoken>` readable / `Type__member$0<hash>` hashed, identity from
  `FrozenType`, serializer in `Codegen.Common`).
- **The `toFixed(1)` float-fidelity rule does NOT belong in the sink.** It cannot: printf never
  calls `Write(double)`. Float rendering is *already* owned by the formatter on both targets —
  CLR `Formatter.AppendFormatted` / `AppendDynamicPrecisionFloat`, JS `EmitJsFormat.buildHole`.
  Putting a `42.0 → "42.0"` rule in the sink would make it a **third** independent copy of a
  rule that already exists twice, firing only on a direct user `w.Write 42.0` that no printf
  path emits — a defect, not a feature. The rule stays where the rendering is.

**Net: this is "write a shim", not "implement a mangling scheme, then write a shim."**

*(For the record, since it decides the above: `JsExternalMembers.mangledName`
(`JsExternalMembers.fs:28-31`) is nominal-only — `typeName + "__" + memberName`, plus the
static/property variants — with no parameter or arity component, so two overloads of a name
mangle identically and one silently shadows the other. Its producers, `EmitJsMembers.fs:143`
and `EmitJsContext.fs:247` / `JsExternalMembers.fs:307`, all feed it `m.Name` alone. It is
also **unexercised** territory: the one heavily-overloaded Vesper contract, `formatter.fsi`
(three `new:` ctors + four `AppendFormatted`), is explicitly excluded from JS
(`Vesper.Printf/manifest.toml`: `inline-bodies-js = []`, no `impl-js`), so no JS-compiled
Vesper type has ever had an overloaded member.)*

**2. Class inheritance is NOT supported on the JS target — the sketch's shape is not
expressible.** `EmitJsTypes.fs:402-404` hard-fails any `inherit` clause:
*"class '%s' declares an `inherit` clause; class inheritance is not yet supported on the JS
target"*. The sketched shim is an **abstract base** whose `_writeCore` throws and is
overridden by a stdout writer / string writer — i.e. exactly an `inherit` hierarchy. It
cannot be authored that way.

The expressible shape is the one the repo already uses everywhere else: **a capability-style
`interface` plus concrete implementors** (interfaces *are* supported and are how `disposable`
/ `enumerable` work). The CLR face stays the abstract BCL class; the JS face is an interface.
Nothing in the design requires the two faces to share a *kind* — the provider binds the sink
per target, and (per question 1) the printf lowering only ever calls `Write(string)` on the
receiver. `ScratchSink`'s `new StringWriter()` becomes a concrete JS class implementing that
interface, with the parameterless `ToString` the `%a` capture-first residue block already
demands (`InferApp.fs:493-499`).

This is cheap precisely *because* the surface is one method. An abstract-base-with-`_writeCore`
hierarchy would have needed JS class inheritance; a one-method interface needs nothing that
does not already work.

**3. Disposal: plug into the EXISTING protocol; do not declare `Dispose()`.** The sketch
declares both `Dispose()` and `[Symbol.dispose]()`. That would invent a parallel convention,
and it is the wrong one. The protocol:
- `use w = …` lowers through `Disposal` (`Tast.fs:194-203`). `Disposal.ViaCapability` emits
  `w[Symbol.dispose]()` — and JS **names its own slot**, ignoring the CLR `slot` key
  (`EmitJs.fs:790-793`, `EmitJsCapabilities.fs:110-111`).
- On the impl side, a type that declares `interface disposable` (`Vesper.Core/capabilities.fsi`)
  has its `Dispose` emitted **as** the `[Symbol.dispose]()` method — the `Disposers` partition
  (`EmitJsMembers.fs:107-109`, `EmitJsTypes.fs:95`).
- A **bare** `Dispose()` member with no `interface disposable` is `Disposal.ViaOwnMember` (the
  ref-struct / external-type carve-out) and lowers to the free receiver-first
  `TextWriter__Dispose(w)` — a different call shape entirely.

So the `.fsi` must say **`interface disposable`**, and the `[Symbol.dispose]()` method comes
out for free. `disposable`'s CLR face is `System.IDisposable`, so the same declaration disposes
correctly on both targets.

**Who authors it: Vesper F#, not raw `.js`.** The repo's two precedents both compile Vesper F#
to a committed `.mjs` — `Vesper.List/list.js.fs` (`impl-js` → `Vesper.List.mjs`) and
`Vesper.Printf/structural-printer.js.fs` (→ the committed `Vesper.Printf.mjs` runtime asset,
regenerated by the codegen-js suite). A hand-written `runtime/TextWriter.js` would be a second,
divergent authoring pattern.

**Host IO is not an obstacle, and therefore not an argument for raw `.js`.** The JS backend has
a `$N`-template inline intrinsic (`EmitJs.fs:520-521` → `EmitJsFormat.expandTemplate`), used in
production today as `(# "$0[$1]" target name : ^TResult #)`
(`Vesper.Core/ops-dynamic.js.fs:31`). `(# "process.stdout.write($0)" s : unit #)` is expressible
in Vesper F#.

The remaining argument is single-sourcing: a hand-written `.js` sink would sit outside every
differential test that keeps the two targets' rendering in agreement, and would invite exactly
the third copy of the float rule that question 1 rules out. Authored as F# it compiles through
the same backend, under the same tests, beside `structural-printer.js.fs`.

**Recommendation: `src/Vesper.Printf/textwriter.js.fs`**, wired as `impl-js` and compiled to a
committed `.mjs` like `Vesper.List` — *not* a hand-written `runtime/TextWriter.js`. Its contract
face declares an **interface** (not an abstract class), **one non-overloaded `Write : string ->
unit`** plus `Flush`, and **`interface disposable`**; the concrete implementors are a
stdout/stderr writer (bottoming out in the `$N` template intrinsic) and the `StringWriter`
scratch (a buffer plus `ToString`).

### What remains BLOCKED until the shim exists

- `fprintf` / `bprintf` on JS **diagnose** — the intended behaviour (the sink is an observable
  argument). Unblocked by the shim.
- `printf "%a"` / `"%t"` on JS **diagnose** (`PrintfSpecifierTests.fs:81-88`, unchanged — the
  callback receives the state). Unblocked by the shim.
- `printf` / `printfn` / `eprintf` / `eprintfn` **must keep working** — they are unobservable,
  so the shim is *not* on their critical path. The `ToStdOut false` / `ToStdErr false` arms
  still have to be emitted (see "Two capability axes"); that is independent of the shim and of
  everything else here.

**Not blocked on `js-overload-mangling-plan.md`** — established above: the lowering calls only
`Write(string)`. That plan remains a prerequisite for two *other* things, both listed under
Deferred seams: a BCL-shaped `TextWriter` that user code can call as `w.Write 42`, and the typed
`IFormatSink.Child` atoms (its own stated first consumer). Neither is on this path.

### Honest costs

- **The rule is enforced at the GATE, and the format-ANNOTATION route bypasses the gate.**
  This is the one place the design does not reach. `InferLiterals.fs:269-277` (E1(a)) types a
  written format annotation — `let fmt : Printf.TextWriterFormat<int> = "%a…"` — by taking
  `'State`/`'Residue`/`'Result` from **the annotation's own resolved type args**
  (`args.[1..3]`) and feeding them to `PrintfSpec.printerFromSlots`. `PrintfSpec`'s family
  table is never consulted, so `observesSink` never runs. On a BCL-free stack that
  annotation's `TextWriter` resolved through `Translate.fs:571` to a **free typar**, and the
  `%a` callback's state parameter silently unifies with it. That is the *general*
  unresolved-dotted-name hole (premise 2), not a printf-specific one — printf merely stops
  *depending* on it, because the gate no longer needs the user's annotation to agree with
  anything. The design must not be read as closing it. Closing it means making an
  unresolvable dotted type name an error rather than a free typar, which is a much larger
  change with its own justification (`Translate.fs:533-538` argues the free typar is
  deliberate: "the provider is a partial view").
- **`ScratchSink` does not come out of the contract.** `System.IO.StringWriter` — the
  concrete `TextWriter` a capture-first `%a` `new`s up, because the abstract `TextWriter`
  `State` cannot be constructed (`PrintfSpec.fs:245-253`) — is a *lowering choice*, and no
  signature can express it. It survives as the **last hardcoded BCL name in the front end**,
  resolved through the provider like the rest, and it is the one thing the design does not
  delete. It is demanded only on a callback call, which is observable by definition, so it
  never fires on a target that lacks the sink. (`RuntimeNames.textWriterTypeName` /
  `stringBuilderTypeName` do go; the CLR backend's own `ClrEnv.textWriter`, `ClrEnv.fs:38`,
  stays and *should* — it is a backend fact.)
- **A BCL type lands in a contract a BCL-free target loads.** Declaring `fprintf` puts
  `System.IO.TextWriter` in `printf.fsi`, which the JS stack parses. `formatter.fsi`
  already does this and extracts cleanly to `FTUnknown` — but the manifest has **no way
  to exclude a base `.fsi` from a target**: `files-<t>` only *appends*
  (`ReferencedProject.fs:62-66`, `:147-149`), and the CLR compose passes `None`
  (`SymbolProviders.fs:34`), so there is no `clr` suffix to declare into. JS therefore
  carries a declaration whose sink is `FTUnknown`. That is the honest model — *the
  function exists, its sink does not* — and it is what produces the diagnostic. But this
  design does not add a per-target-contract mechanism, and should not be read as
  assuming one.
- **Two capability axes — and the second one becomes IN SCOPE for whoever builds this.**
  The provider says *this type exists on this target*; the backend says *I can write to this
  sink*. This design supplies the first, which is what `fprintf`/`bprintf` need. The
  `ToStdOut false` / `ToStdErr false` JS crash (`EmitJs.fs:527-531`) is the *second* axis: a
  sink JS can perfectly well express (`process.stdout.write`), merely unimplemented. Under
  the observability rule those calls are **unobservable and must therefore keep working** —
  so `printf`/`eprintf` on JS cannot be left to crash, and emitting the no-newline console
  forms is **part of this work**, not a separate ticket. (The alternative — diagnosing them —
  is ruled out by the rule itself.) Naming the axis is the point; conflating it with the
  provider axis would be the mistake.

### Test-stack consequence

Sink-bearing printf tests cannot mean anything on the BCL-less SemanticAnalysis stack —
they only pass today because of the free-typar laundering above, which is to say they were
never testing what they appear to test. They **move to the Codegen suites**, where a target
and its provider exist. Of the 54 tests in
`test/XParsec.FSharp.SemanticAnalysis.Tests/PrintfTests.fs`, the ones that move are the
**four** sink-bearing lowering tests — `:358` ("fprintf takes the writer first"), `:369`,
`:391`, `:401` — each of which passes a `TextWriter` / `StringBuilder` as a real argument
and is therefore *observable*, hence undiagnosable on a stack with no BCL.

Everything else stays, and now for a stated reason rather than by luck: **every remaining
test is on an unobservable call.** The format-grammar / specifier-parser tests, the
`argTypes` / `totalArity` / `printerType` unit tests (which pass `state`/`residue`
explicitly and never consult a family), the arg-type-mismatch diagnostics, `sprintf`
lowering, the residual diagnostics, the shadowing test — all `printf`/`printfn`/`sprintf`
with no callback hole. The `%a`-on-`printf` type-error test (`:423`, `printfn "%a" 42`) is
the interesting one: it *is* observable, so under the rule it would now raise the sink
diagnostic on a BCL-less stack **in addition to** the callback-arity error it asserts. It
asserts `Diagnostics` is non-empty, so it still passes — but for a partly different reason,
and it should be re-pointed at the CLR suite (where the sink resolves) to keep meaning what
it says. So: **four move, one is re-sited, ~49 stay.**

`test/XParsec.FSharp.Codegen.Js.Tests/PrintfSpecifierTests.fs:81-88` — writer-family `%a`
diagnoses on JS — **survives unchanged**, and that is the rule's best evidence: it is now a
consequence of `observesSink` (a callback receives the state ⇒ the sink must resolve ⇒ the
JS provider has no `TextWriter`) rather than of a `%a`-specific gate. Its CLR counterparts
already exist (`Codegen.Clr.Tests/PrintfHappyPathTests.fs:726-760`,
`FSharpCoreDepsTests.fs:24-40`), which is itself the evidence that the four movers were
target tests all along.

**No fake BCL is required anywhere in this design.** That is not incidental: a design that
needed `MockBuiltins` back would be a design that had gone wrong, and this one is falsified
the moment it reaches for one.

## Deferred seams (designed for, not built)

- **The sink model** — contract-declared, provider-resolved, `SymbolKey`-recognised, and
  resolved on demand by observability; see the section above.
- **A BCL-shaped JS `TextWriter`** — one whose surface matches `System.IO.TextWriter`'s
  (`Write` over string/char/int/float/obj), so user code calling `w.Write 42` is portable.
  Hard-blocked on [js-overload-mangling-plan](js-overload-mangling-plan.md); *not* needed by
  printf, which only ever calls `Write(string)` (see the sink-model section).
- **Typed `Child` atoms** for JS fidelity — overloads for the IntrinsicRepr-encodable
  primitives (bool, char, string, sbyte…uint64, float32, float, decimal). Enumerate
  from `IntrinsicRepr`, *not* `DocLayout.formatPrimitive` (they disagree in ways that
  matter for a set you cannot cheaply extend once frozen). They buy nothing load-bearing on
  the CLR (the box round-trips the runtime type), so they wait for the per-type JS `Format`
  emitter. **Hard-blocked on [js-overload-mangling-plan](js-overload-mangling-plan.md)**,
  whose stated first consumer they are. (That plan supersedes the earlier "use suffixed
  names (`ChildInt32`), not overloads" note here: real overloads are kept, and the *emitter*
  derives `Child$int` / `Child$string` / `Child$obj` from the frozen signature — the naming
  is a backend concern, not a contract deformation.)
- **Per-type JS `Format` emitter** — the consumer the typed atoms serve.
- **Zero-alloc partial application** — [printf-partial-app-plan](printf-partial-app-plan.md).
- **Upstream RFC questions** (a `Case(name)` nullary shortcut, a `kind` enum arg on
  `BeginRecord` for anonymous records, `%+A`) — only if the FSharp.Core pitch
  proceeds. On a no-default-interface-method frozen surface, prefer an extensible
  enum arg over a new method.
