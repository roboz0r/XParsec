# Printf native-coverage sprint — close every FSharp.Core cold-printf degradation

**Goal.** Make *no lowerable printf-family call* fall back to FSharp.Core's cold printf
(`PrintfModule.PrintFormatLine` / `PrintfFormat` ctor / `FSharpFunc::Invoke`), so the compiler
can **drop its FSharp.Core dependency**. Removing `FSharp.Core.dll` is the capstone, not a
free lunch — it is gated on the tracks below plus one adjacent axis (§ Capstone).

This is a self-contained sprint, split out of `printf-partial-app-plan.md` (whose "4b — breadth"
bullet now points here). That plan keeps the *partial-application representation* work (4a landed;
4c the zero-alloc value struct; step 5 the `n > K` residual). Deleted once this sprint lands
(ephemeral — see `feedback_plan_docs_ephemeral`).

## Decisions locked (with the maintainer, 2026-07-04)

- **Close the whole hard tail** (`%a`/`%t`, arbitrary-type `%A`, format-as-value) — over the
  sprint, not necessarily first. No lowerable shape is left permanently cold.
- **FSharp.Core-owned types are excluded from `%A` *fidelity*, not from native lowering.** A
  `%A` of an `FSharpOption` / any type the Vesper engine can't render structurally **degrades
  down the `%A` hierarchy to `.ToString()`** — on the *native engine*, never the FSharp.Core cold
  path. Output may diverge from F#'s reflective `%A` for those types; acceptable for the
  dependency-drop goal. This is *already how the runtime dispatcher behaves* (see § The key lever).
- **Definition of done:** every degradation in the inventory below is closed or (for FSharp.Core
  types) consciously ToString-degraded; the `FSharpCoreDeps` tests flip to `isEmpty`; the three
  cold recipes and the `FSharp.Core` reference are removed.

## The pipeline, and where FSharp.Core is actually pinned

`tryInferPrintfApp` (`InferApp.fs`) sets a `PrintfApp` (happy path) or `PrintfPartial` (4a) marker
→ `FreezeExpr` projects a `TExpr.Format` (or, unmarked/declined, an ordinary `App(printfn, New
PrintfFormat …)`) → codegen. The **three recipes that emit the dependency** live in
`Codegen.Clr/ClrRecipes.fs`: `emitPrintfn` → `PrintfModule.PrintFormatLine`; `emitPrintfFormatCtor`
→ `PrintfFormat`4 .ctor`; `emitFSharpFuncInvoke` → `FSharpFunc`2.Invoke`. Every item below is an
*upstream condition* that routes a call to one of these. Close the upstream condition ⇒ the recipe
is never reached; when *no* condition can reach them, the recipes (and the ref) are deleted.

## The key lever: the runtime `%A` dispatcher is already total

`Vesper.Printf/structural-printer.fs` `RuntimeFormatState.DispatchInner` (~`:518`) resolves, in
order: `Vesper.IStructuralFormattable` → `string`/`char`/`bool` → `ITuple` → `IFormattable`
(→ `DocLayout.formatPrimitive`) → `IEnumerable` (→ structural sequence) → **`_ -> value.ToString()`**.
It is **already total** and reflection-free: an `FSharpList` renders structurally via the
`IEnumerable` arm; an `FSharpOption` (or any unknown) falls to `ToString()`. So the *only* thing
forcing `%A` cold is the **compile-time decline** in `FreezeExpr.structuredArgFaithful` (`:1149`),
which conservatively refuses the engine for non-faithful types. Relaxing that gate — not writing
any new renderer — is most of Track C, and it is why the "close all `%A`" decision is cheap.

---

## Work tracks (ordered easy → hard; breadth accrues fastest first)

Each closed item flips a specific test from asserting the cold shape (`TExpr.App` /
`FSharpCoreDependencies` non-empty) to the native shape (`TExpr.Format` / `isEmpty`). Anchors
below are verified against the current source; line numbers drift — grep the named symbol.

### Track A — specifier-form totality (the "peel each kind" core)

Make `PrintfHoleForm.tryClassify` (`PrintfHoleForm.fs:111-236`) **total over every lowerable
flag/width/precision form**, and extend its CLR projection `ClrHoleFormat.toDotNetFormat`
correspondingly. Every `ValueNone` arm below currently fails `lowerablePlaceholders`
(`InferLiterals.fs:261`), sending the *whole format* cold. Peel them in this order:

- **A0 — `% A` space flag** (`PrintfHoleForm.fs:143-144`). `%A` with the space flag returns
  `ValueNone`; `%+A`/`%-A`/`%0A`/`%NA` already classify. The space is a no-op for the structural
  engine — admit it (drop the `if spaceSign then ValueNone` guard in the `Structured` arm).
  *The single narrowest pure-cold pin.* Flips `FSharpCoreDepsTests` `% A` → `isEmpty` and
  `PrintfHappyPathTests` `% A` (`:470`) → `Format`.
- **A1 — no-op flag/zero-pad forms.** Forms where the flag is meaningless or trivially
  expressible, currently deferred out of caution: `%-d`/`%-05d` (left-align, `:178-183`), `%05u`
  (`:219`), `%08o` (`:220-224`), `%05b` (`:225`), `%05s`/`%05c`/`%05O` (`:190-192`). For each,
  decide the faithful mapping (most are "width ignored" or "pad the stringified value") and emit a
  `FieldFormat` + alignment, extending `ClrHoleFormat.toDotNetFormat` to project it. One
  `PrintfHappyPathTests` case per form flips App→Format.
- **A2 — byte-exact sign/zero-pad float forms.** The genuinely fiddly parity cases, each its own
  `FieldFormat` extension + section-format construction in `ClrHoleFormat`: forced-sign on
  non-`%d`/`%f` (`%+g`/`% g`/`%+e`, `:171-177` `_ -> ValueNone`), sign+zero-pad (`%+05d`,
  `:163-164`), `%08e`/`%010g` (exponential/compact zero-pad, `:209-218`), `%.2M` (decimal
  precision, `:226-232`). Drive each against the test process's own `sprintf` for byte parity
  (`PrintfHappyPathTests.runParity`); the pinned cold tests (`:331,338,345,352,359,366,745`) flip.

### Track B — sink breadth (`fprintf`/`fprintfn`, then `bprintf`)

**Cross-target decision (maintainer, 2026-07-04): the explicit-writer families keep their BCL
surface types on CLR; JS support is deferred until demanded.** The abstract sinks
(`printf`/`eprintf`/`sprintf` → `ToStdOut`/`ToStdErr`/`ToString`) are already platform-agnostic;
only `fprintf`/`bprintf` name a BCL type at the surface, and those are inherently .NET-interop
features. So `fprintf : System.IO.TextWriter -> …` stays TextWriter on CLR (F# source-compat,
zero-overhead — the `Formatter` already flushes to a `TextWriter`); **no** platform-agnostic
Vesper sink type is introduced speculatively. A JS `fprintf`-to-a-writer, when a real use case
demands it, gets a minimal `ITextSink` (`Write(ReadOnlySpan<char>)`) implemented natively on JS —
introduced *then*, not now (routing CLR `fprintf` through a Vesper sink now would cost a
`TextWriter → sink` adapter alloc for an unused JS capability).

- **B1 — `fprintf`/`fprintfn`. LANDED (2026-07-04).** Writer `PrintfSink` + `sinkOf`, `newline`
  field on `FormatSinkG.ToWriter`, gate marks `idx = 1` writer sinks, Freeze threads the writer
  expr + trailing newline into a `ToWriter` `Format` node. (Fully-applied only; `fprintf`
  *partials* stay cold, 4a gate is `idx = 0`.)
  - **Follow-up cleanup (do next): kill the `TyConst(TextWriter)` writer-slot hack.** The family's
    writer slot is a by-name `TyConst("System.IO.TextWriter")` (`PrintfSpec.fs`), a stale hack
    from before the provider returned a `TyClass`. A real writer (`System.Console.Out`) resolves
    to `TyClass(TextWriter)`, and core `unify` compares `TyClass` by key equality
    (`Engine.fs:261`), so the two didn't reconcile — B1 papered over it with a `subsumes`
    reconciliation in the gate. The proper fix: resolve the writer slot to `TyClass(TextWriter)`
    through the *same* provider (`ctx.Provider.TryLookupType "System.IO.TextWriter"` →
    `externalTypeKey`, the path `Console.Out`'s type took, `Scope.fs:57`) so keys match and plain
    `unify` works; then delete the gate's leading-arg `subsumes` special-case.
- **B2 — `bprintf`.** Not in `PrintfSpec.families` at all → doesn't even type via the printf
  rule. Add a `Family` entry (`idx = 1`, StringBuilder leading arg) + a `FormatSink.ToBuilder`
  emission (`EmitFormat.fs:57` currently `failwith`s on `ToBuilder`) driving the handler's
  `StringBuilder` sink. Larger than B1 — new sink end-to-end. **JS note:** when a JS `bprintf`
  lands, the `StringBuilder` shim can likely be a thin wrapper over a single growable string
  field (JS engines optimise string concatenation via ropes), rather than CLR's chunked buffer.

### Track C — `%A` breadth (relax the gate; the runtime is already total)

- **C1 — admit FSharp.Core-owned + arbitrary types (ToString-degrade).** Relax
  `structuredArgFaithful` (`FreezeExpr.fs:1149`) so a `%A` hole no longer declines to cold for
  types the engine renders via `IEnumerable`/`ToString`. Per the locked decision, FSharp.Core
  types (`FSharpOption`, …) lower to the engine and render via its `ToString` fallback rather than
  cold. Net effect: the gate trends toward `true` for every concrete nominal; keep declining only
  where a *runtime* value could still reach FSharp.Core cold (there is none once the engine path
  is chosen). Verify the emitted `AppendStructured<T>` handles each admitted `T`.
- **C2 — polymorphic `%A`.** `let f x = printfn "%A" x` (arg is a typar) hits
  `structuredArgFaithful`'s `_ -> false` (`:1227`) → cold. The runtime dispatcher recovers the
  boxed runtime type, so the engine path is valid; admit a typar/`TyVar` hole and confirm codegen
  emits `AppendStructured<!!i>` (generic method-typar) for a method-generic hole. This is the one
  Track-C item that needs a codegen check, not just a gate relaxation.
- **Non-goal:** matching F#'s reflective `%A` *output* for FSharp.Core / BCL types — ToString
  divergence is accepted.

### Track D — `%a` / `%t` callback holes

`argType` returns `ValueNone` for `FormatFunction`/`Text` (`PrintfSpec.fs:145-146`), so any format
containing them defers (and raises a diagnostic). Native support needs, end to end:
- A **typing** rule: `%a` consumes a printer `(State -> 'T -> unit)` **and** a value `'T`; `%t`
  consumes `(State -> unit)`. `State` is the Vesper sink.
- A **Format segment / `HoleForm`** kind for a callback hole that, at emit, invokes the user
  callback against the live `Formatter`.
- **ABI design (needs its own pass):** the Vesper `Formatter` is a `[<Struct; IsByRefLike>]`
  handler; a user callback taking it as `State` cannot flow through an ordinary boxed closure.
  Decide the callback's sink type (the byref-struct handler by `inref`/`byref`, or a thin sink
  interface the handler implements). This is the hardest *design* item; flag before coding.

### Track E — format-as-value / non-literal format

`formatSpecifiers` returns `ValueNone` for a non-`Expr.String` format (`InferLiterals.fs:249`), so
a `PrintfFormat`-typed value used at the call site defers. Two sub-cases:
- **E1 — compile-time-known literal bound to a `let`** (`let fmt = ... in printf fmt` where `fmt`
  is a literal): constant-propagate the format to the call site and lower normally. Medium.
- **E2 — genuinely dynamic format** (computed at runtime): needs the **runtime spec-runner** this
  project deferred (`printf-architecture.md`) — a `static readonly` parsed-spec + a runtime loop
  over the handler. The heaviest item; likely its own sub-sprint. Until it lands, E2 is the last
  shape that can force cold, so the capstone's ordering must account for it.

### Track F — star-width `%*d`

A **lexer gap**, not a cold degrade: `Lexing.lFormatPlaceholder` rejects `*` (flags are
`anyOf "0+- "`, width is `opt pbigint`), yielding `InvalidFormatPlaceholder` → a compile error.
Teach the `*` width/precision grammar first (a `FormatPlaceholder` that marks width/precision as
star-consumed), then route it through the **per-hole arg-type seam** already designed for it
(`PrintfSpec.argType` becoming a hole→arg-types mapping; see `printf-architecture.md`). Independent
of the FSharp.Core drop (it's a *new feature*, currently an error, not a degrade) — sequence last
or defer past the sprint.

---

## Capstone — remove `FSharp.Core.dll`

Reachable only when Tracks A–E are closed (F is a separable feature). Then:
1. Delete the three cold recipes (`emitPrintfn`, `emitPrintfFormatCtor`, `emitFSharpFuncInvoke`)
   and their `ClrProvider` routing (`isColdPrintf`, `TryEmitFSharpFuncInvoke`); the gate/Freeze no
   longer produce an unmarked printf `App`, so nothing reaches them.
2. Flip every `FSharpCoreDepsTests` cold-pin assert (`:44` `% A`, …) to `isEmpty`, and the
   `PrintfHappyPathTests` `App`-asserting cases to `Format`.
3. Drop the `FSharp.Core` reference row / on-disk copy from the app-materialisation path where the
   printf family was its only consumer.

**Adjacent precondition (separate axis).** Even with all printf native, `%A` of a *list/`option`
literal* pins FSharp.Core via the **list/option type representation** (`FSharpList`/`FSharpOption`
construction — `[1;2;3]` builds `Cons`/`Empty`), independent of printf. `FSharpCoreDepsTests:112`
pins exactly this. A *full* `rm FSharp.Core.dll` also needs list/option to resolve to the Vesper
types (the self-host posture, `ctx.DefaultListIsVesper`) — track separately; this sprint's success
is measured by *printf* pinning nothing, not by the whole program being FSharp.Core-free.

## Ordering summary

A0 (`% A`, one-line win) → B1 (`fprintf`) → A1 (no-op forms) → C1/C2 (`%A` gate relax; big
coverage jump) → A2 (byte-exact float forms) → B2 (`bprintf`) → D (`%a`/`%t`, design pass first)
→ E1 (const-literal format) → E2 (runtime runner; heaviest) → capstone. F (`%*d`) is a feature,
sequenced independently.

## Testing

- **Flip, don't delete, the cold-pin tests** — each `PrintfHappyPathTests` `failtest`-on-`Format`
  case becomes an assert-`Format` case as its form is closed; `FSharpCoreDepsTests` cold pins
  become `isEmpty`.
- **Byte parity** via `runParity` against the test process's own `sprintf` for every Track-A/B
  form (except `%A` of FSharp.Core types, where ToString divergence is expected — assert the
  native-ness / no-dependency, not F# parity).
- **`%a`/`%t` and format-as-value** get new runnable tests once their machinery lands.
