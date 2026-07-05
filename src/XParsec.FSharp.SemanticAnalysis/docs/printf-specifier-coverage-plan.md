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

Make `PrintfHoleForm.tryClassify` (`PrintfHoleForm.fs`) **total over every lowerable
flag/width/precision form**, and extend its CLR projection `ClrHoleFormat.toDotNetFormat`
correspondingly. Every `ValueNone` arm currently fails `lowerablePlaceholders`
(`InferLiterals.fs`), sending the *whole format* cold.

**Method (use it for every remaining A-form): oracle-first.** Real F# is the parity spec, and it
also decides which flag/width/type *combinations the F# compiler even accepts* (many are FS0741
compile errors, not lowerable forms). Before coding a batch, confirm the exact bytes with
`dotnet fsi` — the working oracle script is **`tmp/printf_a1.fsx`** (extend it, re-run
`dotnet fsi tmp/printf_a1.fsx`). This already corrected one wrong in-code invariant (below) and
found several F#-rejected forms. Then flip each `PrintfHappyPathTests` cold assertion to a
`Format` node + a `runParity` check against the oracle bytes.

- **A0 — `% A` space flag. LANDED (`ae6e424e`).** Admitted the space flag in the `%A` arm (a
  no-op — `GenericToString` never consults it). Repointed the cold-recipe guard tests to `%08e`.
- **A1a — inert-flag & left-wins forms. LANDED (`16b69e2f`).** Two classifier-only classes (map
  to already-working shapes, no runtime change): (i) a width-less `-`/`0` flag is inert
  (`%-d ≡ %d`, `%-.2f ≡ %.2f`) — implemented by normalising `leftAlign`/`zeroPad` to `&& hasWidth`
  so the flag disappears (and every `width.Value` read is then safe); (ii) left-align wins over
  zero-pad for **non-float** types (`%-05d ≡ %-5d`, spaces on the right). The `%A` arm reads the
  **raw** `0` flag (`%0A`/`%05A` force flat regardless of width). F#-**rejected** (not targets):
  `%05s`/`%05b`/`%05c`/`%0s` (FS0741, `0` unsupported on non-numeric).
- **A1b — zero-pad on unsigned / octal. LANDED (`dea85b42`).** `%05u`/`%08o` (and their
  overflow forms) now lower to native `Format` nodes. Oracle (pinned in `tmp/printf_a1.fsx`):
  `%05u` 42 → `00042`, `%05u` -1 → `4294967295` (10 digits, overflows width ⇒ no pad); `%08o` 8 →
  `00000010`, `%08o` -1 → `37777777777`. **Corrected an in-code invariant:** `FieldFormat.IntRadix`'s
  doc claimed "`%o` never zero-pads — always None", but F# **does** zero-pad octal. What landed:
  `FieldFormat.Unsigned` grew a `zeroPad: int option` slot and octal now emits
  `IntRadix(Radix.Octal, Some w)`; two new `HoleKind`s (`UnsignedZeroPad`/`OctalZeroPad`) carry the
  width in the alignment slot as a `Const` (zero-pad and space-pad share no operand slot); two new
  Vesper handler members (`AppendZeroPaddedUnsigned`/`AppendZeroPaddedOctal`) reuse a shared
  `ZeroPadAfterSign` extracted from `AppendZeroPaddedFloat` (overflow is a no-op — no truncation).
  JS backend kept in sync (`%05u` via `padStart`) with a `runsLines` parity test covering both
  overflow cases. `%-05u`/`%-08o` were already covered by the A1a left-align-wins normalisation
  (drops `zeroPad`, routes through the space-pad path).
- **A2 — byte-exact sign / zero-pad float (+ adjacent) forms. LANDED (`ab78a321`).** All five forms
  now lower natively; Track F's dynamic-precision handlers + A1b's `ZeroPadAfterSign` meant only one
  new `Formatter` member was needed. Oracle bytes pinned in `tmp/printf_a2.fsx`. What landed:
    - **`%.2M`** — F# silently ignores precision on `%M`, so a literal/absent precision is inert and
      lowers as plain `%M` (`Verbatim`); `%.*M` (star) stays cold. Classifier-only.
    - **`%+05d` / `% 05d`** — zero-pad *through* the sign via a wider section format `"+0000;-0000"`
      (digit count `w-1`); `FieldFormat.ForcedSign` gained a `zeroPad` field. Projection-only.
    - **`%+e` / `% e` / `%+g` / `%+G`** — scientific/compact can't ride a section format, so routed to
      `AppendDynamicPrecisionSignedFloat` with a **constant** precision (`EmitFormat.dynamicFloat` now
      takes a push-precision thunk; `constSignedExpCompact` detects the e/E/g/G letters).
    - **`%08e` / `%014e` / `%010g`** — reuse `AppendZeroPaddedFloat` over the `"e6"`/`"g6"` body via a
      new `FieldFormat.ExpCompactZeroPad` → `HoleKind.ZeroPaddedFloat`.
    - **`%-05.2f`** — the one new handler: `AppendRightZeroPaddedFloat` (F# left-align + zero-pad fills
      the *right* with zeros; overflow no-op). New `FieldFormat.FixedRightZeroPad` /
      `HoleKind.RightZeroPaddedFloat`.
  Cold-recipe guard tests repointed `%08e` → `%+08.2f` (sign+zero-pad float has no faithful section
  format, stays cold). JS backend in sync: byte-parity for the integer/fixed forms, accepted
  `toExponential`/`toPrecision` divergence pinned for scientific/compact.

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

**DONE** (lex `FormatDim` → typing `argTypes` seam → native width-star → native
precision-star; design and verified semantics in `printf-architecture.md` § star-width).
Remaining star **cold residuals**, pinned in `FSharpCoreDepsTests`: zero-pad star
(`%0*d`, `%0*.Nf`), flagged star-`%A` (`%-*A`/`%+*A`/`%0*A`). The capstone (deleting the
cold recipes) must lower or re-error these first. `%*%`/`%5%` are *rejected* (accepted
deviation — F# consumes the width and prints a bare `%`).

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

## Ordering summary & status (updated 2026-07-05)

**Landed:** A0 (`% A`, `ae6e424e`) → B1 (`fprintf`/`fprintfn`, `0c11484b`) + writer-slot `TyClass`
cleanup (`9498ad84`) → A1a (inert-flag / left-wins forms, `16b69e2f`) → **Track F** (star-width
`%*d` / precision `%.*f` / `%*A`, `927aaabe`…`416495b2` — a separable feature, landed on top of the
A1a/A1b-docs commits) → A1b (zero-pad `%u`/`%o`, `dea85b42`) → A2 (float sign/zero-pad forms,
`ab78a321`). All on branch `semantic-analysis`. **A2 verified 2026-07-05:** clean build, all suites
green — **CLR 1215** (1191 + 24 new) **/ JS 263 (259 + 4 new) / semantic 710** (+1 skipped). With A2
in, **Track A is complete** — `tryClassify` is total over every lowerable flag/width/precision form.
Track F's cold star **residuals** (`%0*d`, `%0*.Nf`, `%-*A`/`%+*A`/`%0*A`) are pinned in
`FSharpCoreDepsTests.fs` (`test/XParsec.FSharp.Codegen.Clr.Tests/`) and remain a capstone precondition
(see § Track F). The `%+08.2f` cold pin (sign + zero-pad float, no faithful section format) now guards
the cold recipes in `FSharpCoreDepsTests`/`SelfHostTests`.

**RESUME HERE → C1/C2** (`%A` gate relax — the big coverage jump; the runtime `%A` dispatcher is
already total, so this is mostly relaxing `structuredArgFaithful` in `FreezeExpr.fs`, not writing a
renderer — see § Track C and § The key lever). C2 (polymorphic `%A`) is the one item needing a codegen
check (generic method-typar hole).

**Then:** B2 (`bprintf`) → D (`%a`/`%t`, design pass first — sink ABI, largely pre-resolved: split by
`Family.State`; writer families get a scratch/underlying `TextWriter`, `sprintf` splices the residue
string; `Formatter`-as-`'State` via `allows ref struct` is the eventual zero-copy ABI break) → E1
(const-literal format) → E2 (runtime runner; heaviest) → capstone. F (`%*d`) is a separable feature.

**Working method (established this sprint):** oracle-first — confirm exact bytes and F#-acceptance
with `dotnet fsi tmp/printf_a1.fsx` before coding a batch; one subagent per step implementing +
flipping/adding tests (NOT committing); maintainer reviews the diff, runs the full suites, and
commits with a short message. Surface design questions (next real one: Track D's sink ABI) before
coding them.

## Testing

- **Flip, don't delete, the cold-pin tests** — each `PrintfHappyPathTests` `failtest`-on-`Format`
  case becomes an assert-`Format` case as its form is closed; `FSharpCoreDepsTests` cold pins
  become `isEmpty`.
- **Byte parity** via `runParity` against the test process's own `sprintf` for every Track-A/B
  form (except `%A` of FSharp.Core types, where ToString divergence is expected — assert the
  native-ness / no-dependency, not F# parity).
- **`%a`/`%t` and format-as-value** get new runnable tests once their machinery lands.
