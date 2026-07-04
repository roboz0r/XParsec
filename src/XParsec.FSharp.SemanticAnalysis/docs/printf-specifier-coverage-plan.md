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
- **A1b — zero-pad on unsigned / octal. NEXT (real formatting; touches the runtime handler).**
  Oracle (pinned in `tmp/printf_a1.fsx`): `%05u` 42 → `00042`, `%05u` -1 → `4294967295` (10 digits,
  overflows width ⇒ no pad); `%08o` 8 → `00000010`, `%08o` -1 → `37777777777`. **This corrects an
  in-code invariant:** `FieldFormat.IntRadix`'s doc says "`%o` never zero-pads — always None" and
  the classifier defers `%08o`, but F# **does** zero-pad octal. Work items:
    1. **Front end:** in `tryClassify`, admit `zeroPad` for `UnsignedDecimalInt` (`%u`) and
       `UnsignedOctal` (`%o`) — today `:219`/`:220-224` return `ValueNone`. Carry the zero-pad
       width: extend `FieldFormat.Unsigned` to `Unsigned of zeroPad: int option` and
       `IntRadix(radix, zeroPad)` already has the slot — just stop forcing `None` for octal.
    2. **CLR projection:** `Codegen.Clr/ClrHoleFormat.toDotNetFormat` — map the zero-pad width onto
       the handler args (mirror how `%05x`/`%05d` already project: hex uses `IntRadix` zeroPad, dec
       uses `DecimalZeroPad`/`HoleKind.ZeroPaddedFloat`-style width-in-alignment-slot).
    3. **Runtime handler (`Vesper.Printf/formatter.fs`, Vesper-compiled):** `AppendUnsigned` /
       `AppendOctal` (`HoleKind.Unsigned` / `HoleKind.Octal`) need a zero-pad width parameter, or a
       dedicated `AppendZeroPadded*` member. **Model it on the existing `AppendZeroPaddedFloat`**
       (the `%0w.pf` handler) — same "zero-pad *after any sign* to a total field of `width`"
       shape. Note the overflow case (`%05u` -1 ⇒ 10 digits, no truncation, no pad).
    4. Tests: `runParity` for each oracle form; no cold assertion currently pins `%05u`/`%08o`
       specifically (they fell under the generic defer), so add fresh `Format`-shape + parity tests.
- **A2 — byte-exact sign/zero-pad float forms (deferred; hardest parity).** Each its own
  `FieldFormat` + section-format construction in `ClrHoleFormat`, oracle-verified: forced-sign on
  non-`%d`/`%f` (`%+g`/`% g`/`%+e`, `:171-177`), sign+zero-pad (`%+05d`, `:163-164`), `%08e`/`%010g`
  (`:209-218`), `%.2M` (`:226-232`), **and the float left+zero case A1a deferred** (`%-05.2f`
  3.14159 → `3.140` — F# zero-pads floats on the *right* under left-align; `tryClassify` keeps
  `leftAlign && zeroPad && isFloatLike → ValueNone`). The pinned cold tests
  (`PrintfHappyPathTests` `%010g`/`%+g`/`% g`/`%+05d`/`%+e`/`%08e`/`%.2M`, and the `%-05.2f` guard
  A1a added) flip as each lands.

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

## Ordering summary & status (updated 2026-07-04)

**Landed:** A0 (`% A`, `ae6e424e`) → B1 (`fprintf`/`fprintfn`, `0c11484b`) + writer-slot `TyClass`
cleanup (`9498ad84`) → A1a (inert-flag / left-wins forms, `16b69e2f`). All on branch
`semantic-analysis`; suites green at each (CLR 1137 / JS 252 / semantic 700).

**RESUME HERE → A1b** (zero-pad on `%u`/`%o`) — fully spec'd in Track A above; oracle bytes in
`tmp/printf_a1.fsx`. First A-form that touches the runtime handler (`Vesper.Printf/formatter.fs`),
so build+run the FULL CLR suite. The maintainer chose to keep A1's *specifier-forms* order rather
than jump to Track C.

**Then:** A2 (byte-exact float forms) → C1/C2 (`%A` gate relax; big coverage jump) → B2 (`bprintf`)
→ D (`%a`/`%t`, design pass first — sink ABI) → E1 (const-literal format) → E2 (runtime runner;
heaviest) → capstone. F (`%*d`) is a separable feature.

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
