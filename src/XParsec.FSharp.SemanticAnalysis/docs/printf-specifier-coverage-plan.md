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
- **B2 — `bprintf`. LANDED (2026-07-05).** `PrintfSink.Builder` + a `builderFamily` (`idx = 1`,
  `StringBuilder` leading arg), `sinkOf`/`families` entries; the gate admits the `Builder` sink at
  `idx = 1` (like `Writer`) and Freeze threads the builder expr into a `FormatSink.ToBuilder` node.
  The `Formatter` grew a third `StringBuilder` ctor + a `Builder` field, and `Flush` appends the
  buffered text to it (no `bprintfn`, so never a trailing newline). `substituteWriter` generalised
  to `resolveExternalSlots` (resolves both the `TextWriter` and `StringBuilder` slots to the
  provider `TyClass` so a leading sink arg unifies directly); `EmitFormat` emits `CtorBuilder` +
  `Flush`. Fully-applied only; `bprintf` *partials* stay cold (4a gate is `idx = 0`). CLR-only, like
  B1 (JS deferred). **JS note:** when a JS `bprintf` lands, the `StringBuilder` shim can likely be a
  thin wrapper over a single growable string field (JS engines optimise string concatenation via
  ropes), rather than CLR's chunked buffer.

### Track C — `%A` breadth (relax the gate; the runtime is already total)

- **C1 + C2 — LANDED (`134d942b`).** `structuredArgFaithful` (`FreezeExpr.fs`) relaxed: the
  external-nominal else branch collapsed to `TyUnion _ | TyRecord _ | TyClass _ -> true` (a `TyClass`
  arm added for BCL types), and `TyVar _ -> true` admits the polymorphic hole. Every concrete
  nominal — Vesper-compiled, `FSharpOption`, arbitrary BCL class — plus a typar now lowers to the
  engine and renders via the dispatcher's `IFormattable`/`IEnumerable`/`ToString` tail. Only shapes
  the CLR encoder can't author a type argument for (`TyUnknown`, `TyOr`/`TyKeyOf`/… type-level
  constructs a real `%A` never carries) stay cold. **C2 verified end-to-end:** `let f x = printfn
  "%A" x` — the hole's typar arrives as `FTTypar(Method, i)`, `encodeType` → `!!i`,
  `appendStructured` emits `AppendStructured<!!i>` cleanly (full IL codegen + node execution). No
  typar wall. The gate is shared, so JS benefits too (a polymorphic `%A` runs under node at
  int/string/list). **Non-goal (accepted):** matching F#'s reflective `%A` output for
  FSharp.Core/BCL types — ToString divergence is the locked decision; tests assert native-ness, not
  parity. The list/option-*literal* representation pin (`FSharpList` `Cons`/`Empty` from
  construction) is independent of printf and left in place (see § Capstone adjacency).

### Track D — `%a` / `%t` callback holes

`argType` returns `ValueNone` for `FormatFunction`/`Text` (`PrintfSpec.fs:182-183`), so any format
containing them defers (and raises a diagnostic). Two mechanical pieces still to build:
- A **typing** rule: `%a` consumes a printer `('State -> 'T -> 'Residue)` **and** a value `'T`; `%t`
  consumes `('State -> 'Residue)`. Both are slots the printf rule already carries: `'State =
  Family.State`, `'Residue = Family.Residue` — `unit` for the writer/builder families, `string` for
  `sprintf`. (The `'Residue` slot is load-bearing: the `sprintf` splice below reads a *real residue
  string*, so the rule is `'State -> 'T -> 'Residue`, not the `… -> unit` an earlier draft wrote.)
- A **Format segment / `HoleForm`** kind for a callback hole that, at emit, invokes the user
  callback and splices its output at the hole position.

**Where the typing rule actually lands (scouted 2026-07-05).** It is larger than "a rule" — it
widens the per-hole typing seam, but needs *no* new inference/unification machinery:
- **Shared typar by node-sharing.** `%a`'s two arg entries `['State -> 'T -> 'Residue; 'T]` must
  reference the *same* `'T`. Mint **one** `fresh ()` per `%a` hole and reuse the returned `TyVar`
  node in both entries; the App-loop unification (`InferApp.fs` ~`:321-343`) then pins both
  occurrences with nothing added — identical to how `%A` shares its single fresh today. The callback
  lambda's 2nd parameter unifies with `'T` through its printer-arrow domain; the value arg unifies
  with the same node; F# already requires the two to agree.
- **Construct it in `argTypes`, not `argType`.** `argType` (`:166`) is single-`fresh`/single-return
  and has no `Family`, so it can express neither two entries nor the `'State`/`'Residue` dependency.
  `argTypes` (`:191`) must branch at the top for `FormatFunction`/`Text`, returning the callback
  list directly and bypassing the `starDim … @ [value]` path (F# `%a`/`%t` carry no width/precision,
  so there is zero star interaction).
- **Signature ripple.** `argTypes` gains the family (or just `state`/`residue`). Callers:
  `appliedTypeOf` (`:359`) already holds `fam` — thread it through the `argTypes` call at `:368`;
  `totalArity` (`:345`) must now count `%a` = 2 / `%t` = 1 (the full-application gate reads
  `args.Length = totalArity + idx + 1`, so a miscount mis-fires). The count is type-independent, so
  `totalArity` can stay fam-free via a small arity helper if threading `fam` there is noisy.
- **`'State` is already the resolved type here.** `resolveExternalSlots` (`InferApp.fs:290-296`)
  rewrites `fam.State` to the provider `TyClass(TextWriter)` / `TyClass(StringBuilder)` *before*
  `appliedTypeOf` runs, so a callback parameter `(w: TextWriter)` unifies with the printer's `'State`
  slot directly — B1's writer-slot `TyClass` cleanup carries Track D for free.
- **Partials stay cold** (fully-applied only, like B1/B2): leave `hasConcreteArgType` /
  `isUnaryConcreteHole` returning `false` for `FormatFunction`/`Text` so `%a`/`%t` never enter the 4a
  value-struct lowering. Only `PrintfHoleForm.tryClassify` gains a case (the new callback `HoleForm`),
  which is what flips `lowerablePlaceholders` to admit them. Stale in-code comments to correct when
  this lands: `PrintfSpec.fs:157-165` ("`%a`/`%t` aren't modelled yet") and `:204-208`.

**ABI decision (maintainer, 2026-07-05) — RESOLVED; no longer an open design item.** `State` is
**not** uniformly `TextWriter`: it is already family-dependent in `PrintfSpec.Family` (writer
families `State = TextWriter` / `Residue = unit`; `sprintf` `State = unit` / `Residue = string`;
`bprintf` `State = StringBuilder`). So the native ABI splits cleanly along `Family.State`, and **no
new sink type is introduced now** — the `[<IsByRefLike>]` `Formatter`-vs-boxed-closure problem is
sidestepped per family:
**v1 emit = capture-first (chosen 2026-07-05).** Every family produces a residue *string* that
`Formatter.AppendLiteral`s at the hole — no `Formatter` change, no non-releasing drain, correct
ordering by construction (nothing hits the real sink until the terminal `Flush`). The callback is a
`Vesper.Fun` value, invoked via the native `EmitInvoke` path (NOT `FSharpFunc.Invoke`), so this
reintroduces no FSharp.Core dependency. Per family:
- **Writer families** (`printf`/`printfn`/`eprintf`/`fprintf`): callback `TextWriter -> 'a -> unit`.
  Emit constructs a scratch `System.IO.StringWriter` (a `TextWriter`), invokes `cb(sw)(value)`, then
  `AppendLiteral(sw.ToString())`. Byte-identical to F#'s inline write (the callback writes only to the
  writer it is handed). One `StringWriter` alloc per hole — `%a` is rare; flush-and-pass + a
  non-releasing `Formatter.Drain` stays a later optimization (`Formatter.Flush` is terminal — it
  releases the pooled buffer — so it can't be reused mid-format).
- **`sprintf`** (`State = unit`, `Residue = string`): no sink type at all — invoke `cb(unit)(value)`
  and `AppendLiteral` its returned residue string. The one family with **no external sink dependency**.
- **`bprintf`** (`State = StringBuilder`): scratch `StringBuilder`, invoke `cb(sb)(value)`, then
  `AppendLiteral(sb.ToString())` (the `StringBuilder` entity is already wired for B2).

**Lowerability is provider-capability-driven — this is the load-bearing model check (decision B,
2026-07-05).** `tryClassify` classifies `%a`/`%t` → `HoleForm.Callback` **syntactically** (target-
neutral, no provider — respects `feedback_freeze_no_backend_knowledge`). Whether that hole *lowers*
on a given target is then decided at the gate (`InferApp`, which holds `ctx.Provider`) by whether the
family's sink type is available on **this target's provider**:
- **`sprintf`** needs no external sink (`State = unit`, callback returns the residue), so it lowers on
  **every** target — including JS (residue splice = string concat).
- **Writer / builder families** need `System.IO.TextWriter` / `System.Text.StringBuilder`.
  `resolveExternalSlots` already resolves these via `ctx.Provider.TryLookupType`: a `TyClass` (CLR:
  resolved) ⇒ lowerable; an unresolved by-name `TyConst` (JS: no such type) ⇒ **not** lowerable, so
  the gate raises a diagnostic (`ctx.Diagnostics.Add`, `Severity.Error`) — there is no cold path to
  fall back to once FSharp.Core is dropped.

The **`sprintf`-lowers-but-`printf`-diagnoses asymmetry on JS** is the proof that printf lowering is
genuinely driven by backend-declared capability (the provider's `TryLookupType`), not a hardcoded
`if target = JS`. Same `%a` specifier, same lowering path; the provider's type resolution is the only
thing that differs.

**Eventual zero-copy ABI (post-public break; NOT required for the dependency-drop goal).** Because
Vesper owns the printf types, `'State` can later become the `Formatter` itself (`allows ref struct`
on the `'State` slot), so the callback is `Formatter -> 'a -> unit`, writes straight into the live
handler — no `TextWriter`, no copy — and the `Family.State` split collapses to one sink type
everywhere. It divides into an easy half available *today* and a hard half that waits:
  - **Statically-known callback** (the common `printfn "%a" (fun w x -> …) v` — the lambda is at the
    call site): **inline the callback body** against the live `Formatter` at emit. No `FSharpFunc`,
    no ref-struct-through-a-delegate, no `allows ref struct` — zero-copy now, gated only on the
    surface type being `Formatter`-compatible.
  - **Opaque callback value** (`let p = … in printfn "%a" p v`): can't inline, so the ref struct
    must flow as a value; `FSharpFunc<Formatter,_>` can't exist (a heap closure would box it), so
    this is the half that genuinely needs `allows ref struct` — a ref-struct-clean delegate /
    function-pointer printer ABI.
  Note: `allows ref struct` does **not** make the `TextWriter` surface zero-copy — a ref struct can
  never substitute for the concrete `TextWriter` class; it is relevant only for this Vesper-native
  `Formatter`-as-`State` surface (or a future JS `%a`).

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
`ab78a321`) → **Track C** (`%A` gate relax — C1 + C2, `134d942b`) → **B2** (`bprintf`, 2026-07-05).
All on branch `semantic-analysis`. **Track A is complete** (`tryClassify` total over every lowerable
flag/width/precision form), **Track C is complete** (every `%A` hole lowers natively), and **Track B
is complete** (`fprintf`/`fprintfn`/`bprintf` all lower natively; fully-applied only, JS deferred).
**B2 verified 2026-07-05:** clean build, all suites green — **CLR 1223** (1219 + 4 new) **/ JS 264 /
semantic 711** (+1 skipped) / **Vesper 49**. Remaining printf cold residuals: Track F's star forms
(`%0*d`, `%0*.Nf`, `%-*A`/`%+*A`/`%0*A`) and the deliberate `%+08.2f` (sign + zero-pad float, no
faithful section format) — both classifier-level, pinned in `FSharpCoreDepsTests.fs`/`SelfHostTests.fs`,
and capstone preconditions (the capstone must lower or re-error them). The list/option-*literal*
representation pin is a separate axis (§ Capstone).

**Track D IN PROGRESS (2026-07-05)** — design fully resolved (§ Track D): v1 emit is **capture-first**
(per-family scratch sink → residue string → `AppendLiteral`; `Vesper.Fun` `EmitInvoke`, no
FSharp.Core), and lowerability is **provider-capability-driven** (`sprintf` lowers everywhere incl.
JS; writer/builder need `ctx.Provider.TryLookupType(TextWriter/StringBuilder)`, else a diagnostic).
Implemented in steps:
- **Step 1 — typing seam. LANDED (`c8233247`).** `argTypes` types `%a` (`'State -> 'T -> 'Residue`
  plus value `'T`, one shared typar) / `%t` (`'State -> 'Residue`), threading `fam.State`/`fam.Residue`;
  `tryClassify` untouched so both still route cold. Suites green (711/1223/264).
- **Step 2 — CLR native + shared machinery. LANDED (2026-07-05).** `HoleForm.Callback of hasValue`;
  `tryClassify` admits `%a`→`Callback true` / `%t`→`Callback false`; frozen `FormatSegG.CallbackHole`
  (spec + callback + `value voption`), walked by every TAST traversal incl. escape/free-var
  (`TastWalk`/`TastConvert`/`TastLower`/`Regions`/`ResolvedTypes`/`PlatformTypes`); Freeze captures the
  callback + (for `%a`) value exprs positionally. Provider-capability gate + diagnostic in `InferApp`
  (`sinkAvailable = fam.State is unit or a resolved TyClass`; `rejectCallback` blocks the marker + adds
  a `Severity.Error`). CLR capture-first emit (`EmitFormat.callbackHole`): `sprintf` invokes
  `cb(unit)[(v)]`→residue string→`AppendLiteral`; writer/builder invoke `cb(scratch)[(v)]` into a fresh
  `StringWriter`/`StringBuilder` (discard the unit residue), then `AppendLiteral(scratch.ToString())`;
  callback applied via the native `EmitInvoke` recipe (no FSharp.Core). `StringWriter` TypeRef +
  scratch ctor/ToString member refs wired (`ClrEnv`/`ClrEncoder`/`ClrRecipes`/`FormatHandles`). JS got a
  temporary `failwithf` reject arm (unreached by tests). 10 new CLR tests (shape + byte-parity for
  sprintf/printf/fprintf/bprintf `%a`, sprintf/fprintf `%t`, + a closure-over-local parity test proving
  the escape-walk ripple). No `%a`/`%t` cold pins existed to flip (the cold-recipe guard stays `%+08.2f`).
  Suites green: **CLR 1233** (1223 + 10) **/ JS 264 / semantic 711** (+1 skipped).
- **Step 3 — JS asymmetry (proves the model). LANDED (2026-07-05).** JS `EmitJs.buildCallbackHole`
  invokes the curried `Vesper.Fun` via the ordinary `JsExpr.Call` shape (`cb(undefined)[(v)]`; `unit`
  = `undefined`) and splices its residue string; only the `ToString` sink is reachable (non-`ToString`
  callback holes are a defensive `failwithf` — the gate diagnoses writer/builder `%a` before Freeze,
  confirmed empirically: the JS provider surfaces no `System.IO.TextWriter`/`StringBuilder`). 5 new JS
  tests: `sprintf "%a"`/`%t`/closure-over-local/multi-segment run byte-exact under Node, and
  `printf "%a"` on JS asserts the *"requires a sink type"* diagnostic — the two-outcome proof in one
  file. **Incidental gap closed:** plain `sprintf` (the `ToString` sink) was entirely unwired in JS
  emit (`failwithf "unsupported format sink"`); added `| ToString -> arg`, so ALL `sprintf` now lowers
  on JS, not just `%a`. Suites green: **JS 269** (264 + 5) **/ CLR 1233 / semantic 711** (+1 skipped).

**Track D is COMPLETE** — `%a`/`%t` lower natively (CLR every family; JS `sprintf`), writer/builder
`%a` on JS diagnoses via the provider-capability gate. Only the `TextWriter`-shim future increment
(above) would extend JS to the writer families.

**Future increment (not this sprint): JS writer-family `%a` via a `TextWriter` shim.** The step-2
gate diagnoses writer/builder `%a` on JS purely because `ctx.Provider.TryLookupType` doesn't surface
`System.IO.TextWriter` there — a *capability-absent* state, not a dead end. A platform-specific
`.js.fs`/`.fsi` shim (a `TextWriter`-compatible class — `Write`/`Flush`/`ToString`, plus a
`StringWriter` and a `Console.Out`-equivalent), registered through the JS symbol provider via the
dual-face `Class` mechanism (`ExternalSymbols.resolveCapabilities`, canonical `System.IO.TextWriter`
↔ platform face), flips that capability: the *same* gate then admits writer `%a` on JS with **zero
changes** to `SemanticAnalysis` or the gate, and the step-2 capture-first CLR emit transliterates
straight onto the JS `StringWriter`. Keeping the surface type `System.IO.TextWriter` (rather than a
separate JS-only `ITextSink`) is what makes `fprintf (w: TextWriter) "%a" …` source-identical on both
targets — it supersedes the earlier B-track "JS gets an `ITextSink`" note. Wrinkles for then: the emit
calls PascalCase `.ToString()`/`Write`, so the shim's compiler-facing surface must be PascalCase; and
`Console.Out`'s line-buffer/flush semantics vs the `Formatter`'s single terminal flush is a parity
call to pin with an oracle.

**Then:** D → E1 (const-literal format) → E2 (runtime runner; heaviest) → capstone.
F (`%*d`) is a separable feature.

**Working method (established this sprint):** oracle-first — confirm exact bytes and F#-acceptance
with `dotnet fsi tmp/printf_a1.fsx` before coding a batch; one subagent per step implementing +
flipping/adding tests (NOT committing); maintainer reviews the diff, runs the full suites, and
commits with a short message. Surface design questions before coding them (Track D's sink ABI —
the last big one — is now resolved, § Track D).

## Testing

- **Flip, don't delete, the cold-pin tests** — each `PrintfHappyPathTests` `failtest`-on-`Format`
  case becomes an assert-`Format` case as its form is closed; `FSharpCoreDepsTests` cold pins
  become `isEmpty`.
- **Byte parity** via `runParity` against the test process's own `sprintf` for every Track-A/B
  form (except `%A` of FSharp.Core types, where ToString divergence is expected — assert the
  native-ness / no-dependency, not F# parity).
- **`%a`/`%t` and format-as-value** get new runnable tests once their machinery lands.
