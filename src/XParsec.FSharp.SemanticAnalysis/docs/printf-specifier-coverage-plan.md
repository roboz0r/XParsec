# Printf native-coverage sprint — remaining work to drop FSharp.Core

**Goal.** No lowerable printf-family call falls back to FSharp.Core's cold printf, so the compiler
can drop its `FSharp.Core.dll` dependency. Ephemeral doc — deleted once the sprint lands
(`feedback_plan_docs_ephemeral`).

## Status

**Landed (Tracks A–F + E1), all on branch `semantic-analysis`:** every lowerable flag/width/precision
form (`%05u`, `%08o`, `%+05d`, `%-05.2f`, `%08e`, …); the writer/builder sinks
(`fprintf`/`fprintfn`/`bprintf`, fully-applied, CLR); `%A` breadth (every concrete nominal + a
polymorphic typar renders on the native engine, ToString-degrading FSharp.Core/BCL types); `%a`/`%t`
callback holes (CLR every family, JS `sprintf`; writer/builder `%a` on JS diagnoses via the
provider-capability gate); star-width `%*d` / `%.*f` / `%*A`; and E1 (a format literal bound to a
name or ascribed — `let fmt : Vesper.Format<…> = "%d"`, `(… : PrintfFormat<…>)` — lowers to the same
native `TExpr.Format` a syntactic literal does).

**Remaining to remove `FSharp.Core.dll`:** lower-or-re-error the cold residuals (§1) → capstone (§2).
E2 (dynamic format, §3) is deferrable and NOT on the critical path.

## Where FSharp.Core is pinned (the capstone target)

The three cold recipes that emit the dependency live in `Codegen.Clr/ClrRecipes.fs`:
`emitPrintfn` → `PrintfModule.PrintFormatLine`; `emitPrintfFormatCtor` → `PrintfFormat`4 .ctor`;
`emitFSharpFuncInvoke` → `FSharpFunc`2.Invoke`. They are routed via `ClrProvider` (`isColdPrintf`,
`TryEmitFSharpFuncInvoke`), reached only when the gate/Freeze leave an *unmarked* printf `App` — a
form that didn't lower natively. Close every such form ⇒ nothing reaches the recipes ⇒ delete them.

---

## Outstanding work

### 1. Cold residuals — lower or re-error (capstone precondition)

These still route cold, pinned in `FSharpCoreDepsTests` / `SelfHostTests`. The capstone must lower
each natively OR turn it into a `Severity.Error` at the gate (a *diagnosed* form pins nothing):

- **Star + flag combos:** `%0*d`, `%0*.Nf` (zero-pad star), and `%-*A` / `%+*A` / `%0*A` (flagged
  star-`%A`).
- **`%+08.2f`** — sign + zero-pad float; it has no faithful section format, so no native lowering
  today.

(`%*%` / `%5%` are *rejected* — an accepted deviation; F# consumes the width and prints a bare `%`.
Not residuals.)

### 2. Capstone — remove `FSharp.Core.dll`

Once §1 leaves no reachable unmarked printf `App`:
1. Delete `emitPrintfn` / `emitPrintfFormatCtor` / `emitFSharpFuncInvoke` + their `ClrProvider`
   routing (`isColdPrintf`, `TryEmitFSharpFuncInvoke`).
2. Flip every `FSharpCoreDepsTests` cold-pin assert to `isEmpty`, and the `PrintfHappyPathTests`
   `App`-asserting cases to `Format`.
3. Drop the `FSharp.Core` reference row / on-disk copy from the app-materialisation path where the
   printf family was its only consumer.

**Adjacent axis (separate — needed for a *full* `rm FSharp.Core.dll`, not for "printf pins
nothing").** `%A` of a *list/`option` literal* pins FSharp.Core via the list/option *representation*
(`[1;2;3]` builds `FSharpList` `Cons`/`Empty`), independent of printf (`FSharpCoreDepsTests:112`). A
full removal also needs list/option to resolve to the Vesper types (`ctx.DefaultListIsVesper`). Track
separately; this sprint's success is measured by *printf* pinning nothing.

### 3. E2 — genuinely dynamic format (deferrable)

`Printf.StringFormat(runtimeStr)` — the format string is computed at runtime. **Not on the critical
path:** the un-lowered `New PrintfFormat(nonLiteral)` boundary is *typed*, so E2 can be **diagnosed**
(`Severity.Error` at the gate) until a real use case demands a runtime runner — and a diagnosed E2
already satisfies the capstone precondition.

**Load-bearing fact — E2 is never runtime-variadic.** The `PrintfFormat<'Printer,'State,'Residue,'Result>`
type argument statically pins the arity and every hole's type; obtaining a runtime format forces an
explicit annotation that fixes the shape (`let f : Printf.StringFormat<int -> string> =
Printf.StringFormat(runtimeStr)`). So the *apply signature* is always compile-time-known; only the
*body* (which literal chunks, `%d`-vs-`%x` per typed hole) is runtime.

**When the runtime runner is built (its own sub-sprint):** model each format as one typed `Apply`
(parameters = the holes, from the `PrintfFormat` type — never variadic), with the interpreter body a
`static readonly` parsed-spec + handler loop (`printf-architecture.md`). Feasibility spike FIRST — can
a synthesised inline template ride `Passes.InlineExpansion` down to straight-line code as tight as
today's literal path? If not, E1's const-prop generalises and the interpreter is reserved for the
genuinely-dynamic case.

### 4. Optional cleanups & future increments (not blockers)

- **Track D refactor — dissolve the capture-first emit special case (designed; chosen 1b).** Track
  D's CLR/JS emit (`EmitFormat.callbackHole` / `EmitJs.buildCallbackHole`) hand-wires the scratch sink
  as a by-name `FTConst("System.IO.StringWriter")` / `FTConst("System.Text.StringBuilder")` — a second
  spelling of BCL types the general external path already resolves. **Fix:** lower writer/builder
  `%a`/`%t` to an ordinary residue block in Freeze — `{ let s = new <Scratch>() in (cb s [value] |>
  ignore); s.ToString() }` — every node of which Freeze already mints (`TExpr.New` / curried `App` of
  the `Vesper.Fun` callback via the native invoke path / `ExternalMember` `.ToString()` / `Let`);
  `sprintf` stays the trivial `cb unit [value]`, no block. Frozen `FormatSeg.CallbackHole(spec,
  callback, value voption)` collapses to `CallbackHole(spec, residue: TExpr)`, so both emits and the
  two-field arms across `TastWalk`/`TastConvert`/`TastLower`/`Regions`/`ResolvedTypes`/`PlatformTypes`
  reduce to "walk one expr". Model the scratch as a `Family.ScratchSink` rewritten by
  `resolveExternalSlots`; the gate resolves the `ToString` `SymbolKey` and enriches the `PrintfApp`
  marker; Freeze synthesises the block (no provider access). **Deletes:** both emit helpers, both
  `ClrEncoder` `FTConst(StringWriter/StringBuilder)` arms, `eStringWriter`, four `FormatHandles`
  scratch fields, two `ClrRecipes` helpers, both `HoleForm.Callback` unreachable-projection failwiths.
  Pure structural refactor — the byte-parity tests must stay green with **zero** expectation changes.

- **JS writer-family `%a` via a `TextWriter` shim.** The gate diagnoses writer/builder `%a` on JS only
  because `ctx.Provider.TryLookupType` surfaces no `System.IO.TextWriter` there. A platform
  `.js.fs`/`.fsi` shim (a `TextWriter`-compatible class + a `StringWriter` + a `Console.Out`
  equivalent), registered via the dual-face `Class` mechanism
  (`ExternalSymbols.resolveCapabilities`, canonical `System.IO.TextWriter` ↔ platform face), flips
  that capability with **zero** gate/`SemanticAnalysis` changes; the CLR capture-first emit
  transliterates onto the JS `StringWriter`. Keeping the surface type `System.IO.TextWriter` is what
  makes `fprintf (w: TextWriter) "%a" …` source-identical on both targets. Wrinkles: the shim's
  compiler-facing surface must be PascalCase (`.ToString()`/`Write`); `Console.Out`'s line-buffer /
  flush semantics vs the `Formatter`'s single terminal flush is a parity call to pin with an oracle.

- **Zero-copy `%a` ABI (post-public-break; not required for the goal).** Once Vesper owns the printf
  types, `'State` can become the `Formatter` itself (`allows ref struct`): a *statically-known*
  callback inlines its body against the live `Formatter` (zero-copy, available today); an *opaque*
  callback value needs a ref-struct-clean function-pointer printer ABI (the hard half).

- **`Printf.StringFormat` / `TextWriterFormat` / `BuilderFormat` abbreviations** don't exist in
  `Vesper.Printf` (only `PrintfFormat`4` + the `Format`4` abbreviation), so E1 source uses the
  `Vesper.Format<…>` / `PrintfFormat<…>` spelling. Adding the F#-named abbreviations to `Vesper.Printf`
  for source-compat is a small library follow-up.

## Working method & testing

- **Oracle-first:** confirm exact bytes + F#-acceptance with `dotnet fsi` (`tmp/printf_*.fsx`) before
  coding a form — real F# is the parity spec *and* decides which flag/type combinations even compile
  (many are FS0741 errors, not lowerable forms).
- **Flip, don't delete, the cold-pin tests** — a `PrintfHappyPathTests` cold assert becomes an
  assert-`Format`; `FSharpCoreDepsTests` cold pins become `isEmpty`.
- **Byte parity** via `runParity` against the test process's own `sprintf` (except `%A` of
  FSharp.Core/BCL types, where ToString divergence is expected — assert native-ness, not F# parity).
