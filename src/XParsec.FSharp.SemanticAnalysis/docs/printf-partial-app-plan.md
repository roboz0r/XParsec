# Partial application — a `Fun` value struct over a statically-emitted spec

**Status: substrate LANDED; the printf gate (step 4) and the `n > K` residual
codegen (step 5) remain.** The `Fun` value-struct substrate that partial-app rides on
(flat interfaces + arity-≤`K` inference + arity-≤`K` closure codegen) is built, tested,
and committed — proven end-to-end for *any* saturated multi-arg lambda, not yet wired to
printf. For the landed printf architecture see
[printf-architecture](printf-architecture.md). This plan is deleted once steps 4–5 land.

Decisions locked:

- Flat-arity cap `K = 4`.
- `n > K` degrades **FSharp.Core-style — one flat-`K` head + a curried tail** (option A),
  NOT a greedy flat→flat chain. Rationale: the tail is existing curried closure codegen (no
  flat→flat residual), and A→B is a non-breaking internal change until we target binary
  compatibility of emitted assemblies — far past compiler v1. The greedy flat chain (option
  B) stays documented as the future promotion.
- Within-chunk partial application is deferred (Phase B).

## What has landed (substrate — steps 1–3)

General `Fun` function-representation work; printf-independent. Each step is its own commit;
existing suites stayed green throughout (no arity-3/4 slot existed before, so it is purely
additive). Code is the source of truth — the anchors below are entry points, not a spec.

- **Step 1 — semantic arity peel (`4922e9f2`).** `funSlotArityOf` (`Engine.fs`) now maps a
  `Fun`(n)` slot to arity `n - 1` for `n ∈ 2..5` (was hard-coded 2→1, 3→2). The arrow↔`Fun`
  correspondence in `Subsume.fs` and the grounding arm in `Engine.fs` were replaced by one
  arity-parametric peel (peel exactly `targs.Length - 1` arrow domains, residual codomain
  matched whole — a `> K` tail stays curried, not peeled). `FunVerdict.Arity` (`SideTypes.fs`)
  now carries 1..4. `InferApp.fs`'s verdict-recording was already arity-driven — no change.
  *One behavioural delta*: the Subsume arm now `resolveStep`s a linked-`TyVar` codomain before
  matching the inner arrow (strictly the more-correct resolve-then-match; arity-1 untouched).
- **Step 2 — library interfaces (`8321632d`).** `Fun<'A,'B,'C,'D>` (CLR `Fun`4`, flat arity 3)
  and `Fun<'A,'B,'C,'D,'E>` (CLR `Fun`5`, flat arity 4) declared in
  `src/Vesper.Core/prim-types-min.fsi` + `.fs`, mirroring the arity-2 `Fun<'A,'B,'C>`. Golden
  `.parsed` snapshots regenerated (`-UpdateSnapshots`).
- **Step 3 — arity-≤`K` closure codegen (`b8d9a6c4`).** A saturated `N`-arg lambda
  (`N ∈ 2..4`) into a `Fun<…>`-bounded slot lowers to ONE flat value-struct `Invoke(a,b,c[,d])`
  — no box, no `newobj`, no nested inner closures. Changes:
  - `EmitTypes.fs`: `Emit.Closure.Param2 : … voption` → `ExtraParams : (NodeKey * FrozenType *
    Frozen.TPat) list` (the flat params beyond the first; length `FunArity - 1`). `FunArity`
    now `1..4`.
  - `EmitClosures.fs`: the discovery peel (`go`) and `registerClosure` peel `arity - 1` inner
    lambdas into `ExtraParams`, recursing into the deepest body; a non-saturated shape falls
    back to arity-1.
  - `ClrEnv.fs`: `eFun4`/`eFun5` TypeRefs + `flatFunEntity arity` (3⇒`Fun`3`, 4⇒`Fun`4`,
    5⇒`Fun`5`).
  - `ClrEncoder.fs` / `ClrRecipes.fs` / `ClrProvider.fs`: arity-parametric `InvokeSignatureN`
    and `FlatFunInterfaceSpecN` replace the per-arity `InvokeSignature`/`InvokeSignature2` /
    `FlatFunInterfaceSpec` (now deleted).
  - `Assembler.fs` dispatches on `FunArity`; `Emit.fs` binds `ExtraParams.[i]` → `ldarg.(2+i)`.
  - Tests: `StructSeqTests.fs` `apply3`/`apply4` (clone of `apply2`) assert value-type base,
    flat `Invoke` param count 3/4, exactly one closure type, no `newobj`, `constrained.`, no
    `box`.
- **Testing infra (`c82aa62d`, not a printf step but a prerequisite that surfaced here).**
  `Vesper.Core.dll` is now compiled **fresh from source** at test time — the committed
  `src/Vesper.Printf/refs/Vesper.Core.dll` (which shadowed the fresh core at runtime and
  drifted silently on any surface change — e.g. it lacked the new `Fun`4`/`Fun`5`) is
  **deleted**, along with its `<Reference>`, the `REGEN_VESPER_CORE_REF` sync-guard, and the
  frozen `StructuralFormatBaseline`/`Bench` perf duplicate. The last fsc compile-time consumer
  (`StructuralFormatTests.fs`'s hand-written `IStructuralFormattable` impls) moved to a `<None
  Include>` fixture (`StructuralFormatFixtures.fs`) compiled through our backend at runtime via
  `TestHelpers.compileFixtureFile`. **Consequence for future work: adding a type to Vesper.Core
  needs no binary regen** — the fixtures recompile from source.

## Current behaviour (the gap step 4 closes)

`let p = printfn "%d"` already *works* — but via the FSharp.Core cold path, not the
Vesper handler. The happy-path gate (`Passes/Unification/InferApp.fs`, `tryInferPrintfApp`)
marks for inline lowering only fully-applied literals (`idx = 0 && args.Length =
specs.Length + 1 && lowerablePlaceholders`); an under-applied call is left unmarked, so
`ElaborateExpr.fs` keeps the `App printfn` intact and it lowers to
`Microsoft.FSharp.Core.PrintfModule`. Correct output, but it allocates the `PrintfFormat`
object + closures the rest of `Vesper.Printf` is built to avoid.

## Target design

`let p = printfn "%d"` becomes a value struct `S : Fun<int, unit>`:

- **Stateless ⇒ `default(S)`, zero heap allocation.** The format is a compile-time
  constant, **baked into the statically-emitted `Invoke` body** — exactly as the happy path
  bakes it into the call site. So `S` carries no per-instance state, and
  `constrained.callvirt Fun::Invoke` devirtualises (rides the landed `Fun` value-struct work —
  see [function-representation-plan](function-representation-plan.md)).
- **No `PrintfFormat` on this path.** For a literal format there is no `PrintfFormat` value
  and no runtime spec — same as the happy path. `PrintfFormat` survives *only* on the cold
  path (non-literal / format-as-value), demoted to a static-field representation driven by the
  runtime spec-runner.
- **Multi-hole: one flat-`K` head + a curried tail (option A).** `printfn "%d %s"` fully
  unapplied is a *flat* value struct `Fun<int, string, unit>` (the landed flat arity-2
  interface; stateless). Matching FSharp.Core's `OptimizedClosures` (never nests flat
  closures): take **one** flat chunk of arity `min(K, n)`, and the codomain beyond it is the
  **ordinary curried `Fun`2` chain**, not another flat chunk:
    - `n ≤ 4` → one flat `Fun`*(n+1)*, `Invoke(h1..hn) : unit` (one dispatch, zero-alloc). **The
      substrate for this is landed (step 3); step 4 mints these from the printf gate.**
    - `n = 6` → `Fun<h1,h2,h3,h4, (h5 -> h6 -> unit)>` — a flat-4 whose codomain is the
      curried arrow `h5 -> h6 -> unit`. Full application is **one** flat `Invoke(h1..h4)`
      returning a residual capturing `h1..h4` (remaining segments baked into the residual's
      statically-emitted `Invoke` — no runtime spec to capture), then two ordinary curried
      applies. **This head-returns-curried-residual codegen is step 5 (greenfield).**

  Library-wise `K = 4` is three flat interfaces: `Fun`3`/`Fun`4`/`Fun`5` (all landed). The
  `n > K` residual is a plain curried closure — existing closure codegen, no `Curried`/
  `Flattened` adapters, no flat→flat value-struct. Because `> K` holes is rare for printf, the
  curried-tail alloc there is acceptable (FSharp.Core pays the same on its `Adapt` slow path).

- **Within-chunk partial application is Phase B (deferred).** Supplying FEWER than the flat
  head's arity (`printfn "%d %s" x`, a flat-2 with one arg) needs the `Curried`*k* residual
  over the head's remaining args, reusing the `Curried`/flat-`Fun` adapter machinery in
  `Vesper.Core/core-types`. For `n ≥ 2` holes, only the fully-unapplied struct and the
  fully-saturated call land in step 4; a proper subset stays on FSharp.Core until Phase B.

- **Escape caveat.** Zero-alloc only while `p` flows into `Fun`-bounded generic positions or
  is invoked directly; an interface-typed `Fun<int, unit>` slot boxes it. There is **no**
  non-`inline` single-use-let copy-propagation in the front end (`Passes/InlineExpansion.fs`
  beta-reduces only lambdas bound to `inline` parameters), so `let p = printfn "%d" in p 3`
  does **not** fuse to the happy-path `printfn "%d" 3` — the gate lowers the under-applied
  `printfn "%d"` to `default(S)` and `p 3` invokes it directly. Non-escaping, so still
  zero-alloc, but via the value-struct path, not by inlining.

## Remaining work

### Step 4 — the printf gate (`n ≤ K`)

Wire the landed substrate to printf. **Entry point: `Passes/Unification/InferApp.fs`
`tryInferPrintfApp`.** Add an *under-applied lowerable* case beside the fully-applied
happy-path marker: a literal format, `idx = 0`, all placeholders lowerable, `%a`/`%t`-free,
`args.Length < specs.Length + 1`. Divert it to the value-struct lowering instead of falling
through to FSharp.Core.

- **Arity comes from a per-hole arg-type seam — do NOT assume holes = args.** Peel the
  flat/curried arity from the saturated function type the happy path already builds
  (`PrintfSpec.appliedTypeOf`, via the per-hole `argType : FormatType -> SemType`). Today every
  lowerable hole yields exactly one arg type, so arity = hole count and `args.Length =
  specs.Length + 1` is the saturation test — but key on the *summed per-hole arg count*.
  Dynamic width/precision (`%*d`) is a planned feature (lexer currently rejects `*`, see
  [printf-architecture](printf-architecture.md)); landing it turns `argType` into a
  hole → arg-types mapping (`%*d` = width + value), which an arity peel routed through the seam
  absorbs without a structural change.
- **The open design question — RESOLVED (2026-07-04), with a scope-widening caveat.** The
  landed step-3 substrate produces value structs from *source lambdas* via the node-keyed
  `FunVerdict` + `EmitClosures` peel. A printf partial is **not** a lambda — so step 4 must
  either (a) synthesise a lambda-shaped `Frozen` node the closure machinery already lowers, or
  (b) add a dedicated printf-partial value-struct emission whose `Invoke` body is the
  `EmitFormat` unroll.

  **What the code review found** (the premise the fork silently assumed, now checked against
  `EmitClosures.discoverClosures`): the landed substrate does **not** produce a free-standing
  value struct at all. It only ever mints one for a lambda that is *simultaneously* (i) an
  **argument** landing on an explicit `:> Fun<…>`-bounded combinator typar (that is the sole
  source of a `FunVerdict` — `InferApp.recordFunArityVerdicts`), (ii) **anonymous**
  (`ValueOption.isNone selfKey` — a `let p = <lambda>` is disqualified and falls to a heap
  closure), and (iii) **monomorphic** (`currentTypars = 0`). And it is only ever *invoked* from
  **inside** that combinator body as `f.Invoke(…)` on the constrained typar
  (`constrained.callvirt`, the `apply2`/`apply3`/`apply4` tests). There is **no** existing path
  for "bind a value struct to a name / return it, then apply it later" — a value struct today is
  constructed and consumed in the *same* combinator call, never stored, returned, or applied via
  a bare `p x`.

  A free-standing printf partial has **none** of those anchors: `let p = printfn "%d"` types `p`
  as the bare arrow `int -> unit` (no `Fun` bound, no combinator, no verdict), the binding sets
  `selfKey` (heap-disqualified), and `p 3` is an ordinary curried-closure `App`, not a
  constrained-typar `Invoke`. So the true content of step 4 is **greenfield representation +
  application dispatch for a free-standing value-struct closure** — the piece the substrate never
  needed. The (a)/(b) fork is only about *how to fill the `Invoke` body*; **both** arms still
  need that free-standing bridge, and route (a) additionally fights the anonymous-only /
  no-`Fun`-bound restrictions (it would need the closure machinery to value-struct a *named,
  bare-arrow-typed* binding — a substrate change to `discoverClosures`' `isValueStruct` gate,
  which is step-3 territory, not step 4).

  **Decision: route (b), a dedicated printf-partial value-struct emission, with a
  statically-unrolled `Invoke` body** (confirmed with the maintainer 2026-07-04). It matches the
  escape-caveat/acceptance text already in this doc (the partial *is* `default(S)`, `p 3` is
  `S.Invoke(3)` — a direct call on the concrete struct, NOT a `constrained.callvirt` on a typar,
  so it is actually *simpler* than the combinator case), and it does not perturb the
  combinator-arg substrate. The `Invoke` body **statically unrolls** through the existing
  `EmitFormat` lowering (no runtime spec-runner, no spec value, no `static readonly` spec field);
  a runtime-runner + parsed-spec-field design was considered and deferred — it only pays off once
  the *whole* `Xprintf` family (cold / format-as-value paths) converges on one runner, which is
  out of scope here. The work route (b) entails, none of which the substrate supplies:
    1. **Mint a concrete stateless closure struct** `S : Fun<h1,…,hn,tail>` per marked partial
       (flat arity `n ≤ K`), no instance fields, format baked into its `Invoke` body via the
       `EmitFormat` unroll — reuse the value-struct closure *emission* machinery (the same path
       that turns an `apply2` `fun x y -> …` into a `Fun`-implementing struct with a flat
       `Invoke`), but driven from the printf gate, not from a `FunVerdict`-tagged `Lambda`.
    2. **Represent the partial's value** as `default(S)` (stateless ⇒ zero-init struct) at the
       binding/return site, even though its *front-end type* is the bare arrow `int -> unit`.
    3. **Lower a saturated application** `p h1 … hn` of such a partial to one flat `S.Invoke(…)`
       (direct, non-virtual — `S` is concrete), the `n > K` residual deferred to step 5.

  **The dispatch crux (found while mapping codegen — this, not the `Invoke` body, is the hard
  part).** The value-struct *emission* machinery is fully reusable: a `Closure` record with
  `IsValueStruct = true`, `Captures = []`, `FunArity = n`, `Body = <a Format node>` flows through
  `Layout`→`Assembler` to a sealed sequential-layout `Vesper.Fun`N`-implementing struct with zero
  instance fields, and a captureless one constructs as `ldloca; initobj; ldloc` — exactly
  `default(S)` (`EmitConstruct.buildValueStructClosure`). What is **not** built is *dispatch of a
  free-standing partial*, and it cannot be bolted on the way the combinator path was:
    - For **soundness the partial's front-end type must stay the arrow** `int -> unit`. `p` may
      escape into an arrow-typed slot (`List.iter p xs`), where it must unify as `int -> unit`
      and box to the `Fun<int,unit>` interface. So `S` is a *representation*, never the node's
      type — typing `p` as `S` would break every non-invoke use.
    - Therefore `p 3` → `S.Invoke(3)` (direct, unboxed) vs. a boxed `callvirt` on escape is a
      **representation/escape decision codegen must make**, keyed on *which values are currently
      in unboxed-`S` form* — NOT on `typeOfExpr`, which says `int -> unit` for both. The existing
      value-struct path never faced this: a combinator arg is constructed inline and consumed in
      the *same* call via `constrained.callvirt` on the `:> Fun` typar — it is never stored,
      returned, or re-loaded, so no "is this local boxed?" question ever arises. A free-standing
      partial stored in a `let` (or returned from a function, the `f ()`/`g` case) is exactly
      that missing analysis.

  This makes the zero-alloc value struct a **representation-analysis** feature, materially larger
  than "reuse the emission machinery" implied. So the decomposition leads with a correct,
  Vesper-native, still-allocating slice and isolates the representation work:

  **Sub-step breakdown** (each a full green vertical slice — the suite is source→output
  end-to-end, so a partial slice would leave an un-lowerable node and break the build).
  **Priority (set by the maintainer 2026-07-04): breadth before the zero-alloc optimization.**
  Getting the *whole* printf family lowered natively so the compiler can drop its FSharp.Core
  dependency is the near-term goal; the value-struct optimization (former 4b) is deferred behind
  that breadth. Revised order:
    - **4a — Vesper-native *heap* closure (correctness baseline, nearly free). LANDED
      (2026-07-04).** Synthesised a Vesper closure `fun h1 … hn -> Format(sink, …)` for the
      fully-unapplied lowerable partial, emitted on the *existing heap* closure path (a
      `let`-bound lambda is already heap by `discoverClosures`' `selfKey` rule; `Invoke` body =
      the `EmitFormat` static unroll), dispatched via the existing curried-closure
      `Fun`2`::Invoke` machinery (`emitInvoke`) — **no new representation analysis**. Drops the
      FSharp.Core `PrintfModule`/`PrintfFormat` path for these. Code: `PrintfSpec.hasConcreteArgType`,
      `SideTables.PrintfPartial`, the `InferApp.tryInferPrintfApp` under-applied marker,
      `ElaborateExpr.translatePrintfPartial`; tests in `PrintfPartialTests.fs`. Not zero-alloc (one
      closure object); that is 4c.
    - **4b — native breadth (its own sprint): close every remaining FSharp.Core cold-printf
      degradation.** Moved to **[printf-specifier-coverage-plan](printf-specifier-coverage-plan.md)**
      (a verified degradation inventory + tracks A–F + the `rm FSharp.Core.dll` capstone). It is
      the near-term priority and is independent of the value-struct representation work below —
      it stays on the heap-closure lowering throughout (coverage, not allocation). Decisions
      locked there (2026-07-04): close the whole hard tail (`%a`/`%t`, arbitrary `%A`,
      format-as-value); FSharp.Core-owned `%A` args ToString-degrade on the native engine rather
      than route cold.
    - **4c — zero-alloc value struct (deferred optimization; the representation work).**
      Re-represent the non-escaping partial as `default(S)` and dispatch `p h1…hn` as a direct
      flat `S.Invoke`, boxing to the `Fun<…>` interface only where it escapes into an arrow-typed
      slot — the free-standing-value-struct bridge + escape decision above. Covers `let p = … in
      p 3` and the `let f () = printf "%d %s %b"` / `let g d s b = f () d s b` return-crossing
      case (the `apply3` analogue). Within-chunk partials stay Phase B.
- **Freeze:** `ElaborateExpr.fs` currently diverts a marked happy-path call to a `TExpr.Format`
  node. The partial case needs its own lowering — a value-struct closure of arity = hole count
  whose `Invoke`, given `h1..hn`, runs the same segment-unroll a `TExpr.Format` does.
- **Invoke body = the `EmitFormat` unroll.** `Codegen.Clr/EmitFormat.fs` materialises the
  happy-path handler calls (one `AppendLiteral`/`AppendFormatted<T>` per segment, trailing
  `"\n"` for `printfn`, `Flush` vs `ToStringAndClear` sink). The partial `Invoke` reuses this,
  so the trailing newline + sink choice + byte-for-byte parity fall out for free.
  `Codegen.Clr/ClrHoleFormat.fs` maps each spec to handler args.
- **Scope:** `printf` / `idx = 0` / `n ≤ K` / lowerable / no `%a`/`%t` only.
  `fprintf`/`sprintf`/`eprintf`, the `idx ≠ 0` writer/builder sinks, within-chunk (Phase B),
  and `n > K` (step 5) stay on FSharp.Core. Review the whole family once `printf` lands.

### Step 5 — the `n > K` curried-residual codegen (greenfield)

The flat-`K` head `Invoke(h1..hK)` **captures** `h1..hK` and **returns** an ordinary curried
closure for the tail (`h(K+1) -> … -> unit`), with the remaining format segments baked into
the residual's statically-emitted `Invoke`. This is the one greenfield piece: a prior survey
confirmed there is **no** existing "flat head `Invoke` returns a captured curried closure"
codegen — everything flat today fully saturates in one `Invoke`. Only needed for **5+‑hole**
partial printf; those stay on FSharp.Core (correct, just allocating) until this lands. This
was the "head flat `Invoke` returns that curried residual" half of the original combined
step 3 — split out because it is greenfield and separable from the saturated substrate.

**Call-side application (option A).** Saturated application of an `n > K` partial
(`p a b c d e f`) then lowers to one flat `Invoke(a,b,c,d)` on the head, then ordinary curried
applies for the tail (`… e f`) — exactly FSharp.Core's `invokeFast5 … a6` shape. No `⌈n/K⌉`
flat-chunk loop, no flat→flat residual.

## Picking up in a fresh session

- **Where things stand:** substrate (steps 1–3 + infra) landed and committed; `git log
  --oneline` shows `b8d9a6c4` (step 3) back through `8321632d` (step 2). The next commit is
  step 4.
- **Prove the substrate quickly:** `./claude_tools.cmd -Action Test -TestProject
  "XParsec.FSharp.Codegen.Clr.Tests"` (1111 green). The `apply3`/`apply4` tests in
  `StructSeqTests.fs` are the template for how a flat value-struct `Invoke` is asserted
  (value-type base, param count, no box); an analogous `printf`-partial test is the step-4
  acceptance vehicle.
- **Key source anchors:** gate = `Passes/Unification/InferApp.fs:tryInferPrintfApp`; arity seam
  = `PrintfSpec.fs:argType`/`appliedTypeOf`; happy-path freeze = `ElaborateExpr.fs` (`TExpr.Format`);
  happy-path emit = `Codegen.Clr/EmitFormat.fs` + `ClrHoleFormat.fs`; the landed closure
  machinery = `Codegen.Clr/EmitClosures.fs` (`ExtraParams`, the peel) + `EmitTypes.fs`
  (`Emit.Closure`) + `ClrEnv.fs` (`flatFunEntity`) + the `…N` recipes in `ClrEncoder`/
  `ClrRecipes`/`ClrProvider`.
- **Gotcha carried over:** the arity seam. `argType` returns one `SemType` per hole today;
  the gate's arity computation must sum per-hole arg counts (a no-op now, but the shape that
  lets `%*d` drop in later). Don't hard-code `arity = specs.Length`.
- **Follow `feedback_redesign_doc_first`:** resolve the step-4 "synthesise a lambda node vs
  dedicated printf-partial emission" fork in this doc before coding.

**Prior art — FSharp.Core `OptimizedClosures`** (local port,
`src/XParsec.FSharp.Lib/Clr/prim-types.fs`, the `OptimizedClosures` module ~`:3779+`). Flat
`FSharpFunc<T1..Tn,U>` for **n = 2..5** (cap 5), each *inheriting* the curried form; `Adapt` =
type-test-or-wrap (≈ our `Curried`/`Flattened`, Phase B); `invokeFast{2..5}` = a
widest-flat-then-degrade `:?` cascade. Crucially FSharp.Core **never nests flat closures** —
`invokeFast` uses at most ONE flat `Invoke` and curries the rest (6-arg = `invokeFast5 … a6`),
which is exactly option A. Reuse the *design* (small cap, one-flat-head + curried-tail, `Adapt`
for Phase B) — NOT the mechanism: inheritance + heap classes + runtime type-tests are what the
value-struct / static-verdict / no-inheritance-bridge design rejects (Vesper picks the flat
head statically via the verdict, never by a runtime cast, and the head is a value struct, not a
heap class). *(Line numbers drift — grep the `OptimizedClosures` module.)*

**Option B (future promotion, not v1) — greedy flat→flat chain** (`n = 6` as
`Fun<_,_,_,_,Fun<_,_,_>>`, flat-4∘flat-2), for better perf on rare wide saturated calls. The
chunk-boundary residual is a **heap reference, not a value struct** — deliberately: it captures
the whole head chunk (`K` args + format), so passing a pointer beats copying `K + 1` words, and
it boxes anyway the moment it flows through a `Fun`-typed slot. The one case a value copy would
save an alloc (a non-escaping, monomorphic, immediately-saturated residual) is exactly what
option A's curried tail already covers. So B's boundary residual is ordinary heap-closure
codegen. B is non-breaking over A (same `Fun`-typed public surface up to arity `K`; only the
`> K` codomain shape changes) until emitted-assembly binary compatibility is a goal — far past
compiler v1 — so it is deferred.

## Acceptance

- `let p = printfn "%d" in p 3` prints `3` with **no heap allocation**. No non-`inline`
  single-use-let inlining (see Escape caveat), so this does *not* fuse to `printfn "%d" 3`; the
  gate lowers `printfn "%d"` to `default(S)` and `p 3` invokes `S.Invoke(3)` directly —
  zero-alloc because `default(S)` is stateless and `p` does not escape. An interface-typed
  `Fun<int, unit>` slot boxes it.
- **Motivating multi-hole scenario** (forces the flat-head machinery — the natural step-4 test,
  cf. `apply3`): given `let f () = printf "%d %s %b"`, `f ()` yields the fully-unapplied flat-3
  struct `Fun<int, string, bool, unit>`; `let g d s b = f () d s b` saturates it in **one** flat
  `Invoke(d, s, b)`. The within-chunk form `let h d s = f () d s` is Phase B (FSharp.Core until
  then).
- Multi-hole lands flat (option A): `n ≤ 4` as a single flat `Fun<…>`, zero-alloc where it
  doesn't escape (step 4); `n > 4` as one flat-`K` head + a curried tail (step 5), so
  `printfn "%d %s %b %f %d %s"` fully applied runs as one flat-4 `Invoke(h1..h4)` (returning a
  curried residual) then two ordinary curried applies — one flat dispatch + two curried, not
  six curried.
- Byte-for-byte parity with FSharp.Core's output across the spec matrix — including the
  `printfn` trailing newline, from sharing the `EmitFormat` lowering.
- The cold-path spec-runner and the partial-application `Invoke` share one **handler**
  (`formatter.fs`); the `Invoke` itself is statically unrolled, not run through the runtime
  spec-runner.
- **Scope: `printf` only.** The rest of the family is a follow-up review, not this landing.
