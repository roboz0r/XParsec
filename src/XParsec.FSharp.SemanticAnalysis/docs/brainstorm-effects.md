# Brainstorm — Purity / effects, and what (if anything) `pure` is for

Started from a narrower question — "should every `Option` module function be
`inline`?" — which generalises into: how should Vesper reason about purity, and
does it need a `pure` annotation (the keyword F# reserves but never used)?

**Leading decision (EF5): purity is *inferred*, not annotated.** F# is already
immutable-by-default and type-inferred; purity inference is the natural extension
of the same machinery. A surface `pure` keyword, *if it exists at all*, is
reserved for the handful of bodies the inferencer cannot see through — SRTP /
inline-IL intrinsics like `(+)`. Everything below is the reasoning that leads
there and the obligations it imposes.

## EF1 — Separate "inline this" from "optimise across this"

Inlining is *always* legal in a strict language — it's substitution; you never
needed purity to license it (you can inline a side-effecting function). What a
compiler lacks is not *permission* to inline but *justification* (is the bloat
worth it). So purity is the wrong lever for inlining; `inline`-as-hint is.

The real prize is the opposite: purity lets the optimiser reason **across a call
it does *not* inline** — CSE two identical calls, hoist one out of a loop, drop an
unused result, reorder, memoise — and, expressed as a *summary* rather than a
body, **it survives the assembly boundary**. That is the clean resolution of the
inline-vs-ABI tension: keep `Option.map` out-of-line (stable ABI, no body leak,
no bloat per [package-split-plan](package-split-plan.md) PS2), and still let a
downstream assembly SROA through it and elide redundant calls. `inline` optimises
by exposing the *implementation*; purity optimises by exposing a *contract*.

## EF2 — Pick the axes; "pure" is overloaded

Different guarantees license different optimisations. Decide which `pure` asserts:

| Axis | Meaning | Licenses |
|---|---|---|
| **effect-free** | no observable mutation / IO | reorder, dedup |
| **deterministic** | result depends only on args; reads no mutable state | CSE, loop-hoist |
| **total / nothrow** | cannot throw or diverge | drop-if-unused, free reorder |

GCC already splits the first two: `__attribute__((const))` (reads nothing — full
hoist) vs. `((pure))` (may read memory — weaker). D splits the third out as a
separate `nothrow`. Vesper should treat them as **composable axes**, not one
boolean.

## EF2a — The partiality trap (`Option.get`)

`Option.get None` and `.Value` on `None` **throw**. They are effect-free and
deterministic but **partial**. If `pure` is read as licensing DCE-of-unused or
free reordering, deleting a `get` that would have thrown is an *observable*
change. Two sound choices:

1. Exceptions are a real effect → keep a separate `total`/`nothrow` axis; `get`
   is `pure` but not `total`.
2. GHC-style **imprecise exceptions** → `pure` may throw, but *which* exception
   survives optimisation is not guaranteed, so reordering/dropping stays legal.

This is the crux for `get`/`Value`. Most of the module is pure + total; those two
are pure-but-partial. (Note: .NET's old `[Pure]` from Code Contracts meant only
"no visible side effects" and never licensed DCE of throwing calls — and went
unused. Don't repeat that; see EF7.)

## EF3 — Higher-order purity is conditional

`Option.map f` is pure **iff `f` is**. So the purity of a combinator is
*parametric in the effect of its `Fun` argument*. This is the fork that decides
how big the feature is:

- **No effect polymorphism** → only the *first-order* functions (`isSome`,
  `isNone`, `get`, `count`, `flatten`, `defaultValue`, `orElse`) can be soundly
  marked pure. The higher-order ones (`map`, `bind`, `filter`, `fold`, `iter`,
  `exists`, `forall`, `defaultWith`, `orElseWith`) cannot — someone passes an
  effectful lambda — so a flat annotation quietly covers only half the module.
- **Effect-polymorphic signatures** → `map : ('T ->{e} 'U) -> 'T option ->{e} 'U option`,
  pure when `e` is pure. That is a (lightweight) effect system, Koka-style rows.

Inference (EF5) makes this tractable *without* surface syntax: the inferencer
derives "`map` is pure in its callback's effect" the same way it derives types.

## EF4 — Effect-polymorphic example (illustrative)

```
// derived, not written:
val map    : ('T ->{e} 'U) -> 'T option ->{e} 'U option      // pure iff callback is
val isSome : 'T option ->{pure} bool                          // unconditionally pure
val get    : 'T option ->{pure, may-throw} 'T                 // pure but partial (EF2a)
```

The point of EF5 is that none of these annotations appear in source; they are
*inferred* and *recorded on the signature* (EF6).

## EF5 — Inference is the standard (the decision)

Because Vesper is immutable-by-default and type-inferred, **purity (and the
effect-polymorphism of EF3) should be inferred for every body the compiler can
see**, exactly like types. Ordinary code — the whole `Option` / `Result` / `List`
surface — carries *no* purity annotations and gets the full optimisation benefit.
Writing `pure` by hand on `Option.map` would be redundant at best and a
maintenance lie at worst.

This mirrors the project's existing stance elsewhere: derive properties, don't
make the user restate them.

## EF6 — Inference must be *persisted onto signatures*

Inference happens at the definition site, but the payoff (EF1: optimise across an
out-of-line, cross-assembly call) requires consumers in *other* assemblies to
know the result *without the body*. So the inferred effect must be **serialised
as an effect summary on the public signature** — emitted into metadata and
reflected in the `.fsi` contract the front-end already consumes. Concretely: the
same `.fsi` that is `Vesper.Option`'s symbol contract gains an inferred-effect
annotation per `val`, so a separately-shipped `Vesper.Option.dll` (PS2) hands its
optimisation contract across the DLL boundary. Inferred at the producer;
*consumed* at every cross-assembly call site.

## EF7 — Where a hand-written annotation survives (if anywhere)

The inferencer fails on exactly one class of body: the ones it cannot read.

- **Inline-IL intrinsics** — `ops-platform.fsi`'s `(+)` / bitwise / comparison
  operators lower to per-target `(# … #)` CIL. The inferencer cannot see effects
  through raw IL, so purity there is a **trusted axiom** the author asserts.
- **SRTP-resolved members** that bottom out in such intrinsics.

These are precisely the cases that *also* keep F#'s `inline` (a `let inline (+)`
body needs inline IL / `--compiling-fslib`), which is why this is where a `pure`
annotation, if Vesper has one at all, earns its keep. Two consequences:

1. A trusted `pure` is **unchecked** — wrong = miscompile. It deserves the same
   scrutiny as the extern/intrinsic conformance sets (`Conformance.fs`, P4):
   if an intrinsic is asserted `pure`, that assertion is part of the contract and
   should be conformance-checked, not just believed.
2. It may not need to be a surface keyword at all — an internal attribute on the
   intrinsic registry would do, leaving the *language* with no user-facing `pure`.
   "If used at all" (the title's question) resolves to: **only on intrinsics,
   possibly only internally.**

## EF8 — Division of labour with the type system (the Rust warning)

Rust *had* a `pure` keyword and removed it before 1.0: it decided immutability +
ownership in the *type* system gave the optimiser its guarantees without a
function annotation it couldn't enforce well. Directly relevant here:

- The **value-level** guarantees (`readonly struct`, no identity — see
  [brainstorm-option-representation](brainstorm-option-representation.md) OR2)
  already unlock SROA on *non-escaping locals*. That needs no purity.
- **Inferred purity** adds what the type system can't express on its own:
  *inter-procedural* and *inter-assembly* reasoning (CSE/DCE/hoist across an
  out-of-line call; the cross-DLL summary of EF6).

Scope each so neither is redundant: ownership/immutability handle local
representation; inferred purity handles the cross-call story. Avoid the failure
mode where `pure` becomes decorative (.NET `[Pure]`) — if the backend doesn't
consume it, don't ship it.

## Open questions

- **EF-Q1 — Exception model.** EF2a's fork (effects-with-`total` vs. imprecise
  exceptions) is a whole-language decision, not an Option-local one. It governs
  every partial function and interacts with `match` exhaustiveness.
- **EF-Q2 — How rich an effect lattice.** Just pure/impure, or rows that track
  IO / state / divergence separately? Cheaper inference vs. more optimisation
  headroom. Start with pure-vs-impure + a `total` bit; widen only if a consumer
  needs it.
- **EF-Q3 — Effect-summary format in the contract.** What does an inferred effect
  look like serialised in the `.fsi` / metadata (EF6), and does the front-end's
  symbol extractor (`ExtractCtx`) round-trip it? Needs the same care as the type
  shapes it already carries.
- **EF-Q4 — Conformance of trusted intrinsic purity** (EF7.1) — extend the P4
  sig/impl check so an intrinsic asserted pure is verified against its `(# … #)`,
  not merely trusted.

## Cross-references

- [brainstorm-option-representation](brainstorm-option-representation.md) — OR7's `Fun::Invoke` copy/SROA tension is resolved by EF1/EF6 (optimise across the out-of-line combinator).
- [package-split-plan](package-split-plan.md) — PS2 one-DLL-per-package is why EF6 (cross-assembly effect summary) is load-bearing rather than a nicety.
- [function-representation-plan](function-representation-plan.md) — `Fun` and closure devirtualisation; the other half of erasing the combinator-call cost.
- [minimal-core-lib-plan](minimal-core-lib-plan.md) — `ops-platform` inline-IL operators (`(+)`), the EF7 boundary where a trusted annotation survives.
- `Conformance.fs` / [self-host conformance (P4)](self-host-rung2-plan.md) — the model EF7.1/EF-Q4 extend to check trusted intrinsic purity.
