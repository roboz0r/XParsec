# Brainstorm — Option (and immutable struct DU) representation

Captures a design discussion behind the `Vesper.Option` package
([`../../Vesper.Option/`](../../Vesper.Option/)). The headline decision —
`Option<'T>` is a **struct** whose `None` is the **zero-initialized value** — is
already implemented in `option.fsi` / `option.fs`; this doc records *why*, and the
optimisation strategy for the one real cost (by-value copies of a large
payload). It generalises to `Result` and every immutable struct DU, so read it
alongside the struct-union layout entry of [du-architecture](du-architecture.md)
(the physical layout: `_tag` plus a `Payload` struct whose unmanaged fields
overlay at offset 0) and [brainstorm-effects](brainstorm-effects.md) (the
cross-call reasoning that makes the copies disappear).

## OR1 — Struct option; `None` is the zero-initialized struct

Vesper does **not** inherit F#'s reference-typed, `UseNullAsTrueValue`
(`None = null`) option. `Option<'T>` is a value type:

- `default(Option<'T>)` is `None`; an array of options starts as all-`None`; an
  uninitialised field is `None`. The "zero-init = `None`" property — the one
  genuinely useful thing the null representation bought — is **recovered without
  a reference type and without allocation**.
- Neither `None` nor `Some` allocates on the heap.

This is the Rust `Option` / Swift `Optional` shape. F#'s reason for a *separate*
struct option (`ValueOption`) — that its default option is a reference type —
does not apply, which is why `ValueOption` is redundant in Vesper.

## OR2 — The enabling invariant: representation freedom

Every optimisation below is legal for one reason: a **`readonly struct`** option
is **immutable and has no identity**. There is no mutation to observe and no
`ReferenceEquals` to preserve, so the compiler may copy, alias, dedupe, or
re-lay-out option values at will. The *only* obligation is that observable
semantics are invariant across whatever representation it picks:

1. `default` is `None`,
2. structural equality / hashing (per [brainstorm-structural-equality](brainstorm-structural-equality.md)),
3. **identity is never observable** — Vesper must forbid taking the reference
   identity of, or locking on, an option value.

Hold (3) and a reference-class representation, a scalar-replaced representation,
and a niche-encoded representation are all interchangeable. Lose it and most of
the toolbox becomes illegal.

> **Precondition (CLR).** The option must be a `readonly struct`
> (`[<IsReadOnly>]`) with readonly members. Otherwise `in`/`inref` parameters
> trigger *defensive copies* on member access, silently defeating the by-ref
> optimisations below.

## OR3 — The one cost, and the goal

A struct `Option<'T>` is `sizeof('T) + tag (+ padding)`. For small `'T`
(`Option<int>`, `Option<ref>`) it is free; for `Option<BigStruct>` it is copied
by value on assignment, argument passing, return, and match-binding.

The goal is **not** to push users toward `Option<Box<BigStruct>>`. The moment a
user must box to make options viable, the representation has leaked. `Box` is for
when you genuinely want a *shared heap payload*; it must never be the remedy for
"options of big things are slow." Target: make `Option<BigStruct>` compile to
roughly what a hand-tuner would write.

## OR4 — The seam that organises everything: public ABI vs. private

| Scope | Rule |
|---|---|
| **Public, cross-assembly signature** | Frozen, *deterministic* ABI. The other side must agree without seeing the body — so a fixed size threshold ("structs over N pointer-widths pass by hidden `inref`, return via a hidden return-slot pointer"), the C/Rust/Swift large-value ABI. A type's size crossing the threshold is a breaking change, so the rule must be stable, not adaptive. |
| **Private body / whole-program / closed instantiation** | Unrestricted. The compiler owes nothing to anyone and may pick *any* representation per use. |

This is the boundary between "must be predictable" and "optimise freely."

## OR5 — The toolbox, tiered by cost

**Tier 1 — keep the struct, kill the copies (transparent, free):**

- **Scalar replacement (SROA)** — the dominant case. `match tryFind k with Some x -> … | None -> …`
  never needs an `Option` to materialise: the tag lives in one local, the payload
  in another (or reuses the producer's slot). Construction sets the tag; match
  branches on it. Zero copy, zero alloc — the option is a compiler fiction.
  Inlining the producer into the consumer makes this fire almost everywhere,
  because option lifetimes are tiny.
- **Hidden `inref` args + return-slot (sret) returns** for large options. Args
  become pointer-sized; returns are constructed in place in the caller's slot
  (RVO). The "copy" degrades to at most one spill-to-stack when the value is not
  already addressable.

**Tier 2 — layout tricks (transparent):**

- **Niche encoding.** For `Option<&T>` / `Option<string>`, encode `None` as the
  null bit-pattern: no separate tag, pointer-sized, and `default = None` still
  holds (null *is* the zero pattern). The old null-as-`None` layout is thus
  *derived as an optimisation* where it's a win, rather than mandated everywhere.
  Same for `Option<NonZero>`-style niches. Uniform across shared reference-type
  generic instantiations, so it composes with .NET's shared generics.

**Tier 3 — swap to a reference internally (a judgment call, NOT free):**

- For a *private* `Option<BigStruct>` instantiation that escapes, lower it to a
  reference class for that instantiation only: one allocation + an indirection,
  but cheaper than copying a large payload through many frames. Size-threshold
  heuristic; ABI-invisible because instantiation-private. Note the generics
  interaction: .NET monomorphises value-type generic args, so `Option<BigStruct>`
  gets its own code and is free to choose; reference instantiations share code and
  must pick a uniform representation (the null niche, which is uniform — fine).
- **Copy-on-escape**: struct in locals/args (Tier 1), box only at the store into a
  heap field/array. Representation varies by storage location. Boundary
  conversions cost; worth it only when the stored option is read rarely relative
  to its size.

**Tier 4 — user-visible escape hatches:**

- A representation attribute (`[<Value>]` / `[<Reference>]`) on a hot type, and
  `Box` for genuine payload sharing — explicitly *not* the default remedy for OR3.

## OR6 — Honest ledger

| Technique | Cost |
|---|---|
| SROA on non-escaping options | free / better-than-free |
| Niche encoding | free |
| `readonly struct` discipline | free (an attribute + discipline) |
| Hidden `inref` / sret for large values | free at the ABI (one spill at worst) |
| Internal reference lowering | trade: copy-elision for one alloc + indirection |
| Copy-on-escape boxing | trade: conversion cost at boundaries |

## OR7 — The `Fun::Invoke` tension (the open issue)

The module leg currently plans **non-inline combinators dispatched through
`Fun::Invoke`** (`option.fs`: "the backend lowers each application to `callvirt
Fun::Invoke`"). For small `'T` that's fine. For `Option<BigStruct>`,
`Option.map f (Some big)` copies the payload *across the interface-call boundary*
— into the `Invoke` frame and back — and the indirection **blocks the SROA /
inlining** that would otherwise erase the copy. So large-struct options and the
non-inline-`Fun` decision are in mild conflict: the combinators are exactly where
naive struct copies multiply.

Two non-exclusive fixes, both already on the roadmap:

1. Let the backend optimise *across* the combinator call without inlining it —
   this is what an inferred-purity / effect summary buys (see
   [brainstorm-effects](brainstorm-effects.md) EF1/EF6): keep `map`/`bind`
   out-of-line for ABI stability, yet SROA through them.
2. Devirtualise / specialise the `Fun` callback so the payload stays in one slot —
   the closure-devirtualisation endgame of
   [function-representation-plan](function-representation-plan.md).

So the `Option<BigStruct>` story is not independent work; it builds on the
devirtualisation + purity reasoning rather than needing bespoke option support.

## OR8 — Generalisation

Nothing here is option-specific. `Result`, and any immutable struct DU emitted in
the split-payload layout ([du-architecture](du-architecture.md), "struct-union
split-payload layout"), get the same treatment: SROA on locals from the value-level guarantees (immutable +
no identity), cross-call elision from inferred purity. Option is just the first
and smallest instance.

## Open questions

- **OR-Q1 — Size threshold.** What `N` (in pointer-widths) flips the public ABI
  to hidden-`inref`/sret, and is it the same threshold the private
  reference-lowering heuristic uses? Pin it once, document it as ABI.
- **OR-Q2 — Niche discovery.** How far does niche encoding go beyond null and
  `NonZero` — user-declarable niches on value types? Interacts with the
  split-payload layout (a spare tag bit in the unmanaged block).
- **OR-Q3 — Where SROA must be guaranteed vs. best-effort.** Users will rely on
  `match`-immediately being allocation-free; is that a *guarantee* (spec'd, like
  RVO in C++17) or an optimisation? A guarantee constrains the backend but is
  what makes struct options trustworthy for hot code.

## Cross-references

- [`../../Vesper.Option/option.fsi`](../../Vesper.Option/option.fsi) — the implemented contract; OR1/OR2 decisions in situ.
- [core-lib-architecture](core-lib-architecture.md) — one impl DLL per package; why the data types are structs, and why the public-ABI seam (OR4) and cross-assembly effect summaries matter.
- [du-architecture](du-architecture.md) — the landed split-payload physical layout this representation sits on top of (the "struct-union split-payload layout" entry).
- [brainstorm-structural-equality](brainstorm-structural-equality.md) — the equality/hashing half of OR2's observable-semantics invariant.
- [brainstorm-effects](brainstorm-effects.md) — inferred purity as the cross-call summary that resolves OR7 without inlining.
- [function-representation-plan](function-representation-plan.md) — `Fun` and the closure-devirtualisation endgame (OR7).
