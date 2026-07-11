# Exception type identity on JS

**Status (2026-07-11): live plan.** Spun out of `operator-clause-parity-plan.md` (deleted on
landing) — that plan named this as the next tenant of the backend conformance corpus, once the
corpus existed. It now does (`test/Codegen.Conformance/`), so this is a program-plus-golden away
from being a red test rather than an argument. Delete when it lands (`feedback_plan_docs_ephemeral`).

## The gap

Every type in the `Vesper.Exceptions` roster erases to the `exn` root (`Error`) on JS — the package
header says so outright. So a type-based catch cannot tell `InvalidOperationException` from any
other error there, while on the CLR it can. Two backends, two different answers to the same
program: exactly what the conformance corpus exists to make visible.

## The prerequisite, which is bigger than the fix

**`try … with` is unemittable on BOTH backends today.** `TExprG.TryWith` parses, infers and freezes,
but neither `Codegen.Clr/EmitExpr.fs` nor `Codegen.Js/EmitJs.fs` has an emit arm — it falls through
to `failwithf "unsupported expression"`. There are no typed catches either; the whole surface is
unimplemented. (The only catching test in the JS suite reaches for a raw JS `try/catch` template to
get the behaviour, which is the tell.)

So none of what follows is *observable* until `TryWith` lowers on both targets. That is the first
piece of work, and it is not small. Until then the erasure costs nothing, because nothing can catch.

## The fix, once catching exists

Lower each roster entry to a real `class … extends Error`, plausibly in a `Vesper.Exceptions.mjs`
runtime asset declared via `runtime-js` — mirroring how `Vesper.Core.mjs` already works, and keeping
the shim declarative rather than teaching the backend to synthesise class declarations from a
`sig-only` contract. The prototype chain then gives subtype matching for free, and the CLR side does
not move at all (that package is not referenced by CLR builds).

Two things make it more than mechanical:

- **The declared hierarchy becomes observable.** `exceptions.fsi` currently has every type inheriting
  `exn` *directly* — a flat roster, harmless only because it is never consulted. The BCL's is not
  flat: `ArgumentNullException` derives from `ArgumentException`. So on the CLR
  `with :? ArgumentException` catches a thrown `ArgumentNullException`, and a flat JS roster would
  not. **The corpus program that pins this is the deliverable; the lowering is merely the fix.**
- **`instanceof` is realm-fragile** (iframes, workers, `vm` contexts). This codebase already has the
  better answer: `Vesper.Core.mjs` brands union prototypes with a non-enumerable `$type` and uses
  `Symbol.for("vesper.equality")` as a cross-realm-safe registry key. A `Symbol.for` brand check
  beats `instanceof` and is precedented here.

## What it will NOT fix

Identity is restored only for exceptions *we* throw. A host-originated `TypeError` has no Vesper
identity, so `:? NullReferenceException` stays a CLR-only proposition unless the catch lowering also
translates host errors — which is probably not worth doing.

## The structural point: the roster is one package too far downstream

`prim-types-exn` defines `exn` inside `Vesper.Core`, and it is *core operations* — integer division,
array indexing, invalid casts — that need to throw BCL-named exceptions. Yet `Vesper.Exceptions`
declares `depends-on = ["Vesper.Core"]`, so **core is structurally forbidden from naming the very
exceptions it must raise.**

This already bit: the integer division-by-zero guard (`checkedDivisor`, in `Vesper.Core.mjs`) throws
a bare `exn` carrying the BCL's message ("Attempted to divide by zero.") rather than a
`DivideByZeroException`, because it *cannot* name that type. Today that is faithful — every JS
exception erases anyway, so a bare `exn` is exactly as catchable as anything else. **The moment
catches become nominal, it becomes observably wrong**, and the corpus's `arith-div-by-zero` row will
say so.

A roster core cannot reach is a roster in the wrong place. If the nominal lowering happens, fold the
exception roots into `Vesper.Core` alongside `exn` rather than keeping a separate package that core
is forbidden to use.
