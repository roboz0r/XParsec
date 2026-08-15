# Exception type identity on JS

**Status (2026-07-11): live plan.** Spun out of `operator-clause-parity-plan.md` (deleted on
landing) — that plan named this as the next tenant of the backend conformance corpus, once the
corpus existed. It now does (`test/Codegen.Conformance/`), so this is a program-plus-golden away
from being a red test rather than an argument. Delete when it lands (`feedback_plan_docs_ephemeral`).

## The gap

Every type in the `Vesper.Core/exceptions.js.fsi` roster erases to the `exn` root (`Error`) on JS —
its `sig-only` entry says so outright. So a type-based catch cannot tell `InvalidOperationException`
from any
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

## Do the `.fsi` hierarchy now; the body is blocked (2026-08-15)

From the `sig-only` discussion — see [retire-sig-only-plan](retire-sig-only-plan.md).

- **The roster is not really an exemption.** `JsExternalMembers.exnReprOf`
  (`JsExternalMembers.fs:106-139`) walks `FrozenBaseType` to the first type carrying an
  intrinsic repr, so `new FormatException("x")` already emits `new Error("x")`
  (`EmitJs.fs:217-232`). The roster HAS a target representation; conformance's check is
  syntactic and one level deep and cannot see it. `sig-only` here is a conformance gap.
- **Fix the declared hierarchy now — it is free.** The climb is depth-capped at 16, so a
  BCL-shaped chain (`ArgumentNullException : ArgumentException : exn`) still lands on `Error`
  and today's output is byte-identical. The decision gets recorded and checked while it costs
  nothing; the eventual lowering becomes transcription rather than design.
- **The body is blocked twice.** `EmitJsTypes.fs:313-316` hard-fails on ANY `inherit` clause
  on the JS target, so a BCL-shaped `exceptions.js.fs` is unemittable until class inheritance
  exists — and `TryWith` still has no emit arm on either backend, so nothing can observe the
  difference regardless.
- **Only three of the six are exercised.** `ExceptionTests.fs` covers
  `InvalidOperationException`, `ArgumentException` and `NotSupportedException`;
  `ArgumentNullException`, `IndexOutOfRangeException` and `FormatException` are referenced
  nowhere else in the repo. `ExceptionTests.fs:103-109` pins the erasure as INTENDED
  (`Expect.isFalse (src.Contains "ArgumentException")`), so that assertion is what the
  nominal lowering has to change.
- Note also that every constructor argument past the first is silently dropped
  (`EmitJs.fs:225-230`), which matters for `ArgumentNullException(paramName, message)`.

## What it will NOT fix

Identity is restored only for exceptions *we* throw. A host-originated `TypeError` has no Vesper
identity, so `:? NullReferenceException` stays a CLR-only proposition unless the catch lowering also
translates host errors — which is probably not worth doing.

## The structural point: the roster is one package too far downstream

`prim-types-exn` defines `exn` inside `Vesper.Core`, and it is *core operations* — integer division,
array indexing, invalid casts — that need to throw BCL-named exceptions. This used to be a hard
blocker: the roster was its own package declaring `depends-on = ["Vesper.Core"]`, so **core was
structurally forbidden from naming the very exceptions it must raise.**

**RESOLVED.** The roster is now `Vesper.Core/exceptions.js.fsi`, so core names its own exception
roots and the cycle is gone. What remains below is the identity question alone.

This already bit: the integer division-by-zero guard (`checkedDivisor`, in `Vesper.Core.mjs`) throws
a bare `exn` carrying the BCL's message ("Attempted to divide by zero.") rather than a
`DivideByZeroException`, because it *cannot* name that type. Today that is faithful — every JS
exception erases anyway, so a bare `exn` is exactly as catchable as anything else. **The moment
catches become nominal, it becomes observably wrong**, and the corpus's `arith-div-by-zero` row will
say so.

A roster core cannot reach is a roster in the wrong place. If the nominal lowering happens, fold the
exception roots into `Vesper.Core` alongside `exn` rather than keeping a separate package that core
is forbidden to use.
