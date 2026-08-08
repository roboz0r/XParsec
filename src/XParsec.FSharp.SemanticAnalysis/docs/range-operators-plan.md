# Range operators — `(..)` / `(.. ..)` as real seq operators

**Status (2026-07-08): deferred, not started.** Spun out of Stage 5b of
`contract-sourced-intrinsic-identity-plan.md`. Stage 5b took **Route 2** (the small,
honest move): it deleted the `tySeqInt` placeholder and made a range legal ONLY as the
source of a `for … in` loop that lowers to a counted `ForTo`; every other range use
(bound to a value, passed as an argument, stepped `a..s..b`) is rejected with a
front-end diagnostic. This plan is **Route 1** — the F#-faithful end state that makes a
range a first-class `seq<int>` value. Delete this doc once it lands (or is abandoned)
per `feedback_plan_docs_ephemeral`.

## The premise (confirmed 2026-07-08)

In F#, `..` is not syntax — it is an ordinary `inline` operator returning a materialised
`seq<'T>`. `src/XParsec.FSharp.Lib/Clr/prim-types.fs:7052` (and the `.fsi` at `:3905`):

```fsharp
val inline (..): start: ^T -> finish: ^T -> seq< ^T >
                    when ^T: (static member (+): ^T * ^T -> ^T)
                    and ^T: (static member One: ^T)
                    and ^T: equality
```

The impl dispatches per-type to `RangeInt32`/`RangeDouble`/`RangeGeneric`/… (`:7053`),
each of which is a real library function building an `IEnumerator` (`RangeInt32 … :
seq<int>`, `prim-types.fs:6176`; the enumerator machinery `integralRange` /
`integralRangeStep` / `integralRangeStepEnumerator` lives at `:5947`–`:6189`). The
counted `for` loop is a **separate optimiser peephole** layered on top — it is NOT the
semantics of `..`.

## Why the mechanism already exists

The Vesper-Core arithmetic operators (`src/Vesper.Core/ops-platform.fsi` / `.fs`) are
already exactly this shape: a `.fsi` SRTP signature + a `.fs` multi-branch
`when ^T : int32 = (# "add" … #)` inline body, shipped across the package boundary by
`SymbolProviders.inlineBodies`, spliced at each call site by `InlineExpansion`, and
lowered by codegen. So `(..)` can follow the identical road — the multi-branch inline
SRTP operator is a **supported, working construct** (parser coverage:
`test/XParsec.FSharp.Tests/data/321_static_optimization_multi_constraint.fs`).

The difference from `(+)`: `(+)`'s branches bottom out in a single IL opcode, whereas
`(..)`'s branches bottom out in `RangeInt32`/`RangeGeneric`, which are **real functions
returning a materialised `seq`**. That enumerator implementation is the substantial part
of this work — the operator declaration is the small part. Route 1's centre of gravity
is the seq-range library, not the operator.

## The work, in order

1. **Range-enumerator seq implementation.** Port / author the `RangeInt32` family + the
   `integralRange` / `integralRangeStep` enumerator classes so a range materialises a
   real `IEnumerable<'T>` that both backends can emit and walk. This is the gate — until
   it exists and codegens, a range-as-value can type-check but cannot run, which is
   exactly the downstream-failure trap Route 2 was chosen to avoid. Do NOT land the
   front-end half without this.
2. **Declare `(..)` / `(.. ..)` as Vesper-Core inline operators.** Mirror the
   `ArithmeticOperators` shape in `ops-platform.fsi`/`.fs` (or a dedicated
   `range-operators` contract): SRTP signature `^T -> ^T -> seq<^T>`, multi-branch impl
   dispatching to the per-type range builders. The operator names are `op_Range` /
   `op_RangeStep` (confirm against `OperatorNames`).
3. **Desugar `Expr.Range` → operator application.** In `Passes/Desugar.fs`, rewrite
   `Expr.Range(a,b)` → `App(App((..), a), b)` and `Expr.SteppedRange(a,s,b)` →
   `App(App(App((.. ..), a), s), b)`. Then `1..10` types as `seq<int>` through ordinary
   operator resolution — no `inferRange`, no `TExpr.Range`, no placeholder.
4. **Delete the Route 2 residue.** Remove `inferRange` (`InferApp.fs`), the `isRangeSource`
   branch of `inferForIn` (`InferControlFlow.fs`), the `Expr.Range`/`Expr.SteppedRange`
   arms of `Freeze/Elaborate.translateExpr`, the unsupported-range diagnostic, and the
   `TExpr.Range` case in `Tast.fs` (+ its `TastLower`/`TastConvert`/`TastWalk` arms). The
   Route 2 `CoverageTests` (range-as-value rejected / stepped rejected) invert back to
   "types as `seq<int>`" / "range-as-value runs".
5. **Re-add the counted-loop peephole.** Without it, `for i in 1..10` would inline-expand
   `(..)`, allocate a seq, and walk it via the enumerator — a regression from the counted
   loop. Add a lowering that recognises a `for x in (App (..) a b)` source (the desugared
   form) and emits `TExpr.ForTo`, exactly the syntactic detection `Freeze.translateForIn`
   does today, moved off `Expr.Range` onto the operator-application form. Keep the
   pattern guard (simple boundVar) so a non-simple pattern falls back to the real
   enumerator walk (now that a real seq exists).

## Verification

- `for i in 1..10` still lowers to a counted loop (CLR `for-in-range-counted`,
  `StructTests`) — the peephole must fire.
- `let r = 1..10; for x in r do …` now runs end-to-end (walks the materialised seq) —
  the case Route 2 rejects.
- `Seq.sum (1..10)` / passing a range where `seq<int>` is expected now type-checks AND
  runs.
- Stepped `for i in 1..2..10` runs (walks the stepped enumerator) — no longer rejected.

## Relevant memories

`feedback_prototype_correct_semantics_over_fsharp_parity` (this is the correct-semantics
end state; the counted loop is an optimisation, not the meaning of `..`),
`feedback_fsharpcore_port_transliterate` (port `RangeInt32` & friends literally with
`file:line` refs), `feedback_dynamic_intrinsics_over_du_cases` (resolve `(..)` through the
contract, don't author a front-end special case).
