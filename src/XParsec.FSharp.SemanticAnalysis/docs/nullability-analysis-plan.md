# Flow-sensitive nullability analysis — guard narrowing on `T | null`

**Status (2026-07-08): deferred, not started.** Spun out of the contract-sourced intrinsic
identity work, whose nullability stage landed in two tiers before this one (`objnull` is the
ordinary `obj | null` union end to end; `null` has one cross-backend identity
`RuntimeNames.nullKey`; CLR reference-null erasure applies at the three ABI seams). Those
tiers make `T | null` a faithful, representable, codegen-safe union — but nothing yet
*removes* the `null` member along a branch that has proven the value non-null. This plan is
that analysis. It is a standalone feature: those tiers do not need it, and it needs nothing
from them beyond the representation they landed. Delete this doc once it lands
(or is abandoned) per `feedback_plan_docs_ephemeral`.

## The gap

Today a value of type `T | null` keeps that type on every use, even inside a branch that
has just tested it:

```fsharp
let f (x: string | null) =
    if x <> null then
        x.Length      // x is still `string | null` here — no narrowing
    else
        0
```

`InferControlFlow.inferIfThenElse` (`Passes/Unification/InferControlFlow.fs:258`) types the
condition, then types each branch against the *unchanged* binding — it never consults the
guard to refine what a branch sees. The only narrowing that exists is
`computeArmNarrowing` (`InferControlFlow.fs:196`), which shrinks a **closed union
scrutinee** across `match` arm *patterns* (`:? T` type-tests). It is a useful template but
explicitly "Pure over the arm *patterns* — it reads nothing from body typing" and is
scoped to `match`, not to `if`/`&&`/`||` guard flow. There is no flow environment threaded
through ordinary control flow.

## What "narrowing" means here

A guard `x <> null` (CLR) / `x != null` / `x !== undefined` (JS) proves, on its true
branch, that `x` does not inhabit the `null` (resp. `undefined`) member of its union. The
then-branch should observe `x : T` — the union with that absence-sentinel member dropped —
and the else-branch should observe the complementary refinement (`x : null` when the union
was exactly `T | null`). The member-removal operation already exists in two shapes:

- `mkUnion` / `SemType.MkUnion` (`SemTypeOps.fs`, `SemanticInfo.fs`) — the smart ctor that
  flattens/dedups and **collapses a singleton** (`obj | null` minus `null` ⇒ `obj`).
- `UnificationEngineCore.stripReferenceNull` (`Passes/Unification/EngineCore.fs:40`) — the
  Tier-B erasure that drops the `null` member throughout a type. That is the *unconditional*
  ABI-seam erasure; guard narrowing is the *flow-conditional* analogue, dropping the member
  only along the proven branch. The `(|TyNull|_|)` / `(|FTNull|_|)` recognizers
  (`RuntimeNames.fs`, `IntrinsicTypePatterns`) landed with Tier B are the identity check both
  reach for — reuse them, do not re-spell `TyConst(nullKey, Block.empty)`.

So the new work is **not** the set algebra (it exists) — it is the **flow environment**: a
per-branch overlay that says "for the extent of this branch, binding `k` is observed as the
narrowed type `t'` instead of its declared type."

## The design question to settle first (before coding)

**Where does the narrowed type live, and how does a use site see it?** A bound identifier
resolves its type through `inferIdentDefault` (`Passes/Unification/InferIdentExpr.fs:130`):
`ctx.Bindings.Binding.TryGetValue key |> instantiateBinding`. The narrowing must interpose
between "which binding does this ident name" and "what type do we report for it." Two
candidate shapes (decide with the user — `feedback_design_discuss_not_multiplechoice`):

1. **A flow environment threaded as an explicit parameter** through the `infer` recursion
   (`Infer.fs:47`) — an immutable `Map<binding-identity, SemType>` overlay consulted in
   `inferIdentDefault` before falling through to the declared binding. Pro: purely
   functional, no mutation to unwind, matches the `computeArmNarrowing` "pure pre-pass"
   grain. Con: `infer` currently takes `(ctx, e)` only — every arm would thread the env, a
   wide signature change (or the env is stored on `ctx` as a mutable scoped field, see 2).

2. **A scoped mutable overlay on `PassContext`** (push on entering a narrowed branch, pop
   on exit) consulted at the same seam. Pro: no signature churn; localizes to the branch
   constructs that push/pop. Con: mutable scoping is easy to leak — needs a `try/finally`
   or a `use`-scoped guard so an exception mid-branch cannot strand a narrowing.

The binding-identity key is the open question feeding both: narrowing keys on the *binding*
(the `let`/param that `x` refers to), not the use-site `NodeKey` — so the env must key on
whatever identity `ctx.Bindings.Binding` resolves a use to (a `ResolvedBinding` / its
declaration `NodeKey`). Confirm that identity is stable and hashable before building on it.

## The work, in order

1. **Guard recognizer.** A pure function `tryNullGuard : PassContext -> Expr -> (binding
   * Polarity) voption` that recognizes the narrowing-bearing condition shapes and returns
   *which binding* is proven non-null and *on which branch* (true/false polarity):
   - `x <> null` / `x = null` (CLR), `x != null` / `x == null`, `x !== undefined` /
     `x === undefined` (JS) — an `InfixApp` (`Infer.fs:58`) whose one operand is a bound
     ident and the other is `Expr.Null` (`Infer.fs:101`) or the `undefined` literal.
   - `not (…)` inverts polarity; `isNull x` / `not (isNull x)` (the `Vesper.Core`
     `ops-platform` operator) is the same proof through a call, worth handling since it is
     the idiomatic form.
   Keep it syntactic and total-returns-`ValueNone` — anything unrecognized simply yields no
   narrowing (the current behavior), so this is strictly additive.

2. **Branch application in `inferIfThenElse`.** With a recognized guard, type the
   then-branch under the env overlaid with the non-null narrowing (member dropped via
   `mkUnion (members \ null)`), and the else-branch under the complementary overlay. Restore
   on exit. The elif chain (`InferControlFlow.fs:272`) accumulates: each `elif` sees the
   *negation* of all prior guards. Start with the two-branch `if` and add elif/else
   composition once the two-branch case is solid.

3. **Short-circuit operators `&&` / `||`.** `a && b` narrows `b`'s typing by `a`'s
   true-refinement (a guard in `a` holds while `b` is evaluated); `a || b` narrows `b` by
   `a`'s false-refinement. These are `InfixApp` nodes — the same env-push, scoped to the RHS
   subexpression. This is what makes `if x <> null && x.Length > 0` work.

4. **Early-return / assertion flow (optional, larger).** `if x = null then failwith …`
   followed by straight-line code that then sees `x : T`. This needs the env to persist
   *past* the `if` into the following statements of a `Sequential` (`Infer.fs:72`), gated on
   the then-branch being divergent (never-returning). Genuinely new flow reasoning
   (divergence analysis); separable — land 1–3 first and scope 4 on its own.

5. **JS `undefined` parity.** The `undefined` intrinsic (`prim-types-undefined.js.fsi`,
   `IntrinsicSet.Undefined`) is the JS optional-absence sentinel and unions the same way
   (`T | undefined`). The guard recognizer and member-drop are identical modulo which
   sentinel member is removed — parameterize the analysis over the sentinel key
   (`nullKey` | `undefinedKey`) rather than hardcoding `null`, so both fall out of one path.

## Verification

- `if x <> null then x.Length else 0` on `(x: string | null)` type-checks — the then-branch
  observes `string`, so `.Length` resolves (today it would see `string | null` and the
  member access would be against the union).
- The else-branch sees the complement: `match`/use in the else that assumes non-null is
  rejected.
- `if x <> null && x.Length > 0` narrows across `&&` (RHS sees the refinement).
- Negation: `if not (isNull x) then x.Length` narrows identically.
- JS `if x !== undefined then …` on `(x: T | undefined)` narrows to `T` (same test under the
  JS target).
- No regression to `computeArmNarrowing`: `match` narrowing is untouched; the two analyses
  are independent (pattern-narrowing vs guard-narrowing) and must not be conflated into one
  path unless a later cleanup proves they share a core.
- Non-guard conditions (`if b then …` for a plain `bool`) produce **no** narrowing and no
  overhead — the recognizer returns `ValueNone` and the env is untouched.

## Relevant memories

`feedback_design_discuss_not_multiplechoice` (settle the flow-env shape by sketching with
the user before coding — question in the "design question" section above),
`feedback_systematic_tests_over_whackamole` (build the guard/branch matrix as systematic
isolation tests, not one-off fixes),
`feedback_dynamic_intrinsics_over_du_cases` (narrow by dropping a union *member* via the
existing `mkUnion` / `TyNull` machinery — do not add a nullability axis or new SemType
case), `feedback_freeze_no_backend_knowledge` (the `null` vs `undefined` sentinel choice is
a front-end union-member fact; keep the analysis target-neutral and parameterized over the
sentinel key, not branched per backend).
