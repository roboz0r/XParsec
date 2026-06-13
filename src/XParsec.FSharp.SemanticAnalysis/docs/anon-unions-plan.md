# Anonymous (structural) union types plan — `TyOr`

**Status:** plan, not started. Backend-independent front-end work. This is the
keystone shared phase for both faithful TypeScript interop
([codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)) and
ergonomic nullability on the JS backend
([codegen-js-plan](codegen-js-plan.md)), but it stands on its own as a
type-system feature and benefits the CLR backend too.

## Why

The CST already parses arbitrary `ident1 | ident2 | ident3` type syntax (and
`T | null` / `obj | null` in the F# 9 nullness vein) — it is *parser surface*
with no semantics attached, per the repo's "relax the grammar, defer semantic
rejection" principle ([[feedback_relax_parser_defer_to_typecheck]]). This plan
gives that surface meaning as **TypeScript-style anonymous structural unions**:
`X | Y | null | undefined` is a real type, `null`/`undefined` are real members,
and values flow without a wrapper.

This is deliberately **distinct from nominal DUs**, which Vesper already has
(`type Foo = A | B`). The gap this fills is the *anonymous/structural* union —
the thing C# is only now adding in C# 15, and the thing TS is built on. Notably,
`option<'T>` stays an honest nominal DU (never erased to `null`); nullability is
expressed with anonymous unions (`T | null`) instead. The two never conflate —
which is strictly more faithful than Fable's erased-option approach (Fable
overloads JS `null` for `None` and pays for it with `Some null`/`Some None` edge
cases).

## The keystone insight: `obj` is already a degenerate anonymous union

The scary framing is "retrofit subtyping into an equality-based unifier." The
real framing is far smaller: **the checker already has a directional
assignability layer, and `obj` is already the union of everything.** This plan
*generalises the one crack that already exists* rather than perturbing the
equality core.

The symmetric equality unifier — `unify` ([`Engine.fs:699`](../Passes/Unification/Engine.fs))
— stays untouched. `unify int string` is an error today and **must remain one**.
Sitting on top of it, used at value-flow positions, is a directional layer:

- `subsumes ctx src tgt → Equal | Subtype | Unrelated`
  ([`Engine.fs:482`](../Passes/Unification/Engine.fs)) — read-only directional query.
- `tryUpcastWitness` ([`Engine.fs:440`](../Passes/Unification/Engine.fs)) — walks
  the inherit/interface chain for an upcast witness.
- `tryCoerceUpcast` / `unifyArg` ([`Engine.fs:1314`](../Passes/Unification/Engine.fs))
  — the *committing* directional path for arguments / `:>`.
- `absorbsAsObj` ([`Engine.fs:~670`](../Passes/Unification/Engine.fs)) — the rule
  that `obj` **accepts any value without unifying** (no typar pinning), the box
  materialised at codegen ([[project_obj_universal_supertype_coercion]]).

`absorbsAsObj` is the whole game. `obj` accepts by assignability, does not pin
the source typar, and erases/boxes at the boundary. `TyOr [members]` is exactly
`obj` restricted to an enumerated member set. So the design is: **generalise
`absorbsAsObj` into `acceptsByAssignability`, and teach `subsumes` about union
membership.** The symmetric core never learns about unions.

## The principality rule (non-negotiable)

> **Unions enter the type graph only at annotation sites** — user-written
> `X | Y | null`, or read from an external TS manifest
> ([codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)).
> **Inference never *synthesises* a union.** `unify int string` stays a type
> error.

Making `unify ?a int` followed by `unify ?a string` widen `?a` to `int | string`
would destroy principality, fight generalisation (Rémy levels,
[`SemanticInfo.fs:488`](../SemanticInfo.fs)), and turn every conflict into a
silent widening. So the feature is **bidirectional/checking-mode**, which the
code is already shaped for: the expected-type path (`unifyArg`) uses
assignability; the no-expected-type path (`unify`) runs pure equality as today
and never invents unions. TS does infer unions in a few positions (best-common-
type); **v1 punts on that** — annotation-driven only. Far simpler, fully sound.

## Two layers, kept separate (mirrors function-representation-plan)

### Equality layer (unchanged)

`unify` stays symmetric nominal/structural equality. Two nominals unify iff their
keys are equal and args unify positionally (invariant). No subtyping, no unions
created. The only additions are *mechanical* (recurse into a new `TyOr` case in
`resolveStep` / `zonk` / `occursAndAdjust`).

### Assignability layer (extended)

All union relations land in `subsumes` and the `acceptsByAssignability`
generalisation of `absorbsAsObj`. Three relations, each recursing through the
existing `tryUpcastWitness` walk for members:

1. **member → union** (`A` ≤ `A | B`): `subsumes src (TyOr ts)` = `Subtype` when
   `src` subsumes into some `t ∈ ts`.
2. **union → union** (`A | B` ≤ `A | B | C`, order-insensitive):
   `subsumes (TyOr ss) (TyOr ts)` = every `s ∈ ss` subsumes into some `t ∈ ts`.
3. **union → member** (`A | B` ⋠ `A`): `Equal`/`Subtype` only when *every* member
   subsumes the target (essentially target = `obj` or a wider union); otherwise
   `Unrelated` — the consumer must narrow first.

The no-pin rule generalises: `acceptsByAssignability (expected = TyOr ts)`
accepts an `actual` that subsumes into a member **without unifying the actual's
typar**, exactly as `obj` does today — so a generic value threaded through a
union-typed parameter is not wrongly fixed.

## Canonicalisation (the one genuinely fiddly bit)

`TyOr` members are an order-insensitive, deduped, flattened **set**. A smart
constructor `mkUnion` — never the raw DU case — enforces:

- flatten: `(A | B) | C ≡ A | B | C`
- dedup: `A | A ≡ A`
- collapse singleton: `TyOr [A] ≡ A`
- `never ≡ TyOr []` (bottom; subsumes into everything)

Members are stored sorted by a stable structural key so `string | int` compares,
unifies, and freezes equal to `int | string`. This is what makes union identity
work under the equality layer's `n1 = n2` discipline.

## `null` / `undefined` / `never` / `unknown`

- `never` = `TyOr []` (bottom).
- `null` / `undefined` = singleton literal types. Simplest v1: reserved
  `TyConst` names (`"null"`, `"undefined"`), members like any other.
- `unknown` ≈ `obj`-that-must-narrow — **map to `obj` in v1**, refine to a
  distinct top later. (`obj` already forbids nothing; `unknown` forbidding access
  until narrowed is a v2 refinement.)

## Narrowing — greenfield, but the hook exists

Today there is **zero narrowing**: `inferRules`
([`InferControlFlow.fs:481`](../Passes/Unification/InferControlFlow.fs)) unifies
every arm's pattern against the *same* scrutinee type, and `:? T as x`
([`InferPat.fs:329`](../Passes/Unification/InferPat.fs)) only stashes the test
type in `ctx.Resolution.TypeTestTargets` for codegen (the `isinst` operand). That
side table is the narrowing hook.

- **v1 — binder narrowing.** In a `match` on a `TyOr ts` scrutinee, a pattern
  testing for member `M` binds the arm value at `M` (narrowed); the fall-through
  residual is `TyOr (ts \ M)`. This generalises the existing `:? T as x` binding
  and covers the common case without flow-sensitive re-typing of arbitrary
  references.
- **Exhaustiveness for free.** Anonymous unions are *closed*, so matching all
  members is provably exhaustive and a missing member is a warning — unlike the
  open `obj`/inheritance case, which cannot be.
- **v2 — flow narrowing.** Full TS-style "every reference to the variable narrows
  after a `typeof`/null guard," threading a refinement environment. Deferred.

## Constraints (the inverse problem)

`SemanticConstraintKind` ([`SemanticInfo.fs:428`](../SemanticInfo.fs)) already has
a `Coercion of target` case, and `checkConstraint`
([`Engine.fs:959`](../Passes/Unification/Engine.fs)) returns
`Satisfied | Violated | Defer`, drained on `Link`. A `TyOr` satisfies a structural
constraint (`equality`, `comparison`) iff **every member** satisfies it — a
`reduceOutcome` over members, the same shape `checkConstraint` already uses for
`TyTuple`/`TyRecord` fields. This is the constraint side of the brainstorm's
"inverse problem": F# inference asks `when 'T : equality` of a type the union may
not uniformly answer; the rule is all-members-or-defer.

## Freeze

`TyOr` must survive `freeze` into a new `FTOr` case (members recurse):
`freezeTy` ([`Freeze.fs:53`](../Passes/Unification/Freeze.fs)) gains
`TyOr → FTOr`. Codegen then sees `FTOr [members]` and lowers per backend (see
below). Canonical form is preserved through freeze (members already sorted).

## Representation at codegen (informational — owned by the backends)

`TyOr` erases to each backend's existing universal-supertype primitive; **neither
backend needs a new runtime type for the MVP**:

- **JS** ([codegen-js-plan](codegen-js-plan.md)): erased. Values are bare JS
  values; `null`/`undefined` are literal. Narrowing → native `typeof` /
  `instanceof` / `=== null`. Zero runtime cost.
- **CLR**: `obj` in signatures + `isinst` discrimination on consumption — which
  the backend **already emits** (`absorbsAsObj` boxing + the `TypeTest`/`isinst`
  machinery + `TypeTestTargets`). This is precisely **C# 15's** chosen union
  representation (a single `object`-typed field + runtime cast/unbox on
  consumption). C# 15 wraps it in a named struct for nominal identity and to dodge
  value-type boxing; Vesper's anonymous unions are structural, so the MVP is bare
  `obj`+`isinst`, with the struct-wrapper available as a later value-type
  optimisation.

## The mechanical surface

`TyOr of EqArray<SemType>` in `SemType` ([`SemanticInfo.fs:288`](../SemanticInfo.fs))
and `FTOr of EqArray<FrozenType>` in `FrozenType`
([`SemanticInfo.fs:263`](../SemanticInfo.fs)), threaded through:

- `resolveStep`, `zonk`, `occursAndAdjust`
  ([`Engine.fs:127`](../Passes/Unification/Engine.fs)) — recurse into members.
- `subsumes` / `tryCoerceUpcast` / `acceptsByAssignability` — the three relations.
- `checkConstraint` — all-members-or-defer reduction.
- `mkUnion` smart constructor — canonicalisation; the raw DU case is never built
  directly.
- `translateType` — map the CST union syntax to `mkUnion` (the front door).
- `freezeTy` — `TyOr → FTOr`.
- `inferRules` / `InferPat` — binder narrowing + closed-union exhaustiveness.

## Scope

**MVP:** `TyOr`/`FTOr` + canonicalisation; assignability in
`subsumes`/`acceptsByAssignability`; annotation-driven only (no inference
synthesis); binder narrowing + closed-union exhaustiveness; constraint reduction;
`null`/`undefined`/`never`. CLR repr via existing `obj`+`isinst`.

**Deferred:** full flow narrowing (v2); union *inference* (best-common-type);
`unknown` as a distinct top; CLR struct-wrapper optimisation; literal types
(`"GET" | "POST"` → narrow nominal singletons).

## Seed milestone

One vertical slice, mirroring the CLR backend's "printfn hi" discipline: a
user-annotated `let f (x: int | string) = match x with :? int as i -> … | :? string as s -> …`
type-checks (assignability accepts both `int` and `string` calls; the match is
exhaustive; binders narrow) and round-trips through freeze to `FTOr`. No codegen
required for the front-end slice — but it is the unblock for the union rows of
both backend plans.
