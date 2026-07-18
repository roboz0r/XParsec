# Overload resolution plan

Align overload resolution with F#'s, as implemented in
`fsharp/src/Compiler/Checking/ConstraintSolver.fs` (`ResolveOverloadingCore`,
`GetMostApplicableOverload`) and `TypeRelations.fs`
(`TypeFeasiblySubsumesType`). Where this document names an fsc function,
the intent is to transliterate its rule, not to reinvent one.

Overload resolution **works today** for the *external* vocabulary:
`UnificationInferOverload.pickBestOverload` filters an `ExternalMember[]`
by arity and a structural applicability test, ranks survivors by
specificity, and commits the unique winner at the `unifyAppliedSig` seam
(`Engine.fs:487`). It is load-bearing for BCL calls, external constructors,
and `.d.ts` members.

Two gaps, and one architectural divergence from F# that explains both.

```fsharp
// Gap 1 — subtyping is invisible to the filter.
type Base() = class end
type Derived() = inherit Base()
type C() =
    member _.M(b: Base) = 1
    member _.M(o: obj) = 2

C().M(Derived())        // fsc: picks M(Base). Here: only M(obj) is applicable.

// Gap 2 — user-declared members have no overload set at all.
type Printer() =
    member this.Show(x: int) = string x
    member this.Show(x: string) = x        // silently unreachable

let p = Printer()
p.Show(1)        // picks Show(int)  — first in declaration order
p.Show("hi")     // ALSO picks Show(int) — type error, not a second candidate
```

## The divergence: a structural pre-check vs. trial unification

This is the load-bearing difference, and every other decision follows
from it.

**fsc trial-unifies each candidate and undoes.** `Trace`
(`ConstraintSolver.fs:463`) is a list of `(action, undo)` pairs; every
solver mutation goes through `OptionalTrace.Exec f undo` (`:477`), which
pushes the undo before running the action. `FilterEachThenUndo` (`:503`)
runs the full member-signature check per candidate against a fresh
`Trace`, then calls `trace.Undo()` before inspecting the result. So the
filter is *real unification*, speculatively performed and rolled back.
Only the winner is re-checked for keeps — `ConstraintSolver.fs:3676`
says it outright: **"If we've got a candidate solution: make the final
checks - no undo here!"** — either by re-running `CanMemberSigsMatchUpToCheck`
under `NoTrace`, or by replaying the winner's recorded trace
(`AddFromReplay`, `:482`).

**We do a read-only structural pre-check instead.** `applicabilityMatches`
(`InferOverload.fs:41`) never mutates, so our destructive unifier
(`UnionFind.union` + `TypeVar.Link`, no undo trace) never has to roll back.
That is why the wildcard arms exist: an open method typar or a carried
type-level node "matches anything", because we cannot afford to find out
by trying.

Notice that our wildcard arms are not an improvisation — they are
`TypeFeasiblySubsumesType`'s first rule:

```fsharp
// TypeRelations.fs:99-102
match ty1, ty2 with
| TType_measure _, TType_measure _
| TType_var _, _
| _, TType_var _ -> true
```

A type variable is feasibly anything. Our `applicabilityMatches` *is* a
`TypeFeasiblySubsumesType` missing only its class-hierarchy arm. But fsc
uses that relation for **ranking only** (`compareTypes`, `:3763`); it uses
trial unification for **filtering**. We use one structural relation for
both jobs, and it is wrong for each in a different direction.

**It over-accepts.** The method-typar wildcard is per-position and forgets
its bindings. `M<'T>('T, 'T)` called with `(1, "s")` is "applicable":
position one says `'T` matches `int`, position two says `'T` matches
`string`, and nothing remembers `'T` was already spoken for. fsc gets this
right because the trial really unifies, so `'T := int` is in effect when
position two is checked. Today the over-acceptance is deliberately deferred
to the `unifyAppliedSig` commit seam, where it surfaces as a type error
rather than as a fallback to a different candidate.

**It also under-accepts.** There is no `TyVar _, _ -> true` arm — the only
`TyVar` case is `TyVar x, TyVar y -> ReferenceEquals(find x, find y)`, so an
unresolved caller-side `TyVar` against a concrete `TyConst "int"` parameter
falls through to `| _ -> false`. Compare `TypeRelations.fs:101`. And
`InferExternalCall.fs:241` calls `pickBestOverload` on `argElemsOf argTy`
straight after `infer ctx argExpr`, with no ground-ness guard — so a call
whose argument type isn't yet pinned makes *every* concrete-parameter
candidate inapplicable and reports "no applicable overload". Codegen's
`pickBestOverloadFrozen` escapes this only because its arguments "arrive
ground (frozen)" by construction.

Two errors in opposite directions is the tell: one relation is doing two
jobs. Whatever replaces it must be *exact enough to filter* while remaining
*speculative*. Two ways to get there.

### Option 1 — trial unification into a scratch substitution (preferred)

Speculatively unify each candidate's parameters against the argument types,
accumulating bindings in a substitution local to that trial — the
candidate's own method typars *and* any caller-side metavars the call
touches. Drop it on failure; keep it for ranking on success. The winner
commits through the existing `unifyAppliedSig` seam, where real mutation has
always happened.

Speculative *and* read-only with respect to the shared union-find. There is
no undo because nothing was done. It fixes both errors above: `'T := int` is
recorded before position two is checked, and an unresolved caller `TyVar`
binds rather than failing.

Shape: generalise `subsumes` from a boolean into a bindings-accumulating
relation — `matchTypes bindings arg param` — where today's wildcard arms
become **binder** arms rather than `true` arms. Ideally real `unify` then
becomes "run `matchTypes`, apply the bindings destructively, drain": one
matching rule, two commit policies.

*Why preferred.* `architecture.md` §Parallelism already rejects persistent
unification globally — path-copying and persistent maps cost 5–10× on HM
inference. A scratch substitution doesn't contradict that: it is
persistent-in-the-small, scoped to one candidate trial and bounded by the
call's arity, not by the program's type graph. Nothing outside overload
resolution pays for it.

*The risk, and the thing to settle before writing code.* A trial must not
fire `unify`'s side effects — `drainConstraints`, the pending-dot-access
drain, region edges. It is a *query*, not a unification. So `unify`'s occurs
check, level adjustment, and constraint drain have to factor cleanly out of
its matching core. If they do, this is strictly better than a trace. If they
don't, we are maintaining two matchers that can silently drift apart —
exactly the failure mode Option 2 exists to prevent.

### Option 2 — `Trace`-based undo, as fsc does it

Thread an `OptionalTrace` through the real unifier's mutation points
(`UnionFind.union`, every `Link` assignment) so a speculative unification
can be rolled back. Transliterates `FilterEachThenUndo`.

*Why it might win.* There is exactly **one** unifier. Every rule, side
effect, occurs check, and constraint interaction is shared between the
speculative and committing paths by construction, not by discipline. That is
a real virtue and the reason fsc pays for it.

*Cost.* Every ordinary, non-speculative unification pays a branch and an
undo-closure allocation it will never run. The mutation points are spread
through `Engine.fs` / `EngineCore.fs`, so the change is invasive in exactly
the hot path `architecture.md` says to keep tight.

### Deciding

**Mild preference for Option 1**, on the strength of confining the cost to
overload resolution rather than taxing all of inference. But the choice
turns on a question this document does not answer: *can `unify`'s matching
core be separated from its occurs check, level adjustment, and constraint
drain?* That needs `Engine.unify` read end to end, plus a survey of which
drains are reachable from a parameter/argument match. **Do that analysis
when this plan is executed, not before.** If the factoring is clean, take
Option 1. If it isn't, Option 2 is the honest choice and its cost is the
price of one unifier.

Either way, fsc's single-candidate fast path (`ConstraintSolver.fs:3614`)
skips the trial machinery entirely and returns the sole candidate. That is
the overwhelming majority of call sites here, including every non-overloaded
user member — so whichever option lands, it only pays for itself where an
overload set genuinely exists.

## Status quo, precisely

**Gap 1.** `argAssignable` (`InferOverload.fs:94`) is
`applicabilityMatches argTy paramTy || isObjectTy paramTy`. `isObjectTy`'s
comment states the limit: "`object`/`obj` is the only supertype we model —
no other reference hierarchy, so a non-`object` param only matches an arg
it equals."

Meanwhile `Subsume.fs` already decides exactly this relation over user
`inherit` chains *and* BCL metadata, reconciling `exn`'s `TyConst` with
`TyClass("System.Exception", _)` through `IntrinsicReprTypes`.
`Engine.fs:692` relies on it to make a thrown `InvalidOperationException`
satisfy a `Coercion` constraint against `exn`, and its doc comment notes it
is pure precisely so it is safe to call with no undo trace. `subsumes` is
our `TypeFeasiblySubsumesType`. It is already legal to call from the
picker; it simply isn't.

**Gap 2.** Every `pickBestOverload` call site is external:
`InferExternalCall.fs:242,316`, `InferCtor.fs:155`, `Unification.fs:821`,
and codegen's `ClrExternalMembers.fs:519`. User-declared members never
reach it — they resolve by first-match-on-name:

| Site | Lookup |
|------|--------|
| `EngineCore.fs:326` | `Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic)` |
| `Engine.fs:58` | same |
| `InferRecordAccess.fs:169` | same |
| `Freeze/Resolve.fs:173` | `tryClassMember` |
| `Freeze/Resolve.fs:197` | `tryClassMemberByKey` |

`TypeMemberInfo` (`SideTables.fs:70`) lives in a flat
`Members: TypeMemberInfo[]`; `MemberRegistration.addMember` appends without
a duplicate-name check. Two same-name members both register, the first wins
everywhere, the second is silently unreachable, and nothing diagnoses.

The lookup is duplicated across **two passes**. A fix that teaches only
Unification to pick among candidates leaves Freeze resolving to a different
member than inference chose. Both must move together, and must agree by
construction rather than by implementing the same rule twice.

## Goal

- A speculative candidate check that is exact enough to filter — by scratch
  substitution (Option 1) or `Trace` undo (Option 2) — with a
  single-candidate fast path.
- Two-tier candidate filtering: exact match, then subsumption — fsc's
  `ResolveOverloadingCore`.
- Betterness ranking under the feasibly-subsumes ordering, per
  `GetMostApplicableOverload`.
- User-declared class / union members participate in the same machinery as
  external members.
- Inference and Freeze resolve to the same member without duplicating the
  rule.
- Ambiguity, no-applicable-candidate, and genuinely-duplicate members all
  diagnose.

## The algorithm, after fsc

### Tier structure (`ResolveOverloadingCore`, `:3495`)

There is no "A vs B" choice about exact-match versus subsumption. fsc does
**both, in order**, and the ordering is the rule:

1. **Single candidate** (`:3614`) — return it. No trial, no trace.
2. **Exact-match tier** (`:3509`) — `FilterEachThenUndo` with
   `ArgsEquivOrConvert` ("args exact"). If exactly one candidate survives,
   it wins immediately. This is why `Show(int)` / `Show(string)` needs no
   specificity reasoning at all.
3. **Applicable tier** (`:3532`) — `FilterEachThenUndo` with
   `ArgsMustSubsumeOrConvert` ("args can subsume"). Subtyping enters here
   and only here.
4. **Betterness** (`GetMostApplicableOverload`, `:3750`) — rank the
   applicable set; a unique maximum wins, otherwise ambiguity diagnoses.

Note that both tiers pass `TypesMustSubsume` for the *instantiation* check
("obj can subsume") regardless — the tiers differ only in the argument
check. That subtlety is worth preserving verbatim.

### Betterness (`GetMostApplicableOverload`, `:3750`)

The ordering primitive is `compareCond` (`:3759`), lifted over
feasibly-subsumes:

```fsharp
// ConstraintSolver.fs:3762-3764 — note the reversed operands.
let compareTypes ty1 ty2 =
    (ty1, ty2) ||> compareCond (fun x1 x2 ->
        TypeFeasiblySubsumesType ndeep g amap m x2 CanCoerce x1)
```

`ty1` beats `ty2` when `ty2` subsumes `ty1` — i.e. **more derived wins**.
`M(Base)` beats `M(obj)` for a `Derived` argument. Our `isObjectTy` special
case is the degenerate instance of this rule and disappears into it.

Then `better` (`:3798`) is a lexicographic chain of tiebreakers. Argument
lists are compared element-wise and reduced by the rule at `:3858`:

> "all args are at least as good, and one argument is actually better" → 1
> "all args are at least as bad, and one argument is actually worse" → −1
> "argument lists are incomparable" → 0

Our `betterThan` (`InferOverload.fs:139`) already has exactly this shape —
`List.forall2 asSpecificOrEq && List.exists2 (not << applicabilityMatches)`.
It is correct; it is just ranking under an impoverished `compareTypes`.

Of fsc's tiebreaker chain, adopt now: **argument-wise `compareArg`** and
**prefer non-generic methods** (`compare candidate.CalledTyArgs.IsEmpty
other.CalledTyArgs.IsEmpty`, `:3883`). Defer the rest, each of which ranks
a feature we don't have — type-directed conversions, param arrays, out
args, optional args, extension members, `Func<_>`-beats-other-delegates,
`T`-beats-`inref<T>`, `T`-beats-`Nullable<T>`. Leave a comment at the chain
naming what was skipped, so a later feature knows where its tiebreaker goes.

### Gap 1 mechanically

Once the speculative check is in place, the applicable tier's argument check
calls `subsumes` rather than the structural pre-check, and `compareTypes`
becomes:

```fsharp
let compareTypes (ctx: PassContext) (t1: SemType) (t2: SemType) : int =
    compareCond (fun a b ->
        match subsumes ctx b a with
        | SubsumeOutcome.Equal | SubsumeOutcome.Subtype -> true
        | SubsumeOutcome.Unrelated -> false) t1 t2
```

`isObjectTy` is deleted: `subsumes` answers `Subtype` for any reference
type against `obj` once the BCL provider is populated. Keep
`applicabilityMatches`'s wildcard arms — they are `TypeFeasiblySubsumesType`'s
`TType_var` rule and must stay ahead of any hierarchy walk. Rename it to
say what it is (`feasiblySubsumes`), since that is the fsc relation it
implements.

**The `ctx` threading risk.** `subsumes` needs `PassContext`. That threads
through `argAssignable` (`:94`), `asSpecificOrEq` (`:97`), and
`pickBestOverload` (`:120`) — `betterThan` (`:139`) is a closure inside the
latter and picks `ctx` up free — and therefore into `pickBestOverloadFrozen`,
which codegen calls from `ClrExternalMembers.fs:519` where no `PassContext`
exists.

Resolve this *first*. Either codegen's re-pick gets the slice of context
`subsumes` needs, or — better, and the reason `SymbolKey` exists — the front
end records the chosen member's identity in the TAST so codegen never
re-picks. Check whether that re-pick is load-bearing or a leftover before
threading anything. If it is a leftover, deleting it removes the only
obstacle here, and the same question resurfaces in gap 2's `resolveMember`.

### Gap 2 mechanically

`MemberRegistration.addMember` stops assuming name uniqueness. Keep
`Members: TypeMemberInfo[]` — a `Dictionary<string, TypeMemberInfo[]>`
stores the same data twice, since ordered walks (Freeze, conformance,
emission) still need the array. Add one shared `resolveMember` that takes
the array, a name, a static-ness, and optionally the call-site argument
types, and route all five lookup sites through it. That is what makes
inference and Freeze agree by construction.

`resolveMember` delegates to `pickBestOverload` only when a name has more
than one candidate — fsc's single-candidate fast path, and the reason the
speculative machinery costs nothing at the vast majority of sites.

Freeze must not re-run the pick. `Freeze/Resolve.fs:173` resolves a member
for its `SymbolKey`, not for a call, and has no argument types. So:

- Unification records the chosen `TypeMemberInfo`'s `DeclKey` on the
  dot-access node's side table when the name was overloaded.
- Freeze reads it back. No second pick, no divergence.

This mirrors the external path's use of `SymbolKey`, and is why
`SymbolKey.MemberKey` already carries `argSig: EqArray<string>`
(`SemanticInfo.fs:87`). But note `SemanticInfo.fs:391`: `FrozenType` is the
sound value-based overload-identity key that "retires the lossy
`SymbolKey.MemberKey.argSig` string". Key project-local overloaded members
the same way rather than inventing a mechanism.

New diagnostics, all currently absent:

- Same name, same static-ness, `feasiblySubsumes`-identical parameter
  shapes → "Duplicate member" (the unreachable case, distinct from a legal
  overload).
- No applicable candidate → fsc's `csMethodNotFound` / no-overload-matches
  shape.
- No unique best → "Ambiguous call", listing the tied candidates' `DeclKey`s.

## Ordering of work

1. **Resolve the `pickBestOverloadFrozen` context question.** It gates
   both gaps.
2. **Decide Option 1 vs Option 2** on the evidence, per §Deciding. This is
   the detailed analysis the plan defers: read `Engine.unify` end to end and
   establish whether its matching core separates from its occurs check,
   level adjustment, and constraint drains. Nothing below should be written
   before this is settled — both gaps sit on top of it.
3. **Land the speculative check**, whichever option won. Single-candidate
   fast path first, so nothing regresses while the machinery arrives.
4. **Gap 1.** Two-tier filter; `compareTypes` over `subsumes`; delete
   `isObjectTy`.
5. **Gap 2.** `resolveMember`; the recorded-`DeclKey` handshake into Freeze;
   diagnostics.

Steps 4 and 5 are independent once 3 lands.

## Test strategy

**`UnificationTests.fs`:**

1. Exact match beats subsumption. `M(Base)` / `M(Derived)`, argument
   `Derived` → `M(Derived)` via the exact tier, no ranking.
2. Subsumption tier fires when no exact match. `M(Base)` alone, argument
   `Derived` → `M(Base)`.
3. More-derived wins. `M(obj)` / `M(Base)`, argument `Derived` → `M(Base)`.
4. `obj` is no longer special-cased — same result via `subsumes`, with
   `isObjectTy` gone.
5. BCL subtype argument resolves. `w.Write(someException)` on `TextWriter`.
6. User-declared overload by parameter type. `Show(int)` / `Show(string)`.
   *(Fails today — picks `Show(int)` twice.)*
7. User-declared overload by arity. `M()` / `M(int)`.
8. Ambiguous user overload diagnoses.
9. Genuinely duplicate member diagnoses.
10. Static and instance members of the same name don't collide.
11. Overload on a generic class substitutes typars before ranking.
    `Box<'a>` with `M('a)` / `M(string)`, at `Box<int>`.
12. Prefer non-generic. `M<'a>('a)` / `M(int)`, argument `int` → `M(int)`.
13. **A failed trial leaves no residue.** Two candidates where the first
    trial binds a caller-side TyVar and then fails; assert the TyVar is
    still free afterwards. The regression test for whichever speculative
    mechanism lands — and one that passes vacuously today, only because
    nothing is ever tried.
14. **Shared method typar is consistent across positions.** `M<'T>('T, 'T)`
    called with `(1, "s")` is *not* applicable. *(Wrongly applicable today:
    the wildcard arm forgets `'T := int`.)*
15. **Non-ground argument doesn't kill the candidate set.** A call whose
    argument type is still an unresolved TyVar resolves against a
    concrete-parameter overload rather than reporting "no applicable
    overload". *(Fails today: `TyVar` vs `TyConst` falls to `| _ -> false`;
    cf. `TypeRelations.fs:101`.)*

**`FreezeTests.fs`:**

16. Freeze resolves the same overload inference chose — `Show(string)`'s
    frozen `MethodCall` carries the string member's key. Regression test for
    two-pass divergence; fails today for any overloaded name.

**`CoverageTests.fs`:**

17. TAST shape for an overloaded user member call — `key` distinguishes the
    two `Show` members.

**Codegen:**

18. Both overloads emit and are callable. On JS this needs signature-derived
    mangling; a user overload set is exactly the collision
    `JsExternalMembers.mangledName`'s nominal-only scheme produces.

## Out of scope

- **Type-directed conversion** (`ArgsMustSubsumeOrConvert`'s "or convert",
  op_Implicit, `int32`→`int64`). fsc's first three betterness tiebreakers
  rank it; we have none of it. Its absence is why our applicable tier can
  ignore `ReturnTypesMustSubsumeOrConvert`.
- **Optional and named arguments**, **`params` arrays**, **out args**.
  Each has a dedicated fsc tiebreaker to port when it lands.
- **Extension members** and their most-recently-opened priority rule.
- **Return-type-directed selection**, except fsc's `alwaysCheckReturn` for
  `op_Explicit` / `op_Implicit` — which we don't have either.
- **Operator overloading** (`static member (+)`). Its own channel.
- **`OverloadResolutionCache`** (`Checking/OverloadResolutionCache.fs`).
  A performance layer keyed before `FilterEachThenUndo` runs; port only if
  overload-heavy call sites show up in a profile.
- **SRTP trait solutions during overload resolution**
  (`AssumeMethodSolvesTrait`). Lands with SRTPs.

## Follow-up: universally-total local member-key mint

Gap 2 makes the local `MemberKey` mint total **only for overloaded names** —
inference computes the structural `ArgSig` (freezing the resolved member's
value-parameter `SemType`s in the declaring type's open typars, exactly as
`ExternalSymbols.argSigOfParameters` does for external members) and records it
on the call node's side table, and Elaborate reads it verbatim. Every other
local member call still mints through `LocalSymbolKey.ofMember`
(`TypeInfos.fs`), which fills `ArgSig` with `arity` copies of the placeholder
`FTUnknown ""`. That placeholder is *genuinely unique* for a non-overloaded
name (nothing else shares the name at that arity), so there is **no
correctness gap** — the lossiness only ever bit *within* an overload set,
which Gap 2 closes.

The follow-up is to retire the placeholder mint entirely so a lossy local
member key is *unrepresentable by construction*: thread the resolved member's
frozen parameter signature to all ~13 `ofMember` call sites (`Elaborate/Resolve.fs`,
`ElaborateExpr.fs`, `Elaborate/Idents.fs`, `Elaborate/Access.fs`, `Inline.fs`)
so every local member key carries a real `ArgSig` + `MethodTyparArity`. Deferred
because it touches the property / interface-method / inline key paths — whose
keys are correct today — and its blast radius outweighs its benefit until a
second consumer needs local keys to be uniformly total (e.g. the JS
overload-mangling work in [js-overload-mangling-plan](js-overload-mangling-plan.md),
which keys emitted names by signature). It is a strict superset of Gap 2's
mint: the same freeze machinery, applied unconditionally rather than only when
a name is overloaded.

## Cross-references

- `fsharp/src/Compiler/Checking/ConstraintSolver.fs` — `Trace` (`:463`),
  `FilterEachThenUndo` (`:503`), `ResolveOverloadingCore` (`:3477`), the
  no-undo commit (`:3676`), `GetMostApplicableOverload` (`:3750`).
- `fsharp/src/Compiler/Checking/TypeRelations.fs` — `TypeFeasiblySubsumesType`
  (`:133`), whose `TType_var` wildcard rule (`:99-102`) our
  `applicabilityMatches` already implements.
- `InferOverload.fs` — the picker this plan extends. Its wildcard-arm
  rationale must survive the `subsumes` change intact.
- `Subsume.fs` — our `TypeFeasiblySubsumesType`: pure, and already walking
  user + BCL `inherit` chains.
- [js-overload-mangling-plan](js-overload-mangling-plan.md) — the JS
  emission side; user-declared overload sets land in the same collision.
- `SemanticInfo.fs:391` — why `FrozenType`, not `argSig`, is the sound
  overload-identity key.
