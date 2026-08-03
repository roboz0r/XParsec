# Intrinsic constraint sourcing — capability vs representation

**Status (2026-07-15): design draft, no code.** Direct continuation of
`contract-sourced-intrinsic-identity-plan.md`, whose standing frontier named this work:
*"derive the equatable/comparable enumeration from contract capability interfaces via
`FrozenInterfaces`."* That doc made an intrinsic's **identity** contract-sourced; this one makes an
intrinsic's **constraint verdicts** (equality / comparison / struct / …) contract- and
provider-sourced, retiring the last hardcoded name table in the solver. Delete on landing
(`feedback_plan_docs_ephemeral`).

## Problem

`checkConstraint` (`Passes/Unification/Engine.fs`) answers `when 'T : …` constraints. For the
nominal data types it reads a per-type verdict — `info.EqualitySupport` / `info.ComparisonSupport`
(stamped at registration from attributes / structural defaults, `Passes/Attributes.fs`). For an
**intrinsic** (`TyConst`) it instead falls through to `primitiveSupports`, a name-keyed table over
a hardcoded set:

```fsharp
let private primitiveValueTypes =
    Set.ofList [ "int"; "int64"; "byte"; "bool"; "float"; "float32"; "char"; "unit" ]
```

Intrinsics are the ONLY types whose constraint verdicts are not sourced the way every other type's
are. The table is wrong on two counts:

1. **Name-keyed** — it answers by `SymbolKeyOps.intrinsicName`, re-introducing the string axis the
   identity work spent itself removing. A user type whose simple name collides never actually
   reaches it (intrinsics resolve first), but the currency is still a string.
2. **It bakes backend knowledge into the solver.** The set is really two different facts fused:
   *which primitives are value types* (a CLR representation fact) and *which primitives support
   `=` / `<`* (a portable capability fact). Neither belongs hardcoded in the unifier.

The `prim-types-*.fsi` contracts today say nothing that would let either fact be sourced — they are
bare (`type int = extern`, `type string = extern`), carrying only a per-target platform repr from
the `.fs` binding.

## Principle — two axes, two homes (decided, this design)

The six constraint kinds split cleanly, and the split is forced by asking what a non-CLR target
(JS today; a native / GPU target later) can say about each:

- **Capability axis** — `Equality`, `Comparison` (and the existing interface capabilities
  `disposable` / `seq`). A **behavioral, target-agnostic** fact: `int` is comparable on CLR, JS,
  and a GPU alike. → declared **on the type, in the shared `.fsi` contract**, resolved through the
  capability interfaces that already exist. Single source of a universal truth; drift-free.
- **Representation axis** — `Struct`, `ReferenceType`, `Nullness`, `NotNull`. A **physical,
  per-target** fact: "is a value type" is a notion only some targets have. → supplied by the
  **provider** as a per-target *representation model*, not a fact on the shared type.

The value-type axis being meaningless on JS is not a gap to paper over — it is the evidence that
forces representation OUT of the contract (you would otherwise either lie in a target-agnostic file
or invent target-conditional contract syntax) and INTO the provider, where each target answers in
its own terms. `feedback_freeze_no_backend_knowledge` (representation stays in the backend),
`feedback_codegen_js_owns_assignability` (the backend owns intrinsic-repr; the unifier must not
double up).

## Decisions locked (user, this design)

1. **Derive-from-operators is the wrong direction.** The `when ^T: int` clauses in `comparison.clr.fs`
   / `ops-platform.clr.fs` are a **codegen optimization** (inline `clt`/`ceq` vs. the `Comparer<^T>` /
   generic base) — a *subset* of what supports the operation, chosen for "can we emit an opcode."
   Using them to answer `when 'T : comparison` would under-approximate (a comparable type reachable
   only through the base comparer would be wrongly rejected). The clause list stays the source for
   the *emit* decision (already owned by the static-opt machinery, `Inline.staticOptTypesMatch`) and
   nothing else.
2. **`when 'T : struct` stays a first-class PORTABLE constraint.** It is not demoted to CLR-only.
3. **A target that cannot inhabit a polarity yields a WARNING, not an error.** On JS the value-type
   polarity is empty, so `when 'T : struct` "can never hold here" — a portability warning. The axis
   is symmetric: a reference-free native / GPU target empties the *reference* polarity, so
   `when 'T : class` warns there by the identical mechanism. The constraint text is portable; only
   its *satisfiability* is target-parameterized.

## Design — capability axis

**Contract.** The `prim-types-*.fsi` primitives declare the capability interfaces they implement,
exactly as a Vesper type does (`interface comparable<int>`), and truthfully (BCL `Int32 :
IComparable<Int32>, IEquatable<Int32>`; `String`, `Char`, `Boolean`, the numerics likewise):

```fsharp
type int = extern
    interface equatable<int>
    interface comparable<int>
```

The capability anchors already exist as `extern interface` shapes (`capabilities.fsi`;
`CapabilityIdentity.Equatable` / `.Comparable` in `RuntimeNames`), and the impl-witness machinery
(`FrozenInterfaces`, `Unification.implementsSelf`, `CapabilityIdentity.Matches`) already answers
"does this type implement this capability?" for nominals. Wiring primitives onto it means their eq/
cmp verdict is derived, not enumerated.

**Solver.** The `TyConst` arm of `checkConstraint` stops calling `primitiveSupports` for
`Equality` / `Comparison`. Instead the resolved intrinsic shape yields an `EqualitySupport` /
`ComparisonSupport` verdict the same way a nominal `IInterfaceImplHost` does:

- implements `equatable<self>` ⇒ `EqualityVerdict` supporting; absent ⇒ `NoEquality`.
- implements `comparable<self>` ⇒ `ComparisonVerdict` supporting; absent ⇒ `NoComparison`.

so the record/union/class and intrinsic arms collapse onto one verdict read. `primitiveValueTypes`'
equality/comparison duty is gone.

**Feasibility crux (first milestone).** Can a bare `type int = extern` carry an `interface …` impl?
The extern grammar today proves the *anchor* side (`extern interface with abstract member`) and
`inherit` inside an extern body (`enumerator ... inherit Vesper.disposable`), but not (yet
confirmed) an **impl declaration with no member body** on a repr-only extern type — the platform,
not the contract, provides the implementation. Resolve this before anything else:
- `.fsi` signature grammar (`SignatureParsing.fs`) — does `type X = extern` admit trailing
  `interface T` clauses?
- extractor (`VesperLib` / `ExtractCtx`) — does it record those onto the intrinsic shape's
  `FrozenInterfaces` so the witness machinery sees them?
If not, that parse/extract widening is milestone 1 and gates the rest.

## Design — representation axis

**Provider representation model.** The provider stops answering a per-type `isValueType : bool` and
instead exposes the target's model, enough for the solver to classify a resolved intrinsic AND to
know whether a polarity is inhabited at all on this target:

- per-intrinsic: `ValueType | ReferenceType` on a target that has the distinction; on CLR sourced
  from metadata `IsValueType` (already surfaced as `externalIsValueType`).
- per-target: which polarities the model inhabits (`HasValueTypes`, `HasReferenceTypes`). CLR: both.
  JS / Python: reference only. A reference-free native / GPU target: value only.

**Solver — three-way satisfiability.** Today `ConstraintOutcome` is `Satisfied | Violated | Defer`.
The warning decision (locked #3) needs a fourth verdict — call it `UnsatisfiableHere` (emits a
warning, not an error). For `Struct` on type `T` (mirror for `ReferenceType`):

| target has value types? | T classifies as value type? | outcome |
|---|---|---|
| yes | yes | `Satisfied` |
| yes | no (genuinely a reference type) | `Violated` (error, as F#/CLR today) |
| **no** | — (polarity empty) | `UnsatisfiableHere` (**warning**) |
| — | T still a free typar, target has the polarity | `Defer` (as today) |

The error/warning split is exactly "is the required polarity inhabited on this target": empty ⇒ the
source is portable-legit and only *this* target can't satisfy it ⇒ warn; inhabited but wrong-
polarity pinned type ⇒ a real mistake ⇒ error. `Nullness` / `NotNull` stay `Defer` (separate track,
`nullability-analysis-plan.md`) but move to the same provider-model read when that track lands.

**Producers unaffected.** The constraint is still produced portably — front end
(`Translate.fs`, a user's `when 'T : struct`) and library extraction (`VesperLib/TypeTranslate.fs`,
e.g. the CLR `nullV<'T when 'T : struct …>` helper). Only the *verdict source* changes.

## Work order

1. **Feasibility spike** — extern-type interface-impl grammar + extraction (the capability crux
   above). If it needs parser/extractor work, that is the first landing.
2. **Capability axis** — declare `equatable`/`comparable` on the `prim-types` primitives; source the
   intrinsic eq/cmp verdict from `FrozenInterfaces`; delete the eq/cmp half of `primitiveSupports`.
   Regression: the existing SRTP `when 'T : equality/comparison` coverage over `int`/`string`/… must
   stay green with no name table.
3. **Representation model** — provider exposes the per-target model; add the `UnsatisfiableHere`
   warning verdict; `checkConstraint`'s `Struct`/`ReferenceType` arms read the model; delete
   `primitiveValueTypes`. Regression: `when 'T : struct` on a CLR compile unchanged; a JS compile of
   the same constraint warns rather than errors/defers.
4. **Retire** — `primitiveSupports` and `primitiveValueTypes` gone; the `TyConst` arm of
   `checkConstraint` reads verdicts uniformly with the nominal arms.

## Open questions (settle in the doc before coding each milestone)

- **Extern impl grammar** (milestone 1) — the gating unknown; everything downstream assumes a
  primitive can carry `FrozenInterfaces`.
- **`unit` equatable/comparable — OPEN (user reconsidering).** Equatable is settled (trivially
  true). Comparison is the live question, and the framing matters:
  - *For comparable* — `unit` is a one-element totally ordered set, so `compare () () = 0` is total,
    reflexive, and the unique correct answer (not "meaningless"). Keeping it comparable preserves
    `Set<unit>` / `Map<unit, _>` (legal, degenerate — `Set<unit> ≅ bool`) and lets generic
    `'T when 'T : comparison` code instantiate at `unit` instead of erroring. This is what "correct
    math" says (`feedback_prototype_correct_semantics_over_fsharp_parity`) AND matches F#. It is also
    the current `primitiveSupports` behavior, so it is the no-change option.
  - *Against* — comparing units carries no information; a program that does it is plausibly a
    mistake, and rejecting it surfaces that. But dropping it turns `Set<unit>` from degenerate-legal
    into a compile error.
  - The eq/cmp split does NOT hinge on this: `obj`/`exn` are the load-bearing equatable-not-
    comparable primitives (reference equality, no ordering), so two independent per-type declarations
    are needed either way.
  - Contract-location sub-question stands: does `unit` have a `prim-types` contract to hang the
    declaration(s) on, or must one be authored?
- **Verdict shape for the warning** — a new `ConstraintOutcome.UnsatisfiableHere` case vs. a
  severity field on `Violated`. New case is cleaner for exhaustiveness; decide when milestone 3 is
  scoped.
- **Where the per-target model lives** — a field on the provider surface vs. a small dedicated
  capability the provider answers. Sketch alongside the intrinsic-shape change.

## Relevant memories

`feedback_redesign_doc_first` (this doc), `feedback_freeze_no_backend_knowledge` (representation ⇒
backend/provider), `feedback_codegen_js_owns_assignability` (backend owns intrinsic-repr; no unifier
double-up), `feedback_dynamic_intrinsics_over_du_cases` (resolved-not-authored intrinsic facts),
`feedback_mockbuiltins_is_a_trap` (no front-end shadow set), `feedback_prototype_correct_semantics_over_fsharp_parity`
(correct cross-target semantics over F# parity), `feedback_plan_docs_ephemeral` (delete on landing).
