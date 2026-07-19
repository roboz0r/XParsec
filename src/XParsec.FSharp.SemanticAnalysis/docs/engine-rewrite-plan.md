# Inference engine rewrite — SemType / UnionFind / Unification

Status: **design doc, not a landed contract.** Records a decided direction and the
invariants a rewrite must preserve; open points marked ⟨OPEN⟩. Supersedes nothing yet —
existing code is authoritative until a step lands.

## Decision

Rewrite the `SemType` / `UnionFind` / `Unification` triad from the current mutable-node
"ball" to a textbook shape, in **two phases**:

- **Phase A — substrate.** Immutable, interned `SemType`; a dense `int<TyVarId>` metavar
  **arena** (parallel arrays) replacing the ten-slot `TypeVar` node; plain path-compression
  `find`; payload families become store side-tables with associative/idempotent joins. Keeps
  the *algorithm* (eager, syntax-directed Algorithm J + Rémy levels). This is the existing
  [unification-store-redesign-plan](unification-store-redesign-plan.md) taken to completion —
  see it for the arena shape, join-semilattice payloads, interning/caching, and the staged
  integration steps. This doc is the umbrella; it does not restate that detail.
- **Phase B — solver.** Make the *implicit* constraint solver explicit: elaboration emits a
  constraint set; a solver drains a worklist with **suspension** (never backtracking).
  Replaces the four ad-hoc pending-payload families + `migrateBounds` + `drainAll` with one
  constraint language and one loop.

**A then B.** B on the clean arena is far cheaper than B on today's soup; A is the required
substrate for B either way, and may itself buy most of the legibility win.

## The boundary: `FrozenType` is frozen

`FrozenType` (`SemanticInfo.fs:704`) is the **total, immutable data contract in and out of
Semantic Analysis** — the post-`Freeze` TAST (step 11 in [passes.md](passes.md)) *and* the
form external / cross-unit signatures arrive in and are thawed from. It stays as-is, to the
extent it is correct today; its incompleteness (`FTUnknown`, `FTLocalTypar`, the external-only
`FTKeyOf`/`FTIndexedAccess`/`FTConditional`) is **accepted, not in scope**.

This makes the rewrite a **strangler behind the thaw⇄freeze boundary**: everything between
`FrozenType → SemType` (thaw, minting fresh metavars) and `SemType → FrozenType` (freeze) is
replaceable; codegen (Clr/Js), `ExternalSymbols`, and the frozen TAST are insulated.

**Oracle (two-tier — the goldens are NOT a correctness oracle).** Current coverage is thin and
the current engine has known bugs we explicitly do **not** preserve, so "frozen output identical
⇒ correct" is false (it would certify the bugs). Instead:

- **Phase A is representation-only.** The corpus goldens (`REPORT.txt` + frozen snapshots) are a
  strict **change-detector**: through all of A they must stay *identical*; any diff is a refactor
  regression. Thin coverage bounds how much A is checked but does not weaken the invariant.
- **Phase B may change semantics** (fixing the ad-hoc solver's bugs). There the goldens are a
  **review surface** ("what moved, and is each move a fix or a regression?"), not pass/fail.
  Correctness is defined by **fresh tests written against desired semantics** — the `dotnet fsi`
  oracle for F# behavior, plus algebraic **property tests** on the engine (unify commutativity,
  `zonk` idempotence, generalize/instantiate round-trip, `subsumes` reflexive + transitive).
  These specify the target; they are not investment in the old impl, and are written only when B
  begins. No effort goes into raising coverage of, or bug-fixing, the code being replaced.

## Preserve-checklist (correct today — carry verbatim)

These are deliberate, well-known-good decisions, **not** the ad-hoc part. A rewrite that drops
one is a regression, not a simplification.

1. **Nominalism / principality.** Inference never synthesizes a `TyOr`; unions enter only at
   annotation sites; membership/assignability lives in the *directional* `subsumes`, never in
   symmetric `unify` (`SemanticInfo.fs:907`+, `TyOr`; `FTOr` mirror at `:733`).
2. **Intrinsics by key identity, never stringly** (`TyConst`/`FTConst`, `IntrinsicTypePatterns`).
3. **Rémy levels** for generalization: `min`-on-union, occurs-lowering, quantify level >
   enclosing scope (`TypeVar.Level` `:1191`; `UnionFind.union` `:42`; `TypeScheme` `:1931`).
4. **Measures as abelian-group equality on union** (`TypeVar.Units` `:1173`; `MeasureTerm`).
5. **Freeze contract**: post-freeze tree has no `TyVar`; **key on the union-find root** when
   equating vars (`Freeze.fs:123`). A residual unlinked `TyVar` is tolerated → `FTUnknown`.
6. **Read-only dispatch — the unifier never trial-unifies *today*.** Operands are ground at
   dispatch, so disjunctive/overload dispatch decides via the *read-only* overload filter
   (`codegen-by-key-plan` deferral), and the substitution is never speculatively mutated-then-
   undone. This is the semantic invariant to preserve. It does **not** fix the substitution's
   *data structure*: whether the store is rollback-capable is a separate, deferred substrate
   choice — see "Rollback substrate" below. (Earlier drafts overstated this as "no
   semi-persistent UF is needed"; that was a substrate opinion, not an invariant.)
7. **Forward pipeline, no pass-level fixpoint** ([passes.md](passes.md) §"Why no fixpoint"):
   the one feedback loop (deferred resolution) stays contained inside the Unification pass.
8. **Freeze knows no backend.** `Freeze` emits target-agnostic *semantic classification*
   (including `FrozenConstraint`); target dialect / lowerability stays in the backend. Semantic
   facts a backend needs go upstream into the frozen contract, never a backend hook into freeze.

## Target data model

- **`type TyVarId = int<tyVarId>`** — dense, monotone, measure-tagged id (erased to `int`;
  type-separated from `NodeKey`/`RegionId`/array indices). The right *spelling* of the id.
- **`SemType`** becomes a pure, interned (hash-consed) value: structural equality ⇒ id compare;
  resolved types are memoizable. The `TyVar` case holds a **thin handle** `TypeVar { Id: TyVarId }`
  during migration (reference identity retained so the ~dozen `HashIdentity.Reference` sites in
  Elaborate/Freeze/Inline/GeneralizedTypars/PassContext/Validation/Unification keep compiling
  while they flip to id-keying one at a time). Collapsing to raw `SemType.TyVar of TyVarId` is an
  **optional final** step, not the first move (⟨OPEN A⟩ in the store plan — lean: handle).
- **`TypeStore`** — the arena: `parent`/`rank`/`level`/`solution`/`units` as `TyVarId`-indexed
  arrays; payload families as side-tables under the representative id. `find`/`union`/`zonk`
  become array ops. Grow-only per file. Backing representation (flat vs rollback-capable) is a
  swappable implementation detail — see below.

### Rollback substrate (⟨OPEN D⟩)

The **store-mutation seam is the real commitment** (integration step 2: every substitution write
goes through a store accessor with private setters, compiler-enforced). Consumers call
`find`/`setSolution`/`snapshot`/`rollback`, never the backing arrays — so the *persistence
mechanism* is swappable behind the seam without touching the ~dozen consumer sites. This is what
makes a future "flip to trial-and-undo" a contained implementation swap, not a re-architecture.

Two candidate backings, chosen empirically when/if a **named** backtracking driver lands (chained
constrained `inline` (F#+), generic-math Regime 2, implicit-conversion overloads):

- **flat arrays + watermarked trail** — an undo log `(array, id, old)` pushed at the seam's
  setters; `rollback` restores to a watermark. Zero-allocation writes on the fast path; the trail
  is added in the *one* seam, so it is as safe as the (compiler-enforced) seam is exhaustive.
- **semi-persistent Conchon–Filliâtre UF** (`Vesper.UnionFind` — already built + tested; the
  classic unification-backtracking structure). Type-enforced versioning seam. Cost: ~2 small
  allocations per genuine write (`Diff` + `PaCell`) and a branch per read, paid **always, even
  used forward-only** — exactly the overhead the flat arena removes. Confined to the substitution
  path; the dense-id interning/caching lever is unaffected either way.

**Lean: flat-ephemeral first** (honors the perf driver; the linear workload is 100% of current
use), with the semi-persistent UF held as the ready drop-in. Note the flip is more than a backing
swap: it also needs the dispatch rewrite (read-only filter → commit-and-undo) and **cache
invalidation across the abandoned branch** (semi-persistence rolls back the substitution, not the
caches — the generation stamp must version with the branch). Neither backing gives those for free.

## Constraint theory (Phase B) — three relations, not six axes

Elaboration emits constraints; the solver drains a worklist, **suspending** stuck ones. The
well-known-good-practice model (Jones's *qualified types*; the "X" in HM(X)/OutsideIn(X)) is three
theories, and the constraint "axes" collapse into two of them:

```fsharp
type Constraint =
    | CEq      of SemType * SemType                     // unify (symmetric)
    | CSub     of sub: SemType * super: SemType         // subtyping / coercion (directional)
    | CPred    of Predicate                             // qualified types
    | CMeasure of SemType * MeasureTerm                 // abelian group
    | CDefault of SemType * chain: SemType list         // weak, resolution-ordered

and Predicate =
    | Capability of CapabilityKind * SemType            // equality/comparison/struct/nullness — derived
    | HasMember  of recv: SemType * MemberShape         // structural trait (SRTP) + ordinary dot-access
    | Implements of SemType * iface: TypeKey * args      // nominal trait (SAIM/IWSAM, generic math)
```

- **`CSub` is one relation with a rule table keyed on `super`'s shape** — class → inheritance
  walk; interface → impl-set membership; `TyOr` → member-wise disjunction; primitive → identity
  (nominalism, preserve-1). So interface-impl / class-inheritance / union-membership are *arms of
  `CSub`*, not distinct kinds, and the existing `SemanticConstraintKind.Coercion` (`:1139`) is
  `CSub` mis-filed under "constraints" today. **Two lifetimes:** checked read-only at a use site,
  but a typar *bound* (`when 'T :> exn`) **suspends and generalizes** — it rides the scheme
  context like a predicate. One relation, decided-at-use *or* quantified-at-binding.
- **`CPred` is one mechanism (qualified types) with per-constructor entailment.** Capabilities,
  structural traits (SRTP), and nominal traits (SAIM/IWSAM) differ only in how a witness is found:
  derivation rule / structural member-shape match / nominal interface-set lookup. SRTP and SAIM
  are the *same* "T provides operation O" — structural vs nominal witness; two constructors, one
  loop, kept distinct because elaboration differs (member splice vs interface dispatch).
- **Interface appears in both `CSub` and `CPred.Implements` — keep distinct.** `CSub(T,I)` is
  assignability (elaborates to a coercion); `Implements(T,I)` is static-member dispatch (a
  witness). Conflating them because both name `I` is the classic trap.
- **Dot-access and SRTP are both `HasMember`, differing only in generalization policy** — a stuck
  ordinary dot-access at generalization is the "lookup on indeterminate type" error; an SRTP bound
  may be quantified.

A predicate is **solved** (witness on the ground type), **stuck** (receiver not ground → re-woken
when it grounds), or **quantified** (var generalizes → predicate rides `TypeScheme.constraints`
(`:1931`) as `P => τ`). That field is already the qualified-type context — the machinery is
half-there, smeared across four node slots today.

### Payload family → A side-table → B constraint

| today (on `TypeVar`) | A: store side-table (join) | B: constraint |
|---|---|---|
| `Constraints` (`:1179`; capability subset) | grow-only set under rep | `CPred(Capability kind)` |
| `Constraints`→`Coercion` (`:1139`) | — (moves to subtyping) | `CSub(T, target)` |
| `SrtpBounds` (`:1181`; shared `Resolved` `:1116`) | grow-only set + **`solved` table** (kills by-ref aliasing) | `CPred(HasMember)` structural / `CPred(Implements)` nominal |
| `PendingDotAccess` (`:1198`) | grow-only set under rep | `CPred(HasMember)` (no-generalize policy) |
| `Defaults` (`:1207`) | ordered list under rep | `CDefault(a, chain)` |
| `Link` (`:1165`) / `Units` (`:1173`) | `solution` / `units` cell | `CEq` / `CMeasure` |
| `subsumes` | read-only relation + suspendable bound | `CSub` |

Solver loop: `union` joins two reps' stuck-constraint sets; **grounding a var wakes its watchers**;
`CSub`/`Capability` checked read-only; `HasMember`/`Implements` resolved when the receiver head
grounds; unresolved-at-end are diagnostics (or generalized). Suspension, not backtracking —
consistent with preserve-6.

## Constraints survive freeze — backend lowers or rejects

The generalized typar constraints on **functions, members, and types** must reach
`Frozen.TastFile` so a backend can lower them (CLR generic constraints: `struct`/`new()`/base
class/interface, incl. **SAIM interfaces** for generic math) or **reject the TAST** when it
cannot lower one adequately (a residual structural `HasMember` with no nominal witness on a target
without SRTP). This is not new machinery, only *partial*: `FrozenConstraint` (`SideTypes.fs:113`)
has **only `Coercion`** today, and the frozen TAST already threads a per-binding `FrozenConstraint
list` (`TastFile.GenericFnSchemes`, `Tast.fs:1112`, keyed by `NodeKey`) end-to-end to codegen.

The work: **widen `FrozenConstraint` to the full residual set** — the frozen, typar-index-relative
mirror of `Constraint`/`Predicate` (leaves are `FTTypar(axis, idx)` / `FTLocalTypar`, exactly as
`Coercion.target` already is) — carried on all three typar axes: **types** (`Declaring`),
**members**/**functions** (`Method`), and body-local schemes (`FTLocalTypar`).

This honors **preserve-item: freeze must not know a backend.** `FrozenConstraint` is a
*target-agnostic semantic classification*; each backend maps it to its own lowering vocabulary:

- Target-independent *unsatisfiability* is caught earlier by the solver (a `Capability` that
  cannot hold is a semantic error pre-freeze).
- Target-relative *unlowerability* is the backend's call — CLR lowers `Implements`/capabilities to
  IL constraints; a structural `HasMember` that never resolved and isn't inlined has no CLR form →
  backend rejection. This relocates today's early "SRTP-outside-inline is an error" (`passes.md`
  §inline) into a per-target lowering decision, which is more honest once SAIM gives a nominal
  escape hatch on the CLR.

⟨OPEN⟩ carry the `FrozenConstraint list` **on the frozen decl node** (self-contained contract) vs.
the current `NodeKey`-keyed side-map. Lean: on-node — a frozen decl should own its constraints as
part of the total immutable contract.

## Staging (each step landable + green against the corpus)

Phase A: follow the store plan's integration steps (add `Id` + counter → route mutations through
a single store accessor → move one payload family at a time behind existing accessor names →
intern → cache). Every step is payload-preserving and frozen-output-identical.

Phase B (only once A's side-tables exist):
1. Define the constraint DU; make elaboration **emit** it alongside today's inline unify (shadow
   mode — asserted equal, not yet authoritative).
2. Turn the side-tables into a worklist with explicit wake-up; delete `drainAll`/`migrateBounds`.
3. Flip authority to the solver; delete the inline on-link callbacks and `MemberSignature.Resolved`.

## Risks / open

- **Oracle is a change-detector, not proof.** The goldens only *surface* diffs; they certify
  nothing (thin coverage, known bugs — see "Oracle" above). Phase B correctness rests on the
  fresh spec/property tests, so those are a hard prerequisite for flipping B's authority.
- **`FrozenType` incompleteness leaking in.** The thaw side must keep minting fresh metavars for
  `FTLocalTypar`/`FTTypar` exactly as today (`Inline.freshen`); the arena must not change thaw semantics.
- ⟨OPEN B-vs-A boundary⟩ how much of B is worth doing vs. stopping at A. Re-decide after A lands
  and the corpus is green — A may suffice.
- ⟨OPEN⟩ caching invalidation granularity (global generation stamp vs per-var) — inherited from
  the store plan (⟨OPEN F⟩ there).
