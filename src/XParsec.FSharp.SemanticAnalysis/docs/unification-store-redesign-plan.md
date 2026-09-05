# Unification store redesign — sketch + integration path

Status: **integration steps 1–3 + 6 have LANDED** (2026-07-19, frozen-output-identical
commits on `semantic-analysis`); steps 4 (interning) and 5 (caching) remain, **paused before 5**.
A follow-up beyond those steps also landed — the thin `TypeVar` handle was **collapsed to the
raw `SemType.TyVar of TyVarId`** (⟨OPEN A⟩ resolved to *raw id*) and root reads were made
correct-by-construction with a `Rep` type; see "Landed vs remaining" and ⟨OPEN A⟩ below.
Open decisions are marked ⟨OPEN⟩. The "Current state" and "Target data structures" sections
below are now largely *history* — the target is built. See "Integration path" for the
per-step landed/remaining status.

## Landed vs remaining (fresh-session handoff)

- **Landed:** dense `TyVarId`; per-file `TypeStore` arena (union-find `parent`/`rank`,
  root-authoritative `level`/`link`/`units`, write-once `region`); the four deferred-constraint
  families as store side-tables (`Constraints`/`Defaults` grow-only lists; `Srtp`/`Pda`
  = `DischargeTable`, grow-only + reference-keyed `solved`); `migrateBounds` and
  `MemberSignature.Resolved` **deleted**.
- **Handle collapse + `Rep` (follow-up, landed):** the sealed `TypeVar` class AND the id→handle
  `Node` array are **deleted** — `SemType.TyVar of TyVarId` carries the id directly, `find`/`union`
  operate on `TyVarId` with structural equality, and the ~two dozen `HashIdentity.Reference`
  metavar-keyed dictionaries/sets became plain structural `TyVarId` keys. A `[<Struct>] Rep =
  private Rep of TyVarId` (private case in `TypeStore.fs`; sole producer `UnionFind.find`, moved
  into that file) now gates `Link`/`Level`/`Units` and the side-tables — a non-root authoritative
  read is a compile error. `Parent`/`Rank` and write-once `Region` stay keyed by raw `TyVarId`.
- **Step-2 finding (important):** "make the node's setters *private to the store module* so
  the compiler enforces the one seam" is **not achievable standalone** here — `TypeVar` is in
  the mutually-recursive `SemType` block, there are no `.fsi` files, and it is one assembly,
  so F#'s only true "one writer" enforcement is **deleting the field** (step 3). The seam
  therefore became compiler-proven *per family, as each slot was deleted* — steps 2 and 3
  are inseparable here, and that is how they landed. (The later `Rep` collapse adds the dual
  guarantee on the *read* side: an authoritative read now also cannot be spelled off a non-root.)
- **⟨OPEN B⟩ resolved by outcome:** `Srtp`/`Pda` went **grow-only + `solved`**; but
  `Constraints` and `Defaults` **kept consumption** (remainder writeback / wholesale clear,
  now routed through the store) — a struct `SemanticConstraint` has no reference identity and
  `propagateToFreeArgs` needs value independence; `Defaults` discharges a chain wholesale with
  shared singleton targets. Making those two grow-only belongs to Phase B's solver.
- **Remaining:** step 4 (interning — lands the two-level home, step 4 note) then step 5
  (caching). **Do 4 + 5 together behind a benchmark**; interning's payoff comes through
  caching, and step 5 is benchmark-gated. The arena alone may suffice — re-decide after
  profiling (engine-rewrite-plan ⟨OPEN B-vs-A boundary⟩).

**Driver: reasonability and performance — NOT reversible speculation.** The disjunctive
SRTP dispatch that first motivated a reversible store lands via *deferral* on the existing
read-only overload filter instead (see `codegen-by-key-plan.md` → "Deferred: disjunctive
dispatch"): because this codebase rejects implicit conversions, operands are ground at
dispatch time and the read-only `matchTypes` filter decides without trial-and-undo. So the
no-speculative-unification stop stays, and rollback is **not** a requirement. This redesign
proceeds on its own two merits:

1. **Reasonability.** The union-find node *is* the constraint store — a payload-heavy
   mutable object hand-migrated on every union. Separating the disjoint-set from the
   constraint bookkeeping is worth doing for legibility alone.
2. **Performance.** Type checking is the classic F# cost sink. A dense, id-indexed arena
   (the shape that gives competitive-DSU / e-graph implementations their speed), with the
   genuinely **immutable** parts broken out so they can be interned/shared/**cached**
   rather than re-walked, is where the wins are.

Rollback is parked: if a *future* feature needs reversible speculation — the concrete
candidate is **chained constrained `inline` dispatch (F#+ style)**, if real usage turns out
to need cross-layer backtracking (see `codegen-by-key-plan.md` → "Future: chained
constrained `inline`"); others are generic-math Regime 2 (SAIM-constrained generic methods)
and general overload resolution *with* implicit conversions — the id-arena makes adding a
trail or semi-persistence cheap later. It is out of scope now, and nothing below depends on
it.

## Current state (what we are replacing)

`TypeVar` (`SemanticInfo.fs:1163`) is a `[<Sealed>]` class, **reference identity only — no
stable id**, with ten mutable slots:

| slot | role | migrated on union? |
|---|---|---|
| `Parent`, `Rank` | union-find structure | — |
| `Level` | Rémy's levels (generalization) | yes (min) |
| `Link` | the solution (authoritative on root) | yes |
| `Units` | measure constraint | yes (`mergeUnits`) |
| `Region` | region id | — |
| `Constraints` | `SemanticConstraint list` | yes |
| `SrtpBounds` | `MemberSignature list` (the SRTP traits) | yes |
| `PendingDotAccess` | `DeferredMemberAccess list` | yes |
| `Defaults` | `SemType list` (`default ^T`) | yes |

Six of ten are **deferred-constraint payload**, hand-migrated in `EngineCore.migrateBounds`
(`EngineCore.fs:121`) and hand-discharged on link in `dischargeAll` (`Engine.fs:489`). `find`
(`TypeStore.fs:242`) does **full path compression** — a mutating read — and `resolveStep`/
`zonk` call it transitively, so essentially every dereference mutates. That is the "mutable
soup": a node that is simultaneously graph structure, solution, measure, four families of
pending constraint, and a generalization level, with bespoke migration for each.

## The immutable / mutable boundary (the caching thesis)

The performance case rests on drawing a line the current design blurs:

- **Genuinely immutable, once produced** — cacheable / internable: resolved `SemType`
  structure (a fully-zonked type never changes), generalized `TypeScheme`s, the intrinsic
  types (`ctx.Intrinsics`), and contract-extracted external signatures. These are re-walked
  today (every `zonk` re-traverses); interning them (hash-cons) makes structural equality a
  pointer compare and lets resolution results be **memoized** by id.
- **Mutable, until ground** — the union-find substitution (`parent`/`solution`) and the
  pending-constraint payload. Only this needs to stay mutable, and only until its vars
  ground.

Caching is safe exactly across that line: memoize keyed on the *immutable* side, invalidate
only when a var on the *mutable* side grounds. Today nothing is cached because the two are
fused on one object with no identity to key on — which is the deeper reason for the redesign.

## Target data structures (in isolation)

### 1. `TyVarId` — a stable dense id

A plain `int` (or `[<Struct>] TyVarId of int`) from a monotone counter (always per-file so `int` is large enough) in the store. Dense
so the store holds state in **arrays**, not per-object slots or reference-keyed
dictionaries: parent/rank/level/solution become cache-friendly array reads, and every
side-table and cache can key by `int` instead of hashing object identity. This is the single
biggest perf lever and it is independent of rollback.

Challenge: rewriting `SemType.TyVar of TyVarId` outright (clean result, but ripples through `SemType`,
`freeze`, every `Infer*` pass). **DONE** — the collapse landed as a follow-up; the ripple was
mechanical (`.Id` projection at the id-keying sites) and the one real hazard it surfaced was two
`ReferenceEquals`-on-`TyVarId` sites in `Engine.fs` that boxed to always-false and silently dropped
a `union` loser's deferred-obligation payload (now `=`).

### 2. `TypeStore` — the arena (parallel arrays, id-indexed)

```
type TypeStore =
    { mutable parent   : TyVarId[]        // ValueNone ≡ self (root)
      mutable rank      : int[]
      mutable level     : int[]           // Rémy level, authoritative on root
      mutable solution  : SemType voption[]   // was TypeVar.Link
      mutable units     : MeasureTerm voption[]
      // deferred-constraint payload, keyed by representative id:
      mutable srtp      : BoundSet[]      // was SrtpBounds
      mutable constrs   : ConstraintSet[] // was Constraints
      mutable dots      : DotSet[]        // was PendingDotAccess
      mutable defaults  : DefaultSet[] }  // was Defaults
```

`find` / `union` / `resolveStep` / `zonk` become array operations over `parent`. Payload
lives **only under the representative id**.

⟨OPEN A⟩: Needs more design. Consider an append-only store for the scope/lifetime of a single file.

### 3. Payload as join-semilattices (the e-graph lesson)

Make each deferred-payload family a value with an associative/commutative/idempotent
**join**, so `union` payload handling is `store.x[root] <- join x[a] x[b]` — one
order-independent line — replacing the bespoke per-family `migrateBounds` logic. Union = set
union. **"Resolved" stops being a shared mutable flag**: a solved bound is recorded in a
separate `solved` side table rather than flipped in place, which kills the by-reference
`MemberSignature.Resolved` aliasing (a correctness win in its own right).

⟨OPEN B⟩ `dischargeConstraints` (`Engine.fs:796/873/890`) today writes back a *remainder*
(removes satisfied constraints). Grow-only sets + a `solved` side table keep merges monotone
and order-independent; the alternative is to keep consumption. **Lean: grow-only + `solved`
table.**

### 4. Interning + result caching

Hash-cons resolved `SemType`s so structural equality is identity, and memoize the hot
read-only queries keyed by id: `zonk` results, `matchTypes`/`subsumes` verdicts, member
lookups (`TypeRegistry.tryClassByKey`, external `TryLookupMember`). Invalidate a cache entry
only when a var it depended on grounds — tractable because the mutable side is now a small,
id-addressable set, not "somewhere in the object graph". **This memoisation is load-bearing,
not cosmetic, for chained constrained `inline` (F#+):** that pattern re-dispatches the same
SRTP shapes across many inline instantiations and is the canonical case of SRTP compile-time
blow-up, so caching the dispatch verdicts across instantiations is what keeps it from going
exponential in inline depth (`codegen-by-key-plan.md` → "Future: chained constrained
`inline`"). ⟨OPEN F⟩ how fine-grained the
dependency tracking needs to be (per-var vs per-generation stamp) — start coarse (a global
"substitution generation" counter; bump on any `union`/solution write; caches carry the
generation they were computed at). **Lean: generation stamp first, refine only if profiled.**

## Deferred (only if reversible speculation is ever needed)

Not built now. Recorded so the shape is known if a future feature forces it: a watermarked
undo log (a DU of cell-writes, `(cell, id, old)`, not closures) routed through a single store
seam, giving O(writes-in-trial) rollback — or the semi-persistent (Conchon–Filliâtre)
persistent-array variant, whose advantage is that the versioning seam is *enforced by the
data structure* rather than by audit. ⟨OPEN D⟩ mutable+trail vs semi-persistent — **defer
the choice** until there is a concrete rollback requirement; the id-arena is compatible with
either, and picking now would be speculative.

## Integration path (each step independently landable + green)

Staged so the pass builds and tests pass after every step; no big-bang swap. Steps are a
pure substrate refactor — **no dispatch-semantics change** (the suspension gate and
support-set candidates belong to the disjunctive-dispatch work in `codegen-by-key-plan.md`,
and can land before, after, or independently of this).

1. **[LANDED] Add `Id` to `TypeVar`, mint from a store counter** — purely additive, no behavior
   change. Introduce `TypeStore` holding the counter + empty arrays. Proves the arena
   allocation seam.
2. **[LANDED — folded into 3] Route every `TypeVar`-mutation site through a store accessor.**
   The intended standalone form — make the node's setters `private` so the compiler enforces the
   single seam — is **not achievable** in this single-assembly, no-`.fsi` codebase (see the
   step-2 finding above); the seam becomes compiler-proven only as each slot is *deleted*. So this
   landed per-family, inseparable from step 3.
3. **[LANDED] Move one payload family at a time off the node into a store array**, starting with
   `SrtpBounds` (smallest), behind existing accessor names, with a join on union replacing
   its `migrateBounds` arm. Delete that node slot. Repeat for `Constraints`,
   `PendingDotAccess`, `Defaults`, then `Link`/`Units`/`Level`/`Parent`/`Rank`. After each,
   `migrateBounds` shrinks by one arm and eventually vanishes. (`Constraints`/`Defaults` kept
   consumption; `Srtp`/`Pda` went grow-only + `solved` — see ⟨OPEN B⟩ note above.)
4. **[REMAINING — do with 5, behind a benchmark] Intern resolved `SemType`s** (hash-cons) — structural equality becomes identity.
   Independently valuable; unlocks step 5. **Two-level home — the intern pool is NOT the
   arena kept longer.** The mutable arena (`parent`/`rank`/`level`/`solution`/`units` + pending
   payloads) is strictly per-file and discarded at `Freeze`: every metavar is resolved and frozen
   away (post-`Freeze` a metavar is unrepresentable), and the next file thaws the frozen
   signatures it needs into a *fresh* arena via `Inline.freshen`. Nothing in the arena is
   invariant to the next file — the metavar-free `FrozenType` is deliberately the whole cross-file
   contract. The intern pool, by contrast, holds the *immutable* side (ground resolved `SemType`s,
   generalized `TypeScheme`s, intrinsic identities, thawed external signatures), which every
   referencing file re-thaws/re-walks identically — so it hangs at **compilation scope** (on / beside
   the cross-unit `IExternalSymbolProvider`, `PassContext.fs:551`), not on the per-file `PassContext`,
   making cross-file structural equality a pointer compare and avoiding re-thawing the same signature
   per file. **Hard constraint: only ground (metavar-free) types may be shared cross-file** — a value
   still holding a per-file `TypeVar` handle must stay per-file, or one file's arena identity leaks
   into the next. Natural shape: compilation-scoped intern pool of ground types + a per-file scratch
   for in-flight (non-ground) types. Refines ⟨OPEN A⟩ — the intern table's home is the compilation
   scope; this decides nothing for steps 1–3 (the per-file arena is right regardless).
5. **[REMAINING — the pause point] Add result caching** (`zonk`, `matchTypes`/`subsumes`, member lookups) keyed by id, with
   generation-stamp invalidation (⟨OPEN F⟩). This is where the profiled perf win should land;
   gate it behind benchmarks so a cache that doesn't pay is not kept.
6. **[LANDED — with step 3] Delete `MemberSignature.Resolved`** and its by-reference sharing once step 3 makes it
   dead — correct-by-construction: no shared mutable dedup flag survives.

Steps 1–3 + 6 landed and are payload-preserving; the single mutation seam became
compiler-proven per-family as slots were deleted (step-2 finding). Steps 4–5 are the
performance payoff and must be benchmark-gated — do them together.

## Open decisions

- ⟨OPEN A⟩ thin-handle `TypeVar {Id}` vs raw `SemType.TyVar of TyVarId`. **RESOLVED: raw id.**
  The handle bought nothing once the arena existed — it was a heap box around a 4-byte int plus a
  parallel `Node` array of pure indirection, and the `HashIdentity.Reference` sites it "kept
  working" were simpler and faster as structural int keys. Collapsed to `SemType.TyVar of TyVarId`;
  a `Rep` type recovers the one thing reference identity gave for free (root-vs-non-root
  discipline) as a compile-time gate. The arena's *home* is settled (per-file `PassContext`); the
  intern pool's home is separate and compilation-scoped — see step 4's two-level split.
- ⟨OPEN B⟩ grow-only sets + `solved` table vs trailed consumption. **RESOLVED (mixed):**
  `Srtp`/`Pda` grow-only + `solved`; `Constraints`/`Defaults` kept consumption (struct/no-ref-identity
  and wholesale-clear respectively) — full grow-only for those belongs to Phase B's solver.
- ⟨OPEN D⟩ trail vs semi-persistent — **defer; no rollback requirement in scope.**
- ⟨OPEN E⟩ does `Region` ever need special handling, or is it write-once? **RESOLVED:**
  write-once — it landed as a plain store array cell (`store.Region`/`SetRegion`), NOT folded
  into the union join, and (post-`Rep`) stays keyed by raw `TyVarId` rather than `Rep`: it is not
  migrated on union, so every node has one valid cell and a non-root read is legitimate.
- ⟨OPEN F⟩ cache-invalidation granularity: global generation stamp vs per-var dependency.
  **Lean: generation stamp first.**

## Perf notes / risks

- **Preserve Rémy levels** — `Level` moves to a store array but keeps min-on-union +
  occurs-lowering semantics. Generalization perf must not regress.
- **Preserve near-constant `find`** — path compression on the `parent` array (no rollback to
  fight it now that trials are out of scope).
- **Dense arrays grow** — amortized doubling; ids never reused within a pass.
- **Reference-identity call sites** (formerly `HashIdentity.Reference`, e.g.
  `EngineCore.mkNamedTypeSubst`, `InferOverload.TrialBindings`) — **DONE:** all flipped to
  structural `TyVarId` keying when the handle collapsed; no metavar `HashIdentity.Reference`
  remains (the one surviving reference-keyed set, `DischargeTable.solved`, keys the obligation
  *items*, not vars — intrinsic to the item and untouched).
- **Caching is the sharp edge** — a stale cache entry is a correctness bug, so step 5 must be
  gated on both benchmarks (does it pay?) and invalidation tests (is it sound?). If a cache
  can't be shown to pay, it doesn't land; the arena + interning stand on their own.
