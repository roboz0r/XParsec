# Frozen tree carries a SemType residue: `GeneralizedTypars` → FrozenType-domain rewrite

Status: design / deferred. Ephemeral plan doc — delete once the work lands.

## The residue

Freeze is meant to be the cut point where `SemType` stops being the currency and
`FrozenType` takes over (`Freeze.run` = the single `SemType → FrozenType` rebuild, and
the doc claims "after this the assembly's output tree holds no `SemType.TyVar`"). One
field breaks that claim:

- `TTypeMemberG.MethodTypeParams : GeneralizedTypars` (`Tast.fs:892`).

`GeneralizedTypars` (`GeneralizedTypars.fs:57`) is `private (string * TypeVar)[]` — it
holds live union-find **`TypeVar`** cells, a `SemType`-domain construct. It is **not**
parameterized over `'ty`, so the *frozen* instantiation `TastFileG<FrozenType, SyntaxToken>`
carries the exact same cell-bearing type as the `SemType` instantiation. So the "frozen"
tree is not actually `SemType`-free: a residual `SemType` carrier rides it, and it
**crosses the SemanticAnalysis boundary** into the backends —
`Codegen.Common/SymbolProviders.fs`, `Codegen.Clr/NominalEmit.fs`, `Codegen.Clr/Layout.fs`
all read `GeneralizedTypars` off the frozen member.

(Contrast `TAbstractMethodG.MethodTypeParams : EqArray<string>` (`Tast.fs:1003`) — the
abstract-method carrier is already names-only and has no residue. The rewrite makes the
concrete-member carrier match it.)

## Why it matters now

Two concrete symptoms, both surfaced by the frozen-cache work:

1. **The frozen serializer can't faithfully round-trip a non-empty carrier.** Live
   `TypeVar` cells cannot survive a byte round-trip, so `FrozenCodec` serializes only the
   names (in their already-canonical order) and rehydrates via
   `GeneralizedTypars.unsafeOfNames`, minting fresh roots. That is sound *only* because
   post-freeze consumers read exclusively `names` + `count` (`GeneralizedTypars.fs`) — the
   cells are dead. But `unsafeOfNames` is a hole poked in the type's correct-by-construction
   guard (`canonical` was meant to be the sole builder), and it exists solely to paper over
   this residue.

2. **A `thaw ∘ flatten = structural identity` gap.** Because the rehydrated cells are
   fresh, a *non-empty* `MethodTypeParams` would not compare structurally equal after a
   round-trip. It is inert **today** only because the JS conformance corpus declares **zero
   generic members**, so every `MethodTypeParams` is `empty` and the path is never
   exercised. A single generic member would expose it.

## The fix (deferred)

Give freeze a **FrozenType-domain carrier** for a member's own method typars — names +
arity, no live cells (a plain `EqArray<string>`, or a small named record if a per-typar
constraint ever needs to ride along). Freeze projects `GeneralizedTypars.names` /
`count` into it, exactly as it already projects everything else `SemType → FrozenType`.
Then:

- `TTypeMemberG`'s frozen face carries no union-find cell → the frozen tree is genuinely
  `SemType`-free, and no backend sees a `SemType` construct.
- `FrozenCodec` (de)serializes it as ordinary data — no fresh-cell rehydration, so the
  round-trip is truly structural and the corpus gap closes.
- `GeneralizedTypars.unsafeOfNames` is **deleted**, restoring `canonical` as the sole
  builder of the analysis-domain type.

### Steps

1. **Audit the backend reads.** Confirm `SymbolProviders` / `NominalEmit` / `Layout` read
   only `names` + `count` off the frozen member (expected — the cells are dead post-freeze).
   Any read of a `TypeVar` cell off a *frozen* member is itself a latent bug to fix here.
2. **Add the frozen carrier type** (e.g. `TTypeMemberG` gains a `MethodTypeParamNames`
   in the frozen projection, or `MethodTypeParams` becomes `'ty`-neutral names-only). Keep
   the `SemType` instantiation's analysis needs (`canonical` / `methodEnv` / `refreshRoots`)
   intact — those run pre-freeze and legitimately need the cells.
3. **Freeze projects** names/arity into the new carrier in `TastConvert.file` / `Freeze.run`.
4. **Migrate the backend consumers** to the new carrier.
5. **Drop** `GeneralizedTypars.unsafeOfNames` and its `FrozenCodec` use; simplify the
   member codec to plain data.
6. **Extend the round-trip corpus** with a generic member so the (now truly structural)
   round-trip is actually exercised.

## Scope

This is a **freeze-representation** change, distinct from the frozen-SoA-cache work
(`frozen-soa-cache-plan.md`) that surfaced it. It is not a prerequisite for that plan —
the cache ships correctly today via the `unsafeOfNames` workaround — so it is deferred and
tracked here. It naturally belongs with, or just before, the Phase-B pool rewrite, where
the frozen member representation is being reshaped anyway.
