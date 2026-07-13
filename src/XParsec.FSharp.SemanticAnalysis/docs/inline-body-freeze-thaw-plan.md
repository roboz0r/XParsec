# Freezing the inline-body channel — remaining phases

*Prerequisite to [multi-file-compilation-units-plan](multi-file-compilation-units-plan.md).*

**Phases 0 and 1 have LANDED**, and the code is their canonical record. The inline-body
channel now speaks `FrozenType` end to end: `Freeze.run` publishes the unit's inline
vocabulary (`TastFileG.InlineBodies`) under a `SymbolKey` it MINTS from the binding's
declaring module chain; the provider folds a published body onto the resolved entry that
owns that key (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`, reached via
`IExternalSymbolStore.TryLookupByKey`); and `Inline.thawBody` is the single
immutable→mutable transition, on the consumer's side, minting fresh cells by construction.
Read those five for the WHY — it is written where it is enforced.

What is still scoped here is the tail below: nested types, Phase 2, Phase 3, Phase 4.

## Carrier constraint (cross-link, not restated)

Frozen inline bodies still **never live in the runtime artifact** — no F#-style embedded
`FSharpOptimizationData` resource in the `.dll`. Canonical statement and rationale:
[publishing-format-plan](publishing-format-plan.md) §"Runtime artifact ≠ distribution
package". Freezing changes the body's *shape* (`SemType` → `FrozenType`), not its
*carrier*: intra-assembly a live in-memory `Frozen.TDecl` for the one build;
inter-assembly it rides the target-neutral sidecar/source channel exactly as now.

## Standing constraint the later phases must not break

**No `SemType` re-widening.** `InlineBody.Decl` is a `Frozen.TDecl`, and nothing on
`IExternalSymbolProvider` returns `SemType` except the sibling `Instantiate` closures —
which Phase 2 retires. Do not add a `SemType` escape hatch to make a splice site
convenient; `Inline.thawBody` is the seam, and it is the consumer's.

(The three constraints that governed the thaw itself — one shared freshener cache per
thawed decl across all three axes; `FTLocalTypar` is not a catch-all; its `binder` is
body-relative — are now enforced at their sites: `Inline.thawBody`, `Freeze.freezeTy`, and
`FrozenType.FTLocalTypar`'s own doc-comment.)

## Phasing

- **Nested types (enabled by Phase 0's containment chain).** Make a module-held type a real
  CLR nested type — mint `TypeHolder.InModule`, give `typeMetaName` its `+` arm, move the
  `…Module`-suffix rule out of `Elaborate` into the key, and add NestedClass emission to the
  CLR backend. This is a **change to the emitted binary**, not a key-hygiene refactor, which
  is why it was NOT folded into Phase 0. Ground truth from a PE this repo built: `namespace
  N` + `module M` + `type T` emits a TOP-LEVEL TypeDef `N.T` — the module is not folded into
  the namespace, it is DISCARDED — and the module's own holder class is emitted as a SIBLING.
  The NestedClass table has zero rows and the backend has no code that writes one. So the
  flattening is a latent correctness bug, not untidiness: `module A = type T` and `module B =
  type T` in one namespace are two distinct types with ONE identity. Land it as its own
  commit, against a test that pins that collision.
- **Phase 2 (separate, enabled):** retire the `ExternalSymbol.Instantiate` closures onto
  `instantiateWith`, making `IExternalSymbolProvider` `SemType`-free in full.
- **Phase 3 (independent of 0–2; do whenever):** narrow the key-typed fields and
  interface parameters that are *always* one case. The reshape proved the trade on
  `MemberKey.Decl : TypeKey` (it deleted three `failwithf` arms); it stopped one level
  out, so ~25 impossible-arm fallbacks remain. All mechanical:
  - `ExternalMember.Key : MemberKey` (always is one). Deletes the re-narrowing at
    `InferOverload`, `InferApp`, `InferExternalCall`, `VesperLib`, and the five surviving
    `failwithf "… is not a MemberKey"` in `ClrExternalMembers` / `EmitCall` / `EmitMember`.
  - `ExternalSymbol.Key : BindingKey`; `IntrinsicIdentity.Canon` / `IntrinsicInterfaceShape.Canon` /
    `IntrinsicReverseCanon` : `TypeKey` (kills the unreachable arm in
    `ExternalSymbolProviders.stampType`).
  - `ICodegenProvider.ExternalMemberRef` / `ExternalMemberRefOn` / `ExternalFieldRef` /
    `TryCapabilityBaseMemberKey` take a `MemberKey`; `ClrEnv.externalClassRef` /
    `externalRecordRef` / `externalUnionRef` / `externalIsValueType` / `LookupTypeByKey`
    and `TypeRegistry.try*ByKey` / `IExternalSymbolStore.TryLookup*` take a `TypeKey`.
    Narrow ONCE at the type-IR boundary instead of at every use. (The `_` arms that
    *fabricated* a `TypeRef` are already fixed; these deletions remove the arms entirely.)
  - Side tables typed wider than their only writer: `TypeRegistry.SymbolKeyOrigins`,
    `PassContext.ResolvedType`, `TypeRegistry.IntrinsicKeys`.
  - Promote asm-blind `TypeKey` equality (`RuntimeNames.sameTypeAsmBlind`, currently
    `private`) into `SymbolKeyOps` and route the three hand-rolled string compares through
    it: `EmitResolve.fs` (a *correctness gate* — `ExternalMemberRefOn` vs
    `ExternalMemberRef`), `EmitClosures`' `HashSet<string * string>` (→ `HashSet<TypeKey>`),
    `Unification.fs`'s `qualifiedName k = qualifiedName info.Key`.
  - Drop `IInterfaceImplHost.TypeKey` (no interface-level consumer; callers use the
    concrete info's member).
- **Phase 4 (independent; do whenever):** make `ICodegenSymbols` key-addressed. Today
  `ClrExternalMembers` renders a `TypeKey` to a metadata name, hands it across
  `ICodegenProvider.TryLookupType/TryLookupMember : string -> …`, and `CodegenSymbols`
  immediately **re-parses it back into a `TypeKey`** (`lookupKeyOfCompiledName`), dropping
  the `Origin` in transit — the "asm-blind by design" note is a description of what the
  round-trip loses. Key-addressing the type/member faces retires
  `lookupKeyOfCompiledName`'s codegen consumer; combined with Phase 0 + `declTypeKey`
  (already structural), it leaves `typeKeyOf`'s `+`-parser with **no producer at all**,
  which is the correct end state — `+` becomes a pure rendering concern.
- **Not scheduled — `SemType`/`FrozenType` nominal payloads carry `TypeKey`.** The IR's
  `TyClass`/`TyRecord`/`TyUnion` / `FTClass`/… still carry a `SymbolKey` where only a type
  is possible, costing three narrow-and-fail-loud sites (`Elaborate.Resolve.nominalDeclKey`,
  `Inline.nominalHeadKey`, `EmitResolve.nominalTypeKey`). Mechanical but ~1300 sites; gated
  on size, not design.
- **Not scheduled — `SymbolKeyOps` API shape.** The `Of` suffix means "returns the narrow
  record" in `typeKeyOf`/`bindingKeyOf`/`memberKeyOf`/`moduleKeyOf`/`externalTypeKeyOf` and
  "returns `SymbolKey`" in `valueKeyOf`/`qualifiedTypeKeyOf`; the collision forced
  `qualifiedTypeKeyOfT` into existence. One convention (narrow constructors named for what
  they return) deletes five `SymbolKey`-returning pass-throughs and `…OfT` with them. Do it
  when Phase 3 is already touching these call sites, not as its own churn.
- **Not scheduled — generic closures.** Emptying `FTLocalTypar` of population (so it
  could become a hard error everywhere) means lifting a locally-generalized binding to
  its own typar axis, the way F# emits `f<'a,'b>` plus a generic closure class
  `g@2T<'c>`. It is **not** "project the roots onto the enclosing method's axis" — F#
  pointedly does not do that, as it would change `f`'s ABI. A codegen-representation
  change (we box via `Vesper.Fun` today), and not needed for the identity fix.

