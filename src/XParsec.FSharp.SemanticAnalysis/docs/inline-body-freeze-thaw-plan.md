# Freezing the inline-body channel — design plan

*Prerequisite to [multi-file-compilation-units-plan](multi-file-compilation-units-plan.md);
lands before it.*

## Problem

`IExternalSymbolProvider` is meant to be a frozen-domain oracle — every symbol it
hands a consumer is immutable `FrozenType`, so a consumer's inference can never reach
back and mutate a producer's inference state. One member still breaks that: the
inline-body channel hands out **`SemType`**.

```fsharp
// ExternalSymbols.fs:776
type InlineBody = { Decl: TDecl; ParamAttrs: ParamAttrs[] }   // TDecl = TDeclG<SemType,_>

// IExternalSymbolProvider  (ExternalSymbols.fs:886)
abstract TryLookupInlineBody: key: SymbolKey -> InlineBody voption
```

Every type annotation in a `TDecl` is a `SemType`, and a `SemType.TyVar` is a mutable
`UnionFind` cell — so the provider leaks live inference cells across the boundary.
Tolerable today only by accident of *where* the cells come from: cross-package inline
bodies are re-parsed from shipped `.fs` source into extraction-fresh `SemType`, not
another live compilation's tree.

That accident does not survive multi-file. There, file N's frozen unit becomes a
provider that file N+1 resolves against, and N's inline bodies are its **own live
`SemType`**. `Inline.substType` / `resolveTraitCall` / `substExpr` are end-to-end
`SemType` and would `UnionFind.union` into N's cells — backward flow (N+1 → N), the
thing the assembly-is-linear model forbids, prevented today by discipline rather than
by construction.

**Root cause is age, not design.** `IExternalSymbolProvider` predates `FrozenType`.
Every other channel has since migrated to frozen templates realized through one seam
(`instantiateWith`); the inline channel never did, because inline bodies were exempted
from freeze entirely — so there was no `Frozen.TDecl` to hand out. This plan pays that
down: the provider speaks only `FrozenType`, and the immutable→mutable transition
happens exactly once, on the consumer side, minting fresh cells by construction.

## Landed: the two prerequisites

Both are in the tree; the code is their canonical record, so it is not restated here.

1. **`FrozenType.FTLocalTypar of binder: NodeKey * index: int`** — the identity-bearing
   leaf for a typar bound by a body-local `let`'s own scheme (see its doc-comment in
   `SemanticInfo.fs`, and `Freeze.schemeBinders` for the attribution). `instantiateWith`
   gained a third policy, `localTypar`, alongside `declaring` and `methodVar`. This
   replaced `FTUnknown "?free-typar"`, which name-equated every such root and so
   conflated distinct typars across the round-trip.
2. **`TStaticOptConstraintG<'ty>`** — the static-opt clause constraints are now
   `'ty`-generic and mapped by `TastConvert.constraintOf`, closing the raw-`SemType`
   hole that would otherwise have smuggled a live cell through a frozen clause.

Also landed, out of the `SymbolKey` reshape's review (the code is their record):
`SymbolOrigin` no longer carries a `DeclaringType` string — a member's declaring type is
`MemberKey.Decl : TypeKey`, and an origin names a *place*, not a containment;
`MetadataSymbols.declTypeKey` reads the holder chain off `Type.DeclaringType` instead of
cutting `FullName` on `.` and `+`; and `ClrEnv.externalRecordRef` / `externalUnionRef` no
longer fall back to a fabricated `(ns = "", name = <whole dotted name>)` `TypeRef` on a
non-type key.

Freeze still **drops** inline decls from the emittable `Decls`. Making it publish them
is Phase 1 — gated on Phase 0 below.

## Phase 0 — finish the `SymbolKey` containment chain (BLOCKS Phase 1)

`SymbolKey` became a containment chain (assembly → namespace → module\* → type → member).
For `TypeKey` that is real: `ClrEnv.externalClassRef` walks `TypeHolder.InType`, and
`MetadataSymbols.declTypeKey` reads the chain off `Type.DeclaringType`. **For
`ModuleKey` / `BindingKey` it is not.** `SymbolKeyOps.moduleKeyOf` always mints
`Holder = InNamespace`, taking the last dotted segment as the module name, so:

- `ModuleHolder.InModule` never wraps a nested `ModuleKey` — nested modules stay
  flattened into the namespace path, and the doc-comment on `TypeHolder.InModule`
  claiming *"`ModuleHolder.InModule` already has producers"* for them is false.
- `ClrRecipes.externalModuleRef` must therefore **recover** the module chain by
  subtracting a blanket package namespace from the key's namespace path with a
  segment-prefix test. Its own comment concedes it: *"Where the namespace ends and the
  module chain begins is the ORIGIN's fact, not the key's."* When the prefix test fails,
  `dropped = 0` and every namespace segment is emitted as a nested-module `TypeRef` — a
  ref that does not bind, emitted silently.
- The same lossy mint reappears wherever a binding key is built from a flat string:
  `TsManifestProvider.stampValueSymbol` gives one `ExternalSymbol` an `Origin` whose
  namespace and a `Key` whose namespace **disagree**; `JsExternalMembers.erasedGroupingRef`
  round-trips a `TypeKey` through `(asm, dotted-ns)` to build a sibling binding;
  `ExternalSymbols.restampKey` re-derives the whole holder chain from a rendered string
  just to change the home assembly at its root.

This is the last of the string-flattening, and it is what Phase 1's key-minting step
would be minting *against*.

**The move.** `bindingKeyOf` / `moduleKeyOf` take a **`ModuleHolder`**, not a flat
dotted string. Most producers already know the segments — a TS `export namespace`, a
manifest's `(Namespace, Holder)` — and today throw them away at the boundary. NOTE
`ModuleMemberInfo` is NOT one of them: it is `{ Namespace: string option; Holder: string;
Name: string }`, as flat as `bindingKeyOf`'s input, so it must be reshaped too — an
unstated dependency of this phase. Then:

- `externalModuleRef` becomes the mirror of `ClrEnv.typeRefOf`: a 5-line recursive walk
  over `ModuleHolder`, no `metaNs` parameter, no prefix test, no `dropped` fallback
  (~29 lines → ~5, in a 1075-line file).
- `restampKey` becomes a structural `SymbolKeyOps.reroot : Origin -> SymbolKey ->
  SymbolKey` that rewrites the `Origin` at the root of the chain — total over all three
  key kinds, so its `| _ -> k` arm goes too.
- `TsManifestProvider`'s two conventions collapse to one, and the origin/key
  disagreement is unrepresentable.

**The `TypeHolder.InModule` producer is NOT part of this phase** — it was scoped in here,
and it does not belong. It is on the TYPE axis, which nothing in Phase 1 touches, and it
is not a key-hygiene refactor but a **change to the emitted binary**. Ground truth, from
the metadata of a PE this repo built:

- A module-held type does not merely get its module *folded into the namespace* — the
  module is **dropped entirely**. `namespace N` + `module M` + `type T` emits a top-level
  TypeDef `Namespace = N`, `Name = T`. (`CstWalk.walkModuleTreeWith`: *"A module is a
  holder, not a namespace segment, so `declNs` passes through unchanged"*.) The module's
  own holder class is emitted as a **sibling** of the types declared inside it. The
  NestedClass table has zero rows; the backend has no code that writes one.
- So the doc-comment on `TypeHolder.InModule` in `SemanticInfo.fs` is wrong twice over: it
  claims the module is folded into the namespace path (it is discarded), and it claims
  `ModuleHolder.InModule` "already has producers" (it does not — that is this phase's
  item 1).
- `SymbolKeyOps.typeMetaName` has **no `InModule` arm** — the case falls into a `| _ ->`
  wildcard and renders the bare name, so an `InModule` key renders `N.T` today, silently.
  Minting `InModule` while leaving the renderer alone would therefore be a pure refactor;
  rendering it as `+` is what changes the emitted bytes, and needs NestedClass emission
  plus the `…Module`-suffix rule moved out of `Elaborate` into the key.
- **The flattening is a latent correctness bug, not just untidiness:** `namespace N` with
  `module A = type T` and `module B = type T` are two distinct types with one identity.

`+` (real CLR nesting) is the correct end state, and it lands as **its own commit**,
against a test that pins the collision above.

**Verify:** `SymbolKeyTests` now pins the blanket-origin mis-cut, the nested-type
render/parse round-trip, the `moduleFullName`/`moduleKeyOf` round-trip, and the
unqualified-binding holder. Phase 0 must keep all four green and add the nested-module
case (`moduleFullName` of a hand-built `InModule` chain), which no producer can currently
mint.

## Phase 1

### Producer: freeze-inline

Per inline `TDecl.Let(isInline = true)`:

1. **Quantify** its decl-type typars to `FTTypar` leaves, via the existing
   `Elaborate.remapDeclTypars` / `mkMethodQuantEnv` cut. Body-local scheme roots are
   *not* covered by that cut and do not need to be: they freeze to `FTLocalTypar`.
2. **Freeze** the body. `StaticOptimization` / `TraitCall` / `TStaticOptClause` flow
   into their already-present frozen arms, constraints included.
3. **Publish** as `InlineBody.Decl : Frozen.TDecl`. `collectInlineBodies`
   (`SymbolProviders.fs:140`) reads the frozen unit rather than the pre-freeze SemType
   tree; the member twin (`MemberInlineBody`, `:42`) freezes the same way.

### Producer: give inline values a real `SymbolKey`

Freeze currently conflates two questions into one filter, and gets the second wrong:

- *Is this decl **emittable**?* — inline: **no**. It stays dropped from `Decls`.
- *Is this decl part of the unit's **exported vocabulary**?* — inline: **yes**. Freeze
  answers "no" by dropping it entirely, and that is the bug.

Today an inline body's key is **reconstructed, not owned**:

```fsharp
// SymbolProviders.fs — ValueInlineBody.Key : SymbolKey voption
Key = ctx.Resolver.TryLookup(qualifiedValueName info) |> ValueOption.map (fun s -> s.Key)
```

`qualifiedValueName` flattens `ModuleMemberInfo`'s `(Namespace, Holder, Name)` into a
dotted string and looks it back up — to recover a `BindingKey` built from that same
triple. Its own doc-comment concedes the failure mode: *"a wrong reconstruction simply
misses (`ValueNone`), never mis-keys."* It works today only because the cross-package
path extracted the package's contract from its `.fsi`, so the symbol already exists in a
provider for the name to hit.

**Multi-file has no `.fsi` per file.** File N's provider is built from N's compiled
unit, and freeze drops inline decls — so there is nothing for the name to recover
against. So:

- Freeze **mints** a `BindingKey` for each inline value directly from `ModuleMembers`,
  and publishes it in the unit's symbol vocabulary.
- `ValueInlineBody.Key : SymbolKey voption` collapses to a real `SymbolKey`, and the
  did-the-lookup-hit branch stops existing.

**BLOCKED ON PHASE 0.** The old "check first" caveat here — *`qualifiedValueName`
concatenates three parts but the key has two slots, so confirm how module values fold
`Holder` into `ns` before minting* — is not a caveat to discharge by inspection. It is
Phase 0's defect: `BindingKey.Decl` is minted from a FLAT dotted string
(`SymbolKeyOps.bindingKeyOf`), whose last segment becomes the module and whose prefix
becomes the namespace. So the three-part `(Namespace, Holder, Name)` fact is destroyed at
the mint and guessed back at every consumer. Minting an inline value's key against that
would be minting against a lossy encoding — exactly the "trade an honest miss for a
silent mis-key" outcome this step must avoid. Land Phase 0 first; then `ModuleMembers`'
`(Namespace, Holder)` maps onto a `ModuleHolder` with nothing thrown away, and this step
is a direct construction.

Stated plainly, because it inverts the intuition: **an inline function needs a real
`SymbolKey` precisely *because* it never reaches codegen.** Every other symbol gets its
identity minted as a side effect of being emitted; inline values are the one kind that
never are, so theirs must be minted deliberately.

### Consumer: thaw at splice

4. `InlineExpansion` fetches the `Frozen.TDecl` and thaws it with
   `TastConvert.file (instantiateWith declaringFreshener methodFreshener localTyparFreshener)`
   and **one freshener cache shared across the whole decl**, so every occurrence of a
   given leaf — declared *or* local — maps to the same fresh consumer-owned cell.
   `Inline.substType` / `freshen` / SRTP-resolution then run **unchanged**.
5. **Delete the two stale `TODO(frozen-type Phase 2)` comments** (`Inline.fs`, on
   `quantifiedTypars` and `substType`). They say those functions must re-key from
   `TyVar` roots to `(axis, index)` "once freeze emits `TyTypar` for an inline binding's
   quantified typars". Under this plan they must **not**: thaw re-mints `TyVar` cells
   *before* the splice, so keying by root stays correct. That is why step 4 can say
   "unchanged" — but the comments assert the opposite, so a future reader will implement
   them for no reason.

### Interface

- `InlineBody.Decl : Frozen.TDecl` (was `TDecl`).
- Fold the body onto the resolved entry: an `InlineBody voption` field on
  `ExternalSymbol` / `ExternalMember` retires **both** carriers — `TryLookupInlineBody`
  and `MemberInlineBody` (whose only reason to be separate, key-agreement at store time,
  is satisfied for free by the entry carrying its own minted key). NOT lazy: the provider
  builds its symbols *from* the frozen unit, so the `Frozen.TDecl` is already in memory
  and a `Lazy` would defer work already done.
- **The fold needs a key-addressed symbol face — `TryLookupByKey : SymbolKey ->
  ExternalSymbol voption` — which replaces `TryLookupInlineBody` rather than merely
  deleting it.** An earlier draft had the splice sites re-reach the body by *name*
  (`TryLookup name → .InlineBody`). They cannot:
  - Only two sites consume an `InlineBody` (`InlineExpansion.fs`, the value/operator head
    and the member head). The value head has a `SymbolKey`, not a resolvable name:
    `collectInlineBodies` rewrites a harvested body's intra-body sibling refs to
    `TExpr.External` carrying the **simple** name, precisely because the simple name does
    NOT resolve (the index is qualified-name keyed, the holder is not auto-opened). That
    rewrite is why the by-key channel exists at all.
  - Re-resolving a spelling at splice time also reintroduces the user-shadow hazard the
    key channel was introduced to kill.
  - The standing objection to a key-addressed symbol face — *"a value key does not
    round-trip to its fully-qualified spelling"* — **is Phase 0's defect, and Phase 0
    repeals it.** Once a `BindingKey` is a real containment chain it renders back to its
    spelling losslessly, so the face is constructible: the store indexes its
    `ExternalSymbol`s by `Key`.
  - Bonus: the member head must re-select by key, not re-look-up by name. `TryLookupMember`
    collapses overloads to a single best-by-arity pick, so a name re-lookup can return a
    *different overload* than the one whose key is on the node. An exact key match cannot.

## Carrier constraint (cross-link, not restated)

Frozen inline bodies still **never live in the runtime artifact** — no F#-style embedded
`FSharpOptimizationData` resource in the `.dll`. Canonical statement and rationale:
[publishing-format-plan](publishing-format-plan.md) §"Runtime artifact ≠ distribution
package". Freezing changes the body's *shape* (`SemType` → `FrozenType`), not its
*carrier*: intra-assembly a live in-memory `Frozen.TDecl` for the one build;
inter-assembly it rides the target-neutral sidecar/source channel exactly as now.

## Design constraints

1. **One shared freshener cache per thawed decl**, across all three axes. A per-node
   cache would give two occurrences of the same typar independent cells and break the
   body's internal type links.
2. **`FTLocalTypar` is not a catch-all.** It is for a typar a local scheme legitimately
   quantified. A root no scheme quantified is a metavar leak, already an error-severity
   `ResolvedTypes` diagnostic, and degrades to `FTUnknown` — never a fabricated binder.
3. **`FTLocalTypar.binder` is body-relative.** Never a cross-file resolution key (those
   go by name — see the multi-file plan), never merged into a `NodeKey`-keyed side
   table. It is consumed at thaw.
4. **No `SemType` re-widening.** Once `InlineBody.Decl : Frozen.TDecl`, nothing on
   `IExternalSymbolProvider` returns `SemType` except the sibling `Instantiate`
   closures. Do not add a new `SemType` escape hatch to make a splice site convenient —
   thaw is the seam.

## Phasing

- **Phase 0 (blocks Phase 1):** finish the containment chain on the module/binding axis —
  `bindingKeyOf` / `moduleKeyOf` (and `ModuleMemberInfo`) take a `ModuleHolder`;
  `externalModuleRef` walks it; `reroot` replaces `restampKey`. See above.
- **Phase 1 (this plan):** freeze-inline, minted inline `SymbolKey`s, the decl-scoped
  thaw glue, `InlineBody.Decl : Frozen.TDecl`, the interface fold onto `TryLookupByKey`,
  the two `Inline.fs` TODOs deleted. No change to splice semantics — same lowering, fresh
  cells.
- **Nested types (independent of 0–2; enabled by Phase 0's chain):** make a module-held
  type a real CLR nested type — mint `TypeHolder.InModule`, give `typeMetaName` its `+`
  arm, move the `…Module`-suffix rule into the key, and add NestedClass emission to the
  CLR backend. Fixes the same-name-in-two-modules collision. See Phase 0 above for why it
  is not Phase 0.
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

## To verify during implementation

- ~~Enumerate every reader of `TryLookupInlineBody` / `MemberInlineBody`.~~ **Done.** Only
  TWO sites in the tree consume an `InlineBody`, both in `InlineExpansion.fs`: the
  value/operator head (via `lookupExternal`, keyed by `TExpr.External`'s `SymbolKey
  voption`) and the member head (keyed by `TExpr.ExternalMember`'s `SymbolKey`).
  Everything else is provider plumbing, a `ValueNone` stub, or a test. Both are served by
  `TryLookupByKey` + the folded field; neither can be served by name. See §Interface.
- Confirm the nullary intrinsic body special-case (`Inline.nullaryIntrinsicValueBody`;
  spliced at `InlineExpansion.fs:783`) survives freeze — an operand-less `(# … #)` body
  with no typars is the identity case and must thaw to itself.
- Confirm `ParamAttrs` (the `[<CallAtMostOnce>]` channel) is domain-neutral and rides
  unchanged.
- Once the splice lands, add the two cross-unit tests the prerequisite work could not:
  a real cross-file **splice** over colliding `NodeKey`s, and freeze-in-A / splice-in-B
  ≡ in-unit splice. `instantiateWith` consults no ambient unit state, so both are small
  additions to `InlineFreezeThawSpikeTests`.
