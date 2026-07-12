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

Freeze still **drops** inline decls from the emittable `Decls`. Making it publish them
is Phase 1.

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
dotted string and looks it back up — to recover a `SymbolKey.ValueKey(asm, ns, name)`
built from that same triple. Its own doc-comment concedes the failure mode: *"a wrong
reconstruction simply misses (`ValueNone`), never mis-keys."* It works today only
because the cross-package path extracted the package's contract from its `.fsi`, so the
symbol already exists in a provider for the name to hit.

**Multi-file has no `.fsi` per file.** File N's provider is built from N's compiled
unit, and freeze drops inline decls — so there is nothing for the name to recover
against. So:

- Freeze **mints** `ValueKey(asm, ns, name)` for each inline value directly from
  `ModuleMembers`, and publishes it in the unit's symbol vocabulary.
- `ValueInlineBody.Key : SymbolKey voption` collapses to a real `SymbolKey`, and the
  did-the-lookup-hit branch stops existing.
- **Check first:** `qualifiedValueName` concatenates *three* parts but `ValueKey` has
  *two* slots (`ns`, `name`), so confirm how module values fold `Holder` into `ns`
  elsewhere before minting. Minting a key that disagrees with the resolver's would
  trade an honest miss for a silent mis-key — strictly worse.

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
- Fold the body onto the resolved entry rather than a separate keyed lookup: the head
  that resolves a symbol *is* the identity-correct entry, so an optional **lazy**
  `Frozen.TDecl` field on `ExternalSymbol` / `ExternalMember` retires **both** carriers
  — `TryLookupInlineBody` (`ExternalSymbols.fs:886`) and `MemberInlineBody`
  (`SymbolProviders.fs:42`, whose only reason to be separate, key-agreement at store
  time, is satisfied for free by the entry carrying its own minted key). Lazy so a
  consumer that never splices an inline pays nothing.

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

- **Phase 1 (this plan):** freeze-inline, minted inline `SymbolKey`s, the decl-scoped
  thaw glue, `InlineBody.Decl : Frozen.TDecl`, the interface fold, the two `Inline.fs`
  TODOs deleted. No change to splice semantics — same lowering, fresh cells.
- **Phase 2 (separate, enabled):** retire the `ExternalSymbol.Instantiate` closures onto
  `instantiateWith`, making `IExternalSymbolProvider` `SemType`-free in full.
- **Not scheduled — generic closures.** Emptying `FTLocalTypar` of population (so it
  could become a hard error everywhere) means lifting a locally-generalized binding to
  its own typar axis, the way F# emits `f<'a,'b>` plus a generic closure class
  `g@2T<'c>`. It is **not** "project the roots onto the enclosing method's axis" — F#
  pointedly does not do that, as it would change `f`'s ABI. A codegen-representation
  change (we box via `Vesper.Fun` today), and not needed for the identity fix.

## To verify during implementation

- Enumerate every reader of `TryLookupInlineBody` / `MemberInlineBody`
  (`ExternalSymbolProviders.fs:81,:324,:478,:534`; `Elaborate.fs:1883`;
  `MetadataSymbols.fs:812`) and confirm each obtains the identity-correct entry, so the
  folded lazy field serves it (operator/desugared heads reach it via
  `TryLookup name → .InlineBody`).
- Confirm the nullary intrinsic body special-case (`Inline.nullaryIntrinsicValueBody`;
  spliced at `InlineExpansion.fs:783`) survives freeze — an operand-less `(# … #)` body
  with no typars is the identity case and must thaw to itself.
- Confirm `ParamAttrs` (the `[<CallAtMostOnce>]` channel) is domain-neutral and rides
  unchanged.
- Once the splice lands, add the two cross-unit tests the prerequisite work could not:
  a real cross-file **splice** over colliding `NodeKey`s, and freeze-in-A / splice-in-B
  ≡ in-unit splice. `instantiateWith` consults no ambient unit state, so both are small
  additions to `InlineFreezeThawSpikeTests`.
