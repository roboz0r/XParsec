# Nested-type emission plan — a module-held type is a nested class

## Why

A type declared inside an F# `module` compiles to a class **nested inside the
module's static holder class**. Vesper models this correctly in the key algebra —
`TypeHolder.InModule` names the module's *compiled holder type* — and then loses it
everywhere else:

- `SymbolKeyOps.typeNestedName` (`../SymbolKeyOps.fs:161`) `+`-joins only
  `TypeHolder.InType`; `InModule` falls into a wildcard and the module is **dropped**.
  So `typeMetaName` is not injective: `N.A.T` and `N.B.T` both render `N.T`.
- The CLR backend writes **every** `TypeDef` flat. There is not one `NestedClass` row
  in any emitted assembly, and no code that writes one.
- The contract extractor — the one face that *knows* about modules — renders the module
  into a **dotted string** (`VesperLib.fs:772-787`), which a consumer re-mints through
  `splitLastDot` into `TypeHolder.InNamespace ["N"; "MModule"]`. The module is absorbed
  into the namespace path and the `InModule` holder is never reconstructed.

The consequence is that one type has several unequal `SymbolKey`s depending on who minted
it, under a design whose entire premise is that **`SymbolKey` equality is the single
identity test in the tree**. Nothing has broken yet only because the faces never meet:
project-local symbols are resolved before the provider is consulted, and the contract face
round-trips against itself. This plan closes it.

It also fixes two live bugs that fall straight out of the flattening:

- `diagnoseExternalClaim` (`../Passes/NameResolution/TypeRegistration.fs:235`) — the CS0433
  analogue — probes the provider with `typeMetaName key`. For a module-held type that is
  `N.T`, so it **false-positives** against any referenced type literally named `N.T` and
  **false-negatives** on the real module-held claimant.
- `MetadataSymbols` resolves by `asm.GetType name`, whose spelling for a nested type is
  `Ns.Outer+Inner`. Both current spellings of a module-held type (`N.T` from the renderer,
  `N.M.T` from the contract) fail to bind. Real cases already exist in the contract corpus:
  `OptimizedClosures.FSharpFunc<…>` and `ByRefKinds.Out/In/InOut`
  (`../../XParsec.FSharp.Lib/Clr/prim-types.fsi:1235,2312`).

## The model

**Identity is F#/Vesper semantics. IL is a target, not the source of truth.**

IL has no notion of a module. That is not a gap to be papered over — it means the two
symbol populations are genuinely different, and the key algebra is already right to
distinguish them:

| Source of a type's key | Holder minted | Correct because |
|---|---|---|
| **Vesper package** — `.fsi` contract + manifest | `TypeHolder.InModule` | the contract *is* the metadata channel (`publishing-format-plan` PF1/PF2/PF3), and it carries a real `ModuleHolder` chain |
| **Bare .NET assembly** — reflection | `TypeHolder.InType` | there are no modules there; a nested class really is a nested class |

So `InModule` ⇔ "Vesper metadata says a module holds this" and `InType` ⇔ "bare IL says a
class holds this". These are **non-overlapping populations, not two spellings of one
thing.** For a Vesper package the contract always answers — `publishing-format-plan` PF8
guarantees the `.fsi` is always a committed artifact — so reflection over our own emitted
IL is a *fallback*, and the `InType` it would produce is a degradation to be avoided, not
an identity to be reconciled.

Two options were considered and rejected:

- **Recover module-ness from IL** by emitting and reading `CompilationMappingAttribute`.
  Rejected: it makes IL authoritative for a fact IL cannot express, and it is unnecessary —
  the contract already knows. (We emit no such attribute today, and `declTypeKey`
  discriminates on `t.IsNested` alone.)
- **Collapse `TypeHolder.InModule` into `InType`** of the holder's `TypeKey`. Rejected: it
  destroys true facts — `ModuleKey` has *no arity* because modules are not generic, and a
  module holds modules *and* types, which a `TypeKey` does not model. It would be letting
  the target dialect dictate the semantic algebra.

## Decisions

**NT1 — The contract mints keys, not strings.** `VesperLib.registerTypeDecl` builds a
`TypeKey` with a real `TypeHolder.InModule` chain, and the store's index string is *derived*
from that key by the one renderer. Today it dot-joins a string and lets the consumer
re-parse it into an `InNamespace`, which is the single point at which module-ness is lost.
The name is a rendering of the identity; it must never be the route back to one.

**NT2 — `typeNestedName` is exhaustive, and `InModule` renders `+`.** A module-held type's
metadata name is `Ns.MModule+T` — the CLR truth, the string `asm.GetType` binds, and the
spelling that makes `typeMetaName` injective. The wildcard at `../SymbolKeyOps.fs:166` goes;
adding a `TypeHolder` case must break the build at this site.

**NT3 — `typeKeyOf` is honestly a partial inverse, and that is fine.** A bare metadata
string cannot say whether `M` is a module or a class, so `typeKeyOf` mints `InType` and
**cannot** mint `InModule`. That is correct for its one caller population (bare IL). The
"exact inverses" claim (`../SymbolKeyOps.fs:17`, `../SemanticInfo.fs:129`) must be re-scoped
to the `InNamespace`/`InType` sublattice it actually holds on. An `InModule` key is minted
only from Vesper metadata — the contract extractor (NT1) or local registration — never
parsed out of a name. Delete the doc reference to `moduleTypeKey`, a function that has never
existed.

**NT4 — The `…Module` suffix rule has one implementation and three readers, in a new
`ModuleRules` module.** The compiled holder name gains its suffix when the module collides
with a same-named nominal type in the unit **or** when
`[<CompilationRepresentation(ModuleSuffix)>]` is present — both halves, always. That rule is
`moduleHolderName` (`../Passes/NameResolution/TypeRegistration.fs:115`) today, shared by the
local key mint and the emitter but *not* by the contract extractor, which implements only the
attribute half (`VesperLib.fs:1570`). So a `.fsi` with `module SetTree` alongside `type
SetTree` names the holder `SetTree` while the local mint names it `SetTreeModule` — two names
for one class.

The rule cannot stay in a `Passes` module: the contract extractor is upstream of name
resolution, and everything downstream references `SemanticAnalysis`. It moves to a new
`ModuleRules` module in `SemanticAnalysis`, which becomes the home for module-specific logic
generally — the suffix rule, the holder-chain construction shared by type and binding keys
(NT5), and the nesting predicate the emitter needs (NT6).

**NT5 — Nested modules are preserved in *every* key.** `localTypeHolder` builds the full
module chain for a type key, but `Elaborate.fs:1817` roots a *binding* key's holder with
`moduleInNamespace ns h` — the innermost module only, dropping the outer chain. For `module A
= module B =` a type gets `InModule(B ∈ InModule(A ∈ N))` while a binding gets `InModule(B ∈
N)`. One containment chain, built one way, for types and bindings alike.

**NT6 — The CLR backend emits `NestedClass` rows.** A module-held type's `TypeDef` is nested
in its module's holder `TypeDef`: empty namespace column, nested visibility, and a
`NestedClass` row. This is the concession `typeNestedName`'s flat rendering was silently
paying for; once it is gone, the key's spelling and the emitted metadata agree by
construction rather than by coincidence.

**NT7 — `ClrEnv`'s `InModule` `TypeRef` arm is wired.** `externalModuleRef`
(`../../XParsec.FSharp.Codegen.Clr/ClrEnv.fs:442`) already exists and already produces the
correct nested chain. The adjacent `failwithf` (`:492`) becomes
`toEntity (ctx.TypeRef(externalModuleRef info.Origin m, "", typeSegmentName t))`, with
`info.Origin` already in scope. Its comment claiming "no producer mints this holder yet" is
false and goes with it.

**NT9 — `layout.Types` becomes hierarchical, not merely legal.** Two shapes were available:
keep the by-kind grouping and add an enclosing link, making the row order *legal* (nested
after enclosing) while the `NestedClass` rows carry the containment; or make the row order
*express* the containment — each holder immediately followed by the types it holds. The
second is chosen. Row order is already load-bearing (`layout.Types` position **is** the table
row); a grouping that no longer means anything structural while a side table carries the real
relationship is exactly the "invariant guarded by a check rather than a type" shape this
codebase has been moving away from. The cost is that Fields, Methods, `HolderPlan`'s
holder-method block and `ModuleValueFieldOrder` must be reshuffled to match, all at once.

**NT10 — The IL gets asserted directly, not through reflection.** `verifyTypeHandle` checks
that emitted handles match the layout's predictions; it cannot say the resulting metadata is
well-formed. With row order carrying structural meaning (NT9), that gap is the main risk in
this plan, and it grows with every future emitter change. So this work builds a
`MetadataReader`-based test instrumentation over the emitted PE, able to assert on rows and
flags directly: `NestedClass` rows and their enclosing/nested pairs, `TypeDef` name and
namespace columns, visibility flags, and the field/method range contiguity the prefix sums
assume. Reflection (`TestHelpers.loadAssembly`) answers what the *runtime* makes of the
assembly; this answers what we actually wrote. The nesting tests are its first consumer, not
its only one.

**NT8 — A nested type's visibility is capped by its holder's.** `internal` is
assembly-scoped, so anything inside an assembly-scoped module is *at most* assembly-scoped:
the emitted visibility is the **minimum** of the type's own and its holder chain's. This is
the invariant; it is not implementable yet, because **nothing models `internal` in emission
today** — every type-attribute site hard-codes `TypeAttributes.Public`
(`Assembler.fs:502,514,525,536`; `Metadata.fs:258,291`), so `module internal SetTree` already
emits a public holder. This plan therefore emits `NestedPublic` uniformly, which preserves
today's behaviour exactly rather than regressing it, and states the capping rule as the one
to honour when accessibility lands. Nesting is what makes the rule *expressible* — a flat
`TypeDef` has no holder to be capped by — so recording it here is the point, not deferring it.

## Work

### The key layer

- `typeNestedName`: exhaustive match; `InModule` renders `moduleFullName`-chained `+`
  segments (NT2). `typeNs` continues to report the root namespace — a nested type's
  namespace column is its outermost holder's, which is what the CLR does.
- Re-scope the inverses doc + `SymbolKeyTests` round-trip pins to `InNamespace`/`InType`
  (NT3), and add a pin that an `InModule` key renders `N.MModule+T`. The existing behaviour
  freeze (`SymbolKeyTests.fs:290-297,329`, "the rendered metadata name does not move") is
  the pin this change flips.
- Fix `Elaborate.fs:1817` to build the full module chain for binding keys (NT5).

### The contract extractor

- `registerTypeDecl` mints a `TypeKey` with the `InModule` chain and derives its index string
  from `SymbolKeyOps.typeMetaName` (NT1). The store's key and `qualifiedName` then agree by
  construction — the property `TsManifestTypes` already states of itself.
- Share `moduleHolderName` (NT4) — it lives in `NameResolutionTypeRegistration` and the
  extractor is upstream of it, so the rule needs a home both can reach.
- The `.fsi` module-held types (`OptimizedClosures`, `ByRefKinds`) are the regression
  surface; they are consumed today only through the contract leaf, never by reflection.

### The CLR emitter — the substantial part

The obstacle is not the `NestedClass` row; it is that **`TypeDef` row order is load-bearing**.
`layout.Types` position *is* the table row (`Layout.fs:1356`), handles are predicted from it
before rows exist, and `verifyTypeHandle` asserts the prediction. The Fields and Methods lists
are built in the *matching group order* and prefix-summed per slot. Nested types must follow
their enclosing type, so:

- `TypeSlot` gains an enclosing link (`Enclosing: TypeSlotKey voption`); `nominalSlot`
  (`Layout.fs:557`) reads the module chain off `td.Key` instead of discarding it, forcing
  `Namespace = ""` for a nested slot.
- **Types, Fields and Methods must be reordered in lockstep** into the hierarchical order
  (NT9) — each holder immediately followed by the types it holds — along with `HolderPlan`'s
  contiguous holder-method block and `ModuleValueFieldOrder`. This is the crux of the work,
  and NT10's instrumentation is what makes it reviewable.
- **Holder discovery gains a third source.** Today holders come only from `TDeclG.Let`
  (`HolderPlan.fs:239-261`); `TDeclG.Type` is skipped. A module that holds *only* types gets
  no holder class at all today and must get one.
- `HolderKey` is `(string option * string)` (`EmitTypes.fs:280`) and **cannot express module
  nesting** — a nested module's holder is currently flattened into the namespace column. It
  becomes a `ModuleKey`.
- Visibility: every type-attribute site hard-codes `TypeAttributes.Public`
  (`Assembler.fs:502,514,525,536`, plus two sites inside `Metadata.fs` the layout cannot
  reach). Nested visibility is a *replacement* of a 3-bit field, not an addition, so a nested
  slot emits `NestedPublic` (NT8) — the two `Metadata.fs` sites must take the attrs as a
  parameter rather than hard-coding them.
- `MetadataContext` gains an `AddNestedType` wrapper over `MetadataBuilder.AddNestedType`.

### The JS backend

Nothing structural: `CstWalk.fs:1204` erases the `module` wrapper, so the JS emitter never
sees one and emits types and module functions flat. Its only exposure is the union `$type`
brand (`EmitJsTypes.fs:350`), which is minted once in the declaring assembly, rides the
imported prototype, and is never re-derived, split, or compared to a literal by a consumer.

- `src/Vesper.Printf/Vesper.Printf.mjs:7` regenerates: `"Vesper.Doc"` → `"Vesper.StructuralPrinter+Doc"`
  (its `Doc` union lives in `module StructuralPrinter`). `Vesper.List.mjs` / `Vesper.Option.mjs`
  are unaffected — their types sit directly in a namespace.
- This **fixes** a latent brand conflation: two same-named unions in sibling modules currently
  brand identically, so `eqStructural` (`Vesper.Core.mjs:75`) would treat them as one type.
- Observable, and worth stating: `Vesper.Comparison.mjs:61` *orders* by the brand string when
  comparing heterogeneous branded values, so that ordering shifts. Reachable only through
  erased/`obj` comparisons.

## Out of scope

Named because they are adjacent and will look like omissions:

- **Holder-aware `TypeClaims` and module-scoped name resolution.** `TypeClaims` is keyed by a
  bare short name (`TypeRegistry.fs:223`), so two sibling modules declaring the same type name
  still contest one claim and the second is rejected as a duplicate — pinned as current-and-wrong
  in `DuplicateTypeNameTests.fs:196-216`. This plan is a **precondition** for flipping that pin
  (the two types must be distinguishable in metadata before they can both be claimed), not the
  flip itself. That work also needs module-scoped resolution: today a bare `T` resolves purely
  by short name, and `SetTree.SetIterator` written from outside resolves to nothing local.
- **The ref-struct set's `(ns, name)` key** (`EmitClosures.fs:244`) is module-blind, so a
  ref-struct `A.T` would make a non-ref-struct `B.T` fail `isFieldEmittable`. Latent today,
  reachable the moment holder-aware claims admit both types.
- **JS's own sibling-module collision.** Local JS class names come from `td.Name`, not the key,
  so `module A / type T` + `module B / type T` emits two top-level `class T` in one file — a hard
  `SyntaxError`. This plan neither fixes nor worsens it, but it argues the same way: names must
  come from the holder chain.

## Tests to flip

- `SymbolKeyTests.fs:290-297`, `:329` — the behaviour freeze pinning `typeMetaName` flat at
  `N.T` for an `InModule` key. This is the pin this plan exists to flip.
- `LocalModuleTests.fs:107-113` — pins the flat `TypeDef` emission, with a comment saying it
  must be flipped once the emitter nests.
- `StructuralPrinterTests.fs:39` — the committed-`.mjs` golden; regenerate.
- New: a module-held type's key round-trips through the contract extractor and the local mint
  to the **same** `SymbolKey` (the cross-face equality this plan buys).
- New: an emitted assembly's module-held type binds by `asm.GetType "N.MModule+T"`.

## Risks

- **The lockstep reorder is the whole risk.** Types, Fields and Methods are three lists whose
  group order is an unstated contract, cross-checked only by `verifyTypeHandle` at emit time.
  Getting it wrong fails loudly rather than silently, which is the one mercy — but it must be
  got right in all three lists plus `HolderPlan` at once, not incrementally.
- **`registerTypeDecl` and `typeNestedName` must move together.** Changing the renderer alone
  is inert (no `InModule` key reaches a provider today). Changing the extractor's key mint
  without the renderer breaks contract lookups outright. Neither is independently shippable.
