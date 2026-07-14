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

**NT11 — The contract store is keyed by `TypeKey`, not by a rendered name.** A `.fsi` writes
a module-held type with dots (`byref<'T, ByRefKinds.In>`, `prim-types.fsi:1251`) while its
metadata name nests it (`ByRefKinds+In`). That a *spelling* difference can break a *lookup*
at all is the tell: the lookup is string-keyed, so two renderings of one identity miss each
other, and the only repair available inside that design is a second string table
reconciling them. **The spelling is not the problem; the keying is.**

So the contract leaf — the one face that actually HOLDS the containment, having minted the
`ModuleKey`s itself — is keyed by `TypeKey`. Its by-key store face hits that index directly;
`qualifiedName` leaves the by-key lookup path entirely. Its by-name face does what name
resolution means: resolves a written spelling against the containment it knows (`is A.B a
module? then C is the type it holds`) into a **key**, and looks up by key. No spelling is
reconciled with another spelling, because there is one identity reached from either face.

Two leaves are correctly left alone. `TsManifestMembers` and `JsNativeSymbols` mint only
`TypeHolder.InNamespace` keys, so their rendered name and their key agree by construction.
And `MetadataSymbols` stays name-addressed *because it is right*: it answers by reflection
(`asm.GetType`), reflection is name-addressed by nature, and in the bare-IL population — which
has no modules — the name IS the identity.

This is finding 1 of `docs/thermo-review-938dd9da34.md` ("the key is structured; every table
that consumes it is still string-keyed") landing where that finding predicted it would.

**NT9 — `layout.Types` becomes hierarchical, not merely legal.** Two shapes were available:
keep the by-kind grouping and add an enclosing link, making the row order *legal* (nested
after enclosing) while the `NestedClass` rows carry the containment; or make the row order
*express* the containment — each holder immediately followed by the types it holds. The
second is chosen. Row order is already load-bearing (`layout.Types` position **is** the table
row); a grouping that no longer means anything structural while a side table carries the real
relationship is exactly the "invariant guarded by a check rather than a type" shape this
codebase has been moving away from. The cost is that Fields, Methods, `HolderPlan`'s
holder-method block and `ModuleValueFieldOrder` must be reshuffled to match, all at once.

**NT9a — The hierarchy is a `TypeNode` tree; the three lists are projections of it.** Every
`…Parts` builder in `Layout.fs` already produces a `(slot, fields, methods)` triple — a
`TypeNode` in all but name. Only the holder and `Program` slots index into shared row lists
(`HolderPlan.MethodPlan`, `ModuleValueFieldOrder`), and that is the drift. So:

```fsharp
type internal TypeNode =
    { Slot: TypeSlot                  // Key / Kind / Namespace / MetaName / Typars — no counts
      Enclosing: TypeSlotKey voption  // ValueNone = root; ValueSome = a NestedClass row
      Fields: FieldSlot list
      Methods: MethodRow list
      Nested: TypeNode list }
```

`FieldCount`/`MethodCount` are **deleted** from `TypeSlot`: they are the last hand-maintained
agreement. The pre-order flattening of the roots *is* the `TypeDef` table, and Fields, Methods
and every prefix sum are `List.collect`s over it — so their agreement becomes definitional
rather than checked, and the "slots claim N method rows but the enumeration has M" guard
becomes unrepresentable. `HolderPlan.MethodPlan` and `ModuleValueFieldOrder` lose their
row-order meaning entirely.

The one invariant that survives and can still be violated is **completeness**: every built
slot placed exactly once, none dropped, none invented. That is a set-equality assertion over
the flattening, and it is cheap. The likeliest way to get this wrong is holder discovery from
types (NT6's third source) being incomplete, or a nominal landing in both the roots and a
module's `Nested`.

**NT9c — Sibling order within a holder stays by-kind.** Nothing constrains it: no consumer
inspects a numeric row, and the value of the hierarchy is that it makes the *layout code*
correct by construction, not that the byte order means anything to a reader. So today's
by-kind grouping simply applies within each holder rather than globally. True
source-declaration order is a free choice we are not taking — it would need `tast.Decls` to be
verified order-preserving across module boundaries, for no observable gain. If row order is
ever revisited it will be for load-time locality, which is not a concern today.

**NT9b — Closures stay roots.** A closure has a natural lexical home (FSC lifts a lambda into
a class nested in the enclosing *module*, which `Layout.fs:833-838` already records), but
nesting one changes its `TypeDef` name, namespace and visibility, and forces a `NestedClass`
row, for a type nothing names or resolves — `TypeSlotKey.Closure name` is its only address and
the name is already globally unique. The hierarchy must admit non-holder roots anyway
(`<Module>`, namespace-level types, `Program`), so leaving closures there costs nothing and
emits identical bytes.

**NT10 — The IL gets asserted directly, not through reflection.** `verifyTypeHandle` checks
that emitted handles match the layout's predictions; it cannot say the resulting metadata is
well-formed. With row order carrying structural meaning (NT9), that gap is the main risk in
this plan, and it grows with every future emitter change. So this work builds a
`MetadataReader`-based test instrumentation over the emitted PE. Reflection
(`TestHelpers.loadAssembly`) answers what the *runtime* makes of the assembly; this answers
what we actually wrote. What it must check, in priority order — the first is the one that
catches a bad NT9, and is worth having even if nesting slipped:

1. **Range partition.** Walk `TypeDefinition` rows in order and assert each one's field and
   method ranges are consecutive, non-overlapping, gap-free, and together cover the whole
   `Field`/`MethodDef` tables — and that each row's name is the one the layout put there. This
   is a direct assertion of the prefix-sum assumption, and it is precisely what the existing
   tautological checks cannot see.
2. **Entry point in range** — `Main`'s `MethodDef` handle lies inside `Program`'s `MethodList`.
3. **`<Module>` is row 1.**
4. **`NestedClass` rows** — exactly one per module-held type, `(nested, enclosing)` matching the
   key's holder chain, table ascending by the nested handle.
5. **Pre-order contiguity** — for a holder at row `r` with subtree size `n`, rows `(r, r+n]` are
   exactly its transitive nested set. This is what distinguishes *hierarchical* from merely
   *legal*, and nothing else can assert it.
6. **Flags** — a nested `TypeDef` has an empty namespace column, a dot-free name, and
   `NestedPublic` visibility (NT8's no-regression pin).

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

- **The lockstep reorder is the whole risk, and nothing today would catch it.**
  `verifyTypeHandle` is a *tautology*: it compares the handle `AddTypeDefinition` returned
  against a prediction derived from the slot's position in `layout.Types`, and the writer
  walks `layout.Types` in that order. It cannot fail unless a `TypeDef` is added out of band.
  The method check is tautological for the same reason. The only real guards are **sums**
  (total field count, total method count), so a mis-order that preserves the counts — Fields
  hierarchical while Methods stay grouped — writes a `TypeDef` whose `FieldList`/`MethodList`
  points at **another type's rows**, serialises cleanly, and fails at load/JIT or never. This
  is why NT10 is a deliverable of this work and not a follow-up: the range-partition assertion
  is the first thing in the tree that could catch it.
- **`Main` is a live trap today.** It is appended to `layout.Methods` *globally last*
  (`Layout.fs:1239-1245`) while the `Program` slot's `MethodCount` counts it
  (`:1288-1291`) — so `Program` must be the final slot or `Main` silently falls outside its
  `MethodList` range. The derivation (NT9) dissolves this by giving `Program` its own method
  list, but any reorder that does *not* dissolve it must preserve Program-last.
- **`registerTypeDecl` and `typeNestedName` must move together.** Changing the renderer alone
  is inert (no `InModule` key reaches a provider today). Changing the extractor's key mint
  without the renderer breaks contract lookups outright. Neither is independently shippable.
