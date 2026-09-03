# Struct-union split-payload layout — plan

Supersedes `brainstorm-du-layout.md`. The brainstorm's offset-computing algorithm is replaced
by the per-case-struct overlay agreed in review: every overlaid field sits at `FieldOffset(0)`
and is itself a JIT-laid-out struct, so the compiler never computes a size, an alignment or an
offset. The brainstorm doc is still cited by `brainstorm-option-representation.md` (three
links) and `du-architecture.md:287`, so it is deleted at Step 6 together with a reference
sweep, not at Step 1; delete this plan when Step 7 completes.

All work is in `Codegen.Clr`. SemanticAnalysis and the JS backend are untouched: physical CLR
layout is the target-dialect projection the levelling rule pushes into the backend, and the JS
backend emits a hierarchy for every union.

## Target layout

For a `UnionRegime.StructTagged` union, the emitted value type holds:

1. `_tag : int32` — unchanged contract (`UnionRegime.hasTag`, `get_Tag`, match switching).
2. `_payload : Payload` — one `initonly` field typed as a synthesized nested *sequential*
   struct `Payload`, generic over the union's typars exactly when an exact-type slot mentions
   one. Its fields are the physical slots (`FlatUnionPlacements.Slots`):
   1. One overlay field `_data`, typed as a synthesized `assembly` NON-GENERIC struct
      `<Union>$Data` with `ExplicitLayout`, emitted BESIDE the union in its container: one
      field per payload-bearing case, each a synthesized *sequential* struct nested in the
      overlay holding that case's transitively-unmanaged fields, all at `FieldOffset(0)`. The loader computes every size; overlapping unmanaged structs is
      legal; `nativeint`/`unativeint` lay out per-platform correctly because nothing is
      computed at compile time.
   2. Shared `object` reference slots: fields classified `Reference` map to slot indices,
      slot count = max ref-field count over cases. Reads insert `castclass` to the declared
      type.
   3. Exact-type slots: every remaining field (managed value types `ManagedStruct`, typars,
      `Undetermined`) shares a slot only with a field of an *identical* stored `FrozenType`
      in another case. Sharing identical types is unconditionally safe, so this is the
      universal fallback; a typar field is just the `FTTypar` instance of it (subsumes the
      brainstorm's `GenericSlot`).
3. One public readonly view struct `Payload_<Case>` per payload-bearing case, nested in the
   union, wrapping a single `Payload` field and exposing one get-only property per logical
   field, each reading through that field's placement. Every view has the layout of
   `Payload` by construction, so a view is obtained by a plain by-value copy of `_payload`
   and needs no reinterpretation. A `GetPayload_<Case>` instance method on the union returns the
   view (the tag is the caller's responsibility to have checked, as with FSC's `Item`
   properties).

A `Payload` with no unmanaged fields omits `_data`; a purely unmanaged union's `Payload` is
only `_data`. One code path, no separate regime, no user-facing attribute — classification is
automatic.

Two read surfaces, one placement table:

- **Match-arm ABI** (Step 2): per-(case, field) getter methods on the union, reading in place
  through `ldflda _payload`, so a compiled `match` copies nothing. This is what a
  cross-assembly arm calls, plus the existing factories and `get_Tag`.
- **Consumer surface** (Step 6): the `Payload_<Case>` views, for hand-written callers and
  C# interop. A view costs one copy of `Payload` at `GetPayload_<Case>`; the JIT inlines both
  surfaces to the same field chain thereafter. The compiled match does not use views, so
  the copy is paid only where a caller asks for one.

The nested structs, slot assignment and `Payload`'s own field layout are private,
per-compilation decisions. A reinterpret cast between differently laid out per-case structs
was considered and rejected: the CLR has no verifiable IL for it, and it would put size and
offset computation back into the compiler.

Structural bodies *become* tag-dispatched per-case field walks. Today `prepareFlatStructural`
(`UnionEmit.fs:430`) builds one flat walk over every case's fields concatenated, with the tag
as a leading discriminant, relying on each logical field owning a typed slot and inactive
fields holding their zero default. Once a ref slot is typed `object` (Step 4) a flat walk would
compare a `string` through `IComparable.CompareTo(object)` instead of the typed ordinal path,
and once `_data` exists (Step 5) the payload is behind an opaque struct. Equality, hashing and
comparison therefore need a new `EmitStructural` shape: a tag dispatch (a `beq` chain, since
`ILInstr` has no `switch`) to one field walk per case, read through placements. Format already
carries a per-case list.
Never a raw byte compare of `_data` (padding inside case structs is not preserved across
copies).

**Generic types cannot have explicit layout** (loader: "Could not load type 'Overlay`1' …
because generic types cannot have explicit layout", confirmed with `dotnet fsi`). The overlay
and case structs are emitted with ZERO typars even when the union is generic, and only
`Unmanaged` fields land there, so no typar is ever referenced. IL permits a non-generic type
nested in a generic one, but C# always gives a nested type its enclosing type's typars, so
that shape is one C# never emits and ICSharpCode.Decompiler throws rendering a reference to
it. The overlay is therefore the union's SIBLING, named `<Union>$Data`, or `<Union>$Data$<N>`
for arity `N` (`GBox$Data$1`), so two unions differing only in arity keep distinct overlays.
`$` keeps it outside any name source can declare, and the spelling carries no backtick, so
`SymbolKeyOps.typeKeyOfSegment` reads it back as a plain arity-0 name. This diverges from the
`unionCaseNode` precedent (`LayoutNodes.fs:353`), which redeclares the union's typars on the
nested case type.

## Decisions already taken (confirm before Step 1)

- **Same-emitter invariant holds**: referenced packages are compiled by this emitter
  (`UnionRegime.ofExternalShape` already leans on it), so accessors are the only cross-assembly
  contract and no placement recomputation from `ExternalUnionShape` is needed.
- **Tag stays `int32`**; no per-union tag enum, no byte tag.
- **`SingleCase` struct regime keeps its inline layout** — one case has nothing to overlay.
  `EnumLike` likewise unchanged.
- **No `[<UnmanagedUnion>]`** or any surface syntax.
- **Accessor spelling** (Step 2 fixes it): plain public instance methods, one per
  (case, field), named deterministically from `UnionCaseFields.names`
  (`UnionCaseShape.fs`) — proposal: `Get_<Case>_<fieldName>`. Not property rows, to avoid
  colliding with the FSC `Item` convention we deliberately don't follow.
- **View spelling** (Step 6 fixes it): nested readonly struct `Payload_<Case>`, properties
  named by `UnionCaseFields.names` in the own-type spelling (`Radius`, `Item1`), union method
  `GetPayload_<Case>` returning it (Step 7 fixes the prefix). Only payload-bearing cases get a view.

## Steps

Each step lands green on its own, in order. Step 3 may land in parallel with Step 2.

### Step 1 — placement intermediate over the current layout (pure refactor) — LANDED

`FlatUnionPlacements` (`Codegen.Clr/UnionPlacements.fs`) covers every flat regime, not only
`StructTagged`, because the flat `.ctor`, factories and structural walk are one code path
across `SingleCase` and `StructTagged`; the computation keeps `SingleCase` at one slot per
field permanently. It holds the physical `Slots` (`UnionSlot = { Key: UnionSlotKey; MetaName;
Ty }`, in `.ctor` order after `_tag`) and a map from logical (case, fieldIndex) to
`UnionFieldAccess`, the DU of read paths. One case at this step (`Direct of UnionSlot`:
`ldfld` the slot, no conversion); Step 4 adds the `object` slot with a `castclass` to the
declared type, Step 5 the `ldflda _data` / `ldflda <caseStruct>` / `ldfld <field>` chain.
`UnionSlotKey` likewise grows a case per physical slot kind (`CaseField` now; `RefSlot`,
`ExactSlot`, `Data` later), and `FieldKey.UnionSlot of SymbolKey * UnionSlotKey` is the
layout identity; `FieldKey.UnionCaseField` is now hierarchy-only.

Computed once per union at partition (`UnionDecl.Placements`, `ValueSome` exactly where the
regime is flat). Consumers routed through it:

- field minting — `LayoutNodes.buildUnionNodes` mints one row per slot
- `.ctor` signature, parameter names and stores — `UnionEmit.flatCtor` via `slotRefsOf`
- factory zero-default construction — `UnionEmit` `UnionFactoryShape.StructTagged` arm:
  `Param` for a slot this case's placements own, `Default` for every other
- extraction and structural walks — `UnionEmit.caseFieldKey` / `fieldRefsOf`
- generic `MemberRef` spelling — `NominalRegistration` registers each logical field's
  `(MetaName, Ty)` from placements and the slot types as `GenericUnionShape.Slots`, which
  `ClrGenerics` `UnionMember.Ctor` encodes. Step 4 needs a `UnionMember.Slot` case for the
  `.ctor` stores of a slot not owned by exactly one logical field.

No metadata change; `StructTests` stayed green.

### Step 2 — accessor surface and external rerouting (the ABI cut) — LANDED

- `MethodKey.UnionCaseGetter of SymbolKey * case: string * index: int`: one public instance
  `Get_<Case>_<i>` per (case, field), spelled by `UnionCaseFields.getterName`, declared
  exactly where `UnionRegime.hasCaseGetters` holds (`StructTagged`). `LayoutNodes` mints the
  rows after the factories; `UnionEmit.prepareUnion` builds each body as `ldarg.0` /
  `ldfld <slot>` through the case's placement, sharing the `.ctor` pass's slot refs so a
  generic union's `MemberRef` count per pass is unchanged.
- `ICodegenProvider.ExternalUnionCaseField` returns `UnionCaseAccess * FrozenType`, where
  `UnionCaseAccess = Field of EntityHandle | Getter of EntityHandle`. `ClrExternalMembers`
  yields `Getter` for a `StructTagged` external union and `Field` otherwise;
  `EmitPattern` calls a `Getter` on `ldloca scrutSlot`. A union emitted in the current
  assembly still reads its fields directly (`Field`), so a local match copies nothing.
- `du-architecture.md` non-goals entry updated.
- Tests: `StructTests` pins the getter rows (`MetadataStructure.methodAttrsOf`) and reads a
  payload back by reflection; `OptionTests` pins that a cross-package `match` on the struct
  `option` references `Get_Some_0` and never the `Some_0` field. The `struct-union` and
  `struct-union-generic` byte-identity goldens were regenerated for the new rows.

After this step no emitted code outside the defining assembly references a slot field. The
slot fields themselves are still `public initonly` (`LayoutNodes.buildUnionNodes`); they
become private when Step 5 moves them behind `_payload`.

### Step 3 — tri-state unmanaged classifier — LANDED

`Unmanagedness = Unmanaged | Managed | Undetermined of blocker: FrozenType`
(`Codegen.Clr/Unmanagedness.fs`), a pure function over `FrozenType` resolved through
`ICodegenSymbols`. `Undetermined` is treated as `Managed` by layout but carries the blocking
type, so widening the classifier later is an observable worklist, not an audit.

- An intrinsic (`FTConst`) classifies by the CLR type its `(# … #)` binding names, read
  through `ICodegenSymbols.TryPlatformTypeId` (the `IntrinsicTypeMap`), never by its Vesper
  name: `PlatformTypeIds.isUnmanagedScalar` (the ECMA-335 primitive value element types,
  `System.Decimal`, `System.ValueTuple` for `unit`, and pointer syntax) is `Unmanaged`, IL
  array syntax is `Managed`, and any other platform type (`System.String`,
  `System.Numerics.BigInteger`) is settled by `IsValueType`: a reference is `Managed`, a
  struct is `Undetermined`. Numeric enums: `Unmanaged`. `FTFun`, `FTOr`, interfaces,
  reference records/unions/classes, string and mixed enums: `Managed` (they are `Reference`
  for slotting — see Step 4). Typars: `Managed` (exact-slot).
- Nominals, this compilation's and referenced alike, resolve through one path: the analysed
  assembly's `Visibility` publishes local declarations as `ExternalTypeShape`s, so a struct
  record or struct union recurses into its case/record field templates instantiated by
  `FrozenTypeBridge.substituteDeclaring`. `ICodegenSymbols.IsValueType` (target first, then
  the declaration) decides value-kindness. A cycle on the instantiation path is
  `Undetermined` at the repeated type.
- A value type whose fields the provider cannot enumerate (a BCL struct such as `Guid` or
  `DateTime`, a struct class with `val` fields, an intrinsic scalar outside the tables) is
  `Undetermined` at itself. No BCL allowlist yet.
- `FTTuple` recurses into its items directly. `FTUnknown`, `FTKeyOf`, `FTIndexedAccess` and
  `FTConditional` are `Undetermined`.
- `UnmanagednessTests` covers primitives, local struct records/unions at concrete and open
  generic instantiations, the referenced `Vesper.Option` struct union at `int` and `string`,
  BCL `Guid`/`StringBuilder`, a hand-built cyclic record, and pins the census over the
  `StructUnion*` data programs (`StructUnionExternalPayload.fs` was added to the corpus so
  the census carries `Undetermined` entries).

### Step 4 — slot sharing without the overlay — LANDED

`FlatUnionPlacements.ofCases` takes `ICodegenSymbols` and splits in two: `unshared` (the
Step 1 computation, one slot per field at FSC's spelling) exactly where
`UnionCaseFields.ownType` holds, and `shared` for `StructTagged`. No explicit layout yet, so
no loader risk.

- `UnionStorage` (`UnionPlacements.fs`) classifies a field by `TypeLayout.resolve` over an
  `ILayoutOracle` on `ICodegenSymbols`: a settled `Reference` erases, a value type, a typar
  and any unsettled layout are `Exact`. Step 3's `Unmanagedness` is not consulted until the
  overlay exists.
- `Reference` → `UnionSlotKey.RefSlot of index`, stored `object`, read through
  `UnionFieldAccess.Erased(slot, declared)`, which carries the `castclass` target.
- Everything else → `UnionSlotKey.ExactSlot of index`, shared only between identical stored
  `FrozenType`s. Slots are minted in case-then-field declaration order and a case never
  claims one twice, so its own fields land in distinct slots.
- `UnionMember.Slot of UnionSlotKey` mints every flat slot ref (the `.ctor` stores, the
  getters, the structural walks and the match arms alike); `UnionMember.Field` is
  hierarchy-only, and `GenericUnionShape.Cases` keeps the declared field types, which the
  case factories' `MemberRef`s are spelled from. `GenericUnionShape.Slots` carries
  `UnionSlot` rather than a bare type so the ref can be spelled from the key.
- Getters read through the placements, `Emit.buildErasedFieldGetter` appending the cast.
  `UnionCaseAccess.ErasedField` does the same for a local match arm, casting to the
  sub-pattern's own type, which is the use site's instantiation of the declared type.
- Structural bodies changed shape: `EmitStructural.StructuralWalk` is now
  `Flat of seed * fields` or `Tagged of tagField * per-case fields`, and every body
  (equality, hashing, comparison) dispatches on `_tag` to the active case's walk. A
  `StructuralField` carries the `castclass` an erased read needs, so a `string` is still
  compared through `EqualityComparer<string>` rather than `object`. `%A` already dispatched
  per case and only gained the cast.
- Tests: `UnionPlacementsTests` pins the placement table over the `StructUnion*` corpus, the
  determinism of two computations, the distinct-slots-per-case invariant and the
  `SingleCase` carve-out; `StructTests` pins the reduced field set (`_tag`, `_val0`,
  `_val1`), the `object` typing of `_ref0` and a getter round-trip through the cast.
  `ChoiceTests` moved off field reads onto the Step 2 getter ABI. The `struct-union` and
  `struct-union-generic` byte-identity goldens were regenerated.

This step already delivers most of the footprint win.

### Step 5 — unmanaged overlay via per-case structs — LANDED

- `UnionStorage.ofFrozen` classifies `Unmanaged` (through `Unmanagedness`) ahead of the
  Step 4 reference/exact split. An unmanaged field is `UnionFieldAccess.Overlaid`, placed on
  its case's data struct (`UnionCaseData`, FSC-spelled field names); `UnionSlotKey.Data` is
  the `_data` slot, minted exactly when some case has an unmanaged field.
- `FlatUnionPlacements.Home: UnionSlotHome` holds the slots where they are declared:
  `Inline of slots` on the union (`SingleCase`; `EnumLike` with none) or `Payload of
  UnionPayloadSlots` on the nested `Payload` struct, with the overlay (`_data` slot plus its
  case data structs) as a `voption`, the `object` slots and the exact slots as separate
  pools. `Slots` is computed from `Home` in that order, so the `_data` slot exists exactly
  when the overlay does. `FlatUnionPlacements.NestedTypes` (under the union) and
  `OverlayTypes` (the overlay's subtree) enumerate the value types behind `_payload`
  (`UnionNestedType`), which the layout nodes and the provider registration both map; each
  type's row placement derives from its own case (`UnionLayoutNodes.ownedPlacement`).
- Owned types (`UnionPayloadType`): `Payload` (sequential, nested in the union, redeclares
  the union's typars whenever the union is generic, keyed like a case type with arity 0),
  the `ExplicitLayout` overlay `<Union>$Data` (the union's sibling; one field per case data
  struct, each with a `FieldLayout` row at offset 0) and one `Data_<Case>` per overlaid case
  nested in the overlay. The overlay and the case data structs declare no typar. All three
  are `assembly` value types with `assembly` non-`initonly` fields, so
  a same-assembly match arm reads through them directly and the factory writes them through
  `ldflda`. No `ClassLayout` row: the loader computes every size (Roslyn likewise emits none
  for pack 0 / size 0).
- The union's fields are `_tag` and `_payload` (`assembly initonly`), the `.ctor` takes
  `(tag, Payload)`, and a factory builds the payload in an `initobj`-zeroed local
  (`Emit.buildStructUnionPayloadFactory`). An `EnumLike` union's `.ctor` is `TagOnly`
  whichever its value kind, and a struct one's factory is `UnionFactoryShape.StructTag`.
- Every read path is an `ldfld` chain (`UnionCaseAccess.Field of EntityHandle list`,
  `EmitStructural.StructuralField.Path`, `EmittedCaseField.Steps`): `ldfld` accepts an
  object reference, a managed pointer and a value-type instance alike, and unlike `ldflda`
  is verifiable on an `initonly` field outside the `.ctor`. Match arms, getters and
  structural bodies share one placement table. `UnionEmit` derives every path once as
  `EmitTypes.FieldStep<FieldKey>` (the `UnionMember` a generic instantiation re-spells a
  `Member` step by rides along) and a pass resolves every step's handle once, up front
  (`FlatPass.Refs`); a match arm sees the same steps mapped to `Def` tokens.
- Generic unions: `GenericUnionShape.Home` carries the `UnionSlotHome`; `Payload` is
  registered as a generic class so `UnionMember.Slot` mints on its `TypeSpec`, and
  `UnionMember.Payload` mints `_payload` on the union's.
- Tests: `StructUnionTests` pins the nested type set, the `ExplicitLayout` flag, the
  `FieldLayout` rows, the absence of `ClassLayout` rows, field attributes, a loader pass
  over every `StructUnion*` program, a mixed union of all four storage kinds
  (`StructUnionMixedStorage.fs`: overlay, `object`, managed struct exact, `Guid` exact) read
  back through the getters, and a generic union with an unmanaged case
  (`StructUnionGenericOverlay.fs`). `UnionPlacementsTests` pins the new placement table and
  the overlay/`_data` invariant; the two byte-identity goldens were regenerated.

Deviations from the plan as written: `Payload` is generic exactly when the union is, rather
than only when a slot mentions a typar (one generic/mono decision, matching the case-type
precedent); reads use `ldfld` chains rather than `ldflda`; the types are `assembly`-visible
rather than private. Step 6's views wrap a `Payload` field, which the `assembly` visibility
permits.

### Step 6 — per-case payload views (additive) — LANDED

- `FlatUnionPlacements` holds one table, `Cases: UnionCasePlacement list`, whose entries carry
  each field's placement and both reader names (`UnionCaseField.GetterName` /
  `PropertyName`). The two public surfaces are members over it: `Getters` flattens every
  field of a `Payload` home, `Views` is the payload-bearing placements of a `Payload` home. A
  view IS its case's `UnionCasePlacement`; `UnionNestedType.CaseView` puts it in the one
  enumeration `UnionLayoutNodes` and `NominalRegistration` already map, so a view is
  registered, keyed and laid out like the storage types. `UnionNestedType.Fields` is the one
  spelling of a nested type's field rows, which registration and layout both read.
- `Payload_<Case>` (`TypeSlotKey`/`TypeSlotKind.UnionCaseView`) is a sealed sequential
  `NestedPublic` `IsReadOnly` value type redeclaring a generic union's typars, holding one
  `private initonly _payload` (`FieldKey.UnionCaseViewPayload`) and declaring an
  `assembly .ctor(Payload)` (`MethodKey.UnionCaseViewCtor`) whose only caller is the union.
  `Payload` is `assembly`-visible, so the `.ctor` is too.
- One `Property` row per logical field (`PropertyKey.UnionCaseViewField`), named by
  `UnionCaseFieldName.fsharpNames` in F#'s own spelling — a declared field verbatim, `Item`
  for a lone positional field, `Item<n>` otherwise, confirmed against `dotnet fsi`. The
  plan's `Radius`/`Item1` illustration was corrected to that verbatim spelling. Each getter
  is `MethodKey.UnionCaseViewGetter`, `Public HideBySig SpecialName`.
- `Get_<Case>` on the union (`MethodKey.UnionCaseViewAccessor`, respelled `GetPayload_<Case>`
  at Step 7) is `ldarg.0`, `ldfld
  _payload`, `newobj` the view's `.ctor`.
- One function mints both read surfaces: `UnionEmit`'s `fieldGetterIr` takes the root the
  `ldfld` chain hangs off — `_payload` for a union getter, the view's wrapped field for a
  view property — and the rest of the chain plus the `castclass` comes from the shared
  placement. The view's property getters mint no slot ref of their own; they reuse
  `FlatPass.Refs`.
- A generic union's views are registered as generic classes over its typars
  (`UnionNestedType.IsGeneric`), with a `.ctor` over their fields (`HasCtor`), so
  `ClassMember.Ctor` and `ClassMember.Field` mint on the view's own `TypeSpec`.
- Tests: `StructUnionTests` pins the view rows (visibility, layout, the single `_payload`
  field, the `.ctor`-then-getters method order, the `assembly` `.ctor`, the get-only
  properties and the view accessors beside the field readers on the union), a reflection round-trip
  reading every storage kind of `StructUnionMixedStorage` through its view, and the generic
  views of `StructUnionGenericOverlay` at a `string` instantiation. `UnionPlacementsTests`
  pins that a view covers exactly one payload-bearing case through the same placements.
  `MetadataStructureTests` now counts four `_payload` `MemberRef`s: the count is per
  (declaring type, member), and each view spells its own wrapped field with that name. The
  two byte-identity goldens were regenerated.

### Step 7 — reader naming: a diagnostic and a prefix — LANDED

`dotnet fsi` splits the two gaps the step was written against.

**The case-name gap is the backend's.** F# accepts `X_0 of int` beside `X of int * int`, struct
or not, so the collision between `Get_X_0` (case `X`, field 0) and `Get_X_0` (case `X_0`'s view
accessor) had to be spelled away rather than diagnosed. The view accessor is now
`GetPayload_<Case>` (`UnionCaseFields.viewGetterName`), which the field readers cannot reach:
`Get_<Case>_<i>` ends in a digit run, so its final `_` sits at the case name's length whichever
case produced it, and `GetPayload_` differs from `Get_` at the fourth character.
`StructUnionReaderNameClash.fs` is the corpus entry for the pair of case names.

**The field-name gap is the front end's.** F# refuses two fields of one case claiming a single
logical name under FS3176, in two sentences: `M of a: int * a: float` is "Named field 'a' is
used more than once." and `M of Item2: int * float` is "Named field 'Item2' conflicts with
autogenerated name for anonymous field." `Kind.UnionCaseFieldNameClash of name *
UnionFieldNameClash` reproduces both from `NameResolutionUnionRegistration.checkCaseFieldNames`
(the union section of `TypeRegistration.fs`, extracted to `UnionRegistration.fs`), in fsc's
order and at most once per case: a declared name repeated among the declared names reports
"used more than once" at its first occurrence, and only a case with no such repeat reports a
declared name that spells an anonymous field's position, at the declared name. A
positional name is claimed only where an anonymous field actually takes it and the position
counts every field, so `M of Item2: int`, `M of Item: int * float` and `M of a: int * Item1:
float` all compile.

`UnionCaseFieldName` (the type, `ofCase` and `fsharpNames`) moved from `Codegen.Common` up into
`SemanticAnalysis`, so the diagnostic and both backends read one spelling of F#'s logical names.
`fscFieldNames` is a CLR backing-field spelling and stayed behind, in
`Codegen.Clr`'s `UnionCaseFields`.

FS3585 stays relaxed (`StructUnionSameNameFields`): each case owns its own placements, so
same-name fields of differing types never share a slot.

Not closed, both pre-dating the layout and both needing a case or field name nobody writes:

- A field declared `_payload` gives its view a PROPERTY row of that name beside the view's own
  private `_payload` FIELD row. The two tables are unrelated (ECMA-335 II.22.15, II.22.34), so
  the type loads and the property reads, confirmed by `Reflection.Emit` under .NET 10. F#
  refuses to declare the pair (FS0023) and C# refuses it (CS0102).
- The factories are spelled `<Case>` with no prefix, so a case named `Get_A_0` or
  `GetPayload_A` collides with a reader of case `A`. Any fixed prefix leaves this open; closing
  it means a collision sweep over the union's whole emitted member set.

### Step 8 — cleanup and doc migration

- Update `du-architecture.md`: rewrite the "struct-union layout optimisation" known-gap entry
  as a description of the landed layout; fix the non-goals entry per Steps 2 and 6.
- Delete `brainstorm-du-layout.md` and this plan; rewrite the three links in
  `brainstorm-option-representation.md` and the one in `du-architecture.md` as the
  substantive point; strip any plan citations from code comments.

## Migration checklist (strike off before deleting this doc)

- [x] Same-emitter accessor ABI: stated on the `UnionCaseAccess` doc comment
      (`ICodegenProvider.fs`).
- [x] "Structural bodies never byte-compare `_data`" — stated on
      `EmitStructural.StructuralWalk.Tagged`; the emitter has no whole-struct compare path, and
      the payload is only reachable field by field through the placement table.
- [x] Determinism of placement assignment: `UnionPlacementsTests` "two computations of one
      shape agree".
- [x] `Undetermined` worklist: `UnmanagednessTests` "census over the struct-union data
      corpus" is the durable home.
- [x] Exact-slot sharing is unconditionally safe: stated on `UnionSlotKey.ExactSlot`.

## Risks

- **Loader rejection** is confined to Step 5 and surfaced by the load-based tests; Steps 1–4
  carry none. The one known rejection, explicit layout on a generic type, is designed out by
  emitting the nested structs non-generic.
- **Enregistration**: an explicit-layout member can inhibit JIT struct promotion; the win is
  copy/array footprint, not registers. Benchmark before claiming more (perf-tuning skill).
  The same inhibition means a `Payload_<Case>` view's copy may not be elided, which is why
  the compiled match reads through the in-place getters and never through a view.
- **Classifier breadth**: initially most external structs are `Undetermined` and fall to exact
  slots — correct, just less packed; the census test makes the gap visible.
- **Interface change blast radius**: Step 2's `ExternalUnionCaseField` split touches the
  provider interface, `ClrProvider`, `ClrExternalMembers` and `EmitPattern`; it is the one step
  whose diff crosses files broadly, which is why it carries no layout change.
