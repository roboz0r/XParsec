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
   1. One overlay field `_data`, typed as a synthesized private nested NON-GENERIC struct
      with `ExplicitLayout`: one field per payload-bearing case, each a synthesized private
      nested *sequential* struct holding that case's transitively-unmanaged fields, all at
      `FieldOffset(0)`. The loader computes every size; overlapping unmanaged structs is
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
   and needs no reinterpretation. A `get_<Case>` instance method on the union returns the
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
  C# interop. A view costs one copy of `Payload` at `get_<Case>`; the JIT inlines both
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
comparison therefore need a new `EmitStructural` shape: a tag `switch` dispatching to one
`StructuralWalk` per case, read through placements. Format already carries a per-case list.
Never a raw byte compare of `_data` (padding inside case structs is not preserved across
copies).

**Generic types cannot have explicit layout** (loader: "Could not load type 'Overlay`1' …
because generic types cannot have explicit layout", confirmed with `dotnet fsi`). The overlay
and case structs are emitted with ZERO typars even when the union is generic: IL permits a
non-generic type nested in a generic one, and only `Unmanaged` fields land there, so no typar
is ever referenced. This diverges from the `unionCaseNode` precedent (`LayoutNodes.fs:353`),
which redeclares the union's typars on the nested case type.

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
  `get_<Case>` returning it. Only payload-bearing cases get a view.

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

### Step 4 — slot sharing without the overlay

Change the `FlatUnionPlacements` computation and `LayoutNodes` minting; no explicit layout
yet, so no loader risk:

- `Reference`-classified fields → shared `object` slots (`UnionSlotKey.RefSlot of index`),
  read through a new `UnionFieldAccess` case carrying the declared type to `castclass` to;
  count = max over cases.
- Everything else → exact-type slots (`UnionSlotKey.ExactSlot`), keyed by canonical stored
  `FrozenType`, shared only between identical types. Deterministic assignment: declaration
  order of cases, then fields.
- `UnionMember.Slot of UnionSlotKey` for the generic `.ctor` store refs (`slotRefsOf`).
- `UnionCtorShape.FlatTagged` becomes one parameter per *physical* slot; the factory's
  zero-default list shrinks accordingly (`UnionEmit.fs:245`).
- Getters read through the updated placements (mechanical, given Step 1).
- Structural bodies change shape here, not mechanically: the flat walk becomes a tag `switch`
  over per-case walks (see "Target layout"), because an `object` slot cannot be compared
  through its declared type by a flat walk. This is the substantive work of the step.
- Tests: `StructUnionSameNameFields` and `StructUnionGenericShape` (`StructTests.fs:834`)
  stay green — same-name *different*-type fields land in distinct exact slots, so the FS3585
  relaxation is preserved; add `MetadataStructure` assertions on the reduced field count and a
  determinism pin (two computations of one shape agree).

This step already delivers most of the footprint win.

### Step 5 — unmanaged overlay via per-case structs

- For each case with ≥1 `Unmanaged` field, synthesize a private nested sequential struct
  holding those fields; synthesize the private `ExplicitLayout` overlay struct with every case
  struct at `FieldOffset(0)`; replace those fields' slots with the single `_data` field.
- Introduce the `Payload` struct here, in the same writer change: the union's fields become
  `_tag` and `_payload`, `FlatUnionPlacements.Slots` become `Payload`'s fields, the union
  `.ctor` takes `(tag, Payload)`, and a factory builds a `Payload` in an `initobj`-zeroed
  local (`ldloca` + `stfld` per owned slot, so `Payload`'s slots are plain private, not
  `initonly`). Step 4 keeps the slots directly on the union to stay free of writer changes.
- `LayoutModel.fs`: new `TypeSlotKind` cases (case-data struct, overlay struct), a
  `UnionSlotKey.Data` slot for `_data` plus `FieldKey` cases for the case-struct member
  fields, nested via the existing
  `TypeNode.Nested`/`NestedClass` machinery (`TypeSlotKey.UnionCase` is precedent for nesting
  only: the new nodes carry `Typars = []` regardless of the union's arity, per "Target
  layout").
- Writer: first uses of `AddTypeLayout`/`AddFieldLayout`; `ExplicitLayout` flag on the overlay.
  `MetadataStructure` assertions on the ClassLayout/FieldLayout rows are part of the
  deliverable (Codegen.Clr CLAUDE.md).
- IL details: `_data` stays `initonly`, written once by the union `.ctor` from an overlay
  parameter. Case-struct member fields are plain (non-`initonly`) private, because
  `initonly` would forbid the factory writing them through `ldflda`; factories build the
  overlay in an `initobj`-zeroed local. No `Unsafe`, no C#-level workaround.
- Extraction: `ldflda _payload` / `ldflda _data` / `ldflda <caseStruct>` / `ldfld <field>`
  through placements; a ref or exact slot is `ldflda _payload` / `ldfld <slot>`.
- Tests: run the full struct-union suite through `PeInspection.loadAssembly` (the loader is
  the arbiter of overlay legality), a mixed union exercising all four storage kinds in one
  type, and a GENERIC struct union with an unmanaged payload case loaded the same way
  (`StructUnionGenericShape` only carries a `'T` field, so it would not catch a generic
  overlay).

### Step 6 — per-case payload views (additive)

- For each payload-bearing case, a nested public readonly struct `Payload_<Case>` with one
  `Payload` field, one get-only property per logical field whose getter is the Step 2 getter
  body re-rooted at the wrapped field, and a union method `get_<Case>` that copies `_payload`
  into a new view. `TypeSlotKind` and `MethodKey` cases for the view, its properties and the
  accessor; the view shares `Payload`'s typars.
- Nothing in the compiled match or the structural bodies reads through a view; the
  placements table is the single source for both surfaces, so the property bodies are minted
  by the same function as the Step 2 getters.
- Tests: `MetadataStructure` assertions on the view rows, a reflection round-trip through
  `PeInspection.loadAssembly` reading every field of a mixed union via its view, and a check
  that the view's only instance field is of type `Payload`.

### Step 7 — cleanup and doc migration

- Update `du-architecture.md`: rewrite the "struct-union layout optimisation" known-gap entry
  as a description of the landed layout; fix the non-goals entry per Steps 2 and 6.
- Delete `brainstorm-du-layout.md` and this plan; rewrite the three links in
  `brainstorm-option-representation.md` and the one in `du-architecture.md` as the
  substantive point; strip any plan citations from code comments.

## Migration checklist (strike off before deleting this doc)

- [x] Same-emitter accessor ABI: stated on the `UnionCaseAccess` doc comment
      (`ICodegenProvider.fs`).
- [ ] "Structural bodies never byte-compare `_data`" — a test with a padding-bearing case
      struct whose padding is deliberately dirtied, or a sited comment on the structural body
      emitter if undirtiable.
- [ ] Determinism of placement assignment: pinned by test (Step 4).
- [x] `Undetermined` worklist: `UnmanagednessTests` "census over the struct-union data
      corpus" is the durable home.
- [ ] Exact-slot sharing is unconditionally safe: stated on `UnionSlotKey.ExactSlot`.

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
