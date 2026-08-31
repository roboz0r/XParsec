# CLR reference unions as a class hierarchy

Supersedes the "Reference unions emit flat, not as F#'s class hierarchy" gap in
[`du-architecture.md`](du-architecture.md) (`:275`). Struct unions keep the flat
`(tag, every case field)` shape permanently — a value type cannot inherit — so everything
below is scoped to `UnionValueKind.RefType`.

## FSC selects one of four representations

Probed with `dotnet fsi` across case counts 1–7 and every nullary/payload mix. The choice is
driven by the case count and by whether any case is nullary; the threshold is exactly 4.

| regime | when | base | discriminant | nested types |
| --- | --- | --- | --- | --- |
| **SingleCase** | 1 case | sealed, fields inline | none | none |
| **EnumLike** | ≥2 cases, all nullary | sealed, `_tag` + one `_unique_<Case>` singleton per case | `_tag` | none |
| **TypeTested** | 2–3 cases, ≥1 payload | abstract, **no fields** | `get_Tag` is an `isinst` chain, highest tag first, lowest as fall-through | one per case; a nullary case gets `_<Case>`, `NestedAssembly`, and a `_unique_<Case>` singleton on the base |
| **Tagged** | ≥4 cases, ≥1 payload | `_tag : int32`, `assembly initonly`, set by `.ctor(int32)`; abstract iff no case is nullary | `get_Tag` is `ldfld _tag` | one per **payload** case; a nullary case is a base instance held in `_unique_<Case>` |

In every hierarchy regime a case type is nested public, `extends` the base, holds `assembly
initonly` fields (`item`, or `item1..n`), and sets them in its own `.ctor`, which chains the
base. Case types are **not** sealed. A generic union's case type redeclares the union's type
parameters and `extends` a `GENERICINST` of the base over its own `!0`.

Two consequences for the gap list:

- **Your gap list is right for `Tagged`.** There the tag load survives and a match arm pays
  `ldfld _tag; bne` plus a `castclass` to reach the payload, exactly as gap 4 describes.
- **`TypeTested` has no `_tag` at all**, and there the `isinst` case test *is* the downcast, so an
  arm pays one type check rather than a tag compare plus a cast. That is why FSC switches at 4: an
  `isinst` chain is O(cases) while a tag load is O(1), so the chain stops paying beyond three.

## Why the field is not carried on every union

A discriminant field costs 8 bytes per instance on x64 — 4, padded — measured by allocating a
million of each shape and differencing `GC.GetTotalAllocatedBytes`:

| shape | without `_tag` | with `_tag` |
| --- | --- | --- |
| two reference payloads (a cons cell) | 32 B | 40 B |
| one reference payload (`Some x`) | 24 B | 32 B |

+25% and +33%, on the smallest and most heavily allocated unions in the stdlib. What it buys back
in `TypeTested` is bounded by the case count: at three cases the `isinst` chain the structural
bodies walk is at most two tests, and the *match* path is faster without the field, because
`isinst` subsumes the `castclass`. That is the trade FSC's threshold encodes, and the measurement
is why this plan keeps it rather than carrying `_tag` everywhere.

## Every case gets its own type, so the base is always abstract

FSC represents a nullary case as a **base instance** in a `_unique_<Case>` singleton whenever a
`_tag` exists to discriminate it, which is what forces its base concrete in that regime. We give
every case its own nested type in both hierarchy regimes instead, and the base is then abstract
uniformly.

Abstractness is a consequence of that choice, not a knob, and it does not reach codegen: FSC's own
`≥4`-case unions with no nullary case are abstract *and* discriminate by `ldfld _tag`. The reason
to deviate is the emitter, not the runtime — within a hierarchy regime every case carries a case
type, so no consumer branches on whether a nullary case has one, and the two hierarchy regimes
then differ in exactly one axis, whether `_tag` exists. The cost is one `TypeDef` per nullary case
in `Tagged`, which is metadata only: the singleton is still one instance, allocated once.

`EmittedCase.CaseType` is still a `TypeKey voption`, because the flat regimes are permanent and a
consumer reaching a case's payload must tell the two apart: `EmitPattern`'s union arm casts on
`ValueSome` and reads off the scrutinee on `ValueNone`.

## Today's shape already matches two of the four

The flat emission is one `TypeDef` carrying `_tag` plus every case's payload
(`LayoutNodes.fs:300-333`). For a **SingleCase** or **EnumLike** union that is already FSC's
type shape; the only divergences are that we emit a redundant `_tag` on a single-case union, and
that we allocate per nullary construction where FSC returns a cached singleton. Both are small,
separable optimisations rather than shape changes.

So the hierarchy work targets **unions with ≥2 cases and ≥1 payload case**, and nothing else. That
also gives the staging its safety property: a regime classifier lets each regime keep the flat
emission until its migration lands.

## What the flat shape makes load-bearing today

- `Emit.buildUnionFactory` (`Emit.fs:412`) `newobj`s the union's nullary `.ctor`, then `dup`/`stfld`s
  the tag and each payload. That post-construction store is why reference-union fields cannot be
  `initonly` (`LayoutNodes.fs:308`) — gap 7.
- `EmitPattern`'s union arm (`EmitPattern.fs:230-283`) tests `ldfld _tag; bne`, then `extractField`
  (`:158`) `ldfld`s each payload straight off the scrutinee local.
- The synthesised structural bodies walk **every case's fields unconditionally** once the tags
  agree — equality (`Emit.fs:642`), hashing (`:676`), comparison (`:791`) — which is sound only
  because inactive-case fields are co-resident and zeroed (the invariant stated at `Emit.fs:612`).
  `%A` already switches per case (`EmitStructuralFormat.fs:97`) but still `ldfld`s off `this`.
- `EmittedUnion.Cases` maps a case name to field handles on the one `TypeDef`
  (`EmitTypes.fs:63`, `:111`); `ClrGenerics.genericUnionMemberRef` (`ClrGenerics.fs:33`) parents
  every case field's `MemberRef` on the union's own `TypeSpec`.
- The external path mints the same convention against a referenced Vesper package: `_tag`
  (`ClrExternalMembers.fs:309`), `<Case>_<i>` (`:329`), `<Case>` factory (`:274`). It changes in
  lockstep, because a referenced package is compiled by this same emitter.

Construction is the one site that does **not** move: `EmitConstruct.buildUnionCons`
(`EmitConstruct.fs:173`) `call`s the case's static factory and never sees the representation.
Keeping a static factory per case — including nullary ones, where the body becomes
`ldsfld _unique_<Case>; ret` — is what holds that true, and is why this plan does not adopt FSC's
`get_<Case>` property for nullary cases.

## Staged plan

`_tag` survives both hierarchy regimes through step 2, so the discriminant changes exactly once,
after the layout work is green and every reader has moved.

**Step 0 — the classifier. DONE.** `UnionRegime` and `UnionRegime.classify`
(`Codegen.Clr/UnionRegime.fs`) hold the four regimes and the threshold rule. `classify` takes the
union's value kind, its case count, and whether any case carries fields — the three facts the
threshold rule reads, which the local `Frozen.TUnionCase`, `GenericUnionShape.Cases` and a
referenced package's `ExternalCaseShape` all supply, so step 3's external path calls the same
function. A `[<Struct>]` union classifies as `SingleCase`, `EnumLike` or `StructTagged`: a value
type has no subclass to test, so `TypeTested` and `Tagged` are reference-union regimes, and
`StructTagged` is the flat `(tag, every case field)` shape.

**The value kind is folded into the classification**, so `UnionRegime.isHierarchy` takes the
regime alone and a `[<Struct>]` union in a hierarchy regime is unrepresentable. `classify` already
takes the value kind, so a regime that still needed it to be interpreted was carrying half a
decision; `UnionCaseFields.ownType` takes one argument for the same reason.

Every carrier **derives** its regime from its own cases rather than being handed one, so a
carrier cannot disagree with the cases beside it: `UnionDecl.Regime`,
`GenericUnionShape.Regime` (`ClrEnv.fs:43`), `EmittedUnion.Regime` (`EmitTypes.fs:135`) and
`ClrExternalMembers.externalRegime` are each a function of that carrier's own cases and value
kind. `TypeSlotKind.Union of valueKind * regime` (`LayoutModel.fs`) is the one carrier holding a
stored copy, because a `TypeSlot` has no cases to derive from; the two fields drive different
things there — the regime abstract-vs-sealed, the value kind `IsReadOnly` and the `ValueType`
base. `UnionRegimeTests.fs` pins the type-test boundary against `TypeTestCaseLimit`, the
all-nullary-beats-count rule, and the struct rows.

`RegisterGenericUnion` takes the union's `UnionValueKind`, not a regime: the regime is a function
of the `cases` argument already passed, and a second parameter for it could contradict the first.
`EmittedUnion` carries `ValueKind: UnionValueKind` for the same reason, exposing `IsValueType` as
a member so its use sites are unchanged.

The four cases flatten two independent axes — whether a discriminant field exists, and whether
each case gets its own nested type — which is why the type is not named for the discriminant
alone. `SingleCase` and `TypeTested` carry no `_tag`; `EnumLike` and `Tagged` share one.

Each case's doc on `UnionRegime` describes the shape that regime is emitted in **once steps 1–5
land**, not today's flat emission. Steps 1, 2 and 5 make them true in that order.

**Step 0.5 — `NominalEmissionInput` carries the decls. DONE.** `NominalEmissionInput.Union of
UnionDecl | Record of RecordDecl | Class of ClassDecl` (`CodegenTypes.fs:158`), matching the
`Class` arm that already took its decl whole. Steps 1–4 add fields to `UnionDecl` and read them
in `NominalEmit`, rather than widening a tuple and rethreading every match site. `OfUnion` /
`OfRecord`, the `NominalEmit.userInterfacesOf` helper and the three-arm `isStruct` match are
replaced by `NominalEmissionInput.Interfaces` and `.IsValueType`, the latter now covering the
class arm on the same terms as the other two.

**Step 1 — slot keys, nodes and row prediction, both hierarchy regimes. DONE**, together with
step 2: the field move breaks every `ldfld` off the base, so the two land as one change. New
`TypeSlotKey.UnionCase of SymbolKey * case`, `TypeSlotKind.UnionCase`,
`MethodKey.UnionCaseCtor of SymbolKey * case`, `FieldKey.UnionCaseSingleton of SymbolKey * case`.
`FieldKey.UnionCaseField` keeps its shape and only reparents. `buildUnionNodes` moves a case's
fields onto a nested `TypeNode` hung off the union's `Nested`, and the base `.ctor` gains its
`int32` tag parameter. Every case gets a type in both regimes and the base is abstract, so the
regimes are identical here; they diverge only at step 3. Field names move to the rule in decision
5, and a `TypeNode`'s field names are checked unique in this step (see below). Two edits follow:
`Layout.fs:287` must flatten `nominalNodes` into `BuiltKeys` (today it takes only top-level keys,
and the completeness check at `:505` would report the case slots as invented), and `Assembler`'s
type-row walk gains a `TypeSlotKind.UnionCase` arm writing `nestedAttrsOf` against a base supplied
through `TypeRowExtras`. Handle derivation (`Layout.fs:539`), the field pass and
`verifyTypeHandle` are generic over the key types and need no change — which is the check that
this step is right.

`TypeSlotKind.Union` carries the regime AND the value kind, but for different columns: the regime
settles abstract-vs-sealed through `UnionRegime.isHierarchy`, and the value kind drives the
`IsReadOnly` marker and the `ValueType` base.

The rows themselves come from one `structuralRows` (`LayoutNodes.fs`), which takes a
`StructuralRowAttrs` — `concreteStructuralAttrs` or `abstractStructuralAttrs`, selected once per
type — rather than an `isHierarchy` flag threaded into three builders.

**Step 2 — bodies, both hierarchy regimes. DONE.** The step that cannot be subdivided, because moving
the fields breaks every `ldfld` off the base at once. `_tag` stays the discriminant for both
regimes here, so a match arm is unchanged apart from the cast.

**Split `NominalEmit.fs` as part of this step.** It is 1205 lines before step 1 and this step
rewrites the union half of it. The union-specific functions are `prepareUnion`,
`unionStructuralFields`, `tagFieldRefOf`, `prepareUnionStructural` and `register`'s union arm —
roughly 210 lines, interleaved with helpers the record arm shares (`selfMemberRef`, `bodyOf`,
`prepareEqualityTriple`, `prepareComparisonPair`, `prepareStructuralFormat`). Scoping is
top-down, so a `UnionEmit.fs` cannot both call those helpers and be called by `register` while
they sit in one file; the shared helpers move down into a `NominalShared.fs` first, then
`UnionEmit.fs`, leaving `NominalEmit.fs` as `register` / `prepare` orchestration. Splitting while
rewriting these bodies costs little; splitting after steps 3 and 4 have grown them again costs a
merge.

`Emit.fs` splits on the same terms: the synthesised equality / hashing / comparison bodies leave
for an `EmitStructural.fs` beside the `EmitStructuralFormat.fs` that already holds `%A`, taking
`Emit.fs` from 1030 lines back to 535.

`NominalEmit.prepare` dispatches on `NominalEmissionInput` exactly twice — once before the member
loop for the `.ctor` / field / factory rows and the `extends` column, once after it for the
structural bodies. `StructuralMembers.ofInput` supplies the verdicts to both halves, so the
`isDataShape` / `emitsEqualityTriple` / `emitsComparisonPair` / `emitsStructuralFormat` chain and
its two `failwith "unreachable"` arms are gone.

1. `EmittedCase` gains the case's `TypeKey`, from which every use site mints the token it needs;
   `ClrGenerics` reparents `UnionMember.Field` onto the case and gains `UnionMember.CaseCtor` and
   `UnionMember.CaseSingleton`. A generic union's case type registers as a generic CLASS over the
   union's typars (`Assembler.buildPrelude`), so the `TypeSpec`, the `.ctor` ref and each field ref
   all come from machinery that already exists rather than a new dictionary.
2. A case type needs an encodable `FrozenType`. Use the precedent already in the tree:
   `RegisterStackClosureValueType` (`ClrProvider.fs:203`) mints a synthetic `TypeKey` into
   `UserTypes` for a type no signature could otherwise name. `UnionCaseType.key` does the same,
   spelling the nesting as `TypeContainer.InType` so `typeMetaName` yields the emitted
   `Ns.Union`1+Case`. The MATCH arm needs only a token, not a `FrozenType`, which is what lets the
   same code path serve a referenced package's case type — that one is a `TypeRef` no `UserTypes`
   entry names.
3. `buildUnionFactory` becomes `newobj` the case ctor for a payload case and `ldsfld` for a
   nullary one; a new case `.ctor` chains the base with its literal tag and stores its fields —
   that is `Emit.buildClosureCtor` with a different base handle and a leading constant, so it is a
   call, not a copy. `_tag` and the payload fields both become `initonly`, closing gap 7.
4. `EmitPattern`'s union arm keeps the `_tag` test and `castclass`es the scrutinee before each
   extraction. `extractField` generalises to take the source PUSH rather than a slot, so a record
   and a flat union keep loading the scrutinee directly and a hierarchy case casts — a source that
   needs no `FrozenType`, only a token.
5. The structural members become abstract on the base and are implemented per case, so no body
   dispatches at all. See "Structural members dispatch on the runtime type" below, which
   supersedes the per-case dispatch builder this item first described.
6. The external path (`ClrExternalMembers.fs:274-355`) mints case fields on the case `TypeRef` and
   gains a case-type ref.

### Structural members dispatch on the runtime type

`=`, `compare` and `hash` on a union reach it through `EqualityComparer<U>.Default` /
`Comparer<U>.Default` (`src/Vesper.Core/ops-platform.clr.fs:40`, `:66`), which bind
`IEquatable<U>::Equals` and `IComparable<U>::CompareTo` by interface dispatch;
`Vesper.Printf` reaches `%A` through `IStructuralFormattable::Format`; `GetHashCode` is
`Object`'s slot. All four are therefore *already* dispatched virtually before any body runs.

In a hierarchy regime those four slots become **abstract on the base**, and each case type
overrides them. The dispatch that already happens then lands on the case's own implementation, so
no body compares a tag to select an arm. Per case:

| member | attrs | body |
| --- | --- | --- |
| `Equals(U)` | override | `ldarg.0; ldarg.1; isinst <Case>; call Equals(<Case>); ret` |
| `Equals(<Case>)` | public, non-virtual | null ⇒ false, else the case's own field walk |
| `GetHashCode()` | override | `HashCode` seeded with the case's tag as a **literal**, then its own fields |
| `CompareTo(U)` | override | its case ⇒ `call CompareTo(<Case>)`; otherwise the ordinal difference |
| `CompareTo(<Case>)` | public, non-virtual | null ⇒ 1, else the case's own field walk |
| `Format(IFormatSink)` | override | `BeginCase(name); Child (box field)×k; EndCase` |

`Equals(object)` and `CompareTo(object)` stay concrete on the base and `callvirt` the typed slot,
so neither gets a row per case. `Equals(U)` needs no branch of its own: `isinst` yields `null` for
another case, and the null guard inside `Equals(<Case>)` already answers that as `false`.

A case's field walk is the **record** walk over its own fields — `buildRecordFieldEquality` and
`buildRecordFieldComparison` (`Emit.fs:701`, `:843`) already have that shape, and a case's
`Format` is `buildRecordFormat` with `BeginCase` / `EndCase` in place of
`BeginRecord` / `EndRecord`. This collapses the union/record duplication rather than adding a
fourth open-coded switch: `buildUnionFormat`'s tag dispatch (`EmitStructuralFormat.fs:112-144`)
and `buildTagAndFieldEquality` / `buildTagAndFieldComparison` are all reached only by the flat
regimes afterwards.

The flat regimes keep today's bodies whole. `SingleCase` and `EnumLike` have no case type to
override anything, and a `[<Struct>]` union has no subclass at all, so the tag-then-every-field
walk stays their implementation permanently, and `structuralRows` (`LayoutNodes.fs`) selects the
attrs by regime.

**Which slots exist, and which of them are abstract, are each decided once.**
`UnionCaseSlot.required` (`LayoutModel.fs`) takes a `StructuralMembers` and yields the case type's
slots in row order; `LayoutNodes` turns each into a `MethodRow` and `UnionEmit` prepares a body
for each, so the rows a case declares and the bodies prepared for it cannot describe different
sets. `PreparedMethod.Body` is a `PreparedBody` (`Abstract | At of offset`) rather than an
offset with `-1` meaning abstract, and `Assembler.WriteMethods` pairs it against the row's
`MethodAttributes.Abstract` bit, failing on a disagreement. Without that pairing an abstract row
carrying a body — or the reverse — writes a PE the loader rejects and no golden reports.

**Three of the four stop reading a discriminant.** `Equals`, `GetHashCode` and `Format` become
regime-independent, since `isinst` and a literal tag serve `TypeTested` and `Tagged` alike. Only
`CompareTo` still needs an ordinal for `other`, the one value it did not dispatch on.

**Step 3 — move `TypeTested`'s readers off the tag. DONE**, together with step 5's first half,
its prerequisite. After the structural members move to the case types, the surviving readers are
the match arm and `CompareTo`'s ordinal for `other`. The arm's test is a DU beside the provider
interface:

```fsharp
type UnionCaseTest =
    /// A single-case union: every value inhabits the case, so no test is emitted.
    | Irrefutable
    /// Load `tagField` off the scrutinee and compare with the case's `tag`.
    | TagEquals of tagField: EntityHandle * tag: int
    /// `isinst` the case's own type: a non-null result settles the case.
    | IsInst of caseType: EntityHandle
```

`UnionCaseTest.ofRegime` maps the five regimes onto these three, so the mapping is total and
lives in one place, shared by the local match path and the referenced-package one; it takes the
`_tag` and case-type mints as thunks, each forced exactly under the case that reads it, because
the handles differ by scope (`Def` token, self-`TypeSpec`, use-site instantiation). Two
predicates beside `classify` name the layout facts the emitter branches on:
`UnionRegime.hasTagRow` (false only for `SingleCase` until step 4 adds `TypeTested`) and
`UnionRegime.readsTag` (false for `SingleCase` and `TypeTested`) — merged into one
`UnionRegime.hasTag` by step 4, which adds `UnionCtorShape.ofRegime` beside it.
**Step 5's first half was a prerequisite for this step**, not merely wanted early: `SingleCase`
yields `Irrefutable`, and a single-case union carried a `_tag` its match arm and its flat
structural bodies still loaded until that field was dropped.

`EmittedUnion.TagField` is an `EntityHandle voption`, `ValueSome` exactly where the discriminant
IS the tag. A `TypeTested` union therefore already hands no tag handle to any consumer, ahead of
step 4 deleting the row itself; the row's one surviving writer is the base `.ctor(int32)`, which
reaches it by `FieldKey.UnionTag` directly.

The match arm reads its test off the discriminant: a local hierarchy case lands in a case-typed
local — filled by the single `isinst` in `TypeTested`, by one `castclass` after the tag compare
in `Tagged` — and an arm extracting nothing takes neither the local nor the cast. A referenced
package's case type is a token no `FrozenType` names, so no local can be typed at it and each
extraction casts the scrutinee in place instead. The external path's `externalUnionTag` became
`ExternalUnionCaseTest`, answering through the same `UnionCaseTest.ofRegime` over
`UnionRegime.ofExternalShape`; the cons-list's provider arm answers `IsInst` on its nested case
types and its `_tag` recipe is deleted.

`Case::CompareTo(U)` is the only structural body this step touched, because it alone needs an
ordinal for `other`, the value it did not dispatch on: `EmitStructural.OtherOrdinal` is
`ldfld _tag` in `Tagged`, and in `TypeTested` a bounded `isinst` chain over the remaining cases'
tokens, minted once per union and shared with each case's own dispatch.

**Step 4 — delete the vestigial field. DONE.** `TypeTested` declares no `_tag` row and its base
`.ctor` is nullary, so its base carries no fields at all.

`hasTagRow` and `readsTag` coincided once the row went, so they are one `UnionRegime.hasTag`: the
tag exists exactly where a consumer loads it, and two predicates over one fact have nothing to
catch a divergence. `MetadataStructureTests` pins an all-payload `TypeTested` union's base at zero
field rows against a four-case `Tagged` one at `_tag`, and the `Shape` row pin drops to
`_unique_Dot` alone.

The ctor shape itself moved into the type system as `UnionCtorShape`, `ofRegime` mapping
`(value kind, regime)` onto `FlatTagged` / `Flat` / `TagOnly` / `Nullary` — the same
one-mapping-two-consumers shape `UnionCaseTest.ofRegime` already takes. `UnionEmit` builds the
`MethodDef` from it and `ClrGenerics` the generic `MemberRef` signature from it, so the two ends
agree by construction rather than by the "Mirrors `UnionEmit.prepareUnion`" comment that used to
hold them together, and a sixth regime cannot compile until both matches are extended.

Step 3's prediction that a case `.ctor` is `Emit.buildClosureCtor` "with a different base handle
and a leading constant, so it is a call, not a copy" is now literal: `buildClosureCtor`,
`buildRecordCtor` and `buildUnionCaseCtor` are one `Emit.buildChainedCtor baseCtor baseArgs
fields`, whose `Call` arity is `List.length baseArgs + 1` rather than a hand-written 1 or 2.
`baseArgs` is `[ ILInstr.LdcI4 tag ]` under `TagOnly` and empty everywhere else.

**Step 5 — `SingleCase` and `EnumLike` polish. First half DONE with step 3**, which it gates: a
single-case union declares no `_tag` row, its `.ctor` (the flat struct form included) takes its
fields alone, its factory stores no tag, its structural walk is the record's, and its `%A` body
is the one a hierarchy case type carries. That settles the collision below. The second half —
`_unique_<Case>` singletons for an enum-like union, so nullary construction stops allocating —
remains, and is independent of steps 1–4.

**Step 6 — fix a name resolution gap.** A bare union case declared in a module-held union does not resolve across a file boundary; the namespace-level form does. `test/XParsec.FSharp.Codegen.Clr.Tests/CrossFileTests.fs:216` routes around it — the cross-file `obj`-box test declares `type Holder = Wrap of obj` at namespace level with a comment saying why. Contrast `CrossFileTests.fs:320`, where a module-held union is reached cross-file, and `LongIdentResolutionTests.fs:157` ("module-held case, bare after open, construct + match"), which passes within the same file. So the missing piece is the module-held case's bare spelling specifically on the cross-file provider channel, not module-held unions in general.

### A latent name collision

`type C = C of tag: int` is a `SingleCase` union, so its payload sits on the same type as the
discriminant, and under the naming rule that payload is called `_tag` — the same name and
signature as our discriminant field. FSC never meets this, because a single-case union carries no
discriminant at all (verified: `K of tag: int` emits one field, `_tag : Int32`, which *is* the
payload). `EnumLike` is safe from the other side: its cases have no fields to name. In the
hierarchy regimes it is benign — the case field lands on the case type while the discriminant
stays on the base, and FSC emits exactly that shadowing pair, base `_tag : int32` and case
`_tag : string`, which loads and matches correctly.

Step 5's first half has since landed with step 3, so the collision is closed: a single-case
union declares no discriminant row, the payload owns the name `_tag` outright, and
`MetadataStructureTests` pins that `type C = C of tag: int` compiles to exactly that shape.
What the naming rule did gate is a **field-name uniqueness
check per `TypeNode`**, added in step 1 alongside the completeness check at `Layout.fs:505`.
Without it the emitter writes two `FieldDef` rows of one name and signature on one type: the two
`FieldKey`s differ, so nothing collides internally and the result is silently invalid metadata
rather than a failure. The check is worth having permanently and covers records, classes and
closures on the same pass.

## Decisions taken

1. **All four regimes, at FSC's threshold of 4.** Settled; `Tagged` uniformly was the alternative
   and gives up the single-`isinst` match on the stdlib's hottest unions.
2. **`_tag` survives both hierarchy regimes until step 4.** The layout and metadata work lands
   under bodies that still read a field, and the discriminant changes exactly once, after every
   reader has moved. Steps 3 and 4 split so that "the field is now vestigial" is a type-level
   fact before the row is removed. It stays permanently in `Tagged`, where it earns its 8 bytes
   by keeping the structural bodies O(1) and leaving room for a later jump-table dispatch on a
   many-armed match.
3. **A nested type for every case, and a uniformly abstract base**, deviating from FSC's
   nullary-as-base-instance in `Tagged`. Metadata-only cost, and it removes a branch from every
   consumer of `EmittedCase`.
4. **Where `_tag` exists, every read of it is `ldfld`.** No `Tag` property, and in particular no
   abstract or virtual slot: the tag is never reached by dispatch. `UnionCaseTest.TagEquals`
   carries a *field* handle, so this is unrepresentable rather than merely intended — a consumer
   holding it has nothing to call. FSC emits a `get_Tag` property; we have no caller for one,
   because match arms and structural bodies both load the field inline.

5. **A field takes the name the source declared, and FSC's positional name otherwise.**
   `of radius: float` emits `_radius`, matching FSC's backing-field spelling; a positional field
   emits `item` when the case has one field and `item<n>` otherwise. The index is the field's
   **position in the case**, not a counter over the positional fields alone — FSC emits
   `M1 of tag: string * float` as `_tag` and `item2`, and the JS backend already numbers the same
   way. One convention across both emitters is what would let the external path read an
   FSC-emitted union directly, which today needs provider recipes for FSharp.Core's list and
   option (`EmitConstruct.fs:201`).

   The data is already carried: `TUnionCaseG.Fields` is `(string voption * ty)` with `ValueNone`
   for a positional field (`TastDecl.fs:84-87`), and today's CLR emitter discards it in favour of
   `sprintf "%s_%d"`. The JS backend implements this exact rule in `EmitJsTypes.synthFieldNames`
   (`:36-46`), differing only in spelling `Item` where the CLR wants `item`. Lift the
   target-neutral part — declared name, or 1-based position — and let each backend spell it,
   rather than writing the rule a second time.

   The `_` prefix is FSC's, and there it avoids the property of the same name. We emit no
   properties, so it buys us only the shared convention; that is reason enough, and it is what
   opens the latent collision described under step 5.

6. **A union case is not a type in the source language.** No expression is ever statically typed
   at a case: the case type is a synthetic backend `TypeKey`, minted the way
   `RegisterStackClosureValueType` (`ClrProvider.fs:203`) mints one for a type no signature could
   otherwise name. So a direct call the emitter grows later — a fast path for `=` on a union
   field, a `callvirt Format` in place of erasing through `sink.Child(box …)` — arrives holding a
   value typed at the **union**, and binds `IEquatable<U>` / `IComparable<U>`. That is the slot
   the abstract-per-case shape makes land on the case's implementation immediately.

7. **The field walk lives in `Equals(<Case>)` / `CompareTo(<Case>)`, and the `U`-typed override is
   the guard.** One entry point holds the predicate; the other is four instructions. Splitting it
   the other way, or holding the walk in both, gives two bodies asserting one predicate with
   nothing to catch a divergence.

   The typed pair is emitted as plain public methods and the case type does **not** declare
   `IEquatable<<Case>>` / `IComparable<<Case>>`. The `InterfaceImpl` row and the interface
   `TypeSpec` pay off only for `EqualityComparer<<Case>>.Default`, which needs a case-typed
   *generic argument*, and decision 6 says none can arise. Declaring the interfaces later is
   purely additive — no base change, no body change, no signature change — whereas moving the walk
   later is not, which is why the split lands now and the interfaces do not.

## Recorded, absent an objection

8. **Seal our case classes** (FSC leaves them unsealed). Note that `LayoutModel.fs:28` currently
   justifies `call` dispatch on members by the union being sealed; that justification is wrong
   today — the members are non-virtual, which is what makes `call` bind — and becomes visibly
   wrong when the base stops being sealed. The comment gets corrected in step 1.
9. **Nullary cases keep a static factory method**, not FSC's static property, so
   `EmitConstruct.buildUnionCons` and the `UnionMember.Factory` `MemberRef` signature are untouched.
10. `Tags`, `Is<Case>`, `get_Item`, `__DebugDisplay` and the debug proxies are **non-goals**.

## Scope and risk

Under the classifier, only reference unions with ≥2 cases and ≥1 payload case change shape. In
`src/Vesper.*` that is the cons-list, `Option` and `Result`, all three `TypeTested`. `Choice`2`…
`Choice`7` are declared `[<Struct>]` (`src/Vesper.Choice/choice.fs:4`), so they stay flat and
`ChoiceTests` needs no change — the plan's earlier reading of them was wrong. Step 2 therefore
moves the cons-list, `Option` and `Result` onto the hierarchy at once, and steps 3–4 change only
how they discriminate.

The suites that read the flat representation directly, and were rewritten rather than
re-snapshotted, are `MetadataStructureTests.fs` (the pinned `Shape` field/method rows, now an
abstract base plus a nested type per case), `StructuralEqualityTests.fs` (the typed `Equals(Self)`
is abstract, not final) and `UnionTests.fs` (the cons-list's head field, now `_Head` on the nested
`Cons`). `StructTests.fs:797` is a struct union and stays. The CLR conformance goldens churn;
regenerate with
`./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests" -UpdateSnapshots`
**after** reading a sample diff, since a wrong layout also produces a self-consistent golden.

Per this project's `CLAUDE.md`, assert on the metadata through `MetadataStructure` — the nesting
rows, the per-case field ranges, the `extends` column, and the `GenericParam` rows on a generic
union's case type — ahead of loading the assembly and reflecting. The regime boundaries (3 vs 4
cases, all-nullary vs mixed) get a test each, because they are a threshold rule and will otherwise
drift.
