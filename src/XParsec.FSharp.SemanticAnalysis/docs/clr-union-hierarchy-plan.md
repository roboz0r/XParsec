# CLR reference unions as a class hierarchy

Supersedes the "Reference unions emit flat, not as F#'s class hierarchy" gap in
[`du-architecture.md`](du-architecture.md) (`:275`). Struct unions keep the flat
`(tag, every case field)` shape permanently — a value type cannot inherit — so everything
below is scoped to `UnionValueKind.Reference`.

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
to deviate is the emitter, not the runtime — `EmittedCase` always carries a case type, so no
consumer branches on whether a case has one, and the two hierarchy regimes then differ in exactly
one axis, whether `_tag` exists. The cost is one `TypeDef` per nullary case in `Tagged`, which is
metadata only: the singleton is still one instance, allocated once.

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

**Step 0 — the classifier.** A `UnionRepr` DU with the four cases above, behind one pure
`classify` over the case shapes — the count, and which cases carry no fields. Both inputs are
available locally and on a referenced package's union shape, so the local and external paths call
the same function and the threshold rule has a single definition. The result is carried on
`TypeSlotKind.Union`, on `EmittedUnion` and into the generic-union shape rather than re-derived at
use sites. `SingleCase` and `EnumLike` route to today's flat emission and stay there; the other
two route to it until step 1 lands, which is what keeps each step green.

**Step 1 — slot keys, nodes and row prediction, both hierarchy regimes.** New
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

**Step 2 — bodies, both hierarchy regimes.** The step that cannot be subdivided, because moving
the fields breaks every `ldfld` off the base at once. `_tag` stays the discriminant for both
regimes here, so a match arm is unchanged apart from the cast.

1. `EmittedCase` gains the case's `TypeDef` handle and its `.ctor`; `ClrGenerics` gains a case
   `TypeSpec` and reparents `UnionMember.Field` onto it, plus a `UnionMember.CaseCtor`.
   `GenericUnionShape`'s per-case record carries the case `TypeDef`, so no new dictionary appears.
2. A case-typed local needs an encodable `FrozenType`. Use the precedent already in the tree:
   `RegisterStackClosureValueType` (`ClrProvider.fs:199`) mints a synthetic `TypeKey` into
   `UserTypes` for a type no signature could otherwise name. A case type registers the same way.
3. `buildUnionFactory` becomes `newobj` the case ctor for a payload case and `ldsfld` for a
   nullary one; a new case `.ctor` chains the base with its literal tag and stores its fields —
   that is `Emit.buildClosureCtor` with a different base handle and a leading constant, so it is a
   call, not a copy. `_tag` and the payload fields both become `initonly`, closing gap 7.
4. `EmitPattern`'s union arm keeps the `_tag` test and adds a `castclass` into a case-typed local
   before extraction. Generalise `extractField` (`EmitPattern.fs:158`) to take the source slot; the
   record arm keeps passing the scrutinee.
5. Equality, hashing, comparison and `%A` each gain a per-case downcast walk. Write **one**
   per-case dispatch builder taking `(this, other)` and fold `buildUnionFormat`'s open-coded tag
   switch (`EmitStructuralFormat.fs:112-144`) into it; four hand-rolled switches is the
   predictable failure mode of this step.
6. The external path (`ClrExternalMembers.fs:274-355`) mints case fields on the case `TypeRef` and
   gains a case-type ref.

**Step 3 — move `TypeTested`'s readers off the tag.** The discriminant becomes a DU on
`EmittedUnion`:

```fsharp
type UnionDiscriminant =
    /// `Tagged` / `EnumLike`: the `_tag` field handle every test and structural body loads.
    | TagField of EntityHandle
    /// `TypeTested`: a case's runtime type discriminates it.
    | TypeTest
```

A `TypeTested` union then carries no tag handle, so no consumer can emit a load of one: the match
arm collapses to a single `isinst` into the case-typed local, replacing both the tag compare and
step 2's `castclass`. The per-case dispatch builder from step 2 takes the discriminant, so both
regimes keep sharing it. The external path's `externalUnionTag` (`ClrExternalMembers.fs:309`)
resolves through the same `classify`, so a referenced package's 2–3-case union is matched by type
test too.

A `get_Tag` property is emitted for convenience only. Equality and `%A` need only to *dispatch*, which the
`isinst` chain already is, and inside a dispatch arm the case is known, so hashing seeds with a
literal rather than a load. Comparison is the one body needing an ordinal for a value it did not
dispatch on — `other` — and takes a bounded `isinst` chain of at most two tests inline. Should
that prove worth factoring into a method, it is a private non-virtual one reached by `call`.

**Step 4 — delete the vestigial field.** Drop the `_tag` field row and the base `.ctor`'s tag
parameter for `TypeTested`, leaving its base with no fields. The proof that the field is
vestigial is step 3's type change rather than an audit: the `TypeTest` case carries no handle, so
a surviving reader cannot compile. A `MetadataStructure` assertion that a 2–3-case union's base
carries no field rows pins the outcome.

**Step 5 — `SingleCase` and `EnumLike` polish.** Drop the redundant `_tag` from a single-case
union; give an enum-like union `_unique_<Case>` singletons so nullary construction stops
allocating. No hierarchy involved, and independent of steps 1–4, but the first half should land
early for the reason below.

### A latent name collision

`type C = C of tag: int` is a `SingleCase` union, so its payload sits on the same type as the
discriminant, and under the naming rule that payload is called `_tag` — the same name and
signature as our discriminant field. FSC never meets this, because a single-case union carries no
discriminant at all (verified: `K of tag: int` emits one field, `_tag : Int32`, which *is* the
payload). `EnumLike` is safe from the other side: its cases have no fields to name. In the
hierarchy regimes it is benign — the case field lands on the case type while the discriminant
stays on the base, and FSC emits exactly that shadowing pair, base `_tag : int32` and case
`_tag : string`, which loads and matches correctly.

No source in the tree declares such a field, so this does not gate the naming rule; step 5 removes
the discriminant well before anyone writes one. What it does gate is a **field-name uniqueness
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
   abstract or virtual slot: the tag is never reached by dispatch. `UnionDiscriminant.TagField`
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

## Recorded, absent an objection

6. **Seal our case classes** (FSC leaves them unsealed). Note that `LayoutModel.fs:28` currently
   justifies `call` dispatch on members by the union being sealed; that justification is wrong
   today — the members are non-virtual, which is what makes `call` bind — and becomes visibly
   wrong when the base stops being sealed. The comment gets corrected in step 1.
7. **Nullary cases keep a static factory method**, not FSC's static property, so
   `EmitConstruct.buildUnionCons` and the `UnionMember.Factory` `MemberRef` signature are untouched.
8. `Tags`, `Is<Case>`, `get_Item`, `__DebugDisplay` and the debug proxies are **non-goals**.

## Scope and risk

Under the classifier, only unions with ≥2 cases and ≥1 payload case change shape. In `src/Vesper.*`
that is `Lst`, `Option`, `Result` and `Choice`2`…`Choice`7`; all but `Choice`4`…`Choice`7` land in
`TypeTested`. Step 2 therefore moves the whole stdlib onto the hierarchy at once, and steps 3–4
change only how the majority of it discriminates.

The suites that read the flat representation directly, and must be rewritten rather than
re-snapshotted, are `ChoiceTests.fs:37-43` and `:84-87` (reflecting `_tag` and `<Case>_<i>` off the
union type — every case there is a lone positional field, so `Choice1Of2_0` becomes `item` on the
case type in step 2, and `Choice`2` / `Choice`3` lose `_tag` in step 4),
`MetadataStructureTests.fs:164-168` (the pinned `Shape` field/method rows, where `Line_0` becomes
`item`), `OptionTests.fs:226` and `ListModuleTests.fs:179-189` (comments asserting the `_tag`
compare).
`StructTests.fs:797` is a struct union and stays. The CLR conformance goldens churn; regenerate with
`./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests" -UpdateSnapshots`
**after** reading a sample diff, since a wrong layout also produces a self-consistent golden.

Per this project's `CLAUDE.md`, assert on the metadata through `MetadataStructure` — the nesting
rows, the per-case field ranges, the `extends` column, and the `GenericParam` rows on a generic
union's case type — ahead of loading the assembly and reflecting. The regime boundaries (3 vs 4
cases, all-nullary vs mixed) get a test each, because they are a threshold rule and will otherwise
drift.
