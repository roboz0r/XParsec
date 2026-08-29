# One round trip per external type

*UNSTARTED (written 2026-08-13). Step 1 is a hard prerequisite for step 2; step 3 is a hard
prerequisite for step 4, and taking 4 first would carry the bare/arity-suffixed key wart into
the new channel where it is much more expensive to remove.*

*This plan SUPERSEDES the `ProviderDecorator` two-level-base sketch raised in review of
`f4a32ab8`: that restores the compile-time totality the decorator gave up, but the decorator
itself is what this plan deletes, so the base class would be thrown away.*

## The defect

An external type's facts are fetched through SEVEN independent by-key channels, each walked
separately, through every composed layer, on every query:

| `IExternalSymbolStore` channel | what it addresses |
|---|---|
| `TryLookupType: SymbolKey` | one type |
| `TryLookupMembers: SymbolKey * string` | one type |
| `TryLookupMemberByKey: MemberKey` | one type |
| `TryLookupIndexSignature: SymbolKey` | one type |
| `IsValueType: TypeKey` | one type |
| `TryLookupByKey: SymbolKey` | a BINDING, not a type |
| `IntrinsicTypeMap` | the whole provider |

Five of the seven address exactly one type. They are one query wearing five hats, and the cost
of that shows up in four places:

1. **The metadata tail already does the round trip and nobody consumes it.** `MetadataSymbols.computeType`
   (`:468-493`) enumerates properties, methods, indexers, fields, ctors, interfaces, base type,
   flags and origin in ONE locked reflection pass, and hands them back on
   `ExternalClassShape.Members`. `computeMembers` (`:508`) then re-resolves the same `Type` and
   re-reflects its members per member NAME, into a second cache. Two caches (`:236`, `:239`),
   two reflection passes, one type.

2. **Every wrapper must re-state every channel.** `ProviderDecorator`
   (`ExternalSymbolProviders.fs:212-267`) exists solely to spare three wrappers from writing
   twelve forwarding members each. It buys that by defaulting every channel to a forward — which
   is safe in `memoize` (a missed cache is slow), tolerable in `withInlineBodies`, and a
   SOUNDNESS hazard in `mapProviderTypes`, whose entire job is to put a variance polarity on
   every `FrozenType` that leaves the provider. A channel added to the contract now escapes
   variance mapping silently. Before the decorator, the compiler refused to build.

3. **Composition rules get re-derived downstream.** `IsValueType`'s ladder — the target's layout
   first, the declaration second — is `TypeLayout.resolve`, which both ends now share, but each
   still supplies its own `declared` rung (the front end's local `TypeRegistry`, the backend's
   `EmitEnv.Classes` / `.Records`) and `CodegenSymbols.ofProvider` (`:110`) keeps its own fold of
   the two provider rungs. That is the tell: a ladder over composed sources is a property OF THE
   COMPOSITE, and
   until `stack` settles it, every consumer holding a different subset of the rungs has to
   re-derive the order.

4. **`stack` is already the composite-with-transform, and is already used as one.**
   `ReferencedProject.fs:383` is `stack (ValueSome home) ambient [ inner ]` — a single-source
   `stack` whose only purpose is to apply a per-entry home stamp. The wrappers are that same job
   done in the wrong place.

## The shape

`IExternalSymbolStore`'s five type-addressed channels collapse to one:

```fsharp
/// Everything a resolved external type states, from ONE fetch. A provider fills this once
/// per key; nothing downstream re-reads a source about a type it already holds.
type ResolvedType =
    {
        Key: TypeKey
        Shape: ExternalTypeShape
        /// FULL member list in DECLARATION order: the by-name overload scan and the by-key
        /// selection are both orderings of this one list.
        Members: EqArray<ExternalMember>
        IndexSignatures: EqArray<ExternalIndexSignature>
        /// Settled by the COMPOSITE: the target's layout, else what the declaration asked for.
        IsValueType: bool voption
    }

type IExternalSymbolStore =
    abstract TryResolveType: key: TypeKey -> ResolvedType voption
    abstract TryLookupByKey: key: SymbolKey -> ExternalSymbol voption
    abstract IntrinsicTypeMap: IntrinsicTypeMap
```

`TryLookupMembers` and `TryLookupMemberByKey` become pure selectors over `ResolvedType.Members`
in `module ExternalSymbols` — no provider, no interface, no cache.

**`IExternalSymbolResolver` does not change.** It is the name→identity half, and it cannot fold:
`TryRecordsWithField` is a REVERSE index over every type a source declares, and
`AmbientOpenPrefixes` is not per-type at all. The existing two-interface split already names this
boundary correctly; this plan only makes the store half honest about being one query.

### What folds, and what it deletes

| today | after |
|---|---|
| 5 by-key channels | `TryResolveType` |
| `mapProviderTypes` (wrapper) | a `ResolvedType -> ResolvedType` passed to `stack` |
| `withInlineBodies` (wrapper) | a field filled at resolve time |
| `memoize`'s 11 dictionaries | one `TypeKey -> ResolvedType voption` |
| `ReferencedProject.wrap`'s 1-source `stack` | a stamp argument, which it already is |
| `CodegenSymbols.isValueType`'s ladder | settled in `stack`'s fold |
| `TypeLayout.declaredOf`'s external rung | one `TryResolveType` read |
| `ProviderDecorator` | deleted |
| `MetadataSymbols.computeMembers` + `membersCache` | deleted; `computeType` already has the data |

## Steps

### 1. `ResolvedType`, and the selectors, over the CURRENT channels

Introduce the type and `ExternalSymbols.members` / `.memberByKey` / `.indexSignatures`
selectors. Add `TryResolveType` to `IExternalSymbolStore` as a DEFAULT implemented over the five
existing channels, so every provider keeps compiling. Move consumers onto it one at a time.

No behaviour change. This step is only about there being one name for the query before there is
one implementation.

### 2. Move each source onto `TryResolveType` natively, and delete the five channels

Four channel construction sites in `src` (`JsNativeSymbols.fs:72`, `TsManifestProvider.fs:189`,
`FrozenSignature.fs:474`, `VesperLib/TyparCapture.fs:256`) plus `MetadataSymbolProvider`, which
implements the interface directly. The contract sources already hold whole-type dictionaries
(`KeyIndexedChannels.ShapesByKey` / `.MembersByKey`), so for them this is a join, not new work.

`MetadataSymbolProvider` is where the win lands: `computeType` becomes the sole reflection pass
and `computeMembers` goes.

**Settle here:** `ExternalClassShape.Members` (`ExternalDeclarations.fs:427`) becomes redundant
with `ResolvedType.Members` and must not survive as a second home. Two other shapes carry a
`Members` field (`:488`, `:549`) — verify what they are before assuming the same applies.

### 3. Fix the key convention at the source

`CodegenSymbols.reconciledLookup` (`:12-22`) exists because sources disagree on whether a generic
type is keyed BARE (`Vesper.Option`, contract layer) or ARITY-SUFFIXED (`Vesper.Option\`1`,
metadata layer). It double-probes every lookup to paper over that.

If `TryResolveType` inherits the double probe, the wart survives the refactor in a channel that
is now the ONLY way to reach a type — strictly worse than today. Pick one convention, make each
source key on it, delete `reconciledLookup`.

*This commit generalising `reconciledLookup` to `'a voption` so it could serve a second channel
is the signal: the next use is the one to remove it before, not after.*

### 4. Fold the transforms into `stack`

`stack` gains a `ResolvedType -> ResolvedType` (already has its `stampHome`, which becomes one
instance of it) and settles `IsValueType` in the fold. `mapProviderTypes` and `withInlineBodies`
become the two functions passed in; `memoize` becomes one dictionary and may collapse into
`stack` outright.

Call sites to re-point: `NumberCovariance.fs:48`, `SymbolProviders.fs:282`, `PassContext.fs:239`,
`TsManifestProvider.fs:212`, `ReferencedProject.fs:383`.

## Settled design rules

1. **A transform CONSTRUCTS a `ResolvedType`; it does not `{ r with … }` it.** Record
   copy-and-update forwards unstated fields exactly the way the decorator defaults do — it would
   move the hazard, not delete it. A variance mapper that spells every field turns "a new fact
   escaped mapping" into a compile error in the one place that matters. This is the whole reason
   the redesign beats patching `ProviderDecorator`; do not give it up for brevity.

2. **`TryLookupByKey` (bindings) stays as it is.** It already returns one whole `ExternalSymbol`
   from one fetch — it is not the defect. Giving it a matching `ResolvedSymbol` is optional
   symmetry and explicitly NOT in scope.

3. **No `Lazy` fields on `ResolvedType` up front.** Both sources that matter already resolve
   eagerly. Add per-field laziness only when a specific source is shown to need it.

4. **`IExternalSymbolResolver` is untouched.** Anything reaching for it during this work is a
   sign the name→identity half is being dragged in; it is a different query and stays one.

## Constraints

- **17 test files build mock providers as `{ NamedChannels.empty with … }`** and must not churn.
  `NamedChannels` / `KeyIndexedChannels` are the ergonomic surface, `IExternalSymbolStore` is the
  contract, and only the latter is changing shape. Keep `NamedChannels` field-per-channel and derive
  `TryResolveType` in `ofNamedChannels`, so a source that models three channels still states three.
- **This is a re-indexing, not new eagerness.** If any step makes a source resolve more than it
  resolves today, that step is wrong — the eager work is already happening (twice, at the
  metadata tail).
- `MapProviderTypesTests` is the regression gate for rule 1: it pins that a record field is
  covariant and an index key contravariant. It must keep passing channel-for-channel.

## Not in scope

- ~~`EmitPattern`'s hand-rolled value-type key list~~ **DONE (2026-08-13)** — it classifies
  through `TypeLayout.shapeOfFrozen` and the shared ladder now. One shape still has no key for
  the query to resolve under; `tuple-platform-type-plan.md` carries it.
- `IntrinsicTypeMap` as a provider-level channel. The canon↔repr edge is walked at ~6 independent
  sites; resolving a canon THROUGH its repr once is a natural consequence of this plan but not a
  precondition for it.

## Anchors (verify before editing)

- The contract: `IExternalSymbolStore` (`ExternalSymbols.fs:72-103`), `IExternalSymbolResolver`
  (`:50-67`), `ICodegenSymbols` (`:141-161`).
- The wrappers: `ProviderDecorator` (`ExternalSymbolProviders.fs:212-267`), `mapProviderTypes`
  (`:457`), `withInlineBodies` (`:569`), `memoize` (`:601`), `stack` (`:279`), `composite`
  (`:448`).
- Channel construction: `ofKeyedChannels` (`:158`), `NamedChannels` (`:37`), `KeyIndexedChannels` (`:68`),
  `KeyedChannels` (`:99`).
- The double reflection pass: `MetadataSymbols.fs:236`, `:239` (caches), `:468` (`computeType`),
  `:508` (`computeMembers`), `:626-639` (the two by-name lookups).
- The value-ness ladder: `TypeLayout.resolve`, shared by both ends, each supplying its own
  `declared` rung; and `CodegenSymbols.fs:110` (inside `ofProvider`, so `ICodegenSymbols`
  publishes the settled provider verdict and no emission site re-derives it).
- The key-convention patch: `CodegenSymbols.fs:12-22`.
- Composition sites: `AssemblyFiles.fs:130`, `:139`; `ReferencedProject.fs:383`, `:519`, `:554`;
  `ClrDriver.fs:143`; `SymbolProviders.fs:262`, `:282`.
- Shapes carrying a `Members` field: `ExternalDeclarations.fs:427`, `:488`, `:549` — confirm
  which are genuinely per-type before collapsing any of them.
