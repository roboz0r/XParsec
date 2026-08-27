# Long-identifier resolution — the remaining deletions

**Status (2026-08-26): R1–R8 are LANDED (R8 uncommitted, awaiting review).** This doc scoped
what was left of the original step 5 ("delete the speculation"), re-planned after a wiring
survey corrected one of its premises. The original design (FCS-shaped algorithm, forks A–C,
steps 1–4) was confirmed by the user on 2026-08-23 and is now code. R8's five steps all landed
on 2026-08-26 under the confirmations in §5; the scope is complete, so this doc is ready for
deletion once the deferred list at the end of §4 moves to its homes.

## 1. What exists now

- `Passes/NameResolution/LongIdent.fs` resolves expression / pattern / type positions in FCS's
  order over two `IScopeContents` halves, local (`LocalScope.fs` over `TypeRegistry`) first,
  then the composite provider stack. One result type (`ResolvedItem`), one stamp
  (`Resolution.Resolved: NodeKey → ResolvedItem`); every consumer projects it through
  `ResolvedStamps`, and the per-kind external stamps, `LocalModules` and the seven-disjunct
  suppression list are gone.
- `Kind.RequireQualifiedAccessCase` (FS0035) reports an RQA case reached other than through
  its union's name, in both positions; a bare RQA case is not in the environment.
- Both surface producers publish a value through `PublishedSurfaceBuilder.addValue`
  (`PublishedSurface.fs:130-145`), handing it a `SourceSpelling` (the declaring scope's
  source path plus the binding's source short name). It files the value under its
  `BindingKey`, the source spelling under `SourceSpellings` where the two differ, and the
  module's source path into `ModuleContainers`, so a `ModuleSuffix` module's source path is a
  container — and a path a type resolves through — whichever half built the surface (R1).
  `scopeOf` files a value in its declaring container under its key's short name and under its
  source spelling's, so `Vesper.Set.empty` reaches `SetModule.Empty` (R2, R5).
- A value is reached through its declaring container or by `BindingKey`; the provider stack
  renders a value name nowhere (R6).
- Expression position resolves `modulePath → typeFirst → folded`; pattern position
  `typeFirst → modulePath → folded`. Both read the order off `qualifiedReadings`.
- Every published scope is TOTAL: a source answering for a container answers for its values
  and its types alike, the TS manifest included, which publishes a `PublishedSurface` like
  any other referenced package (`TsManifestProvider.publicationOf`).
- Suites at the last landed step (R7.1): SemanticAnalysis 1483, Clr 1550, Js 667.

## 2. Premises corrected by the wiring survey (2026-08-24)

The original step-3 follow-up assumed `Set.singleton` against the built `Vesper.Set` resolved
through a scope-less metadata channel, and gated the original step 5 on "a consolidated signature file
overlaid on a referenced Vesper assembly". The survey shows the overlay **already is the live
path**:

- A referenced Vesper package is consumed by `PackageProviders.buildProviderSeeded`
  (`PackageProviders.fs:115-191`) → `AssemblyAnalysis.analyseUnits` under
  `Publication.AcrossAssemblies`: the `.fsi` half through `SignatureResolution.resolveFile`
  (`AssemblyAnalysis.fs:223-244`), published as a provider with a REAL scope
  (`PublishedSurface.toProvider` at `AssemblyAnalysis.fs:271`, `scopeOf` at
  `PublishedSurface.fs:276-379`); the `.fs` half fully analysed for inline bodies and
  intrinsic pairing. Both backends take this path in production
  (`ClrSymbolProviders.compilationContract`, `JsNativeSymbols.jsNativeContract`).
- The built target artifact is **never read for symbols** — the `.dll` is an emit-time
  `AssemblyRef` and a runtime load only (`PackageHarness.fs:50-62`).
- The scope-less sources (`Scope = ScopeContents.empty`) are layer 2 only: the BCL metadata
  reader (`MetadataSymbols.fs:666`), which models types and their members alone, and the JS
  stub tables (`JsNativeSymbols.fs:89-97`), which answer for types alone. No Vesper symbol
  resolves through them. The TS manifest channel was the exception,
  and is the one production feeder R2 had to fix: its own `Js.spin` resolved by whole name.
  It now publishes a `PublishedSurface` and takes `toProvider` wholesale, so its values,
  types, members and scope come off one table; `IndexSignatures` decorates that provider with
  the one channel a surface has no room for.

So the original step 5 is **not** gated on a distribution-format change. The distribution boundary —
consuming a package without its sources — is [publishing-format-plan](publishing-format-plan.md)
PF8/PF9's scope: the printed `.fsi` rides beside the target artifact and enters this same
signature path, with no resolver change. The gates on the deletions are the specific feeders
in §3.

## 3. What still feeds each fallback

- **`folded.indexedCase`** — DELETED in R3b, against a dead path: both feeders went in R3
  and all three suites stayed green without it. `tryExternalCase`'s `qualifier` parameter and
  `ExternalUnionCase.ResolvesWith` went with it, a qualified case now resolving only through
  its declaring type.
- **`inType`'s existence check for a referenced type's static member** — added in R3,
  extended in R3b to referenced enums and capability interfaces and narrowed to STATIC
  members; R3c put the LOCAL enum arm back on the checked side with the other local arms.
  `staticMember ()` unchecked now survives only where nothing is knowable: an abbreviation,
  an intrinsic repr and an unmodelled type, which publish no member table.
- Carried, out of scope here: `tryStaticQualifier` answers only for a non-generic class, so a
  generic class's static reached without type arguments still falls to the TyVar fallback;
  mid-file `open` order (`BindingRank` across both halves) stays pinned as `ptest`s;
  `module A.B.C` whole-file homing is [fsi-front-end-plan](fsi-front-end-plan.md)'s gap.

## 4. Steps

Each step leaves the tree green and is a separate review.

1. **R1 — An implementation-published surface carries the source spelling. LANDED.**
   `ModuleBindingInfo` carries the binding's source short name beside the compiled one;
   the declaring scope's source path is `FrozenFileResidue.ModuleSourcePaths`, one entry per
   declared module, frozen from `TypeRegistry.declaredModulePaths`. Both producers publish
   through `PublishedSurfaceBuilder.addValue`, so the two paths cannot diverge again. Pinned
   in `ScopeContentsTests` ("an implementation with no signature publishes the source
   spelling too"), which also pins a type resolving through the module's source path.
2. **R2 — `wholeNameValue` is deleted. LANDED.** The scope walk reaches every spelling it
   used to answer for; §1 states the rule and `ScopeContentsTests.expectBagSpellings` pins it
   over both producers. Four tests identified three residual feeders, each fixed at the
   feeder: the TS manifest publishes a scope, and the stubs in `ExternalSymbolStampTests`,
   `CoverageTests` and `InlineFreezeThawTests` publish through `PublishedSurface` instead of
   a bare `TryLookup` channel.
3. **R3 — Honest stubs, honest pins. LANDED (2026-08-25).** Four stub providers
   (`ExternalUnionCaseStampTests`, `ExternalTypeKeyStampTests`, `OpenResolutionTests`,
   `ExternalUnionRecordQualifierStampTests`) publish through `PublishedSurface` and the
   `providerOfSurface` / `publishUnion` / `publishClass` helpers, so types, cases, members
   and scope come off one table. The bare-case namespace gate became
   `declaringUnionInScope`, applied to a qualified reference too: `Color.Red` and
   `Shade.Green` with their namespaces unopened are FS0039 in `dotnet fsi`, and are now
   unresolved here. `inType` checks an external class's static member exists, which turned
   the CLR suite's `System.Math.PI` pin red — a finding, since the fall-through elaborated
   to `External("System.Math.PI", ValueNone, TyVar 0)`, a leaked free type variable with no
   diagnostic. That pin is now a `ptest` asserting the name resolves and `p` types as
   `float`, which goes green when `MetadataSymbols.fieldMemberOf` stops dropping
   `[<Literal>]` fields. It asserts the TYPE rather than the diagnostic, so a return of the
   silent fall-through leaves it red.
4. **R3b — R3's review findings. LANDED (2026-08-25).**
   - `PublishedSurfaceBuilder.addTypeWith` is the one entry point for publishing a type: the
     name, shape, member table and the case or field index the shape implies, so a producer
     filling one table and not another is no longer expressible. All four producers
     (`FrozenSignature`, `SignatureResolution`, `TsManifestProvider`, `TestHelpers`) go
     through it. `ExternalTypeShape.Record` gained `requiresQualifiedAccess`, which had
     lived only on `ExternalRecordCandidate` while `Union` carried its own.
   - `declaresExternalStatic` filters on `IsStatic`, as its local twin `declaresStatic`
     always did. `T.InstanceMember` is FS3214 in `dotnet fsi`; against a referenced type it
     had been resolving, and R3 widened that to every class.
   - `inType`'s EXTERNAL `Enum` arm falls through to the member check rather than missing
     outright: `System.DayOfWeek.Equals(1, 1)` compiles in `dotnet fsi`, and the metadata
     member table carries the inherited static. `IntrinsicInterface` joins the checked arms,
     its members being published by `publishExtern`.
   - `MetadataSymbols` publishes `[<Literal>]` fields with their `TConstValue`, and
     elaboration substitutes the constant. This closes R3's `System.Math.PI` pin and the
     wider hole it stood for: EVERY BCL `const` (`Int32.MaxValue`, `Math.E`,
     `Char.MaxValue`) was FS0039 under R3's check.
   - `TryLookupUnionCase` became `TryLookupUnionCases`, a multimap like its
     `TryRecordsWithField` sibling. First-wins could not survive a scope filter applied to
     its single answer: two unions declaring `Green` where the key-first one is out of scope
     dropped the in-scope one.
5. **R3c — R3b's review findings. LANDED (2026-08-25).**
   - `tryExternalCase` picked the FIRST case surviving the scope filter, in provider order
     then key order, which decided a two-union collision by an order no use site can see.
     It is `externalCasesInScope`, returning every claim, and `caseInEnv` runs one 0/1/many
     match over both halves: `AmbiguousCase` now covers two REFERENCED unions as it already
     covered two of this file's, so `ResolvedItem.AmbiguousCase` carries `ResolvedUnionCase`.
     Pinned in `ExternalUnionCaseStampTests` over both positions. NOTE this is not F#, which
     shadows to the last `open` in both halves alike (`probe2.fsx`); reconciling the compiler
     to shadowing is one change across both halves, not an external-only one.
   - The cons-list case-index exception moved from `SignatureResolution` into
     `addTypeWith`, and `addTypeUnindexed` is deleted. `FrozenSignature.toSurface` publishes
     the same union with no exception, so `Cons` / `Empty` were out of the bare-name table
     for the compilation that declares the cons-list and back in it for every dependent
     reading it frozen. Pinned in `PublishedSurfaceTests`.
   - `MemberStorage.Literal` is deleted; `ExternalMember.ConstValue` alone carries the
     `[<Literal>]`. The case could never reach `TExpr.ExternalMember`, elaboration having
     substituted the constant, so it cost two `failwithf` guards and a `FrozenCodec` tag on
     both sides that could not be written or read, plus a `Storage`/`ConstValue` pairing
     maintained by hand in `fieldMemberOf` and overridden in `indexerAccess`.
   - `inType`'s LOCAL `Enum` arm misses rather than admitting every name as an unchecked
     static. `MyEnum.Nope` is FS0039, and R3b's justification does not hold: Unification's
     enum arm reports `NoCase` for `MyEnum.Equals` too, so it resolves nothing here. A local
     enum's inherited `System.Enum` statics are the same gap `MyClass.Equals` falls into.
6. **R4 — Delete the remaining legacy channels. LANDED (2026-08-25).** All five named
   channels are gone; a written name now reaches a referenced case or type through the
   container that declares it.
   - `IExternalSymbolResolver.TryLookupUnionCases` is deleted, and
     `IScopeContents.TryUnionCase` is `UnionCasesNamed`, a multimap like its `TypesNamed`
     sibling. `externalCasesInScope` walks the root namespace plus every active `open`
     prefix, so the `[<RequireQualifiedAccess>]` filter and the 0/1/many `AmbiguousCase`
     match are the only checks left. R3c's 0/1/many now covers a QUALIFIED `M.Red` too,
     where the single-answer channel had picked by insertion order.
   - `localQualifiedCase`, `resolveCtorName` and `resolveQualifiedCtor` are deleted:
     Unification and Elaborate read `ResolvedStamps.tryLocalUnionCase` at the node key.
     That removes three re-derivations of a resolution NameResolution had already made, the
     `not (isAmbiguousCase …)` guards that existed only to suppress the duplicate
     `AmbiguousConstructor` report, and `inferLocalCasePattern`'s `voption` parameter with
     its dead arm. `casesNamed` survives as `caseInEnv`'s scoped bare-case read, its one
     remaining caller.
   - `SymbolKeyOps.tryDottedInModule` is deleted and `PublishedSurface`'s `ResolveTypeName`
     answers for the compiled rendering alone. `tryPickExternalWritten` falls through to
     `IScopeContents.TypesNamed` over the containers the written path denotes, which is
     what `resolveType`, `typeInEnv`, `typeFirst`, `folded` and `resolveInheritParent` all
     take. `SymbolKeyOps.typeSourceName` went with the check it fed.
   - The `probes: string -> (string * int) list` parameter is an `int list`: both probe
     builders were `arityName candidate` over an arity set, so `arityProbes` is `[ arity ]`
     and `qualifierProbes` is `qualifierArities`. The per-arity fallback probe is deleted —
     a generic type is keyed `` Name`arity `` in the name index and by its key's own arity
     on the scope, so the bare probe answered for nothing.
   - Two findings, both fixed here: `mapProviderTypes` decorated the by-name channels and
     left `Scope` untouched, so `Vesper.Set.empty` reached JS with an undecorated scheme
     while a bare `empty` reached it decorated; and `tryExternalEnumCaseKey` had lost its
     last caller to the `ResolvedItem.EnumCase` stamp.
   - The sixth channel R5's note anticipated was left standing here; R6 deletes it.
7. **R6 — the by-name value channel is deleted. LANDED (2026-08-25).**
   `IExternalSymbolResolver.TryLookup` and `toProvider`'s by-name value index are gone, so a
   value is reached only through its declaring container or its `BindingKey`, and a value name
   is rendered nowhere in the provider stack.
   - `ScopeContents.tryValueAt` reads a written name off an `IScopeContents`: its leading
     segments are the container, its last segment the short name. `OpenScope.tryResolve` over
     it is `NameResolutionLongIdent.externalValueInScope`, which `valueInEnv`,
     `stampExternalSymbol` and `tryStampExternalValue` share; `PassContext.CoreAccess` reads
     it against the ambient prefixes.
   - `KeyIndexedChannels.SymbolsByKey: BindingKey -> ExternalSymbol` is what
     `TryLookupByKey` answers from. `EmitJsContext.externalValueRef` and
     `JsFlatFns.externalGroups` had been rendering a `SymbolKey` to reach the name index and
     now take the key channel; `ConformanceTypars.checkFile` reads the binding's own key,
     which retires `lookupNames`' three-probe fall-through.
   - `ScopeContents.decorate` is the one place a scope's answers are rewritten.
     `stack` applies the home stamp there, which `TryLookup` alone had carried, so a value
     read off a composed scope now carries the `SymbolHome` JS import emission needs;
     `withInlineBodies` and `mapProviderTypes` decorate through the same function, and
     `memoize` caches the four scope queries as it caches the key channels.
8. **R5 — `Symbols` is keyed by identity. LANDED (2026-08-25), ahead of R3/R4.** Brought
   forward because R2 shipped a defect: `scopeOf` recovered a source short name by splitting
   the rendered key at its last `.`, so a binding whose own name holds a dot
   (`` let ``a.size`` ``, which F# accepts) claimed the slot of a sibling named `size`.
   `Symbols: BindingKey -> ExternalSymbol` beside `SourceSpellings: SourceSpelling ->
   BindingKey` deleted the split, the cloned `{ sym with Name = written }` record,
   `ExternalSymbol.Name` (a cache of `Key`'s rendering, and the field the clone corrupted),
   and `checkValues`' `seen: HashSet<BindingKey>`. A value name was left rendered in exactly
   one place, `toProvider`'s by-name channel, which R6 deleted.
   Pinned in `ScopeContentsTests.expectBagSpellings` over both producers.
9. **R7 — one container walk, one hit constructor. LANDED (2026-08-25).** R6's review found
   the resolver split across three scope rules and the type route split across two hit
   producers. Both are one rule and one producer here.
   - `containersAtPath ctx useSite path` is the single container walk, and
     `NameResolutionLongIdent.externalValueInScope`, `externalCasesInScope` and
     `tryPickExternalWritten` are three reads off it. The `OpenScope.tryResolve` value route
     is deleted, so a written value name reaches a module abbreviation and the use site's
     enclosing chain exactly as a written type name does. `externalValueInScope` takes the
     qualifier and the short name APART: `OperatorNames.qualifiedOpName` had joined
     `li.Idents` for `A.B.(+)` and `ScopeContents.tryValueAt` split the result at its last
     `.`, the join/split pair R5 deleted one layer down. `qualifiedOpName` survives as the
     RENDERING two elaboration sites want; `Scope.fs` reads `ofIdentOp` and the path.
     `tryValueAt` survives for a LITERAL spelling, which is what tests assert against.
   - `openedContainers` deduplicates, which `firstSegmentContainers` always did.
     `OpenScope.Prefixes` is a cons list, so `namespace Vesper` under an ambient `Vesper`
     prelude prefix listed the container twice, and `externalCasesInScope` paid for it with
     an `ExternalUnionCase`-by-value scan. `ScopeContents.composite` deduplicates
     `UnionCasesNamed` across sources, which puts the bare and the qualified case paths on
     the same answer: a duplicate had reached `caseWhere`'s 0/1/many unfiltered and reported
     `M.Red` ambiguous while a bare `Red` resolved.
   - `caseAmong` takes the `[<RequireQualifiedAccess>]` reading as a parameter and is the one
     0/1/many rule. `inContainer.caseWhere` had a second copy of it, and `caseAmong` had
     restated the flag as a literal `false`.
   - `ExternalTypeHit` is deleted; both routes yield the `struct (TypeKey *
     ExternalTypeShape)` pair the channels already carry. `UseSiteKey` was the registered key
     from one producer and `useSiteKeyOf`'s cut of the probe string from the other, and
     `ProbedTyparArity` was a free variable on one and definitionally equal to the shape's own
     arity on the other, so the `Shape.TyparArity = ProbedTyparArity` check callers wrote read
     two ways. `useSiteKeyOf` is deleted: a nominal shape always took the registered key, and
     for an abbrev, an intrinsic and an unmodelled shape the probe's cut equals the registered
     key wherever the name route answers — a name-keyed source mints its key from the same
     name (`ExternalSymbols.nameKeyedTypeHit`), and a key-indexed source answers that route
     only under its compiled rendering.
   - `WrittenArity` replaces the probe list at the call sites, so the arity QUESTION is typed
     rather than encoded as `[ n ]` versus `[ 0..4 ]`. `Exact n` admits one arity on both
     routes; `Any` admits every arity a container publishes and guesses
     `0 .. MaxProbedQualifierArity` on the name route. A referenced generic wider than the cap
     now resolves. R8 (§5) enumerates the name route's sources, which retires the guess.
   - The scope route runs FIRST, so a published surface answers with its registered identity
     rather than a cut of the spelling. `stack` folds the intrinsic surface onto the scope's
     types, which `TryLookupType` alone had carried, and decorates unconditionally rather than
     only under a home stamp.
   - `IExternalSymbolStore.TryLookupByKey` and `ICodegenSymbols.TryLookupOpenSignature` take a
     `BindingKey`. Three implementations had a `Member`/`Type` arm returning `ValueNone`,
     `tryInlineBody` matched `SymbolKey.Binding` before calling one, and
     `ClrRecipes.emitExternalCall` held a `BindingKey` and wrapped it to call the other.
     `ModuleBindingInfo.BindingKey` is the identity; `Key` wraps it.
   - `KeyedChannels` carries every channel `ofKeyedChannels` forwards; `NamedChannels` is the
     name-keyed source's own contract, and loses `Scope`. `ofKeyIndexes` had filled four
     fields of a nested `NamedChannels` and left `TryLookupIndexSignature` and `Platform` at a
     default `ofKeyedChannels` still read, so a key-indexed source could not answer either by
     construction rather than by declaration.
   - `ScopeContents.memoize` sits beside `decorate`, and `mapProviderTypes` maps a symbol
     through one named function on both the scope and the key channel.
   - Pinned in `ScopeContentsTests` ("composition"), over the two deduplications.
     `ReferencedProjectTests` pins the arity question: `Vesper.Fun` is declared at 2, 3, 4 and
     5 in one namespace, and it went red while the scope route admitted any published arity.
10. **R7.1 — the `IScopeContents` ordering and identity contracts. LANDED (2026-08-25).** R7's
    review found the arity ordering asserted at one consumer and settled nowhere.
    - **`TypesNamed` answers ASCENDING by arity, and `UnionCasesNamed` one entry per declaring
      union.** Both are stated on `IScopeContents`, so the implementations R8 steps 1 and 2 add
      are written against them rather than retrofitted. `PublishedSurface.scopeOf` sorts each
      arity slot once at build. `ShapesByKey` is ordered by ORDINAL metadata name, under which
      `` P`10 `` precedes `` P`2 ``, so the two orders had agreed below arity 10 by coincidence;
      `ScopeContentsTests` "a name declared at several arities answers narrowest first" declares
      `P` at 0, 2 and 10 and goes red on `[0; 10; 2]` without the sort.
    - `tryPickExternalWritten` reads the channel in its own order and drops the per-query
      `toArray`/`filter`/`sortBy`/`ofArray` chain. `typesIn` and the written-type route had
      disagreed about which arity a container's claim resolves to.
    - `ScopeContents.tryValueIn` is the one "first container declaring `name`" rule;
      `PassContext.CoreAccess` had a second copy in `option`.
    - `CaseClaim` carries a case beside its `[<RequireQualifiedAccess>]`, read once.
      `caseWhere` had filtered on the flag and `caseAmong` re-derived it for the survivor.
    - `OperatorNames.qualifiedOpParts` is the one rule for how a qualified operator name is
      spelled; `qualifiedOpName` joins it for the two elaboration sites that render, and
      `Scope.fs` takes the parts rather than re-deriving the join.
    - `ScopeContents.mapValues` for the value-only decoration `stampInlineBodies` wants.

**The by-name TYPE channel stays through R7, and R8 (§5) deletes it.** R7 shipped with the
claim that a metadata type has "no container to walk". That is wrong, and §5 records the
correction: `MetadataMapping.declTypeKey` already builds the full `InType` / `InNamespace`
chain. What the metadata layer lacks is a DIRECTORY, which R8 supplies.

Deferred beyond this doc (move to their homes when this doc is deleted): the mid-file `open`
order follow-up (`BindingRank` applied across both halves — its `ptest`s state the assertion);
the generic-class static TyVar fallback; contract-build cost — `buildProviderSeeded`
re-analyses the full closure (nine packages for `Vesper.Set`) on every contract build, which
is publishing-format-plan PF5's content-hash cache when it shows in a profile.

## 5. R8 — the by-name TYPE channel is deleted. LANDED (2026-08-26).

### The premise R7 got wrong

R7 shipped saying `IExternalSymbolResolver.TryLookupType` is "the contract for a source whose
types have no container to walk". A metadata type has a container:
`MetadataMapping.declTypeKey` (`MetadataSymbols.fs:219-233`) builds the chain by recursion
through `Type.DeclaringType` — `InType` when nested, `InNamespace(t.Namespace)` at the root —
and its comment says outright that it avoids cutting `FullName`. The resolver's own side
agrees: `ExternalSymbols.nameKeyedTypeHit` → `SymbolKeyOps.qualifiedTypeKeyOf` →
`typeKeyOf` (`SymbolKeyOps.fs:153-164`) splits the namespace at the last `.` and parses `+`
into the same `InType` chain, round-tripping through `typeMetaName`.

What `MetadataSymbols` lacks is a DIRECTORY: it is demand-driven, `asm.GetType(name)` per
name behind `resolveCache`, so it holds no set of types to file under a container.

The second half of the R7 objection was cost, and it does not survive measurement. Over this
machine's reference closure, `GetExportedTypes()` plus `GetGenericArguments().Length` per
generic — no members, no signatures, no `computeType`:

| | |
|---|---|
| assemblies | 193 |
| top-level exported types | 6339 |
| namespaces including prefixes | 206 |
| `(namespace, name)` slots | 4950 |
| elapsed | 38 ms |

38 ms behind a `lazy`, once per provider. Reproduce with a scratch `.fsx` that opens a
`MetadataLoadContext` over `AppContext.GetData "TRUSTED_PLATFORM_ASSEMBLIES"` and folds
`GetExportedTypes()`; it needs `#r` on the
`System.Reflection.MetadataLoadContext.dll` beside a built test project.

### What R8 does

Steps 1 and 2 are independent; 3 needs both; 5 needs 4. Each leaves the tree green.

1. **`MetadataSymbols` publishes a real `IScopeContents`.** A `lazy` directory over
   `GetExportedTypes()`, holding `namespace → name → arities` for TOP-LEVEL types, plus every
   namespace prefix as its own container. `TypesNamed` resolves each arity's shape through the
   existing `computeType` cache, so the shape stays demand-driven; the directory carries
   identity alone. `TryValue` and `UnionCasesNamed` miss: IL declares no free function and no
   F# union case. `GetExportedTypes()` selects exactly the types the existing `tryAsm`
   `t.IsVisible` filter admits, so visibility is unchanged. `GetForwardedTypes()` names join
   the directory (identity alone): `asm.GetType` follows a type forwarder while
   `GetExportedTypes()` omits it, so a facade-heavy reference set would otherwise lose names
   the demand-driven path resolves.
2. **`JsNativeSymbols` publishes one too**, off the `Map<string, ExternalTypeShape>` it
   already holds — a smaller version of the same directory.
3. **`tryPickExternalWritten` loses its second half**, becoming the container walk alone.
   `WrittenArity.probes` and `MaxProbedQualifierArity` go with it, and an arity is then
   whatever a container publishes rather than a guess. Two R7 review findings land here:
   - The `WrittenArity.admits` re-check wrapped around `pick` on the name route
     (`LongIdent.fs:257-266`) never fires. `tryPickExternalType` already gates on
     `shape.TyparArity = arity` for each arity in `probes`, so `Exact n` implies it and `Any`
     admits everything. It is deleted with the route it guards.
   - **The written spelling is then taken APART, as the value route already takes it.** The
     only reason `tryPickExternalWritten` accepts a joined `written: string` and re-splits it
     at the last dot is `OpenScope.tryResolve` on the name route. With that gone, callers pass
     the qualifier and the short name, `containersAtPath` stops dispatching on `path.Length`,
     and the `""`-for-bare sentinel at six call sites (`externalCasesInScope`, and `Scope.fs`
     at four operator/dynamic-lookup sites) goes with it. This is the same join/split pair R7
     deleted one layer down; R7 left it on the type route because the name route still needed
     the join.
4. **`tryPickRuntimeType` splits into its two queries.** Its doc says "Never a source-written
   name" (`ExternalSymbols.fs:413-415`) and one of its two callers contradicts that:
   `IntrinsicResolve.tryResolveIntrinsicKey` (`Intrinsics.fs:12-23`) falls through to it, and
   `NameResolutionInheritParent.resolveInheritArgName` (`InheritParent.fs:76`) reaches that
   fall-through with `ctx.NameOf li.Idents.[0]`, a bare name as WRITTEN. The other caller,
   `JsExternalMembers.exnReprOf` (`JsExternalMembers.fs:135`), passes a platform repr. So the
   `AmbientOpenPrefixes` scan is load-bearing for the first caller and dead weight for the
   second, and one name covers two queries. Split them: an exact repr/canon lookup on
   `IExternalSymbolStore`, and a written bare name taking the container walk with every other
   written name. The walk reads the use site's own `open`s where the scan read the ambient
   prefixes alone, a confirmed widening.
   `resolveInheritArgName` also checks intrinsics BEFORE the local registry, so a provider
   answer shadows a local declaration in inherit-arg position. An intrinsic is an ordinary
   type everywhere outside the hardcoded literal type names (user, 2026-08-26): the registry
   arms run first, and the intrinsic read joins the container walk with every other written
   name.
5. **`IExternalSymbolResolver.TryLookupType` is deleted**, step 4 having given both halves a
   home. `NamedChannels`, `KeyedChannels.ofNamed` and `ExternalTypeProbe` go with it. Two more
   R7 review findings land here:
   - `stack` spells `stampType (foldIntrinsicSurface key shape)` three times
     (`ExternalSymbolProviders.fs:356, 367-368, 383`). Deleting the resolver overload removes
     one; bind the remaining composition once.
   - **`KeyedChannels` goes too.** R7 flattened `NamedChannels` into it, leaving an 11-field
     record with 9 function-typed fields — the record-of-closures shape the root `CLAUDE.md`
     calls a smell. Once `ofNamed` is deleted it wraps a single source and buys nothing that
     `ProviderDecorator(nullProvider)` — 30 lines up the same file, `abstract`/`default`
     members rather than closure fields — does not already buy. `PublishedSurface.toProvider`
     and the ~10 test stubs become `{ new ProviderDecorator(nullProvider) with … }`, and
     `nullProvider` is minted directly rather than through `ofNamedChannels`.
     Weigh honestly before committing: `{ Channels.empty with X = … }` reads better than an
     object expression at a test stub, and `ofNamed`'s four adaptations (`typeMetaName`,
     `ExternalMemberName.ofKeyed`, `nameKeyedTypeHit`, arity rendering) move into the sources
     that need them. If the stub ergonomics win, say so in this doc and keep the record.

### Scope and risk

- A nested type stays outside the directory: `ModuleContainer` spells `InNamespace` and
  `InModule` only. Nothing is lost, because a written `Outer.Inner` misses the name index
  today as well — reflection spells nesting `+`, so `asm.GetType("Ns.Outer.Inner")` finds
  nothing, and `typeFirst` is what reaches such a name. `inType`'s arms are union case, enum
  case and static member, so a nested type is unreachable in BOTH designs. R8 neither fixes
  nor worsens that; it is its own gap, and `MetadataSymbols` publishing `MembersByKey` for a
  nested type is where a fix would start.
- `MetadataLoadContext` is not thread-safe. The enumeration takes the existing `gate`, and
  the `lazy` must not hold it across `computeType`.
- `ScopeContents.composite` picks the FIRST source answering `TryContainer`, so a directory
  answering for a path it did not find would take a real module's slot. A directory built
  from what the assemblies declare answers for 206 namespaces rather than for any dotted
  path, which is what makes step 1 safe where R6's proposed `ofNameKeyedTypes` adapter was
  not.
- Deleting `TryLookupType` moves BCL type resolution onto the scope wholesale. Expect the CLR
  suite to be the one that finds the gaps; a red test there is a finding about the directory's
  contents, not a reason to restore the fallback.

### Confirmed (user, 2026-08-26)

- **The arity cap and probe list go.** `TypeKey` already carries the plain `Name` plus
  `TyparArity: int`; the backtick suffix is a rendering (`typeMetaName`), so the cap was an
  artifact of the `string ->` channel alone. Measured: no BCL `(namespace, name)` slot holds
  only arities above the cap, so step 3 is correctness by construction, not a live-defect fix.
- **The directory is eagerly built.** 38 ms behind a `lazy` is accepted; no per-assembly
  second index.
- **Step 4's split is the contract.** The written half (an intrinsic reached as `inherit exn`)
  takes the container walk under the use site's opens; the platform repr
  (`"System.Exception"`) takes an exact `IExternalSymbolStore` lookup and reads no opens. The
  canon living under `Vesper.exn` while the repr names a BCL type is why the two are separate
  queries. Intrinsics receive no special resolution outside the hardcoded literal type names.

## 6. Settled semantics (user, 2026-08-23)

- Local-first between the registry and the provider stack; mid-file `open` order is the
  pinned follow-up.
- Pattern position is type-first, expression position module-first — match FCS, do not unify
  the two orders.
- An RQA case reached through its module (`M.Red`) resolves and is then reported (FS0035), as
  `dotnet fsi` does; it is not an unresolved name.
- `module A.B.C` as a whole file still homes in the global namespace; `FirstSegment` inherits
  that until fsi-front-end-plan fixes it, which is orthogonal.
