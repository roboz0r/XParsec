# Long-identifier resolution — the remaining deletions

**Status (2026-08-24): the resolver is LANDED; this doc scopes what is left of the original
step 5 ("delete the speculation"), re-planned after a wiring survey corrected one of its
premises.** The original design (FCS-shaped algorithm, forks A–C, steps 1–4) was confirmed by
the user on 2026-08-23 and is now code; this rewrite drops the landed history and keeps only
the settled semantics, the corrected premises, and the work left.

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
- A value's written name is rendered in one place, `toProvider`'s by-name channel; every
  other consumer reaches a value by `BindingKey`.
- Expression position resolves `modulePath → typeFirst → folded`; pattern position
  `typeFirst → modulePath → folded`. Both read the order off `qualifiedReadings`.
- Every published scope is TOTAL: a source answering for a container answers for its values
  and its types alike, the TS manifest included, which publishes a `PublishedSurface` like
  any other referenced package (`TsManifestProvider.publicationOf`).
- Suites at the last landed step: SemanticAnalysis 1480, Clr 1550, Js 667.

## 2. Premises corrected by the wiring survey (2026-08-24)

The original step-3 follow-up assumed `Set.singleton` against the built `Vesper.Set` resolved
through a scope-less metadata channel, and gated step 5 on "a consolidated signature file
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
  reader (`MetadataSymbols.fs:647`), whose `TryLookup` is unconditionally `ValueNone`
  (`:648`), and the JS stub tables (`JsNativeSymbols.fs:89-97`), which answer for types
  alone. No Vesper symbol resolves through them. The TS manifest channel was the exception,
  and is the one production feeder R2 had to fix: its own `Js.spin` resolved by whole name.
  It now publishes a `PublishedSurface` and takes `toProvider` wholesale, so its values,
  types, members and scope come off one table; `IndexSignatures` decorates that provider with
  the one channel a surface has no room for.

So step 5 is **not** gated on a distribution-format change. The distribution boundary —
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
   - **Still open**, the sixth channel R5's note anticipated: `IExternalSymbolResolver.TryLookup`
     and `toProvider`'s by-name value index. Its four production readers (`valueInEnv`,
     `stampExternalSymbol` / `tryStampExternalValue`, `CoreAccess`, `ConformanceTypars`) each
     need their own container or key, and `TryLookupByKey` renders a `BindingKey` to reach it.
7. **R5 — `Symbols` is keyed by identity. LANDED (2026-08-25), ahead of R3/R4.** Brought
   forward because R2 shipped a defect: `scopeOf` recovered a source short name by splitting
   the rendered key at its last `.`, so a binding whose own name holds a dot
   (`` let ``a.size`` ``, which F# accepts) claimed the slot of a sibling named `size`.
   `Symbols: BindingKey -> ExternalSymbol` beside `SourceSpellings: SourceSpelling ->
   BindingKey` deleted the split, the cloned `{ sym with Name = written }` record,
   `ExternalSymbol.Name` (a cache of `Key`'s rendering, and the field the clone corrupted),
   and `checkValues`' `seen: HashSet<BindingKey>`. A value name is now rendered in exactly one
   place, `toProvider`'s by-name channel, which R4 left standing (see R4's last bullet).
   Pinned in `ScopeContentsTests.expectBagSpellings` over both producers.

Deferred beyond this doc (move to their homes when this doc is deleted): the mid-file `open`
order follow-up (`BindingRank` applied across both halves — its `ptest`s state the assertion);
the generic-class static TyVar fallback; contract-build cost — `buildProviderSeeded`
re-analyses the full closure (nine packages for `Vesper.Set`) on every contract build, which
is publishing-format-plan PF5's content-hash cache when it shows in a profile.

## 5. Settled semantics (user, 2026-08-23)

- Local-first between the registry and the provider stack; mid-file `open` order is the
  pinned follow-up.
- Pattern position is type-first, expression position module-first — match FCS, do not unify
  the two orders.
- An RQA case reached through its module (`M.Red`) resolves and is then reported (FS0035), as
  `dotnet fsi` does; it is not an unresolved name.
- `module A.B.C` as a whole file still homes in the global namespace; `FirstSegment` inherits
  that until fsi-front-end-plan fixes it, which is orthogonal.
