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
- Suites at the last landed step: SemanticAnalysis 1472, Clr 1549, Js 667.

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

- **`folded.indexedCase`** — the bare-case-name reverse index behind the legacy
  `TryLookupUnionCase` channel. Feeders: the stub provider in `ExternalUnionCaseStampTests`
  publishes cases but no types, and `OpenResolutionTests` pins `Color.Red` resolving with
  `Tests` NOT opened, which `dotnet fsi` rejects.
- **`inType` skips the existence check for an external class's static member**, because the
  stub providers in `ExternalTypeKeyStampTests` publish no members.
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
3. **R3 — Honest stubs, honest pins.** Rebuild the stub providers in
   `ExternalUnionCaseStampTests` / `ExternalTypeKeyStampTests` through the real surface
   builder (`FrozenSignature` / `PublishedSurface`) so they publish types, cases and members
   with real scope contents, per `test/CLAUDE.md` ("wire real contracts"); flip
   `OpenResolutionTests` to F#'s semantics (`Color.Red` with `Tests` not opened is
   unresolved); add the static-member existence check in `inType` now that stubs publish
   members.
4. **R4 — Delete the reverse index and the legacy channels.** `folded.indexedCase`, the
   provider-level `TryLookupUnionCase`, `ResolvesWith`, `localQualifiedCase`,
   `casesNamed`-as-global-reverse-lookup, `tryDottedInModule`, and `arityProbes`' per-prefix
   loop. Score the step by the runtime checks removed, per the repo rule.
5. **R5 — `Symbols` is keyed by identity. LANDED (2026-08-25), ahead of R3/R4.** Brought
   forward because R2 shipped a defect: `scopeOf` recovered a source short name by splitting
   the rendered key at its last `.`, so a binding whose own name holds a dot
   (`` let ``a.size`` ``, which F# accepts) claimed the slot of a sibling named `size`.
   `Symbols: BindingKey -> ExternalSymbol` beside `SourceSpellings: SourceSpelling ->
   BindingKey` deleted the split, the cloned `{ sym with Name = written }` record,
   `ExternalSymbol.Name` (a cache of `Key`'s rendering, and the field the clone corrupted),
   and `checkValues`' `seen: HashSet<BindingKey>`. A value name is now rendered in exactly one
   place, `toProvider`'s by-name channel, which R4 deletes. Pinned in
   `ScopeContentsTests.expectBagSpellings` over both producers.

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
