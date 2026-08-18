# One fold over one file list

**Goal:** the expected dataflow of [fsi-front-end-plan](fsi-front-end-plan.md) §"Expected
dataflow" encoded by ONE function every interested caller uses. Delete this doc when step 4
lands (`feedback_plan_docs_ephemeral`).

**Status (2026-08-17): step 1 LANDED**, including its ride-alongs; steps 2–4 remain. Absorbs
the remnants of `package-parse-once-plan.md`, which is deleted — its account of the remaining
seam was misdiagnosed, see §"What this closes".

## Where it stands: two folds implement one dataflow

`PackageProviders.buildProviderWith` and `AssemblyFiles.analyseParsedWith` are the same fold.
Both resolve each `.fsi` through `SignatureResolution.resolveFile` against a nearest-first
stack of what the files before it published, and push the result. The package one stops after
`resolveFile`; the compiling one goes on to analyse, freeze and conform the `.fs`.

Everything they differ in is one difference wearing four costumes:

| | compiling | referencing |
| --- | --- | --- |
| home | `Origin.InFile implPath` | `Origin.InAssembly name` |
| ambient prefixes | none | the package's `[<AutoOpen>]` |
| analyse the `.fs` | yes | no |
| a `.fs` without a `.fsi` publishes | the surface it infers | nothing |

The first two rows are already stated as one parameter in
[fsi-front-end-plan](fsi-front-end-plan.md) §"Constraints the merged path must keep". The
last two rows describe the code but are NOT design: rows three and four are the gap
§"A `.fs` without a `.fsi` must publish across the boundary" closes — the referencing route
already analyses every `.fs` (for inline bodies) and F# parity requires such a file's inferred
surface to cross.

Compile order permits the merge outright — `AssemblyFiles.fs` already sees `PackageSource`,
`SignatureResolution`, `Freeze` and `Pipeline`, and `PackageProviders` is after it.

## The blocker: one fold needs one order, and the manifest has two — RESOLVED by step 1

`[core] files` and `[core] impl` are independently ordered. Two folds can each be internally
consistent over two orders; one fold must pick.

Checked across all 20 package manifests, the two orders agree everywhere **except**
`Vesper.Core/manifest.clr.toml`, where `fun-adapters` is last in `files` (20th) and 16th in
`impl`, before `structural-format`, `ops-platform`, `ops-std` and `int-comparison`.

**That one conflict is free, and this is what the old plan could not say.** `fun-adapters.fsi`
declares `Curried` / `Flattened` and an `[<AutoOpen>] module FunAdapters` over `Fun` alone, and
nothing after it in either list names any of them — `structural-format.fsi`'s only hit on
`flatten` is the English word in a doc comment. So its position does not matter, and the
manifest's inability to say so is the defect rather than either ordering being wrong.

Every other manifest has at most four entries per list with the shared keys in agreeing
relative order, so the interleave is determined.

## Merge rule

**`impl`-driven ordering is canonical (user, 2026-08-16).** The merged list is `impl` order,
with each `.fsi` immediately before its companion `.fs`.

The retire-sig-only work has landed (2026-08-17): every corpus `.fsi` HAS a companion, so that
rule is total and the merged list is fully determined by `impl` order.

- Every `.fs`-to-`.fs` relative order is preserved, so `SymbolProviders.inlineBodies`'s
  later-body-wins is untouched. That is the one place the merged order is load-bearing beyond
  readability, and it wants a test BEFORE the change, not after.
- The package compile order is preserved exactly: it already reads `impl` order alone.
- `fun-adapters.fsi` is the only entry that moves, earlier, in contract extraction only.

## Step 1: collapse `files` + `impl` into one ordered list — DONE (2026-08-17)

**Landed as planned, with two deviations the merge itself forced:**

- **`PackageSetFault.FileWrongHalf` and `PackageSource.FileFault.WrongHalf` are DELETED, not
  typed.** The plan wanted the fault to carry `expected: Half`; with one extension-classified
  list there is no independent "expected" left — the half IS the extension — so the fault had
  no producer and the `readPackage` wrong-half check deleted outright. The manifest-level
  parse error (unknown extension) is the surviving refusal.
- **The role type is `SourceFileKind` (user, 2026-08-17), not `PackageSource.Half`**: hoisted
  into its own `SourceFileKind.fs` above `ReferencedProject.fs`, renamed (it also collided with
  `System.Half`), with `tryOfPath : string -> SourceFileKind voption`; the total `ofPath` and
  `describe` are gone with their only callers.

The ride-alongs all landed: `loadManifest`/`buildClosure`/`parseManifest` return
`PackageSetFault` (new case `MalformedManifest of path * detail`, V251), `PackageUnits.ofManifest`
/ `ConformancePass.checkManifest` / `PackageProviders.buildProvider` carry it through,
`Hashing.compilationDigest` keys both failure arms on the fault's CODE,
`AssemblyFiles.setFaultDiagnostics` is the one whole-set-fault primitive, and the duplicate-type
diagnostic anchors at `AssemblyFileId.nowhere`. Wire change (FileWrongHalf out, MalformedManifest
in, tags renumbered) bumped `Cache.CodeVersion` 32→33. The later-body-wins order pin landed
first as `JsPackageTests.inlineBodyOrderTests`.

**Review hardening (2026-08-18):** `classifyFiles` also rejects a duplicated entry and
enforces `.fsi`-immediately-before-companion-`.fs` (see the step 2 note below), which let
`sourceInputs` drop its `List.distinct`; `pairingKey` takes the target string, so the check
runs at parse time; `parseManifest` flattened over `coreTable` / `packageName`; the
`(PackageSetFault.describe e)` failure boilerplate across the test projects collapsed into
`Codegen.Common.Tests.PackageFaults.okOrFail`.

Parse into a TYPED ordered list. A bare `string list` whose five consumers each re-derive the
role by sniffing the extension trades two honest lists for one over-wide one
(`feedback_overwide_types_are_string_keys`).

The role type already exists: `PackageSource.Half` (`Signature` / `Implementation`, with
`Half.ofPath`). Reuse it rather than coining `ManifestRole`
(`feedback_reuse_established_verb`) — which means moving `Half` up in compile order, above
`ReferencedProject.fs`. **That move is owed anyway**: `PackageSetFault.FileWrongHalf` carries
`expected: string` today only because `Half` is declared after `Diagnostics.fs`, so hoisting it
above both lets the fault carry `expected: Half` and the codec write a byte. One move, two
string keys deleted.

```fsharp
type ManifestFile = { Relative: string; Half: Half }
```

- `Manifest.Files : ManifestFile list` — the single ordered list.
- An entry whose extension is neither `.fs` nor `.fsi` is a parse ERROR, as an unknown
  `[core]` key already is: read as silence it resolves a stale manifest to a plausible wrong
  file set.
- Drop `impl` in one commit rather than run a compatibility path. Twenty manifests, all in
  this repo, nobody else consuming the format — a migration flag day for that is a moving
  piece worth not having.
- `runtime` stays: an asset list deliberately excluded from `sourceInputs` so an asset edit
  does not read as a source edit. (`sig-only` is retired outright — deleted from the schema
  2026-08-17.)
- `sourceInputs` collapses to the file list; it is sorted before hashing, so nothing moves.

**Inventory.** 19 manifests under `src/` (9 packages × 2 targets, plus CLR-only `Vesper.Set`)
and `test/XParsec.FSharp.Codegen.Js.Tests/fixtures/widget/manifest.js.toml`. The synthetic
manifests in `ReferencedProjectTests` are written at test time, not tracked.
`test/Codegen.Conformance/manifest.toml` is NOT in this set — it is a `[[program]]` corpus with
no `[core]` table, and the old plan was wrong to list it.

**Touched:** `ReferencedProject.parseManifest` / `coreKeys` / `sourceInputs`; `PackageSource.readPackage`;
`ConformancePass` (`m.Impl`); `PackageProviders.buildProviderWith`; `SymbolProviders.inlineBodies`;
`Hashing` (manifest bytes are hashed, so every package hash moves once → one full rebuild).
`ReferencedProjectTests` pins `m.Files` / `m.Impl` directly (`:123`, `:701-704`, `:875`,
`:917-924`) and needs updating.

**Ride along with step 1**, because each touches the code this step is already rewriting and
each is otherwise homeless:

- **Type `loadManifest` and `buildClosure`'s error channels** as `PackageSetFault` (a
  `MalformedManifest of path * detail` case covers them). `resolveManifest` is already typed, so
  callers currently DOWNGRADE it — `Result.mapError PackageSetFault.describe |> Result.bind
  PackageUnits.ofManifest` appears three times in test helpers, paying to undo the improvement.
  It also makes `Hashing.compilationDigest`'s comment true: it claims the key never folds
  diagnostic WORDING, which the `buildClosure` arm violates today.
- **`PackageUnits.ofManifest`'s nested `Result`** goes with that: its outer error is the one
  channel that stayed stringly.
- **Collapse the three `PackageSetFault` → diagnostics helpers** —
  `PackageSource.FileFault.toFailure`'s inner `setFault`, `PackageProviders.setFault`, and
  `SymbolProviders.setFaultDiagnostics` are one primitive spelled three ways. Put
  `PackageSetFault -> AnchoredDiagnostic list` beside `unpositionedDiagnostics` and let the
  others be one-liners over it.
- **`PackageProviders`'s duplicate-type diagnostic** anchors to
  `AssemblyFileId.ofRelative manifest.Name`, fabricating a file named after the package.
  `AssemblyFileId.nowhere` is used fourteen lines below for the same kind of finding.

**Exit:** whole corpus green, both targets, with no behaviour change other than
`fun-adapters.fsi`'s position in contract extraction.

## Sequencing: retire-sig-only FIRST — SATISFIED (landed 2026-08-17)

The retire-sig-only work is fully landed: every corpus `.fsi` pairs (the runtime-served pair
became `[<Import>]` + `jsNative` bindings in a real `.fs`), the `sig-only` schema is deleted,
and an unpaired signature is an unconditional hard error. The pairing is TOTAL, which
collapses this plan's step 2 and simplifies its merge rule:

- `Unit` is not a three-case DU. Every unit of a WELL-FORMED package has an implementation
  and an optional signature. (An earlier draft said "the package route uses
  `AssemblyFiles.ParsedUnit`" — wrong on two counts, see step 2: `ParsedUnit` holds parsed
  TREES where the conformance pass needs the per-file read OUTCOMES, and the unpaired `.fsi`
  must stay representable for the referencing route.)
- `impl`-driven ordering is canonical AND COMPLETE, not canonical for a spine with
  companion-less entries positioned off `files`. The merged list is a mechanical interleave
  with no exceptions.

## Step 2: one unit type — REFINED (user, 2026-08-17)

`readPackage` already computes the pairing and then splits it into two lists whose element
types each make one half mandatory. Emit the pairing itself. The original three-case DU is
superseded: total pairing makes "paired" and "implementation-only" one shape, and that shape
is the record (user):

```fsharp
type ParsedSource =
    {
        Signature: ReadFile<ParseChain.ParsedSignature> voption
        Implementation: ReadFile<ParseChain.ParsedFile>
    }
```

The same axis as `AssemblyFiles.ParsedUnit` — mandatory implementation, optional signature —
but as READ: each half keeps its `Outcome`, because the conformance pass turns a per-file
fault into `PairOutcome.ParseFailed` and the provider build reports it from the list that
names the file.

**One state the record cannot carry, and must survive: the unpaired `.fsi`.** Conformance
refuses it (`SigWithoutImpl`, hard error), but conformance gates only the compiling and test
routes; the REFERENCING route (`composeContract` → `buildProviderWith`, `PackageProviders.fs:84`)
resolves and publishes every signature file regardless of companion, and the synthetic test
corpus leans on exactly that — `writeSyntheticPackageWithType`
(`ReferencedProjectTests.fs:83`) ships `contract.fsi` alone, as do the Hashing,
`StructTests` `point.fsi` and `FrozenCacheIncrementalTests` fixtures. Losing the state would
change the referencing route and break those fixtures, which is not this plan's scope. So the
element type carries it as the degenerate case beside the dominant record:

```fsharp
[<RequireQualifiedAccess>]
type PackageUnit =
    | Source of ParsedSource
    | UnpairedSignature of ReadFile<ParseChain.ParsedSignature>
```

- `ParsedPackage.Units : PackageUnit list` in merged-list order replaces
  `Signatures` / `Implementations` and the `Companion` fields on both, so signature
  resolution order (currently the signatures' own `files` order) is preserved even where an
  `UnpairedSignature` interleaves with `Source` units.
- `buildProviderWith` walks the one list: a `Source` signature resolves against reprs read
  off ITS OWN record (`companionReprs` takes the unit), an `UnpairedSignature` resolves and
  publishes exactly as today, and a faulted implementation reports from the unit that names it.
- `ConformancePass.check` walks the same list: `Source` with a signature → the pair check;
  `UnpairedSignature` → `PairOutcome.SigOnly`; `Source` without one → nothing (a `.fs` owes
  no `.fsi`).
- `PackageUnits.ofPackage` becomes the projection `ParsedSource →
  Result<AssemblyFiles.ParsedUnit, UnparsedFile>` over the `Source` cases. The compiling
  route stays blind to `UnpairedSignature`, which conformance separately refuses.
- Pairing stays KEY-based (`pairingKey`), but `.fsi`-immediately-before-`.fs` is
  **parse-enforced** (user, 2026-08-18), no longer a style convention: a `.fsi` whose sole
  key-mate sits elsewhere in the list, a `.fsi` two `.fs` entries key-match, or a duplicated
  entry is `MalformedManifest`. Key pairing and list adjacency therefore provably agree, and
  step 2 may read the pairing either way.

**Exit:** `ConformancePass.check`, `buildProviderWith` and `inlineBodies` each walk one list,
and no consumer re-derives a pairing.

## A `.fs` without a `.fsi` must publish across the boundary (user, 2026-08-17)

(Not "unsigned" — that reads as cryptographic artifact signing. "`.fs` without a `.fsi`" is
the term.)

The §"Where it stands" table's fourth row — a `.fs` without a `.fsi` publishes "nothing" when
referencing — described the code and was wrong as a design. F# parity: in
`a.fsi, a.fs, b.fs, c.fsi, c.fs`, a consumer of the built assembly sees types from all three
units, `b.fs`'s included. F#'s mechanism is that the consumer reads the PRODUCER COMPILE's
output (the pickled inferred signature); the boundary rule is "only signatures cross", and
for a `.fs` without a `.fsi` the signature is the one its compile inferred. So the
cross-assembly view of `b.fs` is derivable only from the compile path.

**The referencing route already runs that compile and discards its surface.**
`SymbolProviders.inlineBodies` (`SymbolProviders.fs:59-135`), on the referencing route via
`buildContractWith`, analyses EVERY impl file of every referenced package through
`Pipeline.analyseSemWithContextFor` and freezes it, to harvest inline templates — while
`buildProviderWith` walks only the `.fsi`s. `b.fs`'s inferred surface is computed and thrown
away: a stage discarding an intermediate. The fix is therefore not "make referencing analyse"
but "stop discarding": in the merged fold, ONE analysis per impl feeds the inline bodies AND,
for a unit with no `.fsi`, the published surface. It also gives that analysis the proper
nearest-first prior-file environment, where `inlineBodies` today analyses against the final
whole-set composite.

This lands with step 3 (it IS the fold's referencing arm done right), and PF5 / PF8 of
[publishing-format-plan](publishing-format-plan.md) are producer-side MATERIALISATIONS of the
same semantics — a cached resolved surface, or a generated-and-committed `.fsi` — not
alternatives to it.

**Enumerate before flipping — this is a behaviour change:**

- `Vesper.Printf` (js): `structural-printer.js.fs` has no `.fsi` by design; its
  `structuralFormat` / `float32ToString` start genuinely publishing, which is also what lets
  `EmitJsContext.printfRuntimeRef`'s hardcode die.
- The widget fixture violates F# pairing semantics: `widget.fsi` declares `gadget`, whose
  members live in `gadget.js.fs`, which has no `.fsi` of its own. Publishing gadget's inferred surface
  double-declares the type; the fixture needs restating first.
- A2 of [codegen-common-followups-plan](codegen-common-followups-plan.md) stops being
  cosmetic: a broken impl currently contributes no bodies SILENTLY; once surfaces flow from
  the same analysis, its errors must surface.

## Step 3: one fold

In `AssemblyFiles`, over `PackageUnit list`, with the discriminator that made the four rows one:

```fsharp
/// Which side of the assembly boundary this fold publishes for.
type Publication =
    /// Compiling these units: each file homes in itself and the frozen `.fs` is kept for
    /// codegen.
    | InAssembly
    /// Reading them as a reference: every symbol homes in the assembly and the package's
    /// `[<AutoOpen>]` prefixes are published.
    | AcrossAssemblies
```

Both arms analyse every implementation file (the referencing arm already does, for inline
bodies), and on both a `.fs` without a `.fsi` publishes the surface it infers; a unit with a
`.fsi` publishes its declarations. What remains of the axis is the home, the ambient prefixes, and
whether the frozen bodies are kept for codegen — the original `analyse: AnalyseFile` payload
dissolves into the fold itself.

Two things the fold makes one that are two spellings today: the intrinsic-repr pre-scan
(`signatureView` reads `scope.Implementation`, `companionReprs` reads `entry.Companion` — the
same `IntrinsicReprs.ofImplementationInto` call), and the prelude at the floor of the stack,
which the package route appends explicitly and the compiling route inherits from `external`.
The second is a real question the merge forces: an in-assembly `.fsi` compiled against an
EMPTY reference set gets no prelude today, and a package `.fsi` always does. Settle it in the
fold rather than leaving it to which caller you came through.

**Exit:** `buildProviderWith` and `analyseParsedWith` are both call sites; neither holds a
loop.

## Step 4: the callers collapse onto it

- `PackageProviders.buildProviderWith` → `AcrossAssemblies`.
- `AssemblyFiles.analyseParsedWith` / `analyseGatedParsed` → `InAssembly`. With
  `analyseGated` gone, the `Parsed` suffix distinguishes nothing and should go too.
- `TestHelpers.buildPackage` reads the package ONCE and folds it twice — once compiling, once
  as its own reference — off one `ParsedPackage`.

**Ride along with step 4**, since it is already rewriting these callers:

- **`ComposedContract` and `Contract`** are one idea at two layers, and only `Contract` has a
  `gate` — which is why `SymbolProviders.buildWith` returns a value nobody can gate and
  `ClrSymbolProviders.build` drops it. Fold `ComposedContract` into `Contract` with empty
  bodies/origins. NOTE: this does NOT fall out of the fold merge on its own — `composeOrdered`
  survives it and still returns its own type.
- **`ClrDriver.unanchored`** flattens `AnchoredDiagnostic list` to `Diagnostic list`, discarding
  path and line. Its comment says the file is in the message and there is nothing to anchor to;
  that is true of `PackageSet` faults and false of the positioned signature-resolution errors
  the gate also passes. Widen the single-file entry's error channel, or fold `path:line` into
  the rendered message.

**Exit:** the dataflow in [fsi-front-end-plan](fsi-front-end-plan.md) §"Expected dataflow" is
one function, and `a.fsi, a.fs, b.fs, c.fsi, c.fs` is a list you can write in a manifest.

## Independent of every step, and deliberately not scheduled

- **`ParseChain.parse` and `parseSignature` are one function written twice** — identical lex →
  reader → failure plumbing, differing only in which parser runs and which AST case is accepted.
  `ParsedFile` and `ParsedSignature` are the same record with a different tree field. One
  `Parsed<'Tree>` plus a `parseAs` collapses ~60 lines to ~30 and deletes a type, and the stack
  already parameterises over exactly that axis (`ReadFile<'Tree>`, `ParsedHalf<'tree>`). Safe to
  do at any point.

**Do NOT fix these separately — the steps above delete them:**

- `PackageSource.readPackage`'s two `Dictionary` fill loops (step 2 rewrites the function).
- `bindExternRepr`'s `fileOn true` / `fileOn false`, which reintroduces the boolean blind
  `publishExternPrimitive` removed (the sentinel-repr capability bodies pass through
  `SignatureResolution` anyway).

## What this closes

**The "reads a package set twice" seam, without a parse cache.** A compilation that both
REFERENCES and COMPILES a package reads it once per entry point today. `package-parse-once-plan`
diagnosed that as cache invalidation and pointed the next session at a parse cache keyed on the
manifest. That was wrong: the cause is that neither fold can express a package's full file
list, so a self-package build needs both routes over the same package. Steps 1-4 make it one
read and one fold, and the cache question does not arise. Do not build the cache.

`Hashing.dependencySignatureHash` stays deliberately outside all of this: it hashes contents
and never parses.

## Contract defects this made visible, still open

Neither is a front-end bug — each is a declaration naming a type the target's contract does not
publish, reported as a WARNING-severity `SignatureNotPublished` the drivers gate on:

- `Vesper.Printf`'s `Formatter` constructors name `TextWriter` / `StringBuilder`.
- `Vesper.List`'s `GetSlice` names `int option` — see [fsi-front-end-plan](fsi-front-end-plan.md).

## Not in this plan

- `AttributeDecode`'s short-name matching — [fsi-front-end-plan](fsi-front-end-plan.md) step 5.
- Content-addressed memoization of `PublishedSurface`, and the `ValRepr` flat-grouping fix it
  waits on.
- Conformance over two `PublishedSurface`s rather than two CSTs.
