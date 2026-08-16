# One fold over one file list

**Goal:** the expected dataflow of [fsi-front-end-plan](fsi-front-end-plan.md) §"Expected
dataflow" encoded by ONE function every interested caller uses. Delete this doc when step 4
lands (`feedback_plan_docs_ephemeral`).

**Status (2026-08-16):** the inputs are in place and the merge is blocked on the manifest
format alone. Absorbs the remnants of `package-parse-once-plan.md`, which is deleted — its
account of the remaining seam was misdiagnosed, see §"What this closes".

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
| an unsigned `.fs` publishes | the surface it infers | nothing |

The first two rows are already stated as one parameter in
[fsi-front-end-plan](fsi-front-end-plan.md) §"Constraints the merged path must keep". The
other two follow from it: across the boundary only what a `.fsi` declares crosses, which is
what makes a referenced package readable without compiling it.

Compile order permits the merge outright — `AssemblyFiles.fs` already sees `PackageSource`,
`SignatureResolution`, `Freeze` and `Pipeline`, and `PackageProviders` is after it.

## The blocker: one fold needs one order, and the manifest has two

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

Once [retire-sig-only](retire-sig-only-plan.md) lands, every `.fsi` HAS a companion, so that
rule is total and the merged list is fully determined by `impl` order. Until then it is
canonical only for the paired spine, and a companion-less `.fsi` has no position in `impl` to
be canonical about — which is the second reason to sequence that plan first.

- Every `.fs`-to-`.fs` relative order is preserved, so `SymbolProviders.inlineBodies`'s
  later-body-wins is untouched. That is the one place the merged order is load-bearing beyond
  readability, and it wants a test BEFORE the change, not after.
- The package compile order is preserved exactly: it already reads `impl` order alone.
- `fun-adapters.fsi` is the only entry that moves, earlier, in contract extraction only.

## Step 1: collapse `files` + `impl` into one ordered list

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
- `sig-only` and `runtime` stay. Neither is a group of files that gets compiled: `sig-only` is
  a declaration of intent that OUTRANKS the content split and has no spelling in a file list,
  and `runtime` is an asset list deliberately excluded from `sourceInputs` so an asset edit
  does not read as a source edit. (`sig-only` should be retired rather than relocated — see
  [retire-sig-only-plan](retire-sig-only-plan.md) — and nothing here depends on that.)
- `sourceInputs` collapses to the file list: `sig-only` is enforced to be a subset of `files`
  (`ConformanceVerdict.UnknownSigOnly`), and it is sorted before hashing, so nothing moves.

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

## Sequencing: [retire-sig-only](retire-sig-only-plan.md) FIRST

That plan's headline is now "make a signature file with no implementation file unrepresentable
for analysis" (user, 2026-08-16). Landing it makes the pairing TOTAL, which collapses this
plan's step 2 and simplifies its merge rule:

- `Unit` is not a three-case DU. Every unit has an implementation and an optional signature,
  which is what `AssemblyFiles.ParsedUnit` already IS — so step 2 becomes "the package route
  uses `ParsedUnit`", not "widen `ParsedUnit`".
- `impl`-driven ordering is canonical AND COMPLETE, not canonical for a spine with
  companion-less entries positioned off `files`. The merged list is a mechanical interleave
  with no exceptions.

Doing the merge first would mean building a `SignatureOnly` case in order to delete it, and
writing the spine/companion-less positioning rule in order to drop it. So this plan waits.

**What it waits on**, from that plan's inventory of 15 bodiless signatures: four entries gated
on other plans, two sentinel-repr bodies and two abbreviation bodies (all four gated on
nothing), and three `.fsi` dropped from `manifest.js.toml` for types js does not have.

**The long pole is the runtime-served pair, deferred by decision — the one class left without
an answer.** If it stays open, do NOT stall here: proceed carrying `SignatureOnly` and delete
the case when the pairing becomes total. A case with no inhabitants is cheaper to remove than a
missing one is to add back.

## Step 2: one unit type

`readPackage` already computes the pairing and then splits it into two lists whose element
types each make one half mandatory. Emit the pairing itself:

```fsharp
type Unit =
    | Paired of signature: ReadFile<ParsedSignature> * implementation: ReadFile<ParsedFile>
    | SignatureOnly of ReadFile<ParsedSignature>
    | ImplementationOnly of ReadFile<ParsedFile>
```

`ParsedPackage.Units : Unit list` in merged-list order replaces `Signatures` / `Implementations`
and the `Companion` fields on both. `AssemblyFiles.ParsedUnit` — which requires an
implementation, and is why a `sig-only` `.fsi` is invisible to the compiling route today — is
this type.

`PackageUnits.ofPackage` disappears: it exists only to project one of the two lists back into
a unit list.

**Exit:** `ConformancePass.check`, `buildProviderWith` and `inlineBodies` each walk one list,
and no consumer re-derives a pairing.

## Step 3: one fold

In `AssemblyFiles`, over `Unit list`, with the discriminator that made the four rows one:

```fsharp
/// Which side of the assembly boundary this fold publishes for.
type Publication =
    /// Compiling these units: each file homes in itself, the `.fs` is analysed and frozen,
    /// and an unsigned `.fs` publishes the surface it infers.
    | InAssembly of analyse: AnalyseFile
    /// Reading them as a reference: every symbol homes in the assembly, the package's
    /// `[<AutoOpen>]` prefixes are published, and only a `.fsi` crosses.
    | AcrossAssemblies
```

`AnalyseFile` is reachable only on the compiling arm, so "referencing, but it analysed the
bodies" is not a state that exists.

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
  `publishExternPrimitive` removed (retire-sig-only's class A′ passes through
  `SignatureResolution` anyway).

## What this closes

**The "reads a package set twice" seam, without a parse cache.** A compilation that both
REFERENCES and COMPILES a package reads it once per entry point today. `package-parse-once-plan`
diagnosed that as cache invalidation and pointed the next session at a parse cache keyed on the
manifest. That was wrong: the cause is that neither fold can express a package's full file
list, so a self-package build needs both routes over the same package. Steps 1-4 make it one
read and one fold, and the cache question does not arise. Do not build the cache.

**`sig-only` files invisible to the compiling route.** `list-bcl.clr.fsi` is unreachable when
`Vesper.List` compiles and works only because `buildContractForSelf` sends the package down the
other railway. Step 2 puts it in the list.

`Hashing.dependencySignatureHash` stays deliberately outside all of this: it hashes contents and
never parses, and it must cover the `sig-only` paths no consumer wants trees for.

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
- Retiring `sig-only` — [retire-sig-only-plan](retire-sig-only-plan.md).
