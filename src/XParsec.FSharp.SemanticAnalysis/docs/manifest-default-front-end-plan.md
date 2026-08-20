# Manifest-driven analysis as the only front end

Status: revised 2026-08-19. Everything under "Landed already" has landed, and so has step 1 of
the staged plan; steps 2-5 have not started. This revision replaces the earlier gated/ungated
pair with an `analyse` / `compile` split, and carries the type names the source-identity rename
settled on.

## Root cause

The two subsections below are the state BEFORE step 1, kept for the reasoning steps 2-5 rest
on. Step 1 has since deleted tier 3's no-assembly entries and typed the assembly name.

There are three ways into the front end, and they disagree about what a compilation is.

| Tier | Entry | Gate | Diagnostics | Home assembly |
|---|---|---|---|---|
| 1 | `AssemblyFiles.analyseGated` | errors refuse the assembly (`AssemblyFiles.fs:797-802`) | anchored per file | yes |
| 2 | `ClrDriver.compile` / `compileApp` / `compileForTfm` | codegen-level, added 2026-08-18 | flat, unanchored | yes |
| 3 | `Pipeline.analyseSem` / `analyseSemFor` called directly | none | none | tier 3 mostly not |

Tier 2 has **no caller under `src/`** — it is a test API with a production name. Tier 3 is
called from 47 test files. So the invariant "no error diagnostics reach codegen" already held at
tier 1 by construction, and enforcing it across tiers 2 and 3 is what made commit `a57405a4`
touch 28 test files.

### What the home assembly changes

`Pipeline.analyseSem` carries `CompilingAssembly.none`. Within the pass chain that value is read
at exactly two sites:

- `NameResolution/TypeRegistration.fs:179` — `asm <> ctx.AssemblyName` waives the CS0433 analogue
  for a type this compilation declares itself. Under `Name = ""` nothing is waived, so a locally
  declared type that a reference also answers for is refused. `UnionTests.fs:380` hit this.
- `PlatformTypes.fs:36` — `ctx.Target` names the target in "primitive not supported on X". Under
  `Target = ""` the message is wrong; nothing resolves differently.

`AssemblyFiles` reads it in three more places, reachable only through the fold: the two
`LexedFile.inFile { Assembly = assembly.Name; … }` identities at `:452`/`:478`, and the
`SignatureInputs.Assembly` at `:490`.

### The `""` assembly is one sentinel, not three

`CompilingAssembly.none` is where the empty name originates, and it flows outward:
`AssemblyFiles.fs:452`/`:478` pipe `assembly.Name` straight into `AssemblyFilePath.Assembly`, so
under `none` every file that fold produces carries `Assembly = ""` as well. Two further sites
mint it directly — `AssemblyFilePath.nowhere` and `AssemblyFilePath.ofText`
(`Anchor.fs:157`/`:175`) — plus the parse-failure identity at `AssemblyFiles.fs:261`.

Those three cases are not the same fact. `none` means *unknown*, and is the one that misresolves
(below). The other two mean *genuinely no assembly*: text handed over with no file behind it, and
a file that failed to parse so no assembly claims it. Typing the field as `AssemblyName voption`
splits them; typing it as `AssemblyName` with an empty string inside does not. Step 1 retires all
of them together, because typing `AssemblyFilePath.Assembly` while `CompilingAssembly.Name` stays
a bare `string` just moves the seam one call up.

### Landed already: the compile cache and the content hash are deleted

**2026-08-18.** `Cache.fs`, `FrozenCache.fs`, `ClrDriver.compileCachedWith` / `compileCached` /
`prepare` / `PreparedCompilation` / `compilationDigest`, and the key half of `Hashing.fs` are
gone, with their four test files.

**2026-08-19** (`50ff4f41`). What survived of `Hashing.fs` was source identity: a content hash on
every retained file, checked in `LexedFiles.tokenAt` before an anchor was read. That check answers
a cross-BUILD question — a file edited between two builds leaves every index in range and pointing
at a different token — and only the cache created one. In-process a file is lexed once and the
`LexedFile` retained, so the hash on a tree and the hash on the retained file are the same value
by construction and the branch could not fire.

So `InputHash.fs` and `Hashing.fs` are deleted, and `FileStamp` degenerated to the
`AssemblyFilePath` it wrapped. `FilePathRow` lost its `ContentHex`, shrinking the wire tables. The
one thing the hash still does is mint a distinct `<text:…>` identity for text with no file behind
it, which is now private to `AssemblyFilePath.ofText`.

Reviving the wire axis re-introduces the stamp: a persisted tree thawed against a re-read file is
exactly the case the check covered. That is a cost of the revival, not a reason to carry the
field now.

The cache served a design-time incremental need with wire machinery — `flatten` → `compress` →
store → `decompress` → `thaw` — which is why its key folded source bytes over the whole package
closure and could never cut off early. `D:\roboz0r\merkle-dag` replaces that axis with
structural hashing over an in-memory DAG and explicitly has no codecs, so nothing in the
deleted layer survives the transition. See the two-axis split in
[fsi-front-end-plan.md](fsi-front-end-plan.md).

`FrozenCodec` and `Compression` stay, dormant, for the wire axis.

## The shape

Two entries named for what they do, rather than one entry duplicated by whether it refuses.

**`analyse` is total.** It runs the front end over every unit a manifest names, in manifest
order, and returns what came out. A parse failure and an error diagnostic are members of the
result. Design time reads this: as much information as is recoverable, including from a file the
parser had to patch and from the files after one that yielded no tree.

**`compile` takes `analyse`'s output** and returns either an artifact or the errors that
prevented one. The gate lives here, once, at the only boundary where a broken tree matters.

```fsharp
// SemanticAnalysis — target-parameterised, backend-independent.

/// An assembly's inputs as the front end takes them: what it emits into, and its units in
/// compile order.
type AssemblySources =
    { Assembly: CompilingAssembly
      Units: Result<ParsedUnit, UnparsedFile> list }

module AssemblySources =
    /// Name and target off the manifest, units in its file order.
    val ofPackage : PackageSource.ParsedPackage -> AssemblySources
    /// `ofPackage` for a caller holding only the path.
    val ofManifest : ReferencedProject.ManifestPath -> Result<AssemblySources, PackageSetFault>
    /// In-memory sources compiled under a caller-supplied name.
    val synthetic : name: string -> target: string -> Set<string> -> SourceUnit list -> AssemblySources

/// Every unit of the assembly as analysed, in manifest order.
type AnalysedAssembly =
    { Units: FoldedUnit list }
    /// Every unit's findings, each anchored in its own file.
    member Diagnostics : AnchoredDiagnostic list

/// An assembly with every file analysed and no error-severity finding. Minted only by `gate`.
type EmittableAssembly = { Files: FrozenFile list; Retained: LexedFiles }

module AnalysedAssembly =
    val analyse : AnalyseFile -> IExternalSymbolProvider -> AssemblySources -> AnalysedAssembly
    val gate : AnalysedAssembly -> Result<EmittableAssembly, AnchoredDiagnostic list>
```

`AssemblySources` takes its name from the existing `SourceFile` / `SourceUnit` / `PackageSource`,
where "source" is the INPUT text. `Retained` is the retained-`Lexed` collection an anchor is read
through. The two are distinct and neither is spelled `Source` alone.

```fsharp
// each backend
val ClrDriver.emit    : string list -> ProjectInfo -> EmittableAssembly -> ClrArtifact
val ClrDriver.compile : string list -> ProjectInfo -> AnalysedAssembly
                        -> Result<ClrArtifact, AnchoredDiagnostic list>
```

### What the shape buys

**The assembly name stops being an argument.** `CompilingAssembly` is derived inside
`AssemblySources.ofPackage` from `Manifest.Name` (`ReferencedProject.fs:76-79`, `[core] name`
else the manifest's directory name) and `Manifest.Path.Target`. A caller cannot pair a name with
another package's units. `synthetic` demands the name because it has no manifest to read one
from.

**`EmittableAssembly` deletes the codegen gate rather than proving it unreachable.**
`Codegen.compileFilesWithReferences` taking a type only `gate` mints makes the residue check
unrepresentable, and `emit` becomes total. That removes `ClrDriver.reanchored` (`:78-84`) and
its `AssemblyFileId.nowhere` filing, `JsDriver.fs:125-189`'s staging list and second `Ok`-matching
pass, and both `Kind.Driver d.Message` re-wraps.

**Most of the tier-3 pull disappears.** Of ~310 `ctx.*` reads across the test suite, 182 are
`ctx.Diagnostics` — the `PassContext` used as a diagnostic bag. `AnalysedAssembly.Diagnostics`
serves those directly. The remaining ~130 (`ctx.Store` 34, `ctx.Types` 30, `ctx.Resolution` 21,
`ctx.Bindings` 20, `ctx.Intrinsics` 11) are pass-internal side tables belonging to tests of a
pass rather than of a compilation.

**Analysis needs no DLL.** A `depends-on` entry contributes through its package directory, read
and analysed under `Publication.AcrossAssemblies` (`PackageProviders.fs:142`). The `dllPaths` in
`compilationContract` (`ClrSymbolProviders.fs:139-145`) feed the .NET metadata reader alone. So
a dependency chain analyses without any DLL existing, and DLLs enter at `emit`, for the
`AssemblyRef`.

### Where a pass-level entry survives

`Pipeline.analyseSemWithContextFor` stays, as the single-file entry for a test inspecting a
pass's side tables, and `analyseSemWithContextForCore` beside it for the one that also needs the
region verdicts (`RegionProbe`). Both keep their `CompilingAssembly` parameter and gain no
default. The division is then: `analyse` / `compile` compiles, `analyseSemWithContextFor`
inspects — not two tiers of the same job.

## Staged plan

### 1. Delete the no-assembly entries and type the assembly name — LANDED 2026-08-19

Every suite is green with no disposition to take: the tests that would have exercised
`diagnoseExternalClaim` under a home assembly were already moved onto `analyseAs` /
`compilingClr` by `a57405a4`, so naming an assembly at the remaining sites changed no verdict.
`Manifest.Name`, `ProjectInfo.AssemblyName` and `Kind.Conformance`'s first field stay `string`
and convert at their boundary; `JsHome.Assembly` reads through `AssemblyName.toStored`.

`Pipeline.analyseSem`, `analyse`, `analyseSemWithContext`, `analyseWithContext`,
`analyseSemWithRegions` (`Pipeline.fs:79-102, 125-137`) and `CompilingAssembly.none`
(`PassContext.fs:15`). Every caller then names an assembly and a target: 67 no-assembly
`Pipeline.analyse*` sites across 39 files, plus 21 hand-built `PassContext(…)` sites across 14
test files.

Mechanical per site, and the red is the deliverable — a test that only passed under `Name = ""`
is a `UnionTests.fs:380` finding about `diagnoseExternalClaim`, and one that only passed under
`Target = ""` is asserting on a malformed message. Do not batch-convert silently; the
disposition per case is fix the front end / fix the test / pin a `ptest` gap.

With `none` gone, the remaining `""` assemblies are the two that mean it, so the field can carry
the fact:

- `AssemblyFilePath.Assembly : string` becomes `AssemblyName voption`. `AssemblyName`
  (`SymbolKeys.fs:12`) already models this and is already the payload of `SymbolHome.InAssembly`,
  which sits three lines from the `SymbolHome.InFile` that reaches the untyped one. It moves up
  into `Anchor.fs` (item 13 in the `.fsproj`, ahead of `SymbolKeys.fs` at 24) with no other
  reordering.
- `SymbolHome.AssemblyOption` then returns `AssemblyName voption` rather than collapsing the
  typed case to a string, and `TypeRegistration.fs:179`'s `asm <> ctx.AssemblyName` becomes a
  typed compare.
- `CompilingAssembly.Name : string` becomes `AssemblyName`, since it feeds
  `AssemblyFilePath.Assembly` directly at `AssemblyFiles.fs:452`/`:478`.

`ValueNone` then means "no assembly claims this file" at exactly two sites —
`AssemblyFilePath.ofText` and the parse-failure identity — and nothing means "unknown".

This step harvests the resolution findings without touching what any test asserts on.

### 2. `AssemblySources` — LANDED 2026-08-19

`AssemblySources.fs`, after `PackageUnits.fs`: `ofPackage` over `PackageSource.ParsedPackage`
(units via the existing `PackageUnits.ofPackage`), `ofManifest` for a caller holding only the
path, and `synthetic` for in-memory sources.

`ManifestPath` is minted only by `resolveManifest`, which is what makes a manifest's files and
its target agree. `synthetic` therefore mints an `AssemblySources` directly rather than a
synthetic `Manifest`.

`ofManifest` moved off `PackageUnits` and kept its `ManifestPath` parameter rather than taking
target and package directory: `JsPackageTests` and `ConformanceTests` hold a resolved
`ManifestPath` already, and re-resolving one from its parts would read the manifest twice.

Every `PackageUnits.ofManifest` / `ofPackage` caller now goes through `AssemblySources` and
passes `.Units` on to the driver, which still derives its `CompilingAssembly` from
`ProjectInfo.AssemblyName` — step 3 is where the driver takes the pairing instead.
`JsPackageTests` already drops its `loadManifest` and names the package from
`sources.Assembly.Name`. `AssemblySourcesTests.fs` pins the name and target off the manifest,
manifest file order, and that `synthetic` parses under the compilation defines it is given.

### 3. Total `analyse`, `gate`, `EmittableAssembly`

`analyse` is `foldUnits` with the two filters of `analyseGated:783-812` lifted out into `gate`.
Today's `AnalysedAssembly` is renamed `EmittableAssembly`; the name `AnalysedAssembly` moves to
the total result. `analyseGated`, `analyseWith`, `analyseAssemblyWith` and `analyseAssembly`
collapse into `analyse`.

`Codegen.compileFilesWithReferences` and the JS peer take `EmittableAssembly` and stop returning
`Result`.

`gate` reports every error-severity finding, parse failures first in file order, and truncates
nothing. The suppression at `analyseGated:791-794` goes: design time reads `analyse`, where a
parse failure sits on its own file and the cascade it caused sits on the others.

### 4. Migrate the compile-shaped tests

The tests whose subject is a compiled artifact or an assembly's diagnostics move to
`AssemblySources.synthetic` → `analyse` → `compile`. `compileSource` and its family
(`test/…Clr.Tests/TestHelpers.fs:449-540`) become thin wrappers over that, which is where the
`defaultPackages` contract and `withCore` references stay wired.

Tests inspecting a pass's side tables stay on `analyseSemWithContextFor` and are not touched
beyond step 1.

### 5. Delete the single-file driver path

`ClrDriver.compile`, `compileApp`, `compileForTfm`, and the `Codegen.compile` / `compileWith`
single-file wrappers once nothing calls them. `ClrDriver.compileAssembly` /
`compileAssemblyWith` are subsumed by step 3's `compile`.

## Independent findings

These came out of the `a57405a4` review and do not depend on the migration.

**`TypeStore.Quantified` is not arena-shaped** (`TypeStore.fs:183`). Every other per-var fact is
a parallel array authoritative on the root (`level`, `link`, `units`, `:89-91`) or a rep-keyed
payload joined in `MergePayloads` (`:185-189`). A bare `HashSet<TyVarId>` is neither, is not
migrated on union, and forces its only reader to rebuild a set of re-`find`ed roots per call
(`Unification.fs:947-948`) — which is the only reason `resolveNullLiterals` guards on
`NullLiterals.Count = 0`. Replace with a `bool[]` set on the root and OR'd on union; the OR is
exactly what re-`find`ing every id computes today, so the change is behaviour-preserving, and it
deletes the temp set, the guard, and the doc comment's instruction to read through
`UnionFind.find`.

**`obj` is minted in three places.** `TyConst(RuntimeNames.objKey, EqArray.empty)` at
`Unification.fs:606`, `Unification.fs:945` and `Elaborate/ObjArgs.fs:12`. Add `tyObj` to
`RuntimeNames` beside `objKey`.

**`tryClassChainMemberOrField` re-looks-up its class.** `InferRecordAccess.fs:297` has `info`
bound and still uses it on the next line, but the new helper (`EngineCore.fs:326`) re-runs
`TypeRegistry.tryClassByKey` internally. The extraction is right — `Engine.fs:400` needed it —
but that call site pays a second registry lookup on the miss path.

**`compileSourceTo` widened for the minority.** 15 call sites; 12 now open `let _, artifact =`
or `|> snd` so 3 in `UnionTests` can reach the tast. Restore `ProjectInfo -> string ->
ClrArtifact` and give the 3 a separate helper. Related: the surviving
`Expect.isEmpty tast.Diagnostics` at `UnionTests.fs:231,384,458` now covers warnings only,
since `compileSourceTo` fails on errors through `emitted` — say so or drop it.

**Two printf specifier gaps are now hard failures.** `printfn "%d" 200uy` and `%g` on a
`float32` compiled and printed correctly before the gate; they are `ptest`s as of `a57405a4`
(`LiteralTests.fs`, `PrintfHappyPathTests.fs`). `%d` types its argument as exactly `int` and
`%g` as exactly `float`, where F# types each as a typar over its numeric family. The standing
state is that a printf specifier outside its default width is uncompilable, which wants a fix
rather than two disabled tests.

## Determinants of a compiled file

Captured from `HashingTests.fs` before it was cut down, because these are facts about the
compiler and not about the cache that once hashed them. Whatever computes a firewall's identity
must cover all of them. (What survives of that file is `AssemblyFilePathTests.fs`, pinning that
`AssemblyFilePath.ofText` is a function of the text.)

- The home assembly name, and the backend target.
- The reference assemblies, by identity and IN ORDER — resolution is first-hit by name, so a
  reorder is a different environment. A named-but-absent reference differs from no reference.
- The package set, as a SET: order- and multiplicity-insensitive.
- The self package, distinct from the same package as a reference: inside its own compile a BCL
  signature presents that package's primitives.
- The compilation defines, and which symbol is defined — they pick the `#if` branch parsed.
- The transitive `depends-on` closure, not just the roots named.
- Per target: a body only the `js` manifest lists cannot move a `clr` consumer.
- Within a package: the manifest's file ORDER, every listed `.fsi` and `.fs`, inline bodies that
  splice into a consumer's tree, intrinsic-repr companions that decide what a primitive resolves
  to, and the distinction between an absent file and a present empty one.

## Scope and risk

Step 1 was the bulk and carried the only real uncertainty: the count of tests failing once they
carry a home assembly turned out to be zero. Steps 2, 3 and 5 are mechanical. Step 4 is bounded
by how many tests are compile-shaped rather than pass-shaped.

Nothing here changes emitted code. Step 3 changes diagnostic SHAPE on the assembly path twice: a
codegen refusal stops being re-filed as `Kind.Driver` under `nowhere` (no current test asserts on
it, the branch being unreachable), and a parse failure stops suppressing the other units'
findings.

Sequencing note: a test migrated in step 4 is touched twice, once in step 1. Step 1 runs first
regardless, because it is what produces the resolution findings and it produces them without
depending on any of the new shape.

## Assumptions for confirmation

1. **The single-file path has no consumer this repo cannot see.** `ClrDriver.compile` and
   friends have no `src/` caller; if an out-of-repo driver binds to them, step 5 is breaking.
2. **`gate` truncates nothing.** A missing early file can cascade into spurious unresolved-name
   errors across every later unit, and `analyseGated:791-794` exists to hide that cascade behind
   the parse failure. Under this plan `gate` returns all of them, parse failures first, and
   whether to stop at the first faulted file is the caller's presentation choice.
3. **Merkle.Dag is the design-time answer**, so no replacement for the deleted cache is wanted
   in the interim and steps 1-5 need not preserve a caching seam. This now also covers the
   deleted content hash: if the dormant wire axis is revived, a per-file stamp comes back with
   it, and the "Determinants" list above is what such a stamp has to cover.
4. **The printf specifier gaps are front-end work**, not codegen work — i.e. the fix is to type
   `%d`/`%g` over a numeric-family typar rather than to widen at the call.
