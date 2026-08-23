# Manifest-driven analysis as the only front end

Status: revised 2026-08-20. Everything under "Landed already" has landed, and so have all six
steps of the staged plan. This revision replaces the earlier gated/ungated pair with an
`analyse` / `compile` split, and carries the type names the source-identity rename settled on.
What remains is the "Independent findings" section.

## Root cause

The two subsections below are the state BEFORE step 1, kept for the reasoning steps 2-5 rest
on. Step 1 has since deleted tier 3's no-assembly entries and typed the assembly name.

There are three ways into the front end, and they disagree about what a compilation is.

| Tier | Entry | Gate | Diagnostics | Home assembly |
|---|---|---|---|---|
| 1 |  `CompileAssembly.analyseGated` | errors refuse the assembly (`AssemblyFiles.fs:797-802`) | anchored per file | yes |
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
      Units: AssemblyUnit list }

module AssemblySources =
    /// Name and target off the manifest, units in its file order.
    val ofParsedManifest : ParsedManifest -> AssemblySources
    /// `ofParsedManifest` for a caller holding only the path.
    val ofManifest : ReferencedProject.ManifestPath -> Result<AssemblySources, PackageSetFault>
    /// In-memory sources compiled under a caller-supplied name.
    val synthetic : name: string -> target: string -> Set<string> -> SourceUnit list -> AssemblySources

/// Every unit of the assembly as analysed, in manifest order.
type AnalysedAssembly =
    { Units: UnitOutcome list }
    /// Every unit's findings, each anchored in its own file.
    member Diagnostics : AnchoredDiagnostic list

/// An assembly with every file analysed and no error-severity finding. Minted only by `gate`.
type EmittableAssembly = { Files: FrozenFile list; Retained: LexedFiles }

module AnalysedAssembly =
    val analyse : AnalyseFile -> IExternalSymbolProvider -> AssemblySources -> AnalysedAssembly
    val gate : AnalysedAssembly -> Result<EmittableAssembly, AnchoredDiagnostic list>
```

`AssemblySources` takes its name from the existing `SourceFile` / `SourceUnit`, where "source" is
the INPUT text. `Retained` is the retained-`Lexed` collection an anchor is read through. The two
are distinct and neither is spelled `Source` alone.

```fsharp
// each backend
val ClrDriver.emit    : string list -> ProjectInfo -> EmittableAssembly -> ClrArtifact
val ClrDriver.compile : string list -> ProjectInfo -> AnalysedAssembly
                        -> Result<ClrArtifact, AnchoredDiagnostic list>
```

### What the shape buys

**The assembly name stops being an argument.** `CompilingAssembly` is derived inside
`AssemblySources.ofParsedManifest` from `Manifest.Name` (`ReferencedProject.fs:76-79`, `[core] name`
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

`AssemblySources.fs`, last in compile order: `ofParsedManifest` over `ParsedManifest`,
`ofManifest` for a caller holding only the path, and `synthetic` for in-memory sources.

`Units` is `AssemblyFiles.AssemblyUnit list` — the same type `AssemblyAnalysis.analyseUnits` takes, so both
routes hand the analysis what they built and nothing re-widens on the way in.

`ManifestPath` is minted only by `resolveManifest`, which is what makes a manifest's files and
its target agree. `synthetic` therefore mints an `AssemblySources` directly rather than a
synthetic `Manifest`.

`ofManifest` kept its `ManifestPath` parameter rather than taking target and package directory:
`JsPackageTests` and `ConformanceTests` hold a resolved `ManifestPath` already, and re-resolving
one from its parts would read the manifest twice.

Every caller now goes through `AssemblySources` and
passes `.Units` on to the driver, which still derives its `CompilingAssembly` from
`ProjectInfo.AssemblyName` — step 3 is where the driver takes the pairing instead.
`JsPackageTests` already drops its `loadManifest` and names the package from
`sources.Assembly.Name`. `AssemblySourcesTests.fs` pins the name and target off the manifest,
manifest file order, and that `synthetic` parses under the compilation defines it is given.

### 3. Total `analyse`, `gate`, `EmittableAssembly` — LANDED 2026-08-20

`CompileAssembly.fs` is now `AnalysedAssembly.fs`, last in compile order because `analyse` takes
an `AssemblySources`. `analyseGated`, `analyseWith`, `analyseAssemblyWith` and `analyseAssembly`
collapsed into `analyse` + `gate`, and `consolidatedDiagnostics` moved into `AssemblyFiles`
beside the `fileDiagnostics` it folds.

`gate` returns every error-severity finding in manifest order, as
`AnchoredDiagnostic.errors analysed.Diagnostics` — the same one-filter shape as
`PackageProviders.AnalysedManifest.gate`. Every suite is green: the removed suppression changes
which findings a refusal carries, and no test asserted on that.

`analyseUnits` returns an `AnalysedUnits`, carrying the units' published views beside their
outcomes, so the visibility stack it composed is the one every consumer uses.
`AssemblyAnalysis.visibility` is that composition, shared by the signature floor, the
across-assemblies body provider and `AnalysedAssembly.analyse`. `PackageProviders` reads
`.Published` rather than reversing a `ResizeArray` it filled in parallel, and
`AnalysedAssembly`/`EmittableAssembly` carry `Assembly` and `Visibility`.

Both backends' multi-file emission is total. `Codegen.compileFilesWithReferences` became
`Codegen.emitAssembly : string list -> ProjectInfo -> EmittableAssembly -> ClrArtifact`, since a
function that neither takes files nor returns `Result` had outgrown the name; it composes its
own `ICodegenSymbols` from `assembly.Visibility`, which deleted `ClrDriver.emit` along with
`ClrDriver.reanchored` and its `AssemblyFileId.nowhere` filing. `Codegen.compileFiles` had no
caller and is deleted. On the JS side `Codegen.emit` stays private under a new
`Codegen.emitAssembly : AnalysedManifest -> EmittableAssembly -> (AssemblyFileId * JsArtifact)
list`, which took the per-file project construction and the anchor-domain join out of
`JsDriver` along with its staging list, second `Ok`-matching pass and `Kind.Driver` re-wrap;
`compileWith` remains the gated single-file entry the tests use.

Both drivers reach `AssemblySources` through `AssemblySources.ofUnits`, which `synthetic` now
also builds on, so an assembly's name and target are minted in one place.

`ClrDriver.compileAssemblyWith` / `compileAssembly` keep their names: the name `compile` is the
single-file entry until step 5 deletes it, and only then can the pair be renamed onto it.

The bench fixtures moved with the tests — `SemanticAnalysisFixtures.analyseStage` and
`stageErrorCount` now speak `UnitOutcome` rather than `Result<FrozenFile, UnparsedFile list>`.

### 4. Migrate the compile-shaped tests — LANDED 2026-08-20

`TestHelpers.compileAgainst` is the one seam every compile-shaped helper reaches emission
through, and it is `ClrDriver.compileAssemblyWith` of a single unit, so the suite pins the
production composition rather than a copy of it. The driver splits into
`analyseAssemblyWith` (stopping before the gate, for a caller that wants the diagnostics) and
`emitAnalysed` (gate → `Codegen.emitAssembly`), with `compileAssemblyWith` their composition.
`analyseAssemblyWith` takes the assembly NAME, not a `ProjectInfo`: resolution reaches analysis
through the provider built from a reference set, so a `ProjectInfo` parameter would have offered
a `References` field that analysis never reads and let a caller analyse under one project and
emit under another.
`compileAgainst` replaced seven open-coded `parseFile` → `Pipeline.analyseFor` →
`CodegenSymbols.ofProvider` → `Codegen.compile` copies (`vesperListDll`,
`compileStructuralEngine`, `compileFixtureFile`, `compilePackages`, `CrossAssemblyEscapeTests`
twice, `SelfHostTests`, `StructTests`), so `TestHelpers.compilingClr` is deleted and
`Codegen.compile` is deleted outright, ahead of step 5.

`compileSource` and its family return `ClrArtifact` rather than `TastFile * ClrArtifact`, which
is what let 287 sites drop `let _,`. The 56 sites that read the tast asserted only that its
diagnostics were empty, so the family is SILENT by default: a warning refuses the compile.
Measured across the suite, exactly four tests warn, over two messages — the heterogeneous-enum
notice and a redundant downcast. Those four name what they tolerate through
`compileSourceWarning` / `runsLinesWarning`, which also require the warning to be PRESENT, so
two messages nothing used to pin are now covered. `analyseAs` became `diagnoseSource` /
`diagnoseSourceErrors`, returning the assembly's `AnchoredDiagnostic`s, with `mentioning` and
`diagnosticMessages` beside them for the message filters its callers ran.

Tests inspecting a pass's side tables stay on `analyseSemWithContextFor`.

**The finding:** nine class-inheritance tests went red with "inherits external base … but no
'.ctor' overload matches". `Codegen.emitAssembly` composes its `ICodegenSymbols` from
`EmittableAssembly.Visibility`, which carries every unit's own published view, so a class the
compilation itself declares is now answerable through the provider. `NominalEmit`'s base-class
classification assumed the opposite ("a project-local key is never in the provider's external
table") and filed a same-assembly base as external, minting an `AssemblyRef` back to ourselves.
`ClrProvider.InterfaceHandleOf` and `ClrExternalMembers` had each already open-coded a
local-first guard against exactly this, and `ClrEncoder` a fourth encoding of it as match-arm
ORDERING. The fix is one three-way answer, `ClassOrigin` (`Local` handle / `Foreign` handle /
`Unresolved`), computed by `ClrEnv.classOrigin` and routed through all four sites.
`BaseShape.LocalMono` carries the handle it was re-deriving through `provider.UserTypeHandle`,
and `Unresolved` — which used to reach `UserTypes.[key]` and throw `KeyNotFoundException` — is
now a diagnosed arm.

The encoder went one step further: `ClrEncoder` had probed `userTypes` in a match-arm guard AND
again inside the external recogniser beneath it. `(|UnionToken|_|)` / `(|RecordToken|_|)` /
`(|ClassToken|_|)` each answer "which token, and does it tag `VALUETYPE`" for one nominal
flavour, so local-vs-foreign precedence is stated once per flavour rather than carried by arm
order, and `encodeNominal` absorbs the seven transcriptions of the arity-0-or-`GENERICINST`
body. `ClrExternalMembers` routes its FIELD declaring type through `ClassOrigin` too — it had
kept the old foreign-only lookup under the same failure message — and `externalCtor` /
`externalParameterlessBaseCtor` now say `Foreign` where they used to reach past the DU.

**Second finding, NOT fixed:** the multi-file half of that bug is unreachable, because a CLASS
declared by a prior file resolves from no position in a later one. `inherit Shape(t)` reports
"Cannot inherit from unknown type 'Shape'" (`MemberRegistration.fs:797`) and a bare `Shape(42)`
fails the same way: `resolveThroughProvider` reaches intrinsic and platform classes only, with
no arm for an ordinary class a prior file published. Records, interfaces and module functions
all cross a file boundary. `CrossFileTests` carries the case as a `ptest`.

### 5. Delete the single-file driver path — LANDED 2026-08-20

`ClrDriver.compile`, `compileApp` and `compileForTfm` are gone, with the two private helpers
only they reached — `driverDiagnostic` and `unanchored`, the latter being the flattening that
rendered a file and position into a message. So every CLR entry now returns
`AnchoredDiagnostic list`. `Codegen.compileWithReferences` went with them, and `assembleGated`
under it, which leaves `Codegen.emitAssembly` as the only route into `assemble` and takes
`FrozenPools.blockingErrorsOfAll` (its sole caller) with it: the error gate now lives once, in
`AnalysedAssembly.gate`.

With the single-file path gone every entry is assembly-shaped, so the suffix that distinguished
them is dropped: `analyseAssemblyWith` → `analyseWith`, `compileAssemblyWith` → `compileWith`,
`compileAssembly` → `compile`. `emitAnalysed` keeps its name. The JS driver's `compileWith`
single-file entry is untouched.

`ClrDriverTests` was the only caller of the deleted pair, and it is the acceptance gate for the
`ClrCompilation` shape (an explicit `net8.0` ref pack rather than the host's runtime
assemblies), so both tests moved onto `compile` of a one-unit `SourceUnit list`; the ref-pack
test calls `Codegen.materialiseApp` itself, which is what `compileApp` added over `compile`.
Every suite is green.

### 6. Converge the two drivers on one seam — LANDED 2026-08-20

Step 5's review found the drivers rebuilding an `AssemblySources` their callers had just taken
apart: `compileWith` took `(project, AssemblyUnit list)` and re-derived name and target from
`project.AssemblyName` + a hard-coded `Target.Clr`, while `TestHelpers`, `SelfPackageIntrinsics`
and `JsPackageTests` all held one from `AssemblySources.ofManifest` and passed `.Units`. Both
`compileWith` entries now take `AssemblySources` whole, so the manifest's name and target reach
emission rather than being restated beside it.

`Codegen.Common/Frontend.fs` carries the analyse → gate → emit chain both backends share,
generic in the artifact each emits. `ClrDriver.analyseWith` was a pass-through over it once
`AssemblySources` arrived whole and is gone; `JsDriver.compileAssemblyWith` → `compileWith`,
matching its CLR counterpart again.

`AnchoredDiagnostic.render` / `renderAll` moved the `file(line,col): message` rendering into
`AssemblyFiles`, where step 5 had left four hand-rolled copies across three test projects and a
fifth that dropped the position. `ClrCompilation.forTfm` gives `RefPack` back a `src/` consumer
and ties the resolved pack to `Project.TargetFramework`, which `ClrDriverTests` had been
spelling twice. `ClrArtifact` carries its `ProjectInfo` instead of copying two fields off it, so
`materialiseApp` takes the artifact alone and cannot be handed a different project's.

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

~~**`obj` is minted in three places.**~~ Fixed 2026-08-20 as `BuiltinTypes.tyObj`, which is
where the other primitive `SemType`s already live (`RuntimeNames` carries keys, not types).
`checkOverrideConformance`'s four sibling locals went with it.

**`tryClassChainMemberOrField` re-looks-up its class.** `InferRecordAccess.fs:297` has `info`
bound and still uses it on the next line, but the new helper (`EngineCore.fs:326`) re-runs
`TypeRegistry.tryClassByKey` internally. The extraction is right — `Engine.fs:400` needed it —
but that call site pays a second registry lookup on the miss path.

~~**`compileSourceTo` widened for the minority.**~~ Fixed by step 4: the whole `compileSource`
family returns `ClrArtifact`, and the sites that read the tast only to assert its diagnostics
were empty went onto the `…Clean` variants.

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
carry a home assembly turned out to be zero. Steps 2, 3 and 5 were mechanical. Step 4 turned out
to be 343 call sites across 43 files, all of them compile-shaped.

Step 3 changes diagnostic SHAPE on the assembly path twice: a codegen refusal stops being
re-filed as `Kind.Driver` under `nowhere` (no current test asserts on it, the branch being
unreachable), and a parse failure stops suppressing the other units' findings.

Step 4 changes emitted code, which the plan did not anticipate: a base class declared by the
compilation itself was reached through an `AssemblyRef` back to our own assembly. The fix is
under step 4 above; the same rule was already open-coded for interfaces and for a cross-file
member's declaring type.

## Assumptions for confirmation

1. ~~**The single-file path has no consumer this repo cannot see.**~~ Taken as confirmed by step
   5, which deleted it. An out-of-repo driver bound to `ClrDriver.compile` gets the
   `SourceUnit list`-shaped `compile` in its place.
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
