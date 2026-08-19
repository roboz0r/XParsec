# Manifest-driven analysis as the only front end

Status: proposed, 2026-08-18. The cache deletion in "Root cause" below has landed; everything
under "Staged plan" has not started.

## Root cause

There are three ways into the front end, and they disagree about what a compilation is.

| Tier | Entry | Gate | Diagnostics | Home assembly |
|---|---|---|---|---|
| 1 | `AssemblyFiles.analyseGated` | errors refuse the assembly (`AssemblyFiles.fs:797-802`) | anchored per file | yes |
| 2 | `ClrDriver.compile` / `compileApp` / `compileForTfm` | codegen-level, added 2026-08-18 | flat, unanchored | yes |
| 3 | `Pipeline.analyseSem` / `analyseSemFor` called directly | none | none | tier 3 mostly not |

Tier 2 has **no caller under `src/`** — it is a test API with a production name. Tier 3 is
called directly from roughly 50 test files. So the invariant "no error diagnostics reach
codegen" already held at tier 1 by construction, and enforcing it across tiers 2 and 3 is what
made commit `a57405a4` touch 28 test files.

The divergence is semantic, not merely procedural. `Pipeline.analyseSem` carries no
`CompilingAssembly`, which changes how a locally declared type resolves — a type the home
assembly declares versus one a reference claims. `UnionTests.fs:380` already hit this and
worked around it by hand-rolling `analyseSemFor` with an explicit `compilingClr project`; the
workaround was replaced by a `compileSourceTo` call in `a57405a4`. Nothing establishes that it
was the only instance.

The target shape already exists and is load-bearing: `TestHelpers.fs:213-227` (`vesperCoreDll`)
runs `resolveManifest` → `PackageUnits.ofManifest` → `compileAssemblyWith`, and it does so for
the hardest case in the repo — `Vesper.Core`, which declares its own primitives and seeds its
own intrinsic axis. This plan promotes that path to the default rather than inventing one.

### Landed already: the compile cache is deleted (2026-08-18)

`Cache.fs`, `FrozenCache.fs`, `ClrDriver.compileCachedWith` / `compileCached` / `prepare` /
`PreparedCompilation` / `compilationDigest`, and the key half of `Hashing.fs` are gone, with
their four test files. `InputHash` moved to `InputHash.fs`, which is now source-identity only.

The cache served a design-time incremental need with wire machinery — `flatten` → `compress` →
store → `decompress` → `thaw` — which is why its key folded source bytes over the whole package
closure and could never cut off early. `D:\roboz0r\merkle-dag` replaces that axis with
structural hashing over an in-memory DAG and explicitly has no codecs, so nothing in the
deleted layer survives the transition. See the two-axis split in
[fsi-front-end-plan.md](fsi-front-end-plan.md).

`FrozenCodec` and `Compression` stay, dormant, for the wire axis.

## Staged plan

### 1. A one-file manifest entry point, gated and ungated

Until analysing one file through a manifest is as cheap to call as `compileSource`, tiers 2 and
3 regrow. Two functions, because a test whose subject IS a diagnostic must still be able to
analyse a program that fails:

- gated: assembly name + sources → `Result<ClrArtifact, AnchoredDiagnostic list>`, through
  `compileAssemblyWith`.
- ungated: the same inputs → the analysed files and their diagnostics, no refusal.

Both name the assembly and take an ordered file list, one entry being the common case. Ship
both in this step: bolting the ungated one on later is what produced tier 3.

### 2. Migrate the direct `Pipeline.analyse*` callers

Roughly 50 test files. Mechanical per file, but expect red, and the red is the deliverable —
each migrated test gains the gate, the per-file anchoring and the home assembly at once, and a
test that only passed under "no home assembly" is a finding of the `UnionTests.fs:380` kind.

Do not batch-convert silently. A test that goes red here is evidence about resolution, and the
disposition (fix the front end / fix the test / pin a `ptest` gap) is per-case.

### 3. Delete the single-file driver path

`ClrDriver.compile`, `compileApp`, `compileForTfm`, and the `Codegen.compile` / `compileWith`
single-file wrappers once nothing calls them.

### 4. Remove the codegen-level gate

With tier 1 the only way in, the gate `a57405a4` added to `Codegen.compileFiles*` is
unreachable: `analyseGated` refuses on `u.Surfaced` filtered to `Severity.Error`
(`AssemblyFiles.fs:797-802`), `Surfaced` under `Publication.InAssembly` is `fileDiagnostics`
(`:689-691`), and that includes `Frozen.Residue.Diagnostics` (`:279-287`) — a strict superset
of the gate's `Diagnostic.errors pools.Residue.Diagnostics`.

Deleting it collapses, in order of value:

- `JsDriver.fs:125-189` — back to one comprehension. The anonymous-record staging list, the
  `refusals` pass and the second `Ok`-matching pass exist only for the impossible branch.
- `ClrDriver.reanchored` (`:78-84`) — which files findings under `AssemblyFileId.nowhere`,
  because `FrozenPools.blockingErrorsOfAll` flattens a per-file list and loses the file. JS
  gets this right by gating per file; the CLR side cannot, which is itself evidence the gate
  sits at the wrong granularity.
- Both `Kind.Driver d.Message` re-wraps, which flatten a structured `Kind` into prose.

If a belt-and-braces check is still wanted on the assembly path, make it a `failwith`, matching
the precedent `unpositionedDiagnostics` sets at `AssemblyFiles.fs:216` for exactly this class of
internal contradiction. An impossible state does not deserve a `Result` branch plumbed through
two drivers.

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

Captured from `HashingTests.fs` before its deletion, because these are facts about the compiler
and not about the cache that once hashed them. Whatever computes a firewall's identity must
cover all of them.

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

Step 2 is the bulk and the only step with real uncertainty: the count of tests that fail once
they carry a home assembly is unknown, and each is a separate judgement. Steps 1, 3 and 4 are
mechanical and each leaves the tree green on its own.

Nothing here changes emitted code. Step 4 changes diagnostic SHAPE on the assembly path (a
refusal stops being re-filed as `Kind.Driver` under `nowhere`), which no current test asserts on
because the branch is unreachable.

## Assumptions for confirmation

1. **The single-file path has no consumer this repo cannot see.** `ClrDriver.compile` and
   friends have no `src/` caller; if an out-of-repo driver binds to them, step 3 is breaking.
2. **`analyseAs`'s "no home assembly changes resolution" is the whole divergence** between
   tiers 1 and 3, rather than one symptom of several.
3. **Merkle.Dag is the design-time answer**, so no replacement for the deleted cache is wanted
   in the interim and steps 1-4 need not preserve a caching seam.
4. **The printf specifier gaps are front-end work**, not codegen work — i.e. the fix is to type
   `%d`/`%g` over a numeric-family typar rather than to widen at the call.
