# Per-target package manifests — `manifest.<target>.toml`

*Replaces the `[core]` + `[targets.<t>]` two-tier manifest with one flat manifest per target.
Depends on `extern-is-self-evident-plan.md` landing first: while intrinsic-ness is derived from
the union of every target's `impl` list, no single-target manifest can reach the right verdict.*

## The defect: the shared tier does not share

`[core]` and `[targets.<t>]` resolve by CONCATENATION, shared-first
(`ReferencedProject.fs:109-121`). That ordering is wrong for `impl` in exactly the two packages
with non-trivial impl ordering, and both manifests say so in their own comments:

- `src/Vesper.Core/manifest.toml:27-29` — "The target-neutral bodies (`core-types`,
  `structural-format`, `ops-std`) are listed per target, not shared: a shared `impl` would append
  BEFORE each target's list, ahead of the `Fun` and `Attribute` declarations those bodies need."
- `src/Vesper.Array/manifest.toml:19-20` — "`array.fs` is target-neutral but repeated per target,
  not shared: inherited lists append AFTER shared ones, which would put it BEFORE the prelude it
  splices."

So `[core] impl` is unused by the packages that would benefit from it. The mechanism costs a
section, a concatenation rule, and a `target` parameter threaded through the whole resolution
API, and buys sharing only for `[core] files`, where prepending happens to be correct.

## The design

One manifest per target, flat, in the package directory:

```
src/Vesper.Core/manifest.clr.toml
src/Vesper.Core/manifest.js.toml
```

Flat file names rather than `clr/manifest.toml`, so manifest-relative source paths are unchanged.
Each file carries `[core]` only — `name`, `description`, `depends-on`, `files`, `impl`,
`sig-only`, `impl-only`, `runtime` — as one ordered list per key, exactly what the compile
consumes. `[targets.<t>]` and the `SharedLists`/`TargetLists` split are deleted.

## What it buys

- **The append-order hack disappears.** Each list is the compile order, written once, read once.
  The two apology comments above are deleted rather than reworded.
- **`target` stops threading through SemanticAnalysis.** It is a parameter on `resolveFiles`,
  `resolveImpl`, `resolveSigOnly`, `resolveImplOnly`, `resolveRuntime`, `pairingKey`,
  `runtimeModules`, `buildProviderWith`, `composeOrdered`, `composeContract`, `provider`, and
  `SymbolProviders.inlineBodies`. Inside the analysis the string is read in exactly two live
  places: a diagnostic label (`VesperLib.fs:1131`) and the compilation hash (`Hashing.fs:132`).
- **`depends-on` becomes per-target, correctly.** `src/Vesper.Printf/manifest.toml` depends on
  `Vesper.List` because "the CLR `%A` engine uses [it] as its `Doc` child lists and frame stack";
  the JS `%A` engine is a free function that does not. Today the JS closure drags `Vesper.List`
  in regardless.
- **Hashing gets finer.** `dependencySignatureHash` folds `sourceInputs`
  (`Hashing.fs:74-98`), which is deliberately target-blind — every target's lists unioned
  (`ReferencedProject.fs:148-161`) — so a CLR-only body edit invalidates the JS frozen cache.
  Per-target manifests make the digest per-target for free.
- **A missing target becomes loud.** `listsFor` falls back to `TargetLists.empty`
  (`ReferencedProject.fs:104-105`), so "this package does not build for JS" and "this package
  contributes nothing extra on JS" are indistinguishable states. An absent `manifest.js.toml` is
  unambiguous.
- **Per-target conformance becomes cheap**, which closes the loose end from
  `extern-is-self-evident-plan.md`: the real-package sweep is CLR-only today
  (`ConformanceTests.fs:250`), so `ExternWithoutIntrinsic` is never checked on JS.

## Accepted costs

**Contract-list duplication — ACCEPTED, do not mitigate with `include`/`extends`.** Vesper.Core's
18 `[core] files` entries get written in both manifests, and drift (add a `.fsi` to clr, forget
js) is a new failure mode with a distant symptom. An inheritance key would reintroduce the design
being removed. Duplicate the list; if it bites, add a conformance check that both manifests name
the same contract set modulo each one's declared extras. Only Vesper.Core is large — most
packages list one or two files. `name` / `description` / `depends-on` duplicate likewise; `name`
already has to match the directory (`ReferencedProject.fs:241-247`), so a mismatch is caught.

**The `.clr.fs` / `.js.fs` filename suffixes SURVIVE.** They exist so two targets' bodies coexist
in one directory; a manifest split renames no files. `pairingKey`
(`ReferencedProject.fs:130-143`) only shrinks from "strip a suffix for any target this manifest
declares" to "strip `.<myTarget>`". Killing the suffix entirely needs per-target subdirectories or
explicit `.fsi`↔`.fs` pairs in the manifest — a separate move, deliberately NOT bundled here.

## Migration surface

- **Types.** `SharedLists`, `TargetLists`, and `Manifest.Targets` collapse to one flat record.
  `sourceInputs` loses its union. `listsFor` and the six `resolve*` functions become field reads.
- **`dependencyManifestPath`** (`ReferencedProject.fs:280-283`) gains the target in the filename;
  `buildClosure` / `buildClosureWithDeps` / `closeAndOrder` (`:288-408`) gain a target parameter
  or take pre-qualified paths.
- **Call sites.** ~30 hardcoded `"manifest.toml"` literals across tests and helpers. Most already
  have the target in hand — `TestHelpers` in both codegen test projects, `ConformanceTests`,
  `HashingTests`, `ReferencedProjectTests`, `SpecializationTableTests`, `ExternMemberInlineTests`,
  `CrossAssemblyEscapeTests`, `FrozenCacheIncrementalTests`, `StructTests`, `Conformance.fs`,
  `SemanticAnalysisFixtures`. A `manifestPath dir target` helper covers nearly all.
- **Fixture packages.** `test/Codegen.Conformance`, `test/…/fixtures/widget`, and the `tmp/`
  scratch packages regenerate.
- **Hashing tests.** `files-target-change`, `target-companion-change`, and `absent-to-empty`
  encode the target-blind digest and need rewriting to the finer per-target semantics.
  `manifest-order-change` and `self-manifest-edit` still hold.

**Do NOT drop `CompilationInputs.Target`** (`Hashing.fs:105-107`) on the grounds that the manifest
path now encodes the target. `platform-facts-plan.md` makes backend-supplied facts drive
type-checking verdicts, at which point the target is a determinant of the frozen tree in its own
right.

## Anchors (verify before editing)

- Manifest types + parse: `ReferencedProject.fs:11-81` (records), `:163-263` (key sets, `parseTargets`,
  `parseManifest`).
- Resolution API to flatten: `ReferencedProject.fs:104-161`.
- Closure + dependency path: `ReferencedProject.fs:277-408`.
- Provider build (also the `everyBody` site retired by `extern-is-self-evident-plan.md`):
  `ReferencedProject.fs:452-560`.
- Hash inputs: `Hashing.fs:74-98` (`dependencySignatureHash`), `:102-119` (`CompilationInputs`).
- Other manifest readers: `ConformancePass.fs:145-200`, `SymbolProviders.fs:150-208`
  (`inlineBodies`), `ReferencedProject.fs:413-431` (`runtimeModules`).
- Target tag constants (their doc comment names `[targets.<t>]` and must be rewritten):
  `Codegen.Common/Target.fs`.
