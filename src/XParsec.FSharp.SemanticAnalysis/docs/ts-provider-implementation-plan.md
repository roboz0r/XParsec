# TS provider — implementation plan (resilient extraction → real packages)

**Status:** not started. **Companion to**
[`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md) — that doc
holds the *decisions* (the why); this one is the *runnable sequence* (the how), so a
fresh session can execute it. Ephemeral per the repo convention — delete once the
phases land and fold any durable facts into module headers / memory.

Read the design plan's **"Failure contract: resilient extraction with diagnostics"**
and **"`null` / `undefined` as JS-intrinsic types"** subsections first; this plan
assumes those decisions.

## Orientation — the files you will touch

- **Schema (shared contract):** `src/Vesper.Ts.Manifest.Schema/Schema.fs` (the IR
  types) + `Codec.fs` (serialise/deserialise). Fable-compiled for the extractor *and*
  used natively by the F# loader — so it must stay Fable-safe.
- **Extractor (producer, Fable F# → JS, runs under Node):**
  `src/Vesper.Ts.Extractor/Extractor.fs`. Built with
  `dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`, then run
  via `node dist/Program.js`. Key entry points: `mapType` (line ~153, the
  `ts.Type → Schema.TypeRef` mapper — where most degradations live), the per-export
  dispatch (`toExport`-style arms ~524–690), `extractPackage` (~781), `run`/`runPackage`.
- **Provider (consumer, .NET):** `src/XParsec.FSharp.Codegen.Js/TsManifestProvider.fs`
  — `toFrozen` (`TypeRef → FrozenType`, line ~24) and `providerOfManifest`.
- **Front-end types:** `SemanticInfo.fs` (`SemType`/`FrozenType`; `TyOr`/`FTOr`
  exist, `TyDynamic` does not). Intrinsic registration: CLR side
  `src/XParsec.FSharp.Codegen.Common/IntrinsicRepr.fs`; JS side the `*.js.fs`
  prim-types companions in `src/Vesper.Core/` (e.g. `prim-types-min.js.fs`) + the
  `IIntrinsicReverseCanon` map in `Codegen.Js/JsNativeSymbols.fs`.
- **Test harness:** producer `test/Vesper.Ts.Extractor.Tests/TestHelpers.fs`
  (golden infra: `testManifestCanonical`, `testProviderResolves`,
  `testExtractorMatchesGolden[Package]`, `UPDATE_SNAPSHOTS=1`); consumer
  `test/XParsec.FSharp.Codegen.Js.Tests/TestHelpers.fs` (`runJs`/`runNode` —
  compile `.fs` → `.mjs` → run under Node; `emitWithCalc` inline-manifest pattern in
  `MemberOverloadTests.fs`).

Build/test via the **xparsec-dev** skill (`./claude_tools.cmd -Action Build|Test|Format`),
never raw `dotnet`. Line numbers above are from this writing — confirm before editing.

## Ordering at a glance

```
Phase 0  null/undefined intrinsics ......... gates every real package
Phase 1  diagnostics channel (schema+codec)  independent of 0
Phase 2  resilient extractor (1 → 2) ........ failwith → diagnostic + degrade
Phase 3  neutral fixture location ........... independent; do before mitt
Phase 4  mitt producer fixture (0,2,3 → 4) .. golden + provider-resolves
Phase 5  real-package e2e (0,4 → 5) ......... .fs → emit → run vs real runtime
```

Phases 0/1/3 are independent and can land in any order; 2 needs 1; 4 needs 0+2+3; 5
needs 4+0. Land each phase green before the next.

---

## Phase 0 — `null` / `undefined` as intrinsic types + values

**Goal.** `string | null` (and `| undefined`) resolves through the provider AND
survives JS emit, so nullable-saturated real `.d.ts` doesn't fall over. `null` =
cross-backend core intrinsic; `undefined` = JS-only intrinsic type + value literal.
Neither unified with `unit`.

**Steps.**
1. **Locate the JS intrinsic-registration pattern.** Find how an existing JS
   primitive (`unit`, `int`, `obj`) is registered end-to-end: the `*.js.fs`/`.fsi`
   prim-types companion in `src/Vesper.Core/`, the canon↔platform-repr wiring, and
   the `IIntrinsicReverseCanon` map (`JsNativeSymbols.fs`). The value `unit → JS
   undefined` lowering already lives at `JsEmitHelpers.fs:113` — note it but do NOT
   reuse it for `undefined` the type (that coincidence is the trap; see the design
   plan). Memory `project_js_numeric_reprs_todo` describes this layer.
2. **Register `null` as a core intrinsic** with a per-target repr (CLR: null
   reference / `ldnull`; JS: `null`), canon `"null"`. It is the F# 9 `T | null`
   nullable-reference surface, so check whether `null`-the-type already half-exists
   on the CLR path before adding a JS-only entry.
3. **Register `undefined` as a JS-only intrinsic type** (canon `"undefined"`, JS repr
   `"undefined"`) **and a value literal**. Value-literal fork (decide here, low
   stakes): a dedicated keyword like `null`, or an intrinsic binding. Recommend a
   keyword for symmetry with `null`.
4. **Make `EmitJs.validatePlatformTypes` accept both** (the check that errors on
   BCL-fallback types — see memory `project_js_numeric_reprs_todo`). Before this,
   `null`/`undefined` are unregistered names and would trip it.

**Acceptance.**
- A new `Codegen.Js.Tests` case emits a program whose type involves `T | null` and
  `T | undefined` (reuse the shapes in `specs/unions/unions.d.ts`) and **runs under
  Node** (`runJs`) without a `validatePlatformTypes` error — round-trips a nullable
  value and a present value.
- `null` and `undefined` resolve as distinct types; neither unifies with `unit`
  (a direct unit-vs-undefined unification test fails as expected).

**Gotchas.** Keep the "both emit JS `undefined`" knowledge in the backend repr, not
in type identity. `null` is NOT JS-only — don't bury it in a JS-only companion if the
CLR path wants it too.

---

## Phase 1 — diagnostics channel in the schema + codec

**Goal.** The manifest carries `diagnostics` so the extractor can record degradations
instead of throwing.

**Steps.**
1. **`Schema.fs`:** add
   ```fsharp
   [<RequireQualifiedAccess>]
   type Severity = | Warning | Error

   type Span = { File: string; Start: int; End: int }   // {file, span} coordinate; reuse for the authored-form AST backlink

   type Diagnostic =
       { Severity: Severity
         Code: string          // stable id, e.g. "method-axis-typar-erased"
         Symbol: string        // qualified symbol the diagnostic attaches to
         Span: Span option
         Message: string }
   ```
   and add `Diagnostics: Diagnostic list` to `PackageManifest`. Keep everything
   Fable-safe (records + RQA DUs only — no .NET-only APIs).
2. **`Codec.fs`:** serialise/deserialise the new field + types. Match the existing
   hand-rolled style in that file.
3. **No `SchemaVersion` bump** (prototyping; manifests are repo-only). Regenerate all
   existing goldens with `UPDATE_SNAPSHOTS=1` (they get an empty `diagnostics: []`).

**Acceptance.** `testManifestCanonical` passes on every regenerated golden
(deserialise→serialise round-trips, including an empty diagnostics list). A
hand-written manifest with one diagnostic round-trips.

**Gotcha.** Decide the diagnostic-code vocabulary now (a small closed list — see
Phase 2) so the codes are stable from day one; the coverage golden keys on them.

---

## Phase 2 — resilient extractor: `failwith` → diagnostic + degrade

**Goal.** Per-type mapping failures emit a `Warning` diagnostic and return a degraded
`TypeRef`; extraction never aborts on a single bad type. Fatal plumbing stays fatal.

**Steps.**
1. **Thread a diagnostics accumulator.** The extractor is a single-threaded Node
   program, so a `ResizeArray<Schema.Diagnostic>` captured by the extraction (or
   passed to `mapType`/the export walk) is simplest. `extractPackage` drains it into
   `manifest.Diagnostics`.
2. **Convert each per-type `failwith` to `emit diagnostic + degrade`** (the "if it can
   be named, degrade it" bar):
   | Site (≈line) | Code | Degrade to |
   |---|---|---|
   | method-axis typar `:166` | `method-axis-typar-erased` | `TypeRef.Named("obj", [])` |
   | structural object `:241` | `structural-object-stubbed` | `TypeRef.Structural(hash, fields)` (already a stub on the provider) |
   | asymmetric accessor `:324` | `asymmetric-accessor-narrowed` | the getter's type (pick one side) |
   | merged class+namespace `:675` | `merged-namespace-dropped` | keep class half, diagnose the dropped namespace |
   Attach the symbol name + span (the `ts.Node` gives `getSourceFile().fileName` +
   `getStart()`/`getEnd()`).
3. **Leave fatal throws alone:** "not a module / no exports" (~708), source unreadable
   (~731), specifier unresolvable (~808/825). These abort the run by design.
4. Rebuild the extractor (`dotnet fable …`) before running producer tests.

**Acceptance.**
- Every existing single-feature spec still extracts to its golden (regenerate;
  diagnostics stay empty — none of them hit a degrade path).
- A new spec that deliberately uses a method-axis generic (`fn<U>(x: U): U` on a
  member, or a `keyof` method) extracts **without throwing**, with exactly one
  `method-axis-typar-erased` warning, and the member resolves via
  `testProviderResolves`.

**Gotcha.** A degraded member must still produce a *valid* `TypeRef` the provider can
resolve — `obj`/`Structural`/`Dynamic` all already map in `toFrozen`. Don't emit a
`TypeRef` shape the consumer can't rehydrate.

---

## Phase 3 — neutral shared-fixture location

**Goal.** Package fixtures (consumed by *both* test projects) live outside either, so
neither reaches into the other's tree. (Design plan: *Test ownership* subsection.)

**Steps.**
1. Create `test/ts-fixtures/` (NOT `packages/` — the repo `.gitignore`
   `**/[Pp]ackages/*` swallows that; the existing dir is `pkgs/` for this reason).
2. Move the existing `pkgs/multifile/` there; update the path constants in
   `Vesper.Ts.Extractor.Tests/TestHelpers.fs` (`packagesDir`, `packageManifestOf`,
   the globs) to point at the neutral dir.
3. Confirm the dir is committed (not gitignored) and the orphan guard still passes.

**Acceptance.** Extractor test suite green against the moved fixture; `git status`
shows the fixture tracked.

---

## Phase 4 — `mitt` producer fixture

**Goal.** A real npm package extracts to a committed golden with **zero diagnostics**.

**Steps.**
1. **Vendor** into `test/ts-fixtures/mitt/`: mitt's published `index.d.ts`, a minimal
   `package.json` (pin the version — it is the refresh provenance), and mitt's runtime
   `mitt.mjs` (needed by Phase 5). No `npm install` at test time.
2. Generate the golden in package mode: run the extractor `--package ./mitt
   <ts-fixtures-dir> mitt <out>` via `UPDATE_SNAPSHOTS=1`, committing
   `mitt/mitt.manifest.json`.
3. Add `mitt` to the package-fixture suites: `testExtractorMatchesGoldenPackage`
   (golden diff, Node) + `testProviderResolves` (pure-F#) + assert
   `manifest.Diagnostics = []`.

**Acceptance.** All three pass. If mitt produces a diagnostic, investigate — either
it is genuinely less clean than assumed (record the expected diagnostic) or a Phase-2
degrade fired where it shouldn't.

**Gotcha.** mitt leans on generics (`Emitter<Events>`, `Handler<T>`) and nullable
handler maps — exactly what Phase 0 must already cover. If extraction emits a
`method-axis-typar-erased` warning, that is a real signal about generics fidelity, not
a mitt problem; decide whether to accept the degrade or prioritise faithful generics.

---

## Phase 5 — real-package e2e (Codegen.Js, runs under Node)

**Goal.** The *semantic* oracle: a Vesper program using mitt emits JS that runs
against the **real** vendored mitt runtime and returns the right answer.

**Steps.**
1. In `Codegen.Js.Tests`, build a `TsManifestProvider` from the **committed**
   `mitt.manifest.json` (read the file — do NOT run the extractor here), stacked over
   the standard JS provider (mirror `MemberOverloadTests.calcProvider`).
2. Write a tiny Vesper `.fs` program: create an emitter, register a handler, emit an
   event, record that the handler fired (e.g. set a ref / print).
3. Emit to `.mjs` and run via `runJs`/`runNode`, with the real `mitt.mjs` copied into
   the emit output dir so the emitted `import` resolves to genuine mitt. Assert the
   handler observed the event.

**Acceptance.** The program exits 0 and prints/returns the expected value, proving the
extracted manifest is *behaviourally* correct end-to-end (not just self-consistent).

**Gotchas.** This is the layer where any residual Phase-0 nullable gap surfaces.
Codegen.Js tests must read only committed files (no extractor run, no
`ProjectReference` to `Extractor.Tests`). The import path must resolve to the real
runtime, not a stub — that is the whole point of this tier.

---

## After the phases

- Point the *same* resilient extractor at `@types/node` (module-shaped) and commit its
  diagnostics report as a coverage golden — the ranked codes become the roadmap to the
  DOM (which additionally needs the deferred ambient-global entry mode).
- Delete this doc; fold durable facts into module headers + the relevant
  `project_*` memory.
