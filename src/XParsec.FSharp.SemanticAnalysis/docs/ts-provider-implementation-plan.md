# TS provider — next milestone: consuming `@types/node`

**Status (2026-07-04).** The "faithful real-package consumption" tranche (Walls 1–5) and the
`dynamic`/heritage gaps (G1 numeric-covariance + structural width, G2 dynamic-escape warning,
G3 `retype`→`Vesper.Unsafe`, G5 external interface heritage) have all **LANDED** and now live in
the code itself (module headers + the named tests), per [[feedback_durable_knowledge_in_code]].
This doc has been **rescoped to the next breadth destination — `@types/node`** — and tracks only
what is NOT built for it. `Js.Dom` is the destination *after* node and gets its own doc when node
lands; the old §Breadth/§Faithful-later material is folded into the node work items below.

Every landed piece followed one design pattern, and all node work MUST keep to it: *real types via
JS intrinsics + operators/recognizers/erasing-nominals, NOT new SemType DU cases with magic unifier
behaviour.* No wall added a SemType case or changed `unify`/`subsumes`; node adds none either.

**Workflow.** Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the extractor
(`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) — needed for any change
under `src/Vesper.Ts.Extractor/`. Goldens regenerate via `-UpdateSnapshots` against
`test/Vesper.Ts.Extractor.Tests` (consumer tests NEVER run the extractor). Anchors below are symbol
names — confirm by reading before editing. Isolation-first is the rule
([[feedback_systematic_tests_over_whackamole]]): pin behaviour with a hand-built fixture before any
real-package golden regen.

---

## Already in place (context, not work)

The code + named tests are the canonical record; do not re-narrate here.

| Landed | Anchor |
|---|---|
| Walls 1–5 (undefined, dynamic, structural identity, iterables, module `let mutable`) | `UndefinedIdentityTests`, `DynamicTypeTests`, `StructuralNominalTests`, `IterableForInTests`, `PrimitiveExprTests` |
| G1 numeric covariance + record→interface structural width | `Codegen.Js.NumberCovariance`; `NumberFamilyTests`, `StructuralWidenTests`, `TypeArgNumberTests` |
| G2 `dynamic` implicit-escape warning | `DynamicEscape.fs`; `DynamicTypeTests` |
| G3 `retype` → non-AutoOpen `Vesper.Unsafe` | `ops-dynamic.js.fsi`/`.fs`; `DynamicTypeTests` |
| G5 external interface heritage (transitive upcast + inherited reads) | `ExternalHeritageTests` |
| W1 ambient-module extraction entry (`--ambient-modules`, one manifest per quoted module, per-symbol resilience, cross-module ref homing) | `Extractor.extractAmbientModules`; `TsInterop.enclosingQuotedModuleName`; `MapCtx.ModuleHome`; `ExportMap.mapExportResilient`; `specs/ambient-modules/` golden (`testExtractorMatchesGoldenAmbientModules`) |
| W2 CommonJS/Namespace import lowering + `node/* → Node.*` mount (mount/is-global split) | `ImportForm.CommonJs`/`Namespace`; `TsManifestTranslate.importFormOfShape`; `ExternalClassFlags.ImportForm`; `JsRuntime.addRef` + `JsStatement.ImportNamespace`; `TsGlobalHomes.mountFor`/`isGlobalHome`; `ImportFormLoweringTests` |
| W3 per-param optional-fill (trailing optionals → `OptionalDefaults`, omitted slot = `undefined`) + `overloadArgSigs` degrade-and-dedup (ErasedDistinction throw excised) | `TsManifestMembers.trailingOptionalCount`; `TsManifestTranslate.overloadArgSigs`; `OptionalParamTests` |
| W4 index signatures (`x.[k]` read/write → `GetIndex`/`SetIndex` bracket) + optional graduation (`SymbolFlags.Optional` → `Member.Optional` / `T \| undefined`) + value-level `undefined` (retires the `optionalDefaultNode` unit hack) | `Codegen.Js.GetIndex`/`SetIndex`; `Schema … index` facet; `TypeMap.mapIndexInfo`; `TryLookupIndexSignature`; `inferIndexedLookup` recognizer; `Vesper.undefined`/`Inline.nullaryIntrinsicValueBody`/`BuiltinTypes.tyUndefined`; `IndexSignatureTests`, `indexsig` golden |

**Generic machinery node RIDES (already built, reused verbatim):** the `--package` module-entry
extraction (`extractPackage`); the refs table (identity-only homed `FTClass`, the ECMA-335
`TypeRef`/`AssemblyRef` analog — never the foreign shape); member-overload expansion (per-`argSig`
keys, `expandMethod`/`expandCtor`); the burndown-contract pattern + the closed `DiagCode` taxonomy +
the `SymbolWalkFailed` per-symbol resilience backstop; the pure-record→erasing-nominal structural
machinery (Wall 3); and the faithful `Fun`/`Tuple`/literal/`keyof`/indexed-access arms.

### Inert residue (not node work)

**G4 — es2015 `Error`-subclass ctor residue.** Six `Error`-subclass ctors are return-type-divergent;
the ctor dedupe keeps the first and nothing constructs them, so it is harmless. Revisit only if an
`Error` subclass is ever constructed from Vesper. Node does not force this.

---

## The milestone: what `@types/node` IS

`@types/node` is **module-based**, not a global script and not a single-`export *` package:

- **Many quoted ambient modules** — `declare module "fs" { … }`, `declare module "path"`,
  `declare module "events"`, `declare module "node:fs"`, … dozens of them.
- **A small true-globals subset** — `Buffer`, `process`, `global`, and the `NodeJS` namespace
  (declaration-merged across files).
- **CommonJS-shaped exports** — many modules are `export =` / `import fs = require("fs")`.
- **Heavy overloading + optional/callback params** — `fs.readFile(path, options?, callback)` and
  siblings; `EventEmitter.on`/`emit` overload storms.
- **Index signatures and mapped types** — `process.env` (`{ [k: string]: string | undefined }`),
  `NodeJS.Dict`, `Readonly`/`Partial`/`Record`.

This shape — *a package that declares many quoted modules plus a few globals* — drives every work
item below. It is why node needs a new extraction entry, not just a bigger `--package` run.

---

## Packaging & extraction entry — **DECIDED: per-quoted-module manifests (A)**

The **#1 blocker**: nothing enumerates quoted ambient modules. `checker.getAmbientModules()` exists
in the vendored binding (`vendor/TypeScript.fs`) but has **zero call sites**; `extractPackage`
resolves ONE specifier's module symbol and `extractModuleExports` walks `getExportsOfModule`, which
does not descend into each `declare module "…"` body. Both extraction modes today assume a *single*
module entry or a global script.

**Decision (2026-07-04): (A) per-quoted-module manifests.** The new extractor entry enumerates
`getAmbientModules()` and emits **one manifest per module** — home e.g. `node/fs` → mounted namespace
`Node.Fs`. This fits the existing refs-home model (each module is a home, like `es2015` is one home)
and lets the provider stay lazy *per module* (W6). The cost accepted: many manifest artifacts, and a
cross-module ref-home convention for intra-node references (`fs` → `events.EventEmitter` homes to
`node/events`). The rejected alternative — one aggregate `Namespace`-keyed manifest — is simpler to
wire but forces the eager provider (W6) to materialise the *entire* node surface on construction,
which node scale is exactly what makes painful. W1 is built to shape (A); W2–W5, W7 are
shape-independent.

**Follow-ups (A) pulls in, resolved as W1 lands:**
- **Cross-module ref home naming.** `classifyHome` (`Diagnostics.fs`) returns the nearest
  `package.json` `name` for an external decl — for `@types/node` that is one string (`@types/node`),
  NOT the per-module `node/fs` home (A) wants. W1 must home an intra-`@types/node` ref by its
  declaring *module specifier*, not the package name, so `fs`'s reference to `EventEmitter` resolves
  to the `node/events` manifest.
- **Home→namespace mount.** `globalLibHomes` (`TsManifestTypes.fs`) maps only `es2015 → Js`; W1 adds
  the `node/* → Node.*` mount convention (or a general `home → namespace` rule the node homes slot
  into).

---

## Work items (what's NOT built)

Ordered by dependency. Each is grounded in a read anchor; each keeps to the wall design pattern.

### W1 — Ambient-module extraction entry *(the blocker)* — **LANDED (extraction), mount deferred to W2**

**Landed.** The extractor entry (`--ambient-modules <packageName> <outDir> <dts…>` in `Program.fs`,
writing ONE manifest per module into `outDir`) enumerates `checker.getAmbientModules()` (filtered to
fixture-declared modules, mirroring the globals path's `isFixtureDeclared`) and, for each quoted
module, emits its own manifest homed `<packageName>/<module>` (e.g. `node/fs`) via the shared
`extractModuleExports`. The two extraction-side (A) follow-ups landed with it: the cross-module ref
homes by DECLARING module specifier (`MapCtx.ModuleHome` → `classifyHome` consults it first;
`enclosingQuotedModuleName` walks the parent chain to the quoted `declare module`), and the
per-symbol resilience wrapper now guards the module path (`ExportMap.mapExportResilient`,
`SymbolWalkFailed` on throw — `extractModuleExports` was a bare `List.choose (mapExport …)`; now
resilient for `extractFile`/`extractPackage` too). Node's true globals (`Buffer`, `process`,
`NodeJS`) still ride the existing globals path (sibling run). Golden: the two-quoted-module
`specs/ambient-modules/` fixture asserts both modules enumerate into their own manifest with the
cross-module ref homed to `node/a`.

**Deferred to W2 — the `node/* → Node.*` mount.** `globalLibHomes` (`TsManifestTypes.fs`) is NOT
just a namespace mount: membership there ALSO stamps `isGlobalPack = true` → emit with **no import**
(`JsRuntime.addRef`, `TsManifestProvider.mountPrefix`). Node modules REQUIRE `import fs from "fs"`,
so wiring `node/*` into `globalLibHomes` as-is would give node exports the WRONG import-free
lowering. The namespace mount is coupled to import semantics, which W2 (CommonJS/Namespace import
lowering) owns — and no manifest is provider-CONSUMED until W2 anyway (W1's fixture is
extraction-only). So the mount lands in W2, where the global-pack/no-import bit is split from the
namespace mount rather than conflated. W1 is otherwise complete.

### W2 — CommonJS / Namespace import **lowering** *(hard blocker)* — **LANDED**

**Landed.** `ImportForm` gained `CommonJs` + `Namespace` arms; the ONE
`TsManifestTranslate.importFormOfShape` maps `Schema.ImportShape` faithfully (no more
everything-but-`Default` → `Named` collapse), shared by `stampValueSymbol` and the grouping
builder. The overloaded-free-function grouping type no longer **throws** on a non-`Named` import:
`buildOverloadGroupingTypes` computes the group's uniform import form (throws only on a mixed
group — a manifest anomaly) and stamps it on the new `ExternalClassFlags.ImportForm`, which
`JsExternalMembers.erasedGroupingRef` reads to lower the erase — `import { f }` (Named), `import f`
(Default/CommonJs — `export =` binds `module.exports` to a DEFAULT import under esModuleInterop),
or `import * as ns; ns.f` (Namespace, via the new `JsStatement.ImportNamespace` + `ImportEntry.Namespace`
slot). `CommonJs` shares the `Default` `addRef` path but stays a distinct arm so the brand survives
to a later CJS target. **The `node/* → Node.*` mount landed too:** `TsGlobalHomes` split the
former `globalLibHomes` `Map` (which conflated namespace-mount with import-suppression) into
`mountFor` (home → mount namespace — `es2015 → Js`, `node/<mod> → Node.<Mod>`) and `isGlobalHome`
(import-free set — es2015 only), so a node module MOUNTS under `Node.Fs` while STILL emitting a
real import (`Global = false`). Fixtures: `ImportFormLoweringTests` — an overloaded `export =`
module (default import + bare-erased call + no grouping throw), an overloaded Namespace module
(`import * as` + member call), and a `node/fs`-homed manifest (mounts under `Node.Fs`, emits a real
import — the mount/is-global split).

**Residue (deferred, not W2):** a node module's import SPECIFIER still flows through the
`JsRuntimeModule.FileName` seam (`import … from "./fs.mjs"`), not the bare node specifier
(`"fs"`/`"node:fs"`). Bare-specifier emission (and the runtime-asset-vs-external-package
distinction it needs) lands with real `@types/node` consumption (W7), where a node manifest carries
its own import specifier instead of a synthetic `.mjs`. The `Namespace` arm is also not yet
extractor-PRODUCIBLE for a value export (`importShapeOf` routes a module-flagged symbol to an
`Export.Namespace`, never a `Namespace`-branded `Function`/`Variable`); its lowering is pinned by a
hand-built consumer fixture, faithful to whatever shape a manifest declares.

### W3 — Per-parameter optional/rest + `OptionalDefaults` — **LANDED**

**Landed.** The member path (`expandMethod`) now carries the extractor's per-param `Optional`
flag: a trailing run of optional parameters becomes the member's `OptionalDefaults`
(`TsManifestMembers.trailingOptionalCount` → `List.replicate n TConstValue.Unit`), so the SHARED
optional-fill seam (`InferExternalCall.tryFillOptionalCall` admits the under-applied arity; the
single-pick `resolveFieldStep` and `commitExternalOverload` both forward `OptionalDefaults` into
`ExternalAccess`; `FreezeExpr.optionalDefaultNode` synthesises each omitted slot) permits
`api.readFile(path, cb)` and `api.greet("x")`. Each omitted slot is `TConstValue.Unit` — its JS
VALUE repr is `undefined` (the correct absence value for an omitted TS optional). The `undefined`
TYPE is a distinct identity from `unit` (`prim-types-undefined.js.fs`); the fill exploits only the
shared `unit`→`undefined` VALUE repr, and the fill node is never re-unified against the parameter
type. The `ErasedDistinction` **throw is gone**: `overloadArgSigs` now DEGRADE-AND-DEDUPS
(keep-first) for both the method and ctor sites — node's `number`-family / config-object overload
storms collapse to one `argSig` pervasively, so the former abort was untenable; the collapsed
`OverloadCollision` DU + `label` are excised. Fixture: `OptionalParamTests` — `greet(name, title?)`
omit/supply/emit-`undefined`, `readFile(path, cb, opts?)` callback + structural-width-on-supplied +
omit, and a doubly-declared `log(x: number)` proving dedup-not-throw at provider construction.

**Deferred residue (not W3):** a REST parameter (`...args: T[]`) is carried through the schema/DSL
(`restParam'`) but NOT made omittable — an omitted rest is ZERO args, which a single-`undefined`
fill would wrongly materialise as one element (`trailingOptionalCount` excludes a trailing rest, so
it stays a required array param). Variadic rest lowering (spread-emit / multi-arg application) lands
when a real node signature forces it. Constructors keep `OptionalDefaults = []` (ctor optional-fill
is a separate `InferCtor` path, unexercised by the fixture).

### W4 — Index signatures + optional graduation — **LANDED**

**Landed.** Index signatures (`x.[k]` read / `x.[k] <- v` write) and the optional graduation are consumed
end to end and live in the code + `IndexSignatureTests`. Anchors: `Codegen.Js.GetIndex`/`SetIndex`
(`ops-platform.js.fs`, the `$0[$1]` bracket template); `Schema.TypeRef.Structural`/`Export.Interface`/
`Class`'s `index: (TypeRef * TypeRef) list` facet (codec omit-when-empty); `TypeMap.mapIndexInfo` +
ungated `carriesFaithfullyAsFields`; `IExternalSymbolProvider.TryLookupIndexSignature` (FrozenType
templates, realised per use site); `inferIndexedLookup`'s index-sig recognizer + Freeze's
`GetIndex`/`SetIndex` routing; a fieldless index-only shape (bare `Record<K,V>`) freezes to a nominal so
its index is reachable (only a truly-empty shape stays `FTUnknown`). Optionality reads from
`SymbolFlags.Optional` — a named member fills `Member.Optional`, an anonymous structural field carries
`T | undefined`. The value-level `undefined` (`Vesper.undefined`, a zero-operand intrinsic the JS backend
inlines via `Inline.nullaryIntrinsicValueBody`) landed too, retiring the `optionalDefaultNode`
`unit → undefined` repr hack (now an honest `undefined`-typed fill via `BuiltinTypes.tyUndefined`). The
callable-object-with-props graduation is NOT here — split to **W8** (no node type forces it).

### W5 — `namespace` / declaration merging

Node's `NodeJS` namespace merges pervasively (`namespace NodeJS {}` + `interface NodeJS.*`). Today a
`Module`-flagged symbol with a dominant type/value flag drops the namespace half with
`MergedNamespaceDropped` (`ExportMap.fs`). Land the "type carrying a static namespace" seam so both
halves survive.

### W6 — Step 5: lazy per-symbol provider index *(perf/architecture; no correctness dependency)*

`buildContractFor` builds `types`/`funcs` via eager `Map.ofList` in the ctor, plus whole-manifest
pre-scans (`buildStructuralTypes`, `buildOverloadGroupingTypes`, `buildCtx`'s full mint pass). At
node scale this freezes the entire surface on first construction. Flip to a lazy per-query index —
**keeping identity-minting eager** (`buildCtx` needs it for forward references) and deferring only
member/shape expansion (`toTypeShape`, the structural scan). Independent of correctness; node scale
is where it earns its keep. Do NOT entangle with W1–W5.

### W7 — The node burndown contract

Once W1 lands: run the ambient-module extraction over a vendored `@types/node` fixture, commit it as
a golden, and add a `nodeBurndownContract` — a committed `(DiagCode, count)` ranking asserted to
equal the manifest's diagnostics, drift-either-way fails (the scoreboard for shrinking residue),
exactly like `es2015BurndownContract`. Ranked over the same closed `DiagCode` vocabulary; W4/W5
landings show up as the counts fall.

### W8 — Callable-object-with-props (`Vesper.Fun`-implementers) — *deferred, build-when-a-type-forces-it*

Split out of W4 (2026-07-05): a TS callable object `{ (x: number): string; prop: boolean }` → an
erasing nominal carrying `Fun<number,string>` in its heritage/interface set (the call signature) plus
the data props as Property members. The **typing** side rides mostly-existing machinery: `Fun<a,b>` /
`Fun<a,b,c>` is already the interface function values satisfy, with a value-struct `Invoke` sized by
arity (`InferApp.fs` `recordFunArityVerdicts` + the `:> Fun<a,b>` bound), and external heritage +
Fun-coercion landed with G5. The intent: called from Vesper via `.Invoke`, but passable into any
function-accepting parameter slot with a matching signature (the object and a lambda are
interchangeable to a `Fun`-bounded slot).

It is NOT just data-carrying — it has a lowering wrinkle exactly parallel to W4's `$0[$1]` decision:
- **`.Invoke` on an external callable object must lower to a DIRECT JS call `$0($1…)`**, not a
  `.Invoke(...)` method call — a TS callable is invoked as `obj(x)`, and a JS object has no `.Invoke`.
  A new lowering, sibling to `GetIndex`.
- **Fun-coercion for an external nominal** must be verified: lambdas coerce into a `Fun`-bounded slot;
  an external Fun-implementing nominal flowing into an arrow slot needs confirming (it should ride the
  G5 upcast + the arg-position structural width, but is unexercised).
- **Overloaded call signatures** compound it — `isFunctionType` already bails on >1 call sig, so a
  callable object with overloaded call sigs needs the multi-signature story first.

Schema slot: the call signature(s) carried on the erasing nominal (a `Structural`/heritage contract
bump — Fable + goldens). **No `@types/node` type forces this** (EventEmitter/process/timers/streams
are not callable-with-props), so it stays deferred with its own isolation fixture, landing when a real
consumed type (node or `Js.Dom`) produces a callable object.

---

## Sequencing + isolation fixtures

Isolation-first ([[feedback_systematic_tests_over_whackamole]]): pin each capability with a
hand-built fixture BEFORE the real-package regen.

1. **W1** — **DONE.** `specs/ambient-modules/two-modules.d.ts` (`declare module "a" { … } declare
   module "b" { … }`, with a cross-module `b → a` ref); asserts both modules enumerate into their own
   manifest and the cross-module ref homes to `node/a`. (The `SymbolWalkFailed` backstop is in the
   code path — `mapExportResilient` — but not exercised by a fixture: engineering a deterministic
   throw from a `.d.ts` symbol is impractical, so it stays a reviewed-in-place insurance like its
   globals-path sibling `mapGlobalSymbolResilient`.)
2. **W2** — **DONE.** `ImportFormLoweringTests`: an overloaded `export =` module (CommonJS
   default import + no grouping throw), an overloaded Namespace module (`import * as`), and a
   `node/fs`-homed manifest (mounts under `Node.Fs`, still a real import). Specifier-shape residue
   (bare `"fs"` vs `"./fs.mjs"`) deferred to W7.
3. **W3** — **DONE.** `OptionalParamTests`: `greet(name, title?)` (optional-omit call + supply +
   emit-`undefined`), `readFile(path, cb, opts?)` (callback + config-object width on the supplied
   optional + omit), and a doubly-declared `log(x: number)` (same-`argSig` overloads dedup rather
   than throw). Rest-param variadic lowering deferred (a trailing rest stays a required array param).
4. **W4** — **DONE.** `IndexSignatureTests` (index read/write/emit `obj[k]`, `string | undefined` value,
   fieldless `Record<K,V>`, optional-field graduation) + the `indexsig` extractor golden. **W5/W8** —
   targeted fixtures per graduation as each bites a real node type.
5. Only then: vendor real `@types/node`, regen goldens, commit **W7** burndown.

Node-specific shapes need NO special code: `Buffer`/`EventEmitter`/typed arrays ride the generic
refs path (typed arrays are NOT on the intrinsic-overlap skip-list, so they home normally);
node-style `(err, data) => void` callbacks map to a faithful `Fun`. Their only friction is the
enclosing overload storm (W2/W3), not the shapes themselves.

---

## Retained invariants / guardrails (binding for all node work)

- **The design pattern:** no new SemType DU case, no `unify`/`subsumes` edge — real intrinsics +
  operators/recognizers/erasing-nominals ([[feedback_dynamic_intrinsics_over_du_cases]]).
- **Extractor carries, front end evaluates, backend homes** ([[feedback_freeze_no_backend_knowledge]]);
  **codegen-js owns assignability/intrinsic repr** ([[feedback_codegen_js_owns_assignability]]).
- **Refs are identity-only** — home + kind + arity, NEVER the foreign shape/members (the staleness
  trap). Member access resolves through the stacked home manifest or fails with a "package not
  referenced" diagnostic.
- **The arity law** ([[project_arity_overloaded_type_names]]): a homed key's simple name is
  `arityName name Arity`.
- Structural width stays CONFINED to the foreign-call arg position, gated on the provider's
  `IsInterface` (carried from G1).

---

## When to delete this doc

Delete when `@types/node` lands and its burndown is committed; fold durable facts into module
headers + [[project_js_ref_pack]] / `reference_*` memories ([[feedback_plan_docs_ephemeral]]).
`Js.Dom` then gets its own doc (ambient-global entry at 10× scale, deeply `extends`-chained
event/element hierarchy consuming the landed G1 + G5). The `@types/node` decisions doc — the *why* —
lives in [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md).

---

## Orientation — files and symbols (node-relevant)

- **Extraction entry (W1/W2):** `src/Vesper.Ts.Extractor/Extractor.fs` (`extractPackage`,
  `extractGlobalsCore`, `mapGlobalSymbolResilient` — the resilience wrapper to extend to the module
  path); `ExportMap.fs` (`extractModuleExports`, `mapExport`, `importShapeOf`, the
  `MergedNamespaceDropped` arm for W5); `vendor/TypeScript.fs` `getAmbientModules` (the zero-call-site
  W1 seam); `Program.fs` (CLI dispatch — add `--ambient-modules`). Rebuild via the one sanctioned
  Fable command (workflow header).
- **Degradation / faithful-later (W4):** `TypeMap.fs` (`carriesFaithfullyAsFields`, `isFunctionType`,
  `isPureRecordObject`, the `getIndexInfosOfType` gate); `Diagnostics.fs` (`emitWarning`,
  `recordForeignRef`/`classifyHome`/`classifyKind`/`refArity`, `drainDiagnostics`); `Schema.fs`
  (`DiagCode`, `Export`, `ImportShape`, `RefEntry`, `Param.Optional`/`Rest`).
- **Provider consumption (W2/W3/W6):** `TsManifestProvider.fs` (`buildContractFor` — the eager
  `Map.ofList` index + the import-form map, `Default` vs `Named`); `TsManifestMembers.fs`
  (`expandMethod`/`expandCtor`, `buildOverloadGroupingTypes` — the CommonJS throw-gate,
  `buildStructuralTypes`); `TsManifestTypes.fs` (`toFrozen` nominal/refs resolution, `paramsFrozen` —
  drops optional/rest, `overloadArgSigs` `ErasedDistinction`, `globalLibHomes`).
- **Optional-fill seam (W3):** `Passes/Unification/Engine.fs` (`unifyArgCoerce`/`tryCoerceUpcast`);
  `InferExternalCall.fs` (`commitExternalOverload`).
- **Tests as patterns:** `test/Vesper.Ts.Extractor.Tests` (`es2015BurndownContract` /
  `mittDiagnosticsContract` in `Tests.fs`; `TestHelpers.testExtractorMatchesGoldenLibGlobals`;
  `UPDATE_SNAPSHOTS`); `test/XParsec.FSharp.Codegen.Js.Tests` (`StructuralWidenTests` — the G1
  config-object fixture pattern W3 reuses); `test/ts-fixtures/{mitt,es2015}/`.

## Relevant memories

[[project_js_ref_pack]] (canonical shipped state),
[[reference_js_external_instance_member_walls]] (R1–R4a baseline + closed walls),
[[feedback_freeze_no_backend_knowledge]] (extractor carries, front end evaluates, backend homes),
[[feedback_codegen_js_owns_assignability]] (codegen-js owns assignability/intrinsic repr),
[[feedback_dynamic_intrinsics_over_du_cases]] (real intrinsics over new SemType cases),
[[feedback_systematic_tests_over_whackamole]] (isolation fixture before wiring),
[[feedback_durable_knowledge_in_code]] (landed work lives in code, not this doc),
[[project_arity_overloaded_type_names]] (the arity law),
[[feedback_plan_docs_ephemeral]] (delete this doc when node lands),
[[feedback_user_commits]] (user reviews and commits).
