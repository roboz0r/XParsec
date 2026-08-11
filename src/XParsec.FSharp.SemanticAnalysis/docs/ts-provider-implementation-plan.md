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

**Residue (deferred, not W2):** a node module's import SPECIFIER still travels through the
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
`ExternalAccess`; `ElaborateExpr.optionalDefaultNode` synthesises each omitted slot) permits
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

**Refactor surfaced post-landing (2026-07-05): W4's index-sig mechanism is a SECOND indexer
path parallel to the canonical `Item`/`get_Item` one — retire it under W9.** W4 shipped a
working-but-non-canonical shape: `x.[k]` desugars through F#'s `Item` indexer
(`get_Item`/`set_Item`) canonically, and the read side ALREADY resolves that via
`resolveExternalIndexer clsQual clsArgs "get_Item"`. W4 bolted a parallel seam
(`GetIndex`/`SetIndex` intrinsics + `TryLookupIndexSignature` + `tryIndexSignature`) alongside
it rather than synthesising `Item` members. Not node-blocking; see **W9**.

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

It is NOT just data-carrying — it has a lowering wrinkle, its OWN mechanism (distinct from W9):
- **`.Invoke` on an external callable object must lower to a DIRECT JS call `$0($1…)`**, not a
  `.Invoke(...)` method call — a TS callable is invoked as `obj(x)`, and a JS object has no `.Invoke`.
  This is a call-site LOWERING DECISION on the compiler-known `Fun` INTERFACE (whose `Invoke` is an
  `abstract` member with no body) — a `MemberLowering`-style axis, NOT W9's concrete inline-bodied
  intrinsic member. The two only resemble each other in emitting a non-dotted shape; the mechanisms
  differ (interface-call lowering vs `(# … #)` body splice) and neither depends on the other.
- **Fun-coercion for an external nominal** must be verified: lambdas coerce into a `Fun`-bounded slot;
  an external Fun-implementing nominal passed to an arrow slot needs confirming (it should ride the
  G5 upcast + the arg-position structural width, but is unexercised).
- **Overloaded call signatures** compound it — `isFunctionType` already bails on >1 call sig, so a
  callable object with overloaded call sigs needs the multi-signature story first.

Schema slot: the call signature(s) carried on the erasing nominal (a `Structural`/heritage contract
bump — Fable + goldens). **No `@types/node` type forces this** (EventEmitter/process/timers/streams
are not callable-with-props), so it stays deferred with its own isolation fixture, landing when a real
consumed type (node or `Js.Dom`) produces a callable object.

### W9 — Concrete inline-bodied members on intrinsic/declared types; indexers as its first consumer — *foundational; supersedes W4's index-sig facet*

---
#### ▶ STATUS / RESUME HERE (2026-07-05)

**Stage 1 (the general primitive) is COMPLETE and committed** — a concrete `(# … #)`-bodied member on
an intrinsic/`extern` type is now a real, reusable capability, end-to-end (parse → elaborate → capture
→ harvest → splice → emit). Commits on branch `codegen-js`:
- `52bfac38` **1a** — parse `type X = (# … #) with member …` (`TypeDefn.Abbrev` gained an `extensions` slot).
- `e8e099cb` **1b** — impl-side type-augmentation ELABORATION (the four drop-site arms + the shared
  `tryNonClassMemberHost`/`…Decl` seams + `IntrinsicAbbrevInfo` host, `SideTables.fs`) reusing the
  union/record `this`-first host-member path; CONSUMER capture (VesperLib invariant lifted → Class +
  `CapabilityFace`); member-keyed harvest/store (`SymbolProviders.harvestMemberBody` + `buildContractCached`).
- `1f7222bb` **1c** — the `TExpr.ExternalMember` splice arm (`InlineExpansion.fs`; object argument prepended to
  the arguments, splice-vs-call fork on `TryLookupInlineBody`) + end-to-end emit fixture (`fixtures/widget/`).
- `d2d890bf` **1d** — key the harvest store by `SymbolKeyOps.qualifiedName tdecl.Key` (namespaced types work).
- `0fbec4d0` **2a** — array `arr.[i]` READ via a `get_Item` member on `'T[]` (JS), byte-identical, with a
  white-box anti-masking assertion (`ArrayIndexMemberTests`). New files `src/Vesper.Core/array-index.js.fsi`
  + `array-index-body.js.fs`, wired into `manifest.toml`.

**Decisions locked this milestone (do NOT re-litigate):**
- `.fsi` member sigs use STANDARD-F# spelling (already parse); only the `.fs` abbrev host was new (1a).
- Only the INLINE intrinsic-abbrev augmentation is in scope. STANDALONE `type X with member …`
  (`TypeDefn.TypeExtension`) and EXTRINSIC/cross-module extensions are DEFERRED (the core is built to
  accept them later; not needed here).
- GUARDRAIL: only an ILIntrinsic-RHS abbrev may carry members (transparent alias rejected).
- Array STAYS a bare `TyConst("[]")` — NOT promoted to `TyClass`. `EngineCore.fs:500` /
  `isStructuralConstructorName` stay closed. Member lookup keys DIRECTLY on the array's contract name.
- **Qualifying intrinsic identities (bare `TyConst("[]")`/`("string")` → namespaced) LANDED as its
  own milestone** — intrinsics carry qualified `Vesper.*` `SymbolKey`s; the successor work
  (contract-sourced resolution, shadow-set deletion) continues in
  [`contract-sourced-intrinsic-identity-plan.md`](contract-sourced-intrinsic-identity-plan.md).
  W9 landed indexers localized on top of the then-bare identities.
- **Array contract-name reality (corrects §3 below):** the key-agreement string is NOT `"[]``1"`.
  The name is the bare `[]`: the escape is stripped at the token → name read, and a structural
  constructor takes arity 0 however it is minted, so both the consumer contract AND the lifted
  store key it as `[]`. The `InferRecordAccess` array branch looks it up under
  `RuntimeNames.arrayMemberHostKey`, which differs only in being GLOBAL-namespace: the indexer
  is declared there so it cannot displace the `Vesper` shape carrying the array's capabilities.

**REMAINING (pick up in order):**
- **2b** — array WRITE (`arr.[i] <- v` → `set_Item`) via the new `inferIndexedSet` from
  `inferAssignment` (`InferControlFlow.fs:811`, no write resolution exists today) + `arr.Length` via a
  `get_Length` member (retire the three `.Length` special-cases at `InferRecordAccess.fs:440`,
  `ElaborateExpr.fs:556`, `Resolve.fs:816`). Same escaped-name + byte-identical + white-box pattern as 2a.
- **2c** — string `s.[i]` via a `get_Item`/`get_Chars` member (string keys CLEANLY as `"string"` — a
  simpler path than array's escaped name; `GetString`'s `$0[$1]` body migrates). Byte-identical
  (`ArrayLoopTests` string-index, `IndexSignatureTests`).
- **2d** — CLR target: migrate the CLR `GetArray`/`SetArray`/`GetArrayLength`/`GetString` bodies into the
  same members so CLR emit stays byte-identical too (2a-2c are JS-only so far; the `.fsi` contract is
  shared, the `.fs` bodies are per-target).
- **Stage 3** — external CLR `get_Item` (real call, no inline body — proves the splice-vs-call fork on ONE
  path) + TS index-sig (provider-synthesised `$0[$1]` member body). THEN the big DELETE (see §6): the
  `GetArray`/`SetArray`/`GetArrayLength`/`GetString` free functions + their Freeze emit sites, the
  `getArrayIndex`/`stringOrArrayIndex` fallbacks, `tryIndexSignature`/`TryLookupIndexSignature`, the
  `GetIndex`/`SetIndex` intrinsics. The fallbacks are STILL PRESENT through Stage 2 (so a key mismatch
  would emit byte-identically) — that is why every Stage-2 slice needs the WHITE-BOX assertion that
  resolution took the member path (an `ExternalAccess` `get_Item`/`set_Item` entry), not the fallback.

Blast-radius recon confirmed de-specializing array is LOCALIZED (~a dozen sites) as long as array stays a
`TyConst`; the systemic part (qualified identities, array-as-real-`TyClass`, literal JS repr) is the
separate deferred milestone. The §Stage-work and §Sequencing below are the ORIGINAL plan; the STATUS block
above is authoritative where they conflict (esp. the `"[]``1"` key claim in §3 — it is `` ``[]`` `` in reality).

---

**Direction (user, 2026-07-05): (a-literal). The enabling primitive is GENERAL — an intrinsic /
`extern` type MAY carry concrete members with `(# … #)` inline bodies — NOT an indexer feature.** It
is a first-class platform-binding strategy for any type the automatic boundVar (`TsManifestProvider`)
doesn't cover or can't express. Indexers (`array`/`string` `get_Item`/`set_Item`/`Length`) are its
first consumer. (Distinct from W8: `Vesper.Fun` is a compiler-known INTERFACE with an `abstract Invoke`
and a call-site lowering decision — not a concrete inline-bodied intrinsic member. The two are
independent; W9 does not subsume it and need not precede it.) Significant effort, but it DELETES more
than it adds — the object-argument-classification ladder, the parallel index-sig seam, the
`.Length` special-cases, and the `GetArray`/`Get*` free-function intrinsics all collapse into one
member-resolution + one member-inline-splice path. Keeps [[feedback_dynamic_intrinsics_over_du_cases]]
(no SemType case) and [[feedback_codegen_js_owns_assignability]] (splice-vs-call is backend lowering).

**The invariant at `VesperLib.fs:1354-1374` is LIFTED — it was "not yet", not "never".** Its comment
fences "a concrete member surface on an intrinsic primitive" as a DURABLE rejection ("the `(# … #)`
repr is for structurally inert leaves"); the decision overrules that — a concrete member surface is a
wanted general capability. The admitted-exception arm right above it (`:1334-1353`) already shows the
mechanism: a `type X = extern with …` registers as a `Class` (which HAS member slots) via
`extractBodiedClassLike` AND attaches the `(# … #)` repr as a `CapabilityFace`, so the canonical
primitive identity survives for codegen / `subsumes`. The concrete-member case takes the SAME path
(register Class + repr) instead of `registerIntrinsic ()` + diagnostic. (`ExternalTypeShape.Intrinsic`
carries no member slots — the Class-plus-repr shape is precisely how the capability arm already solved that.)

**Member inlining REUSES function inlining (user steer; the load-bearing mechanism).** A concrete
accessor `member _.Item with get (i) = (# "ldelem" … #)` IS the inline function
`get_Item (this) (i) = (# "ldelem" this i #)` — `this` prepended as the first inline param. So:
- Capture mints an `InlineBody` from each concrete accessor body, keyed by the MEMBER (`arrayName 1` +
  `get_Item`/`set_Item`/`get_Length`), `this`-first — the member-sourced twin of `collectInlineBodies`'
  `let inline` case (`SymbolProviders.fs:46-104`).
- `InlineExpansion` gains ONE new applied-function arm: a `TExpr.ExternalMember` whose member carries a
  registered inline body splices via the SAME `reduceApplication` / `ParamAttrs` / `expandExternalAt`
  path the `TExpr.External` arm uses (`InlineExpansion.fs:589`). A member with NO inline body (a real
  CLR `get_Item` runtime method) stays a real call — so the splice-vs-call fork is simply "does this
  member carry an inline body", uniform for every object argument.
- Both `x.[i]` (indexer sugar) and dotted `x.get_Item(2)` resolve the SAME member and hit the SAME
  splice — so `get_Item` stays nameable (the FSI-confirmed F# semantics) with no divergence.

**Stage work (grounded in the four-stage pipeline map):**
1. **Parser** (`src/XParsec.FSharp/`) — **LANDED (1a).** Only ONE host needed: the `.fs`
   `= (# … #) with member …` augmentation — `TypeDefn.Abbrev` gained an `extensions` slot (mirroring
   `Record`/`Union`) and `parseAbbrevOrImplicitClass` now parses the trailing `with` block
   (`TypeDefnParsing.fs:1331`; fixture `399_extern_member_intrinsic.fs`). The `.fsi` side needed NO
   change — DECIDED standard-F# spelling (`member Poke: int -> int`, `member Item: int -> 'T with get, set`)
   already parses inside `extern with` (`sig_16_extern_with.fsi`). The plan's `src/XParsec.FSharp.Parser/`
   paths were WRONG — the parser is `src/XParsec.FSharp/`.
2. **Capture** — two sub-stages, because the impl-side member BODIES are not reachable without a
   front-end elaboration step the plan originally missed:
   - **2a — impl-side type-augmentation elaboration** (`Passes/Desugar.fs`, `Passes/NameResolution/`
     `TypeRegistration.fs`+`MemberRegistration.fs`, `Elaborate.fs`). A `TypeDefn.Abbrev` carrying
     `extensions` elaborates its members through the SAME `this`-first host-member path records/unions
     already use for their inline `with member` blocks (`Elaborate.elaborateHostMembers` →
     `translateClassMember` → `tryClassType`; body typing via `Unification.fillTypeMembers`). Today it is
     DROPPED at four sites — `Desugar.fs:182`, `MemberRegistration.fs:925`, `Elaborate.fs:1612`
     (no `Abbrev` classify arm), `TypeRegistration.fs:518` (binds only `typeName`/`typ`, ignores
     `extensions`). GUARDRAIL: only an **ILIntrinsic-RHS** abbrev may carry members (a transparent alias
     `type T = int with member` is rejected — F# rejects it too: `tcTypeAbbreviationsMayNotHaveMembers`).
     IDENTITY: `X` keeps its `TyConst` identity (stays in `IntrinsicReprTypes`, resolves to `TyConst name`
     at use sites); members elaborate `this`-first with `ThisTy` = `X`'s intrinsic type; the produced
     `TDecl.Type` is consumed only by the harvest (2b), never emitted. This is the general
     inline-augmentation mechanism; STANDALONE `type X with member …` (`TypeDefn.TypeExtension`, already
     parsed, still dropped) and EXTRINSIC/cross-module extensions (F#'s `eIndexedExtensionMembers` table)
     are DEFERRED (build the core so they slot in later; not needed for W9). F# scout confirmed our
     `this`-first `TTypeMember` model already matches F#'s member-`Val` + `tcaug_adhoc` attachment — no
     new representation to copy.
   - **2b — consumer capture + member-keyed harvest/store.** Consumer side: lift the
     `VesperLib.fs:1354-1374` invariant, routing a concrete-member intrinsic through the
     Class + `CapabilityFace` arm (so `TryLookupMember` resolves + mints the finalized member `Key`).
     Impl side: `collectInlineBodies` (`SymbolProviders.fs`) gains a `TDecl.Type` → Class-member arm
     (`harvestMemberBody`) minting the `this`-first inline `TDecl.Let`; `buildContractCached` stores it
     under the FINALIZED `TryLookupMember(...).Key` (never a hand-rolled `MemberKey` — a method's argSig
     is rewritten from frozen params at `VesperLib.fs:438-446`, so keying off the resolved member is the
     only agreement-safe choice). Isolation: `TryLookupInlineBody(mem.Key).IsSome` from a real loaded
     `widget` package.
3. **Resolution** (`InferRecordAccess.fs`) — `inferIndexedLookup` resolves `get_Item`/`set_Item` via
   `TryLookupMember` on the object argument INCLUDING the array/string intrinsic; add the write mirror
   `inferIndexedSet` (`set_Item`; no `set_Item` resolution exists in source today — external CLR indexer
   writes land as a free byproduct); element type pinned from the member signature. **Load-bearing
   question — RESOLVED (spike, 2026-07-05): ROUTABLE with a local change, no gate relaxation.**
   `EngineCore.fs:500`'s `isStructuralConstructorName → ValueNone` (in `externalSurfaceKeys`, the
   object-argument→provider-key mapper) is a routing DEFAULT, not a block, and must STAY closed — relaxing it
   would mis-map `"[]"` to the IL repr `"!0[]"` (line 502) and hand the provider the wrong key. Instead
   the indexer path keys the lookup DIRECTLY on `arrayName 1`: an array is a `TyConst` (not `TyClass`),
   so it misses the `TyClass`-only external guard at `InferRecordAccess.fs:631` and falls to the `_` arm
   (line 641); add an array branch there that calls the EXISTING `resolveExternalIndexer (arrayName 1)
   args "get_Item"` (line 531, needs no change) before `stringOrArrayIndex()`. The write mirror wires
   from `inferAssignment` (`InferControlFlow.fs:811`) when the LHS is `Expr.IndexedLookup` (today it just
   re-`infer`s the LHS through `inferIndexedLookup` — no write resolution). Optionally route `arr.Length`
   through `TryLookupMember(arrayName 1, "get_Length")` at `resolveFieldStep` line 440 to retire the
   hardcoded branch. **The ONE precondition:** key AGREEMENT — capture stores members under the
   arity-suffixed compiled name (`arityName "[]" 1 = "[]``1"`), but a `TyConst` surface's key is the bare
   `"[]"`. W9 must make both sides agree on `"[]"` (either key the array's members under `"[]"`, deviating
   from the default `compiled` keying, or translate `TyConst("[]")` → `"[]``1"` at the lookup). Ordinal,
   no normalization, so once aligned `TryLookupMember("[]", "get_Item")` hits.
4. **Inline splice** (`SymbolProviders.fs`, `InlineExpansion.fs`) — the general member-inline mechanism
   above (member-keyed `InlineBody` + the `TExpr.ExternalMember` splice arm).
5. **Emit** — nothing new: a spliced `(# … #)` emits as today; a bodiless member stays a real call
   (`ClrExternalMembers.fs`, `EmitJs.fs:621-663`).
6. **Migrate + delete** — move `GetArray`/`SetArray`/`GetArrayLength` (and `GetString`) bodies from the
   `ops-platform.clr.fs` / `.js.fs` free `let inline` functions INTO `'T[]` / `string` member accessors; the
   TS index-sig `get_Item`/`set_Item` become provider-synthesised members whose inline body is the
   `$0[$1]` bracket, served via `TryLookupInlineBody` (so `GetIndex`/`SetIndex` become those bodies — no
   separate lowering flag). Then DELETE: `tryIndexSignature`, `TryLookupIndexSignature` (+ its ~12
   impls), the Freeze `zonk arrTy` re-derivation ladder (read + write — Finding 1's duplication with it),
   the `TyConst(arrayName 1) && "Length"` special-cases (`InferRecordAccess.fs:440`, `ElaborateExpr.fs:556`,
   `Resolve.fs:816`), and the now-unreferenced `GetArray`/`Get*` free functions. `[IndexerName]` stays
   NON-generalised: `string` is the sole `get_Chars` producer in .NET, so the string companion maps
   `get_Item` → `get_Chars` on CLR / native `s[i]` on JS by hand, not by attribute.

**Net deletion >> addition** (the user's expectation): one member-resolution + one member-inline-splice
arm REPLACE the object-argument-classification ladder, the parallel index-sig facet, the `.Length`
special-cases, and the free-function indexer intrinsics — and the enabling primitive (concrete inline
members on intrinsics) is reusable platform-binding surface well beyond indexers.

**Sequencing + isolation (isolation-first):**
1. The GENERAL primitive FIRST — a minimal `extern` type with ONE concrete `(# … #)`-bodied member
   (NOT an indexer): assert parse → capture (Class + repr) → member-inline-splice → emit.
   Proves the primitive independent of indexer sugar.
2. THEN array/string `get_Item`/`set_Item`/`Length` (migrated bodies) — array index/length + string
   index tests and `IndexSignatureTests` must stay emit-BYTE-IDENTICAL (pure re-plumbing).
3. THEN external CLR `get_Item` (real call, no inline body) + TS index-sig (provider-served bracket
   body) — proving the splice-vs-call fork resolves on ONE path. The `set_Item` write half is new
   capability, so it needs its own read-AND-write assertion.

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

**W9** is OFF the node critical path (it supersedes landed W4 and establishes a general platform-binding
primitive, not a node capability) and is INDEPENDENT of W8 (different mechanism — see W8). It is the
largest single item here — a four-stage change lifting the `VesperLib.fs:1354-1374` invariant — so it
lands as its own tranche, isolation-first per its own §Sequencing (general primitive → array/string
migration → external/TS), with array/string + `IndexSignatureTests` emit byte-identical as the
regression guard and the `set_Item` write half as new capability. The Resolution load-bearing question
is RESOLVED (spike 2026-07-05: routable by keying lookups on `arrayName 1`, no `EngineCore.fs:500`
relaxation); the one open precondition is capture/lookup key AGREEMENT on `"[]"` vs `"[]``1"` (see W9 §3).

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
  `recordForeignRef`/`classifyHome`/`classifyKind`/`refArity`, `finalizeDiagnostics`); `Schema.fs`
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
