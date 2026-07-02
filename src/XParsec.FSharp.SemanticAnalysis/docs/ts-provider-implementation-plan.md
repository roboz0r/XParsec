# TS provider — R5: the JS ref pack (cross-package nominals, lib.es2015, `Js` namespace)

**Status (2026-07-02).** R1–R4a are DONE — the mitt full-fidelity gate is MET (golden
`Diagnostics = []`, mitt's complete public surface driven from Vesper under Node;
commits 39ce989a/ffd9f5ca/a4e76dc6/a92c477d). R5 makes CROSS-PACKAGE NOMINALS real:
today a foreign named type (`mitt`'s `all: Map<…>`) degrades to an opaque `FTConst` —
it survives as a carried nominal but its members can never resolve. This doc is the
runnable sequence for the first R5 tranche: refs-table identity → ambient-global
extraction → a vendored `lib.es2015` ref pack → `Js.*` consumption. `@types/node` and
DOM (`Js.Dom`) are follow-on tranches scoped AFTER this lands.

**Companion:** [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md)
holds the *decisions*. Three sections are load-bearing here and are RESOLVED — do not
relitigate:
- §"Cross-package nominals, globals, and the JS ref pack" — identity+kind BAKED at
  extraction (manifest-level refs table, the ECMA-335 `TypeRef`/`AssemblyRef` analog);
  shape NEVER inlined (members resolve through the provider stack at inference time);
  ref pack extractor-generated and NEVER hand-authored; `Js` namespace mirroring TS's
  lib structure (ES core flattens into `Js`, hosts split later as `Js.Dom`); `Global`
  no-import flag (Fable `[<Global>]` as data).
- §"Literal types stay structural…" + the `EqSet` sub-decision (R4a machinery this
  work builds on).

Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the
extractor (`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) —
steps 1–3 need it. Goldens regenerate via the skill's `-UpdateSnapshots` against
`test/Vesper.Ts.Extractor.Tests` (consumer tests NEVER run the extractor). Anchors
below are symbol names, not line numbers — confirm by reading before editing.

---

## What has landed (context, not work)

| Area | State |
|---|---|
| R1–R3 | Manifest nominal → `FTClass` via qualified-name→`TypeKey` resolver; `AttachMembers` native `receiver.member(args)` lowering (escape eta-wraps); Bus e2e green. |
| R4a | Faithful `TypeRef.KeyOf/IndexedAccess/Conditional/Literal` + `Signature.TypeParamBounds`; inert `FTKeyOf/FTIndexedAccess/FTConditional` carriers through every walk; `Engine.evalTypeLevel` ground folds; `InferExternalCall.admitLiteralMethodTypars` call-site literal grounding; single-candidate field-walk freshens method typars. mitt golden `Diagnostics = []`. |
| mitt gate | `MittE2ETests` drives factory/`on`/`off`/`emit`±payload/`all`-read against real vendored `mitt.mjs` under Node. `UnannotatedMittTests` pins the annotation-required policy + the undefined-vs-unit GAP. |
| Default imports | The bit rides the resolved symbol: `ExternalSymbol.ImportForm` (stamped by the TS provider from the manifest's `ImportShape`), read at `JsImports.addRef` — no side channel. |
| Test patterns | Hand-built-manifest isolation suites: `ExternalNominalClassTests`, `MethodAxisGenericTests`, `MethodAxisSingleCandidateTests`, `TypeLevelFoldTests`, `LiteralUnionTests`. |

## Current truth — the gaps R5 phase 1 closes (and the ones it doesn't)

1. Foreign `Named` → `FTConst(name, args)`: `TsManifestProvider.toFrozen`'s resolver
   consults only the manifest's OWN registry. `Map` has no home, no kind, no members.
   mitt's `all` is pinned READS-ONLY (`e.all` → `instanceof Map` asserted; `.get`
   etc. cannot type). **Steps 1+5.**
2. The extractor is module-entry only: `extractFile`/`extractPackage` both throw
   `"not a module (no exports found)"` on a global-scope `.d.ts` (`moduleExportsOf`).
   `lib.es*.d.ts` are script files declaring globals. **Step 2.**
3. No standard-library surface exists at all (`JsNativeSymbols` hand-authors exactly
   one type, `Error`, as the intrinsic `exn` repr). **Step 3.**
4. Nothing mounts under a namespace and every external nominal emits an import.
   **Step 4.**
5. NOT this tranche (pinned, honest): the undefined-vs-unit intrinsic step-0
   (`UnannotatedMittTests` GAP test); module-level `let mutable` captured in a JS
   closure (`.contents` read vs bare-value decl — surfaced by the gate harness,
   recorder idiom sidesteps); `@types/node` / DOM breadth.

Two PRE-EXISTING extractor-suite failures you will inherit red: `provider
resolution.resolves: generics.manifest.json` ("Box") and `resolves:
mitt.manifest.json` ("Handler") — `testProviderResolves` calls
`TryLookupType(bareName)` for a GENERIC type but the provider registers
arity-suffixed names (`` Box`1 ``). Step 1 rebuilds this resolver seam: fix it there
(deliberately, with the test asserting the intended lookup contract), not as a
drive-by.

---

## Step 0 — review-debt burndown (do BEFORE the R5 steps)

Residue of the 2026-07 thermo-nuclear review. Groups G1–G4 landed (commits
`Review G1`…`Review G4`: defect fixes; SemType/FrozenType child-walk skeletons;
extractor MapCtx/SigAxis/5-file split + `DiagCode`; Engine split
EngineCore←Subsume←Engine + fold-in-subsumes; provider TranslateCtx + one
identity mint + 3-file split; JsExternalMembers + `classFlagsOf`; default-import
side channel deleted (`ExternalSymbol.ImportForm`); argSig renderer unified in
`ExternalSymbols`). What remains, in priority order:

### 0a. Test-harness extraction (~550 duplicated lines; do first — R5 tests inherit it)

- [ ] `TestHelpers` (JS tests): `stackTs manifest` (the 8-site provider-stack
      one-liner), `analyseWith provider input` (5 near-identical wrappers), and
      `emitWith provider runtime exportTopLevel input` — route through the
      existing `frozenImplJs` + ONE `WalkCtx` builder, collapsing the ~10
      hand-built `EmitJs.WalkCtx` literals (BusE2E, ExternalAttachMembers,
      LiteralUnion, MethodAxisGeneric, MethodAxisSingleCandidate, NullUndefined,
      MittE2E, TsManifestEnum, FreeFnOverload, MemberOverload). Note the copies
      drift from production wiring (`Resolver = ValueNone` with
      `Source = ValueSome` where `Codegen.compileWith` builds a LineIndex
      resolver) — the helper should match production.
- [ ] `expectNodeOutput name files expected` — the 8-site Node round-trip match
      block (skip-on-node-absent documented once; exact `Expect.equal`, not the
      two weak `stringContains` variants in ExternalAttachMembersTests).
- [ ] `SchemaDsl.fs` (new test file): the `named`/`sig0`/`sig1`/`param'`/
      `method'`/`property'`/`strLit`/`keyof`/`idx`/`cond` builder set privately
      re-declared in 6 files (~245 lines); convert the 3 raw-JSON-string
      manifests (MethodAxisGeneric, MethodAxisSingleCandidate, NullUndefined) —
      stringly schema breaks at runtime only.
- [ ] SemanticAnalysis tests: shared `mkSignature`/`mkMember` with defaulted
      `MethodTyparBounds`/`ImportForm` (the `MethodTyparBounds = [||]` addition
      forced 9 mechanical test edits; next field should be one-site).
- [ ] `MittFixture` helper shared by `MittE2ETests`/`UnannotatedMittTests`
      (fixtureDir + golden deserialize + stack, currently duplicated).

### 0b. Type/boundary stragglers (small, independent)

- [ ] `FTConditional`/`TyConditional`: record payload (`{ Check; Extends;
      WhenTrue; WhenFalse }`) — four positionally-matched same-typed fields; a
      silent whenTrue/whenFalse swap typechecks today. Cheap now that walks go
      through the G2 skeletons.
- [ ] `ExternalClassFlags`: replace the `Erased`+`AttachMembers` bool pair with
      a 3-case `MemberLowering` DU (attached-native / receiver-first /
      erased-bare) — both-true is unrepresentable nonsense the type admits.
      Consumers: `JsExternalMembers.classFlagsOf` dispatch, provider stamps.
- [ ] `decodeClassFlags` (MetadataSymbols) + JsNativeSymbols: build via
      `{ ExternalClassFlags.Default with … }` so provider-foreign flags (incl.
      R5's planned `Global`) stop being restated per site.
- [ ] `ExternalSignature.make` (defaulted bounds) so the next signature field
      does not repeat the 11-site churn.
- [ ] `EmitResolve.headOf`: the carrier-node → `"obj"` arm is unreachable
      (encodeType rejects loudly upstream) and its comment misdescribes the
      backend — make it `failwithf` or re-comment as unreachable-by-invariant.
- [ ] `ClrEncoder.recoverOpenTypars` / `FrozenType.iterChildren2`: the `FTOr`
      positional pairing rests on an insertion-order claim over a set-semantic
      type; add a head-keyed fallback on length-match-but-head-mismatch, or a
      loud ambiguity failure + a pinning test.
- [ ] `EqSet`: trim the zero-production-caller API (`map` — the documented
      attractive nuisance the FTOr docs warn against — plus `singleton`,
      `contains`, `tryFind`, `ofList`, `AsSpan`) and delegate the duplicated
      facade/loops to `EqArray` (`EqArray.ofImmutable` is the free bridge);
      fix the overstated "a total order on FrozenType does not exist" rationale
      (one existed — it was arbitrary, a maintenance surface, and destroyed
      declared order; say that).

### 0c. Inference-seam stragglers

- [ ] `ConstArgFacts`: compute the per-position syntactic-constant facts ONCE
      per external call and thread to both the pick refinement
      (`admitLiteralMethodTypars`) and the commit seed
      (`methodTyparConstantSeed`) — deletes the pick/commit re-derivation and
      the raw `argExpr` parameter on `commitExternalOverload` (7 params).
- [ ] Static-probe asymmetry: the instance probe refines literals before
      `pickBestOverload`; the static probe only seeds at commit. Either wire the
      same refinement or add the one-sentence deliberate-scope comment.
- [ ] Enum literal readers: `TypeRegistration.registerEnumTypeDefn`'s
      string-projection (bare single-part strings only) diverges from
      `Elaborate.resolveEnumCaseValue` (peels blocks + escapes) — a legal
      `| A = ("auto")` silently declines literal-union admission. Extract one
      shared projection helper; fix the stale docs (TypeRegistration's "values
      are resolved later by Elaborate" + `SideTables.EnumTypeInfo`'s "needs only
      the case names").
- [ ] Stale sort-era comments: `SemanticInfo` `TyOr` docstring still says
      "canonical (sorted) form" (+ the named raw-case-building exceptions that
      no longer exist); Engine's "sound because both are sorted/deduped"
      soundness comments now rest on EqSet set-equality — say so.
- [ ] `unifyAnnotation`: the `TyOr expected` arm and the literal-bearing arm
      reduce to the same `subsumes <> Unrelated` — one arm, one guard.
- [ ] `evalTypeLevel`: return `t` unchanged when no child folded (currently
      rebuilds every compound unconditionally), so casual calls are cheap.
- [ ] `Subsume.groundMemberType` realises members via the freshening
      `instantiateSignature` inside a read-only query — safe only because the
      arm pre-filters `IsValueMember`; use the non-freshening realiser to state
      it structurally.

### 0d. Known-failing baseline (pre-existing, verified at pre-review HEAD)

`Vesper.Ts.Extractor.Tests` has TWO standing failures unrelated to the review
work: `provider resolution.resolves: generics.manifest.json` ("type 'Box'
should resolve") and `…: mitt.manifest.json` ("type alias 'Handler' should
resolve") — bare-name vs arity-suffixed `TryLookupType` misses in the test's
resolution walk. Fix or pin deliberately before leaning on that suite as an R5
gate.

---

## Step 1 — refs table: foreign identity + kind baked at extraction

**Goal.** Every foreign named reference in a manifest carries `{home; kind; arity}`;
the provider mints a HOMED `FTClass(TypeKey)` for foreign class/interface refs; the
regenerated mitt golden spells `Map`'s home; member access on a homed type whose home
manifest is absent from the compilation fails with a clear "package not referenced"
diagnostic.

1. Schema (`src/Vesper.Ts.Manifest.Schema/{Schema,Codec}.fs`, additive,
   `SchemaVersion` stays 1): a manifest-level `Refs` table — foreign name →
   `{ Home: string; Kind; Arity: int }`. Exports stay the "TypeDef" side. Codec omits
   the table when empty so ref-free manifests stay byte-identical (the
   `typeParamBounds` precedent).
2. Extractor (`src/Vesper.Ts.Extractor/Extractor.fs`): where `mapType` falls through
   to `looksNominal`-gated `Named`, classify the symbol's HOME: declaration's
   `getSourceFile()` + `program.isSourceFileDefaultLibrary` /
   `isSourceFileFromExternalLibrary` (vendored binding `TypeScript.fs:4308` — the
   `Program` methods; thread `program` to `mapType`'s callers if only `checker` rides
   today). Default lib → reserved home mirroring TS's lib grouping (`es2015`, …);
   external package → its specifier (the `packageId`/nearest-`package.json` machinery
   from the package entry mode); local → no ref entry (the existing own-registry
   path). Kind from `SymbolFlags` (interface/class/alias/enum). Fable rebuild + regen
   mitt golden (Map gains a ref entry) in the same change.
3. Provider (`TsManifestProvider.toFrozen` + `providerOfManifest`): the resolver
   consults the refs table AFTER the own registry — a foreign class/interface ref
   mints `FTClass(TypeKey(home-qualified))` (arity law applies:
   `SymbolKeyOps.arityName` suffix-at-lookup). Member access then resolves through
   the ORDINARY provider stack at inference time (`resolveFieldStep` →
   `TryLookupMember` with the home-qualified name) — no two-phase loader, no new
   machinery. Absent-home miss: sharpen the existing no-such-member diagnostic to
   name the missing package.
   **Open sub-decision (owned here):** foreign ALIAS/ENUM-kind refs — a homed alias
   must resolve through its home manifest's `Abbrev` at lookup rather than minting
   `FTClass`; decide + pin (keeping them carried `FTConst` until the home manifest is
   stacked is acceptable v1 if documented).
4. Fix the bare-name arity gap while the resolver is open: decide the lookup contract
   (`TryLookupType` accepts bare name + arity? callers always suffix?) and make
   `testProviderResolves` assert it. The 2 known failures flip green.
5. Isolation first (systematic-tests rule): hand-built two-manifest fixture — package
   A exports `Box`, package B's export references it — pin FTClass minting, stack
   resolution, and the absent-home diagnostic, BEFORE the mitt golden regen.

**Trap.** Baking is IDENTITY ONLY. Do not inline the foreign type's members/shape
into the referencing manifest — that is the staleness trap the design section
forbids.

---

## Step 2 — ambient-global extraction entry mode

**Goal.** The extractor can walk a global-scope (script) `.d.ts` and emit a manifest
of its globals — the entry mode `lib.es*.d.ts` needs, fixture-first.

1. New entry point beside `extractFile`/`extractPackage` (say `extractGlobals`):
   enumerate GLOBAL declarations instead of `moduleExportsOf` (which correctly stays
   fatal for module entry). Enumerate by SYMBOL, not per-file statement — TS MERGES
   interface declarations across files (`Map` accretes members from
   `es2015.collection` + `es2015.iterable` + …), and the merged symbol's declared
   type is the truth. The manifest's `Package` is the reserved home (`es2015`),
   `Version` = the TypeScript package version.
2. The class-like global pair must FUSE: `interface Map<K,V>` (instance side) +
   `declare var Map: MapConstructor` (value side) + `interface MapConstructor`
   (statics + construct signatures) are THREE symbols that Vesper needs as ONE
   class-like export named `Map` — instance members from the type-side interface,
   ctors from the ctor-var's construct signatures, statics from `MapConstructor`'s
   other members. This fusion is the main new extractor complexity and is ACCEPTED
   (never hand-author around it). Free global functions / `declare var` without the
   pair stay ordinary function/value exports.
3. Fixture-first: a hand-written `globals.d.ts` fixture (one fused pair, one merged
   interface split across two fixture files, one free function, one alias) with a
   committed golden, BEFORE touching the real lib. The golden-regen flow is the
   existing `UPDATE_SNAPSHOTS` one.
4. Diagnostics: same resilient contract — degrade + warn per symbol, never abort;
   the fixture asserts its expected diagnostics exactly.

**Trap.** Do not evaluate anything (unchanged rule); and do not enumerate
per-source-file — you will emit duplicate/partial interfaces and miss merges.

---

## Step 3 — the `lib.es2015` ref pack + burndown coverage golden

**Goal.** Run step 2's mode over TypeScript's own `lib.es2015.*` closure; commit the
manifest(s) + a ranked diagnostics report as the BURNDOWN golden that surfaces
high-value extractor gaps empirically.

1. Input: the `typescript` package already installed for the extractor
   (`node_modules/typescript/lib/lib.es2015*.d.ts` — the es2015 set pulls `lib.es5`
   via its own references; decide whether the pack is "es2015 flat including es5" and
   record it in the manifest header). Output: manifests mirror TS's lib grouping.
2. Vendor location: start as committed test fixtures (`test/ts-fixtures/es2015/`,
   sibling of mitt) consumed by tests; PROMOTION to a compiler-shipped artifact (or a
   `Vesper.Js` std-lib package) is a later, deliberate packaging step — record the
   choice, don't block on it.
3. The coverage golden is a BURNDOWN, not a gate: commit the diagnostics report
   ranked by code frequency (`mittDiagnosticsContract` pattern, but asserting the
   COMMITTED report matches — any drift fails; shrinking it is the R5 scoreboard).
   Expect real residue (mapped types, `Symbol.iterator` members, getter/setter
   asymmetry); that is the point — do NOT hold this step to `Diagnostics = []`.
4. **Primitive-overlap policy (decide + pin here).** `lib.es5/es2015` declare
   `interface String/Number/Boolean/Object/Function/Array/Symbol` — but Vesper's
   primitives ride `IntrinsicRepr` (canon `.fsi` names, per-target platform reprs)
   and Vesper arrays ARE native JS arrays. Registering these as `Js.*` nominals would
   double-represent them and fight the intrinsic subsumes/canonName machinery
   (see `reference_intrinsic_repr_overloaded_canonname_codegen` /
   `reference_null_undefined_already_survive_js` for the trap class).
   Recommended v1: a skip-list — the ref pack does not REGISTER the intrinsic-repr'd
   names (their member surface via `keyof`/receiver-members is a later, separate
   question); revisit per burndown evidence. Whatever is chosen: pinned by a test,
   stated in the manifest/module header.

---

## Step 4 — `Js` namespace mounting + `Global` no-import emission

**Goal.** Ref-pack types are reachable from Vesper as `Js.Map<…>` and emit NO
import.

1. Mounting: the ref-pack manifests record PLAIN global names; the PROVIDER mounts
   them under the `Js` namespace at registration (the dotted-qualified-name machinery
   `testProviderResolves`' `qualify` exercises already exists; the `ns→namespace`
   re-role is the design-table row). ES core flattens into `Js` — the lib version
   suffixes are TS's compile-target mechanism, not semantic namespaces. Front-end
   annotation path: `let m : Js.Map<string, int>` must resolve through the external
   annotation seam (`project_translatetype_external_types` machinery); pin with an
   isolation test on a hand-built namespaced manifest before the real pack.
2. `Global` flag: a marking on the external class shape/origin (sibling of
   `ExternalClassFlags.AttachMembers`, likewise Fable-named — `[<Global>]`) stamped
   by the provider for ref-pack/global-home types; `JsImports` consults it and emits
   NO import; `new Js.Map(...)` lowers to bare `new Map(...)` (the external-new arm
   handles `new` heads — the BCL-exn→`new Error` precedent). Refs-table homes that
   ARE real packages keep normal import emission — `Global` rides the home, not the
   type.
3. Home-qualified `TypeKey` + `Js.` display: make diagnostics print the Vesper-facing
   name (`Js.Map`), not the wire home.

---

## Step 5 — ★ the consumer gate: `Js.Map` end-to-end + mitt `all` upgraded

Both halves, then this tranche is DONE and `@types/node`/`Js.Dom` may be scoped:
1. A Vesper program constructs and drives `Js.Map` directly — `new Js.Map()`, `set`,
   `get`, `has`, `size` (property read), `delete` — emitted via Codegen.Js, run under
   Node, behaviour asserted (isolation suite on the vendored pack; the
   `MethodAxisSingleCandidateTests` harness shape). Iteration (`for … in` over a
   `Js.Map`, `Symbol.iterator`) is explicitly OUT of this gate — pin it as the next
   burndown item (the capability-interface machinery exists; wiring it to TS
   iterables is its own step).
2. mitt's `all` graduates from reads-only: `e.all` resolves as a HOMED `Js.Map` via
   the refs table, and `MittE2ETests` exercises a member call on it (e.g.
   `e.all.has("ping")` or a `get`+invoke) against the real runtime. Remove the
   reads-only caveats from `MittE2ETests`/`UnannotatedMittTests` headers, the design
   doc's ref-pack section, and `project_js_ref_pack`/
   `reference_js_external_instance_member_walls` memories as they close.

---

## After this tranche (scoped later, do not start)

`@types/node` burndown (module-entry, no ambient mode needed — a refs-table +
scale test); `Js.Dom` (ambient mode at 10× scale, browser-only, its own namespace);
structural content-hash + SCC cycles (design §"Structural → content-hash");
undefined-vs-unit intrinsic step-0; `Vesper.Platform.Map` portability layer
(recorded, out of scope). Delete this doc when step 5 lands; fold durable facts into
module headers + `project_js_ref_pack` / `reference_*` memories.

---

## Orientation — files and symbols

- **Schema/codec:** `src/Vesper.Ts.Manifest.Schema/{Schema,Codec}.fs` — `TypeRef`,
  `Signature.TypeParamBounds` (the omit-when-empty codec precedent), `EnumValue`.
- **Extractor:** `src/Vesper.Ts.Extractor/Extractor.fs` — `mapType` (the
  `looksNominal` fallthrough is where foreign names pass unhomed), `extractFile` /
  `extractPackage` / `moduleExportsOf` (the fatal "not a module" seam step 2
  bypasses), `asGenericInstantiation` (`__type` rejection), diagnostics accumulator +
  `relativizeDiagnostics`. Vendored binding `vendor/TypeScript.fs` (`Program`
  methods `isSourceFileDefaultLibrary`/`isSourceFileFromExternalLibrary` ~:4308).
  Rebuild via the one sanctioned Fable command (header). TRAPS learned in R4a: raw
  `TypeFlags` numerics DRIFT between the vendored binding and installed TS — classify
  by runtime predicate or FIELD PRESENCE; `getConstraint()` EVALUATES (use the
  declaration node); `__type` is TS's reserved anonymous-type name.
- **Provider:** `src/XParsec.FSharp.Codegen.Js/TsManifestProvider.fs` — `toFrozen`
  (resolver, arity suffix-at-lookup), `providerOfManifest` (`typeKeys`/`typeKinds`),
  `signatureOf` (`MethodTyparBounds` threading), `stampValueSymbol` (`ImportForm`);
  argSigs render via the shared `ExternalSymbols.argTypeName`.
- **Stack/inference:** `ExternalSymbols.fs` (`stack`/`composite` first-hit-wins,
  `ExternalSignature`, `instantiateSignature[Bounds|With]`);
  `Passes/Unification/InferRecordAccess.fs` (`resolveFieldStep` external arms — where
  member lookup hits the stack); `Passes/Unification/Engine.fs` (`evalTypeLevel`,
  `subsumes`); `Passes/Unification/InferExternalCall.fs` (overload commit,
  `admitLiteralMethodTypars`).
- **Emission:** `Codegen.Js/EmitJs.fs` (`WalkCtx`, external-new arm,
  `validatePlatformTypes`), `JsRuntime.fs` (`JsImports.create`/`addRef` — step 4
  adds the `Global` consult), `Codegen.fs` (`compileWith`),
  `JsNativeSymbols.fs` (shrinks toward the intrinsic-repr seam; `Error` stays).
- **Tests as patterns:** `test/Vesper.Ts.Extractor.Tests` (goldens, `UPDATE_SNAPSHOTS`
  regen, `testProviderResolves`, `mittDiagnosticsContract`);
  `test/XParsec.FSharp.Codegen.Js.Tests/{ExternalNominalClassTests,
  MethodAxisSingleCandidateTests,TypeLevelFoldTests,MittE2ETests,UnannotatedMittTests}.fs`;
  `test/ts-fixtures/mitt/` (vendored fixture shape to mirror for `es2015`).

## Relevant memories
`project_js_ref_pack` (the resolved R5 decisions — canonical),
`reference_js_external_instance_member_walls` (R4a baseline + residues),
`feedback_freeze_no_backend_knowledge` (extractor carries, front end evaluates),
`feedback_systematic_tests_over_whackamole` (isolation tests before wiring),
`reference_intrinsic_repr_overloaded_canonname_codegen` +
`reference_null_undefined_already_survive_js` (the primitive-overlap trap class),
`project_arity_overloaded_type_names` (the arity law),
`reference_eqarray_percentA_cache_key` (never key caches on `%A` of these types),
`feedback_user_commits` (user reviews and commits unless directed otherwise).
