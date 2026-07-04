# TS provider — outstanding gaps toward `@types/node` and `Js.Dom`

**Status (2026-07-03).** The "faithful real-package consumption" tranche (Walls 1–5,
sequenced BEFORE the two breadth destinations) has essentially **landed**. This doc has been
rewritten to track only what is **NOT built** — the shipped walls are now recorded in the
code itself (module headers + the tests named below), per
[[feedback_durable_knowledge_in_code]]. What remains:

- ~~**One reopened wall step** — Wall 3's *inflow structural widening* (§G1)~~ **LANDED
  2026-07-04.** The `number→float` decision was relocated out of the front end into the JS
  provider (`Codegen.Js.NumberCovariance`). See §G1 for the as-built record; only the
  orthogonal **Step 5** (full provider laziness — a perf/architecture cleanup, no correctness
  dependency) remains, deferrable.
- **Two small `dynamic` follow-ups** (§G2, §G3), **one inert residue** (§G4), and **two external
  interface-heritage resolution gaps** (§G5 — orthogonal to G1, bite at §Breadth).
- **The two breadth destinations** — `@types/node` and `Js.Dom` (§Breadth) — plus the
  **faithful-later graduations** they will pull in (§Faithful-later).

Every landed wall followed one design pattern, and any remaining work MUST keep to it:
*real types via JS intrinsics + operators/recognizers/erasing-nominals, NOT new SemType DU
cases with magic unifier behaviour.* No wall added a SemType case or changed `unify`/`subsumes`.

**Companions (still load-bearing):**
- [`dynamic-typing-design.md`](dynamic-typing-design.md) — the AS-BUILT `dynamic` design
  (Wall 2). The §G2 escape-warning follow-up is specified there.
- [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md) — the original
  *decisions* doc. Two of its sections are SUPERSEDED by what shipped (its `TyDynamic`-SemType
  framing → Wall 2 shipped an opaque intrinsic + `?`; its `null`/`undefined`-as-core framing →
  Wall 1 shipped `undefined` as a JS-only intrinsic, `null` needed no entry).

**Workflow.** Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the
extractor (`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) — needed
only when work touches `src/Vesper.Ts.Extractor/`. Goldens regenerate via `-UpdateSnapshots`
against `test/Vesper.Ts.Extractor.Tests` (consumer tests NEVER run the extractor). Anchors
below are symbol names — confirm by reading before editing. Isolation-first is the rule
([[feedback_systematic_tests_over_whackamole]]): pin the behaviour with a hand-built fixture
before any golden regen.

---

## What shipped (context, not work)

One row per landed piece — the code + named tests are the canonical record; do not re-narrate
here.

| Wall / area | State | Anchor + test |
|---|---|---|
| R1–R4a, R5 tranche‑1 | Manifest nominal → homed `FTClass`; native `receiver.member(args)` lowering; type-level carriers + `Engine.evalTypeLevel`; refs table; ambient-global mode; vendored `es2015` pack + `es2015BurndownContract`; `Js` namespace; bare `new Map()`. | `JsMapE2ETests`, `MittE2ETests` |
| **Wall 1** — `undefined` distinct from `unit` (`809901dd`) | A **real JS-only intrinsic** (`prim-types-undefined.js.*`, `files-js`), published `Intrinsic(canon="undefined", platform=Some "undefined")` via the files-js harvest, resolving forward before the reverse-canon map collapses it to `unit`. `null` needed no entry. | `UndefinedIdentityTests`, `NullUndefinedTests` |
| **Wall 2** — `dynamic`, a disciplined `any` (`6790cc3d`) | A **plain opaque JS intrinsic** (`type dynamic = (# "any" #)`), zero assignability edges, NO SemType case. Enter via `dynamic x` (erasing `retype`); access ONLY via `x?foo`/`x?foo <- v` (`op_Dynamic`/`op_DynamicAssignment`, SRTP `default ^TResult : dynamic`); dotted `.foo` on a `dynamic` ERRORS. See `dynamic-typing-design.md`. | `DynamicTypeTests` |
| **Wall 3 (Steps 1,2,4)** — structural object → erasing nominal | Anonymous `{x,y}` gets a **canonical field-order-invariant shape-hash** (`TsManifestTypes.structuralHash`; named refs as leaves) and rehydrates to a **resolvable homed `@struct` nominal** (`FTClass(structuralKey …)`), registered by `TsManifestMembers.buildStructuralTypes` as an `IsInterface`/no-ctor class whose fields resolve + lower to native `receiver.field`, emitting nothing. Object-only **intersections graduate** to a merged `Structural` in the extractor (`TypeMap.fs` `isIntersection` arm); non-object stay `IntersectionErased → obj`. (Commits `92c4fc9c`/`1f6efa54`/`28fa702b`.) | `StructuralShapeHashTests`, `StructuralNominalTests`, extractor `Tests.fs` intersection asserts |
| **Wall 4** — TS iterables → JS `for … of` | Provider homes a `[Symbol.iterator]` (`__@iterator@N`) member to `seq<T>` by injecting `enumerableInterfaceName` into `FrozenInterfaces` (`TsManifestMembers.tryIteratorElement`); the existing `pickEnumerableElem → tryForInEnumerator → EmitJs.ForOf` chain lights up. Tuple binder in the `for..of` lowering (`compileMatchPattern` reuse); fixed all-required TS tuple → `Schema.TypeRef.Tuple` in the extractor. | `IterableForInTests` |
| **Wall 5** — module-level `let mutable` captured in a JS closure | `RefCellPromotion.collectPromotions` no longer seeds the **top-level** `TDecl.Let` binder into `promote` — a module-level mutable is a static field (CLR) / ambient reassignable `let` (JS), shared by the backend natively, never a `.contents` heap cell. Makes `rewriteDecl`'s "a top-level binding cannot itself be a promoted cell" hold by construction; latent-fixed the same miscompile on CLR. | `PrimitiveExprTests` (closure-writes-module-mutable), CLR `CapturedMutableTests` |

Note: the `StructuralEquality/Comparison/Format/Printer` test families are a DIFFERENT
"structural" (F# runtime equality/`%A`), unrelated to Wall 3.

---

## Outstanding gaps

### G1 — Structural inflow widening (Wall 3's last step) — LANDED (2026-07-04)

**As built.** The `number→float` decision was relocated OUT of the shared front end and INTO the
JS provider, resolving the layering violation the reopening flagged (`polarizeNumber` naming the
literal `"number"`/`"float"` in `SemanticAnalysis`). The canonical record is now the code + tests
below ([[feedback_durable_knowledge_in_code]]); this note is a pointer, not a spec.

- **The covariant/invariant resolution lives in `Codegen.Js.NumberCovariance`** (module header is
  the durable design). It is a provider DECORATOR wrapping the COMPOSED provider (installed in
  `TsManifestProvider.buildContractFor` and, in tests, `TestHelpers.stackWithAmbient`). At wrap it
  ASSERTS `IntrinsicForwardRepr["float"] = "number"` (throws otherwise — the datum that licenses
  naming `float` as the covariant target), and reads the invariant family off
  `IntrinsicReverseCanon["number"]`. A variance-tracked `FrozenType` walk resolves: covariant scalar
  `number → float`; contravariant parameter → the retained `number` token; invariant generic
  type-argument → the repr-family union `int|float|float32|…`.
- **The front end is now number-agnostic.** `ExternalSymbols.polarizeNumber` and the raw/polarized
  realiser split (`instantiateSignatureRaw`/`RawWith`) are DELETED; `instantiateSignature` /
  `openSignature` realise the provider-resolved signature verbatim. `SemanticAnalysis` names no
  `"number"`/`"float"` in the realisation path (only doc comments remain).
- **Structural width is repr-sibling-driven.** `Engine.reprSiblings` (name-agnostic, off
  `ctx.Provider.IntrinsicForwardRepr`) admits a record field whose canon shares a forward repr with
  the member's — so an `int` field satisfies a now-`float` (`number`-repr'd) interface member.
  `Engine.tryStructuralWiden` realises members normally and admits iff
  `subsumes <> Unrelated || reprSiblings`. `numericFamilyOr` (contravariant param family, off the
  reverse axis) is UNCHANGED.
- **The `type number = float` abbreviation is GONE** (`prim-types-number.js.fsi` + its
  `manifest.toml` `files-js` entry deleted). `number` names no Vesper-side type; it survives ONLY as
  the provider-internal retained token (contravariant param → `numericFamilyOr`). The extractor is
  unchanged (still retains the `number` token the provider consumes).
- **The forward intrinsic axis was already populated on JS** — it flows to the composite from the
  Vesper.Core `.js.fs` harvest (`ExtractCtx.toProvider` builds forward + reverse side-by-side),
  identical to the reverse axis; no provider-leaf change was needed. Pinned by
  `IntrinsicForwardReprJsTests`.

Tests: `IntrinsicForwardReprJsTests` (forward axis), `NumberFamilyTests` (scalar covariant read =
`float`; param widens), `StructuralWidenTests` / `StructuralWidenE2ETests` (record→interface width),
`TypeArgNumberTests` (`Box<number>` value read = family union, NOT scalar float/int).

**Scope landed:** the numeric-variance relocation, record-only structural width. Structural admission
over nominal `TyClass` (the arbitrary-POJO-into-`{x,y}` case) stays punted to the nominal upcast path
(`subtypeNominalOf`) — a FOLLOW-UP, unchanged.

**Deferred (documented, not blocking):**
- **Type-argument union usability caveats.** A covariant element read yields the UNION, so
  `arr.[i] + 1.0` needs a narrow; Array invariance means a Vesper `int[]` does not flow into a
  `number[]` param without element-wise subsumption at the arg seam. Typed binary arrays are out of
  scope. The split (scalar `float` vs type-arg union) is on record and tested; the downstream
  narrowing lands when a real consumer bites.
- **Step 5 — full provider laziness (orthogonal, deferrable).** Flip `TsManifestSymbolProvider` from
  the eager `Map.ofList`-in-ctor index to a lazy per-query index. The `NumberCovariance` decorator
  already resolves + caches at the query seam, so correctness does NOT depend on this; Step 5 is a
  perf/architecture cleanup. Keep separate — do not entangle with the variance work.

#### Retained invariants (carried from the original design — still binding)

- Structural width is **CONFINED to the foreign-call arg position** (`unifyArgCoerce` / its eager
  twin `tryCoerceUpcast`) — NOT a general `subsumes`/`unify` edge. Vesper-internal code cannot
  widen a record to a structural type.
- **Gated on the provider's `IsInterface`** (real TS interfaces and the `@struct` erasing nominals
  both surface as `Class { IsInterface = true }`), so the front end never recognises the `@struct`
  home. A foreign CLASS (`IsInterface = false`) is nominal — construct it — and does NOT admit width.
- Each REQUIRED (non-optional, via `ExternalMember.IsOptional`) member must be supplied by a
  same-named field admitted per Step 1; success ABSORBS (no pin; record/class emits verbatim), else
  fall through to the nominal error.
- The reverse-axis `Map<string,string list>` shape and its sole CLR consumer
  (`EngineCore.canonName`'s `System.Exception → exn` reconciliation, which takes the single element)
  are unchanged.

#### Entry-point consumer shape & fixtures (unchanged)

The pervasive real inflow is the **options/config-object call** against a NAMED foreign interface
(DOM `addEventListener(…, options: AddEventListenerOptions)` / `scrollIntoView(ScrollIntoViewOptions)`;
node `fs.readFile(path, options)`). Vesper has no `{| |}` literal yet, so the argument is a NAMED
record.

- **Isolation fixture:** `interface Options { retries: number; label: string; verbose?: boolean }`
  + `configure(opts: Options)`, called with a Vesper `type Cfg = { retries: int; label: string }`
  value — width (`Cfg` ⊇ required), numeric admission (`int` satisfies `number` via repr-sibling),
  verbatim emit, negatives (missing / `string`-vs-`int` `label` rejected). Plus the scalar
  `configure2(x: number)` ← `int` (family absorption alone). These are the existing
  `StructuralWidenTests` / `NumberFamilyTests`; keep them green across every step.

### G2 — `dynamic` implicit-escape warning — STAGED

`d?foo + 1` silently escapes `dynamic → int` today. Design (in `dynamic-typing-design.md`):
warn on an implicit escape, suppressed by `#nowarn` or a direct `(d?foo : int)` ascription.
Needs tyvar-origin tagging + suppression plumbing. **Decide the syntactic-vs-loose suppression
fork when building.**

### G3 — `retype` surface — OPEN

`retype` (the reinterpret intrinsic that powers `dynamic x`) is currently public `[<AutoOpen>]`
(a general unsafe cast). FSharp.Core keeps `retype` internal; nothing yet depends on it being
public, so it is still cheap to restrict. **Decide public (FFI-friendly) vs internal.**

### G4 — es2015 `Error`-subclass ctor residue — INERT (note, not a step)

Six `Error`-subclass ctors are return-type-divergent; the ctor dedupe keeps the first and
nothing constructs `Error` subclasses, so it is currently harmless. **Revisit only if/when an
`Error` subclass is actually constructed from Vesper.**

### G5 — external INTERFACE heritage is unresolved (two gaps) — OPEN

Surfaced while confirming the G1 `NumberCovariance` relocation is total (`mapProviderTypes`
threads `number` through a class's `FrozenInterfaces` / `FrozenBaseType` heritage args): those
surfaces are threaded, but the FRONT END has no wired consumer for external *interface*
heritage, so a `number` there has no observable end-to-end behaviour (and neither does anything
else declared through interface `extends`). Two independent gaps, both empirically confirmed with
hand-built manifests:

- **Interface-to-interface supertype assignability does not fire at the foreign-arg seam.**
  Passing a value of `interface NumChild extends Base<T>` where a `Base<T>` parameter is expected
  is REJECTED (`Type mismatch`), while the CLASS-base analogue (`class C extends Base<T>` → a
  `Base<T>` param) is ACCEPTED. So the nominal upcast walk reaches an external base CLASS
  (`EngineCore.subtypeParentOf` → `instantiateBaseType`) but not an external interface's
  `extends`-interfaces (`subtypeInterfacesOf` → `instantiateInterfaces` is built but not consulted
  on this path). Anchor: `Passes/Unification/Engine.fs` `tryCoerceUpcast` / `EngineCore.fs`
  `subtypeInterfacesOf`.
- **External INHERITED member reads are not walked.** Reading a member declared on a base
  interface off a subtype receiver (`sub.value` where `value` lives on `Base`) MISSES: the external
  member-access path resolves OWN members only (`Passes/Unification/InferRecordAccess.fs`,
  `ctx.Provider.TryLookupMember(receiverName, …)` — no heritage walk), and the TS provider stores
  heritage as `FrozenInterfaces` / `FrozenBaseType` WITHOUT flattening inherited members into
  `Members` (`TsManifestMembers.build`). The metadata (reflection) layer sidesteps this because
  `GetInterfaces()` / the `inherit` chain surface the transitive set; the TS-manifest layer does not.

Orthogonal to G1 (the numeric-variance relocation is complete and correct regardless). These bite
the moment a real `@types/*` package needs `interface Foo extends Bar<…>` member inheritance or
super-interface assignability — i.e. **§Breadth** (`@types/node` config objects, `Js.Dom`'s deeply
`extends`-chained event/element hierarchy). Build-when-it-bites; pin with an isolation fixture
(`interface Base<T> { m(): T }` + `interface Child extends Base<int>` + a read and an upcall) first.
The precise per-surface `number` resolution these WOULD expose is already pinned structurally by
`MapProviderTypesTests` (`ExternalSymbols.mapProviderTypes`), so no end-to-end number fixture is owed
here — only the inheritance-resolution capability itself.

---

## Breadth destinations (scoped later — do NOT start here)

- **`@types/node`** — module-entry breadth + refs at scale (no ambient mode needed). Where
  lazy-per-symbol extraction starts to earn its keep. A refs-table + scale test; commit a ranked
  diagnostics burndown like `es2015BurndownContract`. Its config-object calls are the natural
  first consumer of **G1**.
- **`Js.Dom`** — the eventual destination (browser is where a JS target earns its keep).
  Ambient-global entry mode at 10× scale, its own namespace, deeply cyclic types. Needs **G1**
  (inflow widening) AND **G5** (its event/element hierarchy is deeply `interface … extends …` —
  super-interface assignability + inherited member reads) on top of the already-landed structural
  identity (Wall 3) + iteration (Wall 4) + ambient mode (R5). **NOTE:** the old "needs SCC" framing was overstated — an
  anonymous structural type cannot self-reference (TS recursion requires a *name*, hashed as a
  leaf), so the shape-hash is acyclic by construction; Dom needs structural *identity*, not
  cycle canonicalisation. SCC stays purely a region/closure concern.

---

## Faithful-later graduations (build-when-it-bites)

These are honest partial-degrades today (they warn, and missing capabilities self-enforce as
resolution errors — sound, not wrong). Each graduates to faithful via the SAME erasing-nominal
machinery Wall 3 built, when a real consumer needs it. No rework of the honest gate.

- **Callable-object-with-props → `Fun`-implementing erasing nominal.** A TS callable object with
  data props (`{ (x): void; prop: string }`) currently falls to `Structural(props)`, DROPS the
  call signature, and warns. Faithful model: an erasing nominal listing `Fun<params,ret>` in its
  interface set (call signature carried) AND the data props as Property members — `f(x)` resolves
  through `Fun`, `f.prop` through member access, both zero-emission. Needs (a) a schema slot for
  the call signature alongside the fields (a `Structural`-node contract bump: Fable + goldens),
  and (b) provider work so application resolves through `Fun` on an EXTERNAL nominal. Its own
  small design pass.
- **Partial structural resolution + a caller-facing warning.** The honest gate keeps a *non-pure*
  structural OPAQUE (`fields = []` → consumer `FTUnknown`) rather than carrying a partial field
  set — the fields would be usable (a caller can read the representable subset), but the
  extractor's incompleteness warning dies at extraction and blanket partial-carry bloats goldens.
  Faithful middle ground: carry the partial fields AND propagate a `partial`/`faithful` bit on the
  `Structural` schema node through the provider to a front-end warn-on-use. Deferred until a
  consumer needs the usable subset of a specific non-faithful type. Many such types are
  "faithful-later" anyway (callable-object above; `Readonly<T>`/`Partial<T>` are near-pure records;
  index signatures → a future dictionary/index capability), so this state is transitional.
- **`retype` override layer** — a composite provider layer that overrides a lossy mapping; lands
  when the first lossy mapping actually bites a real package. (Distinct from the `retype`
  reinterpret intrinsic in Wall 2 — same name, different thing.)
- **`Vesper.Platform.Map` portability layer** — a target-agnostic `Map` forwarding to
  `SCG.Dictionary`/`Js.Map` per target; RECORDED as a possibility, explicitly out of scope
  (`Js.*` stays JS-target-only by design).

---

## When to delete this doc

Delete when **G1** lands and the breadth destinations get their own docs; fold any durable facts
into module headers + [[project_js_ref_pack]] / `reference_*` memories
([[feedback_plan_docs_ephemeral]]). The `dynamic` design is already durably recorded in
`dynamic-typing-design.md` + the `ops-dynamic.js.fsi` / `prim-types-dynamic.js.fsi` headers.

---

## Orientation — files and symbols (outstanding-relevant)

- **G1 seam:** `Passes/Unification/Engine.fs` (`unifyArgCoerce` / `tryCoerceUpcast` — the
  confined foreign-arg absorption home the `TyOr` family-widen and the record→interface width
  check land in); `Passes/Unification/InferExternalCall.fs` (`commitExternalOverload` — the
  arg-binding seam; `obj`-absorption + literal-typar + optional-fill are its siblings).
- **G1 variance / repr family (LANDED):** `Codegen.Js.NumberCovariance` (`wrap` — the provider
  decorator that resolves covariant `number→float` / invariant type-arg → family-union off the
  forward + reverse axes). Reverse axis `IExternalSymbolProvider.IntrinsicReverseCanon`
  (`Map<string,string list>`) built in `VesperLib/TyparCapture.fs` (`intrinsicReverse`) +
  `ReferencedProject.fs`, merged in `ExternalSymbols.mergeReverseCanon`; forward axis
  `IntrinsicForwardRepr` built alongside (`intrinsicForward`). `Engine.reprSiblings` (forward-axis
  width test) + `Engine.numericFamilyOr` (reverse-axis param family). `ExternalSymbols.
  instantiateSignature` / `openSignature` realise the (already provider-resolved) template verbatim.
  Extractor unchanged: `src/Vesper.Ts.Extractor/TypeMap.fs` retains the `number` token the provider
  consumes.
- **G1 provider structural facts:** `TsManifestMembers.fs` (`buildStructuralTypes` — the
  `IsInterface`/@struct erasing nominal + Property members).
- **`dynamic` follow-ups (G2/G3):** `InferGeneralize.applyDefaults` (the SRTP `default`-constraint
  pass `?` rides — the escape-warning tyvar-origin seam); `ops-dynamic.js.fsi` /
  `prim-types-dynamic.js.fsi`; `retype` in Vesper.Core (surface decision).
- **Extractor (structural / faithful-later):** `src/Vesper.Ts.Extractor/TypeMap.fs`
  (`isIntersection` arm — object-only merge; `carriesFaithfullyAsFields` — the pure-record gate a
  callable-object/index-signature graduation would extend); rebuild via the one sanctioned Fable
  command (workflow header).
- **Type-system guardrails:** `PlatformTypes.fs` (`isUnrepresentable` — a JS-only intrinsic must
  have `platform = Some`); Wall 2 added NO SemType case (`dynamic` is `TyConst "dynamic"`) — keep
  it that way.
- **Tests as patterns:** `test/XParsec.FSharp.Codegen.Js.Tests/{StructuralNominalTests,
  StructuralShapeHashTests,IterableForInTests,DynamicTypeTests,JsMapE2ETests,MittE2ETests,
  PrimitiveExprTests}.fs`; `TestHelpers`/`SchemaDsl` scaffolding; `test/Vesper.Ts.Extractor.Tests`
  (goldens, `UPDATE_SNAPSHOTS`, `es2015BurndownContract`, `testProviderResolves`);
  `test/ts-fixtures/{mitt,es2015}/`.

## Relevant memories
[[project_js_ref_pack]] (canonical shipped state),
[[reference_js_external_instance_member_walls]] (R1–R4a baseline + closed walls),
[[feedback_freeze_no_backend_knowledge]] (extractor carries, front end evaluates, backend homes —
the G1 guardrail),
[[feedback_codegen_js_owns_assignability]] (codegen-js owns assignability/intrinsic repr — the G1
oracle lives here),
[[feedback_systematic_tests_over_whackamole]] (isolation fixture before wiring),
[[feedback_durable_knowledge_in_code]] (shipped walls live in code, not this doc),
[[project_arity_overloaded_type_names]] (the arity law),
[[feedback_plan_docs_ephemeral]] (delete this doc when G1 lands),
[[feedback_user_commits]] (user reviews and commits).
