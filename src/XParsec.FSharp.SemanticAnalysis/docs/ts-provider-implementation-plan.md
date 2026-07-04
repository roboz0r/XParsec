# TS provider — outstanding gaps toward `@types/node` and `Js.Dom`

**Status (2026-07-03).** The "faithful real-package consumption" tranche (Walls 1–5,
sequenced BEFORE the two breadth destinations) has essentially **landed**. This doc has been
rewritten to track only what is **NOT built** — the shipped walls are now recorded in the
code itself (module headers + the tests named below), per
[[feedback_durable_knowledge_in_code]]. What remains:

- **One deferred wall step with a settled design** — Wall 3's *inflow structural widening*
  (§G1), held back build-when-it-bites for its first real consumer.
- **Two small `dynamic` follow-ups** (§G2, §G3) and **one inert residue** (§G4).
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

### G1 — Structural inflow widening (Wall 3's last step) — DESIGN SETTLED (2026-07-03)

The one unbuilt Wall 3 step. **Outflow is done** (a TS fn returning `{x,y}` resolves members
through the erasing nominal above). **Inflow** — passing a Vesper record/class *into* a foreign
function whose parameter is structural/interface — still only unifies nominally: at
`InferExternalCall.commitExternalOverload` the sole widenings are the `obj` implicit-box,
`admitLiteralMethodTypars`, and `tryFillOptionalCall`. There is **no** path admitting a Vesper
record to a structural/interface parameter by width, and no path admitting an `int` argument to
a TS `number` parameter.

The design has **two independent pieces**. Both keep JS-specific knowledge in `Vesper.Core`'s
`.js.fs` intrinsics + the provider seam — no backend-convention leak
([[feedback_freeze_no_backend_knowledge]]), no bespoke front-end assignability
([[feedback_codegen_js_owns_assignability]]). The earlier framing (a provider "coercion oracle"
interface member returning a rewritten signature) is SUPERSEDED by the variance model below.

**Piece 1 — TS `number` as a variance-polarized abbreviation.** A JS `number` is genuinely
wider than any one Vesper numeric, but Vesper `float` IS exactly JS `number` semantics. Express
the two faces by *variance*, not a new type:
- The extractor **RETAINS** TS `number` (stop rewriting `number → float` in `TypeMap.fs`),
  emitting `Named("number", [])`.
- `Vesper.Core` declares `type number = float` — a transparent ABBREVIATION (an alias, **not**
  `(# "number" #)`; no new intrinsic, no arithmetic wiring). This is the *source* expression of
  "number = float."
- Resolution is **variance-polarized** (the one piece that must be code — variance is a property
  of *position*, so no type declaration can carry it):
  - **Covariant** (return, property read, annotation) → expand the abbreviation to `float`.
    Return values are real `float`, fully usable; zero new behaviour.
  - **Contravariant** (parameter, and fields of a param-position structural target) → widen to
    the repr FAMILY `TyOr[int; float; float32]` (the canons that share `float`'s platform repr
    `"number"`). An `int`/`float32` argument is then absorbed by the EXISTING `TyOr` rule in
    `unifyArgCoerce` (and its eager twin `tryCoerceUpcast`) — no pin, verbatim emit.
- The family comes from `IExternalSymbolProvider.IntrinsicReverseCanon`, whose shape is
  **CORRECTED to `Map<string, string list>`**: the `{ platform-repr -> canon }` relation is
  genuinely one-to-many on JS (`"number" -> [int; float; float32]`); the old
  `Map<string,string>` lossily collapsed it — the same collapse Wall 1 had to dodge. Its one CLR
  consumer (`EngineCore.canonName`'s `System.Exception -> exn` reconciliation) is single-valued
  per key on CLR and takes the sole element.
- **Directionality is free:** only a slot that was literally `number` (the abbreviation) widens;
  a genuine `float`/`int` parameter stays strict. The variance rule names NO concrete type — it
  is driven entirely by abbreviation resolution + the source-derived repr map, so **Codegen.Js is
  untouched.**

**Piece 2 — record/class → structural-interface admission by width** (the genuinely new edge):
- **CONFINED to the foreign-call arg position** (`unifyArgCoerce`) — NOT a general
  `subsumes`/`unify` edge. Vesper-internal code cannot widen a record to a structural type.
- **Gated on the provider's `IsInterface`** (both real TS interfaces and the `@struct` erasing
  nominals surface as `Class { IsInterface = true }`), so the front end never recognises the
  `@struct` home. A foreign CLASS (`IsInterface = false`) is nominal — construct it — and does
  NOT admit width.
- When `actual` is a Vesper `TyRecord`/`TyClass` and `expected` is an external `IsInterface`
  `TyClass`: for each REQUIRED (non-optional) interface member, the arg must supply a same-named
  field whose type `subsumes` into the interface field's type, translated CONTRAVARIANTLY (so a
  `number` field becomes the family). On success, ABSORB (no pin; the record/class emits
  verbatim). Else fall through to the nominal error.

**Entry-point consumer shape.** The pervasive real inflow is the **options/config-object call**
against a NAMED foreign interface (DOM `addEventListener(…, options: AddEventListenerOptions)` /
`scrollIntoView(ScrollIntoViewOptions)`; node `fs.readFile(path, options)`) — so driving with the
interface case builds the general thing `@types/node`/`Js.Dom` demand. Vesper has no `{| |}`
literal yet, so the argument is a NAMED record.

- **Isolation fixture:** `interface Options { retries: number; label: string; verbose?: boolean }`
  + `configure(opts: Options)`, called with a Vesper `type Cfg = { retries: int; label: string }`
  value. Exercises width (`Cfg` ⊇ required), the numeric family (`int` satisfies `number` via
  `TyOr`), verbatim emit (POJO record), and negatives (missing / `string`-vs-`int` `label`
  rejected). Plus a cheaper scalar test: `configure2(x: number)` ← `int` (the family absorption
  alone, no structural width).
- **Test:** the fixtures above, Node round-trip; `Cfg` with an extra field also passes (width);
  missing/incompatible field rejected.

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

---

## Breadth destinations (scoped later — do NOT start here)

- **`@types/node`** — module-entry breadth + refs at scale (no ambient mode needed). Where
  lazy-per-symbol extraction starts to earn its keep. A refs-table + scale test; commit a ranked
  diagnostics burndown like `es2015BurndownContract`. Its config-object calls are the natural
  first consumer of **G1**.
- **`Js.Dom`** — the eventual destination (browser is where a JS target earns its keep).
  Ambient-global entry mode at 10× scale, its own namespace, deeply cyclic types. Needs **G1**
  (inflow widening) on top of the already-landed structural identity (Wall 3) + iteration
  (Wall 4) + ambient mode (R5). **NOTE:** the old "needs SCC" framing was overstated — an
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
- **G1 variance / repr family:** `IExternalSymbolProvider.IntrinsicReverseCanon`
  (`ExternalSymbols.fs`) — SHAPE CORRECTED to `Map<string,string list>`; built in
  `VesperLib/TyparCapture.fs` (`intrinsicReverse`) and `ReferencedProject.fs`, merged in
  `ExternalSymbols.mergeReverseCanon`, consumed on CLR by `EngineCore.canonName`.
  `ExternalSymbols.instantiateSignature*` translates `.Parameters` (contravariant) vs `.Return`
  (covariant) — the variance-injection points. `Passes/Unification/Translate.fs` expands the
  `type number = float` abbreviation. Extractor: `src/Vesper.Ts.Extractor/TypeMap.fs` (retain
  `number`); `Vesper.Core` prim-types `.fsi` (declare `type number = float`).
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
