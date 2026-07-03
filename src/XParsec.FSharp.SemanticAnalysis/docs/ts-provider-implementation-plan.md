# TS provider — the walls before `@types/node` and `Js.Dom`

**Status (2026-07-03).** R5 tranche‑1 SHIPPED (see below). **Walls 1 and 2 now SHIPPED**
too — but Wall 2 landed via a substantially DIFFERENT design than this doc originally
sketched (a disciplined opaque `dynamic` type + the `?` operator, NOT an infectious
`TyDynamic` SemType). Walls 3–5 remain; **Wall 3 is now DESIGNED** (design pass done
2026-07-03 — see its section) and scoped into four individually-shippable steps. Both
landed walls followed a design pattern worth carrying into the rest: *real types via JS
intrinsics + operators/recognizers, not new SemType DU cases with magic unifier
behaviour.* Wall 3 keeps to it — no new SemType case, no `unify`/`subsumes` change.

This doc scopes the tranche of **walls that make real-package consumption faithful**,
sequenced BEFORE the two breadth destinations — `@types/node` (module-entry at scale) and
`Js.Dom` (ambient at 10× scale).

**Companions:**
- [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md) — the
  original *decisions* doc. Note two sections are now SUPERSEDED by what shipped: its
  §"`any` — the one genuinely new SemType surface" (Wall 2 did NOT add a `TyDynamic`
  SemType — see [`dynamic-typing-design.md`](dynamic-typing-design.md)) and its
  `null`/`undefined`-as-core-intrinsic framing (Wall 1 shipped `undefined` as a JS-only
  intrinsic; `null` needed no entry).
- [`dynamic-typing-design.md`](dynamic-typing-design.md) — the AS-BUILT design for
  `dynamic` (Wall 2). Load-bearing.

Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the
extractor (`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) —
needed whenever a wall touches `src/Vesper.Ts.Extractor/` (Walls 1–2 did NOT). Goldens
regenerate via the skill's `-UpdateSnapshots` against `test/Vesper.Ts.Extractor.Tests`
(consumer tests NEVER run the extractor). Anchors are symbol names, not line numbers —
confirm by reading before editing. Isolation-first is the rule
([[feedback_systematic_tests_over_whackamole]]): every wall ships a hand-built fixture
pinning the behaviour before any golden regen.

---

## What has landed (context, not work)

| Area | State |
|---|---|
| R1–R4a | Manifest nominal → homed `FTClass`; native `receiver.member(args)` lowering; faithful `KeyOf`/`IndexedAccess`/`Conditional`/`Literal` carriers + `Engine.evalTypeLevel` ground folds; call-site literal grounding. mitt full-fidelity gate MET. |
| R5 tranche‑1 | Refs table (`PackageManifest.Refs`, homed `FTClass(TypeKey)`); `extractGlobals`/`extractLibGlobals` ambient mode + class-like fusion; vendored `es2015` pack + `es2015BurndownContract` scoreboard; `TsGlobalHomes.globalLibHomes` (`es2015→Js`) mounting; `ExternalClassFlags.Global` no-import + bare `new Map()`; `JsMapE2ETests` gate. |
| **Wall 1 (SHIPPED, `809901dd`)** | `undefined` distinct from `unit`. See section below — landed as a real JS-only intrinsic, not the RuntimeNames-recognizer the original text proposed. |
| **Wall 2 (SHIPPED, `6790cc3d`)** | `dynamic` (disciplined `any`). REDESIGNED — see section + [`dynamic-typing-design.md`](dynamic-typing-design.md). |
| Free-fn import | CLOSED. `stampValueSymbol` stamps the manifest module spec so single-signature free functions carry `ValueKey(Some spec,…)` and import/emit; overloaded free functions route through the synthetic erased grouping type (`FreeFnOverloadTests`). |
| files-js intrinsic harvest | NEW (Wall 1). `ReferencedProject.buildProviderWith` now harvests intrinsic reprs from `files-<t>` contract companions too (not only base `files`), so a TARGET-ONLY intrinsic (`undefined`, `dynamic` — no CLR analog) publishes as a real `Intrinsic`. Its single `.js.fs` companion is BOTH marker and platform face. |
| Test harness | `TestHelpers` (`stackTs`/`analyseWith`/`emitWith`/`expectNodeOutput`/`MittFixture`) + `SchemaDsl.fs`. NOTE (Wall 1): `stackTs`/`stackTsMany` now aggregate the sources' ambient prefixes (mirroring `ExternalSymbols.composite`), so a JS-only intrinsic resolves by bare name in tests exactly as in production. |

---

## The walls

### Wall 1 — `undefined` distinct from `unit` — SHIPPED (`809901dd`)

Landed, but NOT as this doc first proposed (a `RuntimeNames`-name recognizer in
`canonName`). AS-BUILT: `undefined` is a **real JS-only intrinsic** —
`prim-types-undefined.js.fsi` (`type undefined = extern`) + `.js.fs`
(`type undefined = (# "undefined" #)`), scoped to `manifest.toml` `files-js`. Published as
`Intrinsic(canon="undefined", platform=Some "undefined")` via the new files-js harvest
(above). This makes `canonName "undefined"` resolve forward through the provider BEFORE
the reverse-canon map (which maps unit's JS repr `"undefined"` back to `unit`) can collapse
it — so `unit`/`undefined` are distinct identities in `subsumes`, exactly like `int`/`float`
are distinct yet both repr as JS `number`. The value-level repr coincidence (both emit JS
`undefined`) stays a backend fact.

- `null` needed NO entry: it is a reserved keyword (`type null = extern` cannot parse) and
  was already a distinct identity from `unit` (nothing maps to the reverse-canon key
  `"null"`); its `Type.Null`/`Expr.Null` value path is untouched.
- Mitt no-payload-emit GAP flipped accept→reject for a `unit`-typed event
  (`UnannotatedMittTests`); `UndefinedIdentityTests` pins all directional cases;
  `NullUndefinedTests` stays green.

### Wall 2 — `dynamic`, a disciplined F# `any` — SHIPPED (`6790cc3d`)

**Superseded the original `TyDynamic`-SemType plan.** The infectious-`any` /
absorbs-on-unification / `x.foo`-admits-any-member design was spiked and REVERTED. F# is
not fast-and-loose with types and we respect that. AS-BUILT (see
[`dynamic-typing-design.md`](dynamic-typing-design.md) for the full rationale):

- `dynamic` is a **plain opaque JS intrinsic** (`type dynamic = (# "any" #)`, `files-js`),
  the F#-side landing point for TS `any` (`Schema.TypeRef.Dynamic → FTConst "dynamic"`). It
  has NO special unifier behaviour and **zero assignability edges** — you cannot silently
  enter or leave it. There is NO `TyDynamic`/`FTDynamic` SemType case.
- **Enter:** `dynamic x` (an erasing conversion built on `retype`, `(# "" x : 'U #)` — the
  identity intrinsic, now honoured on the JS backend: an empty-template intrinsic emits its
  operand verbatim).
- **Access:** ONLY the F# dynamic operator — `x?foo` / `x?foo <- v` desugar (in
  `Infer`/`FreezeExpr`) through real `op_Dynamic` / `op_DynamicAssignment` operators
  (`ops-dynamic.js.fsi`, bodies `$0[$1]` / `$0[$1] = $2` → computed member access). Their
  SRTP `default ^TResult : dynamic` gives infectious-by-default (chains stay dynamic) +
  target-typed-on-pin (`let n : int = d?foo` pins `^TResult` to int) — riding the existing
  `InferGeneralize.applyDefaults` pass with zero new inference machinery.
- **`.foo` (dotted) on a `dynamic` ERRORS** — the load-bearing F#-fidelity point.

**Two follow-ups (NOT yet built):**
1. **Implicit-escape warning (STAGED).** `d?foo + 1` silently escapes `dynamic → int`
   today. Design (in `dynamic-typing-design.md`): warn on an implicit escape, suppressed by
   `#nowarn` or a direct `(d?foo : int)` ascription. Needs tyvar-origin tagging +
   suppression plumbing. Decide the syntactic-vs-loose suppression fork when building.
2. **`retype` surface [OPEN].** Currently public `[<AutoOpen>]` (a general unsafe cast).
   FSharp.Core keeps `retype` internal; nothing yet depends on it being public, so it is
   still cheap to restrict. Decide public (FFI-friendly) vs internal.

Additive parser support that landed with it: `OperatorNames.ofToken` maps `?`/`?<-` →
`op_Dynamic`/`op_DynamicAssignment`; `Token.isOperatorKeyword` accepts `OpDynamicAssignment`
so `(?<-)` parses as a paren-operator binding head.

### Wall 3 — structural object → content-hash *erasing* nominal — DESIGNED (2026-07-03)

Design pass done (superseded the original `TyRecord(hash,…)`-SemType sketch below).
Re-derived against the Walls 1–2 pattern and settled with the user. **No new SemType
case; no `unify`/`subsumes` change.** The current state is NOT the doc's old "rejected /
`obj` stub" text: the extractor already routes an anonymous `{x:number,y:number}` to
`Schema.TypeRef.Structural(printed, fields)` with **fields harvested** (gated on genuine
`TypeFlags.Object`), and the provider rehydrates it to an opaque `FTUnknown("structural:"
+ printed)` — **fields dropped**. Wall 3 stops dropping the fields and gives the shape a
real, resolvable identity.

**The design (as settled):**

- **Identity = content-hash nominal equality** over an **acyclic structural shape-hash**:
  sorted field-name list + each field type's hash, recursively, with **named refs as
  leaves** (hashed by name, never expanded). Field-order-invariant, so `{x;y}` ≡ `{y;x}`,
  and (Step 4) an object-intersection's merged shape ≡ the equivalent flat literal. The
  tsc-`printed` string demotes to a **diagnostic side-table name only**, never the
  identity key.
- **Representation = an anonymous, hash-keyed *erasing* foreign nominal.** NOT a
  Vesper-emitted record `TDecl` (we must never emit a `new SyntheticHash(...)` class). It
  rides the **existing provider-homing mechanism** — the same machinery that homes mitt's
  `Map` / the es2015 classes as `FTClass(TypeKey)` whose members resolve + lower to native
  JS access while the *type itself* emits nothing. This IS Fable's "interface that erases
  at compile time," reached through machinery R5 already built, extended from *named*
  foreign types to *anonymous* ones via the hash key. So `Structural(hash, fields)`
  rehydrates to `FTClass(SyntheticHashKey)`, `.x` resolves, and nothing is emitted for the
  shape.
- **Both value flows have zero footprint for the shape.** *Outflow* (a TS fn returns
  `{x,y}`): the value is a foreign POJO; the descriptor supplies member resolution.
  *Inflow* (you pass a Vesper `Point`): records emit as data-only JS classes
  (`EmitJs.fs:22`), so a `Point` instance already carries own `x`/`y` props and flows
  **verbatim**. Neither path constructs the structural type — it correctly has no ctor.
- **Inflow admissibility = a checked widening CONFINED to the foreign-call arg position**
  (`InferExternalCall.fs`), NOT a general lattice edge. `unify`/`subsumes` are untouched;
  Vesper-internal code cannot silently widen a record to a structural type (and there is
  no Vesper-internal structural target anyway — the only ones that exist are foreign
  signatures). The rule: a Vesper record (or another structural) whose fields ⊇ the
  param's, each field boundary-compatible, may be passed to a foreign function — width
  subtyping allowed, checked field-wise, emitted verbatim. `int`/`float` satisfying a TS
  `number` field (`prim-types-min.js.fs:22`, `int = (# "number" #)`) is a repr-compatible
  **boundary** coercion at the same site, not a global `int <: float` claim.
- **Cycles are NOT reachable under this design — so SCC is not pulled in.** An anonymous
  structural type cannot self-reference: recursion in TS requires a *name*
  (`interface Node { children: Node[] }` is nominal `Node`, hashed as a leaf, never
  inlined). Named refs being leaves makes the shape-hash acyclic **by construction**;
  the recursion bound is the finite schema tree. `brainstorm-tarjan-scc.md`'s canonical
  numbering would only be needed if we chose to unify structurally-equal *named* types,
  which we deliberately do not — so SCC stays purely a region/closure concern. (This
  also means the Wall 5 / `Js.Dom` "needs SCC" note is overstated: Dom needs Wall 3's
  structural identity, not cycle canonicalisation.)

**Steps — each individually builds green, is isolation-tested, and commits on its own:**

1. **Canonical shape-hash, still opaque (provider F#-only; no extractor/Fable/golden
   change).** Compute the acyclic shape-hash from the `Structural(_, fields)` schema in
   `TsManifestTypes.toFrozen`; keep rehydrating to `FTUnknown("structural:" + hash)` but
   with `hash` now canonical. Because `FTUnknown` unifies by name equality
   (`SemanticInfo.fs:966`), this alone makes two field-order-permuted shapes **unify as
   opaque** — identity without member resolution yet. Keep `printed` as the diagnostic
   name. **Test:** two permuted anonymous shapes share one frozen identity; a nested
   shape hashes stably; a named-ref field stays a leaf.

2. **Erasing anonymous nominal + member resolution (the meat).** Mint the hash-keyed
   erasing foreign nominal (member descriptor from `fields`) into the provider registry;
   rehydrate `Structural → FTClass(SyntheticHashKey)`; homed so `.x` resolves and lowers
   to native access, emitting nothing for the type. **Test:** a fixture foreign fn returns
   `{x:number,y:number}`; read `.x`; emit + Node round-trip observes the value. Two
   distinct fns returning the same shape resolve to one identity.

3. **Foreign-call arg widening (inflow).** At `InferExternalCall.fs` arg binding, admit a
   record/structural whose fields ⊇ the structural param's, each boundary-compatible
   (incl. `int`/`float` → `number`), emitting the operand verbatim. **Test:** pass a
   Vesper `Point` to a foreign fn wanting `{x,y}`, Node round-trip; a `Point3D{x,y,z}`
   also passes (width); a missing/incompatible field is **rejected** (negative test).

4. **Intersection graduation (extractor + Fable rebuild + goldens).** Change the
   `t.isIntersection()` arm (`TypeMap.fs:273`): an **object-only** intersection harvests
   the checker's merged `getPropertiesOfType` → `Structural(printed, mergedFields)`
   instead of erasing to `obj`; non-object intersections (function&, branded) stay erased.
   The Step 1 shape-hash then makes `{x:number} & {y:number}` intern to the **same
   identity** as a flat `{x:number,y:number}`. **Test:** an intersection param/return
   unifies with the flat literal + resolves members; `IntersectionErased` golden flips for
   the object case; the non-object case still warns.

### Wall 4 — iteration: TS iterables → JS `for … of` (`Symbol.iterator`)

**Why.** Iterables are pervasive (`Js.Map` entries, node streams, DOM node lists). The
`Js.Map` gate explicitly descoped iteration; the capability-interface machinery exists but
is not wired to TS iterables. NOTE: this is the JS-target `for..of` path, DISTINCT from the
CLR `for..in`/`GetEnumerator` work in [`get-enumerator-gaps.md`](get-enumerator-gaps.md) —
do not model it on the IL `MoveNext`/`constrained.` enumerator descriptor.

**Goal.** `for x in (m: Js.Map<_,_>)` and any TS `Iterable<T>`/`IterableIterator<T>` lowers
to a native JS `for (const x of src)`.

1. Recognise the TS `[Symbol.iterator](): Iterator<T>` member as the JS-target iterable
   capability (the extractor already carries members — confirm `Symbol.iterator` survives
   extraction; it may currently drop as a computed/symbol-keyed member).
2. Front end: resolve `for x in src` against that capability (the JS analog of
   `Infer.tryForInEnumerator`), taking the element type from the `Iterable<T>` typar /
   iterator `next()` payload.
3. EmitJs: lower to `for (const x of src) { … }` — native, no enumerator object.

**Trap.** JS iteration is native `for..of`; there is no `GetEnumerator`/`MoveNext` walk.
`Symbol.iterator` is a computed member key — the extractor's member enumeration must not
silently drop symbol-keyed members.

**Test.** Isolation — a Vesper `for x in m` over a `Js.Map` (or a fixture `Iterable`),
emitted + run under Node, summing entries; remove the iteration descope caveat from the
`JsMapE2ETests` header when it closes.

### Wall 5 — codegen-correctness residue that bites real callbacks

Independent of the provider, but blocks FAITHFUL real-package programs (event callbacks
mutate captured state), so it belongs in this tranche.

- **Module-level `let mutable` captured in a JS closure emits wrong** — the cell is read as
  `x.contents` but declared as a bare value, so a lambda that writes a module-level mutable
  produces incorrect JS. Currently sidestepped in tests via the recorder external object
  (the mitt/Bus idiom). Anchor: the ref-cell promotion decision for module-level mutables
  under closure capture (`RefCellPromotion.fs`). **Test:** a Vesper program with a
  module-level `let mutable` written from inside a lambda, run under Node, observes the
  mutation — replacing the recorder sidestep.

- **es2015 provider-load residue (INERT — note, not a step).** Six `Error`-subclass ctors
  are return-type-divergent; the ctor dedupe keeps the first and nothing constructs `Error`
  subclasses, so it is currently harmless. Revisit only if/when an `Error` subclass is
  actually constructed from Vesper.

---

## After this tranche (scoped later — do NOT start here)

- **`@types/node`** — module-entry breadth + refs at scale (no ambient mode needed).
  Where lazy-per-symbol extraction starts to earn its keep. A refs-table + scale test;
  commit a ranked diagnostics burndown like `es2015BurndownContract`.
- **`Js.Dom`** — the eventual destination (browser is where a JS target earns its keep).
  Ambient-global entry mode at 10× scale, its own namespace, deeply cyclic types — needs
  Walls 3 (SCC) and 4 (iteration) LANDED, on top of the ambient mode from R5.
- **`retype` override layer** — a composite provider layer; lands when the first lossy
  mapping actually bites a real package. (Distinct from the `retype` reinterpret intrinsic
  shipped in Wall 2 — same name, different thing: this is the provider-override mechanism.)
- **`Vesper.Platform.Map` portability layer** — a target-agnostic `Map` forwarding to
  `SCG.Dictionary`/`Js.Map` per target; RECORDED as a possibility, explicitly out of scope
  (`Js.*` stays JS-target-only by design).

Delete this doc when Walls 3–5 land; fold durable facts into module headers +
[[project_js_ref_pack]] / `reference_*` memories ([[feedback_plan_docs_ephemeral]]). The
`dynamic` design is already durably recorded in `dynamic-typing-design.md` + the
`ops-dynamic.js.fsi` / `prim-types-dynamic.js.fsi` headers.

---

## Orientation — files and symbols

- **Type system:** `SemanticInfo.fs` / `SideTypes.fs` (`SemType`/`FrozenType`);
  `RuntimeNames.fs` (`nullTypeName`/`undefinedTypeName`); `PlatformTypes.fs`
  (`isUnrepresentable` — the `Intrinsic(platform=None)` trap; a JS-only intrinsic must have
  `platform = Some`). NOTE: Wall 2 added NO SemType case — `dynamic` is `TyConst "dynamic"`.
- **Vesper.Core contracts (the landed pattern):** a target-only intrinsic is a
  `<name>.js.fsi` (`type X = extern`) + `<name>.js.fs` (`type X = (# "repr" #)`) in
  `manifest.toml` `files-js`, harvested as marker+platform face. Examples:
  `prim-types-undefined.js.*` (Wall 1), `prim-types-dynamic.js.*` + `ops-dynamic.js.*`
  (Wall 2). Harvest: `ReferencedProject.buildProviderWith`.
- **Unification:** `Passes/Unification/{Engine,EngineCore,Subsume}.fs`
  (`canonName` reverse-canon — the Wall 1 seam; `evalTypeLevel`/`subsumes` folds);
  `InferGeneralize.applyDefaults` (the SRTP `default`-constraint pass Wall 2's `?` rides);
  `InferApp.inferDynamicLookup`/`inferDynamicSet` (the `?`/`?<-` desugar);
  `InferExternalCall.fs` (overload commit / literal grounding).
- **Iteration:** `Infer.tryForInEnumerator` (`Passes/Unification/InferControlFlow.fs`) is
  the CLR shape — Wall 4 adds the JS-target analog; `EmitJs.fs` for the `for..of` lowering.
- **Extractor (Wall 3+ only):** `src/Vesper.Ts.Extractor/` — `mapType` (`any → Dynamic`
  already done; structural object → content-hash for Wall 3; classify by field presence,
  not raw `TypeFlags`), member enumeration (`Symbol.iterator` survival for Wall 4),
  `Diagnostics.classifyHome/classifyKind`. Rebuild via the one sanctioned Fable command
  (header). NOTE: `Schema.TypeRef.Dynamic` already exists + has a golden — Wall 2 only
  changed the F#-side deserialize (`TsManifestTypes.toFrozen`).
- **Provider/emission:** `TsManifestProvider.fs` / `TsManifestTypes.fs` (`toFrozen`);
  `JsRuntime.fs` (`JsImports.addRef`); `EmitJs.fs` (member lowering, external-new,
  `validatePlatformTypes`, the empty-template identity intrinsic for `retype`);
  `RefCellPromotion.fs` (Wall 5 mutable-capture).
- **Tests as patterns:** `test/Vesper.Ts.Extractor.Tests` (goldens, `UPDATE_SNAPSHOTS`,
  `es2015BurndownContract`, `testProviderResolves`);
  `test/XParsec.FSharp.Codegen.Js.Tests/{JsMapE2ETests,MittE2ETests,UnannotatedMittTests,
  UndefinedIdentityTests,NullUndefinedTests,DynamicTypeTests,TypeLevelFoldTests}.fs`;
  `TestHelpers`/`SchemaDsl` scaffolding; `test/ts-fixtures/{mitt,es2015}/`.

## Relevant memories
[[project_js_ref_pack]] (R5 tranche‑1 shipped — canonical state),
[[reference_js_external_instance_member_walls]] (R1–R4a baseline + closed walls),
[[reference_null_undefined_already_survive_js]] (Wall 1 background — note it predates the
as-built real-intrinsic approach),
[[reference_intrinsic_repr_overloaded_canonname_codegen]] (the primitive-overlap trap class),
[[feedback_freeze_no_backend_knowledge]] (extractor carries, front end evaluates),
[[feedback_systematic_tests_over_whackamole]] (isolation tests before wiring),
[[feedback_codegen_js_owns_assignability]] (codegen-js owns assignability/intrinsic repr),
[[project_arity_overloaded_type_names]] (the arity law),
[[feedback_plan_docs_ephemeral]] (delete this doc when the walls land),
[[feedback_user_commits]] (user reviews and commits).
