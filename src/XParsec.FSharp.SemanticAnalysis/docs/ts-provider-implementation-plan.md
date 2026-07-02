# TS provider — remaining work (real Vesper code consuming a manifest, end-to-end)

**Status:** Phases 0–5 of the original sequence have LANDED (resilient extraction →
faithful generics → `mitt` golden → free-function/default-export emit). What remains is
the part that actually matters: **compile real Vesper code that USES a manifest-exported
API — driving semantic analysis through `IExternalSymbolProvider` and lowering through
Codegen.Js — and run it against the real package under Node.** Today only a *free
function / default-export factory* round-trips; calling an external object's **instance
members** (`emitter.on(...)`, `emitter.emit(...)`) from Vesper does not work yet.

**Companion:** [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md)
holds the *decisions* (the why). This doc is the *runnable sequence* for a fresh session.
Ephemeral per the repo convention — delete once the remaining milestones land and fold
durable facts into module headers / memory.

Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the
extractor (`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) — only
needed if you change the extractor, which the remaining work does NOT. Line/anchor numbers
below are from this writing — confirm by reading before editing.

---

## What has landed (context, not work)

| Area | State |
|---|---|
| Manifest schema + codec (`Vesper.Ts.Manifest.Schema`) | `diagnostics` channel; `TypeRef.MethodTypar`; Fable-safe; `SchemaVersion = 1` (no bump while prototyping) |
| Resilient extractor (`Vesper.Ts.Extractor/Extractor.fs`) | per-type `failwith` → `Warning` + degrade; fatal plumbing still throws; spans relativized to the extraction root |
| Faithful generics | method-axis typars (`MethodTypar i` → `FTTypar(TyparAxis.Method,i)`), TS function types → `TypeRef.Fun`, structural-degrade cascade killed + diagnostics deduped |
| `mitt@3.0.1` fixture (`test/ts-fixtures/mitt/`) | vendored `.d.ts` + `package.json` + `dist/mitt.mjs` + golden; golden pins the honest **5-warning** residue (keyof / indexed-access / conditional), asserted by `mittDiagnosticsContract` |
| Provider (`Codegen.Js/TsManifestProvider.fs`) | `providerOfManifest`; interfaces/classes/aliases/enums/free-functions/variables → seam shapes; free-function + value symbols now carry a real module-spec `Origin`/`Key` |
| Free-function / default-export emit | `let factory = mitt` emits `import $_mitt from "./mitt.mjs"` and runs the real runtime under Node; default-import lowering (`JsImports.createWithDefaults` + `TsManifestProvider.defaultValueKeys`); `TastLower.etaExpand` preserves the External value node's `SymbolKey` |
| null/undefined | already survive JS emit as bare `TyConst`; NOT registered as intrinsics (would self-trip `PlatformTypes`); distinct from `unit` |

**The honesty gap to close.** `test/XParsec.FSharp.Codegen.Js.Tests/MittE2ETests.fs`
proves only the *factory binding* path: its Vesper program is literally `let factory =
mitt`, and a hand-written **JS harness** does `emitter.on(...)` / `emitter.emit(...)`.
That is NOT the goal — the manifest exists so that *Vesper source* can call the external
API and have it type-check + lower. The remaining milestones make that real, then rewrite
`MittE2ETests` so the member calls live in the **Vesper** program, not the harness.

---

## The three walls (root cause of the remaining work)

A TS `Named("Emitter", …)` from the manifest currently becomes `FTConst → TyConst`
(`TsManifestProvider.toFrozen`, `TsManifestProvider.fs:26-27`; `FrozenTypeBridge.instantiateWith`,
`SemanticInfo.fs:866`). Three things break for instance-member use:

1. **Front-end member access rejects a `TyConst`.** `resolveFieldStep`
   (`Passes/Unification/InferRecordAccess.fs:237`) resolves `.member` only on
   `TyClass`/`TyRecord`/`TyUnion`/`TyVar`; a `TyConst` falls to the catch-all
   *"Cannot read member from non-record non-class type"* (`:403`). The type IS registered
   (the `Emitter` interface → `ExternalTypeShape.Class { IsInterface = true; Members = [on;off;emit] }`),
   so once the receiver is a `TyClass(key,…)` the existing `TryLookupMember(s)` path resolves
   the member. The blocker is purely that `toFrozen` emits `FTConst`, not `FTClass`, for a
   name the manifest knows is a class/interface.
2. **Codegen lowers external instance/interface members as receiver-first free-fn imports.**
   `EmitJs.fs` lowers an external member as `$on(emitter)` imported from the module — the
   convention Vesper's OWN emitted runtimes use. A third-party object has genuine prototype
   methods (`emitter.on(t,h)`) and no such export. Note `EmitJs.fs:595-599` ALREADY has the
   right shape for *local* interfaces: `ctx.LocalInterfaces.Contains(...)` → `attachedAccess`
   → `receiver.member(args)`. The work is routing *external manifest* interface/class members
   through an equivalent native-object-method lowering.
3. **The generic is never grounded.** `mitt<Events>()` and `on`/`emit` leave `Events`
   unsolved (their signatures use a method typar + the degraded `keyof Events` / `Events[Key]`
   structural stubs, which are opaque `FTUnknown`). A use site needs either an annotation or a
   non-generic external type to exercise first.

---

## Ordering at a glance

```
R1  external nominal → FTClass ........ front-end: toFrozen emits FTClass for manifest classes/interfaces
R2  external object-method lowering ... Codegen.Js: external instance members → receiver.member(args)
R3  real Vesper e2e (R1+R2 → R3) ...... a Vesper program CALLS the external API; rewrite MittE2ETests
R4  ★ mitt FULL-FIDELITY GATE ......... ZERO-degradation extraction + full mitt API driven from Vesper source
R5  coverage + breadth (GATED on R4) .. @types/node diagnostics golden; ambient-global (DOM) entry mode
```

R1 and R2 are independent and can land in either order, but R3 needs both. Start with a
**non-generic external object** fixture (sidestep wall #3) to land R1+R2+R3 on easy mode,
THEN return to mitt's generics.

**R4 is a hard gate: no second JS package until mitt is complete.** mitt is the proving
ground — it must extract with **zero diagnostics** (every keyof / indexed-access /
conditional construct faithful, NOT degraded) AND have its full public API exercised by
tests that compile real Vesper source. R5 (breadth / `@types/node` / DOM) does not begin
until R4 passes. This reverses, *for mitt*, the earlier "stays degraded by design" deferral
of keyof/indexed/conditional — that deferral was the v1 expedient; the standing decision now
is that the foundation must be fully faithful on one real package before accumulating more.

Land each phase green before the next.

---

## R1 — external nominal types resolve as classes (front-end)

**Goal.** A manifest `Interface`/`Class` name resolves to `TyClass(key, args)`, so
`resolveFieldStep` admits `.member` access and the existing `TryLookupMember(s)` path
(already wired for the manifest provider) returns the member.

**Steps.**
1. In `TsManifestProvider.toFrozen` (`TsManifestProvider.fs:24`), the `Named(name, args)`
   arm must emit `FTClass(SymbolKey.TypeKey(Some moduleSpec, nsPath, name), args)` when
   `name` resolves to a class/interface in THIS manifest, and keep `FTConst(name, args)`
   for everything else (primitives, unresolved/cross-package names, type aliases — aliases
   stay transparent via `ExternalTypeShape.Abbrev`). `FTRecord` is the records analog if/when
   a manifest emits record-shaped nominals; interfaces+classes both use `FTClass` (the seam
   stores both as `ExternalTypeShape.Class`).
2. `toFrozen` is a top-level `let rec private` with NO access to the manifest's type table —
   thread a resolver into it. The kind/key data already exists inside `providerOfManifest`
   as `typeKinds` / `kindOf` (`TsManifestProvider.fs:462-472`). Build the resolver as a
   `Map<string, SymbolKey>` keyed by QUALIFIED name (`qualify nsPath name`) in the SAME pass
   that builds `typeKinds`, and have it return the key directly (`name -> SymbolKey option`)
   — minting the key inside `toFrozen` from parts would duplicate the nsPath/simple-name
   decomposition. Threading options: (a) a resolver param on `toFrozen` — note the
   TRANSITIVE caller chain: `paramsFrozen`, `signatureOf`, `expandCtor`, `expandMethod`,
   `toExternalMembers`, `classifyHeritage`, `toTypeShape`, `toFunctionSymbol`,
   `toValueSymbol` ALL thread it; or (b) move `toFrozen` inside `providerOfManifest` —
   which drags that same private-helper chain inside with it. The diffs are comparable
   (neither is meaningfully "less churn"); prefer (a) — it keeps the helpers top-level and
   testable. Churn note: `classifyHeritage` calls `toFrozen` on heritage entries (`:221`),
   so base-class refs also become `FTClass` — desirable, but it will surface in any test
   that pins provider-emitted `FrozenType` shapes.
3. **Forward references.** `Emitter` is declared AFTER `mitt` in mitt's `.d.ts`, and a
   member signature of one export can name another export. Build the name→key/kind table in
   a FIRST pass over all flat exports (it already is — `typeKinds` is computed before the
   per-export `toTypeShape`), so the resolver sees every type regardless of declaration
   order. Confirm `toFrozen` consults the COMPLETE table, not a partially-built one.
4. The `TypeKey` must match what `toTypeShape`'s `build` already mints
   (`SymbolKey.TypeKey(Some moduleSpec, nsPath, name)`, `TsManifestProvider.fs:292`). The
   operative identity equation: `SymbolKeyOps.qualifiedName mintedKey` must EQUAL the
   provider's map key (`qualify nsPath name`) — that is the exact string the front end's
   external `TyClass` arm hands to `TryLookupMember` (`InferRecordAccess.fs:276-278`). If
   the two spellings diverge, the member lookup silently misses and falls to
   "Unknown class type".

**Acceptance.**
- A new front-end (`Codegen.Js.Tests` or `SemanticAnalysis.Tests`) case: a Vesper program
  with a value annotated at a manifest interface type accesses an instance member
  (`let e : Emitter = … in e.emit`) and TYPE-CHECKS — no "non-record non-class" diagnostic;
  the member's signature resolves through the provider. Use a NON-generic fixture interface
  first (e.g. a hand-authored manifest `interface Box { get(): int; set(x: int): unit }`) to
  isolate R1 from wall #3.
- All existing tests stay green — `FTConst`→`FTClass` only for names the manifest registers
  as classes/interfaces; primitives/aliases unaffected (guard with the kind resolver).

**Trap.** Do NOT emit `FTClass` for a primitive or an alias name — only for a registered
class/interface. An alias (`type Handler<T> = …`) is an `Abbrev`, transparent; turning it
into `FTClass` would break alias expansion. Gate strictly on `kindOf name = Some _`.

---

## R2 — external object-method lowering (Codegen.Js)

**Goal.** An instance-member call on an external manifest object lowers to a NATIVE
`receiver.member(args)`, not a receiver-first free-fn import (`$on(receiver)`), so the
emitted JS calls the real object's prototype method.

**Steps.**
1. Read the existing external-member lowering in `EmitJs.fs`: the external receiver-first
   path is the `TExprG.ExternalMember` arm (`:630-676` — mangled `addMemberRef` import +
   `$Member(receiver)` call). `attachedAccess` + the `ctx.LocalInterfaces` guards
   (`:595-599`) emit the native `receiver.member(args)` shape this work needs; the LOCAL
   `FieldGet`/`PropertyGet`/`MethodCall` arms (`:524-626`) are the contrast.
2. **The args are NOT at the node.** `TExprG.ExternalMember` carries only the receiver;
   call arguments arrive through enclosing curried `App`s (the arm's own comment,
   `:628-629`). `attachedAccess` works for local interfaces because `TExprG.MethodCall`
   CARRIES its args — the external node does not, so the native lowering must recover them
   from the application spine. Two routes: (a) a head-case in the `App` arm's spine
   collapse (`:444+` already calls `TastWalk.collectSpine` to flatten module-function
   calls) that recognizes an external-manifest `ExternalMember` head and folds the whole
   spine into `receiver.member(a, b, …)`; or (b) an upstream TAST change so external
   manifest member calls carry their args on one node. Prefer (a): it is backend-local
   (per the freeze/backend-knowledge separation) and follows the existing flat-call
   precedent; (b) touches the shared TAST and the CLR backend for a JS-only need.
3. **`this`-binding when the member ESCAPES.** A bare member extraction (`let f =
   emitter.on` … later `f t h`) must NOT emit a detached `emitter.on` — JS loses `this`
   on extraction. When the `ExternalMember` arm is reached WITHOUT an applying spine (the
   member escapes as a value), eta-wrap it: `(...args) => receiver.member(...args)` (or
   `.bind(receiver)`). mitt happens to be closure-based and would not catch this bug — do
   not let that mask it; pin it with a fixture whose method reads `this`.
4. Distinguish a **third-party manifest object** (native methods, `receiver.member(args)`)
   from **Vesper's own emitted runtime** (receiver-first free-fn imports). The signal:
   the member's `Origin`/`SymbolKey` home assembly is a TS-manifest module (the provider
   stamps `ExternalMember.Origin = originFor moduleSpec nsPath`; an `InterfaceMethod`/`Method`
   from a `TsManifestProvider` type shape). Decide where the signal lives — likely a flag on
   the resolved `ExternalMember` (e.g. the existing `ExternalClassFlags`, or a new "native
   object members" bit) set by `TsManifestProvider` and read in `EmitJs`, mirroring how the
   `Erased` flag drives `isErasedGroupingType`. Keep the target-dialect knowledge in the
   backend (Codegen.Js), per the freeze/backend-knowledge separation — the provider says
   "these are native object methods," EmitJs decides the JS form.
5. Static members and constructors: a manifest `Class` static method lowers to
   `Cls.method(args)` (or the bare import for an erased grouping type — already handled);
   `new` lowers to `new Cls(args)` against the default/named import. Scope R2 to INSTANCE
   members first (what mitt needs); note static/ctor native lowering as a follow-up if the
   first fixture doesn't need it.

**Acceptance.**
- Emitting a Vesper program that calls an external instance member produces
  `receiver.member(args)` in the JS (assert on the emitted source), and running it against a
  hand-authored runtime whose object HAS that prototype method returns the right value under
  Node (`runNodeFiles`). Use the non-generic `Box` fixture from R1.
- The existing Vesper-runtime receiver-first lowering is UNCHANGED for non-manifest members
  (no regression in the broader Codegen.Js suite).

---

## R3 — the real Vesper e2e (rewrite `MittE2ETests`)

**Goal.** A Vesper program that creates a mitt emitter, registers a handler, AND emits an
event — ALL in Vesper source — emits JS that runs against the real vendored `mitt.mjs` under
Node and observes the handler firing. This replaces the harness-driven scenario; the member
calls move INTO the Vesper program.

**Steps.**
1. R3 is the FIRST e2e and is allowed to be provisional — its job is to prove the R1+R2
   consumption machinery end-to-end, NOT to reach mitt fidelity (that is R4's gate). Land it on
   the easiest path:
   - Prefer a **non-generic** hand-authored emitter-style fixture (e.g. `Bus { on(name, handler);
     emit(name, payload) }` with plain types) so R1+R2 are exercised by a Vesper program that
     genuinely calls instance members, with zero generics in the way.
   - If you point R3 at mitt directly, you may temporarily annotate the emitter at a concrete
     `Events` and tolerate that `keyof`/`Events[Key]` are still degraded `FTUnknown` — but this
     is a STEPPING STONE only. R4 forbids that degradation; do not treat a degraded-mitt R3 as
     done. Note precisely what mitt's generic surface can't yet express and carry it into R4.
2. Rewrite `test/XParsec.FSharp.Codegen.Js.Tests/MittE2ETests.fs`: the Vesper `program` string
   contains the `on`/`emit` calls; the JS "harness" shrinks to just `import` + invoking the
   emitted entry + asserting the observed value (or the program prints the sentinel itself).
   Keep the hard rules: read ONLY committed files (`mitt.manifest.json` + `dist/mitt.mjs` +
   the `.fs` program); NEVER run the extractor; NO `ProjectReference` to `Extractor.Tests`.
   Seed `JsImports.createWithDefaults` with `TsManifestProvider.defaultValueKeys manifest`.
3. Update the test's header comment to state truthfully what is now Vesper-driven vs. any
   residual harness glue.

**Acceptance.** Node exits 0 and the handler-observed value is asserted, with the `on`/`emit`
calls originating in the Vesper program (grep the emitted JS to confirm the member calls came
from compiled Vesper, not hand-written harness lines).

---

## R4 — ★ mitt full-fidelity gate (REQUIRED before any other package)

**The bar (both halves must hold):**
1. **Zero-degradation extraction.** mitt's golden `Diagnostics` must be `[]` — every one of
   the five residual constructs (`keyof Events`, `keyof T`, `T[keyof T]`, `Events[Key]`, and
   the conditional `undefined extends Events[Key] ? Key : never`) is represented FAITHFULLY,
   not as a `structural-object-stubbed` degrade. Flip `mittDiagnosticsContract` to assert
   `man.Diagnostics |> List.isEmpty`.
2. **Full API driven from Vesper source.** mitt's complete public surface — `mitt()` (factory),
   `emitter.on`, `emitter.off`, `emitter.emit`, and `emitter.all` — is exercised by tests whose
   **Vesper program** (not a JS harness) makes the calls, emits via Codegen.Js, and runs against
   the real vendored `mitt.mjs` under Node with asserted behaviour. Multiple event types
   (`Events` with ≥2 keys of different payloads) to prove the key/value typing is real.

This GATE is the reason no second JS package starts yet: prove the whole stack
(extract → manifest → provider → infer → emit → run) is faithful on one real package before
breadth multiplies the unknowns.

### R4a — faithful keyof / indexed-access / conditional (the hard structural work)
This is the genuinely-hard TS-type-system modeling the companion design doc deferred. It is
large — scope it honestly. Each needs a faithful representation across the stack, NOT a stub:

- **String-literal singleton types are a PREREQUISITE, not a detail.** "keyof evaluates to
  the union of member names" presupposes a LITERAL type (`"ping"`) exists across the schema,
  `FrozenType`, `SemType`, and inference — none of which is true today (`TypeRef` has no
  literal arm; `FTOr` unions carry only ordinary types). The same feature is what lets
  `emit("ping", 7)` select the right payload: the string-literal EXPRESSION must type-check
  against a literal union. Scope it as its own front-end type-system extension (schema arm +
  frozen/sem nodes + unifier admission + literal-expression typing) BEFORE the keyof work
  builds on it — and per the redesign-doc-first rule, sketch it in the companion design doc
  first.
- **`keyof T`** — a type operator over a type parameter (or a concrete type). Add a faithful
  schema arm (e.g. `TypeRef.KeyOf of TypeRef`) + a `FrozenType`/`SemType` node, and front-end
  resolution: when the operand is GROUNDED to a concrete record/interface, `keyof` evaluates to
  the union of its member names (string-literal union); ungrounded, it stays a `keyof` type node
  the unifier can carry. The extractor must STOP routing it to the structural-stub arm.
- **`T[K]` indexed-access** — the value type at key `K` of `T`. Faithful schema arm
  (`TypeRef.IndexedAccess of obj: TypeRef * index: TypeRef`) + node; resolution looks up the
  member type when `T` is grounded and `K` is a known key (the dependent `Events[Key]` is the
  stress case — the payload type DEPENDS on the key value, which Vesper's type system cannot do
  dependently; decide the faithful-but-expressible answer, e.g. resolve to the union of value
  types, and document the precision limit).
- **conditional `A extends B ? X : Y`** — faithful schema arm + node; evaluate when both sides
  are grounded (the no-payload `emit` overload is the stress case). The DEEPEST item — if full
  conditional evaluation proves out of reach, settle on a faithful representation + a documented,
  test-pinned evaluation rule for exactly the shapes mitt uses, rather than a silent degrade.

Update the design doc's mapping table + the `Schema.fs` grammar comments as these land. Watch
the SCC/cycle note (`interface Node { children: Node[] }`) only if a faithful form recurses;
mitt's residue does not contain a plain anonymous object literal, so structural-record
content-hashing is NOT required for this gate (it stays in R5).

### R4b — supporting inference / emit gaps (will bite during R4a/R3)
- **Single-overload method-typar freshening.** An external generic member's method typar
  (`FTTypar(TyparAxis.Method,i)`) is freshened per call ONLY on the multi-candidate
  overload-commit path (`ExternalSymbols.instantiateSignature` via `commitExternalOverload`);
  the single-candidate `TryLookupMember` field-walk does NOT, leaving the marker unbound
  (`TyConst int` vs `TyTypar(Method,0)`). mitt's `on`/`emit` are multi-overload so they dodge
  it — but exercising the FULL API faithfully may surface a single-overload member; fix by
  making the single-candidate external path call `instantiateSignature` too. **This is an
  inference/overload-resolver change** — do it deliberately; touching the unifier as part of
  provider/codegen work is the exact thing the codegen-owns-assignability guardrail warns
  against. (Memory `reference_single_overload_method_typar_no_freshen`.)
- **Default-import production wiring.** `JsImports.createWithDefaults` + `defaultValueKeys` are
  proven in the test but the production emit pipeline still builds `JsImports` via `create`
  (empty default set). Wire whatever assembles `EmitJs.WalkCtx.Imports` in the real compile to
  seed the default set from the active TS-manifest provider(s).
- **Trailing-optional v1 policy.** `toFunctionSymbol` DROPS a trailing optional parameter
  (`mitt(all?)` → `unit -> Emitter`). Revisit for mitt's full API if a callable optional matters
  (the faithful model is two arities / an overload).

**Acceptance (the gate).** mitt golden `Diagnostics = []`; `mittDiagnosticsContract` asserts
empty. A Vesper-source test calls factory + `on`/`off`/`emit`/`all` over a ≥2-key `Events`,
emits, and runs against real `mitt.mjs` under Node with asserted results. All suites green.
Any construct that genuinely cannot be made faithful is escalated (it changes the bar), not
silently re-degraded.

---

## R5 — coverage + breadth (GATED on R4 passing)

Do NOT start until R4's gate holds. Then:
- Point the resilient extractor at `@types/node` (module-shaped) and commit its diagnostics
  report as a **coverage golden** — ranked by code frequency it is a burndown chart; the top
  codes are the roadmap (structural-record content-hashing with SCC cycles, and whatever keyof/
  indexed/conditional shapes beyond mitt's are still unfaithful).
- The DOM is the eventual destination but needs a second **ambient-global extraction entry
  mode** (`declare global`, no module exports) the module-based `extractPackage` lacks, on top
  of every deferred feature firing at once. Capstone, not an early bite.
- Delete this doc; fold durable facts into module headers + `project_*` / `reference_*` memory.

---

## Orientation — the files you will touch

- **Provider (the R1/R2 center):** `src/XParsec.FSharp.Codegen.Js/TsManifestProvider.fs`
  — `toFrozen` (`:24`), `providerOfManifest` (`:435`), `typeKinds`/`kindOf` (`:462-472`),
  `toTypeShape`/`build` (`:279+`), `defaultValueKeys` (`:624`).
- **Front-end member access:** `resolveFieldStep`
  (`Passes/Unification/InferRecordAccess.fs:237`; TyConst catch-all `:403`; external
  `TyClass` provider lookup `:271-294`); `SemanticInfo.fs` `FrozenType` (`:377` `FTConst`,
  `:384` `FTClass`) + `FrozenTypeBridge.instantiateWith` (`:862`, `FTConst→TyConst` /
  `FTClass→TyClass`); `ExternalSymbols.fs` (`:857+` signature instantiation,
  `instantiateSignature` for freshening).
- **Codegen.Js emit:** `src/XParsec.FSharp.Codegen.Js/EmitJs.fs` — external
  `TExprG.ExternalMember` arm (`:630-676`), `App` spine collapse (`:444+`,
  `TastWalk.collectSpine`), `attachedAccess` + `ctx.LocalInterfaces` (`:595-599`, the
  native-method shape to reuse); `JsImports`/`JsRuntime.fs` (default-import set),
  `JsAst.fs`/`JsPrint.fs` (the `Import` statement with `defaultBinding`).
- **Tests:** `test/XParsec.FSharp.Codegen.Js.Tests/` — `MethodAxisGenericTests.fs` /
  `NullUndefinedTests.fs` (provider-stack + `runNodeFiles` harness pattern), `MemberOverloadTests.fs`
  (`calcProvider` stacking), and `MittE2ETests.fs` (the file R3 rewrites). The vendored fixture
  is `test/ts-fixtures/mitt/`.

## Relevant memories
`reference_js_external_instance_member_walls` (the three walls),
`reference_single_overload_method_typar_no_freshen` (R4 freshening gap),
`reference_null_undefined_already_survive_js` (null/undefined + the now-fixed free-fn asm=None),
`feedback_codegen_js_owns_assignability` / `feedback_freeze_no_backend_knowledge` (where target
knowledge may and may not live), `project_js_*` (the JS backend landscape).
