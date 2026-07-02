# TS provider — R4 remaining: the mitt full-fidelity gate

**Status (2026-07-02).** R1–R3 and R4a STEP 1 have LANDED (see the context table). What
remains is R4a steps 2–4: make the extractor emit mitt's five residual constructs
FAITHFULLY (step 2), give the front end their evaluation rules (step 3), and drive
mitt's FULL public API from Vesper source with a zero-diagnostic golden (step 4 — the
gate). R5 (breadth: `@types/node`, DOM) stays GATED on step 4 passing. No second JS
package before the gate.

**Companion:** [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md)
holds the *decisions*. Two sections are load-bearing for this work and are RESOLVED —
do not relitigate them:
- §"Literal types stay structural, in the external vocabulary only" — `FTLiteral`+`FTOr`,
  the NOMINALISM INVARIANT (Vesper inference never mints a literal type), directional
  admission at the external-arg seam, `keyof`/`T[K]`/conditional as ground-evaluation
  rules, erase-to-base emission.
- The `EqSet` sub-decision — union members: insertion-ordered storage, set-semantic
  equality, smart-constructor-owned flatten/dedupe/singleton-collapse.

Build/test/format via the **xparsec-dev** skill (`./claude_tools.cmd -Action
Build|Test|Format`), never raw `dotnet`. The ONE exception is the Fable rebuild of the
extractor (`dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`) —
step 2 IS the step that needs it. Anchors below are symbol names, not line numbers —
confirm by reading before editing.

---

## What has landed (context, not work)

| Area | State |
|---|---|
| R1 — external nominal → `FTClass` | `TsManifestProvider.toFrozen` mints `FTClass` for manifest-registered classes/interfaces via a qualified-name→`TypeKey` resolver; aliases/primitives stay `FTConst`/`Abbrev`. Front-end `.member` access resolves through `resolveFieldStep`'s external-`TyClass` arm. |
| R2 — native member lowering | `ExternalClassFlags.AttachMembers` (Fable `[<AttachMembers>]` semantics) stamped by the provider; EmitJs `App`-spine head-case folds to `receiver.member(args)` (argSig-driven arity: 0 drops lone unit, ≥2 spreads literal tuple); method ESCAPE eta-wraps for `this`-binding; Property = plain data read. Vesper's own free-fn form = opportunistic tree-shaking optimisation, unchanged for non-manifest members. |
| R3 — Vesper-driven e2e | `BusE2ETests`: factory + `on(name, lambda)` + `emit` + read-back under Node, lambda-as-callback included, zero src changes needed. |
| Arity law compliance | Provider mints/keys every nominal by `SymbolKeyOps.arityName` (suffix-at-mint for declarations, suffix-at-lookup by applied arg count for `Named` refs). `let e : Emitter<int> = mitt()` TYPE-CHECKS (pinned in `MittE2ETests`). |
| R4a STEP 1 — literal machinery | `EqSet<'T>` (`EqSet.fs`); `FTOr`/`TyOr` members are `EqSet` behind smart constructors (`FrozenType.MkUnion` / `SemType.MkUnion`, `TyOr`'s payload is private-ctor `UnionMembers` — canonical form TYPE-ENFORCED); `FTLiteral`/`TyLiteral` arms (`LiteralConst` String/Int) through every walk; `Schema.TypeRef.Literal` + codec (ADDITIVE, `SchemaVersion` still 1, extractor does NOT emit it yet); directional admission — constant set-membership in (`InferApp.tryAdmitLiteralConstArg`, consults the arg EXPRESSION — printf precedent), enum value-set ⊆ in (`Engine.enumAdmitsIntoLiteralUnion`, reads `EnumTypeInfo.CaseStringValues`), literal widens OUT to base (`subsumes` + `unifyAnnotation` literal-guarded arm); erase-to-base on both backends (`ClrEncoder.encodeType` FTLiteral arm; JS erases). Test matrix: `LiteralUnionTests.fs` (6 cases + Node e2e). |
| Pinned directional miss | `let m = "auto" in x.setMode(m)` ERRORS — `m` generalises to `string`; the constant is visible only at the arg site. Deliberate (TS widens the same way without `as const`). |
| Parser | Top-level bare statement before `let`/`use` no longer swallows the sibling decl (`pSepVirt`, goldens 394–396). |
| mitt fixture | `test/ts-fixtures/mitt/` vendored; golden pins the 5-warning degrade residue via `mittDiagnosticsContract`; committed goldens are BYTE-IDENTICAL through step 1. |

## The remaining mitt walls (truth mirror of the `MittE2ETests.fs` header)

1. UNANNOTATED `mitt()` leaves `Events` ungrounded ("1 unresolved TyVar"). Annotated
   form works. Whether the unannotated form should infer anything is a step-3/4 call —
   an annotation-required policy is acceptable if documented and pinned.
2. ~~annotation arity mismatch~~ — FIXED (arity law).
3. `on`/`emit`'s `type` param is a method typar `Key extends keyof Events` — arrives as
   a raw `TyTypar(Method,0)` marker a string literal can't unify with. Needs step 2
   (faithful `keyof` constraint on the typar) + step 3 (grounding rule).
4. `on`'s `handler` param degraded to opaque `__type` (`Handler<Events[Key]>` — the
   indexed access inside a named generic). Step 2 (faithful arm) + step 3 (fold).
5. `emit`'s payload is the `structural:Events[Key]` stub. Same.

---

## R4a STEP 2 — extractor fidelity (the ONLY extractor-touching step)

**Goal.** `Vesper.Ts.Extractor` emits mitt's five residual constructs as FAITHFUL
schema arms instead of `structural-object-stubbed` degrades; the regenerated committed
mitt golden's `Diagnostics` shrinks accordingly (target: `[]` — flip
`mittDiagnosticsContract` to assert empty when reached).

**Steps.**
1. Schema arms (additive, `SchemaVersion` stays 1, codec arms mirror the `"literal"`
   precedent from step 1): `TypeRef.KeyOf of TypeRef`, `TypeRef.IndexedAccess of
   objTy: TypeRef * index: TypeRef`, `TypeRef.Conditional of check: TypeRef * extends:
   TypeRef * whenTrue: TypeRef * whenFalse: TypeRef`. `TypeRef.Literal` already exists.
2. Extractor (`src/Vesper.Ts.Extractor/Extractor.fs`): route `ts.TypeOperator`
   (keyof), `ts.IndexedAccessType`, `ts.ConditionalType`, and literal types to the new
   arms INSTEAD of the structural-stub degrade path. A method-typar CONSTRAINT
   (`Key extends keyof Events`) must survive onto the signature (find how typar bounds
   are (not) carried today — the manifest may need a typar-constraint slot; if the
   schema has none, that is an additive schema change this step owns). Fable rebuild
   required (the one sanctioned `dotnet fable` command, header above).
3. Provider (`TsManifestProvider.toFrozen`): map the new arms to CARRIER FrozenType
   nodes — `FTKeyOf`, `FTIndexedAccess`, `FTConditional` (new arms in `SemanticInfo.fs`
   with `TyKeyOf`/`TyIndexedAccess`/`TyConditional` SemType mirrors via
   `instantiateWith`). Step 2 keeps them INERT (carried, not evaluated) — every
   FrozenType/SemType walk handles them (they HAVE children, unlike `FTLiteral`);
   `argSigOf` needs a stable spelling (e.g. `keyof(...)`) sharp enough for mitt's `on`
   overload pair (`type: Key` vs `type: '*'` — the literal arm already renders `"*"`).
4. Regenerate the mitt golden by the flow `test/Vesper.Ts.Extractor.Tests` uses
   (discover it there — consumer tests still NEVER run the extractor). Diff the new
   manifest by eye: the five diagnostics should be gone and the five constructs
   faithfully spelled. Commit the regenerated golden + the diagnostics-contract flip
   in the SAME change as the extractor edit.
5. All consumer suites must stay green with the CARRIED (unevaluated) nodes — mitt's
   factory/annotation tests exercise `Emitter<int>` whose member SIGNATURES now carry
   the new nodes; carrying must not break resolution that never touches them.
   `MethodAxisGenericTests` / `MittE2ETests` are the canaries.

**Trap.** Do not let the extractor evaluate anything (no keyof-expansion in TS-land):
the manifest carries the CONSTRUCT, the front end owns evaluation (step 3) — same
freeze/backend-knowledge separation as everywhere else.

---

## R4a STEP 3 — evaluation rules (front end)

**Goal.** The carried nodes evaluate exactly as the design section specifies, closing
walls 3–5 for concrete call sites.

**Steps.**
1. `keyof` fold: `TyKeyOf t` with `t` ground to a record/interface (project-local
   record, manifest interface, or the type-arg the annotation supplied) evaluates to
   `MkUnion [TyLiteral name; …]` of its member names. Unground: carried, and it must
   not poison unification (treat like a deferred node; decide + pin the "still unground
   at generalisation" diagnostic).
2. Method-typar grounding at the call site: for `on<Key extends keyof Events>(type:
   Key, …)` instantiated at a SYNTACTIC string constant, the existing
   `tryAdmitLiteralConstArg` seam grows the ability to SOLVE the freshened `Key` var to
   `TyLiteral "ping"` when the constraint's keyof-fold contains it (this stays within
   the nominalism invariant — the literal enters via the EXTERNAL signature's typar,
   never as the type of a Vesper expression). A non-constant key falls back to the
   documented precision limit: `Events[Key]` = union of member value types.
3. `T[K]` fold: `TyIndexedAccess(t, k)` with `t` ground and `k` a `TyLiteral` (or a
   literal union) → the member's type (union of the members' types). This is what
   types `handler` correctly per key.
4. Conditional fold: ground check/extends → pick a branch; the `extends` test on a
   ground union is `TyOr`/`subsumes` membership. mitt's no-payload `emit` overload
   (`undefined extends Events[Key] ? Key : never`) is the pinned stress case.
5. **R4b freshening will bite here**: the single-candidate `TryLookupMember` field-walk
   does NOT freshen method typars (`ExternalSymbols.instantiateSignature` runs only on
   the multi-candidate overload-commit path). Fixing it is IN SCOPE for step 3 — it is
   an inference/overload-resolver change; make it deliberately, with isolation tests
   first (systematic-tests rule), not as a drive-by.

Isolation tests per rule BEFORE wiring (hand-built manifests, `LiteralUnionTests`
pattern), then the mitt-shaped integration: `e.on("ping", fun p -> …)` types `p` from
`Events.ping`.

---

## R4a STEP 4 — ★ the gate

Both halves, then R5 may start:
1. mitt golden `Diagnostics = []`, `mittDiagnosticsContract` asserts empty (done in
   step 2 if extraction got there; re-verify).
2. mitt's COMPLETE public surface — `mitt()` factory, `on`, `off`, `emit` (with and
   without payload), `all` — driven from Vesper source over a ≥2-key `Events` with
   DIFFERENT payload types, emitted via Codegen.Js, run against the real vendored
   `mitt.mjs` under Node, behaviour asserted. Rewrite `MittE2ETests` so the harness is
   import+assert only; delete the walls list from its header as each closes.

Also in step 4 (R4b leftovers that gate "complete"):
- **Trailing-optional policy**: `mitt(all?)` is collapsed to `unit -> Emitter`; if the
  full API needs the 1-arg form, model two arities (overload) — revisit deliberately.
- **Default-import production wiring**: `JsImports.createWithDefaults` +
  `TsManifestProvider.defaultValueKeys` are test-proven; the production emit pipeline
  still builds `JsImports.create` (empty default set). Wire the real compile path.
- `emitter.all` is a `Map` — expect a BCL-vs-JS `Map` reconciliation question; if it
  explodes in scope, pin the gap honestly and descope `all` READS only with user
  sign-off (it changes the bar).

---

## R5 — coverage + breadth (GATED on step 4)

Unchanged: `@types/node` diagnostics coverage golden as a burndown; DOM needs the
ambient-global extraction mode; structural-record content-hashing with SCC cycles.
Delete this doc at R5 start; fold durable facts into module headers +
`project_*`/`reference_*` memory.

---

## Orientation — files and symbols

- **Schema/codec:** `src/Vesper.Ts.Manifest.Schema/{Schema,Codec}.fs` — `TypeRef`
  (`Literal` arm exists; step 2 adds `KeyOf`/`IndexedAccess`/`Conditional`),
  `EnumValue` (the literal-tagging precedent), codec `"literal"` arm.
- **Extractor (step 2 only):** `src/Vesper.Ts.Extractor/Extractor.fs`; rebuild via the
  one sanctioned Fable command; golden-regen flow lives in
  `test/Vesper.Ts.Extractor.Tests`.
- **Provider:** `src/XParsec.FSharp.Codegen.Js/TsManifestProvider.fs` — `toFrozen`
  (resolver + arityName suffix-at-lookup), `argSigOf` (literal renders quoted),
  `providerOfManifest` (`typeKeys`/`typeKinds`).
- **Types + walks:** `src/XParsec.FSharp.SemanticAnalysis/SemanticInfo.fs` —
  `LiteralConst`, `FTLiteral`/`TyLiteral`, `FTOr`/`TyOr` + `MkUnion`/`UnionMembers`,
  `FrozenTypeBridge.instantiateWith`; `EqSet.fs`.
- **Unification seam:** `Passes/Unification/Engine.fs` — `subsumes` (literal widening,
  `enumAdmitsIntoLiteralUnion`, TyOr arms), union unify (`TyOr m1 = m2` set equality),
  `unifyAnnotation` literal-guarded arm; `Passes/Unification/InferApp.fs` —
  `tryAdmitLiteralConstArg` (the constant-consultation seam step 3 extends);
  `ExternalSymbols.instantiateSignature` (the step-3 freshening fix site).
- **Emission:** `Codegen.Clr/ClrEncoder.fs` (`FTLiteral` erase-to-base arm — new carrier
  nodes need arms too), `Codegen.Js/EmitJs.fs` (`validatePlatformTypes`).
- **Tests as patterns:** `test/XParsec.FSharp.Codegen.Js.Tests/LiteralUnionTests.fs`
  (admission matrix + Node e2e), `ExternalNominalClassTests.fs` (hand-built manifests),
  `BusE2ETests.fs`/`MittE2ETests.fs` (e2e + the walls-truth header),
  `test/XParsec.FSharp.SemanticAnalysis.Tests/EqSetTests.fs`.

## Relevant memories
`reference_js_external_instance_member_walls` (walls status),
`reference_single_overload_method_typar_no_freshen` (the step-3 freshening fix),
`feedback_freeze_no_backend_knowledge` (extractor carries, front end evaluates),
`feedback_systematic_tests_over_whackamole` (isolation tests before wiring),
`reference_eqarray_percentA_cache_key` (never key caches on `%A` of these types).
