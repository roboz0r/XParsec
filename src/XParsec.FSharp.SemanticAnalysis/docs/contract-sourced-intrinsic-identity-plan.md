# Contract-sourced intrinsic identity (kill the front-end shadow set)

**Status (2026-07-07): the single live plan for this work.** Its predecessors LANDED and were
rolled up and deleted: `qualified-intrinsic-identities-plan.md` (intrinsics got qualified
`SymbolKey`s, ns still shadow-set-derived) and `resolution-time-intrinsic-reconciliation-plan.md`
(`obj`/`exn` became `IntrinsicClass` heritable primitives; `System.Object`/`System.Exception`
canonicalize eagerly at resolution; `canonName`'s reverse tier, `normalizeObj`, and
`isSystemObjectKey` are deleted). All suites green: SA 765 / Clr 1253 / Js 348 / Vesper 49.
Delete this doc when the residue stages (5a bigint — LANDED 2026-07-08 / 5b ranges — LANDED
2026-07-08 / 5c nullability — Tiers A+B LANDED 2026-07-08, Tier C flow-narrowing SPUN OUT) and
byref land — as of 2026-07-09 ALL are landed or spun out: byref Piece 1 (type identity
`"&"`→`Vesper.byref`) LANDED, byref Pieces 2–3 (contract + `~&` operator) SPUN OUT to
`byref-address-of-plan.md` (`feedback_plan_docs_ephemeral`). Custom numeric
literals — surfaced by 5a — was spun out to its own plan (`custom-numeric-literals-plan.md`); the
Route-1 seq-operator design surfaced by 5b was spun out to `range-operators-plan.md`; the Tier-C
flow-sensitive nullability analysis was spun out to `nullability-analysis-plan.md`.

## Principle (user, confirmed)

Strings flow only at **name resolution** (user/bootstrap source → a fully-qualified identity).
After that, `SymbolKey`s flow everywhere and compare by **exact `=`**. An intrinsic's identity is
**resolved from the `prim-types-*` contract** through ordinary name resolution — never authored
from a hardcoded front-end name set (`feedback_mockbuiltins_is_a_trap`).

## Landed — the code handles

- **Local registration**: `PassContextTypes.IntrinsicKeys` (name → qualified key, stamped from the
  declaring `namespace` by `TypeRegistration.registerAbbreviationDefn`); `TypeRegistry.intrinsicKeyOf`
  is the single local-mint resolver every guarded intrinsic mint routes through.
- **`ctx.Intrinsics : IntrinsicSet`** (`SideTables.fs`): lazy per-field, cached, loud-fail on a
  contract miss — **do NOT reintroduce a resolver fallback**; composing test fakes over the real
  contract (`ExternalSymbols.composite [ stub; realProvider ]`) is the sanctioned fix when a test
  loud-fails. Resolution: `IntrinsicResolve.tryResolveIntrinsicKey` = local `IntrinsicKeys` first,
  else `ExternalSymbols.tryPickRuntimeType` (bare name, then ambient prefixes, scanning PAST a
  non-intrinsic hit), reading the authoritative `canon` off the matched shape.
- **Shapes carry `SymbolKey` canon**: ONE case `ExternalTypeShape.Intrinsic of IntrinsicShape`
  (`{ Id: IntrinsicIdentity; Class: IntrinsicClassSurface voption }`, with
  `IntrinsicIdentity = { Canon: SymbolKey; Arity; Platform }`). `Class = ValueSome` ⇔ a heritable
  primitive (`obj`/`exn`, from `(# class "…" #)` + `extern class with inherit/new:`) — the
  identity axis is single-pattern BY CONSTRUCTION; the class surface (contract base + `.ctor`s)
  feeds `subtypeParentOf` (base chain), `resolveInheritParent` (downstream `inherit exn`),
  `inferIntrinsicClassCtorCall` (the ONE contract-ctor check `new exn` and `inherit exn(args)`
  share — target-agnostic; the explicit platform spelling `new System.Exception(…)` is the
  opt-in to the platform's wider catalogue), and codegen `IntrinsicClassBase` (heritability =
  the surface lookup; the `class` kind tag IS the predicate). Resolved-key consumers route
  through `ExternalSymbols.tryIntrinsicClass` (direct qualified lookup — no short-name re-scan).
- **Provider maps re-keyed**: forward `IReadOnlyDictionary<SymbolKey, string>` (canon → platform
  repr), reverse `Map<string, SymbolKey list>` (platform runtime name → canons).
- **Eager canonicalization at resolution**: metadata `MetadataSymbols.tryBuildType` (partition
  `not t.IsInterface`) and the source-name twin `UnificationTranslate.externalClassTy` (also used
  by the ctor sugar) — no raw BCL nominal enters the unifier; capability INTERFACES keep `TyClass`.
  `unifyAnnotation` admits the concrete-subtype → supertype upcast (strict `Subtype` walk only).
- **SA static sweep complete — NO `BuiltinTypes.ty*` remains in live code.** `tySeqInt` was the
  last one; Stage 5b deleted it (`tyUndefined`/`tyBigInt` had already migrated to
  `ctx.Intrinsics.*` — Stage 5 / 5a, 2026-07-08). (`BuiltinTypes.tyInt`/`tyBool`/… still exist but
  are test-only fixtures, not live-code mints.)

**Standing invariants:** `arity` stays a separate field — keys are VERBATIM name, NO arity suffix
(`"int"`, `"[]"`, `"byref"`; arity rides in the args; user premise). `SymbolKey` is
equatable-but-NOT-comparable (`EqArray` is `[<CustomEquality; NoComparison>]` by design — do not
add comparison), so `SymbolKey`-keyed axes are `Dictionary`/`IReadOnlyDictionary`, never `Map`.
Canon keys are minted asm-blind (`asm = None`). Build/test ONLY via
`./claude_tools.cmd -Action Build|Test`.

## Remaining work, in order

### 0. Review cleanup — LANDED 2026-07-07 (all suites green: SA 765 / Clr 1253 / Js 348 / Vesper 49)

Findings from the 2026-07-07 quality review of the landed milestone, all accepted and landed:

1. **Collapse `IntrinsicClass` into `Intrinsic`**: one case `Intrinsic of IntrinsicShape` with
   `IntrinsicShape = { Id: IntrinsicIdentity; Class: IntrinsicClassSurface voption }`,
   `IntrinsicIdentity = { Canon: SymbolKey; Arity: int; Platform: string option }`,
   `IntrinsicClassSurface = { BaseType: FrozenType voption; Members: ExternalMember[] }`.
   Principle: **a DU case per identity semantics, a field per added capability on the same
   identity** — heritability is a capability on the same `TyConst` identity, so the ~13
   dual-pattern identity-axis sites collapse by construction. (`IntrinsicIdentity` is
   introduced now because `IntrinsicInterface` below will share it.)
2. **One canonical resolved-key lookup**: `ExternalSymbols.tryIntrinsicClass`
   (direct `TryLookupType (qualifiedName canon)`, the `ClrExternalMembers.IntrinsicClassBase`
   idiom; `ExternalSymbols.tryLookupType` is the existing key-accepting front door) replaces
   the short-name re-scans in `Unification.fillBaseCtorCall` and `InferCtor` — a resolved key
   must never round-trip through a string scan. VERIFIED (scout, 2026-07-07): these two are the
   ONLY canon-key re-resolution sites (every other `intrinsicName` use is display / predicate /
   repr-bridge); all live producers of a HITTING canon are `Vesper`-qualified, matching the
   provider's `compiled` key, and the self-host shape-miss stays a silent no-op — equivalent.
   One structural caveat: the extraction mint (`VesperLib` `RuntimeNames.intrinsicKey short`)
   would stamp `ns=""` for a heritable intrinsic named outside the shadow set — un-exercised
   today (`obj`/`exn` only) and already scheduled away by Stage 4's producer-mint re-sourcing.
3. **One constructible surface for `new exn` vs `inherit exn(…)`**: CONTRACT-for-both (user,
   confirmed 2026-07-07) — both sites read the shape's contract `.ctor`s; `InferCtor` drops its
   platform-repr routing for the canon spelling. The platform-only overloads (`message * inner`)
   stay reachable ONLY via the explicit platform spelling (`new System.Exception(…)`) — an
   opt-in to losing platform generality, not the default surface. Fix the
   `IntrinsicClass.members` doc comment to match (it currently claims `new exn` reads the
   shape). Emit-time `contract ⊆ platform` stays an invariant per target binding.
4. **`ClrEncoder` `PrimitiveRepr` fallback** must not encode an unknown value-type repr as a
   reference class: gate on `externalIsValueType` and keep the loud `failwithf` (Stage-3
   principle: no silent mis-emit).
5. **VesperLib snapshot-then-patch-twice republish**: mint the heritable-primitive shape ONCE
   at finalization (base + ctors complete) instead of extraction-time snapshot + two patches.
   VERIFIED feasible (scout, 2026-07-07): `finalizeDeferred` runs to completion BEFORE the
   provider's eager intrinsic-axis snapshots (`ExtractCtx.toProvider` chains them), and no
   intermediate reader needs the shape to already be intrinsic-class-shaped (the freeze path
   short-circuits `obj`/`exn` via `isPrimitiveName` before consulting the shape; the ctor-loop
   arity read has a `Class` fallback). Two implementation notes: (a) the `ExternKind.Class`
   tag is NOT recoverable at finalize (`DeferredBody.Class` doesn't carry it) — the Extern arm
   must record the flagged `compiled` names in a small side set on `ExtractCtx`;
   (b) the republish must be a DEDICATED finalize step iterating that set (reading the
   already-frozen `FrozenBaseType` + the frozen `.ctor`s from `TypeMembers`), not a patch
   inside the base/ctor loops — a contract type with a base but no ctors (or vice versa)
   enters only one loop. Bonus: this makes the Extern arm's two branches symmetric (both stay
   repr-carrying/plain `Class` until finalize).
6. **Minor**: delete `IntrinsicSet.BigInt`/`.Undefined` (guaranteed loud-fail until their
   contracts land); extract `IntrinsicSet`/`IntrinsicResolve` from `SideTables.fs` (decouple
   `IntrinsicResolve` to take the `IntrinsicKeys` dictionary, not `PassContextTypes`); retire
   `RuntimeNames.systemObjectKey` if `systemObjectQualifiedName` can stand alone.

### 1. Stage 2 — `SymbolKey` currency in the unifier — LANDED 2026-07-08 (SA 765 / Clr 1253 / Js 348 / Vesper 49)

A pure **currency change** — the roots canonicalize at resolution and `canonName`'s reverse tier
was already gone, so no reverse-map tier was rebuilt. What landed:

- `canonName : PassContext -> string -> string` became `canonKey : PassContext -> SymbolKey ->
  SymbolKey` (`EngineCore.fs`): local `IntrinsicReprTypes` (keyed by `simpleName`, ⇒
  `TypeRegistry.intrinsicKeyOf`) first, else provider `Intrinsic` canon via the open scope on the
  QUALIFIED name (a user nominal whose SIMPLE name coincides with an intrinsic misses the provider),
  else the key itself.
- `subtypeNominalOf` returns `struct (SymbolKey * args)`. Identity compares (`tryUpcastWitness`,
  `subsumesNominal`) go exact `=`; the `seen` sets are `HashSet<SymbolKey>`. The string-needing
  boundaries (`subtypeParentOf`/`subtypeInterfacesOf` provider lookups, `tryExternalInheritedMember`'s
  `TryLookupMember`, `funSlotArityOfArgs`) project `SymbolKeyOps.qualifiedName` at the seam —
  `subtypeParentOf`/`subtypeInterfacesOf` keep their `name: string` params, fed the qualified canon.
- `PassContext.IntrinsicCanonCache` re-keyed to `Dictionary<SymbolKey, SymbolKey>`. `numericFamilyOr`
  now stringifies with `simpleName` (the reverse map's KEYS stay `string` platform names).
- Bridges deleted where they died: `canonKey`'s provider read returns the canon KEY (no
  `intrinsicName canon`); `subtypeNominalOf`'s `TyConst` arm and `numericFamilyOr` dropped their
  `intrinsicName`. `SymbolKeyOps.intrinsicName` itself STAYS — its other feeds (Freeze/Printf,
  Freeze/Access, Regions, Inline, VesperLib, `TyStructuralCtor`, Subsume's `v.BaseName` compare,
  Engine `primitiveSupports`, `externalSurfaceKeys`) are Stage-4 work.
- **`sameTypeAsmBlind` audit — NOT empty, so it stays.** But its domain does NOT include the subtype
  walk: the ONE cross-asm hazard the currency change exposed was `subtypeInterfacesOf` surfacing an
  external interface with `asm = None` (matched only asm-agnostically by the old string compare) vs a
  written `A<int>` param carrying `asm = Some home`. Fixed by CANONICALIZING the producer — the
  surfaced interface key is now minted through `SymbolKeyOps.externalTypeKey ifaceShape.Origin`
  (re-resolving the interface's OWN home off its provider shape, since it may live in a different
  package than the implementing class), so exact `=` holds. `sameTypeAsmBlind` survives only for the
  `RuntimeNames` well-known-singleton recognizers (`isVesperListKey`, `CapabilityIdentity.Matches`,
  `isPrintfFormatKey`, …), which are asm-blind BY DESIGN for origin-less mints (test helpers,
  asm-blind codegen paths) — a distinct concern from the walk, not retired here.

### 2. Stage 3 — codegen repr validation — LANDED 2026-07-08 (Clr 1253 / Vesper 49)

`ClrEnv.TryPrimitiveRepr` now takes the canon `SymbolKey` directly — codegen carries the open-
resolved canon on its `FTConst` node, so the provider forward-repr lookup is `key` itself, retiring
the `RuntimeNames.intrinsicKey name` bridge. Own-unit `reprs` stays `simpleName`-keyed (projected at
the seam). The `ClrEncoder` `(|PrimitiveRepr|_|)` pattern is now key-keyed; the sole roundtrip
(`simpleName key |> intrinsicKey`) is gone and the CLR suite proves the direct key hits every
primitive the old roundtrip did.

The "map to a known IL/JS type or **emit a diagnostic** (no silent mis-emit)" invariant was already
discharged: cleanup item 4 (Stage 0) gated the `ClrEncoder` reference-class fallback on
`externalIsValueType` with a loud `failwithf`, so an unknown value-type repr can no longer be
mis-encoded as a class. On JS the backend resolves reprs by its own path and never reads this axis
(the one JS forward-repr reader, `NumberCovariance`, is a producer-mint scheduled by Stage 4).

### 3. Stage 4 — the shadow set falls out — LANDED 2026-07-08 (SA 765 / Clr 1253 / Js 348 / Vesper 49)

What landed:

- **`knownIntrinsicNames` + the classifying `intrinsicKey` are DELETED.** `RuntimeNames` now exposes
  named canonical key constants (`unitKey`/`intKey`/`objKey`/`stringKey`/`byrefKey`/`arrayKey rank`/
  … — the `vesperListKey` idiom, correct-by-construction) that every `TyConst`/`FTConst` producer of
  a fixed intrinsic reaches for, plus two minters: `primitiveKey name` (Vesper, for the
  runtime-primitive-name sites — a literal's `BaseName`, an SRTP `primName`, an enum's `Underlying`)
  and `opaqueKey name` (`ns = ""`, the genuinely-unresolved fallback). The ~150 call sites across
  ~40 files (+ the test suites) were swept onto these. **`intrinsicNamespace` could NOT be deleted**
  as the plan hoped — it survives `private`, the single literal backing the constants + `primitiveKey`,
  because the pure `IntrinsicTypePatterns` active patterns and the codegen synthetic mints have no
  resolver in hand (fully sourcing them from resolution is infeasible). The static `BuiltinTypes.ty*`
  survive (Stage 5 deletes them).
- **Producer re-source (item 1):** the VesperLib extraction canons mint through
  `SymbolKeyOps.intrinsicCanonKey compiled short` — `asm = None`, VERBATIM short name, ns read from
  the qualified `compiled`. This is CONTRACT-sourced, so `disposable`/`dynamic`/`undefined`/(the
  member-bearing `widget` fixture)/(the base array) canons moved from `ns = ""` (the old classifier's
  latent bug — none were in `knownIntrinsicNames`) to `ns = "Vesper"` (their real `namespace Vesper`).
  Verified behavior-safe for array/byref (the base-array canon is an inert island; byref has no
  contract). `dynamic` was the load-bearing correction: it IS a contract intrinsic
  (`prim-types-dynamic.js.fsi`), so its canon and `RuntimeNames.dynamicKey` are now BOTH `Vesper.dynamic`
  — the old `ns = ""` on `dynamicKey` was the same latent bug.
- `primitiveSupports` + the pure-identity checks needed **no change**: they already key on the
  resolved key's name (`SymbolKeyOps.intrinsicName nameKey`) against the passes' own verdict tables
  (`primitiveValueTypes`, `numericTypeNames`), never `knownIntrinsicNames`.

**FINDING — a syntactic Vesper-primitive-name recogniser SURVIVES, and cannot be fully deleted here.**
Two sites classify a BARE name (Vesper-primitive → `primitiveKey`, else `opaqueKey`) with no provider
in hand, so `opaqueKey`-everywhere (the plan's premise for them) was wrong:
  - `TsManifestTypes` `intrinsicOrOpaque` — a manifest param spells a Vesper primitive by its canon
    name (`float`, `string`, `undefined`); it must mint the `Vesper` key to unify with the front end's
    literal arg. Uses the shared `RuntimeNames.numericTypeNames` core + the reference primitives.
    `number` (the widening token) / `null` / every real external name stay opaque. (The `Translate` /
    `MemberRegistration` opaque fallbacks ARE genuinely opaque — provider resolution precedes them.)
  - `Translate` opaque fallback — a written `undefined` (JS-only, no CLR repr, hardcoded
    `BuiltinTypes.tyUndefined`) resolves to `undefinedKey` when the provider lacks it, so it agrees
    with the Freeze/optional-default form in a stack without the JS contract.
  Follow-up (Stage 5 / frontier): give the manifest translator provider access so it resolves these
  intrinsics through the contract (retiring the syntactic set), and contract-source `undefined` so the
  front-end bridge dies. Frontier (out of scope): derive the equatable/comparable enumeration from
  contract capability interfaces via `FrozenInterfaces`.

### 4. Stage 5 — residue (unpacked into per-item stages)

The original five residue bullets were unpacked: two landed, three became their own stages
(each needs its own contract-authoring or design work, tracked below).

**LANDED 2026-07-08 (SA 771 / Js 348):**

- **`undefined` migrated to contract.** `prim-types-undefined.js.fsi/.fs` publish `undefined`
  as an `Intrinsic`; `IntrinsicSet.Undefined` added; `Freeze/Apply.optionalDefaultNode` (the one
  JS-only omitted-optional fill) now reads `ctx.Intrinsics.Undefined`, and `BuiltinTypes.tyUndefined`
  is deleted. `Passes/Unification/Translate.fs`'s written-name arm KEEPS its `TyConst(undefinedKey)`
  mint — that is the deliberate fallback for a stack that has NOT loaded the JS contract (where
  `ctx.Intrinsics.Undefined` would loud-fail); a JS compilation resolves the name through the
  provider first.
- **Numeric spelling aliases verified + regression-locked.** `int8`/`uint8`/`uint`/`int32`/
  `single`/`double` already resolve-through-alias — the contracts declare them as abbreviations and
  BOTH translators dealias (`Translate.tryResolveExternalType`'s `Abbrev` arm; VesperLib's
  `dealiasPrimitiveAbbrev`). Added `CoverageTests` "alias … resolves to …" asserting each unifies
  with its canonical literal (`(5y : int8)` ⇒ `sbyte`, no mismatch). No production change was needed.

**Stage 5a — `prim-types-bigint` contract — LANDED 2026-07-08 (SA 772 / Clr 1253 / Js 348 / Vesper 51).**
Authored `prim-types-bigint.fsi` (`type bigint = extern`), `.fs` (`(# "System.Numerics.BigInteger" #)`
canon+CLR repr) and `.js.fs` (`(# "bigint" #)` JS platform name); wired the `.fsi`/`.fs` into
`manifest.toml` `files`/`impl` (the `.js.fs` is auto-discovered by the `<base>.<target>.fs` sibling
convention — `ReferencedProject.targetOverrideFs` — so it needs NO manifest entry, matching
`prim-types-float.js.fs`). Added `IntrinsicSet.BigInt`, migrated the `InferLiterals` `NumBigInteger*`
arm to `ctx.Intrinsics.BigInt`, and deleted `BuiltinTypes.tyBigInt` + the now-dead
`RuntimeNames.bigintKey`. `VesperCoreContractTests` gained the two parse goldens; `CoverageTests`
gained a "written `bigint` annotation resolves to the contract intrinsic" regression (before the
contract, the name fell to an opaque `TyConst(ns="")`).

Follow-up → its own plan, `custom-numeric-literals-plan.md` (user-confirmed 2026-07-08). A bigint
LITERAL is NOT a primitive constant: `52I` is F#'s custom-numeric-literal syntax — it desugars
through a `NumericLiteralI` module (`FromZero`/`FromOne`/`FromInt32`/`FromInt64`/`FromString`, chosen
by magnitude) to a CONSTRUCTED value, which is exactly why `Freeze.parseConst` rejects it ("non-
representable literal NumBigIntegerI in constant position" — same wall under the old `tyBigInt`
mint). 5a correctly made the bigint TYPE (what `NumericLiteralI.From*` returns) a contract-sourced
intrinsic; wiring the literal through the module is that plan. The `InferLiterals` `NumBigInteger*`
arm 5a migrated is a pragmatic in-built stand-in — it types all six suffix tokens (`I/N/Z/Q/R/G`) as
`bigint`, which over-claims (only `I` is bigint) and skips the value construction; that plan corrects it.

**Stage 5b — delete `tySeqInt` / range expressions — LANDED 2026-07-08 (SA 774 / Clr 1253 /
Js 348 / Vesper 51).** The design fork (make `1..10` a real `TyClass(IEnumerable,[int])` seq value —
"Route 1" — vs. keep it a for-in-only construct — "Route 2") was decided **Route 2** (user,
2026-07-08): a range materialises no seq value in this compiler, and a range-as-value would
type-check but die at the codegen catch-all (`Emit`/`EmitJs: unsupported expression`), so the honest
move is to reject any non-lowerable range up front. What landed:
- `tySeqInt` DELETED (the last live `BuiltinTypes.ty*`). `InferApp.inferRange` now returns
  `TyUnknown "range"` (concrete → a surviving `TExpr.Range` freezes cleanly) after pinning endpoints
  to int; `InferControlFlow.inferForIn`'s `isRangeSource` branch dropped the `unify srcTy tySeqInt`
  and just pins the pattern to int (the counted-`ForTo` lowering in `Freeze.translateForIn` is
  syntactic and was always independent of the range's type).
- Rejection is emitted at the ELABORATION choke point (`ElaborateExpr.translateExpr`'s `Range` arms),
  which fire ONLY for a range NOT consumed by the `ForTo` lowering — value position, a stepped range,
  or a non-simple for-in boundVar — all genuinely unsupported. Inference can't tell a for-in source
  from a value, so the position-aware place is lowering. `for i in 1..10` (unit step, simple boundVar)
  is unaffected — it never reaches the `Range` arm.
- `CoverageTests` retyped: the two "types as seq<int>" tests became "range-as-value rejected"; a
  "counted loop accepted" and "stepped for-in rejected" test added; `Tast`/`Intrinsics` doc refs
  updated. Route 1 (real `(..)` seq operator + counted-loop peephole) spun out to
  `range-operators-plan.md`.

**Stage 5c — `objnull` / `TyOr` nullability. Tiers A + B LANDED 2026-07-08
(SA 774 / Clr 1253 / Js 348 / Vesper 51). Tier C remains.**
Goal: `objnull` is NOT a primitive — it is the ordinary `obj | null` union.

- *Already built (no work):* `TyOr` is a first-class `SemType` (smart ctor `mkUnion`/`MkUnion`,
  order-insensitive `UnionMembers` EqSet, frozen mirror `FTOr`), with full directional `subsumes`
  (`T <: T|null` holds via the `src', TyOr ts` arm; `T|null <: T` only when every member subsumes
  `T`) and match-pattern narrowing (`InferControlFlow.computeArmNarrowing`: `:? T` type-tests shrink
  a closed union's residual, with an exhaustiveness warning).
- *Tier A — `objnull` representation — LANDED.* The VesperLib EXTRACTION path
  (`VesperLib/TypeTranslate.fs` `UnionType` arm) now translates `T | null → FrozenType.MkUnion [T;
  null]` (was collapse-to-`T`), and `objnull` was dropped from `RuntimeNames.referencePrimitiveNames`
  (so `isPrimitiveName "objnull"` is false and a written `objnull` EXPANDS its abbrev instead of
  dealiasing to bare `obj`). `objnull` = `TyOr [obj; null]` end to end; the `VesperLibTests` objnull
  test asserts the `FTOr [obj; null]` union.
- *Tier B — `null` identity + CLR reference-null erasure — LANDED (adjusted).* `null` got ONE
  canonical cross-backend identity, `RuntimeNames.nullKey`, single-sourced and wired into all three
  producers (front-end `Translate.fs` `Type.Null`, the extractor, `TsManifestTypes`). **Correction to
  the original sketch (user, 2026-07-08):** `null` is a language KEYWORD, not a `namespace Vesper`
  type, so `nullKey` is the BARE `null` (`opaqueKey`, `ns=""`) — NOT `Vesper.null`. It is deliberately
  NOT registered in any provider: `PlatformTypes.run` (the real `validatePlatformTypes`) only rejects a
  type the provider resolves to an `Intrinsic{Platform=None}`, so `null` survives JS emit PRECISELY
  because it stays unregistered — registering it would trip that gate (verified: `T | null`/`T |
  undefined` round-trip under Node, `NullUndefinedTests`). "Per-target repr" is the existing VALUE
  lowering (JS literal `null`; CLR `ldnull`), not a new registered type-repr — there is no consumer of
  a null-type repr. The real, previously-unscoped work Tier A forced: **CLR reference-null erasure**
  (`T | null → T`, `obj | null` ≡ the `System.Object` slot), applied at the three CLR-ABI seams so
  `objnull` behaves as `obj` there — (1) `UnificationEngineCore.stripReferenceNull` on both sides of
  interface/Object-override conformance (`Unification.checkInterfaceConformance` /
  `checkObjectOverrideConformance`); (2) `InferTypeOps.inferDynamicDowncast` erases the source's `null`
  member so `(x: T|null) :?> U` is governed by `T` exactly as F# governs it (`obj | null` downcasts
  like `obj` — admitted; `string | null` like sealed `string` — FS0016 "no proper subtypes"); (3)
  `ClrEncoder.encodeType`'s new `FTOr` arm erases the `null` member and encodes the single reference
  survivor. CLR VALUE-type nullability (`int | null` ⇒ `System.Nullable<int>`) stays OUT — it
  would wrongly erase to `int`, but the self-host emits none.
- *Tier C — flow-sensitive nullability analysis — SPUN OUT to `nullability-analysis-plan.md`
  (2026-07-08).* The "analysis" proper: narrowing on `if x <> null` / `x != null` / `!== undefined`
  GUARDS (not just match patterns) — flow-sensitive union-member removal along branches, so the
  then-branch sees `x : obj`. No such flow-typing sub-pass exists today (only the match-pattern
  `computeArmNarrowing`, a useful template but not guard-flow); it is a new flow environment threaded
  through `if`/`&&`/`||`/early-return — the biggest chunk, and a standalone feature that can wait
  (Tiers A/B, landed, do not need it). Moved to its own plan since it needs nothing from this doc
  beyond the representation Tiers A/B landed.

### 5. byref TYPE identity re-source — LANDED 2026-07-09 (SA 777 / Clr 1253 / Js 348 / Vesper 51)

The *type* is `byref`; `&`/`~&` is the operator that makes one (user, confirmed). Scouting
(2026-07-09) found the plan's single "byref migration" bullet actually decomposes into three
pieces at very different risk/scope, and — crucially — the address-of OPERATOR already
half-exists (`op_AddressOf` → `InferApp.inferPrefix` types `&local` as the byref intrinsic;
`Freeze/Apply.fs` lowers it to a hardcoded `ldloca`). Only **Piece 1** finishes the
shadow-set kill this doc is about; the operator work has no consumer yet (user, confirmed
2026-07-09 — no pressing consumer; goal is eventual full F# coverage for native lowering), so
it was split out.

**Piece 1 — type identity `"&"` → `Vesper.byref` — LANDED.** Pure currency change:
`RuntimeNames.byrefName` flipped `"&"` → `"byref"`. Fully centralized — the sole producer
(`MetadataSymbols.tryBuildType`, `t.IsByRef`), the `&local` mint (`InferApp.inferPrefix`), the
recognizers (`TyByref`, `isStructuralConstructorName`, `TyStructuralCtor`) and the three CLR
codegen sites (`ClrEncoder`, `ClrExternalMembers` ×2, `EmitClosures`) all route through
`byrefName`/`byrefKey`, so no consumer needed a per-site edit; the rest were sited comments
spelling the old `"&"` identity. No contract authored — byref is never written in source today,
so nothing resolves the *name* `byref` from a provider; the identity is self-consistent through
the `byrefKey` constant. The byref end-to-end tests (`Span<char>` byref indexer read;
`Int32.TryParse(s, &r)` out-param) are the behavior-preserving regression guard.

**Pieces 2–3 — SPUN OUT to `byref-address-of-plan.md` (2026-07-09), deferred (no consumer).**
The `byref<'T>` contract (`prim-types-byref.fsi`/`.fs`) + source-writable byref annotations,
and the `~&` address-of OPERATOR proper — turning the `op_AddressOf` front-end special-case
into a resolved `(~&)` contract symbol with a `[<LocatorValue>]` lvalue judgment and
context-selected `ld*a` codegen (`ldloca/ldflda/ldsflda/ldarga/ldelema`), generalising beyond
the single mutable-local case that works today. Design-first; gated on an actual byref
*producer* consumer materialising.

### 6. Custom numeric literals — SPUN OUT to `custom-numeric-literals-plan.md` (2026-07-08)

Surfaced by 5a (a bigint LITERAL is a custom numeric literal, not a primitive constant) and split
to its own plan. `bigint` the TYPE stays the Stage-5a `prim-types` intrinsic; the literal mechanism
(`NumericLiteral<suffix>` resolution + syntactic desugaring, replacing the `InferLiterals`
`NumBigInteger*` stand-in) is tracked there.

## Deferred / verify (recorded, not scheduled)

- **Capability interfaces** (`disposable`/`equatable`/`comparable`) — decisions locked (user,
  2026-07-08), and the work SPLIT into two independently-shippable pieces once the true blast
  radius was scouted:

  *Decisions.* (a) Apply to ALL THREE anchors (they share the `extern with abstract member` form
  and are all repr-carrying `Class`es today) — migrating one would keep `CapabilityFace` alive and defeat
  the payoff; all or none. (b) Share ONE `IntrinsicIdentity` record between `Intrinsic` and the
  planned `IntrinsicInterface`, accepting the `Platform = None` POLARITY (scalar `None` =
  unrepresentable, gate rejects; interface `None` = backend-anchored on JS, normal) — SAFE because
  the unrepresentability gate + forward-repr harvest are `Intrinsic`-ONLY by construction and never
  see an interface, so no shared match arm reads `Platform` across both polarities; it is a
  sited-comment concern, not a live hazard (the only future risk is a generic helper over
  `IntrinsicIdentity` that reads `Platform` — none exist).

  *Piece 1 — the (c) drift closure — LANDED 2026-07-08 on the EXISTING repr-carrying `Class`
  representation, NO DU case.* The two `IsInterface` partition tests
  (`MetadataSymbols.tryBuildType`, `UnificationTranslate.externalClassTy`) were redundant the
  moment the DEAD interface entries left the reverse-canon map. Scouted: `IntrinsicReverseCanon`
  has exactly three readers — `MetadataSymbols` (guarded out by `not t.IsInterface`),
  `NumberCovariance` (`number` token only), the composition fold (pure aggregation) — so
  `System.IDisposable → disposable` was dead weight (capability matching reconciles via
  `CapabilityIdentity`, not the reverse map). Dropping the `TyparCapture` reverse fold's
  `Class { CapabilityFace = ValueSome }` arm removes those entries; with no interface canon in the
  map, both guards are provably no-ops (an interface name just misses the lookup → `FTClass`/
  `TyClass` either way) and were deleted. The "change both or drift" coupling is GONE; the
  repr-carrying `Class`, `ClrEnv.externalClassRef`, and `resolveAnchor` are untouched.

  *Piece 2 — the `IntrinsicInterface` DU-case representation cleanup — LANDED 2026-07-08
  (SA 776 / Clr 1253 / Js 348 / Vesper 51).* Added `ExternalTypeShape.IntrinsicInterface of {
  Id: IntrinsicIdentity; Members: ExternalMember[]; Origin: SymbolOrigin }` and DELETED
  `CapabilityPlatformFace` + `ExternalClassShape.CapabilityFace` (every plain class dropped the
  mostly-`None` field). Key deviations from the original sketch, forced by the code:
  - **`Origin` IS carried** (the sketch said "no `Origin` needed, resolve to `TyClass(Id.Canon)`").
    A capability interface's VALUE resolution key is `externalTypeKey Origin` (asm-qualified
    `Vesper.Core`), NOT the asm-blind `Id.Canon` — resolving to `Id.Canon` would change the key's
    asm and break exact-`=` subtype compares. Carrying `Origin` keeps the `TyClass` identity
    byte-identical to the repr-carrying `Class` it replaces; `Id.Canon` is the reconciliation key only.
    `Origin` is stamped by `ExternalSymbols.stack`'s `stampType` (new `IntrinsicInterface` arm),
    exactly as a `Class`'s is.
  - **Produced at FINALIZE, not extraction** (mirrors `PendingIntrinsicClasses` for `obj`/`exn`).
    A new `ExtractCtx.PendingCapabilityInterfaces` side table is recorded at the `Extern` arm;
    `finalizeDeferred` republishes each as `IntrinsicInterface` AFTER the interface member-copy
    loop populates `shape.Members` (minting at extraction would capture empty members → conformance
    sees zero members).
  - **The `Extern` `| _ ->` arm handles TWO cases, split by `shape.IsInterface`.** All-abstract
    body (`disposable`/`equatable`/`comparable`) → `IntrinsicInterface`. A CONCRETE-member intrinsic
    (`widget`, `member Poke` — synthetic-only, no real contract uses it) is NOT an interface and
    STAYS a member-bearing `Class` (resolving to `TyClass`, members via `TryLookupMember`). Missing
    this split is what the `widget` test caught.
  - **CLR-only.** On JS no `.fs` binds the repr, so a capability stays a plain canon-only
    interface `Class` (the `resolveAnchor`/`PlatformTypes` JS arms match `Class`, the CLR arms match
    `IntrinsicInterface`). This is now the CLR/JS asymmetry the tests assert.
  Reachable structural consumers handled (the blast-radius finding held — non-exhaustive `| _ ->`
  fallbacks FS0025 does NOT flag): conformance (`Unification.checkInterfaceConformance`, OR-pattern
  binding `Members`), the `isInterface` gate, the record→interface widen (`Engine.tryStructuralWiden`),
  the member-name list (`Subsume`), `subtypeInterfacesOf`'s key-mint (`EngineCore`), `mkNominal`
  (VesperLib freeze → `FTClass`), the arity/keyOf resolvers (`Scope`, `Translate`), and the variance
  `mapShape`. Audited-and-correct-by-default (an interface is not enumerable / constructible / a
  static-access class / a for-in source): `InferCtor`, `InferControlFlow.tryForInEnumerator`,
  `InferResolve.isExternalClass`, `Freeze/Resolve.underlyingClassName`, `Infer.externalInterfaces`,
  `EngineCore.canonKey`/`intrinsicPlatformName`. The projection helper the sketch proposed was
  dropped in favour of explicit arms (each reads one field; OR-patterns binding `Members` cover the
  conformance/widen/member-list sites without a synthesized-shape allocation). CAUTION (sited on
  `IntrinsicInterfaceShape.Id`): interface `Platform = None` = backend-anchored (JS), the OPPOSITE
  polarity of a scalar's `None`.

  *Sequenced-after polish (optional, not load-bearing):* declare the anchors `type disposable =
  extern interface with …` and switch VesperLib from `bodyIsInterface` INFERENCE
  (`VesperLib.fs:1188`, "all-abstract body IS an interface" — already correct for the flat anchors)
  to the DECLARED `kindTag`, matching the `extern class` precedent (`obj`/`exn`) and robust for a
  marker (zero-member) or default-method interface. `ExternKind.Interface` + the `TypeParsing`
  parse path already exist; only the `.fsi` SIGNATURE parser (`SignatureParsing.fs:394`, currently
  `opt (pClass |>> ExternKind.Class)`) needs widening to accept `pInterface`. Not required for
  Piece 2 (inference already yields interface-ness), so it must not gate it.
- **`top` vs `obj` split — DEFERRED 2026-08-03, and the naming reversed.** `obj` conflates the
  value ⊤ (JS `unknown`) with the heritable class root (JS `Object`); CLR collapses both to
  `System.Object`.

  What is being done instead, now: JS binds `type obj = (# class "Object" #)`, mirroring
  `exn = (# class "Error" #)` — `(# class … #)` already means something on JS, and heritability
  in the contract is a claim about the declaration, not a demand that the emitter support
  `inherit` (nothing inherits `obj`). That removes the `prim-types-object.fsi` heritability
  disagreement without a per-target contract split. It is inert at runtime: JS emission is
  type-erased, so no `obj`-typed body observes the repr text.

  If the split lands, `top` takes the ⊤ meaning (JS `unknown`, CLR non-heritable `System.Object`)
  and `obj` stays the class root — the REVERSE of this bullet's earlier "the high-frequency ⊤
  meaning should keep the default name". Reasons: the contract already declares
  `type obj = extern class with new: unit -> obj`, `obj` is the name that appears in
  upcast/inherit positions, and only the ⊤ sites need renaming, which is the bounded edit.

  The forcing function is **`.d.ts` emission**, not runtime behaviour: `Object` is the wrong TS
  spelling for a ⊤ parameter (it excludes `null`/`undefined` and admits primitives only boxed).
  The ⊤ sites are countable today — `IFormatSink.Child`, the `structural-printer.js.fs` helper
  signatures, and `structuralEquals` / `structuralHash` / `structuralCompare`. Until declarations
  are emitted, the distinction has no observable consequence.
  (`feedback_prototype_correct_semantics_over_fsharp_parity`.)
- **Intrinsic-abbrev self-type mints — LANDED 2026-07-09 (SA 777).** Both self-type mints
  (`SideTables.IntrinsicAbbrevInfo.MkSelfType`, `Elaborate.tryIntrinsicAbbrevType`) minted
  `RuntimeNames.primitiveKey name` (hardcoded `ns = "Vesper"`, `intrinsicKey` having been
  deleted in Stage 4), a latent split-brain for a NON-Vesper user intrinsic-abbrev: the member
  `ThisTy` carried `Vesper.X` while use sites resolved `X` to its real `declNs.X`. Fix: the
  resolved intrinsic identity (`TypeRegistry.intrinsicKeyOf ctx.Types name`, the single
  local-mint resolver, stamped at registration just before the host is constructed) is now
  stored as a NEW `IntrinsicAbbrevInfo.SelfKey` field (distinct from the arity-suffixed `Key`
  used for the member-harvest host path); both mints read `info.SelfKey`. The existing
  `ExternMemberElabTests` `widget` fixture (declared in `module Widgets`, so already a
  non-Vesper case) was strengthened from `simpleName`-only to FULL-key equality between the
  member self-type key and the `idW` use-site key (plus a `qualifiedName ≠ "Vesper.widget"`
  guard) — the assertions that catch the divergence.
- **Coverage gap**: a dedicated opaque-fallback test (a genuinely-unknown NON-primitive name)
  for `Translate`'s opaque branch — a literal RHS now routes through `ctx.Intrinsics`, so the old
  test no longer exercises it.

## Relevant memories

`feedback_mockbuiltins_is_a_trap` (the shadow set is the trap), `feedback_redesign_doc_first`
(this doc), `feedback_freeze_no_backend_knowledge` (identity asm-blind; platform repr stays in
the backend), `feedback_dynamic_intrinsics_over_du_cases` (one `TyConst` identity, resolved not
authored), `feedback_plan_docs_ephemeral` (delete on landing).
