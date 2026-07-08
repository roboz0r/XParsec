# Contract-sourced intrinsic identity (kill the front-end shadow set)

**Status (2026-07-07): the single live plan for this work.** Its predecessors LANDED and were
rolled up and deleted: `qualified-intrinsic-identities-plan.md` (intrinsics got qualified
`SymbolKey`s, ns still shadow-set-derived) and `resolution-time-intrinsic-reconciliation-plan.md`
(`obj`/`exn` became `IntrinsicClass` heritable primitives; `System.Object`/`System.Exception`
canonicalize eagerly at resolution; `canonName`'s reverse tier, `normalizeObj`, and
`isSystemObjectKey` are deleted). All suites green: SA 765 / Clr 1253 / Js 348 / Vesper 49.
Delete this doc when the residue stages (5a bigint — LANDED 2026-07-08 / 5b ranges — LANDED
2026-07-08 / 5c nullability) and byref land (`feedback_plan_docs_ephemeral`). Custom numeric
literals — surfaced by 5a — was spun out to its own plan (`custom-numeric-literals-plan.md`); the
Route-1 seq-operator design surfaced by 5b was spun out to `range-operators-plan.md`.

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
   faced/plain `Class` until finalize).
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
  Engine `primitiveSupports`, `tryExternalReceiver`) are Stage-4 work.
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
canon+CLR repr) and `.js.fs` (`(# "bigint" #)` JS platform face); wired the `.fsi`/`.fs` into
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
- Rejection is emitted at the ELABORATION choke point (`FreezeExpr.translateExpr`'s `Range` arms),
  which fire ONLY for a range NOT consumed by the `ForTo` lowering — value position, a stepped range,
  or a non-simple for-in binder — all genuinely unsupported. Inference can't tell a for-in source
  from a value, so the position-aware place is lowering. `for i in 1..10` (unit step, simple binder)
  is unaffected — it never reaches the `Range` arm.
- `CoverageTests` retyped: the two "types as seq<int>" tests became "range-as-value rejected"; a
  "counted loop accepted" and "stepped for-in rejected" test added; `Tast`/`Intrinsics` doc refs
  updated. Route 1 (real `(..)` seq operator + counted-loop peephole) spun out to
  `range-operators-plan.md`.

**Stage 5c — `objnull` / `TyOr` nullability (own stage; effort assessed 2026-07-08).**
Goal: `objnull` is NOT a primitive — it is the ordinary `obj | null` union. Making *TyOr
nullability* a realisable feature breaks into three tiers by effort; how far to go is the open
scope question.

- *Already built (no work):* `TyOr` is a first-class `SemType` (smart ctor `mkUnion`/`MkUnion`,
  order-insensitive `UnionMembers` EqSet, frozen mirror `FTOr`), with full directional `subsumes`
  (`T <: T|null` holds via the `src', TyOr ts` arm; `T|null <: T` only when every member subsumes
  `T`) and match-pattern narrowing (`InferControlFlow.computeArmNarrowing`: `:? T` type-tests shrink
  a closed union's residual, with an exhaustiveness warning). The FRONT-END `Translate.fs` ALREADY
  translates a written `T | null` to `TyOr [T; TyConst(opaqueKey "null")]` and bare `null` to that
  reserved `TyConst`.
- *Tier A — finish `objnull` representation (small).* The VesperLib EXTRACTION path diverges: it
  collapses `T | null → T` (`VesperLib/TypeTranslate.fs` `UnionType` arm) and lists `objnull` in
  `isPrimitiveName`. So `objnull`'s abbrev RHS freezes to `FTConst obj` (null lost), and a written
  `objnull` dealiases to plain `obj`. Fix: translate `T | null → FTOr [T; null]` in extraction and
  drop `objnull` from `isPrimitiveName`; rewrite the `VesperLibTests` objnull test (it currently
  asserts the collapse-to-`obj` this inverts). Then `objnull` = `TyOr [obj; null]` end to end.
- *Tier B — `null` as a core intrinsic + platform repr (medium).* Today `null` is `opaqueKey "null"`
  (`ns=""`, not a registered intrinsic, no per-target repr). Per the sibling
  `codegen-js-symbol-provider-plan.md` (§"`null`/`undefined` as JS-intrinsic types"), `null` should
  become ONE cross-backend intrinsic with a per-target repr (JS `null`; CLR `ldnull` / F# 9 `T|null`
  nullable-ref) — sharing the nullable machinery across both targets. Complication: `null` is a
  reserved keyword, so `type null = extern` cannot parse (cf. the `undefined.js.fsi` note); it needs
  a built-in registration path, not a contract file. `EmitJs.validatePlatformTypes` must admit `null`
  / `TyOr [T; null]` members — UNVERIFIED whether a `T | null` program survives JS emit today (the
  sibling plan flags this as the concrete verification entry point). CLR value-type nullability
  (`System.Nullable<T>`/`T?`) is a structurally different repr and stays OUT (deferred).
- *Tier C — flow-sensitive nullability analysis (large; genuinely new).* The "analysis" proper:
  narrowing on `if x <> null` / `x != null` / `!== undefined` GUARDS (not just match patterns) —
  flow-sensitive union-member removal along branches, so the then-branch sees `x : obj`. No such
  flow-typing sub-pass exists today (only the match-pattern `computeArmNarrowing`, a useful template
  but not guard-flow). This is a new flow environment threaded through `if`/`&&`/`||`/early-return —
  the biggest chunk, and separable from Tiers A/B.

  Recommendation: Tier A is the direct "objnull is not a primitive" fix and is small; Tier B lights
  up shared CLR+JS nullable interop and is the prerequisite for `null` surviving codegen; Tier C is a
  standalone feature that can wait. Decide the target tier before starting.

### 5. byref migration (LAST)

The *type* is `byref`; `&` is the operator that makes one (user, confirmed). Current
`RuntimeNames.byrefName = "&"` conflates them. Declare `type byref<'T> = (# "!0&" #)` (and
`byref<'T,'Kind>`) in a new `prim-types-byref.fsi`/`.fs` under `namespace Vesper` ⇒ identity
`Vesper.byref` (verbatim name, arity in args). `&` becomes an operator: prefix address-of
`let inline (~&) ([<LocatorValue>] obj: 'T) : byref<'T> = (# "ld*a" : byref<'T> #)` in the impl
file. `"ld*a"` is deliberate — no single IL opcode fits; Codegen.Clr picks from
`ldloca/ldloca.s/ldflda/ldsflda/ldarga/ldarga.s/ldelema` in context. The semantic analyzer must
enforce that `~&`'s argument is an actual **lvalue** (field, local, array element, argument).
Byref keeps its current hardcoded `byrefName`/structural-ctor handling until the contract + the
operator split land; the recognizers (`TyByref`, `isStructuralConstructorName`) move from `"&"`
to `"byref"` at that point.

### 6. Custom numeric literals — SPUN OUT to `custom-numeric-literals-plan.md` (2026-07-08)

Surfaced by 5a (a bigint LITERAL is a custom numeric literal, not a primitive constant) and split
to its own plan. `bigint` the TYPE stays the Stage-5a `prim-types` intrinsic; the literal mechanism
(`NumericLiteral<suffix>` resolution + syntactic desugaring, replacing the `InferLiterals`
`NumBigInteger*` stand-in) is tracked there.

## Deferred / verify (recorded, not scheduled)

- **`HeritableExternBases` may now be redundant**: downstream heritability flows through the
  provider `IntrinsicClass`; VERIFY whether any self-host path still needs the local set before
  retiring it.
- **Capability interfaces** (`disposable`) still reconcile late via `CapabilityFace`; revisit
  reconciling them at resolution the way the roots were (they resolve to `TyClass`, so the churn
  profile differs). Follow-up design (user, confirmed 2026-07-07): introduce
  `ExternalTypeShape.IntrinsicInterface of { Id: IntrinsicIdentity; Members: ExternalMember[] }`
  as a DISTINCT case — an interface differs on the identity axis (`TyClass` constraint, not
  `TyConst` value identity; excluded from the forward-repr harvest and the unrepresentability
  gate; participates in reverse canon), so per the case-vs-field principle (cleanup item 1) it
  earns a case, where FS0025 exhaustiveness is a feature. Payoffs: DELETE
  `CapabilityPlatformFace` + `ExternalClassShape.CapabilityFace` (every plain class drops a
  mostly-`None` field; `ClrEnv.externalClassRef`'s one-hop face redirect becomes an
  `IntrinsicInterface` arm), and the `IsInterface` partitions in `MetadataSymbols.tryBuildType`
  / `UnificationTranslate.externalClassTy` become data-driven off the published shape kind (the
  "change both or drift" seam disappears). CAUTION: for an interface, `Id.Platform = None`
  means "anchored by the backend symbol table" (JS), NOT "unrepresentable" — the opposite of a
  scalar's `None`; needs a sited comment on the field.
- **`top` vs `obj` split** (JS-only refinement): `obj` conflates the value ⊤ (JS `unknown`) with
  the heritable class root (JS `Object`); CLR collapses both to `System.Object`. Sharpest payoff
  is boxing on JS. The high-frequency ⊤ meaning should keep the default name.
  (`feedback_prototype_correct_semantics_over_fsharp_parity`.)
- **Intrinsic-abbrev self-type mints** (`Elaborate`, `SideTables.MkSelfType`) still mint
  `intrinsicKey name` — a latent split-brain for a NON-Vesper user intrinsic-abbrev, currently
  un-exercised. Route through `intrinsicKeyOf` (or store the resolved key on
  `IntrinsicAbbrevInfo` — note its existing key IS arity-suffixed).
- **Coverage gap**: a dedicated opaque-fallback test (a genuinely-unknown NON-primitive name)
  for `Translate`'s opaque branch — a literal RHS now routes through `ctx.Intrinsics`, so the old
  test no longer exercises it.

## Relevant memories

`feedback_mockbuiltins_is_a_trap` (the shadow set is the trap), `feedback_redesign_doc_first`
(this doc), `feedback_freeze_no_backend_knowledge` (identity asm-blind; platform repr stays in
the backend), `feedback_dynamic_intrinsics_over_du_cases` (one `TyConst` identity, resolved not
authored), `feedback_plan_docs_ephemeral` (delete on landing).
