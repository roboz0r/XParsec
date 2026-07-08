# Contract-sourced intrinsic identity (kill the front-end shadow set)

**Status (2026-07-07): the single live plan for this work.** Its predecessors LANDED and were
rolled up and deleted: `qualified-intrinsic-identities-plan.md` (intrinsics got qualified
`SymbolKey`s, ns still shadow-set-derived) and `resolution-time-intrinsic-reconciliation-plan.md`
(`obj`/`exn` became `IntrinsicClass` heritable primitives; `System.Object`/`System.Exception`
canonicalize eagerly at resolution; `canonName`'s reverse tier, `normalizeObj`, and
`isSystemObjectKey` are deleted). All suites green: SA 765 / Clr 1253 / Js 348 / Vesper 49.
Delete this doc when the residue stage lands (`feedback_plan_docs_ephemeral`).

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
- **SA static sweep complete.** The only `BuiltinTypes.ty*` left in live code are the deferred
  three: `tySeqInt` (Tast, InferApp, InferControlFlow — a deletion, not a rename), `tyUndefined`
  (Freeze/Apply — JS-only, no CLR contract), `tyBigInt` (InferLiterals — no contract yet).

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

### 2. Stage 3 — codegen repr validation

The forward-repr VALUE is an opaque platform string; codegen must map it to a known IL/JS type or
**emit a diagnostic** on an unrecognized repr (no silent mis-emit). `ClrEnv.TryPrimitiveRepr`
takes the canon `SymbolKey` directly (codegen already holds `FTConst(key, …)`), retiring its
`RuntimeNames.intrinsicKey name` bridge.

### 3. Stage 4 — the shadow set falls out

- **Re-source the remaining producer mints** still on `RuntimeNames.intrinsicKey`: the `VesperLib`
  extraction canons (`Intrinsic`/`IntrinsicClass`/`CapabilityFace` — the qualified `compiled` is
  in hand at each mint site; FIRST verify the harvested keys are qualified for the array/byref
  generics, else ns diverges). The `PassContext.IntrinsicReverseCanon` local invert is already
  re-sourced (`TypeRegistry.intrinsicKeyOf`).
- Then: delete `knownIntrinsicNames`; give the enumerated opaque mints an explicit
  `opaqueKey name = TypeKey(None, "", name)` (the `Translate` opaque fallback, the
  `MemberRegistration` opaque member mint, `VesperLib/TypeTranslate`, `TsManifestTypes` — these
  are INTENTIONALLY `ns = ""`, not registered intrinsics; do not convert them to resolution);
  collapse/retire `intrinsicKey` (~116 call-site lines across 39 files, mostly codegen `FTConst`
  mints — each either reads a resolved key already in hand or is an opaque mint); delete
  `intrinsicNamespace`; delete the static `BuiltinTypes.ty*` once the residue stage clears the
  last three. Compiler-driven: comment a static → Build → the FS0039 list is the worklist.
- `primitiveSupports` + pure-identity checks key on resolved identity. The *verdicts* (value
  types are structurally equatable/comparable; `string` is an equatable reference type) are
  language rules the passes own; only the KEY changes. Frontier (out of scope): derive the
  enumeration from contract capability interfaces via `FrozenInterfaces`.

### 4. Stage 5 — residue (then delete this doc)

- `prim-types-bigint` contract (CLR `System.Numerics.BigInteger`, JS `bigint`) → migrate the
  `InferLiterals` `tyBigInt` arm.
- `tySeqInt` — DELETE and retype range expressions (`1..10`, `1..2..10`) properly; the only
  consumer whose behaviour changes, not just resolves differently.
- `undefined` — JS-only intrinsic; migrate the two `Freeze/Apply` sites once its contract story
  is settled (it has no CLR binding by design — `PlatformTypes` gates representability).
- `objnull` — NOT a primitive: reclassify as the ordinary `obj | null` `TyOr` alias.
- Numeric spelling aliases (`uint`/`int8`/`uint8`/`double`/`single`) resolve-through-alias to
  their canonical intrinsic; no distinct identity.

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
