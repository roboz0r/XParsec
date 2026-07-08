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
- **Shapes carry `SymbolKey` canon**: `ExternalTypeShape.Intrinsic(canon: SymbolKey, arity, platform)`
  and `IntrinsicClass(canon, arity, platform, baseType, members)` — the heritable primitives
  (`obj`/`exn`, from `(# class "…" #)` + `extern class with inherit/new:`). `IntrinsicClass`
  resolves to `TyConst(canon)` exactly like `Intrinsic` (value sites untouched, platform member
  routing intact); its class surface feeds `subtypeParentOf` (base chain), `resolveInheritParent`
  (downstream `inherit exn`), `Unification.fillBaseCtorCall` (the `inherit exn(args)` arg check
  against the contract `.ctor`s — target-agnostic), and codegen `IntrinsicClassBase`
  (heritability = the shape lookup; the `class` kind tag IS the predicate).
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

### 1. Stage 2 — `SymbolKey` currency in the unifier

Reframed after the resolution-time milestone and now much smaller than first planned: the roots
canonicalize at resolution and `canonName`'s reverse tier is already gone, so this is a **currency
change**, not new resolution logic. Do not rebuild a reverse-map tier into it.

- `canonName : PassContext -> string -> string` (`EngineCore.fs`) becomes
  `canonKey : PassContext -> SymbolKey -> SymbolKey`: local `IntrinsicReprTypes`/`IntrinsicKeys`
  → provider `Intrinsic`/`IntrinsicClass` canon via the open scope; a non-reconciled nominal
  returns its own key.
- `subtypeNominalOf` returns `struct (SymbolKey * args)`; `subsumes`/compare consumers go exact
  `=` on canonical keys. Compile-driven: flip the signatures and the error list in
  `Engine`/`EngineCore`/`Subsume` IS the worklist.
- Re-key `PassContext.IntrinsicCanonCache` to `Dictionary<SymbolKey, SymbolKey>`.
  `numericFamilyOr` (the JS `number`-family widening — the reverse map's one unify-time reader)
  drops its `intrinsicName` stringify at the lookup; the reverse map's KEYS stay `string`
  (platform runtime names, the genuine string boundary).
- Delete the `SymbolKeyOps.intrinsicName` bridges as each goes dead; end state deletes
  `intrinsicName` itself (repr-bridge feeds use `simpleName`; identity checks compare resolved
  keys / recognizers).
- **`sameTypeAsmBlind` retirement tail**: with resolution canonicalizing every producer and the
  compare sites on exact `=`, its domain shrinks to any residual genuinely-cross-asm BCL face —
  audit what is left and delete it if empty.

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
  profile differs).
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
