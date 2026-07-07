# Contract-sourced intrinsic identity (kill the front-end shadow set)

**Status: STEPS 1, 1b, 2 + Step-3 SA-HALF LANDED & GREEN (SA 763, Clr 1251, Js 348, Vesper 49).**
All premises VERIFIED. All design questions RESOLVED (the `(i)/(ii)` and "Step 2 finalized spec"
sections below are now HISTORY — implemented). `ctx.Intrinsics` is live: an honest `tryResolve`
resolver (`SideTables.fs`) with NO fallback — a miss LOUD-FAILS. Every SemanticAnalysis consumer of
the 17 always-available intrinsics now resolves through the contract; the static `BuiltinTypes.ty*`
still coexist (used by codegen + the 3 deferred intrinsics) and are deleted in Step 5.

## FRESH-SESSION ENTRY POINT — remaining work, in order

**Read this section + the two "LANDED" sections (search `LANDED`) and you have the full state.**
Everything below those is design rationale/history, kept for reference; do not re-litigate it.

Code handles: `ctx.Intrinsics` = `IntrinsicSet` + `IntrinsicResolve.tryResolveIntrinsicType`
(`SideTables.fs`, local `IntrinsicKeys` first, else provider via `AmbientOpenPrefixes`, else
`None`→loud-fail in `IntrinsicSet.get`); `TypeRegistry.intrinsicKeyOf` (local-mint resolver);
`PassContextTypes.IntrinsicKeys`. Build/test ONLY via `./claude_tools.cmd -Action Build|Test`
(never raw dotnet). **Do NOT reintroduce a resolver fallback** — retiring fakes / composing test
providers over the real contract is the sanctioned fix when a test loud-fails.

1. **Codegen `IntrinsicSet` (Step 3 remainder).** Codegen.Clr/Js have NO `PassContext`; their
   `BuiltinTypes.ty*` sites (~half the original 188) still use the statics. Give codegen its OWN
   `IntrinsicSet` fed its provider/env (reuse `IntrinsicResolve.tryResolveIntrinsicType`). FIRST
   trace how Codegen.Clr/Js hold their provider/env to decide where the set hangs (one shared handle
   vs per-emit-context). Heavy files: Clr `EmitLoops.fs`(9)/`MetadataSymbols.fs`(6)/`EmitResolve.fs`(4)/
   `Layout.fs`,`EmitBindings.fs`(3); Js `TsManifestTypes.fs`(4)/`NumberCovariance.fs`,`JsNativeSymbols.fs`(3).
   Same defer list (below): bigint / undefined (JS-only, no CLR contract) / seq<int> stay static.
2. **Step 4** — `primitiveSupports` + pure-identity checks key on resolved identity; delete
   `SymbolKeyOps.intrinsicName` (repr-bridge feeds use `simpleName`, identity checks use
   `IntrinsicTypePatterns` / resolved keys).
3. **Step 5 (residue + delete the shadow set)** — add `prim-types-bigint` contract (CLR
   `System.Numerics.BigInteger`, JS `bigint`) then migrate BigInt sites; delete `tySeqInt` +
   retype range exprs; reclassify `objnull` as an `obj|null` `TyOr` alias; numeric aliases
   resolve-through-alias; migrate `undefined` sites once safe; then DELETE the static
   `BuiltinTypes.ty*`, `RuntimeNames.knownIntrinsicNames`, `intrinsicNamespace`. Compiler-driven:
   comment/delete a static → `./claude_tools.cmd -Action Build` → the FS0039 list IS the worklist.
4. **byref migration (LAST)** — see the byref section; new `prim-types-byref` contract + `~&`
   operator + lvalue enforcement + move recognizers off `"&"` onto `"byref"`.
5. **Lower-priority Step-1b cleanups** (behaviour-identical today, safe to defer): provider mint
   path `Translate.fs:531-533`; intrinsic-abbrev self-type `Elaborate.fs:1583` / `SideTables.fs`
   `MkSelfType`. See "NOT yet done / known-deferred".
6. **Restore coverage:** a dedicated opaque-fallback test (genuinely-unknown NON-primitive name)
   for `Translate.fs:217` — a literal RHS no longer exercises it (see the SA-half LANDED note).

Successor to `qualified-intrinsic-identities-plan.md` (LANDED): that milestone gave
intrinsics a qualified `SymbolKey` but sourced the `Vesper` namespace from a **hardcoded**
front-end set (`RuntimeNames.knownIntrinsicNames ∪ numericTypeNames ∪
isStructuralConstructorName`, glued by `intrinsicNamespace = "Vesper"`). This milestone
deletes that shadow set: an intrinsic's identity is **resolved from the `prim-types-*`
contract through ordinary name resolution**, exactly like a user nominal `Widgets.widget`.

## The wart chain that motivates this

1. `SymbolKeyOps.intrinsicName` (last commit) claims to be a "non-lossy" projection but is
   byte-identical to `simpleName` over its whole domain (no intrinsic key's `name` carries a
   `` `N `` suffix, so `bareName n = n`). It is a leaf symptom.
2. The real loss both projections incur is **namespace/assembly**, not arity. Every consumer
   that feeds a bare string into a string-keyed repr map has already thrown away the `Vesper`
   the migration just added.
3. The reason the namespace has to be re-invented downstream at all is that it is **dropped at
   every producer** and re-synthesized from a hardcoded list:
   - self-host `TypeRegistration.registerAbbreviationDefn`: `declNs` (`namespace Vesper`) is in
     hand but `IntrinsicReprTypes.[name] <- repr` is keyed by the **bare** name;
   - harvest `VesperLib.fs:1325`: the shape is stored under the **qualified** `compiled`
     (`Vesper.int`) but its payload is `Intrinsic(short, arity, platform)` with `canon = short`
     (bare); the forward/reverse canon maps (`TyparCapture.fs`) are built from that bare `canon`;
   - `RuntimeNames.intrinsicKey` then re-invents `ns` from `knownIntrinsicNames`.
4. `knownIntrinsicNames` is therefore a hand-maintained mirror of the `.fsi` `extern`
   declarations — the exact `feedback_mockbuiltins_is_a_trap` failure mode. Add
   `type widen = extern` to a contract and forget the mirror ⇒ its `TyConst` silently gets
   `ns = ""`, with no compiler backstop.

## The full intrinsic set is already in the contract (verified)

Every `BuiltinTypes.ty*` maps 1:1 onto an `extern` declaration in a `prim-types-*.fsi`:
`int/sbyte/byte/int16/uint16/uint32/int64/uint64` (`prim-types-int.fsi`),
`float32/float` (`-float`), `decimal` (`-decimal`), `char/string` (`-string`), `obj`
(`-object`), `exn` (`-exn`), `nativeint/unativeint/voidptr` (`-nativeint`), `bool/unit/int`
and the array (`-min`), the nd-array ranks (`-nd-array`), `undefined`/`dynamic` (JS `.js.fsi`).
So a name-resolution lookup for any of these through ambient `open Vesper` **can** find its
qualified identity. The mirror carries no information the contract lacks.

## Target architecture

**Intrinsic identity is resolved, never authored.** The front end holds no static intrinsic
`SymbolKey`s and no name→namespace table. Instead:

- A single resolver builds the intrinsic identity bag **once per compilation**, from the same
  type environment ordinary nominal resolution uses (local cumulative registry for self-host
  Vesper.Core; the `IExternalSymbolProvider` via ambient `open Vesper` downstream). Store it on
  `PassContext` (e.g. `ctx.Intrinsics : IntrinsicSet` with fields `Int`, `String`, `Bool`, …).
- `BuiltinTypes` stops being a bag of module-init constants and becomes the **builder** of that
  bag: `BuiltinTypes.resolve (env) : IntrinsicSet`, resolving each intrinsic by name through the
  environment. Resolution failure (contract lacks the name) is a **loud** pipeline-start error —
  this replaces option (B)'s separate load-time assertion: the contract is authoritative and the
  check is just "did resolution find it," enforced by the same machinery as every other name.
- The 188 `BuiltinTypes.tyX` sites become `ctx.Intrinsics.X` (mechanical rename; resolution
  happens once, not per site — keeps the ergonomics and the perf of a field read).

This makes intrinsic-ness fall out of the resolved shape (`ExternalTypeShape.Intrinsic` /
local `IntrinsicReprTypes` membership → `TyConst`, per existing `Translate.fs`), so there is no
separate "is this an intrinsic" predicate to keep in sync.

### Carry the namespace through the two producers (prerequisite)

- `ExternalTypeShape.Intrinsic`'s `canon` face carries the **qualified** identity (or gains an
  origin/ns field). The extractor has `compiled` (`Vesper.int`) at the mint site
  (`VesperLib.fs:1325`); feed that instead of bare `short`. Forward/reverse canon maps and
  `canonName` then key on the qualified identity — the canonicalization is established in the
  backend from the contract, as intended.
- Self-host `IntrinsicReprTypes` records the `declNs` already passed into
  `registerAbbreviationDefn` and dropped today.

## How far it goes: what is eliminated vs the irreducible language floor

**Eliminated (becomes contract-resolved):**
- `RuntimeNames.knownIntrinsicNames`, `intrinsicNamespace`, the ns-decision inside
  `intrinsicKey` (the mint helper collapses to a plain resolver hit, or is deleted if every
  producer resolves).
- `SymbolKeyOps.intrinsicName` (deleted; the repr-bridge feeds use `simpleName`, the pure-
  identity checks compare resolved keys / `IntrinsicTypePatterns`).
- The static `BuiltinTypes.ty*` constants (become `ctx.Intrinsics.*`).
- `numericTypeNames`/`knownIntrinsicNames` **as namespace sources** (they may survive only if
  still needed as a language-rule enumeration — see floor).

**Irreducible language floor (stays front-end — this is syntax/semantics, NOT type identity):**
- **Literal lexeme → canonical name**: `Token.NumInt32 → "int"`, `KWTrue → "bool"`,
  `CharLiteral → "char"`, string → `"string"`, `() → "unit"`, float → `"float"`. This is the
  language's literal grammar — platform-INVARIANT. `literalCarrier` gains `ctx` and returns
  `ctx.Intrinsics.<name>`; the token→name map is language knowledge, the identity is resolved.
  **Per-platform representability is NOT part of this map** — it falls out of the existing
  `PlatformTypes` gate: `float32`'s canon identity always resolves (it *is* declared in
  `prim-types-float.fsi`), but on JS there is no `prim-types-float.js.fs` binding for it, so its
  `ExternalTypeShape.Intrinsic` carries `platform = None` and `PlatformTypes` already rejects it
  (the same path that catches `decimal`/`nativeint` on JS). So `3.0f` on a JS target resolves
  cleanly as `float32` and is rejected at the platform gate — wire the source token through so
  the diagnostic reads "`3.0f` cannot be compiled: `float32` is not supported on this platform"
  rather than a bare type-name message. The lexeme map itself never becomes platform-aware.
- **Language-semantic constraint verdicts** (`primitiveSupports`: value types are structurally
  equatable/comparable; `string` is an equatable reference type). The *verdict* is a language
  rule the passes own (the provider is a dumb oracle — `feedback` on capability predicates). The
  *key* becomes the resolved identity (compare against `ctx.Intrinsics.*`, not a bare-name set).
  **Frontier:** the value-type/equatable *enumeration* could later be contract-derived if
  `prim-types-*.fsi` declared capability interfaces on the primitives (`int with interface
  IEquatable`), read via `FrozenInterfaces`. Out of scope here; flag as future.
- **Structural-constructor lowering** (array/byref get a structural backend repr): backend
  concern, legitimately not an identity question.

## Bootstrap / ordering argument — VERIFIED, no hazard

A project's ordered files are **concatenated into one source string and compiled as a single
`PassContext`** (`Pipeline.fs:23`; driver concatenation `ClrDriver.fs`, `TestHelpers.fs:301-318`).
Type registration is a **whole-unit pre-pass** (`NameResolution.fs:438-476`, `TypeRegistration.fs:7-10`)
that registers every `type` in the one cumulative `ctx.Types` **before any body is walked** —
so intra-project type visibility is order-INDEPENDENT (even forward references resolve). The
provider stack (`ExternalSymbols.stack`) is **cross-project only** (`SideTables.fs:1397-1401`);
a project's own files are never provider entries.

Consequences for the resolver:
- **Local-first, then provider** — the exact pattern `canonName` uses (`SideTables.fs:1400-1401`):
  self-host finds `int` in `ctx.Types`, downstream finds it in the provider via ambient `open Vesper`.
- **Build `ctx.Intrinsics` lazily after the registration pre-pass** (first access during body
  resolution). There is a clean phase boundary, so it can never race ahead of the declaration.
  No per-file ordering assumption is needed — the earlier "prim-types-min is first" argument is
  moot; whole-unit registration makes all intrinsics present before any literal is typed.

### How the resolver obtains the QUALIFIED key  [RESOLVED — implemented additively: `IntrinsicKeys` index (i-flavoured), see LANDED Step 1/1b]

For `int` to "resolve like a user nominal `Widgets.widget`," its registration must carry the
qualified `SymbolKey` (`TypeKey(None, "Vesper", "int")`, asm-blind per `sameTypeAsmBlind`). Today
the local intrinsic table `IntrinsicReprTypes` is **bare-keyed** (name → platform repr) and lives
*apart* from the nominal `SymbolKey` registry, and `Translate.fs` resolves an intrinsic via
`IntrinsicReprTypes.ContainsKey name -> bare TyConst`. Two ways to make resolution yield the
qualified key (decide at step 2):
- **(i) ns-augment the local table.** `IntrinsicReprTypes` (and the harvested `Intrinsic` canon,
  step 1) carry the ns; `Translate`'s intrinsic branch and the resolver read it. Smaller change.
- **(ii) unify with nominal resolution.** Register intrinsics in the same name→`SymbolKey` index
  as nominals with an "intrinsic" marker, so `int` flows through the *identical* lookup path as
  `widget` and the `IntrinsicReprTypes` table degrades to a pure repr side-table. Most faithful to
  the stated goal; larger blast radius. **Lean (ii) if cheap once (i)'s ns plumbing exists.**

### Remaining risks (smaller, handle during implementation)
- **ctx availability at every `BuiltinTypes.ty*` site.** Literal typing (`InferLiterals`),
  unification, Freeze all have `ctx`. Codegen reads the provider. Test fakes need an `IntrinsicSet`
  seeded from a minimal fake provider — the one place to add plumbing.
- **Freeze boundary.** Identity must be resolved and stamped by Freeze; post-freeze `FTConst`
  consumers read the baked key. Codegen equality checks (`= BuiltinTypes.tyInt`) compare against a
  resolved-once reference, not a fresh mint.

## byref: the *type* is `byref`, `&` is the operator that makes one (user, confirmed)
Current `RuntimeNames.byrefName = "&"` conflates the type with its constructor operator. Correct
model: declare `type byref<'T> = (# "!0&" #)` (and `byref<'T,'Kind>`) in a new `prim-types-byref.fsi`/
`.fs` under `namespace Vesper` ⇒ type identity `Vesper.byref` (verbatim `"byref"`, arity in args, no
suffix). `&` becomes an OPERATOR (produces a byref from an lvalue), separate from the type — the prefix
address-of `let inline (~&) ([<LocatorValue>] obj: 'T) : byref<'T> = (# "ld*a" : byref<'T> #)` (goes in the byref impl
file alongside the type). Byref is the LAST name to migrate: it keeps its current
hardcoded `byrefName`/structural-ctor handling until its contract file + operator split land, so
nothing breaks meanwhile. Recognizers (`TyByref`, `isStructuralConstructorName`) move from `"&"` to
`"byref"` at that point. `"ld*a"` is selected as there's no one IL op-code to choose from. Codegen.Clr
must see `"ld*a"` in context and choose from `ldloca/ldloca.s/ldflda/ldsflda/ldarga/ldarga.s/ldelema`.
The semantic analyzer must strictly enforce that the argument passed to `~&` is an actual **lvalue**
(a field, a local, an array element, or an argument).

## Two implementation subtleties (verified)
- **Uniqueness gate is safe.** Routing intrinsics through the nominal key path (`SymbolKeyOrigins`)
  asserts key-uniqueness, but `.js.fs`/target companions are harvested for the platform face
  (`buildProviderWith`/`targetOverrideFs`), NOT compiled into the impl unit — the base `.fs` is the
  sole impl source, so `type int` is declared once per compilation. No double-declare, gate won't fire.
- **Generic intrinsics mint verbatim (no arity suffix).** `stampLocalTypeKey` arity-suffixes
  (`Point` → `Point`2`); intrinsics must NOT (`[]` stays `"[]"`, `byref` stays `"byref"`; arity rides
  in `args`). So (ii) mints `TypeKey(None, declNs, name)` directly for intrinsics rather than reusing
  `stampLocalTypeKey` verbatim.
- **Transition is behaviour-preserving.** The registered key `TypeKey(None, declNs, name)` equals
  `intrinsicKey name` for every contract intrinsic (their `declNs` IS `Vesper` = `intrinsicNamespace`),
  so resolved keys unify with the still-static `BuiltinTypes.ty*` until the step-3 sweep retires them.
- **Storage is additive.** `IntrinsicReprTypes` is already a pure name→repr side-table; identity is
  synthesized at use via `intrinsicKey`. (ii) adds a name→`SymbolKey` index (`IntrinsicKeys`) populated
  at registration with `declNs`; `Translate` reads it instead of `intrinsicKey`. `IntrinsicReprTypes`
  unchanged.

## Residue (names in the front-end sets with no clean 1:1 contract extern) — RESOLVED
- `seq<int>` (`tySeqInt`) — **DELETE.** An early hack (range-expr result pending generics), not a
  real intrinsic. Remove `tySeqInt` and retype range expressions (`1..10`, `1..2..10`) properly;
  track that retyping as part of this work (it is the only consumer that must change behaviour, not
  just resolve differently).
- `bigint` — a **real intrinsic**: declare `type bigint = extern` in the contract with platform
  reprs `System.Numerics.BigInteger` (CLR, `prim-types-*.fs`) and `bigint` (JS, `.js.fs`). Then it
  resolves through the contract like every other primitive; drop it from the hardcoded set.
- `objnull` — **NOT a primitive.** It is `type objnull = obj | null`, a `TyOr` **alias**. Remove
  from `knownIntrinsicNames` / `BuiltinTypes`; it resolves through ordinary alias resolution to a
  `TyOr`, no intrinsic identity of its own.
- Numeric aliases (`uint`/`int8`/`uint8`/`double`/`single`) — F# spelling aliases, **resolve
  through alias** to their canonical intrinsic (`uint→uint32`, `int8→sbyte`, `uint8→byte`,
  `double→float`, `single→float32`). No distinct identity.

## LANDED — Step 1/1b (local mint-site contract-sourcing)

The **local (self-host) resolution path is now contract-sourced**:
- `PassContextTypes.IntrinsicKeys : Dictionary<string, SymbolKey>` added (`SideTables.fs`), a
  name→qualified-key index. `IntrinsicReprTypes` stays the pure name→repr side-table.
- `TypeRegistration.registerAbbreviationDefn` (the `Type.ILIntrinsic` branch) populates
  `IntrinsicKeys.[name] <- TypeKey(None, declNs, name)` — VERBATIM name, no arity suffix — beside the
  existing `IntrinsicReprTypes` write.
- `TypeRegistry.intrinsicKeyOf (types) (name)` (`SideTables.fs`) is the single resolver: reads
  `IntrinsicKeys`, falls back to `RuntimeNames.intrinsicKey` (defensive). Every pass can call it.
- Guarded local-intrinsic mint sites now route through it: `Translate.fs` (both the nullary ~163 and
  generic ~408 arms) and `MemberRegistration.fs:706`.

Behaviour-identical for the real Vesper intrinsics (`declNs = "Vesper"` = `intrinsicKey`'s ns).

### NOT yet done / known-deferred (start here)
- **Provider (downstream) path** `Translate.fs:531-533` still mints `intrinsicKey short` from the
  qualified `key`. Behaviour-identical today. To contract-source it: build the key from the qualified
  `key` string (split ns/name, VERBATIM name, no `arityName` suffix) — but FIRST verify the harvested
  `ExternalTypeShape` key is qualified (`Vesper.[]`, not bare `[]`) for the array/byref generics, else
  ns diverges. See the array/byref landmine note above.
- **Intrinsic-abbrev self-type sites** `Elaborate.fs:1583` and `SideTables.fs:384` (`MkSelfType`) still
  mint `intrinsicKey name`. Would split-brain vs the use-site for a NON-Vesper user intrinsic-abbrev
  (`type widget = (# "object" #)` in some other namespace); currently UN-EXERCISED (suite green). Route
  them through `intrinsicKeyOf` (they need `PassContextTypes` in hand, or store the resolved key on
  `IntrinsicAbbrevInfo`, which already carries a `stampLocalTypeKey` key — note that one IS arity-suffixed).
- **Opaque/external fallback mints** stay on `intrinsicKey` intentionally (`ns=""` is the opaque
  identity): `Translate.fs:228,464`, `MemberRegistration.fs:718`, `VesperLib/TypeTranslate.fs:570`,
  `Codegen.Js/TsManifestTypes.fs:469-470`. Do NOT convert these — the name is not a registered intrinsic.

## Step 2 — finalized spec  [DONE — implemented; kept for rationale] (bootstrap-in-tests premise VERIFIED)

Investigation confirmed: SA unit tests carry the real contract on the `PassContext` provider
(`test/…SemanticAnalysis.Tests/TestHelpers.fs:32-36` `realProvider`); `MockBuiltins` is gone.
`int` is resolvable as **`Vesper.int`** (`ReferencedProjectTests.fs:114-122`); bare
`TryLookupType "int"` MISSES (`:235`) — bare names resolve via the ambient-open prefix set
(`AmbientOpenPrefixes` includes `Vesper`, `:238-249`). So:

- **Resolver = reuse existing name resolution, do NOT hardcode `"Vesper." + name`.** For each
  intrinsic name, resolve it the way a written `int` annotation resolves: local
  `IntrinsicReprTypes`/`IntrinsicKeys` first (self-host), else the provider through the ambient
  open scope (`OpenScope.tryResolve ctx.Resolution.OpenScope … name`, as `EngineCore.canonName`
  does; or `tryResolveExternalType`). The resolved key is `Vesper.int` either way — behaviour-
  identical to today's `BuiltinTypes.intrinsicKey name`, which is the green guardrail.
- **Safe increment: add `ctx.Intrinsics` ALONGSIDE the static `BuiltinTypes.ty*`; do NOT delete
  the statics in Step 2.** Both mint `Vesper.int`, so they unify — green. The deletion happens in
  Step 5 after the Step 3 sweep retires every consumer.
- **Laziness / failure timing.** Prefer lazy PER-FIELD resolution (`ctx.Intrinsics.Int` resolves
  on first use) over eager-build-all: a test that never types an `int` never triggers its
  resolution, so the two empty-map fake providers only need seeding for the intrinsics they
  actually exercise. Resolution miss = loud error at the use-site (correct — fails exactly where a
  missing intrinsic is needed).
- **`IntrinsicSet` fields** = the current `BuiltinTypes.ty*` minus `tySeqInt` (deleted, Step 5):
  Int, Int64, Byte, SByte, Int16, UInt16, UInt32, UInt64, NativeInt, UNativeInt, BigInt, Float,
  Float32, Bool, Char, Decimal, Unit, String, Undefined. (`bigint` needs its contract first —
  Step 5 — or resolve it via the fallback until then.)
- **Prove-it before the sweep:** a targeted test asserting `ctx.Intrinsics.Int = BuiltinTypes.tyInt`
  (and a few others) under `realProvider` — verifies the resolver yields the identical key without
  touching the 188 consumers. Seed the two fake-provider tests.
- **Fake-provider seed:** `UnificationTests.fs:129-130`, `MemoizeTests.fs:46-47` (empty
  `IntrinsicForwardRepr`/`IntrinsicReverseCanon`) — give them an `IntrinsicSet` (or delegate to
  `realProvider`) so any intrinsic they touch resolves.

## Step 3 — triage (from the compiler worklist: comment the `ty*` block, build)

**SA project: ~92 distinct sites / 15 files** (`ty*` FS0039 count when the statics are commented):
- `Passes/Unification/InferControlFlow.fs` (21), `InferApp.fs` (8), `InferLiteralExpr.fs` (3),
  `InferPat.fs` (2), `InferTypeOps.fs` (2), `Infer.fs` (1), `EngineCore.fs` (1) — **have `ctx`**,
  pure `BuiltinTypes.tyX → ctx.Intrinsics.X` renames.
- `Passes/Unification/InferLiterals.fs` (19) — `literalCarrier`; **needs `ctx` threaded** (takes only
  a `SyntaxToken` today; thread `ctx` to it and its call sites, then `ctx.Intrinsics.<name>`).
- `Passes/NameResolution/MemberRegistration.fs` (7) — has `ctx`.
- `Freeze/Apply.fs` (13), `Freeze/Printf.fs` (8), `Freeze/Access.fs` (4), `Freeze/Strings.fs` (1),
  `Freeze/Resolve.fs` (1), `FreezeExpr.fs` (1) — **verify the Freeze context handle**; Freeze runs
  post-unification, confirm whether it carries `ctx`/an `IntrinsicSet` before renaming.
- `tySeqInt` sites (4, across the above) — **Step 5 DELETION, not a rename** (retype range exprs).
- `tyBigInt` (2 sites) — **DEFER to Step 5.** `IntrinsicSet` now loud-fails on an unresolvable name
  (no silent fallback), and there is no `type bigint = extern` contract yet, so `ctx.Intrinsics.BigInt`
  throws until `prim-types-bigint` lands. Keep these two on the static `BuiltinTypes.tyBigInt` until then.
  (Verify the other intrinsics — `decimal`/`exn`/`obj`/`nativeint`/`voidptr`/`undefined` — DO have a
  contract before migrating their sites; the SA prove-it test only covers the common nine.)

**Codegen.Clr / Codegen.Js: the other ~half of the 188 — NO `PassContext`.** They cannot use
`ctx.Intrinsics`. Codegen carries its own env + provider, so it needs its OWN `IntrinsicSet` built
from that provider (the same `resolveIntrinsicType` shape, fed the codegen provider). This is a
distinct sub-task, sequenced AFTER the SA sweep (codegen won't build until SA is green anyway).
Heavy files (from a `BuiltinTypes.ty` grep): `Codegen.Clr/EmitLoops.fs` (9), `MetadataSymbols.fs`
(6), `EmitResolve.fs` (4), `Layout.fs`/`EmitBindings.fs` (3); `Codegen.Js/TsManifestTypes.fs` (4),
`NumberCovariance.fs`/`JsNativeSymbols.fs` (3).

**Execution order:** (a) thread `ctx` into `literalCarrier` + confirm the Freeze handle; (b) fan out
the SA renames per-file to subagents (edits only), build SA centrally, iterate the FS0039 list to
zero; (c) give codegen its own `IntrinsicSet`, repeat for Clr then Js; (d) only THEN delete the
statics (Step 5). Keep `tySeqInt` until its Step-5 retype.

### Step 3 — SA half LANDED & GREEN (SA 763, Clr 1251, Js 348, Vesper 49)
- 17 intrinsics migrated `BuiltinTypes.tyX → ctx.Intrinsics.X` across the SA project. `bigint`,
  `undefined` (JS-only, no CLR contract), `seq<int>` (deletion) left static — deferred to Step 5.
- Leaf helpers threaded with `ctx`: `Passes/Unification/InferLiterals.literalCarrier`,
  `Passes/Unification/EngineCore.tupleOrSingle` (7 call sites), `Freeze/Apply.optionalDefaultNode`.
- The honest no-fallback resolver surfaced ~18 tests using ad-hoc fake providers that never exposed
  the stdlib intrinsics. Fixed test-only, in the "retire the fakes" direction: composed each fake
  OVER the real contract (`ExternalSymbols.composite [ stub; realProvider ]`) or replaced it with the
  real provider (`realProvider` / `ClrSymbolProviders.build [vesperCoreManifest]` / the JS
  `stackWithAmbient` that keeps the ambient `Vesper` prefix). No production fallback reintroduced.
- **Coverage gap to restore later:** `ResolvedTypesTests "primitive annotations pin to TyConst"` used
  to exercise `translateType`'s opaque-fallback branch (`Translate.fs:217`) via `nullProvider`; a
  literal RHS now routes through `ctx.Intrinsics`, so that test moved to the real Intrinsic path. Add a
  dedicated opaque-fallback test using a genuinely-unknown NON-primitive name to re-cover that branch.

### Step 3 — REMAINING: codegen `IntrinsicSet` (Codegen.Clr / Codegen.Js)
Codegen has no `PassContext`; its `BuiltinTypes.ty*` sites (the other ~half of the original 188) keep
using the statics and stay green for now. To let Step 5 DELETE the statics, codegen needs its own
`IntrinsicSet` fed its provider/env (same `tryResolveIntrinsicType` shape). Until then the statics +
`intrinsicKey` + `knownIntrinsicNames` survive for codegen only.

## Migration path (staged, each stays green)
1. Producers carry ns: `Intrinsic` canon qualified + self-host `IntrinsicReprTypes` ns. Canon
   maps re-keyed. (No consumer change yet; `intrinsicName`/`simpleName` still work.)
2. Add the resolver + `ctx.Intrinsics`; build once; loud-fail on miss. Seed test fakes.
3. Mechanical `BuiltinTypes.tyX → ctx.Intrinsics.X` sweep (188 sites, disjoint file sets).
4. `primitiveSupports` + pure-identity checks key on resolved identity.
5. Delete `knownIntrinsicNames`, `intrinsicNamespace`, `intrinsicName`, static `BuiltinTypes`
   constants. Delete this doc (`feedback_plan_docs_ephemeral`).

## Relevant memories
`feedback_mockbuiltins_is_a_trap` (the shadow set is the trap), `feedback_redesign_doc_first`
(this doc), `feedback_freeze_no_backend_knowledge` (identity asm-blind; platform repr stays in
backend), `feedback_dynamic_intrinsics_over_du_cases` (still one `TyConst` case, just resolved),
`feedback_plan_docs_ephemeral` (delete on landing).
