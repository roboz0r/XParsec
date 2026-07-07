# Qualify intrinsic type identities (carry the namespace) — resolved (a), LANDED

**Status (2026-07-06).** LANDED & GREEN (full suite: SemanticAnalysis 762, Codegen.Js 348,
Codegen.Clr 1251, Vesper 49, XParsec.FSharp 1440 — 0 failures). Decision resolved by user:
**option (a)** — `TyConst`'s (and `FrozenType.FTConst`'s) payload changed from bare `string` to
`SymbolKey`, so an intrinsic carries its qualified identity (`ns = "Vesper"`, `asm = None`,
verbatim bare `name`) like every other type. `intrinsicKey` (single mint source) lives in
`RuntimeNames.fs`; `SymbolKeyOps.simpleName key` recovers the bare codegen/repr key. Ephemeral per
[[feedback_plan_docs_ephemeral]] — delete once the two remaining cleanups below land.

**Also landed: identity-matching discipline for the well-known intrinsics.** The mechanical
sweep initially recovered intrinsic names everywhere via `SymbolKeyOps.simpleName` — which is the
*human-facing, arity-stripping* projection, wrong for name resolution / semantic analysis. Fixed
across the SemanticAnalysis passes: discrete checks now match by KEY IDENTITY through the
`IntrinsicTypePatterns` active patterns (`TyBool`/`TyUnit`/`TyObj`/`TyString`/`TyDynamic`/`TyArray`/
`TyByref`/`TyStructuralCtor`/`FTUnit`, in `RuntimeNames.fs`); same-type checks use `k1 = k2`; the
canon→platform string-keyed maps use the non-lossy `SymbolKeyOps.intrinsicName`. `simpleName` is now
reserved for diagnostics/display and backend name mangling (its documented purpose).

**Also landed (surfaced by this work): interfaces are no longer `TyConst`.** The subtype walk's
external branch (`EngineCore.subtypeInterfacesOf`) used to wrap a class's implemented interfaces as
`TyConst(name, args)` — a category error, since an interface as a value's static type is already a
`TyClass` (an `ExternalTypeShape.Class` with `IsInterface`). It now surfaces them as
`TyClass(qualifiedTypeKey name 0, args)`, matching the local branch and every other interface site.
That also removed the transient `SymbolKeyOps.rawName` band-aid (needed only because `simpleName`
stripped the `` `N `` arity off the interface name a `TyConst` was wrongly carrying).

## The mistake being fixed

An intrinsic (`int`/`string`/`bool`/`[]`/…) resolved to a **bare** `TyConst("string", …)` — the
declaring namespace (`Vesper`) was dropped, while a nominal `Widgets.widget` carries
`TyRecord(TypeKey(Some asm, "Widgets", "widget"), …)` structurally. Intrinsics were the ONE
identity class that threw its namespace away. This milestone gives them a `SymbolKey` like every
other nominal.

## Two findings that make (a) clean (the plan's original fears no longer apply)

1. **The typar-marker role already left `TyConst`.** The declaring-type typar marker is now a
   dedicated `SemType.TyTypar of axis * index` (and `FrozenType.FTTypar`), ~84 uses. The old
   `TyConst("'A", [])` marker is gone; the `SemanticInfo.fs` doc-comment on `TyConst` that still
   mentions it is STALE and is corrected by this change. So `TyConst`'s live roles are only:
   - **nominal intrinsics** — `int`, `bool`, `string`, `unit`, `char`, every numeric, `obj`, `exn`,
     `decimal`, `undefined`, `seq<int>`, …;
   - **generic intrinsics** — the array `[]`/`[,]`/… (`RuntimeNames.arrayName`) and the by-ref `&`
     (`RuntimeNames.byrefName`), which forward type args;
   - **opaque/reserved names** — `null`/`undefined`/`never` anonymous-union members and the
     Translate opaque fallback for an unresolved bare name (many unresolved names are already the
     separate `TyUnknown of name` case, not `TyConst`).

2. **The `name` component stays the verbatim current string, so emit is byte-identical.** The
   codegen/repr identity axis (`ClrEnv.TryPrimitiveRepr`, `IntrinsicReprTypes`,
   `EngineCore.canonName`, JS `IntrinsicForwardRepr`) keys on the **bare short name** and must stay
   bare. We satisfy that for free: the intrinsic's `SymbolKey.name` IS the exact bare string it is
   today (`"int"`, `"[]"`), so `SymbolKeyOps.simpleName key` reproduces the string every consumer
   already matches. The namespace rides only in structural equality / Freeze identity.

## The key scheme (single source: `intrinsicKey`)

Every `TyConst` mint from a bare name routes through ONE helper so all mints of the same intrinsic
compare EQUAL (unification depends on it):

```
// name = the verbatim current bare string; ns = "Vesper" for a known intrinsic, "" otherwise.
// asm = None: an intrinsic's home assembly is target-dependent (Vesper.Core on CLR, a JS module
// on JS), so it is deliberately asm-blind — matching the RuntimeNames `sameTypeAsmBlind` convention
// and keeping backend/home knowledge out of the identity (feedback_freeze_no_backend_knowledge).
intrinsicKey (name: string) : SymbolKey = SymbolKey.TypeKey(None, nsOf name, name)
```

- `nsOf name = "Vesper"` when `name` is in the known-intrinsic set (numerics ∪
  {bool,char,string,unit,obj,objnull,voidptr,exn,decimal,undefined,seq<int>} ∪ array ranks ∪ byref),
  else `""`. Array/byref keep their `RuntimeNames` names verbatim (NO `` `1 `` arity suffix — the
  name field is the identity string, not an arity-qualified registry name).
- `BuiltinTypes.tyInt = TyConst(intrinsicKey "int", EqArray.empty)`, and so on for all 25.
- Home for the helper: `BuiltinTypes` (it already owns the canonical intrinsic `SemType`s and can
  see the numeric-name set via `RuntimeNames.numericTypeNames`).

## The sweep — exactly two rules

Changing the DU payload makes the whole solution not compile until every `TyConst`/`FTConst`
site is updated. Every site is one of:

- **Producer** `TyConst(<bareName>, args)` → `TyConst(intrinsicKey <bareName>, args)`. If the site
  already holds a `SymbolKey` (e.g. re-wrapping a destructured key), pass it through unchanged.
- **Consumer / matcher**
  - `TyConst(name, args) -> … name …` → `TyConst(key, args) -> …` binding
    `let name = SymbolKeyOps.simpleName key` (or inline `SymbolKeyOps.simpleName key`).
  - literal match `TyConst("bool", _)` → `TyConst(key, _) when SymbolKeyOps.simpleName key = "bool"`
    (or match a `RuntimeNames`/`BuiltinTypes` recogniser where one already exists — e.g. array uses
    `RuntimeNames.isStructuralConstructorName (SymbolKeyOps.simpleName key)`).
  - the `FrozenType` structural-eq shortcut `FTConst(n1,_), FTConst(n2,_) -> n1 = n2` →
    `k1 = k2` (SymbolKey equality — strictly finer, and correct: two intrinsics are equal iff their
    qualified identities are).

**Emit invariant to hold at every consumer:** extract the intrinsic's name with
`SymbolKeyOps.simpleName` (bare), NEVER `qualifiedName`. Any codegen path that rendered the
`TyConst`/`FTConst` name into IL/JS output must keep rendering the bare form. This is the
byte-identical-emit guardrail and the thing to check in review.

## Foundation (done first, by hand)

`SemanticInfo.fs` — 4 structural edits only (`mapChildren`/`iterChildren`/`forallChildren`/
`existsChild` already wildcard the first field, so they are untouched):
1. `| TyConst of key: SymbolKey * args: EqArray<SemType>` (was `name: string`); fix the stale
   typar/`"'A"` prose in the doc-comment.
2. `| FTConst of key: SymbolKey * args: EqArray<FrozenType>`.
3. Bridge: `toFrozenWith` `TyConst(key, args) -> FTConst(key, …)` and `instantiateWith`
   `FTConst(key, args) -> TyConst(key, …)`.
4. The `FTConst` structural-eq shortcut (`n1 = n2` → `k1 = k2`) and the two `FTConst` maps
   (895, 940) that carry the payload through unchanged (rename `name`→`key`).
Then `BuiltinTypes` + `intrinsicKey`.

## Delegation model

- By hand (judgment-heavy producers/matchers, so the SemanticAnalysis project compiles): the mint
  sites (`Passes/Unification/Translate.fs`), the unification passes, `PrintfSpec.fs`, the `Freeze/*`
  projection, `MemberRegistration.fs` / `TypeRegistration.fs`, `ExternalSymbols.fs`,
  `RuntimeNames.fs`.
- To cheap sonnet subagents (mechanical `destructure→simpleName` per the two rules, disjoint file
  sets, rulebook + API handed over): `Codegen.Clr/*`, `Codegen.Js/*`, and the ~119 test
  expectations. They cannot build the whole solution (it is red until every file lands), so their
  edits are integrated and the build is driven centrally, iterating on the residual error list.

## Repr maps stay bare-keyed (unchanged)

`IntrinsicReprTypes` (`Dictionary<string,string>`), `IntrinsicForwardRepr`,
`IntrinsicReverseCanon`, `canonName`, `intrinsicPlatformName`, `ClrEnv.TryPrimitiveRepr` all keep
their bare-name keys. Consumers feed them `SymbolKeyOps.simpleName key` (the same string as before),
so no rekeying is needed. The registration site (`TypeRegistration` `IntrinsicReprTypes.[name] <- …`)
already has the bare name in hand — unchanged.

## Remaining cleanups (this doc stays until both land)

**A. `TyInterface` — a first-class case (deliberate follow-up, NOT this block).** Agreed with user:
interfaces deserve their own `SemType`/`FrozenType` case, not a shared `TyClass`. Motivators that make
it correct-by-construction (any one justifies the split, like `TyEnum`'s precedent): declaration-site
variance (`IEnumerable<out T>`) modeled in unify/subsume; structural / witness satisfaction distinct
from class inheritance; **multiple inheritance** (interfaces compose, classes don't); forbidding an
interface where an instantiable type is required (`new I()`). Today interface-ness is a mere
`ExternalClassShape.IsInterface: bool` and interfaces flow as `TyClass`, so introducing `TyInterface`
now would be a re-tagged `TyClass` touching every `TyClass ->` arm with no payload difference — do it
WHEN one of those features lands, at which point the `TyClass` this milestone put in
`subtypeInterfacesOf` becomes `TyInterface`. Write a design doc first.

**B. Array naming normalization (below).**

## Downstream this unblocks (do after the identity change lands green)

Array naming normalization (surfaced by the W9 array-index slice): array's runtime identity is the
clean `RuntimeNames.arrayName 1 = "[]"`, but its MEMBER-CONTRACT identity is the backtick-escaped
`` ``[]`` `` (`RuntimeNames.arrayContractName`), because VesperLib's `nameOfTok` keeps the raw
source token and `arityName`'s backtick-guard suppresses the `` `1 ``. Once intrinsic identity is a
`SymbolKey`, normalize array naming so the contract identity aligns with `"[]"`, DELETE
`arrayContractName`, and let array/string share one clean member-lookup path (string already keys
cleanly as `"string"`). Gated by, and downstream of, this identity change.

## Relevant memories
[[feedback_dynamic_intrinsics_over_du_cases]] (keeps intrinsics as `TyConst`, just qualified — one
case, no new SemType cases), [[feedback_freeze_no_backend_knowledge]] (asm-blind identity; bare
codegen canon stays in the backend), [[feedback_plan_docs_ephemeral]] (delete this doc when landed),
[[feedback_hotspot_engineering_diminishing]] (the deliberate structural fix, scoped on its own).
