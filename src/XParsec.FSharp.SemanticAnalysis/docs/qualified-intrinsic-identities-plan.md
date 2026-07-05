# Qualify intrinsic type identities (carry the namespace) — deferred milestone

**Status (2026-07-05).** DEFERRED, not started. Split out of W9 (the TS-provider /
indexer-as-member work) by user decision: W9 lands array/string indexers *keeping array a
bare `TyConst("[]")`*; this doc captures the separate, larger change of giving every
intrinsic identity its namespace, to be scoped and staged on its own later. Ephemeral per
[[feedback_plan_docs_ephemeral]] — delete once landed and fold durable facts into the code.

## The mistake to fix

An intrinsic (`int`/`string`/`bool`/`[]`/…) resolves to a **bare** `TyConst("string", …)` —
the declaring namespace (`Vesper`) is dropped. A regular nominal `Widgets.widget` resolves to
`TyRecord(TypeKey(Some asm, "Widgets", "widget"), …)` — namespace + assembly carried
structurally. So intrinsics are the ONE identity class that throws its namespace away.
This is the "bare name key" the user wants to stop tolerating: intrinsics should carry their
qualified identity like every other type.

## Why it is SYSTEMIC (~205 match sites, ~40 files) — the blast radius

The root is structural: **intrinsics and nominals do not share a representation.**
`SemanticInfo.fs:492-522` —
```
| TyConst of name: string * args: EqArray<SemType>     // intrinsics: BARE string, NO ns slot
| TyRecord of key: SymbolKey * args: EqArray<SemType>  // nominals: full SymbolKey (asm, ns, name)
| TyUnion  of key: SymbolKey * args: …
| TyClass  of key: SymbolKey * args: …
```
`SymbolKey.TypeKey(asm: string option, ns: string, name: string)` (`SemanticInfo.fs:56-58`) is
where a namespace physically lives. `TyConst` has no field for it. So "carry the namespace"
means EITHER:
- **(a) change `TyConst`'s payload from `string` to `SymbolKey`** — mechanically breaks every
  `TyConst(name, …)` pattern in the tree (most invasive; a clean DU-shape change), OR
- **(b) embed `"Vesper.string"` in the name string** — less type-breaking but pushes the burden
  onto ~205 literal-string comparisons + the repr-map keys.

### The linchpin: `BuiltinTypes` is bare
`ExternalSymbols.fs:1677-1704` — all 25 primitives are `TyConst("int", …)`, `TyConst("string", …)`,
etc. Every literal (`InferLiterals`), printf hole (`PrintfSpec.fs:52-53,144-154`), operator result
(`Engine.fs:966,986`), and control-flow predicate (`InferControlFlow.fs:57,139,551`:
`TyFun(_, TyConst("bool", _))`) compares structurally against these bare values. If a mint site
starts producing `TyConst("Vesper.int", …)` while `BuiltinTypes.tyInt` stays bare, annotation↔literal
unification SILENTLY stops matching (`let x: int = 3` fails). So `BuiltinTypes` and every bare producer
must move in LOCKSTEP.

### Mint sites (the leverage points — few, identifiable)
- `Translate.fs:157` — `Type.NamedType` argless intrinsic → `TyConst(name, empty)` (the main one).
- `Translate.fs:398` — generic intrinsic (e.g. `[]`) → `TyConst(name, translatedArgs)`.
- `MemberRegistration.fs:706` — member/type re-resolution → `TyConst(name, args)`, PRECEDED by
  hardcoded bare arms `"int" -> BuiltinTypes.tyInt … "string" -> tyString` (`:699-705`).
- `TypeRegistration.fs:565` — the repr-table KEY: `ctx.Types.IntrinsicReprTypes.[name] <- …` (bare;
  `declNs` is IN SCOPE at `:516` but deliberately discarded — the dict is `Dictionary<string,string>`,
  `SideTables.fs:788`).

Qualifying only the mints does NOT let downstream "follow automatically" — downstream pattern-matches
bare literals and compares against bare `BuiltinTypes`, so the mint change must be COORDINATED with the
match-site + map-key + codegen rewrite.

### Match-site count (the true splash)
| Scope | Bare `TyConst`/`FTConst` literal matches | Files |
|---|---|---|
| `src/` (production) | 86 | 32 |
| `test/` (behavioural assertions) | 119 | 24 |
| extra string compares (`= "int"`, `"int" ->`) in `src/` | ~23 | 12 |

Production hotspots: `ExternalSymbols.fs` (13, incl. `BuiltinTypes`), `PrintfSpec.fs` (7),
`EmitLoops.fs` (6), `MemberRegistration.fs` (5), plus 2-4 each across ~20 CLR/JS codegen files
(`EmitMember`/`EmitResolve`/`EmitTypes`/`ClrRecipes`/`MetadataSymbols`/`NominalEmit`/`EmitCall`/…) and
the unification passes. Spread across the WHOLE compiler, not clustered.

### Both backends deliberately assume the bare canon
- CLR: `ClrEncoder.fs:126-129` rekeys via `PrimitiveRepr` = `ClrEnv.TryPrimitiveRepr name`
  (`ClrEnv.fs:549-552`), a canon→platform lookup (`"int"` → `"System.Int32"`). `ClrEnv.fs:545-548`
  is explicit: the bare canon IS the codegen identity (NOT `TryLookupType`, which is qualified).
  A `Vesper.string` arriving here misses → `failwithf "no IL encoding …"`.
- JS: `NumberCovariance.fs:19,22` keys on `FloatCanon = "float"`; `JsNativeSymbols.fs:31,78` build
  `FTConst("string"/"bool", …)`.
- Plus hard bare-literal arms not behind the map: `ClrEncoder.fs:108,119-121,140` (`"obj"`,
  `"System.HashCode"`, `"[]"`), `EmitLoops.fs`, printf holes.

### Repr maps are bare-keyed (both directions)
`IntrinsicReprTypes` (`Dictionary<string,string>`, `SideTables.fs:788`), `IntrinsicForwardRepr`,
`IntrinsicReverseCanon` (`SideTables.fs:1456`) — every lookup uses the bare name
(`EngineCore.fs:433-436,468`; `MemberRegistration.fs:779`; `ClrEnv.fs:552`). `canonName`/
`intrinsicPlatformName` are memoized on `PassContext` inside the subtype recursion (`EngineCore.fs:421`)
— hot paths touched by all of unification. Qualifying identities means requalifying every one of these
keys (or a normalize-to-bare step at each lookup).

## Recommended staging (when picked up)
A coordinated multi-file change, NOT a surgical patch:
1. Decide (a) `TyConst payload → SymbolKey` vs (b) embed-in-string. (a) is cleaner long-term
   (consistent with nominals) but mechanically breaks every `TyConst(name,…)` pattern; (b) is less
   type-breaking but leaves ~205 string comparisons to sweep.
2. Move the mint sites + `BuiltinTypes` (all 25) + the repr-map KEYS in lockstep.
3. Rekey the codegen canon→repr maps (`ClrEnv.TryPrimitiveRepr`, JS `IntrinsicForwardRepr`) and the
   hard bare-literal arms.
4. Sweep the ~119 test expectations.
5. Land behind byte-identical emit (the identities change but the emitted output must not).

## Relationship to W9 / arrays
W9 keeps array a bare `TyConst("[]")` and resolves `arr.[i]` via members hung on the landed
`IntrinsicAbbrevHost` (identity unchanged), with local key-agreement on `arityName "[]" 1 = "[]``1"`.
When THIS milestone lands, array's identity becomes qualified like every other intrinsic and the
"normal sealed class with an intrinsic name" end-state (and, if wanted, an explicit JS repr / promotion
toward a nominal) can be revisited — but that is downstream of, and gated by, this identity change.

## Relevant memories
[[feedback_dynamic_intrinsics_over_du_cases]] (real intrinsics over new SemType cases — this change
keeps intrinsics as `TyConst`, just qualified),
[[feedback_plan_docs_ephemeral]] (delete this doc when landed),
[[feedback_hotspot_engineering_diminishing]] (bias to the structural fix — this IS it, but scoped
deliberately, not smuggled into the indexer work).
