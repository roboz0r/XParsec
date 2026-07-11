# The name-resolution boundary — one resolver face, a key-only store

> **Origin.** Combines and supersedes two plans: *provider-identity-channels*
> ("resolve once, thread the key" — narrowing the string-keyed inline channel)
> and *opens-overhaul* (the `OpenScope` machinery and its residual gaps). They
> looked contradictory — one said "narrow the string-keyed API", the other said
> "the provider stays string-keyed, keep it" — but they describe two faces of
> the same boundary. This doc states the boundary once and stages the work.
> Completed work recorded in those docs (opens Gaps 1 & 4, identity Stage 0,
> the open stack itself) is landed and canonical in the code; it is not
> re-described here.

## The boundary statement

**Spelling → identity resolution (`string × OpenScope → SymbolKey`) happens at
exactly one layer — NameResolution and the contract extractor. Every pass
downstream of that layer speaks only `SymbolKey`.**

- **Resolver face** — `string -> identity`. Opens-aware: this is where
  `OpenScope`, RQA suppression, module abbrevs, ambient prefixes, and
  (eventually) kind-tagged prefixes and the resolution cache live. String-keyed
  is *correct* here and stays.
- **Store face** — `SymbolKey -> payload`. What Unification, Freeze,
  InlineExpansion, and codegen speak. No consumer pass re-derives identity from
  a spelling, and no consumer pass stringifies a key to feed a string lookup.

**End state for Unification: zero resolver-face calls.** The audits found
nothing type-directed in Unification's spelling lookups, so no standing
exception is needed. The two things that *feel* like inference-time name
resolution are not: member access `x.M` needs the receiver's inferred type,
but that is a store-face lookup (`declaring key × member name`) — the member
name is a post-dot spelling, not opens-sensitive; and overload resolution
selects among candidates the store already returned by argument-type
betterness — a type-directed *choice over resolved identities*, not
resolution. The narrow-capability escape hatch (a pass keeping one typed
resolver method) remains available if a future corpus case genuinely needs
it, but nothing audited does.

The tell for a violation today is the round-trip idiom
`ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedName key)` — the caller
*holds* the identity and converts it back to a string because the interface
offers nothing better (`Subsume.fs:80`, `Engine.fs:273`, `EngineCore.fs:832`,
`ExternalSymbols.fs:979`; same pattern in codegen: `EmitJsContext.fs:309`,
`JsFlatFns.fs:55`, `ClrRecipes.fs:326`, `ClrExternalMembers.fs:675`).

## Target design

Literally split `IExternalSymbolProvider` into two interfaces, with the
combined interface inheriting both so every backing object stays a single
object:

```fsharp
type IExternalSymbolResolver =            // spelling → identity
    abstract TryLookup: name: string -> ExternalSymbol voption
    abstract TryLookupType: name: string -> ExternalTypeShape voption
    abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption
    abstract AmbientOpenPrefixes: string list

type IExternalSymbolStore =               // identity → payload
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption
    abstract TryLookupMember: key: SymbolKey * memberName: string -> ExternalMember voption
    abstract TryLookupMembers: key: SymbolKey * memberName: string -> ExternalMember[]
    abstract TryLookupIndexSignature: key: SymbolKey -> (FrozenType * FrozenType) list
    abstract TryLookupInlineBody: key: SymbolKey -> InlineBody voption
    // + the reverse-intrinsic axis (platform-repr keyed; that string is a
    //   different axis, not a source spelling — it belongs here)

type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore
```

- Every provider (VesperLib contract, metadata, JS-native, TS-manifest, test
  fakes) implements `IExternalSymbolProvider` exactly as today — one object,
  both duties. The decorators (`firstHit` composite, caching, `mapMember`,
  `withInlineBodies`) keep wrapping the combined interface.
- Store-face implementations may satisfy the key-addressed methods by
  `SymbolKeyOps.qualifiedName` *internally* — the string round-trip becomes an
  implementation detail buried inside the provider (replaceable later by a real
  keyed index), not a call-site idiom.
- Member *names* stay strings on the store face: a post-dot member name is not
  opens-sensitive; only the declaring type's identity is.
- **Enforcement is exposure, not the split itself.** `PassContext.Provider`
  (SideTables.fs:1438) is declared `IExternalSymbolStore`; the resolver face is
  handed only to `NameResolution.run` and the extractor. After the flip, a
  future pass *cannot* reintroduce a string lookup — the method isn't
  reachable. Both faces are upcasts of the same object: zero allocation, no
  forwarding (no struct-forwarding trap).
- The "dumb oracle" doctrine and the thread-safety contract from the current
  interface doc apply to both halves and move with them.

## Key semantics — the store's lookup contract

`SymbolKey` is structurally adequate for types and values, but four implicit
conventions must become one explicit contract before the store face freezes.
None requires new fields now.

1. **Assembly is a tiebreaker, not a requirement.** `qualifiedName` drops
   `asm`, so every existing lookup is de facto `(ns, arity-name)`-keyed with
   first-hit-wins across sources. Asm-blind mint paths are real and deliberate
   (`qualifiedTypeKey`, `intrinsicCanonKey` / `sameTypeAsmBlind`), so full
   structural key equality would make asm-blind keys miss asm-carrying
   entries. **Contract:** store lookup is addressed by `(ns, arity-qualified
   name)`; `asm` disambiguates only when `Some` on both sides. This codifies
   today's behaviour as a documented invariant inside the store.
2. **Capability dual faces normalise inside the store.** A capability type has
   two nominal keys (platform `` IEnumerable`1 `` vs canon
   `Vesper.Collections.seq`); bare `=` doesn't reconcile them
   (`sameNominalKey`). The store answers lookups for *either* face,
   normalising platform → canon internally via the reverse-intrinsic axis it
   already publishes. Requiring callers to canonicalise first would re-scatter
   the discipline this plan removes.
3. **`MemberKey` is never decomposed for lookup.** Its `argSig` is
   confessed-lossy (SemanticInfo.fs:77–93) and `qualifiedName` on a
   `MemberKey` returns the bare member name with no declaring type. Member
   lookup stays `(declaring TypeKey, memberName) → overload set`; `MemberKey`
   keeps only its existing duties (inline-body addressing, `ExternalAccess`
   stamps), where the key flows back to the provider that minted it. Do not
   widen them.
4. **Union cases have no key shape — defer.** External case identity travels
   as the `ExternalUnionCase` payload. When Stage 3 moves pattern-position
   case resolution upstream, the side-table stamp likely needs only a
   `(union TypeKey, caseName)` pair; add a `SymbolKey.CaseKey` DU case only if
   the pair proves insufficient (additive DU cases ripple through every match).

**Collision check — CONFIRMED a real hazard (audit 2026-07-10), resolved by
detection, not by re-keying.** Shared namespaces are the *norm*, not an edge
case: `Vesper.Core/Choice/Comparison/Option/Printf/Result` all contribute
into `namespace Vesper`, the collection packages into `Vesper.Collections`
(the manifest "PS5" split convention), and `Vesper.Exceptions` declares
types in `System` (a target-scoped shim for JS builds, where no BCL tail
exists — see the layering note below). A genuine `(ns, arity-name)`
collision between two packages is representable, undetected at manifest
parse / extraction / composition, and resolves as a **silent first-hit
shadow** in dependency order (`composeOrdered` → `composite` → `firstHit`,
ExternalSymbols.fs:1556–1564) — the loser's type is minted a correct key but
is unreachable by lookup. Assembly *is* populated on every result (`stack`'s
stampers re-stamp `SymbolOrigin.Assembly` onto everything;
identity `=` already compares it), so promoting it to a required lookup
discriminator is *possible* — but it fights the deliberate asm-blind mint
paths (§1) and is unnecessary if composition refuses ambiguity instead.
**Decision:** keep `(ns, arity-name)` addressing, and make a same-name
collision a **compiler error equivalent to C#'s CS0433** — "The type
'`ns.Name`' exists in both '`AssemblyA`' and '`AssemblyB`'", naming both home
assemblies. Scope and layering:

- **No composite is *supposed* to contain a same-name overlap at all** —
  cross-layer included. The one package that defines BCL-named types
  (`Vesper.Exceptions`, `System.InvalidOperationException` et al. in
  `exceptions.fsi`) is a *target-scoped shim*: JS builds reference it
  precisely because no `System.*` metadata tail exists there; CLR builds
  don't reference it because the platform provides those types. Target
  differences are handled by per-target package referencing, not by
  shadowing. The "referenced project beats a referenced assembly" first-hit
  ordering (`ExternalSymbols.fs:1746–1747`) remains as tie-break plumbing,
  but no design relies on it and it must not become one — the error, not
  the ordering, is the contract.
- The **composition-time sweep covers peer packages** (contracts are
  extracted and enumerable). A package-vs-metadata-tail overlap cannot be
  swept eagerly — the BCL/native tail is not cheaply enumerable — so it is
  diagnosed lazily instead: on a package-layer hit for a `System.*`-style
  platform namespace, a debug-assert/lookup-time probe of the tail is
  acceptable, or it waits for the store's key-addressed index. Either way
  it is a defect to surface, never a feature.
- Intra-package own-shape-over-dependency shadowing (documented, intended)
  stays.
- Detection is **composition-time** (when `composeOrdered` builds the
  composite): package contracts are extracted and enumerable, so the
  peer-package duplicate sweep is cheap and eager, and no lookup signature
  changes. Note this is deliberately *stricter* than CS0433's use-site
  firing — a collision errors even if the type is never referenced. C#'s
  use-site timing exists to tolerate polyfill/shim assemblies that ship
  duplicate types consumers never name; if Vesper ever grows that pattern,
  the diagnostic can move to lookup time without changing its content.

(The docs already flag the analogous intrinsic short-name collapse as an
assumption, not an invariant —
`package-type-extraction-architecture.md:253–255`.)

## Staged plan

Each stage lands green and independently.

### Stage 1 — inline name channel removal — **LANDED (2026-07-10)**

The store face is now born without `TryLookupInlineBodyByName`. Mechanism as
shipped: a new `PassContextResolution.IntrinsicKey : SideTable<SymbolKey>` (the
operator/intrinsic twin of `ExternalValue`), keyed by the expression `NodeKey`
both Unification and Freeze compute as `CstKeys.ofExpr e`. Unification stamps the
resolved `sym.Key` at each resolution site it already ran — `inferInfix` /
`inferPrefix` (operators), `inferIndexedLookup` (`GetArray`/`GetString`/`GetIndex`),
`resolveFieldStep` (`GetArrayLength`), `inferDynamicLookup` / `inferDynamicSet`
(`op_Dynamic`/`op_DynamicAssignment`) — plus a NEW write-path resolution in
`inferAssignment` for `SetArray`/`SetIndex` (the read path never resolved the write
intrinsic). Freeze reads the stamp at each mint site (threading the node `key` into
`translateAssignment`/`translateDotLookup`/`translateDynamicLookup`/`fieldStep`,
which previously took none). Group 3 (`collectInlineBodies`' `rewriteInlineVars`)
keys sibling intra-body refs from the same `TryLookup(qualifiedValueName info)` that
mints `ValueInlineBody.Key`. `InlineExpansion.lookupExternal` collapsed to a
key-only lookup; `TryLookupInlineBodyByName` deleted from the interface, all impls,
decorators, the caching layer, and every stub. The simple-name `byName` map is
retained ONLY as the `contractInlineBodies` test-introspection return (no provider
channel). `arrayOfList` and ctor-as-value heads correctly stay `key = ValueNone`
(codegen recipe / eta-expansion, never spliced). Regression guards:
`IntrinsicKeyStampTests.fs` (front-end key-presence) plus the existing JS/CLR
end-to-end splice suites, all green with the name channel gone.

Original scoping (retained for provenance): unchanged from the identity plan; done
first so the store face is born without `TryLookupInlineBodyByName`.

`TryLookupInlineBodyByName` has exactly one consumer —
`InlineExpansion.lookupExternal`'s `key = ValueNone` fallback — serving
`TExpr.External(_, ValueNone, _, _)` heads minted at ~13 sites in three
groups:

1. **Desugared operators** — `a + b` mints `External("op_Addition",
   ValueNone, …)` (`Freeze/Apply.fs`, `FreezeExpr.fs`).
2. **Synthesised intrinsics** — `op_Dynamic`, `GetArrayLength`,
   `op_DynamicAssignment`, get/set-index (`Freeze/Access.fs`,
   `Freeze/Resolve.fs`).
3. **Intra-body rewrite** — `rewriteInlineVars` re-points a sibling inline
   `Var` to `External(name, ValueNone, …)` (`SymbolProviders.fs`). Directly
   enabled by the landed Stage 0 (`ValueInlineBody.Key`): build a
   `NodeKey -> SymbolKey` map alongside `inlineNames`.

Stamp keys at all three groups, collapse `lookupExternal` to
`provider.TryLookupInlineBody key`, delete `TryLookupInlineBodyByName`
(interface method, real impl in `SymbolProviders.withInlineBodies`, caching
layer, all `ValueNone` stubs) — net LOC down.

**Failure mode:** once the name channel is gone, a missed stamp is a
mis-splice or phantom `call`, not a graceful miss — each mint site needs a
use-site test exercising the splice, as the `DefaultOfInline` suites do for
the value channel.

### Stage 2 — introduce the split interfaces — **LANDED (2026-07-10)**

`IExternalSymbolProvider` now `inherit`s `IExternalSymbolResolver` (spelling face:
`TryLookup`, `TryLookupType(string)`, `TryLookupUnionCase`, `AmbientOpenPrefixes`) and
`IExternalSymbolStore` (identity face: `TryLookupType(SymbolKey)`, `TryLookupMember`,
`TryLookupMembers`, `TryLookupIndexSignature`, `TryLookupInlineBody`, the two intrinsic
axes) — the store methods key-addressed, projecting `qualifiedName` internally.
`TryLookupType` is deliberately overloaded across the two faces (string on the resolver,
key on the store). **F# mechanics gotcha (encoded in the code, do not undo):** an object
expression / class implementing an overloaded inherited-interface method under ONE
combined `IExternalSymbolProvider with` block fails FS3213 — every implementer is split
into explicit `interface IExternalSymbolResolver with` + `interface IExternalSymbolStore
with` blocks (concrete classes add an empty `interface IExternalSymbolProvider`). All
providers/decorators/fakes (`stack`, `mapProviderTypes`, `memoize`, `nullProvider`,
VesperLib `toProvider`, `withInlineBodies`, `MetadataSymbols`, `JsNativeSymbols`,
`TsManifestProvider`, every test fake) migrated. All SA-side `qualifiedName` round-trips
deleted (key threaded directly, e.g. `DotSource.ExternalClass` now carries a `SymbolKey`).
The Stage-3 spelling holdouts that lack a stamped key mint a TRANSITIONAL asm-blind key at
the call site (`SymbolKeyOps.qualifiedTypeKey spelling 0`, whose `qualifiedName`
round-trips to the same string) — flagged inline as `Stage 3 holdout (bucket 3b)`, to be
replaced by NameResolution stamps. The `ICodegenSymbols` bridge (`CodegenSymbols.ofProvider`)
and codegen value-by-name lookups (`EmitJsContext`/`JsFlatFns` `TryLookup(qualifiedName key)`)
stay string-addressed — Stage 5.

- `PassContext.Provider` stays combined-typed — both faces reachable; the exposure flip is
  Stage 4.
- The **composition-time `(ns, arity-name)` duplicate diagnostic** landed in
  `composeOrdered`: `buildProviderWith` now returns a `BuiltPackage` carrying each
  package's `DeclaredTypeKeys` (own Class/Record/Union/Enum shapes — intrinsics/capability
  faces excluded as asm-blind by design), and the compose loop refuses a qualified type key
  owned by two DIFFERENT home assemblies with a CS0433-equivalent error naming both. Peer
  packages only (the BCL/native metadata tail is not enumerable → lazy diagnosis, deferred).
  Regression guards in `ReferencedProjectTests` (fires on a two-package clash; no false
  positive on a solo package); the real Vesper set composes clean across the SA + JS + CLR
  suites.

### Stage 3 — burn down the downstream resolver-face holdouts — **LARGELY LANDED (2026-07-10)**

**Provenance audit (2026-07-10)** traced every `TryLookup*` site in
Unification, Freeze, and ConformanceTypars, following string arguments through
callers. Headline: no pass ever *read* a NameResolution identity stamp as its
lookup string — the stamps were written by these paths, never read; Unification
was a co-equal resolver, not a consumer of NameResolution's results. The three
buckets landed as below. A residue of resolver-face calls that are NOT
type-identity resolution remains (recorded under "Remaining") — those are
exactly the calls a Stage 4 flip would surface as type errors.

**3a — Bare-spelling union-case recognition — LANDED.** Raw `ctx.NameOf` token
text was handed straight to `TryLookupUnionCase` at `InferPat` (both external
arms), `tryExternalCasePattern` (InferResolve, reached from InferPat /
InferIdentExpr), `Freeze/Patterns.isExternalUnionCase` (×3 callers), and
`Freeze/Resolve.tryCtorRef` — doubly wrong: a boundary violation AND an opens
bypass (a case resolved via the provider's global reverse index even if its
union's namespace was never opened). Mechanism as shipped: NameResolution
recognises the case — pattern- and expression-position — and stamps the
resolved payload into `PassContextResolution.ExternalUnionCaseStamp :
SideTable<ExternalUnionCase>`, keyed by the ctor-head `NodeKey`
(`CstKeys.ofPat` / `CstKeys.ofExpr`, the same keys the consumers read). The
`(union TypeKey, caseName)`-pair option (key-semantics §4's preferred shape)
was tried first and abandoned: fake/minimal providers implement the reverse
`TryLookupUnionCase` index WITHOUT the forward key-addressed `TryLookupType`,
so recovering field types by union key both changes behaviour and breaks green;
carrying the resolved `ExternalUnionCase` payload keeps identity resolved once
upstream (§4 explicitly sanctions the payload channel). Two parity gaps closed:
(1) a dedicated recursive pattern walk `stampPatCases` reaches *every*
sub-pattern position `bindingsOfPat` skips because it binds nothing (or-pattern
alternatives, type-tests, cons tails, records), invoked at every pattern-entry
site (`extendScope`, `bindingsToScope`, the `EnterForIn`/`EnterMatchArm`
hooks); (2) the qualified-external leg (`Result.Ok x`) is now recognised via a
shared `tryExternalCase` / `isPatNamedCtorHead` classifier, so its sub-patterns
bind correctly (previously dropped). Every read site reads the stamp by key;
`TryLookupUnionCase` no longer appears anywhere downstream of
`NameResolution/Scope.fs`. **Opens false-accept closed** (confirmed a real F#
divergence by the compiler audit — F# has no global reverse case index; a bare
case needs its declaring module opened, `AddPartsOfTyconRefToNameEnv`):
`tryExternalCase` now gates a *bare* case on the declaring union's namespace
being reachable through the current `OpenScope` (matched with
`OpenScope.tryQualify` over the union's `UnionName` — the authoritative
namespace, since the reverse-index provider stamps a blanket package `Origin`);
RQA and qualified `Union.Case` legs stay ungated (F# resolves those without the
namespace opened). The ambient prelude keeps `Some`/`None`/`Ok`/`Error`
bare-visible. Regression guards: `ExternalUnionCaseStampTests.fs` (stamp
presence at bare / qualified / nested-in-tuple / both or-alternatives, plus
opened-vs-unopened namespace) and the updated `OpenResolutionTests.fs`
positive/negative pair.

**3b — expression-position OpenScope re-resolution — LANDED (approach A).**
The static-member-split / ctor-head / generic-static-receiver sites resolved a
type identity through `OpenScope.tryQualify`/`tryResolve` at inference time,
then minted a transitional asm-blind `qualifiedTypeKey <spelling> 0`. Confirmed
against the F# compiler (audit 2026-07-10, `NameResolution.fs`
`ResolveExprLongIdentPrim`): the type/member split for a *static* dotted path
is decidable at name-resolution time WITHOUT type inference — F# consumes the
longest namespace→module→type→static-member prefix over symbol tables and only
the residual tail applied to an inferred *value* is type-directed. Mechanism as
shipped: NameResolution runs the opens-aware longest-type-prefix walk
(Class-only, via `tryResolveExternalClassKey`) and stamps the resolved type
`SymbolKey`; Unification/Freeze read it and do store-face
`TryLookupMember(typeKey, name)` / `TryLookupMembers(typeKey, ".ctor")` lookups
(member *selection* stays type-directed — F#'s Phase 2 — keyed by the resolved
type; the post-dot member name is not opens-sensitive, §3). Two tables carry
the stamp: the existing `ResolvedType` for ctor-sugar heads + generic static
receivers, and a NEW `ExternalStaticReceiver : SideTable<SymbolKey>` for the
folded static-member *receiver prefix* — these had to split because a
static-member LongIdent and a ctor head key off the same node with opposite
meanings (prefix-of-T vs whole-name-is-T), and one shared table
mis-constructed. **Class-only stamping is load-bearing:**
`tryResolveExternalTypeKey` resolves any external type, but stamping broadly
made scalar-intrinsic conversion functions (`float x`, `int x`) mis-fire as
ctor calls (the store returns a `Class` shape for a scalar's backing metadata
type), so the stamp itself — not just a consumer guard — filters to `Class`.
Migrated: `inferExternalStaticMember` / `splitExternalClassPrefix` /
`tryExternalStaticLongIdent` / `tryResolveExternalStaticMemberRef` /
`tryExternalTypeReceiver` (InferResolve), `inferExternalCtorOn` /
`tryInferExternalCtorApp` / `tryInferExternalGenericCtorApp` (InferCtor), the
static-method overload set (InferExternalCall), plus a bonus key-round-trip
removal in `tryExternalDispose` (Infer.fs). All `Stage 3 holdout (bucket 3b)`
markers and transitional `qualifiedTypeKey <spelling> 0` mints removed from
these sites. Regression guard: `ExternalTypeKeyStampTests.fs`.

**3c — Vestigial OpenScope wrappers over already-qualified keys — LANDED.**
`canonKey` and `subtypeParentOf`/`subtypeInterfacesOf` (EngineCore.fs) funnelled
a resolved key's `qualifiedName` back through `OpenScope.tryResolve` — a no-op
shell over an already-qualified identity. Converted to the key-addressed store
face (`TryLookupType(key)`), threading the canon `SymbolKey` through the
callers. `intrinsicPlatformName` (listed in the original inventory as `:543`)
was NOT a shell and stays on the string face: its input is a *short* name and
its opens funnel does genuine short→qualified resolution.

**Remaining (a residue of NON-type-identity resolver-face calls; these gate the
Stage 4 flip).** The audit's 3b inventory was not uniformly type-identity
resolution; each of the following needs a different vehicle and is deferred:

1. **`new T` written-platform-class arm — LANDED (2026-07-10); type
   annotations (`x: A.B.T`, `Translate.fs`) — deferred.** Both live in `Type`
   nodes the expression walk does not traverse. The `new T(…)` holdout is
   closed: NameResolution's `visit` now has a dedicated `Expr.New` arm that
   resolves the written `Type.NamedType` head — opens-aware, Class-only, via
   the existing `tryResolveExternalClassKey` — and stamps `ResolvedType` keyed
   by the `Expr.New` node. `InferCtor.inferNew`'s `TyConst` arm reads that
   stamp (`ctx.Resolution.ResolvedType.TryGetValue key`) and does a
   key-addressed `.ctor` lookup via `inferExternalCtorOn`; no stamp means a
   heritable-primitive canon head (`new exn "boom"`) and falls to the intrinsic
   constructible-surface path — so the `OpenScope.tryQualify` +
   `qualifiedTypeKey <spelling> 0` mint is gone from the site. Class-only
   stamping is load-bearing for the same reason as 3b (a scalar-intrinsic head
   must not be stamped constructible). `inherit T(…)` needed no change:
   `fillBaseCtorCall` already reads the pre-resolved `info.BaseType`
   (`TyClass`/`TyConst`) and dispatches by key/canon. Regression guard:
   `ExternalTypeKeyStampTests.fs` (new-head stamped when the class is known;
   not stamped for an unknown head), plus the existing CLR raise/ctor splice
   suites. The **type-annotation general case** (`tryResolveExternalType` in
   `Translate.fs`) stays deferred: it resolves a written spelling to a *full
   SemType* (abbrev expansion, canon `TyConst`, capability `TyClass`, `TyEnum`
   …), not a Class key, and moving it upstream needs the general type-walker
   this plan deliberately avoids — it is Stage 4 residue, surfaced by the flip.
2. **Dotted external value refs (`A.B.v`, `InferIdentExpr`) and
   operator/intrinsic symbol resolutions (`InferApp`) — LANDED (2026-07-10).**
   Both needed the value's polymorphic *scheme* (`instantiateSymbol`), not just
   its key, and the store face exposes no scheme-by-key method — so the fix is a
   symbol-payload stamp (the `ExternalUnionCaseStamp` shape), NOT a store
   capability: a value `SymbolKey` does not round-trip to its fully-qualified
   spelling, so a key-addressed re-lookup could not reproduce the resolver-face
   result. Mechanism as shipped: a new
   `PassContextResolution.ExternalSymbolStamp : SideTable<ExternalSymbol>`. Every
   NameResolution resolver-face hit that already resolved a value now ALSO stamps
   the full `ExternalSymbol` (not just `sym.Key` into `ExternalValue`): the
   single-ident (`resolveIdent`) and dotted (`resolveQualifiedExternal`) value
   paths, and the bare-`(+)` / qualified-`A.B.(+)` operator-value arms. The
   operators — which NameResolution did not previously visit — get FRESH stamping
   arms in `visit`: `InfixApp` / `PrefixApp` recover the compiled op name from
   `ctx.Desugared` (Desugar runs first — `Pipeline.fs`) and resolve it;
   `op_Dynamic` is stamped on a `DynamicLookup`, `op_DynamicAssignment` on the
   enclosing dynamic `Assignment`. `::` (`ConsExpr`) and `op_AddressOf` resolve to
   no stamp (not an `OpName` / no provider symbol), matching the arms Unification
   handles without the provider. Unification READS the stamp at all six sites
   (`inferIdentDefault`, `inferIdent`'s `(+)`-value arm, `inferInfix`,
   `inferPrefix`, `inferDynamicLookup`, `inferDynamicSet`) via
   `ExternalSymbolStamp.TryGetValue key` — the same `CstKeys.ofExpr` key
   NameResolution stamped — and calls `instantiateSymbol` on it; the App operator
   sites still derive `IntrinsicKey` from the stamped `sym.Key` (that stamping
   stays in Unification, unchanged, so Freeze/InlineExpansion are untouched).
   `tryMeasuredArith` still short-circuits `inferInfix` BEFORE the stamp read
   (type-directed; the stamp is present-but-unread for measured arithmetic). No
   `OpenScope.tryResolve … ctx.Provider.TryLookup` call remains in
   `InferIdentExpr.fs` or `InferApp.fs`'s value/operator sites. Regression guard:
   `ExternalSymbolStampTests.fs` (stamp presence at dotted value / bare +
   qualified operator-value / infix / dynamic get+set; absent for an unknown value
   and an unsurfaced operator), plus the existing JS/CLR operator + dynamic-access
   splice suites, all green. **Still deferred (same mechanism, out of item-2
   scope):** the synthesised-intrinsic resolver-face calls in `InferRecordAccess`
   (`GetArrayLength`, get/set-index, `op_Dynamic` in record-access position) —
   these are not in `InferApp`; an `ExternalSymbolStamp` (or `IntrinsicKey`-twin)
   stamp on the `IndexedLookup` / `.Length` chain closes them the same way.
3. **`Freeze/Resolve.tryClassRef` — LANDED (2026-07-10).** The feared "reading a
   key needs a TAST/codegen change" dissolved on audit: `TExpr.New`'s `className`
   string is **diagnostic-only** — both backends resolve the construction by the
   node's result-type `SymbolKey` (CLR `EmitConstruct.buildNew` via `env.Classes`
   / `env.Provider.TryEmitCtor(ctorKey…)`; JS `EmitJs` via `receiverShape ty`),
   and `className` survives solely in the "no constructor" / "no JS analogue"
   error messages. And the abbreviation expansion (`ResizeArray<'T>` → `List`1`) is
   already carried on that `ty` (Unification's `tryInferExternalGenericCtorApp` →
   `tryResolveExternalNominal` pins the expanded class), so `tryClassRef` never
   needed to reproduce it. Mechanism as shipped: `tryClassRef`'s external arm now
   READS the `Resolution.ResolvedType` stamp NameResolution already writes on every
   ctor-sugar head — single-ident (`Scope.resolveIdent`, Class-only), qualified
   (`resolveQualifiedExternal`, Class-only), and the generic `TypeApp` receiver
   (the `Expr.TypeApp` visit, exact-arity) — keyed by the same head `NodeKey`
   Freeze computes; the stamp's PRESENCE is the "head names a constructible external
   type" verdict, mirroring `tryInferExternalCtorApp`/`tryInferExternalGenericCtorApp`.
   The returned name is `SymbolKeyOps.qualifiedName` of the stamped key (cosmetic).
   The `OpenScope.tryQualify` + resolver-face `TryLookupType(string)` re-resolution
   (and its `lookupShape`/`underlyingClassName`/`tryExternal` scaffolding and the
   `arity` parameter) is deleted from the site — Freeze recognition now aligns
   exactly with Unification's typing (both key off the same upstream resolve). Bare
   (non-generic) abbrev ctor-sugar (`ResizeArray()` with no `<'T>`) is intentionally
   dropped: it was never stamped nor typed as a ctor by Unification post-3b, so
   Freeze recognizing it could not have produced correct code — the corpus/tests
   only exercise the generic form. Regression guards: the existing end-to-end
   ctor-sugar splice suites across both backends — CLR `InferResolutionTests`
   (`Exception("x")` bound / opened / `System.…` qualified / raise-position),
   `TypeTestAsBinderTests`, `ForInTests` (`ResizeArray<int>()`); JS `ExceptionTests`
   (single-ident + qualified `InvalidOperationException`/`ArgumentException` app-form);
   `ClassTypeApp` (local generic `Box<int>(5)`) — plus the upstream stamp presence
   asserted by `ExternalTypeKeyStampTests`.
4. **External enum-case `E.C1` (`tryExternalEnumCase`) — LANDED (2026-07-10).**
   The resolver-face `E`-qualify + `TryLookupType(string)` enum probe moved
   upstream. Mechanism as shipped: a new
   `PassContextResolution.ExternalEnumCaseStamp : SideTable<SymbolKey>` (the
   enum-case sibling of `ExternalUnionCaseStamp`; only the enum's nominal key is
   needed — an enum case is a named constant on a closed set, not a ctor arrow, so
   a bare `SymbolKey` suffices, not a payload). NameResolution recognises the case
   in BOTH positions via a shared `tryExternalEnumCaseKey` (opens-aware, arity-0)
   and stamps the key: expression-position in `resolveQualifiedExternal`'s
   two-segment block (alongside the union-case stamp; the pre-existing
   `isExternalStaticMember` prefix probe already suppresses the unresolved-name
   diagnostic — `E` resolves as an external type), pattern-position in
   `stampPatCases`'s `Pat.Named` arm. Unification's `InferIdentExpr` and `InferPat`
   enum arms READ the stamp by node key (`CstKeys.ofExpr` / `ofPat`) and type the
   node `TyEnum key`; the resolver-face `tryExternalEnumCase` and its
   `ExternalEnumCaseLi` active pattern are deleted from `InferResolve`. Freeze
   needed no change: it recovers the enum key from the node's `TyEnum` type
   (`Freeze/Resolve.enumKeyOfTy`), never the resolver face. Two-segment only,
   matching the old active pattern (a namespace-qualified `Tests.Direction.Up`
   3-ident head was never handled here and still isn't). Regression guards:
   `ExternalEnumCaseStampTests.fs` (stamp presence at expression / pattern; absent
   for an unknown case; opened-vs-unopened namespace), plus the existing JS
   `TsManifestEnumTests` and CLR `ForInTests` external-enum end-to-end suites.
5. **`resolvesAsExternalUnionOrRecord` (feeds `tryQualifiedExternalMemberMiss`) —
   LANDED (2026-07-10).** Though it only shapes an error message, it was a
   resolver-face call (`OpenScope.tryQualify` + `TryLookupType(string)`) the Stage 4
   flip would surface — so it moved upstream on the same stamp pattern as the
   others. Mechanism as shipped: a new
   `PassContextResolution.ExternalUnionRecordQualifier : SideTable<SymbolKey>`.
   NameResolution's `resolveQualifiedExternal` classifies a ≥2-segment qualified
   `Q.member` whose qualifier `Q` (every segment but the last) resolves — opens-aware,
   any small arity `` `0 ``..`` `4 `` — to an external UNION or RECORD (a new
   `tryResolveExternalUnionOrRecordKey`, the union/record sibling of
   `tryResolveExternalClassKey`) and stamps `Q`'s resolved key under the node's
   `CstKeys.ofExpr` key. Unification's `tryQualifiedExternalMemberMiss` READS the stamp
   (presence) instead of calling the resolver face; the `(qualifier, member)` strings
   for the message are recovered from the node. A CLASS qualifier is NOT stamped — its
   unmodelled-static silence stays a fresh TyVar (the union/record-only filter is
   load-bearing, exactly as the class-only filters in 3b/item-1). The stamp is
   present-but-unread when the tail resolves (a valid case / value / static never
   reaches the miss path). The dead `isExternalClass` probe (no call sites, itself a
   resolver-face `TryLookupType(string)`) was excised alongside. No resolver-face
   `TryLookupType(string)` remains in `InferResolve.fs`'s miss path. Regression guard:
   `ExternalUnionRecordQualifierStampTests.fs` (stamp presence at union / record
   qualifier; absent for class / unknown qualifier; opened-vs-unopened namespace;
   plus end-to-end assertions that the stamp drives the member-miss diagnostic and a
   class qualifier stays silent).

**Member names are not holdouts.** Every `TryLookupMember(s)` second argument
(`ctx.NameOf memberTok` at `InferExternalCall.fs:221/298`, `d.MemberName`,
etc.) is a post-dot member spelling — not opens-sensitive, stays a string on
the store face by design (key-semantics §3).

### Stage 4 — flip the exposure

`PassContext.Provider : IExternalSymbolStore`; resolver face threaded only to
`NameResolution.run` and the extractor. **The flip compiling is the
compiler-checked "done" bit for Stages 1–3** — any missed holdout is a type
error, not a review find. The Stage 3 *Remaining* residue must be resolved
first: the type-annotation (§1) call still needs the string resolver face, so it
is precisely what the flip would surface. Items 2 (value schemes / operator
symbols, the `ExternalSymbolStamp` symbol-payload channel), 3 (`tryClassRef`, now
reading the `ResolvedType` stamp), 4 (enum-case `E.C1`, now reading the
`ExternalEnumCaseStamp`), and 5 (union/record member-miss, now reading the
`ExternalUnionRecordQualifier` stamp) have LANDED, so those
value/operator/ctor-sugar/enum-case/member-miss resolver-face calls are already
gone; the remaining `InferRecordAccess` synthesised intrinsics and the
type-annotation general case want the same stamp before the flip.

### Stage 5 — codegen `BuiltinOps` by-name → by-key

The second by-name mechanism: codegen recognises operators by the `External`
name string. Same class of leak, larger blast radius; Stage 4's key-only
codegen face makes it look as anomalous as it is. Separate doc when reached.

### Deferred residue (carried forward from the opens plan — triggers unchanged)

- **Type-vs-module shadowing (kind-tagged prefixes).** Resolver-internal after
  Stage 4. Wants a forcing corpus case before the flat prefix list grows a
  kind tag / namespace tree; surface with a diagnostic before generalising.
- **Resolution caching.** A per-`PassContext` memo `(OpenScope identity, name)
  → hit-or-miss` inside `tryResolve`/`resolveIdent`; cache misses too;
  per-file lifetime. Land only against a measured hot path — but note Stage 4
  improves the eventual design: the resolver face becomes the *only* string
  surface, so the memo covers the entire spelling-lookup seam, not one cache
  among many.

## Premises to confirm (gating, in order)

1. **Operator keys (gates Stage 1).** Does every desugared-operator head have
   a resolved `SymbolKey` reachable at its mint site (does
   Desugar/Unification already record one, keyed by the op node), or does
   threading it need a new side-table entry?
2. **Synthesised-intrinsic keys (gates Stage 1).** Do `op_Dynamic` /
   `GetArrayLength` / index ops have canonical keys resolvable via the
   provider or as `RuntimeNames` `*Key` constants, so mint sites stamp
   without bespoke lookups? *Partial evidence from the provenance audit:*
   Unification already resolves these names live via
   `OpenScope.tryResolve … ctx.Provider.TryLookup "op_Dynamic"` etc.
   (`InferApp.fs:611/642`, `InferRecordAccess.fs:441/482/502/609`) — so the
   symbols resolve through the ambient prelude and the resolved
   `ExternalSymbol` (with its key) is in hand *at inference time*; the
   remaining question is only threading it to Freeze's mint sites.
3. **String-typeName provenance — CONFIRMED (audit 2026-07-10).** ~24 of the
   audited type/value-name arguments are key round-trips (mechanical Stage 2
   migration): all of Engine/EngineCore/Subsume/Unification/ConformanceTypars
   type-name args, most of InferRecordAccess, `InferCtor.fs:102`,
   `Infer.fs:167/190`, `InferApp.fs:464`, `Freeze/Access.fs:62/279`. A
   handful are well-known constants resolved via the prelude (Stage 1
   material). The spelling-derived remainder is exactly the Stage 3 inventory
   (3a/3b above). No site reads a NameResolution stamp as its lookup string —
   category B is empty.
4. **Pattern-position union cases — CONFIRMED movable (audit 2026-07-10).**
   Every guard input at every case-vs-binder site (InferPat both arms,
   `tryExternalCasePattern`, Freeze's recognisers) is name-shaped: spelling,
   capitalization, ident count, `CtorIndex`/`Union` registries, the provider
   case index, `ResolvesWith` (RQA + qualifier). No `SemType`, no scrutinee
   type, no unification result is consulted; a wrong-arity case stays a case
   with a diagnostic, never falls back to binder; uppercase ctor heads are
   not shadowed by local binders in any pass (matching F#). NameResolution
   **already computes the decision** — `isCtorName` inside `bindingsOfPat`
   (Scope.fs:181–237) classifies every pattern head to derive the binder set,
   then discards the classification. Stage 3a is therefore "stamp what
   `bindingsOfPat` already decided" per ctor-head `NodeKey`
   (`CstKeys.ofPat` — the same key InferPat/Freeze read), with two known
   parity gaps to close: the stamping walk must reach *all* pattern
   positions (today patterns are visited only via the scope hooks), and
   `bindingsOfPat` lacks the qualified-external leg (`Result.Ok x`) that
   InferPat.fs:268 / Freeze Patterns.fs:184–185 recognise. No
   narrow-capability escape hatch is needed for patterns.
5. **`(ns, arity-name)` collision — CONFIRMED possible, silent today.** See
   the key-semantics section: resolved by a composition-time duplicate
   diagnostic, not by re-keying lookups on assembly.

## Non-goals

- Re-keying the resolver-face calls in NameResolution / the extractor — those
  *are* the resolve-once boundary; string-keyed there is the design.
- Fixing `MemberKey.argSig` overload identity (open design question recorded
  at SemanticInfo.fs:77–93; this plan only avoids widening `MemberKey`'s
  duties).
- Materialising a real `DefaultOf<T>()` method (`unchecked-defaultof-plan.md`,
  Half 2).
- Cross-file / cross-package resolution caching.
- Anything in the descriptor payload (`BuildSignature` / `ExternalSignature` /
  `FrozenType`).

## Key files

- `ExternalSymbols.fs:782–` — `IExternalSymbolProvider` (the split target),
  decorators (composite `firstHit` ~1676, mapMember ~1858, caching ~1911),
  null provider ~1483.
- `SideTables.fs:1438` — `PassContext.Provider` (the Stage 4 flip site).
- `SymbolKeyOps.fs` — `qualifiedName` (the round-trip to bury), mint helpers.
- `SemanticInfo.fs:56–94` — `SymbolKey` + the `MemberKey` lossiness TODO.
- `Passes/InlineExpansion.fs:311–372` — `lookupExternal` (Stage 1 consumer).
- `Codegen.Common/SymbolProviders.fs` — `withInlineBodies` (Stage 1),
  `rewriteInlineVars` (Stage 1 group 3).
- `Freeze/Apply.fs`, `FreezeExpr.fs`, `Freeze/Access.fs`, `Freeze/Resolve.fs`
  — the `ValueNone` mint sites (Stage 1 groups 1–2).
- `Passes/Unification/InferPat.fs:92/266`, `InferResolve.fs:104/191–391`,
  `InferCtor.fs:131/257/261`, `InferIdentExpr.fs:140`, `Translate.fs:532`,
  `Freeze/Patterns.fs:27`, `Freeze/Resolve.fs:78/510/536` — the audited
  Stage 3 holdout inventory (buckets 3a/3b).
- `Passes/NameResolution/Scope.fs`, `CstWalk.fs:11–70` — the resolver face's
  permanent home (and the deferred residue's landing zone).
