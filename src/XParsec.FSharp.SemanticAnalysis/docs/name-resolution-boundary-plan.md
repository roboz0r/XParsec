# The name-resolution boundary — one resolver face, a key-only store

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

**Target end state for Unification: no *type-directed* resolver-face calls.** The
single surviving resolver reach (`tryResolveExternalType`, § Remaining) is a
*written-spelling* resolution fused with live-`SemType` construction — name → type,
not type-directed name resolution — and is the one sanctioned exception, on a
documented handle. Beyond it, nothing audited is type-directed name resolution: the
two things that *feel* like inference-time resolution are not: member access `x.M`
needs the receiver's inferred type, but
that is a store-face lookup (`declaring key × member name`) — the member name is
a post-dot spelling, not opens-sensitive; and overload resolution selects among
candidates the store already returned — a type-directed *choice over resolved
identities*, not resolution.

The tell for a violation is the round-trip idiom
`ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedName key)` — the caller *holds*
the identity and converts it back to a string because the interface offered
nothing better. Those are gone from the SA passes (the flip caught the last three as
type errors). Post-flip, `ctx.Provider` is the store face and *cannot* resolve a
spelling; the genuine spelling reaches that remain live on the narrow `ctx.Resolver`
handle, enumerated on its doc-comment (§ Remaining) — one permanent by design
(`tryResolveExternalType`) and the deferred codegen channel (NameResolution itself,
the resolve-once home, is the third and expected reader).

## Target design

`IExternalSymbolProvider` is split into two faces, with the combined interface
inheriting both so every backing object stays a single object (this has landed —
`ExternalSymbols.fs:789–921`):

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
    // + the reverse-/forward-intrinsic axes (platform-repr keyed; a different
    //   axis than a source spelling, so they belong here)

type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore
```

- Every provider (VesperLib contract, metadata, JS-native, TS-manifest, test
  fakes) implements `IExternalSymbolProvider` — one object, both duties. Both
  faces are free upcasts of the same object: zero allocation, no forwarding.
- Store-face implementations satisfy the key-addressed methods by
  `SymbolKeyOps.qualifiedName` *internally* — the string round-trip is an
  implementation detail buried inside the provider, replaceable later by a real
  keyed index, never a call-site idiom.
- Member *names* stay strings on the store face: a post-dot member name is not
  opens-sensitive; only the declaring type's identity is.
- **F# mechanics gotcha (encoded in the code, do not undo):** an object
  expression / class implementing the overloaded inherited `TryLookupType` under
  ONE combined `IExternalSymbolProvider with` block fails FS3213 — every
  implementer is split into explicit `interface IExternalSymbolResolver with` +
  `interface IExternalSymbolStore with` blocks (concrete classes add an empty
  `interface IExternalSymbolProvider`).
- **Enforcement is exposure, not the split itself — and, as landed, exposure means
  *convention*, not *unreachability*.** The flip declares `PassContext.Provider :
  IExternalSymbolStore` (the default face — it cannot resolve a spelling) alongside a
  narrow `PassContext.Resolver : IExternalSymbolResolver`. The original aim was that the
  resolver method be structurally *unreachable* from a consumer pass; blocker 2
  forecloses that (`Translate.tryResolveExternalType` needs the resolver deep in
  Unification with only `ctx` in hand, so `Resolver` must be a reachable `ctx` member).
  What lands instead: the store is the default, and every spelling reach is one named,
  greppable member with an enumerated reader set — a new consumer-pass string lookup
  reads as the anomaly it is. See § "What has landed" → the flip bullet for the full
  account.

## Key semantics — the store's lookup contract

`SymbolKey` is structurally adequate for types and values, but four conventions
are explicit contract, not incidental behaviour. None requires new fields.

1. **Assembly is a tiebreaker, not a requirement.** `qualifiedName` drops `asm`,
   so store lookup is de facto `(ns, arity-name)`-keyed with first-hit-wins
   across sources. Asm-blind mint paths are deliberate (`qualifiedTypeKey`,
   `intrinsicCanonKey`/`sameTypeAsmBlind`), so full structural key equality would
   make asm-blind keys miss asm-carrying entries. **Contract:** store lookup is
   addressed by `(ns, arity-qualified name)`; `asm` disambiguates only when
   `Some` on both sides.
2. **Capability dual faces normalise inside the store.** A capability type has
   two nominal keys (platform `` IEnumerable`1 `` vs canon
   `Vesper.Collections.seq`); the store answers lookups for *either* face,
   normalising platform → canon internally via the reverse-intrinsic axis.
3. **`MemberKey` is never decomposed for lookup.** Its `argSig` is
   confessed-lossy (SemanticInfo.fs:77–93) and `qualifiedName` on a `MemberKey`
   returns the bare member name. Member lookup stays `(declaring TypeKey,
   memberName) → overload set`; `MemberKey` keeps only its existing duties
   (inline-body addressing, `ExternalAccess` stamps). Do not widen them.
4. **Union cases have no key shape.** External case identity travels as the
   `ExternalUnionCase` payload (the `ExternalUnionCaseStamp` side table). Add a
   `SymbolKey.CaseKey` DU case only if a `(union TypeKey, caseName)` pair proves
   insufficient (additive DU cases ripple through every match).

**`(ns, arity-name)` collision — a real hazard, resolved by detection.** Shared
namespaces are the norm (`Vesper.Core/Choice/Comparison/Option/Printf/Result`
all contribute into `namespace Vesper`; the collection packages into
`Vesper.Collections`; `Vesper.Exceptions` declares `System.*` shim types for JS
builds). A genuine collision between two packages resolves as a **silent
first-hit shadow** in dependency order. **Decision (landed in `composeOrdered`):**
keep `(ns, arity-name)` addressing and make a same-name collision between two
DIFFERENT home assemblies a **compiler error equivalent to C#'s CS0433**, naming
both assemblies. `buildProviderWith` returns each package's `DeclaredTypeKeys`
(own Class/Record/Union/Enum shapes; intrinsics/capability faces excluded as
asm-blind by design); the compose loop refuses the duplicate. Peer packages
only — the BCL/native metadata tail is not eagerly enumerable, so a
package-vs-tail overlap is diagnosed lazily (a lookup-time probe, or the future
keyed index). Guarded by `ReferencedProjectTests`. This is deliberately
*stricter* than CS0433's use-site firing (it errors even if the type is never
referenced); if Vesper ever grows a polyfill/shim pattern the diagnostic can
move to lookup time without changing its content.

## What has landed

The boundary is in place, **exposure flip included**. In brief:

- **The split interfaces** and every provider/decorator/fake migrated; the
  *enumerated* SA-side `qualifiedName` round-trips deleted (keys threaded directly) —
  three stragglers survived until the flip turned them into type errors (see the flip
  bullet); the composition-time CS0433 duplicate diagnostic.
- **The inline name channel is gone** — `TryLookupInlineBodyByName` deleted from
  the interface, impls, decorators, and caching; inline splices addressed by
  `SymbolKey` (operator/intrinsic keys stamped via
  `PassContextResolution.IntrinsicKey`, keyed by the expression `NodeKey`).
- **Every enumerated downstream resolver-face holdout moved upstream** as a
  NameResolution stamp read by Unification/Freeze by node key:
  - bare & qualified **union-case recognition** → `ExternalUnionCaseStamp`
    (opens-gated for bare cases, closing an F# false-accept);
  - **static-member split / ctor-head / generic-static receiver** → `ResolvedType`
    + `ExternalStaticReceiver` (Class-only stamping is load-bearing — a scalar
    intrinsic head used as a conversion fn must not be stamped constructible);
  - **`new T` written head** → `ResolvedType` on the `Expr.New` node;
  - **dotted value refs & operator/intrinsic symbols** → `ExternalSymbolStamp`
    (a symbol *payload*, because a value key does not round-trip to its spelling
    and the store exposes no scheme-by-key);
  - **`Freeze.tryClassRef`**, **external enum-case `E.C1`**
    (`ExternalEnumCaseStamp`), and the **union/record member-miss** classifier
    (`ExternalUnionRecordQualifier`).
  - vestigial OpenScope wrappers over already-qualified keys (`canonKey`,
    `subtype*Of`) converted to store-face `TryLookupType(key)`.
- **The last tractable Unification-side resolver reaches (A/B/D) removed:**
  - `checkInterfaceConformance` — `TryLookupType(qualifiedName ifaceKey)` →
    key-addressed `TryLookupType ifaceKey` (a pure round-trip the caller held).
  - the **synthesised access intrinsics** (`GetArray`/`GetString`/`GetIndex`/
    `GetArrayLength`/`SetArray`/`SetIndex`) — these live in `[<AutoOpen>]` prelude
    modules and so are opens-insensitive. Resolved ONCE per file into
    `PassContext.CoreAccess : Lazy<CoreAccessIntrinsics>` (against the ambient
    scope), read by field at the `inferIndexedLookup` / `.Length` /
    `inferAssignment` sites — no `ctx.Provider.TryLookup` per node.
  - the **printf external sink** (`InferApp`) — the slot names
    (`System.IO.TextWriter`, `System.Text.StringBuilder`, `System.IO.StringWriter`)
    are fixed and fully qualified, so the declaring class resolves by KEY on the
    store face (`TryLookupType(qualifiedTypeKey name 0)` → `externalTypeKey
    origin`), minting the identical `TyClass` a real sink argument carries.
- **`EngineCore.intrinsicPlatformName` reworked to the store's forward axis** — the
  first of the two former flip blockers, now closed. It mapped an intrinsic *short
  name* to its `.Platform` repr through `OpenScope.tryResolve … providerPlatform`; but
  its sole caller (`tryExternalReceiver`'s `TyConst` arm) already holds the receiver's
  canon `SymbolKey`, so the opens-resolve was **vestigial** — the value was downgraded
  to a spelling only to re-resolve it (the round-trip tell). It now takes the
  `SymbolKey` and reads the store's `IntrinsicForwardRepr : SymbolKey → repr` for the
  referenced-package path (self-compiling intrinsics still answer first from the local
  short-name `IntrinsicReprTypes` table, which was never a resolver reach). No opens
  funnel; mirrors the already-landed `canonKey` rework two frames up. Green across the
  SA + CLR + JS suites (intrinsic-receiver member routing — `obj.ToString`,
  `exn.Message` — on both targets).
- **The exposure flip landed.** `PassContext` now exposes `member Provider :
  IExternalSymbolStore` as the default face every downstream pass speaks, plus one
  narrow, documented `member Resolver : IExternalSymbolResolver` (both free upcasts of
  the same backing object; `SideTables.fs:1607/1627`). Compiler-driven: narrowing
  `Provider` turned every surviving resolver-face reach into a type error. That
  surfaced **three more `TryLookupType(qualifiedName key)` round-trips** the earlier
  sweep missed (`InferRecordAccess.fs:326/380`, `EngineCore.fs:713` — interface keys
  harvested off a resolved shape), each converted to a key-addressed store lookup the
  caller already held. The sanctioned `ctx.Resolver` readers are enumerated on the
  member's doc-comment: NameResolution (the resolve-once home),
  `Translate.tryResolveExternalType` (the permanent escape hatch, § Remaining), and
  codegen's cross-package inline-body key interning (`SymbolProviders`, the Stage-5
  by-name channel).
  - **Enforcement is convention, not structure — and blocker 2 forecloses the
    stronger form.** The plan aimed for "the resolver method isn't reachable"; but
    `Translate.tryResolveExternalType` needs the resolver deep in Unification with only
    `ctx` in hand, so the resolver *must* be a `PassContext` member and is therefore
    reachable by any pass. What the flip actually buys: the store face is the *default*
    (`ctx.Provider` cannot resolve a spelling), and every resolver reach is a single
    named, greppable member with an enumerated reader set — a new consumer-pass string
    lookup reads as the anomaly it is, rather than being structurally impossible. This
    is a real softening of the original promise, recorded here honestly.
  - **NameResolution reads `ctx.Resolver`, not a threaded `run` parameter** (a
    sanctioned deviation from the "run gains a parameter" wording). The recursive
    CST-walk call graph is deep enough that a separate `resolver` argument would be
    pure redundant plumbing duplicating what `ctx` already carries.
- **`PlatformTypes.isUnrepresentable` reworked to the store face** — the blocker-1 twin
  the flip surfaced. Its caller (`addUnrepresentable`) held the receiver `SymbolKey` and
  downgraded it to a short name to re-resolve over the ambient prelude; now it answers
  by key via `ExternalSymbols.tryLookupType ctx.Provider key`. One fewer `ctx.Resolver`
  reader (the three remaining are the resolve-once home, the blocker-2 handle, and the
  deferred codegen channel). Green on SA + JS suites (the `decimal`-on-JS
  unrepresentable diagnostic).

## Remaining — post-flip follow-ups

The flip has landed; **nothing gates the boundary now.** One resolver reader remains by
design and one is deferred to its own sprint.

### `Translate.tryResolveExternalType` — the permanent escape hatch (decided)

`Translate.fs:502–600`, reached from `translateType` / `resolveNamedGeneric` at
`:234/254/328/477/769`. Resolves a written type *spelling* to a full `SemType` — abbrev
dealias, canon `TyConst`, capability `TyClass`, `TyRecord`/`TyUnion`/`TyEnum`.

This is **not** the same category as `intrinsicPlatformName`. That caller already held
a resolved key, so its string reach was vestigial and rework was the obvious call.
Here the input genuinely *is* a spelling — a written type annotation nothing upstream
has resolved — and, decisively, it is **name → type, not name → key**: `x: Box<_>`
mints a live `SemType` whose `_` is a fresh inference `TyVar`, and even a
fully-written annotation is *forming a fixed identity the receiver's `TyVar` must
comply with*. That is inference work, not resolution, and it belongs in the inference
section by construction. So the resolution is **not** hoisted upstream *by default* —
the escape hatch is the pragmatic home. The *structural* alternative — a general
NameResolution `Type`-node walker that resolves every written head to a `SymbolKey` and
stamps it, so this site reads the stamp on the store face and never resolves a spelling
— has since **landed** (`CstWalk.iterType` + `ResolvedTypeHead` stamping, read by
`Translate.tryResolveExternalTypeStamped`). It **narrowed** this hatch — Translate now
speaks the store face for every stamped head — but did **not** close it: a by-name
fallback remains for the positions still unstamped upstream (see the *cst-walker update*
below for which, and what retiring it would take).
Until then the flip keeps a single documented `IExternalSymbolResolver` handle used ONLY
by this site, and
that handle's whole surface is the **already-existing**
`IExternalSymbolResolver.TryLookupType : string` (`ExternalSymbols.fs:798`) — no new
method. This is the one sanctioned exception to the boundary statement (a spelling →
identity reach outside NameResolution), justified because the head-resolution is fused
here with the live-`SemType` construction it can't be cleanly severed from; the handle
is reachable, so this stays the single explicit string surface in Unification.

**Why the handle is that minimal — the blocker-2 trace:**

- The sole opens-sensitive reach inside `tryResolveExternalType` is the one
  `TryLookupType key` **string** probe per `keysFor` candidate. Every shape arm then
  reduces to a single surviving identity: Union/Record/Enum/Class/IntrinsicInterface
  each mint one `externalTypeKey origin key arity`; Intrinsic reads the shape's
  `Id.Canon`; and an **Abbrev has no surviving key at all** — it *dealiases* to its
  (already-frozen) body via `instantiateDeclaring`. So the head is one
  `string → identity` step for every case; the `SemType` construction (arg
  translation, thaw) around it is inference-resident and touches no resolver.
- **`Vesper.Choice<int,string>` resolves to `TypeKey(Some asm, "Vesper", "Choice\`2")`** —
  arity-suffixed via `arityName`/`externalTypeKey`, `asm` the CS0433 tiebreaker,
  confirming the arity-disambiguated identity.
- The abbrev dealias is symmetric on **both** faces: the producer `mkNominal`
  (`VesperLib/TypeTranslate.fs:457`) `substituteDeclaring`-expands an abbrev named in a
  contract body, and the consumer here `instantiateDeclaring`-expands one written in
  user source. Neither pins an abbrev's name as a nominal identity. (The stale
  `Translate.fs` doc-comment claiming abbrevs are "left to the opaque fallback …
  [preserving] the abbrev name the extractor convention pins" was corrected to state
  this dealias, in both directions.)
- **Decoupled from `inline-body-freeze-thaw-plan`.** This site's one thaw-shaped arm
  already runs on `FrozenType` (`instantiateDeclaring : FrozenType → SemType[] →
  SemType`), so it is a *precedent* for that plan's frozen-template + consumer-thaw
  doctrine, not a dependent of it. Both converge on the shared `instantiateWith`
  (`SemanticInfo.fs:1271`) seam; neither gates the other. The flip can land ahead of,
  behind, or independent of the freeze-thaw work.

**cst-walker update — the structural alternative's coverage wall, and why no
`Type` mapper unifies the descents (2026-07-10).** The CST `Type`-walker work (which
built `CstWalk.iterType` and the type-head stamping) probed the "read the stamp, never
resolve a spelling" alternative and mapped its real cost against this hatch:

- The one head that forced the fallback from a **synthesized** node — the `float<m>`
  measure carrier, which `translateType` rebuilt as a phantom `Type.NamedType` and
  re-resolved — is gone: the measure arm now resolves its carrier by name directly
  (`resolveBareTypeName`, off the stamped path), retiring that synthesized-node reader.
- But the `translateType` sites **still cannot drop the by-name fallback**: dropping it
  regresses (`ExternMemberElab` — an `int` at a member position resolves opaque `("", int)`
  vs the contract's `("Vesper", int)`), because written positions BEYOND the measure arm
  remain unstamped — a type member's **argument-pattern** annotations (`stampMethodOrProp`
  stamps only the return type), a member body's **ILIntrinsic result-type** annotation
  (`stampExprEmbeddedTypes` does not reach it), and the **intrinsic-abbrev host**'s
  side-elaborated member signatures. Retiring the hatch's Unification reach needs those
  three stamped upstream, then all three suites re-verified. So the hatch narrows but does
  not close — consistent with "permanent escape hatch (decided)" above.
- **A `Type` mapper does not unify the remaining `Type → SemType` descents — assessed,
  declined.** `translateType` is fused with mutable inference state (levels, `TyparScope`
  mutation, abbrev fill, `MarkInferenceHole`, fresh-TyVar back-fill for a bare generic) and
  is not a pure fold — the same fact that makes the name→type construction inference-resident
  here; `translateInheritArg` could be a fold but is the sole such consumer, with a bespoke
  per-node algebra (its own registry cascade, immutable `typarScope`, deliberate opaque-abbrev
  policy) and is a v1 registration-time stopgap; `resolveInheritParent.head` is a shallow
  head-peel, not a fold. A shared catamorphism would serve ~one bespoke consumer, so no
  mapper was built. Only the pure *iteration* (`implicitMemberTypars`'s free-typar walk) fit
  the visit-only `CstWalk.iterType`, and was migrated.

### Wiring, as landed

- `PassContext` keeps the full `IExternalSymbolProvider` internally (constructor uses
  the resolver for `CapabilityIds`, `Intrinsics`, `CoreAccess`, `AmbientOpenPrefixes`)
  and exposes `member Provider : IExternalSymbolStore` + `member Resolver :
  IExternalSymbolResolver` (`SideTables.fs:1607/1627`).
- NameResolution's helpers (`Scope.fs` string-lookup sites, `tryResolveExternalTypeKey`
  / `tryResolveExternalClassKey` / `tryResolveExternalUnionOrRecordKey` /
  `stampExternalSymbol`, `MemberRegistration.fs:701/812`) read `ctx.Resolver`.
  `Translate.tryResolveExternalType` reads `ctx.Resolver`; everything else in
  Unification/Freeze reads the store-face `ctx.Provider`.
- Store-only and resolver-only helper params were narrowed to the minimal face
  (`ExternalSymbols`/`Intrinsics`: `tryLookupType`/`tryIntrinsicClass` →
  `IExternalSymbolStore`; `tryPickRuntimeType`/`tryRuntimeType`/`tryResolveIntrinsic*`
  → `IExternalSymbolResolver`), confining resolver access to genuine spelling helpers.
- `ConformanceTypars.checkFile` takes its own `IExternalSymbolProvider` parameter —
  like the extractor, not a boundary consumer; keeps the full provider.

## Stage 5 — codegen `BuiltinOps` by-name → by-key

The second by-name mechanism: codegen recognises operators by the `External`
name string (`EmitJsContext.fs`, `JsFlatFns.fs`, `ClrRecipes.fs`,
`ClrExternalMembers.fs`), and the `ICodegenSymbols` bridge / codegen
value-by-name lookups stay string-addressed. Same class of leak, larger blast
radius; the store-only codegen face makes it look as anomalous as it is.
Separate doc when reached.

## Deferred residue (triggers unchanged)

- **Type-vs-module shadowing (kind-tagged prefixes).** Resolver-internal after
  the flip. Wants a forcing corpus case before the flat prefix list grows a kind
  tag / namespace tree; surface with a diagnostic before generalising.
- **Resolution caching.** A per-`PassContext` memo `(OpenScope identity, name) →
  hit-or-miss` inside `tryResolve`/`resolveIdent`; per-file lifetime. Land only
  against a measured hot path — the flip improves the eventual design, since the
  resolver face becomes the *only* string surface, so the memo covers the entire
  spelling-lookup seam.

## Non-goals

- Re-keying the resolver-face calls in NameResolution / the extractor — those
  *are* the resolve-once boundary; string-keyed there is the design.
- Fixing `MemberKey.argSig` overload identity (SemanticInfo.fs:77–93).
- Materialising a real `DefaultOf<T>()` (`unchecked-defaultof-plan.md`).
- Cross-file / cross-package resolution caching.
- Anything in the descriptor payload (`BuildSignature` / `ExternalSignature` /
  `FrozenType`).

## Key files

- `ExternalSymbols.fs:789–921` — the split faces + combined provider; decorators
  (composite `firstHit`, `mapMember`, caching), null provider.
- `SideTables.fs:1607/1627` — `PassContext.Provider : IExternalSymbolStore` (default
  store face) + `PassContext.Resolver : IExternalSymbolResolver` (the narrow handle,
  with its enumerated-reader doc-comment); also `PassContext.CoreAccess` /
  `CoreAccessIntrinsics` (the landed access-intrinsic resolution).
- `SymbolKeyOps.fs` — `qualifiedName` (the round-trip buried in the store),
  `qualifiedTypeKey` / `externalTypeKey` mint helpers.
- `Passes/NameResolution/Scope.fs`, `CstWalk.fs` — the resolver face's permanent
  home; the stamp writers; reads `ctx.Resolver`.
- `Passes/Unification/EngineCore.fs:535` (`intrinsicPlatformName`) — key-addressed via
  the store's `IntrinsicForwardRepr`; no longer a resolver reach.
- `Passes/Unification/Translate.fs:502` (`tryResolveExternalType`) — the permanent
  escape hatch; reads `ctx.Resolver` (the single sanctioned `TryLookupType : string`).
- `PlatformTypes.fs:45` (`isUnrepresentable`) — reworked to the store face
  (`tryLookupType ctx.Provider key`); no longer a resolver reader.
