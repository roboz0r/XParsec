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

**Target end state for Unification: zero resolver-face calls.** Nothing audited
is type-directed name resolution. The two things that *feel* like inference-time
resolution are not: member access `x.M` needs the receiver's inferred type, but
that is a store-face lookup (`declaring key × member name`) — the member name is
a post-dot spelling, not opens-sensitive; and overload resolution selects among
candidates the store already returned — a type-directed *choice over resolved
identities*, not resolution.

The tell for a violation is the round-trip idiom
`ctx.Provider.TryLookupType(SymbolKeyOps.qualifiedName key)` — the caller *holds*
the identity and converts it back to a string because the interface offered
nothing better. Those are gone from the SA passes; the two remaining
resolver-face reaches through `ctx.Provider` are genuine spelling resolutions
(§ Remaining).

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
- **Enforcement is exposure, not the split itself.** The flip (§ Remaining)
  declares `PassContext.Provider : IExternalSymbolStore`; the resolver face is
  handed only to `NameResolution.run` and the extractor. Once flipped a future
  pass *cannot* reintroduce a string lookup — the method isn't reachable.

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

The boundary is in place *except for the exposure flip*. In brief:

- **The split interfaces** and every provider/decorator/fake migrated; all
  SA-side `qualifiedName` round-trips deleted (keys threaded directly); the
  composition-time CS0433 duplicate diagnostic.
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

## Remaining — the deferred exposure flip

Two resolver-face reaches through `ctx.Provider` survive in Unification, plus a
mechanical threading fan-out. The flip cannot compile until the two are gone, so
each must be resolved (or given the sanctioned escape hatch) first. **The flip
compiling is the compiler-checked "done" bit for the whole boundary** — any
missed holdout becomes a type error, not a review find.

**The two genuine blockers (not mechanical):**

1. **`EngineCore.intrinsicPlatformName`** (`EngineCore.fs:535–548`). Maps a short
   intrinsic canon name → its `.Platform` repr via `OpenScope.tryResolve …
   providerPlatform`, so the dot-access resolvers can key an intrinsic
   *receiver*'s instance members on the platform type name. This was
   *deliberately kept* on the string face (its input is a short name and its
   opens funnel does genuine short→qualified resolution) — a decision that now
   directly conflicts with the flip. The store already publishes
   `IntrinsicForwardRepr : SymbolKey → platform-repr`, so a key-addressed
   rework is plausible IF the caller (`tryExternalReceiver`'s `TyConst` arm)
   threads the canon `SymbolKey` rather than the bare name. **Undecided:** rework
   to the store's forward axis, vs. the narrow escape hatch.

2. **`Translate.tryResolveExternalType`** (`Translate.fs:502–600`, reached from
   `translateType` / `resolveNamedGeneric` at `:234/254/328/477/769`). Resolves a
   written type *spelling* to a full `SemType` — abbrev expansion, canon
   `TyConst`, capability `TyClass`, `TyRecord`/`TyUnion`, `TyEnum`. Moving it
   upstream needs a general `Type`-node walker in NameResolution that this plan
   deliberately avoided (the existing stamps each key one narrow expression
   position). **Undecided:** build the upstream type-walker + a SemType stamp,
   vs. the narrow escape hatch.

**The narrow-capability escape hatch** the boundary statement reserves — a single
documented `IExternalSymbolResolver` handle used ONLY by these two sites — lets
the flip land now with the store face as the default (the enforcement win)
while these two spelling resolutions stay explicitly string-faced. It weakens
enforcement (the handle is reachable), so it is a fallback, not the goal.

**The mechanical fan-out the flip forces (no design questions):**

- `PassContext` constructor keeps the full `IExternalSymbolProvider` internally
  (it already uses the resolver for `CapabilityIds`, `Intrinsics`, `CoreAccess`,
  `AmbientOpenPrefixes`) and exposes only `member Provider : IExternalSymbolStore`.
- `NameResolution.run` gains the resolver face as a parameter; NameResolution's
  own helpers (`Scope.fs`'s ~15 `ctx.Provider.TryLookup*` sites,
  `tryResolveExternalTypeKey` / `tryResolveExternalClassKey` /
  `tryResolveExternalUnionOrRecordKey` / `stampExternalSymbol`, and
  `MemberRegistration.fs:777`) read that parameter instead of `ctx.Provider`.
- `ConformanceTypars.checkFile` already takes its own `IExternalSymbolProvider`
  parameter — like the extractor, it is *not* a blocker and keeps the resolver.

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
- `SideTables.fs` — `PassContext.Provider` (the flip site) and
  `PassContext.CoreAccess` / `CoreAccessIntrinsics` (the landed access-intrinsic
  resolution).
- `SymbolKeyOps.fs` — `qualifiedName` (the round-trip buried in the store),
  `qualifiedTypeKey` / `externalTypeKey` mint helpers.
- `Passes/NameResolution/Scope.fs`, `CstWalk.fs` — the resolver face's permanent
  home; the stamp writers; the helpers the flip must re-thread.
- `Passes/Unification/EngineCore.fs:535` (`intrinsicPlatformName`),
  `Passes/Unification/Translate.fs:502` (`tryResolveExternalType`) — the two
  remaining resolver-face reaches that gate the flip.
