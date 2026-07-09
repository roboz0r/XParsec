# `seq` / `enumerator` iteration capability — design & plan

## Objective

Promote `seq<'T>` from a transparent abbreviation (`type seq<'T> = IEnumerable<'T>`,
`capabilities.fsi`) to a first-class **language capability**, the same species as
`disposable` / `equatable` / `comparable`: a BCL-free `extern interface` that a Vesper
type can implement directly, that each backend lowers to its platform iteration idiom,
and through whose member slots `for … in` is lowered on every target.

The decisive design choice (settled): the capability is **member-bearing**, not a
zero-member marker. The other three capabilities are protocols the compiler lowers
*through* (`use`→`Dispose`, `[<CustomEquality>]`→`Equals`, `[<CustomComparison>]`→
`CompareTo`); iteration joins them. A zero-member `seq` would only *tag* iterability and
leave the "how" on the untyped structural `GetEnumerator` path — the opposite of
capturing iteration in the type system. Member-bearing `seq` is the only option that lets
Vesper code **author** its own iterable types through the type system.

The member surface is deliberately **not** the BCL signature. It is the minimal
platform-agnostic *pull* protocol both current targets (and future ones) reduce to;
each backend lowers it to its idiom.

## Semantic premises (confirm before coding)

- **P1 — Pull protocol, two capabilities.** Iteration is a *cluster* of two capabilities:

  ```fsharp
  // Vesper.Collections
  type seq<'T> = extern interface with
      abstract member GetEnumerator: unit -> enumerator<'T>

  type enumerator<'T> = extern interface with
      abstract member MoveNext: unit -> bool
      abstract member Current: 'T
  ```

  `seq`'s member returns the *sibling capability* `enumerator<'T>`, **not** the BCL
  `IEnumerator<'T>` — that is what keeps the cluster BCL-free and self-contained (it was
  the false blocker in the earlier "it drags in `IEnumerator`" analysis).

- **P2 — `enumerator<'T>` is a NEW, 5th capability.** `CapabilityIds`
  (`RuntimeNames.fs:303-319`) is closed-by-construction; this adds one field.
  `enumerator` reconciles to `System.Collections.Generic.IEnumerator\`1` on CLR and to the
  JS iterator object on JS.

- **P3 — No `Reset`, no non-generic bases, no `Dispose` in the capability.** The capability
  declares only `MoveNext`/`Current`. Everything the BCL interface hierarchy additionally
  demands (`IEnumerator.Reset`, non-generic `IEnumerable`/`IEnumerator`, `object Current`)
  is **synthesized by the CLR backend** during reconciliation (see Lowering §CLR). Disposal
  is **not** part of this capability — it composes: an `enumerator` that *also* implements
  `interface disposable` is disposed in the loop's `finally`, exactly as today's
  `dispose: bool` axis already handles (`SideTypes.fs`, `EmitLoops.emitEnumeratorLoop`).

- **P4 — `seq<'T>` becomes nominal/opaque.** It loses transparent `= IEnumerable<'T>`.
  A value flows into a `seq<'T>` parameter by the nominal upcast `List<'T> :> seq<'T>` plus
  cross-face reconciliation (`CapabilityIdentity` two-face match + `IntrinsicReverseCanon`),
  the `exn === System.Exception` mechanism generalized. This is the main churn surface
  (see §Blast radius).

- **P5 — `for … in` lowers through the capability slots on every target.** Not through the
  hardcoded BCL `IEnumerable`/`IEnumerator` slots. The working precedent is the
  `ConstrainedInterface` axis (`ForInGetEnumG.ConstrainedInterface` /
  `ForInEnumMembersG.ConstrainedInterface`, `SideTypes.fs:104-130`), which already lowers
  `for` over a generic typar source through an arbitrary non-BCL seq interface's
  `GetEnumerator`/`MoveNext`/`Current` via `constrained. callvirt`
  (`InferControlFlow.tryTyparSeqSource`, `EmitLoops.constrainedSlot`).

- **P6 — `for` stays the idiom, but the protocol is now user-implementable.** Manual
  enumeration (`xs.GetEnumerator()` …) is *technically* possible against the members (as
  `x.Dispose()` is against `disposable`), but `for … in` remains the sanctioned surface
  syntax and the only thing codegen specially lowers.

## Contract shape (the three declaration sites — mirrors the other three)

1. **`src/Vesper.Core/capabilities.fsi`** — move `seq` from the bottom into the anchor
   block as an `extern interface`, and add `enumerator`. Both stay in `Vesper.Collections`
   so every bare `seq<'T>` reference and the `Vesper.List`-anchoring rationale are unchanged.
2. **`src/Vesper.Core/capabilities.fs`** — platform reprs, so `harvestIntrinsicReprsInto`
   builds the `{platform → canon}` reverse-canon entries:
   ```fsharp
   type seq<'T> = (# "System.Collections.Generic.IEnumerable`1" #)
   type enumerator<'T> = (# "System.Collections.Generic.IEnumerator`1" #)
   ```
   Repr strings must match provider `FullName` byte-for-byte incl. the backtick arity.
3. **`src/Vesper.Core/capabilities-compat.js.fsi`** — JS fold rows so BCL-spelled source
   resolves on JS the same way BCL metadata answers on CLR:
   ```fsharp
   type IEnumerable<'T> = Vesper.Collections.seq<'T>
   type IEnumerator<'T> = Vesper.Collections.enumerator<'T>
   ```

Extraction/finalize (`VesperLib.fs:1386-1438`, `:561-583`) already republishes an
`extern interface` as an `IntrinsicInterface` with its `Members` populated — no changes;
`seq`/`enumerator` ride the existing `PendingCapabilityInterfaces` path.

## Authoring — already covered by the kind-agnostic interface-impl pipeline

No new authoring machinery. `interface seq<'T> with member GetEnumerator() = …` flows
through exactly the path `interface disposable with …` uses:

- `extractInterfaceImpls` → `ClassInterfaceImplInfo` (raw `InterfaceCst`) on the
  kind-agnostic `IInterfaceImplHost` (`MemberRegistration.fs:387-410`, `SideTables.fs:146-183`).
- `resolveInterfaceImpls` stamps `Resolved = TyClass(Vesper.Collections.seq, …)` — an
  `IntrinsicInterface` always qualifies as an interface target (`Unification.fs:938-975`).
- `checkInterfaceConformance` name-matches + **invariantly unifies** each body against
  `IntrinsicInterfaceShape.Members` (`Unification.fs:846-886`, esp. `:853-855` which already
  accepts the `IntrinsicInterface` face). For `seq` the single required member is
  `GetEnumerator: unit -> enumerator<'T>`; the body's return type must unify with
  `enumerator<'T>`, which forces the returned type to *itself* implement `interface enumerator`.
- Freeze projects `(Resolved, bodies)` into `TTypeKind.*.interfaces` (`Elaborate.fs:706-737`,
  `Tast.fs:580-596,653`); the frozen `FTClass(canonKey,args)` **is** the capability identity.

## Capability resolution changes (`RuntimeNames` + `ExternalSymbols`)

- `RuntimeNames.CapabilityIds` gains `Enumerator: CapabilityIdentity voption`
  (`RuntimeNames.fs:303-319`), plus `.none` (`:313-319`).
- `ExternalSymbols.resolveCapabilities` (`ExternalSymbols.fs:1039-1082`): `Enumerable`
  switches from `resolveAbbrevHead "Vesper.Collections.seq\`1"` to
  `resolveAnchor "Vesper.Collections.seq\`1"` — upgrading it from a single-faced
  (`Key = IEnumerable\`1`, `CanonKey = ValueNone`) to a **dual-faced** identity
  (`Key = System…IEnumerable\`1`, `CanonKey = Vesper.Collections.seq`). Add
  `Enumerator = resolveAnchor "Vesper.Collections.enumerator\`1"`. Delete `resolveAbbrevHead`
  if it has no other caller.
- Recognizer sites that read `ctx.CapabilityIds.Enumerable` keep working (now dual-faced);
  audit for any that assumed the single-faced shape. Add `Enumerator` consulting where the
  enumerator identity must be recognized (JS partition already keys enumerator members
  structurally off the returned type — verify).

## Lowering

### CLR (the meaty part: co-slot synthesis)

For BCL interop, a Vesper type implementing `interface seq<'T>` must *really* emit as
`System.Collections.Generic.IEnumerable\`1` (so it can be handed to any BCL API). The BCL
interface hierarchy demands more than the capability declares, so reconciliation must
**synthesize the co-slots** the author never wrote:

- `interface seq<'T>` → emit `IEnumerable<'T>` **and** non-generic `IEnumerable`, the latter
  forwarding to the capability `GetEnumerator`.
- `interface enumerator<'T>` → emit `IEnumerator<'T>` **and** non-generic `IEnumerator`
  (`object Current` forwarding to `Current`, `Reset` throwing `NotSupportedException`,
  non-generic `MoveNext`).

This is genuinely new: `disposable`/`equatable`/`comparable` reconcile to single-method BCL
faces with no co-slots. This is the concrete meaning of "each backend lowers the abstract
protocol to its platform idiom." Candidate home: the nominal-emit / capability-reconcile
path (`NominalEmit.fs` / `ClrExternalMembers.fs` / `ClrEnv.fs`), synthesizing forwarding
`MethodDef`s when a frozen type carries the `seq`/`enumerator` canonical interface.

`for … in` lowering: the CLR `Interface` arm currently hardcodes the BCL slots
(`EmitLoops.fs:371-466`, minting `IEnumerator\`1` at `:391-395`). Two viable routes —
   (a) keep hardcoding BCL slots and rely on co-slot synthesis so the reconciled type
       genuinely has them (simplest; interop-correct by construction), or
   (b) generalize the `Interface` arm to mint slots off the *canonical capability interface*
       via `constrainedSlot`, reusing the `ConstrainedInterface` machinery (`EmitLoops.fs:62-84`).
   Route (a) is likely less code and keeps the BCL path untouched; (b) is purer but
   duplicates what co-slot synthesis already guarantees. **Decision needed.**

### JS (mostly already done)

`partitionClassMembers` (`EmitJsTypes.fs:197-287`) already buckets `caps.Enumerable`
(`seq`/`IEnumerable`) impls into an `[Symbol.iterator]` generator, and `emitIteratorMethod`
(`EmitJsMembers.fs:70-100`) already drives `const e = <GetEnumerator body>; while
(e.MoveNext()) yield e.Current()` — i.e. the JS protocol row and its pull-shape assumption
exist today. `for … in` on JS lowers to `for…of` over the runtime `Symbol.iterator`
(`EmitJs.fs:701-728`). Remaining JS work is small: confirm the enumerator side needs no
distinct `[Symbol.iterator]` on the enumerator object (JS iterators expose `next()`, not a
nested iterable) — the generator shim already bridges `MoveNext`/`Current`→`next()`, so the
`enumerator` capability may need **no** JS emission of its own beyond being recognized.
Verify against `emitIteratorMethod`.

## Blast radius — the abbreviation → nominal shift

Every site that leans on `seq<'T>` being *transparently* `IEnumerable<'T>` moves to the
nominal upcast + reconciliation path:

- **`Vesper.Seq`** — `fold`/`reduce`/`truncate`/`toArray` all take `source: seq<'T>`
  (`seq.fsi`/`seq.fs`) and iterate internally. Iteration over a `seq<'T>`-typed value now
  lowers through the capability Interface path (reconciled to the platform iterable);
  passing a concrete collection in is a `:> seq<'T>` upcast.
- **`Vesper.List`** — `ofSeq: seq<'T> -> …`, `toSeq: … -> seq<'T>` (`list.fsi:181,184`).
  And critically, `list.fs:36-40` currently implements the **BCL** `IEnumerable<'T>` /
  `IEnumerable` directly with `ListEnumerator` implementing BCL `IEnumerator<'T>` /
  `IEnumerator` (`:58-61`). Under B these become `interface seq<'T>` / `interface enumerator<'T>`
  (Vesper-authored); `ListEnumerator` implements `interface enumerator`. The CLR co-slot
  synthesis then re-derives the BCL faces so nothing downstream (or interop) regresses.
  `list.js.fs` already goes through the capability path on JS.
- **Reconciliation** — `harvestIntrinsicReprsInto` / `IntrinsicReverseCanon`
  (`ExternalSymbols.fs:842-856`) must now cover `seq` and `enumerator` (falls out of the
  `capabilities.fs` reprs automatically).
- **Upcast/coercion** — `List :> seq<'T>` must be admitted as a capability coercion
  (`Subsume.fs:81-84` already admits `IntrinsicInterface` coercion; verify the generic-arg
  case). This is the same territory as `for … in` Gap 3 (generic user interface impls,
  `get-enumerator-gaps.md`) — watch for the "Free type parameter 'T not declared" impl gap
  when `List<'T> : seq<'T>` is generic.
- **Docs/comments** — `capabilities.fsi` header, `Vesper.Seq`/`Vesper.List` READMEs and
  manifests all describe `seq` as "= `IEnumerable<'T>`"; update to the capability framing.

## Build order

1. **Contract** — declare `seq`/`enumerator` in `capabilities.fsi` + `.fs` reprs + JS compat
   shim. Add `Enumerator` to `CapabilityIds` / `.none`; switch `resolveCapabilities`.
   (Compiles; capability resolves dual-faced; nothing consumes it yet.)
2. **CLR co-slot synthesis** — reconcile `interface seq`/`interface enumerator` to the full
   BCL faces. Gate every later CLR item.
3. **`Vesper.List` migration** — reauthor `List`/`ListEnumerator` onto the capabilities;
   prove `for x in aList`, `List :> seq`, and BCL-interop round-trip on CLR and JS.
4. **`for … in` route decision** (Lowering §CLR route a/b) + `Vesper.Seq` migration.
5. **Recognizer/coercion audit** — dual-faced `Enumerable`, `List :> seq<'T>` generic upcast.

## Progress & findings

**Step 1 (contract) — DONE & verified.** `seq`/`enumerator` declared as `extern interface`
capabilities (`capabilities.fsi` + `.fs` reprs + JS compat rows); `Enumerator` added to the
closed `CapabilityIds`; `resolveCapabilities` switched `Enumerable` to `resolveAnchor` and added
`Enumerator`. Both resolve **dual-faced** on CLR (`Key = System…IEnumerable\`1`/`IEnumerator\`1`,
`CanonKey = Vesper.Collections.seq`/`enumerator`) — asserted in `ReferencedProjectTests` (the
"dual-faced IntrinsicInterfaces" test now covers all five capabilities). Parse goldens
regenerated; the new interfaces parse with no recovery diagnostics.

**Latent bug found & fixed (`ExternalSymbols.stampType`).** `IntrinsicInterface.Origin` was
overwritten with the package-blanket manifest namespace (`Vesper`), but `seq`/`enumerator` live
in the sub-namespace `Vesper.Collections`. `externalTypeKey` then split the use-site key at the
wrong dot (`ns = "Vesper"`, `name = "Collections.seq\`1"`), so it never matched `Enumerable`'s
`CanonKey` (`ns = "Vesper.Collections"`) → **949/1253 CLR self-host tests failed** (Vesper.List's
`ofSeq` `for x in source` cascaded to everything depending on it). Fix: the `IntrinsicInterface`
arm now takes its origin namespace from `Id.Canon` (the real declaring namespace), keeping the
package assembly — mirrors what `stampSymbol` already does for value keys. `disposable` et al.
(in `Vesper`) are unaffected. **949 → 37 CLR failures.**

**Enumerator disposal composition — DONE.** `Vesper.Seq`'s eager terminals do
`use e = source.GetEnumerator()`, relying on the enumerator being disposable (BCL
`IEnumerator<'T> : IDisposable`). Fixed per **D1** by having the `enumerator` capability
**inherit `disposable`** (composition, not a declared `Dispose` member). Mechanics landed:
- `capabilities.fsi`: `type enumerator<'T> = extern interface with inherit Vesper.disposable; …`
  (`extern interface` DOES admit `inherit`; parses cleanly).
- `VesperLib.extractBodiedClassLike`: an **interface's** `inherit` clause is routed into the
  interface set (→ `FrozenInterfaces`), not the base type — interfaces have no base class.
- `IntrinsicInterfaceShape` gained an `Interfaces` field (the capability-interface analogue of
  `ExternalClassShape.FrozenInterfaces` / `Union.interfaces`), populated in the republish.
- `Infer.tryExternalDispose` gained an `IntrinsicInterface` arm surfacing `iface.Interfaces`, so
  `use e` on an abstract `enumerator<'T>` sees the inherited `disposable`.
  Asserted in `ReferencedProjectTests` (enumerator's shape carries `disposable`). The `use`
  disposal error is cleared.

**Capability face reconciliation at unify / subsume / overload — DONE.** A capability's two
faces (canonical `Vesper.Collections.seq` and BCL platform `System.Collections.Generic
.IEnumerable\`1`) are now INTERCHANGEABLE at the three nominal-key comparison seams, driven by
the resolved `CapabilityIds` (never a `seq`/`IEnumerable` string literal), fixing all five
capabilities uniformly:
- `EngineCore.capabilityCanonKey` folds either face → the canonical face; `sameNominalKey ctx
  k1 k2 = k1 = k2 || capabilityCanonKey k1 = capabilityCanonKey k2` is the shared predicate.
- Applied ONLY at key-EQUALITY seams: `unify`'s `TyClass/TyClass` arm (`Engine.fs`, the
  return / plain-`unify` direction), `subsumesNominal`'s `Equal` decision + `tryUpcastWitness`'s
  target match (`EngineCore`/`Subsume`, the argument-coercion / `:>` direction), and the
  overload filter `applicabilityMatches` (`InferOverload`, threaded as a `canon` fn; the frozen
  codegen path passes `id`). Deliberately NOT folded into `canonKey`/`subtypeNominalOf`: those
  drive the base/interface-chain LOOKUPS, and rewriting a platform key there erases the BCL
  type's own bases — it broke a genuine `IEnumerator\`1 :> IEnumerator` co-slot upcast until the
  reconciliation was moved to the comparison-only seams. `Enumerable.Take(source, count)` on a
  `seq<'T>` source now resolves and builds; all analysis errors cleared (CLR self-host 37 → 10,
  no regressions; SemanticAnalysis / Vesper suites green; JS unchanged — capabilities are
  single-faced there so the reconciliation is a no-op).

**Remaining CLR (10) — FIXED (manual-enumeration member-face bug).** `Vesper.Seq`'s terminals
(`fold`/`reduce`/`truncate`/`toArray`) enumerate MANUALLY (`use e = source.GetEnumerator()`;
`while e.MoveNext()`), not via `for … in`. A PLAIN `e.MoveNext()` on an abstract `enumerator<'T>`
receiver was keyed by the canonical capability (`Vesper.Collections.enumerator`), which reconciles
to the platform face `IEnumerator\`1` — but `MoveNext` is NOT declared on `IEnumerator\`1`
(inherited from the non-generic `System.Collections.IEnumerator` base), so the emitted member-ref
faulted `MissingMethodException: IEnumerator\`1.MoveNext()`. (`GetEnumerator`/`Current` are
declared ON the face, so they were correct; `use` disposal keys `Dispose` off `disposable.Key` =
`System.IDisposable` directly, also correct — MoveNext was the only broken slot.)

Fix (D2 co-slot member-face resolution, capability-general, no hardcoded BCL strings): a new
provider seam `ICodegenProvider.TryCapabilityBaseMemberKey` (impl `ClrExternalMembers`) recognises
a capability-interface declaring key (`ExternalTypeShape.IntrinsicInterface`), re-resolves the
member on the platform face's **metadata interface hierarchy** (`symbols.TryLookupMembers` walks
base interfaces and reports each member's true declaring type), and — when the member is NOT
declared on the face itself but inherited from a base — returns the key rebased onto that base's
real declaring type. `EmitResolve.externalInstanceMemberRef` applies the rebase BEFORE its
receiver-shape routing, so the rebased declaring key (`System.Collections.IEnumerator`) now
differs from the receiver key (`enumerator`) and the ref is minted against the base — the same
declaring types `for … in` hardcodes (`EmitLoops`), here derived from metadata. `recoverOpenTypars`
does same-head descent, so only non-generic bases (arity 0, no typar recovery) are rebased; the
face-declared generic slots are left untouched, which is why the approach is safe. **CLR self-host
10 → 0 (1253/1253 pass).** SemanticAnalysis 776/1 skip and Vesper 51 stay green; JS unchanged
(single-faced capabilities — `TryCapabilityBaseMemberKey` is a no-op there, and JS has its own
provider interface).

**Remaining JS — external-pack BCL face — FIXED.** JS capabilities are single-faced `Class` (no
`.js.fs` repr), so `Enumerable`'s only face was canonical `seq`; external TS packs (`Js.Set`/
`Js.Map`) spell their interface `System…IEnumerable\`1`, which no longer matched. Fix (two parts,
JS-resolution only — no codegen/emission change):
- `ExternalSymbols.resolveCapabilities` now gives the JS iteration capabilities a **second (BCL)
  face**. `resolveAnchor` takes the capability's BCL reconciliation spelling and, on the JS
  single-faced `Class` branch, mints a dual-faced identity (`Key` = BCL `System…IEnumerable\`1`,
  `CanonKey` = canonical `seq`) — mirroring the CLR `IntrinsicInterface` polarity — **only when
  the `capabilities-compat.js.fsi` shim CONFIRMS** the abbreviation `bcl → canonical`
  (`shimConfirms`; never blindly trusted). The already-landed `capabilityCanonKey` then folds an
  external `IEnumerable\`1` → `seq` on JS exactly as on CLR. The leaf caps stay single-faced on JS
  (their `use`/eq/comp fold at freeze via the shim). Design fork acknowledged: the provider exposes
  no reverse-abbreviation index, so the BCL spelling is supplied as the reconciliation constant and
  *verified* against the shim rather than reverse-derived.
- `ExternalSymbols.stack`'s `stampType` mis-split the **use-site** key of a JS capability `Class`.
  `seq`/`enumerator` live in the sub-namespace `Vesper.Collections` but `Vesper.Core`'s manifest
  namespace is `Vesper`; blanket-stamping `o.Namespace` left `externalTypeKey` to split
  `Vesper.Collections.seq` at the wrong dot (`ns = "Vesper"`), so the mis-split target defeated the
  capability `Matches` (asm-blind but ns-exact) and `List<'T> :> seq` failed with "no inheritance
  relationship". `stampType` now derives the namespace from the looked-up compiled `name` when it
  strictly extends the manifest namespace (guarded → types directly in the namespace unchanged) —
  the `Class`/`Union`/`Record`/`Enum` analogue of the `IntrinsicInterface` arm's `Id.Canon` fix. On
  CLR `seq`/`enumerator` are `IntrinsicInterface` (that arm already corrected them), so this is a
  no-op there; on JS it corrects the single-faced `Class`. **JS codegen tests 10 → 0 (348/348).**

**`Vesper.Seq` terminals → `for … in` (portability).** `fold`/`reduce`/`toArray` (`seq.fs`) were
manual `use e = source.GetEnumerator(); while e.MoveNext() do … e.Current` — CLR-idiomatic, no JS
lowering. Rewritten to `for x in source` over a mutable accumulator (lowers on BOTH targets: CLR
interface path, JS `for…of`). `reduce` has no explicit first-move AND the Vesper backend has no
portable "default value of `'T`" primitive (`Unchecked.defaultof` has no recognizer), so it
materialises via `for … in` then folds from the first element. `truncate` left as CLR-Linq `Take`.
Sited WHY comments added. CLR self-host stays green (1253); the `seq.fs.parsed` golden regenerated.

**Future JS work — manual enumeration lowering.** The manual `GetEnumerator`/`MoveNext`/`Current`
protocol's JS lowering is intended future work: map `seq.GetEnumerator()` via an intrinsic to
`$0[Symbol.iterator]()` with a small `Vesper.Core.mjs` runtime adapter providing the
`MoveNext`/`Current` split over the native `next() → { value, done }` (a stateless `(# #)` can't
express the shared-`next()`-result state). Recorded in `docs/get-enumerator-gaps.md` ("Remaining
work" item 2); the `seq.fs` `for … in` comment is the sited pointer.

## Decisions (settled)

- **D1 — CONFIRMED.** Disposal stays the *separate* `disposable` capability composed via the
  existing `dispose` axis; `enumerator` declares no `Dispose`. Leaf-member symmetry preserved.
- **D2 — CONFIRMED: co-slot synthesis (route a).** The type must **genuinely emit** the BCL
  interfaces so a C# consumer of a Vesper assembly can iterate it. So the CLR `Interface` arm's
  hardcoded BCL slots stay untouched; reconciliation synthesizes the full `IEnumerable`/
  `IEnumerator` faces (incl. non-generic bases, `object Current`, `Reset`) as forwarding shims.
- **D3 — CONFIRMED: vertical slice first.** Land capability + `Vesper.List` (steps 1-3), then
  `Vesper.Seq` separately.
- **D4 — CONFIRMED: nothing** relies on a `seq { }` CE builder or the non-generic
  `IEnumerable`. The non-generic co-slots are interop-only (synthesized, never authored).
</content>
</invoke>
