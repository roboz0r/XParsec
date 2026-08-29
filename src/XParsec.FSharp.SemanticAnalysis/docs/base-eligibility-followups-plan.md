# Base-eligibility follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are as of the commit that added this file; re-locate by construct.

## What already landed (context, not work)

`BaseEligibility.fs` answers "may this written type be an `inherit` parent?" for both front
ends: `BaseVerdict` is the verdict DU, `classify` maps a resolved `SemType` to one given a
key→interface oracle, and `admit` is the shared diagnostic table. The `.fs` ladder
(`InheritParent.fs`) feeds its reject arms through `admit`; the `.fsi` side
(`SignatureResolution/Members.fs`, `bodiedClassSurface`) classifies the translated base before
freezing, so an interface or a non-nominal base diagnoses instead of publishing or crashing.
Pinned by tests: the interface/non-nominal pair in `SignatureResolutionTests`, the
interface/tuple/control set in `Codegen.Clr.Tests/ClassTests`.

The follow-ups below are in dependency order; 1 and 2 are independent of each other.

## 1. Narrow the TAST base slot to `FrozenNominal voption`

`classify` is now the only producer of a class's base type, so the slot's SemType is always
`TyClass`/`TyConst` and its freeze always has a nominal head — but the TAST still stores a raw
frozen type and makes every consumer re-narrow:

- `TastDecl.fs:176` — the class node's `BaseType: 'ty voption`, generic over the type rep.
- `FrozenSignature.fs:249` — `c.BaseType |> ValueOption.map (FrozenNominal.OfFrozen "an
  \`inherit\` clause")`, a runtime narrowing that would throw.
- `Codegen.Clr/LayoutNodes.fs:101` — the same `OfFrozen` re-narrowing.
- `Codegen.Js/EmitJs.fs:923` — a defensive `TryOfFrozen` on the same field.
- `Members.fs` (`bodiedClassSurface`) — the remaining `OfFrozen "an \`inherit\` clause"`,
  now an assertion of the classify invariant rather than a reachable crash.

The change: make the class node's base slot `FrozenNominal voption` and delete all four
narrowings. Score it by checks deleted, not sites touched.

Mechanics to expect:

- `TastDecl`'s field is generic (`'ty`), instantiated at `SemType` pre-freeze and `FrozenType`
  post-convert (`TastConvert.fs:332` maps it with `fTy`). Either the field needs its own
  evidence type independent of `'ty`, or the narrowing happens once at `TastConvert` and the
  frozen-side node type changes. Prefer whichever keeps one narrowing site; per the repo rule,
  land the swap additively behind the existing accessors and delete the old shape in a
  separate change.
- Enumerate every reader of the field before touching it — `FrozenCodecDecls.fs:292`
  serialises it, `PlatformTypes.fs:127` walks it, `Unification/EngineCore.fs:255,545`
  instantiate through it.
- `ClassTypeInfo.BaseType: SemType voption` (`TypeInfos.fs:450`) can stay `SemType`: it is a
  mutable registration slot cleared on inheritance cycles
  (`MemberRegistration.fs:619-635`), and its values are already classify-vetted. A dedicated
  evidence DU there is a design fork, not a requirement.

Verification: full `SemanticAnalysis.Tests`, `Codegen.Clr.Tests`, `Codegen.Js.Tests` runs;
no new tests needed, the deletion is the point.

## 2. "May this written type be implemented?" — the interface-list twin

The dual question has the same disease the base type had:

- `.fs` side: `Unification`'s `resolveInterfaceImpls` owns the check ("Type '%s' is not an
  interface").
- `.fsi` side: `freezeInterfaces` (`Members.fs`, beside `bodiedClassSurface`) drops a
  non-nominal `interface` clause silently (`TryOfFrozen … | ValueNone -> ()`), and applies no
  interface-ness check at all — `interface SomeClass with` in a signature publishes the class
  in `FrozenInterfaces` without a diagnostic (assumed from reading, not probed; probe first).

The shape of the fix mirrors what landed: an `ImplVerdict` (or a generalisation of
`BaseVerdict` — decide when writing it; the admit-message table differs, the classify skeleton
and the `isInterfaceKey` oracle are shared with `Interface`/`Base` swapping roles), classify
before freeze in `freezeInterfaces`, route `resolveInterfaceImpls`' reject arm through the
shared message table.

Probes to pin red first, following the base-type tests in `SignatureResolutionTests`:

- `.fsi` class with `interface SomeClass with`-equivalent spec (`InterfaceSpec`) naming a
  non-interface — expect diagnostic, nothing published in `FrozenInterfaces`.
- `.fsi` interface spec naming a tuple/undefined type — silent-drop vs `AlreadyDiagnosed`
  split, same as the base path.
- An INTERFACE's own `inherit` clause routes into `interfaceTypes`
  (`bodiedClassSurface`), so interface inheritance must keep working — the capability chain
  (`enumerator inherit disposable`) is the regression canary.

## 3. `classify` admits every `TyConst`: `inherit int` in a signature still publishes silently

`BaseVerdict.Base` is returned for any `TyConst` head. On the `.fs` side that is safe — the
ladder only produces `TyConst` for vetted heritable canons — but on the `.fsi` side
`translateType` gives `TyConst` for *any* intrinsic, so a signature's `inherit int` publishes
an `FTConst` base with no diagnostic (pre-existing behaviour, deliberately preserved).

Tightening requires a heritability oracle beside `isInterfaceKey`:

- provider: `ExternalTypeShape.Intrinsic { Class = ValueSome surface }` with
  `surface.Heritable` (`ExternalSymbols.fs`, `IntrinsicClassSurface`);
- local file: `ctx.Types.IntrinsicReprKeys` → `repr.Heritable` (as
  `InheritParent.fs`'s heritable-local arm reads it).

Risk, and why this was scoped out: the contract bootstrap signatures (`obj`, `exn`,
`Attribute` in the Vesper contract stack) are exactly the files that write intrinsic bases,
and resolution order decides whether the oracle can even see the shape yet. Per the working
agreement on guards: enumerate what the tightening would reject across the real contract
`.fsi` inputs before writing it — run the full suites with a probe version that *reports
without rejecting* if in doubt.

## 4. A capability named as a class base

`inherit disposable` (a capability / `IntrinsicInterface`) was flagged in the original
inherit-interface plan and remains unconfirmed on both sides:

- `.fsi`: likely resolves to `TyConst` of the capability canon → admitted by `classify`'s
  `TyConst` arm (same hole as item 3, interface-flavoured).
- `.fs`: `providerBaseOf` (`InheritParent.fs:118`) matches `ExternalTypeShape.Class` and
  intrinsic-class shapes only, so an `IntrinsicInterface` makes the provider decline and the
  name falls through to unknown/not-a-class — a diagnostic, but the wrong one.

Probe both before deciding: the right outcome is `BaseVerdict.Interface` with the
"implement it with 'interface … with'" message. On the `.fsi` side this may fold into item
3's oracle (a capability canon is not heritable); on the `.fs` side it is one new arm in
`providerBaseOf`.

## Observation to confirm (no work scheduled)

`bodyIsInterface` (`Members.fs:244-260`) counts a `TypeSignatureElement.Inherit` as CONCRETE,
so a bodied signature that is all-abstract *plus* an `inherit` is judged a CLASS. In F#, `type
I2 = inherit I1  abstract M: …` in a signature is an interface. If that parity matters, the
fix interacts with item 2 (the `inherit` would need to route to `interfaceTypes`, not
`baseClause`). Confirm intended semantics with the user before touching it.
