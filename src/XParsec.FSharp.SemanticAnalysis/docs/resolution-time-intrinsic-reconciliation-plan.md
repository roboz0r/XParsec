# Resolution-time intrinsic reconciliation (retire the unify-time reverse-canon bridge)

**Status: DESIGN — doc-first, no code beyond the `exn`/`obj` `extern class` enabling edits
(LANDED & GREEN: SA 763, Vesper 49, Clr 1251, Js 348).** Premises marked *VERIFY* must be
confirmed against the code before implementation. This doc supersedes the
`canonName→canonKey` framing of Stage 2 in
`contract-sourced-intrinsic-identity-plan.md` for the `obj`/`exn` subtype-root cases — see
"Interaction with Stage 2" below.

## The principle (user, confirmed)

Strings flow only at **name resolution** (user/bootstrap source → a fully-qualified identity).
After that, fully-qualified `SymbolKey`s flow everywhere and compare by **exact `=`**. A
platform-string reverse map consulted *inside the unifier* (`Engine.canonName`), and the
`sameTypeAsmBlind` compare it forces, are symptoms of reconciliation happening too late — at
every compare — instead of once, at the resolution boundary. The goal of this milestone is to
move the `System.Object ↔ obj` / `System.Exception ↔ exn` reconciliation to resolution so the
unifier sees canonical `Vesper.obj` / `Vesper.exn` identities and never reconciles by string.

## Why this is possible now (the `extern class` change unblocked it)

`MetadataSymbols.tryBuildType` (`MetadataSymbols.fs:83-95`) already documents the exact blocker
and why the reconciliation was split:

> Canonicalize a BCL primitive (`System.Int32 → int`) only when it is a SEALED leaf type. The
> harvested reverse map also carries the unsealed subtype ROOTS (`System.Object → obj`,
> `System.Exception → exn`) … those must keep their BCL nominal form here so ctor / `new` /
> subtype resolution still keys on it — they reconcile to their canon at the unification bridge
> (`Engine.canonName`), not eagerly. Scalar primitives + `string` are sealed; `obj`/`exn`/
> interfaces are not, so `IsSealed` partitions them exactly.

So the current architecture is **hybrid**:
- **Sealed leaves** (`System.Int32`, `System.String`, …) — reconciled **eagerly at resolution**
  (`tryBuildType` returns `FTConst(int)` directly). No unify-time reconciliation.
- **Unsealed roots** (`System.Object`, `System.Exception`) + capability interfaces — kept in BCL
  nominal form at resolution, reconciled **late** at the `canonName` unify bridge, *because as
  opaque value-intrinsics they had no class / heritable / member surface to carry ctor, `new`,
  base-chain, and member resolution.*

The `type exn = extern class` / `type obj = extern class` change removes that reason. `exn`/`obj`
are now heritable external classes (`HeritableExternBases`) with platform reprs
(`IntrinsicForwardRepr`: `Vesper.exn → System.Exception`, `Vesper.obj → System.Object`), so the
intrinsic identity can now carry everything the BCL-nominal form was being kept for. The unsealed
roots can join the sealed leaves on the **eager / resolution-time** side, and the unify-time
bridge for them retires.

## Empirical baseline (stage-1 probe — what actually fails today)

Two acceptance tests were written and run against the current tree (post-`extern class`, pre-move):
- **`inherit exn` downstream FAILS:** `"Cannot inherit from unknown type 'exn'"` +
  `TyClass(MyErr) does not support the 'subtype of TyConst(Vesper.exn)' constraint`. Root cause:
  `HeritableExternBases` (`SideTables.fs:807`) is populated **only in the self-host compilation** of
  Vesper.Core (`TypeRegistration.fs:580`, off the local `Type.ILIntrinsic(class)` decl). It does
  **not** propagate through the provider — a *referencing* unit sees `exn` as
  `ExternalTypeShape.Intrinsic`, which carries no heritability/base, so `resolveInheritParent`
  (`MemberRegistration.fs:774`, gated on the local `HeritableExternBases`) can't admit it. **⇒ The
  Intrinsic→faced-`Class` flip (stage 2) is not merely about ctors/forward-repr; it is what makes
  `exn` heritable downstream at all** — a `Class` shape with `FrozenBaseType` is admitted as a
  parent through the ordinary provider path, no `HeritableExternBases` propagation needed.
- **Upcast to `exn`/`obj` via annotation FAILS:** even though `raise (InvalidOperationException)`
  argument-subsumption works, an explicit `: exn = e` / `: obj = e` coercion of a BCL exception does
  not type-check — the `System.Exception ↔ exn` reconciliation is not applied at the
  annotation-coercion seam. Enabled once metadata surfaces the roots as canon identities (stage 3).

These confirm the milestone is load-bearing (not cosmetic) and give the two `ptest` acceptance
gates. `HeritableExternBases` may become fully redundant once the flip lands and downstream
heritability flows through the `Class` shape — *VERIFY* whether any self-host path still needs it.

## Stage-2 attempt #1 (faced-`Class` flip) — REVERTED; what it taught us

Declaring `inherit obj` + `new:` ctors on `exn`/`obj` `.fsi` (forcing the `Intrinsic → faced-Class`
flip) was tried and **reverted** — it broke **~935 CLR tests**. Root cause: the flip changes
`obj`/`exn` from their `TyConst` intrinsic identity to a `TyClass`, and `obj` is a *pervasive value
type* (`value: obj` throughout `Vesper.Printf`/`structural-printer.fs`, etc.). The moment `obj` is a
`TyClass`, `IntrinsicBclMember` (which routes `value.ToString()` to the platform `System.Object`
per-target) stops firing, and `.ToString()` fails with "Cannot read member … from non-record
non-class type" — because `ToString` is (correctly) NOT contracted (it is CLR-only; not on JS).

The intended fix — make a faced non-interface `Class` resolve back to `TyConst(canon)` (preserving
the intrinsic identity, so platform member routing stays intact and contract members MERGE with
platform members) — is **correct in principle** (`Translate.fs` `tryResolveExternalType` was patched
this way and SA stayed green) but does **not** hold up in practice: a provider `Class` shape is
resolved to `TyClass` at **multiple** sites (`Translate.fs:210`, `:442`, `:507`, plus wherever else),
and `CapabilityFace` population is conditional (`VesperLib.fs:1347`, gated on `IntrinsicReprs`).
Patching every site back to `TyConst` AND guaranteeing the face is a wide, fragile sweep — and a
single miss on `obj` is catastrophic given its ubiquity.

**Design pivot — CHOSEN: a new `ExternalTypeShape.IntrinsicClass` case (user).** Not the faced-`Class`
flip (attempt #1, silent `TyClass` degradation), and not fields-on-`Intrinsic` (bloats the lean
scalar case + breaks every positional `Intrinsic(...)` match). A dedicated case keeps `Intrinsic`
lightweight for the truly opaque scalars (`int`/`float`/`char` — no base, no members) and gives the
class-shaped primitives (`obj`/`exn`) their own shape:

```fsharp
| Intrinsic of canon: SymbolKey * arity: int * platform: string option
| IntrinsicClass of
    canon: SymbolKey * arity: int * platform: string option *
    baseType: FrozenType voption *   // exn → obj; obj → ValueNone (root)
    members: ExternalMember[]        // `new:` ctors, frozen to `.ctor`
| Class of ExternalClassShape        // capability INTERFACES stay here (→ TyClass)
```

**The load-bearing win over attempt #1: compiler-forced exhaustiveness.** A new DU case makes every
non-exhaustive `match ExternalTypeShape` an FS0025 error until it decides how to treat
`IntrinsicClass` — turning the silent `obj → TyClass` degradation (the cause of the 935 failures)
into a compiler-enumerated worklist (`feedback_durable_knowledge_in_code`).

**Invariant: `IntrinsicClass` resolves to `TyConst(canon)`, exactly like `Intrinsic`.** So `value:
obj` stays `TyConst`, `obj.ToString()` keeps routing to `System.Object` via `IntrinsicBclMember`,
and the pervasive-value-type churn never happens. The class surface is read only by the three sites
that need it: the subtype walk (`baseType`), `InferCtor` (`members` → `.ctor`), `resolveInheritParent`
(admit as parent). The member MERGE holds by construction — `TyConst` receiver keeps platform
routing AND carries contract ctors.

It also simplifies the earlier open questions:
- `intrinsicForward` = `Intrinsic ∪ IntrinsicClass` (no `IsInterface` guard; capabilities stay `Class`).
- `translateType` = `Intrinsic ∪ IntrinsicClass → TyConst` (no `CapabilityFace` guard, no rename).
  `CapabilityFace` stays exactly as-is, for capability interfaces only.

Extraction (`VesperLib.fs:1327+`): `extern class with inherit/new:` + `isIntrinsic` → `IntrinsicClass`
(base from the `inherit` elem, ctors from the `new:` elems), NOT the faced-`Class` path. Capabilities
(`extern with abstract member`, interface) are unchanged.

Attempt-#1 residue kept as inert-but-correct: `translateType`'s faced-`Class`→`TyConst` guard
(`Translate.fs:507`) — harmless (no faced non-interface `Class` is produced under the chosen path),
remove during the sweep if it reads cleaner.

### Staging for the chosen approach (checkpoint after step 2)
1. Add the `IntrinsicClass` case (empty of producers). Build → the FS0025 list IS the site worklist.
2. Extraction produces `IntrinsicClass` for `obj`/`exn`. **Checkpoint: build the data-model + extraction
   green before the wide resolution sweep** (catch surprises early, not at 900 red).
3. Work the FS0025 list: mechanical majority `| Intrinsic … | IntrinsicClass … -> TyConst`; the three
   structural sites read the surface; `intrinsicForward`/`intrinsicPlatformName` include `IntrinsicClass`.
4. Re-add `exn`/`obj` `.fsi` contract members; flip the `inherit exn` acceptance test to green.

### Stage-2 STATUS — `IntrinsicClass` LANDED & GREEN (SA 763, Clr 1251, Js 348, Vesper 49)
The `IntrinsicClass` mechanism is implemented and the whole suite is green — the pervasive-value-type
regression (attempt #1's 935 failures) is fully avoided (`obj`/`exn` keep `TyConst`, member routing
intact). Done:
- `ExternalTypeShape.IntrinsicClass(canon, arity, platform, baseType, members)` added; the 7 FS0025
  exhaustive-match sites handled (identity axis → `TyConst`/canon; `mapShape` maps base+members).
- Non-exhaustive consumer sites given `IntrinsicClass` arms: `intrinsicReverse`/`intrinsicForward`
  (`TyparCapture`), `canonName` + `intrinsicPlatformName` (`EngineCore`), `PlatformTypes`,
  `SideTables.intrinsicKeyOf`, `JsExternalMembers`, `translateType` (→ `TyConst`).
- Extraction (`VesperLib`): `extern class with` + `isIntrinsic` → `IntrinsicClass` (base from
  `inherit`, ctors from `new:`); capability interfaces stay faced-`Class`.
- Subtype base-chain: `subtypeParentOf` reads `IntrinsicClass.baseType` (`instantiateBaseTypeFrozen`).
- `resolveInheritParent`: downstream `inherit exn` admitted via a provider `IntrinsicClass` probe →
  base `TyConst(exn)`.
- `exn`/`obj` `.fsi` contract members re-added (`inherit obj` + `new:` on `exn`; `new:` on `obj`).

**REMAINING (next increment) — codegen reference-class encoding for `inherit exn`.** The `inherit exn`
acceptance test now RESOLVES through SA (no more "unknown type") and reaches codegen, where it errors:
`ClrEncoder.encodeType` routes `exn`'s repr `System.Exception` through the value-type `PrimitiveRepr`
path ("no IL encoding for intrinsic representation System.Exception"). `exn`/`obj` are REFERENCE
classes — an `FTConst(exn)` used as a base (or any encoded position) must emit a class **TypeRef**
(`extends [runtime]System.Exception`), via the forward repr, not the value-type encoder. `obj` already
dodges this (`System.Object → ELEMENT_TYPE_OBJECT`); `exn` needs the reference-class path. Until then
the `inherit exn` test stays `ptest` (with a note); the upcast test stays `ptest` for stage 3.

## Target architecture

1. **`tryBuildType` canonicalizes the unsealed roots too.** Drop the `IsSealed` partition
   (`MetadataSymbols.fs:91-95`): every `reverseCanon` hit maps its BCL name to the canon identity
   at surfacing time, sealed or not. A metadata `System.Exception` surfaces as the canon `exn`
   identity; `System.Object` as `obj`. The `IsSealed` guard existed *only* to protect the roots,
   so it goes.
2. **The unifier stops reconciling by string.** `Engine.canonName`'s reverse-map tier (the
   `IntrinsicReverseCanon` lookup, `EngineCore.fs:452-454`) is dead for these cases once nothing
   surfaces a raw `System.Exception`/`System.Object` into the walk. The subtype walk compares
   canonical `Vesper.*` keys by `=`. This is what lets the Stage-2 `subtypeNominalOf`/compare use
   exact key equality for these roots instead of `sameTypeAsmBlind` (see Stage-2 interaction).
3. **`obj`/`exn` carry their class surface through the intrinsic identity.** Ctor/`new`, member,
   and base-chain resolution on `exn`/`obj` route through the platform repr
   (`intrinsicPlatformName`, `Vesper.exn → System.Exception`), exactly as `exn.Message` /
   `tryExternalReceiver` already route an intrinsic receiver's *instance members* today.

## The two hard sub-problems (feasibility gates — resolve in the design, before coding)

### A. Ctor / `new` resolution — DECLARE the ctors on the contract (user, preferred)
`new exn(...)` resolves ctors via `ctx.Provider.TryLookupMembers(name, ".ctor")` (`InferCtor.fs:105`),
keyed on a type-name string. Today `name` is the BCL `System.Exception` (kept nominal), so the
lookup hits the metadata `.ctor` catalogue directly. After eager canonicalization the receiver is
`exn`, so the ctors must be reachable under the canon identity.

**Resolution (chosen): declare the ctors on the contract.** `extern` signatures take a `with`
member surface (`capabilities.fsi` precedent: `type disposable = extern with abstract member …`),
and `new : … -> T` is a first-class signature member (`TypeSignatureElement.Constructor`,
`SignatureParsing.fs:71`). VesperLib freezes each `new: … -> T` into a `.ctor` `ExternalMember`
(`VesperLib.fs:483-507`, mirroring the metadata layer's `Name=".ctor"`, instance, `MemberKind.Method`
shape), keyed on the type's `compiled` key (`Vesper.exn`). So:

```
type exn = extern class with
    new: message: string -> exn
    new: unit -> exn
type obj = extern class with
    new: unit -> obj
```

makes `new exn "msg"` resolve through the ordinary member path at resolution — no
`intrinsicPlatformName` routing, no unify-time indirection. The contract *states* `exn`'s
constructible surface, exactly the "encode it, don't reconstruct it" direction. (The alternative —
keep `Intrinsic`, route `.ctor` lookup through `intrinsicPlatformName` at the receiver — is smaller
but leaves ctor resolution as a unify-time platform indirection; rejected in favour of the contract
declaration.)

**Consequence — the shape flips `Intrinsic → dual-faced Class` (the central ripple to validate).**
The `Intrinsic` shape carries no member slots, so *any* `with member` on an intrinsic re-registers
it via `extractBodiedClassLike` as `ExternalTypeShape.Class` with `CapabilityFace = { Canon =
Vesper.exn; Platform = "System.Exception" }` (`VesperLib.fs:1347-1360`) — the canonical identity is
preserved on the face, but the top-level shape is now `Class`, not `Intrinsic`. Effects:
  - **Helps gate B:** a `Class` shape has `FrozenBaseType`/`FrozenInterfaces` slots, so `exn→obj`
    can flow the ordinary `subtypeParentOf` provider path (fork B2) instead of a special-case.
  - **Breaks the codegen forward repr unless extended:** `intrinsicForward` (`TyparCapture.fs:411-424`)
    matches only `ExternalTypeShape.Intrinsic` and excludes faced `Class` shapes
    ("reconciliation-only, reverse"). Flipping `exn`/`obj` to `Class` would drop their
    `→ System.Exception` / `→ System.Object` forward repr that codegen needs for `newobj` / type
    refs. **Extend `intrinsicForward` to include a faced `Class` iff `not shape.IsInterface`** — the
    honest discriminator: capability *interfaces* (`disposable`) need no codegen value-repr
    (reconciliation-only), whereas `exn`/`obj` are *classes* that get instantiated/emitted, so
    `IsInterface` partitions them exactly (cleaner than a `Platform <> canon` guard, and it reuses a
    flag already on the shape).
  - **Every `match ExternalTypeShape.Intrinsic` site that currently fires for `exn`/`obj`** must be
    re-audited for the faced-`Class` case. The canon maps already handle both
    (`TyparCapture.fs:382-389`); `PlatformTypes`, `translateType`, `tryExternalReceiver`,
    `intrinsicPlatformName`, and codegen's repr path are the suspects. *VERIFY each.*

**Data-model note (the face is no longer capability-specific).** The dual-face marker on
`ExternalClassShape` is today `CapabilityFace: CapabilityPlatformFace voption` — the "IsIntrinsic
flag with data" (`{ Canon; Platform }`) that makes a `Class` *also* a reconcilable primitive.
`Intrinsic` (narrow scalars: `int`/`float`/`[]`/…) stays a distinct DU case — giving it member
slots would duplicate `ExternalClassShape`, so keep the three categories: scalars = `Intrinsic`;
capability interfaces + heritable primitive classes (`exn`/`obj`) = faced `Class`. Since `exn`/`obj`
now use the face, **rename `CapabilityFace`/`CapabilityPlatformFace` → `IntrinsicFace`/
`IntrinsicPlatformFace`** (mechanical; it is the general "this class also has a primitive canon/
platform identity" marker, not capability-specific). Do this rename in the same stage as the flip.

*VERIFY:* that the frozen `.ctor` members land under `Vesper.exn` (not the platform `System.Exception`)
in `TypeMembers`, so `InferCtor`'s `TryLookupMembers` keys on the canon identity the eager
canonicalization now produces.

### B. The base chain continues past the canonicalized root — DECLARE `inherit obj` on `exn`
`buildClassBaseType` maps a class's `t.BaseType` through `reverseCanon` level-by-level
(`MetadataSymbols.fs:527`). A BCL exception `T`'s `FrozenBaseType` already becomes `exn`. The walk
then needs `exn`'s base to be `obj`, and `obj`'s base to be the root (⊥). Once gate A flips `exn`/
`obj` to a `Class` shape, this dissolves by the **same contract mechanism**: `inherit` is a
signature member (`TypeSignatureElement.Inherit`, `SignatureParsing.fs:168`), and
`extractBodiedClassLike` freezes a declared `inherit <type>` into `FrozenBaseType`
(`VesperLib.fs:389-401`, already the mechanism behind "the Step-8 JS exception hierarchy"). So:

```
type exn = extern class with
    inherit obj
    new: message: string -> exn
    new: unit -> exn
```

sets `exn`'s `FrozenBaseType = obj`; `obj = extern class` (no `inherit`) is the terminal root. The
subtype walk's `subtypeParentOf` provider path (`EngineCore.fs:578-582` → `instantiateBaseType`)
then flows `T → exn → obj → ⊥` through ordinary contract heritage — **fork B2, no special-case**.
The earlier fork B1 (hard-coding the two roots' bases in the walk) is unnecessary given the flip;
prefer the declared `inherit`.

*VERIFY:* whether any BCL-exception subtype test exercises `bclExn :> obj` (vs only `:> exn`); if
`:> obj` is unexercised today the continuation gap is latent, so this is additive coverage rather
than a regression risk. Also confirm `obj`'s own `.ctor` (`new: unit -> obj`) and rootless base are
accepted (obj has no `inherit`, `FrozenBaseType = ValueNone`, matching `System.Object`).

## Other surfaces that consume the reverse map / BCL-nominal roots (must all move or stay coherent)

- **The contract provider's own `IntrinsicReverseCanon`** (`TyparCapture.fs:378-397`) is the
  *source* of the `{ System.Exception → exn }` entries (folded into `ctx.IntrinsicReverseCanon`).
  Retiring the *unify-time consumption* does not require deleting the map — it has **four** live
  readers, of which this milestone retires exactly **one**:
    1. `EngineCore.canonName` reverse tier (`EngineCore.fs:452`) — the subtype-root reconciliation
       — **RETIRES here.**
    2. `Engine.numericFamilyOr` (`Engine.fs:249`) — the **JS numeric-family widening** (`number` ←
       int/float/float32), keyed on the reverse map's *multi-canon* entries. Orthogonal to the
       subtype roots (single-canon), owns the JS `number` coercion — **STAYS.**
    3. `MetadataSymbols.tryBuildType` (resolution-time, CLR metadata) — **STAYS** (in fact does
       *more* work after this milestone: it canonicalizes the roots too).
    4. Codegen repr paths (`SymbolProviders.fs`, CLR backend) — **STAY.**
  So the map is load-bearing and remains; only `canonName`'s tier goes dead. (Corrects the earlier
  "sole consumer" premise — VERIFIED there are two *unify-time* readers, `canonName` + `numericFamilyOr`.)
- **`isSystemObjectKey` and the equality/derives predicates** (`RuntimeNames.fs:368`, its
  consumers at the unify equality boundary) match an *incoming* `TyClass` against `systemObjectKey`
  asm-blind. Once metadata surfaces `System.Object` **as `obj`** (canon identity, `asm=None`),
  these predicates should compare against the canon `obj` key by `=`, not the BCL `systemObjectKey`
  by `sameTypeAsmBlind`. This is the same "canonicalize then `=`" retirement of `sameTypeAsmBlind`
  the parent plan tracks — fold it in here for `obj` specifically.
- **JS.** No CLR-metadata provider on JS, so there is no `System.Object`/`System.Exception` to
  reconcile — the reverse map's CLR entries never arise. `exn`→`Error` is a single-face heritable
  class (value face and base face coincide), so it needs no eager-canonicalization work. `obj`→
  `unknown` stays the untagged value face (`unknown` is TS's ⊤, not a heritable class); the JS
  heritable root is `Object`, which is never emitted as an explicit base (rootless `class Foo {}`
  sits implicitly under `Object.prototype`, `JsPrint.fs:205`). So JS is unaffected by this
  milestone — confirm no JS test regresses, no JS code change expected.

## Interaction with Stage 2 (`canonName→canonKey`) — this REFRAMES it

The parent plan's Stage 2 proposed `canonKey : PassContext -> SymbolKey -> SymbolKey` whose 3rd
resolution tier is the reverse map, compared with `sameTypeAsmBlind`. **This milestone removes the
reason that tier and that comparator exist for the two subtype roots.** Sequencing:
1. **This milestone first** (resolution-time reconciliation for `obj`/`exn`): metadata surfaces
   the roots as canon identities; `canonName`'s reverse tier goes dead for them.
2. **Then Stage 2 currency change** is smaller and cleaner: `subtypeNominalOf → struct(SymbolKey *
   args)` comparing by **exact `=`** (or the arity-retaining asm-blind helper only where a genuine
   cross-asm BCL face remains — ideally none once resolution canonicalizes). Building `canonKey`
   *around* the reverse map first (as the parent plan's Stage 2 literally reads) would author logic
   this milestone then deletes — hence do this first.
3. **`sameTypeAsmBlind` retirement** becomes tractable: with the roots canonicalized at resolution
   and the well-known recognizers (`isSystemObjectKey`, …) re-keyed to canon `=`, the helper's
   domain shrinks to any residual genuinely-cross-asm BCL face; track its deletion as the tail.

## Staging (each stays GREEN; build/test only via ./claude_tools.cmd)

1. **Coverage FIRST — DONE (committed as pending acceptance tests).** The pre-existing behaviour
   (BCL-exception subsumption in `raise` context, `raise 42` rejected) is already guarded by the 5
   existing `ExceptionTests`. The NEW capabilities this milestone delivers do **not** work on the
   current tree (empirically probed — see "Empirical baseline" below), so they are committed as
   `ptest` (pending) acceptance tests in `ExceptionTests.fs`: `inherit exn` (raise + observe as a
   `System.Exception` subclass) and BCL-exception upcast to `exn`/`obj`. Flip each `ptest → test`
   as its enabling stage lands — the milestone is done when both are green.
2. **Declare the class surface on the contract** — add `inherit obj` + `new: …` ctors to
   `prim-types-exn.fsi` and `new: unit -> obj` to `prim-types-object.fsi` (and mirror the impl
   `.fs` as the conformance check requires — `SigShape.ExternClass` ↔ `ImplShape.IntrinsicClass`).
   This is the `Intrinsic → dual-faced Class` flip. **Fix the flip ripple in the same step:**
   extend `intrinsicForward` (`TyparCapture.fs:411-424`) to include a faced `Class` where
   `not IsInterface`, rename `CapabilityFace`/`CapabilityPlatformFace` → `IntrinsicFace`/
   `IntrinsicPlatformFace`, and audit every `match Intrinsic` site (`PlatformTypes`,
   `translateType`, `tryExternalReceiver`, `intrinsicPlatformName`, codegen repr). Compiler-driven:
   the flip changes the shape, so the incomplete-match warnings + test failures ARE the worklist.
   GREEN — this stage alone must hold before touching the unifier.
3. **Eager-canonicalize the roots in `tryBuildType`** (drop the `IsSealed` partition,
   `MetadataSymbols.fs:91-95`): every `reverseCanon` hit maps to canon at surfacing, sealed or not,
   so `System.Exception`/`System.Object` surface as `exn`/`obj`. GREEN.
4. **Retire `canonName`'s reverse tier** (`EngineCore.fs:452`) for the roots; confirm dead (no test
   relies on it — `numericFamilyOr` keeps its own use). GREEN. First removed `sameTypeAsmBlind`-era
   caller.
5. **Re-key `isSystemObjectKey` + equality/derives predicates** to compare canon `obj` by `=`
   (metadata now surfaces `System.Object` as `obj`). GREEN.
6. **Then** proceed to the parent plan's Stage 2 currency change, now reverse-map-free for the roots.

## Out of scope (recorded, deferred)

- **`top` vs `obj` split.** `obj` in F#/.NET conflates two concepts JS separates: the value ⊤
  (JS `unknown`, CLR `System.Object`) and the heritable class root (JS `Object`, CLR
  `System.Object`). A future `type top = (# "unknown" #)` (value ⊤) alongside `type obj =
  (# class "Object" #)` (heritable root) would make JS erasure honest — sharpest payoff is
  **boxing** (a boxed value on JS is `unknown`, never `Object`). CLR collapses both to
  `System.Object`, so the distinction is a JS-only refinement, orthogonal to this milestone (the
  CLR reconciliation only needs `obj` to be the root *on CLR*, where root = ⊤). Naming caveat: the
  ⊤ meaning is the high-frequency use of `obj`, so whichever name is the default should be ⊤. Aligns
  with `feedback_prototype_correct_semantics_over_fsharp_parity`.
- **Capability interfaces** (`disposable` etc.) also ride the reverse map's dual-face path
  (`TyparCapture.fs:386-389`, `CapabilityPlatformFace`). They are NOT subtype-root classes and are
  out of scope here; revisit whether they can likewise reconcile at resolution once the roots land.

## Premises to VERIFY before coding
1. ✅ VERIFIED: `IntrinsicReverseCanon` has two *unify-time* readers — `canonName`
   (`EngineCore.fs:452`, retires) and `numericFamilyOr` (`Engine.fs:249`, JS numeric widening,
   stays) — plus resolution-time codegen + `tryBuildType` (stay). Only `canonName`'s tier retires.
2. Declaring `new: …` on `exn`/`obj` freezes `.ctor` `ExternalMember`s keyed on the canon
   `compiled` key (`Vesper.exn`), so `InferCtor.TryLookupMembers` finds them post-canonicalization
   (`VesperLib.fs:483-507`). Gate A is contract-declared, not platform-routed.
3. Declaring `with member`/`new` flips `exn`/`obj` from `Intrinsic` to faced `Class`
   (`VesperLib.fs:1347-1360`) — the central ripple. `intrinsicForward` (`TyparCapture.fs:411-424`)
   excludes faced shapes and MUST be extended to include a faced `Class` where `not IsInterface`,
   else codegen loses `exn`/`obj` reprs. Rename `CapabilityFace` → `IntrinsicFace` in the same stage.
   Declaring `inherit obj` on `exn` sets `FrozenBaseType = obj` (B2, no special-case). Confirm
   `bclExn :> obj` is exercised.
4. `exn`/`obj` must NOT double-declare when routed as heritable classes — same uniqueness-gate
   argument as the parent plan (base `.fs` is the sole impl unit; `.js.fs` is harvested, not
   compiled).
5. No JS test path surfaces a CLR root — JS is untouched.

## Relevant memories
`feedback_redesign_doc_first` (this doc), `feedback_freeze_no_backend_knowledge` (canon identity
asm-blind/None; platform repr stays in backend), `feedback_mockbuiltins_is_a_trap` (source of truth
is the contract, not a hardcoded set), `feedback_dynamic_intrinsics_over_du_cases` (still one
`TyConst`/canon identity, resolved not authored), `feedback_prototype_correct_semantics_over_fsharp_parity`
(the `top`/`obj` split), `feedback_plan_docs_ephemeral` (delete on landing).
