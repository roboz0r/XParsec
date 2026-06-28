# Platform-independence plan: capability model (remaining work)

**Owner seam:** `XParsec.FSharp.SemanticAnalysis` — the target-agnostic front end.
**Audience:** a future session picking up the REMAINING work (§4) cold. Self-contained.

> This is an **ephemeral plan doc** (per repo convention). The bulk of the original
> plan (Tier 1/2/3 de-CLR-ing, the §14 capability-interface mechanism) has **landed** and
> was pruned from this doc — the CODE is the canonical record. What remains is §4. Delete
> this file — and the `platform-independence-plan` references in code comments — once §4 lands.
>
> **Before deleting, migrate the durable invariants per §6** — each one becomes correct-by-construction
> in the type system, or (where that is impossible) a sited comment and/or test assertion. Nothing
> durable should rely on this doc surviving.

---

## 1. Status — what has landed

The front end is target-neutral: the passes carry **zero hardcoded BCL capability identities**.

- **Tier 1 / Tier 2 — LANDED.** The CLR/BCL leaks (`IEnumerable`/`IDisposable`/`IEquatable`/
  `IComparable`/`TextWriter` recognizers, the tuple-arity cap) were isolated then made
  provider-resolved. `CapabilityIds` resolves capability identities through the provider; the
  CLR-literal fallbacks were deleted (an unnamed capability is `ValueNone`, resolve-on-use,
  never a silent CLR substitution).
- **Tier 3 (tuple arity) — DONE.** The CLR `MaxTupleArity = 7` semantic cap was *removed*, not
  target-gated: tuples are unbounded on every target; the CLR backend packs arity-8+ into nested
  `System.ValueTuple`8<…,TRest>`. Pure backend representation knowledge.
- **§14 — the capability-interface mechanism — LANDED.** A Vesper type that implements a capability
  interface emits the target protocol member (§3 has the durable detail an implementer needs):
  - JS capability dispatch is uniform `obj[<symbol>]()`: native `Symbol.iterator` / `Symbol.dispose`,
    registry `Symbol.for("vesper.equality"|"comparison"|"hash")`.
  - **Unions AND records implement interfaces front-to-back on both targets.** `List` implements
    `seq` natively → bare `for x in [1;2;3]` iterates at runtime on CLR and JS; `ListSeq` retired.
  - **`use` is interface-required** (real-F# parity) with a `[<IsByRefLike>]` ref-struct carve-out;
    JS emits/calls `obj[Symbol.dispose]()`, CLR keeps `IDisposable::Dispose`.

**Net: capabilities *function* today.** §4 is the remaining *authoring + cleanup* sprint — making
`disposable`/`equatable`/`comparable` implementable in BCL-free source and retiring the residual
BCL-spelling hacks — not net-new capability behavior.

---

## 2. The target-neutral model (durable context)

`XParsec.FSharp.SemanticAnalysis` takes the F# CST, infers types, and freezes a TAST that *either*
backend (`Codegen.Clr`, `Codegen.Js`) lowers. Per-target representation rides the **provider**
(`IExternalSymbolProvider`), fed by per-target `.fs` intrinsic bindings
(`type int = (# "System.Int32" #)`) and the neutral `.fsi` contract — never hardcoded in the passes.
`PlatformTypes.fs` reads representability *off the provider*, knowing nothing about any backend.

**Non-goals (durable decisions):**
- **No capability predicates on the provider.** It is a mechanical metadata/contract oracle (members,
  interfaces, repr). A capability verdict is a language-semantics judgment; derive it in the passes
  from the raw facts (`FrozenInterfaces`, `Members`, `TryLookupMember`). The provider's *data* may be
  augmented (the `.fsi` publishes capability interfaces); the provider *interface* is not.
- **Codegen owns the target dialect.** The CLR `IDisposable::Dispose` slot, the JS `Symbol.dispose` /
  `Symbol.iterator` lowering. The front end classifies the capability **neutrally** (which interface
  is implemented); the backend owns the dialect — the freeze-no-backend-knowledge invariant.

**The four capabilities split along F#'s own grammar** (this decides how each is made neutral):
- **`equality` / `comparison`** are first-class *named structural constraints*
  (`SemanticConstraintKind.Equality`/`.Comparison`) — the constraint side is already target-neutral.
  The only CLR leak was the separate FS0378 nominal `IEquatable`/`IComparable` **validation**, now
  identity-resolved per target.
- **iteration / disposal** have **no** constraint keyword — F# models them as a subtype constraint
  against a nominal interface plus duck-typing at the `for` / `use` site. They get a **short interface
  alias** (`seq` exists; `disposable` added), satisfied structurally or via `:>` coercion. **No new
  `iteration`/`disposal` constraint keywords.**

---

## 3. Background mechanisms §4 builds on

All file:line anchors verified against the code as of this writing.

- **`CapabilityIds` (`PassContext`).** Holds, per capability, a `CapabilityIdentity voption` resolved
  from the provider by `resolveCapabilities` (`ExternalSymbols.fs:710-728`). `Enumerable` resolves off
  the `seq` abbreviation head; `disposable`/`equatable`/`comparable` currently off the `Intrinsic`
  platform face. Recognizers read `caps.*`; `ValueNone` (capability unnamed) surfaces as a non-match
  or an honest FS0378, never a CLR substitution.
- **`extern with` (the contract surface).** `type X = extern with <member/interface decls>` lets the
  `.fsi` publish the capability-relevant member/interface surface target-neutrally. The extractor arm
  `extractBodiedClassLike` (`VesperLib.fs:1417`) populates `ExternalClassShape.Members` /
  `FrozenInterfaces` for a **non-intrinsic** extern.
- **The intrinsic-repr reverse-canon (the `exn === System.Exception` mechanism).** `(# "<platform>" #)`
  type reprs are harvested (`harvestIntrinsicReprsInto`, `VesperLib.fs:1726-1752` — matches **only**
  `(# … #)`, not bare abbreviations) into a `{platform → canon}` reverse map
  (`TyparCapture.fs:376-384`, built by iterating **only** `ExternalTypeShape.Intrinsic` shapes);
  `canonName` (`Engine.fs:420-449`) folds an incoming BCL name back to the canon. **Guardrail:** the
  extractor is `intrinsic XOR bodied-class` and *hard-errors* on a member surface over an intrinsic
  repr (`VesperLib.fs:1435`). §4 keeps this guardrail — it is the type system correctly fencing the
  `(# … #)` repr to inert leaves.
- **§14 JS dispatch already in place.** A type implementing a capability interface already emits the JS
  protocol member, keyed on the `caps.*` identity: `Symbol.iterator` (iteration), `Symbol.dispose`
  (disposal), `Symbol.for("vesper.equality"|"comparison"|"hash")` (eq/comp/hash), via
  `partitionClassMembers` / `emitIteratorMethod` / `emitDisposeMethod` / `emitProtocolMethod`. CLR
  keeps the real BCL interface slots. §4's JS table (§4.4) *consolidates* these existing hardcodings;
  it is not new dispatch.
- **`JsNativeSymbols` fabrication.** The JS provider fabricates synthetic
  `System.IDisposable`/`IEquatable\`1`/`IComparable\`1`/`IEnumerable\`1`/`IEnumerator\`1`
  (`JsNativeSymbols.fs:262-285`, `mkErasedGenericIface` `:111-146`) so BCL-spelled JS source resolves.
  §4 retires these in favour of source-level compat abbreviations.
- **`MockBuiltins`** is the `SemanticAnalysis.Tests` provider; it must answer `caps.*` for the FS0378
  custom-eq/comp, `use`, and `for-in` goldens.

---

## 4. REMAINING WORK — capabilities as interfaces + a backend protocol table

**Status: investigated, decision-grounded design. NOT YET IMPLEMENTED.** Supersedes an earlier draft
that extended the type-level `(# … #)` repr to capabilities (`type disposable = (# "Symbol.dispose" #)`),
abandoned as a category error (§4.1). Premises are flagged `verified` / `net-new` / `verify`.

### 4.1 Why the repr-extension was dropped (the category error)

The type-level `(# "<x>" #)` repr was designed for **inert leaves** (`type int = (# "System.Int32" #)`),
carrying three implicit assumptions: (1) the type is *structurally inert* — analysis never looks inside
it; (2) resolution is *one-way w.r.t. the type system* — the canon is the unification identity, the
repr feeds only codegen emission + reverse reconciliation of incoming metadata; (3) the string names a
platform **type**. A capability violates all three: it is **not** a leaf (member surface + subtyping),
it is **bidirectional** (implementers conform *to* it; sites dispatch *through* it), and — decisively —
**`Symbol.dispose` is not a type, it is a method-dispatch key**. So `(# "System.IDisposable" #)` and
`(# "Symbol.dispose" #)` do not represent the same *kind* of thing; the string's denotation became
target-dependent, so codegen could only interpret it by *already knowing which capability it is* — at
which point the string is redundant with the capability identity. The extractor's `intrinsic XOR
bodied-class` hard-error (`VesperLib.fs:1435`), `TypeShapes` being single-valued, and the reverse-canon
builder reading **only** `Intrinsic` shapes (`TyparCapture.fs:376-384`) are the type system *correctly
refusing* this overload — not obstacles to route around.

The deeper realization: the repr-string never shrank the Codegen↔analysis seam, it **disguised** it as
source. The seam is irreducible; a typed backend table is its *honest, minimal, auditable* form. **The
`(# … #)` type-repr stays admissible only where the string names a real platform TYPE that an inert
alias defers to** — `System.Int32`, `System.Exception`, *and* the CLR capability face `System.IDisposable`
(the alias is inert at the repr level; its structure comes from the deferred BCL interface + the `.fsi`
member surface — see §4.5 Premise 1). It is **not** admissible where the string is not a type and there
is nothing to defer to — the JS `Symbol.dispose` case, which is the genuine category error and is why
JS anchors move to the backend table (§4.4). (The *expression-level* `(# … #)` — intrinsic op bodies —
is untouched.)

### 4.2 The model — three tiers

A capability decomposes into three concerns, each placed where it belongs:

1. **Identity & member surface → source, BCL-free.** Each capability is a real abstract interface in
   `capabilities.fsi`: `type disposable = extern with abstract member Dispose : unit -> unit`. A
   `Class{IsInterface=true}` carrying its own canonical identity (`disposable`, never `System.*`) and
   member surface. The neutral `.fsi` carries no `(# … #)` — the per-target platform face rides the `.fs`
   companion (§4.3), reconciled to the canonical (Premise 6), so the frozen TAST stays BCL-free.
   Conformance and `interface disposable` resolve through the ordinary nominal-interface machinery.
2. **Behavior bodies → source.** A user's impl body *is* Vesper:
   `interface disposable with member this.Dispose() = cleanup()`. The whole *content* of an impl is
   source; only the protocol *anchor* leaves source — and only on JS (tier 3).
3. **Protocol anchors → per-target linkage.** "Implements `disposable`" must lower to the target's
   native dispatch, and the anchor's *nature* differs per target — that difference is the whole design:
   - **CLR anchor = a type** (`System.IDisposable`) ⇒ expressible **in source** as a per-target
     abbreviation. **No backend table, no net-new emission.**
   - **JS anchor = a symbol** (`Symbol.dispose`, a behavior not a type) ⇒ **a small closed backend
     table**, JS-only.

So the seam is *smaller* than "N capabilities × 2 targets": **CLR stays entirely in source; only JS
carries a table of N entries** — and §14 already half-owns it (the dispatch is keyed on `caps.*`, not on
any repr string).

### 4.3 Per-target realization (the asymmetry)

The capability is **one neutral interface in the `.fsi`** carrying a member surface; each target's
identity rides its `.fs` companion. The §4.5-Premise-1 probe ruled out a `.fs` *abbreviation* (it is
inert — neither harvested nor extracted), so the CLR identity rides the existing `(# … #)` repr (a
legitimate type-repr — `System.IDisposable` IS a type), and the two faces (member surface + platform
identity) are carried by **one dual-faced shape** (§4.6 slice 1).

```fsharp
// capabilities.fsi  (neutral contract — canonical, BCL-free; the member surface both backends and
//                    conformance read, and the JS-side identity)
type disposable = extern with
    abstract member Dispose : unit -> unit

// capabilities.fs   (CLR: the platform identity, as the existing `(# … #)` repr. UNCHANGED from
//                    today's `disposable` repr; the `extern with` surface above now rides alongside it
//                    via the slice-1 dual-faced shape. `disposable` resolves to `System.IDisposable`
//                    on CLR — transparent, so impl-recording + emission use the real BCL key.)
type disposable = (# "System.IDisposable" #)

// capabilities.js.fs (JS: NO repr for the capability — the dispatch anchor `Symbol.dispose` is a method
//                     key, not a type, and lives in the JS backend table (§4.4). The BCL spelling is a
//                     JS-only forward abbreviation so ported/BCL-spelled source resolves.)
namespace System
type IDisposable = disposable
```

- **CLR:** `disposable` keeps its **canonical** key (BCL-free frozen TAST); the dual-faced shape's
  platform face makes it **reconcile** with `System.IDisposable` — the *same* mechanism as
  `exn === System.Exception`, widened to read the platform face off the capability `Class` (Premise 6).
  Incoming metadata `System.IDisposable` folds to `disposable` via the reverse-canon, so a BCL type
  implementing it matches `caps.Disposable`; outgoing, `ClrProvider.InterfaceHandleOf` consults the
  platform face to emit the real `System.IDisposable` interface row. The member surface is consulted only
  to conformance-check the user's body. The observable effect is "transparent" (either spelling works,
  emits the BCL interface), but the canonical never evaporates — see Premise 6 for why not resolve-through.
- **JS:** no metadata, so the canonical `disposable` (from the `.fsi`) *is* the real identity; the
  `System.IDisposable = disposable` forward abbreviation lets BCL-spelled source resolve to it.
  `caps.Disposable` resolves to the canonical `disposable` key. Emission + use-site lowering read the
  **backend protocol table** (§4.4), mapping `Disposable → Symbol.dispose`.

`equatable<'T>` / `comparable<'T>` are identical: `.fsi` `abstract member Equals : 'T -> bool` /
`CompareTo : 'T -> int`; CLR abbrevs `= System.IEquatable<'T>` / `= System.IComparable<'T>`; JS compat
abbrevs the reverse; JS table entries the registry symbols `Symbol.for("vesper.equality")` /
`"vesper.comparison"` (and `"vesper.hash"`). Iteration (`seq`) is the existing precedent and is **left
as-is** for now (`seq<'T>` abbreviates the enumerable head; JS dispatch is `Symbol.iterator`); §4.8
notes the optional retrofit.

### 4.4 The JS backend protocol table

The N JS anchors live in **one place in `Codegen.Js`** — the only thing that genuinely leaves source.
A small **declarative table is preferable for readability**, but it is hardcoded in the backend either
way, so the choice is convenience/perf, not architecture — and **if expressing it as a table means
inventing a type zoo for an abstraction that is just interpreted in place, a direct `match` on the
`caps.*` identity is better.** Recommended: a flat literal `(capability-identity → anchor)` list/map
keyed on the resolved `caps.*` `SymbolKey`, consumed by the existing `partitionClassMembers` /
`emit*Method` sites — *no* new abstraction layer. Adding a capability is then one row + the front-end
`caps.*` field it already requires.

| Capability | JS anchor | Kind |
|---|---|---|
| iteration | `Symbol.iterator` | native well-known (already emitted) |
| disposal | `Symbol.dispose` | native well-known (already emitted) |
| equality | `Symbol.for("vesper.equality")` | Vesper registry (already emitted) |
| comparison | `Symbol.for("vesper.comparison")` | Vesper registry (already emitted) |
| hashing | `Symbol.for("vesper.hash")` | Vesper registry (already emitted) |

Every row is **already emitted today** by §14 — this slice consolidates the scattered hardcodings into
the one table and points them at the resolved `caps.*` identities; it is not net-new dispatch.

### 4.5 Premise-by-premise feasibility

1. **CLR per-target identity via a `.fs` abbreviation — DISCONFIRMED (probed 2026-06-28). A `.fs`
   abbreviation supplies NO identity; the dual-face is real and slice 1 must resolve it.** The probe
   (test `extern-with-abstract-member extracts as an interface Class…`, `VesperLibTests.fs`) and a
   read of the extraction driver establish:
   - **The member-surface half is FREE** (empirically green): `type disposable = extern with abstract
     member Dispose : unit -> unit` extracts to a `Class{IsInterface=true}` carrying `Dispose`, *because*
     no `(# … #)` repr is present so `isIntrinsic=false` and the bodied arm (`VesperLib.fs:1417`) runs
     `extractBodiedClassLike` (which sets `isInterface = bodyIsInterface elems`, `:1257-1259`).
   - **A `.fs` plain abbreviation establishes NOTHING.** `extractSymbols` runs **only** over the
     `manifest.Files` (the `.fsi`); the `.fs` companion is touched **only** by the harvest, which matches
     **only** `Type.ILIntrinsic` i.e. `(# … #)` (`harvestIntrinsicReprsInto`, `VesperLib.fs:1747`;
     driver `ReferencedProject.fs:495-530`). So `type disposable = System.IDisposable` in the `.fs` is
     inert — no shape, no identity, no reconciliation.
   - **A `.fsi` plain abbreviation works mechanically but breaks the model.** It *is* extracted
     (`extractAbbrevBody` → `Abbrev` head, `VesperLib.fs:887-905`), but the `.fsi` is neutral (can't name
     `System.IDisposable` BCL-free) and **target-uniform** (JS would resolve the same wrong head).
   - **The dual-face is a hard XOR today.** Put the CLR repr back (`(# "System.IDisposable" #)`) to get
     the identity and `isIntrinsic=true` — which **skips** the bodied arm and **drops** the member
     surface + raises the "capability surface on an intrinsic primitive is not yet supported" hard-error
     (`VesperLib.fs:1416-1418`, `:1435`). So a capability cannot today be *both* an interface-with-members
     *and* carry a per-target identity.

   ⇒ **Slice 1 is NOT "add a `.fs` abbreviation"; it is "make ONE shape carry both the interface member
   surface (from `.fsi extern with`) and the per-target platform identity (from the `.fs` `(# … #)`)."**
   Net-new but bounded — see the revised §4.6 slice 1. The CLR `(# "System.IDisposable" #)` repr **stays**
   (it is a legitimate type-repr — `System.IDisposable` IS a platform type that an inert alias defers to;
   contrast §4.1's category error, which was *only* the JS non-type `Symbol.dispose`). JS carries no repr;
   its anchor is the backend table (§4.4).
2. **Abbrev at interface-head — CONFIRMED, conditional.** `tryResolveExternalType` expands an `Abbrev`
   before the interface-ness check (`Translate.fs:509-510`; gate `Unification.fs:843-854`), so both
   `interface disposable` and `interface System.IDisposable` work **iff** the RHS resolves to a
   `Class{IsInterface=true}` (CLR: the real BCL interface; JS: the `.fsi` abstract interface) — exactly
   satisfied by tier-1 capabilities being real interfaces (no `Intrinsic` RHS, which would expand to
   `TyConst` and fail "not an interface").
3. **`resolveCapabilities` reads the Class/abbrev head — CONFIRMED feasible.** Rewire
   `resolveCapabilities` (`ExternalSymbols.fs:710-728`) from the `Intrinsic.platform` reader to a
   Class/abbrev-head reader — **exactly how `Enumerable` already resolves off the `seq` abbrev**. On CLR
   the head is `System.IDisposable`; on JS the canonical `disposable`. Same path, target supplies the
   head.
4. **Retiring `JsNativeSymbols` fabrication — CONFIRMED feasible.** Consumers (interface-ness gate,
   `validateCustomEqCompImpls`, `partitionClassMembers`, `resolveCapabilities`) work once the
   capabilities are real `Class` shapes with JS-only `System.* = capability` compat abbrevs. (`Error`/
   `exn` is unrelated and stays.)
5. **`validatePlatformTypes` must exempt capability/interface shapes — net-new, small.** A capability
   interface has **no value representation** (you never encode a `disposable` field as a runtime value
   the way you encode an `int`), so its JS `platform = None` is *not* the encodable-leaf failure that
   check guards. The old draft minted a fake `(# "Symbol.dispose" #)` repr purely to dodge this; the
   correct fix is the exemption.
6. **CLR emission — dissolves through the SAME mechanism `exn` already uses (decided 2026-06-28).**
   "Transparent" is the *goal* (`disposable` and `System.IDisposable` interchangeable; emits the real BCL
   interface; BCL types match), but achieve it by **reconciliation, keeping the canonical primary** —
   NOT by resolve-through to the BCL key. Resolve-through would re-bake `System.*` into the frozen TAST on
   CLR, undoing the de-CLR-ing; reconciliation keeps the frozen TAST BCL-free and confines the BCL name to
   the emission boundary. This is exactly `exn === System.Exception`: `exn` keeps its key, `canonName`
   folds `System.Exception → exn` (incoming), and the platform face drives emission (outgoing). The work
   is to **widen that one mechanism** from `Intrinsic`-only to also read the platform face off the
   dual-faced capability `Class`:
   - **Incoming:** widen the reverse-canon builder (`TyparCapture.fs:376`, currently `Intrinsic`-only) to
     `Intrinsic | Class-with-platform-face`. Then `disposable` reconciles identically to `exn`.
   - **Outgoing:** `ClrProvider.InterfaceHandleOf` (`ClrProvider.fs:127-140`) consults the capability
     `Class`'s platform face to emit the `System.IDisposable` interface row — the *same* platform-face
     consult `exn` already does for type-position emission, applied at the interface-row site. Bounded.

   `exn` does NOT move — it stays an `Intrinsic` (no `.fsi` member surface; borrows `System.Exception`'s
   members from metadata) and rides the widened mechanism unchanged. The shared part is the
   reconciliation + emission, not the shape.

### 4.6 Slicing (each committable, gated). Order is dependency-forced.

1. **Dual-faced capability shape + reverse-canon widen — LANDED (the mechanism only; the real
   `capabilities.fsi` flip is slice 2).** `ExternalClassShape` gained a `CapabilityFace:
   CapabilityPlatformFace voption` (`{ Canon; Platform }`) — `ValueSome` only for a `Class` that, like
   `Intrinsic`, reconciles to a per-target platform type while ALSO publishing a member surface. The
   `VesperLib.fs` `extern` arm no longer hard-errors on a bodied intrinsic: when `isIntrinsic` **and**
   `bodyIsInterface elems`, it runs `extractBodiedClassLike` (member surface + interface-ness) then
   attaches `CapabilityFace = ValueSome { Canon = short; Platform = repr }` from `IntrinsicReprs`
   (`ValueNone` when the target omits the repr — JS — so the canonical stands). A *concrete*-member
   intrinsic (the deferred `string`/StringIntrinsics case) still errors + stays `Intrinsic`. The
   reverse-canon builder (`TyparCapture.fs`) gained a `Class { CapabilityFace = ValueSome face }` arm
   emitting `face.Platform → face.Canon`, so `disposable` reconciles by the *same* path `exn` uses;
   canonical stays primary, no resolve-through. **`capabilities.fsi`/`.fs` UNTOUCHED** (a flip would
   break `resolveCapabilities` until slice 2), so the mechanism is proven by a *synthetic* gate test.
   *Gate (green):* test `dual-faced capability interface: extern-with-abstract-member + (# … #) repr →
   Class carrying platform face + reverse-canon` (`VesperLibTests.fs`) — `Class{IsInterface=true}` with
   `Dispose` in `Members` AND `CapabilityFace = (disposable, System.IDisposable)`, and the provider's
   `IntrinsicReverseCanon` folds `System.IDisposable → disposable`. Full SemA 637 / Clr 1062 / Js 175
   green (no real type changed shape).
2. **`capabilities.fsi` flip + `resolveCapabilities` rewire (Premise 3) — COUPLED, land together.**
   Flip `capabilities.fsi` `disposable`/`equatable`/`comparable` to `extern with abstract member …`
   (keeping the CLR `.fs` `(# … #)` reprs), which turns them from `Intrinsic` → dual-faced `Class`.
   That flip **breaks** `resolveCapabilities` (`ExternalSymbols.fs:710-728`, `resolveIntrinsic` matches
   `Intrinsic`) unless rewired in the same change: `caps.{Disposable,Equatable,Comparable}` read off the
   Class's `CapabilityFace` (CLR → `System.IDisposable`; JS `CapabilityFace = ValueNone` → the canonical
   `disposable` key). Hence slice 1 deliberately did NOT touch the contract. *Gate:* `SemanticAnalysis.Tests`
   + `ReferencedProjectTests` green; `caps.*` still match user impls and metadata-sourced BCL impls on CLR.
3. **`validatePlatformTypes` interface exemption (Premise 5).** *Gate:* a JS build of a capability with
   no value repr no longer errors; `PlatformTypes` diagnostics for genuine encodable leaves unchanged.
4. **JS protocol table (§4.4).** Consolidate the §14 hardcodings into the single `caps.* → anchor`
   table; point `partitionClassMembers` / `emit*Method` at it. *Gate:* `Codegen.Js.Tests` — the
   already-green class/union/record capability emissions still pass, now table-driven.
5. **CLR canonical authoring — confirm reuse, no net-new (Premise 6).** A class/union/record writing
   `interface disposable`/`equatable<Self>` emits + dispatches the real BCL interface via the existing
   path. *Gate:* `Codegen.Clr.Tests` — canonical-spelled impls run at runtime, identical IL to the
   BCL-spelled form.
6. **JS canonical authoring + retire `JsNativeSymbols` fabrication (Premise 4).** `interface disposable`
   resolves on JS and routes through the §4.4 table to `[Symbol.dispose]`/`[Symbol.for(...)]`; remove the
   synthetic shapes (BCL-spelled fixtures now resolve via the compat abbrev). *Gate:* `Codegen.Js.Tests`
   — canonical-spelled disposable/equatable run under Node; `ClassEmitTests` still compile with no
   `JsNativeSymbols` fabrication.

**Dependency edges:** 1→{2,3,4,5,6}; 2→{5,6}; 4→6. Slice 1 (the dual-faced shape) is the gate; the
net-new pieces are the dual-faced shape + its reverse-canon read (1), the `validatePlatformTypes`
exemption (3), and — only if slice 1 does not make CLR transparent — the Premise-6 emission map (5). All
bounded. Equatable/comparable ride the same slices as disposable (no separate sprint).

### 4.7 What this retires / subsumes

- **The repr-extension draft is dropped wholesale** — no `type disposable = (# "Symbol.dispose" #)` (a
  non-type repr), and no *suppression* of the `VesperLib.fs:1435` guardrail by routing a JS symbol
  through it. (Slice 1 *does* introduce a dual-faced shape — ONE `Class` carrying a member surface AND a
  CLR platform face — but only where the repr names a real platform TYPE, §4.1; that is the guardrail
  being *refined to admit a legitimate case*, not bypassed.)
- **The structural bare-member eq/comp reframe (a prior deferral) is DROPPED** — making the capability a
  real implementable interface keeps the existing identity-keyed interface-impl dispatch
  (`partitionClassMembers` keyed on `caps.*`); re-sourcing eq/comp from a bare member is unnecessary.
- The `capabilities.js.fs` "keep `System.IDisposable` byte-spelling as the match key" hack is retired —
  JS resolves the canonical `disposable` directly.
- The `JsNativeSymbols` synthetic-interface fabrication is retired in favour of source-level compat
  abbreviations (slice 6).

### 4.8 Open sub-decisions for the implementing session

1. **Premise-1 probe — DONE (2026-06-28).** Result: a `.fs` abbreviation establishes NO identity (the
   `.fs` is harvested only for `(# … #)`, extracted never); the dual-face is a hard XOR today; the member
   surface is free only when no repr is present. ⇒ slice 1 is the dual-faced shape, not an abbreviation.
   See §4.5 Premise 1 and the characterization test `extern-with-abstract-member extracts as an interface
   Class…` (`VesperLibTests.fs`). The remaining slice-1 design choice is Premise 6 (transparency vs an
   emission map).
2. **Exact CLR platform-repr spelling.** Confirm what `MetadataSymbols` surfaces for `System.IDisposable` /
   `` IEquatable`1 `` / `` IComparable`1 `` (and whether `[<CustomComparison>]` needs both the generic
   and non-generic `IComparable`) so the CLR `(# … #)` repr matches the metadata key exactly. (The
   existing `capabilities.fs` reprs are the starting point — they already feed today's reconciliation.)
3. **`seq` retrofit — optional.** Leave iteration on its current `seq`-abbrev-head resolution, or fold
   it into the uniform tier-1 shape. No correctness pressure; decide opportunistically.
4. **MockBuiltins / fixture cost.** After tier-2 resolution, `MockBuiltins` must still answer `caps.*`.
   Enumerate the affected goldens (FS0378 custom-eq/comp, `use`, `for-in`) before flipping.

---

## 5. Other remaining / notes

- **Latent CLR ref-struct dispose gap (untriggered).** A project-local ref-struct `use` carve-out
  records a LOCAL dispose key, which the CLR `ValueSome` `ExternalMemberRef` path would fault on. TODO
  at `EmitBindings.fs`; fix = a local-vs-external branch. Not exercised today.
- **`#6 `systemObjectKey`` — left.** `obj`-as-universal-supertype is a genuine language concept; the
  recognizer fires only on the CLR metadata-reconciliation path. No correctness pressure; could ride
  the provider-resolution mechanism opportunistically.
- **Durable-doc follow-up.** `records-architecture.md` has stale spots re: record members/interfaces
  (post §14.6). Non-blocking.

---

## 6. Encoding the durable invariants on deletion

When §4 lands and this doc is deleted, each durable fact below must already live in code — **ideally
correct-by-construction in the type system; failing that, a comment and/or test assertion at the named
site.** Order of preference: a shape that *can't* be expressed wrongly > a test that *fails* if it is >
a comment that *explains* why. A comment alone is the weakest form and the last resort.

| Durable invariant | Encoding | Site |
|---|---|---|
| `(# … #)` type-repr is for **inert leaves only** (capabilities are interfaces — the category error, §4.1) | **Correct-by-construction**: the `intrinsic XOR bodied-class` guardrail already hard-errors. Add a comment stating it is *intentional* (cites the category error) + a test asserting it fires. | `VesperLib.fs:1435` + an extraction test |
| The capability set is **closed/bounded**, not an open registry | **Correct-by-construction**: `CapabilityIds` is a fixed record, not a dictionary. Comment that adding a capability is a deliberate field + table row. | `CapabilityIds` definition |
| **No capability predicates on the provider** (derive verdicts in the passes) | **Correct-by-construction**: the absence of `IsDisposable`/etc. on the interface *is* the constraint. A comment prevents re-introduction. | `IExternalSymbolProvider` |
| **Freeze carries no backend knowledge** (no BCL FQN in the frozen TAST for a capability) | **Test assertion**: frozen-TAST assertions over a `use` / `for-in` / custom-eq impl that no BCL FQN appears. | `SemanticAnalysis.Tests` |
| CLR anchor = a **type in source** (abbreviation); JS anchor = a **symbol in a backend table** (the asymmetry, §4.2–4.4) | **Correct-by-construction** on CLR (the `capabilities.fs` abbreviation is self-documenting source). JS: a comment on the table explaining *why* it can't be source (symbols aren't types) + per-target codegen tests. | `capabilities.fs` / `capabilities.js.fs`; the JS `caps.* → anchor` table; `Codegen.{Clr,Js}.Tests` |
| The reverse-canon is **one mechanism** (canonical + platform face) shared by `exn`/primitives AND the dual-faced capability `Class` — not `Intrinsic`-only (§4.5 Premise 6) | **Correct-by-construction**: the widened builder reads the platform face off `Intrinsic \| Class-with-platform-face`; a comment states capabilities ride the same `exn === System.Exception` path (canonical stays primary, no resolve-through). | `TyparCapture.fs:376` |
| `validatePlatformTypes` **exempts capability/interface shapes** (no value repr) | **Test assertion**: a capability with `platform = None` builds on JS without error. Comment on the exemption branch states why. | `PlatformTypes` exemption + a JS build test |
| The latent **CLR ref-struct local-dispose** fault (§5) | **Comment/TODO** already sited; add a test only when the path is made reachable. | `EmitBindings.fs` |

This table is itself the migration checklist: when an invariant's encoding lands, strike its row. The
doc is safe to delete only when the table is empty.
