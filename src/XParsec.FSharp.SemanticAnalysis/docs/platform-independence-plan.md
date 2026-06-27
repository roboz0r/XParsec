# Platform-independence plan: de-CLR-ing the semantic-analysis passes

**Status:** Tier 1 landed. Tier 2 (this plan) and Tier 3 are open.
**Owner seam:** `XParsec.FSharp.SemanticAnalysis` — the target-agnostic front end.
**Audience:** a future session picking up Tier 2 cold. Self-contained.

> This is an **ephemeral plan doc** (per repo convention). Delete it — and the
> `platform-independence-plan` references in any code comments — once Tier 2/3 land.

---

## 1. Goal

`XParsec.FSharp.SemanticAnalysis` is meant to be **target-agnostic**: it takes the
F# CST, infers types, and freezes a TAST that *either* backend (CLR via
`Codegen.Clr`, JS via `Codegen.Js`) lowers. Per-target representation knowledge is
supposed to ride the **provider** (`IExternalSymbolProvider`), fed by per-target
`.fs` intrinsic bindings (`type int = (# "System.Int32" #)`) and the neutral `.fsi`
contract — never hardcoded in the passes. `PlatformTypes.fs` is the reference model:
it reads representability *off the provider* and reports a clean diagnostic, knowing
nothing about any backend.

A small set of **CLR/BCL semantics leaked into the analysis logic** as hardcoded BCL
identifiers. Tier 1 isolated them; Tier 2 makes them genuinely target-neutral.

### Non-goals
- **Not redesigning the provider interface.** (See §4 — the explicit decision is *not* to
  add capability predicates to it.)
- Codegen still **owns the target dialect** — the CLR `IDisposable::Dispose` slot, the JS
  `Symbol.dispose` / `Symbol.iterator` lowering. The front-end de-CLR-ing is what this plan
  primarily owns.

> **Scope note (revised — this is no longer a pure front-end plan).** The original draft
> listed "not touching the backends" as a non-goal and proved each slice only against the
> frozen TAST. That understated the sprint. The frozen-TAST assertion proves a neutral
> identity is *present*; it cannot prove it is *sufficient* for a real backend to lower.
> So for the two **loop** capabilities (iteration, disposal) this sprint **does** land the
> JS emission arms — `for-in` → `obj[Symbol.iterator]()`, `use` → `obj[Symbol.dispose]()`
> — but as an **adjacent commit inside each slice, gated on that slice's frozen-TAST
> assertion** (§10), never woven into the front-end change itself. This keeps the two
> failure domains (front-end de-CLR vs. net-new JS codegen) independently bisectable and
> lets the front-end commit land without waiting on the JS provider. Equatable/comparable
> (§5.3) is a pure FS0378 *diagnostic* with **no emission on either target**, so it has no
> JS arm — the asymmetry is structural, not a deferral.

---

## 2. What Tier 1 did (already landed — context, do not redo)

The audit found six CLR leaks. Tier 1 addressed the four that were **hardcoded BCL
contract names used in inference logic** by consolidating every literal into
`RuntimeNames.fs` (single source, matching the existing `vesperListKey` /
`systemObjectKey` / `IStructuralFormattable` precedent) and rewiring the call sites
through recognizers. No behavior change; all 628 `SemanticAnalysis.Tests` pass; full
build green.

`RuntimeNames.fs` now owns:
- `ienumerableKey` (private) → `isIEnumerableKey`, `isIEnumerableName`
- `idisposableKey` (private) → `isIDisposableKey`, `isIDisposableName`
- `iequatableKey` / `icomparableKey` (private) → `iequatableQualifiedName`, `icomparableQualifiedName`
- `textWriterTypeName`

Each backing key is still a literal `TypeKey(Some "System.Runtime", …)`. **That literal
is the remaining leak Tier 2 removes** — the identity is CLR-specific; it should be
target-supplied.

Call sites now routed through `RuntimeNames` (all unchanged in shape — Tier 2 only
changes what backs the recognizers):
- `Passes/Unification/InferControlFlow.fs` — `for … in` enumerator resolution
  (`tryLocalInterfaceEnumerator`, `tryForInEnumerator`, the external interface-scan
  fallback) and `IDisposable` disposal scans (`probeExternalEnumerator`,
  `probeLocalEnumerator`).
- `Passes/Unification/Infer.fs` — `tryExternalDispose` (`use`-binding disposal).
- `Passes/Unification.fs` — the `[<CustomEquality>]`/`[<CustomComparison>]`
  conformance checks (`implementsSelf`, ~line 1300/1314).
- `PrintfSpec.fs` — `tyTextWriter` writer sink.

### Leaks NOT in Tier 1/2 scope
- **#5 — Tuple arity cap.** `Passes/Validation.fs` `MaxTupleArity = 7` rejects 8+
  tuples as a *semantic* error. This is a CLR ABI limit (`System.ValueTuple` tops out
  at 7); JS tuples are arrays with no cap. → **Tier 3** (§7).
- **#6 — `systemObjectKey`.** Already single-sourced + asm-blind; `obj` is a genuine
  universal-supertype language concept and the recognizer only fires on the CLR
  metadata-reconciliation path. Low priority; note in §8.

---

## 3. The four capabilities are NOT the same problem

The tell is the duck-typing already present: it tracks the **F# spec**, which
duck-types `for-in` and (C# 8) ref-struct `using`, but treats custom-equality
conformance as a *nominal* interface rule.

| Capability | Nature | F# resolution | What downstream needs |
|---|---|---|---|
| Enumerable (`for-in`) | capability for lowering | duck-typed: `GetEnumerator()` → `{ MoveNext():bool; Current:'T }`, else `IEnumerable<'T>` | the **element type `'T`** |
| Disposable (`use`/`for-in` finally) | capability for lowering | duck-typed: prefer own `Dispose():unit`, else `IDisposable` | yes/no **+ dispose slot** (codegen picks) |
| Equatable (`[<CustomEquality>]`) | **validation only** (FS0378) | nominal: must implement `IEquatable<Self>` | nothing — pure diagnostic |
| Comparable (`[<CustomComparison>]`) | **validation only** (FS0378) | nominal: must implement `IComparable<Self>` | nothing — pure diagnostic |

Note the sibling already done right: the `[<CustomEquality>] ⇒ must override
`GetHashCode()`` check (`Unification.fs` ~line 1309) is **already structural and
already marked target-agnostic** (`m.Name = "GetHashCode" && m.IsOverride`). That's
the template for the equatable/comparable end state.

### 3.1 How these map onto F#'s own capability grammar (decided)

The four capabilities are not one mechanism — they split along F#'s own grammar, and
that split decides how each is made target-neutral:

- **Named structural constraints — `equality`, `comparison`.** Already first-class:
  `SemanticConstraintKind.Equality` / `.Comparison` (`SemanticInfo.fs:682-683`), parsed
  from `when 'T : equality/comparison` (`TypeTranslate.fs:325-331`). **The constraint
  kind carries no BCL identity — the front-end constraint side is already target-neutral.**
  The CLR leak is *only* in the separate FS0378 **validation** (`Unification.fs:1300/1314`),
  which checks nominal `IEquatable`/`IComparable`. **Decision: keep that check nominal,
  but resolve its identity per target via §5.0** (the equatable/comparable rows stay in
  §5.0 — four identities, not two). The structural reframe (§5.3b) remains a deferred
  follow-up, not this plan.
- **Interface-alias capabilities — iteration, disposal.** F# has **no** `iteration` /
  `disposal` constraint keyword; it models these as a *subtype constraint* against a
  nominal interface (`when 'T :> seq<_>` / `:> IDisposable` → `SemanticConstraintKind.Coercion`)
  plus duck-typing at the `for` / `use` site. **Decision (F#-parity): do NOT invent
  `iteration`/`disposal` constraint kinds.** Instead give each a **short interface alias**
  the way `seq` already aliases the iterable interface — `seq` exists; **add a `disposable`
  alias.** The alias is the `:>` coercion target *and* the §5.0 per-target resolution
  anchor for the use-site fallback. Satisfaction stays structural (the duck-typed probe);
  the alias is the nominal advertisement, never a new keyword.

This is why the §3 table's "nature" column matters: constraints are predicates the front
end already owns neutrally; the alias capabilities need a per-target *identity*, which is
exactly what §5.0 + the `.fsi` names (§6) + `extern with` (§12) supply.

---

## 4. Design decision: **no `IsEnumerable/IsDisposable/…` predicates on the provider**

Considered and rejected. Derive the verdicts in semantic analysis instead.

Why not provider predicates:
1. **Wrong layer.** The provider is a mechanical metadata/contract oracle (members,
   interfaces, repr). A capability verdict is a *language-semantics* judgment; baking
   it in forces every provider implementation (`MetadataSymbols` / `JsNativeSymbols` /
   the `.fsi` contract layer) to re-implement the same rule, and hides it from the
   passes where it belongs.
2. **Closed set.** One abstract method per capability freezes the list into the
   provider interface; the next capability (`IFormattable`, awaitable, pattern-`using`)
   needs another method on every implementation.
3. **Raw facts already exposed.** `ExternalClassShape.FrozenInterfaces`,
   `.Members`, and `TryLookupMember` are exactly the primitives needed to derive each
   verdict. The provider's job is done; derivation is upstream.

The one legitimate "augmentation" is to the **contract *data*** (have the `.fsi`
publish the capability interfaces / member sigs so the contract layer can answer at
all) — **not** to the provider *interface* with semantic predicates.

### The fact that drives the mechanism
`ExternalClassShape` (`ExternalSymbols.fs:337`):
- `Members` — methods/properties, **but contract-layer providers leave this empty**
  until the `.fsi` extractor publishes member sigs.
- `FrozenInterfaces` — directly-implemented interfaces, which the `.fsi` *does*
  reliably commit to.

So **structural member probing is the rich/fast path on the metadata (CLR) layer, but
not always available on the contract (JS) layer**. Interface-implementation via the
contract is the reliable cross-target seam. ⇒ keep **both**: structural preferred,
contract-sourced interface identity as backstop. Never a hardcoded CLR FQN.

---

## 5. Tier 2 — per-capability plan

### 5.0 Shared prerequisite: provider-resolved identities in `RuntimeNames`

Replace the four literal `TypeKey(Some "System.Runtime", …)` constants with identities
**resolved through the provider** from a canonical language-level name, so CLR's
provider maps to `System.IEquatable` while the JS provider maps to its own/erased
identity. The recognizers and every Tier-1 call site stay identical — only the backing
identity changes.

**Wrinkle:** `RuntimeNames` is a stateless module with no `PassContext`/provider in
scope. Options:
- **(A) Thread the provider.** Change the recognizers to
  `isIEnumerableKey (provider) (k)`. Most explicit; touches the eight call sites below.
- **(B) Memoized resolver.** A `provider -> ResolvedIdentities` cache (the
  `ExternalSymbols` layer already uses `ConditionalWeakTable`/`ConcurrentDictionary`
  keyed on provider identity — mirror that). Recognizers take the resolved record.
- **(C) Resolve once at pipeline entry.** `Pipeline.fs` resolves the capability keys
  from the provider and stashes them on `PassContext`; recognizers read
  `ctx.CapabilityIds`. Cleanest threading, one resolution per compilation.

**Recommended: (C).** One resolution site, no per-call cost, no global cache lifetime
questions, and `PassContext` is already the carrier for cross-pass facts. Add a
`CapabilityIds` field resolved from canonical names via `ctx.Provider.TryLookupType`.

**The record must carry two projections per capability, not one `SymbolKey`.** The
consumers split: some compare a `SymbolKey`, others compare a *rendered* qualified
interface name (the strings that come back from `ExternalSymbols.instantiateInterfaces`).
So `CapabilityIds` holds, per capability, both the `SymbolKey` *and* its
`SymbolKeyOps.qualifiedName`. The exact eight consumer sites (audited; all have `ctx`
in scope) and which projection each needs:

| Site | Recognizer | Projection |
|---|---|---|
| `InferControlFlow.fs:62` | `isIDisposableName` | rendered string |
| `InferControlFlow.fs:121` | `isIDisposableKey` | `SymbolKey` |
| `InferControlFlow.fs:448` | `isIEnumerableKey` | `SymbolKey` |
| `InferControlFlow.fs:579` | `isIEnumerableKey` | `SymbolKey` |
| `InferControlFlow.fs:599` | `isIEnumerableName` | rendered string |
| `Infer.fs:130` | `isIDisposableName` | rendered string |
| `Unification.fs:1300` | `iequatableQualifiedName` (`implementsSelf`) | rendered string |
| `Unification.fs:1314` | `icomparableQualifiedName` (`implementsSelf`) | rendered string |

Plus one **producer**, not a recognizer: `PrintfSpec.fs:55` *mints* the writer sink
`TyConst(textWriterTypeName, …)`. It needs the resolved sink identity to construct
with, so it reads `CapabilityIds` too (or stays CLR-only until a JS printf sink exists).

**The CLR-literal fallback is temporary scaffolding, not the end state.** During the
migration the resolver falls back to today's literals when the provider has no entry
(keeps CLR green before §6/§12 land). But the literals are *the leak* (§2) — the
terminal step **§5.4 deletes them** so a provider that fails to name a capability is a
compiler error, never a silent CLR substitution. See §5.4.

Canonical language-level names to resolve (must exist in the Vesper core `.fsi`; see
§6 dependency): the enumerable interface / `seq<'T>` (already concrete per the fsi
extraction work — see memory `project_fsi_extraction_resolves_bcl`), a disposable
interface, equatable/comparable interfaces.

### 5.1 Enumerable — make structural the primary, contract-identity the fallback

`for-in` already prefers a pattern `GetEnumerator()`; the interface identity is only
needed to (a) pull `'T` when a value is typed *as* the bare enumerable, or (b) handle a
contract-layer type that publishes interfaces but no members.

Steps:
1. Keep `probeExternalEnumerator` / `probeLocalEnumerator` (structural) as the
   preferred path — no change.
2. The interface-only fallbacks (`tryLocalInterfaceEnumerator`, the
   `tryForInEnumerator` direct-interface arm, the external interface scan) keep using
   `isIEnumerableKey` / `isIEnumerableName`, now backed by the resolved identity (§5.0).
3. *Optional hardening:* generalize the fallback to probe the matched interface type's
   own `GetEnumerator`/`Current` members (so element-type extraction is uniform and the
   interface identity becomes purely a "which interface to look through" hint). Defer
   unless contract-layer `seq` element extraction needs it.

Artifact: element `'T` — already extracted from `Current` (structural) or interface
args (fallback). No change to the artifact path.

### 5.2 Disposable — verdict is structural; slot is codegen's

The verdict the front end records is "needs disposing in a finally." F# (and the
existing code) already prefer the type's own `Dispose` member; the interface scan is
the backstop.

Steps:
1. `tryExternalDispose` (`Infer.fs`) already prefers `TryLookupMember(name,"Dispose")`
   — structural, target-neutral. Keep.
2. The interface scans (`isIDisposableName` / `isIDisposableKey`) back the "is it
   disposable" booleans; now resolved via §5.0.
3. **Decouple the slot from the verdict — `use` leaks across Freeze; `for-in` does not
   (TRACED).** The two constructs carry disposal differently:
   - **`for-in` is already correct.** The frozen `ForInEnumerator.Pattern` carries
     `dispose: bool` only (`SideTypes.fs:161`); the `System.IDisposable::Dispose` slot is
     minted **inside the CLR backend** (`EmitLoops.mintDisposeHandle`, `EmitLoops.fs:86-90`),
     never crossing Freeze. A JS backend reads the bool and synthesizes its own. **This is
     the template.**
   - **`use` leaks.** The frozen `TExpr.Use` carries `dispose: SymbolKey voption`
     (`Tast.fs:183`). For an external binder with no *own* `Dispose`, `ValueSome key` is
     the hardcoded `MemberKey(TypeKey(None,"System","IDisposable"),"Dispose",…)` minted at
     **`Infer.fs:132`** — a CLR interface-slot identity that *does* cross Freeze. The CLR
     backend consumes it via `Provider.ExternalMemberRef(key,…)` (`EmitBindings.fs:129-144`).
   - **Latent, not active:** the JS emitter has **no `Use` or `ForIn` arm** yet (`EmitJs.fs`),
     so nothing breaks today — but JS `use` emission would be handed a `System.IDisposable`
     key it can't honor. ⇒ **`Infer.fs:132` must change** (answers prior open-question #4).
   - **Fix (mirror `for-in`):** carry the structural *own*-`Dispose` member key when the
     type has one (already neutral — it's the type's own member, target-resolvable);
     otherwise carry a **neutral "dispose via the `disposable` capability" marker** (the
     §3.1 alias, §5.0-resolved) that each backend lowers to its own slot — *not* the BCL
     interface FQN. This is slice-4's first task, before any JS `use` test.
   - **Representation is unchanged — `TExpr.Use.dispose` stays `SymbolKey voption`
     (`Tast.fs:183`).** The marker is *still a `SymbolKey`* (the provider-resolved
     `disposable`-alias `Dispose` member key); only the *source* of the minted key changes,
     from a literal to a §5.0 resolution. On CLR the resolved key is structurally the same
     `System.IDisposable::Dispose`, so the CLR backend (`EmitBindings.fs:129-144`) is
     **untouched** — no new `Use.dispose` DU, no Tast change rippling into both backends. On
     JS the same slot resolves through the JS provider to `Symbol.dispose`; the JS `Use` arm
     (slice 4's adjacent commit, §10) lowers `ValueSome key` to `obj[Symbol.dispose]()` and
     a `ValueSome own-member` to a direct call — the same own-preferred / capability-fallback
     two-tier shape as CLR.
4. Honor the existing TODO (ref-struct pattern-`Dispose` on a non-`IDisposable`
   `[<IsByRefLike>]` type) only if it falls out naturally; otherwise leave the TODO.

### 5.3 Equatable / Comparable — validation rule, two options

These consume nothing downstream — pure FS0378 diagnostics in `Unification.fs`.

- **Option (a) — keep nominal, contract-resolved identity.** Smallest change:
  `implementsSelf` keeps comparing against `iequatableQualifiedName` /
  `icomparableQualifiedName`, now derived from the provider-resolved key (§5.0). The
  rule stays "implements the interface"; CLR maps it to `System.IEquatable`, JS to its
  own. Low risk.
- **Option (b) — reframe structurally (target-honest end state).** Match the adjacent
  `GetHashCode` check: require the type to *author* the equality/comparison **member**
  (an `Equals`-shaped / `CompareTo`-shaped member on `info.Members`) rather than
  implement a BCL interface. More faithful on a JS target where custom equality is
  shape-based (memory `project_js_step6_equality_hashing`,
  `project_js_interface_impls_attached_members`). Changes the diagnostic's precise
  trigger; needs a test sweep of the FS0378 goldens.

**Recommended:** ship **(a)** with §5.0 first (mechanical, safe), then evaluate **(b)**
as a follow-up once the JS custom-eq/comp dispatch path (currently blocked per
`project_js_interface_impls_attached_members`) is unblocked — they're the same design
question and should be decided together.

### 5.4 Terminal step: delete the literals — no silent fallback

The four `TypeKey(Some "System.Runtime", …)` constants (`RuntimeNames.fs:126-143`) are
the actual remaining leak (§2). The CLR-literal fallback in §5.0 is a migration crutch;
this step removes it so the passes contain **zero hardcoded BCL identities** and an
unnamed capability fails loudly instead of silently resolving to a CLR identity (the
exact "JS path silently falls back" hazard §6 warns about, turned into a hard error).

- **`extern with` (§12) is a hard prerequisite for this step, not an optional follow-on.**
  Deleting the literals requires the CLR `.fsi` itself to *name* disposable/equatable/
  comparable and resolve them through the CLR provider — and that naming **is** `extern with`
  work (§12.4: declaring the capability interfaces as `extern with …`). The §5.0 CLR-literal
  fallback keeps CLR green only *until* this step; the moment the literals are deleted, the
  CLR path depends on the same contract surface the JS path does. So §12 lands inside this
  sprint (it is independently testable via the parser golden files + frozen-tree assertions,
  ahead of any backend), and §5.4 cannot complete before it.
- **Runs last**, after §6 (core `.fsi` names all four) *and* after each target's
  `.fs`/provider resolves them — so deleting the literals can't regress CLR.
- **CLR stops being special-cased.** Its `.fsi` names the capabilities too, resolved
  through the CLR provider to the BCL identities. The existing 628 tests then exercise
  the *same* provider-resolved path JS will, instead of bypassing it through a literal —
  this is what makes §5.0 verifiable before the JS tests exist (closes the original
  review concern that §5.0–5.3 land with no coverage until the very end).
- **Resolve-on-use, hard diagnostic — not eager-at-entry.** Fire the error only when a
  capability is actually exercised (a `use` whose source has no resolvable disposable
  identity → diagnostic *at that site*). Eager mandatory resolution would force *every*
  compilation — including minimal `MockBuiltins` fixtures that never touch
  `for-in`/`use`/custom-eq — to declare all four or fail.
- **Cost to budget:** any test/real provider that *does* exercise a capability path
  (`MockBuiltins`, the Clr/Js test providers) must now declare that capability surface
  or those specific tests start erroring. This is the real price of removing the crutch;
  it surfaces exactly the providers that were relying on the implicit CLR identity.

---

## 6. This plan owns naming the capabilities in the Vesper core `.fsi`

§5.0 resolves identities from canonical language-level names, which presupposes the
contract declares them. **This plan owns creating them** (resolving the prior open
question — not a separate contract change). Per §3.1's split, the names to add:

- **iteration → `seq<'T>`** — already present (`seq.fsi` declares `seq<'T> ≡
  IEnumerable<'T>`; resolves concretely today). No new name.
- **disposal → a new short `disposable` interface alias** — the §3.1 decision. The `:>`
  coercion target and the §5.0 resolution anchor for `use` / `for-in` finally.
- **equatable / comparable interfaces** — needed because §3.1 keeps the FS0378 check
  nominal-but-§5.0-resolved (the equatable/comparable rows stay in §5.0). Declare the
  interface identities so the resolver has a canonical name; a short `equatable` /
  `comparable` alias is optional sugar, consistent with `seq` / `disposable`.

No new `iteration` / `disposal` constraint *keywords* (§3.1, F#-parity) — these are
interface aliases, satisfied structurally or via `:>` coercion, not constraint kinds.

If a name is absent, §5.0's temporary CLR-literal fallback keeps CLR green, so this can
land incrementally (CLR first, JS as each `.fsi` name appears) — until §5.4 deletes the
fallback. **Sequence the `.fsi` additions before flipping each recognizer to
provider-resolved**, or the JS path silently falls back to the CLR literal (which won't
match JS-surfaced identities).

**How the `.fsi` expresses these capabilities is the subject of §12** (`extern with`
member decls). §6 is the *what* (which names); §12 is the *mechanism* (how an extern
type publishes the member/interface surface). They land together.

---

## 7. Tier 3 — move tuple-arity out of semantic validation (#5)

`Passes/Validation.fs` `MaxTupleArity = 7` + `checkTuplePat`/`tupleArityError` reject
8+ tuples as a language error. This is a CLR ABI limit, not a language rule.

Plan: fold it into the `PlatformTypes.fs` representability model — let the **compiling
target's provider** declare its max tuple arity (CLR = 7, JS = unbounded) and emit the
diagnostic from there (or from a provider capability flag), gated on target. Until
then the current message is honest but wrongly rejects valid JS programs. Independent
of Tier 2; can be done any time.

---

## 8. #6 `systemObjectKey` — note, likely leave

`RuntimeNames.systemObjectKey = TypeKey(Some "System.Runtime","System","Object")`.
Already single-sourced, asm-blind, fires only on the CLR metadata-reconciliation path
(JS `obj` arrives as `TyConst "obj"` → `unknown` repr). `obj`-as-universal-supertype is
a genuine language concept. Lowest priority; if §5.0 lands a provider-resolution
mechanism, `systemObjectKey` *could* ride it for consistency, but there's no
correctness pressure. Decide opportunistically.

Also: the synthesized `MemberKey(TypeKey(None,"System","IDisposable"),"Dispose",…)` in
`Infer.fs:132` is a producer mint (asm-blind `None`) Tier 1 left untouched. **The §5.2
trace confirms it DOES cross Freeze** (via `TExpr.Use.dispose: SymbolKey voption`,
`ValueSome` = this key) and is consumed by the CLR backend at `EmitBindings.fs:129-144`.
It is **no longer optional to revisit** — §5.2 step 3 replaces it with the structural
own-`Dispose` key or the neutral `disposable`-capability marker. (`for-in` never had
this problem — it carries `dispose: bool` and the slot is backend-minted.)

---

## 9. Test strategy

- **Baseline:** `./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.SemanticAnalysis.Tests"`
  (628 pass, 1 skip today). Must stay green after §5.0 + 5.1 + 5.2 + 5.3(a) — those are
  behavior-preserving on CLR. **§5.4 also keeps it green** (CLR flips to provider-resolved,
  which returns the same identities) — but watch for fixtures whose provider must now
  declare a capability surface it previously got from the literal (§5.4 cost note).
- **CLR regression:** full build + `Codegen.Clr.Tests` (`for-in`, `use`, custom-eq/comp
  paths are exercised there).
- **Two-layer JS proof per loop slice (iteration, disposal).** `EmitJs.fs` has **no `ForIn`
  and no `Use` arm** today (it handles `While` / `ForTo` only) and no enumerator emission, so
  the proof is staged across two commits within the slice (§10):
  - **Layer 1 — frozen-TAST assertion (the slice gate).** Assert the *frozen TAST* carries a
    neutral identity — the §5.0-resolved capability key / `dispose` marker, no CLR FQN —
    without running JS codegen. This is what **gates** the front-end commit: it proves the
    de-CLR directly and is the green light for the adjacent JS commit. Proves *presence*.
  - **Layer 2 — real JS codegen (adjacent commit, gated on Layer 1).** Build the JS
    `ForIn` / `Use` arm (`obj[Symbol.iterator]()` / `obj[Symbol.dispose]()`), then assert
    real emitted JS over a JS iterable / JS disposable. Proves *sufficiency* — that the
    neutral tree Layer 1 froze is actually lowerable. **PREREQUISITE: the `tsc`-extracted JS
    symbol provider** (§12.2) must resolve the iterable / `disposable` alias to the JS
    identity, or `EmitJs.validatePlatformTypes` falls back to a BCL type — so the JS provider
    is an explicit dependency of these Layer-2 commits (§10 commits 5/7), not "delivered
    later." **`try`/`finally` is absent (CONFIRMED — not an open check):** `JsStatement`
    (`JsAst.fs:90-120`) has only `While` / `For` / `Throw`; there is **no Try/Catch/Finally
    node** and `JsPrint.fs` renders none (Step 8 added `Throw` but no `try` form). So the JS
    `use` arm (commit 7) is the heaviest in the sprint — it must add a `TryFinally` node to
    `JsStatement`, render it in `JsPrint`, *then* lower `TExpr.Use` to
    `try { … } finally { obj[Symbol.dispose]() }`.
  - **Equatable/comparable has no JS arm.** §5.3 is a pure FS0378 *conformance diagnostic* —
    no emission on either target — so its only proof is the front-end diagnostic test (Layer
    1 only). The JS custom-eq/comp *dispatch* path is separately blocked
    (`project_js_interface_impls_attached_members`) and is out of scope regardless.
- **Tier 3:** a golden that an 8-tuple compiles on JS and errors on CLR.

---

## 10. Sequencing — a multi-commit sprint, each commit with its own test gate

This **cannot land in one commit** — it spans two assemblies (`XParsec.FSharp` parser/AST
and `XParsec.FSharp.SemanticAnalysis`), adds gated grammar, changes inference, and grows two
JS backend arms. The discipline below is therefore the load-bearing part of the plan:
**every commit names the test that must be green before the next starts.** Build the shared
plumbing once, then drive **one capability at a time end-to-end** (identity → recognizer →
`.fsi`/`.fs` → CLR test → frozen-TAST gate → JS arm).

Two test infrastructures carry this, and the split is what makes the sprint bisectable:
- **Frozen-TAST / parser-golden assertions** — in-seam, backend-free. They gate every
  front-end commit and prove *presence* of the neutral identity. The `extern with` parser
  work is provable here alone (parser golden files), independent of any backend.
- **`Codegen.Clr.Tests` / `Codegen.Js.Tests`** — real emission. CLR regression guards every
  commit; the JS arm (its own commit per loop slice) proves *sufficiency*.

> **Per-slice proof shape:** front-end commit gates on its frozen-TAST/diagnostic assertion;
> the JS emission arm is an **adjacent commit gated on that assertion**, never the same
> change (§1 scope note, §9). Slice 5 (equatable/comparable) is diagnostic-only — **no JS
> arm**.

### Progress (live)

- **LANDED:** commit 1 (`2a855be5` `CapabilityIds`), commit 2 (`b81e5dbf` `extern with` parser),
  commit 3a (`7a8c6d47` `extern with` extractor), commit 6 (`use` disposal de-CLR).
- **Commits 4 (enumerable) and 8 (equatable/comparable) are SATISFIED by commit 1 — no new
  code.** Commit 1 routed *all four* recognizers through `CapabilityIds`, not just "the
  mechanism." The `for-in` frozen tree carries no hardcoded BCL identity (the `Interface` case
  has no key; `Pattern`'s `ConstrainedInterface` carries the *user's* constraint interface, not
  `IEnumerable`), and the FS0378 check reads `CapabilityIds` directly. Their gates — the 1052
  `Codegen.Clr.Tests` `ForInTests` and the FS0378 goldens in the 629 `SemanticAnalysis.Tests` —
  are green. So the only substantive front-end leak was commit 6 (`Infer.fs:132`), now fixed.
- **JS arms (commits 5, 7) DEFERRED to a separate follow-on effort** (decided). `JsNativeSymbols`
  is a hand-authored stub (only `Error`), the `tsc`-extracted JS provider doesn't exist, and
  `EmitJs` has no `ForIn`/`Use` arm or `TryFinally` node — building all three is net-new
  backend+provider work out of this sprint's scope (per §9's "its own tracked effort").
- **REMAINING in this sprint:** 3b (declare the capabilities in `Vesper.Core` `.fsi` + per-target
  `.fs`) and 9 (delete the four literals, provider-resolve `CapabilityIds`). Architectural +
  shipping-contract; investigated and designed in **§13 (NEXT SESSION STARTS HERE)** — the CLR
  premise is confirmed: it rides the `exn === System.Exception` intrinsic-repr reconciliation,
  which already generalizes to interfaces. §13 has the concrete file changes, the resolver
  relocation, the gates, and the three open sub-decisions.

### Commit-by-commit

1. **§5.0 mechanism (C).** Add `PassContext.CapabilityIds` (both `SymbolKey` *and* rendered
   `qualifiedName` per capability), resolve in `Pipeline.fs` with the *temporary* CLR-literal
   fallback, repoint the eight recognizer sites + the `PrintfSpec` producer at it.
   **Gate:** the 628 `SemanticAnalysis.Tests` stay green (behavior-preserving — same
   identities, new source) + full build. *Shared foundation; no JS behavior yet.*
2. **`extern with` parser + AST (`XParsec.FSharp`), own commit. — LANDED (`b81e5dbf`).**
   Optional `members: TypeExtensionElementsSignature voption` field on `TypeSignature.Extern`;
   the `KWExtern` arm parses the trailing `with` block via the verbatim record-arm
   `opt (choiceL [parse; parseLight])`; `AstTraversal` walks it only when `ValueSome`.
   **GATE DECISION (changed from the §12.4 draft): parsed UNCONDITIONALLY — no `ParseState`
   feature flag.** The golden-test harness parses every `.fsi` fixture through a fixed
   `ParseState`, so a parse-time flag can't be toggled per-fixture; and the construct only
   appears in toolchain-controlled `.fsi`. Per §12.4's sanctioned fallback, the **opt-in gate
   moves to the extractor (commit 3)**. **Gate met:** new golden fixture
   `sig_16_extern_with.fsi` (bare extern + `extern with interface … member …`); parser suite
   1418 pass; no existing snapshot shifted; SemanticAnalysis 628 unchanged. Backend-free.
3. **SPLIT into 3a (extractor mechanism, now) and 3b (core `.fsi` capability declarations,
   deferred to just before commit 9).** Rationale: commit 1's CLR-literal fallback already
   resolves all four capabilities, so the CLR front-end slices (4/6/8) need NOTHING from the
   core `.fsi` — only the literal-deletion (commit 9) and the JS slices (5/7) do. And the
   gate is dropped entirely (decided: the extractor always consumes; the construct is
   toolchain-only). So:
   - **3a — `extern with` extractor arm (LANDED/now).** Consume the new AST `members` field:
     when present and the type resolves to the `Class`/interface branch, register the deferred
     class body (so `finalizeDeferred` fills `FrozenInterfaces`, and interface `Members`) and
     reuse `extractTypeMembers` (members → `ctx.TypeMembers`, served via `TryLookupMember`) —
     mirroring the `Anon`/`Class` arm. The `Intrinsic`-carrying-members case (`string = extern
     with member`, the `StringIntrinsics` migration, §12.3) is **explicitly deferred** — left
     as a TODO. **No `Vesper.Core` change.** **Gate:** a new extraction test on an in-test
     `extern with interface IBar member M` fixture asserts a `Class` shape with `IBar` in
     `FrozenInterfaces`; 628 + parser 1418 green.
   - **3b — declare the capability interfaces in `Vesper.Core` `.fsi` + per-target `.fs`
     (DEFERRED, lands right before commit 9).** The high-stakes shipping-contract decision:
     whether `disposable`/`equatable`/`comparable` are abbreviations (à la `seq<'T> ≡
     IEnumerable<'T>`, declared in `Vesper.List/list.fsi`, NOT a new keyword), `extern with`
     aliases, or distinct interfaces, and how each maps per target (CLR → `System.IDisposable`
     etc.; JS → `Symbol.dispose`). Designed when commit 9 / the JS slices actually need it.
     (`seq` already exists; Vesper.Core declares its own interfaces today only as
     abstract-member bodies — `IFormatSink`, `IStructuralFormattable` — never as BCL
     re-exports, so this needs deliberate design, not a guess now.)
4. **Slice — Enumerable, front-end (§5.1).** Resolve the iterable identity via `CapabilityIds`;
   confirm structural-preferred still holds; the interface-only fallbacks read the resolved
   id. **Gate:** `SemanticAnalysis.Tests` + `Codegen.Clr.Tests` (`for-in`) green **and** a
   frozen-TAST assertion that a `for-in` source freezes a neutral iterable identity (no CLR
   FQN). This assertion is the green light for commit 5.
5. **Slice — Enumerable, JS arm (adjacent, gated on commit 4).** Build the `EmitJs` `ForIn`
   arm → `obj[Symbol.iterator]()` / `.next()` / `{value,done}`. **Prereq:** `tsc` JS provider
   resolves the iterable identity (§9). **Gate:** `Codegen.Js.Tests` `for-in`-over-JS-iterable.
6. **Slice — Disposable, front-end (§5.2).** First task is the **fix** (already traced, not an
   investigation): reroute `Infer.fs:132` off the hardcoded `System.IDisposable::Dispose`
   `MemberKey` to the structural own-`Dispose` key or the §5.0-resolved `disposable` marker —
   `TExpr.Use.dispose` stays `SymbolKey voption` (§5.2 step 3). **Gate:** `SemanticAnalysis`
   + `Codegen.Clr.Tests` (`use`) green **and** a frozen-TAST assertion that an external `use`
   binder freezes a neutral dispose key (no `System.IDisposable` FQN).
7. **Slice — Disposable, JS arm (adjacent, gated on commit 6) — heaviest commit.** `try`/
   `finally` is **confirmed absent** (`JsStatement` has no Try node, §9), so this is three
   layers: (a) add a `TryFinally` node to `JsAst.JsStatement`; (b) render it in `JsPrint`;
   (c) lower `TExpr.Use` → own-member call else `obj[Symbol.dispose]()`, wrapped
   `try { … } finally { … }`. **Prereq:** `tsc` provider resolves the `disposable` alias to
   `Symbol.dispose`. **Gate:** `Codegen.Js.Tests` `use`-over-JS-disposable. *(Consider splitting
   the `TryFinally` AST/print node into its own sub-commit if it lands independently testable.)*
8. **Slice — Equatable/Comparable (§5.3a), front-end only — no JS arm.** `implementsSelf`
   compares against the §5.0-resolved equatable/comparable identity. **Gate:** the
   `[<CustomEquality>]` / `[<CustomComparison>]` FS0378 goldens (pure diagnostic, both
   targets) + 628 green.
9. **§5.4 terminal — delete the four literals.** Now safe: commit 3 named the capabilities in
   the CLR `.fsi`, commits 4/6/8 route through them. Remove the `TypeKey(Some "System.Runtime",
   …)` constants; switch to resolve-on-use hard diagnostics; update `MockBuiltins` / test
   providers that must now declare the surface (§5.4 cost note). **Gate:** the 628 tests now
   exercise the *resolved* path, not the literal — green proves §5.0 end-to-end on CLR.
10. **§5.3(b)** — revisit the structural reframe alongside the JS custom-eq/comp unblock.
    Out of this sprint; tracked separately.
11. **Tier 3** — tuple arity, independently (§7).

**Dependency edges that must not be reordered:** 2→3 (extractor needs the AST field); 3→9
(literal deletion needs the CLR `.fsi` names); 4→5 and 6→7 (JS arm gated on its slice's
frozen-TAST assertion); {3,4,6,8}→9 (every capability routes through `CapabilityIds` before
the fallback is removed). Commits 5/7 additionally block on the `tsc` JS provider.

---

## 11. Open questions for review

Still open: *(none — the disposable-slot question below is now traced and answered.)*

### Resolved by tracing (§5.2 step 3)
- **Does Freeze hand a CLR interface slot?** **`use`: yes** — `TExpr.Use.dispose:
  SymbolKey voption` carries the `System.IDisposable::Dispose` key from `Infer.fs:132` for
  external binders (CLR consumes it at `EmitBindings.fs:129-144`). **`for-in`: no** — it
  carries `dispose: bool`; the slot is backend-minted (`EmitLoops.fs:86-90`). Latent only:
  the JS emitter has no `Use`/`ForIn` arm yet. ⇒ **`Infer.fs:132` must change** (slice-4's
  first task): structural own-`Dispose` key, else the neutral `disposable`-capability
  marker — never the BCL FQN. `for-in` is the template.

### Resolved (review decisions)
- **Capability model → F#-parity, interface aliases (§3.1).** equality/comparison stay
  the only named constraints (they already are `SemanticConstraintKind` cases). iteration
  /disposal get **interface aliases** (`seq` exists; **add `disposable`**), satisfied
  structurally or via `:>` coercion — **no new `iteration`/`disposal` constraint keywords**.
- **§6 ownership → this plan.** It creates the capability names in the core `.fsi`
  (`disposable` alias + equatable/comparable interface identities; `seq` already there).
- **§5.0 threading → (C) `PassContext.CapabilityIds`**, carrying *both* the `SymbolKey`
  and the rendered `qualifiedName` per capability (the eight consumer sites split across
  key- and string-keyed comparison; see §5.0 table). **Four identities** (enumerable,
  disposable, equatable, comparable). The capability set is a bounded record keyed to
  language features, **not** an open registry (confirmed against §12.3 anchor-scoping;
  formattable/awaitable would each be a deliberate addition).
- **§5.3 → ship (a) now (nominal, §5.0-resolved), defer (b)** until JS custom-eq/comp
  unblocks. equatable/comparable stay in §5.0 — the front-end *constraint* is already
  neutral (`SemanticConstraintKind`), but the FS0378 *validation* keeps its nominal
  interface check, identity-resolved per target (§3.1).
- **No silent fallback (§5.4).** Delete the four CLR literals as a terminal step; an
  unnamed capability is a resolve-on-use compiler error, never a CLR substitution.
- **Vertical slices, not horizontal layers (§10).** Drive enumerable, then disposable,
  then equatable/comparable end-to-end (identity → recognizer → `.fsi`/`.fs` → CLR + JS
  test) rather than landing all mechanism first.
- **`extern with` is required to *complete* §5.4 (not deferred).** §5.4's literal deletion
  needs the CLR `.fsi` to name the capabilities, which is `extern with` work. It lands early
  in the sprint and is independently testable via parser golden files + a frozen-tree
  `ExternalClassShape` assertion, ahead of any backend (§10 commits 2–3, §12.5).
- **JS emission arms land *in this sprint*, as adjacent per-slice commits (§1 scope note,
  §9, §10).** `for-in` → `obj[Symbol.iterator]()`, `use` → `obj[Symbol.dispose]()` — each its
  own commit, **gated on its slice's frozen-TAST assertion**, never woven into the front-end
  change. The original "frozen-TAST proof only" stance is superseded: the assertion proves
  *presence* and gates the commit; the JS arm proves *sufficiency*. Equatable/comparable is a
  pure FS0378 diagnostic with **no JS arm** (structural asymmetry, not a deferral). Commits
  5/7 additionally depend on the `tsc`-extracted JS provider.
- **`XParsec.FSharp` parser + AST in its own commit — LANDED (`b81e5dbf`); gate is SEMANTIC,
  not parse-time.** The parse-time `ParseState` flag was rejected (golden harness can't toggle
  it per-fixture); `extern with` parses unconditionally and the opt-in gate moves to the
  extractor (commit 3), per §12.4's fallback. **AST blast-radius (as predicted, smaller in
  practice):** only the positional `AstTraversal.walkTypeSignature` arm and the
  `SignatureParsing` constructor were compile-forced; the SemanticAnalysis consumers
  (`Conformance.fs`, `VesperLib.fs`) use named/wildcard patterns and did not need edits.

### Resolved (captured in §12)
- **Member → target binding** — reuse the existing `(# "…" #)` intrinsic-expression
  substitution (already in `ops-platform.fs`); no new machinery. (§12.1)
- **Metadata reconciliation / drift** — a non-issue: the per-target `.fs` companion is
  type-checked against the real platform symbols (CLR metadata; a `tsc`-extracted JS
  provider, delivered after/with this work), so a divergent `.fsi` member sig fails to
  compile its `.fs`. (§12.2)
- **Scope** — strictly language-semantic anchors (`for`, `use`, structural eq+comp for
  records/unions); never general BCL redeclaration. (§12.3)

---

## 12. Structural contracts for extern types (`extern with`) — the contract foundation

**Decision:** extend the `extern` `.fsi` form so an extern type can publish the
member sigs + interface impls that language semantics anchor on. This is the mechanism
that makes §5.0/§6 real on the contract (JS) layer — without it, member shapes for
extern types come only from `MetadataLoadContext` (CLR-only).

Today `type X = extern` (`Signatures.fs:111`, `Signature.Extern`) is opaque: name +
arity, `ExternalClassShape.basic` with empty `Members` / `FrozenInterfaces`. The repr
rides the per-target `.fs` companion (`(# "System.Int32" #)`). `prim-types-string.fsi`
is just `type string = extern`; `string`'s members come from BCL metadata, not the
contract.

`type X = extern with <member-decls>` lets the `.fsi` declare the capability-relevant
surface target-neutrally, so the provider's `ExternalClassShape.Members` /
`FrozenInterfaces` populate from the contract — the missing input for §5.1/§5.2
structural probing and §5.0 identity resolution on the JS layer.

### 12.1 Member → target binding: reuse the existing `(# "…" #)` substitution
Already proven at expression granularity in the per-target `.fs` companions —
`ops-platform.fs` lowers `(# "add" x y : ^T #)` / `(# "ceq" … #)` per primitive. An
extern member body in the `.fs` companion uses the same form
(`member _.GetEnumerator() = (# … #)`). So **no new emission machinery**: the
type-level `(# … #)` (intrinsic abbrev) and expression-level `(# … #)` (intrinsic
expr) mechanisms both already exist, and this is their composition at member
granularity.

### 12.2 Drift is a compiler error, not a maintenance hazard
The per-target `.fs` companion is type-checked by the backend against the *real*
platform symbols, so a `.fsi` member sig that diverges from the platform's actual
signature fails to compile its `.fs` binding. The `.fsi` is the neutral contract; the
`.fs` must satisfy both it and the platform. CLR checks against metadata today; **a JS
symbol provider extracted from `tsc`** does the same for JS `.fs` companions (delivered
after or alongside this work). Reconciliation is mechanical — no hand-maintained
"which wins" rule.

### 12.3 Scope: strictly language-semantic anchors
Additions are admitted *only* to match a language feature — never general BCL
redeclaration:
- `for … in` → the enumerable surface (`GetEnumerator` / `MoveNext` / `Current`);
- `use` → the disposable surface (`Dispose`);
- default **structural equality / comparison for records and unions** → the
  equatable / comparable surface;
- **receiver intrinsics the language desugars to** — e.g. the `s.[i]` indexer on
  `string` (the `string` analogue of `arr.[i]` → `GetArray`), and similar
  language-syntax anchors (`.Length`, operator/equality desugarings).

The discipline: a member earns a place on an extern type **only because a language
feature targets it**. `s.Substring(…)` is an ordinary method call, *not* a language
feature, so it stays resolved through the metadata / `tsc` provider and is never
hand-declared. That fence is what keeps this from becoming "redeclare `System.String`."

**Migration target — `StringIntrinsics`-style sidecars.** `ops-platform.fsi:443`
`module StringIntrinsics` (`val inline GetString : string -> int -> char`) is a
free-function sidecar that exists *solely* because `type string = extern` can't carry
instance members today: `s.[i]` desugars to `GetString` on the JS target (CLR prefers
metadata `get_Chars`), via a front-end routing fork. `extern with member` retires it —
the indexer becomes a member on `string`, bound per target by the `.fs` companion's
`(# … #)`, and `s.[i]` resolves through normal member resolution + §12.2 precedence
on every target. This collapses the special-case receiver-resolution precedence
(`reference_intrinsic_receiver_members_and_void_methods` /
`reference_by_value_indexer_get_chars_get_item`). On CLR it's a consistency refactor
(metadata already has `get_Chars`); on JS it's load-bearing. Other free-function
sidecars migrate **only** if they back a language feature — general helpers do not.

### 12.4 Build work

> **Cross-assembly + commit/gating note. — RESOLVED: semantic gate, not parse-time flag.**
> The parser + AST changes live in the **`XParsec.FSharp`** project (`SignatureParsing.fs`,
> `Signatures.fs`, `AstTraversal.fs`), *not* the `XParsec.FSharp.SemanticAnalysis` owner-seam
> — the extractor change crosses back into SemanticAnalysis. The parser + AST change landed
> in its own commit (`b81e5dbf`). **The parse-time `ParseState` feature flag was evaluated
> and REJECTED:** the golden-test harness auto-discovers `.fsi` fixtures and parses each
> through a fixed `ParseState` (no per-fixture knob), so a parse-time flag couldn't be
> toggled from the suite — it would make the positive golden test impossible while adding a
> record field + both `create` factories + both `Reader.ofLexed` factories + harness
> threading. **Adopted §12.4's documented fallback — the semantic gate:** `extern with`
> parses **unconditionally**; the extractor (commit 3) honours the trailing members only when
> the compilation opts in. The construct only appears in toolchain-controlled `.fsi`, so the
> ungated parser surface is low-blast-radius. (The `ParsingTypes.fs:203` "split input from
> mutable state" TODO is now untouched by this work.)

`extern with` **fails to parse today** — the `KWExtern` arm
(`SignatureParsing.fs:378-381`) returns `TypeSignature.Extern` immediately after
`pExtern`, never looking for a trailing `with`. It needs a **small parser change**,
which slips into the existing type-extension grammar (no new member/interface
productions):

- **Parser:** in the `KWExtern` arm, after `pExtern`, parse an optional
  `opt (choiceL [TypeExtensionElementsSignature.parse; parseLight])` — a **verbatim
  mirror of the record arm** (`SignatureParsing.fs:392-404`, which already does
  `type X = { … } with member …`). The `=` is consumed upstream (line 316), so the
  surface is `type X = extern with …`. Gate the optional trailing parse on the §12.4
  feature flag.
- **AST:** add one optional field to `Signature.Extern` (`Signatures.fs:111`):
  `members: TypeExtensionElementsSignature<'T> voption`. Every other `Signature.Extern`
  match arm adds a `_`.
- **Interfaces come free.** The type-extension element dispatcher (`elementDispatcher`,
  `SignatureParsing.fs:209-225`) already routes `KWInterface → pInterfaceSpecSig`,
  `KWInherit → pInherit`, plus `member`/`val`/`abstract`/`static`/`new`. So
  `extern with interface IEnumerable<'T>  member GetEnumerator : …` parses with zero new
  productions — exactly the surface needed to populate **both**
  `ExternalClassShape.Members` *and* `FrozenInterfaces`.
- **Extractor:** consume the new field → populate `ExternalClassShape.Members` /
  `FrozenInterfaces` (the element AST is the same one `sig_09` / `sig_04` already
  produce, so the member-shape extraction path is reused).
- **`.fs` companions:** give the capability members `(# … #)` bodies per target (§12.1).
- **Capability interfaces as extern types:** declare the disposable / equatable /
  comparable interfaces themselves as `extern with …` in the core `.fsi` (iteration is
  `seq`, already present — §6); this satisfies §5.0 (identities) and §6 (names) in one
  move. Per §3.1 these are interface *aliases*, not new constraint keywords.

### 12.5 Sequencing vs Tier 2
Not a hard prerequisite to *start* (§5.0's temporary CLR-literal fallback keeps CLR green
through commit 1), **but a hard prerequisite to *finish*:** §5.4 (delete the literals) needs
the CLR `.fsi` to name the capabilities, which is the `extern with` extractor + core-`.fsi`
work (§5.4, commits 2–3). So `extern with` lands inside this sprint, early — and it is
**independently testable the moment it lands** via parser golden files + a frozen-tree
`ExternalClassShape` assertion (§10 commits 2–3), with no backend in the loop. It is also
what lets the contract express the shapes the JS provider answers, so the JS arms (§10
commits 5/7) build on it too. Per §10: §5.0 mechanism → `extern with` parser/AST (own commit,
opt-in gate) → extractor + capability interfaces in core `.fsi` → per-capability slices
(front-end commit + adjacent JS-arm commit) → §5.4 deletes the literals last.

---

## 13. Commit 3b + 9 — the finale design (NEXT SESSION STARTS HERE)

**Self-contained.** Commits 1, 2, 3a, 6 have landed; commits 4/8 are subsumed by commit 1;
the JS arms (5/7) are a deferred follow-on. What remains is **3b** (declare the four
capability identities in the `Vesper.Core` contract so the provider can resolve them) and
**9** (delete the four CLR literals; resolve `CapabilityIds` from the provider). This section
is the investigated, ready-to-implement design. All premises below were verified against the
code in the session that wrote this.

### 13.1 The mechanism is the `exn === System.Exception` intrinsic-repr reconciliation

The front end already treats canonical `exn` as equivalent to BCL `System.Exception`, and the
machinery **generalizes to interfaces and generic interfaces** — verified:
- `subtypeNominalOf` (`Engine.fs:504-512`) and `subtypeInterfacesOf` (`Engine.fs:571-576`)
  run *every* metadata nominal/interface through `canonName` before comparison. `canonName`
  (`Engine.fs:420-449`) folds a BCL platform name back to the canon via the `{platform->canon}`
  reverse map (`PassContext.IntrinsicReverseCanon`, `SideTables.fs:1121-1134`; provider face
  `IExternalSymbolProvider.IntrinsicReverseCanon`, built at `TyparCapture.fs:373-381`).
- Nothing special-cases `exn`. A type whose metadata `GetInterfaces()` yields
  `System.IDisposable` **will** match a canonical `disposable` — *iff* a reverse-map entry
  `"System.IDisposable" -> "disposable"` exists. Generic interfaces work too (the canon string
  is name-only; type args are a separate invariant-checked axis), contingent on the repr
  string matching the metadata name exactly.

This is what "the CLR symbol provider assists semantic analysis to understand
`disposable === System.IDisposable` for casts and interface constraints" means concretely —
it already exists. 3b just feeds it the data.

### 13.2 The reverse-map entry comes ONLY from an `(# ... #)` intrinsic-repr — NOT a bare abbreviation

Critical, and the one place the implementation differs from the informal description:
- `harvestIntrinsicReprsInto` (`VesperLib.fs:1681-1707`) collects **only** `(# "<repr>" #)`
  forms. A bare `type disposable = System.IDisposable` abbreviation in the `.fs` is **not**
  harvested, contributes **no** reverse-canon entry, and yields **no** reconciliation.
- The reverse map is built from `Intrinsic(canon, platform=Some p) when p <> canon` shapes
  (`TyparCapture.fs:373-381`). So the CLR `.fs` companion MUST be the intrinsic-repr form:
  `type disposable = (# "System.IDisposable" #)`.
- Declaration mirrors `exn` exactly (verified `prim-types-exn.{fsi,fs,js.fs}`):
  `.fsi` -> `type exn = extern`; CLR `.fs` -> `type exn = (# "System.Exception" #)`; JS
  `.fs` -> `type exn = (# "Error" #)`. The per-target `.fs` is picked by **filename
  convention** (`<base>.fs` for CLR, `<base>.js.fs` for JS — `ReferencedProject.fs:428-441`),
  NOT a manifest key.

### 13.3 The Intrinsic-vs-members conflict — and why 3b is CLR-complete WITHOUT it

An `(# ... #)` repr sets `isIntrinsic = true` (`VesperLib.fs:1380`), routing extraction to the
`Intrinsic` branch — which **drops the `extern with` member surface** (the deferred TODO at
`VesperLib.fs:1402-1405`, the StringIntrinsics / Intrinsic-carrying-members case commit 3a
deferred). So a capability declared `extern with member Dispose …` + CLR `.fs`
`(# "System.IDisposable" #)` reconciles identity correctly **but its declared `Dispose` member
is dropped on the CLR contract layer.**

That is **fine for 3b/9**, because:
- CLR identity resolution needs only the `Intrinsic` platform face (the reverse-map entry),
  NOT the member surface — CLR gets members from metadata.
- The member surface matters only for **JS structural probing** (deferred commits 5/7), where
  the JS provider can't read members from metadata. That needs the Intrinsic-carrying-members
  migration (§12.3) first — part of the deferred JS effort, not 3b.

⇒ **3b decision:** declare the four capability types as bare `extern` (no `with` — the member
surface would silently not extract on CLR anyway); add `extern with member …` later as part of
the JS work when the Intrinsic-carrying-members migration lands. The CLR `.fs` carries the
`(# ... #)` repr.

### 13.4 Concrete 3b changes (contract)

A dedicated `capabilities.{fsi,fs}` is cleaner than extending `core-types`:
- **`src/Vesper.Core/capabilities.fsi`** (target-neutral): `type disposable = extern`,
  `type equatable<'T> = extern`, `type comparable<'T> = extern`. (Iteration: `seq<'T>` already
  exists — `Vesper.List/list.fsi:135`, an ABBREVIATION for `IEnumerable<'T>`; see §13.6.1.)
- **`src/Vesper.Core/capabilities.fs`** (CLR repr — the reconciliation source):
  `type disposable = (# "System.IDisposable" #)`, `type equatable<'T> = (# "System.IEquatable\`1" #)`,
  `type comparable<'T> = (# "System.IComparable\`1" #)`. **VERIFY the exact repr string** the
  metadata provider surfaces (arity-suffix spelling, namespace) — the reconciliation is an exact
  string match. `IComparable` may need BOTH generic `` IComparable`1 `` and non-generic
  `System.IComparable` (the `[<CustomComparison>]` rule targets the generic; metadata may surface
  either).
- **`src/Vesper.Core/manifest.toml`**: add `"capabilities.fsi"` to `files` (lines 33-50) — only
  listed `.fsi` are parsed. The `.fs` is discovered by filename convention; need NOT be in `impl`.
- **JS `.fs`** (`capabilities.js.fs`) is DEFERRED with the JS arms (without it these get
  `platform = None` on a JS build, which `PlatformTypes` flags — fine, JS is out of scope).

### 13.5 Concrete 9 changes (resolver + literal deletion)

- **Compile-order problem (same as commit 1):** `RuntimeNames.fs` (fsproj line 15) compiles
  BEFORE `ExternalSymbols.fs` (line 25), so `resolveCapabilities` **cannot name
  `IExternalSymbolProvider` while in `RuntimeNames`**. ⇒ **Move the resolver** to a module
  compiled after `ExternalSymbols.fs` (`SideTables.fs`, where `PassContext` already calls it,
  or a helper in `ExternalSymbols.fs`). The `CapabilityIdentity`/`CapabilityIds` **types** stay
  in `RuntimeNames` (they name only `SymbolKey` + `string`).
- **New resolver (sketch):** for each capability, `provider.TryLookupType canonName` ->
  `ExternalTypeShape.Intrinsic(platform = Some fqn)` -> build the identity by splitting the BCL
  `fqn` into ns + name for the `Key` and using `fqn` as `QualifiedName`. `ValueNone` if the
  provider doesn't name it. The recognizers already compare `QualifiedName` against
  `instantiateInterfaces` output and `Key` asm-blind — both satisfied (asm is a don't-care;
  `sameTypeAsmBlind` ignores it).
- **Delete** the four literal keys `ienumerableKey`/`idisposableKey`/`iequatableKey`/
  `icomparableKey` (`RuntimeNames.fs:124-143`).
- **Resolve-on-use, not at-construction (§5.4):** make `CapabilityIds` hold
  `CapabilityIdentity voption` per capability; recognizer SITES emit a graceful diagnostic on
  `ValueNone` (capability exercised but unnamed). Do NOT eagerly fail in the `PassContext` ctor —
  that would force every minimal fixture to declare all four.

### 13.6 Open sub-decisions for the next session

1. **Enumerable resolution path.** `seq<'T>` is an *abbreviation* (transparent), so
   `TryLookupType "seq"` returns `Abbrev`, not `Intrinsic`. Options: (a) read the `seq` Abbrev
   body's `IEnumerable` key; or (b) add an `iterable = extern` intrinsic to `capabilities` with
   `(# "System.Collections.Generic.IEnumerable\`1" #)` and resolve all four uniformly via the
   Intrinsic platform face. **Recommend (b)** for one code path (it duplicates the IEnumerable
   identity, but uniformity wins).
2. **Exact repr strings.** Probe what `MetadataSymbols`/`instantiateInterfaces` emit for
   `System.IDisposable` / `` IEquatable`1 `` / `` IComparable`1 `` / `IComparable` and match the
   `(# ... #)` reprs byte-for-byte.
3. **MockBuiltins cost (§5.4).** The 629 `SemanticAnalysis.Tests` run on `MockBuiltins`, which
   does NOT name the capabilities and DOES exercise the FS0378 custom-eq/comp goldens. After the
   literals are deleted those resolve `ValueNone` and would error. Either add the capability
   intrinsics to `MockBuiltins` or switch the affected goldens to a provider that has them.
   Enumerate the affected tests first.

### 13.7 Gate for 3b + 9
- **3b alone:** a contract-extraction test (à la commit 3a's `VesperLibTests`) that
  `TryLookupType "disposable"` returns `Intrinsic(platform = Some "System.IDisposable")` and that
  `IntrinsicReverseCanon` carries `"System.IDisposable" -> "disposable"`.
- **9:** full `SemanticAnalysis.Tests` (after the MockBuiltins fix) **and** full
  `Codegen.Clr.Tests` (1052 today) — `use`-over-external-disposable (`UseTests.fs:105`) and
  `for-in` disposable (`ForInTests.fs:348`) now exercise the *provider-resolved* path
  end-to-end. Green there is the terminal proof the four literals are gone with zero CLR
  regression. `buildContract defaultManifests` already reads `src/Vesper.Core/manifest.toml`
  first, so `capabilities.fsi` flows in with no test wiring.
