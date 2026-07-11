# `for … in` / `GetEnumerator` — remaining work (handoff)

`for x in src` is supported across the BCL and most project-local shapes. This
doc is the handoff for what's left, in **recommended implementation order**. Each
item is independent; none is blocked on unbuilt capability — they're scope, not
dead ends.

## Supported today

The `Interface` path lowers on **both** targets. Every `Pattern` row below is
**CLR-only**: `EmitJs` handles `ForInEnumeratorG.Interface` (a `for…of` over the
source, deferring to the JS iterator protocol) and fails hard on `Pattern`. That is
by construction, not an oversight — a duck-typed source never type-checks against
the BCL-free JS provider, so on JS a source's only enumerable surface *is* the
interface.

| source | enumerator | path |
|---|---|---|
| `IEnumerable<'T>` directly (`Linq.Range`) | interface `IEnumerator<'T>` | `Interface` |
| external class impl. `IEnumerable<'T>` (no pattern `GetEnumerator`) | interface | `Interface` (fallback) |
| external class, pattern `GetEnumerator()` → **struct** `E` (`List<'T>`) | value-type `E` | `Pattern`, both axes `External` |
| external class, pattern `GetEnumerator()` → **reference** `E` (`BitArray`) | reference `E` | `Pattern`, both axes `External` |
| **user** class impl. `IEnumerable<'T>` (incl. a generic `C<'T>`, walked via `this`) | interface `IEnumerator<'T>` | `Interface` (local probe) |
| **user** RECORD impl. the seq capability (`interface seq<'T>`), incl. a generic `Bag<'T>` | interface `IEnumerator<'T>` | `Interface` (local probe, shared `IInterfaceImplHost` walk) |
| **user** source, pattern `GetEnumerator()` → **reference** user `E` (no interface) | reference user `E` | `Pattern`, both axes `Local` |
| ↳ where that user `E` also `: IDisposable` | reference user `E`, disposed in `finally` | `Pattern`, both axes `Local`, `dispose = true` |
| **user** source, pattern `GetEnumerator()` → **value-type** user `E` (`[<Struct>]`) | by-address user `E`, direct `call` | `Pattern`, both axes `Local` |
| **user** source, pattern `GetEnumerator()` → **external** `E` (`List<'T>.Enumerator`) | local `GetEnumerator` + external `E` members | `Pattern`, `Local` get-enum / `External` members |
| **value-type** source (a `[<Struct>]` collection, e.g. `MapSeq`/`ArraySeq`) | source addressed (`ldloca`) for `GetEnumerator` | either arm; by-address `call` (`Pattern`) or `constrained.` (`Interface`) |
| **generic typar** source (`'S :> ISeq`/`IStructSeq<'E>`, a custom non-`IEnumerable` seq interface) | `GetEnumerator` via `constrained. !S callvirt`; concrete `E` → by-address walk, typar `E` → `constrained. !E callvirt` | `Pattern`, `ConstrainedInterface` on the get-enum and/or members axis |
| range `for i in a .. b` | — | pre-existing |

## The descriptor (current model)

`ForInEnumeratorG<'ty>` (`SideTypes.fs`) has exactly two cases — there is no
separate case per source/enumerator combination:

- **`Interface`** — lower through the `IEnumerable<'T>` / `IEnumerator<'T>` interface
  slots with `callvirt`. The default for the range form and for any source whose
  only enumerable surface is the interface. The one case JS lowers.
- **`Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose)`** — the
  duck-typed / non-boxing `foreach` path. The resolution mechanism is split onto
  **two independent axes**, each a three-case DU, so the historically-distinct
  duck-typed forms are the valid *combinations* of those axes rather than separate
  cases:
  - **`getEnumerator: ForInGetEnum`** — how codegen refs the source's
    `GetEnumerator`: `External key` (mint via `ExternalMemberRef`), `Local` (resolve
    off the source expression's type via `resolveInstanceMember`), or
    `ConstrainedInterface(iface, ifaceArgs)` (the source is a generic typar, or a
    value reached only through a custom seq interface — emit `constrained. <Source>
    callvirt iface::GetEnumerator`, so a struct source dispatches by address and a
    class source by reference; the slot is looked up by name in the
    `EmittedInterface` registry).
  - **`members: ForInEnumMembers`** — how codegen refs `E`'s `MoveNext` / `Current`:
    `External(mnKey, curKey)` (mint via `ExternalMemberRefOn` against `enumeratorTy`),
    `Local` (resolve off the user `E` `TypeDef`), or
    `ConstrainedInterface(iface, ifaceArgs)` (`E` is itself a generic typar
    constrained to an enumerator interface — emit `constrained. <E> callvirt
    iface::MoveNext / iface::get_Current`, which dispatches a struct enumerator typar
    by address, no box).
  - `isValueType` is `E`'s value-type-ness (selects by-address, non-boxing
    emission). `dispose: bool` is `true` iff `E : IDisposable`; disposal is **always**
    the `System.IDisposable::Dispose` interface slot, which codegen mints itself —
    no member key is carried.

  The axes are genuinely independent, so not every combination is meaningful. The
  ones that arise: external/external (a BCL source like `List<'T>`), local/external
  (a user source over a BCL enumerator), local/local (a user source over a user `E`),
  and the constrained-typar combinations (a typar source whose `E` is either concrete
  — `ConstrainedInterface`/`Local`-or-`External` — or itself a typar —
  `ConstrainedInterface` on both). External/local is unrepresentable in practice: an
  external source never hands back a project-local enumerator.

## The two driving abstractions

- **Front end** — `Infer.tryForInEnumerator` (`Passes/Unification/InferControlFlow.fs`)
  resolves the source to an element type + a `ForInEnumeratorG` descriptor, stored in
  `ForInShape` and frozen onto `TExpr.ForIn`. External sources go through
  `tryDuckTypedEnumerator`; project-local sources go through
  `tryLocalDuckTypedEnumerator` (falling back to `tryLocalInterfaceEnumerator`); a
  generic-typar source goes through `tryTyparSeqSource`, which resolves the enumerable
  surface off the typar's `Coercion` constraint. A project-local *record* source resolves
  its `interface seq<'T>` through `tryLocalInterfaceEnumeratorOn`, the kind-agnostic
  `IInterfaceImplHost` walk the class path also uses — records reach only the `Interface`
  surface, since `EmitResolve.resolveInstanceMember` carries no record member table and so
  could not emit a `Local` duck-typed axis. The two duck-typed resolvers share
  two probe helpers that read an enumerator `E`'s members and return the element type
  / value-type-ness / disposability: `probeExternalEnumerator` (reads
  `ExternalClassShape`, interns provider keys) and `probeLocalEnumerator` (reads the
  user class's `Members` / `InterfaceImpls`). The local-source resolver picks the
  probe by whether `E` is itself project-local or external.
- **Codegen** — `EmitLoops.buildForIn` has one arm per `ForInEnumeratorG` case
  (`Pattern` / `Interface`), both feeding the shared `emitEnumeratorLoop`. The
  `Pattern` arm resolves `geHandle` off `getEnumerator` and `(mnHandle, curHandle)`
  off `members`, dispatching each axis independently — the `ConstrainedInterface`
  case of either axis mints its slot through `constrainedSlot` (name lookup in the
  `EmittedInterface` registry, routed through `EmitResolve.memberRef` so a generic
  interface lands on the instantiated `TypeSpec`).

  Receiver/dispatch in the shared emitter turns on three flags, not one:
  - a **value-type** `E` addresses the receiver (`ldloca`) and dispatches `E`'s own
    `MoveNext` / `Current` with a **direct `call`** — *not* `constrained. callvirt`,
    which mis-dispatches a non-virtual struct `MethodDef` against an uninitialised
    receiver;
  - a **typar** `E` (`MembersViaConstrained`) also addresses the receiver, but *does*
    use `constrained. <E> callvirt` — the JIT then dispatches a struct typar by address
    (no box) and a class typar by reference. So `constrained.` is not reserved for
    `Dispose`; it is the correct form for a typar, and the wrong form for a concrete
    struct;
  - a **value-type or constrained-typar source** (`GetEnumViaConstrained`) is spilled
    to a local and addressed for the `GetEnumerator` call, since a method call on a
    value needs its address (an `FTTypar` is not statically a value type, but
    `constrained. callvirt` needs the address anyway).

  `Dispose` is the lone member always reached through the `IDisposable` interface
  slot, so it keeps `constrained. callvirt` for a struct `E`. `emitEnumeratorLoop`
  mints the `Dispose` handle once (`mintDisposeHandle`) when `dispose = true`.

---

## Remaining work

### 1. Ref-struct enumerator with a pattern `Dispose()` (no `IDisposable`)

`dispose` is currently a `bool`, and disposal always goes through the
`System.IDisposable::Dispose` interface slot. A `[<IsByRefLike>]` enumerator can't be
boxed to `IDisposable`, so an F#-parity walk must instead call the ref struct's *own*
public `Dispose()` when it has one. Closing this needs:
- a byref-like predicate — `SemType` has no ref-struct case today (see
  `InlineExpansion.fs` / `Regions.fs`);
- the `Pattern` descriptor's `dispose` to carry *which* `Dispose` to call (a member
  key or "own/interface" tag), not just a bool.

This mirrors the `use`-binder precedent `Infer.tryExternalDispose` (prefer the type's
own `Dispose`, fall back to the interface slot). See the sited TODO in
`probeLocalEnumerator`.

### 2. Manual enumeration protocol — no JS lowering (`GetEnumerator`/`MoveNext`/`Current`)

`for … in` lowers on both targets (through the `Interface` case — see the scope note
above), but the **manual** pull protocol — `let e = source.GetEnumerator()` then
`while e.MoveNext() do … e.Current` — is CLR/F#-idiomatic and does **not** lower on JS
today. JS iteration is `Symbol.iterator` + `next() → { value, done }`, which
*combines* advance+read; the capability deliberately **splits** them into `MoveNext`
(advance, `bool`) and `Current` (read, `'T`), and a stateless `(# … #)` intrinsic
cannot carry the shared `next()`-result state the split needs.

Intended lowering (future work):
- Map `seq.GetEnumerator()` via an intrinsic to `$0[Symbol.iterator]()` (the
  native JS iterator).
- Provide a small `Vesper.Core.mjs` runtime **adapter** that wraps that native
  iterator and exposes the `MoveNext` / `Current` split over `next() →
  { value, done }` (holding the last `next()` result between the `MoveNext`
  advance and the `Current` read — the state a `(# … #)` can't express).

Until then, `Vesper.Seq`'s terminals (`fold` / `reduce` / `toArray`) are written
with `for … in` (not the manual protocol) precisely so they stay portable — see
the sited comment in `src/Vesper.Seq/seq.fs`. `truncate` stays CLR-Linq
(`System.Linq.Enumerable.Take`) and is a separate portability concern.

---

## Cross-references

- [`brainstorm-seq-module.md`](brainstorm-seq-module.md) — the enumerator design
  context.
