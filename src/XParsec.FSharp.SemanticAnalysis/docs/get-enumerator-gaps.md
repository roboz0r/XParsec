# `for … in` / `GetEnumerator` — remaining work (handoff)

`for x in src` is supported across the BCL and most project-local shapes. This
doc is the handoff for what's left, in **recommended implementation order**. Each
item is independent; none is blocked on unbuilt capability — they're scope, not
dead ends.

## Supported today

| source | enumerator | path |
|---|---|---|
| `IEnumerable<'T>` directly (`Linq.Range`) | interface `IEnumerator<'T>` | `Interface` |
| external class impl. `IEnumerable<'T>` (no pattern `GetEnumerator`) | interface | `Interface` (fallback) |
| external class, pattern `GetEnumerator()` → **struct** `E` (`List<'T>`) | value-type `E` | `Pattern`, both axes `External` |
| external class, pattern `GetEnumerator()` → **reference** `E` (`BitArray`) | reference `E` | `Pattern`, both axes `External` |
| **user** class impl. `IEnumerable<'T>` (incl. generic, via `this`) | interface `IEnumerator<'T>` | `Interface` (local probe) |
| **user** source, pattern `GetEnumerator()` → **reference** user `E` (no interface) | reference user `E` | `Pattern`, both axes `Local` |
| ↳ where that user `E` also `: IDisposable` | reference user `E`, disposed in `finally` | `Pattern`, both axes `Local`, `dispose = true` |
| **user** source, pattern `GetEnumerator()` → **value-type** user `E` (`[<Struct>]`) | by-address user `E`, direct `call` | `Pattern`, both axes `Local` |
| **user** source, pattern `GetEnumerator()` → **external** `E` (`List<'T>.Enumerator`) | local `GetEnumerator` + external `E` members | `Pattern`, `Local` get-enum / `External` members |
| range `for i in a .. b` | — | pre-existing |

## The descriptor (current model)

`ForInEnumeratorG<'ty>` (`SideTypes.fs`) has exactly two cases — there is no longer
a separate case per source/enumerator combination:

- **`Interface`** — lower through the `IEnumerable<'T>` / `IEnumerator<'T>` interface
  slots with `callvirt`. The default for the range form and for any source whose
  only enumerable surface is the interface.
- **`Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose)`** — the
  duck-typed / non-boxing `foreach` path. The two resolution mechanisms are split
  onto **two independent axes**, so the historically-distinct duck-typed forms are
  now the valid *combinations* of those axes rather than separate cases:
  - **`getEnumerator: ForInGetEnum`** — how codegen refs the source's
    `GetEnumerator`: `External key` (mint via `ExternalMemberRef`) or `Local`
    (resolve off the source expression's type via `resolveInstanceMember`).
  - **`members: ForInEnumMembers`** — how codegen refs `E`'s `MoveNext` / `Current`:
    `External(mnKey, curKey)` (mint via `ExternalMemberRefOn` against `enumeratorTy`)
    or `Local` (resolve off the user `E` `TypeDef`).
  - `isValueType` is `E`'s value-type-ness (selects by-address, non-boxing
    emission). `dispose: bool` is `true` iff `E : IDisposable`; disposal is **always**
    the `System.IDisposable::Dispose` interface slot, which codegen mints itself —
    no member key is carried.

  Three of the four axis combinations are reachable: external/external (a BCL source
  like `List<'T>`), local/external (a user source over a BCL enumerator, "Gap 3"),
  and local/local (a user source over a user `E`). External/local is unrepresentable
  in practice — an external source never hands back a project-local enumerator.

## The two driving abstractions

- **Front end** — `Infer.tryForInEnumerator` (`Passes/Unification/InferControlFlow.fs`)
  resolves the source to an element type + a `ForInEnumeratorG` descriptor, stored in
  `ForInShape` and frozen onto `TExpr.ForIn`. External sources go through
  `tryDuckTypedEnumerator`; project-local sources go through
  `tryLocalDuckTypedEnumerator` (falling back to `tryLocalInterfaceEnumerator`). Both
  duck-typed resolvers share two probe helpers that read an enumerator `E`'s members
  and return the element type / value-type-ness / disposability:
  `probeExternalEnumerator` (reads `ExternalClassShape`, interns provider keys) and
  `probeLocalEnumerator` (reads the user class's `Members` / `InterfaceImpls`). The
  local-source resolver picks the probe by whether `E` is itself project-local or
  external.
- **Codegen** — `EmitLoops.buildForIn` has one arm per `ForInEnumeratorG` case
  (`Pattern` / `Interface`), both feeding the shared `emitEnumeratorLoop`. The
  `Pattern` arm resolves `geHandle` off `getEnumerator` and `(mnHandle, curHandle)`
  off `members`, dispatching each axis independently. For a **value-type** `E` the
  shared emitter addresses the receiver (`ldloca`) and dispatches `E`'s own
  `MoveNext` / `Current` with a **direct `call`** — *not* `constrained. callvirt`,
  which mis-dispatches a non-virtual struct `MethodDef` against an uninitialised
  receiver. `Dispose` is the lone member reached through the `IDisposable` interface
  slot, so it keeps `constrained. callvirt` for a struct `E`. `emitEnumeratorLoop`
  mints the `Dispose` handle once (`mintDisposeHandle`) when `dispose = true`.

---

## Remaining work

### 1. Value-type *source*  — low priority

`buildForIn` emits `GetEnumerator` as a `callvirt`/`call` after loading the source —
correct for a reference source, malformed for a **value-type** source (a method call
on a stack value needs the value boxed or the receiver by address). The fix mirrors
the enumerator's value-receiver handling one level up: evaluate the source to a
local, `ldloca` it, and call `GetEnumerator` by address (`ExternalMemberRefOn` /
`resolveInstanceMember` for the value-tagged parent, `constrained. <Source>` for an
interface-slot call).

**Low priority**: no `set.fs` site sources a value-type collection, and BCL
value-type enumerables are mostly ref-struct spans (which can't be a `for … in`
source here anyway).

### 2. Ref-struct enumerator with a pattern `Dispose()` (no `IDisposable`)

`dispose` is currently a `bool`, and disposal always goes through the
`System.IDisposable::Dispose` interface slot. A `[<IsByRefLike>]` enumerator can't be
boxed to `IDisposable`, so an F#-parity walk must instead call the ref struct's *own*
public `Dispose()` when it has one. Closing this needs:
- a byref-like predicate — `SemType` has no ref-struct case today (see
  `InlineExpansion.fs` / `Regions.fs`);
- the `Pattern` descriptor's `dispose` to carry *which* `Dispose` to call (a member
  key or "own/interface" tag), not just a bool.

This mirrors the `use`-binder precedent `Infer.tryExternalDispose` (prefer the type's
own `Dispose`, fall back to the interface slot). See the TODO on `probeLocalEnumerator`
/ `probeExternalEnumerator`.

### 3. Generic user interface impls — orthogonal front-end gaps

The `Interface` path already substitutes class typars with use-site args, so it is
ready for generic user sources *once two pre-existing impl/upcast gaps close*: a
generic `Box<'T> : IEnumerable<'T>` hits "Free type parameter 'T is not declared…" on
the interface impl, and `IEnumerator<'T> :> IEnumerator` is rejected as "no
inheritance relationship". These are not `for … in` bugs — track them with the
interface-implementation work.

---

## Cross-references

- [`brainstorm-seq-module.md`](brainstorm-seq-module.md) — the enumerator design
  context.
