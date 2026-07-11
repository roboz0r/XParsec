# `seq` / `enumerator` iteration capability — outstanding work

## Where this stands

The capability is **declared, resolved, authored, and lowered on both targets.** `Vesper.List` is
the first Vesper type to implement it, and a BCL consumer can iterate it.

Landed:

- **Contract.** `seq<'T>` / `enumerator<'T>` are member-bearing `extern interface` capabilities
  (`capabilities.fsi`), with platform reprs (`capabilities.fs`) and JS compat rows
  (`capabilities-compat.js.fsi`). `enumerator` inherits `disposable`, so `use e = …` on an abstract
  enumerator type-checks; disposal composes rather than being declared (**D1**).
- **Resolution.** `Enumerator` joined the closed `CapabilityIds`; both capabilities resolve
  dual-faced (canonical `Vesper.Collections.seq` ⟷ platform `System…IEnumerable\`1`), and the two
  faces are interchangeable at the three nominal-key comparison seams (`unify`, `subsume`/upcast,
  overload filter) via `EngineCore.capabilityCanonKey`.
- **Authoring.** `Vesper.List` implements the capabilities directly (`list.fs`, `list.js.fs`,
  `list.fsi`) — no BCL faces in the source. The kind-agnostic interface-impl pipeline needed **no
  changes** to accept the first authored capability impl.
- **CLR lowering.** Member-face rebasing for manual calls (`TryCapabilityBaseMemberKey`) plus
  **co-slot synthesis** (**D2**, below) — so a C# consumer of a Vesper assembly iterates the type
  normally.
- **JS lowering.** Already keyed off the capability, not the BCL spelling: `partitionClassMembers`
  buckets the impl into `[Symbol.iterator]` unchanged.
- **`Vesper.Seq`.** `fold`/`reduce`/`toArray` iterate with `for x in source` (portable to both
  targets).

## Design (unchanged, for reference)

A member-bearing, BCL-free pull protocol in `Vesper.Collections`:

```fsharp
type enumerator<'T> = extern interface with
    inherit Vesper.disposable
    abstract member MoveNext: unit -> bool
    abstract member Current: 'T

type seq<'T> = extern interface with
    abstract member GetEnumerator: unit -> enumerator<'T>
```

**D2 — co-slot synthesis (settled, landed).** A capability's platform face drags in a wider BCL
hierarchy than the capability's member surface: `IEnumerable\`1` inherits the non-generic
`IEnumerable`; `IEnumerator\`1` inherits `IEnumerator`'s `object Current` and `Reset`. The CLR
requires every method in a declared interface's transitive closure to be implemented, so the backend
synthesises the members the author never wrote (`CoSlot`, `CapabilityCoSlots`, `Layout.coSlotRows`,
`NominalEmit`, `Emit.build*CoSlot`) as forwarding shims. The *generic* face slots need no synthesis —
the authored members bind to them implicitly by name + signature, which is also why `MoveNext`, whose
signature is identical on the non-generic `IEnumerator`, needs no shim. The capability is recognised
structurally (an `IntrinsicInterface` shape's platform face), never by a canonical
`Vesper.Collections.seq` string; the BCL knowledge lives in the CLR backend, as it already does in
`EmitLoops`. Proven end-to-end by the `SelfHostTests` BCL round-trip: a plain .NET consumer builds a
`List<int>` and enumerates it through the *non-generic* `IEnumerable` — the face made entirely of
synthesised members — with `Reset` throwing `NotSupportedException`.

## Outstanding

### `Vesper.Seq.truncate`

Still CLR-Linq `Enumerable.Take` (`seq.fs:64-65`) — the last member of `Vesper.Seq` with no JS
lowering. A knowing deferral, blocked on sequence expressions (the backend cannot yield lazily), and
documented in the sited comment.

### JS manual-enumeration lowering

Logged in `docs/get-enumerator-gaps.md` ("Remaining work" item 2). Manual
`GetEnumerator`/`MoveNext`/`Current` has no JS lowering; map `seq.GetEnumerator()` to
`$0[Symbol.iterator]()` with a small `Vesper.Core.mjs` adapter providing the `MoveNext`/`Current`
split over native `next() → { value, done }` (a stateless `(# #)` cannot hold the shared-result
state). Moot while the library only uses `for … in`; real the moment user code enumerates manually
on JS.

### Capability co-slot synthesis on a *generic record*

`NominalEmit.capabilityMember` fails loudly for a record: `RecordMember` has no augmentation-member
case, so a generic record cannot mint a self-`TypeSpec` ref to its own method. No such type exists
(records rarely implement `seq`), and the failure is explicit rather than silently wrong IL — but a
generic record implementing the iteration capability would need that case added.
