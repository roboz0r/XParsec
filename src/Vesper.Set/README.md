# Vesper.Set

The immutable AVL-tree `Set<'T>` type and the `Set` module for **Vesper** (the
language; `XParsec.*` is the *compiler*), packaged standalone — one package per
type, see
[`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md).
Sibling of `Vesper.List`.

Both `set.fsi` and `set.fs` are **verbatim copies of FSharp.Core's
`set.fsi`/`set.fs`** with the namespace patched (`Microsoft.FSharp.Collections`
→ `Vesper.Collections`) and the `Microsoft.FSharp.*` opens dropped. Nothing
else has been edited: the implementation file is the same AVL-tree code, the
same internal `SetTree` representation, the same `Set` module, and the same
`#if NETSTANDARD2_1_OR_GREATER` `Create` / `CollectionBuilder` block.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `set.fsi` | **contract** — `Set<'T>` + `module Set` signatures in namespace `Vesper.Collections` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `set.fs` | **runtime impl** — the AVL-tree set + `module Set` bodies | this repo's own backend → `Vesper.Set.dll` (eventually; see below) |

A normal signature/implementation pair, both ours-once-copied.

## `set.fs` is parse-tested, not yet compiled

The impl is verbatim FSharp.Core, so it uses `OptimizedClosures.FSharpFunc`,
`LanguagePrimitives.FastGenericComparer`, the `SR.*` string-resource
indirection, `Seq.fold` / `Seq.reduce`, `Array.fold` / `Array.zeroCreate`,
`List.ofSeq`, etc. — all of which are out of scope for the present self-host
rung. Today the file is parse-tested via the golden `.parsed` snapshot
(`test/Vesper.Tests/VesperCoreContractTests.fs`); it grows into the compiled
impl once the backend supports the surface it lands on (or once the file is
edited to a self-hostable subset, the way `Vesper.List/list-min.fs` was carved
out of `List.fs`).

This is the same stance `Vesper.List/List.fs` held before the cons-list
cutover landed — verbatim target source, parse-only, replaced or shrunk into
a compiled subset when the backend catches up.

## Naming / shape decisions

- **Package name ≠ namespace**: the package is `Vesper.Set`, but it contributes
  type `Set` and module `Set` into namespace **`Vesper.Collections`** (shared with
  `Vesper.List`, `Vesper.Array`, `Vesper.Seq`, and the future `Vesper.Map`).
- **The widest `depends-on` in the tree** — Core, List, Array, Seq, Choice,
  Option, Comparison, Printf. That breadth is a property of the verbatim
  FSharp.Core source, not of the design: the `Set` module surfaces `Set.toList` /
  `Set.ofList` (⇒ `Vesper.List`'s `'T list`), `SetTree.mk`/`rebalance`/`balance`/
  `add` use the ordering operators (⇒ `Vesper.Comparison`), and the
  invariant-violation messages call `sprintf` (⇒ `Vesper.Printf`).
- The module is **`[<RequireQualifiedAccess>]`** + **`ModuleSuffix`** (compiled
  name `SetModule`), matching the rest of the tree.

## A separate `Vesper.Set.dll`

Once the backend can compile `set.fs`, an emitted program that uses sets will
carry a `Vesper.Set` `AssemblyRef` *alongside* the `Vesper.Core` ref (for `Fun`)
and the `Vesper.List` ref (for the `'T list` the `Set` module's list-bridging
members name). The provider's refs are `lazy`, so a program that touches no set
pins no `Vesper.Set` ref.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.List` / `Vesper.Option` / `Vesper.Result`, this tree
is not built by `dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp` and
walked into an `IExternalSymbolProvider`; the `.fs` is compiled by our own backend
(eventually). Parser coverage is verified by golden `.parsed` snapshots committed
next to each source (`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md) — the per-package split this realises, and the dependency graph it sits in.
- [`../Vesper.List/README.md`](../Vesper.List/README.md) — the sibling package whose split this mirrors; also the source of `'T list`.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
