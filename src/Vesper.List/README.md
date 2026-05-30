# Vesper.List

The immutable cons-list type and the `List` module for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type). Carved out of `Vesper.Core`'s `core-types.fsi`. This
is the **template the other collection packages copy** (package-split-plan
milestone item 2): the first per-type package whose runtime impl is actually
compiled and *consumed by emitted programs* (the canonical sample's `[1;…]` +
`List.fold`).

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `list.fsi` | **contract** — the type + `List` module signatures, in namespace `Vesper.Collections` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `list-min.fs` | **runtime impl** — the union + member bodies | this repo's own backend → `Vesper.List.dll` (BCL-only) |
| `List.fs` | **verbatim target** — the `[]`/`::` + `module List.fold` end-state | parse-tested only; not yet compiled (see below) |

A normal signature/implementation pair, both ours. `list-min.fs` is a partial,
growing subset of the contract (only `fold` in the `List` module; the rest of the
module is contract-only for now) — expected while the self-hosting ladder is
climbed, the same stance as `Vesper.Option`.

## `list-min.fs` vs `List.fs` (the compiled vs verbatim split)

`Vesper.List.dll` is compiled from **`list-min.fs`**: the cons-list with explicit
**`Nil` / `Cons`** case names (a sanctioned deviation, minimal-core-lib-plan D6)
and instance `IsEmpty` / `Head` / `Tail` (`Head` / `Tail` raise via `failwith`,
which lowers to a BCL `System.Exception`, so the DLL carries no `FSharp.Core`
reference). The named cases keep the source inside the proven front-end subset; a
`[1; 2; 3]` literal still binds to the union by **arity** (nullary terminator +
binary cons), not by case name.

**`List.fs`** is the verbatim `[]` / `::` operator-case form with the higher-order
`module List.fold` written in Vesper. It is the end-state `list-min.fs` is replaced
by once the backend gains *public module-function compilation* (a self-reference to
`Vesper.Fun` + a real `Vesper.Collections.ListModule` holder).
Until then `List.fold` is emitted **inline** in the consuming program rather than
compiled into `Vesper.List.dll`.

> On Windows (case-insensitive filesystem) `list-min.fs`, `List.fs` and `list.fsi`
> are distinct names; a `list.fs` impl would collide with `List.fs`, so the
> compiled impl keeps the `list-min.fs` name.

## Two legs (package-split-plan PS3)

- **Type leg** — the generic `List<'T>` union (`Cons` / `Nil` + `IsEmpty` /
  `Head` / `Tail`). Landed: a real generic `TypeDefinition` (`List\`1`) with static
  case factories and instance members (rung 2 / R2).
- **Module leg** — `List.fold` is higher-order, so it rides **R1** (the
  `Fun`-not-`FSharpFunc` cutover, done). It is emitted inline today; compiling it
  *into* `Vesper.List.dll` waits on public module-function compilation.

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.List`, but it
  contributes type `List` and module `List` into namespace **`Vesper.Collections`**
  (shared with the future `Vesper.Map` / `Vesper.Set`).
- **Depends on `Vesper.Option`** as well as `Vesper.Core`: the contract's
  `List.GetSlice: startIndex: int option * endIndex: int option` names `int option`,
  whose `Option` now lives in `Vesper.Option`. This resolves the forward-reference
  `src/Vesper.Option/README.md` documented while `List` still lived in Core. The
  *compiled* `Vesper.List.dll` does not actually reference `Vesper.Option` (the
  minimal impl has no `GetSlice`); the dependency is a contract-level edge.
- The module is **`[<RequireQualifiedAccess>]`** + **`ModuleSuffix`** so it shares
  the `List` name with the type (compiled name `ListModule`), matching the rest of
  the tree.
- Module functions are **not `inline`** (unlike FSharp.Core): the Vesper backend
  lowers each functional-argument application to `callvirt Fun::Invoke`.

## A separate `Vesper.List.dll` (package-split-plan PS2)

`List` used to be compiled into `Vesper.Core.dll` (R3). PS2 (one impl DLL per
package) makes it its own assembly: an emitted program that uses lists now carries
a `Vesper.List` `AssemblyRef` (for the `List` type + `Cons`/`Nil` factories +
`IsEmpty`/`Head`/`Tail`) *alongside* the `Vesper.Core` ref (for `Fun`). Both are
`Vesper.*` — no `FSharp.Core`. The P2/G6 lazy-ref machinery means a program that
touches no list pins no `Vesper.List` ref. So the canonical sample's bundle ships
`Vesper.Core.dll` **and** `Vesper.List.dll`, no `FSharp.Core.dll`.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Option` / `Vesper.Printf` / `XParsec.FSharp.Lib`,
this tree is not built by `dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp`
and walked into an `IExternalSymbolProvider`; the `.fs` is compiled by our own
backend. Parser coverage is verified by golden `.parsed` snapshots committed next
to each source (`test/XParsec.FSharp.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`minimal-core-lib-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/minimal-core-lib-plan.md) — the single-tree core this is split out of; D6 (`Nil`/`Cons` deviation), D8 (`List` mirrors `FSharpList`).
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
- [`../Vesper.Option/README.md`](../Vesper.Option/README.md) — the sibling package whose split this mirrors.
