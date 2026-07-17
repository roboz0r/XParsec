# Vesper.List

The immutable cons-list type and the `List` module for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone — one package per type, see
[`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md).
Carved out of `Vesper.Core`'s `core-types.fsi`. This is the **template the other
collection packages copy**: the first per-type package whose runtime impl is
actually compiled and *consumed by emitted programs* (the canonical sample's
`[1;…]` + `List.fold`).

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `list.fsi` | **contract** — the type + `List` module signatures, in namespace `Vesper.Collections` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `list.fs` | **runtime impl** — the verbatim `[]`/`::` union + member/module bodies | this repo's own backend → `Vesper.List.dll` (BCL-only) |

A normal signature/implementation pair, both ours. `list-min.fs` is a partial,
growing subset of the contract (only `fold` in the `List` module; the rest of the
module is contract-only for now) — expected while the self-hosting ladder is
climbed, the same stance as `Vesper.Option`.

## `list.fs` (the compiled impl)

`Vesper.List.dll` is compiled from **`list.fs`**: the verbatim FSharp.Core
`[]` / `::` operator-case cons-list, with instance `IsEmpty` / `Head` / `Tail`
(`Head` / `Tail` raise via `failwith`, which lowers to a BCL `System.Exception`,
so the DLL carries no `FSharp.Core` reference) and the `List` module (`fold` plus
the proven "grow" set `isEmpty`/`length`/`head`/`tail`/`map`/`filter`/`append`/`rev`).
The `[]` / `::` cases compile to FSharpList's exact shape — `[]` → a static
`Empty` factory, `(::)` → a static `Cons` factory + `Cons_0` / `Cons_1` payload
fields — and a `[1; 2; 3]` literal binds to the union by **arity** (nullary
terminator + binary cons). This is the **cutover** (vesper-lib-test-plan): it
became buildable once the front end gained cons-pattern / cons-construction /
empty-list-pattern lowering (`h :: t`, `x :: xs`, `[]` → `TPat.Union` /
`TExpr.UnionCons`). `List.fold` is compiled *into* the DLL (its folder is a
`Vesper.Fun`, so the DLL carries a `Vesper.Core` `AssemblyRef`).

`ofSeq` / `toSeq` stay **contract-only** in `list.fsi` (not in the compiled
module): they ride Phase 4's `for x in IEnumerable` + seq comprehensions, not yet
compilable.

`list-min.fs` was the placeholder this replaced — the cons-list with explicit
**`Nil` / `Cons`** case names (a sanctioned deviation from the `[]`/`::` surface)
that kept the source inside the proven front-end subset *before* cons patterns
landed. It was removed once the cutover landed (see git history for the `Nil`/`Cons`
reference); `list.fs` (matching `list.fsi`'s casing and FSharp.Core's own
`list.fs`/`list.fsi`) is now the sole impl.

## The type and the module are separate legs

- **The type** — the generic `List<'T>` union (`[]` / `::` → `Empty` / `Cons` +
  `IsEmpty` / `Head` / `Tail`): a real generic `TypeDefinition` (`List\`1`) with
  static case factories and instance members.
- **The module** — `List.fold` (and the grow set) is higher-order, so it needs the
  `Fun`-not-`FSharpFunc` function representation. The module is compiled *into*
  `Vesper.List.dll` as the `Vesper.Collections.ListModule` static class; a consuming
  program `call`s `ListModule::fold` via a `MethodSpec` rather than inlining it.

The legs are separable because each package gets its own DLL (below), so a
type-only half-package is still a coherent unit — which is why the type could land
while the module still waited on the function representation.

## Naming / shape decisions

- **Package name ≠ namespace**: the package is `Vesper.List`, but it contributes
  type `List` and module `List` into namespace **`Vesper.Collections`** (shared
  with `Vesper.Array` / `Vesper.Seq` / `Vesper.Set`).
- **`depends-on` is `Vesper.Core` only.** The contract's `List.GetSlice:
  startIndex: int option * endIndex: int option` names `int option`, but the
  compiled `list.fs` has no `GetSlice`, so nothing in the build needs
  `Vesper.Option`. `depends-on` tracks the build dependency; `Vesper.Option`
  returns here if `GetSlice` is implemented.
- The module is **`[<RequireQualifiedAccess>]`** + **`ModuleSuffix`** so it shares
  the `List` name with the type (compiled name `ListModule`), matching the rest of
  the tree.
- Module functions are **not `inline`** (unlike FSharp.Core): the Vesper backend
  lowers each functional-argument application to `callvirt Fun::Invoke`.

## A separate `Vesper.List.dll`

`List` used to be compiled into `Vesper.Core.dll`. One impl DLL per package
makes it its own assembly: an emitted program that uses lists now carries
a `Vesper.List` `AssemblyRef` (for the `List` type + `Cons`/`Nil` factories +
`IsEmpty`/`Head`/`Tail`) *alongside* the `Vesper.Core` ref (for `Fun`). Both are
`Vesper.*` — no `FSharp.Core`. The provider's refs are `lazy`, so a program that
touches no list pins no `Vesper.List` ref. So the canonical sample's bundle ships
`Vesper.Core.dll` **and** `Vesper.List.dll`, no `FSharp.Core.dll`.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Option` / `Vesper.Printf`, this tree is not built by
`dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp` and walked into an
`IExternalSymbolProvider`; the `.fs` is compiled by our own backend. Parser
coverage is verified by golden `.parsed` snapshots committed next to each source
(`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`core-lib-architecture.md`](../XParsec.FSharp.SemanticAnalysis/docs/core-lib-architecture.md) — the per-package split this realises, and the dependency graph it sits in.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
- [`../Vesper.Option/README.md`](../Vesper.Option/README.md) — the sibling package whose split this mirrors.
