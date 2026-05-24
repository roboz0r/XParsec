# Vesper.Option

The `option` type and the `Option` module for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type). Carved out of `Vesper.Core`'s `core-types.fsi`.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `option.fsi` | **contract** — the type + `Option` module signatures, in namespace `Vesper` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `option.fs` | **runtime impl** — the union + module bodies | this repo's own backend → `Vesper.Option.dll` (BCL-only) |

A normal signature/implementation pair, both ours. Today `option.fs` is a
partial, growing subset of the contract (the array/list/`Nullable`/obj
conversions and `map2`/`map3` are contract-only for now) — expected while the
self-hosting ladder is climbed, the same stance as `Vesper.Core`'s `List`.

## Two legs (package-split-plan PS3)

- **Type leg** — the `Option<'T>` struct union (with `Value`/`IsSome`/`IsNone`
  and the static `Some`/`None`/`op_Implicit`). Does not need `Fun`, but it does
  need **struct-union emit** in the backend: the rung-2 union path currently
  emits reference classes, so value-typed unions are the backend follow-up this
  representation implies.
- **Module leg** — `Option.map`/`bind`/`fold`/… are overwhelmingly higher-order,
  so they ride **R1** (the `Fun`-not-`FSharpFunc` cutover). Until R1 the module
  leg can sit on a C# interim per PS3.

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.Option`, but it
  contributes type `Option` and module `Option` into namespace **`Vesper`** (not
  `Vesper.Option`).
- **`Option` is a struct; `None` is the zero-initialized struct** — Vesper does
  **not** inherit F#'s reference-typed, `UseNullAsTrueValue` (None = `null`)
  representation. `Option<'T>` is a value type, so `default(Option<'T>)` is
  `None`, an array of options starts as all-`None`, and neither `None` nor `Some`
  allocates on the heap (Rust/Swift-style). The price is the usual struct
  trade-off: `Option<'T>` is copied by value (sized `'T` + tag) and boxes if it
  crosses an `obj` boundary. The contract fixes only "struct whose default is
  `None`"; the exact layout is a backend decision.
- The module is **`[<RequireQualifiedAccess>]`** + **`ModuleSuffix`** so it
  shares the `Option` name with the type (compiled name `OptionModule`), matching
  `Vesper.Core`'s `List`.
- Module functions are **not `inline`** (unlike FSharp.Core): the Vesper backend
  lowers each functional-argument application to `callvirt Fun::Invoke` rather
  than expanding at the call site, the same choice as `List.fold`.

## `ValueOption` is out of scope — and now redundant

`ValueOption`/`voption` still live in `Vesper.Core`'s `core-types.fsi`. With
`Option` itself now a struct (zero-init `None`, no allocation), `ValueOption` is
**the same representation under a second name** — F#'s reason for a separate
struct option (its default option being a reference type) does not apply to
Vesper. `ValueOption` is therefore a candidate for removal; left in place for now
as a separate decision.

## Decomposition status & the `GetSlice` forward-reference — resolved

Extracting `Option` out of `core-types.fsi` briefly left one textual dangle: the
list's `GetSlice: startIndex: int option * endIndex: int option` named a type
owned by `Vesper.Option`, and `Vesper.Core` could not depend back on
`Vesper.Option` without a cycle. This is now **resolved**: `List` has moved to its
own `src/Vesper.List/` package, which `depends-on` `Vesper.Option`, so
`GetSlice`'s `int option` resolves cleanly with no cycle (Core ← Option ← List).

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Printf` / `XParsec.FSharp.Lib`, this tree is not
built by `dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp` and walked into
an `IExternalSymbolProvider`; the `.fs` is compiled by our own backend once the
self-host ladder lands. Parser coverage is verified by golden `.parsed` snapshots
committed next to each source
(`test/XParsec.FSharp.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`minimal-core-lib-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/minimal-core-lib-plan.md) — the single-tree core this is split out of.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on; `Fun`, `unit`, the contract/impl mechanics reused here.
