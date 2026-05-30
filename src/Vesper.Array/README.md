# Vesper.Array

The `Array` module over the intrinsic `'T[]` type for **Vesper** (the language;
`XParsec.*` is the *compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type/module). Mirrors [`Vesper.List`](../Vesper.List/README.md)'s
`module List`.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `array.fsi` | **contract** — the module signature, in namespace `Vesper.Collections` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `array.fs` | **runtime impl** — the module bodies | this repo's own backend → `Vesper.Array.dll` (BCL-only) |

A normal signature/implementation pair, both ours.

## Why this package exists

`Vesper.Set` consumes two `Array` operations:

- `Array.zeroCreate` — `set.fs:713` (`toArray` allocates the destination array).
- `Array.fold` — `set.fs:728` (`ofArray` folds elements into the set).

A focused starter surface; the rest of the FSharp.Core `Array` surface is
additive later, the same "grow the module additively" stance as
[`Vesper.List`](../Vesper.List/README.md) and [`Vesper.Result`](../Vesper.Result/README.md).

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.Array`, but it
  contributes the `Array` module into namespace **`Vesper.Collections`** (not
  `Vesper.Array`) — the same namespace `set.fs` lives in, so its `Array.fold` /
  `Array.zeroCreate` calls resolve without an extra `open`.
- **No `Array` *type`** — the array type `'T[]` is the intrinsic declared in
  `Vesper.Core/prim-types-min.fsi`. This package adds only the module. The
  `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`
  gives it the compiled name `ArrayModule` (matching the FSharp.Core surface)
  and lets it coexist with the BCL `System.Array` name.
- **`zeroCreate` is open-coded over the `newarr` IL intrinsic** — the same
  `(# "..." #)` surface the arithmetic / comparison operators use
  (`Vesper.Core/ops-platform.fs`, `Vesper.Comparison/comparison.fs`), so it
  carries no dependency beyond the intrinsic.
- **`fold` is a counted index loop** over the array (`for i = 0 to
  array.Length - 1`); `folder` is a `Vesper.Fun`, so each application lowers to
  `callvirt Fun::Invoke`.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.List`, this tree is not built by `dotnet`/`fsc`.
The `.fsi` is parsed by `XParsec.FSharp` and walked into an
`IExternalSymbolProvider`; the `.fs` is compiled by our own backend. Parser
coverage is verified by golden `.parsed` snapshots committed next to each source
(`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`vesper-set-sprint-phase-8.md`](../XParsec.FSharp.SemanticAnalysis/docs/vesper-set-sprint-phase-8.md) — §8.2, the step this package lands.
- [`../Vesper.List/README.md`](../Vesper.List/README.md) — the sibling module package this mirrors.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on (the `'T[]` intrinsic + `Fun`).
