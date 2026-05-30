# Vesper.Seq

The `Seq` module over `seq<'T>` (= `IEnumerable<'T>`) for **Vesper** (the
language; `XParsec.*` is the *compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type/module). Mirrors [`Vesper.Array`](../Vesper.Array/README.md)'s
`module Array`.

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `seq.fsi` | **contract** — the module signature, in namespace `Vesper.Collections` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `seq.fs` | **runtime impl** — the module bodies | this repo's own backend → `Vesper.Seq.dll` (BCL-only) |

A normal signature/implementation pair, both ours.

## Why this package exists

`Vesper.Set` consumes three `Seq` operations:

- `Seq.fold` — `set.fs:820` (`Set.Union(sets)` folds the sets together).
- `Seq.reduce` — `set.fs:823` (`Set.Intersection(sets)` reduces the sets).
- `Seq.truncate` — `set.fs:961` (`List.ofSeq (Seq.truncate 4 x)` peeks the first
  few elements).

`Seq.toArray` ships alongside for symmetry — the natural companion terminal
operation. A focused starter surface; the rest of the FSharp.Core `Seq` surface
is additive later, the same "grow the module additively" stance as
[`Vesper.Array`](../Vesper.Array/README.md) and [`Vesper.List`](../Vesper.List/README.md).

## Minimal reference impls — not the deforesting design (yet)

These four functions are **eager, explicit-enumerator reference impls**: each
pulls an `IEnumerator<'T>` from `source.GetEnumerator()` under a `use` (so the
enumerator is disposed) and drives it with `MoveNext` / `Current`. They are *not*
the zero-allocation, struct-chaining, JIT-deforesting `Seq` design described in
[`brainstorm-seq-module.md`](../XParsec.FSharp.SemanticAnalysis/docs/brainstorm-seq-module.md)
— that is a future sprint. The minimum surface unblocks `set.fs` now
([`vesper-set-sprint-phase-8.md`](../XParsec.FSharp.SemanticAnalysis/docs/vesper-set-sprint-phase-8.md) §8.4).

- `fold` / `reduce` / `toArray` are strict (a `while e.MoveNext()` loop).
- `truncate` is lazy — it returns a `seq { }` whose counted enumerator loop
  yields at most `count` elements, stopping early without draining `source`.
- `reduce` throws via `invalidArg` (Phase 3 sugar) on an empty sequence, matching
  the FSharp.Core surface.

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.Seq`, but it
  contributes the `Seq` module into namespace **`Vesper.Collections`** (not
  `Vesper.Seq`) — the same namespace `set.fs` lives in, so its `Seq.fold` /
  `Seq.reduce` / `Seq.truncate` calls resolve without an extra `open`.
- **No `Seq` *type*** — `seq<'T>` is the `IEnumerable<'T>` abbreviation declared
  in `Vesper.List/list.fsi`. This package adds only the module. The
  `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`
  gives it the compiled name `SeqModule` (matching the FSharp.Core surface) and
  lets it coexist with the `seq<'T>` abbreviation / `seq { }` builder.
- **The functional arguments are `Vesper.Fun`s** — each application lowers to
  `callvirt Fun::Invoke`.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Array`, this tree is not built by `dotnet`/`fsc`.
The `.fsi` is parsed by `XParsec.FSharp` and walked into an
`IExternalSymbolProvider`; the `.fs` is compiled by our own backend. Parser
coverage is verified by golden `.parsed` snapshots committed next to each source
(`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`vesper-set-sprint-phase-8.md`](../XParsec.FSharp.SemanticAnalysis/docs/vesper-set-sprint-phase-8.md) — §8.4, the step this package lands.
- [`brainstorm-seq-module.md`](../XParsec.FSharp.SemanticAnalysis/docs/brainstorm-seq-module.md) — the zero-allocation deforesting `Seq` design this reference surface defers.
- [`../Vesper.Array/README.md`](../Vesper.Array/README.md) — the sibling module package this mirrors.
- [`../Vesper.List/README.md`](../Vesper.List/README.md) — the base package supplying the `seq<'T>` / `ResizeArray<'T>` abbreviations.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on (`Fun`, `int`, `'T[]`).
