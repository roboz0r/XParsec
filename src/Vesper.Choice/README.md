# Vesper.Choice

The `Choice<'T1, 'T2>` type for **Vesper** (the language; `XParsec.*` is the
*compiler*), packaged standalone per
[`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md)
(PS1: one package per type). Mirrors [`Vesper.Result`](../Vesper.Result/README.md).

## Two artifacts that must agree

| File | Role | Consumed by |
|---|---|---|
| `choice.fsi` | **contract** — the type signature, in namespace `Vesper` | `XParsec.FSharp` parser → `IExternalSymbolProvider` (front-end symbol resolution) |
| `choice.fs` | **runtime impl** — the union body | this repo's own backend → `Vesper.Choice.dll` (BCL-only) |

A normal signature/implementation pair, both ours.

## Why this package exists

`Vesper.Set` (`set.fs:440, 441, 446, 773, 1064`) consumes `Choice<'T1, 'T2>`:
`partition1With` births a `Choice` from `partitioner k` and consumes it in the
very next `match`. The value never escapes, never gets stored, and runs once per
set element — which is exactly the case where a struct wins (zero heap
allocations per element vs one allocation + GC pressure as a class).

## Naming / shape decisions

- **Package name ≠ namespace** (PS5): the package is `Vesper.Choice`, but it
  contributes type `Choice` into namespace **`Vesper`** (not `Vesper.Choice`).
- **`Choice` is a struct** — the identical shape to `Vesper.Result`'s `Result`
  (a two-case heterogeneous DU, one payload per arm). `Result` already ships as
  `[<Struct>]`, so the backend's two-case heterogeneous struct-DU support is
  already proven; there is no capability gap. The price is the usual struct
  trade-off: the value carries `sizeof 'T1 + sizeof 'T2 + tag`, is copied by
  value, and boxes if it crosses an `obj` boundary — trivial here, where the arms
  are reference types (two words + tag, cheaper to copy than to allocate).
- **`Data`, not `State` — opt-in `[<StructuralComparison>]`**: `Choice` is
  unconditionally structurally equatable and structurally comparable *iff its type
  arguments are* (the `Comparison.Structural ⇒ Equality.Structural` invariant
  holds), exactly as `Result`.
- **Named-field cases** `Choice1Of2 of Choice1Of2: 'T1` /
  `Choice2Of2 of Choice2Of2: 'T2`, matching the FSharp.Core surface
  (compiled name `FSharpChoice`2``).
- **No `Choice` module** today — the sole consumer (`set.fs`) uses only the
  constructors and pattern matching. Combinators are additive later, the same
  "grow the module additively" stance as `Vesper.Result`.

## Follow-ups (deferred)

- **Higher-arity `Choice`.** Once the 2-arity case is fully proven (ships,
  round-trips through equality/comparison, consumed in `set.fs`), add
  `Choice<'T1, …, 'T16>` to match the FSharp.Core surface.
- **SROA for non-escaping `Choice`.** A private, non-escaping value is a
  candidate for scalar replacement of aggregates once escape analysis lands — a
  strictly-internal codegen optimisation, gated on the value not crossing a
  public boundary.

## No `.fsproj`

Like `Vesper.Core` / `Vesper.Option` / `Vesper.Result`, this tree is not built by
`dotnet`/`fsc`. The `.fsi` is parsed by `XParsec.FSharp` and walked into an
`IExternalSymbolProvider`; the `.fs` is compiled by our own backend. Parser
coverage is verified by golden `.parsed` snapshots committed next to each source
(`test/Vesper.Tests/VesperCoreContractTests.fs`).

## Cross-references

- [`package-split-plan.md`](../XParsec.FSharp.SemanticAnalysis/docs/package-split-plan.md) — the per-package split this realises (PS1/PS2/PS3/PS5).
- [`../Vesper.Result/README.md`](../Vesper.Result/README.md) — the sibling package this mirrors.
- [`../Vesper.Core/README.md`](../Vesper.Core/README.md) — the base package this depends on.
