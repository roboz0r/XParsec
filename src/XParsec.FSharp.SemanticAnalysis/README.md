# XParsec.FSharp.SemanticAnalysis

Semantic analysis pipeline for the F# CST produced by `XParsec.FSharp`.

**Status:** scaffolding only. No pass is implemented yet.

## What this is

This project takes an immutable F# CST (from `XParsec.FSharp.Parser`) and runs a
sequence of focused passes that attach semantic information — name resolution,
type inference, unit-of-measure inference, region/escape analysis, validation —
to produce a frozen, immutable Typed AST (TAST).

The original specification lives in `../../semantic-analysis.md` at the repo
root. Architectural decisions made after that spec was drafted live in
[`docs/`](docs/) and are summarised below.

## Reading order

1. [`docs/architecture.md`](docs/architecture.md) — the big picture: CST in,
   side tables for in-flight mutation, freeze to TAST, one-way pass pipeline.
2. [`docs/nodekey.md`](docs/nodekey.md) — the 64-bit `NodeKey` used to index
   semantic side tables, including the synthetic-node discriminator.
3. [`docs/passes.md`](docs/passes.md) — the forward pipeline (Desugar →
   NameResolution → Unification → Regions → Validation → Freeze) and each
   pass's pre/post-condition contract.
4. [`docs/typevar.md`](docs/typevar.md) — the 3-axis `TypeVar` (type, units,
   region) and how SRTP / IWSAM bounds attach.

## Layout

```
NodeKey.fs              64-bit key, real vs synthetic
SemanticInfo.fs         TypeVar, RegionId, EscapeState, etc.
SideTables.fs           Dictionary<NodeKey, _> wrappers, freeze plumbing
UnionFind.fs            Algorithm J primitives, generic
Tast.fs                 Frozen output types
Passes/
  Desugar.fs            Writes Desugared slot, mints synthetic keys
  NameResolution.fs     Writes binding-resolution slot
  Unification.fs        Writes TypeVar slot, on-unified callbacks
  Regions.fs            Writes Region slot
  Validation.fs         Read-only: exhaustiveness, value restriction, mutability
Freeze.fs               CST + side tables -> TAST
Pipeline.fs             Top-level entry: runs passes in order
```

## What this is not (yet)

- Not a type checker. The unification step is a placeholder; Algorithm J is
  sketched in [`docs/typevar.md`](docs/typevar.md) but not implemented.
- Not a code emitter. Target-specific specialisation (`Phase 4.6` in
  the original spec) is downstream of `Freeze.fs` and lives in separate
  target-plugin projects, none of which exist yet.
- Not yet wired to consume a real CST. Each pass's entry point is stubbed.
