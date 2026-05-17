# XParsec.FSharp.SemanticAnalysis

Semantic analysis pipeline for the F# CST produced by `XParsec.FSharp`.

**Status:** small but real subset. The pipeline runs end-to-end on
expression-level F#; types are inferred and a frozen TAST is produced.
Regions and most of Validation are still no-ops.

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

## Current coverage

Expressions: `Const`, `Ident`, `LongIdentOrOp` (single-segment), `App`,
`InfixApp` / `PrefixApp` (named built-in operators), `Fun`, `LetOrUse` (`let`,
`let rec`, `let … and …`), `EnclosedBlock`, `IfThenElse` (with `elif` /
`else`), `Tuple`, `Sequential`, `TypeAnnotation`, `EmptyBlock` (`()`),
`While`, `ForTo`, `ForIn` (element typing deferred), `String` (text +
escape parts; interpolation holes stubbed), `Match`, `Function`.

Patterns: `NamedSimple`, `Wildcard`, `EnclosedBlock`, `Tuple`, `Const`,
`As` (alias name dropped). Nested tuple destructuring in `let` heads and
lambda parameters works.

Literals: bool, int, int64, byte, float (`IEEE64`), unit, string.

Diagnostics: unresolved-name errors, type mismatches, occurs check,
unknown-operator failures, `if … then` without `else`, `for-in` not-fully-modelled
info.

## What this is not (yet)

- Not a code emitter. Target-specific specialisation (`Phase 4.6` in
  the original spec) is downstream of `Freeze.fs` and lives in separate
  target-plugin projects, none of which exist yet.
- No generalisation. `let id = fun x -> x` types as `'a -> 'a` where `'a`
  stays unsolved; multiple uses at different types would currently fail.
- No SRTP / IWSAM resolution. The on-unified callbacks per
  [`docs/typevar.md`](docs/typevar.md) aren't wired yet.
- `Regions` and most of `Validation` are still no-ops.
- `TryWith` / `Object` / `Record` / `RecordClone` aren't traversed yet.
