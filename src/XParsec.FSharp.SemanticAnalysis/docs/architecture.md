# Architecture

This document captures the design decisions made *after* the original
`semantic-analysis.md` brainstorm — the ones that shape the project's physical
layout and pass contracts.

## CST in, TAST out, side tables in between

The `XParsec.FSharp` parser produces an **immutable** CST whose nodes preserve
source tokens (`LongIdent.Dots`, exact whitespace via attached trivia, etc.).
That fidelity is load-bearing for tooling (formatter, linter, syntax
highlighting) and we don't want to lose it.

So the rules are:

- The CST is **not mutated** by this project. No mutable fields are added to
  CST records. No "promise to only touch this during analysis" hatch.
- All in-flight semantic information lives in **side tables** keyed by
  [`NodeKey`](nodekey.md). One table per kind of information (resolved
  binding, `TypeVar`, `RegionId`, etc.). See `SideTables.fs`.
- When all passes have run, `Freeze.fs` projects the CST + side tables into a
  brand-new immutable [`Tast`](../Tast.fs) tree. That projection is the
  *only* tree-to-tree transformation in the whole pipeline.

This is the same shape Roslyn and FCS use: keep the syntax tree pure, attach
semantic info alongside, and produce a separate bound/typed tree only when
needed.

## Why not a "working tree" wrapper?

An alternative we considered: build a parallel `WorkingExpr { Cst; mutable
Desugared; mutable TypeVar; ... }` tree eagerly, mutate in place, freeze at the
end. Discarded because:

- It allocates a wrapper per CST node whether or not that node carries
  semantic info. Many CST nodes (literals, trivia-heavy wrappers) carry
  nothing.
- It muddies the "immutable at rest" claim — the wrapper *is* the working
  tree, and downstream code has to know which fields are safe to read when.
- Side tables compose better: adding a new pass means adding a new
  `Dictionary<NodeKey, NewInfo>`, not extending a wrapper record everywhere.

The cost is a hash lookup per semantic query. With the 64-bit `NodeKey` packed
into a value the JIT can hold in a register, that's noise.

## Annotation-only desugaring

Desugaring (`Passes/Desugar.fs`) **does not rewrite the CST shape**. When it
sees `x |> f`, it does *not* replace the `Pipeline` CST node with an
`Application` node. Instead it writes a `DesugaredForm` entry into a side
table, keyed by the `Pipeline` node's `NodeKey`. The constraint generator and
later passes read through that side table.

Consequences:

- The CST view stays queryable at every point during analysis (hover semantics
  on a real-source span work whether or not desugaring has happened yet).
- Pass contracts are clean: "this pass reads slot X, writes slot Y". No pass
  destroys information.
- Synthetic nodes (CE method calls, comprehension expansions, the body of an
  expanded `for-in-do`) have no source position, so they get **synthetic
  NodeKeys** — see [nodekey.md](nodekey.md).

## Pass order is strictly forward

The pipeline order is:

1. **Desugar** — writes the `Desugared` side table; mints synthetic NodeKeys.
2. **NameResolution** — writes the `Binding` side table.
3. **Unification** — writes the `TypeVar` side table; runs the on-unified
   callbacks for deferred SRTP / IWSAM constraints inside its own fixpoint.
4. **Regions** — writes the `Region` side table.
5. **Validation** — read-only; emits diagnostics.
6. **Freeze** — builds the TAST from CST + all side tables.

Each pass's contract:

- **Precondition:** the side tables written by previous passes are populated.
- **Postcondition:** this pass's side table is populated.

No pass re-runs an earlier pass. The only "feedback" is *inside* `Unification`:
when a `TypeVar` is solved, deferred SRTP/IWSAM checks fire and may unify
further variables. That iteration is contained.

### Where `inline` lives

F#'s `inline` is semantically observable (it lets SRTPs dispatch against the
caller's concrete types), so it has to be handled inside `Unification`, not in
a downstream optimisation pass. We deliberately carve `inline` out as the
only "optimisation" that has to live in semantic analysis. Everything else
(CSE, closure conversion, lambda lifting, dead code elim) consumes the frozen
TAST and is out of scope here.

## What's out of scope

- **Target-specific lowering.** `Phase 4.6` in `semantic-analysis.md` (Convert
  `LocalStack` closures to `ref struct` for .NET, `&T` for Rust, etc.)
  consumes the frozen TAST. It will live in separate target-plugin projects
  when those exist.
- **Incremental recompilation.** The `NodeKey` design supports it (a CST edit
  invalidates a known range of keys), but no incremental machinery is built.
- **Generic constraint solver.** The `TypeVar` and union-find code here is
  F#-specific. If a second language ever wants the same solver, we'll
  extract a `src/XParsec.SemanticAnalysis` core then. Not before.
