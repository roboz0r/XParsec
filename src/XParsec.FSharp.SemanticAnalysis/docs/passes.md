# Passes

The semantic pipeline is strictly forward: each pass runs once, in order,
reading the slots its predecessors wrote and writing exactly one new slot.

## Pipeline

| # | Pass             | Writes               | Reads                | Notes |
|---|------------------|----------------------|----------------------|-------|
| 1 | `Desugar`        | `Desugared` table    | CST                  | Mints synthetic `NodeKey`s. Annotation-only — never mutates CST shape. |
| 2 | `NameResolution` | `Binding` table      | CST, `Desugared`     | Handles open-decls, shadowing, qualified lookups. |
| 3 | `Unification`    | `TypeVar` table      | CST, `Desugared`, `Binding` | Algorithm J. Fires on-unified callbacks for SRTP / IWSAM bounds; iteration is internal to this pass. |
| 4 | `Regions`        | `Region` table       | all prior            | Inequality-only escape analysis. |
| 5 | `Validation`     | (diagnostics only)   | all prior            | Read-only. Exhaustiveness, value restriction, mutability. |
| 6 | `Freeze`         | TAST                 | CST + all tables     | Single tree-to-tree projection. Discards side tables. |
| 7 | `ResolvedTypes`  | (diagnostics only)   | TAST + `Scheme`      | Read-only. Asserts every inline `SemType` zonks to a concrete shape or a quantified typar of the enclosing scheme. Catches latent inference bugs that would otherwise surface as broken IL. |

## Pass contracts

Each pass is a module with an explicit `run` function:

```fsharp
module Desugar =
    val run : ctx: PassContext -> file: ImplementationFile<SyntaxToken> -> unit
    // Postcondition: ctx.Desugared is populated for every CST node that has a
    // desugared form. Nodes without a desugared form are simply absent from
    // the table — the constraint generator falls through to the CST view.
```

The postcondition is what the next pass relies on. Keep it explicit at the
top of each pass file.

## Where `inline` fires

F#'s `inline` keyword is semantically observable: an `inline` function with
an SRTP-constrained type parameter dispatches against the caller's concrete
types. So:

- `NameResolution` records that a binding is `inline`.
- `Unification` checks the inline flag when resolving an SRTP bound. If the
  binding is inline, the bound becomes resolvable; if not, it's an error
  (deferred SRTP outside an inline binding).

This is the only place a downstream pass "depends on" a flag set earlier.
It's not an optimisation; it's part of unification's correctness. See
[architecture.md](architecture.md#where-inline-lives).

At the codegen boundary, `inline` also drives per-call-site body expansion.
`TDecl.Let` carries an `Inline` marker, the binding's body is retained in
the TAST, and an `inlineExpand` helper substitutes the caller's resolved
types into the body at each use site. This is the codegen-side companion
to the unification correctness above — without retained bodies, `inline`
bindings would lose the chance to dispatch through the constrained
generic typars that the Fun-style function representation relies on
([function-representation-plan](function-representation-plan.md)). See
[front-end-gaps-plan](front-end-gaps-plan.md) §C for the work that lands
the `Inline` marker and the expansion helper.

## Validation diagnostics

`Validation` is the only pass that doesn't write a side table. It reads
everything and emits diagnostics:

- **Pattern match exhaustiveness** — uses the resolved union-type info from
  `Unification` to build a decision tree per `match` and report uncovered
  cases.
- **Value restriction** — checks that no generalised `'a` ends up bound to a
  mutable ref cell.
- **Immutability** — checks that `<-` only targets bindings flagged
  `mutable` by `NameResolution`.

Diagnostics get accumulated in a `PassContext.Diagnostics` channel rather
than thrown. Earlier passes also emit into this channel for their own errors
(unresolved names, unification failures, etc.).

## Why no fixpoint at the pass level

It might look tempting to re-run earlier passes after a later one discovers
new information. For example: `Regions` might want to influence which trait
impl `Unification` chose (Rust-style lifetime-driven dispatch). The spec
explicitly avoids this by making regions inequality-only and downstream of
unification — a deliberate simplification. Adding bidirectionality would
turn the whole pipeline into a fixpoint, which is a much bigger architectural
commitment than the current design.

The one "feedback" mechanism — deferred constraint resolution inside
`Unification` — is contained to that single pass and doesn't escape into
pass-level iteration.
