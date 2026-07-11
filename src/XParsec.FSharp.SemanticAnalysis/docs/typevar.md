# TypeVar

This document expands on §2 of `semantic-analysis.md`. The original spec is
correct in intent; this file pins down the F#-specific representation choices.

## Three axes

Every expression node has, after `Unification` and `Regions` run, three pieces
of inferred information:

| Axis | What it carries          | Solved by   | Solved against    |
|------|--------------------------|-------------|-------------------|
| 1    | Semantic type (`SemType`)| Algorithm J | Equality          |
| 2    | Unit-of-measure exponent | Algorithm J | Abelian group eq. |
| 3    | Allocation region        | Region pass | Inequality (≤)    |

Axes 1 and 2 unify by equality. Axis 3 is partial-order: "the lifetime of A
must outlive the lifetime of B" produces inequality constraints solved by
least-upper-bound, not equality.

## Storage

The live definition is in [`SemanticInfo.fs`](../SemanticInfo.fs) and has grown
past the original sketch; read it there. In outline:

```fsharp
[<Sealed>]
type TypeVar() =
    member val Link   : SemType voption     = ValueNone         // solved? (union-find)
    member val Units  : MeasureTerm voption = ValueNone         // axis 2
    member val Region : RegionId            = RegionId.Unknown  // axis 3
    member val Level  : int                 = 0                 // Rémy's let-depth
    member val Parent : TypeVar voption     = ValueNone         // union-find
    member val Rank   : int                 = 0                 // union-find
    // Deferred obligations, all drained by `unify` when `Link` is set and
    // merged across a `union` by `migrateBounds`:
    member val Constraints      : SemanticConstraint list  = []  // `when 'a : equality`, `:> T`, …
    member val SrtpBounds       : MemberSignature list     = []  // `when ^a : (member …)`
    member val PendingDotAccess : DeferredMemberAccess list = [] // `x.Foo` on a still-free `x`
    member val Defaults         : SemType list             = []  // `default ^T : dynamic`
```

**Everything except `Parent` / `Rank` is authoritative only on the union-find
representative — call `UnionFind.find` before reading it.** That is the single
easiest mistake to make against this type.

A few choices worth noting:

- **Class, not record.** `TypeVar` participates in union-find and is mutated
  in place by `Unification`. Records-with-mutable-fields work, but a sealed
  class makes the identity semantics (each `new TypeVar()` is its own
  variable) read more naturally.
- **`voption` not `option`.** All mutable fields use `voption` to avoid
  per-field heap allocations on every parse. The semantic-info path is
  hot — fresh `TypeVar`s are minted per CST expression node.
- **Union-find on the `TypeVar` itself.** `Parent`/`Rank` live directly on
  the record. No separate `Dictionary<TypeVar, TypeVarRef>` indirection.
  `find` walks pointers; `union` rewires them.

## Deferred obligations (the on-unified callbacks)

A constraint on a *free* TyVar cannot be checked yet — there is nothing to check
it against. So it is parked on the variable and **drained when the variable is
solved**: when `Unification` writes `Link`, it walks the obligation lists and
dispatches. A `union` merges two variables' obligations onto the survivor via
`migrateBounds`. Newly-discovered obligations can fire further unifications; that
iteration is internal to the pass and never escapes into pass-level re-running
(see [architecture.md](architecture.md#pass-order-is-strictly-forward)).

This machinery is **wired**. What varies is how much of each obligation kind is
modelled:

- **`Constraints`** (`SemanticConstraint`) — the `when 'a : …` clauses. The
  trait-table subset is live: `equality`, `comparison`, `struct` / `not struct`,
  `: null` / `: not null`, plus `Coercion` (`:> T` subtype bounds, checked
  through the read-only `subsumes` relation). Deferred: `MemberTrait`,
  `DefaultConstructor`, `Enum`, `Unmanaged`, `Delegate`.
- **`SrtpBounds`** (`when ^a : (member Foo : unit -> int)`) — the field and the
  callback exist; the member-trait resolution behind them is the deferred
  `MemberTrait` case above. This is the real remaining SRTP gap.
- **`PendingDotAccess`** — `x.Foo` where `x`'s type is still free. Parked until
  the receiver is known, then resolved against record fields vs class members.
- **`Defaults`** — a `default ^T : dynamic` chain from an external symbol's
  declared defaults; applied at generalisation if nothing else pinned the
  variable.

**IWSAM** (`when 'a :> ISomething<…>`) rides the `Coercion` constraint. It maps
cleanly to .NET 7+ static-abstract interfaces, and to Rust traits.

### SRTP resolution and target capabilities

For SRTP bounds that resolve against built-in primitives (e.g. `^T + ^T` where
`^T = int32`), the resolution is not pure type-system reasoning. It depends on
**what the target can lower**. FSharp.Core's `(+)` is defined with
`when ^T : int32 = (# "add" x y : int32 #)` — a .NET-only clause. For a Rust
or JS target the same bound resolves through a different shim. See
[[project_inline_il_target_specific]] for the full rationale.

This means the on-unified callback for SRTP bounds is a two-step query:
1. `ctx.Provider.TryLookup` to find the symbol the bound names.
2. Consult the active target's primitive-lowering table to confirm it can
   handle this `^T`. If not, generate a different diagnostic ("`(+)` on
   `decimal` is not supported on the JS target") than a generic SRTP
   resolution failure.

Which is why the inline IL is **not modelled here** — the resolved operator's
compiled name drives target-specific dispatch, so it stays in the backend (see
the header of [`ExternalSymbols.fs`](../ExternalSymbols.fs)). Primitive identity
itself is **contract-sourced**: `int` / `string` resolve through the provider and
the compiling target's `.fsi` contract ([`Intrinsics.fs`](../Intrinsics.fs)),
never from a hardcoded name set in a pass.

## Generalisation

Rémy's levels. Each `TypeVar` is stamped with the **let-depth it was minted at**
(`Level`); `unify` lowers a variable's level when it becomes reachable from a
shallower scope, and `union` propagates the `min` of the two roots'. Generalising
a binding then means quantifying exactly those variables whose `Level` exceeds
the enclosing scope's — an O(1) test per variable, instead of scanning the type
environment for what's free in Γ.

The result is a `TypeScheme` (quantified vars + body) in the `Scheme` side table.
Each *use* of the name instantiates it with fresh variables at the current level,
so independent use sites don't share variables — the same freshen-per-use shape
`ExternalSymbols.instantiateSymbol` uses for provider-supplied symbols, but over
the finitely many `'a`s a user-written `let` produces. Quantified variables stay
live in the union-find graph; they are simply no longer free with respect to the
outer scope.

**Value restriction is split across two passes**, deliberately:

- The **gate** is in `Unification` (`shouldGeneralise` skips a binding carrying a
  `mutableToken`), so a mutable binding never gets a scheme and every use unifies
  against the one shared variable.
- The **diagnostic** is in `Validation`, which runs late enough that every use
  site has already had its chance to pin a free variable. Emitting at `let`-time
  would fire prematurely on `let mutable r = []` — legal, because a later
  `r <- [1]` pins it.

## Why not just FCS's `TyparData`?

We could lift FCS's existing `TyparData` shape and avoid this design work
entirely. The reason we don't:

- FCS's typars carry compiler-internal state we don't need (display names
  for error messages, source ranges in FCS's own range type, attached XML
  doc info).
- We want a clean integration with the region axis, which FCS doesn't have.
- FCS encodes typars inside its own typed AST. We're producing a different
  typed AST, so the alignment cost is high.

The bones of the algorithm — union-find, occurs check, generalisation,
deferred bounds — are direct ports of FCS / TypeShape conventions. The
storage layout differs.

## Open questions

- **Bounds on a *quantified* TyVar.** Generalisation quantifies a TyVar that may
  carry `Constraints` / `SrtpBounds`. `instantiate` should re-instantiate those
  obligations alongside the fresh variable, or a use site of a constrained
  generic loses its constraint. Under-exercised while `MemberTrait` is still
  deferred — revisit when SRTP member resolution lands.

Answered since this doc was written, kept as a record of the reasoning:

- ~~**MeasureTerm representation.**~~ The flat sorted list won, as predicted:
  `MeasureTerm` is a sealed class over a normalised, name-sorted
  `(string * Rational) list` — zero exponents dropped, so structural list
  equality *is* abelian-group equality.
- ~~**InterfaceBound vs SrtpBound: same type or two?**~~ Two, and the split fell
  differently than expected. Interface/subtype bounds became the `Coercion` case
  of `SemanticConstraint` (checked via `subsumes`), sitting alongside the other
  `when 'a : …` clauses rather than in a type of their own; `SrtpBounds` stayed
  separate as `MemberSignature list`.
- ~~**RegionId allocation.**~~ A `[<Struct>]` over a sequential `int`, with
  `Unknown = -1`. No union-find on regions — they are an inequality (partial
  order), solved by least-upper-bound propagation, not by merging equivalence
  classes.
