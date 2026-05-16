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

```fsharp
[<Sealed>]
type TypeVar() =
    member val Link    : SemType voption       = ValueNone with get, set
    member val Units   : MeasureTerm list      = []         with get, set
    member val Region  : RegionId              = RegionId.Unknown with get, set
    member val IfaceBounds : InterfaceBound list = []      with get, set
    member val SrtpBounds  : MemberSignature list = []     with get, set
    member val Parent  : TypeVar voption       = ValueNone with get, set  // union-find
    member val Rank    : int                   = 0         with get, set  // union-find
```

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

## SRTP and IWSAM bounds

F# has two species of ad-hoc polymorphism that have to be deferred:

- **SRTP** (`when ^a : (member Foo : unit -> int)`) — resolved when the
  concrete type of `^a` becomes known. The bound is the member signature.
- **IWSAM** (`when 'a :> ISomething<...>`) — resolved by trait/interface
  lookup on the concrete type. Maps cleanly to .NET 7+ interfaces with
  static abstract members, and to Rust traits.

Both live on `TypeVar` as lists. When `Unification` writes `Link`, it walks
both bound lists and dispatches the on-unified callbacks. Newly-discovered
bounds during the callback can fire more unifications — the iteration is
internal to the pass.

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

The mock provider plus `MockBuiltins` skirts this entirely for the tiny
subset — `(+) : int -> int -> int` is exposed as a monomorphic primitive
with no SRTP machinery. The full FSharp.Core (+) story comes online when we
start consuming real `FSharp.Core.dll`, which is firmly in the .NET-integration
phase, not the self-contained-file slice.

## Generalisation

After `Unification` finishes the main pass, any `TypeVar` whose `Link` is
still `ValueNone` and which is bound at a `let` boundary gets generalised
into a polymorphic parameter (`'a`). The value restriction (checked in
`Validation`) catches generalisation of mutable references.

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

These are deferred until a real implementation starts:

- **MeasureTerm representation.** Sorted list of `(unit, exponent)` pairs?
  A small `Dictionary<UnitName, int>`? Cost depends on how many measure
  variables a typical program uses — probably very few, in which case a
  flat sorted list wins on every dimension.
- **InterfaceBound vs SrtpBound: same type or two?** They have different
  resolution semantics (interface lookup vs member-signature lookup) but
  similar shape. Probably two distinct types; revisit if the validators end
  up duplicating logic.
- **RegionId allocation.** Sequential `int`? A `[<Struct>]` over `int`?
  Whether we need union-find on regions too (we shouldn't — they're
  inequality, not equality).
