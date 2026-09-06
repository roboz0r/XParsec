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

The live definitions are in [`SemanticInfo.fs`](../SemanticInfo.fs) and
[`TypeStore.fs`](../TypeStore.fs); read them there. A metavar **IS** its
`TyVarId` — a dense, monotone, per-file `int<tyVarId>` minted by
`TypeStore.NewTypeVar`; `SemType.TyVar` carries that id directly, with no heap
box. All metavar state lives id-indexed on the per-file `TypeStore` (one instance
per `PassContext`).

`TypeStore` holds, keyed by `TyVarId`:

- **Union-find structure + region** — `parent`/`rank` parallel arrays
  (`parent.[i] = i` marks a root), read/written via `store.Parent`/`SetParent`,
  `Rank`/`SetRank` on a **raw `TyVarId`** (`find` legitimately walks non-roots).
  Plus the write-once `region` cell (`store.Region`/`SetRegion`, raw `TyVarId` — not
  migrated on union, so every node has one valid cell).
- **Root-authoritative cells** — `level`/`link`/`units` (Rémy level, the solution,
  the measure) and the deferred-obligation side-tables `store.Constraints` /
  `Defaults` (grow-only lists) / `Srtp` / `Pda` (grow-only lists + a reference-keyed
  `solved` set; they replace the former on-node `Constraints`/`Defaults`/`SrtpBounds`/
  `PendingDotAccess` slots). These are meaningful ONLY on the union-find
  representative, and the accessors enforce it: they take a **`Rep`**, not a raw
  `TyVarId`.

**The representative invariant is correct-by-construction, not a convention.**
`Rep` is a `[<Struct>]` single-case wrapper over a `TyVarId` whose case is `private`
to `TypeStore.fs`; the SOLE producer is `UnionFind.find` (which lives in that file
precisely so it can mint one). A root-authoritative read/write therefore *cannot* be
spelled without a value that provably came from `find` — "read
`link`/`level`/`units`/an obligation off a non-root" stops type-checking rather than
silently returning a stale or empty cell. Project `rep.Id` to key a `TyVarId` table
or rebuild a `SemType` (`TyVar rep.Id`).

A few choices worth noting:

- **Raw id, not a boxed handle.** `SemType.TyVar of TyVarId` — the id *is* the
  handle, so `SemType` is fully value-comparable (no reference-identity node) and the
  metavar-keyed dictionaries key by the dense int structurally. Within one file ids
  are unique, so structural int equality IS variable identity: a `TypeStore` is
  per-`PassContext` = per file, and `freeze` erases every surviving `TyVar`, so ids
  from two files never meet in one table.
- **Dense arrays, not per-object slots.** State is id-indexed store arrays —
  cache-friendly, and the immutable parts are internable. `voption` cells (`link` /
  `units`) avoid per-entry heap allocation on the hot path.
- **Union-find in the store.** `parent` / `rank` are store arrays; `find` walks and
  path-compresses them (handing back a `Rep`) and `union` rewires them. The id *is*
  the index — no `Dictionary` indirection.

## Deferred obligations (the on-unified callbacks)

A constraint on a *free* TyVar cannot be checked yet — there is nothing to check
it against. So it is parked on the variable and **discharged when the variable is
solved**: when `Unification` sets the root's `link`, it walks the obligation
side-tables and dispatches. A `union` folds the loser's obligations onto the
survivor via one associative join per family at the store's union seam (replacing
the former `migrateBounds`). Newly-discovered obligations can fire further unifications; that
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
  the object argument is known, then resolved against record fields vs class members.
- **`Defaults`** — a `default ^T : dynamic` chain from an external symbol's
  declared defaults; applied at generalisation if nothing else pinned the
  variable.

**IWSAM** (`when 'a :> ISomething<…>`) is represented by the `Coercion`
constraint. It maps cleanly to .NET 7+ static-abstract interfaces, and
to Rust traits.

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

- The **gate** is in `Unification` (`generalises` skips a binding carrying a
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

Resolved since this doc was written, kept as a record of the reasoning:

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
