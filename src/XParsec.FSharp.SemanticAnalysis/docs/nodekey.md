# NodeKey

Every entry in every semantic side table is keyed by a `NodeKey` — a 64-bit
value that identifies a CST node (real or synthetic).

This document describes the **analysis regime**: `NodeKey` as a content address,
recomputable from any CST node with no side index. That representation is
load-bearing through the whole semantic pipeline. It is deliberately *not* what
the frozen artifact uses — after freeze, identity becomes positional and the
64-bit key dissolves. See § *Analysis identity vs. frozen identity* below, and
`TastPoolTypes.fs`'s header for the frozen regime in full.

## Wire format

```
bit 63        bit 62..48           bit 47..32        bit 31..0
+----------+----------------------+-----------------+----------------------+
| syn:1    | reserved:15          | kind:16         | offset:32            |
+----------+----------------------+-----------------+----------------------+
```

- **`offset` (low 32 bits)** — for *real* nodes, the source character offset
  of the node's first token. The parser already stores this as
  `SyntaxToken.StartIndex` (a plain `int`). 32 bits is plenty:
  `XParsec` is in-memory only and the `Reader` is 32-bit ([[project_in_memory_only]]).

- **`kind` (bits 32..47)** — a `uint16` enum value identifying the CST node
  type (`Expr.Application`, `Pat.LongIdentPat`, `Type.FunctionType`, …).
  `uint16` matches `NodeKind`'s underlying representation and gives us 65535
  distinct kinds, an order of magnitude more than the full F# grammar needs.

- **`reserved` (bits 48..62)** — always zero today. Reserved for a
  per-spawning-construct counter to disambiguate synthetic nodes if we ever
  hit a same-kind collision (see "Synthetic NodeKeys" below). `Kind` reads
  only the low 16 bits of the kind window, so silently ignores reserved
  bits when they get populated.

- **`syn` (bit 63)** — 0 for real nodes, 1 for synthetic.

## Choice of "offset" per CST construct

The offset isn't always the node's leftmost token. For nested same-kind
constructs starting at the same physical position, naive "use the first
token" produces collisions. Two cases hit this in practice:

- **`InfixApp` keys on the operator token**, not the left subexpression.
  Without this, `a + b + c` (parsed left-assoc as `(a + b) + c`) gives the
  outer and inner `InfixApp` the same `(offset, kind)` — both start at `a`'s
  offset, both are `ExprInfixApp`. Operator-precedence chains like
  `(x > 0) && (x < 100)` hit the same collision (outer `&&` and inner `>`
  both at the leftmost `x`'s offset). Using the operator's offset
  disambiguates because each binary operator occupies a distinct source
  position.
- **`PrefixApp` keys on the operator token**, which is also its leftmost
  token. No special handling needed but worth noting the symmetry.

When adding new CST cases that could nest at the same offset (e.g. `TypeApp`
on `Ident`), pick a discriminating token rather than blindly using the
leftmost. See `CstKeys.firstTokenOfExpr` for the current pattern.

## Why we need both `offset` and `kind`

Two different CST node types can legitimately start at the same source offset.
For example, in `let x = 1`, the `LetBinding` node and the `Pattern` for `x`
both start at the `let` keyword's offset (the pattern logically starts at `x`,
but if we key on "the node's first token's StartIndex" both reduce to the same
number under some encodings).

Pairing `offset` with `kind` makes the key unique without us having to be
clever about which token counts as "the start" for each node type. The cost
is having to enumerate every CST node kind in one big enum — see
`NodeKey.fs`.

## Synthetic NodeKeys

When `Desugar` produces a synthetic node (e.g. the `Bind` method call that
replaces `let!` inside a CE), that node has no source position. Its key is:

```
syn = 1
kind = 16-bit "synthetic kind" — typically the kind of the *desugared* node
offset = the offset of the *construct that spawned it*
```

The "spawning offset" keeps a synthetic node locality-grouped with its source
construct: when you sort side-table entries by key, synthetic nodes for a
given construct cluster next to it. That helps both debugging (hover over a
real expression and see the synthetics it generated) and any future
incremental-invalidation logic.

We don't allocate a sub-counter for multiple synthetics sharing the same
spawning offset *and* kind, because in practice that doesn't happen — each
spawning construct produces synthetics of distinct kinds (an `Application`, a
`Lambda`, an `Identifier`, …). If we ever do hit a collision, options are:

1. Use a per-spawning-construct counter in the 15 reserved bits between
   `kind` and `syn`. Update `Kind` to mask appropriately and the counter
   becomes a third coordinate.
2. Maintain a separate `Dictionary<NodeKey, NodeKey list>` overflow table.

Option 1 is simpler; option 2 keeps the common-case key narrow.

## Why not just allocate sequential IDs?

We could mint fresh `int` IDs as we walk the CST. We don't, because:

- Sequential IDs require a separate `CstNode -> int` lookup. The 64-bit
  derived key needs no lookup — given a CST node, you can compute its key
  directly from its first token's `StartIndex` and its node-kind tag.
- Sequential IDs aren't stable across edits. The derived key *is* stable for
  any node that didn't move: a CST edit that doesn't shift offsets keeps all
  unaffected NodeKeys valid. That's the cheapest possible foundation for an
  incremental compiler, should we ever want one.

The trade-off is that re-parsing a file *does* invalidate every key whose
offset changed — but you'd be invalidating semantic info for those spans
anyway.

## Analysis identity vs. frozen identity

The argument just above ("don't allocate sequential IDs") holds only *during
analysis*. After `freeze` it inverts, and both halves matter:

- **During analysis**, consumers hold a bare CST token and must correlate it
  against the shared side tables. The derived `(offset, kind)` key lets them do
  that by *computing* the key — no `CstNode → id` map to build or thread. A
  sequential ID would require exactly that map. The 64 bits earn themselves here.

- **After freeze**, the opposite is true: nothing recomputes keys from tokens
  (the frozen tree carries its own identities), so the property the derived key
  buys is no longer needed — and it costs. `freeze` therefore assigns each
  distinct `NodeKey` a **dense sequential id** (a pool index), and:
  - **`kind` dissolves.** It existed only to keep `(offset, kind)` unique and to
    let a reference name a def by value. Distinct pool slots are already unique,
    and references name defs by dense id, so no frozen consumer reads
    `NodeKey.Kind` — in fact the `.Kind` accessor is read nowhere in logic (only
    in `ToString`); pre-freeze, kind acts purely through full-key equality in
    `Map<NodeKey,_>` lookups. `kind` is a *pre-freeze content-address* freeze drops.
  - **`offset` demotes from identity to naming data.** Codegen's only use of a
    key's bits is `boundVarName` (source slicing / `_s<n>` synthesis). Freeze keeps
    the offset (or a synthetic's `NameIndex`) as node-local data, sourced from the
    boundVar's own `tok` where present, so emitted names stay byte-identical — but
    it is no longer part of *identity*.
  - **Edges stay, shrunk.** Half of all `NodeKey` fields are resolved references
    (`TExpr.Var.binding`, `Map<NodeKey,_>` keys). Those are irreducible name-
    resolution output; freeze keeps them, as dense ids rather than 64-bit keys.

So the two regimes are complements, not a contradiction: **content key with role
during analysis** (role = the CST case; see `CstKeys.ofExpr`/`ofPat`),
**positional identity after freeze** (role dissolves into pool position).

## Dictionary performance

`NodeKey` is a `[<Struct>]` `uint64` wrapper. Equality is integer equality;
hashing is a single multiplicative scramble (or just `GetHashCode` on the
underlying `uint64`, which already scrambles). No boxing, no virtual calls,
no string operations. The dictionary lookups are O(1) and effectively free
relative to the work being done by each pass.
