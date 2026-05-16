# NodeKey

Every entry in every semantic side table is keyed by a `NodeKey` — a 64-bit
value that identifies a CST node (real or synthetic).

## Wire format

```
bit 63        bit 62..32                      bit 31..0
+----------+---------------------------------+----------------------+
| syn:1    | kind:31                         | offset:32            |
+----------+---------------------------------+----------------------+
```

- **`offset` (low 32 bits)** — for *real* nodes, the source character offset
  of the node's first token. The parser already stores this as
  `SyntaxToken.StartIndex` (a plain `int`). 32 bits is plenty:
  `XParsec` is in-memory only and the `Reader` is 32-bit ([[project_in_memory_only]]).

- **`kind` (bits 32..62)** — an enum value identifying the CST node type
  (`Expr.Application`, `Pat.LongIdentPat`, `Type.FunctionType`, …). 31 bits
  is gross overkill; a `uint16` would do. We keep the field 31 bits wide so
  the syn flag can have its own bit and the whole thing packs into 64 bits
  without bit-twiddling on the offset half.

- **`syn` (bit 63)** — 0 for real nodes, 1 for synthetic.

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
kind = 31-bit "synthetic kind" — typically the kind of the *desugared* node
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

1. Use a per-spawning-construct counter packed into the low bits of the kind
   field. We have 31 bits, plenty of headroom.
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

## Dictionary performance

`NodeKey` is a `[<Struct>]` `uint64` wrapper. Equality is integer equality;
hashing is a single multiplicative scramble (or just `GetHashCode` on the
underlying `uint64`, which already scrambles). No boxing, no virtual calls,
no string operations. The dictionary lookups are O(1) and effectively free
relative to the work being done by each pass.
