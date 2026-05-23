# TAST string-allocations plan

Brainstorm / design note, not yet scheduled. Triggered by the printf work
(front-end-gaps-plan §B): re-parsing a format specifier used to copy the
token text out of the source (`ctx.NameOf`) before parsing it. That copy is
now gone — `Lexed.GetTokenReadable` / `PassContext.ReadableOf` hand back a
`ReadableString` view over the original source, and the placeholder grammar
parses in place. The open question is whether the same "reference the
source, materialise on demand" idea should generalise to the strings the
**TAST itself** stores.

## The idea (as raised)

> The TAST could just store token references and generate `Span<char>` /
> `ReadableString` on demand from the original file source string.

Concretely: replace the `string` fields scattered across `Tast.fs`
(field/case/member/type names, the compiled name on `External`, the value
on `TConstValue.String`) with a lightweight source reference (a token
index, or `struct (start, length)` plus a file id), and only build a real
`string` when a consumer actually needs one.

## Where the TAST's strings actually come from

Not all of them are the same, and the distinction decides what's worth
doing. Three buckets:

1. **Synthesised constants** — `"op_Addition"`, `"Cons"` / `"Nil"`,
   `"Microsoft.FSharp.Collections.ArrayModule.OfList"`, the `PrintfFormat`
   name, the primitive type names (`"int"`, `"unit"`). These are F# string
   *literals*, so the runtime interns them: one allocation for the whole
   program, regardless of how many nodes carry them. **Token references buy
   nothing here** — there is no source span to point at, and nothing to
   save.

2. **Source-derived identifiers** — record field names, DU case names,
   class member/method/property names, user record/union/class type names.
   Today each occurrence is a fresh substring via `ctx.NameOf`, so the same
   field `X` accessed 100 times is 100 identical heap strings. This is the
   real cost, and the bucket the idea targets.

3. **String literal values** (`TConstValue.String`) — these are *not*
   verbatim source slices: the lexer/Freeze decode escapes (`\n` → newline,
   `A` → `A`). A raw `ReadableString` view over the source would hand
   back `\n` (two chars), not the newline. So this bucket can't be a plain
   source slice; at most it can be a *lazily decoded + cached* value.

`TExpr.Var` already sidesteps the problem: it stores a `NodeKey`, not a
name, and consumers resolve the name through the binding site. That's the
existing precedent the idea wants to extend to the other identifier-bearing
nodes.

## The catch: lifetime / ownership

`Tast.fs` opens with a deliberate contract — *"The TAST is sharable; the
CST + side tables are scoped to one compilation."* Today the TAST is
self-contained: once Freeze runs you can throw away the source string, the
`Lexed` table, and every side table.

A token reference breaks that. To turn `struct (start, length)` back into
text you need the original source string retained for as long as the TAST
is alive — and in a multi-file world, you need to know *which* file's
source, so the reference grows to `(fileId, start, length)` and something
must own the `fileId -> source` map for the lifetime of every TAST that
references it. NodeKey already encodes a source offset, but it's used as an
*identity* key, never dereferenced for text; promoting source offsets to
"fetch the bytes later" is a genuinely new coupling, not just a wider use
of what's there.

So the design isn't free: it trades per-identifier string allocations for a
retained-source dependency and a heavier reference type. Whether that's a
good trade depends entirely on (a) how much identifier allocation actually
costs and (b) how often the TAST outlives the source in practice.

## A lighter alternative: intern source-derived identifiers

Before reaching for token references, there's a smaller lever that captures
most of bucket 2's win without the lifetime coupling: **intern identifiers
during analysis**. A per-compilation `Dictionary<hash-of-span, string>` (or
a span-keyed pool) that `ctx.NameOf`-style lookups go through would collapse
the 100 copies of `X` to a single shared string. The TAST keeps holding
plain `string`s — fully self-contained, no API churn at consumers — and the
allocation count drops to "one per *distinct* identifier" instead of "one
per *occurrence*".

Interning wins when identifiers repeat (they do); token references win on
top of that only for identifiers that are *never materialised* (dead nodes,
or names a given backend never emits). The two aren't exclusive — interning
is the obvious first step; token references are the heavier follow-up if
profiling still shows identifier materialisation dominating.

## Variation: store `ReadableString` instead of `string`

A cleaner carrier than a bespoke `(fileId, start, length)` token reference:
make the identifier-bearing TAST fields hold a `ReadableString`. The same
type covers both cases the design has to straddle —

- **source-derived names** become a zero-copy view (`GetTokenReadable`),
- **synthesised / decoded values** wrap a fresh string (`ReadableString s`,
  e.g. `ReadableString "op_Addition"` or a decoded `TConstValue.String`),

so consumers see one uniform field type and don't branch on provenance, and
there's no separate `materialise` step or fileId map: a view already holds
its own reference to the source string, a wrapper holds its own fresh
string, and the GC keeps whichever alive for as long as the node is. That
answers the ownership question Phase 2 otherwise raises — at a price:

- **Source pinning.** A 3-char view keeps its *entire* backing source
  string alive for the node's lifetime. For a long-lived TAST over a large
  source (or many files), a handful of small views can retain megabytes.
  Acceptable while the TAST is short-lived; a hazard if TASTs are cached.
  (Mitigation if it bites: copy-to-fresh — `ReadableString(view.ToString())`
  — at a chosen boundary, trading the copy back for releasing the source.)
- **Node width — profile it.** `ReadableString` is a `[<Struct>]` of
  `(string ref, int start, int length)` — ~16 bytes on 64-bit. So this
  *helps* the allocation axis (a source view is just inline struct bytes:
  no substring, and no separate wrapper object — and a fresh/interned-literal
  value is the same struct pointing at the existing string, again no extra
  heap object) but *widens every name field* from an 8-byte string pointer
  to the 16-byte struct. Wider nodes carry a value-copy cost as TAST nodes
  are built and passed around — exactly the effect measured in
  [[feedback_struct_value_size_cost]] (a bigger inline struct field cost
  ~3-4% Medium/Large wall-time purely through copying). That's the trade to
  profile: fewer/zero substring allocations vs. wider, costlier-to-copy
  nodes — and the answer may differ for hot identifier fields vs.
  rarely-touched ones.
- **Equality changes — affects the test DSL and any dedup.** A struct gets
  default *structural* equality over its fields: two `ReadableString`s are
  equal iff they share the same backing-string reference, start, and length.
  That is **not** content equality — a source view of `"X"` and a fresh
  `ReadableString "X"` compare unequal, as do two views of the same text at
  different offsets. Any structural comparison of TAST nodes (the
  `Expect.equal` tests, hashing, node dedup) would need a content-aware
  equality on the carrier rather than the default. Non-trivial churn.
- **Lookups get cheaper regardless.** `ReadableString` exposes `AsSpan` (no
  boxing — the `Reader`/`IReadable` path is generic over the struct), so
  field / member / case resolution can compare against declared names by
  span without materialising either side — a win independent of the width
  question.

If profiling favours this, it *is* the Phase 2 carrier (replacing the
bespoke struct below); the phasing is otherwise unchanged.

## Proposed shape (if/when this is scheduled)

Measure first — see the gate below — then, in increasing order of cost:

1. **Phase 0 — point fixes (done / ongoing).** Where a consumer re-parses
   or re-scans a token, read it through a `ReadableString` view rather than
   `ctx.NameOf`. The printf specifier path is the first instance
   (`GetTokenReadable` / `ReadableOf` / `parseFormatSpecifierView`). Add a
   `ctx.SpanOf` sibling if a span (rather than a `ReadableString`) is the
   better fit for a given call.

2. **Phase 1 — intern source-derived identifiers.** Route `NameOf` for
   identifier positions through a per-`PassContext` string pool. TAST shape
   unchanged; the win is dedup. Lowest risk, no consumer changes.

3. **Phase 2 — source-referencing fields (only if justified).** Migrate the
   identifier-bearing TAST fields off `string`. Leading carrier candidate is
   `ReadableString` (see the variation above): one uniform type for both
   source views and fresh strings, self-owning, no fileId map. The bespoke
   `(fileId, start, length)` + `materialise` alternative is heavier and only
   worth it if `ReadableString`'s source-pinning or wrapper cost proves
   unacceptable. Either way this touches every TAST consumer + the test DSL
   (`TastShape`) and needs a real profile — including node width and wrapper
   allocations, per the variation — behind it.

`TConstValue.String` (bucket 3) stays a decoded `string`; if it ever shows
up in a profile, the move is a lazily-decoded cache, not a source slice.

## Decision gate (measure before building)

The existing perf work in this repo is all on the **parser/lexer** hot path
(see the `project_*` / `reference_*hotspots*` notes); there is no evidence
yet that **semantic-analysis / TAST** string allocation is a hotspot. So:

- Don't open Phase 2 without an allocation trace showing identifier
  materialisation (or `TConstValue.String`) as a meaningful share of a
  realistic end-to-end run (parse → analyse → freeze, ideally → emit).
- For Phase 2 specifically, the profile must cover the *carrier's* own cost,
  not just the substrings it removes: with the `[<Struct>]` `ReadableString`
  that's the node-width / value-copy overhead (~16-byte field vs. 8-byte
  pointer) and retained-source bloat from view pinning, not allocation. The
  carrier can lose on copy cost even though it removes substring allocations.
- Phase 1 (interning) is cheap and self-contained enough to land on a
  weaker signal, but still measure the before/after.

## Cross-references

- [front-end-gaps-plan](front-end-gaps-plan.md) §B — the printf work that
  motivated this; `parseFormatSpecifierView` is the Phase-0 exemplar.
- `Tast.fs` header — the "TAST is sharable / CST is scoped" contract this
  plan would have to renegotiate for Phase 2.
- [nodekey](nodekey.md) — how source offsets already serve as identity keys
  (and why dereferencing them for *text* is a new step).
