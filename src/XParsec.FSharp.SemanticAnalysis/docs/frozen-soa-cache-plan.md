# Frozen TastFile: wire format + per-file compile cache

Status: design. Ephemeral plan doc — delete once the work lands.

## Goal

A serializable, compact encoding of `Frozen.TastFile` serving two ends:

1. **Trivial serialization + hashing** for a file-based compilation cache.
2. **CPU-friendly access on read** — the endgame is that Codegen and `FrozenSignature`
   *project from* the encoded form directly (option B below), not a pointer-graph they
   must first rebuild.

Non-goal for this work: a queryable Merkle DAG of derivations, cross-file artifact dedup,
or a demand-driven incremental engine. Those are deferred (see *Deferred*), but the cache
**key shape** is chosen so it does not foreclose them.

## What `Frozen.TastFile` is

`TastFileG<FrozenType, SyntaxToken>` — three interlocking domains:

1. **Expr/decl/pat tree** — `TExprG`/`TDeclG`/`TPatG` (recursive DUs, `EqArray` children).
2. **Type lattice** — `FrozenType` (recursive DU, `EqArray`/`EqSet` children). Heavily
   *shared* (`FTConst(intKey,[])` recurs thousands of times) → interning target.
3. **Identity keys** — `NodeKey` (a flat `uint64`), plus `Map<NodeKey,_>` side tables and
   `TypeKey`/`SymbolKey` string-bearing record trees.

## The thesis: two identity regimes

`NodeKey` today is a **content address** — `(offset, kind, synth)` packed in a `uint64`
(`NodeKey.fs`). Its defining property is that it is a *pure function of the CST node*
(`NodeKey.ofToken firstTok kind`), so any pass, holding only a CST token, recomputes the
identical key with **no `CstNode → id` side index** to thread. That is load-bearing during
analysis and worth keeping. It is *dead weight* after freeze. This work lives on that seam:
**a content key with role during analysis; positional identity after freeze.** Kind is a
pre-freeze content-address that freeze dissolves.

### Analysis regime — content key, role = the CST case

- **Role is the CST case, exactly.** `CstKeys.ofExpr`/`ofPat` are total `match case -> kind`
  functions (`CstKeys.fs:207-273`): `Pat.NamedSimple → PatIdent`, `Expr.Fun → ExprLambda`,
  … (A couple of nodes look one level deeper to avoid collisions — a method-call
  `App(funcExpr = DotLookup …)` keys off the *member* token — but the *kind* stays
  case-pure; only the token choice varies.)
- **`(offset, kind)` are both needed** because two distinct nodes share a token: a
  `let`-binding decl (`DeclLetBinding`) and its head pattern (`PatIdent`) both anchor at the
  same ident; kind separates them.
- **Why the key is stored on the tree at all** — neither reason is redundancy:
  - It **memoizes a context-dependent identity.** The role (kind) is fixed by the
    *syntactic position* the node occupied — known at mint, not at a downstream consumer
    holding a bare token. The stored key transports that context.
  - Half the `NodeKey` fields are **resolved edges, not self-identity.** `TExpr.Var.binding`
    is the *definition* site's key (a different token than the `Var`'s own) — the output of
    name resolution, irreducible; you cannot recompute it from the `Var`.
- This regime is **unchanged** by this work — `NodeKey` stays a 64-bit content key through
  the whole semantic pipeline. See also `docs/nodekey.md` (§ *Why not just allocate
  sequential IDs?* — the same argument).

### Freeze regime — positional identity, kind dissolves

Freeze has all context. It assigns each distinct `NodeKey` a **dense `int` id** (pool
index) once, and thereafter:

- **Identity is positional.** A binder's identity *is* its slot in the binder pool. The
  `let`-decl-vs-pattern collision that motivated `kind` becomes two distinct pool entries
  with two distinct ids — disambiguated by *position*, with no help from `kind`.
- **Kind is dead information.** It existed only to make `(offset, kind)` unique and to let a
  reference name a def by value. Positional ids provide uniqueness; references name defs by
  id. **No post-freeze consumer reads `NodeKey.Kind`** — verified: across codegen, freeze,
  and `FrozenSignature` the `.Kind` accessor is read *nowhere in logic*, only in
  `NodeKey.ToString` (debug). During analysis, kind acts solely through full-`Raw` equality
  in `Map<NodeKey,_>` lookups, which positional ids replace. So freeze projects kind away
  entirely — the sense in which *kind is a pre-freeze content-address that freeze dissolves.*
- **Edges become dense ints.** `TExpr.Var.binding`, every `Map<NodeKey,_>` key → the
  referent's dense id; side tables become dense-keyed arrays. (References still store
  *something* — identity is never free; freeze only shrinks it from a 64-bit key to a pool
  index.)
- **Naming survives as node data, not as identity.** Codegen's one use of a key's *bits* is
  naming: `binderName` (`JsEmitHelpers.fs:73`) slices the source at the binder's offset, or
  renders a synthetic as `_s<NameIndex>`. Freeze preserves each binder's **naming integer**
  separately from its positional id:
  - *real binders* — the source offset, read from the binder node's own `tok`
    (`NamedSimple.tok`, `ForTo.identTok`), which equals the dissolved key's offset by the
    mint invariant (corpus: 643 real `NamedSimple` binders with `offset = tok.StartIndex`,
    **0 mismatches**), so names stay byte-identical; where a binder carries no token (a
    `Params` slot), the offset is retained as data.
  - *synthetics* — the original `NameIndex` verbatim, so `_s<n>` names don't renumber
    (renumbering would desync cached vs. non-cached output).

  So the synthetic negative-offset / counter space (`ofSyntheticCounter`) also dissolves:
  post-freeze a synthetic is just an id with a retained naming integer.

The one obligation freeze *adds*: a few codegen sites recompute a key from a token because
the node carries no inline key. The complete set (verified — every `NodeKey.of*` call on the
codegen/`FrozenSignature` path) is **3 recompute sites across 2 node kinds**:
`EmitClosures.fs:755` and `ClosureVerdictRewrite.fs:87` (`ofToken (exprTok node) ExprLambda`)
and `SymbolProviders.fs:87` (`ofSynthetic bodyTok … SynthLambdaBody`). Under positional ids
these cannot recompute, so **freeze must stamp those recompute-target nodes (the lambda expr
and its body) with their dense id inline**, and the codegen sites read it off the node.
Out of scope: `ClrDriver.fs:34`'s `ofSynthetic 0 SynthUnsupportedDecl` is a *fresh* mint for
a whole-file driver diagnostic (no node to correlate), and `FrozenSignature` and the JS
backend recompute no keys at all.

## The accessor API — the A→B spine

A→B is cheap only if it rests on **one seam**: a TAST-shaped accessor API (`exprKind id`,
`exprChildren id`, `patBinder id`, `binderName id`, …) that both the DU and the pools can
back. It is introduced at the **start of Phase B, backed by the existing `Frozen.TastFile`
DU**; consumers migrate onto it while it is a thin projection, and only the final step flips
the backing to pools — so the consumers never move again. The accessor must exist *before*
the pool byte-layout is designed, so the layout serves the real access pattern; Phase A's
interim DU serializer imposes no constraint on it and is deliberately throwaway.

## Wire format

- **Id-indexable pools, not a decode stream.** B fetches "node id `k` and its children by
  id" at random, so pools are fixed-width id-records (tag column + dense child-id columns),
  indexable in O(1) — even though A only ever reads them linearly. A forward-only varint
  stream would satisfy A and force a re-layout for B; don't build that.
- **Binder pool**: identity by position; each entry carries its naming integer
  (offset / `NameIndex`) and its type id. Kind not stored.
- **Edges**: dense ids. Side tables: dense-keyed.
- **`FrozenType` + `TypeKey`/`SymbolKey`**: interned (dedup by structural equality —
  `EqArray`/`EqSet` already give it in-memory). A size/read optimization, decoupled from
  correctness (the cache key hashes inputs, not the blob — see *Cache*), so it can trail the
  first cut.
- **Tokens / `Lexed`**: `SyntaxToken.StartIndex` retained where a node needs naming; `Lexed`
  is not serialized (name recovery needs the source string, which is the cache key's input —
  a re-lex is one linear pass, deferred as a pure read-speed question).
- **Compression**: whole-blob `zstd`, eagerly decompressed to the in-memory pools on load.
  Per-file blobs are small; skip mmap/block-framing. This is what actually pays for size —
  including all offset redundancy — so the in-memory pools are free to hold real integers for
  fast projection, with no reconstruction step.

Thaw rebuilds the `Frozen.TastFile` DU under option A (the DU stays the working
representation, so the compiler's invariants and F#'s exhaustive matching are retained);
under option B, Codegen and `FrozenSignature` read the pools through the accessor and the DU
is never materialized.

### Rejected alternatives

- **Reconstruct-on-read** (store a `Lexed` token *ordinal*, re-lex to recover the offset, so
  the wire form carries no offsets): rejected. A hand-rolled disk-size codec whose whole
  payoff — offset-free-ness — buys only source-relocatability, a non-goal (see *Deferred*).
  `zstd` dedupes the offset bytes on disk for free, and in RAM you want the real offset
  anyway. It also imported a load-bearing corpus invariant and a synthetic carve-out that the
  store-then-compress path does not need.
- **Serialize `NodeKey.Raw` verbatim** (it is a `uint64`): rejected for the *frozen* form.
  Post-freeze the 64-bit content key is dead weight (kind dissolved); dense positional ids
  are both the natural identity and a compaction of the edge/side-table representation.
  (`Raw`-verbatim is, however, exactly what the *analysis* regime does — correctly.)

## Cache

- **Per-file, input-keyed.** Key = `hash(source ⊕ dependency-signature-hashes)`. Because the
  key hashes *inputs*, the stored blob needs **no** byte-canonicalization for the hash — that
  removes all canonicalization burden, and makes interning purely a size optimization.
- **Store key shape:** `(QueryId, codeVersion, inputHash)`.
  - `QueryId` names the derivation (`Freeze`, later `Signature`, `Lex`) — an enum today.
  - `codeVersion` guards against cross-compiler-version cache poisoning: a bugfix to `freeze`
    must invalidate everything it produced. Cheap now, painful to retrofit.
  - `inputHash` folds in dependency-artifact hashes — the Merkle spine, the one thing to get
    right today, because it is what makes cross-file caching correct.
- **Dumb content-addressed KV store.** No demand-driven engine, no query EDSL yet.

## Deferred

- Typed query builder / EDSL — add it when there are many queries, not three.
- Demand-driven incremental engine (Salsa-style red/green invalidation) — the hard, valuable
  part; slots under the same store later.
- Cross-file shared intern pool / Merkle-addressed sub-artifacts.
- Self-contained artifact (name-baking / source-relocatable form) — only wins if an artifact
  must travel without its source; out of scope while the cache is same-build and
  source-keyed. (This is what reconstruct-on-read would have served; deferred with it.)

## Staging

The two phases are **very** asymmetric, which the coarse split hid. **Phase A** (cache on the
existing DU) is modest and lands the cache with *zero* consumer churn — observable behavior
is unchanged, exactly what "A first, let the format settle" wants. **Phase B** (accessor +
pools + projection) is the large effort: a pervasive representation change plus a
consumer-by-consumer migration. Do A fully first; schedule B separately. Cache-first is
deliberate — the accessor is a Phase-B prerequisite, not needed to ship the cache, so it
does not gate A's value.

Every step is a standalone commit: green build, and (past 0.1) the byte-identity gate holds.

> **Status: Phase 0 and Phase A are LANDED; Phase B is IN PROGRESS.** The opt-in,
> input-keyed frozen-compile cache ships — XxHash128 keys, Brotli blobs, verbatim DU
> flatten/thaw, `ClrDriver.compileCached`. In Phase B the accessor (B.1) and the **entire
> B.2…B.k consumer migration** have landed: every `Codegen.Js` **and** `Codegen.Clr` consumer
> now reads the frozen tree through `TastAccessor` — **zero `TExprG`/`TPatG`/`TDeclG` DU
> matches remain in either backend** (verified repo-wide), each step byte-identical (485/485
> golden JS tests, 1407/1407 golden CLR tests). `FrozenSignature` has no expr-tree matches (a
> non-op for this seam). The CLR migration grew the accessor to cover CLR-only payload the JS
> half never read — additive `…View` field extensions (`DeclLetView.Ty`, `Via` on
> `PropertyGetView`/`MethodCallView`) and scalars (`exprNewChosenCtor`,
> `exprILIntrinsicTypeOperand`, `exprTypeTestTestTy`, `exprStaticOptimizationDefault`,
> `patTypeTestTestTy`, `declExpressionTy`, plus an `exprTryWith`/`TryWithView` mirroring
> `exprMatch`) — all reused across both backends, no JS churn. The 0.2 `TastWalk.lambdaKey`
> recompute sites (`EmitClosures`, `ClosureVerdictRewrite`) stay as-is until B.k+3. The pool
> steps (B.k+1…B.k+7) are untouched. A `GeneralizedTypars.unsafeOfNames` concession made in
> A.4 is tracked in `frozen-tree-semtype-residue-plan.md` (deferred, naturally folds into B).

### Phase 0 — scaffolding & de-risking (no behavior change)

- **0.1 Byte-identity gate.** Extend the corpus/codegen tests to record emitted JS (and the
  CLR artifact hash) as goldens and assert equality — a regression tripwire for every later
  step. *Gate: goldens captured, green.*
- **0.2 Single-source the recompute keys — construction homed in `SemanticAnalysis`.** The
  three sites split by shape, so two small functions, **owned by `SemanticAnalysis`** (where
  `NodeKey`/`NodeKind`/`TastWalk.exprTok` live, reachable from both `Codegen.Clr` and
  `Codegen.Common`): `lambdaKey : Frozen.TExpr -> NodeKey` for the two `ExprLambda` recomputes
  (`EmitClosures.fs:755`, `ClosureVerdictRewrite.fs:87`), and `synthLambdaBodyKey` for
  `SymbolProviders.fs:87` — which is *not* a recompute but a fresh synthetic **mint** (unread
  filler that keeps the reconstructed inline node total; its key is never looked up). The
  backends call these and no longer assemble keys from `ofToken`/`ofSynthetic`/`NodeKind`
  themselves, so **the only place that knows how a frozen key is constructed is
  `SemanticAnalysis`** — the invariant B.k+3 later mutates. Deliberately *not* the heavier
  option (freeze stamps each `Lambda` with its own `NodeKey` inline): that touches the shared
  generic `TExprG.Lambda` case and so ripples into analysis construction and the DU serializer
  — deferred to **B.k+3**, where the dense id it would carry actually exists. Pure refactor.
  *Gate: goldens hold.*
- **0.3 Single-source binder naming.** `JsEmitHelpers.identName` is already the *sole* site
  that unpacks a key's naming bits (`Offset` to slice the source name, `NameIndex` for the
  `_s`/`_v` synthesis) — so this is a rename to the intent-revealing `binderName`, not a
  consolidation. Signature stays `string voption -> NodeKey -> string`: source is ambient
  (real-binder naming *slices* it, so it cannot be dropped — the plan's `NodeKey -> string` is
  shorthand for "keyed on node identity"), and the arg stays a `NodeKey`, not a `SyntaxToken`
  — tokenless `Params` slots and counter-minted synthetics (`ofSyntheticCounter`, negative
  offset) have no token and are named by `NameIndex`. Confines the future "read the naming
  integer off the node / pool" change (B.k+4) to this one body. (`EmitJs.fs:57` feeds
  `k.Offset` to `curryAdapter` — a location, not naming — and is out of scope.) *Gate: goldens
  hold.*

### Phase A — cache on the DU (option A; pipeline unchanged)

- **A.1 KV store.** `(QueryId, codeVersion, inputHash)` key types + a content-addressed store
  interface with filesystem and in-memory impls. *Gate: store round-trip unit tests.*
- **A.2 Input hashing.** `inputHash` = hash of source folded with dependency-signature
  hashes; computed in the pipeline but not yet consulted. *Gate: hash-stability +
  dependency-sensitivity tests.*
- **A.3 DU flatten/thaw — leaves.** Structural writer/reader for `FrozenType`,
  `TypeKey`/`SymbolKey`, `NodeKey` (verbatim `Raw`), tokens. *Gate: round-trip equality on
  those domains over the corpus.*
- **A.4 DU flatten/thaw — tree.** Extend to `TExpr`/`TDecl`/`TPat` and the `Map<NodeKey,_>`
  side tables (verbatim, no interning). *Gate: `thaw ∘ flatten = structural identity` on the
  full corpus.* (Split by domain — exprs; decls+pats; side tables — if the diff is too large.)
- **A.5 Byte-identity through the round-trip.** Run codegen on `thaw (flatten (freeze …))`
  and assert output equals codegen on the direct freeze. *Gate: 0.1 goldens hold through the
  round-trip.*
- **A.6 Compress + store, flag-off.** `zstd`-wrap the blob; on the `Freeze` `QueryId`, miss ⇒
  freeze + flatten + compress + store, hit ⇒ load + decompress + thaw. Disabled by default.
  *Gate: hit/miss parity — cached output byte-identical.*
- **A.7 Enable + incremental smoke test.** Turn the cache on in the driver; compile → edit →
  recompile exercises hit/miss and dependency invalidation. *Gate: end-to-end incremental
  test.*

### Phase B — accessor + pools (option B; pools become the working rep)

- **B.1 Accessor interface + DU backing.** Define the TAST-shaped accessor; implement it over
  `Frozen.TastFile`. Unused. *Gate: compiles.* **LANDED** — `TastAccessor.fs` (RQA module, after
  `TastWalk.fs`): shape tags `ExprShape`/`PatShape`/`DeclShape`, handle aliases
  `ExprId`/`PatId`/`DeclId` (transparent over the DU now; become dense-id handles at B.k+5, which
  is why every access routes through this seam), and `exprKind`/`exprTy`/`exprTok`/`exprChildren`,
  `patKind`/`patTy`/`patTok`/`patChildren`/`patBinder`, `declKind`. `exprTy`/`patTy` delegate to
  `TastWalk`, whose two projections were generalized `TExprG<'ty,'tok> -> 'ty` (additive) so the
  frozen tree reuses one enumeration instead of a parallel match. Deferred out of B.1 (would only
  duplicate existing code): `binderName` — its sole impl reads `NodeKey` naming bits in the JS
  backend, re-homed to read a pool naming integer at **B.k+4**; and decl-field accessors — the
  decl consumer (`FrozenSignature`) migrates late in B.2…B.k, so only the `declKind` entry tag
  exists today.
- **B.2 … B.k Migrate consumers, one per commit.** Switch each codegen/`FrozenSignature`
  consumer from direct DU matching to the accessor, still DU-backed and output-identical —
  roughly one commit per emit file: **~15–20 commits**, the bulk of the effort. *Gate
  (each): goldens hold.*

  **JS half LANDED** (commits `873812c7`→`bfc2e719` on `codegen-js`, 9 commits). The
  payload-accessor API the one-liner glossed was settled as: a per-case `[<Struct>] …View`
  (named fields, `failwith`-guarded, dispatched behind `exprKind`/`patKind`/`declKind`) for
  multi-field cases; a single field accessor for a lone scalar; a labeled accessor
  (`patRecordFields`/`exprRecordConsFields`/…) where `exprChildren` would drop names;
  children/type/token via the existing `exprChildren`/`exprTy`/`exprTok`. **`TastAccessor.fs`
  is the canonical record of the convention — read it, don't re-derive.** Order landed:
  `JsExternalMembers`, `EmitJsCapabilities`, `JsFlatFns`, `EmitJsTypes`, `EmitJsContext`,
  `JsEmitHelpers`, `EmitJs` (split ×3 — `buildExpr`; the statement-builders; `buildProgram`
  decls), then a shared `(|InstanceExternalMember|_|)` recognizer factored into
  `JsExternalMembers`. `binderName`'s `NodeKey`-bits logic was left untouched (its `NodeKey`
  now arrives via `patBinder`/`exprVarBinding`) per the B.k+4 deferral.

  **CLR half LANDED** (`codegen-js`, 15 file-scoped commits `771e1e40`→`dc5d144f`, one per emit
  file bar the four 1-match trivia grouped into one). Same mechanical recipe over `Codegen.Clr`.
  Order landed: `EmitLoops`, `EmitMatch`, `EmitFormat` (comment-only), the trivia group
  (`EmitLower`/`LayoutNodes`/`Layout`/`NominalEmit`), `HolderPlan`, `Emit`, `EmitBindings`,
  `EmitConstruct`, `EmitCall`, `EmitIntrinsic`, `EmitMember`, `EmitPattern`,
  `ClosureVerdictRewrite`, `EmitExpr`, `EmitClosures`. The CLR consumers being more
  sophisticated than JS, the accessor grew to cover payload JS never read — always by
  **extending an existing `…View` additively** (`DeclLetView.Ty`, `Via` on
  `PropertyGetView`/`MethodCallView`) or adding a lone scalar / a new View mirroring a sibling
  (`exprTryWith` ≈ `exprMatch`), never a per-file helper and never a near-duplicate; all reused
  across both backends with zero JS churn. Two seam invariants worth recording for the later
  steps: (a) the accessor is **read-only** — a consumer that *constructs* a frozen node (the
  `buildUse` dispose synthetic in `EmitBindings`, the `buildEta` eta-expansion and decl rebuilds
  in `EmitClosures`, the retype rebuilds in `ClosureVerdictRewrite`) keeps a `Frozen.TExpr.*` /
  `Frozen.TPat.*` / `Frozen.TDecl.*` construction, which the grep gate tolerates; (b) the 0.2
  `lambdaKey`/`synthLambdaBodyKey` sites (`EmitClosures`, `ClosureVerdictRewrite`) stay as-is
  until B.k+3.
- **B.k+1 Id-children pools.** Add the id-indexable pools; `freeze` populates them alongside
  the DU (both coexist). *Gate: pools structurally mirror the DU — cross-check over the
  corpus.*
- **B.k+2 Dense-id remap.** Assign pool ids; remap side-table keys and references
  (`Var.binding`, …) to dense ids in the pool form. *Gate: id resolution round-trips.*
- **B.k+3 Stamp lambda ids.** Pool lambda / lambda-body nodes carry their dense id inline; the
  0.2 helper reads it. *Gate: goldens hold.*
- **B.k+4 Naming integers in pools.** Pool binders carry offset / `NameIndex`; the 0.3 helper
  reads pool data. *Gate: goldens hold.*
- **B.k+5 Flip the backing.** Point the accessor at the pools; `freeze` stops materializing
  the DU. *Gate: goldens hold — the projection payoff.* NB the read-only accessor covers only
  *reads*: the handful of node-**construction** sites the B.2…B.k migration left as
  `Frozen.TExpr.*`/`Frozen.TPat.*`/`Frozen.TDecl.*` (the `buildUse` dispose synthetic; the
  `buildEta` eta lambdas and decl rebuilds; the `ClosureVerdictRewrite` retype rebuilds) cannot
  ride a dense-id handle unchanged — this step needs a construction seam (or to lower those
  sites to emit directly, as the JS backend does) alongside flipping the read backing.
- **B.k+6 Serialize pools directly.** Replace A's DU flatten/thaw with pool (de)serialization;
  intern `FrozenType`/keys for size. *Gate: round-trip + size regression check.*
- **B.k+7 Remove dead DU paths.** Delete the DU thaw and any now-unused DU plumbing. *Gate:
  green.*
