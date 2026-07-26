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
> recompute sites (`EmitClosures`, `ClosureVerdictRewrite`) stay as-is until B.k+3. **B.k+1 has
> landed**: `TastPools.fs` (`FrozenPools` — per-domain arrays of shape-tagged id-records, child
> edges as dense ids, non-child payload riding the retained DU node) plus `TastPools.toPools`/
> `ofPools`, produced by `Freeze.buildPools` alongside the DU (both coexist, accessor still
> DU-backed, nothing reads the pools yet). Interconversion is proven codegen-invariant over the
> full CLR corpus (`ConformanceRoundTripByteIdentityTests` now judges `ofPools (toPools frozen)`
> against direct codegen per program) plus an SA-project smoke set. **B.k+2 has landed**: a
> dedicated minimal `Binders` column (simple name bindings — `NamedSimple.binding` / `ForTo.var`
> — interned to a dense `BinderId`, retaining the whole `NodeKey` for now), with `Var.binding`
> and all seven `Map<NodeKey,_>` side tables re-expressed as `BinderId`-keyed dense forms; a
> two-pass `toPools` (enumerate binders, then resolve references — a `Var` may name a binder
> pooled after it) and a `failwith`-on-miss resolver that proved every reference/side-table key
> resolves to a simple binder over the whole corpus. `ofPools` rebuilds `Var.binding` and the
> side tables through the binder pool, so the round-trip gate exercises the remap. **B.k+3 has
> landed**: the one lambda-*expression*-keyed side table `FunVerdicts` moved off the binder pool
> onto a lambda id space (a lambda's dense id is its `ExprPoolId`), fixing B.k+2's vacuous
> binder-remap of it (`FunVerdicts` is empty across the corpus, so that pass proved nothing); the
> generic remap/rebuild pair is now parameterized by a key resolver (`binderIdOf` for the six
> binder tables, `lambdaIdOf` for `FunVerdicts`), and a focused unit test injects a synthetic
> verdict keyed by a real frozen lambda to exercise the otherwise-unreachable path. The
> codegen-facing `lambdaKey` rewire stays deferred to B.k+5. **B.k+4 has landed**: each
> `BinderPoolEntry` now carries a `BinderNaming` triple (`IsSynthetic`/`Offset`/`NameIndex`)
> sourced from its `NodeKey` — the naming data that outlives the key at the flip — with the
> `NodeKey` retained (the DU round-trip still reconstructs `Raw`, kind included). A corpus test
> re-verifies the mint invariant (every real binder's `Offset` equals its node token's
> `StartIndex`, zero mismatches) that makes the flip naming-preserving; the `binderName` rewire
> to read pool data stays deferred to B.k+5. **B.k+5 step (A) is UNDERWAY**: the EXPRESSION pool
> is now struct-of-arrays — dense parallel columns (`ExprShapes`/`ExprTys`/`ExprToks`/
> `ExprChildren`/`ExprPatChildren`/`ExprVarBinder`) plus an `ExprPayload` DU side array carrying
> each case's residual scalars and the composite structure (Match/TryWith arm guard-flags, Format
> sink/segment shapes, StaticOptimization clause constraints, Range/RecordCons/ExternalMember
> presence) — and the expr DU `Node` is GONE; `ofPools` rebuilds exprs from columns alone, proven
> codegen-invariant over the full CLR corpus. **Step (A) is now COMPLETE**: the pat and decl pools
> and the binder pool are struct-of-arrays too (`PatShapes`/`PatTys`/`PatToks`/`PatChildren`/
> `PatPayloads`; `DeclShapes`/`DeclExprChildren`/`DeclPatChildren`/`DeclPayloads`; the binder pool as
> `BinderKeys`/`BinderNamings`) — NO pooled tree node retains a DU `Node`; every domain rebuilds
> from columns, proven codegen-invariant over the CLR corpus. The one remaining DU residue is
> deliberate and known: `FrozenPools.File` still holds the source `Frozen.TastFile` (for the side
> tables / `InlineBodies` / diagnostics — its `.Decls` are re-authored by `ofPools`), and a `Type`
> decl's member bodies + `InlineBodies` are carried opaquely (not pooled). **Part (B) — the flip —
> was deferred, then UN-DEFERRED — see *The flip* for the architecture that replaced the
> deferral, including the premise correction that forced it.** It is the project's crux, not a
> mechanical step:
> the CLR backend's eta-bridging (`bridgeStaticFnEscapes`) and `ClosureVerdictRewrite` are
> CLR-specific TAST→TAST pre-passes that MINT new nodes `discoverClosures` must then walk (the JS
> backend has no analogue — its native-curried model needs neither), so a pool-backed emit cannot
> simply read them. The agreed future architecture: the SA-built pool stays the IMMUTABLE,
> backend-neutral canonical artifact, and the **CLR backend derives its OWN working DU/pool from it
> (via `ofPools` or a projection) for its backend-specific lowering** — rather than forcing those
> rewrites onto the shared pool or reimplementing closure emission at the emit site. Under that
> design the accessor flip, the `binderName`/`lambdaKey` pool-read rewires, and the `File`/DU
> severance all land together in the dedicated flip effort (overlapping B.k+6/B.k+7). Until then the
> accessor and both backends stay DU-backed. What THIS arc banks: the pools are built, fully
> columnar, interconvertible, and proven codegen-invariant over the corpus — the substrate the flip
> and the direct pool serialization (B.k+6) build on. **B.k+6 has landed**: the pools ARE the
> stored wire form — `FrozenCodec.flatten` is `TastPools.toPools` then the column writers and
> `thaw` their inverse then `ofPools`, so the corpus-wide `thaw ∘ flatten` gate now also gates the
> pool interconversion. `FrozenPools.File` (the whole source `Frozen.TastFile`) narrowed to a
> four-field `FrozenFileResidue` — diagnostics, the two `SymbolKey` dictionaries, and
> `InlineBodies` — which is exactly what `ofPools` ever read off it, so `FrozenPools` is now
> self-contained and serializable, and the residue NAMES what is still unpooled. The DU tree codec
> stays: it is what serializes the two opaque domains (a `DeclPayload.Type`'s member bodies, the
> inline vocabulary), which **F.1** pools. Whole-file structural equality moved to
> `TastFileG.structurallyEqual` (library knowledge — two of the record's twelve fields are
> `IReadOnlyDictionary` and break the derived `=`), de-circularising the `TastPoolsTests` oracle,
> which had been using `flatten` as its judge. The column form is **~30% LARGER compressed** than
> the recursive tree codec (the tree encodes its spine in the nesting for free; columns must name
> every child edge) — recorded in `FrozenBlobSizeTests` with measured ceilings, and accepted:
> interning is dropped from this arc as a pure size optimization. A
> `GeneralizedTypars.unsafeOfNames` concession made in A.4 is tracked in
> `frozen-tree-semtype-residue-plan.md` (deferred, naturally folds into B).

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
  0.2 helper reads it. *Gate: goldens hold.* **Refined scope (confirmed):** of the seven side
  tables, only `FunVerdicts` is keyed by a lambda-*expression* NodeKey (`ofToken … ExprLambda`,
  read via `TastWalk.lambdaKey`); the other six — `ClosureReprs` included — are binder-keyed and
  B.k+2 homed them correctly. B.k+2 routed `FunVerdicts` through the *binder* pool too, which is
  the WRONG home and only passed because `FunVerdicts` is **empty across the whole corpus** (the
  value-struct/stack-closure path is not yet emittable). B.k+3 fixes the pool form: introduce a
  lambda id space (a lambda's dense id is its `ExprPoolId` — positional), re-key pool-form
  `FunVerdicts` onto it (off the binder pool), `ofPools` inverting through the pooled lambda node,
  with the same fault-on-miss discipline. The codegen-facing rewire (`lambdaKey`/
  `synthLambdaBodyKey` reading a dense id off a pool handle) stays deferred to **B.k+5** — codegen
  is DU-backed until then. Because the corpus never populates `FunVerdicts`, the gate is a focused
  unit test that injects a synthetic verdict keyed by a real frozen lambda's `lambdaKey`, plus the
  corpus round-trip staying green.
- **B.k+4 Naming integers in pools.** Pool binders carry offset / `NameIndex`; the 0.3 helper
  reads pool data. *Gate: goldens hold.*
- **B.k+5 — split into (A) payload extraction, then (B) the flip (confirmed).** B.k+1..4 built
  pool entries that RETAIN the whole DU node (payload rides `Node`); since a retained node holds
  its subtree, the DU is still materialized. So the flip is done in two parts. **(A) Payload
  extraction / columnar SoA:** convert each domain from an array-of-id-records to struct-of-arrays
  — dense parallel columns for the uniform fields (`Shape`, `ty`, `tok`, the child-id arrays, and
  the binder/lambda/var ids) plus a few typed side arrays (a per-domain `…Payload` DU carrying the
  residual per-case scalars + the composite structure needed to re-nest the flat child columns) —
  and DROP the `Node`. `ofPools` then rebuilds the DU from columns alone; the corpus round-trip
  (which runs `ofPools (toPools frozen)` per program) proves the columns are Node-sufficient. Done
  as its own commit(s), one domain at a time (exprs first, then pats+decls+binder pool), DU still
  coexisting. **(A) is LANDED** (commits `f0b81afe` exprs, `cf3b4956` pats+decls+binder) — every
  domain is struct-of-arrays, no pooled tree node retains a DU `Node`, proven codegen-invariant
  over the CLR corpus. **(B) the flip is DEFERRED** to a dedicated effort (see below).
- **B.k+5 Flip the backing — UN-DEFERRED; see *The flip* below** for the architecture and the
  F.1…F.5 staging that replaces this one-line entry. Point the accessor at the pools; `freeze`
  stops materializing the DU. *Gate: goldens hold — the projection payoff.*
- **B.k+6 Serialize pools directly.** Replace A's DU flatten/thaw with pool (de)serialization.
  *Gate: round-trip.* Interning `FrozenType`/keys is **dropped from this arc** — a pure size
  optimization, and the blob is compressed downstream anyway; revisit when size is the
  complaint.
- **B.k+7 Remove dead DU paths.** Folded into **F.5**.

## The flip

### A premise correction

The earlier deferral rested on: *the construction sites are CLR-specific, and the JS backend has
no analogue.* **That is false**, and the whole design changes with it. The JS backend mints too —
it just does so *indirectly*, which is why a grep for `Frozen.TExpr.*` under `Codegen.Js` returns
nothing:

- `JsEmitHelpers.substVar` → `TastLower.mapChildren` rebuilds the spliced path inside
  `(|InlinableLet|_|)`, which `EmitJs.buildExpr` matches **mid-walk** (`EmitJs.fs:95`, `:664`,
  `:768`).
- `TastLower.lower` — the single largest mint in the system (119 constructor occurrences) — is
  run by **both** backends before emit (`Layout.fs:26`, `EmitJs.fs:986`).

So node construction is not a CLR quirk to route around; it is universal, and the flip must meet
it head-on. Correspondingly the builder below is **not CLR-only** — it is a shared facility both
backends open.

### Three mint regimes

- **Whole-tree pre-pass** — `TastLower.lower` (both backends); `bridgeStaticFnEscapes`/`buildEta`
  (CLR, `HolderPlan.fs:141`, strictly *before* `discoverClosures` at `Layout.fs:121`, whose walk
  then covers the eta lambdas); `SymbolProviders.collectInlineBodies`.
- **Per-body batch** — `ClosureVerdictRewrite.retypeBody`/`retypeDecl`, applied per method /
  module-value immediately before that body's emit (`Assembler.fs:1037`, `:1072`, `:1094`,
  `:1123`). Deliberately reference-preserving for untouched subtrees, because `discoverClosures`
  keyed its lambdas by `HashIdentity.Reference`.
- **Interleaved** — no batch point at all: `EmitBindings.buildUse`'s dispose synthetic
  (`EmitBindings.fs:138`, constructed and emitted inside one `IlBuilder` callback), and the JS
  `substVar` path above.

The interleaved regime is what rules out "re-pool once between phases".

### The design: a stacked pool

**The SA-built pool stays immutable and canonical.** A backend opens a **builder** over it that
*stacks* rather than copies: ids `0 .. n-1` address the immutable base columns, ids `≥ n` address
the builder's own append-only columns. The read side therefore sees **one flat id space** and
never learns there are two layers — `exprKind`, `exprChildren`, every `…View` resolve an id
without caring which layer answers. Nothing is copied, and base ids are **preserved exactly**,
which is what makes the stack sound: an edge minted in the overlay may name a base node by its
own id, and every id a consumer cached before the overlay existed stays valid.

Two consequences worth naming:

- **Id preservation retires the reference-identity fragility.** Six dictionaries key on
  `Frozen.TExpr` object identity today (`ClosureVerdictRewrite.fs:84`, `EmitClosures.fs:764`,
  `:832`, `Assembler.fs:294`/`:299`/`:310`/`:313`), and `Layout.fs:73` records the resulting
  constraint. Under stacked ids these become **value**-keyed on a pool id — strictly better, and
  the per-body `retypeBody` batch stops having to preserve object identity by hand.
- **Rewrites collapse to row edits.** In columnar form `mapChildren` is "append a row identical
  to row *i* with new child-id arrays" — one row copy, *no per-case match*, replacing 119
  constructor occurrences. A `ClosureVerdictRewrite` retype is "copy the row, write a different
  `ExprTys` entry". This is the flip's real payoff, not just the allocation win.

**Handles carry their pool.** `ExprId`/`PatId`/`DeclId` become `[<Struct>] { Pool; Id }` rather
than a bare `int`. A bare int forces a pool parameter through every one of the ~70 accessor
signatures and every call site the B.2…B.k migration just finished touching; the fat handle keeps
all of those shapes intact. It also lets pools that are *not* the file's tree exist — see the
`ValRepr` residue below — at the cost of 16-byte handles in emit loops, which is a prototype-stage
trade we can revisit if it shows up in a profile.

**The DU survives as a construction vocabulary, not a walked representation.** A mint site may
keep building `Frozen.TExpr.*` and hand it to `appendTree : builder -> Frozen.TExpr -> ExprId`;
what disappears is anything *walking* a DU. That decouples the mint-site rewrites from the
accessor flip, and lets each mint site move to native row-appends on its own schedule.

### Known residues the flip must resolve

- **`ValRepr` patterns have no pool home.** `ArgGroupG.GTuple` carries a `TPatG`
  (`Tast.fs:561-574`), and for an *external* symbol those pats are minted from an `.fsi` contract
  by `TastLower.externalValRepr` (`VesperLib.fs:299`) — they are in no file's tree, yet
  `JsFlatFns.fs:143` reads them through `TastAccessor.patKind`. They get their own small pool,
  owned by the external symbol table; the fat handle makes that a non-event.
- **`Inline.thawBody`** (`Inline.fs:73`) converts a frozen `TDecl` to the `SemType` domain via
  `TastConvert.decl`. There is no pooled `SemType` side, so this needs either a pool→`SemType`
  walk or an `ofPools` of that one subtree.
- **Un-migrated DU consumers**: `FrozenSignature` (decl *heads* + opaque `TTypeDecl` only — it
  never reads a value position), `ConformanceTypars`, `SymbolProviders.collectInlineBodies`.
- **`ofPools` stays** as a debug/test facility. 288 direct `Frozen.TDecl`/`TExpr`/`TPat`
  references across 46 test files read `tast.Decls` for shape assertions; keeping `ofPools`
  means the flip does not drag a 46-file test rewrite behind it.

### Staging

- **F.0 `let inline` is an ordinary function that is ALSO a template.** A prerequisite for
  F.1: it removes one of the three carriers F.1 would otherwise have to pool, and closes the
  bug class the binder-keying fix just cleaned up after.

  **The semantics being corrected.** F# does not skip emitting an `inline` function. It emits a
  normal module function with that body, callable at runtime, *and* opportunistically splices
  the body at use sites. Both, not either. Today freeze does neither faithfully:
  - `Freeze.emittable` drops every `TDecl.Let(isInline = true)` from `Decls`, so **no** inline
    function is ever emitted — a caller that cannot be inlined has nothing to call.
  - Publication into `InlineBodies` needs `Map.tryFind k tast.ModuleMembers`, and `Elaborate`
    fills `ModuleMembers` only when a binding has a holder; a **top-level** `let inline` goes to
    `TopLevelNames` instead, so it has no minted `SymbolKey` and is published nowhere. It is
    spliceable within its own unit (`InlineExpansion` runs pre-freeze) and invisible outside —
    an asymmetry with module-level `inline` that is a fallout of the identity model, not a
    decision.

  **The target.** An `inline` binding stays in `Decls` with `IsInline = true` — the flag
  `TDecl.Let` and `DeclPayload.Let` already carry — and both backends emit it as an ordinary
  module function. `IsInline` becomes a property read off the decl rather than a reason the decl
  is absent. A top-level `inline` binding gains an exportable identity like any other top-level
  binding.

  **What `InlineBodies` becomes.** It is purely the CROSS-UNIT export channel — `InlineExpansion`
  splices within a unit pre-freeze and never reads it; only `SymbolProviders.collectInlineBodies`
  (serving other units) and `Inline.thawBody` (consuming another unit's template) do. Since the
  same body now lives in `Decls`, the stored channel is redundant with a projection: the export
  form is `rewriteSiblingRefs` applied to the decl. Prefer deriving it at the export seam over
  storing a second copy — but confirm first that the emitted form and the exported form can be
  the same tree (the export rewrites sibling `Var` references to `External` + `SymbolKey`; the
  emitted form resolves those `Var`s through the side tables, and `Freeze`'s own comment claims
  the two agree by construction). If they cannot be the same tree, keep the two forms and say why.

  **LANDED.** Four corrections to the above, all of which the implementation established:

  - **There were THREE drop sites, not two, and the third was the real blocker.**
    `Passes.InlineExpansion` skipped templates entirely (`InlineExpansion.fs:897`), so every
    template violated codegen's own input invariant — un-eta'd `External` values, unspliced
    inline call heads. Three programs crashed on emission until the walk covered them. The two
    known sites (`Freeze.emittable`, `TastLower.lower`) were the visible half of the problem.
  - **Emittability splits in two, exactly along the SRTP/static-opt line.** A `TraitCall` body
    is template-only and always will be: "type `^T` has this member" has no CLR encoding, so
    there is no signature to emit under, and `Inline.substMapper` discharges the node only when
    a SPLICE grounds `^T` to a nominal carrying the member. A `StaticOptimization` body IS
    emittable — clause selection is a compile-time choice and the node carries `defaultExpr`
    for precisely the un-pinned case, which is what the ordinary compiled form is. The CLR
    backend already emitted the default; the JS backend had **no arm at all** and fell to a
    catch-all `failwith`. So the skip is now a checkable predicate on the body
    (`TastLower.hasTraitCall`), justified at the emit site and far narrower than `isInline` —
    it fires nowhere in the corpus.
  - **The emitted and exported forms did NOT unify; both are kept.** Publishing the walked body
    moved two conformance goldens (`arith-uint32`, `arith-unsigned-div`): definition-site
    expansion bakes the generic fallback into every future splice, because a template's
    static-opt clauses and trait calls must resolve against a CALL SITE's operand types, not
    against the nothing that is ground at its definition. `ctx.InlineTemplates` holds the
    unwalked snapshot, taken under the same typar cut. **`InlineBodies` therefore stays a real
    carrier — F.1's carrier list does NOT shrink**, contrary to the hope above.
  - **The top-level export gap is not inline-specific and was scoped out.** No top-level
    binding of any kind is exported (`FrozenSignature.fs:446`) — one may only exist in an exe's
    entry file. A top-level `inline` binding now gains an identity the same way every other
    top-level binding does: by being emitted. Publishing its template would need either a
    Program-holder key minted in `Freeze` (backend layout knowledge in the wrong place) or
    rerouting top-level bindings through `ModuleMembers` (which moves CLR holder placement).

  Goldens: **zero existing goldens moved** — the two-form split is what keeps previously-emitted
  code identical. Coverage added at `test/Codegen.Conformance/inline/` (top-level and named
  module), since the corpus previously contained no `let inline` at all.
- **F.1 Pool the remaining tree carriers. LANDED.** A `TTypeDecl`'s member bodies (`TExpr` at
  seven slots; **no `TPat`** directly, every param being a bare `NodeKey * 'ty`), `InlineBodies`'
  decl trees, and the `ValRepr` pats. **`FrozenFileResidue` is now `Diagnostics` +
  `IntrinsicReprKeys` + `Accessibility` — no tree.**

  - `DeclPayload.Type` carries `TTypeDeclG<FrozenType, SyntaxToken, ExprPoolId>` — the F.1a
    `'body` parameter paying off exactly as intended: same spine, ids in the body slots. The ids
    ride the *shape*, not `DeclExprChildren`, because which body fills which slot is structure a
    flat child column cannot express without a re-nesting record.
  - `FrozenPools.InlineTemplates` is its own root array. Template and emitted function stay two
    independent trees (F.0), neither derived from the other.
  - `ValReprG`/`ArgGroupG`/`StaticParamG`/`CompiledFormG` took `'pat` and dropped the
    now-phantom `'tok`. The file's `BindingValReprs` sits at `PatPoolId`; `ExternalSymbol.ValRepr`
    stays at the DU, since `.fsi`-minted pats belong to no file — the split the flip's
    external-symbol pat pool needs.
  - **No third walk.** `TastConvert`'s type-decl and compiled-form clusters became *bifunctors*
    in `('ty,'body)` / `('ty,'pat)`; the `'ty` freeze is the diagonal, pooling is
    `typeDecl id (poolExpr sink)` and draining is `typeDecl id fromExpr`.
    `FrozenSignature.valReprToDeclaring` collapsed onto the same traversal.
  - **A new binder class surfaced:** a type decl binds `this`/`base`/member params/ctor
    params/ctor locals with **no pattern node**, and member bodies name them by `Var` — so
    `binderIdOf` faulted until `TTypeDeclG.boundKeys` interned them. Pattern-less binders are a
    real category; the flip must not assume a binder implies a `TPatG`.
  - **Deleted:** the `internDeclBinders`/`internExprBinders`/`internPatBinders` helpers (the
    normal walk reaches templates now) and **the entire DU tree codec** — `writeExpr`/`writePat`/
    `writeDecl` and mirrors, ~640 lines, nothing rides opaquely any more. This is what B.k+7
    called for; it lands here because F.1 is what made it dead.
  - Corpus gap closed: it contained no class preamble, secondary ctor, `inherit`, or interface
    impl, so those four went into the round-trip and serialization gates, with a non-vacuity
    test asserting each newly-pooled domain is actually non-empty.
- **F.2 The stacked builder.** The base/overlay pool type, the flat id space, `appendTree`, and
  the row-copy primitives. Unused. *Gate: unit tests on the stack — base ids resolve unchanged,
  overlay edges may name base nodes.*
- **F.3 Flip the accessor. LANDED — both phases.** Every accessor body reads columns; the
  B.k+4/B.k+3 rewires (`binderName` off `BinderNamings`, `lambdaKey` off the lambda's
  `ExprPoolId`) landed with it. **Neither backend contains a single `TExprG`/`TPatG`/`TDeclG`
  reference any more — not even a construction site** (verified repo-wide). Goldens held: 485
  JS, 1407 CLR.

  - **Handle:** one generic `[<Struct; NoComparison>] Handle<'Id> = { Pool: PoolBuilder; Id }`,
    instantiated three ways. `PoolBuilder` is `[<ReferenceEquality>]`, so equality is pool
    identity + id and a handle is a sound dictionary key — two ids denote the same node only
    when they came from the same pool. The pool riding the handle is what lets a consumer speak
    in whole nodes (`e.Body`, `arm.Guard`) exactly as when a node WAS the tree, and lets pools
    that are not a file's tree coexist with it.
  - **No mint goes through a rebuilt DU.** Phase 2 landed in full: the rewrites are native row
    operations (`mapChildren`, `retype`/`retypeWithChildren`, `mapDeclExpr`) and the fresh nodes
    are `mintVar`/`mintNamedPat`/`mintLambda`/`mintAppSpine`/`mintMethodCall`. `appendPatTree` —
    the DU bridge — survives in exactly one production use, `FrozenSignature` pooling an
    external provider's re-axised pats.
  - **Identity went value-keyed.** All six `HashIdentity.Reference` dictionaries are now
    `Dictionary<ExprPoolId, _>`, and the `ReferenceEquals` guards are GONE: `copyExprWith`
    returns the *original* id when the edited row equals the original, which is the id-space
    analogue those guards were hand-rolling. This retires the fragility `Layout.fs:73` recorded.
  - **`TastLower.lower`'s expression walk was DELETED, not ported** — it was structurally the
    identity (`App` collect-then-rebuild the same spine; everything else `mapChildren` of
    itself), so it only ever deep-copied. `lower` is now purely decl flattening.
  - **External-symbol pats** live in their own builder over a zero-column base
    (`TastPoolBuilder.openEmpty`), one per `ValRepr`/provider — the second-pool case the fat
    handle was chosen to allow.
  - **`TastAccessor` grew (1212 → 1266), it did not shrink.** The ~300 lines of DU walking left
    for `TastPools` (now the last DU walker), and the file absorbed the mint API, the generic
    traversal, and the remaining views. The predicted large deletion happened — it just moved.
  - **A third instance of the same bug class surfaced**, because `toPools` is now on every
    compile's path rather than only the cache's: an elided E1 format-alias binding recorded a
    `TopLevelNames` entry for a decl the tree never contains, faulting 5 printf tests. Fixed at
    the producer, keyed off the same `elided` condition that governs the elision, so the two
    cannot drift. Third time this session: **a side table naming a node the tree does not bear
    is the recurring defect of this codebase's identity model.**
- **F.4 Migrate the stragglers.** `FrozenSignature`, `ConformanceTypars`, `SymbolProviders`,
  `Inline.thawBody`. *Gate: green.*
- **F.5 Sever the DU.** `freeze` stops materializing the DU (cheap version: build, `toPools`,
  drop — freeze building columns natively is a separate optimization); delete the DU paths that
  actually died. *Gate: green.*
