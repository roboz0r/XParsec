# Inline provenance — deferred body placement via `TExpr.InlineCall`

**Status (2026-07-29): designed, not started.** Replaces the physical TAST-level splice
performed by `Passes/InlineExpansion.fs` with a resolved-specialization table plus an
`InlineCall` edge node, so that a body's ORIGIN FILE survives into the frozen TAST and
codegen flattens the graph at emit. Delete this doc once it lands (or is abandoned) per
`feedback_plan_docs_ephemeral`. Step numbers below are for this doc only — they must not
appear in code or test comments (`feedback_no_milestone_refs_in_code`).

## The premise (confirmed 2026-07-29)

### `relocate` is forced by the `Anchor` invariant, not chosen

`Anchor` is an index into ONE file's `Lexed`, deliberately narrowed to an `int` for pool
width (`Anchor.fs:6-12`). The doc comment states the consequence outright: *"An index is a
position IN ONE FILE and means nothing against another's tokens, which is why a tree
leaving its pool (`Wire.TDecl`) sits `nowhere` throughout."*

So splicing a body into a consuming decl array puts its nodes into a tree whose anchors
must all be readable against the CONSUMER's file. `Inline.relocate` (`Inline.fs:423`)
overwrites every node's token accordingly; `Inline.spliceAt` (`:429`) is
`freshen |> relocate`. `Inline.thawBody` takes the call site `at` as a parameter and maps
every anchor to it (`Inline.fs:82`, `:98`).

**Correction (verified on disk 2026-07-29).** The earlier claim here — that a `Wire.TDecl`
"carries no position at all" — is FALSE, and it made this step look far larger than it is.
`Wire.TDecl` is `TDeclG<FrozenType, Anchor, NodeKey>` (`TastPoolNodes.fs:169-173`): the
position axis IS `Anchor`. The drain passes the producer's real anchors straight through —
`patTree`/`exprTree`/`declTree` call `TastUnpool.substitute*` with `row.Tok` and
`widenTok = id` (`TastPoolBuilder.fs:490`, `:499`, `:540`). Nothing widens or blanks them.
The producer's anchors reach the consumer intact and are destroyed at exactly ONE call
site, `Inline.fs:98`'s `(fun (_: Anchor) -> at)`.

There is also no wire SERIALIZER: `Wire.TInlineValue`/`TInlineBody` never meet a codec, the
whole path being in-process. So there is no format to widen and no round-trip to extend.

Five doc comments currently assert the false version and must be corrected as part of this
work, not left to rot: `Anchor.fs:11-12`, `TastPoolNodes.fs:162-167`,
`TastPoolBuilder.fs:523-525`, `ExternalSymbols.fs:90-92`, `Inline.fs:76-81`.

The deeper reading: the wire's anchor axis has NO type-level domain marker, so a producer
anchor and a consumer anchor are the same type. Today that is safe only because the single
consumer relocates. The prose in those five comments was enforcing by assertion what the
type does not — which is precisely the invariant this work should move into the type.

**Worked trace.** A same-unit `let inline f` whose body calls a cross-unit `val inline g`,
called at token `T`:

1. `expandAt T` → `Inline.spliceAt mint T` (`InlineExpansion.fs:501`) — every node of
   `f`'s body now carries `T`.
2. the walk descends to the `External g` head, which now also carries `T`, and calls
   `thaw T` → `thawBody store T` (`InlineExpansion.fs:373`) — every node of `g`'s body
   carries `T` too.

Both bodies collapse onto one token. Nothing downstream can distinguish a node written in
the consuming file from one that came from `f` from one that came from `g`, and nesting
depth is unrecoverable.

### The consumer already holds the producer's `Lexed`

`SymbolProviders.inlineBodies` resolves the manifest's `inline-bodies` key to `.fs` PATHS
and parses them itself — `VesperLib.parseFileFull file` over each resolved
`VesperLib.LibFile` (`SymbolProviders.fs:211-219`). So the producer's token stream is
already materialised in the consuming build.

But it is TRANSIENT (verified 2026-07-29): `parseFileFull` returns `{ File; Input; Lexed;
Ast }`, and after `collectInlineBodies` harvests the `Wire.TInlineValue`s, `parsed` — with
its `Lexed`, its `Input` and its `LibFile` identity — goes out of scope at the end of the
loop body (`SymbolProviders.fs:246`). Nothing returned reaches it: `FrozenPools` has no
`Lexed` and no source-text field. So "the consumer already holds it" is true of the moment
and false of the artifact; retaining it is real new plumbing, not a re-wiring of something
already kept. The `LibFile` that step 2 wants for `OriginFile` is likewise already
constructed, at `SymbolProviders.fs:212-217`, and likewise dropped.

That is the whole opening: **give the anchor domain a home on the enclosing entry, and the
positions become recoverable with no position baking and no wire widening.**

## The shape

**`OriginFile` belongs on the entry, not on the node.** A specialization table entry
carries one file identity (`LibFile`-shaped: `BucketName` / `Relative` / `Absolute`); every
node inside that entry resolves its `Anchor` against THAT file's `Lexed`. The node column
stays an `int` and keeps the narrowing `Anchor.fs:9` bought. A physically spliced body has
no enclosing entry to hang an origin on — which is exactly what deferral restores. Note it
is a FILE and not a unit: a unit has many files, and an anchor indexes one.

**Provenance lives on the edge.** `InlineCall` carries `{ Spec: SpecializationId; CallSite:
Anchor }`. The frame chain is reconstructed by walking up edges rather than stored per
node — O(call sites), not O(nodes), and no two nodes of one body redundantly carry an
identical chain:

```
node                       anchored in g's file        (g's body)
  ↑ InlineCall             anchor = the `g` ref inside f's body, in f's file
  ↑ InlineCall             anchor = T, in the consuming file
```

**One entry shape.** An entry is always a `TDecl.Let` of lambdas; the node always carries
args. Parameters that `reduceApplication` FUSED are simply not parameters of the entry, so
arity = surviving-parameter count and there is no double-counting. Keyed by `(templateKey,
resolvedTypeArgs)`; entries reference entries, so the table is a DAG.

**Outlinable ⟺ closed over its parameters.** All three of `reduceApplication`'s vanishing
mechanisms fuse call-site material and therefore leave free variables: the bare-`External`
value substitution (`InlineExpansion.fs:559`), inline-first lambda elimination (`:581-586`),
and `[<CallAtMostOnce>]` substitution (`:639-640`). Since both backends ultimately splice,
this is NOT a policy input — it is a validity assertion (an entry with free vars must have
exactly one call edge), and it is the check that catches a fusion bug turning into a body
silently shared across sites.

## Why the mechanism half-exists

`InlineTemplates` is already a separate pool root array with its own codec and unpool path
(`PassContext.fs:954`, `Elaborate.fs:2096`, `TastPoolTypes.fs:323`, `TastUnpool.fs:353`,
`FrozenCodec.fs:500`). This adds RESOLVED entries alongside the existing templates and an
edge node — it does not invent a side table.

Resolution itself does not move. Static-opt selection, SRTP trait dispatch and typar
grounding stay pre-freeze in `Inline.substMapper` / `deriveInlineTypeArgs`, where union-find
is native and `PassContext` is in hand; the `TraitNotSupported` diagnostic
(`InlineExpansion.fs:304`) still fires before codegen. Only PLACEMENT is deferred.

## The work, in order

1. **`TExpr.InlineCall` case + specialization table.** Add the node and thread it through
   the surviving-node pipeline. The table parallels `InlineTemplates` as a second root array.

   The touch list was measured off `ILIntrinsic`, which turns out to be a POOR calibration
   node (corrected 2026-07-29). It over-counts: `ILIntrinsic` is spelled in SOURCE syntax
   (`(# ... #)`), so its arms in `ElaborateExpr.fs`, `CstWalk.fs`, `CstKeys.fs`,
   `Elaborate/*`, `Passes/Unification/*` and `Passes/NameResolution/*` exist for parsing
   reasons that do NOT apply to a compiler-minted, source-unspellable `InlineCall`. And it
   under-counts: the doc's list names `Tast.fs` and `TastPools.fs`, which contain zero
   `ILIntrinsic` mentions, while omitting `TastPoolTypes.fs:323` (where the `InlineTemplates`
   root array is actually declared) and all eleven backend files that do mention it —
   `Codegen.Clr`'s `EmitIntrinsic.fs`, `ClrRecipes.fs`, `Cil.fs`, `ClrProvider.fs`,
   `EmitLower.fs`, `EmitExpr.fs`; `Codegen.Js`'s `EmitJs.fs`, `JsEmitHelpers.fs`,
   `EmitJsFormat.fs`, `JsAst.fs`; and `Codegen.Common`'s `SymbolProviders.fs`.

   Edit files on the merits, not by analogy to `ILIntrinsic`. `Hashing.fs` and
   `FrozenCodecRows.fs` have zero mentions; they need arms only if the new root array
   genuinely requires them.

2. **`OriginFile` on the entry; stop relocating.** Materially SMALLER than first written —
   the wire already carries producer anchors (see the correction above), so there is no
   format change and no widening. Three things: retain the `Lexed` + `LibFile` past
   `SymbolProviders.fs:246` and thread a `file → Lexed` map so an anchor inside an entry is
   resolvable; delete the `(fun (_: Anchor) -> at)` collapse at `Inline.fs:98` for entry
   construction, so `thawBody` keeps the producer's anchors; and correct the five stale doc
   comments listed above. `Inline.relocate` and the `at` parameter of `thawBody`/`spliceAt`
   go away for entry construction. `Anchor.toStored`/`ofStored` already round-trip anchors
   for the frozen-pool path (`FrozenCodecPrimitives.fs:246-247`, driven from
   `FrozenCodec.fs:487`, `:493`, `:502`), so persisted templates already keep real anchors.

   **`OriginFile` carries a representative content hash** (decided 2026-07-29). An entry's
   anchors are integer indices into a file the consumer re-reads on a later build, and the
   frozen tree is cached to disk (`FrozenCache.fs`). If the producer file changed, every
   anchor silently resolves to the wrong token — wrong source maps, wrong diagnostics, no
   error. `Hashing.dependencySignatureHash` already reads every source file a referenced
   package names (`Hashing.fs:121-123`) and folds it into `compilationDigest`, so the
   `CacheKey` does invalidate today — but only incidentally, as two unrelated subsystems
   happening to agree. Put the hash on the entry and make a mismatch a hard failure, so the
   invariant is checked where it is relied upon. Crude and local on purpose: no separate
   query-engine/salsa-style extraction is in scope here.

3. **Rewire `InlineExpansion.run` to emit edges + entries.** `expandAt`
   (`InlineExpansion.fs:488`) already computes exactly "template + call-site type args →
   resolved body"; the change is to intern that result under a `SpecializationId` and emit
   an `InlineCall` instead of returning the tree. `reduceApplication` keeps its fusion
   logic but reports which parameters survived, fixing the entry's arity. The nullary
   intrinsic direct splice (`:813`) becomes an ordinary entry — it crosses a file boundary
   too, so uniformity is the accurate representation. `etaReify` (`:421`) mints an
   `InlineCall` rather than an `App`.

4. **Cycle detection + closure assertion.** The DAG needs an explicit acyclicity check with
   a real diagnostic. I found no recursion guard in `InlineExpansion.fs` — confirm none
   exists elsewhere; today a self-referential inline appears to recurse until the stack
   goes. Add the closure assertion from the shape section alongside it.

5. **Backend flattening in `Codegen.Common`.** A shared recursive graph expansion that
   splices entries at emit, maintaining the frame stack as it descends. Both backends
   consume it: JS is the branch's target, and CLR's handling is the trivial always-splice
   but has to stay green.

   The pre-freeze eta rationale (`InlineExpansion.fs:405-411`) claims CLR lowering never
   walks member bodies. VERIFIED 2026-07-29 and safe to rely on: `EmitLower.fs:28` forwards
   to `TastLower.lower`, whose `lowerOne` discards `DeclShape.Type` outright
   (`TastLower.fs:553`), and member bodies live only inside `DeclShape.Type`
   (`TastPoolNodes.fs:184`). The same claim is stated normatively at `TastLower.fs:476-481`.
   The CONSEQUENCE for this step is the trap: because CLR lowering never descends into type
   decls, an `InlineCall` inside a member body is invisible to `TastLower` and must be
   flattened by whatever emits members, not by `lower`. A flattening hung off `lower` alone
   would silently miss every member body and pass its tests.

6. **Multi-source `JsSourceMap`.** `JsSourceMap.build` today takes a single `src.Path` /
   `src.Content` off `project.Source` (`Codegen.fs:151`). The V3 segment encoding already
   carries the source-index axis — `[genColΔ, srcIndexΔ, srcLineΔ, srcColΔ]`
   (`JsPrint.fs:415-418`) — it is simply always 0. Extend to `sources[]` /
   `sourcesContent[]` with a live index, fed by the frame stack the emitter is already
   walking in step 5.

   This is the end-to-end proof and the reason it is in scope: an inlined `g` body landing
   on `g`'s own line in a browser debugger is a test that cannot be faked by a
   representation that merely looks right.
