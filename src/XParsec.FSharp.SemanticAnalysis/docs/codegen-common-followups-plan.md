# `Codegen.Common` follow-ups — plan

Raised by the comment overhaul of `XParsec.FSharp.Codegen.Common` (5 files, 567 code lines,
1.4:1 — the densest of the four projects). Part A is defects verified against the code;
Part B is type candidates, each naming the comment its type would delete.

Delete this file when the work lands.

`FlatParams` is already tracked as `codegen-clr-followups-plan.md` **A1/B1**, which concludes it
belongs in this project. The `Codegen.Js` sweep reached the same confusion independently and
closed its own half with the JS-local `TailParams`, which does not reach the CLR call site. Do
not re-file it here; extend that entry instead.

---

## Part A — defects

### A1 — no FS1113-equivalent accessibility check on a published `inline` body

`SymbolProviders.liftMemberBody` publishes any `member inline`, and the value half publishes
any `let inline` (`Elaborate.fs:288-290`, guarded only on the `isInline` flag). Neither checks
that the body names only symbols a *consumer* assembly can read. F# raises FS1113 for this
("marked inline but its implementation makes use of an internal or private function which is
not sufficiently accessible"); `grep -rn 1113 src test` finds nothing in the tree.

Concretely reachable: a class's compiler-generated backing storage (primary-ctor params,
preamble `let`s, `static let`s) is emitted `FieldAttributes.Assembly`
(`Codegen.Clr/LayoutNodes.fs:260`, `compilerGeneratedStorage`), and a `member inline` body —
unlike a module-level `let inline` — is in scope to name one. A `FieldGet` on it is spliced
into a consumer that cannot read it.

### A2 — a broken `impl` file publishes zero inline bodies, silently

`SymbolProviders.inlineBodies` swallows two error arms as `| Result.Error _ -> ()`. The
manifest arm is defensible — `orderedManifestsWithDeps` already `failwithf`s on a malformed
manifest before this runs (verified: `closeAndOrder`'s `load` sets `error` on a `loadManifest`
failure, `ReferencedProject.fs:374-385`). The `VesperLib.parseFileFull` arm has no such
upstream gate: an `impl` file that fails to parse contributes no templates and the compile
proceeds, so a cross-package `inline` degrades to an unresolved symbol instead of a diagnostic
naming the file that failed.

### A3 — `buildWith` has one caller and its doc named a consumer that does not exist

`SymbolProviders.buildWith` (uncached, layer-1 only) is called once, by
`ClrSymbolProviders.build`, which has no caller outside its own module. Its doc claimed the JS
backend injects a tail through it; it does not — the JS backend reaches Common only via
`buildContractWith "jsnative"` (`Codegen.Js/JsNativeSymbols.fs:195`) and
`buildContractWithMetadata "tsmanifest"` (`Codegen.Js/TsManifestProvider.fs:218`). Decide
whether the uncached path should exist at all.

### A4 — `StructuralFormatRecipe` is a `src/` module with no `src/` consumer

Nothing under `src/` reads `recordRecipe` / `unionCaseRecipe`. The only consumer in the tree is
`test/XParsec.FSharp.Codegen.Js.Tests/StructuralFormatRecipeTests.fs`, which renders the
*expected* strings from the recipe and the *actual* ones from the JS runtime. Both runtimes
re-encode the forms themselves (`Vesper.Printf/structural-printer.clr.fs:296` says so in its
own comment: "the record/union layout policy (once `StructuralFormatRecipe`) lowered into the
…"), and `printf-architecture.md:290-292` records the decision: "the CLR emitter no longer
consumes it".

Two consequences, neither currently visible from the file:

- The CLR runtime is tied to this grammar by nothing at all — no call path and no test. The
  file's old header claimed "both targets are pinned to this grammar"; only the JS one is.
- `StructuralFormatRecipeTests.fs:9` still asserts "The CLR backend lowers the recipe to IL
  directly". False since the lowering moved into the runtime — a stale comment in a file this
  sweep did not cover.

Decide whether the oracle belongs in the test project, or whether the CLR runtime should be
differentially tested against it too.

---

## Part B — type candidates

### B1 — a named type for the `{ platform-repr → [canon] }` reverse map

The map is a bare `Map<string, SymbolKey list>` threaded through `MetaTailFactory`,
`IExternalSymbolProvider.IntrinsicReverseCanon`, `ClrSymbolProviders.seeded` / `seedTag`,
`MetadataMapping.tryBuildType` and `PassContext.IntrinsicReverseCanon` — a string key with no
type saying which direction it runs in, which is why every site re-explains it.

Deletes: the surviving 3-line `MetaTailFactory` doc in `SymbolProviders.fs` — the map "folded
from the layer-1 providers' `IntrinsicReverseCanon`, the reverse direction of
`type int = (# "System.Int32" #)`". **This is now the only site in the tree stating where the
map comes from**; the CLR sweep cut `MetadataSymbols.fs` back to the `System.Int32 → [int]`
pairing alone. If the type lands, the derivation must be in its name/definition, not lost.

### B2 — a member does not know its own function type

`liftMemberBody` carried a 30-line block (the longest in the repo, now 3) because a
`TastAccessor.TypeMember` cannot state the function it *is*. The lift rebuilds the curried
shape by hand from `ThisKey` / `ThisTy` / `Params` / `ReturnTy`, minting one lambda per
position and a filler boundVar purely to reach the `TDecl.Let(_, value, _, declTy)` shape
`Inline.inlineExpand` matches (verified at `Inline.fs:275`).

Candidate: have a member carry its `this`-first curried parameter list and `declTy` (or expose
a projection to that effect in SA), so the lift is a projection rather than a construction.

Acceptance: both survivors go — the 3-line doc "`member inline _.M p0 p1 = body` IS the inline
function `M this p0 p1 = body`" becomes a signature, and the "filler boundVar that keeps the node
total" comment has nothing left to explain.

### B3 — `Contract` makes the correct pairing convenient, not mandatory

`Contract.Provider` and `Contract.Origins` are independently readable fields, so a caller can
still resolve a served body's position against an anchor domain from a different manifest set —
a wrong answer that is in range. The record only makes the right pairing the easy one, which is
why the 3-line type doc has to warn about it.

Candidate: make the anchor domain reachable only through the served provider, so the
mismatched pair is unrepresentable.

Deletes: the `Contract` type doc's second and third lines.

### B4 — a node-keyed table and the derivation that makes it readable are two loose values

`InlineExpand.Expansion` hands back `Origins` (keyed by nodes a *later* rewrite may re-author)
and `Derived` as two independent fields, so every consumer must remember to route lookups
through `Derivation.tryFind` and nothing stops a bare `TryGetValue`. Both backends re-state the
pairing by hand: `Codegen.Js/EmitJsContext.fs:32-38` documents it on two adjacent `WalkCtx`
fields and `Codegen.Clr/Layout.fs:41-55` rebuilds it for `FunVerdicts` with an explicit
`resolveAll` merge. Verified: `Layout.fs:52` is load-bearing — without it a value-struct
closure emits as a heap one.

Candidate: a `Derived<'a>` lookup that owns both, whose only reader walks the chain, so an
un-chained lookup on a post-expansion node is unrepresentable.

Deletes: `Derivation`'s 3-line doc, the "Keyed by the copied node, so read it through
`Derived`" sentence on `Expansion.Origins`, `tryFind`'s "NEAREST … chain" sentence, and the
duplicate warning on `WalkCtx.NodeOrigins` / `WalkCtx.Derivation`.

### B5 — the walk threads three state axes that must move together

`go` / `copyNode` / `expandEdge` in `InlineExpand.fs` each take `domain`, `entered` and `site`
as separate parameters, and the one place they interact — the `ExprShape.CallerExpr` arm, which
switches domain, pops the entry stack *and* restores the caller's site in a single step — is
enforced by nothing but that arm being written correctly. Restoring two of the three compiles.
That is why the invariant is spelled in prose on three separate private types (`Domain`, `Site`,
`Entered`) rather than in one signature.

Candidate: one frame record carrying the three axes, with push/pop as its only constructors.

Deletes: `Entered`'s doc ("They pop together …") and `Site`'s second sentence.
