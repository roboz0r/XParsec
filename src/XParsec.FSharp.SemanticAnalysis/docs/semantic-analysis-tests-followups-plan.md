# SemanticAnalysis.Tests follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent unless they pin a specific reported diagnostic — they rot.

Raised by the comment-hygiene sweep of `test/XParsec.FSharp.SemanticAnalysis.Tests`
(70 files, baseline 16355 code / 3783 comment, 4.3:1, 584 blocks of 3+ lines, longest 50).

Reading every comment against the code it claims to describe turns up work the sweep itself
cannot do, because it changes code:

- **Part A — defects.** Wrong assertions, tests that pass on the wrong thing, duplication.
- **Part B — prose that should be a type.** A comment that was genuinely load-bearing and
  long, where the durable fix makes the sentence unnecessary. Each names the comment it
  would delete — that naming is the acceptance test.

Batches swept so far: 1 (Conformance/Coverage, TAST pools, assembly & project refs),
2 (signature extraction, elaboration & inline, scoping & resolution order),
3 (regions & diagnostics, verdicts & validation, keys & hashing),
4 (external stamps, frozen & cache, unification),
5 (types & printf, shared helpers & resolution, small files). **Sweep complete — all 70 files.**

---

# Part A — code defects

## A1. Four resolution failures are unclassified `Kind.Message`, so tests substring-match prose

`Kind` classifies its neighbours — `UndefinedType`, `UnresolvedQualifiedName`, `NoMember` —
but four resolution failures report free text through `Kind.Message`:

- `Passes/NameResolution/Scope.fs:135` — `"Unresolved identifier: %s"`
- `Passes/Unification/InferResolve.fs:273` — `"Unknown record type qualifier: %s"`
- `Passes/Unification/InferResolve.fs:284` — `"No record type matches the field set: %s"`
- `Passes/Unification/InferResolve.fs:291` — `"Field set is ambiguous (%d candidate…)"`

So every test wanting one of them matches on rendered text. `CoverageTests.fs` decides 15
assertions with `d.Message.Contains` (`"mismatch"`, `"first-class value"`, `"for-in"`,
`"downcast"`, `"Unresolved qualified name"`); `AssemblyFilesTests.fs` does it in five places.
A test can pass on the wrong diagnostic, and a message reword flips assertions for no
semantic reason. `DiagCode` already exists and `ConformanceTests.fs` uses it properly.

The same file that substring-matches also states the rule it is breaking: *"Selected on the
verdict the diagnostic carries, not a rendered code, so a renumbering cannot break it."*

Classifying the four collapses **B1** with it.

Would delete: the `unresolvedErrors` and `definitionErrors` docs in `AssemblyFilesTests.fs`,
the three inline `Message.Contains` filters in its record read / construct / pattern tests,
and the `hasRangeValueError` doc in `CoverageTests.fs`.

## A2. `unit` was rewritten to "file" in test names and assertion messages

Commit `fb3d49cd` ("Rephrasing unit to file or other noun") over-applied onto the F# TYPE
`unit`. Every site below asserts `BuiltinTypes.tyUnit` while saying "file":

`CoverageTests.fs` — `test "try-finally cleanup must be file"`,
`"non-file cleanup triggers mismatch"`, ``test "`x <- y` types as file"``,
``test "`for i in 1..10 do ()` types as file with no diagnostic"``,
`"binding type is file"`, ``test "`let () = expr` rejects non-file RHS"``,
`"non-file RHS triggers mismatch"`, `"no args (file-arg fold)"`.

Test names are failure output, so these are code and the sweep could not touch them.
**That commit touched ~90 files across `src/` and `bench/`** — the same over-application
should be grepped for repo-wide, not just here. (`AssemblyFilesTests.fs`'s "cross-file" /
"prior-file" matches are legitimate.)

## A3. `TastShape.fs` carries the `SemType` renderer twice

Module-level `tyName` and the `let rec tyStr` nested in `Renderer.Decl`'s `TDecl.Type` arm
are line-for-line identical over all 17 `SemType` cases — same `TyConditional` assembly, same
`!`/`!!` typar spellings; only the recursive call name differs. A new case must be added
twice, and a divergence between a cast target and a member signature stays invisible until a
golden contradicts itself. Delete `tyStr`, point its call sites at `tyName`.

## A4. A test name carries a plan-doc milestone label

`ReferencedProjectTests.fs`: `test "short names no longer resolve through the provider
directly (O3)"`. `(O3)` cites a document the reader cannot open and "no longer" dates the
test to a submission. Rename to state the rule.

## A5. Two comments were false and were deleted — noted in case they point at real drift

- `AssemblyFilesTests.fs` cited *"see the `ptest` below: construction pins the registered
  `InModule` identity while the annotation carries the re-cut flattened one, and they
  disagree."* There is no `ptest` in the file, and the test that IS there — *"cross-file
  MODULE-HELD type: annotation identity matches construction identity"* — asserts they agree.
- `ReferencedProjectTests.fs` cited the JS shim as manifest key `files-js`. The real key is
  `[targets.js]` + `files`; a sibling test asserts dashed spellings like `impl-js` are
  rejected.
- `TastShape.fs` documented `SymbolKey.MemberKey` — no such case; it is
  `SymbolKey.Member of MemberKey`, field `Decl`. Corrected in place during the sweep.

## A6. `moduleBindings` can list an `inline` binding twice, inflating a vacuous-pass guard

`FrozenSignatureTests.moduleBindings` returns `fromDecls @ fromInline`, and a `let inline`
appears in both `Decls` and `InlineBodies`. One caller dedupes incidentally (`List.find`); the
other is the parity oracle, which increments `checked'` per iteration and closes with
`Expect.isGreaterThanOrEqual checked' 7 "all parity bindings compared"` — a guard whose whole
job is to catch a vacuous pass. A duplicate satisfies it without a distinct comparison.
Latent only because that test's source declares no `inline` binding; the source the sibling
tests use does (`let inline twice`).

Return a `Map<string, SymbolKey>`, so a duplicate cannot reach either caller.

Deletes: the second sentence of the `moduleBindings` doc, which exists only to warn about it.

## A7. `thawedTemplate`'s `namespace` + `module` wrapper rests on a claim the code contradicts

`InlineTests.thawedTemplate` wraps every fixture in `namespace Ns` + `module M =`. The
justification (deleted as false) said a top-level binding "lives in the anonymous Program
container and is published nowhere". But `Elaborate.fs:139` `recordExportedBinding` files a
`ModuleBindingInfo` for any binding with an emitted name and a bound variable, under whatever
`ModuleContainer` it was passed — no declaring module required — and `Freeze.fs` publishes on
the presence of that info. The sibling test in the same file asserts a top-level `let k = 3`
keys as `valueKey (ModuleContainer.InNamespace NamespaceKey.Global) "k"`.

So either the wrapper is dead ceremony, or a top-level `let inline` genuinely fails to publish
and nothing covers it. Add the unwrapped case; whichever way it lands is a fact worth a test.

## A8. `DesugarTests` "nested InfixApp" asserts a count where it means two keys **[MOOT — the file is deleted]**

`Expect.isGreaterThanOrEqual ctx.Desugared.Count 2` could not catch a wrong key — which is
exactly why the comment above it carried wrong offsets (8/12; the operators in
`let x = 1 + 2 * 3` are at 10 and 14) undetected. `b3ed35d6` deleted `DesugarTests.fs` along with
the `Desugar` pass and `ctx.Desugared`, so there is no assertion left to tighten.

## A9. Test helpers are re-declared per file, twice with the SAME NAME and DIFFERENT types

`analyse` is declared privately in **32** files of this project; `errors` and `expectClean`
are re-declared verbatim across the scoping five. Worse, a grep on one name gives two matches:

- `nominalKey : SemType -> TypeKey` (`FileOrderScopingTests.fs:56`) vs
  `nominalKey : SemType -> SymbolKey` (`ModuleScopingTests.fs:94`)
- `soleModuleLetType : TastFile -> SemType` (`FileOrderScopingTests.fs:28`) vs
  `soleModuleLetType : string -> SemType` (`TypeScopeOrderTests.fs:96`) — one takes an
  analysed TAST, the other re-analyses from source.

Candidate for `TestHelpers.fs`. Note the sweep is finding these one batch at a time; worth a
single pass at the end rather than five partial merges.

## A10. `NameResolution.fs:87` — a diagnostic message carrying a design essay

The duplicate-field diagnostic is a four-sentence string ending "that pass is not implemented
yet, so rename one of them". Not a comment, so the sweep could not touch it, but it is the
same prose and the same debt: ctor params, `val` fields and preamble `let`s all mint a field
carrying their source name, and F# uniquifies by position. The real fix is that missing
uniquification pass, after which both the message and the test block go away. Until then the
message should carry the rule, not the roadmap.

## A11. `Frozen.TastFile` / `Pooled.TastFile` name trail — fixed here, possibly live elsewhere

`FrozenSignatureTests.duOf` documented its result as `Frozen.TastFile`; `TastUnpool.ofPools`
returns `Pooled.TastFile` (`TastUnpool.fs:228`). Both are real types one word apart
(`TastFileG<FrozenType, SyntaxToken, NodeKey>` vs `TastFileG<FrozenType, Anchor, BoundVarId>`),
which is why it read plausibly. Corrected in this sweep.

Two comments **outside this project** name `Frozen.TastFile` and were not verified:
`Codegen.Js.Tests/FrozenCodecTreeRoundTripTests.fs:9` and
`Codegen.Js.Tests/FrozenCodecRoundTripTests.fs:18`.

## A12. The resolve-once premise is not uniform — a measure carrier resolves by name

`Passes/Unification/Translate.fs:186` disables the by-name external hook explicitly
(`resolveBareTypeName ctx li.Idents.[0] (fun _name -> ValueNone)`), but line 226 passes a live
one to the same helper (`resolveBareTypeName ctx carrierTok (fun name -> tryResolveExternalType
ctx name EqArray.empty)`). So an external type CAN still be reached by spelling, at the
measure-carrier site, with no recorded verdict. Either the rule holds everywhere and that
callback should go, or it does not and the suite's premise needs restating.

The comment stating the premise as absolute (*"a missing verdict is invisible on the read
side … has no by-name fallback, so nothing else in the suite would catch a regression"*) is
deleted.

**This is the whole of it.** An enumeration of `Passes/Unification/` (see withdrawn **B27**)
found no other opens-aware by-name escape: no `ctx.Resolver.TryLookup*` call at all, every
`ctx.Provider` lookup by key, and every name-minted key a fixed fully-qualified canonical
name. So closing this one callback makes "a consumer never re-resolves on a stamp miss" true
by enumeration, with no visibility refactor behind it.

## A13. `regionOf` in `RegionsTests.fs` is dead, and triplicated

`RegionsTests.fs:66` `let private regionOf` has no call site. The three tests needing a region
identity each define their own local `regionOfPattern` / `regionOfKey` with the same body. Its
old doc claimed the opposite — *"Used by tests that need to check identity"*. Delete it, or
route the three copies through it.

## A14. A stale citation the citation guard structurally cannot catch

`RegionsTests` named `processBindingGroup` as the site pre-recording sibling regions.
`Passes/Regions.fs` has `letChainRegion` / `withBindingGroup` / `processBinding` — no
`processBindingGroup` exists anywhere. Rewritten to state the fact without the name.

The general point: **`CitationTests` scans `src/` only, never `test/`**, so every stale
`Module.func` citation in the test project is unguarded. Extending the guard to `test/` is
cheap and would have caught this one.

## A15. `unpackActivePattern` in `CitationTests` never fires

`ownMembers` unpacks `|EApp|_|`-style compiled names into their cases, but none of the 20
entries in `guarded ()` declares an active pattern — confirmed by grepping `let (|` across
every guarded module, all zero. The old doc illustrated it with `TastAccessor.EApp`, and
`TastAccessor` is not in the table (nor is there one backticked `` `TastAccessor.X` `` in
`src/`). Either add `TastAccessor` to `guarded ()` — it is an accessor surface prose plausibly
cites — or drop the unpacking.

## A16. Two false claims in `SymbolKeyTests`, deleted

- *"The `(name, arity)` CLAIM table stays namespace- and module-blind"* — false.
  `TypeRegistry.TypeClaims` maps a name to a LIST of `TypeIdentity`, each carrying its
  `Container`, and the winner is the max `claimRank useSite reaches` (`TypeRegistry.fs:317-343`).
  Claims are scope-ranked. Whether two same-named sibling-module types still collide is worth
  confirming; if they do, the honest form is a failing test, not a hedge in a passing one.
  Note the helper calls `tryTypeClaim … UseSite.unbounded`, which admits every claim and picks
  arbitrarily.
- *"This is what makes the renderer INJECTIVE: `N.A.T` and `N.B.T` no longer collapse onto one
  name"* — those two strings differ under any rendering, so the example does not show the
  property. The real collapse case is a `module A.B`-held `T` against a nested
  `module A = module B =` one. If injectivity of `typeMetaName` matters, it wants that test.

## A17. A citation to a TODO that does not exist, excusing a missing test

`ValidationTests` carried *"Binding-level return-type annotations … Unification doesn't process
them yet — see the TODO in Passes/Unification.fs."* There is no such TODO, and `Infer.fs`
handles `b.returnType` on both binding paths. What is actually missing is the test the claim
was excusing: no case covers value restriction on `let mutable f : int -> int = fun n -> n + 1`.

## A18. Two verbatim 13-field `TastFile` literals in `ResolvedTypesTests`

The two synthetic-TAST tests differ only in `Decls`, so any field added to `TastFile` must be
added twice in test code. Wants a builder or a `withDecls` helper. No comment attached —
recorded because it was found while reading.

## A19. `TypeInfos.AllowNullLiteral` is stored and never read

Declared at `TypeInfos.fs:401`, decoded in `AttributeDecode.fs`, assigned once at
`Passes/NameResolution/MemberRegistration.fs:554` — and **no reader anywhere in
`src/XParsec.FSharp.SemanticAnalysis`**. Unification lets `null` type as a fresh `TyVar` that
the annotation links to `TyClass`, so `let x: C = null` is accepted for ANY class.

The test `` [<AllowNullLiteral>] permits `let x: C = null` `` in `UnificationClassesTests.fs`
therefore passes with the attribute deleted. It asserts nothing about the attribute, and its
name says otherwise.

## A20. `FrozenTypeTests` cannot notice that its round-trip law is false on `FTLocalTypar`

`sampleFrozenTypes` reaches every `FrozenType` constructor except `FTLocalTypar` — and adding
it would not fix anything, because `FrozenTypeBridge.ofFrozen` mints a fresh `TyVar` for one
(its own doc at `FrozenTypeBridge.fs:78` says so) and `toFrozen` rejects a `TyVar`. So
`ofFrozen >> toFrozen = id` is simply false on that constructor.

Nothing states this. The coverage test hand-lists 14 `SemType` tags and omits `TyVar`, so a
constructor whose image is `TyVar` is invisible to it, and `mapVariantTests` reuses the same
sample for its identity claim. `FrozenTypeTableTests`' sample does intern two, so the
constructor is covered for the row codec but not for the bridge.

Fix shape: a `toFrozen`-total subset the sample is drawn from, or an `FTLocalTypar` case in the
oracle that asserts the throw. See **B26** — the stringly-typed coverage list is what let it
through.

## A21. Blob-size ceilings sit ~1.7× above the last measured figures

`FrozenBlobSizeTests` ceilings are 640 / 360 / 640; the last measured compressed sizes were
**385 / 207 / 371**. A 60% inflation passes the gate silently. The measured figures lived only
in the 50-line header this sweep deleted, so they now exist nowhere in the repo — recorded
here so they are not lost. Fixed together with **B25**.

## A22. `ExternMemberElabTests`' `idW` / `addW` lookups never check the bound name

Both the self-type-key and SRTP tests pull the binding type with
`List.tryPick (function TDecl.Let(TPat.NamedSimple _, _, _, ty) -> Some ty | _ -> None)` —
the FIRST `NamedSimple` let in the file — while the failure message claims it looked for
`idW` / `addW`. Right today only because each source has exactly one module-let; a second one
ahead of it silently moves the assertion. The sibling lookup in the same file *does* guard
with `t.Name = "widget"`, so the file is inconsistent with itself.

## A23. `IntrinsicKeyStampTests.externalKey` only walks `TDecl.Let` values

The walk is `for d in tast.Decls do match d with TDecl.Let(_, value, _, _) -> mapExpr …`, so
an `External` node minted inside a `TDecl.Type` member body is invisible. The `None` branch
then reports "no `%s` External node found", which reads as "the node was spliced away" when it
can equally mean "the node is in a decl shape this helper never visits". Walk all decl shapes,
or say so in the message.

## A24. Three fabricated or contradicted claims in the unification tests, deleted

- `UnificationClassesTests` — the nested-let typar test claimed `'b` appears in `Comparer<'b>`.
  The source reads `let g : 'b -> 'b` and no `Comparer` exists in the file. Fabricated.
- `UnificationTestHelpers` — *"the front end does not yet form a user-declared overload SET"*,
  contradicted by its own sibling test, which analyses full source and picks by parameter type.
- `UnificationOverloadsTests` — *"Fails TODAY: `TyVar` vs `TyConst` is not matched by the
  pre-`matchTypes` filter"* on a test asserting `chosen.IsSome`. **Settled**: the file contains
  no `ptest`/`ftest` and the full suite runs green, so the test passes and the claim was stale.

## A25. Extend the citation guard to `test/`

Recorded at **A14** and confirmed again this batch: every stale `Module.func` citation the
sweep found in the test project was unguarded because `CitationTests` scans `src/` only. The
sweep has now removed them by hand; the guard is what stops them coming back.

## A26. A rejected ctor-arg shape shifts every later parameter's position

`Passes/NameResolution/MemberRegistration.fs:41-66` (`ctorParamsOfPat`). The `| _ ->` arm
reports `Kind.NotYetSupported` and returns **without adding a `ClassCtorParamInfo`**, while
`walk` carries on over the remaining tuple elements. For `new((a): int, b: string)` the
returned array is `[ b : string ]` — length 1 for a two-parameter ctor, with `b` now at index
0. Any consumer pairing `sc.Params` with call-site arguments positionally is mis-aligned, not
merely short.

Decide whether the arm should push a placeholder (keeping arity) or drop the whole ctor once
any param is rejected.

The comment that pointed here was itself half false — it claimed such shapes "reach the
walker, match no arm, and silently fail to advance the cursor". They do reach the walker and
it is not silent: the arm reports. The cursor half is this defect.

## A27. `EnumTests`' enum-equality test asserts nothing

```fsharp
Expect.all (errors tast) (fun d -> d.Message.Contains "mismatch") "…"
```

`Expect.all` over an empty sequence passes, and there are no errors to find: this file
analyses through `realProvider`, whose `(=)` is polymorphic
(`val inline (=): x: 'T -> y: 'T -> bool when 'T: equality`, `Vesper.Core/ops-platform.fsi:221`).
So `TyEnum E = TyEnum E` checks cleanly and the assertion is vacuous. Use
`Expect.isEmpty tast.Diagnostics`, which is what the test title claims.

The deleted comment made the same mistake in prose, citing a monomorphic `op_Equality` from
`MockBuiltins`, which by then existed in no project here and has since been removed from the tree
outright — every test resolves through the real `Vesper.*` contracts. **The stale assertion MESSAGE
still says "the mock's monomorphic-int `=` shape clash"**; it is a string literal, so the sweep left
it.

## A28. `ConstraintsTests` — assertion weaker than the test name

`"multiple constraints attach to the same typar"` asserted `Expect.isGreaterThan n 0`, which
one diagnostic satisfies, while the comment claimed both fire as separate diagnostics. Tighten
to `Expect.equal n 2` if both genuinely fire, or the name overstates.

## A29. `MkUnionTests`' header invariant was false in its own file

*"`mkUnion` is the ONLY sanctioned producer of `TyOr`"* — `rawOr`, five lines below, builds
`TyOr(UnionMembers.OfSeq xs)` directly. `TyOr` is a public DU case (`SemanticInfo.fs:112`);
only `UnionMembers`' constructor is private. The false sentence is gone; if the invariant is
wanted it needs enforcing — see **B34**.

## A30. The shadow `SemType` constructors and their active patterns do not round-trip

`TestHelpers.fs` — the shadow constructors mint `SymbolKeyOps.qualifiedTypeKeyOf name
args.Length`, whose `Name` is the bare source name. The active patterns project the same key
through `SymbolKeyOps.typeMetaName`, which renders the arity suffix. So
`TyUnion("Box", EqArray.singleton t)` builds a key named `Box`, but
`match … with | TyUnion(n, _) -> n` yields `` Box`1 ``.

A test matching a GENERIC nominal by bare source name **silently falls to `| _ ->`** rather
than failing on a name mismatch. Only arity-0 nominals round-trip; the suite is green because
every current match site is arity 0 or compares constructed values.

The deleted header claimed the patterns yield "the arity-stripped qualified name" and that
construction and match sites agree — false in both halves.

## A31. `realProvider`'s doc cited a package that does not exist

The deleted paragraph warned about "the FSharp.Core port `XParsec.FSharp.Lib`" canonicalising
`int`→`int32` and leaving `string` an unfreezable template. **`XParsec.FSharp.Lib` occurs
nowhere in the repo** except a few ephemeral plan docs — no such project, manifest or
directory. Deleted rather than reworded.

Resolve: either the trap is real under some other manifest set — in which case the front end
should reject a contract whose primitive canonicalisation disagrees with `BuiltinTypes`,
rather than relying on a test-helper comment — or the paragraph was stale and this closes.

## A32. Plan milestone labels living in test NAMES

Out of reach of a comment-only sweep, since a test name is a string literal:

- `PrintfTests.fs` — five tests named `"E1: …"`.
- `ReferencedProjectTests.fs` — `"short names no longer resolve … (O3)"` (also **A4**).

## A33. `SignatureResolutionTests`' later-file test lost its unresolved-name half

"a signature referencing a type declared in a LATER file does not resolve" asserts only that
`b.fsi` publishes its own type (`shapeOf r "+Thing"`); the rewrite against
`SignatureResolution.run` dropped the check that `a.fsi`'s val survives carrying `TyUnknown`
for the name it could not see. The within-file case ("a signature naming an out-of-scope type
is reported, and the val still publishes") does assert the carried name, so the RETENTION rule
itself is covered — what is not is that crossing a FILE boundary obeys it, which is the whole
point of that fixture. Restore it against `symbolOf r "needsB"` and `instantiateSymbol`.

---

# Part B — prose that should be a type

## B1. A diagnostic species should be a `DiagCode`, not a message substring

The `hasRangeValueError` doc exists only because the body (`d.Message.Contains "first-class
value"`) says nothing about what it detects. Fixed by **A1**; recorded separately because the
test-side helper change is what deletes the prose.

Deletes: *"A range materialises no seq value, so it is legal ONLY as the direct source of a
`for i in a..b do` counted loop; a first-class range value, or a stepped range (which has no
counted lowering), is rejected at elaboration."*

## B2. `FrozenPools.BoundVarNames : string[]` should be `BoundVarNaming[]`

A bound variable's name and anchor live in two parallel arrays, `BoundVarNames` and
`BoundVarToks`, with `""` and `Anchor.nowhere` meaning "minted". Their agreement — source
written means BOTH, minted means NEITHER — is representable-wrong, so a test asserts it with
a four-arm match whose two mixed arms exist only to `failtestf`. `BoundVarNaming`
(`Source of string | Minted of BoundVarId`) already exists in `TastPoolNodes.fs` and is
already what every consumer reads through. Storing the column in that shape, anchor carried
by `Source`, makes the mixed states unrepresentable.

Deletes: the `checkBoundVarNames` obligation comment, and with it the two `failtestf` arms.

## B3. The pooled side tables should be one carrier abstraction

`checkIdResolution` needs two checkers because a pooled side table either keeps its key
(`('id * 'v)[]`) or gives it up for a position in the bound variable pool
(`BoundVarColumn<'v>`). Which is which is a hand-written list of five call sites, and the
same 1:1-coverage obligation is spelled twice because the two carriers share nothing in the
type system.

Deletes: three comments in `TastPoolsTests.fs` — the dense-side-table re-keying block, the
per-bound-variable FILLED SLOTS block, and the address-space widening block.

## B4. The pooled carriers should be enumerable

`pooledCarrierCoverageTests` exists because the round-trip gate is vacuous for any tree
domain the program set happens not to contain. The only defence is `carrierCounts`, a
hand-maintained fold over exactly three carriers (member bodies, inline templates, `ValRepr`
tuple groups) into a 3-tuple. A fourth carrier added to `FrozenPools` would compile,
round-trip vacuously and never be counted. An accessor enumerating "every tree the pools
bear" makes the coverage test total by construction.

Deletes: the two coverage-rationale comments.

## B5. A `PassContext` should not be constructible un-run

`PassContext(provider, origin)` is a usable value before NameResolution / Unification have run,
and `totalMemberKey` silently depends on registries those passes fill.
A type that only exists post-pass removes the construction protocol from prose.

Deletes: the last sentence of the `extCtx` block in `CoverageTests.fs` — *"The trivial file
initialises what the picker reads."*

## B6. ~~`PairOutcome` cases do not carry their enforcement disposition~~ — WITHDRAWN

The accepted-by-content cases (`Unrepresentable`, `RuntimeServed`) are deleted; every
remaining case raises, so there is no disposition split left to encode. Both test blocks the
item wanted deleted are gone.

## B7. ~~`unrepresentableOf` / `runtimeServedOf` are the same projection twice~~ — WITHDRAWN

Both helpers are deleted: `Unrepresentable` went with the retire-sig-only D work, and
`RuntimeServed` with its C work (`[<Import>]` + `jsNative`).

## B8. One list-kind-keyed manifest resolver instead of five parallel ones

`resolveFiles` / `resolveImpl` / `resolveSigOnly` / `resolveImplOnly` all implement the same
inherit-then-append rule; `resolveRuntime` is the one that does not. The rule and its
exception are currently prose in the tests. Acceptance test: afterwards the exception is
visible in the list-kind type, not restated.

Deletes: the per-target-lists header (*"Every `[targets.<t>]` list INHERITS its `[core]` peer
and APPENDS to it."*) and the `resolveRuntime` comment.

## B9. ~~A capability identity that is one type across targets~~ — WITHDRAWN, already done

Traced and dropped. The proposed fix — "one identity type with an optional platform name" —
**already exists**: `RuntimeNames.CapabilityIdentity = { Key: TypeKey; CanonKey: TypeKey
voption }`, with a `Matches` member that accepts either spelling, and
`ExternalSymbols.resolveCapabilities` as the single reconciliation point that folds all three
provider shapes (CLR `IntrinsicInterface`, JS canon-only `Class` + compat shim, `Intrinsic`)
into it.

So the three test comments are not three shapes of one concept — the concept is already
unified downstream. They describe three legitimately different provider INPUTS, which is a
fact about the manifests, not a modelling defect. Nothing to do, and the guardrail question
never arises because no split is needed.

## B10. An `expectBindsTo` helper — "asserts the RESOLVED IDENTITY, not mere acceptance"

In six variants, this was the single most repeated claim in the scoping batch: three file
headers and five accessor docs. It is prose describing what an assertion helper should BE.
A helper taking a `pick: TastFile -> SymbolKey` and a `readUse: TastFile -> SemType` makes
"compare the use site against the DECLARATION, never a hand-spelled key" structural.

Deletes (all already cut to short form, all still present): the `FileOrderScopingTests` and
`ModuleScopingTests` header lines, the registered-key accessor doc, and the three
`TypeScopeOrderTests` "it BINDS to, not merely accepts" blocks.

## B11. Four `TypeScopeOrderTests` accessors are one traversal parameterised by site

`recordFieldType`, `classMemberParamType`, `classMemberReturnType` and `soleModuleLetType`
each carried a doc re-explaining that it reads the resolved type off the elaborated TAST —
the shared shape, four times. One accessor over a `Site` DU deletes all four docs.

## B12. `expectRejected` pins a verdict, not a message

Both `FileOrderScopingTests` and `ModuleScopingTests` document *"Only the VERDICT is pinned,
not the wording"*. That is the helper's contract and the name does not carry it — it reads as
if a specific rejection were checked. The files already have `expectError needle` for the
message-pinning case, so a name saying "any error" makes both docs unnecessary.

## B13. Splice-eligibility of a `TExpr` node is prose, not a predicate

The `DefaultOfInlineTests` header and the test's own match each enumerate by hand which
`TExpr` cases the inline splice arm accepts (`External`) and which silently miss it
(`StaticPropertyGet` / `StaticMethodCall` / `ExternalMember`). The arm is one
`| TExpr.External(…)` case in `Passes/InlineExpansion.fs:384`; nothing names the distinction,
so a fifth node kind is a silent miss again. A recognizer sited with the splice arm — the arm
matching it, the test asserting it — carries the fact instead of two hand-agreed lists.

Deletes: the whole `DefaultOfInlineTests.fs` module header.

## B14. A wire token index that does not carry its origin file

A published body keeps the PRODUCER's `TokenIndex.Regular i` and does not say which file they
index, so a consumer names that file and reads them there. An edit leaves every index still
in range, so the file's hash is the only thing separating a stale anchor from a live one —
and that hard failure is re-derived at read time (`InlineThaw.bodyAtOrigin`) rather than made
unconstructible. The index is the over-wide half of an (origin file, index) pair that only
ever travels together.

Deletes: the block above `producerSrc` in `InlineTests.fs`, reducing both fault tests to their
assertions.

## B15. `thawedTemplate` returns a bare `TypeStore * TDecl`

The doc has to say the thawed typars are roots of the RETURNED store and must be read back
against that one — a pairing the tuple does not express, and which two tests below depend on
getting right. A `ThawedTemplate` record, or a decl carrying its store, makes it structural.

## B16. `declaringTemplates` is a 4-wide tuple whose doc names its slots

`ExternalSignatureOracleTests` — `(string * int * FrozenType * SemType) list`. The doc exists
purely to say which position is which, and every destructuring site re-spells the order.
A record (`Name` / `DeclaringArity` / `Template` / `Expected`) removes it.

## B17. `memberOracle`'s six positional parameters

`memberOracle name isProperty declTyparArity methodTyparArity signature expected` — a bare
`bool` then two bare `int` axes. Call sites read `"binary method tupled" false 2 0`; nothing
there says which int is which axis. A record parameter also removes the internal
`if isProperty then Property else Method` branch. The surviving fact — `expected = None` means
the method axis freshened, so only arities and the `TyFun` shape are asserted — wants a
two-case DU (`Structural of SemType | ShapeOnly`) rather than prose.

## B18. `TypeClaims` should be claim-keyed, not name-keyed with a later guard

`TypeRegistry.TypeClaims : Dictionary<string, ResizeArray<TypeIdentity>>` lets any number of
claimants share one `(container, name, arity)`; uniqueness is restored afterwards by the
duplicate diagnostic and by ranking at lookup. A claim-keyed table
(`Dictionary<TypeClaim, TypeIdentity>`, short-name index derived from it) makes the
twenty-kind-pair matrix in `DuplicateTypeNameTests` a statement about a key rather than about
a guard that must be wired up per kind.

Deletes: *"One declaration claims one NAME at one ARITY in the MODULE that holds it, and a
claim may be held by at most one type of any kind."* See also **A16**, which is the same table
read from the other side.

## B19. `ValidationTests.hasMessage` is the last substring matcher — retire it with A1

Three of the four files in its batch assert on `d.Kind`; `ValidationTests` matches substrings
(`"immutable field 'X'"`, `"must come first"`, `"value restriction"`). Two files carried
paragraphs arguing why the `Kind` form is the sound one — prose that exists only because the
unsound form is still available next door. Give every diagnostic these tests use a
data-carrying `Kind` case and `hasMessage` goes with the argument. Same root as **A1**/**B1**.

## B20. A `GuardedEntity` DU for the `guarded ()` table

`CitationTests` needs two sited comments explaining why `BoundVarKey` and `Anchor` resolve
through `moduleType "…Module"` rather than `typeof<_>`, and `moduleSourceName` /
`companionModule` / `isFSharpModule` all exist to reconcile "an F# module is a type, but only
sometimes under its declared name" — one concept across three helpers and two inline
exceptions. `OfType of Type | OfModule of sourceName: string`, resolving the compiled name at
construction, makes the table read `OfModule "BoundVarKey"` with no comment.

Deletes: both `Module`-suffix exception comments and the "a module has no companion" block.

## B21. `RegionVerdict { Lifetime; Repr }` — "Axis 1" / "Axis 2" are prose names

`RegionsTests` carries two banner comments defining "Axis 1" (lifetime, `EscapeState`) and
"Axis 2" (representation, `RegionRepr`), plus eight test comments that only parse once you
have read them. The two tables are `ctx.Bindings.Escape` and `RegionVerdicts.Repr`, and the
ref-struct predicate `LocalStack ∧ StackOnlyEligible` is stated in prose in two files (here and
`SemanticScalars.fs:131`) and computed in neither.

Deletes: both axis banners and the `Axis-2` prefixes on the `reprOf` / `reprOfNested` docs.

## B24. `TExpr.External`'s key should not be a `voption`

`IntrinsicKeyStampTests` exists to assert a field is populated that the type permits to be
empty. If minting an `External` node required a `SymbolKey`, the header sentence (*"a
`ValueNone` key here is a silent mis-splice / phantom `call` downstream, not a graceful
miss"*), the `Some ValueNone` arm of `assertStamped` with its 3-line `failtestf`, and the
negative half of the suite all disappear — the remaining tests become "the right key" rather
than "a key at all".

Acceptance test: afterwards nothing in the file needs a sentence about what a missing key
would mean.

## B25. `SizedProgram` should carry `Measured`, not just `Ceiling`

The deleted 50-line header was in substance a table of measured compressed bytes per program
per encoding generation — data about the three records in `programs`, kept as prose beside
them, which is why it grew to 50 lines and why every figure in it is now unverifiable. Add
`Measured: int` beside `Ceiling: int` and assert a tolerance, putting the number where it is
checked on every run. The historical generations are not worth keeping in any form. Fixes
**A21**.

Acceptance test: the surviving clause *"the ceilings are measured figures with headroom"* can
then go too.

## B26. Stringly-typed DU coverage assertions

`FrozenTypeTests` (`tag: SemType -> string` + a hand-written 14-element list) and
`FrozenTypeTableTests` (`rowTag: TypeRow -> string` + a 15-element set) each spell every DU
case a second time, as strings, in a list the compiler never checks against the DU. In
`FrozenTypeTests` that is exactly what let **A20** through: the list is one case short and
nothing notices. A structural coverage helper makes "the samples reach every case" a property
instead of two hand-synced lists.

## B27. ~~The stamp tables should be the only channel a consumer has~~ — WITHDRAWN into A12

Traced. The premise holds almost everywhere already, so the type-level fix is disproportionate
to what is left. Enumerated across `Passes/Unification/`:

- `ctx.Resolver.TryLookup*` — **zero** call sites.
- `ctx.Provider.TryLookupType` / `TryLookupMember` — many, but all **by KEY**, which is what
  the stamp design intends, not a by-name fallback.
- Keys minted from a name (`EngineCore.fs:401`, `InferApp.fs:258`, `InferRecordAccess.fs:588`)
  — all fixed, fully-qualified canonical names, not written source spellings. `InferApp.fs`
  says so at the site.
- `OpenScope.tryQualify` (`InferResolve.fs:194`) — a scope-visibility PREDICATE
  (`|> ValueOption.isSome`), not a resolution.

That leaves exactly **one** opens-aware by-name escape: `Translate.fs:511` →
`NameResolutionTypeRefStamp.tryResolveExternalTypeKey`, reachable only from the measure-carrier
site at `Translate.fs:226`. That is **A12**, already recorded with its two call sites. Close
A12 and the prohibition holds by enumeration; no visibility refactor is needed.

## B28. First-hit-wins external classification

If external name classification were a single `name -> hit voption` whose shape is inspected
only after the hit is fixed, the rule would be unstateable-otherwise and the surviving 3 lines
in `ExternalTypeKeyStampTests` go too. **Check first whether `OpenScope.tryQualify` already
has that shape** — if it does, this item is already closed and only the comment carried doubt.

## B29. Union constraint reduction: equality reduces member-wise, comparison never does

Written twice — a seven-line essay above `checkConstraintKind` and again above the
`comparison on (int | string) is Violated` test. It is a property of the constraint kind, not
of either test, and nothing in `SemanticConstraintKind` distinguishes "reduces over `TyOr`
members" from "is refused for any multi-member `TyOr`". Encode it on the kind, or in the
reduction's signature.

## B30. Arity-keyed type lookup — three tests each restate "not the bare short name"

`UnificationInheritanceTests` has three surviving blocks all saying the same thing: with
`` Box`1 ``/`` Box`2 ``, `` IBox`1 ``/`` IBox`2 `` and `` Base`1 ``/`` Base`2 `` the bare
short name is withdrawn, so the walk must key on the arity-bearing `SymbolKey`. The prose
exists because a bare-short-name lookup is still callable. If the member and interface walks
took only a `SymbolKey`, all three notes go.

## B31. Hand-counted source offsets are the largest remaining comment mass here

~15 surviving lines across `UnificationBasicsTests`, `UnificationGenericsTests` and
`UnificationClassesTests` exist only to justify an integer — e.g.
`// pat p at 55: 29 + 22 char decl lines + "let "` in front of `NodeKey.ofSource 55`.
`UnificationTestHelpers.keyOfLet` already derives the same key from the binding's name. Moving
those call sites onto it deletes every `// pat … at N` comment in the three files and makes
the tests survive an edit to their own source strings.

## B32. `Kind.NotYetSupported of string` is a string key

`SecondaryCtorParamTypesTests.isCtorArgShapeError` recognises the rejection with
`feature.Contains "constructor argument pattern"` — a substring probe against an English
sentence assembled at the report site, so re-wording the message silently passes the test.
Give `NotYetSupported` a named reason case and the test matches the reason directly.

The doc claimed the check was *"Asked of the VERDICT, not of a message substring"* — the
design the code only half has: the `Kind` is matched, but the feature string is still probed
with `Contains`. Fourth site of the theme in **A1** / **B19**.

## B33. "Recovered" vs "clean" parse is a naming convention, not a type

`TestHelpers.parseFile` and `parseRecoveredFile` return the SAME pair,
`Lexed * ImplementationFile<_>`. Which one a test may call is decided by prose plus two
`failwith`s. A distinct type, or a `Recovered | Clean` tag on the tree, makes calling the
wrong helper a compile error and the choice visible at the use site.

## B34. A pre-resolution `TyOr` differs from a canonical `mkUnion` result

`MkUnionTests.rawOr` exists solely because `mkUnion` collapses a singleton while
`UnionMembers.OfSeq` does not, so there is no other way to hand `zonk` a two-member union
still holding an unresolved `TyVar`. The distinction — canonical/ground/collapsed vs
raw/may-hold-a-`TyVar`/uncollapsed — is prose in a test helper plus an unenforced convention
in `src`, since anyone can write `TyOr` directly. Making the payload constructible only
through a resolution-state-carrying wrapper (or `TyOr` private with `MkUnion` the sole
factory) deletes both the `rawOr` doc and the `zonk`-section block, and makes **A29**'s
invariant true rather than merely asserted.

## B35. Printf specifier classification is a table spread across three section headers

`PrintfTests` enumerates, in three header essays, which specifiers are residuals with no
native lowering (`%0*d`, `%0*.Nf`, `%0*A`), which lower natively (`%+08.2f`, `% 08.2f`), and
which flags are inert (`%-*A`, `%+*A`). Each restates a fact `PrintfHoleForm.tryClassify`
already decides. A total classification result type — one case per outcome, declined cases
carrying the reason — turns each into a one-line arm doc at the classifier.

## B36. "The single surfaced enum decl" is asserted by four helpers, enforced by none

`EnumTests` has `enumShape`, `enumCases`, `underlying` and `singleLet`, each re-matching
`tast.Decls` for a shape it expects and `failwithf`-ing otherwise, with `singleLet` needing a
comment to say the enum `TDecl.Type` is skipped. One named accessor removes the repeated
partial matches and the doc.

## B37. The shadow constructors' `open`-order requirement is prose only

Whether `TyUnion` means the shadow or the real `SemType` case depends on the order of two
`open`s in the consuming file, with no diagnostic either way. Distinct names, or a nested
module opened by name at the use site, makes the dependence local and visible.

Deletes: *"Live only in a file that `open`s this module AFTER `open …SemanticAnalysis`."*
Related to **A30** — both are the shadow helpers being under-typed.

## B38. `MapProviderTypesTests`' coverage claim has no enforcement

The fixture claims `mapProviderTypes` threads EVERY position a provider puts a type in, but
nothing fails when the mapper grows one the fixture plants no marker at — the header sentence
is the only record of the intended coverage. An enumeration of those positions that the mapper
and the test both read fails on addition. Same shape as **B4** and **B22**.

Sharpened by the `FrozenInterfaces` retyping: the `IntrinsicInterface` arm was missing from the
mapper for exactly this reason, and it was a compiler error rather than the fixture that caught
it.
