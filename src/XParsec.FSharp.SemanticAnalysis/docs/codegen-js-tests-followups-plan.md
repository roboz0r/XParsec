# Codegen.Js.Tests follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent unless they pin a specific declaration — they rot.

Raised by the comment-hygiene sweep of `test/XParsec.FSharp.Codegen.Js.Tests`
(77 files excluding `fixtures/`, baseline 9898 code / 2353 comment, 4.2:1, 343 blocks of 3+
lines, longest 43). `fixtures/widget/` is off-limits to the sweep: those files are F# source
fed to the compiler under test, so editing a comment there changes a test input. `goldens/`
is emitted JS held for byte comparison, likewise untouchable.

Reading every comment against the code it claims to describe turns up work the sweep itself
cannot do, because it changes code:

- **Part A — defects and duplication.** Wrong assertions, tests that pass on the wrong thing,
  helpers open-coded until the comment explaining them is load-bearing.
- **Part B — prose that should be a type.** A comment that was genuinely load-bearing and
  long, where the durable fix makes the sentence unnecessary. Each names the comment it
  would delete — that naming is the acceptance test.

**The sweep is complete.** All 77 files have been swept, in two rounds of three agents, with
every batch audited against the code it claims to describe. Comments 2353 → 1361, so
4.2:1 → 7.1:1. No comment block anywhere exceeds three lines, against a starting maximum of
43. Code is unchanged at 9898 lines but for one line: a longer test name in
`PrintfSpecifierTests.fs` made Fantomas wrap `test` onto its own line.

Test **name** strings were in scope by the user's direction; nothing else in a test body was.
25 names were rewritten, each to what its body actually asserts. Assertion-message strings
were therefore out of scope, and several carry the same defects the sweep removed from
comments — those are recorded below rather than fixed.

Items marked **[verified]** were re-checked against the source by the orchestrator, not
taken on the reporting agent's word. Three agent claims were wrong and are corrected in
place; see A5 and A18.

---

# Part A — code defects and duplication

## A1. `RefsTableTests.fs`'s header cited a type shape that does not exist **[verified]**

The deleted header claimed B's provider mints `FTClass(TypeKey(Some "A", "", "Box`1"))`.
`TypeKey` (`SymbolKeys.fs:101`) is `{ Container; Name; TyparArity }` — no 3-argument form,
and **no home field at all**. The same header predicted that member access fails with a
`"package not referenced"` diagnostic when A is absent; no such string exists in analysis
source, and the file's own third test asserts the diagnostic is `Unknown class type 'Box`1'`,
naming the type rather than the package.

Both claims are gone. Recorded because the fabricated 3-argument form is evidence the header
was written from memory rather than from the code.

**Type candidate:** the prose kept merging "the identity" with "where the type lives". If
`TypeKey` structurally cannot carry a home and the home only ever rides
`ExternalTypeShape`, that is already correct-by-construction and no action is needed beyond
the deletion — worth confirming.

## A2. A lex failure swallowed into `Resolver = ValueNone` **[LANDED]**

`jsWalkCtx` was deleted by interim changes, but the same shape survived in
`emitWithResolverFor`, and the whole class of defect is now unexpressible: `Lexing.lexString`
returns a `Lexed` rather than a `Result`.

Lexing was total but for one input — a backslash ending a non-verbatim interpolated string
(`$"\`), where `pSkipInterpolatedFragmentChars` stopped without consuming and then failed the
whole file. It now consumes the backslash and the string closes as
`UnterminatedInterpolatedString`, matching the plain-string sibling `"\`. Every other `fail`
in the lexer sits behind a precondition its dispatcher establishes, so `lex`'s `Error` arm is
an `invalidOp`.

The 22 call sites lost their failure arms, `Kind.LexFailure` and
`CorpusParseResult`/`CorpusReport`'s `LexError` are gone, and `ParserTests.testSlicedParsing`
no longer skips boundaries whose slice would not lex.

## A3. The `(&&)` anchor test named the wrong producer file, and asserts no file at all **[verified]**

`SpecializationTableTests.fs` claimed `(&&)`'s `if`/`then`/`else` were written in
`src/Vesper.Core/ops-platform.clr.fs` — a CLR file cited in the JS suite. `let inline (&&)`
is at `src/Vesper.Core/ops-std.fs:5`. Comment corrected.

The test itself never asserts the producer file **name**, only that unmarked indices resolve
against `entry.Origin`. Nothing would have caught the entry being anchored in the wrong
producer file. The sibling `get_Item` test already asserts `"array-cycle.js.fs"`; do the
same here with `entry.Origin.Path.Relative`.

## A4. "ONE token in the consuming file" was false

The fixture is `let a = 1 + 2\n`, which lexes to considerably more than one token. The
assertion resting on it is `List.max indices > lexed.Tokens.Length`, which needs only a
*short* file. Reworded to the fact the assertion actually uses.

## A5. Stale cross-references — corrected against the reporting agent **[verified]**

Three `Module.func` cross-refs were deleted (the skill mandates deletion regardless, since
they rot on rename). The reporting agent claimed all three named identifiers that no longer
exist. Only one does:

- `Pipeline.analyseSemWithContextForCore` — genuinely absent. ✓
- `TastLower.lower` — `TastLower` **exists**, at
  `src/XParsec.FSharp.SemanticAnalysis/TastLower.fs:9`. Only the `lower` function is absent.
- `Elaborate.run` — **exists**, `Elaborate.fs:270`, alongside `Elaborate.elaborate` at
  `:261`. Not stale, and the follow-on claim that `docs/passes.md` is stale for citing it is
  therefore also wrong.

Neither false premise reached the replacement prose: both rewrites state their fact
conditionally without asserting anything about those modules. No action beyond this
correction.

## A6. Names that a rename reached in code but not in prose **[verified]**

- **`globalLibHomes`** — gone from `src/`, split into `TsGlobalHomes.mountFor` and
  `TsGlobalHomes.isGlobalHome`, which is exactly the mount-vs-import distinction
  `JsNamespaceTests` exists to pin. Six comment sites named it; all rewritten. **One survives
  out of scope:** `JsNamespaceTests.fs`, in an `Expect` failure message
  (`"…mint under the Js namespace (globalLibHomes), not bare Widget"`).
- **`ErasedDistinction`** — gone from `src/`. Two `OptionalParamTests.fs` blocks narrated the
  provider as "formerly THROWING" it. Deleted.

## A7. Assertion-message strings carrying the defects the sweep removed from comments

Test bodies, so out of the sweep's scope, but the same failure modes:

- `EnumTests.fs` — `"…(object map is the single source of truth)"`. Self-congratulation in a
  failure message; says nothing about what failed.
- `TsManifestEnumTests.fs` — `"…survive the remap (no longer dropped)"`. History; dates a
  submission, and a reader of the failure cannot use it.
- `SourceMapTests.fs` — `"0a output unchanged"`. A plan-doc milestone label no reader of the
  repo can resolve. The test name carrying the same label was fixed; the message is code.
- Em-dash causal hedges in `sprintf`/message strings: `ArrayIndexMemberTests.fs` (×3),
  `ImportFormLoweringTests.fs` (×2), `EnumTests.fs`.
- `ExceptionTests.fs` — `"the caught throw lets the program exit zero"` is the one exit-code
  assertion in the file that does not interpolate `out`, so a failure there prints less than
  its siblings.

## A8. The array-intrinsic test passes on a spelling mismatch **[LANDED]**

The two spellings were unified, so the lookup this test relied on missing now hits. The
positive invariant it was asked to state — the array publishes an intrinsic shape carrying
JS's repr — is asserted in `ArrayIndexMemberTests.fs`; the `PrimitiveExprTests.fs` test keeps
only the target-verdict check, an `[| … |]` literal still failing on JS for a separate reason.

## A9. A target divergence carried only as a comment, now deleted **[verified]**

A negative `%*` runtime width throws a JS-native `RangeError`, an accepted divergence from
the CLR `ArgumentOutOfRangeException`. True (`EmitJsFormat.fs:289-312`), and no test in the
suite exercises it. The comment was a backward clone of the site that implements it, so it
was deleted. If the divergence is meant to be pinned, the honest form is a test.

## A10. No test pins that a mutable local emits a reassignable `let`

`ArrayLoopTests.fs`. The test name claimed "emits a `let` binding" and a deleted comment
claimed "never a `const`", but the body asserts only `while (` and `(acc = `. The behaviour
(`EmitJs.fs:659`) is unchecked. The name has been narrowed to what it asserts; the missing
assertion is the real fix.

Same shape, smaller: the `` `arr.[i] <- v` / `arr.[i]` / `arr.Length` `` test asserts only
`.length` and `[0]`, never distinguishing the indexed read from the write.

## A11. `ForInTests.fs`'s header was disproved by its own last test **[verified]**

Deleted claim: "duck-typed `Pattern` sources **and bare arrays/lists** are rejected". The
`Pattern` half is true — `EmitJs.fs:724` fails with "duck-typed `for...in` (Pattern
enumerator) is unsupported on JS". The lists half is false: `tryForInEnumerator`'s `TyUnion`
arm admits a cons-list and this very file runs `for x in [1;2;3]` under Node.

**Arrays are untested either way** — there is no `for x in (a: int[])` case anywhere in the
JS suite. Worth one test, whichever verdict it turns out to be.

## A12. An unverified front-end gap claim, deleted, with nothing pinning it

`ForInTests.fs` carried: "referencing `this` or a case PAYLOAD inside a union interface-impl
body hits a separate front-end bound-variable-scoping gap." Deleted as an untraced failure
claim with no named owner. Counter-evidence: unions and records share
`translateNominalMember`, which sets `ThisKey` for both, and the record fixture in the same
file *does* read `this.Stop` and passes. Either the gap is real and wants a failing test
naming it, or the fixture's `this`-free nullary shape is an unnecessary restriction.

## A13. `MittE2ETests.fs` and `UnannotatedMittTests.fs` shipped contradicting each other **[verified]**

`MittE2ETests.fs`'s header asserted a "residual precision gap": "`undefined` is not yet a
registered intrinsic type DISTINCT from `unit` … a `unit`-typed event is wrongly ACCEPTED by
the no-payload `emit` overload." False on both counts — `RuntimeNames.undefinedKey`
(`:284`) and `Intrinsics.Undefined` (`:77`) register it, and `UnannotatedMittTests.fs`
asserts the opposite verdict in a passing test.

Deleted; no code fix needed. Recorded because a header that survives the change it describes
is the failure mode, and this pair is the worked example: the two files could be read in
either order and one of them was always lying.

## A14. `AnchorFileLocalityTests.checkAnchors` documented a check it does not perform **[verified]**

The doc claimed an out-of-file anchor "lands on a token whose text belongs to nothing this
file's tree could have been anchored on". No text check exists: `check` asserts in-range and
not-`Token.EOF`, nothing more. Doc rewritten to what the code does. If the text check was
intended, it is missing.

## A15. `LibraryModeTests.fs`'s `coreTypes` doc was false on two counts **[verified]**

It claimed `core-types.fs` "needs `Fun`" and that the contract "declares the three types the
file itself defines". `core-types.fs` defines **one** type (`Ref<'T>`) and mentions no `Fun`;
`core-types.fsi` declares `Ref<'T>` plus the `'T ref` alias. It also contradicted the file's
own test, which asserts nothing there reaches `Fun`.

## A16. `ClassEmitTests.fs`'s header contradicted its own assertion **[verified]**

The header said a `[<CustomEquality>]` class "emits an ATTACHED `Equals(` instance method",
while `Expect.isFalse (src.Contains "Equals(other)")` in the same file asserts that form is
gone, re-keyed to `[Symbol.for("vesper.equality")]`. Header and test name both fixed. I
checked the rest of the suite: that `isFalse` is the only site mentioning the named-method
form, so nothing else still asserts the retired shape.

## A17. An untraced production-defect claim, deleted

`JsNamespaceTests.fs` asserted in prose that "the REAL es2015 pack has a ctor-merge collision
that would throw on load", offered as the reason nothing in that file runs Node. Nothing in
the file tests it and nothing else in the repo states it. If true it is a production defect
wanting a failing test; if stale it is already gone. Not traced during the sweep.

## A18. `docs/dynamic-typing-design.md` citations — corrected against the reporting agent **[verified]**

`DynamicTypeTests.fs` cited this document twice. The reporting agent deleted both, stating
the file "does not exist anywhere in the tree (nor under `src/**/docs/`)". **It does exist**,
at `src/XParsec.FSharp.SemanticAnalysis/docs/dynamic-typing-design.md`.

The deletions still stand on the self-containment rule — a test comment that can only be
understood by opening another document has already failed — but the stated reason was wrong,
and no one should later "restore" them on the belief that the target was missing.

## A19. Two front-end gaps recorded only as a test workaround

`ExternalAttachMembersTests.fs` binds every `unit` result to a named variable
(`let u = b.set(5)`) because a bare `b.set(5)` mid-sequence produces `Expr.Missing`, and a
top-level `let _ =` is not an emit-supported declaration. Two parser/emit gaps living as a
fixture idiom. A failing test named for each would be the honest form.

## A20. Duplication worth a shared helper

Ordered roughly by how much prose each would delete.

1. **The regenerable-asset trio, five ways.** `ListTests`, `SeqTests`, `ArrayTests`,
   `OptionTests` and `StructuralPrinterTests` each carry a byte-identical `generated` /
   `lf` / `"the committed <Pkg>.mjs matches the generated source"` triple. `lf` is defined
   five times with an identical body, four of them carrying the identical doc line about
   CRLF checkouts — and `OptionTests`' copy has no doc at all, which is the tell. Wants
   `generatedAsset` + `regenerableAssetTest` in `TestHelpers`.
   **Deletes:** four cloned `lf` docs and four of the five `generated` docs.
2. **The `gated` conformance filter, five ways.** Byte-identical 8-line body in
   `FrozenCodecTreeRoundTripTests`, `FrozenCodecRoundTripTests`, `FrozenCacheHitMissTests`,
   `ConformanceByteIdentityTests` and `ConformanceRoundTripByteIdentityTests`. Each copy had
   grown its own paragraph explaining the same exclusion; the sweep cut all five to one line.
   **Acceptance test:** after a shared `gatedFor "js"`, no file needs a sentence saying why
   `Diagnose` programs are excluded.
3. **The one-package fixture stack, three ways.** `boxManifest`/`boxRuntime`/`boxContract`/
   `emitBox`/`resultHarness` recur near-identically in `ExternalAttachMembersTests`,
   `ExternalHeritageTests` (as `chain*`) and `LiteralUnionTests` (as `widget*`) — five
   bindings × three files, differing only in package name and runtime body.
4. **The `Enum`/`Counter` enumerator prelude, four ways.** `ForInTests` restates the whole
   `Enum` type three times (`seqClassSrc`, `seqUnionSrc`, `seqRecordSrc`) and
   `ManualEnumerationTests` once more. The host type is the only per-site variation, which is
   exactly what each test is varying.
5. **The empty-stub-source emit, four ways.** `emitWith <contract> (Map.ofList [ pkg, {
   FileName = …; Source = "" } ]) false input` recurs in `IndexSignatureTests`,
   `TsManifestEnumTests`, `ImportFormLoweringTests` and `OptionalParamTests`, each with a doc
   explaining "the synthetic package has no `.toml` asset". Four copies of one idea.
6. **`SchemaDsl.fs` bypassed by two files.** `UndefinedIdentityTests` and `TypeLevelFoldTests`
   each re-declare their own `named` / `param` / `methodMem` / `cond`. Lift `cond`, `param`,
   `methodMem` and the `typar` helpers into `SchemaDsl` and delete both copies.
7. **The `sourceMappingURL` strip, four ways.** `emitFrozenJs`, `emitJs`, `emitJsLibrary` and
   `emitLibrarySource` in `TestHelpers` each end with the same `IndexOf`/`Substring` tail. A
   named `stripSourceMappingUrl` removes the duplication and the clause repeated in all four
   docs.
8. **Per-file `analyse`/`analyseErrors` wrappers.** `UndefinedIdentityTests` and
   `TypeLevelFoldTests` (parse → `analyseSemForSelfHost` → `Diagnostic.errors`), and
   `NumberFamilyTests`/`TypeArgNumberTests` (`analyseWith` → messages). Both shapes belong in
   `TestHelpers`, parameterised by provider.
9. **`facesOf` declared twice, byte-identical**, in `OpsPlatformJsTests`. Hoist to one
   module-level helper — and rename: `face` is a retired term in this codebase. This local
   reads out an intrinsic's canon identity and platform repr, so `intrinsicIdOf` or `reprOf`
   says what it is.
10. **The two `withInlineBodies` by-key stores** in `ExternMemberInlineTests` build the same
    `Dictionary<SymbolKey, InlineBody>` and the same closure. One `byKeyProvider` covers both.
11. **`FreeFnOverloadTests` / `MemberOverloadTests`** repeat a manifest literal, `contractTs`,
    and an `emitWithX` injecting a one-entry stub `JsRuntimeModule` map. A shared
    `emitWithStubRuntime` deletes the duplicated comment explaining why the stub is needed
    (`JsImports.moduleOf` throws "has no JS runtime module" without it).
12. **Fixture sources retyped rather than bound.** The suite's pattern is "emit-shape test +
    Node test over the same source", and the source is written twice: `UseTests` (first two
    tests), `ArrayLoopTests` (string-index pair, `let _ = effect` pair), `TryFinallyTests`
    (first two), `ClassEmitTests` (the 9-line `Tagged` class, and `Box`). `ClassEmitTests`
    already shows the fix one declaration away, in the shared `offsetSrc`.
13. **`recursiveProducer` / `recursiveMemberProducer`** in `SpecializationTableTests` open with
    an identical inlined `write` helper. `tmpDir` already lives in `TestHelpers`; a
    `writeInto : string -> (string * string) list -> unit` belongs beside it.
14. **`UseTests` and `TryFinallyTests` both own "the body's result survives the finally"** —
    same assertion, same `"<cleanup>\n42"` shape. `use` desugars through the same lowering, so
    one of the two is testing the desugaring rather than its own construct.
15. **`skiptest` message spelled two ways.** `runNodeFiles` sites say "node is not installed";
    `runJs` sites say "node not found on PATH". `TestHelpers.expectNodeOutput` already exists
    and the runtime-module suites predate it.

## A21. External-object fixtures declare their member list twice

`BusE2ETests` (`busManifest`/`busRuntime`) and `MittE2ETests`
(`recorderManifest`/`recorderRuntime`) each declare a member list once as `Schema.Member`
rows and once as a JS object literal. A mismatch is caught only at Node run time, as a
message about a missing property. One description generating both sides is the fix; this is
Part B in fixture form.

---

# Part B — prose that should be a type

## B1. An `Anchor` carries no evidence of which file it indexes

`AnchorFileLocalityTests.fs` opened with a 17-line header — the worst block in its batch —
and every line existed to say that an index into one file's tokens is indistinguishable from
an index into another's, so a leaked anchor "is still a number in range" and resolves
silently to the wrong token. The whole test file is a runtime substitute for that missing
distinction. A3 is the same gap seen from the other end.

If an anchor carried its file identity, or were indexed by a per-file token-stream handle
rather than a bare `int`, the mis-attribution would not be expressible.

**Acceptance test:** neither "a number still in range" nor "the specialization table's
entries are a different index space" needs writing anywhere.

## B2. Local member overloading is unrepresentable, and no test says so

`ExternMemberInlineTests.fs`. Two `(# … #)`-bodied same-name overloads cannot be declared in
one file: the local member key is arity-only and the two collide. The test compensates by
hand-building both lifted signatures and minting keys itself, so nothing in the suite fails
while the limitation stands. Fix is a local member key carrying the argSig axis the finalized
key already carries — at which point the hand-built pin becomes a real declaration.

**Acceptance test:** the sentence "two same-name overloads are not declarable in one file" no
longer needs writing.

## B3. `abstractedParams` is the arity, and nothing says so

`SpecializationTableTests.fs`. No field stores an entry's arity: it is the length of the
leading lambda chain, and the edge's `args.Length` must agree. That agreement is asserted at
one test over every edge in the run, but it is a property of `TSpecialization` /
`TExpr.InlineCall`, not of one fixture. A `TSpecialization` carrying its own arity, or an
`InlineCall` whose args are indexed by parameter id rather than position, makes the assertion
unwritable.

**Acceptance test:** "an edge carries exactly the arguments the entry it names abstracts"
becomes a type error rather than an `Expect.equal`.

## B4. `Schema.Export` is five positional arguments, so the fixtures need prose

`Interface("B", 0, members, extends, indexSigs)` — five positional arguments, two usually
`[]`. That shape is why `ExternalHeritageTests` and `ExternalAttachMembersTests` each open by
restating in prose the hierarchy the calls below already encode.

The larger instance is `IndexSignatureTests`' `ixlib`, whose header carried a 12-line table
pairing each export with its TS shape (`Dict` ⇒ `{ [k: string]: number }`, and so on) because
the DSL below spells the same shapes in a form no reader can scan. A `SchemaDsl` builder
taking the TS source spelling — which `structuralIx` already half-takes as its
`structuralKey` string — would let the fixture read as its own documentation.

**Acceptance test:** the `ixlib` doc block disappears entirely rather than shortening.

## B5. `withSpecialization` grafts payloads by index-search over the pools

`FrozenCodecTreeRoundTripTests.fs` finds a graft site with
`List.find (fun i -> ChildColumn.count … = 0 && …)`, i.e. it depends on the freeze of a
specific source string producing a node of a specific child-arity. Three comments exist only
to justify why that search succeeds. A constructor minting a `FrozenPools` with a chosen
payload at a chosen slot would delete all three and make the fixture independent of what the
front end happens to emit for `let z = not true`.

## B6. `arrowParamBindings` is a string-scraping JS parser

`FunctionEmissionTests.fs`. Splits the arrow's parameter text on `,`, `[`, `]` and space,
then two tests assert `List.distinct names = names`. Its doc carries three sentences
(whole-vector-in-one-arrow, duplicate parameter, `SyntaxError`) precisely because nothing in
the return type says which JS construct produced the list. If the emitter exposed the
parameter vector of a top-level arrow structurally, the doc would be one line. Low priority —
the scraper is confined to one file.

## B7. `jsManifests` has a load-bearing order held only by a comment

```fsharp
// BCL exceptions as Vesper contracts (inherit exn → Error); must precede Vesper.Option.
srcManifest "Vesper.Exceptions"
srcManifest "Vesper.Option"
```

Kept as one line sited on the element, but a comment is the only thing enforcing it. Wants an
ordered-by-construction manifest set, or a provider build that sorts by dependency rather
than trusting list position.

## B8. The `dynamic` access table belongs on the capability, not in a test

`DynamicTypeTests.fs`' header carried an 11-row table of `dynamic`'s access forms (`d?foo`,
`d?a?b`, `d.foo`, `d?foo <- v`, `dynamic x`, …) with each row's outcome. Every row is a test
in the same file, so the table was deleted rather than shortened. Noting it because the table
is really the `dynamic` capability surface, and `ops-dynamic.js.fsi` is where it is already
spelled as signatures.

---

# Smaller notes

- `StructuralFormatTests.fs` pins the emitted import alias
  `$Vesper_StructuralPrinter_structuralFormat` against module `Vesper.Printf` (source file
  `structural-printer.js.fs`). Not a defect — the alias derives from the declaring module,
  not the package — but it is why the assertion message has to spell out "(Printf owns `%A`)"
  to stop the pin reading as a typo.
- `MethodAxisSingleCandidateTests.fs` annotates `echoProvider : IExternalSymbolProvider`
  redundantly with `contractTs`'s return type.
- `ExternMemberInlineTests.fs` used a box-drawing rule (`─── … ───`) as a section divider; no
  other file in the suite does. Replaced with an ordinary comment. If section dividers are
  wanted, they should be a convention rather than one file's habit.
