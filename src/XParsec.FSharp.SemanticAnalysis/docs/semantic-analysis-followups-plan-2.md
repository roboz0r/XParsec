# SemanticAnalysis comment-sweep follow-ups, part 2

Continues `semantic-analysis-followups-plan.md`, which reached ~1900 lines. Same rules; that
file stays as written, and nothing here supersedes it. New entries go at the end of THIS file.

Non-comment issues found while reading every comment in the project as an unverified claim.
Recorded, not acted on: fixing them mid-sweep would break the code-preservation gate that
makes parallel comment editing safe.

Each entry names the comment its fix would delete. That naming is the acceptance test — if
the change lands and the sentence still needs writing, the change was the wrong shape.

Every claim below was verified against the source by the orchestrator, not taken from a
subagent report.

## Scope at the point this file opened

The 3-line comment-block ceiling holds across all 130 files of the project. What remains is
deletion-only work on files that never had an over-long block but carry prose the name and
signature already say. Entries here come from that phase.

### `Tast.fs:163` — three interface-typed fields make `TastFileG`'s derived `=` unsound **[LANDED — as `EqDict`/`EqSet`, not `Map`/`Set`]**

The four collection fields (`ModuleSourcePaths` as well as the three below) are `EqDict`/`EqSet`,
which keep O(1) lookup and carry structural equality, so `=` decides a rebuilt file.
`structurallyEqual` and `dictEqual` are deleted, the mutable fill stays inside the passes, and
`Elaborate.fs:349-357` converts once at the seam. `ElaborateTests`' "TastFile equality" pins it.

`IntrinsicReprKeys` and `Accessibility` are `IReadOnlyDictionary<SymbolKey, _>` and
`GlobalValueKeys` is `IReadOnlySet<SymbolKey>`, so the compiler-derived `=` on `TastFileG`
compares those three by reference and reports two identical files unequal. The whole of
`structurallyEqual` and its private `dictEqual` helper exist to work around that, and every
consumer has to remember to call them instead of `=`. Storing them as `Map<SymbolKey, _>` and
`Set<SymbolKey>` would make `a = b` correct by construction and delete both functions along
with the doc block on `structurallyEqual` that explains the unsoundness. I did not check
whether any producer needs the interface type — `Elaborate` hands over a `Dictionary` it has
just finished filling, so a conversion there looks cheap, but the freeze/pool round-trip has
more sites and was not audited.

### `Tast.fs:136` — `TSpecializationG.Decl` is typed wider than the invariant it must satisfy **[LANDED]**

The entry stores `Pat`/`Value` directly; `TSpecializationG.binding`, its two `failwithf`s and
the threaded `SpecializationId` are deleted, and the pooled/codec seam encodes the two ids in
place of a decl row. `isInline`/`ty` from the old `TDecl.Let` had no reader and were dropped.

### `TastDecl.fs:55` — `TTypeDeclG.Namespace` duplicates `TypeKey.Container`, with ONE load-bearing reader **[LANDED]**

The hedge was warranted — `Codegen.Clr\LayoutNodes.fs:258` fed the field to the emitted
`TypeDef` namespace column — but the value equals `SymbolKeyOps.typeNs td.TypeKey`, so the
reader switched spellings (gated on a green Clr suite) and the field, its writer chain
(including `DeclContainment.namespaceOpt`, whose only caller it was), the `TastConvert` copy
and the codec sites are deleted. Shape-golden output is byte-identical.

### `Unification/EngineCore.fs:75` — the occurs check can miss when `target` is not a union-find root **[LANDED]**

`occursAndAdjust` now takes a `Rep`, whose private case makes `UnionFind.find` the only
producer, so a non-root argument is a compile error. `InferGeneralize.fs` passes the root it
already binds, and the defensive re-`find` inside the walk is deleted.

### `Unification/EngineCore.fs:281` — `capabilityCanonKey` and `capabilityPlatformKey` are the same five-probe chain twice **[LANDED]**

An earlier extraction (`capabilityKeyBy`, commit `7a6da811`) had already collapsed the two
copies to one; the remaining hand-written five-field chain moved to
`CapabilityIds.TryMatch`, beside the field declarations, so a sixth capability field puts
the probe next to the declaration it must cover.

### `Unification/Engine.fs:320` — `unifyArgCoerce` and `tryCoerceUpcast` maintain the same no-pin prefix in parallel **[LANDED]**

An earlier extraction (`absorption`, commit `51e08e32`) had already collapsed the first three
steps; the fourth, `tryStructuralWiden`, was still called from both walkers. All four now live
in `absorbsWithoutPinning : PassContext -> SemType -> SemType -> Absorption`, whose fourth arm
answers `Accepts` on a widened record, so a new absorbing shape is a one-site edit.

The pre-extraction `Refuses` arms differed — `unifyArgCoerce` tried `tryStructuralWiden` after
one, `tryCoerceUpcast` returned `false` — and the two agree because widening requires a
`TyClass` expected, which the `TyOr` and platform-repr arms have already excluded. That
disjointness is now stated on `absorbsWithoutPinning` as the invariant a fifth arm must keep.

### `SemanticScalars.fs:74` — `SafeContext`, `NativeRegionTier` and both `EscapeState` converters have no PRODUCTION consumer **[LANDED — kept, marked `UNBUILT`]**

`EscapeState`, `RegionRepr` and `RegionId` are live (`Passes/Regions.fs`, `PassContext.fs`), but
the two target-flavoured projections off `EscapeState` and the two enums they land in are called
from no `src/` code: the CLR backend never asks for a ref-safe tier and there is no native
backend. They are a design sketch for a lowering that does not exist, and the prose this sweep
cut off them was making forward claims ("the vocabulary the CLR emission target and its verifier
enforce") about a consumer that was never written.

They are NOT unreferenced, though: `test/XParsec.FSharp.SemanticAnalysis.Tests/RegionsTests.fs:608-655`
exercises both converters against every `EscapeState` case, so the mapping is pinned and deleting
the declarations would take those tests with them. The decision is therefore whether the native
tier is still intended, not whether the code is reachable — if it is intended, say so where a
reader can tell it is unbuilt; if not, the tests go too.

Resolution (user): the native tier is intended. Region analysis is optimisation metadata for any
backend to read when it picks a lowering — advisory everywhere, and obligatory for a GC-free
native target, which cannot emit at all without placement. Both projections and their tests stay.
The marker is the word `UNBUILT`, greppable, in the shape `PrintfSpec.fs:222`'s `PROVISIONAL:`
already set: stated once in full above `SafeContext` in `SemanticScalars.fs`, referenced from
each converter's doc, and named in the `RegionsTests` section comment so a reader editing those
three tests knows they are the only consumer. `Passes/Regions.fs`'s header now states the
advisory-versus-obligatory split at the pass itself.

Still open, and untouched here: `EscapeState.ReturnOnly` has no producer (plan-1's entry at
`SemanticScalars.fs:73-127`). `solve` never mints it, so the lub cannot yield it, and both
projections' `ReturnOnly` arms are reachable from the tests alone.

### `SymbolKeyOps.fs:17` — `isEscapedName` also returns true for an already-arity-suffixed compiled name **[LANDED]**

`isEscapedName` is deleted. The escape is stripped at the token → name read
(`Lexed.GetTokenName`), so no name reaching a key carries one, and the arity-0 rule the
predicate stood in for is now stated directly as `isStructuralConstructorName`.

### `SemanticScalars.fs:14` — `Rational`'s `Equals` and `CompareTo` disagree on non-canonical values **[LANDED]**

The reduction moved out of `Rational.create` and into the private `new(n, d)`, the single site
that writes the fields. A non-canonical `Rational` is now unconstructible from inside the type
as well as outside it, so field-wise `Equals` and cross-multiplying `CompareTo` agree on every
value. `create` is a call to that constructor, and `ofInt` / `Zero` / `One` / `(~-)` reduce
along with everything else.

### `TastAccessor.fs:763` — `declExpressionTy` has no consumer anywhere in the repo **[LANDED]**

Deleted. `declExpression` and the `(|DExpression|_|)` pattern stay.

### `TastAccessor.fs:651` — `patBoundVarNaming` is public but reached only through its own active pattern **[LANDED]**

The body moved into `(|PNamedNaming|_|)` and the function is deleted.

### `TastPoolBuilder.fs:234` — `exprCount` and `boundVarCount` have no production consumer

Both are called only from tests (`test/XParsec.FSharp.SemanticAnalysis.Tests/TastPoolBuilderTests.fs:119,183,187,225,386`
and `test/XParsec.FSharp.Codegen.Js.Tests/InlineExpandTests.fs:264`), where they assert that a
rewrite appended nothing or minted exactly one boundVar. That is a legitimate reason for them to
exist, so this is not a deletion candidate — but it does mean the id-space sizes are an assertion
surface rather than something the emit path needs, and the comment above them was written as
though a walker consumed them.

### `TastPools.fs:364` — `FrozenPools` is built in two phases with no type-level marker for the derived field

`fill` constructs the whole record with `BindingValReprs = [||]`, then immediately rebuilds it as
`{ pools with BindingValReprs = bindingValReprs pools }`, because that one table is derived from
the columns rather than remapped from the source file. Nothing in `FrozenPools` distinguishes a
derived field from a carried one, so the placeholder is only prevented from escaping by the two
expressions sitting next to each other, and a reader of the type cannot tell that an empty
`BindingValReprs` is ever a valid intermediate state. Splitting the derived tables into their own
record (or computing them behind a single constructor that cannot be bypassed) would delete the
`// Derived below, off the pools themselves.` comment that currently carries the invariant.

### `PrintfSpec.fs:132` — `argTypes` returns a `voption` that is never `ValueNone`, and three consumers branch on it **[LANDED]**

`argTypes`, `appliedTypeOf` and `printerFromSlots` are total. A `FormatArgTy` classifier
(`argTyOf`) states the letter table once and makes the `%a`/`%t` `failwith` unrepresentable;
`appliedTypeOf` returns an `AppliedTypes` record. Six dead `ValueNone` branches went, across
`PrintfSpec`, `InferApp`, `InferLiteralExpr` and `InferLiterals`.

### `Passes/Unification/Translate.fs:164` — both arms of the `TyparScopeStrict` branch mint the same TyVar four ways **[LANDED]**

The strict-scope `ctx.Report` is a guarded prefix and the shared tail runs once, through
`freshTyVar` plus the `TyparScope` memoise. The two rationale comments merged into one.

### `Diagnostics.fs:347` — `CyclicType(Inheritance)` is unpublished while `CyclicType(Immediate)` maps to FS0954 **[LANDED — both carry FS0954, `Immediate` renamed `StructField`, and `Abbreviation` added carrying FS0953]**

`Kind.code` files only the `Immediate` case under `DiagCode.FSharp 954`, whose fsc resource name
is `tcTypeDefinitionIsCyclicThroughInheritance`, and drops `TypeCycle.Inheritance` into the
`DiagCode.Unpublished` group. `Kind.message` meanwhile renders `Inheritance` as "has a cyclic
inheritance hierarchy" — the sentence 954 exists for. The split may well be deliberate (the two
verdicts come from different checks: fsc's 954 is about a struct field or immediate containment,
while the inheritance WALK is this compiler's own), and a comment saying so was removed here as a
duplicate of the `TypeCycle` type doc. Worth confirming against fsc which of the two numbers each
verdict should carry, since a consumer suppressing FS0954 today gets only half the family.

Resolution: the split was NOT deliberate — `dotnet fsi` files every shape under one number.
Probed four programs, each reporting `error FS0954: This type definition involves an immediate
cyclic reference through a struct field or inheritance relation` on the type-name token:
`type A() = inherit A()`; the mutual `and` form; `[<Struct>] type A = { x: A }`; and the mutual
struct pair. The resource name is misleading — 954 covers the union of both relations, and
"immediate" qualifies both, so it never distinguished them.

`Kind.code` now answers `DiagCode.FSharp 954` for `Kind.CyclicType _`, and both rows in
`DiagnosticCodeTests` pin it. With the code no longer differing, the only axis the DU carries is
WHICH relation, and `Immediate` did not name one — renamed `TypeCycle.StructField`, matching its
producer `checkGroupStructFieldCycles`. The codec tags are positional, so the wire format is
unchanged. `Kind.message` keeps the two distinct sentences: naming the relation beats fsc's
either/or wording, and the end-to-end tests in `TypeScopeOrderTests` assert on those substrings.

Found while probing, and folded in on the user's call: a cycle through an ABBREVIATION is fsc's
FS0953 (`tcTypeDefinitionIsCyclic`, "…through an abbreviation" — verified on
`type A = B and B = A`). `Passes/Unification/Translate.fs`'s `forceFill` detected it but reported
free text through `Kind.Message`, so it published no code. It now reports
`TypeCycle.Abbreviation`, the third case, carrying 953; the codec takes tag `2uy`.
`TypeScopeOrderTests`'s abbreviation test asserts the classified sentence in place of the old
`"is cyclic"` needle.

The three cases now span fsc's two numbers, so `Kind.code` reads the DU again — 953 for
`Abbreviation`, 954 for the other two. `Kind.message` keeps one sentence per relation, and the
`StructField` text dropped fsc's "or inheritance relation" disjunction, which the case had
already decided.

### `Passes/NameResolution/Scope.fs:374` — `resolveQualifiedExternal` computes eight suppression predicates eagerly, then ORs them

The qualified-name arm builds `isQualifiedCtor`, `isQualifiedStatic`, `isLocalQualifiedType`,
`isEnumCase`, `isExternalQualifiedCase`, `qualIsExternalType`, `isExternalStaticMember` and a
`ResolvedType.ContainsKey` probe as separate `let` bindings — each hitting the type registry or
the external classifier — and then ORs all eight in one `if` whose only effect is to skip a
diagnostic. Every predicate is evaluated even when the first already suppresses, and the closure
is ~170 lines with the stamping side effects interleaved between the predicate bindings, so the
order of the `let`s is load-bearing in a way the `||` chain does not show. A named
`SuppressionReason` (returned by a function per candidate, tried in order) would make both the
short-circuit and the stamping/verdict split explicit; several of the comments trimmed here were
labels standing in for that type.

### `CstWalk.fs:497` and `CstWalk.fs:746` — object-expression member signatures are walked by two near-identical helpers **[LANDED]**

`CstTypeWalk.iterMemberDefnSigTypes` (plus `iterBindingReturnType`) is the one walker, with an
exhaustive match and no catch-all; both former local sets are deleted. The entry's file
reference was wrong — `iterTypeDefnTypes` lives in `CstTypeWalk.fs` — and the expression walk
now visits a `val` member's type (unreachable on legal F#; its consumers only stamp).

### `Passes/InlineExpansion.fs:472` — the root expressions are collected by re-running the decl mapper for its side effect **[LANDED]**

The first `List.map` collects each walked root as it is produced (`walkTop`), so the second
`mapDeclExprs` pass and its discarded rebuild are deleted. Same objects, same order.

### `Freeze.fs:44` — a residual typar degrades to `FTUnknown "?unresolved-typar"` with no diagnostic **[LANDED — the diagnostic already existed; `freezeTy`'s tolerance claim was the error]**

The `| _ -> FTUnknown "?unresolved-typar"` arm carried a comment asserting the case was "already
an error-severity diagnostic on this decl". It is not, in general: the only pass that reports an
unresolved root is `checkValueRestriction` in `Passes/Validation.fs`, and it is guarded by
`rb.IsMutable && kv.Key = rb.BindingSite`, so a non-mutable binding whose root no scheme
quantified freezes to the sentinel silently. The same file contradicts the deleted comment two
declarations above, where `freezeTy`'s doc says a residual unlinked `TyVar` is TOLERATED and
normal (`let f () = let g = fun x -> x in (g, g)`). Both cannot be right, and which is right
decides whether the arm owes a diagnostic or the sentinel is a legitimate value. I did not trace
what consumes `FTUnknown` here; note that `ExternalSymbols.fs:802` uses an `FTUnknown` sentinel
deliberately (`unfreezable`), so an `FTUnknown` reaching a consumer is not per se a bug.

Resolution: DIAGNOSTIC, and it was already there. The sweep found `checkValueRestriction` and
stopped; the reporter is `ResolvedTypes.run`, a whole-tree guard `Pipeline` runs at `:43` on
`tast1` — the same tree `Freeze.run` then freezes. It convicts any free root outside the
enclosing schemes with `Kind.Internal(InternalBreak.UnresolvedTyVars n)`, error severity.
`ResolvedTypes`'s `allowed` set is scoped per binding while `Freeze.schemeBoundVars` pools every
scheme in the file, so the guard's catch is the wider one and the sentinel arm sits strictly
inside it.

So the deleted comment was accurate and `freezeTy`'s doc was the error — including its example.
Probed by freezing sixteen programs and counting `TypeRow.Unknown UnresolvedTypar` rows against
the diagnostics: `let f () = let g = fun x -> x in (g, g)`, the cited "tolerated residual",
yields zero rows and zero diagnostics, because `g` generalises and its root reaches
`FTLocalTypar`. Of the sixteen only `let mutable m = []` produced a row, and it carried both the
value-restriction message and the internal break. Generic record, union, class and abbreviation
declarations all froze clean, so `ResolvedTypes`'s `TDecl.Type` skip left no observed hole.

`freezeTy`'s doc now states the contract: an unquantified root freezes to the sentinel, which
`ResolvedTypes` has already reported, making it a recovery value that keeps `freeze` total past
that error. Two tests in `ResolvedTypesTests` pin the pairing in both directions — the sentinel
arrives with the diagnostic, and the doc's own example freezes without one. The sentinel stays:
`freeze` is total by design (`UnknownReason`'s doc says so), so a `Result` here would be a
different decision, not this one.

### `ConformancePass.fs:316` — the conformance gate has no production consumer

`enforce`'s doc claimed "a non-empty result must fail the build". Grepping the whole tree with no
path or include filter, `checkManifest` and `enforce` are called only from
`test/XParsec.FSharp.SemanticAnalysis.Tests/ConformanceTests.fs:296` and
`test/XParsec.FSharp.Codegen.Clr.Tests/TestHelpers.fs:358`; nothing under `src/` calls either. The
same holds for `ConformanceTypars.checkFile` and `checkMembers` (tests only —
`ConformanceTests.fs:848`, `:936`, `ConformanceTyparsTests.fs:49`, `:102`); of that module only
`toDeclaringAxis` has a production caller, in `FrozenSignature.fs`. So the `.fsi`/`.fs`
conformance verdicts are enforced by the test suite, not by the compiler driver. That may well be
intentional for a prototype, but the build-failure wording was not describing any code, and if the
gate is meant to be real the driver is where it is missing.

Resolution: the driver gate exists and is a DIFFERENT rule set; `ConformancePass` gains no wiring.
The finding above conflated two routes (see `conformance-tast-level-plan.md`, and the correction
it already files against this section). `AssemblyAnalysis` conforms every pair over its two
ANALYSED halves as `analyseUnits` walks the manifest, `AnalysedAssembly.gate` refuses emission on
any error-severity diagnostic, and `ConformanceVerdict` carries error severity, so a conformance
finding already fails a build. What is tests-only is `ConformancePass.checkManifest`/`enforce`,
the cheap pre-analysis CST route over parse results; `TestHelpers.buildPackage` runs it to
fail fast before an analysis the in-assembly route would fail anyway. Stages 2–3 of
`conformance-tast-level-plan.md` retire it, and wiring a second rule set into the driver in the
meantime would install exactly the two-derivations shape those stages exist to remove.

Deleting it is the agreed direction, and it is not a straight cut: the route's `[<Import>]` asset
check (`ImportUnknownAsset`, `ImportMissingExport`) reads the manifest rather than the CST and has
no second implementation, and it is what keeps the CST rule set standing, because `checkUnit`
supplies its bindings. `conformance-tast-level-plan.md` Stage 2a lifts `[<Import>]` to a
target-neutral concept first, splitting the two dialect obligations onto an `IRuntimeModules` the
backend implements.

The timing the gate owes — a pair is checked before it is projected for the next file — was
positional, held by the order of statements in `analyseUnits` and by nothing else. It is now
structural: `conformSignature` homes the signature in its implementation AND takes the verdict,
returning both as one `ConformedSignature`, and it is the only site that homes a signature for an
assembly being compiled. A pair therefore cannot reach a later file's scope unchecked.

Note the literal ordering cannot be "check, then project": `ConformanceTypars.checkFile` reads the
homed provider, so the projection is an INPUT to the check. What precedes the check is the push
onto the visibility stack, which is what a later file resolves through.

### `FrozenTypeBridge.fs:42` — `methodVar` memo sharing is a caller obligation nothing enforces

`instantiateWith` takes `declaring` / `methodVar` / `localTypar` as bare functions, and the
surviving doc line records that two templates of one signature must share a `methodVar` memo to
agree on `j`. `methodFreshener` exists to supply that memo but takes the `Dictionary` from its
caller, so nothing stops two instantiations of one signature being run against two caches and
disagreeing on the index. An instantiator value that owns its cache — built once per signature and
passed as a unit — would carry the invariant in the type and delete the doc line.

### `FrozenSignature.fs:36` — the backend clause was prose only; the code is backend-neutral

Removed a comment on `originIn` justifying the origin stamp by what "a per-file backend needs".
The code below it stamps `SymbolHome.InFile declaredIn.Path` and nothing more, and `SymbolHome.InFile`
is documented at its declaration (`SymbolKeys.fs:24`) as `InAssembly` refined to a file, with the
assembly read off the path. No code change is owed here — recording it only so a later reader does
not go looking for the backend coupling the deleted sentence implied.

### `SemTypeWalks.fs:405` — `DesugaredForm.ListLiteral` / `ArrayLiteral` are write-only tags

Both cases carry docs describing a lowering ("a nested `UnionCons` cons/nil chain", "wrapped in
an `Array.ofList` external call"). The lowering is real — `ElaborateExpr.translateListLikeLiteral`
builds exactly that, and the array path goes through `RuntimeNames.arrayOfListName` — but it is
reached by matching the CST directly (`Expr.EnclosedBlock(lParen = ParenKind.List _)` at
`ElaborateExpr.fs:222-225`, `EmptyBlock` at `:249-250`), never by reading the tag. Grepping the
whole tree, `DesugaredForm.ListLiteral` and `DesugaredForm.ArrayLiteral` are written at
`Passes/Desugar.fs:48-49` and have NO production reader: every reader of `ctx.Desugared`
(`Elaborate/Apply.fs:234`, `:268`, `NameResolution/Scope.fs:307`, `Unification/InferApp.fs:448`,
`:538`) matches only `OpName` and `ConsExpr`.

They are NOT unreferenced: `test/XParsec.FSharp.SemanticAnalysis.Tests/DesugarTests.fs:55`, `:64`
and `:73` assert that both tags are recorded for `[1; 2]`, `[|1; 2|]` and the empty-list form, so
deleting the cases takes three tests with them. So two of the four `DesugaredForm` cases are dead
as tags in production but pinned by tests. Either the literal
lowering should route through the tag like `ConsExpr` does, or the cases should go and `Desugar`
stop setting them. I left the docs in place because they are true of the NODE; they are just not
true of anything that reads the case.

### `SemTypeWalks.fs:371` — `TypeScheme.Constraints` was documented far narrower than its producer

The doc ended "Only a `let f<'a when 'a : C>` populates this list." Its one producer,
`Passes/Unification/InferGeneralize.fs:276-282`, builds the list from `store.Constraints.Items`
for EVERY quantified typar, whatever stamped them — including the `Coercion` bounds that the loop
immediately above uses to pull in dependent typars, and constraints minted off external contracts
(`ExternalSymbols.fs:833`, `:867`). A source `when` clause is one way in, not the only one. I
deleted the sentence rather than restate it; the rest of the block is correct.

### `SemTypeWalks.fs:203` — `iterChildren2`'s bare-typar pairing in an `FTOr` is order-dependent

The deleted second half of this comment admitted that two or more bare `FTTypar` members make the
leftover pairing arbitrary, because type-constructor keys cannot tell one bare typar from another. That is a
real limitation of the code as written — the wildcards are consumed in index order — and the arm
above it raises a `failwithf` on the ambiguous case for NON-wildcards while this one silently
picks. The honest form is a test that pins the intended behaviour for `FTOr` with two open
members; I did not write one, and I did not change the code.

### `Anchor.fs:29` — `ofToken`'s doc contradicted its own `failwith`

The removed block said "A node with no anchor takes `nowhere` instead: a VIRTUAL token carries no
lexed index…". `ofToken` does not fall back to `nowhere` on a virtual token — it raises
`failwithf "…the VIRTUAL token %A names no place in the source…"`. A caller that has no token is
expected to write `Anchor.nowhere` itself, which the `nowhere` doc two declarations above already
says. Nothing to fix in the code; recording it as an instance of a doc describing the fallback the
author expected rather than the one below it.

### `TastLower.fs:75` — a CLR-encoder spelling had leaked into the platform-neutral module

The comment on the `FTConst` arm of `matchInstantiationPartial` named the array intrinsic
`` `[]<!!i>` ``. `[]` is right (`RuntimeNames.arrayName` returns `"[]"` for rank 1), but `!!i` is
the CLR metadata typar notation used throughout `Codegen.Clr` and it appears nowhere in this
domain — the type is `FTConst(RuntimeNames.arrayKey 1, [elem])`, and this module's own header
forbids it knowing a backend. Rewritten to `'T[]`. Worth a wider grep of `!!` and `!i` in
`SemanticAnalysis/` comments: this is the shape that survives review because the notation is real
somewhere.

### `TastUnpool.fs:335` — `ofPools` has no production consumer (verified, comment already correct)

Recording the trace so it is not re-derived: grepping the whole tree with no path or include
filter, `TastUnpool.ofPools` is called from `test/` only —
`Codegen.Clr.Tests/InlineFreezeThawTests.fs:126`, `:309`, `Codegen.Clr.Tests/TestHelpers.fs:565`,
`Codegen.Js.Tests/FrozenCodecRoundTripTests.fs:132`, `FrozenCodecTreeRoundTripTests.fs:42`,
`:187`, `SemanticAnalysis.Tests/FrozenSignatureTests.fs:19`, `InlineTests.fs:598`, `:657`,
`TastPoolsTests.fs:262`, `:380`. Nothing under `src/` calls it, and `rebuildFile`'s only caller is
`ofPools` itself. The surviving doc line says exactly that, so no edit was owed; the point of the
note is that the sibling entry in part 1 (`:840`) proposing to collapse `rebuildFile`'s `'id`
parameter is still accurate against the current file.

### `Codegen.Clr.Tests/SelfHostTests.fs:241`, `:309` — plan-doc milestone labels in test section headers **[LANDED]**

Already removed by commits `2cd5797a` and `bc0988c1`; a sweep of `test/**/*.fs` finds no
milestone label remaining.

### `RuntimeNames.fs:3` — the module runs two axes and the header claimed only one

The deleted module header read: "The canonical `*Key` identity of each well-known runtime type.
Identity is the key, never a string." Roughly a third of the module is the opposite — a
platform-repr STRING axis: `objAbbrevName`, `systemObjectQualifiedName`, `textWriterTypeName`,
`stringBuilderTypeName`, `stringWriterTypeName`, `arrayName`, `byrefName`,
`arrayOfListName`, `nullTypeName`, `undefinedTypeName`, plus the `Set<string>` table
`numericTypeNames` and the by-name recognisers `isVesperListName`,
`isStructuralConstructorName`. Nothing in the types tells a caller which axis it is on, so
`vesperListKey` / `isVesperListName` is a pair kept in step only by naming discipline. (The
`matchesKey` / `matchesName` pair was the same shape; `matchesName` has since been deleted
along with its last caller.) The `(|PlatformName|_|)` pattern already gestures at the type
that would fix this — a single-case wrapper for the platform-repr string, so `opaqueKey` takes one
and the name sets are keyed by it. Recording, not fixing; the type change would delete the pair of
comments now sited on `primitiveKey` and `opaqueKey`.

### `ExternalSymbolProviders.fs:11`, `:47` — `NamedChannels` and `KeyIndexedChannels` share six channels by copy

The two channel records differ only in how types are addressed, but each independently declares
`TryLookup`, `TryLookupUnionCase`, `TryRecordsWithField`, `AmbientOpenPrefixes`,
`IntrinsicReverseCanon` and `IntrinsicForwardRepr`, and `KeyedChannels.ofKeyIndexes` copies all six
across field-by-field into a `{ NamedChannels.empty with … }`. Adding a seventh non-type channel means
editing three places, and a field forgotten in the copy silently becomes a permanent miss rather
than a compile error. The candidate is one record of the shared channels held by both (or a
type-addressing DU on a single channel record). The prose this would delete is the pair of block
docs on the two types plus the two `empty` docs — one of which was a byte-near clone of the
other and was removed in this sweep.

### `VesperLib/Manifest.fs:38` — the deleted `.fs`-branch rationale did not match the tree **[RESOLVED — the manifest list selects; no code change]**

`parseFileFull` carried: "The `.fs` branch is not a fallback: a package's per-target primitive
companions (`prim-types-int.clr.fs`) and operator bodies (`ops-platform.clr.fs`) ship no `.fsi`."
Both named files exist, but so do `src/Vesper.Core/prim-types-int.fsi` and
`src/Vesper.Core/ops-platform.fsi` — the package ships a target-neutral signature alongside the
per-target implementation, so "ship no `.fsi`" is true only of the `.clr.` spelling specifically,
which is not what the sentence says. I deleted it rather than repair it (rejected-alternative
shape: "not a fallback" argues against a design the code does not have). The open question, which
I did not chase: what actually selects an implementation file for the extractor — the manifest's
own file list, or the absence of a same-stem `.fsi`? If it is the former, the branch has nothing
to do with `.fsi` presence at all.

Resolution: the former, and the question was already stale when it was written — `VesperLib/`
and `parseFileFull` were gone by the vesperlib split, and the successor path is
`ReferencedProject` + `ParsedManifest`. A manifest's `[core] files` list selects every source
file; `.fsi` presence on disk is never a decision input, and no directory scan discovers sources
(`Directory.GetFiles` under `src/` appears only in the CLR backend's reference-assembly
loading). `classifyFiles` (`ReferencedProject.fs:189`) classifies each entry by extension alone
via `SourceFileKind.tryOfPath`, and `pairUp` (`:208`) pairs a `.fsi` with the implementation
sharing its `pairingKey` — stem minus extension minus a trailing `.<target>` segment, so
`prim-types-int.fsi` pairs with `prim-types-int.clr.fs` — required to be the very next entry.
A listed `.fs` alone is a unit with `Signature = ValueNone` (`:223`); a listed `.fsi` whose
companion is absent or non-adjacent is a parse error (`:230`). The deleted sentence was a comment
defect only: the extension classifier it sat on read the path, never the file system.

### `XParsec.FSharp/Token.fs:2558` — `GetName`'s example contradicts the line under it **[LANDED — example corrected to "OpColonEquals"]**

Outside the swept file set, found while verifying `OperatorNames.fs:38`. The comment reads
"For operator keywords, the name is just the token name (e.g., "op_ColonEquals" for `:=`)", and
the next line is `this.Token.ToString()`, which yields `"OpColonEquals"` — the *token case* name,
not the `op_`-prefixed compiled name. The sentence is right and its example is wrong, and the
example is the half a reader will trust. This matters beyond cosmetics: it is exactly the fact
`OperatorNames.ofToken` exists to work around (the `op_Dynamic` / `op_DynamicAssignment` spellings
have to be listed explicitly there because `GetName` cannot produce them), so a reader who
believes the example will not understand why those two arms are needed.

---

## From the H19 punctuation pass

A later pass read every em-dash in the project and named the connective each one stood in for.
That is a verification pass rather than a rewording one, so it turned up claims that were
false, and two of them are code issues rather than comment issues.

### `Elaborate/TypeDecls.fs`, `typeDefnAccessToken` — three cases silently lose their access token **[LANDED]**

Resolution: no widening occurs today because `tryTypeDecl`'s own catch-all drops `Delegate`,
`Struct`, `TypeExtension` and `AbstractType` entirely — silently, with no diagnostic. Both
catch-alls are now explicit thirteen-case matches; `Delegate`/`AbstractType` read their token,
`TypeExtension` reports absence (fsc ignores an access modifier on an extension). Two `ptest`s
in `ElaborateTests.fs` pin the delegate/struct elaboration gap. Follow-up worth an entry of its
own: the dropped forms deserve an "unsupported declaration form" diagnostic.

Original entry:

The function matches eight `TypeDefn` cases for `typeName = tn` and falls through
`| _ -> ValueNone`. `TypeDefn` has thirteen cases. Of the five that fall through, only
`Missing` and `SkipsTokens` genuinely carry no `TypeName`: **`Delegate`, `TypeExtension` and
`AbstractType` all carry one**, and a `TypeName` is where the `access` token lives.
`Conformance.fs` proves the point by destructuring `typeName = tn` on exactly those three.

So either those three never reach a `TDecl.Type` — in which case the catch-all is hiding a
dead arm — or `type private X = delegate of int -> int` elaborates as `Public`. The second is
a silent accessibility widening, which nothing downstream can detect, because by then the
token is simply absent.

The comment that covered this claimed the fall-through cases were "the variants with no
`TypeName` — a bare delegate / exception form". Wrong twice: the delegate form does carry one,
and `TypeDefn` has no exception form at all. Deleted rather than repaired.

**Wants a test either way**: `type private X = delegate of …` asserting the elaborated
accessibility, which pins down which of the two readings is true.

### `Passes/Unification/Translate.fs`, `translateType` — a forked branch that does not fork **[LANDED]**

Same finding as the `Translate.fs:164` entry above; landed with it.

### `Passes/InlineSpecTable.fs` — two different arities share the name "arity"

`Grounding.Arity` is the count of parameters the CALL SITE applied (`List.length
peeled.Params`, and part of the interning key); the entry's own lambda count is the count of
SURVIVORS after fusion. A comment asserted the two were the same thing, which is how the
collision stayed invisible.

Renaming the field `AppliedArity` makes the distinction correct-by-construction and deletes
the sentence now stating the relation. Same family as the `FlatParams` entry in
`codegen-clr-followups-plan.md` B1: two bare `int`s that mean different things and nothing
tying either to its meaning.

### Duplicated prose whose fix is choosing an owner

One fact stated twice in two places. Rewording either is the wrong fix; the work is deciding
which site owns it and deleting the other.

- The dropped-unresolved-impl policy ("the diagnostic already fired"), implemented AND
  documented in both `Elaborate/TypeDecls.elaborateClassInterfaces` and
  `Elaborate/Members.elaborateHostMembers`, over the same `impl.Resolved` / `ValueNone -> ()`.
- `tryInterfaceMethods` and `tryClassType` open with the same `tryClassByKey` /
  `DeclaredTypeKey` lookup under the same three-line justification.
- `InferOverload.fs` — "`M<'T>('T,'T)` opens to the same index at every position", on the
  `TrialBindings` type doc and again on `matchTypes`' method-typar arm.
- The monomorphic-siblings ⇒ no-polymorphic-recursion rule — `InferGeneralize.instantiateBinding`
  and `Infer.fs`'s `let rec` scheme-drop.
- `InlineSpecTable.fs` — `miscountedFusedEntries`'s `roots` and `SpecTable.finish`'s
  `declExprs` carry the same sentence about edge counting.
- `Engine.fs` — "keyed by platform repr", inline in both `numericFamilyOr` and `reprSiblings`.
- `PrintfHoleForm.fs` — `FixedRightZeroPad`'s doc and the `leftAlign && zeroPad && isFloatLike`
  arm state the same rule with the same `%-05.2f ⇒ "3.140"` example.

### A dedicated H17 pass is owed — the object-negation shape did not stay retired

The H19 sweep read every comment in the project and found the retired *negate-the-object*
shape (`names no type`, `claims nothing`, `carries no key`, `sees none`, `binds nothing`) at
roughly **forty sites** across `TypeRegistration`, `MemberRegistration`, `TypeRefStamp`,
`NameResolution`, `Scope`, `TastLower`, `FrozenTypeBridge`, `FrozenCodecPrimitives`,
`FrozenCodec`, `Freeze`, `TastPoolTypes`, `CstKeys`, `Diagnostics`, `RuntimeNames`,
`VesperLib`, `VesperLib/TypeTranslate`, `VesperLib/TyparCapture`, `Translate`, `Subsume`,
`InferApp`, `InferControlFlow`, `InferResolve`, `InferGeneralize`.

That density in files an earlier vocabulary pass already touched means it either missed this
project or the shape regrew. Fixing forty sites ad hoc at the tail of a punctuation sweep is
the wrong shape of work: it wants one pass driven by the H17 grep signature, which
`.claude/skills/comment-hygiene/taxonomy.md` records at 63/65 precision (the sole false
positive being `names` as a plural NOUN).

The rule when doing it: negate the VERB, then check WHICH is true — nothing was looked up
(**has no**), the lookup missed (**does not resolve to**), or it hit the wrong kind (**is
neither … nor …**). Picking correctly is where the false comments surface.

### Other comment-only residue, `SemanticAnalysis`

- `SymbolKeyOps.memberArity` restates its own one-line body (H3).
- `ReferencedProject.targetKeys` is now "Same rule, same reason." — a bare back-reference,
  not self-contained. Pre-existing, not created by the sweep.
- `CstWalk.isStructShape` — the referent of "which" is ambiguous (the attribute, or the
  attributed type?).
- `ElaborateExpr.translateNew` — "purely defensive for error paths" is an unverified
  reachability claim of the "fails loudly" family; the fallback is a `nameOf` walk returning
  `""`.
- `Elaborate/ObjArgs.memberParamTys` — "the call still emits, just unwrapped" names no locus;
  the consumer is in another file.
- `InferRecordAccess.resolveFieldStep` — "a fact of a shape" needs provider vocabulary from
  another file (H16, not self-contained).
- `Engine.DotSource` — two of five cases are glossed in a block doc above the DU while the
  other two carry per-case docs; the rows are legal H19 form but belong on the cases.

### Comment blocks still over the 3-line ceiling

Pre-existing, none created by the sweep. `TypeRegistry.fs` is the worst: `IntrinsicReprKeys`,
`IntrinsicKeys`, `IntrinsicAbbrevHost`, `intrinsicKeyOf`, `tryIntrinsicAbbrevHostByCanon` (4
each) and `tryNonClassMemberHostByKey` (5). Also `TastUnpool.ofPools` (5), `RuntimeNames`'s
module doc (6), `SemTypeWalks.iterChildren2` (4), `InlineExpansion`'s `PendingCall.Tok` site
(4), `EngineCore.funSlotArityOfArgs` (4), `InferResolve.tryWrittenClassCtorAsFunction` (4),
and two in `TypeRegistration` / one in `MemberRegistration`.

**Triage note worth keeping.** Several of these are not one long comment but TWO unrelated
comments abutting, which a separator resolves without cutting a word — confirmed for
`PassContext`'s `IntrinsicReverseCanon`, `TypeRegistration`'s self-type-key block,
`MemberRegistration`'s heritable-local `inherit` arm, and `RuntimeNames`'s module doc (three
claims). For a `///` doc the separator must be a bare `///` line, since F# requires the block
to be contiguous. The block-length metric cannot tell adjacency from continuation, so check
before cutting.

### Two refinements the sweep produced for the H19 rule itself

Belong in `.claude/skills/comment-hygiene/taxonomy.md`, recorded here so they are not lost:

1. **A colon convention predicts a high cut rate only where the colon is FREE.**
   `PrintfHoleForm.fs` is colon-dominant and went to zero. `PrintfSpec.fs` is legitimately
   dash-tabled because its gloss already contains a colon (`` `AppendZeroPaddedUnsigned(v,
   width)` — F# `%05u`: unsigned decimal ``); converting would collide. A two-level table
   needs both separators.
2. **Do not invent causation to satisfy the rule.** Where two facts are merely coordinate,
   `and` is the correct connective and a manufactured `because` is a new false claim. Observed
   working correctly at `EmitExpr.TryWith`, `TypeRegistration.localContainerChain` and
   `Elaborate/Printf.fs`.

---

## From the `FrozenInterface` review

### `Passes/Unification.fs:611` — the capability-collision closure is keyed by bare NAME strings

`checkCapabilityInterfaceCollisions` walks the transitive interface closure of a capability's
platform interface as a `Set<string>` of `SymbolKeyOps.bareName` values, seeded at `:630` from
`IntrinsicInterfaceShape.Platform`, which is a bare `string` (`ExternalSymbols.fs:426`). Now
that `FrozenInterfaces` carries a `FrozenInterface`, the fold at `:621` takes a `SymbolKey`
and puts it back through `SymbolKeyOps.qualifiedName` purely to feed that set — a key that was
unstringified upstream and re-stringified here. `Platform` and this `Set<string>` are the two
remaining string carriers on the path.

It is NOT simply a key comparison spelled with strings: it compares the BARE name, arity
suffix stripped, and the comment at `:598` gives the reason — the metadata layer keys
`` IEnumerable`1 `` while the contract layer keys `IEnumerable`. So a `Set<TypeKey>` would
compare MORE than the current code does and would stop matching across the two layers.

The fix therefore has a prerequisite, not just a shape: settle whether that arity mismatch is
a real difference between the layers or a defect in one of them. If it is a defect, `Platform`
becomes a `TypeKey` (`ExternalSymbols.fs:687` already cuts one from it with
`qualifiedTypeKeyOf platform 0`) and the closure a `Set<TypeKey>`. The comment the fix would
delete is the two-line bare-name justification at `:598`.
