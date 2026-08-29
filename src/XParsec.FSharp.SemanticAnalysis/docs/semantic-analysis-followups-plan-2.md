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

### `Tast.fs:163` — three interface-typed fields make `TastFileG`'s derived `=` unsound

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

### `Tast.fs:136` — `TSpecializationG.Decl` is typed wider than the invariant it must satisfy

`Decl: TDeclG<…>` can be any declaration, but every entry is in fact a `TDecl.Let` of
lambdas, so `TSpecializationG.binding` pattern-matches it and `failwithf`s on anything else.
The `spec: SpecializationId` parameter exists only to name the offender in that message —
callers thread an id they otherwise do not need. Storing the pattern and value directly
(`Pat: TPatG<…>` / `Value: TExprG<…>`) would make the failure unrepresentable, drop the
parameter, and delete the sentence "`Decl` is always a `TDecl.Let` of lambdas" from the type's
doc. The cost is at the freeze/codec seam, which currently reads and writes a whole `TDecl`.

### `TastDecl.fs:55` — `TTypeDeclG.Namespace` looks write-only and duplicates `TypeKey.Container`

The field is set once in `Elaborate\TypeDecls.fs`, copied by `TastConvert`, and written and
read back by the frozen codec, but I found no site that consults it for behaviour. It is also
a second spelling of information `TypeKey` already carries: `TypeKey.Container` chains through
the namespace and `TypeKey.Namespace` projects it as a `NamespaceKey`, whereas this field is
a `string option` — the over-wide form of the same key. If it really is unread, deleting it
also removes an option-vs-empty-string mismatch between the two spellings. Hedge: I traced
the writers and the round-trip rather than proving no reader exists, so a consumer reached
through a record-copy expression could have been missed.

### `Unification/EngineCore.fs:75` — the occurs check can miss when `target` is not a union-find root **[LANDED]**

`occursAndAdjust` now takes a `Rep`, whose private case makes `UnionFind.find` the only
producer, so a non-root argument is a compile error. `InferGeneralize.fs` passes the root it
already binds, and the defensive re-`find` inside the walk is deleted.

### `Unification/EngineCore.fs:281` — `capabilityCanonKey` and `capabilityPlatformKey` are the same five-probe chain twice **[LANDED]**

An earlier extraction (`capabilityKeyBy`, commit `7a6da811`) had already collapsed the two
copies to one; the remaining hand-written five-field chain moved to
`CapabilityIds.TryMatch`, beside the field declarations, so a sixth capability field puts
the probe next to the declaration it must cover.

### `Unification/Engine.fs:320` — `unifyArgCoerce` and `tryCoerceUpcast` maintain the same no-pin prefix in parallel

Both walk the identical four steps before diverging: `absorbsAsObj`, then a `TyOr` target via
`subsumes <> Unrelated`, then `numericFamilyOr` + `subsumes`, then `tryStructuralWiden`.
`unifyArgCoerce` falls through to `unify`; `tryCoerceUpcast` continues to `subtypeNominalOf` +
`tryUpcastWitness` and reports a bool. Only the tuple recursion (present in both, at different
entry points) and that tail differ, so the "which targets absorb without pinning" policy is
stated twice and can drift — a new absorbing shape added to one walker silently grounds typars
in the other. Extracting the prefix as a single `absorbsWithoutPinning` predicate would delete
the duplicated arm comments this sweep already had to cut from both sites.

### `SemanticScalars.fs:74` — `SafeContext`, `NativeRegionTier` and both `EscapeState` converters have no PRODUCTION consumer

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

### `TastAccessor.fs:763` — `declExpressionTy` has no consumer anywhere in the repo

A bare `grep -rn declExpressionTy .` over the whole tree (excluding `bin/`, `obj/`, `.git/`)
returns only its own definition and the `failwith` string inside it: no production caller, and
no test either. Its sibling `declExpression` reads the other half of the same `(|DExpression|_|)`
tuple and is used, so the accessor was written as a symmetric pair and only one half landed a
caller. Either something that rebuilds an `Expression` decl is supposed to preserve the declared
slot type and does not, or the accessor is dead — worth deciding which before it acquires a
caller by accident. The doc claiming it is "what a value-producing consumer must preserve when
it rebuilds the decl" was trimmed of that forward claim here, since nothing does.

### `TastAccessor.fs:651` — `patBoundVarNaming` is public but reached only through its own active pattern

Its sole caller anywhere is `(|PNamedNaming|_|)` three lines below it, which does have three
production consumers in the JS backend. The function form is therefore public surface nobody
uses directly, and the two declarations differ only in the calling convention — a candidate for
making the function `private`, or dropping it and inlining the two-line body into the pattern.
Not a defect; noted because this sweep deleted the doc that made them look like two distinct
entry points.

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

### `Diagnostics.fs:347` — `CyclicType(Inheritance)` is unpublished while `CyclicType(Immediate)` maps to FS0954

`Kind.code` files only the `Immediate` case under `DiagCode.FSharp 954`, whose fsc resource name
is `tcTypeDefinitionIsCyclicThroughInheritance`, and drops `TypeCycle.Inheritance` into the
`DiagCode.Unpublished` group. `Kind.message` meanwhile renders `Inheritance` as "has a cyclic
inheritance hierarchy" — the sentence 954 exists for. The split may well be deliberate (the two
verdicts come from different checks: fsc's 954 is about a struct field or immediate containment,
while the inheritance WALK is this compiler's own), and a comment saying so was removed here as a
duplicate of the `TypeCycle` type doc. Worth confirming against fsc which of the two numbers each
verdict should carry, since a consumer suppressing FS0954 today gets only half the family.

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

### `CstWalk.fs:497` and `CstWalk.fs:746` — object-expression member signatures are walked by two near-identical helpers

`iterExprEmbeddedTypes`'s local `memberDefnSigs`/`bindingSig` and `iterTypeDefnTypes`'s local
`methodOrProp`/`memberSig`/`returnTypeOf` both decompose `MemberDefn` → `MethodOrPropDefn` →
`Binding.returnType`, with the same five arms in the same order; they differ only in that the
type-definition version also routes argument patterns through `onPat` and handles
`AdditionalConstructor`. One shared helper parameterised by the pattern callback would delete one
of the two, and would make it impossible to add a `MethodOrPropDefn` case to one walk and forget
the other.

### `Passes/InlineExpansion.fs:472` — the root expressions are collected by re-running the decl mapper for its side effect

After the walk, `run` enumerates the finished decls by calling `mapDeclExprs` a second time with a
function that appends to a `ResizeArray` and returns its argument unchanged, discarding the
rebuilt `TDecl` with `|> ignore`. That rebuilds every declaration (and, through the type-decl
mapper, every member body's spine) purely to enumerate the top-level expressions the spec table
counts edges from. An `iterDeclExprs` sibling — or having the first `List.map` collect the roots
as it goes — would drop a whole tree rebuild per file and make the intent readable without noting
that the result is ignored.

### `Freeze.fs:44` — a residual typar degrades to `FTUnknown "?unresolved-typar"` with no diagnostic

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

### `Codegen.Clr.Tests/SelfHostTests.fs:241`, `:309` — plan-doc milestone labels in test section headers

Two section separators carry `(R1)` and `(R3)` tranche labels from a retired plan. The repo
convention is that milestone labels do not appear in code or test comments — they name a
document the reader cannot open and outlive the plan that gave them meaning. The four
equivalents in `SemanticAnalysis.Tests/ConformanceTests.fs` (`T8 Step 3/4.2/5/6`) were removed
during this sweep; these two were left because they sit in a different project's test suite,
outside the swept scope. One-line edits, no behaviour.

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

### `VesperLib/Manifest.fs:38` — the deleted `.fs`-branch rationale did not match the tree

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

### `XParsec.FSharp/Token.fs:2558` — `GetName`'s example contradicts the line under it

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

### `Elaborate/TypeDecls.fs`, `typeDefnAccessToken` — three cases silently lose their access token

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
