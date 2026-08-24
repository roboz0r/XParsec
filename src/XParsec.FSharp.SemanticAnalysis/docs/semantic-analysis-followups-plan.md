# SemanticAnalysis comment-sweep follow-ups

Non-comment issues found while reading every comment in the project as an unverified claim.
Recorded, not acted on: fixing them mid-sweep would break the code-preservation gate that
makes parallel comment editing safe.

Each entry names the comment its fix would delete. That naming is the acceptance test — if
the change lands and the sentence still needs writing, the change was the wrong shape.

Every claim below was verified against the source by the orchestrator, not taken from a
subagent report.

## Defects

### `Lexing.fs` — an interpolated string's escapes are never decoded

`pSkipInterpolatedFragmentChars` folds a `\x` pair into the fragment without emitting
`Token.EscapeSequence`, so `$"\n"` reaches every consumer with the two raw characters.
Plain, verbatim and triple-quoted strings decode through `Lexing.decodeStringEscape`;
the interpolated path bypasses it. Found while landing the shared escape decoder.

### Cross-file resolution — a module-held union's case does not resolve from another file

With file 1 declaring `module M` / `type Holder = Wrap of obj`, file 2 gets "Unresolved
identifier: Wrap" both bare (after `open`) and as `Holder.Wrap`; the namespace-level
declaration resolves. Found while testing the cross-file obj-box fix; the CrossFileTests
case pins the namespace-level shape and notes the gap.

### `TastPools.toPools` — a `match` on an unresolved case crashes instead of diagnosing

In the module-held shape above, adding `match w with | Wrap v -> …` crashes analysis
(`TastPools.toPools: ModuleMembers entry … names a bound variable no declaration in the
frozen file introduces`) instead of surfacing the unresolved-identifier error. Freeze runs
on a file whose analysis already failed; the diagnostics should gate it.

### `TastLower.fs:149-150` — loop bound and array length come from different parameters

`for j in 0 .. instArr.Length - 1 do match holes.[j] with`, where `holes` is
`Array.create typarCount ValueNone`. `typarCount` and `instArr` are independent parameters of
`solvePhantomTypars` (`:110`, `:113`) with nothing tying them, so `instArr.Length > typarCount`
throws `IndexOutOfRangeException`. Not live — both callers pass a matching pair
(`EmitCall.fs:217`+`:237`, `ClrRecipes.fs:389`+`:391-395`) — which is also the argument for
dropping `typarCount` and using `instArr.Length`.

### `ReferencedProject.fs:489` — a manifest-declared runtime asset that is missing on disk is silently skipped

`runtimeModules` now takes an already-resolved `Manifest list`, so the two `Error`-swallowing
arms are gone. The remaining hole: `if File.Exists abs then` has no `else`, so a declared but
missing runtime asset is indistinguishable from "the package declares none", and the backend
emits a program importing a `.mjs` that was never materialised. Plumbing a fault out of the
`Map`-returning function is the small design choice.

### `SourceFileKind.fs:14`, `Elaborate/Strings.fs:47` — culture-sensitive string comparisons on source text

The original `VesperLib/TypeTranslate.fs` site is gone, but the inconsistency class survives:
`SourceFileKind.fs:14,16` (`.EndsWith ".fsi"` / `".fs"`) and `Elaborate/Strings.fs:47`
(`.StartsWith ":"`) use culture-sensitive overloads while `ReferencedProject.fs:143` uses
`EndsWith(suffix, StringComparison.Ordinal)` for the same class of test.

### `CstKeys.fs:105`, `CstKeys.fs:127` — unimplemented shapes fail at runtime

`firstTokenOfExpr` and `firstTokenOfPat` still end in `failwithf "… TODO %A"`. Prototype-stage
gap; listed because the header comment that used to flag it is gone.

### `FrozenCodec.fs:533` — `checkSlots` leaves the parallel columns unchecked

It guards only the five `ChildColumn`s. The plain parallel columns are read as independently
length-prefixed arrays and never cross-checked against the pool they are indexed by: `exprTys`
(`:504`), `exprToks` (`:505`), `exprVarBoundVar` (`:508`) against `exprPayloads.Length`; `patTys`
(`:510`), `patToks` (`:511`) against `patPayloads.Length`; `boundVarToks` (`:521`) against
`boundVarNames.Length`. `TastPoolTypes.fs:160-184` declares these as columns "each indexed by
`ExprPoolId`", so a truncated one is exactly the corruption `checkSlots` exists to catch — it
stays internally consistent and faults only at whichever node first indexes past the end.

### `FrozenCodecTypes.fs:67` — a duplicate key in a blob is silently last-wins

`readSymbolDict` writes `d.[k] <- v`, so a corrupt blob carrying one `SymbolKey` twice keeps the
last entry. The sibling decode of a keyed table, `DenseTable.index` (`TastPoolTypes.fs:23-24`),
faults with `"id %O appears twice"` on the same class of malformed input. One of the two is
wrong.

### `PrintfHoleForm.fs:196,237,245,283,307,342,403,419,434` — an oversized printf width throws

`FormatDim.Literal` carries a `bigint` (`XParsec.FSharp/Lexing.fs:210`) and the lexer parses
it with the unbounded `pbigint` (`:2457`). `tryClassify` converts with a bare `int w` / `int pr`
at all nine sites, and `BigInteger → Int32` throws `OverflowException`. So
`%99999999999999999999d` lexes cleanly and then throws out of the classifier instead of
producing a diagnostic. `renderPlaceholder`'s `string n` is safe; only the numeric conversions
are exposed.

### `SemanticScalars.fs:18` — `Rational`'s raw constructor breaks the type's own equality

`new(n: bigint, d: bigint)` is public, does not reduce, and does not reject `d = 0` — only
`create` calls `invalidArg "d"` (`:21`). `Equals` (`:49`) compares the two fields, while
`IComparable.CompareTo` (`:60`) cross-multiplies. So `Rational(2I,4I)` and `Rational(1I,2I)`
compare EQUAL under `<`/`>` but UNEQUAL under `=`, and would occupy two slots in a `Map`.

Nothing is broken today: every construction outside the type goes through
`create`/`One`/`Zero`/the operators. Making the raw constructor `private` makes the equality
contract hold by construction.

Deletes: "the raw constructor does not, and `Equals`/`GetHashCode` compare the fields, so only
canonical values match."

### `TastExpr.fs:317` — `TraitCall` can only search the LEFT operand's support set

Superseded: the staged change is docs/trait-call-support-set-plan.md; delete this entry when it lands.

It carries a single `supportTy: 'ty`, so F#'s `(^T1 or ^T2)` cannot be honoured: a member
declared solely on the right operand — `static member (+) (i: int, v: Vector)` — is
unreachable. The exemplar the deleted TODO named is
`let inline lerp c p t = t * c + p * (GenericOne - c)` applied at
`lerp 0.1f Vector2.Zero Vector2.One`, which needs `Vector2.op_Multiply` for `float32 * Vector2`.
Carrying a candidate SET rather than one support type is the stated fix. The single-support-type shape
is verified; the `applyDefaults` failure path the TODO described is not.

## Open: the function-type / `Fun` nominal relation

Not from the comment sweep — split out of the annotation-seam work, and NOT to be actioned
before the representation question below is decided.

### What landed

`inferTypeAnnotation` (`InferTypeOps.fs:147`) now uses `unifyAnnotation`, not bare `unify`, so
`(e : T)` admits exactly what `let x : T = e` admits. F# accepts a subtype ascription in both
positions (`("abc" : seq<char>)` and `let f () : seq<char> = "abc"` both check), and the two
seams previously disagreed: a GROUND function was admitted into `Fun<int,int>` at the binding
and rejected at the ascription. `UnificationBasicsTests.fs` pins five rows.

### What still diverges

`((fun x -> x) : Fun<int, int>)` is REJECTED at both seams, where F# accepts the analogue
`((fun x -> x) : FSharpFunc<_,_>)` and leaves it `'a -> 'a`. Cause: `subsumes`' arm
(`Subsume.fs:208-214`) requires each peeled domain be invariant-`Equal` to its `Fun` argument,
so an unpinned `'a` cannot pin THROUGH the nominal — a read-only subsumption query cannot
unify. The pinned row is the test "an unpinned lambda does not pin through the platform
function nominal".

Making `unify` relate `TyFun(a, b)` with `TyClass(Fun`2, [A; B])` component-wise would close it
— in F# `FSharpFunc<'a,'b>` IS `'a -> 'b`, so both snippets there are plain unification, not
subtyping.

### Decide first: what a `Fun`k`` represents

`RuntimeNames.fs:44` states the split — "Curried at 2, the flat overloads at 3–5" — so only
`Fun`2` is the one-argument curried form. Two source types would share one `Fun`3`:
`int -> int -> int` and `int * int -> int`. Only the FIRST may be partially applied, and the
compiler has to keep them apart at the same representation.

`SemType` already distinguishes them — `TyFun(a, TyFun(b, r))` vs `TyFun(TyTuple [a; b], r)` —
and `FunVerdict.Arity` already records the flat arity a lambda argument was keyed to
(`InferApp.fs`, `recordFunArityVerdicts`). So the information exists; what is missing is the
rule tying the shared representation to which of the two the source wrote.

Also unexamined: `Subsume.fs:208` lets a CURRIED chain satisfy a flat `Fun`3+` by peeling
domains. `Curried` and `Flattened` (`Vesper.Core/fun-adapters.fsi:13`, `:21`) exist to convert
between the two forms at run time, which suggests they are not interchangeable — so whether
that peel is correct is part of the same question, and an identity rule for `Fun`2` alone may
be the whole of the safe answer.

## Types that would delete a comment

### `NodeKey.fs:145` — the bit-packing wants to be a struct DU

A `uint64` with hand-rolled shifts for `syn:1 / reserved:15 / kind:16 / offset:32`, where the
offset slot is signed and the sign distinguishes counter-minted keys from source-positioned
ones. `SourceKey of offset * kind | CounterKey of counter * kind` would make the distinction
a case rather than a sign convention.

Deletes: the wire-format header comment, and the runtime `failwithf` in `SourcePos.ofNodeKey`
(`NodeKey.fs:200`) — presently the only thing stopping a counter-minted key being scoped.

Related: the 15 reserved bits are padding. The prose describing them as a future
per-spawning-construct counter was speculative and is cut; if that plan is dead the bits could
fund a narrower `Raw`.

### `NodeKey.fs:213` — `NodeSite`'s invariant is unenforced

`type NodeSite = { Key: NodeKey; Tok: SyntaxToken }` is a public record, so
`{ Key = …; Tok = … }` with a key that did not come from that token is constructible anywhere.
No such literal exists today. A private representation with `NodeSite.ofToken` as the sole
constructor makes it true by construction.

Deletes: the `NodeSite.ofToken` doc asserting the two halves cannot disagree — already cut,
since as written it claimed for the type what only held for that one function.

### `Tast.fs:61` — `TastFileG` derives an unsound structural `=`

Three fields are read-only collection interfaces that compare by REFERENCE:
`IntrinsicReprKeys: IReadOnlyDictionary<…>` (`:74`), `GlobalValueKeys` (`:85`) and
`Accessibility` (`:152`). So `a = b` compiles and silently answers wrong on any rebuilt file.
`TastFileG.structurallyEqual` (`:190`) is the workaround, and every caller has to remember it
(`FrozenCodecTreeRoundTripTests.fs:42`, `TastPoolsTests.fs:262`). No live misuse found — latent.

Giving those three equatable wrapper types in the shape of the existing `EqArray` deletes the
workaround function and its doc along with it.

### `SideTypes.fs:97` — `ForInEnumeratorG.Pattern` carries five positional payloads

`enumeratorTy`, `getEnumerator`, `members`, `isValueType`, `dispose`, respelled as a 5-tuple at
`TastConvert.fs:59`, `FrozenCodecDecls.fs:108-129` and `EmitClosures.fs:586`. A record payload
names them once — and is why that case doc wanted to be an essay, since two of the five fields
had nowhere else to be documented.

### `TastDecl.fs:260` — `TSecondaryCtorG` encodes an XOR as two arrays

Chain form vs explicit-field-init form is carried as "`PrimaryArgs` non-empty" vs
"`FieldInits` non-empty", and `Codegen.Clr/NominalEmit.fs:526` picks the form with
`if not sc.FieldInits.IsEmpty`. A DU over the two bodies makes the both/neither states
unrepresentable.

### `TastDecl.fs:95` — `Record`'s `valueKind` admits a state its producer cannot make

`Elaborate/TypeDecls.fs:480-484` yields only `Struct` or `RefType` for a record — a record is
never byref-like — but the slot is the full `ClassValueKind`, `RefStruct` included.

### `TastDecl.fs:88` — `Union` and `Record` are still positional tuples

3-wide and 4-wide respectively, while the class payload was already lifted into the `TClassG`
record. The same treatment would name their slots.

### `TastDecl.fs:129` and `:205` — `ThisKey` is stored twice and cannot differ

`TClassG.ThisKey` (`Elaborate/TypeDecls.fs:701`) and every instance member's
`TTypeMemberG.ThisKey` (`Elaborate/ClassMembers.fs:225,247`) are all `info.ThisKey`, itself a
pure function of the declaration's `NodeKey` (`BoundVarKey.ofDeclaredThis`,
`MemberRegistration.fs:607`). The member-level copies are derivable from the class-level one.

### `TastExpr.fs:37` — `IsDefault` hand-writes the all-default test

`member this.IsDefault = not this.CallAtMostOnce` on a struct explicitly framed as an extension
point: adding a second flag makes this silently stop meaning "no attributes set". (Its doc
claimed the opposite polarity and has been deleted; the sole consumer, `Elaborate.fs:54`,
writes `not a.IsDefault` and is consistent with the code.)

### `FrozenTypeBridge.fs:36` — an over-wide policy parameter forces an unreachable arm

`toFrozenWith`'s policy is `onVar: SemType -> FrozenType`, but the only call is
`| TyVar _ -> onVar ty` — it can never be handed anything else. That width is what forces
`Freeze.fs:51` to carry a `failwithf` for a case no caller can produce. Narrowing the policy to
the `TyVar` payload deletes the arm; the `failwithf` is honest, so the fix is the signature, not
the message.

### `TypeInfos.fs:427` — `AbbreviationInfo` encodes one state machine in two mutable fields

`Body : SemType voption` and `Status : AbbreviationStatus` vary independently, so `Filled` with
`Body = ValueNone` is representable. `AbbreviationState = NotFilled | InProgress | Filled of SemType`
makes the cycle-detection invariant compiler-enforced — that invariant is exactly what the
surviving `AbbreviationStatus` doc is standing in for.

### `TypeInfos.fs:150` — `Resolved` conflates "not yet" with "failed"

`ClassInterfaceImplInfo.Resolved : SemType voption` is set only inside `if isInterface then`
(`Passes/Unification.fs:853`), so `ValueNone` means both "not resolved yet" and "resolution
failed and a diagnostic already fired". A three-state field lets a later consumer tell a pending
impl from a rejected one.

### `TypeInfos.fs:231`, `:293`, `:346` — `ThisKey` defaults to a well-formed wrong value

`RecordTypeInfo`, `UnionTypeInfo` and `IntrinsicAbbrevInfo` default `ThisKey` to
`Unchecked.defaultof<BoundVarKey>`. `BoundVarKeyG` is `[<Struct>]` over `NodeKey`
(`TastDecl.fs:12-16`, `NodeKey.fs:124-127`), so that default is a perfectly well-formed
`BoundVar(NodeKey 0UL)` — offset 0, `NodeKind.Unknown` — not a detectable "unset". It is assigned
only when the type has members (`MemberRegistration.fs:1102`, `:1110`, `:1126`). `ClassTypeInfo`
takes `thisKey` as a constructor parameter instead; the other three could do the same, or carry
a `voption`.

### `Passes/Unification/InferApp.fs:279-334` — three printf marker tables encode one verdict

`ctx.PrintfApp`, `ctx.PrintfPartial` and `ctx.PrintfCallbackScratch` are separate side tables keyed
by the same `node.Key`, and their mutual exclusivity — a residual sets no marker; partial needs
`args.Length = idx + 1`, full application needs saturation — lives only in the shape of two guard
clauses. A single `PrintfLowering` DU (`Full of sink | Partial of sink | Cold`) in one table makes
the exclusivity a type fact and deletes the 19-line and 8-line essays this sweep cut.

### `Passes/InlineExpansion.fs:71` — `Expander.LambdaEnv` is a hand-balanced mutable scope

A `Dictionary` with an add/remove pair straddling `walkAt` (`reduceClassified`, ~`:296-307`). If the
walk throws between them the entry leaks; nothing scopes it. An env threaded through `Descent`
removes both the leak and the "stack-disciplined add and remove" prose.

### `Passes/Unification/Translate.fs:506` — the measure carrier is the last by-name reach

`tryResolveExternalType` resolves `float` / `int` by name for a `float<m>` carrier, because
The classifying walk recorded that name at its SYNTACTIC arity of 1 and the carrier is wanted at
arity 0. `TypeRegistration.fs:365-367` already recognises the shape (`isMeasuredCarrier` skips
the carrier so `float<kg>` does not blame `kg`), so the classifying walk knows it is looking at a
carrier and could record the arity-0 verdict there instead of skipping the node.

That is what would leave `ctx.Resolver` read only by NameResolution — the enforcement the
deleted `ResolverAllowlistTests` was standing in for, since the handle could then be a parameter
rather than a `PassContext` member. Worth doing for that reason, not for the lookup it saves.

### `Passes/Unification/Translate.fs:118` — the key-minting invariant is written three times

`externalClassTy`'s body, `buildExternalTy`'s doc and `tryExternalTypeOfKey`'s doc each asserted
"mint on the resolved key, never re-cut from the rendered name, because `InModule` keys don't
round-trip through the `+`-metadata name". One short form survives at `:118-120`. Triplication is
the signal that this belongs in `SymbolKeyOps` as a type-level restriction on which key shapes
`typeMetaName` may round-trip.

### `Passes/NameResolution/Scope.fs:225` — `stampPatCasesWith`'s `typeIter` is a mode encoded as a function

The parameter selects between plain stamping and stamping-that-also-diagnoses an unknown type
name, passed as a `CstWalk.TypeIter`. Only two call shapes exist (`:255` is the sole in-file use).
A two-case mode DU, or two named entry points, would delete the third line of that doc — the only
reason it is 3 lines rather than 2.

### `Passes/Unification/Engine.fs:874`, `:886`, `:897` — `unifyAnnotation`'s three admission policies are prose-only

Union subsumption, literal outward-widening and strict nominal upcast, plus a grounding fallback,
are distinguished only by comment and by a guard whose two halves are a `match` inside a `when`.
A classifier returning a named admission verdict deletes all three blocks.

### `Passes/Unification/Engine.fs:140`, `:348`, `:363`, `:828` — the no-pin absorption set is spelled at two seams

Partially landed: `absorbsAsObj` (`Engine.fs:134-137`) now covers the `obj` case at both seams.
The `TyOr` and numeric-family absorption checks are still duplicated at the two call sites
(`:347-349` and `:852-856`, via `numericFamilyOr`); folding them into the shared predicate
finishes the job.

### `Passes/Unification/Engine.fs:62` — `DotSource.ClassChain`'s doc exists to explain a shape mismatch

The block says only why the chain case cannot be the `subst` + `lookup` pair that `Resolved`
carries. A member-lookup abstraction covering both shapes deletes it outright.

### `Passes/Unification.fs:63` — `TypeMembersFill`'s two bools have three legal combinations

`AllowAbstractSig` and `Generalise` are independent fields, and the fourth state is never
constructed. The three live call sites are class `(true, true)` (`:819-820`), interface-impl
`(false, false)` (`:741`, `:744`), union/record host `(false, true)` (`:846-847`). A three-case
DU deletes the `Generalise` field's three-line doc, which currently has to explain in prose that a
generalised `Equals` emits as arity-1 and no longer matches the arity-0 interface slot.

### `PassContext.fs:192` — `ResolvedType` is three tables sharing one key space

One `SideTable<TypeKey>` written from three unrelated meanings:

- **declaration identity** — `TypeRegistration.fs:746`, `:902`, `:959`, at `declSite.Key`;
- **type-position annotation use site** — `Translate.fs:450` (union), `:455` (enum), at the anchor's `Key`;
- **expression-position type name, and separately a static-qualifier PREFIX** — three writers in
  `Scope.fs` alone (post-sweep lines): `:111` a single-ident bare class as a ctor-sugar application, `:490`
  a whole dotted name as a ctor-sugar application, and `:603` a `TypeApp` prefix of ANY shape at exact
  arity. The last is the awkward one: `:603` stamps `ResolvedType` on the prefix key and `:607`
  stamps `ExternalStaticQualifier` on that SAME key, so a ctor-app consumer reading `ResolvedType`
  can be handed a qualifier prefix rather than a constructible type.

Nothing in the type separates the three. Splitting them deletes the 19 lines of prose that
existed to warn the consumer, and would also remove the need for the (false, now deleted)
a `ResolvedType`-partitioning doc claiming the two tables partition by `NodeKind` — they do not.

### `PassContext.fs:21` — `KeyedTable.Remove` exists for exactly one table

`member _.Remove(key: 'K)` has a single caller in the project: `ctx.Bindings.Scheme.Remove key`
(`Passes/Unification/Infer.fs:514`). The other `.Remove` hits in the project are plain
`Dictionary`/`HashSet` (`InlineExpansion.fs:350` is a `Dictionary<NodeKey, FusedLambda>`,
`:97`). Every other side table is written once and read. An append-only table type, with removal
only on whatever `Scheme` needs, makes the deleted "every table is append-only" claim unwritable
rather than merely untrue.

### `TypeRegistry.fs:271` — `ScopeReach` does not record WHICH route reached the scope

`pathReaches` (`:287`) builds its list from three anonymous loops — enclosing-scope descent
(`:293`), `open` (`:304`), root for a fully-qualified path (`:318`) — and the record carries no
tag distinguishing them. The distinction survives only as a convention on `Offset`: `ValueSome`
is written in the `open` loop alone (`:313`), both other loops write `ValueNone`, and
`claimRank` (`:352-354`) reads that as "an `open` fixes the offset, a scope uses the claim's
own `VisibleFrom`". A `Route = Ancestor | Opened | Root` case makes `Offset` a payload of
`Opened` rather than a `voption` whose emptiness means something.

Deletes: the `Offset` field doc, which currently has to spell that convention out.

### `TypeRegistry.fs:203`, `:398`, `:420` — four `table + short-name index` pairs, threaded as two parameters

`Record`/`RecordNames`, `Union`/`UnionNames`, `Class`/`ClassNames` and
`Abbreviation`/`AbbreviationNames` are one shape instantiated four times, and `registerKeyed`,
`tryKeyOfArity` and `tryKeyOfArglessName` each take the table and its index as separate
arguments. A `KindRegistry<'Info>` bundling the two would delete the per-field "keyed by
`TypeKey`" / "same shape as…" doc chain.

### `Anchor.fs:155` — `AssemblyFilePath.nowhere` is a sentinel, not a case

The `Assembly` half landed with step 1 of
[manifest-default-front-end-plan.md](manifest-default-front-end-plan.md): it is
`AssemblyName voption`, and `CompilingAssembly.none` is gone. What remains is `Relative`,
distinguished by `AssemblyFileId.nowhere` = `""`. The "no file is spelled `""`" invariant is
unchecked; `AssemblyFilePath` could be a DU, or `Relative` a non-empty-string type.

### `SymbolKeys.fs:110` — `TypeKey`'s capability duality is enforced by prose

A BCL platform key and a canonical key are both `TypeKey`s, and which one a comparison must
use is stated in a CAUTION rather than in the type. `sameNominalKey` (`EngineCore.fs:386`) and
`capabilityCanonKey` (`:321`) exist precisely because a bare `=` on nominal type constructors is wrong in one
of the two roles, and each carries its own doc restating the rule — key-EQUALITY seams only,
never inside the base/interface-chain lookups, since rewriting a BCL platform key there erases
that type's own bases (`IEnumerator`1 :> IEnumerator`). A wrapper or active pattern making the
bare comparison unwritable would delete all three docs.

### `GeneralizedTypars.fs` — "abbreviations are erased before inference" has no code site

The deleted FS0664 essay rested on this fact, and nothing in the repo asserts it. If it
matters, it belongs in a test name, not a doc.

### `EqSet.fs:17` — a public generic type with an unconditional O(n²) constructor

The quadratic dedupe scan is justified by "member counts are tiny", but the type is public and
generic with nothing narrowing it to union members. Either narrow the type or degrade to a
hash set above a threshold.

Deletes: "The scan is quadratic — member counts are tiny".

## Unenforced conventions

### `CstKeys.fs` — the CST does not retain the `type` / `and` keyword

Verified: `TypeDefn` (`Expr.fs:725`+) carries `typeName`/`equals` but no keyword token, and
`ModuleElem.Type of ImArr<TypeDefn<'T>>` (`Declarations.fs:37`) carries none. This departs from
the convention that the AST preserves source tokens, and it is the sole reason
`tryFirstTokenOfTypeDefn` must argue that nothing can be written between the keyword and the
token it does return.

Deletes: "The `type` / `and` keyword itself is not kept, but nothing can be written between it
and this token, so file-order visibility is exact here." — the anchor becomes exact instead of
argued.

### `ExternalSymbols.fs:225` — `MethodTyparBounds` has two admissible lengths

Documented as "length `MethodTyparArity`, or EMPTY when none" — a choice no type enforces,
which is why the consumer at `Passes/Unification/InferExternalCall.fs:80` has to use
`Array.tryItem`. Folding the bounds into the arity, or into a per-typar record, deletes both the
prose and the defensive read.

Related but NOT a defect: `ExternalSignature.MethodTyparArity` (`:219`) and
`ExternalMember.MethodTyparArity` (`:270`) hold the same number, and the member field is a
cached copy — `VesperLib.fs:408-431` reads `sign.MethodTyparArity` off the frozen signature to
overwrite the extraction-time `0` placeholder on both the member and its key. A sweep agent
reported the signature-side field as having no production reader; that is wrong, and those lines
are the sync point. Deriving the member field rather than storing it is still an option, but
nothing is out of sync today.

### `PassContext.fs:200` — `ExternalStaticQualifier`'s payload shape is a writer-side promise

A `SideTable<SymbolKey>`; nothing in the type says the key names a static-member-bearing shape.
The three writers (`Passes/NameResolution/Scope.fs:679`, `:687`, `:816`) all gate on
`ExternalTypeShape.Class`, and readers dispatch without re-querying the shape on the strength of
that. A key type carrying the guarantee would make the re-query provably unnecessary instead of
conventionally so.

### `PassContext.fs:249` vs `:509` — the dynamic-escape suppression join is by convention

`DynamicEscapeSite = { Root: TyVarId; Node: NodeSite }` while `DynamicEscapeSuppressed` is a
`HashSet<NodeKey>`, so the lookup at `DynamicEscape.fs:25` has to reach through
`site.Node.Key`. Nothing ties the set's key space to the site's. (A doc claiming the site had its
own `Key` field has been deleted — it does not.)

### `TypeRegistry.fs:704` — `tryNonClassMemberHostByKey` takes a key and a name that must agree

The `key` addresses the union and record tables (`:709`, `:712`); the `name` addresses
`IntrinsicAbbrevHost` (`:715`), whose tables are bare-name keyed. Nothing forces the two to
describe the same declaration. The sole call site derives both from one `TypeName` —
`TypeRegistration.fs:147` takes the name, `:149` builds the key from that same name — so a
single argument carrying both would be sound and would delete the doc clause explaining why
the name is passed alongside the key.

## Allocation on the per-node walk and emit paths

Found while reading `TastAccessor.fs` against its comments. None of these is a hot-frame
micro-fix: they are accessors whose SHAPE invites the allocation, and the cheaper primitives
already exist beside them.

- **`TastAccessor.fs:882-888`** — `iterChildren` and `existsChild` both route through
  `exprChildren`, which allocates two arrays per node (a pool slice plus `Array.map (at e)`),
  on the per-node walk path. `exprChildCount` / `exprChild` (`:172-176`) exist precisely to
  avoid that and would make both allocation-free.
- **`TastAccessor.fs:386-396`** — `(|EMethodCall|_|)` allocates four arrays per read:
  `exprChildren` (slice + map), `es.[1..]`, then `EqArray.ofArray`, which copies
  (`EqArray.fs:100-102`). This is on both backends' emit path. `(|ERecordClone|_|)` (`:291-299`)
  has the same `es.[1..]` slice.

## Dead or duplicated structure

### `Conformance.fs` — `SigDecl` and `ImplDecl` are one shape twice

Structurally identical apart from the shape type, and could share one generic record.

### `SemanticScalars.fs:73-127` — the ref-safety tiers have no production consumer

`SafeContext`, `NativeRegionTier`, `EscapeState.toClrRefSafe` and
`EscapeState.toNativeRegionTier` are referenced from nowhere in `src/`. They ARE exercised by
`test/XParsec.FSharp.SemanticAnalysis.Tests/RegionsTests.fs:608-630`, so this is untested-in-
anger scaffolding rather than unreachable code. (`EscapeState` itself, `RegionRepr` and
`ClosureRepr` do have production consumers.)

Relatedly, **`EscapeState.ReturnOnly` is never minted in production**. Outside its declaration
and the two coarsening maps, the only `src/` reference is the lub at `Passes/Regions.fs:656-657`,
which matches `| ReturnOnly, _ | _, ReturnOnly -> ReturnOnly` — pass-through only. With no
producer the lub cannot yield it either; only the tests construct one directly. Same for
`SafeContext.ReturnOnly`.

### `SemTypeWalks.fs:280` — `mapChildren`'s `TyOr` arm defeats the sharing the others preserve

Every other arm uses `EqArray.mapPreserve` / `refEq` and returns `t` itself when nothing
changed. `| TyOr members -> members.Map f` routes through `UnionMembers.Map` → `SemType.MkUnion`,
which always builds a fresh `EqSet`, so a ground `TyOr` re-allocates on every `zonk` /
`substitute` walk. The doc has been qualified to match the code rather than assert the
invariant the code does not hold.

### `TastAccessor.fs:457-477` — two recognizers over one payload case

`(|EInlineCall|_|)` and `(|EInlineCallOrigin|_|)` both destructure `ExprPayload.InlineCall`, so a
consumer needing both pays two payload fetches and two matches —
`Codegen.Common/InlineExpand.fs:274` and `:280` do exactly that on one node. `New` and
`ILIntrinsic` solve this with a single recognizer returning a whole view; an `InlineCallView`
would make it consistent.

### `Passes/InlineExpansion.fs:398-412` — a re-entry branch that may be unreachable

`outlineNullaryIntrinsic` is reached only through `expandingTemplate` with `Args = []`, and an
operand-less intrinsic names nothing, so the `Descent.reentered` answer for this path looks
unreachable. If that holds, the `expandingTemplate` wrapper at `:409` is pure overhead and its
`ValueSome reentered` branch is dead. Confirm before anyone relies on it.

### `Passes/Unification/Translate.fs:286` — `Type.SuffixedType` with a dotted name has no arm

`int A.T` falls to the `_ ->` catch-all and yields a free `TyVar` with no diagnostic, so
unmodelled syntax is indistinguishable from an unresolvable type name for the user.

### `Passes/NameResolution/Scope.fs:585` — the operator-form long-ident catch-all diagnoses rather than resolves

`Expr.LongIdentOrOp` reports `OperatorFormQualifiedName` unconditionally for any operator-form
long ident that is neither symbolic nor qualified, carrying an explicit `TODO`. Active-pattern
and nil op-names used as values are rejected rather than resolved.

### `ExternalSymbolProviders.fs` — `KeyIndexedChannels` cannot publish index signatures

The record has no index-signature channel, so `ofKeyIndexes` builds its `Named` from
`NamedChannels.empty` without overriding `TryLookupIndexSignature`, and `ofKeyedChannels`'s store view
then answers every index-signature query from that constant `fun _ -> []`. A producer that
acquires index signatures and holds `InModule` keys has no way to publish them and gets no
compile error. The fix is a channel on `KeyIndexedChannels`, not a comment.

### `Passes/Unification/EngineCore.fs:392` — the intrinsic-canon lookup's central rule is unenforced

`canonKey` is a two-tier, forward-only, memoized `SymbolKey → SymbolKey` map whose invariant is
that NEITHER tier may project a name back out of the key — a user nominal `MyLib.int` would
false-match. The tiers are read ad hoc from `ctx.Types.IntrinsicReprKeys` and
`ctx.Provider.TryLookupType`, with nothing enforcing the by-key-only rule. This was 23 lines of
prose before the sweep; a named lookup type with a by-key-only API deletes the 3 that remain.

### `Passes/Unification.fs:99-145` and `:329-370` — the canonical-typar computation is written twice

`generaliseMemberTypars` and the `AbstractSignature` arm of `fillTypeMembers` build
`fixedRoots` / `declared` / `knownNames` and call `GeneralizedTypars.canonical` with structurally
identical code, differing only in the type passed (`memberTy` vs `sigTy`) and the class-typar
source (the `classTypars` parameter vs `fc.TypeParams`). The surviving comment at `:325` saying
this arm has "the same shape as that function" is the tell; one helper deletes it.

### `Passes/InlineReduction.fs:167` — `ExternalFunction.Args = ValueNone` is documented as unreachable-by-shape, but is a `failwithf`

The field doc claimed a non-opening argument means "the function cannot be a template". The consumer
disagrees: `InlineExpansion.fs:191-194` matches `ValueSome served, ValueNone` and raises
"the spliced member … was applied to an argument its declared parameters cannot be bound to".
Encoding the two outcomes in the type (a member that opened vs. one that did not) would
remove the pairwise match over `lookupExternal x.Ctx x.Specs ext.Key, ext.Args` and the failure
arm with it.

### `Passes/Unification/Subsume.fs:213` — the `TyFun` ↔ `Fun<…>` recognizer is spelled twice

The `funSlotArityOfArgs` guard followed by `peelFunDomains` appears here and at
`Engine.fs:700-706`, once as a read-only check and once as the grounding `unify`. The
name-spelling divergence is fixed (both sites now pass a `TypeKey`), but the duplication
remains: one `tryFunSlotPeel` returning the aligned `k+1` types would carry both.

### `Passes/Unification/InferGeneralize.fs:23` — `instantiate` keeps four parallel maps over one root set

`subst`, `freshOf`, `constraintSubst` (seeded from `subst`) and `quantifiedRoots` (seeded from
`freshOf.Keys`) are four structures indexed by the same quantified-root ids, differing only in
which freshening policy each expresses. The 15 lines of prose cut from `:34` and the 12 from `:42`
existed entirely to explain how they relate — a single "instantiation" record carrying the root,
its fresh var and whether it was quantified would delete both blocks rather than shorten them.

### `Inline.fs:35` — `quantifiedTypars` order disagrees with the canonical typar order

`quantifiedTypars` is a bare `collectLinkedRoots` walk, so it returns typars in
first-left-to-right-appearance order only. The order a module-`let`'s scheme is actually
quantified in is `GeneralizedTypars.canonical` (via `Elaborate/Typars.fs:71 mkMethodQuantEnv`),
which puts EXPLICITLY-DECLARED typars first in source order and only then the inferred roots by
appearance. For `let inline f<'b, 'a> (x: 'a) (y: 'b) = …` the two disagree, so any `typeArgs`
array built against the frozen/ABI typar index is applied to the wrong roots by `inlineExpand`.
The doc that claimed the two orders match ("This reproduces the order `Unification.generalise`
collects them in") was deleted in the comment sweep; the divergence itself is untested.

### `Inline.fs:177` — an un-expanded `StaticOptimization` reaches codegen and emits silently

The deleted `inlineExpand` doc asserted that "NEITHER backend can emit" `StaticOptimization` or
`TraitCall`. Only half of that is true: `TraitCall` faults via `TastLower.traitCallUnresolved`
(`TastLower.fs:37`), but `Codegen.Js/EmitJs.fs:419` and `Codegen.Clr/EmitIntrinsic.fs:128` both
emit the clause's DYNAMIC DEFAULT for a `StaticOptimization` that survived expansion.

The fallback itself is correct and deliberate — both backend arms say so, and a clause is an
optimisation over the default, never a different meaning. The residue is only that the two nodes
are unlike and were documented as one case: `TraitCall` surviving expansion is a compiler fault,
`StaticOptimization` surviving it is a missed optimisation with no diagnostic. Worth a
`StaticOptimization`-survived counter or debug warning rather than a fault.

### `Passes/NameResolution/MemberRegistration.fs:761` — the heritable-base result wants two named cases, not a `struct` tuple + a `.ctor` probe

`resolveThroughProvider` picks `struct (id, surface)` out of `tryPickExternalType`, then decides
the base's whole representation with `surface.Members |> Array.exists (fun m -> m.Name = ".ctor")`
— present ⇒ `TyConst(SymbolKey.Type id.Canon, …)` (the contract is the constructible surface, so
base-ctor args are checked against it), absent ⇒ `IntrinsicPlatform.Repr` → `reprToExternalBase`,
a `TyClass` bound to the runtime type's own ctors. Two structurally different bases, distinguished
by a string comparison against `".ctor"` on an array, with the consequence recorded only in prose
(a five-line comment cut to three in the sweep). A two-case result from the pick (`ContractCtors
of surface | PlatformOnly of platform`) would put the branch in the type.

### `Passes/NameResolution/MemberRegistration.fs:1047` — `registerGroup`'s five phases are five bare loops over the same collection

The function runs six sequential `for` loops (claim over `defs`, classify over `defs`, file
abbrevs over `claims`, detail over `claims`, interface validation over `defs`, then the close:
force aliases, fill `inherit` slots, two cycle checks). The ordering constraints between them are
real and load-bearing — an abbreviation ENTRY must exist before any other kind's detail runs, and
`BaseType` cannot be filled until every claim in the group has its detail — but nothing in the
code expresses them; before the sweep they were a 25-line numbered list in the doc comment (now
three lines). Phase-typed stages, or at least one named function per phase, would let the
dependency order be read off the pipeline rather than off prose.

### `Passes/NameResolution/MemberRegistration.fs:265` — the declared-typar prefix is an `int` beside an unrelated array

`mkTypeParams ctx.Store (explicit @ implicit)` concatenates two differently-ordered typar lists
into one `SeedTypars`, and `List.length explicit` is passed alongside as `declaredCount` so that
consumers can recover the split with `List.truncate mInfo.DeclaredTyparCount`. The invariant
"the leading `DeclaredTyparCount` entries are the explicit `<'C>`, the tail is appearance-ordered"
is prose in three places (`TypeInfos.fs:70`, here, and both copies of the canonical computation
already noted at `Passes/Unification.fs:99-145` / `:329-370`). A seed carrying the two groups as
separate fields would delete the arithmetic and the comment at every site.

### `Passes/NameResolution/MemberRegistration.fs:335` — a secondary constructor in a union/record augmentation is dropped with no diagnostic

`extractMembers` is shared by class registration and by union/record augmentation, and its
`MemberDefn.AdditionalConstructor` arm is `()` on the grounds that class registration collects
those separately via `extractSecondaryCtors`. For an augmentation there is no such second pass:
`registerNominalMember` calls only `extractMembers` and `extractInterfaceImpls`, so a `new(…)`
written in a `type U with … new(…) = …` block is parsed and then silently discarded. Not verified
whether some earlier pass rejects the shape; if none does, this is a missing "not supported here"
diagnostic rather than a mis-registration.

### `Elaborate/Typars.fs:86` — the dependent-typar fixpoint is duplicated in `InferGeneralize.generalise`

`mkMethodQuantEnv`'s worklist (`:86-95`) and `InferGeneralize.generalise`'s (`InferGeneralize.fs:264-272`)
are the same algorithm: walk the collected roots, fold each root's `Coercion` constraint targets
back into the same `ResizeArray`, and let the array's growth drive the index to a fixpoint — so a
constraint-only typar such as `'E` in `'S :> IStructSeq<'E>` is quantified. The two copies use
different root collectors (`SemTypeWalk.collectLinkedRoots` here, `iterTypeVarRoots` there) and
differ in whether a level test gates the seed, which is exactly the kind of divergence that will
not show up until the two disagree about which typars a scheme has.

### `ExternalSymbols.fs:224` — `MethodTyparArity` is stored twice per member

`ExternalMember.MethodTyparArity` (`:224`) and `ExternalMember.Signature.MethodTyparArity`
(`ExternalSignature`, `:180`) are the same count, written side by side at every construction site —
`VesperLib.fs:313`/`:321`/`:323` set both from one `sign.MethodTyparArity`, and `FrozenTypeTable.fs:288`,
`InferOverload.fs:338` copy them forward as a pair. Nothing keeps them equal, and the two are read
independently: `ClrExternalMembers.fs:142` reads the member's, `instantiateSignatureBounds` reads the
signature's `MethodTyparBounds` whose documented length is the OTHER one. One owner (the signature,
which is where the typars are actually baked) would remove the pairing and the doc line on the field.

### `ExternalSymbols.fs:89` — the `deferredTemplate` sentinel is a two-phase type spelled as a magic value

Five separate fields hold `FrozenTypeBridge.deferredTemplate` (`FTUnknown "<deferred>"`) between
contract extraction and `VesperLib.finalizeDeferred`: `ExternalFieldShape.Frozen` (`:89`),
`ExternalCaseShape.FrozenFieldTypes` (`:107`), `ExternalSignature.Parameters` / `.Return` (`:195`)
and `ExternalSymbol.Scheme` (`:888`). Each needed a doc line saying the value may be a sentinel, and
nothing in the type stops a consumer reading one before the finalize pass overwrites it — a distinct
pre-finalize shape (or a `Deferred<FrozenType>` wrapper the finalize pass consumes) would delete all
five comments and make the ordering a compile error instead of a convention.

### `DynamicEscape.fs:12` — the suppression set is a `HashSet<NodeKey>` parallel to the escape sites

`ctx.DynamicEscapes` holds the escape sites and `ctx.DynamicEscapeSuppressed` is a separate
`HashSet<NodeKey>` (`PassContext.fs:509`) that `InferTypeOps.fs:203` adds to when an ascription sits
directly on the `?` expression. The pairing is only enforced by both sides agreeing on
`CstKeys.ofExpr`/`site.Node.Key` spelling the same key, and it was the subject of a 15-line file
header (now cut to 3). A `Suppressed: bool` on the escape-site record, set where the site is
recorded, would carry the same fact structurally.

### `Passes/NameResolution.fs:23` — `TypeBodiesWalk` is two record shapes in one

Six of the twelve fields (`BaseKey`, `CtorParams`, `InstanceFields`, `StaticPreamble`,
`InstancePreamble`, `SecondaryCtors`, `InheritsExpr`) are meaningful only for a class:
`walkNominalHostBodies` (`:408`) fills every one of them with `[||]` / `ValueNone` for a union or
record host, and `walkTypeBodies` then loops over the empties. Each field needed a doc line saying
"empty for unions". A DU of two cases — a class body and a nominal-host body — would delete those
lines and stop the host path from having to name fields it does not have.

### `Passes/Regions.fs:6` — the pass contract lives in a `Pre:`/`Post:` prose header

*Half landed 2026-08-13: `run` returns a `RegionVerdicts`, so the representation axis is no longer
a `PassContext` table and the snapshot cannot be taken before the pass — it IS the pass's return.*

Which side tables `run` requires (`Bindings.Binding`, `Bindings.TypeVar`) and which it still fills
(`Bindings.Escape`, `Store.Region`) is stated only in the file header, and the same shape recurs in
`Validation.fs`, `Desugar.fs` and `RefCellPromotion.fs`. An explicit input record (the tables read),
threaded by `Pipeline`, would delete four prose headers and turn the ordering into a compile error.

### `Passes/Desugar.fs:95` — the type-body traversal is a third hand-rolled copy

`walkCtorBody` (`:95`), `walkMemberElems` (`:122`) and `walkClassBody` (`:145`) enumerate every
expression reachable from a `TypeDefn`: member and property bodies, auto-property initialisers,
secondary-constructor bodies in all five `AdditionalConstrExpr` shapes, interface-impl members, the
class preamble's `let`/`do`, and the primary `inherit` argument. `NameResolution.fs:244` walks the
same grammar with its own `walkCtorBody`, and `Validation.fs:238` walks a deliberately narrower subset
of it. The three drift independently, one arm at a time: Desugar's copy carries interface-impl,
class-preamble, `inherit`-argument and union/record/abbrev-extension arms that Validation's does not,
and the deleted comments recorded each of those as a bug found after the fact (a missing arm left
`inferInfix` on a free TyVar). Whether Validation's narrower reach is intentional or the same gap
not yet hit is not stated anywhere. One `CstWalk` entry point yielding every `Expr` under a
`TypeDefn`, with each pass supplying only its visitor, would remove the divergence.

### `InferResolve.fs:200` — `bareIndex: bool` is a boolean-blind spelling of two different lookups

`recordFieldSetVerdict` takes `bareIndex: bool` purely to decide whether provider candidates
pass through `admitsBareExternalRecord` (`:176`). The two callers in `resolveRecordFor` (`:271`
and `:279`) already know which they are: the qualified arm has a `typeName` in hand and passes
`false`, the bare arm passes `true`. A `RecordLookup = Bare | Qualified of string` argument
would carry the qualifier that the qualified arm currently filters on AFTER the call
(`resolvedRecordDisplayName r = typeName`), fold both decisions into one place, and delete the
16-line header on `admitsBareExternalRecord` plus the `bareIndex` paragraph on
`recordFieldSetVerdict` (both cut to 3 lines by the comment sweep, so the debt is now invisible).

### `Passes/Unification/InferTypeOps.fs:25` — explicit type application on a bare generic function is a no-op

`inferTypeApp` only pins the explicit arguments through a nominal result: with no nominal
result the explicit args are dropped, so `id<string> 3` type-checks. The fix instantiates the
binding's scheme at the explicit args instead.

### `InferTypeOps.fs:187` — the type test does not strip a reference-`null` source, unlike the downcast

`inferDynamicDowncast` runs `stripReferenceNull` over the source before the `subsumes` check,
on the stated grounds that a `T | null` coerces exactly as `T` does. `inferDynamicTypeTest`
directly above performs the mirror-image check on the unstripped `srcTy`, so a `:?` on a
nullable-reference source appears to reach `subsumes` as a union and risks a spurious
`UnrelatedTypeTest`. The two arms should agree unless the type test is deliberately stricter;
I did not construct a failing case, so treat this as an asymmetry to confirm rather than a
proven bug.

### `InferTypeOps.fs:82` — static-optimization clause bodies are never checked against the declared result

Pinned by the skipped GAP ptest in UnificationBasicsTests; deferred because honest checking
means unifying each clause body under its own constraint substitution.

`inferLibraryOnlyStaticOptimization` types each `OptimizedExpr` only to solve its own subtrees
and deliberately never unifies it with anything: F#'s rule is per-clause ("assume the
constraint, then check the body against the return type"), and a blanket unify would wrongly
fuse the distinct clause results of an `^T3`-returning operator. Soundness therefore rests
entirely on the clause being selected and substituted at inline expansion. Checking it here
needs speculative unification under an assumed constraint with an undo, which the engine does
not have. Recorded because the 24-line header stating it was cut to three by the sweep.

### `Elaborate/Calls.fs:141` — `TExpr.New.ClassName` is a payload field nothing resolves by

`translateNew` (`ElaborateExpr.fs:383`) computes `className` via `SymbolKeyOps.typeMetaName`,
and the doc there claimed the qualified spelling exists so "the backend's external-ctor recipe
resolves" and that "the backend strips to the bare simple name for the project-local class
lookup". Neither happens. `exprNewClassName` has exactly two consumers: `EmitConstruct.buildNew`
(`:18`), which resolves both the local class and the external `.ctor` off the `FTClass` KEY and
whose own comment says "`className` survives only for the error message", and `EmitJs.fs:263`,
which also uses it only in a `failwithf`. So the string is elaborated, frozen and serialised to
carry diagnostic text. Either drop the field and have the backends print the type key, or keep
it and say so on the declaration; the comment has been rewritten to the diagnostic-only reading.

### `Elaborate/Apply.fs:35` — `TConstValue.Unit` doubles as the "slot omitted" marker in `OptionalDefaults`

`TsManifestMembers.fs:54` fills `OptionalDefaults` with `List.replicate n TConstValue.Unit` for
trailing optionals that carry no constant default, and `optionalDefaultNode` then reads that
same case back as "emit `undefined`, not a `unit` value". Every other case in the list is a
genuine literal default. A `TConstValue voption`, or an `OptionalDefault = Omitted | Const of
TConstValue`, would make the two readings distinct instead of relying on a case that also has a
legitimate meaning — and would delete the three-line disclaimer now sitting on the `Unit` arm.
The CLR-side fill happens never to mint `Unit`, so nothing is currently mis-lowered.

### `Elaborate/Calls.fs:109` — `viaOfObjArg` scans every class on every instance access

To decide `CallVia.Base` vs `CallVia.Self` it walks all of `ctx.Types.Class` comparing
`BoundVarKey.identity kv.Value.BaseKey` against the object argument's binding site, and the loop
has no early exit — the `not isBase` guard only skips the comparison, it still iterates the
remainder. The information wanted is a set of base-boundVar `NodeKey`s, which could be built once
per file and consulted in O(1); or the `base` object argument could carry its own `TExpr` case so
the question
never has to be re-derived from a `Var`. Recorded because the six-line header justifying the
cost (citing a "gap doc" no reader of this repo can open) was cut to three by the sweep.

### `ElaborateExpr.fs:568` — the `StaticOpt` side table is positionally aligned with `clauses` by convention only

`translateStaticOptimization` pairs `resolved.[i]` with `clauses.[i]` and silently substitutes
`EqArray.empty` when `i >= resolved.Length`. Since `Inline`'s `clauseSelected` is
`EqArray.forall holds`, an empty constraint set is vacuously true, so a short or missing entry
turns the source's FIRST clause into an unconditional selection rather than falling to the
default — a wrong-body expansion, not a decline. Nothing in the types ties the side table's
length or order to the CST clause list. Recording the constraints ON the clause (or keying them
by the clause's own node key rather than the construct's) would make the misalignment
unrepresentable and delete the eight-line header the sweep cut to three.

### `Elaborate/Apply.fs:224` — SRTP trait calls search the left operand only

Superseded: the staged change is docs/trait-call-support-set-plan.md; delete this entry when it lands.

`TExpr.TraitCall` carries one support type, so `translateStaticMemberInvocation` takes
`args.[0]`'s type and the `(^T1 or ^T2)` support set is never searched on the right. A member
declared only on the right operand — the `int * Vector -> Vector` scalar-prefix multiply shape —
therefore does not resolve, and the failure surfaces at inline expansion as a declined trait
rather than at the invocation. Carrying a candidate set on the node is what would buy it. This
is a genuine semantic gap rather than a comment defect; the note claiming it was the surviving
half of a twelve-line doc, so it is recorded here before being shortened.

### `Elaborate/TypeDecls.fs:618` — the intrinsic-abbrev host reuses `TTypeKind.Class` as a never-emitted lift-only carrier

`tryIntrinsicAbbrevType` builds a full `TClass` with every non-member facet empty (no ctor,
fields, base, preambles or impls) purely because `Class` is the container kind the not-yet-frozen
passes carry most inertly; the decl is consumed only by member-inline lifting and never emitted,
and its members' `ThisTy` is the abbrev's `TyConst`, not a `TyClass`. A distinct `TTypeKind` case
(or a decl-level "not emitted" marker) would make the inertness structural and delete the 16-line
header that argued for the choice — cut to 3 lines by the comment sweep, so the debt is now
invisible.

### `Elaborate/TypeDecls.fs:86` — the four `try*Type` surfacers repeat one typar-env preamble

`tryInterfaceMethods` (`:86`), `tryUnionType` (`:157`), `tryRecordType` (`:379`), `tryClassType`
(`:523`) and `tryIntrinsicAbbrevType` (`:628`) all open with `mkDeclTyparEnv ctx.Store
info.TypeParams`, wrap it in a `ResizeArray`, project `declTypars` off `info.TypeParams`, build a
`selfTy` from `declTyparArgs`, and hand both to `mkMemberElaborator` — differing only in which
`Ty*` constructor makes the self-type. A helper taking that constructor collapses five copies and
removes the freeze-env explanation that was duplicated as a comment at four of the five sites
(three of those duplicates deleted by the comment sweep, leaving the fact stated once).

### `Passes/Unification/Infer.fs:279` — `TyparScope` is a raw mutable field save/restore, so its scoping discipline lives only in prose

`inferBinding` saves `ctx.Resolution.TyparScope`, replaces it with a fresh `Dictionary`, copies
the saved entries back in as the lowest-priority layer, and restores it in a `finally`. The
correctness argument — that `savedScope` is the LEXICAL parent's scope precisely because the
`finally` runs per binding, so a restored sibling can never bleed through — is entirely
manual: nothing in the type prevents a nested walk from mutating the field without restoring,
and the failure mode is a silently ungrounded typar that only surfaces at codegen. A scoped
handle (an `IDisposable` push, or a scope value threaded rather than stored) would make the
nesting structural and delete the eleven-line block that argued it, which the comment sweep cut
to three, so the debt is now invisible.

### `Passes/Unification/InferOverload.fs:54` — `matchTypes` is a hand-maintained parallel copy of `unify`'s concrete-type-constructor arms

The doc's own justification was that the two are "kept auditably parallel so a future reader can
diff the two": `TyConst`/`TyRecord`/`TyUnion`/`TyClass`/`TyFun`/`TyTuple` are each destructured
and recursed in both `matchTypes` and `Engine.unify`, differing only in what the metavar and
method-typar arms do (record into `binds` vs `Link` into the graph). A shared structural walk
parameterised by a per-node handler would make the parallelism mechanical instead of an instruction
to the reader; a new `SemType` case added to `unify` alone currently degrades overload filtering
silently to `| _ -> false`. The sweep cut the twenty-one-line header to three, so the invitation
to diff is gone but the duplication is not.

### `Passes/Unification/InferExternalCall.fs:494` — `LocalMemberCall` is typed `SymbolKey` but only ever holds `SymbolKey.Member`

The side table is declared `SideTable<SymbolKey>` (`PassContext.fs:148`); the only writer here
passes `frozenUserMemberKey`, which returns `SymbolKeyOps.memberKey …`, and the reader
(`Elaborate/Calls.fs:143`) feeds the value straight to `TExpr.MethodCall`, which wants a member
identity. So a `SymbolKey.Type` or `SymbolKey.Value` in this table would be silently accepted
and mis-emitted. Narrowing the table to `MemberKey` (the payload of `SymbolKey.Member`) would
make that unrepresentable. Noting it because the doc here and at `Elaborate/Calls.fs:130` both
claimed the table already held a `MemberKey` — corrected to `SymbolKey` in this file by the
comment sweep, still wrong at the consumer.

### `Elaborate/Printf.fs:266` — `structuredArgFaithful` is only correct on an already-zonked type, and nothing says so in its type

`| TyVar _ -> true` means an unresolved metavar reads as engine-faithful, so the function is
sound only because its one caller passes `Unification.zonk ctx.Store …`; its recursion into
`EqArray` children does not re-resolve links. Today `zonk` is deep (`EngineCore.fs:51` recurses
through `mapChildren`), so no live defect follows, but the precondition is carried entirely by a
comment. A `Zonked` wrapper — or taking the `TypeStore` and resolving at each step, as the
sibling walks in this project do — would make it structural. I did not check whether any future
caller is planned that would pass an unzonked type.

### `Pipeline.fs:138` — closure verdicts reach codegen only by a hand-written side-table snapshot

`analyseSemWithContextForCore` copies two verdict tables onto the `TastFile` by hand
(`ClosureReprs` from `Regions.run`'s return, `FunVerdicts` from `ctx.FunVerdicts`), because
codegen holds no `PassContext`. Nothing types the requirement: a third verdict table added to
`PassContext` compiles and reaches codegen as a silent default. The prose that explained this ran
to eleven lines over three blocks and was cut to two by the comment sweep, so the debt is now
invisible. Note also that those deleted comments were wrong about both tables — they called
`FunVerdicts` "node-keyed" (it is `Map<LambdaKey, FunVerdict>`) and attributed the decision to
`inferApp` (it is `recordFunArityVerdicts`), which is the kind of drift a snapshot step that the
type system does not name will keep producing.

### `Elaborate/Args.fs:57` — `peelOneArg` and `peelCtorArgs` disagree on `(())`

The two functions are otherwise the same match, and `peelOneArg` is documented as "same as
`peelCtorArgs` but for a single argument expression", but `peelOneArg` carries an
`EnclosedBlock(ValueParen, EmptyBlock _) -> EqArray.empty` arm that `peelCtorArgs` does not. In
`peelCtorArgs` that shape falls through to the general `EnclosedBlock(ValueParen, inner)` arm and
becomes ONE argument holding a translated `EmptyBlock`. So `Point(())` peels to arity 1 through
the `Expr.App` path and arity 0 through the `HighPrecedenceApp`/`Expr.New` path. One of the two
is wrong; I did not determine which, since it depends on whether an explicitly-written unit
argument should reach a constructor as a value. Either way the fix is to make `peelCtorArgs`'
single-argument branch call `peelOneArg`, which is what the doc already claims the relationship
is.

### `TypeInfos.fs:176` — an unset `ThisKey` is a *valid* boundVar key, not a detectable hole

`RecordTypeInfo`, `UnionTypeInfo` and `IntrinsicAbbrevInfo` all default `ThisKey` to
`Unchecked.defaultof<BoundVarKey>`, and the docs cut here said it is "set during registration when
there are members" — i.e. left at the default for every type without members. `BoundVarKey` is
`[<Struct>] BoundVar of NodeKey` and `NodeKey` is a struct over a `uint64`, so the default is not
null and cannot fault: it is `BoundVar(NodeKey 0UL)`, a structurally legal key naming offset 0.
Anything that reads `ThisKey` off a member-less type therefore aliases whatever boundVar holds the
zero key rather than failing. `ClassTypeInfo` takes `thisKey` as a constructor parameter instead
and has no such state, which is the shape the other three should have — or the field should be a
`voption` so "not registered" is representable.

### `TypeInfos.fs:181` — the nominal-identity and `IInterfaceImplHost` blocks are written out four times

`RecordTypeInfo`, `UnionTypeInfo`, `IntrinsicAbbrevInfo` and `ClassTypeInfo` each repeat the same
run of members verbatim — `TypeKey`/`Key`, `TypeParams`, `DeclSite`, `ThisName`, `ThisKey`,
`Members`, `InterfaceImpls`, `EqualitySupport`, `ComparisonSupport` — and then an
`interface IInterfaceImplHost` block whose ten members forward each of them one for one, with
only `MkSelfType` differing between the four. `EnumTypeInfo` and `AbbreviationInfo` repeat the
`TypeKey`/`Key` pair as well. Most of the per-field docs deleted in this sweep were four
paraphrases of one fact, which is the usual symptom: a shared record of the nominal-host state,
held as a field by each info and surfaced once, would leave `MkSelfType` as the only per-kind
member and delete the duplication rather than re-documenting it.

### `TypeInfos.fs:55` — `SeedTypars` is documented as unordered and consumed as ordered

The doc cut from `SeedTypars` called it "order-IRRELEVANT (lookups are by name / union-find
root)", but `DeclaredTyparCount` right below it is defined as a count of *leading* entries in
source order, and `EffectiveMethodTypars` hands the same array out as the call-site method-typar
list. If the declared-first prefix is real then the order is load-bearing and the "order
irrelevant" reading is a trap for anyone rebuilding the array; if it is not, `DeclaredTyparCount`
means nothing. An `EqArray<string * TyVarId>` plus a separate `int` cannot enforce either
reading — a type splitting the declared prefix from the implicit tail would settle which one is
true and remove the need for the sentence.

### `PassContext.fs:178` — `ExternalUnionRecordQualifier` is a `SymbolKey` sink that only ever holds a `TypeKey`

Half landed: `ExternalStaticQualifier` is now `SideTable<TypeKey>`. `ExternalUnionRecordQualifier`
(`PassContext.fs:178`) is still `SideTable<SymbolKey>`, and `Scope.fs:499-502` still wraps via
`SymbolKeyOps.externalTypeKey` (→ `SymbolKey.Type`) though only ever a type key.
`externalTypeKeyOf`/`externalTypeKey` (`SymbolKeyOps.fs:365-372`) both survive only for that wrap.
Narrowing the remaining sink to `TypeKey` deletes them.

### `TypeRegistry.fs:548` — three `IInterfaceImplHost` resolvers repeat one cascade

`tryInterfaceImplHostByKey` (Class → Union → Record, by key), `tryNonClassMemberHost`
(Union → Record → `IntrinsicAbbrevHost`, by name) and `tryNonClassMemberHostByKey`
(Union → Record → `IntrinsicAbbrevHost`, by key *and* name) are the same nested
`match … | ValueSome info -> ValueSome(info :> IInterfaceImplHost) | ValueNone -> …` written
three times, differing only in which tables are probed and how. The intrinsic-abbrev leg is what
forces the third one to take both a key and a name, since its table is bare-name keyed while the
other two are `TypeKey` keyed — so the duplication is really a symptom of that one table having a
different key type from its siblings. A list of probes folded over, or a `TypeKey`-keyed
intrinsic-abbrev host, would collapse all three.
