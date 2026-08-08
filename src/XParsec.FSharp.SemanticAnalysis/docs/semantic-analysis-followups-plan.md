# SemanticAnalysis comment-sweep follow-ups

Non-comment issues found while reading every comment in the project as an unverified claim.
Recorded, not acted on: fixing them mid-sweep would break the code-preservation gate that
makes parallel comment editing safe.

Each entry names the comment its fix would delete. That naming is the acceptance test — if
the change lands and the sentence still needs writing, the change was the wrong shape.

Every claim below was verified against the source by the orchestrator, not taken from a
subagent report.

## Defects

### `Passes/NameResolution/Scope.fs` — `bindingsOfPat`'s binds-nothing arm still holds `Pat.Named`

Reached whenever `isCtorPat` fails, so an unrecognised `Foo x` drops `x` with no diagnostic —
the shape the `Pat.Or` / `Pat.And` entry here used to describe, before those two started
collecting their sub-patterns' bindings and the or-chain's gap became one report from the
pass that cannot lower it (`CoverageTests.fs`, "or-pattern that binds names is the only
diagnostic").

### `Passes/Unification/EngineCore.fs:526` — a surfaced external interface takes the local-registry path

`subtypeInterfacesOf` mints an external interface as `TyClass(qualifiedTypeKeyOf n ta.Length, …)`,
and `nominalKeyOf` (`:458-463`) answers `ValueSome` for a `TyClass`. So when the walk recurses
through that surfaced interface (`tryUpcastWitness:549`, and `:582`), it derives a `localKey` from
an EXTERNAL key and tries `tryClassByKey` / `tryInterfaceImplHostByKey` against the project
registry before falling back to the provider.

A deleted comment asserted the opposite as the design — "an external interface is surfaced as a
`TyConst` (no local registry key, so its recursion routes back to the provider by the canon key)".
The `TyClass` mint is itself deliberate and documented (it makes the surfaced interface compare
equal to a written `A<int>` annotation), so the two intents collide. At best a wasted lookup per
level; a mis-route if an external interface's arity key ever matches a project-local one.

### `Passes/Unification.fs:600` — `checkObjectOverrideConformance` pins EVERY override to the `System.Object` slot

The loop tests `mInfo.IsOverride && mInfo.Kind = ClassMemberKind.Method` and then matches on the
NAME alone (`"Equals"` / `"GetHashCode"` / `"ToString"`), unifying `mInfo.Type` against the fixed
`TyFun(objTy, boolTy)` / `TyFun(unitTy, intTy)` / `TyFun(unitTy, stringTy)`. There is no
`info.BaseType` guard in the loop, and none at the sole call site (`:826`). So a class deriving a
project-local base and overriding a base virtual that happens to be named `Equals` with a
non-Object signature (`abstract Equals: MyType -> bool`) is unified against the Object slot and
raises a spurious type error.

A deleted comment claimed the pass was restricted to `inherit`-less classes ("a class deriving a
project-local base … is a later slice"). Nothing enforces that.

### `Conformance.fs:352` — an operator `let` with parameters is reported as missing

`PatternParsing.fs:547-550` parses `let (+) a b = …` as `Pat.OpNamed(ident, args)`, emitting
the bare `Pat.Op` only when `args.IsEmpty`. `boundName` has arms for `Pat.Op`,
`Pat.NamedSimple`, `Pat.Named`, `Pat.EnclosedBlock` and `Pat.Typed`, but none for
`Pat.OpNamed`, so such a binding falls to `_ -> ValueNone` and `summariseImplVals` never
records it. The `.fsi` side's `val ( + ) : …` IS recorded, via `identOrOpRaw`. Every operator
declared with parameters in an implementation therefore raises a false `ValueMissingInImpl`.

Fix: one arm forwarding `Pat.OpNamed(ident = io)` to the same `identOrOpRaw` path as `Pat.Op`.

Deletes: the `boundName` doc clause "An operator applied to arguments (`let (+) a b`)
is a `Pat.OpNamed` and yields `ValueNone`", which currently documents the bug as behaviour.

### `FrozenSignature.fs:419` vs `ConformanceTypars.fs:78` — two files disagree about a named binding missing from `ModuleMembers`

Both loops destructure `TastAccessor.DLet { Binding = TastAccessor.PNamed boundVar }`, and `PNamed`
matches `PatPayload.NamedSimple` alone (`TastAccessor.fs:691-695`) — so a destructuring
`let (a, b) = …` never reaches either lookup. What each does on a `moduleMembers` miss then
differs:

- `FrozenSignature.fs:419` — `| _ -> ()`. The binding is dropped from the published signature.
- `ConformanceTypars.fs:78-84` — recovers the name from the boundVar column
  (`TastPoolBuilder.boundVarNaming` → `BoundVarNaming.Source n`) and checks it.

Either that fallback is dead or `FrozenSignature` has an export hole; both cannot be right. The
writer is `recordExportedBinding` (`Elaborate.fs:146-160`), which records only when
`memberNameOfBinding` (`Elaborate/Members.fs:37-50`) answers `ValueSome`.

The surviving `FrozenSignature.fs:410-412` comment asserts the invariant this questions — "EVERY
module binding rides `Decls` … and its identity is in `ModuleMembers`". A deleted clause excused
the `_` arm as "a destructuring `let` … has nothing to export", which the `PNamed` guard makes
impossible.

### `ConformancePass.fs:208` — `externs` under-reports the decls that made the file bodiless

`externs` collects `SigShape.Extern` / `ExternClass` only, while `bodiless` (`:217`) also accepts
`SigShape.Abbrev`. `externs` feeds the `Unrepresentable` arm (`:201`), reachable only when
`bodiless` holds — so a `.fsi` made bodiless by transparent abbreviations names none of them in
the diagnostic's type list.

### `TastPools.fs:320`, `:332`, `:354` — fault messages misattribute their entry point

All three `failwithf`s live inside the private `fill` (`:181`) and hardcode the prefix
`"TastPools.toPools: …"`. `fill` is shared by `toPools` (`:429`) and `rePool` (`:443`), so a
fault raised through the re-pool direction names the wrong entry point. The test suite already
shows the confusion: `test/…/TastPoolsTests.fs:580` is titled "a FunVerdicts key naming no
pooled lambda faults in toPools" while `:600` calls `rePool injected`. A `label` parameter on
`fill`, supplied by each entry point, fixes it.

### `TastWalk.fs:549` — a field named for a type it no longer has

`| TExpr.ExternalMember(r, k, n, isProp, ty, tok)` binds the field `storage: MemberStorage`
(`TastExpr.fs:270-276`) to a local called `isProp`, and threads that name on at `:559` and
`:567`. `MemberStorage` is `Field | Property | Method` (`SymbolKeys.fs:215-218`), not a bool —
a leftover from the widening. Rename to `storage`.

### `VesperLib.fs:1174-1185` — a tagged `extern` with members drops its intrinsic identity on a repr miss

The `ValueSome tag` branch looks up `ctx.IntrinsicReprs.TryGetValue short` and, on a miss, falls
to `| _ -> ()` (`:1185`) — recording nothing, so the type keeps the plain `Class` shape
`extractBodiedClassLike` wrote at `:1162`. Both sibling paths handle the same miss by calling
`registerIntrinsic ()`: the untagged arm (`:1172`) and the no-member arm (`:1191`). And
`registerIntrinsic` is written to tolerate a missing repr, via
`IntrinsicPlatform.Unsupported ctx.Target` (`:1147`).

So `isIntrinsic` is honoured on two of the three paths and dropped on the third. A deleted
comment asserted this was deliberate ("a target whose `.fs` omits the repr (JS) records nothing,
so the capability stays the plain canon-only interface `Class`") — but that is exactly the
agent-written prose this sweep treats as unverified, and it contradicts the sibling arms'
stated reason for existing. Needs a decision either way.

### `TypeInfos.fs:1125` — an intrinsic abbrev's `interface … with` block is silently accepted

`IntrinsicAbbrevInfo.InterfaceImpls` IS populated from source
(`Passes/NameResolution/MemberRegistration.fs:1125` assigns it exactly as the union and record
arms do), and the kind-agnostic host consumers iterate it
(`Passes/Unification/Infer.fs:220`, `Passes/Unification/InferControlFlow.fs:496`,
`Elaborate/Members.fs:222`, `Passes/NameResolution.fs:513`). So
`type X = (# "object" #) with interface IFoo with …` takes the nominal interface-impl path on a
`TyConst` host with no diagnostic. `requireInlineMembers` (`:1071`) already rejects `override`,
`new` and non-`inline` members and explicitly does nothing for `InterfaceImpl` — either
diagnose it there or handle it.

The doc claiming "`interface … with` blocks are out of scope for the intrinsic host; always
empty" has been deleted as false.

### `ExternalSymbolProviders.fs:467-471` — a capability interface's inherited args are not variance-mapped

`mapProviderTypes`'s `IntrinsicInterface` arm rebuilds only `Members`, while both siblings map
their interface list: `Class` at `:446` (`FrozenInterfaces = mapInterfaces info.FrozenInterfaces`)
and `Union` at `:453` (`mapInterfaces ifaces`). `IntrinsicInterfaceShape` does carry
`Interfaces: (string * FrozenType[])[]` (`ExternalSymbols.fs:498`), so a `transform` reaches
`IStructSeq<'T,'E>`'s type arguments through a `Class` but not through a capability.

This is exactly the surface the deleted "TOTAL over the value-flow surfaces — a bespoke
per-shape walk keeps missing one" claim asserted was covered.

### `TastLower.fs:149-150` — loop bound and array length come from different parameters

`for j in 0 .. instArr.Length - 1 do match holes.[j] with`, where `holes` is
`Array.create typarCount ValueNone`. `typarCount` and `instArr` are independent parameters of
`solvePhantomTypars` (`:110`, `:113`) with nothing tying them, so `instArr.Length > typarCount`
throws `IndexOutOfRangeException`. Not live — both callers pass a matching pair
(`EmitCall.fs:217`+`:237`, `ClrRecipes.fs:389`+`:391-395`) — which is also the argument for
dropping `typarCount` and using `instArr.Length`.

### `ReferencedProject.fs:418-433` — `runtimeModules` discards every error path silently

`| Error _ -> Map.empty` (a failed closure), `| Error _ -> ()` (an unparseable manifest) and a
missing `File.Exists abs` all produce "no runtime asset", indistinguishable from "the package
declares none". The backend then emits a program importing a `.mjs` that was never
materialised. Every sibling entry point — `buildProvider`, `composeContract`, `buildClosure` —
returns a `Result` or faults; this one is the outlier.

### `VesperLib/TypeTranslate.fs:57` — culture-sensitive `EndsWith` on a source identifier

`name.EndsWith "Attribute"` uses the culture-sensitive overload, while `ReferencedProject.fs:141`
uses `EndsWith(suffix, StringComparison.Ordinal)` for the same class of test. Same latent
inconsistency at `VesperLib/Manifest.fs:49` (`.EndsWith ".fsi"`) and `Elaborate/Strings.fs:53`
(`.StartsWith ":"`).

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

### `PrintfHoleForm.fs:273` vs `:334` — the forced-sign arm discards a left-align flag

`elif plusSign || spaceSign` is tested BEFORE `elif leftAlign && zeroPad && isFloatLike`, so
`%+-05.2f` lands in the forced-sign arm, reaches `FloatDecimal` at `:297` and yields
`ForcedSign(space, prec, 'f', zoPad = Some 5)` with `Alignment.None` (`:311`) — the `-` flag is
silently dropped. That contradicts the right-zero-pad rule the file states at `:209-210`.

Either decline `%+-0w.Nf` as the other unhandled sign+zero-pad combinations are declined, or
give it its own form. Check F#'s actual output with a `dotnet fsi` script before choosing —
do not settle it from memory.

Related: `:307` clamps a literal precision with `if n <= 0 then 0 else n` while the four
sibling arms (`:342`, `:403`, `:419`, `:434`) and `precDim` (`:237`) take `int pr` raw. A
digit-parsed `Literal` cannot be negative, so the clamp is inert but reads as though the others
are missing a guard.

### `SymbolKeyOps.fs:20` — `isEscapedName` does not test what its name says

`let private isEscapedName (name: string) = name.Contains '\`'` answers TRUE for an
already-mangled `` List`1 ``, not only for a backtick-escaped name. At `:174`,
`TyparArity = if isEscapedName name then 0 else arity` therefore keys an arity-suffixed name at
arity 0, where it can never compare equal to the parsed key. Also consumed at `:26` and `:183`.

Not currently triggered — every path into these reaches them with a bare source name — but the
predicate is a string test standing in for a distinction the type does not make.

### `Diagnostics.fs:372` — a parser-owned code is published under `DiagCode.Vesper`

`| Kind.Parse c -> DiagCode.Vesper(DiagnosticCode.code c)` forwards a code the parser layer
owns, while `DiagCode.Vesper`'s own doc (`:24-25`) defines that case as "this compiler's OWN
published families". Forwarding rather than renumbering is deliberate, but the DU has no case
for a code owned by another layer, so the string arrives under a label that misdescribes it.

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

### `SemanticInfo.fs` — `FTLocalTypar` and `FTUnknown` share a leaf space

"Bound by a local scheme" and "unexplained metavar" are distinguished only by a string. The
63-line essay in `Freeze.fs` was almost entirely about keeping those two apart by convention.

### `Hashing.fs` — `CompilationInputs`' completeness obligation is enforced by nothing

"A determinant the front end reads and this omits is a silent stale cache hit" was stated in
prose only. Nothing ties the record to the set of inputs the front end actually consults.

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

`obj`, `TyOr` and the numeric family are one "accept without unifying" rule, written out with
near-duplicate prose at each seam. A single `absorbs` predicate deletes four blocks. Where that
predicate's DATA comes from is codegen-js's call (it owns assignability); the factoring is not.

### `Passes/Unification/Engine.fs:62` — `DotSource.ClassChain`'s doc exists to explain a shape mismatch

The block says only why the chain case cannot be the `subst` + `lookup` pair that `Resolved`
carries. A member-lookup abstraction covering both shapes deletes it outright.

### `Passes/Unification.fs:63` — `TypeMembersFill`'s two bools have three legal combinations

`AllowAbstractSig` and `Generalise` are independent fields, and the fourth state is never
constructed. The three live call sites are class `(true, true)` (`:819-820`), interface-impl
`(false, false)` (`:741`, `:744`), union/record host `(false, true)` (`:846-847`). A three-case
DU deletes the `Generalise` field's three-line doc, which currently has to explain in prose that a
generalised `Equals` emits as arity-1 and no longer matches the arity-0 interface slot.

### `ConformanceTypars.fs:63-99` — "a checkable module binding" is three scattered guards

The 49-line header this sweep cut was standing in for a predicate the code never names:
`IsInline = false` (`:63-72`), a resolvable name (`:80-87`), and `sym.TyparArity > 0` (`:99`). An
active pattern yielding name + scheme + arity would put "non-inline, published, generic" in the
type and delete the surviving inline comments at `:64-66` and `:77-79`. `checkMembers` has the
same shape at `:161-172`.

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

### `RecordFieldClassifier.fs:11` — one verdict across two fields

`Exact: 'cand voption` plus `ExactCount: int` encode a single three-state result, which is why
both fields needed docs. `NoMatch | Unique of 'cand | Ambiguous of int` collapses it.

### `Anchor.fs:74` — `OriginFile.nowhere` is a sentinel, not a case

Distinguished by `BucketName = ""` / `Relative = ""`. The "no file is spelled `""`" invariant
is unchecked; `OriginPath` could be a DU, or the fields a non-empty-string type.

Deletes: "Distinguishable from every real origin, which names a path: no file is spelled `""`."

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

### `TEnumCases.fs:56`, `:70` — underlying-type names are spelled literally

`:70` returns the bare `"int"` where `IntWidth.name IntWidth.Int32` is the width→name
projection (`XParsec.FSharp/NumericLiterals.fs:63`), and `:56` spells `"string"` literally
while the adjacent `:57` goes through `RuntimeNames.objAbbrevName`. Two of the three names
bypass the constants. `NumericLiterals.fs:53-56` states that spelling a width name twice is how
the elaborator and the CLR backend come to disagree about an enum's underlying type.

### `Cache.fs:61` — `CodeVersion = 28` is a hand-bumped literal

The protocol ("bump when the wire format changes") is carried by nothing but the prose that
used to sit on it. A codec-shape hash, or a golden test that fails when the encoding moves,
would enforce it.

### `Anchor.fs:65` — `InputHash` does not encode WHICH text it hashes

The invariant "this is `hashString` of `Lexed.Input`" holds by convention at `Hashing.fs:48`;
`FrozenTypeTable.fs:504` builds an `InputHash` from hex with no such tie. A distinct type per
hashed thing would make the mismatch a compile error rather than a runtime fault in
`OriginSources.tokenAt`.

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

### `SemanticInfo.fs:57` vs `:143` — the canonical-set invariant is enforced on one side only

`TyOr of members: UnionMembers` wraps a private constructor, so a non-canonical value cannot
exist. The frozen twin `FTOr of members: EqSet<FrozenType>` takes a raw payload with no such
gate — which is what lets `FrozenTypeTable.fs:610-612` build an `FTOr` without routing through
`MkUnion`. Legitimate today, but canonicalisation on the frozen side is a convention rather
than an invariant.

## A rename that left a name trail

### `?free-typar` is minted nowhere, but six comments still name it

`Freeze.fs:47` mints `FTUnknown "?unresolved-typar"`. A repo-wide grep finds NO `src/` site
minting `"?free-typar"` — yet these comments still describe it as the value a leaked metavar
freezes to:

- `Elaborate/Typars.fs:119` — still ahead in the sweep
- `Passes/Unification/InferGeneralize.fs:323` — still ahead in the sweep
- ~~`Passes/Unification.fs:104`~~ — swept, deleted
- ~~`Passes/Unification/Engine.fs:863`~~ — swept, deleted
- `test/…/Codegen.Clr.Tests/StructTests.fs:593`
- `test/…/Codegen.Clr.Tests/InlineFreezeThawTests.fs:163` (reads as history, so arguably fine)

Separately, `test/…/Codegen.Js.Tests/FrozenCodecRoundTripTests.fs:273` constructs
`FTUnknown "?free-typar"` as round-trip fixture data — valid as bytes, but it names nothing the
compiler produces.

All four `src/` files are still ahead in this sweep; whoever takes them must know the string is
dead, because the comments read plausibly on their own. This is the "a migration leaves a name
trail" mode: the rename touched declarations and no comment.

### `Freeze.fs:47` — `FTUnknown` carries an unstructured string, so the leak is untyped

`"?unresolved-typar"` occurs exactly once in `src/` and `test/`, at the mint site; nothing
matches on it. `FTUnknown` holds a bare `string` (`FrozenTypeTable.fs:333`), so a leaked metavar
is indistinguishable downstream from any other unknown type. An over-wide type standing in as a
string key.

## Carry-forward for the remaining sweep batches

### `§n.n` citations point at a plan doc that is not in the repo

Two different things share the `§` sigil, and only one is legitimate:

- **Keep** — F# spec citations, which name an external published document:
  `Passes/NameResolution.fs:99`, `:198`, `:241` (`F# spec §8.7`),
  `Passes/Unification/InferControlFlow.fs:100` (`§14.6`).
- **Delete** — bare section numbers citing a design/plan artefact that exists nowhere in the
  repo: `ElaborateExpr.fs:444` (`§4.2`), `Passes/Unification/Infer.fs:147` (`§5.0`), `:324`
  (`§4.3`), `Passes/Unification/InferControlFlow.fs:363`, `:457` (`§4.4`), `:651`, `:653`,
  `Passes/Unification.fs:1202` (`§5.4`), `Passes/Validation.fs:214` (`§3.2/§9`).
  These are the milestone-label mode: state the fact the section number stands for, or cut.

### The `Diagnostic` alias note is cloned across three files

`AssemblyFiles.fs:27-28` and `Pipeline.fs:9-10` carry the same two lines — "the alias binds
`Diagnostic` to the SemanticAnalysis one throughout this module; see that type's declaration for
why the bare name would otherwise be the parser's" — and `Diagnostics.fs:560-562` already states
the collision at the type. Backward re-narration; delete both when those files are swept. (The
third copy, in `ConformancePass.fs`, is gone.)

A third form quotes a design doc by phrase rather than number (`Engine.fs:1152`,
`InferApp.fs:158`, `Subsume.fs:59`, `:236`, `:273`, `:283`) — same disposition, since the quoted
document is equally absent.

All of these files are still ahead in the sweep.

## A stale architecture doc

### `docs/core-lib-architecture.md:216-232` describes a manifest schema the code never had

It documents suffixed override keys (`impl-<t>`, `sig-only-<t>`, `files-<t>`, `runtime-<t>`)
with REPLACES-the-base-list semantics, plus resolvers `resolveInlineBodies`, `resolveExtraFiles`
and a `collectOverrides`. None of those three identifiers exists anywhere in `src/` or `test/`.
The real schema is `[targets.<t>]` tables (`ReferencedProject.fs:44-59`) and every target list
APPENDS (`:110-125`). Every line reference in that table (`:112-125`, `:130`, `:138`, `:147`,
`:156`, `:165`, `:409`) is also stale.

This is not a `*-plan.md`, so the plan-doc lifecycle will not retire it. Either correct it or
delete it — a doc describing the opposite merge semantics from the code is worse than none.

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

### `FrozenSignature.fs:75-84` — module containers are keyed by the COMPILED chain

`SymbolKeyOps.moduleFullName` gives a `[<CompilationRepresentation(ModuleSuffix)>]` or
name-collision module its `…Module` spelling, while the `.fsi` extractor keys the source path — so
a cross-file dotted name for a type in such a module does not resolve. An `InType`-nested type
contributes no module container at all (`:199-200`). The deleted prose hedged this; the honest form
is a failing test, and there is none.

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

### `TastPoolShapes.fs:244` — `introducedBoundVar` takes a stringly-typed caller tag

`site: string` exists only to interpolate the caller's name into a `failwithf`, and both call
sites pass a literal equal to their enclosing function: `introducedBoundVar "exprPayload"` inside
`exprPayload` (`:282`) and `introducedBoundVar "patPayload"` inside `patPayload` (`:372`). It goes
stale silently on rename. Same shape as `FrozenTypeBridge.fs:131`.

### `SemTypeWalks.fs:280` — `mapChildren`'s `TyOr` arm defeats the sharing the others preserve

Every other arm uses `EqArray.mapPreserve` / `refEq` and returns `t` itself when nothing
changed. `| TyOr members -> members.Map f` routes through `UnionMembers.Map` → `SemType.MkUnion`,
which always builds a fresh `EqSet`, so a ground `TyOr` re-allocates on every `zonk` /
`substitute` walk. The doc has been qualified to match the code rather than assert the
invariant the code does not hold.

### `TastExpr.fs:210` — `Range.step` is the sole reference `option` in the TAST

Every other optional child is a struct `voption` — `ExternalMember.objArg` (`:271`),
`FormatSegG.DynHole.Width`/`Precision`, `ExprRow.VarBoundVar`. This one forces `Some`/`None`
handling and is the only `Option.iter` in either walker (`TastWalk.fs:422-438`, `:830`).

### `TastPoolBuilder.fs:238`, `:249` — the boundVar space has no `readBoundVar` resolver

Both sites open-code the `i < b.BoundVarBase` layer test and its two branches, while the expr,
pat and decl spaces route through `readExpr` / `readPat` / `readDecl` (`:129`, `:140`, `:151`).
Two sites is small, but it is the one part of the stacking invariant written more than once.

### `TastAccessor.fs:457-477` — two recognizers over one payload case

`(|EInlineCall|_|)` and `(|EInlineCallOrigin|_|)` both destructure `ExprPayload.InlineCall`, so a
consumer needing both pays two payload fetches and two matches —
`Codegen.Common/InlineExpand.fs:274` and `:280` do exactly that on one node. `New` and
`ILIntrinsic` solve this with a single recognizer returning a whole view; an `InlineCallView`
would make it consistent.

### `TastAccessor.fs:282`, `:733` — a length mismatch surfaces as a raw `ArgumentException`

`(|ERecordCons|_|)` and `(|PRecord|_|)` use `Array.map2` over the label array and the child
array, so pool corruption reports as an FSharp.Core `ArgumentException` rather than a message
naming the node.

### `FrozenTypeBridge.fs:201` — `pickInterfaceWitness` allocates an `option` to return a `voption`

`Seq.tryPick` builds an `option` per call which is then matched into `ValueSome`/`ValueNone`,
on a path whose whole signature is `voption`.

### `Passes/Unification/InferApp.fs:118` — one error path reports without minting an error type

`tryAdmitLiteralConstArg` calls `ctx.Report` and then returns `true` (= handled), so the caller
unifies nothing and the argument's type var is left FREE. Every other error path in the file goes
through `errorTy`, which mints a concrete error type. Whether the free var is deliberate is stated
nowhere.

### `Passes/InlineExpansion.fs:398-412` — a re-entry branch that may be unreachable

`outlineNullaryIntrinsic` is reached only through `expandingTemplate` with `Args = []`, and an
operand-less intrinsic names nothing, so the `Descent.reentered` answer for this path looks
unreachable. If that holds, the `expandingTemplate` wrapper at `:409` is pure overhead and its
`ValueSome reentered` branch is dead. Confirm before anyone relies on it.

### `Passes/Unification/Translate.fs:286` — `Type.SuffixedType` with a dotted name has no arm

`int A.T` falls to the `_ ->` catch-all and yields a free `TyVar` with no diagnostic, so
unmodelled syntax is indistinguishable from an unresolvable type name for the user.

### `Passes/Unification/Translate.fs:475` — `buildExternalTy` returns `option` in a `voption` file

`tryExternalTypeOfKey` (`:490-497`) exists partly to convert it back. Cosmetic wart to excise.

### `Passes/NameResolution/Scope.fs:585` — the operator-form long-ident catch-all diagnoses rather than resolves

`Expr.LongIdentOrOp` reports `OperatorFormQualifiedName` unconditionally for any operator-form
long ident that is neither symbolic nor qualified, carrying an explicit `TODO`. Active-pattern
and nil op-names used as values are rejected rather than resolved.

### `Passes/Unification/Engine.fs:652` and `:675` — one scrutinee matched twice per constraint

Two consecutive `match c.Kind with | SemanticConstraintKind.Coercion target -> … | _ -> ()`
blocks sit in the same `for c in cs` iteration, and both open with
`subtypeNominalOf ctx (zonk ctx.Store target)` on the same `target` (`:654`, `:678`). Every
coercion constraint therefore pays two zonks and two nominal walks per discharge. One match with
two guarded sub-arms.

### `Passes/Unification/Engine.fs:680` — one predicate, two spellings of its key

`funSlotArityOfArgs` is fed `SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName tname)` here, but
`bareName (typeMetaName tk)` at `Engine.fs:31` and `Subsume.fs:295`. The two agree only because
`qualifiedName` delegates to `typeMetaName` for `SymbolKey.Type` (`SymbolKeyOps.fs:325`) — it
takes different branches for `Binding` and `Member`. Three call sites of one predicate should
derive the name once.

### `Passes/Unification/EngineCore.fs:88` — `tupleOrSingle` takes a `PassContext` to read one field

It needs only `ctx.Intrinsics.Unit`, while its siblings in the module take a bare `TypeStore`.
That forces `PassContext` onto callers that otherwise need only the store.

### `ExternalSymbolProviders.fs:217`, `:233` — over-wide parameter type

`mergeReverseCanon` / `mergeForwardRepr` take `IExternalSymbolProvider seq` but read only
`IntrinsicReverseCanon` / `IntrinsicForwardRepr`. The narrower sink is the intrinsic axes
themselves.

### `ExternalSymbolProviders.fs` — `KeyIndexedLeaf` cannot publish index signatures

The record has no index-signature channel, so `ofKeyIndexes` builds its `Named` from
`NamedLeaf.empty` without overriding `TryLookupIndexSignature`, and `ofKeyedLeaf`'s store view
then answers every index-signature query from that constant `fun _ -> []`. A producer that
acquires index signatures and holds `InModule` keys has no way to publish them and gets no
compile error. The fix is a channel on `KeyIndexedLeaf`, not a comment.

### `CstWalk.fs:988` — `walkModuleTreeWith`'s `onScope` is unused at 4 of 5 call sites

`CstWalk.fs:1151`, `Elaborate.fs:273`, `Passes/NameResolution.fs:802` and
`Passes/Unification.fs:1270` all pass the literal `(fun _ _ -> ())`; only
`Passes/Validation.fs:238` supplies a real hook. The `walkModuleTree` wrapper that exists to
hide the parameter has two callers (`VesperLib.fs:1848`, `test/…/OpenScopeTests.fs:30`).

### `FrozenTypeBridge.fs:131` — `localTyparInTemplate` takes a hand-written site string

The `site: string` parameter exists only to interpolate into that function's own `failwithf`,
and all seven callers pass a module-qualified literal — `"ExternalSymbols.openSignature"`,
`"FrozenTypeBridge.instantiateDeclaring"`, `"OpenSignature.ofSymbol"`, and one in
`test/…/InlineFreezeThawTests.fs:222`. These rot on rename exactly as the comment cross-refs
this sweep is deleting do, and nothing checks them.

### `Passes/Unification/EngineCore.fs:321` and `:354` — the two capability-key chains are one function

`capabilityCanonKey` and `capabilityPlatformKey` are structurally identical five-way match chains
over `caps.Enumerable / Enumerator / Disposable / Equatable / Comparable`, differing only in the
body of the local `pick`: `ValueOption.defaultValue c.Key c.CanonKey` versus `c.Key`. ~30
duplicated lines. One chain parameterised by the key selector removes a copy — subject to the
allocation-free requirement the surviving comment states.

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

### `PrintfSpec.fs:48` and `:241` — the last-dotted-segment derivation is written twice

`let dot = name.LastIndexOf '.'` and its follow-on are byte-identical in `sinkOf` (`:46`) and
`tryFamily` (`:239`), both keyed on the same eight short names.

Relatedly, `sinkOf` (a match, 8 arms) and `families` (a `Map`, 8 entries at `:224`) are two
parallel tables over one key set, maintained separately: adding a family needs both edited and
nothing ties them together.

### `AttributeDecode.fs:69` and `:97` — the attribute-set traversal is duplicated verbatim

The same nested `for AttributeSet(attributes = entries) … for Attribute(construction = …)`
walk with the same `ObjectConstruction` / `InterfaceConstruction` unwrap appears twice,
differing only in what the inner match does. One `foldAttributeShortNames` would carry both.

### `Tast.fs:238` — the `Frozen` alias module is missing two ForIn aliases

It defines `ForInEnumerator` only, so `FrozenCodecDecls.fs:131,142,152,164` spell
`ForInGetEnumG<FrozenType>` / `ForInEnumMembersG<FrozenType>` longhand, against the
alias-family convention `SideTypes.fs:106-108` follows on the `SemType` side. Two additive lines.

### `Tast.fs:168` — a `failwithf` interpolates a whole declaration tree

`failwithf "…: %A" i other` formats an entire `TDeclG` into the exception message. Narrowing it
to the case name is cheap.

### `SymbolKeys.fs:175` — length test written as an `if`

`if this.Path.Length = 0 then` is the shape the repo prefers as
`match this.Path.Length with | 0 -> … | _ -> …`.

### `TastPoolBuilder.fs:344`, `:263` — accessors that rebuild per call

`moduleMembers` calls `DenseTable.index` on every invocation, building a fresh dictionary from
`b.Base.ModuleMembers`; `roots` does an `Array.copy` per call. The single production caller
(`Codegen.Js/EmitJs.fs:852`) binds `moduleMembers` once, so this is latent rather than live —
but the accessor shape invites a per-decl call.

### `SymbolKeys.fs:19`, `:219` — two unannotated members among annotated siblings

`.Name` and `IsValueMember` carry no return-type annotation; every other member in the file
(`.Dotted`, `.Depth`, `.Namespace`, `.Offset`, `.Written`, `.AssemblyOption`) does.

### `SemanticInfo.fs:224`, `:226` — inconsistent static-member casing

`MeasureTerm.Empty` / `MeasureTerm.ofList` mix PascalCase and camelCase on one type, while the
neighbouring `UnionMembers.OfSeq` and `SemType.MkUnion` (`:191`) are PascalCase throughout.

### `Passes/InlineReduction.fs:222`, `:305`, `:306` — three failures name the wrong module

`resolveAt` and `classifyApplication` raise with messages prefixed `"InlineExpansion: …"`
although both live in `InlineReduction`. A user seeing "InlineExpansion: over-application of an
inline function" is pointed at the sibling file, which contains neither the peel nor the
`TDecl.Let` test that raised.

### `Passes/InlineReduction.fs:305` — a `failwithf` interpolates a whole pattern tree

`failwithf "…: %A" param` formats an entire `TPat` into the exception message; the case name
would carry the same information. Same shape as the `Tast.fs:168` item above.

### `Passes/InlineReduction.fs:113` — `Descent.top` had a false consumer claim

Its doc said `top` is "what [the walk] returns to for every call-site argument it walks". It is
not: arguments are walked under `inFlight.Caller` (`InlineExpansion.fs:270`, `:274`), the
enclosing descent, and `Descent.top` has exactly one use — `InlineExpansion.fs:487`, the
compiling file's own declarations. The clause has been cut; noted here because the same
"returns to `top`" reading would be a real bug if anyone acted on it.

### `Passes/InlineReduction.fs:167` — `ExternalFunction.Args = ValueNone` is documented as unreachable-by-shape, but is a `failwithf`

The field doc claimed a non-opening argument means "the function cannot be a template". The consumer
disagrees: `InlineExpansion.fs:191-194` matches `ValueSome served, ValueNone` and raises
"the spliced member … was applied to an argument its declared parameters cannot be bound to".
Encoding the two outcomes in the type (a member that opened vs. one that did not) would
remove the pairwise match over `lookupExternal x.Ctx x.Specs ext.Key, ext.Args` and the failure
arm with it.

### `Passes/Unification/Subsume.fs:213` — the `TyFun` ↔ `Fun<…>` recognizer is spelled twice

The guard `funSlotArityOfArgs (SymbolKeyOps.bareName …) targs.Length |> Option.isSome` followed by
`peelFunDomains ctx.Store (targs.Length - 1) a b` appears here and at `Engine.fs:678-687`, once as
a read-only check and once as the grounding `unify`. The two differ only in what they do per peeled
type — and in how they get the name (`typeMetaName` off a `TypeKey` vs `qualifiedName` off a
`SymbolKey`), which is the kind of difference that goes stale silently. One `tryFunSlotPeel`
returning the aligned `k+1` types would carry both.

### `Passes/Unification/Subsume.fs:132`, `:142` — the carrier-node triple is enumerated at six sites

`TyKeyOf | TyIndexedAccess | TyConditional` is matched as a group in `isGroundEval`,
`hasCarriedNode` and twice in `tryFoldCarried`, and again at `Regions.fs:151`,
`InferOverload.fs:58-59`, `EngineCore.fs:581` and `Engine.fs:506`. A `SemType.isCarrier`
recognizer (or an active pattern) would give the concept one name; adding a fourth type-level
form currently means finding all of them.

### `Passes/Unification/InferGeneralize.fs:23` — `instantiate` keeps four parallel maps over one root set

`subst`, `freshOf`, `constraintSubst` (seeded from `subst`) and `quantifiedRoots` (seeded from
`freshOf.Keys`) are four structures indexed by the same quantified-root ids, differing only in
which freshening policy each expresses. The 15 lines of prose cut from `:34` and the 12 from `:42`
existed entirely to explain how they relate — a single "instantiation" record carrying the root,
its fresh var and whether it was quantified would delete both blocks rather than shorten them.

### `Passes/Unification/InferGeneralize.fs:115` — `collect` is `let rec` but never recurses

Inside `applyDefaults`, `let rec collect (t: SemType)` only calls its own inner `let rec go`; the
`rec` on the outer binding is inert. `visited` is also hoisted outside a function that is called
exactly once.

### `Passes/Unification/InferGeneralize.fs:200` — `tryListLiteralElem` linear-scans on every root

It walks all of `ctx.ListLiterals` per call, and `prepareListLiterals` calls it once per unlinked
TyVar root reached from the binding's type, so the pair is O(roots x literals) per generalisation.
It also scans the whole list after a hit instead of stopping (`if result.IsNone && …`).

### `Passes/Unification/InferGeneralize.fs:213` — count test written as an `if`

`if ctx.ListLiterals.Count = 0 then () else …` is the shape the repo prefers as
`match ctx.ListLiterals.Count with | 0 -> () | _ -> …`.

### `Inline.fs:35` — `quantifiedTypars` order disagrees with the canonical typar order

`quantifiedTypars` is a bare `collectLinkedRoots` walk, so it returns typars in
first-left-to-right-appearance order only. The order a module-`let`'s scheme is actually
quantified in is `GeneralizedTypars.canonical` (via `Elaborate/Typars.fs:71 mkMethodQuantEnv`),
which puts EXPLICITLY-DECLARED typars first in source order and only then the inferred roots by
appearance. For `let inline f<'b, 'a> (x: 'a) (y: 'b) = …` the two disagree, so any `typeArgs`
array built against the frozen/ABI typar index is applied to the wrong roots by `inlineExpand`.
The doc that claimed the two orders match ("This reproduces the order `Unification.generalise`
collects them in") was deleted in the comment sweep; the divergence itself is untested.

### `Inline.fs:247`, `:327` — the `(TExpr * SemType * SyntaxToken)` argument triple wants a record

`betaReduce` and `deriveInlineTypeArgs` both take `(TExpr * SemType * SyntaxToken) list` and both
destructure it positionally (`(arg, _, appTok)`, `[ for (a, _, _) in args -> … ]`). The middle
component is the applying `App` node's RESULT type, which `betaReduce` ignores entirely and
`deriveInlineTypeArgs` reads only from the LAST element — a fact that currently only exists as
prose. A named record (`Arg` / `AppResultTy` / `AppTok`) removes both the wide tuple and the
comment.

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

### `Passes/InlineSpecTable.fs:69` — `Outlining`'s three `Edge*` fields want to be one record

`EdgeTok`, `EdgeOrigin` and `EdgeTy` are declared together, documented by one shared comment
block, and consumed together at exactly one site (`:302`, building the `TExpr.InlineCall`). An
`Edge = { Tok; Origin; Ty }` record makes "these three are the CALL SITE's, never read off the
entry" structural instead of prose, and shrinks a 9-field record carrying two thunks.

### `Passes/InlineSpecTable.fs:148` — DFS colouring is three untyped `int`s

`let unvisited, onPath, finished = 0, 1, 2` with an `int[]` state array. The middle state is
load-bearing (an edge into `onPath` is a cycle, an edge into `finished` is legal sharing), and a
three-case DU makes the `state.[j] = onPath` / `= unvisited` tests exhaustive instead of
comparisons against magic numbers.

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

### `Passes/NameResolution/MemberRegistration.fs:1009` — the struct-field cycle walk keeps scanning after it finds the cycle

`checkGroupStructFieldCycles`'s inner `walk` sets `cyclic <- true` and then relies on the guard
`| ValueSome fieldKey when not cyclic ->` to make the remaining iterations no-ops: the `for
fieldTy in inlineFieldTypes ctx id` loop still runs to completion at every frame of the recursion
(and `inlineFieldTypes` re-reads the registry to build each sequence). Correct, but the same shape
already recorded for `tryListLiteralElem` — the loop wants to stop at the hit.

### `Passes/NameResolution/TypeRegistration.fs:815` — the intrinsic-vs-alias verdict is decided twice

`tryDeclaredTypeName` (`:212`) matches the abbreviation RHS against `Type.ILIntrinsic` to choose
`TypeDeclKind.IntrinsicRepr` over `TypeDeclKind.Abbreviation`, and that kind rides the claim onto
`TypeIdentity.Kind`. `registerAbbreviationDefn` is then handed that `id` and re-matches the very
same `rhs` (`:815`) to choose which side table to write, never reading `id.Kind`. One
classification, two spellings of it, and nothing makes the second agree with the first.

### `Passes/NameResolution/TypeRegistration.fs:772` — `ilIntrinsicString` exists three times

The `StringBuilder`-over-`StringPart` loop that stitches `(# "System.Int32" #)` into
`"System.Int32"` is written identically in three places: `ilIntrinsicString` here,
`Conformance.fs:121 ilReprString`, and `VesperLib.fs:1490 ilIntrinsicReprString`. All three carry
the same seven-case `StringPart` match, the same `| StringPart.Expr _ -> ()` drop, and (before the
comment sweep) the same doc line. They differ only in how a token becomes text — `ctx.NameOf` vs
`nameOfTok lexed` — so one function over a `SyntaxToken -> string` would serve all three.

### `Passes/NameResolution/TypeRegistration.fs:817` — `IntrinsicReprTypes` and `IntrinsicReprKeys` are one fact in two tables

Two consecutive statements write the same `repr` string into `IntrinsicReprTypes` (keyed by bare
NAME) and into `IntrinsicReprKeys` (keyed by the arity-qualified `SymbolKey`, as an
`IntrinsicReprInfo` that also carries `Heritable`). Nothing structural keeps them in step, and the
name-keyed side cannot distinguish two intrinsics that differ only in arity — `IntrinsicKeys` and
`IntrinsicAbbrevHost` are name-keyed the same way. Not observed to break anything (a namesake pair
of intrinsics at different arities may simply not occur in practice), but the name axis looks
redundant given every consumer downstream already holds a resolved key.

### `Passes/NameResolution/TypeRegistration.fs:580`, `:723` — the index-bucket prepend is copied verbatim

`registerRecordTypeDefn`'s `FieldIndex` fill (`:578-588`) and `registerUnionTypeDefn`'s `CtorIndex`
fill (`:721-731`) are the same eleven lines twice: `TryGetValue`, a `ResizeArray(infos.Length + 1)`
seeded with the new entry, a copy loop, `EqArray.ofResizeArray`, and a `false, _` arm building an
`EqArray.singleton`. Only the dictionary and the element type differ. A
`prependToIndex (index: Dictionary<string, EqArray<'T>>) (name: string) (v: 'T)` helper removes
both, and gives the "newest declaration wins the slot" ordering one place to live.

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

### `Passes/NameResolution.fs:204` — the class preamble deliberately cannot name the object

The instance-preamble scope (`:204-208`) holds the ctor params, the static boundVars and the instance
boundVars above it, but never `this` / `base` / the type-level `as` alias, so an F# program that
writes `type T() as self = let x = self.M()` gets "Unresolved identifier" here where fsc compiles it
(and throws at run time on initialisation-soundness grounds). That divergence was recorded only in a
comment; a named failing test would be the honest form, since nothing else states it.

### `Elaborate/Resolve.fs:174` — four active patterns differ only by the `ClassMemberKind` tested

`(|ClassTailMethod|_|)` (`:174`), `(|ClassTailProperty|_|)` (`:181`), `(|StaticMethod|_|)` (`:188`)
and `(|StaticMember|_|)` (`:195`) are the same four lines each: call the `try*` resolver, filter on
`m.Kind`, and re-project the LongIdent's last segment through `ctx.NameOf`. `StaticMember` is
`StaticMethod` with the filter dropped. Two helpers parameterised by the wanted `ClassMemberKind`
would collapse all four, and would put the `li.Idents.[li.Idents.Length - 1]` re-read in one place.

### `Elaborate/Resolve.fs:90` — `tryLongIdentClassTail` zonks the same `TyVar` twice

The object-argument type is computed as `Unification.zonk ctx.Store (TyVar tv)` in the match scrutinee
(`:90`) and again, identically, when building the result (`:98`). Binding the first result and
matching on it returns the same value with one traversal, and removes the possibility of the two
reads drifting if the second is ever edited.

### `Passes/Regions.fs:6` — the pass contract lives in a `Pre:`/`Post:` prose header

`run` is `PassContext -> EqArray<TDecl> -> EqArray<TSpecialization> -> unit`, so which side tables it
requires (`Bindings.Binding`, `Bindings.TypeVar`) and which it fills (`Bindings.Escape`,
`Bindings.Repr`, `Store.Region`) is stated only in the file header — a 27-line block, now 3, and the
same shape recurs in `Validation.fs`, `Desugar.fs` and `RefCellPromotion.fs`. Nothing checks it:
running `closureReprSnapshot` before `run` silently yields an empty map rather than an error. An
explicit input record (the tables read) and returned output record (the tables written), threaded by
`Pipeline`, would delete four prose headers and turn the ordering into a compile error.

### `Passes/Validation.fs:56`, `:93` — the record-field mutability check is written twice

`checkAssignment`'s multi-segment `LongIdent` arm (`:56-91`) and its `DotLookup` arm (`:93-112`) run
the same five steps against different object arguments: look the object argument's `TypeVar` up, `Unification.zonk`
it, require `TyRecord(recKey, _)`, `TypeRegistry.tryRecordByKey` + `Array.tryFind` on `Name`, and
report `Kind.ImmutableFieldAssignment` when `not field.IsMutable`. Only how the object-argument key and the
field token are obtained differs (`li.Idents.[0]`/`[1]` versus `CstKeys.ofExpr r`/`li.Idents.[0]`). A
`checkFieldIsMutable (ctx) (objArgKey: NodeKey) (fieldTok: SyntaxToken)` helper collapses both, and
gives the deferred deeper-chain case (`r.A.X <- v`) one place to grow into.

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

### `Elaborate/ObjArgs.fs:169` — `unionCaseFieldTys` returns `[]` for an external union, so `obj` case fields are never boxed

Its sibling `recordFieldTy` (`:148`) grew an external arm that reads provider field shapes and
instantiates them at the object argument's args, precisely so `wrapObjArg` boxes an `obj`-typed field
of a cross-file record. `unionCaseFieldTys` immediately below still has only a `LocalUnion` arm
and a `| _ -> []`, so a value flowing into an explicitly `obj`-typed field of an EXTERNAL union
case gets no box. Since inference coerces into such a slot (`InferCtor`'s `unifyArg`), this is
the same invalid-IL shape the record arm was added to fix. Found because
`InferResolve.recordConstructionOf` carried a doc claiming BOTH were still `LocalRecord`-only;
the record half of that claim is false and has been deleted.

### `InferRecordAccess.fs:433` — three copies of the "resolve a core intrinsic and unify its scheme" body

`getArrayIndex` (`:433`), `getStringIndex` (`:453`) and the `GetIndex` arm of `tryIndexSignature`
(`:550-562`) are the same eight lines three times: match a `ctx.CoreAccess.Value.*` accessor,
`IntrinsicKey.Set(node.Key, SymbolKey.Binding sym.Key)`, mint a fresh result var, `unify` the
`instantiateSymbol`'d scheme against `TyFun(objArgTy, TyFun(idxTy, resultTy))`, return the var.
Only the accessor and the `IntrinsicNotInScope` string differ. A helper taking the accessor and
its diagnostic name removes two copies; `resolveFieldStep`'s `GetArrayLength` arm (`:386`) is a
fourth instance differing only in arity (`TyFun(rTy, resultTy)`).

### `InferRecordAccess.fs:211` — the `ExternalAccess` payload literal is built at four sites

The same six-field record (`Key = SymbolKey.Member m.Key`, `IsStatic`, `Storage`, `Signature`,
`OptionalDefaults`) is written out at `commitExternalMember` (`:211`), the `IntrinsicBclMember`
arm (`:366`), `resolveExternalIndexer` (`:482`) and `InferResolve.inferExternalStaticMember`
(`:395`). Three of the four derive every field from the same `ExternalMember m`, differing only
in which signature function they call (`instantiateSignature` vs `openSignature`) and in the
node key stamped; the indexer copy hard-codes `Storage = MemberStorage.Method` and
`OptionalDefaults = []`. A constructor taking `(m, signature)` would make the two overrides
explicit instead of leaving a reader to diff four literals.

### `InferRecordAccess.fs:629` — `inferLongIdentPrefix` differs from `inferLongIdentFieldChain` by one loop bound

Both (`:610` and `:629`) read `li.Idents.[0]`, look the anchor up in `ctx.Bindings.Binding`, fall
back to a fresh var, and fold `resolveFieldStep` across the remaining segments. The only
difference is `for i = 1 to li.Idents.Length - 1` versus `- 2`. One function taking the number
of trailing segments to leave unresolved (0 or 1) collapses them, and makes the "stops one
short so the last segment can be resolved arg-aware" fact a parameter rather than a doc line.

### `InferControlFlow.fs:118` — a ref-struct enumerator with a pattern `Dispose()` is silently never disposed

Both enumerator probes decide disposability by scanning for `System.IDisposable`, and the
`Pattern` descriptor carries the verdict as a bare bool, so it cannot name WHICH `Dispose` to
call. A `[<IsByRefLike>]` enumerator cannot be boxed to `IDisposable`, so a ref struct exposing
a public `Dispose()` gets no `finally` at all — a silent resource leak rather than an error.
The `use`-boundVar path already prefers a type's own `Dispose` before the interface slot, so the
precedent exists; the blocker is that `SemType` has no byref-like predicate to test with, and
that the descriptor's `dispose` field would have to become a member reference. Recorded from a
14-line TODO cut to one line by the comment sweep.

### `InferTypeOps.fs:43` — explicit type arguments of the wrong arity are silently discarded

`inferTypeApp` only unifies the supplied arguments when `freshArgs.Length = List.length
explicit`; every other case falls to `| _ -> ()`, which is also the arm a bare generic function
legitimately takes. So `ResizeArray<int, string>()` — right type constructor, wrong arity — types
exactly as `ResizeArray<_>()` with no diagnostic, and the mistake surfaces later as an
unresolved metavariable or not at all. Distinguishing "no nominal result" (intentionally a
no-op) from "nominal result of a different arity" (a user error) needs the two conditions
split; I did not check whether a later pass catches the arity independently.

### `InferTypeOps.fs:187` — the type test does not strip a reference-`null` source, unlike the downcast

`inferDynamicDowncast` runs `stripReferenceNull` over the source before the `subsumes` check,
on the stated grounds that a `T | null` coerces exactly as `T` does. `inferDynamicTypeTest`
directly above performs the mirror-image check on the unstripped `srcTy`, so a `:?` on a
nullable-reference source appears to reach `subsumes` as a union and risks a spurious
`UnrelatedTypeTest`. The two arms should agree unless the type test is deliberately stricter;
I did not construct a failing case, so treat this as an asymmetry to confirm rather than a
proven bug.

### `InferTypeOps.fs:82` — static-optimization clause bodies are never checked against the declared result

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

### `Elaborate/Access.fs:91` — `fieldStep` demands a `chainKey` its assignment caller can never use

`translateAssignment`'s LongIdent arm mints `liKey` purely to satisfy `fieldStep`'s signature:
the key exists only so the `TyArray _ when segName = "Length"` arm can read
`ctx.Resolution.IntrinsicKey`, and a read-only `.Length` can never be an intermediate segment of
an assignment OBJECT ARGUMENT, so the value is dead on this path. Splitting the array-length arm out of
`fieldStep`, or taking the resolved intrinsic key as a `voption` the caller supplies only when it
has one, removes the unusable parameter and the three lines now explaining it.

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

`TExpr.TraitCall` carries one support type, so `translateStaticMemberInvocation` takes
`args.[0]`'s type and the `(^T1 or ^T2)` support set is never searched on the right. A member
declared only on the right operand — the `int * Vector -> Vector` scalar-prefix multiply shape —
therefore does not resolve, and the failure surfaces at inline expansion as a declined trait
rather than at the invocation. Carrying a candidate set on the node is what would buy it. This
is a genuine semantic gap rather than a comment defect; the note claiming it was the surviving
half of a twelve-line doc, so it is recorded here before being shortened.

### `Elaborate/ClassMembers.fs:252` — a secondary ctor's `go` silently discards the non-chain half of every non-`LetIn` form

`AdditionalConstrExpr` carries `SequenceAfter(stmt, _, rest)`, `SequenceBefore(before, _, expr)`
and `Conditional(_, cond, _, thenBranch, _, elseBranch)`, and `go` recurses into `rest`,
`before` and `thenBranch` respectively. So a leading `do` statement, F#'s post-construction
`then <expr>` block, and BOTH the condition and the else branch of a conditional chain call are
dropped without a diagnostic — `new(x) = if c then C(x) else C(0)` always emits the `then`
arguments. I did not check whether a consumer treats the truncation as intended; the code
carries no diagnostic for it, and the doc line now states the drop rather than excusing it.

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
parameterised by a leaf handler would make the parallelism mechanical instead of an instruction
to the reader; a new `SemType` case added to `unify` alone currently degrades overload filtering
silently to `| _ -> false`. The sweep cut the twenty-one-line header to three, so the invitation
to diff is gone but the duplication is not.

### `Passes/Unification/InferExternalCall.fs:406` — `localHost` returns a four-wide anonymous struct tuple

`localHost` yields `struct (TypeKey * EqArray<string * TyVarId> * EqArray<SemType> *
TypeMemberInfo[])`, and its single consumer immediately destructures it into `declKey`,
`typeParams`, `args`, `members`. Nothing but position distinguishes the two `EqArray`s at the
call site, and the three arms building it (`TyClass` / `TyUnion` / `TyRecord`) repeat the same
four-field projection off three different `*Info` records. A named record — or reusing whatever
common shape those three `Info` types already share — makes the positions checkable and deletes
the comment that had to spell the tuple out (cut to one line by the comment sweep, so the debt
is now invisible).

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

### `Passes/Unification/InferIdentExpr.fs:96` — the static-member lookup and its class-then-union cascade are written twice

`inferIdent`'s `tryStaticMember` (`:96`) and `tryLocalTypeAppStaticMember`'s `resolve` (`:254`)
are the same four lines — `Array.tryFind (fun m -> m.IsStatic && m.Name = …)`, then
`freshNamedInstance` + `substituteWith` — differing only in which local holds the member name.
Each is then driven by an identical `TypeRegistry.tryClass … | ValueNone -> tryUnionBare …`
cascade over the same use site. One helper taking the name and the use site collapses both, and
would remove the second site's need to explain that it "mirrors" the first (that cross-reference
deleted by the comment sweep).

### `Pipeline.fs:138` — closure verdicts reach codegen only by a hand-written side-table snapshot

`analyseSemWithContextForCore` copies two `PassContext` side tables onto the `TastFile` by hand
(`ClosureReprs` from `Regions.closureReprSnapshot`, `FunVerdicts` from `ctx.FunVerdicts`), because
codegen holds no `PassContext`. Nothing types the requirement: a third verdict table added to
`PassContext` compiles and reaches codegen as a silent default. The prose that explained this ran
to eleven lines over three blocks and was cut to two by the comment sweep, so the debt is now
invisible. Note also that those deleted comments were wrong about both tables — they called
`FunVerdicts` "node-keyed" (it is `Map<LambdaKey, FunVerdict>`) and attributed the decision to
`inferApp` (it is `recordFunArityVerdicts`), which is the kind of drift a snapshot step that the
type system does not name will keep producing.

### `Pipeline.fs:115` — `selfHostList: bool` is a flag parameter threaded through five wrappers

`analyseSemWithContextForCore` takes a bare `bool` whose only use is `ctx.DefaultListIsVesper <-
selfHostList`, and the five public entry points differ from each other in nothing but that
literal `true`/`false` plus whether they freeze and whether they keep the context. The result is
that "which list representation does a bare `[]` default to" is answered by reading a call site's
positional `true`. A two-case type named for the choice (FSharp.Core list vs Vesper cons-list)
would make the entry points readable without their docs and would delete the sentence each of
those five docs currently spends restating it.

### `AssemblyFiles.fs:101` — the cross-file "same assembly" waiver is an untyped string round-trip

`analyseAssemblyWith` stamps each file's provider view through `fileSource assemblyName path`,
which lands `assemblyName` in `OriginPath.BucketName`; the duplicate-type check then reads it
back out via `Origin.AssemblyOption` and compares it to `ctx.AssemblyName` as a plain string.
Nothing connects the two ends, so a driver passing a differently-spelled assembly name to
`analyse` than to `fileSource` turns every prior file's export into a spurious "already exists in
the referenced assembly" error. The deleted header comment got this wrong in a way that shows the
coupling is not readable: it claimed the view stamps `Origin.InAssembly`, when the code stamps
`Origin.InFile` and relies on the bucket to carry the assembly.

### `StringLiterals.fs:22` — `decodeEscape` throws on escapes the string lexer accepts

`decodeEscape` handles the escape set of the lexer's `pCharChar` (`Lexing.fs:1206`) — `"`, `\`,
`'`, `n`, `t`, `b`, `r`, `a`, `f`, `v`, `\uXXXX`, `\xHH`, `\DDD` — and every other shape hits its
`failwithf`. But its actual feed is `foldStringParts`, whose tokens come from
`pStringEscapeToken` (`Lexing.fs:1347`), which is strictly more permissive: it emits
`Token.EscapeSequence` for `\UXXXXXXXX`, for a `\u`/`\x` truncated at end-of-input (it falls back
to `SkipN 2`), for a lone trailing backslash (a ONE-char token, so `inner.[1]` is an index error),
and for any unknown escape at all under its `// Unknown escape` arm. So `let s = "\U0001F600"`
and `let s = "\q"` both lex clean and then crash the Elaborate pass with a `failwithf` rather than
a diagnostic. The trigraph arm diverges more quietly: `pCharChar` flags a value over 255 as
`CharChar.InvalidTrigraph`, while `decodeEscape` runs `char (Int32.Parse "999")` and silently
yields U+03E7. I did not check whether the string lexer's permissiveness is deliberate error
recovery — if it is, the fix is a `voption`/`Result` return here plus a diagnostic, not a wider
match; if it is not, the two escape tables should be one.

### `StringLiterals.fs:22` — the lexer already decodes each escape and throws the result away

`pCharChar` computes the decoded character (`CharChar.Escaped '\n'`, `CharChar.UnicodeShort`,
`CharChar.Trigraph`) and the token stream keeps only the source span, so `decodeEscape` re-derives
from raw text what the lexer had in hand a moment earlier. That is why the escape table exists
twice at all, and why the two can drift as they have. Carrying the decoded char on the token — or
having the lexer publish its decoder — would delete this function and the divergence above with
it.

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

### `PlatformTypes.fs:151` — a class's member bodies and interface impls escape the platform check

`walkDecl` walks augmentation and interface-impl member bodies for `Record` and `Union` only; the
`Class` arm falls to `| _ -> ()`, so neither `c.Members` nor the member bodies inside
`c.Interfaces` are ever scanned for an intrinsic the target cannot represent. The comment that
was cut here justified skipping class members on the grounds that a backend may not emit them,
but it gave the opposite justification for interface impls — that backends DO lower them, so an
unrepresentable type in one is a real reject — and a class's interface impls are lowered on the
same footing as a union's. Separately, the hand-rolled match duplicates `TTypeKindG.members` and
`TTypeKindG.interfaceMembers` (`TastDecl.fs:300`, `:311`), which already name exactly "every
member body under a type declaration"; routing through them would make the omission a visible
filter rather than a missing match arm.

### `ResolvedTypes.fs:48` — `pushScheme` silently ignores every non-`NamedSimple` binding

`pushScheme` matches `TPat.NamedSimple` and returns an empty `added` for anything else, so a
binding whose pattern is a tuple or a record pattern contributes no quantified roots to
`allowed`. Any such binding that did carry a scheme would have its quantified typars counted as
unresolved and reported as `InternalBreak.UnresolvedTyVars`. `declSite` immediately below has the
same `NamedSimple`-or-nothing shape, and there it is documented as best-effort attribution, which
is fine for a source location and not obviously fine for a correctness check. I did not confirm
that a non-`NamedSimple` binding can reach here holding a scheme — if it cannot, the invariant
deserves to be in the type rather than in a fall-through arm.

### `Passes/Unification/InferForwardSchemes.fs:33` — fresh-typevar-at-level is written twice inline

`seedBindingTypars` repeats `let tv = ctx.NewTypeVar()` followed by
`ctx.Store.SetLevel(UnionFind.find ctx.Store tv, ctx.CurrentLevel)` in both the seed-miss and the
no-seed branches. `freshTyVar ctx` is already in scope (the same file uses it in
`prebindModuleFunctionSchemes`); whether it applies the same level treatment is the thing to
check before collapsing the two.

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

### `PassContext.fs:155` — `ExternalStaticQualifier` / `ExternalUnionRecordQualifier` are `SymbolKey` sinks that only ever hold a `TypeKey`

Every writer of both tables wraps a type key on the way in: `Scope.fs:501/526` call
`SymbolKeyOps.externalTypeKey`, which exists only to `SymbolKey.Type` the result of
`externalTypeKeyOf : … -> TypeKey`, and `Scope.fs:509/607` write `SymbolKey.Type canon` by hand.
The sibling stamps in the same record — `ResolvedType`,
`ExternalEnumCaseStamp` — are already `SideTable<TypeKey>`, so the two wide ones make a reader
ask which of the three can hold a member key (none can). `TypeRegistry.recordKeyOrigin` and
`PassContextTypes.SymbolKeyOrigins` (`TypeRegistry.fs:125`) have the same shape: the sole caller
passes `SymbolKey.Type key`, and the doc calls the domain "each minted type `TypeKey`".
Narrowing the three sinks to `TypeKey` would delete `externalTypeKey` and the hand-written wraps.

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
