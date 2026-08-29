# SemanticAnalysis comment-sweep follow-ups

Non-comment issues found while reading every comment in the project as an unverified claim.
Recorded, not acted on: fixing them mid-sweep would break the code-preservation gate that
makes parallel comment editing safe.

Each entry names the comment its fix would delete. That naming is the acceptance test — if
the change lands and the sentence still needs writing, the change was the wrong shape.

Every claim below was verified against the source by the orchestrator, not taken from a
subagent report.

## Defects

### `CstKeys.fs` — `kindOfExpr` and `siteOfPat` collapse most shapes to `NodeKind.Unknown`

Found landing the `firstTokenOfExpr`/`firstTokenOfPat` completion (2026-08-25). Both kind
matches end in `| _ -> NodeKind.Unknown`, so every shape without a dedicated `NodeKind`
(`Pat.And`, `Pat.Optional`, `Expr.Object`, `Expr.ControlFlow`, the slices, …) keys as
`Unknown`. Two distinct nodes starting at the same offset — a `Pat.And` and its left
sub-pattern — can then collide on a `NodeKey`. The fix adds `NodeKind` enum values, which
touches the wire-format enum.

Related residue: `Expr.Missing`, `Pat.Missing` and empty `Tuple`/`Sequential`/`Elems`/
`SkipsTokens` genuinely retain no token and still raise (now with the shape named). Making
them honest means a `voption` return rippling through `siteOfExpr`/`ofExpr`/`siteOfPat`/`ofPat`.

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
be the whole of the safe fix.

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

### `TastDecl.fs:129` and `:205` — `ThisKey` is stored twice and cannot differ

`TClassG.ThisKey` (`Elaborate/TypeDecls.fs:701`) and every instance member's
`TTypeMemberG.ThisKey` (`Elaborate/ClassMembers.fs:225,247`) are all `info.ThisKey`, itself a
pure function of the declaration's `NodeKey` (`BoundVarKey.ofDeclaredThis`,
`MemberRegistration.fs:607`). The member-level copies are derivable from the class-level one.

Not done (2026-08-28): only `TClassG` has a type-level `ThisKey`, so a record's or union's member
is the sole carrier of its host's key; removal needs `ThisKey` hoisted to `TTypeDeclG` first.

### `Passes/Unification/Translate.fs:506` — the measure carrier is the last by-name reach

`tryResolveExternalType` resolves `float` / `int` by name for a `float<m>` carrier, because
The classifying walk recorded that name at its SYNTACTIC arity of 1 and the carrier is wanted at
arity 0. `TypeRegistration.fs:365-367` already recognises the shape (`isMeasuredCarrier` skips
the carrier so `float<kg>` reports no diagnostic at `kg`), so the classifying walk knows it is
looking at a carrier and could record the arity-0 verdict there instead of skipping the node.

That is what would leave `ctx.Resolver` read only by NameResolution — the enforcement the
deleted `ResolverAllowlistTests` was standing in for, since the handle could then be a parameter
rather than a `PassContext` member. Worth doing for that reason, not for the lookup it saves.

### `Passes/Unification/Translate.fs:118` — the key-minting invariant is written three times

`externalClassTy`'s body, `buildExternalTy`'s doc and `tryExternalTypeOfKey`'s doc each asserted
"mint on the resolved key, never re-cut from the rendered name, because `InModule` keys don't
round-trip through the `+`-metadata name". One short form survives at `:118-120`. Triplication is
the signal that this belongs in `SymbolKeyOps` as a type-level restriction on which key shapes
`typeMetaName` may round-trip.

### `Passes/Unification/Engine.fs:62` — `DotSource.ClassChain`'s doc exists to explain a shape mismatch

The block says only why the chain case cannot be the `subst` + `lookup` pair that `Resolved`
carries. A member-lookup abstraction covering both shapes deletes it outright.

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

### `PassContext.fs:200` — `ExternalStaticQualifier`'s payload shape is a writer-side promise

A `SideTable<SymbolKey>`; nothing in the type says the key names a static-member-bearing shape.
The three writers (`Passes/NameResolution/Scope.fs:679`, `:687`, `:816`) all gate on
`ExternalTypeShape.Class`, and readers dispatch without re-querying the shape on the strength of
that. A key type carrying the guarantee would make the re-query provably unnecessary instead of
conventionally so.

### An intrinsic abbrev cannot implement an interface — four gates, not one

Found landing the host-cascade collapse (2026-08-25); re-verified 2026-08-28 by lifting each
gate in turn and re-running the pipeline. The original entry named only the missing
intrinsic-abbrev leg in `TypeRegistry.tryInterfaceImplHostByKey`. Adding that leg alone changes
nothing: with the leg in place and the two registration gates lifted, `widget :> IPoke` still
reports `Type mismatch: widget vs IPoke`.

Four independent changes are required, so this is a design question rather than a bounded fix:

1. `MemberRegistration.fs:686` rejects `interface … with` on an intrinsic host outright
   ("the type carries no representation in the output"), pinned green by
   `ExternMemberElabTests.fs:259`.
2. `MemberRegistration.fs:745` (the `TypeDefn.Abbrev` arm of `registerNominalMember`) stamps
   `info.Members` and deliberately skips `info.InterfaceImpls`, which therefore has no writer
   and is always empty.
3. `EngineCore.nominalKeyOf` (`:523`) returns `ValueNone` for `TyConst`, so `subtypeInterfacesOf`
   (`:569`) and `subtypeParentOf` receive `localKey = ValueNone` for an intrinsic and never call
   `tryInterfaceImplHostByKey` at all. Surfacing the `TyConst` key here would also make the
   host's empty `InterfaceImpls` shadow the external provider's `ExternalTypeShape.Intrinsic`
   interfaces, which is how `string` and `char` reach `equatable` / `comparable` today.
4. `Infer.fs:252` (`resolveUseDispose`) matches `TyClass` / `TyUnion` / `TyRecord` only, so a
   `TyConst` cannot reach the `use` path either.

Separately surfaced: a bare `interface IPoke` spec (no `with`) is accepted on any type and
silently dropped — `extractInterfaceImpls` (`MemberRegistration.fs:349`) collects
`TypeDefnElement.InterfaceImpl` only. F# treats the bare spec as a real implements edge and
reports FS0366 when the members are missing, so the silent drop is its own gap.

## Dead or duplicated structure

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

### `Passes/InlineExpansion.fs:398-412` — a re-entry branch that may be unreachable

`outlineNullaryIntrinsic` is reached only through `expandingTemplate` with `Args = []`, and an
operand-less intrinsic names nothing, so the `Descent.reentered` result for this path looks
unreachable. If that holds, the `expandingTemplate` wrapper at `:409` is pure overhead and its
`ValueSome reentered` branch is dead. Confirm before anyone relies on it.

### `Passes/NameResolution/Scope.fs:585` — the operator-form long-ident catch-all diagnoses rather than resolves

`Expr.LongIdentOrOp` reports `OperatorFormQualifiedName` unconditionally for any operator-form
long ident that is neither symbolic nor qualified, carrying an explicit `TODO`. Active-pattern
and nil op-names used as values are rejected rather than resolved.

### `PublishedSurface` has no index-signature table

Residue of the `KeyIndexedChannels.IndexSignaturesByKey` channel (landed 2026-08-25):
`TsManifestProvider.fs:82-89` still publishes index signatures through a private
`ProviderDecorator` because `PublishedSurfaceBuilder`/`PublishedSurface` carry no table for
them. Connecting it is a published-surface format change governed by
docs/publishing-format-plan.md.

Also noted: every `KeyIndexedChannels` construction site uses `{ KeyIndexedChannels.empty
with … }`, so a new channel defaults silently rather than forcing producers to consider it.

### `Passes/Unification/EngineCore.fs:392` — the intrinsic-canon lookup's central rule is unenforced

`canonKey` is a two-tier, forward-only, memoized `SymbolKey → SymbolKey` map whose invariant is
that NEITHER tier may project a name back out of the key — a user nominal `MyLib.int` would
false-match. The tiers are read ad hoc from `ctx.Types.IntrinsicReprKeys` and
`ctx.Provider.TryLookupType`, with nothing enforcing the by-key-only rule. This was 23 lines of
prose before the sweep; a named lookup type with a by-key-only API deletes the 3 that remain.

### `Passes/InlineReduction.fs:167` — `ExternalFunction.Args = ValueNone` is documented as unreachable-by-shape, but is a `failwithf`

The field doc claimed a non-opening argument means "the function cannot be a template". The consumer
disagrees: `InlineExpansion.fs:191-194` matches `ValueSome served, ValueNone` and raises
"the spliced member … was applied to an argument its declared parameters cannot be bound to".
Encoding the two outcomes in the type (a member that opened vs. one that did not) would
remove the pairwise match over `lookupExternal x.Ctx x.Specs ext.Key, ext.Args` and the failure
arm with it.

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

### `Passes/NameResolution/MemberRegistration.fs:265` — the declared-typar prefix is an `int` beside an unrelated array

`mkTypeParams ctx.Store (explicit @ implicit)` concatenates two differently-ordered typar lists
into one `SeedTypars`, and `List.length explicit` is passed alongside as `declaredCount` so that
consumers can recover the split with `List.truncate mInfo.DeclaredTyparCount`. The invariant
"the leading `DeclaredTyparCount` entries are the explicit `<'C>`, the tail is appearance-ordered"
is prose in three places (`TypeInfos.fs:70`, here, and both copies of the canonical computation
already noted at `Passes/Unification.fs:99-145` / `:329-370`). A seed carrying the two groups as
separate fields would delete the arithmetic and the comment at every site.

### `Elaborate/Typars.fs:86` — the dependent-typar fixpoint is duplicated in `InferGeneralize.generalise`

`mkMethodQuantEnv`'s worklist (`:86-95`) and `InferGeneralize.generalise`'s (`InferGeneralize.fs:264-272`)
are the same algorithm: walk the collected roots, fold each root's `Coercion` constraint targets
back into the same `ResizeArray`, and let the array's growth drive the index to a fixpoint — so a
constraint-only typar such as `'E` in `'S :> IStructSeq<'E>` is quantified. The two copies use
different root collectors (`SemTypeWalk.collectLinkedRoots` here, `iterTypeVarRoots` there) and
differ in whether a level test gates the seed, which is exactly the kind of divergence that will
not show up until the two disagree about which typars a scheme has.

### `ExternalSymbols.fs:89` — the `deferredTemplate` sentinel is a two-phase type spelled as a magic value

Five separate fields hold `FrozenTypeBridge.deferredTemplate` (`FTUnknown "<deferred>"`) between
contract extraction and `VesperLib.finalizeDeferred`: `ExternalFieldShape.Frozen` (`:89`),
`ExternalCaseShape.FrozenFieldTypes` (`:107`), `ExternalSignature.Parameters` / `.Return` (`:195`)
and `ExternalSymbol.Scheme` (`:888`). Each needed a doc line saying the value may be a sentinel, and
nothing in the type stops a consumer reading one before the finalize pass overwrites it — a distinct
pre-finalize shape (or a `Deferred<FrozenType>` wrapper the finalize pass consumes) would delete all
five comments and make the ordering a compile error instead of a convention.

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

### `Passes/Unification/InferOverload.fs:54` — `matchTypes` is a hand-maintained parallel copy of `unify`'s concrete-type-constructor arms

The doc's own justification was that the two are "kept auditably parallel so a future reader can
diff the two": `TyConst`/`TyRecord`/`TyUnion`/`TyClass`/`TyFun`/`TyTuple` are each destructured
and recursed in both `matchTypes` and `Engine.unify`, differing only in what the metavar and
method-typar arms do (record into `binds` vs `Link` into the graph). A shared structural walk
parameterised by a per-node handler would make the parallelism mechanical instead of an instruction
to the reader; a new `SemType` case added to `unify` alone currently degrades overload filtering
silently to `| _ -> false`. The sweep cut the twenty-one-line header to three, so the invitation
to diff is gone but the duplication is not.

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

### `TypeRegistration.fs` — a detached `type U with …` extension is unsupported, not registered

The reporting half landed 2026-08-25: `rejectDetachedTypeExtension` reports every standalone
`TypeDefn.TypeExtension` as `NotYetSupported`, including the dotted form
(`type System.String with …`). The open half is the feature: registering a detached
augmentation's members. Extensions on a qualified external type are a separate deferred case,
tracked in docs/ts-provider-implementation-plan.md.
