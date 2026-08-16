# Codegen.Clr follow-ups — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the comment sweep of `XParsec.FSharp.Codegen.Clr`, in progress
(6276 comment lines at 1.9:1 before it started). The sweep itself changes no code; everything
below does, which is why it is here rather than in the diff.

Same structure as the `Codegen.Js` list: **Part A** is defects, **Part B** is prose that
should be a type. One entry appears in both, because the type fixes the defect.

---

# Part A — code defects

## A1. A source-group index is used against the flat parameter list — **DONE (2026-08-15)**

**`EmitCall`, in the value-struct closure typar override.** `leading` held one element per
SOURCE group and the override indexed `sm.ParamTys.[i]`, the FLAT parameter list, so for any
callee with a tupled group ahead of the closure argument the closure's struct type went into
the wrong typar slot: `!TF` stayed the function type, which encodes to the `Fun\`2` INTERFACE.
The `if i < List.length sm.ParamTys` guard only kept it in range. The comment that justified
the indexing claimed a discovery gate on all-`GSimple` callees; no such gate existed.

Landed with B1: `StaticMethodRef` carries one `FlatParams<FrozenType>`, and the override zips
`leading` against `Params.ByGroup`, so each group hands over its own flat slots and there is no
index to get wrong. `data/TupledGroupBeforeLambdaValueStruct.fs` + the `StructSeq` suite pin
it — under the old indexing that program is an `InvalidProgramException`, not merely a box.

## A2. `TryEmitRecordCons`'s `fieldNames` parameter is dead

`ClrProvider` binds it `_fieldNames` and never reads it, while `ICodegenProvider` still
declares it. Six lines of doc described a caller protocol around it — "the caller is
responsible for matching declaration order… a separate reorder if needed" — that nothing
implements. Deleted.

Either the parameter goes or the reorder has to actually happen. `EmitConstruct` carries a
matching hedge admitting the current behaviour only works for the one-field `Ref<'T>` shape,
so the two want resolving together.

## A2b. `zonkedArgs` in `ClrProvider` — a rebinding that names a step it does not take

`UserGenericMemberRef` opens `let zonkedArgs = args` and `TryEmitRecordCons` opens
`let zonkedArgs = tyArgs`, then use only the new name. Nothing zonks. Either the zonk was
removed and the alias survived it, or it was never written; either way the name asserts a
normalisation the reader will look for and not find. Inline both.

## A3. `TypeSlotKind.StructEnum of isMixed: bool` — dead payload

Its only consumer matches `TypeSlotKind.StructEnum _`; the writer reads `IsMixed` off
`StructEnumDecl` instead. The deleted comment admitted it, saying the field was "carried only
for documentation symmetry with the writer".

Dropping the payload is the acceptance test for its own entry: if it lands and that sentence
would still need writing, the change was wrong.

## A4. Cross-package `MethodSpec` arity agreement is unguarded

`staticFnTypars`' doc claimed the producer arity "MUST STAY IN LOCKSTEP with the CONSUMER's
dependent-typar fixpoint… so the graduation test guards divergence end to end". The cited
test builds `Vesper.Seq` and asserts two type names exist; it never consumes `fold`'s
`MethodSpec` across a package boundary.

The claim is deleted. The agreement is real and currently untested — this wants the test the
comment believed in.

## A5. A milestone label is baked into a user-visible error string — DONE

`EmitResolve`'s generic-union static-augmentation failure ended `"… is out of scope (R2)"`.
The house rule against plan-doc milestone labels applies, but this one is a string literal
rather than a comment, so the sweep could not touch it without breaking its own gate.

Landed with the decision to hold message strings to the comment rule, recorded in
`.claude/skills/comment-hygiene`: the message now states the limitation, that this compiler
does not emit a static augmentation member on a generic union.

`EmitLoops`'s five `Rung-3` / `Wall A` comment labels are gone — that file has now been
swept.

## A6. `instantiationFor` swallows every exception — **DONE (2026-08-15)**

Its `try … with _ -> declaringTypars` was meant to catch one unrecoverable-slot failure from
`recoverMemberInst`. It caught everything, so a genuine encoder bug degraded silently into a
wrong-but-plausible instantiation rather than failing.

Landed with B2, by deleting the handler rather than narrowing it: the one caller with a
fallback now takes the `try`-shaped recovery, which answers `ValueNone` for an unrecovered
slot, so an exception from anywhere else propagates.

## A7. An arity over-count wants a regression test, not a comment

`ModuleClassPlan`'s body sweep deliberately does NOT use the front-end `scheme.Quantified.Length`,
which over-counts a quantified-but-body-erased typar. The deleted comment named this "the
`SetTree.compare` regression" — so the honest form is a test with that name.

## A8. A struct enum's case singletons are public fields, by deferral

`LayoutNodes.buildStructEnumNodes` emits each string/mixed-enum case as a
`public static initonly` field. A twelve-line comment recorded the intent: PRIVATE fields
behind public get-only properties, so construction is not public API and the closed set is
guaranteed to EXTERNAL consumers. It is deferred because the metadata writer has no
`Property` / `MethodSemantics` table, and within-assembly access is a direct `ldsfld`.

The comment is deleted. Either emit the properties or accept public fields as the design —
a deferral has no reader once it stops being news.

## A9. The enum `ValueNone -> "int"` fallback is unreachable

`LayoutNodes.partitionTypeDecls` calls `TEnumCases.underlyingTypeName` only inside the arm
where `classify` already returned `Numeric`, and `underlyingTypeName`'s own `Numeric` branch
returns `ValueSome` unconditionally (`"int"` when no explicit width). The `ValueNone` arm
cannot fire. Defensive dead code that also spells the default width a second time.

## A10. Module initialisation order diverges from fsc — decide, don't drift

`moduleClassAttrsOf` drops `BeforeFieldInit`, giving **first-access, per-container** initialisation.
fsc runs file-scope bindings eagerly in file order via startup code. A side-effecting
module-value initialiser can therefore observe a different order than fsc would produce.
Currently unobservable, because every value in scope is pure.

**This is not automatically a defect.** The house position is that correct semantics beat F#
parity and fsc's quirks are not worth reproducing, so lazy per-container init may well be the
better design. What is wrong is that the divergence was recorded in a prose hedge rather than
decided. Either write the test that pins the chosen order, or state the choice in the type —
not in a paragraph that no reader will act on.

## A11b. `NominalEmit.prepareMember`'s `isIfaceImpl` parameter is dead

`prepare` threads `isIfaceImpl` out of `NominalMembers.indexed` into `prepareMember`, which
never reads it — `returnsVoid` is derived from `mem.ReturnTy` alone and the row's attrs were
fixed by the layout. The deleted comment claimed it "selects the `void`-return conformance
below", which is false; the parameter has no effect.

Either drop the parameter (and the destructuring at the call site), or, if an interface-impl
member is genuinely meant to conform to a `void` BCL slot regardless of its declared return
type, that behaviour is missing and wants a test.

## A11. Nested visibility is uniformly `NestedPublic`

Nothing models `internal` in emission — every attribute set in the file hard-codes `Public` —
so a `module internal` emits a public class today. The deleted hedge also carried the rule for
when accessibility does land (a nested type's visibility is the minimum of its own and its
containment chain's), which belongs with that work rather than in the emitter.

## A12. A C# `in` parameter cannot be called — `modreq(InAttribute)` is dropped on both sides

`MetadataSymbols.tryBuildType` collapses `in` / `out` / `ref` to a bare `T&`, and
`ClrExternalMembers.mintMemberRef` encodes that `T&` with no custom modifier. The CLR includes
`modreq(System.Runtime.InteropServices.InAttribute)` in member-ref matching, so a call to any
`in`-parameter member would `MissingMethodException` at JIT — silently, since nothing rejects it
at emit.

No current consumer needs it (`TryFormat` / `TryParse` use `out` plus by-value spans), which is
why it survived. The fix is two-sided: surface the modifier in `tryBuildType` and emit it via
`CustomModifiers(...).Type(true)`. Both sites keep one line saying so — the encoder's carries a
`TODO(inref)` tag; the sixteen lines of detail that were in the encoder now live here.

## A13. A disposable constrained-typar enumerator would emit invalid IL

`EmitLoops.emitEnumeratorLoop` mints its `constrained.` token only when `loop.IsValueType`, and
the non-value-type disposal branch does `ldloc; brfalse` on the enumerator local. When the
enumerator `E` is a generic typar (`MembersViaConstrained`), `IsValueType` is `false`, so
disposal would take that branch and `brfalse` on a local that may be a value type at runtime —
invalid IL, and the every-other-instruction path around it already addresses the same local.

It is unreachable today only because the producer hard-codes it away:
`InferControlFlow.tryConstrainedTyparEnumerator` yields `Disposable = false` for every
`ForInEnumMembersG.ConstrainedInterface`. Nothing in either type enforces that coupling — the
named field states it, no more — and the first constrained-typar enumerator that is
`IDisposable` breaks it. The disposal branch should
key on "is the object argument addressed", which is `IsValueType || MembersViaConstrained` — the same
predicate `loadEnumObjArg` already uses — not on `IsValueType` alone. Pairs with B14.

## A14. `MetadataContext.AddProgramType` is a byte-identical copy of `AddClass`

Same parameter list, same body: both call `mb.AddTypeDefinition` with the identical
nil-if-empty namespace expression and pass every other argument straight through. Only the
docs differ, and what they claim differs (the static container owns no fields, so `firstField`
points past any preceding rows) is the CALLER's to satisfy — neither member enforces it.

Either delete `AddProgramType` and call `AddClass`, or make the container member actually differ:
one that takes no `firstField` and derives it from the row count is the shape its doc
describes.

## A15. `isValueType` answers `false` for tuples and for enums — **DONE (2026-08-13)**

`EmitPattern.isValueType` classifies through `TypeLayout.shapeOfFrozen`, which is TOTAL over
`FrozenType`, so `unit` (a `System.ValueTuple` that the scalar-name set omitted), `FTTuple`,
`FTEnum` and `FTLiteral` each answer for themselves and a new case is a compile error rather
than a silent `false`. Three sites acted on the wrong answer — `EmitIntrinsic.buildUpcast` (`box` vs
nothing), `buildDowncast` (`unbox.any` vs `castclass`) and `EmitPattern`'s `:? T as x` — so
`(t :> obj)` on any of the three pushed an unboxed value where a reference was required:
invalid IL, caught by nothing at emit. `data/ValueUpcast.fs` + the `Struct` suite pin the
`box` per shape; each arm reverted individually drops it.

**Why the key list could not have been patched instead (user, 2026-08-13):** a backend
classifying a CLR REPR fact off `RuntimeNames` keys is a latent bug by construction — the key
is the front-end canon and the fact belongs to the repr the target's `.fs` binds it to, so the
two can only agree by hand. Naming a canon to CONSTRUCT a type
(`FTConst(RuntimeNames.unitKey, …)`) is the opposite and stays: the repr is resolved FROM it,
through `IntrinsicTypeMap`. Three key-set classifications survive in this backend —
`isVesperListKey`, `isFsharpCoreListKey`, `isPrintfFormatKey` — each recognising one nominal
identity rather than a layout, which is why they are not this bug; and `RuntimeNames` itself
still carries four CLR-specific NAMES (`textWriterTypeName`, `stringWriterTypeName`,
`arrayOfListName`, `formatterTypeName`), which is the same seam crossed the other way.

## A16. The ref-struct disposal carve-out can never fire on the CLR — `use` rejects it first

`EmitBindings.buildUse` rejects every value-type boundVar up front
(`if isValueType env varTy then failwithf "… out of scope"`), and `EmittedClass.IsValueType` is
`cd.ValueKind <> ClassValueKind.RefType`, TRUE for `RefStruct`. So a project-local
`[<IsByRefLike>]` boundVar dies at that guard and never reaches the `Disposal.ViaOwnMember` arm.

The front end accepts it: `Infer.resolveLocal` mints `Disposal.ViaOwnMember` from
`tryRefStructOwnDispose` for exactly that shape. A `use` over a local ref struct with a pattern
`Dispose()` therefore type-checks and then fails codegen. Either lower the addressed-object-argument
form (`ldloca` + `call` / `constrained. callvirt`, and no null check — a struct cannot be null)
or reject it in Validation with a message about ref structs rather than "out of scope".

Found through two false comments, both now deleted. The arm's doc claimed the ref struct "lands
here too". The guard's doc claimed a BCL struct disposable "reads as `TyClass` and is
indistinguishable from a class here … the provider's `ExternalClassShape` doesn't surface it" —
wrong twice over: `ExternalClassShape.Flags.IsValueType` is what `ClrEnv.externalIsValueType`
reads, so an external struct enumerator IS distinguished, and it too is rejected by the guard.
(`TyClass` for `FTClass` is the C2 family again; here the block did not survive on its own
merits, so nothing was renamed.)

---

# Part B — prose that should be a type

Each entry names the comment it deletes; that naming is the acceptance test.

## B1. `FlatParams` — shared, and it belongs in `Codegen.Common` — **DONE (2026-08-15)**

`CompiledFns.FlatParams<'T>` holds one segment per SOURCE group — the group and the flat slots
it expands to — with a private representation and `ofSegments` as its only constructor.
`Groups` / `GroupCount` / `Flat` / `FlatCount` / `ByGroup` are all read off it, so there is no
loose pair of counts to swap. Generic in the slot payload, which is what lets one type serve
both backends: `FlatParams<StaticParam>` on `CompiledFn` and `StaticFn`,
`FlatParams<FrozenType>` on `StaticMethodRef` and `CallArity.Grouped`, `FlatParams<string>` on
the JS trampoline.

The segmentation rule is spelled once per payload, in `TastLower`: `compiledSegments` (which
`compiledOf` now flattens) and `groupTypeSegments` (which `flattenGroupShape` now flattens).
A backend cannot re-derive a width.

Deleted with it: `StaticFn`'s two-field `Params` / `Groups` doc pair, `StaticMethodRef`'s
`ParamArity` (which had no reader) alongside `Groups` / `ParamTys`, `CallArity.Grouped`'s
second `flatArgCount` element, `Assembler`'s three lines on why the two counts differ, and the
`TrampolineParams` sentence saying the two are "carried together".

## B2. `DeclaringInstantiation` — `EmitResolve.instantiationFor` — **DONE (2026-08-15)**

The 3-way precedence (result type constructor → signature recovery → bare declaring typars) was a
`match` plus a `try/with`, with a comment at each step. `declaringInstantiation` now returns
`FromResultTy | FromSignature | OpenDeclaring`, so the one call site is a total match and only
the `OpenDeclaring` arm mints the bare typars.

Landed with A6: `fillOpenTyparSlots` carries the structural matching, and the strict
`RecoverOpenTypars` (which keeps its per-axis, per-index failure message) and the new
`TryRecoverOpenTypars` are the two ways of reading its slots. The `try/with` went with it.

## B3. `ExternalParent` — `EmitResolve.externalInstanceMemberRef`

Two 3-line comments exist only to say where the parent `TypeSpec` comes from per object-argument
shape. `ExternalParent = FromObjArg of FrozenType | RecoverFromSignature`, computed once,
deletes both.

## B4. `CallResult` — the `unit` → `void` mapping, restated at four sites

`emitInstanceMember`, `emitConstrainedInterfaceCall`, `buildMethodCall` and
`buildStaticMethodCall` each recompute `returnsUnit` / `resultCount` and each carry a line
saying what it means. One value carries both and deletes all four.

## B5. A named record for `EmitCall`'s argument triple

`(TastAccessor.ExprId * FrozenType * Anchor)`, where the middle element is the
partial-application result type AT THAT STEP, not the argument's own type. That
non-obviousness is the only reason two surviving comments exist. `{ Arg; StepResultTy;
Anchor }` deletes both, and the repo already prefers records over tuples of three or more.

## B6. Node-tagged `FrozenType` — `ClosureVerdictRewrite.nestedSubst`

The substitution keys whole nominals so that two `int -> int` leaves at different chain
depths cannot collide. A frozen type carrying its originating node makes that structural; the
deleted collision-freedom paragraph is now recorded only here, which is the intended state.

## B7. `ClrHoleFormat.toDotNetFormat`'s overloaded third slot

Returns `HoleKind * string option * Alignment`, where the `Alignment` is sometimes the field's
own alignment and sometimes a zero-pad TOTAL WIDTH (`%08o` → `Const 8`), depending on which
`HoleKind` came back. A record with `Width` distinct from `Alignment` deletes the surviving
doc line, which exists only to say which meaning applies when.

## B8. A closure needs a `FrozenType`, so `ClrProvider` fakes one

`RegisterStackClosureValueType` exists because a closure is keyed `TypeSlotKey.Closure name`,
which no signature can encode, yet a value-struct closure's by-value local, its `initobj` and
its constrained-slot `MethodSpec` argument all need an encodable type. It mints an
`FTClass` over a synthetic key in a reserved `<closure>` namespace and registers it in
`UserTypes` + `UserValueTypes` — two side tables whose *membership* is what makes the type
encode as a local `ELEMENT_TYPE_VALUETYPE`. The unspellable namespace is then defended by a
runtime `failwithf` against a collision with a real user key.

A closure key that IS a `TypeKey`, or a `FrozenType` that can name a closure directly, deletes
the surviving three-line doc, the synthetic key and the collision guard together.

## B10. The "does this class emit a primary ctor?" predicate, spelled three times

`isStruct || cd.HasPrimaryCtor || List.isEmpty <secondaries>` appears in
`LayoutNodes.buildClassNodes` (which decides whether to declare the `.ctor` row),
`NominalEmit.register` (which decides whether to reserve its handle) and `NominalEmit.prepare`
(which decides whether to prepare its body). All three must agree or a row is declared without
a body, and each carried a comment saying so — one of them by naming the other file.

A `CtorPlan` computed once off the class decl, carrying the primary-ctor decision and the
`Ctor`-aliases-first-secondary consequence, deletes both surviving `NominalEmit` blocks and
makes the layout/emit agreement structural rather than a coincidence of three copied lines.

## B9. `ClosureTyparScope` is ambient state with an unbalanced Exit

`ExitClosureTyparScope` resets to `ValueNone` rather than restoring, so it is correct only
from an unscoped caller. `Assembler`'s two flat loops satisfy that; `ClrGenerics` does not and
saves/restores the field by hand instead. The surviving INVARIANT doc on `Exit` exists purely
to tell the next caller which of the two shapes it is allowed to use.

A `withClosureTyparScope d f` combinator that saves and restores makes the wrong shape
unwritable and deletes the doc, `Enter`, and `Exit`.

## B11. `OpenMemberSignature` — `ClrExternalMembers.mintMemberRef`'s eight positional parameters

`mintMemberRef` takes `parent`, `methodTyparArity`, `paramsT`, `retT`, `isProperty`, `isStatic`,
`argSigLen`, `memberName` — two adjacent same-typed `bool`s and two `int`-ish counts, all
derivable from the one `ExternalMember` both call sites already hold. `openTemplate` exists only
to fold the same `ExternalMember` into a single `FrozenType` whose meaning (`isProperty`) is
carried by a separate argument.

A record computed once off `ExternalMember` — `{ Parameters; Return; IsProperty; IsStatic;
ArgSigLen; MethodTyparArity }` — makes the swap unwritable and deletes both surviving three-line
docs, whose whole content is which slot means what.

## B12. A normalised type key deletes `CodegenSymbols.reconciledLookup`

A generic type is registered BARE (`Vesper.Option`, contract layer) or arity-suffixed
(`Vesper.Option`1`, metadata layer), so every type-shape probe in the backend has to try both.
`reconciledLookup` is that retry, and its three-line doc exists only to say the two conventions
are real.

The trap the retry leaves behind: `ICodegenSymbols.TryLookupType` is the RAW probe, and only the
free function `lookupTypeByKey` reconciles. Nothing today calls the member directly, but the
interface offers the wrong one first. Normalising the key at registration deletes the helper, its
doc, and the choice.

## B13. `MetadataTailKey` — `ClrSymbolProviders`' two memos differ only in what they may key on

`bclMetaTail` memoises tails process-wide on the reverse map alone, which is sound only because
the host TPA is constant; `bclMetaTailWith` therefore cannot reuse it and mints a fresh
per-instance memo per compilation. Both functions are otherwise identical, and the deleted prose
was a warning not to route the path-taking one through the global memo.

One memo keyed on `{ ReverseCanon; Paths }` makes the unsound reuse unspellable and collapses the
two factories into one taking the paths.

## B14. `EmitLoops.EnumeratorLoop`'s three dispatch bools are one choice each

`GetEnumeratorViaInterface`, `GetEnumViaConstrained` and `MembersViaConstrained` encode the
object-argument/dispatch decision as three independent bools, and two of them are not independent. The
`Pattern` arm assigns the *same* value to `GetEnumeratorViaInterface` and `GetEnumViaConstrained`;
the `Interface` arm sets `GetEnumeratorViaInterface = true` where it is never read, because that
field is consulted only inside the addressed-object-argument branch and the `Interface` arm's
`IsValueType = false` / `GetEnumViaConstrained = false` never enters it. So
`GetEnumeratorViaInterface` is redundant with `GetEnumViaConstrained` at the only site that reads
it. Its deleted five-line doc claimed it distinguished the `Interface` arm from the `Pattern` arm
— a distinction the emitter does not make.

A source-dispatch DU (by-value `callvirt` / by-address `call` / `constrained.` on the address) and
an enumerator-dispatch DU replace all three, delete the two surviving field comments, and make A13
unrepresentable: "is the object argument addressed" becomes one value read by `loadEnumObjArg`, the
`GetEnumerator` call and the disposal branch alike, instead of three bools recombined per site.

## B15. `MetadataSymbolProvider`'s "Must hold `gate`" is a comment, not a token

`MetadataLoadContext` is not thread-safe, so `resolveTypeLocked`, `enumerateClassMembers`,
`buildClassInterfaces` and `buildClassBaseType` may only run under `gate`. Four doc lines say
so and nothing enforces it; the two entry points that do take the lock (`computeType`,
`computeMembers`) are the only reason it holds.

Moving the reflection helpers onto a private inner type that only a `lock`-taking factory can
hand out — or threading a `Locked` token they each require — deletes all four clauses.

## B16. A canon lookup, not `Map<string, SymbolKey list>` threaded through six functions

`reverseCanon` is passed explicitly to `tryBuildType`, `tryMethodSignature`,
`tryPropertySignature` and `tryCtorSignature`, re-explained at the module doc and at the
provider's own doc, and read as `Map.tryFind` + `List.isEmpty` + `List.head` — because on the
CLR the list is always a singleton. The surviving three-line doc exists to say exactly that.

A `PlatformCanon` value with `tryCanon : string -> SymbolKey voption`, built once in the
provider's constructor, removes the first-element read, the emptiness guard, and the doc. The `list`
in `IExternalSymbolProvider.IntrinsicReverseCanon` is real for JS (`number` → several canons),
so this is a CLR-side narrowing at the seam, not a change to the interface.

## B17. A per-level hit DU for CLR by-name hiding — `MetadataSymbols.computeMembers`

`resolve` walks `candidates` most-derived-first and re-derives, at every level, whether the
name is owned outright (property/field, hides everything below), collected (methods, which
overload across levels), or absent. It expresses that as nested `match` on `[| _ |]` / `[||]`
array shapes, which is why it needed a sixteen-line prologue to be readable at all.

`probeLevel : Type -> LevelHit` returning `Owns of ExternalMember | Overloads of
ExternalMember[] | Absent` makes hiding a total match over three cases and deletes the
surviving three-line block.

## B18. Metadata row ORDER is an obligation on every caller, spelled at eight sites

`Metadata.fs` documents an ordering rule on `AddField`, `AddMethod`, `AddParameter`,
`AddClass`, `AddProgramType`, `AddNestedType`, `AddInterfaceImplementation` and
`AddGenericParameter` — fields and methods before the `TypeDefinition` that claims them,
`Param` rows in sequence order, `NestedClass` / `InterfaceImpl` / `GenericParam` sorted by a
column SRM validates on serialize. `Assembler` then carries three more comments stating that
its walk happens to satisfy them.

A type-writer that takes a whole type (its fields, methods and parameters) and emits the rows
in the only valid order, plus a collected-and-sorted sink for the three validated tables,
deletes the obligation clause from all eight docs and the three `Assembler` reassurances with
them. `MetadataContext` would then expose no member whose misuse SRM can only catch at
serialize.

## B19. A prepared compilation — `ClrDriver`'s provider and cache digest are folded separately — DONE

Raised by the `Codegen.Js` sweep as well, and landed once for both. `PreparedCompilation` —
private representation, minted
only by `ClrDriver.prepare` — holds the `ClrCompilation`, its digest and its provider, and
`compileCachedWith` takes one in place of a digest plus the inputs. A digest folded from other
inputs can no longer reach it, and the doc clause that asked for one folded from THESE `inputs`
is gone.

The three identical `buildContractWithRefs` calls became a private `contractFor`, so `compile`,
`prepare` and `compileAssembly` resolve a contract one way. `compile` and `compileAssembly` do
not prepare: neither keys a cache, and folding a digest reads the whole dependency closure.

`compileCached` is `prepare`'s only caller today, and prepares inline. What the type buys is
the unspellable mismatch; the multi-file driver that would amortise one `prepare` across files
is not written yet, so no throughput claim is being collected on.

## B20. A CLR-repr classifier, not a `bool` over three answers

*Half landed with A15 (2026-08-13): `TypeLayout.shapeOfFrozen` enumerates every `FrozenType`
case, so the open `_` arm that made A15 silent is gone and a new case is a compile error. What
remains is the THIRD answer, below.*

The predicate still returns `bool`, so "boxable typar" is not one of its answers: `buildUpcast`
tests `FTTypar` itself, ahead of the call, and `isValueType` answers `false` for a typar it
must nonetheless box. Two shapes, two readers, one of which has to remember the other.

`clrRepr : FrozenType -> ClrRepr` over `Value | Reference | Boxable of typar` gives
`buildUpcast` one match, and puts the typar answer where the other two live rather than in the
one caller that happens to need it.

## B21. A resolved reference set — `ProjectInfo.References` is a `string list` of paths

Which packages a build MUST list, and which fall back to the host, is spread over three places:
`ProjectInfo.References`' doc, `ClrEnv`'s `refRequired` / `refOrHost` split (one `failwithf`
message per required package, each naming its own need), and the `ClrEnv` type doc that
re-states the whole rule. The field itself is bare paths, so nothing connects a path to the
role it plays and a typo'd file name is only ever a run-time `failwithf`.

The 21-line block enumerating `Vesper.Core` → `Vesper.Fun\`2`, `Vesper.List` →
`Vesper.Collections.List\`1`, `Vesper.Printf` → `Vesper.Formatter` and FSharp.Core's optionality
was cut to three lines by the sweep; the enumeration now lives only here and at each
`refRequired` call, which is where it belongs.

A `ReferenceSet` resolved once at construction — each entry an identity read off its file, plus
whether the package is required-on-demand or host-fallback — deletes the surviving three-line
`References` doc, the `ClrEnv` header restating the resolution rule, and the `refRequired` /
`refOrHost` pair, whose only difference is which of the two answers they give.

## B22. Two CLI-fact glossaries that must agree, with nothing making them

Raised by the H19 punctuation pass, which read every dash in the project and so read both
copies side by side.

`Cil.fs` and `IlIr.fs` document the same twelve instructions twice — `castclass`, `box`,
`unbox.any`, `initobj`, `constrained.`, `newarr`, `ldelem`, `stelem`, `ldlen`, `ldobj`,
`leave`, `endfinally` — one set on the `ILInstr` DU cases, one on the `emit*` functions that
build them. Near-identical text, and these are ECMA-335 facts rather than facts about this
code, so neither copy is the obvious owner.

Smaller instance of the same shape: `EmitTypes.EmittedClass.Interfaces` and
`EmitResolve.tryInterfaceWitness` both state the direct-impls-only restriction.

Deleting one copy is a comment edit, which is why the sweep did not do it: choosing WHICH
side owns an opcode's meaning is a structural call. The DU case is the better candidate — the
`emit*` function is a constructor for it and can say what it constructs — but that wants
deciding once for all twelve, not per instruction.

## B23. Comment-only residue left by the H19 pass — `Codegen.Clr`

Filed rather than fixed; the H19 sweep was punctuation-scoped and these are other modes.
Each is a comment edit, no code change.

- **Object-negation (retired `names no X` family).** `EmitClosures.collectProgramValues`
  ("those declaring no enclosing module"), `ClrDriver.ClrCompilation.consumer` ("defines no
  primitives of its own"), `EmitFormat`'s `CallbackHole` branch ("Codegen has no sink
  knowledge" → "does not know about sinks"), `EmitCall`'s phantom-typar block ("a typar in no
  parameter and no result"). Note the discriminator found while triaging: `has no <concrete
  absent artifact>` (`no Dispose row`, `no tag`, `no parameterless ctor`) reads as fact and is
  NOT this defect — only the negated abstract object is.
- **`EmitResolve.resolveInstanceMember`** — "An external one goes to
  `externalInstanceMemberRef`" is a `Module.func` cross-reference that rots on rename.
- **`MetadataSymbols.tryMethodSignature`** — the H19 pass turned `UNCOLLAPSED — one entry per
  value parameter` into a colon, since the left side is a bare term rather than a code
  literal. Recorded in case the dash is preferred.

Already fixed during the sweep, listed so they are not re-reported: `EmitClosures.typeKeyNsName`
named `TypeSlotKey` for a `TypeKey` parameter (a rename that missed the prose).

---

# Part C — cross-cutting, found here but not `Codegen.Clr`'s alone

## C1. Agent memory filenames cited in source comments — **DONE**

A comment that cites `feedback_…` / `project_…` / `reference_…` points at a file in an agent's
machine-local memory directory. It is not in the repo, and no reader can ever open it.

Five instances: two deleted by the `ClrEncoder` sweep, and three fixed directly — two in one
`Vesper.Ts.Extractor/Diagnostics.fs` doc block, one in
`SemanticAnalysis/Passes/Unification/InferPat.fs`. In each case the surrounding fact was kept
and only the pointer removed. `grep -rInE '\b(feedback|project|reference)_[a-z0-9_]{6,}'` over
`src/` now returns nothing; both projects build clean.

Worth re-running before a release — the leak recurred across two projects and neither author
would have seen it as a reference to something private.

## C2. Comments naming things that do not exist, outside the swept files

Found while fixing their twins, and NOT yet verified in these files — the name may be correct
in a different domain, so check before editing:

- `ClrGenerics` cites `ClrEnv.ClosureTyparMode`. That member is `ClosureTyparScope`; the
  twin citation in `ClrEncoder` was false and has been fixed. **DONE** — verified false in
  `ClrGenerics` too (the file's own save/restore reads `env.ClosureTyparScope`) and the
  citation went with its block in that file's sweep.
- `NominalEmit`'s `TyTypar` citation was confirmed wrong-domain (the file is `FrozenType`-
  native throughout — `FTTypar`, `FTUnion`, `FTClass`) and went with its block in the sweep.
  Three more of the same kind, all in `Emit.fs` and all fixed: `SelfTy` fields documented as
  `TyUnion(name, [])` / `TyRecord(name, …)` on `FrozenType`-typed fields.

Two more of the same kind, both in `ClrProvider` and both fixed: `RegisterGenericUnion`
claimed to be "a no-op for a monomorphic union" when it registers unconditionally and the
caller is what guards, and `RegisterClosure` showed the guard as `Closure.Typars = []` where
the parameter is `typarCount: int`. Both were forward claims about a caller.

---

## Done when

- Part A: each defect has a fix, and A4/A7 have the tests their comments claimed existed.
- Part B: each entry's named comment is gone because it has become unstatable.
- The remaining 33 files are swept and this file is deleted.
