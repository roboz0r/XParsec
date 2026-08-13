# Platform facts — move CLR-shaped primitive classification to the backend

*Its `extern`-classification prerequisite has landed, as has the per-target manifest split; only
the note on `CompilationInputs.Target` below still refers to it. The steps here are ORDERED:
step 1 is a hard prerequisite for step 2, and taking them out of order silently breaks
`when 'T : equality` on JS with no diagnostic.*

***Steps 1–3 have LANDED (2026-08-12, 2026-08-13).** `primitiveSupports` now consults a
provider query for every kind it answers and holds no key list of its own; step 4 is
unstarted. Each step's section records what it turned out to need that this doc did not
predict.*

*This doc ABSORBED `intrinsic-capability-representation-plan.md` (2026-08-12), which split the
same six constraint kinds across the same two axes but was written a month earlier and had gone
stale in three places: its "feasibility crux" was resolved in-tree (see step 1), the
`primitiveValueTypes` string table it argued against does not exist under that name or in that
form (`Engine.fs:106` is `SymbolKey`-keyed), and its representation-axis decision was reversed
(see "Decided semantics"). What survived is folded in below.*

## What is already correct

The lexical-token → `TypeKey` mapping is hardcoded, and only that:
`RuntimeNames.literalBaseKey` (`:292-296`) and `intWidthKey` (`:299-311`) map lexical shape to
key; `InferLiterals.fs:51` maps `NumericKind.Decimal` to `ctx.Intrinsics.Decimal`. Every key is
`primitiveKey`, i.e. `Vesper.*` — no BCL name appears in the mapping. `IntrinsicSet`
(`Intrinsics.fs:34-52`) does not hardcode the *type*: it resolves `Vesper.decimal` through the
provider from whatever `prim-types-*` contract is in scope and fails loudly if the contract does
not declare it. **No work here.** This plan is entirely about the other direction.

## The defect

CLR-shaped facts about primitives are hardcoded as three hand-maintained key lists that do not
agree with each other (the `Engine` column is GONE as of step 3):

| | `Engine.isPrimitiveValueType` | `Inline.isStructPrimitive` (`:70`) | `Regions.isNonAllocatingPrimitive` (`:122`) |
|---|---|---|---|
| `decimal` | absent | present | present |
| `unit` | present | absent | present |
| `string` | special-cased | absent | present |
| `char` | present | present | absent |

Each carries its own apology comment. None is derived from anything.

## Decided semantics

**JS has no value types.** The JS backend already states this (`EmitJsTypes.fs:283`, on erasing
the `valueKind` a `[<Struct>]` record carries). Both constraint verdicts follow from that one
fact, and both are honest answers rather than evasions:

- `when 'T : struct` — unsatisfiable on JS. Every concrete type is rejected AT THE USE SITE.
  Declaring the constraint stays legal, so a shared `.fsi` extracts identically on both targets.
- `when 'T : not struct` — satisfied by everything on JS, because everything genuinely is not a
  struct there.

The same source may therefore compile on CLR and not on JS. That is what a developer is asking
for by writing a `[not] struct` constraint, and it is accepted.

The diagnostic already exists verbatim — `Diagnostics.fs:406-407`,
`"The type '%s' does not support the '%s' constraint"` — reached from `Engine.fs:654` via the
`Violated` path. No new diagnostic, no target-refusal mechanism.

**Re-confirmed 2026-08-12, against the absorbed doc's contrary proposal.** Declaring
`when 'T : struct` is always language-legal; on JS no type is a struct, so instantiation or call
always diagnoses "the type does not support the constraint". This is an ERROR at the use site.
The absorbed doc proposed the opposite — a fourth `ConstraintOutcome` case (`UnsatisfiableHere`)
emitting a WARNING when the target cannot inhabit the polarity at all — on the reasoning that the
source is portable-legit and only this target cannot satisfy it. That is retired: a legal
declaration whose every instantiation fails is already fully described by failing those
instantiations, and `ConstraintOutcome` stays `Satisfied | Violated | Defer` (`Engine.fs:101-104`,
pinned by `UnificationUnionsTests.fs:178,189,203,215`).

**`when ^T : struct` as a STATIC OPTIMIZATION is a different mechanism and is out of scope.**
`StaticOptimizationConstraint.WhenTyparIsStruct` (`ExpressionParsing.fs:162,169`) →
`InferTypeOps.fs:97-98` → `TStaticOptConstraint.IsStruct` → `Inline.fs:116` selects clauses; it
does not gate callability. No Vesper source uses that form — the only `: struct` in
`src/Vesper.*` is an unrelated struct-tuple return in `SemiPersistentUnionFind.fs:136` — so
`Inline.isStructPrimitive` (`:70-89`) has no live producer. Delete it or leave it; it must not
weigh in the design.

## Ordering (the risk is entirely in step 1)

### 1. `Equality` / `Comparison` → the provider's capability query — **DONE (2026-08-12)**

**What shipped.** A primitive's declared `interface`s ride its `IntrinsicClassSurface`, and
`ExternalSymbolProviders.stack` FOLDS that surface across sources instead of taking the nearest
one: an impl `.fs` view binds a representation and publishes no surface, so its silence is not an
answer. `primitiveSupports` reads `Equality` / `Comparison` off the folded surface and consults no
list of its own. `SignatureExtractorTests` pins both the contract's answer for `int` and that a
repr-only leaf ahead of the contract does not shadow it.

*(A first cut instead added a parallel `IExternalSymbolStore.IntrinsicCapabilities` channel that
routed around the first-hit shape lookup. That fixed the constraint solver only and left
`subtypeInterfacesOf` / `tryForInEnumerator` / `tryUpcastWitness` reading the shadowed surface —
the same fact answered two ways. Folding in `stack` subsumes it, and the channel is gone.)*

**Three things this doc did not predict, each now in the code:**

1. **The floor is MUTUALLY RECURSIVE with the primitives.** `equatable<'T>.Equals` returns `bool`
   and `comparable<'T>.CompareTo` returns `int`, so those two anchors cannot be declared in a file
   ordered after the primitives that declare them: they moved into `prim-types-min.fsi` as one
   `type … and …` group with `int` / `bool` / `unit` (the extractor finalizes a whole group before
   freezing it, which is what makes the cycle legal). `disposable` moved with them for grouping
   only — no primitive is `disposable`, so nothing forces its position. `capabilities.fsi` keeps
   the `Vesper.Collections` iteration anchors. The CLR reprs moved with them.
2. **A capability anchor is `SigShape.ExternInterface` in conformance, and a target may bind no
   repr for it.** JS has no nominal interfaces, so an anchor absent from the paired `.fs` is a
   statement, not `MissingInImpl`. Previously that was expressible only at FILE granularity
   (`capabilities.fsi` was unpaired on JS); the group above made the file paired. `SigShape` now
   answers `DemandsIntrinsic` / `IsHeritable` / `ImplOptional` rather than being re-matched at
   each consumer, so this cost one DU case and one member arm.
3. **`IntrinsicClassSurface.Heritable`.** A scalar declaring an `interface` now carries a class
   surface, which three readers were treating as "heritable"; all three now go through
   `ExternalSymbols.intrinsicClassOf`, the one place the flag is tested. A scalar's surface is
   attached by `finalizeDeferred` ON the `Intrinsic` shape registered at extraction, so its
   identity is the canon throughout and its own member signatures (`bigint`'s
   `(+): bigint * bigint -> bigint`) freeze against it. Registering it as a `Class` and
   republishing later would make the frozen identity depend on when it was asked.

**The floor as authored:** equatable + comparable on every numeric width plus `bool`, `unit`,
`char`, `string`, `bigint`, `decimal`, `nativeint`, `unativeint`; equatable only on `obj` and
`exn` (reference equality, no ordering).

`decimal` / `nativeint` / `unativeint` joined last (user, 2026-08-12): no target could inhabit
them and not satisfy both. They bind no JS repr, which `interfaceNeedsRepr` used to make an
error — that diagnostic is DELETED, because its trigger (no repr on this target) was exactly the
condition that already marks the type `Unsupported`, so no source the target compiles can name
the type and reach the capability. A paired `.fs` that merely omits the type is a separate,
better-diagnosed case (`ConformanceError.MissingInImpl`).

---

*Original statement of the problem, kept because steps 2–3 still turn on it:*

`isPrimitiveValueType` is the SOLE path to `Satisfied` for these on a primitive:

```
Engine.fs:445-450
| Equality | Comparison ->
    if isValueType || isString then ValueSome true else ValueNone
```

and `ValueNone` on a `TyConst` is `Defer` (`Engine.fs:520-524`). So the moment `isValueType` goes
false on JS, `when 'T : equality` stops being discharged for `int` / `float` / `char` / `bool` —
no error, the constraint simply never resolves. This is the silent-difference failure for real,
landing on the constraint kind developers actually use.

`equatable<'T>` and `comparable<'T>` are declared capabilities in `capabilities.fsi`, with keys
already minted (`RuntimeNames.fs:66-70`). **The verdict is answered by the provider's capability
query** — the design decided in `capability-provenance-plan.md` Change B (user, confirmed
2026-08-12), which that doc holds in full: the query surface, the CLR and JS feasibility, the
`subtypeInterfacesOf` cost, and the rejected alternatives. Read it before starting here.

**The contract PRESCRIBES the floor**: `int` is equatable and comparable because the language
says so, declared in the shared `.fsi`, and the contract leaf answers the query with that. The
platform leaf may WIDEN past it (a CLR array really is an `IList<'T>`); the merge is additive.
What must NOT happen is reading `FrozenInterfaces` off a first-hit shape lookup — that is what
makes the answer depend on which route published the shape, and it is the defect this closes.

This step must be complete and tested before step 2 begins.

#### Declaring the capability — feasible today, and this is the template

Authoring the declarations is the FLOOR itself, so it is load-bearing, not just documentation.
The absorbed doc treated this as an unresolved question gating everything:
can a repr-only `type X = extern` carry an `interface` impl clause with no member body, the
platform rather than the contract supplying the implementation? **Yes, and it is already in
production use.**

- `SignatureParsing.fs:382-413` parses `extern`, an optional `class`/`interface` kind tag, then
  the extension elements; `Token.KWInterface` dispatches to `pInterfaceSpecSig` (`:218`), and
  `parseOpt` synthesises a virtual `with` when member tokens follow (`:258-264`). The sited
  comment at `:382-398` already answers the question.
- `prim-types-array.fsi:11-12` ships it: `type 'T ``[]`` = extern with / interface
  Vesper.Collections.seq<'T>`.
- `VesperLib.fs:1223-1239` extracts the untagged `extern with … interface …` case explicitly,
  and `SignatureExtractorTests.fs:1241-1254` pins the impl surviving extraction → republish and
  instantiating to `seq<int>`.

So this step is contract AUTHORING against an existing template, not a parser or extractor
widening. Two constraints the template carries:

- A primitive declaring an interface MUST have a repr bound in the target's `.fs`, else
  `IntrinsicHost.interfaceNeedsRepr` fires (`VesperLib.fs:1231`).
- Declaring an interface does NOT make the type heritable — `Heritable` stays false and no
  `inherit` may name it (`VesperLib.fs:1220-1222`). That is correct for primitives and is why
  the array's declaration is untagged rather than `extern class`.

Equality and comparison need TWO independent per-type declarations: `obj` / `exn` are the
load-bearing equatable-but-not-comparable primitives (reference equality, no ordering).

#### Why the verdict must NOT be read off a first-hit shape lookup

The tempting implementation — resolve the intrinsic shape, read `Interfaces`, done — makes the
verdict a property of the PUBLISH ROUTE rather than of the type. `TryLookupType` is first-hit at
whole-shape granularity (`ExternalSymbolProviders.fs:239-253`) and per-file `.fs` views sit at
the HEAD of composition, ahead of the contract (`AssemblyFiles.fs:108-109`,
`ClrDriver.fs:141-142`). `FrozenSignature.fs:419-441` builds a class surface only when
`repr.Heritable` and emits `Interfaces = EqArray.empty` when it does, and a capability-declaring
primitive is deliberately NOT heritable (above) — so the head view answers "no interfaces" when
the truth is "this leaf does not know", and the contract's populated shape is never consulted.

For `seq` that hole is latent — nothing in any Vesper package writes `for x in arr`. For eq/cmp
it would not be: Vesper.Core solves `when 'T : equality` constantly. A dedicated ADDITIVE
capability member removes it: the lossy leaf abstains by default and the contract's answer
survives. `capability-provenance-plan.md` holds the mechanism, the precedent
(`AmbientOpenPrefixes`), and the two traps in building it.

#### A helper that does NOT transfer

`Unification.implementsSelf` (`Unification.fs:953-960`) looks like the reusable "does this type
implement this capability?" answer, but it is `let private`-scoped inside
`validateCustomEqCompImpls` (`:942`) and typed over `IInterfaceImplHost` / `SemType`-resolved
impls — the LOCAL registry shape. It is the nominal-side answer and does not reach intrinsics or
cross the provider boundary. Do not plan on lifting it into this path.

#### `unit` — equatable and comparable

Equatable is trivially true. `unit` is a one-element totally
ordered set, so `compare () () = 0` is total, reflexive and the unique correct answer — not
meaningless. Keeping it comparable preserves `Set<unit>` / `Map<unit, _>` (legal, degenerate —
`Set<unit> ≅ bool`) and lets generic `'T when 'T : comparison` code instantiate at `unit`. That is
what correct math says (`feedback_prototype_correct_semantics_over_fsharp_parity`), it matches F#,
and it is the current behaviour.

The contract-location sub-question is CLOSED: `unit` has a contract (`prim-types-min.fsi:48`) with
reprs already bound on both targets (`prim-types-min.clr.fs:23` → `System.ValueTuple`,
`prim-types-min.js.fs:29` → `undefined`), so the `interfaceNeedsRepr` gate would not fire.

#### Do NOT derive the verdict from the operator clauses

The `when ^T: int` clauses in `comparison.clr.fs` / `ops-platform.clr.fs` are a codegen
optimization — inline `clt`/`ceq` versus the `Comparer<^T>` or generic base — and so name a
SUBSET of what supports the operation, chosen for "can we emit an opcode". Answering
`when 'T : comparison` from them would under-approximate: a comparable type reachable only
through the base comparer would be wrongly rejected. That clause list stays the source for the
EMIT decision (`Inline.staticOptTypesMatch`) and nothing else.

### 2. `Nullness` / `NotNull` → union membership — **DONE (2026-08-13)**

**Not a platform fact at all, which this section originally got wrong.** It was headed "the
target's null model" and predicted a per-target answer needing the query channel below. The
tree had already decided otherwise: `null` has one cross-backend identity (`nullKey`), the
nullable form is the ordinary anonymous union, and `type objnull = obj | null` is declared in
the SHARED `prim-types-object.fsi` — which is only meaningful if bare `obj` admits no null on
either target. The CLR's reference-null is erased at the ABI seam
(`EngineCore.stripReferenceNull`) rather than carried in the type. So the verdict is structural
and target-invariant, and step 2 needed no provider surface.

**What shipped.** `admitsNull` answers `TyNull` yes, a `TyOr` yes iff some member does (one
`null` member decides it; an ungrounded member otherwise defers), a `TyClass` from its
`[<AllowNullLiteral>]`, and every other ground shape — primitives included — no. Both kinds
are dispatched from it ahead of the `TyConst` table, which now answers neither.

**Two things this cost that the section did not predict:**

1. **`[<AllowNullLiteral>]` was decoded and stored but never read.** `Vesper.Set` is built on
   it: `SetTree<'T>` is `[<AllowNullLiteral>]` with `let empty: SetTree<'T> = null` and
   `isEmpty = isNull t`. Under "every ground type is non-null" that is 27 test failures, and
   the attribute is exactly the type's own statement of the answer. It now rides `TClassG`
   through the freeze into `ExternalClassFlags` — previously it stopped at `ClassTypeInfo`,
   so a downstream package would have read the `false` default and refused a legal `isNull`.
2. **`when 'T : null` no longer holds at a bare reference primitive.** `isNull (s: string)` is
   now `isNull (s: string | null)`; the `ops-platform.fsi` example and one
   `InferResolutionTests` case moved with it. That is the union model applied consistently,
   not a regression — a bare `string` is non-null in this tree the same way bare `obj` is.

`undefined` is a DISTINCT sentinel and answers neither kind: `T | undefined` is not `T | null`.
Keep them separate, as `nullability-analysis-plan.md` already requires of the guard analysis.

Four corpus programs pin the verdicts in `test/Codegen.Conformance/constraints/`, both targets
on every row, since the sameness is the claim.

### 3. `isPrimitiveValueType` → a backend fact — **DONE (2026-08-13)**

**What shipped.** `IExternalSymbolStore.IsValueType : TypeKey -> bool voption`, folded per FACT
in `stack`: a source with no opinion abstains, so the platform leaf at the tail is reached past
every contract source above it. The CLR's metadata leaf answers a canon through the repr its
`.clr.fs` binds (`Vesper.int` → `System.Int32` → `Type.IsValueType`) and any other key as a
plain metadata name, so a BCL `System.Guid` answers too and a type the reference set does not
carry abstains; it rides the leaf's SHAPE cache, so its answer and the `Class` shape's own flag
cannot disagree. The JS leaf answers `ValueSome false` for EVERY key — that is the whole of "JS
has no value types".

`Engine.valueLayout` is the single consumer: ONE query, and `negate` for the other polarity.
`primitiveSupports` no longer answers either polarity and is now purely about what a contract
DECLARES; the key list is gone. Its ladder is *platform, then declaration*, and that order is
the design — a `[<Struct>]` is a request the target may erase.

**Four things this doc did not predict, each now in the code:**

1. **The `string` special case dissolved with the list.** It existed only to correct a hardcoded
   set that could name no reference primitive. The CLR says `System.String` is not a value type
   and JS says nothing is, so both polarities now fall out of the one query.
2. **User nominals go through the SAME per-key query, and no target-level "has value types"
   fact was needed.** JS answers for every key, so a `[<Struct>]` record is refused there; the
   CLR's leaf abstains for a compilation-local type and the declaration decides — which makes a
   `[<Struct>]` record satisfy `when 'a : struct` on the CLR, where the old arm refused it
   whatever it declared. An ENUM asks for a value type wherever the target lays one out, so it
   takes the same ladder with `true` as its declaration. `TyTuple`/`TyFun`/`TyUnion`/`TyOr` stay
   hardcoded because they are reference shapes on every target; `UnionTypeInfo` carries no
   `IsValueType`, so struct unions are out until it does.
3. **Value-ness had to be added to `ExternalTypeShape.Record` to survive the freeze.** `Class`
   already carried `Flags.IsValueType`; `Record` carried arity/fields/origin only, so a
   `[<Struct>]` record answered "reference" in every CONSUMING unit — the one place the answer
   is not recoverable from the local registry. `ExternalSymbols.declaredValueType` is the one
   reader of that declaration, shared by the front end and the CLR encoder.
4. **The zero-leaf case is REACHABLE and stays silent AT A PRIMITIVE.** SA composes no platform,
   so both polarities `Defer` there — and a deferred constraint is re-queued on its root and
   never swept into a diagnostic, so a test wanting that verdict must be a corpus program. A
   NOMINAL is different: the declaration answers under a zero-leaf compile, so `ConstraintsTests`
   pins both polarities at a record, a class and an enum.

Six corpus programs pin the primitive matrix in `test/Codegen.Conformance/constraints/`, and
each polarity takes OPPOSITE rows on the two targets except at `string`, which is a reference on
both.

**`ClrEnv.externalIsValueType` is FOLDED onto the query** (`CodegenSymbols.isValueType`, reached
through `ICodegenSymbols.IsValueType`), so the element tag an encoder emits and the verdict the
front end reaches come from one fact. It keeps the bare/arity-suffixed key reconciliation the
rest of that layer needs, and it keeps `false` as its floor because a `VALUETYPE`/`CLASS` tag is
emitted either way. The reason it could not simply BE the CLR source for this axis stands: an
`ExternalTypeShape.Intrinsic` has no `Flags` (`codegen-clr-followups-plan.md:192` logs the
sibling symptom), which is why the platform half of the ladder answers a canon through its repr.

### 4. `Regions.isNonAllocatingPrimitive` → a backend fact

Independent of all the above; land it whenever. Purely a codegen concern — it stamps allocation
tracking and no program's meaning depends on it (`Regions.fs:122-146`). Lowest risk of the four,
and a reasonable place to prototype the query shape.

## Query shape

*Built in step 3 as `IsValueType: TypeKey -> bool voption`. Two constraints below were met by
construction rather than by design: the key is the narrow `TypeKey` — nothing else can HAVE a
layout — and the platform repr is looked up FROM it by the leaf that needs one, so no caller
holds an `IntrinsicPlatform` and the many-to-one repr never keys anything. It carries ONE fact
rather than a record, because step 4's fact is codegen-local and reaches no provider.*

`TypeKey * IntrinsicPlatform -> facts`, with these constraints:

- **Return facts, not a type.** Every call site wants a predicate. A small record with "no
  opinion ⇒ defer" preserves the existing `ValueNone` fall-through in `primitiveSupports`.
- **`TypeKey` is the primary key; the repr is secondary.** `IntrinsicIdentity.Platform` is
  explicitly many-to-one (`ExternalDeclarations.fs:471-472`: "Many-to-one, so it must never drive
  unification") — JS maps `float` and `float32` both to `number` (`RuntimeNames.fs:277-278`). A
  map keyed on the repr string, which is the tempting implementation, conflates them. The repr
  earns its place only for types reached through metadata rather than a contract (the CLR's
  `MetadataSymbols` path).
- **`IntrinsicPlatform`, not `string`.** `decimal` on JS has no `(# … #)`; the `Unsupported` arm
  is where the "not supported on this platform" verdict comes from.
- ~~**Its own interface, not `IExternalSymbolProvider`.** Otherwise every test double sprouts a
  method it has no opinion about.~~ **REVERSED (2026-08-10). The facts go ON
  `IExternalSymbolProvider`.**

  `IExternalSymbolProvider` is the ONLY view a file under analysis has of the outside world.
  That is a property worth keeping, and a second interface spends it to save churn that
  turns out not to exist.

  The test-double premise is false on this tree. Exactly ONE test implements the interface
  (`MemoizeTests`, and it counts calls, so it would forward a stub in one line); `MockBuiltins`
  was deliberately deleted and every other test resolves through real `Vesper.*` contracts
  (`feedback_mockbuiltins_is_a_trap`). A test that needs a synthetic surface builds a
  `KeyIndexedLeaf` — a record of dictionaries with a `KeyIndexedLeaf.empty` default — and
  calls `ofKeyIndexes`. That IS the shared, data-driven double, and a new fact channel joins
  it as one more field with a "no opinion" default.

  The purity argument does not survive either: `IntrinsicReverseCanon` and
  `IntrinsicForwardRepr` are already on `IExternalSymbolStore`, and both are platform-repr
  facts rather than symbol lookups. The interface already carries this kind of answer.

  **The real cost, which the reversed bullet mis-stated:** six `{ new IExternalSymbolProvider … }`
  object expressions live in `ExternalSymbolProviders.fs` (`ofKeyedLeaf`, `stack`,
  `mapProviderTypes`, `withInlineBodies`, `memoize`, and `composite` through `stack`). Each
  gains a forwarding arm. That is decorator churn in ONE file, mechanical and compiler-checked,
  not a cost spread over the test suite.

  **Merge policy: FOLD**, like `mergeReverseCanon` / `mergeForwardRepr`, not first-hit.

  In practice the fold has at most one contributor: platform facts come from the PLATFORM
  provider, which is the backend-injected layer-2 leaf at the bottom of the stack
  (`MetaTailFactory`, the CLR's BCL reflection tail / the JS native leaf). Contract providers
  above it have no opinion. So folding and first-hit agree today — fold is chosen because it
  does not DEPEND on that agreeing, and `MetaTailFactory` returns a `IExternalSymbolProvider
  list`, so "exactly one leaf" is an intent the type does not enforce. Whether the fold can be
  optimised on the strength of that intent is a later question; do not build it in.

  **The zero-leaf case.** `noMetaTail` returns `[]`, and
  `SemanticAnalysis.Tests/TestHelpers.realProvider` uses it — so the entire SA front-end
  suite runs today with NO platform provider. An empty contribution set folds to "no
  opinion", which is `Defer`.

  **That is NOT a hazard for step 1**, though an earlier revision of this bullet said it was,
  and the paragraph below still carries the retraction. Step 1's verdicts are
  contract-PRESCRIBED, and SA composes the real `src/Vesper.*` contract, so SA answers them
  without a platform. The zero-leaf case bites only on axes no contract can state — value-ness
  (step 3) and the null model (step 2).

  **DECIDED (user, 2026-08-12) — and NOT by either option this bullet originally offered.**
  Neither "a compilation asserts it has a platform-facts source" nor "`noMetaTail` is replaced
  by an explicit facts-only leaf" is taken. SA having no platform is CORRECT and stays: a
  synthetic platform leaf built to keep tests green is a stand-in for the real provider, which
  is the trap this tree keeps removing (`feedback_mockbuiltins_is_a_trap`).

  The fold's answer for an empty contribution set is therefore "no opinion", and the tests that
  can no longer be constructed in SA MOVE rather than being propped up. Each is either restated
  to need no platform type, or rewritten into `XParsec.FSharp.Codegen.Common.Tests`, where both
  backends run it against their REAL providers — the `Backend` record
  (`Conformance.fs:37-49`) already parameterises the corpus over exactly that, and
  `Obligation.Diagnose` (`:60`) expresses a target-specific compile-time rejection. See
  `capability-provenance-plan.md` for the full statement of this.

  **The relocation is DONE (2026-08-12), and it did not land on step 1.** `capability-
  provenance-plan.md` holds the sweep. `when 'T : equality` over `int` — the constraint SA
  tests most — stays in SA, answered by the contract; the four `struct` / `not struct` cases
  over primitives are the ones that moved, to `test/Codegen.Conformance/constraints/`. Step 3
  is where this bullet's warning actually applies.

## Frozen cache

Backend-supplied facts driving type-checking verdicts makes the target a determinant of the
frozen tree. `CompilationInputs.Target` (`Hashing.fs:105-107`, folded at `:132`) must stay, even
after `per-target-manifest-plan.md` makes the manifest path target-specific.

## Anchors (verify before editing)

- The key lists step 4 still has to remove: `Inline.fs:70-89`, `Regions.fs:122-131`.
- Constraint verdict table: `primitiveSupports`, `valueLayout`, their dispatch arms and
  `reportConstraintViolation`, in that order down `Engine.fs`.
- Diagnostic (already correct): `Diagnostics.fs:231`, `:406-407`.
- Constraint (callability) path: `Constraint.Struct` → `Translate.fs:569` →
  `SemanticConstraintKind.Struct`; contract-sourced at `VesperLib/TypeTranslate.fs:287`.
- Static-optimization (clause-selection) path, out of scope: `ExpressionParsing.fs:162,169` →
  `InferTypeOps.fs:97-98` → `Inline.fs:116`.
- JS's own statement of the fact: `EmitJsTypes.fs:283`.
- Capability keys for step 1: `RuntimeNames.fs:56-70`; the eq/cmp/disposal anchors are in
  `src/Vesper.Core/prim-types-min.fsi`, the iteration ones in `capabilities.fsi`.
  `CapabilityIdentity` / `.Matches` at `RuntimeNames.fs:131-150`; `CapabilityIds` populated by
  `ExternalSymbols.resolveCapabilities` (`ExternalSymbols.fs:758-780`).
- Capability declaration template: `SignatureParsing.fs:382-413`, `prim-types-array.fsi:11-12`,
  `VesperLib.fs:1223-1239`, `SignatureExtractorTests.fs:1241-1254`.
- The route-dependence being designed AWAY from: `ExternalSymbols.fs:479-489` (`Interfaces`
  field), `VesperLib.fs:407` (filled), `FrozenSignature.fs:419-441` (dropped).
- Capability-surface readers to reroute onto the query: `EngineCore.fs:532-561`
  (`subtypeInterfacesOf` — the hard one, both callers need the full edge set),
  `InferControlFlow.fs:592-600` (`for … in` — an exact fit). Neither is the constraint solver.
- The repr→metadata bridge the CLR answer rides: `EngineCore.fs:450-456`, `:469-478` →
  `InferRecordAccess.fs:35,64`.
- The JS leaf that must gain capability knowledge: `JsNativeSymbols.fs:63`, `:87-90`.
- The value-ness query step 3 shipped: `IExternalSymbolStore.IsValueType`, folded in
  `ExternalSymbolProviders.stack`, answered by `MetadataSymbolProvider` and `JsNativeSymbols`,
  consumed by `Engine.valueLayout` and `CodegenSymbols.isValueType`.
- The declaration half of the ladder: `ExternalSymbols.declaredValueType`, over
  `ExternalTypeShape.Class` / `.Record`.
- User-type value-ness, which the query now gates: `MemberRegistration.fs:530`,
  `TypeRegistration.fs:234-238`.
