# Platform facts — move CLR-shaped primitive classification to the backend

*Its `extern`-classification prerequisite has landed, as has the per-target manifest split; only
the note on `CompilationInputs.Target` below still refers to it. The steps here are ORDERED:
step 1 is a hard prerequisite for step 2, and taking them out of order silently breaks
`when 'T : equality` on JS with no diagnostic.*

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
agree with each other:

| | `Engine.isPrimitiveValueType` (`:107`) | `Inline.isStructPrimitive` (`:70`) | `Regions.isNonAllocatingPrimitive` (`:122`) |
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

### 1. `Equality` / `Comparison` → the provider's capability query

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

### 2. `Nullness` / `NotNull` → the target's null model

Same shape, smaller blast radius (`Engine.fs:455-462`). Today `int` yields `ValueSome false`
("cannot be null"); with `isValueType` false it becomes `Defer`, so `int` goes unchecked for
nullness on JS rather than getting JS's actual answer.

These are meaningful on JS but currently derived from the wrong premise — "value types cannot be
null" is CLR reasoning that happens to produce a plausible JS answer. JS has a real null model:
`null`, plus `undefined` from `prim-types-undefined.js.fsi`. Derive them from it. Note both are
`Defer` at the structural arm (`Engine.fs:582-586`), so the primitive table is the only place
they bite.

### 3. `isPrimitiveValueType` → a backend fact

Once 1 and 2 no longer route through it, the flip is small: the predicate becomes backend-
answered and returns false for every key on JS. `Struct` rejects per-type, `ReferenceType`
accepts, both via the existing table at `Engine.fs:437-462`. The structural arms
(`Engine.fs:576-581`) need review — they hardcode `Struct ⇒ Violated` / `ReferenceType ⇒
Satisfied` for `TyTuple`/`TyFun`/`TyRecord`/`TyUnion`/`TyClass`/`TyOr`, which is right on both
targets today but is the same class of assumption.

Also settle user types at this point: `MemberRegistration.fs:530` computes `isValueType` from
`[<Struct>]` / struct shape. Under "nothing is a struct on JS", a user's `[<Struct>]` record must
fail the constraint too — otherwise a user type satisfies `struct` while `int` does not.

**There is no per-type `isValueType` on any provider to re-route** — this is net-new surface, not
a replacement. `ClrEnv.externalIsValueType` (`ClrEnv.fs:470-473`, exposed at `:599`) is
backend-local, drives only the `VALUETYPE`/`CLASS` signature tag in `ClrEncoder` (`:19`, `:56`,
`:109`), and reads `ExternalClassFlags.IsValueType` off a CLASS shape. An
`ExternalTypeShape.Intrinsic` has no `Flags`, so it answers `false` for every intrinsic and
cannot be the CLR source for this axis as it stands. (`codegen-clr-followups-plan.md:192` already
logs the sibling symptom: it answers `false` for tuples and enums.)

Consider adding `IsValueType` as a flag on TAST types.

### 4. `Regions.isNonAllocatingPrimitive` → a backend fact

Independent of all the above; land it whenever. Purely a codegen concern — it stamps allocation
tracking and no program's meaning depends on it (`Regions.fs:122-146`). Lowest risk of the four,
and a reasonable place to prototype the query shape.

## Query shape

`TypeKey * IntrinsicPlatform -> facts`, with these constraints:

- **Return facts, not a type.** Every call site wants a predicate. A small record with "no
  opinion ⇒ defer" preserves the existing `ValueNone` fall-through in `primitiveSupports`.
- **`TypeKey` is the primary key; the repr is secondary.** `IntrinsicIdentity.Platform` is
  explicitly many-to-one (`ExternalSymbols.fs:381`: "Many-to-one, so it must never drive
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

- The three key lists: `Engine.fs:107-118`, `Inline.fs:70-89`, `Regions.fs:122-131`.
- Constraint verdict table: `Engine.fs:437-462` (`primitiveSupports`); `TyConst` dispatch at
  `:520-524`; structural arms at `:565-586`; report at `:654`.
- Diagnostic (already correct): `Diagnostics.fs:231`, `:406-407`.
- Constraint (callability) path: `Constraint.Struct` → `Translate.fs:569` →
  `SemanticConstraintKind.Struct`; contract-sourced at `VesperLib/TypeTranslate.fs:287`.
- Static-optimization (clause-selection) path, out of scope: `ExpressionParsing.fs:162,169` →
  `InferTypeOps.fs:97-98` → `Inline.fs:116`.
- JS's own statement of the fact: `EmitJsTypes.fs:283`.
- Capability keys for step 1: `RuntimeNames.fs:56-70`; contract in `src/Vesper.Core/capabilities.fsi`.
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
- User-type value-ness for step 3: `MemberRegistration.fs:530`, `TypeRegistration.fs:234-238`.
- Backend value-ness for step 3: `ClrEnv.fs:470-473`, `:599`; `ExternalSymbols.fs:402`.
