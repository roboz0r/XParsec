# Platform facts — move CLR-shaped primitive classification to the backend

*Depends on `extern-is-self-evident-plan.md`. Independent of `per-target-manifest-plan.md`
except for the note on `CompilationInputs.Target` below. The steps here are ORDERED: step 1 is a
hard prerequisite for step 2, and taking them out of order silently breaks `when 'T : equality`
on JS with no diagnostic.*

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

**`when ^T : struct` as a STATIC OPTIMIZATION is a different mechanism and is out of scope.**
`StaticOptimizationConstraint.WhenTyparIsStruct` (`ExpressionParsing.fs:162,169`) →
`InferTypeOps.fs:97-98` → `TStaticOptConstraint.IsStruct` → `Inline.fs:116` selects clauses; it
does not gate callability. No Vesper source uses that form — the only `: struct` in
`src/Vesper.*` is an unrelated struct-tuple return in `SemiPersistentUnionFind.fs:136` — so
`Inline.isStructPrimitive` (`:70-89`) has no live producer. Delete it or leave it; it must not
weigh in the design.

## Ordering (the risk is entirely in step 1)

### 1. `Equality` / `Comparison` → the capability contract

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

These are contract facts, not platform facts: `equatable<'T>` and `comparable<'T>` are declared
capabilities in `capabilities.fsi`, with keys already minted (`RuntimeNames.fs:66-70`). A
primitive supports equality because the contract says it implements the capability. Route the
verdict there. This step must be complete and tested before step 2 begins.

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

  **The zero-leaf case is the real hazard, and it is live.** `noMetaTail` returns `[]`, and
  `SemanticAnalysis.Tests/TestHelpers.realProvider` uses it — so the entire SA front-end
  suite runs today with NO platform provider. Once step 1 routes `Equality` / `Comparison`
  through facts, "nobody had an opinion" folds to no opinion, which is `Defer`, which is
  exactly the silent non-resolution this plan opens by warning about — landing on every SA
  test at once rather than on JS.

  So the fold needs a defined answer for an empty contribution set, and it must be LOUD: either
  a compilation asserts it has a platform-facts source, or `noMetaTail` is replaced by an
  explicit facts-only leaf. Decide this BEFORE step 1, not during it.

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
- User-type value-ness for step 3: `MemberRegistration.fs:530`, `TypeRegistration.fs:234-238`.
