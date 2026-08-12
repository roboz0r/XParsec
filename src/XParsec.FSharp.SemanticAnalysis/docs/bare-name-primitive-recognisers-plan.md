# The last bare-name primitive recognisers

**Status (2026-08-12): live plan, not started.** The residue of the contract-sourced intrinsic
identity work, which otherwise landed in full and whose doc was deleted. Delete this one on
landing (`feedback_plan_docs_ephemeral`).

## Principle (carried forward, unchanged)

Strings appear only at **name resolution** — user/bootstrap source → a fully-qualified identity.
After that everything downstream carries `SymbolKey`s and compares by exact `=`. An intrinsic's
identity is **resolved from the `prim-types-*` contract**, never authored from a hardcoded name
set (`feedback_mockbuiltins_is_a_trap`).

That principle now holds everywhere the compiler resolves a type — with two exceptions, below.
Both survive for the same reason: they classify a bare name at a point where **no provider is in
hand**, so neither could be swept onto the resolved-identity path when the rest were.

## The two sites

### 1. `TsManifestTypes.intrinsicOrOpaque` (`Codegen.Js/TsManifestTypes.fs:286-297`)

A manifest may spell a Vesper primitive by its canon name (`float`, `string`, `undefined`), and
that spelling has to mint the `Vesper` key so it unifies with the front end's literal argument.
With nothing to resolve against, the site decides on the name alone against three hardcoded sets:

```fsharp
let isVesperPrimitive =
    RuntimeNames.numericTypeNames.Contains name
    || RuntimeNames.referencePrimitiveNames.Contains name
    || manifestSpellableExtras.Contains name
```

`numericTypeNames` / `referencePrimitiveNames` are `Set<string>` tables (`RuntimeNames.fs:346`,
`:354`). `manifestSpellableExtras` (`:276-279`) is a later, better-behaved addition — it derives
its members from `undefinedKey` / `bigintKey` rather than restating them, so it cannot name a type
that does not exist. It is the shape the other two should reach, but it is still a name set.

The fix is provider access, not a better set. `TranslateCtx` (`:75-96`) carries `Types`, `Refs`,
`ModuleSpec`, `MountPrefix`; `ctx.Resolve` is a `Map` lookup over the manifest's own exports.
Given a provider the site resolves these names through the contract and all three sets go.

`null` must stay off the resolved path either way: it is a language keyword with no `Vesper`
namespace, deliberately registered in no provider, and mints the bare `nullKey`.

### 2. The `undefined` front-end bridge (`Passes/Unification/Translate.fs:382-386`)

```fsharp
| ValueNone when name = RuntimeNames.undefinedTypeName ->
    TyConst(RuntimeNames.undefinedKey, EqArray.empty)
```

`prim-types-undefined.js.fsi` / `.js.fs` exist, so a JS compilation resolves the written name
through the provider and never reaches this arm. It fires only on a stack that has not loaded the
JS contract, where it keeps the written name agreeing with the Freeze/optional-default form.

So this is a fallback for an incomplete stack, not a missing contract. Retiring it is a question
about **what a stack without the JS contract should do with a written `undefined`** — resolve
nothing and diagnose, most likely, now that `unresolvedRefTy` yields a real `Kind.UndefinedType`
verdict (`Translate.fs:47-69`) rather than the opaque `TyConst` it once minted. Settle that before
deleting the arm; it is a behaviour change, not a cleanup.

## Work

1. Thread a provider into `TranslateCtx` and resolve manifest-spelled intrinsics through it.
   Delete `numericTypeNames`, `referencePrimitiveNames` and `manifestSpellableExtras` if nothing
   else claims them — check first, `numericTypeNames` has other readers.
2. Decide the no-JS-contract verdict for a written `undefined`, then delete the bridge arm.

Neither is on the critical path of `capability-provenance-plan.md`.

## Also recorded here

`RuntimeNames.opaqueKey` has one remaining untested front-end fallback,
`MemberRegistration.fs:680`. The `Translate.fs` opaque branch that a deleted coverage note once
asked about no longer exists; its successor is covered by `TypeRefVerdictTests.fs:248-261`.

## Relevant memories

`feedback_mockbuiltins_is_a_trap` (the name set is the trap), `feedback_overwide_types_are_string_keys`
(a `Set<string>` recogniser IS a string key — narrow the sink), `feedback_dynamic_intrinsics_over_du_cases`
(one `TyConst` identity, resolved not authored), `feedback_plan_docs_ephemeral` (delete on landing).
