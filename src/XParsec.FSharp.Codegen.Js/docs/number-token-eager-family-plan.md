# Eager `number` family union in contravariant positions

**Status (2026-08-27): follow-up, not started.** Recorded when the bare-name primitive
recogniser landed. Delete on landing (`feedback_plan_docs_ephemeral`).

## Current state

`NumberCovariance.wrap` resolves the TS `number` token by variance (`NumberCovariance.fs`):
covariant → `float` (the honest JS-repr read type — a TS `number` may be `0.5`, so a
disjunctive read type claiming `int` would be wrong); invariant → the eager
`FTOr [int; float; float32]` family union from `canonsOf "number"`; contravariant → the token
survives into the published signature, and the unifier converts it to the family `TyOr` lazily
at the argument seam (`numericFamilyOr`, `Engine.fs:150-159`).

## The change

Make the contravariant case eager too: `Variance.Contra -> ValueSome familyUnion`, so no
platform token survives past the provider boundary and only Vesper identities circulate.
Absorption already treats an expected `TyOr` as an absorbing shape (`Engine.fs:187`), so
argument acceptance should be unchanged. `numericFamilyOr` then has no remaining source of
platform-named `TyConst`s and is deleted, together with the `PlatformName` pattern if this was
its last reader.

## Verify before landing

1. **Diagnostics**: a mismatch against a `number` parameter will print
   `int | float | float32` instead of `number`, no longer matching the TS signature the user
   reads. Decide whether that rendering is acceptable.
2. **Member/overload identity**: if any lookup keys structurally on parameter `FrozenType`s
   (the ArgSig idiom of `MetadataSymbols.fs:70-72`), rewriting `number` → `FTOr` in published
   signatures changes those keys. Grep before committing.
3. Negative control: with the eager conversion in place, disable `numericFamilyOr` and show
   the argument-seam tests stay green; re-enable nothing.

## Relevant memories

`feedback_overwide_types_are_string_keys` (a surviving platform token is a string key),
`feedback_plan_docs_ephemeral` (delete on landing).
