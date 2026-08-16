# Delete `sig-only`, and make a bodiless signature unrepresentable

**Goal (user, 2026-08-16):** remove `sig-only` from the `manifest.<target>.toml` schema, and
make "a signature file with no implementation file" a state ANALYSIS CANNOT REPRESENT — not a
declared exemption, and not a verdict derived from content either. Delete this doc when it
lands (`feedback_plan_docs_ephemeral`).

**Status (2026-08-16): scoped, gated.** The inventory below is the new part; the four gates
were already known. One fifth entry has already gone.

## Why the key exists at all

`ConformancePass.check` has four acceptance routes for a `.fsi` with no companion. Three are
DERIVED from file content — `Unrepresentable` (every declaration is `extern` or an
abbreviation), `RuntimeServed` (the committed asset exports every declared `val`), and the
"declares nothing" case. `sig-only` is the only DECLARED one, and it exists to OUTRANK the
content split.

A second consequence, easy to miss: an exempted `.fsi` is published to consumers via `files`
but never compiled in its own package, because the unit list is built from `impl`. Nothing
beyond parsing checks it. That is how the `printf.fsi` drift in
[printf-contract-plan](printf-contract-plan.md) survived.

**The headline goes further than the key.** Deleting `sig-only` alone leaves the three derived
routes, and they admit nine more bodiless signatures than the key does. Making the state
unrepresentable means every route goes.

## The inventory: 15 bodiless signatures, 4 classes

Computed by pairing key over every manifest, not from any list. Only the **A** rows are
`sig-only`; the rest are accepted silently by content.

| class | files | answer (user, 2026-08-16) |
|---|---|---|
| **A — owes a body, missing** | `compiler-attributes.fsi` (js), `exceptions.js.fsi`, `printf-format.fsi` ×2, `printf.fsi` ×2 | write the `.fs`; it pairs |
| **A′ — owes a SENTINEL body** | `prim-types-attr.fsi`, `capabilities.fsi` (both js) | write the `.fs`; the repr is a sentinel the backend knows |
| **B — transparent abbreviation** | `capabilities-compat.js.fsi`, `list-bcl.clr.fsi` | write the `.fs`; it pairs |
| **C — served by a runtime asset** | `ops-platform-runtime.js.fsi`, `comparison-runtime.js.fsi` | **deferred** — representation still to be decided |
| **D — the js target has no such type** | `prim-types-decimal.fsi`, `prim-types-nativeint.fsi`, `prim-types-nd-array.fsi` | omit from `manifest.js.toml` entirely |

Every file is classified. C is the only class without an answer.

`list-bcl.clr.fsi`'s `sig-only` entry is **already deleted** — redundant from the day it was
written, since class B is accepted by content anyway. Four suites green after removing it.

## A and B: write the body

**A** is the four gated entries, unchanged:

| entry | answer | gated on |
|---|---|---|
| `compiler-attributes.fsi` | gets a `compiler-attributes.fs` on both targets and pairs | [attribute-representation-plan](attribute-representation-plan.md) |
| `exceptions.js.fsi` | gets an `exceptions.js.fs` declaring the inheritance hierarchy, and pairs | [js-exception-identity-plan](js-exception-identity-plan.md) |
| `printf-format.fsi` ×2 | write the body | [printf-contract-plan](printf-contract-plan.md) |
| `printf.fsi` ×2 | delete from `files`, or make it load-bearing | [printf-contract-plan](printf-contract-plan.md) |

**B: an abbreviation needs an implementation file.** `SigShape.Abbrev.ImplOptional` is `true`
(`Conformance.fs:51-56`) — not a considered divergence from fsc but a hack, hammered in to work
around the same architecture these plans exist to unravel (user, 2026-08-16). Restating the
abbreviation in a `.fs` is two lines each and matches fsc, and `ImplOptional` goes with it.

Its doc comment goes too, and is worth quoting because it is the shape of the problem: "F#
resolves it transitively to its target, so a sig-only abbreviation (`ref = Ref<'T>`) is
conformant with no `.fs` companion of its own." That reads as a semantic justification and is
in fact a description of what the code happened to do. fsc requires the implementation.

### A′: the sentinel-repr cases (user, 2026-08-16)

`prim-types-attr.fsi` and `capabilities.fsi` are declared on js and have no js body, so today
they are `Unrepresentable` and the types come out canon-only — a plain interface `Class` with
no platform spelling, and `Attribute` with no repr at all. The absence of the file is what
says "js has nothing to bind here", which is the same evasion as everywhere else in this plan.

Each gets a body whose repr is a **sentinel the backend recognises and lowers specially**,
rather than a platform type name it resolves:

```fsharp
// capabilities.js.fs
type enumerator<'T> = (# "!Vesper.Collections.enumerator" #)
type seq<'T> = (# "!Vesper.Collections.seq" #)

// prim-types-attr.js.fs — heritable class form, as the CLR's `(# class "System.Attribute" #)`
type Attribute = (# class "!Vesper.Attribute" #)
```

**The sentinel is `"!" + FQN` (user, 2026-08-16).** `!` cannot appear in a JS identifier, so a
sentinel can never collide with a real global — and it is already the convention here:
`prim-types-array.fs` binds `(# "!0[]" #)`.

Three near-misses, recorded so nobody re-picks one: `$` IS a legal JS identifier character;
`#` is private-field syntax since ES2022; and `` ` `` already carries arity INSIDE CLR reprs
(`IEnumerable`1`), so reusing it as a prefix would make the two ambiguous.

No typars in the string. Arity rides `IntrinsicId.TyparArity` on the identity already, and CLR
spells it `` `1 `` — a third spelling of one fact is what the sentinel is meant to avoid.

The failure mode is the point: today's js reprs (`"number"`, `"Error"`, `"Object"`) are names
the backend emits directly, so a missed case emits a PLAUSIBLE WRONG GLOBAL. A `!` repr that
reaches emit is a JS syntax error at parse time.

**A repr is an agreed vocabulary between the `.fs` that BINDS it and the backend that READS
it** — nothing more. `!` has one job, guaranteeing no collision with a JS global; it does not
have to mean the same thing everywhere it appears. So matching stays WHOLE-STRING, as
`NumberCovariance` already compares `IntrinsicPlatform.Repr "number"`, and no prefix rule has
to be made total.

`"!0[]"` is therefore fine as it stands (user, 2026-08-16): an IL artefact the `.fs` and both
backends already agree on. It is not an exception to a scheme, because there is no scheme to be
an exception to — and respelling it `"Array"` would be churn for nothing.

The mechanism already exists and the CLR proves it: `capabilities.clr.fs` binds
`(# "System.Collections.Generic.IEnumerable`1" #)` and `prim-types-attr.clr.fs` binds
`(# class "System.Attribute" #)`. What changes on js is only that the string names nothing at
runtime — JS has no interfaces — so `EmitJs` must match the sentinel where it currently keys
off the ABSENCE of a repr, lowering `for … in` to the JS iteration protocol as it does now.

`IntrinsicTypeMap` needs no guarding: it is derived in `PublishedSurface.ofBuilder` from
published `Intrinsic` shapes and a capability cannot reach it, which is why the CLR's
repr-bound capabilities already stay off the axis.

`NumberCovariance` matching `IntrinsicPlatform.Repr "number"` is the backend side already: a
named token compared against the repr. The sentinel cases are more of the same.

### The `exceptions.js.fsi` correction

The emit path makes its body look optional, and it is not.
`JsExternalMembers.exnReprOf` walks `FrozenBaseType` until it reaches a type carrying an
intrinsic repr. Each roster entry `inherit exn`, and `exn = (# class "Error" #)`, so
`new FormatException("x")` already emits `new Error("x")`. Conformance's check is syntactic and
one level deep where the backend's is semantic and transitive — which is why the file passes
today with nothing behind it. That is why the exemption went UNNOTICED, not a reason to keep
it.

**Do not take the transitive walk as a rule.** "Reaches an intrinsic repr ⇒ no body owed" is
sound on JS and unsound on the CLR, where `compiler-attributes.fsi`'s chain also reaches a repr
(`prim-types-attr.clr.fs`) but the `.fs` is genuinely required — you cannot `newobj` a TypeDef
you never emitted.

## D: a type the target does not have is ABSENT, not declared-and-unimplemented

`prim-types-{decimal,nativeint,nd-array}.fsi` come out of `manifest.js.toml`. A `.fsi` that
exists only to go unimplemented is the same evasion as `sig-only`, one level down.

The verdict survives the deletion because the language already knows these types independently
of any manifest. `Token.NumDecimal` and `Token.NumNativeInt` are lexer tokens, and the
Token → `TypeKey` relationship is hardcoded: `RuntimeNames.decimalKey` / `nativeintKey`,
`BuiltinTypes.tyDecimal` / `tyNativeInt`, and `InferLiterals` mapping `NumericKind.Decimal` to
`ctx.Intrinsics.Decimal`. **A known `TypeKey` never declared in code IS the target not
supporting it** — which is a stronger statement than an `extern` with no repr, because it
cannot be forgotten from a manifest.

`nd-array` needs no token argument: nothing in the language forces it to be known, so it is
simply absent.

### The blocker to clear first: the target must reach `PassContext`

`IntrinsicSet.get` (`Intrinsics.fs:38-51`) **`failwithf`s** when an intrinsic does not resolve:

> `intrinsic '%s' is not resolvable from the prim-types contract in scope`

That path is unreachable today only because every manifest declares every prim-type. Omitting
these three makes `1.0M` on js reach it, turning a compile error into a crash — and the
conformance corpus pins `diagnose = { js = "decimal is not supported on the js target" }`,
which is `Kind.UnsupportedOnTarget`, an ERROR-severity diagnostic.

**This is bigger than it looks, and the reason is worth stating.** Today the target name
reaches that diagnostic through the PUBLISHED CONTRACT: `bindExternRepr` puts it on
`IntrinsicPlatform.Unsupported target`, sourced from `SignatureInputs.Target`, and
`MemberRegistration` reads it back off the published shape. Delete the `.fsi` and there is no
published shape to carry it — so the target has to come from somewhere else, and `PassContext`
does not have one.

Threading it means: `PassContext`'s constructor (~20 call sites, nearly all tests already
passing `""` for the assembly name), `AssemblyFiles.AnalyseFile` — today
`string -> IExternalSymbolProvider -> OriginSource -> ImplementationFile -> FrozenPools`, where
the `string` is the assembly name — and the `Pipeline.analyseFor` / `analyseSemWithContextFor`
chain between them. `SignatureResolution.resolveFile` already holds `inputs.Target` and is free.

**No safe partial exists.** Making the target-optional intrinsics answer `voption` without the
target removes the crash but leaves nothing to report: no `TyUnknown` site in the front end is
paired with a diagnostic, so `1.0M` on js would go from a loud crash to a vague failure or
silence. Do the whole thing or none of it.

Two notes for whoever does: `Decimal`, `NativeInt` / `UNativeInt` (reached only through
`OfIntWidth`) and `Undefined` are the TARGET-OPTIONAL members — `Undefined` is JS-only and
would `failwithf` on the CLR today if anything asked. The rest (`Int`, `Unit`, `Bool`,
`String`, …) are declared by every target, so a missing one IS an internal break and should
keep failing loudly. And `AnalyseFile` is restructured by
[manifest-single-file-list-plan](manifest-single-file-list-plan.md) step 3, so either do this
first and accept it is touched again, or fold it into that step.

`prim-types-attr.fsi` and `capabilities.fsi` are NOT this case: js HAS these types, it just has
no BCL name for them. They are A′ above.

The line between D and A′ is whether the target has the CONCEPT. `decimal` on js is a number
format JS cannot represent at all; `seq<'T>` is iteration, which JS does have and spells
differently. D deletes the declaration; A′ keeps it and names the difference.

## C: deferred

`ops-platform-runtime.js.fsi` and `comparison-runtime.js.fsi` are answered by a committed
`.mjs`, and an F# body would be a second, divergent implementation of `structuralEquals`. How
that is best represented is still open (user, 2026-08-16) — **do not design it here**, and do
not fold it into `Unrepresentable`'s replacement by default.

C is now the ONLY class keeping the bodiless state constructible, so it alone gates the
type-level half of the headline.

## The mechanical removal, once every class is clear

Follows `96837bff Remove impl-only as a category from source manifests` exactly.

- **`ReferencedProject`**: the `Manifest.SigOnly` field, the `sig-only` key, its `coreKeys`
  entry, its `parseManifest` arm. Its `sourceInputs` term is already dead — `sig-only` is
  necessarily a subset of `files`. An unknown key becomes a parse error, which is what makes a
  stale manifest fail loudly instead of quietly losing its exemption.
- **`ConformancePass`**: `PackageOutcome.SigOnlyExemptions`, the `declaredSigOnly`
  short-circuit, and `PairOutcome.SigOnly` / `Unrepresentable` / `RuntimeServed` — all four
  routes go with the state they described. `Unrepresentable` goes with D; `RuntimeServed`
  cannot go until C is answered.
- **`Conformance`**: `SigShape.ImplOptional` and its doc comment, once B lands.
- **`Intrinsics`**: `IntrinsicSet.get`'s `failwithf`, replaced by an
  `UnsupportedOnTarget`-bearing answer — the prerequisite for D.
- **`ConformanceVerdict`**: `StaleSigOnly` / `UnknownSigOnly` / `SigWithoutImpl`, the V240 and
  V243 mappings and their messages, with the `FrozenCodecDiagnostics` wire tags renumbered
  densely and `Cache.CodeVersion` bumped.
- **Manifests**: `Vesper.Core/manifest.js.toml`, `Vesper.Printf/manifest.{clr,js}.toml`.
- **Tests**: `ConformanceTests.fs` (the exemption arms, `mkOutcome`'s second parameter, and the
  target-asymmetry pin, whose expected list goes to `[]`), `ReferencedProjectTests.fs`,
  `Codegen.Js.Tests/FrozenCodecRoundTripTests.fs`, `Codegen.Clr.Tests/TestHelpers.fs`.

## Two halves, landing at different times

The headline is two claims, and only the first is fully scoped:

1. **Delete `sig-only` from the schema.** Needs class A alone — the four gated entries.
   Nothing else blocks it.
2. **Make the bodiless state unrepresentable.** Needs A, A′, B, D **and C**, which is deferred
   by decision. A′, B and D are ready to start and gated on nothing.

## What this settles for the fold

[manifest-single-file-list-plan](manifest-single-file-list-plan.md) carries a `SignatureOnly`
case only because this state is representable, so it is claim 2 that matters there. When claim
2 lands the pairing is TOTAL: every unit has an implementation, `impl`-driven ordering is
canonical AND complete rather than canonical for a spine, and the merged list is a mechanical
interleave with no exceptions.

**Do not let the fold wait indefinitely on C.** If C stays open, the fold should proceed
carrying `SignatureOnly` and delete the case when claim 2 lands — a case with no inhabitants is
cheaper to remove than a missing one is to add back.
