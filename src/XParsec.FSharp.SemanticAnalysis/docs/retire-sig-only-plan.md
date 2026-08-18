# Delete `sig-only`, and make a bodiless signature unrepresentable

**Goal (user, 2026-08-16):** remove `sig-only` from the `manifest.<target>.toml` schema, and
make "a signature file with no implementation file" a state ANALYSIS CANNOT REPRESENT — not a
declared exemption, and not a verdict derived from content either. Delete this doc when it
lands (`feedback_plan_docs_ephemeral`).

**Status (2026-08-17): every class is DONE — A, A′, B, D, and C (`[<Import>]` + `jsNative`,
see below).** `PairOutcome.Unrepresentable` went with D and `RuntimeServed` with C, so every
`.fsi` in every manifest pairs and every unpaired signature is a hard error. All that remains
of this plan is the mechanical `sig-only` schema removal below.

## Why the key exists at all

`ConformancePass.check` has four acceptance routes for a `.fsi` with no companion. Three are
DERIVED from file content — `Unrepresentable` (every declaration is `extern` or an
abbreviation), `RuntimeServed` (the committed asset exports every declared `val`), and the
"declares nothing" case. `sig-only` is the only DECLARED one, and it exists to OUTRANK the
content split.

A second consequence, easy to miss: an exempted `.fsi` is published to consumers via `files`
but never compiled in its own package, because the unit list is built from `impl`. Nothing
beyond parsing checks it. That is how `printf.fsi` came to declare two of its three `val`s with
a `'State` / `'Residue` the compiler disagreed with, for as long as it existed.

**The headline goes further than the key.** Deleting `sig-only` alone leaves the three derived
routes, and they admit nine more bodiless signatures than the key does. Making the state
unrepresentable means every route goes.

## The inventory: 15 bodiless signatures, 4 classes

Computed by pairing key over every manifest, not from any list. Only the **A** rows are
`sig-only`; the rest are accepted silently by content.

| class | files | answer (user, 2026-08-16) |
|---|---|---|
| **A — owes a body, missing** | ~~`compiler-attributes.fsi` (js)~~, ~~`exceptions.js.fsi`~~, ~~`printf-format.fsi` ×2~~, ~~`printf.fsi` ×2~~ | write the `.fs`; it pairs |
| **A′ — owes a SENTINEL body** | ~~`prim-types-attr.fsi`, `capabilities.fsi` (both js)~~ | write the `.fs`; the repr is a sentinel the backend knows |
| **B — transparent abbreviation** | ~~`capabilities-compat.js.fsi`, `list-bcl.clr.fsi`~~ | write the `.fs`; it pairs |
| **C — served by a runtime asset** | `ops-platform-runtime.js.fsi`, `comparison-runtime.js.fsi` | write the `.fs`: `[<Import(name, "./asset.mjs")>] … = jsNative` (user, 2026-08-17) |
| **D — the js target has no such type** | ~~`prim-types-decimal.fsi`, `prim-types-nativeint.fsi`, `prim-types-nd-array.fsi`~~ | **DONE** — omitted from `manifest.js.toml`; see below |

Every file is classified. C is the only class without an answer.

`list-bcl.clr.fsi`'s `sig-only` entry is **already deleted** — redundant from the day it was
written, since class B was accepted by content anyway. Four suites green after removing it.

## A and B: write the body

All four have landed:

| entry | answer |
|---|---|
| ~~`compiler-attributes.fsi`~~ | **DONE** — `compiler-attributes.fs` is now in the js `impl` list too, and pairs on both targets |
| ~~`exceptions.js.fsi`~~ | **DONE** — `exceptions.js.fs` declares the roster, BCL-shaped under `exn` |
| ~~`printf-format.fsi` ×2~~ | **DONE** — see below |
| ~~`printf.fsi` ×2~~ | **DONE** — deleted from `files`; it declared three of the eight printf names and two of the three disagreed with the compiler |

### What `printf-format.fs` cost, and what it bought (2026-08-16)

The body is four lines: a class over the ctor and the `Value` member the contract declares.
The arity-5 `PrintfFormat<…,'Tuple>` went from the `.fsi` rather than gaining a body — nothing
in the repo names it, and it was the only declaration needing an `inherit`.

What it unlocked is bigger than the pairing. `PrintfSpec.formatType` used to mint the
**FSharp.Core** key while a source-level format ANNOTATION resolved to the Vesper one, and a
predicate admitted both so the two never had to meet. With a real Vesper type there is one key,
that predicate is gone, and with it the last FSharp.Core touchpoint in the CLR backend: the
encoder arm that silently substituted `Microsoft.FSharp.Core.PrintfFormat`4` for
`Vesper.PrintfFormat`4`, the `FSharp.Core` `AssemblyRef`, and the whole `FSharpCoreDependencies`
use-set channel it fed. **That channel could only ever report empty once its one producer went**,
so ~20 `Expect.isEmpty artifact.FSharpCoreDependencies` assertions were tautologies; they now
assert on the PE's `AssemblyRef` table instead, which is the artefact fact they were proxying for.

A probe (a `failwithf` in the encoder arm, whole CLR suite green) proved the arm unreachable by
any covered form. The three forms that DO reach it — a format-typed parameter, return type and
record field — turned out to be broken independently: `sprintf` is a front-end intrinsic with no
runtime, so a format arriving as a VALUE has nothing to apply and codegen throws "no call recipe
for external 'sprintf'". They are pinned as pending tests, and a printf runtime is what closes
them.

### What the two js bodies actually cost (2026-08-16)

Neither was the two-line file the table implies. Writing them forced three fixes, each of
which is a defect the absent file had been hiding:

- **`Attribute` had no declared constructor.** Every `compiler-attributes.fs` type writes
  `inherit Attribute()`, and `prim-types-attr.fsi` declared no `new`. The CLR never noticed
  because a member-less heritable primitive falls back to resolving its base through the
  platform repr, and `System.Attribute` resolves. On js the sentinel resolves to nothing, so
  the fallback failed. `prim-types-attr.fsi` now declares `new: unit -> Attribute`, which is
  what the ten `inherit` clauses were already relying on, and BOTH targets now take the
  ctor-bearing path.
- **A capability's `inherit` chain was invisible once it carried a repr.** `enumerator`
  inherits `disposable`, and `subtypeInterfacesOf` had no `IntrinsicInterface` arm — so
  `e.Dispose()` on an `enumerator<'T>` resolved only via the platform key, i.e. only where the
  repr names a BCL type with its own `Dispose`. The sentinel exposed it; the arm is
  target-neutral.
- **A class over an intrinsic-repr base must not be REJECTED on js.** `EmitJsTypes` hard-failed
  on ANY `inherit`. `Attribute` is constructed nowhere and its sentinel repr names no class, so
  a marker over it is dropped; `exn` resolves to `Error`, so a class over it is emitted and
  extends it. `exnReprOf` split into the climb (`inheritedReprOf`) and the runtime-class check,
  and that split IS the discriminator between the two.

The exception roster is now BCL-shaped and emits real classes — see
[js-exception-identity-plan](js-exception-identity-plan.md).

**B: an abbreviation needs an implementation file.** `SigShape.Abbrev.ImplOptional` was `true`
— not a considered divergence from fsc but a hack, hammered in to work around the same
architecture these plans exist to unravel (user, 2026-08-16). Restating the abbreviation in a
`.fs` matches fsc, and the `Abbrev` route goes with it.

Its doc comment went too, and is worth quoting because it is the shape of the problem: "F#
resolves it transitively to its target, so a sig-only abbreviation (`ref = Ref<'T>`) is
conformant with no `.fs` companion of its own." That reads as a semantic justification and is
in fact a description of what the code happened to do. fsc requires the implementation.

### What B cost (landed 2026-08-16)

**The two-file inventory was an undercount, because the inventory pairs whole FILES and the
`Abbrev` route exempts individual DECLARATIONS.** Retiring the route surfaced seven more
abbreviations sitting inside otherwise-paired files — every one of them already restated by the
CLR body, so the js bodies were the laggards, plus two the CLR omitted as well:

| file | restated |
|---|---|
| `prim-types-int.js.fs` | `int8`, `uint8`, `int32`, `uint` |
| `prim-types-float.js.fs` | `single`, `double` |
| `prim-types-object.js.fs` | `objnull` — the CLR body already had it |
| `core-types.fs` | `ref`, on BOTH targets |
| `printf-format.fs` | `Format`, on BOTH targets |

`ref` and `Format` are the two that mattered: a shared body missing an abbreviation the contract
declares is drift no target was catching.

**`ImplOptional` is gone outright.** Retiring the `Abbrev` route left `ExternInterface` as its
one inhabitant: `prim-types-min.fsi` declares `equatable` / `comparable` / `disposable` as
`extern interface` and js had no body for them — A′ work the class-A′ pass missed, since it went
by whole files and these three sit inside an otherwise-paired one. `prim-types-min.js.fs` now
binds the same `"!" + FQN` sentinels `capabilities.js.fs` does, and the member and its call site
are deleted; `MissingInImpl` is now unconditional on an absent name.

Two `ReferencedProjectTests` pins moved with them, and the direction is the point: the three
capabilities were `Class`-shaped and CANON-ONLY on js, and now publish as `IntrinsicInterface`
keyed by the sentinel with the canon beside it — the same shape `seq` / `enumerator` already had.
Every capability now keys alike on both targets, so "js has no repr here" has no remaining
inhabitant to hide in.

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
runtime — JS has no interfaces.

`IntrinsicTypeMap` needs no guarding: it is derived in `PublishedSurface.ofBuilder` from
published `Intrinsic` shapes and a capability cannot reach it, which is why the CLR's
repr-bound capabilities already stay off the axis.

`NumberCovariance` matching `IntrinsicPlatform.Repr "number"` is the backend side already: a
named token compared against the repr. The sentinel cases are more of the same.

**Correction (landed 2026-08-16): `EmitJs` needed no sentinel match.** The prediction above was
that `EmitJs` keys `for … in` off the ABSENCE of a repr and would have to recognise `!`. It does
not: the iteration lowering keys off `CapabilityIds`, and a capability identity compares on its
canon key as well as its platform key, so gaining a repr moved nothing. The `!` string reaches
no emitter — the one site that writes a repr verbatim (`new <repr>(…)`) is already guarded by
resolving the repr to a runtime class first, and a sentinel does not resolve.

What DID move is upstream and target-neutral: a capability publishes as `IntrinsicInterface`
once it has a repr, and `subtypeInterfacesOf` had no arm for that shape, so `enumerator`'s
`inherit disposable` became invisible. See the A-body notes above.

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

That still holds for CONFORMANCE, and the body is written. The same walk is now the js EMIT
rule, which is a different claim: a class over an intrinsic-repr base emits no declaration
BECAUSE its construction already lowers to the base's repr. Owing a body and emitting one are
separate questions, and only the second is answered by the walk.

## D: a type the target does not have is ABSENT, not declared-and-unimplemented — LANDED 2026-08-17

**What landed, and where it differs from the sketch below:**

- The three `.fsi`s are out of `manifest.js.toml`. The target reaches `PassContext` as
  `CompilingAssembly { Name; Target }` (moved to `PassContext.fs` from `AssemblyFiles`), which
  is now the parameter of the whole `Pipeline.…For` family and of `AnalyseFile`; the
  target-less variants pass `CompilingAssembly.none`.
- **Target-optional is every built-in primitive except the `prim-types-min` trio** (user,
  2026-08-17) — `int` / `bool` / `unit` are the only identities a target MUST declare.
  `RuntimeNames.targetOptionalPrimitiveKeys` derives from `numericKeys` /
  `referencePrimitiveKeys` plus `bigint` / `undefined`, minus the trio, rather than naming
  backends' current gaps. `IntrinsicSet.get` keeps its `failwithf` only for the trio; for a
  target-optional key with no contract it mints the canon `TyConst`. No `voption` and no
  diagnostic at the mint: `PlatformTypes` reports the mention, exactly as it already did for
  a published `IntrinsicPlatform.Unsupported`, now also for a language-known key that
  nothing declares. The diagnostic site and message are therefore unchanged, and the corpus
  pins pass as written.
- A WRITTEN `nativeint` / `unativeint` / `decimal` resolves the same way: the bare-name
  fallback in `Translate.resolveBareTypeName` (previously the `undefined`-only arm) mints the
  key, and NameResolution's `classifyingTypeIter` skips its `UndefinedType` report for these
  names. `undefined` on the CLR now gets `UnsupportedOnTarget` rather than silence, which is
  the same rule with the targets swapped.
- `nativeptr` / `ilsigptr` and `nd-array` are built-in-primitive identities of no list, so
  on js they are plain undefined types; `voidptr` has a key (`referencePrimitiveKeys`) and
  is refused as unsupported. Pinned in `UnsupportedOnTargetTests`.
- `PairOutcome.Unrepresentable` is deleted; an all-`extern` val-less companion-less `.fsi` is
  a `SigWithoutImpl` hard error, pinned by a synthetic-package test in `ConformanceTests`.

The original analysis, kept for the reasoning:

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

## C: `[<Import>]` + `jsNative` (user, 2026-08-17)

`ops-platform-runtime.js.fsi` and `comparison-runtime.js.fsi` are answered by a committed
`.mjs`, and an F# body would be a second, divergent implementation of `structuralEquals`. The
answer is Fable's: each val gets a real `.fs` binding whose implementation is DECLARED to be a
named export of a committed runtime asset —

```fsharp
[<Import("structuralEquals", "./Vesper.Core.mjs")>]
let structuralEquals (x: 'T) (y: 'T when 'T: equality) : bool = jsNative
```

This is the value-level counterpart of the A′ sentinel reprs: an agreed vocabulary between the
`.fs` that binds it and the checker that reads it. It converts `RuntimeServed` — a derived,
whole-file, first-asset-only exemption — into a declared, per-binding contract, and the
bodiless state loses its last inhabitant.

### Decisions

- **`ImportAttribute` lives in `compiler-attributes`** (both targets), the first
  compiler-recognised attribute with a parameterised ctor: `new: memberName: string * from:
  string -> ImportAttribute`. Recognised SYNTACTICALLY (as `[<CompiledName>]` is, via
  `AttributeDecode`), because conformance runs on parsed files with no name resolution.
- **`jsNative` is a real Vesper value**: `val jsNative<'T> : 'T` in a new
  `[<AutoOpen>] module JsInterop` of `ops-platform-runtime.js.fsi`, body a throw-IIFE
  template. `extern`'s C-shaped syntax cannot carry `'T when 'T: equality`, which is why
  Fable invented `jsNative`; the same reason applies verbatim here. The body never emits —
  every use sits under `[<Import>]`.
- **The specifier is `./` + a manifest `runtime` entry**, read relative to the declaring
  package's output directory. `./` is load-bearing: a bare specifier is ESM's npm/node
  resolution, a different feature, and is REFUSED here until something needs it.
- **Emission does not change and does not read the attribute.** A consumer already imports the
  val by name from the package barrel (`./Vesper.Core/index.mjs`, golden-pinned), and the
  barrel `export * from` the asset. Emission relies on binding-name = export-name identity;
  the conformance checks below make that identity enforced instead of coincidental. Wiring the
  attribute into `JsImports` to import the asset file directly would churn goldens for no
  semantic gain.

### The checks (all `ConformanceError`, so no new verdict, wire tag or CodeVersion bump)

CST-local, in `Conformance.checkUnit`:
- `[<Import>]` binding whose body is not exactly `jsNative` — marking a real body deletes it.
- `jsNative` body without `[<Import>]` — a throw with nothing declared to serve it. The
  same both-ways discipline as `[<Global>]`.
- Attribute member name ≠ the binding's emitted name — the emitted import would bind a
  different export than the one declared.
- An `[<Import>]` whose arguments do not parse as two strings.

Package-level, in `ConformancePass` (which holds the manifest):
- The specifier does not name a `[core] runtime` entry of the declaring package, or the file
  is absent.
- The asset's scraped exports (the existing `exportedNames` regex, now applied per named
  asset rather than head-of-list-only) do not contain the member.

`ConformancePass.unpaired` collapses to `SigOnly`: `PairOutcome.RuntimeServed` and the
first-asset scrape are deleted, and the export-rename pin becomes a missing-export
conformance error on the pair instead of a silent demotion of the whole file.

### Out of scope, recorded

`EmitJsContext.printfRuntimeRef` hardcodes key AND module for `structuralFormat` /
`float32ToString` because no front-end symbol resolves to them. `[<Import>]` declarations in
a `Vesper.Printf` runtime `.fs` are the mechanism that deletes that hardcode; not this change.

## The mechanical removal, once every class is clear

Follows `96837bff Remove impl-only as a category from source manifests` exactly.

- **`ReferencedProject`**: the `Manifest.SigOnly` field, the `sig-only` key, its `coreKeys`
  entry, its `parseManifest` arm. Its `sourceInputs` term is already dead — `sig-only` is
  necessarily a subset of `files`. An unknown key becomes a parse error, which is what makes a
  stale manifest fail loudly instead of quietly losing its exemption.
- **`ConformancePass`**: `PackageOutcome.SigOnlyExemptions` and `PairOutcome.SigOnly` — the
  routes go with the state they described. ~~`Unrepresentable`~~ went with D;
  ~~`RuntimeServed`~~ and the `declaredSigOnly` short-circuit went with C.
- ~~**`Conformance`**: `SigShape.ImplOptional` and its doc comment~~ — **DONE** with B.
- ~~**`Intrinsics`**: `IntrinsicSet.get`'s `failwithf`, replaced by an
  `UnsupportedOnTarget`-bearing answer~~ — **DONE** with D, as `getTargetOptional` for the
  target-optional members only; the mandatory ones keep failing loudly.
- **`ConformanceVerdict`**: `StaleSigOnly` / `UnknownSigOnly` / `SigWithoutImpl`, the V240 and
  V243 mappings and their messages, with the `FrozenCodecDiagnostics` wire tags renumbered
  densely and `Cache.CodeVersion` bumped.
- **Manifests**: ~~all of them~~ (done) — no manifest carries the key.
- **Tests**: `ConformanceTests.fs` (the exemption arms and `mkOutcome`'s second parameter — the
  target-asymmetry pin's expected list is already `[]`), `ReferencedProjectTests.fs`,
  `Codegen.Js.Tests/FrozenCodecRoundTripTests.fs`, `Codegen.Clr.Tests/TestHelpers.fs`.

## Two halves, landing at different times

The headline is two claims, and only the first is fully scoped:

1. **Delete `sig-only` from the schema.** Needs class A alone. `Vesper.Core` is done; only
   printf's four entries remain, and nothing else blocks it.
2. **Make the bodiless state unrepresentable.** Needs A, A′, B, D **and C** — all five are
   done. What keeps the state representable now is only `PairOutcome.SigOnly` plus the
   `sig-only` schema, which the mechanical removal deletes.

## What this settles for the fold

[manifest-single-file-list-plan](manifest-single-file-list-plan.md) carries a `SignatureOnly`
case only because this state is representable, so it is claim 2 that matters there. When claim
2 lands the pairing is TOTAL: every unit has an implementation, `impl`-driven ordering is
canonical AND complete rather than canonical for a spine, and the merged list is a mechanical
interleave with no exceptions.

**C is landed**, so the fold no longer waits on anything from this plan beyond the mechanical
removal: the corpus pairing is total today.
