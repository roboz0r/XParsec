# Measure resolution

*Written 2026-09-03, against the code as it stands after same-name type resolution landed:
`resolveType` over per-arity `TypeClaims`, every written type reference carrying a stamped
`TypeRefVerdict`, and `Translate.translateTypeRef` reading that verdict instead of the registry.
The measured carrier (`float` in `float<m>`) is the one written type name still resolved by
name, and the measure (`m`) is not resolved at all. Every F# verdict below was probed with
`dotnet fsi` on 2026-09-03. This compiler's current verdict on each was read through the
SemanticAnalysis pipeline the same day.*

Working document. Ephemeral: delete it when the work lands.

## The rule

In F# a measured numeric type is not a special form. FSharp.Core claims each numeric primitive
TWICE, at arity 0 and at arity 1, and the arity-1 claim's type parameter is of MEASURE kind:

```fsharp
// FSharp.Core/prim-types.fs
type float = System.Double
type float<[<Measure>] 'Measure> = float
```

So `float<m>` is the ordinary per-arity type resolution of `float` at arity 1, followed by a
KIND check of the argument against the parameter, and every verdict follows from that:

| written | F# | why |
| --- | --- | --- |
| `float<m>`, `decimal<m>`, `int64<m>`, `float32<m>` with `[<Measure>] type m` | clean | arity-1 claim, measure param, measure arg |
| `type S = float<m>` then `x: S` | clean | an abbreviation of a measured type |
| `float<m>` with no `m` declared | FS0039 "The type 'm' is not defined" | the measure name resolves like a type name |
| `[<Measure>] type m` AFTER the use | FS0039 | scoping is top-down, as for any type |
| `Box<m>` with `type Box<'a>` | FS0704 "Expected type, not unit-of-measure" | measure arg on a type param |
| `x: m` with `[<Measure>] type m` | FS0704 | a measure in type position |
| `float<int>` | FS0705 "Expected unit-of-measure, not type" | type arg on a measure param |
| `float<m>` with `type m = { A: int }` | FS0705 | the name resolves to a type, not a measure |
| `string<m>` | FS0033 "does not expect any type arguments" | `string` is claimed at arity 0 only |
| `MyFloat<m>` with `type MyFloat = float` | FS0033, alone | the abbreviation is claimed at arity 0 only |
| `float<m>` with a local `type float<'a> = { V: 'a }` | FS0704 | the local arity-1 claim wins, its param is a type |
| `P.m` and `Q.m` both opened, `1.0<m> + 1.0<P.m>` | FS0001 "unit of measure 'P.m' does not match 'Q.m'" | measures are identities, not spellings |
| `[<Measure>] type v = m / s` then `float<v>` against `1.0<m/s>` | clean | a measure abbreviation expands to its term |

`float<'u>` with a measure typar, and `let f (x: float<'u>) = x * x`, are also clean in F#.
They are OUT of this plan's scope, which is name resolution of the carrier and the measure;
see "Semantics to confirm".

## Where the compiler stands

`float<m>` is recognised by SPELLING. `TypeRefStamp.isMeasuredCarrier` matches a single-segment
name in `RuntimeNames.numericTypeNames` applied to one argument, and both classifying walks
(`Scope.classifyingTypeIter`, `TypeRefStamp.stampTypeIter`) skip that shape, so neither the
carrier nor the measure carries a verdict. `Translate.translateType`'s guarded `GenericType` arm
then resolves the carrier through `resolveMeasureCarrier`, a by-name lookup at arity 0, and
`translateMeasure` reads the measure as a bare string into `MeasureTerm`. A measure declaration
`[<Measure>] type m` registers no claim at all.

The same probes through this compiler:

| written | this compiler | verdict |
| --- | --- | --- |
| `float<m>` and the other carriers, with `m` declared | clean | agrees, by accident: `m` was never looked up |
| `type S = float<m>` | clean | agrees |
| `float<m>` with no `m` declared, `= 1.0` | "Dimensionless TyConst ({ Container = … }) used where <m> expected" | GAP 1: no FS0039; the one diagnostic prints a record |
| `[<Measure>] type m` after the use | clean | GAP 1: scoping is not applied |
| `Box<m>` | "The type 'm' is not defined" | GAP 2: the measure is no claim, so FS0039 where F# says FS0704 |
| `float<int>` | not probed | GAP 2: no kind check exists |
| `string<m>` | "The type 'm' is not defined" | GAP 2 again; F# reports the arity, FS0033 |
| `MyFloat<m>` | FS0033, then "'m' is not defined", then the dimensionless message | GAP 2: two diagnostics of noise beside the right one |
| local `type float<'a>` shadowing the primitive | clean | GAP 3: the spelling gate ignores the local claim |
| `P.m` beside `Q.m` | not probed; `MeasureTerm` keys on the string `"m"` | GAP 4: two measures of one name are one measure |
| `[<Measure>] type v = m / s` | not probed; the declaration registers nothing | GAP 2 |

`Codegen.Clr.Tests`, `Codegen.Js.Tests` and `Codegen.Conformance` hold no measured fixture, so
a measured binding has never been run through `Freeze` in a test.
`UnificationEngineCore.resolveStep` (`EngineCore.fs:16`) stops at a measure-bearing root, so
`zonk` leaves it a `TyVar`, and `Freeze.freezeTy` (`Freeze.fs:33`) deep-zonks through that same
function before mapping an unlinked root to `FTUnknown UnresolvedTypar`. GAP 5: `let x = 1.0<m>`
freezes to `FTUnknown UnresolvedTypar` rather than to `float`.

**The `UnresolvedTyVars` backstop does not fire on this**, which the step-1 test pins:
`MeasureResolutionTests`' GAP 5 case asserts no blocking error and that assertion passes today.
`ResolvedTypes.addFreeRoots` (`ResolvedTypes.fs:26`) follows `Link` unconditionally and so walks
a measured root through to `float` and accumulates nothing, while `resolveStep` deliberately
stops there. The two derivations of "resolved" disagree, and the backstop is blind to exactly
the class of value `Freeze` cannot lower. Step 6 therefore cannot lean on it; see step 6.

Three further facts the design depends on:

- `resolveType` (`LongIdent.fs:649`) tries the nearest LOCAL arity before an exact-arity
  external lookup, and `TypeRefStamp.fs:29` is the only FS0033 site. An external name at the
  wrong arity (`string<m>`, `string` being Vesper.Core's) ends `Unresolved`, so no FS0033.
- `CstTypeWalk` (`CstTypeWalk.fs:43`) skips `TypeArg.Measure` and treats `Type.MeasureType` as a
  leaf, and `classifyTypeRef` takes a `CstKeys.TypeRef` derived from a `Type` node. A
  `Measure.Named` has no walk and no key.
- `Scope.fs` never visits `Constant.MeasuredLiteral`; `InferLiterals.fs:66` calls
  `translateMeasure` straight off the CST. The literal `1.0<m>` is not stamped by any walk.

### Root cause

One: the carrier is a syntactic special case instead of a claim. The gate matches by spelling
BEFORE resolution, so it cannot see a local claim of the same name (GAP 3), and its carrier
lookup is the last copy of `resolveType` in the tree, the shape the same-name work spent four
steps deleting elsewhere.

Two: a measure is not a name-table citizen. Nothing registers `[<Measure>] type m`, nothing
resolves a `Measure.Named`, and `MeasureTerm` carries spellings rather than identities. Every
GAP 1, 2 and 4 verdict is a consequence: a name that is never looked up cannot be undefined,
out of scope, of the wrong kind, or distinct from its namesake.

Three: a type parameter has no KIND. `TypeParams: EqArray<string * TyVarId>` records names
only, so even with the two claims in place there is nothing to check an argument against.

## Design

**Model the F# rule directly.** The carrier resolves as any type does; the measure resolves as
any type does; the argument is checked against the parameter's kind. In order of what each
part makes correct by construction:

1. **A type parameter carries a `TyparKind`** (`Type` | `Measure`), read from the `[<Measure>]`
   attribute on `TyparDefn` at registration and carried on every typar list this pass models
   (`TypeParams`, every generic `ExternalTypeShape` case, the published contract). The kind
   check at an argument then has a fact to check against, and the frozen blob and its codec
   change with it. This is the one part that touches the publishing format.
   `ExternalTypeShape.Abbrev` (`ExternalSymbols.fs:13`) carries an arity and no typar list, and
   the arity-1 primitive is exactly that shape, so it gains the kind array too.

2. **A measure declaration is a claim**, `TypeDeclKind.Measure`, registered at arity 0 for the
   body-less `[<Measure>] type m` (parsed as `TypeDefn.AbstractType`, whose attributes are
   reachable through `Attributes.attributesOfTypeName`) and for the abbreviation
   `[<Measure>] type v = m / s`. A `Measure.Named` site, in a type (`float<m/s>`) or in a
   literal (`1.0<m>`), is stamped through `resolveType` like any other written name, which
   needs a measure-term descent in `CstTypeWalk`, a `CstKeys` key for a measure name, and a
   `Constant.MeasuredLiteral` visit in `Scope`. `translateMeasure` then reads the stamp: a
   `Measure` claim contributes its key, an abbreviation claim expands to its term, an
   unresolved name is FS0039 at the measure token, and a claim of another kind is FS0705.
   `MeasureTerm` keys on `TypeKey`, so `P.m` and `Q.m` differ (GAP 4). `Kind.MeasureMismatch`'s
   `string * string` pair becomes the two keys, printed as declared paths.

3. **`src/Vesper.Core` declares the arity-1 primitives**, transliterating
   `FSharp.Core/prim-types.fs` with the usual `// FSharp.Core/prim-types.fs:NNNN` references,
   for every key in `RuntimeNames.numericKeys`. The claims then reach a use site through the
   same referenced-assembly leg as `float` itself, at arity 1, and a local `type float<'a>`
   outranks them exactly as it outranks the arity-0 claim (GAP 3).

4. **The special form is deleted.** `isMeasuredCarrier`, `resolveMeasureCarrier`,
   `isNumericCarrier` and `numericTypeNames` go, and the two walks stop skipping the shape.
   `translateType`'s `GenericType` arm reads the stamped verdict and splits the written
   arguments by the claim's typar kinds: a type-kinded parameter takes `translateType` of the
   argument, and a measure-kinded parameter takes `translateMeasure` of it. The parser's
   ambiguity (`float<m>` lands `m` as `TypeArg.Type(NamedType)`, `float<m/s>` as
   `TypeArg.Measure`) is resolved by the parameter's kind, so a single-segment `NamedType` in
   a measure-kinded position is read as `Measure.Named`. A measure claim in a type-kinded
   position is FS0704; a type in a measure-kinded position is FS0705; a wrong count on a
   local name stays FS0033 through `LocalTypeAtOtherArity`, which is how `MyFloat<m>` reports
   the arity and nothing else. `string<m>` needs an external nearest-arity leg in
   `resolveType` to report FS0033; until that leg exists its row is `ptest`.

5. **The representation is unchanged.** A reference to a measure-kinded claim yields the
   measured `TyVar` that `InferLiterals.inferConst` already builds for `1.0<m>`: `Link` to the
   expansion of the claim's body, `Units` to the translated term. The unifier, `Engine`'s
   measure reconciliation and `MeasuresTests` keep their shape. Extending `Link`/`Units` to a
   measure-generic abbreviation (`type Meters<[<Measure>] 'u> = float<'u>`) is the measure-typar
   work this plan excludes.

6. **A measured root freezes to its carrier.** `Freeze` lowers a `TyVar` whose `Units` are set
   to its `Link`, so the frozen type of `let x = 1.0<m>` is `float` (GAP 5). Neither backend
   reads a measure.

Two alternatives, rejected:

- *Keep the spelling gate and route only the carrier through the stamped verdict at arity 0.*
  Closes nothing but the by-name lookup; GAP 1, 2 and 4 stay, and GAP 3 is only half closed
  because the gate still fires before the local arity-1 claim is consulted.
- *A `SemType`/`FrozenType` case for a measured type.* The project rule prefers a real named
  type plus recognisers over a new DU case with unifier behaviour; the measured `TyVar` is
  already that named type.

## Staged plan

The `Vesper.Core` primitives (step 3) come before the measure claims (step 4): step 4 deletes
the two walk guards, and a walk that classifies `float` at arity 1 needs the arity-1 claim to
exist. `TyparKind` (step 2) comes before the primitives because the `[<Measure>]` typar
attribute must register as a kind before the primitives declare it.

**Step 1 — pin the semantics. LANDED.** `MeasureResolutionTests.fs` sits beside
`SameNameResolutionTests.fs`, one case per row of the two tables above, each quoting its F#
verdict under its FS code, `ptest` where this compiler diverges, plus one full-pipeline case
(`analyseFor`, through `Freeze`) for a measured `let` pinning GAP 5, plus one `.fsi`/`.fs` pair
declaring a name at two arities. The red surface is the deliverable: **13 of 16 are `ptest`**,
each verified to fail when enabled. The nine first estimated here counted only the rows of
"Where the compiler stands", which omits the `x: m` and `type m = { A: int }` rows of the rule
table; those two, the GAP 5 pin, and one case added beyond both tables — an undeclared measure
at the LITERAL (`let x = 1.0<m>`), which `fsc` reports as FS0039 exactly as it does in an
annotation — make up the other four.

The three green cases are load-bearing. The two-arity `.fsi` pair proves the claims stay
distinct across the signature match: with `T` and `T<'a>` both published, a consumer writing
`t.X` on a `T<int>` is refused with "Type 'Test.A.M+T\`1' has no field or member 'X'".

**Step 2 — `TyparKind`. LANDED.** `TyparKind` and `DeclaredTypar` (name, prototype TyVar,
kind) sit in `SemanticScalars.fs`. `DeclaredTypar` is the ONE shape a name-plus-prototype
typar list takes, whichever axis it lands on: a type's `TypeParams`, a member's `SeedTypars` /
`EffectiveMethodTypars`, `GeneralizedTypars`, and a binding's `Bindings.DeclaredTypars`.
`declaredTyparsOfTypeName` reads `[<Measure>]` off each `TyparDefn` through
`ctx.ResolveAttributes` against `RuntimeNames.measureAttributeKey` and mints the prototypes in
one step; `typarKindsOfTypeName` is the kinds-only read, for a publisher whose form registers
no typar list. `TTypeDeclG.TypeParams` is `EqArray<TTypeParam>` (name + kind) with its own
codec pair. Each generic `ExternalTypeShape` case carries `EqArray<TyparKind>` in place of its
arity, and `.TyparArity` derives from it, so the two cannot disagree. Reddened nothing; five
`MeasureResolutionTests` cases pin the attribute read, the kind reaching the frozen contract,
and the kind reaching the `extern` and opaque published shapes.

Three findings against the step as written:

- A MEMBER's or VALUE's own typars are type-kinded by construction, at `mkMethodTypars` and at
  `Infer.fs`'s binding-typar read. A `[<Measure>]` method typar is measure-typar work, which
  this plan excludes; those two sites are where it would be read.
- `arityOfTypeName` runs while types are being CLAIMED, before the registry can resolve an
  attribute, so it counts typar slots and the kind read is a separate function called at
  registration or later.
- There is no codec version to bump: the frozen blob carries no version stamp and no
  `Vesper.*` package blob is stored, so every consumer rebuilds from source.

`TyparKinds.typeOnly` remains for the surfaces with no `[<Measure>]` to read: a CLR metadata
row, a TypeScript declaration, an intrinsic `(# … #)` binding's structural typars.

**Step 3 — the arity-1 primitives in `Vesper.Core`. LANDED.** One
`type T<[<Measure>] 'Measure> = T` per numeric key, verbatim from `FSharp.Core/prim-types`
with its line references, across four `.fsi`/`.fs` pairs named for the arity-0 file each
mirrors: `prim-types-int-measured`, `prim-types-float-measured` on both targets,
`prim-types-decimal-measured` and `prim-types-nativeint-measured` on CLR alone, as their
arity-0 contracts already are. FSharp.Core's `[<MeasureAnnotatedAbbreviation>]` has no Vesper
counterpart; the parameter's kind carries that fact. The four aliases FSharp.Core also
declares measured (`double`, `single`, `int8`, `uint8`, `int32`, `uint32<'M> = uint<'M>`) are
measure-GENERIC abbreviations, which this plan excludes, so the port carries the thirteen
canonical keys alone.

Two facts the step as written did not anticipate, each a front-end gap it surfaced:

- **The measured claims cannot sit in the file that declares the arity-0 carrier.**
  `[<Measure>]` on a typar is resolved through `ctx.ResolveAttributes`, so
  `MeasureAttribute` must precede the use; `compiler-attributes` comes after every
  `prim-types-*` file, because the attribute classes need `int`, `string` and `Attribute`.
  Hence the separate files, listed immediately after `compiler-attributes.fs`.
- **THREE compiler changes were needed after all**, each making name resolution per-ARITY
  where it was per-NAME:
  - `ScopeContents.composite.TypesNamed` (`ExternalSymbols.fs:229`) took the first source's
    non-empty arity set. Each unit of the compiling assembly is its own source, so
    `prim-types-int-measured` shadowed `int` outright for every later file of Vesper.Core and
    `IntrinsicSet` failed with "intrinsic 'int' is not resolvable". It now unions the sources'
    arity sets, the nearest source winning each arity it declares, which is F#'s name env
    keyed by demangled name AND arity.
  - `resolveType` (`LongIdent.fs:645`) settled a name on a LOCAL claim at any arity before
    consulting the contracts, so the RHS of `type int<[<Measure>] 'M> = int` read as the
    arity-1 claim being declared: FS0033 plus "involves an immediate cyclic reference", on
    every one of the thirteen. The external exact-arity leg now runs before the
    nearest-arity local fallback, leaving `LocalAtOtherArity` — and `MyFloat<m>`'s FS0033 —
    for a name no contract claims at the written arity.
  - `typesIn` (`LongIdent.fs:200`), the DOTTED-name counterpart of `resolveType`, returned
    this file's claims alone whenever it held any, at whatever arity, and in registry order
    while its consumer takes the head expecting the narrowest. `Test.A.Tag.Item` in a unit
    claiming `Tag<'a>` therefore reached the arity-1 case rather than the arity-0 one an
    earlier unit published. It now merges the two sides per arity, ascending.

Reddened nothing else. Thirteen `MeasureResolutionTests` cases pin both claims of each
numeric key, at arity 0 and at arity 1 over a measure parameter, read from the real
Vesper.Core contract.

**Step 4 — measure claims. LANDED.** `TypeDeclKind.Measure` claims both `[<Measure>]` shapes:
the body-less `TypeDefn.AbstractType` and the abbreviation, whose `m / s` body does reach
`TypeDefn.Abbrev(typ = Type.MeasureType …)` and whose single-name body (`type v = m`) reaches
`Type.NamedType`, the measure retry never firing with no operator after the name.
`MeasureInfo` holds the body and forces it on first reference, as `AbbreviationInfo` does.
A measure ATOM applies the same `CstKeys.TypeRef` a bare type name does, so `float<m>`'s
argument keys alike whether the parser spelled it `TypeArg.Type` or `TypeArg.Measure`;
`CstTypeWalk.iterMeasure` and a `VisitMeasureName` hook carry both walks to it, and
`iterExprEmbeddedTypes` presents `1.0<m>`'s annotation as the `Type.MeasureType` an
abbreviation body writes, so one arm reaches both. `translateMeasure` reads that stamp.
`MeasureTerm` keys on `TypeKey`; `Kind.MeasureExpected` (FS0705) and
`Kind.TypeExpectedNotMeasure` (FS0704) joined it with their codec entries. `MeasuresTests`
gained a three-measure prelude, as predicted.

Nine `MeasureResolutionTests` cases went from `ptest` to green, two more than the step
predicted: four of the five KIND rows, not just `Box<m>` and `x: m`. The `Translate` gate still
reads `float<m>`'s single-segment argument as a measure atom, and the atom read is where a
claim of another kind is caught, so `float<int>` and `float<m>` against `type m = { A: int }`
report FS0705 without the typar-kind split step 5 adds. The four still `ptest` are step 5's
(the local generic `float`, `string<m>`, `MyFloat<m>`) and step 6's (GAP 5). Four findings:

- **A PARSER fix was needed.** `parseBody`'s peek after the type name
  (`TypeDefnParsing.fs:1419`) was not optional, and it fails on a block-closing dedent, so a
  body-less `type m` ENDING a module body failed as a module element: the body closed EMPTY
  and the declaration re-parsed at file level. Two modules each holding only
  `[<Measure>] type m` therefore both claimed under the enclosing NAMESPACE and collided with
  "Duplicate type definition". The peek is now `opt`, and neither `with` nor `=` following is
  the abstract form.
- **FS0039 reports at EVERY undefined measure occurrence.** `dotnet fsi` aborts on the first
  error, so its single-diagnostic output is no evidence of deduplication. Two step-1 fixtures
  wrote the undefined name in both the annotation and the literal; each now writes it once,
  and the literal keeps the case of its own.
- **The measure diagnostics keep their `string` payloads, RENDERED from the terms.** Two keys
  is the wrong shape — a mismatch is between two TERMS, and only the one-atom case is a pair
  of keys — but carrying `MeasureTerm` costs more than the shape: `Kind` is deliberately
  free of the file's type tables, which `FrozenCodecRoundTripTests`' "every Kind case
  round-trips" asserts by reading every kind back against an EMPTY table, and an interned
  `TypeKey` breaks that. `TypeKey.DeclaredPath` is the new rendering, so `MeasureTerm.ToString`
  prints `P.m` against `Q.m` and the identity reaches the message without the key reaching the
  blob.

**Step 4a — A measure declared in another file or assembly is not published.** `ExternalTypeShape` has
  no measure case, so a cross-unit `[<Measure>]` reads as undefined (FS0039). Nothing in the
  tree declares one and no test covers it; publishing it is the same shape of work step 2 did
  for `TyparKind`.

**Step 5 — delete the special form.** The four names above, the guarded `GenericType` arm and
its `TypeArg.Measure`-on-a-non-numeric-carrier sibling. The remaining `GenericType` arm reads
the stamped verdict, takes the claim's typar kinds (local `TypeParams` or external shape), and
splits arguments by kind. Closes GAP 3 and the rest of GAP 2 except `string<m>`, whose FS0033
needs an external nearest-arity leg in `resolveType`; add the leg here if it is a one-arm
change, else leave the row `ptest` and record why.

**Step 6 — freeze a measured root to its carrier.** `Freeze.freezeTy` lowers a `TyVar` whose
`Units` are set to its `Link` (GAP 5), and the step-1 full-pipeline pin goes green.

`InternalBreak.UnresolvedTyVars` cannot serve as the check that no measured type reaches
`Freeze` unlinked, because `addFreeRoots` follows `Link` past a measured root. Align the two
readings here: `addFreeRoots` takes the same measure-aware step `resolveStep` takes, so a
measured root that step 6 fails to lower is caught rather than walked through. Until then, the
step-1 pin on the frozen TYPE is the only thing that would notice.

**Step 7 — wording.** `Engine.fs`'s `Kind.Message(sprintf "Dimensionless %A used where <%O>
expected" …)` becomes `Kind.DimensionlessMeasureMismatch`, which already exists two lines
away, so the record printout in the GAP 1 row is gone.

## Scope and risk

Steps 1, 5, 6 and 7 are SemanticAnalysis-only. Step 4 adds two diagnostic kinds and one
`TypeDeclKind` case, each with a codec entry. Step 2 is the one that changes what a referenced
assembly publishes, so its blob and contract versions bump together and every stored
`Vesper.*` package rebuilds. Step 3 was expected to be runtime-port-only, and was the step
most likely to surface a front-end gap: the `[<Measure>]` attribute on a typar in a `.fsi` had
never been through `SignatureResolution`, and a name claimed at two arities had never crossed
the signature/implementation match. The attribute went through clean; the two arities did not,
and the two per-arity resolution fixes are recorded under the step.

Neither backend reads a measure today, and after step 6 neither should: a measured `TyVar`
freezes to its carrier.

## Semantics to confirm

1. **Measure typars are excluded.** `float<'u>`, `[<Measure>] type Area<[<Measure>] 'u>` and
   generic measured functions stay `NotYetSupported` from `translateMeasure`, as now. This
   plan makes their later modelling a matter of `TyparKind.Measure` on a typar rather than a
   new mechanism, but does not do it.
2. **Qualified measure names (`SI.kg`) resolve off the stamp** since step 4: `Measure.Named`
   reads its verdict whatever the segment count, and `MeasureResolutionTests` pins `M.m`.
3. **`1.0<m>` requires a declared `m`.** Every `MeasuresTests` fixture that omits the
   declaration changes to include it. F# is strict here; the fixtures were pinning the
   unlooked-up name.
4. **Step 3 follows FSharp.Core's current set** of measure-annotated primitives (all the
   integer types, `float`, `float32`, `decimal`, `nativeint`, `unativeint`).
   `RuntimeNames.numericKeys` already lists exactly those thirteen, so the port carries all of
   them rather than the float/decimal subset older FSharp.Core shipped.
5. **`string<m>` may stay `ptest`** after step 5 if the external nearest-arity leg is more
   than a one-arm change.

## Migration checklist

Before this document is deleted, each row is in code or in a test:

- [ ] Every table row above is a test in `MeasureResolutionTests.fs`, green, or `ptest` with the reason quoted in its name (`string<m>` is the one row allowed to stay `ptest`).
- [ ] The full-pipeline measured `let` freezes to its carrier (GAP 5), green.
- [ ] `ResolvedTypes.addFreeRoots` and `UnificationEngineCore.resolveStep` agree on whether a measure-bearing root is resolved, so the `UnresolvedTyVars` backstop covers a measured root.
- [x] Every key in `RuntimeNames.numericKeys` is claimed by `Vesper.Core` at arity 1 with a measure-kinded parameter, beside its arity-0 claim.
- [x] A written name resolves per ARITY on both routes: `resolveType` for a type-position spelling, `typesIn` for a dotted one.
- [x] `TyparKind` is on the typar model, on every generic `ExternalTypeShape` case, and in the contract. A shape published from a source `TypeName` reads its kinds from that name; only a surface with no `[<Measure>]` to read (CLR metadata, TypeScript, an intrinsic binding) uses `TyparKinds.typeOnly`.
- [x] `MeasureTerm` carries `TypeKey`; no `string` measure name survives past the parser, the measure diagnostics' rendered payloads excepted (see step 4).
- [x] `1.0<m>` and `float<m/s>` both stamp `m` through the classifying walk; `translateMeasure` reads only the stamp.
- [ ] `isMeasuredCarrier`, `resolveMeasureCarrier`, `isNumericCarrier`, `numericTypeNames` are deleted, and neither `classifyingTypeIter` nor `stampTypeIter` skips a shape.
- [ ] `TypeRefStamp.fs`'s doc no longer describes a shape no walk stamps.
