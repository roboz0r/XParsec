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
a measured binding has never been run through `Freeze` in a test. `EngineCore.zonk`
(`EngineCore.fs:22`) keeps a measure-bearing root as a `TyVar`, and `Freeze.freezeType`
(`Freeze.fs:47`) deep-zonks through that same function before its `UnresolvedTyVars` backstop.
So a measured root that is not scheme-bound reaches `Freeze` unlowered today. GAP 5: the freeze
of a measured binding is an `InternalBreak`, not the carrier.

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

**Step 1 — pin the semantics.** A `MeasureResolutionTests.fs` beside `SameNameResolutionTests.fs`,
one case per row of the two tables above, each quoting its F# verdict under its FS code,
`ptest` where this compiler diverges, plus one full-pipeline case (`analyseFor`, through
`Freeze`) for a measured `let` pinning GAP 5, plus one `.fsi`/`.fs` pair declaring a name at
two arities, since signature-to-implementation matching of two claims under one name has not
been exercised. The red surface is the deliverable; expect nine of fifteen red.

**Step 2 — `TyparKind`.** The DU, the attribute read at `TypeRegistration.typarNamesOfTypeName`'s
site (rename to carry the kind; `SignatureResolution.Members.fs:319` shares it), the
`TypeParams` element, the kind array on every generic `ExternalTypeShape` case including
`Abbrev`, the contract writer and reader (`FrozenCodecDecls.writeTypeDecl`), the codec
version. No behaviour change: every existing typar is `Type`. Reddens nothing, or a
frozen-blob-size fixture at most.

**Step 3 — the arity-1 primitives in `Vesper.Core`.** `prim-types-float.fs`/`.fsi` and the
integer and decimal files, one `type T<[<Measure>] 'Measure> = T` per numeric key, on both
targets. No compiler change. Reddens nothing on its own, because the spelling gate still
intercepts `float<m>` ahead of resolution.

**Step 4 — measure claims.** `TypeDeclKind.Measure`; registration of the two `[<Measure>]`
declaration shapes (the body-less form is `TypeDefn.AbstractType`; confirm that
`[<Measure>] type v = m / s` reaches `TypeDefn.Abbrev(typ = Type.MeasureType …)` through the
retry at `TypeDefnParsing.fs:1397` before writing the arm); the `Measure.Named` stamp, which
is a measure-term descent in `CstTypeWalk`, a `CstKeys` key for a measure name, a
`Constant.MeasuredLiteral` visit in `Scope`, and a `classifyTypeRef` arm for the measure
site; deletion of the two walk guards in `Scope.classifyingTypeIter` and
`TypeRefStamp.stampTypeIter`, so `m` inside `float<m>` is stamped while the `Translate` gate
still resolves the carrier; `translateMeasure` off the stamp; `MeasureTerm` over `TypeKey`
and `Kind.MeasureMismatch` over two keys; `Kind.MeasureExpected` (FS0705) and
`Kind.TypeExpectedNotMeasure` (FS0704) with their codec entries. Closes GAP 1 and 4, and the
`Box<m>` and `x: m` rows of GAP 2. `MeasuresTests`' literal cases that write `1.0<m>` with no
`m` declared redden, as predicted; each gains the declaration.

**Step 5 — delete the special form.** The four names above, the guarded `GenericType` arm and
its `TypeArg.Measure`-on-a-non-numeric-carrier sibling. The remaining `GenericType` arm reads
the stamped verdict, takes the claim's typar kinds (local `TypeParams` or external shape), and
splits arguments by kind. Closes GAP 3 and the rest of GAP 2 except `string<m>`, whose FS0033
needs an external nearest-arity leg in `resolveType`; add the leg here if it is a one-arm
change, else leave the row `ptest` and record why.

**Step 6 — freeze a measured root to its carrier.** `Freeze.freezeType` lowers a `TyVar` whose
`Units` are set to its `Link` (GAP 5), and the step-1 full-pipeline pin goes green. The
`InternalBreak.UnresolvedTyVars` backstop is then the check that no measured type reaches
`Freeze` unlinked.

**Step 7 — wording.** `Engine.fs`'s `Kind.Message(sprintf "Dimensionless %A used where <%O>
expected" …)` becomes `Kind.DimensionlessMeasureMismatch`, which already exists two lines
away, so the record printout in the GAP 1 row is gone.

## Scope and risk

Steps 1, 5, 6 and 7 are SemanticAnalysis-only. Step 4 adds two diagnostic kinds and one
`TypeDeclKind` case, each with a codec entry. Step 2 is the one that changes what a referenced
assembly publishes, so its blob and contract versions bump together and every stored
`Vesper.*` package rebuilds. Step 3 is runtime-port-only and is the step most likely to
surface a front-end gap, because the `[<Measure>]` attribute on a typar in a `.fsi` has never
been through `SignatureResolution`, and a name claimed at two arities has never crossed the
signature/implementation match.

Neither backend reads a measure today, and after step 6 neither should: a measured `TyVar`
freezes to its carrier.

## Semantics to confirm

1. **Measure typars are excluded.** `float<'u>`, `[<Measure>] type Area<[<Measure>] 'u>` and
   generic measured functions stay `NotYetSupported` from `translateMeasure`, as now. This
   plan makes their later modelling a matter of `TyparKind.Measure` on a typar rather than a
   new mechanism, but does not do it.
2. **Qualified measure names (`SI.kg`) stay `NotYetSupported`** through step 4, though the
   stamp makes them a one-arm addition afterwards.
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
- [ ] `TyparKind` is on the typar model, on every generic `ExternalTypeShape` case, and in the contract; no consumer re-derives it from an attribute.
- [ ] `MeasureTerm` carries `TypeKey` and `Kind.MeasureMismatch` carries two keys; no `string` measure name survives past the parser.
- [ ] `1.0<m>` and `float<m/s>` both stamp `m` through the classifying walk; `translateMeasure` reads only the stamp.
- [ ] `isMeasuredCarrier`, `resolveMeasureCarrier`, `isNumericCarrier`, `numericTypeNames` are deleted, and neither `classifyingTypeIter` nor `stampTypeIter` skips a shape.
- [ ] `TypeRefStamp.fs`'s doc no longer describes a shape no walk stamps.
