# Conformance at the TAST level

**Status: every stage has landed and Gaps 1 to 6 are closed.** Conformance
now runs down one route, `AssemblyAnalysis.conformSignature`, over the two analysed halves. The
sections below are the plan as written, each stage carrying what it landed; the two routes and
the CST rule set they describe are history.

## Root cause

`.fsi`↔`.fs` conformance ran down two routes, because only one of them had analysed halves to
compare.

| Route | Entry | What it holds | Rules applied |
|---|---|---|---|
| In-assembly | `AssemblyAnalysis.conformSignature`, live under `Publication.InAssembly` | the signature's `PublishedSurface` and provider, and the implementation frozen | `ConformanceSurface` + `ConformanceTypars`, over resolved identities |
| Package / manifest | `ConformancePass.checkManifest` → `check` | parse results only (`ReadFile<ParsedSignature>`) | the CST rule set, plus the manifest half of the `[<Import>]` check |

The CST rule set re-derives, off syntax, facts that name resolution and Freeze have already
established — type identity, value identity, compiled names, attribute identity. Two
derivations of one fact, which is the shape this repo treats as wrong by default. Stage 1
retired the in-assembly copy; the manifest route still runs it, and Stages 2a and 3 remove it.

The costs that remain, on the manifest route only:

- **Attribute identity is matched on the long ident's last segment.** `attributeShortName`
  cannot tell `[<Import>]` from `[<MyOwn.Import>]`, and a shadowing declaration goes unnoticed.
- **Value identity is a string off a pattern.** `boundName` plus `checkValuePresence` compare
  raw identifier text, so a `[<CompiledName>]`'d binding reads as absent.

## What the CST rule set checks, and where each landed

| Check | On the CST (manifest route) | On the analysed halves (in-assembly route) |
|---|---|---|
| Type presence (`MissingInImpl`) | `summariseSig`/`summariseImpl`, name strings | `ConformanceSurface.checkTypes`, by `TypeKey`. Delegates are refused at their declaration — Gap 2 |
| `extern`/repr pairing, heritability | CST species off `TypeSignature`/`TypeDefn` | `PublishedSurface.DeclaredReprs` against `Residue.IntrinsicReprKeys` |
| Value presence (`checkValuePresence`) | identifier text | `ConformanceSurface.checkValues`, by `BindingKey` |
| Typar count/order | not checked — nothing frozen to compare | `ConformanceTypars.checkFile` for module values; a member's scheme is part of its body verdict (Gap 5) |
| `[<Import>]` well-formedness, selector vs emitted name | `summariseImports`, last-segment match | `Attributes.declareImportBinding`, keyed on `RuntimeNames.importAttributeKey` |
| `jsNative` body | `Conformance.isJsNativeBody` | the same reader, called during elaboration. Both retire for the `nativeOnly` sentinel — Stage 2a.1 |
| Module decl path | `sigDeclPath`/`implDeclPath` | the same two, called from `conformSignature` |

## Staged plan

**Stage 1 — the in-assembly route adopts surface-vs-surface rules. LANDED.**
`ConformanceSurface.checkTypes`/`checkValues` take type presence, the `extern` ↔ repr pairing
and value presence off `r.Surface` and `impl.Frozen`, by resolved identity;
`conformSignature` no longer calls `Conformance.checkUnit`. The `[<Import>]` checks left
conformance altogether for `Attributes.declareImportBinding`, which reads the attribute by
`RuntimeNames.importAttributeKey`. The package route keeps the CST rule set.

Acceptance evidence: `Codegen.Clr.Tests` compiles every `Vesper.*` package through the
in-assembly route and `PackageConformance` runs the CST rule set over the same corpus, both
green, so the two rule sets agree on the real contracts. `AnalysedConformance` in
`SemanticAnalysis.Tests/ConformanceTests.fs` drives each finding through the analysed route,
including the two cases the CST rules get wrong: a `[<CompiledName>]`'d `let` matching the
`val` it publishes as, and a shadowing local `ImportAttribute` that is not the compiler's.

**Stage 1a — the TAST carries type ABBREVIATIONS. LANDED.** `TTypeKindG.Abbrev of body: 'ty`
holds the resolved RHS; `ElaborateTypeDecls.tryAbbrevType` emits it from the registry entry the
group close fills, `FrozenSignature.toSurface` publishes it as `ExternalTypeShape.Abbrev`, and
`ConformanceSurface.demandsDeclaration` treats it like any other nominal. See "Gap 1".

Acceptance evidence: `AssemblyFilesTests` compiles `type myalias = int` in an unsigned `.fs`
against a second file annotating with it, plain and generic; `AnalysedConformance` reports a
`.fsi` abbreviation the `.fs` omits and accepts a matching pair.

**Stage 2 — SUPERSEDED. The package route is deleted, not upgraded.** As written this stage
routed `ConformancePass.check` through `AssemblyFiles.foldUnits`, keeping a second entry point
onto the same verdicts. The in-assembly route already takes every pairing and presence verdict
by resolved identity for a compiled assembly, so a second entry buys nothing; the package route
goes instead. Stage 2a below is what it costs to get there.

**Stage 2a — lift `[<Import>]` to a Vesper-level concept. LANDED**, in the order below.
`ImportResolution` and `IRuntimeModules` live in `ReferencedProject.fs`, with `EsmModules.create`
as the JS implementation and `RuntimeModules.unsupported` as the CLR's; 2a.4's `AssetMissing`
landed as `ResolvedRuntimeAssets.Missing`, a `PackageSetFault` reported once against the
manifest rather than once per importing binding.

`ConformancePass.checkImport` is the one part of the package route that is not a second
derivation. It reads the manifest, not the CST: the path must be `./` plus a `[core] runtime`
asset of the declaring package, and that asset's ESM source must export the declared selector
(`ConformancePass.exportedNames` scrapes it). Nothing else in the tree reads `[core] runtime`
for this. `Attributes.declareImportBinding` covers the other four import verdicts —
`JsNativeWithoutImport`, `ImportMalformed`, `ImportBodyNotJsNative`, `ImportSelectorMismatch` —
and has no access to the manifest, so `ImportUnknownAsset` and `ImportMissingExport` have no
second home.

This check is why the CST rule set is still standing. `checkUnit` supplies its bindings through
`summariseImports`, which pulls in `boundName`, `findAttribute`, `tryCompiledName` and
`stringLiteralText`, so the "provisional, wrong level" attribute reader stays alive to serve it.
Discharge the check elsewhere and the rest of the rule set falls out with no verdict lost.

`[<Import>]` is the value-level counterpart of `extern`/`(# … #)`: the binding is declared in
Vesper and represented by the target. `ImportAttribute` is already declared target-neutrally in
`Vesper.Core/compiler-attributes.fs`, which both manifests list — the concept is neutral already
and only the checking is JS-branded.

**2a.1 — the body marker becomes an intrinsic sentinel.** `jsNative` is JS-branded, is declared
in the JS-only `Vesper.Core/js-interop.js.fs`, and is matched by IDENTIFIER TEXT:
`Conformance.isJsNativeBody` compares `nameOf tok` against `"jsNative"`, while the attribute
beside it resolves through `RuntimeNames.importAttributeKey`. A shadowing `let jsNative = 42`
therefore satisfies the body check — the defect the attribute reader was fixed for, still live on
the other half of the rule. It is replaced, beside `ImportAttribute` in `compiler-attributes.fs`,
by

```fsharp
let inline nativeOnly<'T> : 'T = (# "$use-import-attribute" : 'T #)
```

recognised by resolved key, which closes the shadowing hole. A `failwith` body would instead ship
a live throw for a condition that is a compiler fault.

The repr is a sentinel every backend refuses, CLR included: the declaration is target-neutral, so
a CLR build compiles it whether or not any CLR file carries an `[<Import>]`. The refusal is an
`InternalBreak` case — `ImportBodyNotDischarged of binding: string` — reported where the backend
would otherwise emit the repr, in place of emitting it. It convicts the compiler rather than the
source, because the user error it might be mistaken for is caught upstream: `nativeOnly` written
without `[<Import>]` is obligation 1's converse and fails during analysis. Reaching a backend
therefore means an `[<Import>]` that analysis accepted went undischarged.

Both backends already read value-level `(# … #)` bodies to emit them — the JS one emits the
sentinel's text today, as `js-interop.js.fs` shows — so the refusal is a case added where that
read happens, not a new mechanism.

**2a.2 — the neutral obligations, and one interface per target.** Five obligations, of which
three are target-neutral outright and stay in `Attributes.declareImportBinding`: the body is the
sentinel, the attribute carries two non-empty string literals, and the selector equals the emitted
name. A target importing by name is what the third assumes, which ESM and Python both satisfy.

The other two ask the same question of every target and answer it differently — an ESM specifier
is relative and carries an extension where a Python one is dotted and carries none, and an ESM
module publishes through `export` where a Python module publishes its top-level bindings, filtered
by `__all__`. They become a target's own answer to two questions:

```fsharp
/// A target's module system, as an `[<Import>]` binding is checked against it.
type IRuntimeModules =
    /// The manifest-listed asset a written import path denotes.
    abstract Resolve: path: string -> ImportResolution
    /// The names an asset publishes to an importer.
    abstract Provided: asset: RuntimeAsset -> Set<string>
```

The interface is declared here, beside the check. `Codegen.Common` holds no part of this: ESM and
Python module resolution share no code, and a classifier the analysis needs cannot live
downstream of it.

**2a.3 — analysis records the obligation; the backend discharges it.** `Provided` reads asset
files, and analysis stays deterministic, so the pass does not hold an `IRuntimeModules`. Instead
`declareImportBinding` records each well-formed import — selector, path, emitted name, and the
anchor it already minted — as a per-unit side product, modelled on `AnalysedUnit.Bodies`. The
backend, which reads and writes files to emit anyway, resolves each record against its own module
system and reports through the stored anchor, so the verdict keeps its position in the `.fs` where
today it is unpositioned against the package.

Nothing is re-derived: the pass has the selector and path resolved, and hands them on rather than
leaving the backend to re-read the CST. The record does not enter `FrozenFileResidue` and the
codec is untouched, because an import is discharged in the assembly that declares it and a
reference package's imports were discharged when it was built.

Mediating the read through the provider was considered and rejected: it puts the same filesystem
read under analysis with an indirection in front of it. The cost of the split is that a target
with no emission step never discharges 4 and 5, so a check-only invocation would want the backend
half run explicitly.

**2a.4 — the asset verdicts deconflate.** `ImportUnknownAsset` currently answers three distinct
failures with one message, and `Resolve` separates them:

```fsharp
[<RequireQualifiedAccess>]
type ImportResolution =
    /// Not a module reference this target can read.
    | Malformed
    /// Well-formed, and names no asset in the manifest's `[core] runtime` list.
    | NotListed
    /// Listed in `[core] runtime`, and absent on disk.
    | AssetMissing of asset: string
    | Resolved of RuntimeAsset
```

`ImportUnknownAsset` retires for `ImportPathMalformed`, `ImportAssetNotListed` and
`ImportAssetMissing`; `ImportMissingExport` survives unchanged, reported when `Provided` omits the
selector. `AssetMissing` is a manifest fault rather than an import fault — a `runtime` entry
absent on disk is broken whether or not a binding imports it — so it reports once against the
manifest instead of once per importing binding.

**Stage 3 — delete the CST rule set and the package route. LANDED.** `ConformancePass.fs` went
entirely. `Conformance.fs` lost `summariseSig`/`summariseImpl`/`check`/`summariseSigVals`/
`summariseImplVals`/`boundName`/`checkValuePresence`/`summariseImports`/`checkUnit`, the shape
DUs, and the provisional attribute reader; `ConformanceError`, `describe` and the
`sigDeclPath`/`implDeclPath` pairing check survive. `isJsNativeBody` goes with `jsNative` in
Stage 2a.1, so identifier-text matching leaves the import rule and `attributeShortName`-style
matching then exists nowhere.

Both costs it carried are now settled:

- **The JS corpus keeps a whole-package check.** `JsCorpusConformanceTests` in
  `Codegen.Js.Tests/JsPackageTests.fs` runs every `Vesper.*` js manifest through
  `Frontend.analyse` and `AnalysedAssembly.gate`, so each package takes the same
  `conformSignature` and `[<Import>]` verdicts a compile takes, imports discharged against the
  committed assets.
- **`PairParseFailure` and `SigWithoutImpl` are retired.** Both were producerless once
  `enforce` went, and both name a state the pipeline no longer reaches: the manifest read
  refuses an unpaired `.fsi`, and a half that fails to parse leaves its unit `Failed` and
  reports `Kind.ParseFailure` in its own text. The codec is renumbered accordingly (0
  `Unimplemented`, 1 `ModulePairingMismatch`, 2 `SignatureNotPublished`, 3 `SignatureRejected`,
  4 `AttributeArgumentsDiffer`), and the round-trip fixture now carries one value per surviving
  case, which it did not before.

## Gaps between the TAST and a full conformance check

A `.fs` decides what it publishes, and the frozen pools are the compiler's record of that
decision. Where the pools omit a declaration, every consumer has to go back to the syntax —
conformance here, `FrozenSignature.toSurface` for the next file in the assembly, and the
package extractor. Each gap below is therefore a hole in the frozen record, sited where the
declaration is dropped rather than where a reader notices.

### Gap 1 — a type abbreviation reaches no frozen declaration. CLOSED by Stage 1a.

`TTypeKindG` had cases for `Record`, `Union`, `Class`, `Interface` and `Enum` and none for an
abbreviation, and `Elaborate.TypeDecls` returned `None` for a `TypeDefn.Abbrev` whose RHS is not
`(# … #)`, so `type myalias = int` in a `.fs` produced no `TDecl` at all. Because
`FrozenSignature.toSurface` publishes what `Decls` holds, an abbreviation declared in an
unsigned `.fs` was invisible to the next file of the same assembly. Two files,
`type myalias = int` then `let f (x: myalias) = x`, reported:

```
two.fs: The type 'myalias' is not defined
two.fs: internal compiler error: the frozen TAST holds 1 unresolved TyVar(s)
```

The abbreviation was reachable only through a `.fsi`, which is why the `Vesper.*` corpus never
hit it: every package file has one.

`TTypeKindG.Abbrev of body: 'ty` now carries the RHS, over the `Declaring` typar axis a use
site instantiates against. `ElaborateTypeDecls.tryAbbrevType` reads it off the registry entry
the group close forces and declines a cyclic abbreviation, whose fill already reported;
`FrozenSignature.toSurface` publishes it as the `ExternalTypeShape.Abbrev` a `.fsi` publishes;
`ConformanceSurface.demandsDeclaration` no longer excepts it. Both backends ignore the kind,
because every use site expanded to the body.

### Gap 2 — a `delegate` declaration claims no identity. CLOSED: refused at its declaration.

`SigDecl.claimedKind` yields `ValueNone` for a delegate, so a `.fsi` publishes
`ExternalTypeShape.Unmodelled(Delegate, arity)` in place of a type. The decision was between
refusing a delegate where it is written and giving it a `TTypeKindG` case with an
`ExternalTypeShape` to match; the refusal is what landed, matching how every other unmodelled
construct is treated.

Both halves report `NotYetSupported "\`delegate\` type declarations"` at the declaration:
`Validation.fs:234` for an implementation, `SignatureResolution.reportRefusedDeclaration` for a
signature. `ConformanceSurface.demandsDeclaration` excepts the `Unmodelled` shape, so a pair of
delegates and a `.fsi`-only delegate both take a refusal per written declaration and no pairing
verdict.

Acceptance evidence: two `AnalysedConformance` tests, one per shape, in
`SemanticAnalysis.Tests/ConformanceTests.fs`.

### Gap 3 — attribute arguments are compared nowhere. CLOSED.

(Relocated from the deleted fsi-front-end-plan, 2026-08-28.)

**The rule, probed against fsc.** The premise this gap was blocked on is settled: an attribute
is expected on *neither* half in particular. Compiling `.fsi` / `.fs` pairs through
`FSharp.Compiler.Service` gives, per position:

| Both halves write it | Verdict |
|---|---|
| arguments fold to the same values | silent; `1` against `0x1` and `A ||| B` against `B ||| A` both pass |
| arguments fold to different values | **warning FS1200**, and the signature's copy is compiled |
| one half alone writes it | silent, and that half's copy is compiled |

The same warning lands on a value, a type declaration, a record field and a `[<Literal>]`.
`[<Sealed>]` is separate — it changes the compiled shape, so a disagreement is FS0296/FS0297 at
error severity, not FS1200. That settles the trade recorded here: FOLDED values, so
`AttributeFold`'s `TConstValue` output is what the comparison runs on.

**What landed, for module-level values.** Both halves now carry their folded attributes:
`ModuleBindingInfo.Attributes` on the implementation (folded once in
`Elaborate.translateModuleLet`, which already classified the binding's `AttrTarget`, and
serialised beside the binding's identity), and `ExternalSymbol.Attributes` on the surface
(filled by `SignatureResolution.registerValSig` for a `.fsi`, and by `FrozenSignature.toSurface`
from the frozen record for an unsigned `.fs`). `ConformanceSurface.checkValues` answers both
value questions off one walk of the implementation — presence, and the arguments of an
attribute both halves wrote — as a `ValueConformance`, whose two lists carry the two
severities; `conformSignature` reports the second as
`ConformanceVerdict.AttributeArgumentsDiffer`, a `V247` at WARNING severity, because fsc
compiles the pair.

Attributes are matched by resolved identity and compared by folded ARGUMENT VALUE: positional
arguments in written order, named arguments by name, so `[<Foo(1, Y = 2, X = 3)>]` and
`[<Foo(1, X = 3, Y = 2)>]` conform. Occurrences of one attribute type compare as an ordered
run, which is what an `AllowMultiple` attribute needs; no attribute in `Vesper.Core` is both
`AllowMultiple = true` and writable on a module value, so that path carries no test.

Both halves classify a module-level value's `AttrTarget` through `AttrTarget.ofModuleValue`,
which is the only place the fsc rule is written. The facts it takes are each half's own — a
`.fsi`'s written argument groups, a `.fs`'s inferred type — so `val f: (int -> int)` classifies
as a value while its `let f x = …` companion classifies as a function. The split reaches
`[<AttributeUsage>]` enforcement and nothing else, and is sited on `ofModuleValue`.

`ModuleBindingInfo` moved out of the `SideTypes.fs` grab-bag into its own file after
`AttributeVerdicts.fs`, which is what makes `TAttributes` available to it.

**What landed, for type declarations (2026-09-05).** A type declaration's folded attributes
travel on the surface, not the shape: `PublishedSurface.AttributesByKey` is a table beside
`ShapesByKey`, filled through `PublishedSurfaceBuilder.addAttributes`. The shape was the wrong
carrier, because the only reader of a record, union, enum or abbreviation's attributes is
the `.fsi` ↔ `.fs` comparison, which reads through a surface; a shape served by a metadata or
TS-manifest provider would have carried an `Attributes` field it could not fill, and empty
would have meant two things. `ExternalClassShape.Attributes` stays, because `AttributeFold`
reads an attribute type's `[<AttributeUsage>]` off a class shape through the provider; the
class's attributes are filed on the table as well.

`Enum` and `Abbrev` were the tuple cases of `ExternalTypeShape`, and are now
`ExternalEnumShape` (`Cases`, `Underlying`, `Origin`) and `ExternalAbbrevShape` (`Typars`,
`Body`). `Record` and `Union` were already records; the premise recorded here that they held
five positional fields was stale. The projection module of the same name as the enum record
carries `ModuleSuffix`, which the compiler requires across two files of one namespace.

Both halves fill the table from the registry entry each already folded at registration:
`SignatureResolution`'s `publishRecord`/`publishUnion`/`publishEnum`/`publishAbbrev`/
`publishClassLike` for a `.fsi`, `FrozenSignature.toSurface`'s `register` from
`TTypeDeclG.Attributes` for every kind of an unsigned `.fs`. The one producer that dropped its
fold was the abbreviation: `DeclRegistration.registerAbbreviationDecl` validated the
attributes and discarded them, and `ElaborateTypeDecls.tryAbbrevType` wrote `EqArray.empty`
into the TAST. `AbbreviationInfo` now carries `Attributes`, and the TAST declaration carries
them through the existing codec row. fsc honours an attribute on an alias (`[<Obsolete>] type
A = int` warns FS0044 at a use), so the alias is a real position for the comparison.

`ConformanceSurface.check` is the one entry point: it runs the private `checkTypes` and
`checkValues`, which both return a `ConformanceFindings` (the former `ValueConformance`,
renamed), and concatenates the two, types first. `checkTypes`' implementation-side table keeps
the whole frozen `TypeDecl` per key instead of narrowing to a family, which is the
intermediate Gap 5 needs as well; the family is derived where the kind verdict is taken. The
divergence loop runs over `AttributesByKey` against `td.Attributes`, reusing
`divergentAttributes`, and `conformSignature` reports each as `AttributeArgumentsDiffer`.

`ExternalSymbol.Attributes` is authoritative only on a symbol reached through a
`PublishedSurface`. A metadata or TS-manifest provider leaves it empty whatever the
declaration wrote, so empty carries two readings on `IExternalSymbolProvider` and one on the
surface. Sited on the field; moving it to a surface table, as the type attributes now are, is
the fix and stays open.

Acceptance evidence: seven `AnalysedConformance` tests for values — a divergence reported and
named, that finding carrying warning rather than error severity, `0x1` against `1` conforming,
`1 ||| 2` against `2 ||| 1` conforming, named arguments conforming in either order, a named
argument's value diverging, and an attribute on one half alone taking no verdict — and five for
types: a divergence reported and named on each of record, union, enum, interface, class and
abbreviation; the finding at warning severity; matching arguments conforming on each of the six
kinds; an attribute on one half alone taking no verdict; and argument-less posture attributes
conforming in either order.

### Gap 4 — the published shape cannot state opacity, so `DeclaredKinds` exists. CLOSED.

`PublishedSurface.DeclaredKinds` was a second table keyed by the same `TypeKey` as
`ShapesByKey`, carried because an opaque `type T` publishes the same `Class` shape a bodied
class does: the shape alone lost whether the signature committed its name to a nominal family.
(Filed off the 2026-08-30 review of the Stage 2–3 landing.)

`ExternalClassShape.IsInterface: bool` is now `Commitment: ClassCommitment`, three-valued over
`Class` / `Interface` / `Opaque`, with `IsInterface` kept as a derived member so every read
site is untouched. A producer carrying only the interface bit states its commitment through
`ClassCommitment.ofIsInterface`. `ExternalTypeShape.DeclaredFamily` reads it, and answers
`ValueNone` for the opaque, abbreviation, `extern` and unmodelled shapes alike.
`ConformanceSurface.checkTypes` takes the family off `ShapesByKey`;
`SignatureResolution.declaredKindFamily`, `PublishedSurfaceBuilder.addDeclaredKind` and both
`DeclaredKinds` fields are deleted.

`DeclaredFamily` is the one reader of `Opaque`. Every other read site goes through
`IsInterface`, which answers `false` for it, so subtyping and base eligibility still treat an
opaque `type T` as a class — the behaviour that held before the marker existed.

The marker touches no codec: `ExternalClassShape` is derived from `FrozenPools` by
`FrozenSignature.toSurface` on the `.fs` side and from resolved signatures on the `.fsi` side,
and is serialised nowhere. (The premise recorded here that it was is wrong.)

Acceptance evidence: `SignatureResolutionTests`' "The published shape states which family the
declaration commits to" pins each family and the opaque non-commitment together; the existing
`AnalysedConformance` kind-drift and opaque-type pairs stayed green.

### Gap 5 — a type's BODY takes no conformance verdict. CLOSED (2026-09-05).

(Filed 2026-09-03, off the review of the measure-resolution test landing.)

**What landed.** `ConformanceSurface.check` compares the two halves as the surfaces each
publishes: the signature's against the one the implementation would publish unsigned
(`FrozenSignature.toSurface` over the frozen pools, built by `conformSignature`). Type and
value presence, family and attribute agreement read both surfaces; the `extern` ↔ repr pairing
alone reads the implementation's `IntrinsicBindings` residue beside them. `ConformanceBodies.check`
compares shape against shape for every key both halves publish under one family. This is
surface-to-surface rather than the TAST-side comparison sketched below, because a member's
projection to an `ExternalMember` already exists once, in `toSurface`; comparing the TAST
directly would have written it a second time. A member compares as a `MemberShape`, the
five-field key (name, staticness, value-member-ness, own-typar count, folded signature) both an
`ExternalMember` and a union case reduce to.

A private implementation type or value behind a public signature declaration now reports as
missing, because the unsigned surface omits it; fsc rejects the same pair on accessibility.

Per family, matching fsc's `SignatureConformance`:

- **Record**: fields by name in both directions (`FieldMissingInImpl`, `FieldMissingInSig`,
  both FS0313), a matched field's type and mutability (`FieldDiffers`, FS0193), and the
  written order once the names agree (`FieldOrderDiffers`). `[<Struct>]` on one half alone
  is `ShapeFlagDiffers`.
- **Union**: cases positionally, because the index is the runtime tag
  (`UnionCaseCountDiffers`, then `UnionCaseDiffers` on name, field names or field types).
- **Enum**: cases by name in both directions, and a matched case's value.
- **Abbreviation**: the bodies (`AbbreviationDiffers`).
- **Members**, on record, union, class and interface: every declared member must be matched
  by a defined member of the same name, staticness, kind, own-typar count and folded
  signature (`MemberMissingInImpl`, FS0193); an implementation-only member is hidden. This
  subsumes `ConformanceTypars.checkMembers`, which reported only a generic member's order
  and only when a same-arity overload existed, and it is deleted with its tests. A union
  case also satisfies the static member its constructor compiles to: `list.fsi`'s
  `static member Cons` and `static member Empty` are the `::` and `[]` cases, exactly as
  FSharp.Core's `FSharpList` carries `Cons` and `get_Empty` as `CompilationMapping(UnionCase)`
  methods with no member behind them.
- **Class**: base type, directly-declared interface set, `sealed` and `struct`
  (`BaseTypeDiffers`, `InterfacesDiffer`, `ShapeFlagDiffers`). An opaque `type T` demands
  no body.

Every new `ConformanceError` case carries rendered text rather than a `FrozenType`, on the
`CompiledNameDiffers` precedent, so the codec gained fifteen rows (tags 15 to 28) and no type
writer. The round-trip fixture carries one value per case.

**Corpus finding.** The check turned `Vesper.List` red: `list.fsi` declares `Item` and
`GetReverseIndex` and `list.fs` defined neither, so a consumer resolving through the
signature would have reached emission before learning so. Both are now transliterated from
FSharp.Core's `prim-types.fs`; `Item` omits the negative-index guard because `<` on `int` is
outside the package's dependency set. `GetSlice` is still undefined and takes no verdict,
because `int option` does not resolve in the signature and the member never publishes.
`Vesper.List.mjs` is regenerated for the two members.

Acceptance evidence: `AnalysedBodyConformance` in `SemanticAnalysis.Tests/ConformanceBodyTests.fs`,
twenty-six tests, one or two per verdict above plus the conforming pair for each family, the
generic member typar-order pair previously pinned against a hand-built provider, and the
case-constructor rule in both directions.

`ConformanceSurface.checkTypes` takes two verdicts per published type: presence
(`MissingInImpl`) and declared family (`TypeKindMismatch`). Its implementation-side input,
`declaredTypes`, narrows each `TTypeDecl` to a `TypeKindFamily voption` and drops the body, so
a record's fields, a union's cases, an enum's cases and every member signature are compared
nowhere. Two halves that agree on the name and the family conform whatever they declare inside
it.

Probed on 2026-09-03. `fsc` against this compiler, on a `.fsi`/`.fs` pair:

| written | `fsc` | this compiler |
| --- | --- | --- |
| `.fsi` `type T = { X: int }`, `.fs` `type T = { W: int }` | FS0313 "the field X was required by the signature but was not specified by the implementation" | silent |
| `.fsi` `type T = { X: int }`, `.fs` `type T = { X: string }` | FS0193 "the module contains the field `X: string` but its signature specifies `X: int`" | silent |
| `.fsi` `type T<'a> = { Y: 'a }`, `.fs` `type T<'a> = { Z: 'a }` | FS0313 | silent |
| `.fsi` `type U = A \| B`, `.fs` `type U = A \| C` | FS0193 "requires a value `member U.IsB: bool`" | silent |

A consumer resolves against the signature's shape, so it reads the field the `.fsi` declares
and the implementation's own field is never reached. The divergence surfaces at emission or
not at all.

This gap differs in kind from Gaps 1–4, which were holes in the frozen record. The record is
complete here — `ExternalTypeShape.Record`/`Union`/`Enum` carry their members on the signature
side and `TTypeKindG` carries them on the implementation side. The narrowing is
`declaredTypes`' alone, and it is the shape this repo treats as wrong by default: a stage that
discards an intermediate its consumer needs.

The check belongs beside `checkValues`, which already compares by resolved identity. The
reshape it wanted landed with Gap 3: every shape is a record, and `checkTypes`' implementation
table now holds the whole frozen `TypeDecl` per key, so the body is in hand and the check is
one more comparison per entry.

### Gap 6 — `[<AbstractClass>]` reaches no declared flag, so abstractness is neither checked nor compared. CLOSED (2026-09-06).

(Filed 2026-09-05, off the Gap 5 landing.)

**What landed.** `AttributeDecode.ClassAttributeVerdict.IsAbstract` is decoded off
`RuntimeNames.abstractClassAttributeKey`, and both Vesper-side producers of
`DeclaredClassFlags` write it: `MemberRegistration` for an implementation class and
`Members.bodiedClassSurface` for a signature class. `ConformanceBodies.checkClassShape`
compares it as the `"abstract"` case of `ShapeFlagDiffers`. The flag already travelled whole
through the TAST, the codec and `FrozenSignature.toSurface`, so no carrier changed.

Acceptance evidence: `ConstraintsTests` "new: an abstract class with a parameterless ctor"
refuses a Vesper `[<AbstractClass>]` under `'T : (new : unit -> 'T)`; `AnalysedBodyConformance`
reports the attribute on one half alone and accepts it on both. The `Vesper.*` corpus stayed
green on both backends.

**As filed.** `DeclaredClassFlags.IsAbstract` had one Vesper-side producer on each half, and both
wrote a constant. `MemberRegistration.fs:614` set `IsAbstract = false` for every implementation
class, and `Members.bodiedClassSurface` (`Passes/SignatureResolution/Members.fs:376`) built a
signature class's flags from `AttributeDecode.decodeClassAttributes`, which decoded `Sealed`,
`AllowNullLiteral` and `Struct` and had no field for `AbstractClass`.
`RuntimeNames.abstractClassAttributeKey` existed and nothing read it. The only producer that
filled the flag was `Codegen.Clr/MetadataSymbols.fs:508`, from reflection.

The flag has one consumer, the `'T : (new : unit -> 'T)` check in
`Unification/ConstraintCheck.fs:214`, which refuses an abstract or interface type. A Vesper
`[<AbstractClass>]` therefore satisfies the `new` constraint whatever it declares, while a
metadata-read abstract class is refused. Gap 5's `checkClassShape` compares `sealed` and
`struct` and skips `abstract` for this reason: comparing a constant `false` on the signature
against a constant `false` on the implementation would report nothing, and fixing one half
alone would report every abstract pair.

The `new` constraint is already affirmed positively, through `NominalDecl.hasParameterlessCtor`
reading a declared `.ctor` off the class; abstractness is a second condition fsc applies on
top of it, because an abstract class may declare a parameterless constructor (a subclass's
`inherit` call needs it published). Probed 2026-09-06: `[<AbstractClass>] type Abs() = class
end` under `'T : (new : unit -> 'T)` is FS0001 "requires that the type 'Abs' be non-abstract",
with the constructor present. Folding the two into one predicate by withholding an abstract
class's `.ctor` from its surface would break the `inherit` path, so the flag stays the carrier.

fsc compares the flag: an `[<AbstractClass>]` on one half alone is FS0193 ("one is abstract").

The corpus writes `AbstractClass` only on the `ModuleSuffix`-modules of `list.fsi` and
`set.fsi`, which are modules rather than classes, so no corpus pair went red.

### Non-gaps, verified

- **`[<Literal>]` signature values.** `registerValSig` publishes them, so
  `ConformanceSurface.checkValues` reports a missing one by identity. No fix needed.
- **`exception` declarations.** `Validation` already reports `NotYetSupported` at the
  declaration, so neither route needs a conformance verdict for one.
- **Accessibility.** `ConformanceSurface` compares presence without an accessibility threshold,
  where `FrozenSignature.toSurface` keeps internal-or-better. A `let private` matching a `val`
  is therefore counted as present; F# rejects it, and rejecting it is the accessibility check's
  job, not conformance's.

## Semantic assumptions, as Stage 1 settled them

1. **The module-decl guard stays syntactic.** It asks whether the two files are a pair at all,
   and two halves resolved under different headers publish into different namespaces, which
   every finding below it would then be about. `conformSignature` calls
   `Conformance.sigDeclPath`/`implDeclPath` directly, and its doc comment says why.

2. **The `extern` species is carried, not re-derived.** `PublishedSurface.DeclaredReprs` maps a
   canonical intrinsic identity to `DeclaredRepr.Opaque`/`Heritable`/`Capability`, filled by
   `publishExtern` and `publishIntrinsicAbbrev`, and matched against the implementation's
   `Residue.IntrinsicReprKeys`. Deriving it from the published shape was not available: a
   capability on a target binding no repr publishes as a plain `Class`. `SignatureResolution`'s
   private `ExternForm` is now that same type.

3. **`jsNative` is read off the CST, and the whole `[<Import>]` check moved out of
   conformance.** The four verdicts read the implementation alone, so `.fsi` pairing was never
   what they were about. `Attributes.declareImportBinding` runs beside `declareGlobalBinding`
   during elaboration, ahead of the inline expansion that turns `jsNative` into the template it
   stands for, and reports positioned in the `.fs` rather than unpositioned against the `.fsi`.

   The placement holds; the CST read does not. Stage 2a.1 replaces the identifier-text match with
   the `nativeOnly` sentinel, read by resolved key like the attribute beside it.

4. **SUPERSEDED with Stage 2.** This recorded the cost of making the package route analyse its
   package: every conformance run pays for full analysis. The route is deleted rather than
   upgraded, so the cost is not incurred. The live question in its place is Stage 2a.3 — where the
   asset read happens. Analysis holding an `IRuntimeModules` would keep the whole check in one
   pass at the price of a filesystem read under analysis and an oracle threaded through
   `CompilingAssembly` to some fifteen construction sites; recording the obligation and letting
   the backend discharge it keeps analysis deterministic and the diagnostic positioned, and is
   what Stage 2a takes.

5. **DECIDED: `[<Import>]` becomes a codegen input.** `059dfd13` wired the attribute for
   conformance only and touched no backend file, so no backend reads it today.
   Stage 2a.3 makes the backend the party that discharges the path and selector against its own
   module system, which is the first backend read of the attribute. `AttributeDecode.tryImport` is
   the key-based reader it calls, with `ImportRef`/`ImportDecl` beside it rather than inside
   `Conformance`. Emitting the import itself stays out of scope; this is the check alone.

## Scope and risk

Stage 1 touched `Conformance.fs`, `AttributeDecode.fs`, `AssemblyFiles.fs`, `PublishedSurface.fs`,
`Passes/SignatureResolution.fs`, `Passes/Attributes.fs`, `Elaborate.fs`, the new
`ConformanceSurface.fs`, and `SemanticAnalysis.Tests/ConformanceTests.fs`. No existing test
went red: every finding the surface rules take is one the CST rules also take on the corpus,
and the two cases where they disagree (`[<CompiledName>]`, a shadowed attribute) are ones no
fixture pinned.

Stage 1a touched `TastDecl.fs`, `TastConvert.fs`, `FrozenCodecDecls.fs` (kind tag `5uy`),
`Elaborate/TypeDecls.fs`, `FrozenSignature.fs`, `ConformanceSurface.fs`, `PlatformTypes.fs`,
`Codegen.Clr/LayoutNodes.fs`, and the `TastShape` renderer. No existing test went red, and the
`Vesper.*` corpus compiles unchanged through both backends: every abbreviation there is
`.fsi`-declared, so the new implementation-side declaration matches a shape already published.

Stage 2a is the stage to land alone, and it lands in its own order: the `nativeOnly` sentinel
(2a.1) first, because it is self-contained and closes a live shadowing hole; then the split of the
five obligations and `IRuntimeModules` with the JS implementation (2a.2–2a.4). It touches
`compiler-attributes.fs`/`.fsi`, `js-interop.js.fs`/`.fsi`, the three `[<Import>]`/`jsNative`
sites in `Vesper.Comparison` and `Vesper.Core`, `Conformance.fs`, `Passes/Attributes.fs`,
`AssemblyAnalysis.fs` for the per-unit record, and the JS backend's package build. Both backends
gain the sentinel refusal.

Stage 3 was deletion, and took `ConformanceTests.fs`'s CST-route lists with it.

The gap closures after it touched, in landing order:

- Gap 2 added two `AnalysedConformance` tests and changed no source.
- Gap 4 touched `ExternalDeclarations.fs`, `ExternalSymbols.fs`, `PublishedSurface.fs`,
  `ConformanceSurface.fs`, `FrozenSignature.fs`, `Passes/SignatureResolution.fs` and its
  `Members.fs`, `Codegen.Clr/MetadataSymbols.fs`, `Codegen.Js/JsNativeSymbols.fs` and
  `TsManifestMembers.fs`, plus nine `ExternalClassShape.basic` call sites in the tests. Every
  break was a compile error, and no test went red.
- Gap 3's value half added `ModuleBindingInfo.fs`, and touched `SideTypes.fs`, `Elaborate.fs`,
  `FrozenCodecTypes.fs`, `ExternalDeclarations.fs`, `ExternalSymbols.fs`, `FrozenSignature.fs`,
  `Passes/SignatureResolution.fs`, `Conformance.fs`, `Diagnostics.fs`,
  `FrozenCodecDiagnostics.fs`, `ConformanceSurface.fs` and `AssemblyAnalysis.fs`. The frozen
  format gained a field on each module-binding row; the whole `Vesper.*` corpus recompiles on
  both backends, and no test went red.
- Gap 3's type half touched `PublishedSurface.fs`, `ExternalDeclarations.fs`,
  `ExternalSymbols.fs`, `ExternalEnumShape.fs`, `ExternalSymbolProviders.fs`,
  `FrozenSignature.fs`, `Passes/SignatureResolution.fs`, `TypeInfos.fs`,
  `Passes/NameResolution/DeclRegistration.fs`, `Elaborate/TypeDecls.fs`,
  `ConformanceSurface.fs`, `AssemblyAnalysis.fs`, the nine `Enum`/`Abbrev` tuple-pattern sites
  across `SemanticAnalysis`, `Codegen.Clr` and `Codegen.Js`, and the test pattern sites. The
  codec is untouched: an abbreviation's attributes travel on the type-declaration row every
  kind already wrote. No test went red, and the `Vesper.*` corpus recompiles on both backends.

- Gap 5 touched `Conformance.fs`, `ConformanceSurface.fs`, `ConformanceTypars.fs` (deletion
  of `checkMembers`), `FrozenCodecDiagnostics.fs`, `AssemblyAnalysis.fs`, `Vesper.List/list.fs`
  and its regenerated `.mjs`, the codec round-trip fixture, and the Clr `ConformanceTyparsTests`
  assertion that called `checkMembers`. One corpus test went red, on `Vesper.List`, and it was
  a finding (above).
- Gap 6 touched `AttributeDecode.fs`, `Passes/NameResolution/MemberRegistration.fs`,
  `Passes/SignatureResolution/Members.fs`, `ConformanceBodies.fs`, `Conformance.fs`, and
  added one `ConstraintsTests` and two `AnalysedBodyConformance` tests. No test went red.

One behaviour tightened along the way: a module binding's attributes now go through
`AttributeFold.build` rather than `enforceTargets` alone, so an attribute argument outside the
constant domain is diagnosed at a `let` where it previously passed. The corpus writes only
constant arguments there, so nothing went red.

## Correction owed to another doc

`semantic-analysis-followups-plan-2.md:243` states that nothing under `src/` calls the
conformance gate, and that `ConformanceTypars.checkFile`/`checkMembers` are "tests only". That
is stale: `AssemblyAnalysis.conformSignature` called both on the live in-assembly path, and their
diagnostics reach the compilation (`checkMembers` is since deleted, folded into Gap 5's body
check). `ConformancePass.checkManifest`/`enforce` were tests-only and
are now deleted with the rest of the CST route, so the in-assembly route is the driver gate and
the only one. That section now carries the resolution.
