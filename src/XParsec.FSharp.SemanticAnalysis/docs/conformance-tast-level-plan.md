# Conformance at the TAST level

## Root cause

`.fsi`↔`.fs` conformance runs down two routes, because only one of them has analysed halves to
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
| Type presence (`MissingInImpl`) | `summariseSig`/`summariseImpl`, name strings | `ConformanceSurface.checkTypes`, by `TypeKey`. Delegates excepted — Gap 2 |
| `extern`/repr pairing, heritability | CST species off `TypeSignature`/`TypeDefn` | `PublishedSurface.DeclaredReprs` against `Residue.IntrinsicReprKeys` |
| Value presence (`checkValuePresence`) | identifier text | `ConformanceSurface.checkValues`, by `BindingKey` |
| Typar count/order | not checked — nothing frozen to compare | `ConformanceTypars.checkFile`/`checkMembers` |
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

**Stage 2a — lift `[<Import>]` to a Vesper-level concept. Blocks the deletion.**

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

**Stage 3 — delete the CST rule set and the package route.** `ConformancePass.fs` goes
entirely. `Conformance.fs` loses `summariseSig`/`summariseImpl`/`check`/`summariseSigVals`/
`summariseImplVals`/`boundName`/`checkValuePresence`/`summariseImports`/`checkUnit`, the shape
DUs, and the provisional attribute reader; `ConformanceError`, `describe` and the
`sigDeclPath`/`implDeclPath` pairing check survive. `isJsNativeBody` goes with `jsNative` in
Stage 2a.1, so identifier-text matching leaves the import rule and `attributeShortName`-style
matching then exists nowhere.

Stage 3 follows Stage 2a and Gap 2's decision. Two further costs to settle as it lands:

- **The JS corpus loses its only whole-package check.** `Codegen.Clr.Tests` compiles every
  `Vesper.*` package through the in-assembly route, so the CLR side is covered by construction.
  `Codegen.Js.Tests` compiles only `Vesper.Core` (`JsPackageTests.fs:274`), so
  `jsPackageConformanceTests` in `ConformanceTests.fs` is what currently holds the js manifests
  to their contracts — including which surface is still un-ported. Either the js suite gains a
  whole-corpus in-assembly compile, or that coverage goes with the route.
- **`ConformanceVerdict.PairParseFailure` loses its producer.** `enforce` is its only one. It is
  serialised at tag `3uy` (`FrozenCodecDiagnostics.fs:79`) and round-tripped by
  `Codegen.Js.Tests/FrozenCodecRoundTripTests.fs:433`, so retiring the case renumbers the codec.
  `SigWithoutImpl` is already producerless on the same DU, so the two retire together or not at
  all — a separate change from this one.

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

### Gap 2 — a `delegate` declaration claims no identity. Decide before Stage 3.

`SigDecl.claimedKind` yields `ValueNone` for a delegate, so a `.fsi` publishes
`ExternalTypeShape.Unmodelled(Delegate, arity)` in place of a type and the implementation is
never asked for one. The CST rule set reports `MissingInImpl` for it, off the written name.

This compiler models no delegate on either target, so the two routes disagree about a construct
neither can compile. Two defensible positions, and the choice is not the checker's to make on
its own:

- **A declared delegate is refused at its declaration**, the way `exception` already is
  (`Validation` reports `NotYetSupported`). Conformance then has nothing to say, the `Unmodelled`
  shape reports at the first use site, and Stage 3 deletes the CST rule with no loss.
- **A delegate claims a type**, gaining a `TTypeKindG` case and an `ExternalTypeShape`, and
  conformance checks its presence like any other nominal.

The first is the cheaper one and matches how the compiler treats every other unmodelled
construct; take it unless delegates are on the roadmap.

### Gap 3 — attribute arguments are compared nowhere

No conformance rule reads a pair's attribute arguments on either route. (Relocated from the
deleted fsi-front-end-plan, 2026-08-28.) The trade to settle when a rule arrives: structural
expression equality needs no folding but rejects `A ||| B` against `B ||| A`, and `1` against
`0x1`, as mismatches; comparing folded values is now possible, since `AttributeFold` folds
each side's arguments to `TConstValue`s (landed 2026-08-28).

**Premise to verify first, against fsc:** whether an attribute is expected on both halves of a
pair at all, or whether F# takes the signature's alone. Do not design the check before probing
`dotnet fsi` / fsc for that.

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

Stage 3 is deletion, and takes `ConformanceTests.fs`'s CST-route lists with it.

## Correction owed to another doc

`semantic-analysis-followups-plan-2.md:243` states that nothing under `src/` calls the
conformance gate, and that `ConformanceTypars.checkFile`/`checkMembers` are "tests only". That
is stale: `AssemblyAnalysis.conformSignature` calls both on the live in-assembly path, and their
diagnostics reach the compilation. `ConformancePass.checkManifest`/`enforce` do remain tests-only,
and stay that way: the driver gate is the in-assembly route, and Stages 2a–3 below retire the CST
one rather than wiring it up beside it. That section now carries the resolution.
