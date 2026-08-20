# Conformance at the TAST level

## Root cause

`.fsi`↔`.fs` conformance runs down two routes, because only one of them has analysed halves to
compare.

| Route | Entry | What it holds | Rules applied |
|---|---|---|---|
| In-assembly | `AssemblyFiles.conformanceDiagnostics`, live under `Publication.InAssembly` | the signature's `PublishedSurface` and provider, and the implementation frozen | `ConformanceSurface` + `ConformanceTypars`, over resolved identities |
| Package / manifest | `ConformancePass.checkManifest` → `check` | parse results only (`ReadFile<ParsedSignature>`) | the CST rule set, plus the manifest half of the `[<Import>]` check |

The CST rule set re-derives, off syntax, facts that name resolution and Freeze have already
established — type identity, value identity, compiled names, attribute identity. Two
derivations of one fact, which is the shape this repo treats as wrong by default. Stage 1
retired the in-assembly copy; the manifest route still runs it, and Stages 2 and 3 remove it.

The costs that remain, on the manifest route only:

- **Attribute identity is matched on the long ident's last segment.** `attributeShortName`
  cannot tell `[<Import>]` from `[<MyOwn.Import>]`, and a shadowing declaration goes unnoticed.
- **Value identity is a string off a pattern.** `boundName` plus `checkValuePresence` compare
  raw identifier text, so a `[<CompiledName>]`'d binding reads as absent.

## What the CST rule set checks, and where each landed

| Check | On the CST (manifest route) | On the analysed halves (in-assembly route) |
|---|---|---|
| Type presence (`MissingInImpl`) | `summariseSig`/`summariseImpl`, name strings | `ConformanceSurface.checkTypes`, by `TypeKey`. Abbreviations and delegates excepted — Gaps 1 and 2 |
| `extern`/repr pairing, heritability | CST species off `TypeSignature`/`TypeDefn` | `PublishedSurface.DeclaredReprs` against `Residue.IntrinsicReprKeys` |
| Value presence (`checkValuePresence`) | identifier text | `ConformanceSurface.checkValues`, by `BindingKey` |
| Typar count/order | not checked — nothing frozen to compare | `ConformanceTypars.checkFile`/`checkMembers` |
| `[<Import>]` well-formedness, selector vs emitted name | `summariseImports`, last-segment match | `Attributes.declareImportBinding`, keyed on `RuntimeNames.importAttributeKey` |
| `jsNative` body | `Conformance.isJsNativeBody` | the same reader, called during elaboration |
| Module decl path | `sigDeclPath`/`implDeclPath` | the same two, called from `conformanceDiagnostics` |

## Staged plan

**Stage 1 — the in-assembly route adopts surface-vs-surface rules. LANDED.**
`ConformanceSurface.checkTypes`/`checkValues` take type presence, the `extern` ↔ repr pairing
and value presence off `r.Surface` and `impl.Frozen`, by resolved identity;
`conformanceDiagnostics` no longer calls `Conformance.checkUnit`. The `[<Import>]` checks left
conformance altogether for `Attributes.declareImportBinding`, which reads the attribute by
`RuntimeNames.importAttributeKey`. The package route keeps the CST rule set.

Acceptance evidence: `Codegen.Clr.Tests` compiles every `Vesper.*` package through the
in-assembly route and `PackageConformance` runs the CST rule set over the same corpus, both
green, so the two rule sets agree on the real contracts. `AnalysedConformance` in
`SemanticAnalysis.Tests/ConformanceTests.fs` drives each finding through the analysed route,
including the two cases the CST rules get wrong: a `[<CompiledName>]`'d `let` answering the
`val` it publishes as, and a shadowing local `ImportAttribute` that is not the compiler's.

**Stage 1a — the TAST carries type ABBREVIATIONS.** The prerequisite for every stage below, and
a live defect in its own right rather than a conformance concession. See "Gap 1".

**Stage 2 — the package route gains analysed halves.** Route `ConformancePass.check` through
`AssemblyFiles.foldUnits` rather than over bare parse results. `ed575bc6` on `semantic-analysis`
consolidated parsing into a single `foldUnits`, which is the seam this needs. The manifest half
of the `[<Import>]` check (path against `[core] runtime`, selector against the asset's exports)
is unaffected — it reads the manifest, not the CST.

**Stage 3 — delete the CST rule set.** `Conformance.fs` loses `summariseSig`/`summariseImpl`/
`summariseSigVals`/`summariseImplVals`/`boundName`/`checkValuePresence`/`summariseImports` and
the provisional attribute reader; `ConformanceError`, `describe`, `isJsNativeBody` and the
`sigDeclPath`/`implDeclPath` pairing check survive. `attributeShortName`-style matching then
exists nowhere.

Deleting it costs nothing only once every verdict it takes is taken from the TAST, so Stage 3
follows Stage 1a and Gap 2's decision.

## Gaps between the TAST and a full conformance check

A `.fs` decides what it publishes, and the frozen pools are the compiler's record of that
decision. Where the pools omit a declaration, every consumer has to go back to the syntax —
conformance here, `FrozenSignature.toSurface` for the next file in the assembly, and the
package extractor. Each gap below is therefore a hole in the frozen record, sited where the
declaration is dropped rather than where a reader notices.

### Gap 1 — a type abbreviation reaches no frozen declaration. Fix in Stage 1a, before Stage 3.

`TTypeKindG` has cases for `Record`, `Union`, `Class`, `Interface` and `Enum` and none for an
abbreviation, and `Elaborate.TypeDecls` answers `None` for a `TypeDefn.Abbrev` whose RHS is not
`(# … #)`. So `type myalias = int` in a `.fs` produces no `TDecl` at all.

This is not only a conformance gap. `FrozenSignature.toSurface` publishes what `Decls` holds,
so an abbreviation declared in an unsigned `.fs` is invisible to the next file of the same
assembly. Two files, `type myalias = int` then `let f (x: myalias) = x`, report:

```
two.fs: The type 'myalias' is not defined
two.fs: internal compiler error: the frozen TAST holds 1 unresolved TyVar(s)
```

A union in the same position resolves. The abbreviation is reachable today only through a
`.fsi`, which is why the `Vesper.*` corpus never hits it: every package file has one.

The fix is a `TTypeKindG.Abbrev of body: 'ty` case, emitted by `Elaborate.TypeDecls` from the
registry entry the group close already forces, and published by `FrozenSignature.toSurface` as
the `ExternalTypeShape.Abbrev` a `.fsi` publishes. Conformance then drops the `Abbrev` exception
in `ConformanceSurface.demandsDeclaration` and the cross-file case starts working; a test that
the two files above compile is what pins it.

Cost: a new `TTypeKindG` case reaches `TastConvert`, the pool fill/unpool, `FrozenCodec` and
every `match` over the kind. That is the price of the fact being in the type system.

### Gap 2 — a `delegate` declaration claims no identity. Decide before Stage 3.

`SigDecl.claimedKind` answers `ValueNone` for a delegate, so a `.fsi` publishes
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

### Non-gaps, verified

- **`[<Literal>]` signature values.** `registerValSig` publishes them, so
  `ConformanceSurface.checkValues` reports a missing one by identity. No fix needed.
- **`exception` declarations.** `Validation` already reports `NotYetSupported` at the
  declaration, so neither route needs a conformance verdict for one.
- **Accessibility.** `ConformanceSurface` compares presence without an accessibility threshold,
  where `FrozenSignature.toSurface` keeps internal-or-better. A `let private` answering a `val`
  is therefore counted as present; F# rejects it, and rejecting it is the accessibility check's
  job, not conformance's.

## Semantic assumptions, as Stage 1 settled them

1. **The module-decl guard stays syntactic.** It asks whether the two files are a pair at all,
   and two halves resolved under different headers publish into different namespaces, which
   every finding below it would then be about. `conformanceDiagnostics` calls
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

4. **Cost of Stage 2.** Making a manifest check analyse the package means every conformance run
   pays for full analysis. If that is unacceptable, the alternative is that the package route
   stays a cheap pre-analysis gate — which keeps two rule sets permanently and gives up the
   point of the exercise. The recommendation is to accept the cost; the gate's only callers
   today are tests.

5. **Whether `[<Import>]` should become a codegen input.** `059dfd13` wired the attribute for
   conformance only and touched no backend file, so the JS backend does not read it. Still out
   of scope, but `AttributeDecode.tryImport` is now the key-based reader a backend would call,
   and `ImportRef`/`ImportDecl` sit beside it rather than inside `Conformance`.

## Scope and risk

Stage 1 touched `Conformance.fs`, `AttributeDecode.fs`, `AssemblyFiles.fs`, `PublishedSurface.fs`,
`Passes/SignatureResolution.fs`, `Passes/Attributes.fs`, `Elaborate.fs`, the new
`ConformanceSurface.fs`, and `SemanticAnalysis.Tests/ConformanceTests.fs`. No existing test
went red: every finding the surface rules take is one the CST rules also take on the corpus,
and the two cases where they disagree (`[<CompiledName>]`, a shadowed attribute) are ones no
fixture pinned.

Stage 1a is the widest of the remaining changes, because a `TTypeKindG` case reaches the pools
and the codec, and it is the one that fixes a live defect. Stage 2 changes what a package check
costs and is the stage to land alone. Stage 3 is deletion.

## Correction owed to another doc

`semantic-analysis-followups-plan-2.md:243` states that nothing under `src/` calls the
conformance gate, and that `ConformanceTypars.checkFile`/`checkMembers` are "tests only". That
is stale: `AssemblyFiles.conformanceDiagnostics` calls both on the live in-assembly path, and
their diagnostics reach the compilation. The section about `ConformancePass.checkManifest`/
`enforce` having no production consumer does still hold.
