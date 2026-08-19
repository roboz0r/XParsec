# Conformance at the TAST level

## Root cause

`.fsi`↔`.fs` conformance runs down two routes with two different rule sets, because only one of
them has analysed halves to compare.

| Route | Entry | What it holds | Rules applied |
|---|---|---|---|
| In-assembly | `AssemblyFiles.fs:321` `conformanceDiagnostics`, live at `:645` under `Publication.InAssembly` | the signature resolved to a provider (`published`) and the implementation frozen (`impl.Frozen`) | CST rule set (`Conformance.checkUnit`) **plus** `ConformanceTypars.checkFile`/`checkMembers` (`:358`, `:361`) |
| Package / manifest | `ConformancePass.fs` `checkManifest` → `check` | parse results only (`PackageSource.ReadFile<ParsedSignature>`) | CST rule set only, plus the manifest half of the `[<Import>]` check (`ConformancePass.fs:110`) |

`AssemblyFiles.fs:319` states the split in the code's own words: the CST rule set, "then typar
ORDER over the two frozen surfaces. The typar half runs only here, because the package route
freezes nothing to compare."

So the in-assembly route already has everything a TAST comparison needs and uses it for one
check only. The CST rule set re-derives, off syntax, facts that name resolution and Freeze have
already established — type identity, value identity, compiled names, attribute identity. Two
derivations of one fact, which is the shape this repo treats as wrong by default.

The concrete costs today:

- **Attribute identity is matched on the long ident's last segment.** `Conformance.fs:412`
  `attributeShortName` cannot tell `[<Import>]` from `[<MyOwn.Import>]`, and a shadowing
  declaration goes unnoticed. `AttributeDecode` was made key-based precisely to end this, and
  the reader at `Conformance.fs:400-517` reintroduces it one file away.
- **Value identity is a string off a pattern.** `boundName` (`:351`) plus
  `checkValuePresence` (`:577`) compare raw identifier text. `ConformanceTypars.checkFile`
  (`ConformanceTypars.fs:115`) does the same job correctly — it resolves the binding's
  compiled name through the member table, so `[<CompiledName>]` is handled — and then throws
  the presence verdict away, using the lookup only to reach the scheme.
- **`RuntimeNames.importAttributeKey` has no consumer.** The key exists; the meaning of
  `[<Import>]` is asserted only by the CST reader.

## What the CST rule set checks, and where each lands

| Check | Today | On two surfaces | Confidence |
|---|---|---|---|
| Type presence (`MissingInImpl`) | `summariseSig`/`summariseImpl` (`:239`, `:262`), name strings | declared types of the signature surface vs the implementation's frozen types, by `TypeKey` | High |
| `extern`/repr pairing, heritability (`SigShape`/`ImplShape`) | CST species off `TypeSignature`/`TypeDefn` | the repr is already harvested into the provider (`implementationReprs`, `AssemblyFiles.fs:310`); species becomes a surface property | Medium — see assumption 2 |
| Value presence (`checkValuePresence`) | identifier text | impl roots vs provider lookup — the miss case `ConformanceTypars.checkFile` currently drops | High |
| Typar count/order | already `ConformanceTypars` over frozen surfaces | unchanged; stops being the exception | Landed |
| `[<Import>]` well-formedness, selector vs emitted name | CST reader, last-segment match | `ResolvedAttributes.TryFind RuntimeNames.importAttributeKey` | High |
| `jsNative` body (`isJsNativeBody`, `:519`) | CST expression match | a TAST body check | Medium — see assumption 3 |
| Module decl path (`sigDeclPath`/`implDeclPath`, `:599`, `:610`) | file header text | not obviously a surface fact | Low — see assumption 1 |

## Staged plan

**Stage 1 — the in-assembly route adopts surface-vs-surface rules.** No new machinery:
`conformanceDiagnostics` already receives `published` and `impl.Frozen`, which is exactly
`ConformanceTypars.checkFile`'s signature. Re-express type presence, value presence and the
attribute reads against those two, and have `conformanceDiagnostics` stop calling
`Conformance.checkUnit`. The package route keeps the CST rule set meanwhile, so both remain
green and the two rule sets can be diffed against each other on the real `Vesper.*` contracts —
that diff is the acceptance evidence for this stage, and any disagreement is a finding about
the CST rules rather than a regression.

**Stage 2 — the package route gains analysed halves.** Route `ConformancePass.check` through
`AssemblyFiles.foldUnits` rather than over bare parse results. `ed575bc6` on `semantic-analysis`
consolidated parsing into a single `foldUnits`, which is the seam this needs. The manifest half
of the `[<Import>]` check (`ConformancePass.fs:110`, path against `[core] runtime`, selector
against the asset's exports) is unaffected — it reads the manifest, not the CST.

**Stage 3 — delete the CST rule set.** `Conformance.fs` loses `summariseSig`/`summariseImpl`/
`summariseSigVals`/`summariseImplVals`/`boundName`/`checkValuePresence`/`summariseImports` and
the provisional attribute reader; `ConformanceError` and `describe` survive as the diagnostic
vocabulary. `attributeShortName`-style matching then exists nowhere.

## Semantic assumptions to confirm before Stage 1

1. **The module-decl guard.** `ModuleDeclMismatch` compares the `.fsi` and `.fs` leading
   declaration paths as text. Whether a namespace/module header disagreement is still
   observable once both halves are analysed, or whether it must stay a syntactic pre-check, is
   undecided. If it stays syntactic, `Conformance.fs` keeps one small file-header check and
   the doc comment should say that is deliberate.

2. **Whether `extern`/`extern class`/`extern interface` survives as a surface distinction.**
   The three species differ in heritability and in whether the repr names an interface. If
   Freeze retains only the repr string, the heritability half of the check cannot be
   reconstructed from the surface and the species has to be carried explicitly — an instance
   of the discarded-intermediate rule, and worth fixing at the freeze rather than re-deriving.

3. **`jsNative` as a body check.** `isJsNativeBody` asserts the body is the bare identifier.
   Post-inline-expansion the body may no longer be recognisable as such. This check may need to
   sit before expansion, which constrains where in the pass order Stage 1's checks run.

4. **Cost of Stage 2.** Making a manifest check analyse the package means every conformance run
   pays for full analysis. If that is unacceptable, the alternative is that the package route
   stays a cheap pre-analysis gate — which keeps two rule sets permanently and gives up the
   point of the exercise. The recommendation is to accept the cost; the gate's only callers
   today are tests.

5. **Whether `[<Import>]` should become a codegen input.** `059dfd13` wired the attribute for
   conformance only and touched no backend file, so the JS backend does not read it. Not part
   of this scope, but the Stage 1 reader should be shaped so a backend consumer can use it.

## Scope and risk

Touches `Conformance.fs`, `ConformancePass.fs`, `AssemblyFiles.fs`, `ConformanceTypars.fs`, and
the two suites that own the verdicts (`SemanticAnalysis.Tests/ConformanceTests.fs`,
`Codegen.Clr.Tests/TestHelpers.fs:328`). Stage 1 is additive and reversible. Stage 2 changes
what a package check costs and is the stage to land alone. Stage 3 is deletion.

Expect Stage 1 to turn some currently-green conformance tests red. Per the repo's reading rule
that is a finding: the CST rules under-report (`attributeShortName` accepts a shadowed
attribute; `checkValuePresence` compares source text rather than compiled names), so a tightened
check failing an existing fixture most likely pins old wrong behaviour.

## Correction owed to another doc

`semantic-analysis-followups-plan-2.md:243` states that nothing under `src/` calls the
conformance gate, and that `ConformanceTypars.checkFile`/`checkMembers` are "tests only". That
is stale: `AssemblyFiles.fs:358,361` call both on the live in-assembly path, and their
diagnostics reach the compilation. The claim predates `conformanceDiagnostics`. The section
about `ConformancePass.checkManifest`/`enforce` having no production consumer does still hold.
