# Conformance at the TAST level

**Status: the redesign has landed. Two follow-ups remain, scoped below, and the doc is deleted
when they land.** `.fsi` ↔ `.fs` conformance runs down one route,
`AssemblyAnalysis.conformSignature`, over the two analysed halves, each as the surface it
publishes. The code and its tests are the record of what landed; the "What landed" section is
the map to them, kept only until the open items close.

## What landed

`conformSignature` compares the signature's `PublishedSurface` against the surface the
implementation would publish unsigned (`FrozenSignature.toSurface` over the frozen pools), by
resolved identity. The module-decl pairing alone stays syntactic (`Conformance.sigDeclPath` /
`implDeclPath`), because two halves resolved under different headers publish into different
namespaces and every later finding would be about the wrong companion.

| Verdict | Where | Test list |
|---|---|---|
| Type presence, `extern` ↔ repr pairing, nominal family, attribute arguments (FS1200, warning) | `ConformanceSurface.checkTypes` | `AnalysedConformance` in `SemanticAnalysis.Tests/ConformanceTests.fs` |
| Value presence, `[<CompiledName>]`, attribute arguments | `ConformanceSurface.checkValues` | same |
| Record fields, union cases, enum cases, abbreviation body, members, class base, interfaces, `sealed` / `abstract` / `struct` | `ConformanceBodies.check` | `AnalysedBodyConformance` in `ConformanceBodyTests.fs` |
| Module-value typar order | `ConformanceTypars.checkFile` | `ConformanceTyparsTests.fs` |
| `[<Import>]` well-formedness, selector vs emitted name, `nativeOnly` body | `Attributes.declareImportBinding`, during elaboration | `ConformanceTests.fs` |
| `[<Import>]` path and export against the target's module system | `AnalysedAssembly.gate`, through `IRuntimeModules` | `JsPackageTests.fs` |

Facts the code carries that a reader may otherwise re-derive:

- **Members compare as a `MemberShape`**: name, staticness, value-member-ness, own-typar count
  and the folded signature. A union case satisfies the static member its constructor compiles
  to, so `list.fsi`'s `static member Cons` and `Empty` are the `::` and `[]` cases. An
  implementation-only member is hidden, not drift.
- **Union cases compare positionally**, because the index is the runtime tag.
- **A private implementation declaration behind a public signature declaration reports as
  missing**, because the unsigned surface omits it. fsc rejects the same pair as FS0034.
- **Every `ConformanceError` case carries rendered text**, never a `FrozenType`, so the
  diagnostic codec has one row per case and no type writer.
- **`ClassCommitment`** is three-valued (`Class` / `Interface` / `Opaque`); an opaque `type T`
  demands no body.
- **`[<Import>]` is discharged at the gate, not at emission.** `AnalysedAssembly.gate` takes
  the target's `IRuntimeModules`, so a check-only caller that passes them takes the same
  verdicts a compile takes. `RuntimeModules.unsupported` is the CLR's.
- **A `delegate` declaration is refused at its declaration** on both halves, and takes no
  pairing verdict.

## Open items

### O1 — `ExternalSymbol.Attributes` moves to a surface table

`ExternalSymbol.Attributes` (`ExternalDeclarations.fs:106`) is filled by two producers,
`SignatureResolution.registerValSig` for a `.fsi` and `FrozenSignature.toSurface` for an
unsigned `.fs`, and left empty by every `IExternalSymbolProvider` (`MetadataSymbols.fs:534`,
`JsNativeSymbols.fs:59`, `TsManifestMembers.fs:203/326/436`) whatever the declaration wrote.
Empty therefore carries two readings on the provider and one on the surface, and the field's
doc comment is the only thing separating them. The type attributes already took the fix:
`PublishedSurface.AttributesByKey` is a table beside `ShapesByKey`, and the shape carries no
attributes field.

Steps, one commit:

1. `PublishedSurface` gains `SymbolAttributesByKey: EqArray<SurfaceEntry<BindingKey, TAttributes>>`,
   with `PublishedSurfaceBuilder.addSymbolAttributes` beside `addAttributes`.
2. `registerValSig` (`Passes/SignatureResolution.fs:696`) and `toSurface`
   (`FrozenSignature.fs:329`) write the table instead of the field.
3. `ConformanceSurface.checkValues` (`ConformanceSurface.fs:192`) reads both tables through
   `PublishedSurface.index`, as `checkTypes` reads `AttributesByKey`.
4. Delete `ExternalSymbol.Attributes` and the five provider sites that fill it with
   `EqArray.empty`. The compiler finds every other site.

Acceptance: the seven value-attribute tests in `AnalysedConformance` stay green, and the
`ExternalSymbol` doc comment's "read it through `PublishedSurface`" hedge is gone with the
field.

### O2 — `Vesper.List.GetSlice` is declared, undefined, and unpublished

`list.fsi:64` declares `member GetSlice: startIndex: int option * endIndex: int option -> 'T list`
and `list.fs` defines nothing for it. The body check does not report it, because `option` is
declared in `Vesper.Option` and `Vesper.List` depends on `Vesper.Core` alone
(`manifest.*.toml`), so `Members.tryResolve` drops the member with a
`ConformanceVerdict.SignatureNotPublished` at `Site.Nowhere`. A consumer resolving through the
signature never sees the member.

Two ways to close it; the first is recommended:

- **`Vesper.List` depends on `Vesper.Option`.** `Vesper.Option` depends on `Vesper.Core` alone,
  so the edge adds no cycle. Then `GetSlice` is transliterated from
  `FSharp.Core/prim-types.fs` (`List<'T>` augmentation, its `PrivateListHelpers` inlined as
  `Item`'s `nth` was) and `Vesper.List.mjs` is regenerated.
- **`option` moves into `Vesper.Core`**, where FSharp.Core declares it. Larger, and it touches
  every package that lists `Vesper.Option`.

Acceptance: the `SignatureNotPublished` finding on `Vesper.List` is gone from both corpus
suites, and `MemberMissingInImpl` reports `GetSlice` if the transliteration is removed.

## Settled questions

- **The module-decl guard stays syntactic.** See "What landed".
- **The `extern` species is carried, not re-derived.** `PublishedSurface.DeclaredReprs` maps a
  canonical intrinsic identity to `DeclaredRepr.Opaque` / `Heritable` / `Capability`, matched
  against the implementation's `Residue.IntrinsicReprKeys`. A capability on a target binding no
  repr publishes as a plain `Class`, so the species cannot be read off the shape.
- **`[<Import>]` is a codegen input.** Analysis records each well-formed import as a per-unit
  side product and the gate discharges it; emitting the import itself is out of scope.
- **Accessibility.** Conformance compares presence against the internal-or-better surface, so
  a private implementation declaration behind a public signature declaration is reported as
  missing. Carrying accessibility onto the surface for a cross-assembly public-only filter is
  `signature-front-end-followups-plan.md` item 1, not conformance's.
- **`[<Literal>]` signature values** publish through `registerValSig`, so a missing one is
  reported by identity.
- **`exception` declarations** are refused as `NotYetSupported` at the declaration and take no
  conformance verdict.

## Before this doc is deleted

- [ ] O1 landed; the `ExternalSymbol.Attributes` field and its doc comment are gone.
- [ ] O2 landed; `Vesper.List` publishes `GetSlice` on both targets.
- [x] No other doc, source or test comment cites this doc. `signature-front-end-followups-plan.md`'s
      two citations were replaced with the facts when this doc was rewritten.
