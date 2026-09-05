# Typar scope — plan

Working document. Ephemeral: delete it when the work lands. The model it implements is
`typar-scope.md`, which is durable and wins where the two disagree.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Two scopes of work are interleaved here: the typar-scope model itself, and the remaining
typar-constraint emission and import stages, because the latter read the shapes the former
replaces. The constraint stages already landed are in the git log: `TyparConstraintG` and
`TyparConstraintKindG` in `SideTypes`, `GenericParamRow` flag bits through
`CliConstraintEncoding.ofKind`, and the closed `GenericFnScheme` (format version 6).

## Preconditions

- `measure-resolution-plan.md` has landed and merged. This plan reads `TyparKind`,
  `DeclaredTypar`, `MeasureTerm` keyed by `TypeKey` and `FTMeasure`.
- Nothing else is open in SemanticAnalysis. Step 3 rewrites `SemanticInfo.fs`,
  `SideTypes.fs`, the frozen codecs and every backend's typar encoding.
- `delegates-plan.md` stage 2 has either landed alone or waits until step 3 has. Two format
  bumps never interleave.

## Steps

Each step lands green on its own. Additive where a widely-used shape changes: the new shape
lands beside the old behind a central alias, readers swap, the old one is deleted in a
separate change.

1. **`MemberOrdinal` and `LocalBindingId`.** Minted at member registration and at
   generalisation respectively, stored on the member and binding tables. No consumers. An
   optional extension's members take an `ExtensionKey` plus an ordinal within the block.

2. **`TyparList`.** `{ Types; Measures; Order }` beside `EqArray<TyparKind>`; every reader of
   the kind array and of `TyparKinds.typeOnly` swaps to it; the array and the helper are
   deleted. A CLR metadata row, a TypeScript declaration and an intrinsic binding build a
   `TyparList` with empty `Measures`.

3. **`TyparScope` and the leaf.** `FTTypar of scope: TyparScope * index: int` and
   `TyTypar` likewise; `FTLocalTypar` folded in as a `LocalFunction` scope;
   `FrozenTypeBridge.ITyparInstantiation` takes a scope. The format bump. `TyparAxis` stays
   as a deleted-in-step-8 shim only if a reader cannot swap in this step.

4. **Per-typar `ConstraintSet` and `FunctionScheme`.** `TypeTypar.Constraints` holds the
   `TyparConstraintKindG` cases plus `Default`; `TyparConstraintG.TyparIndex` and the flat
   `EqSet` on `TTypeDeclG.TyparConstraints`, `TTypeMemberG.MethodTyparConstraints` and
   `TAbstractMethodG.MethodTyparConstraints` are deleted. `GenericFnScheme` becomes
   `FunctionScheme` with `Traits`; `ExternalConstraint.MemberTrait` moves into it and
   `ExternalConstraint.Encodable` / `Default` into the typar. `MemberKey.MethodTyparArity`
   reads `Types.Length`.

   Two `fsc` parity gaps land here, because this step first changes the order rows are read
   in: `fsc` orders typars by first appearance including the constraint clauses (`TFunc, T,
   U, S, E` for `StructSeq.map`, where this compiler gives `TFunc, S, E, T, U`), and two
   coercions on one typar to the same generic interface unify their arguments under FS0064
   where `addConstraintByKind` only dedupes structurally equal constraints.

5. **`GenericParamConstraint` rows.** No code in `src/` emits one. The table is added to the
   assembler with its row-order prediction, the same prefix-sum discipline as every other
   table there. Rows are read off the typar's `ConstraintSet`, so a row's typar is the record
   it hangs off. The `System.Delegate` row reads the kind alone, so it does not wait on
   `delegates-plan.md`. Encoding per constraint, each confirmed against a decompiled `fsc`
   output before it is committed to:

   | Source constraint | CLI encoding |
   | --- | --- |
   | `'a : struct` | `NotNullableValueTypeConstraint ||| DefaultConstructorConstraint` + a `GenericParamConstraint` to `System.ValueType` |
   | `'a : not struct` | `ReferenceTypeConstraint` |
   | `'a : (new : unit -> 'a)` | `DefaultConstructorConstraint` |
   | `'a :> Ty` | `GenericParamConstraint` to `Ty`, class or interface alike |
   | `'a : enum<'u>` | `GenericParamConstraint` to `System.Enum` |
   | `'a : delegate<_,_>` | `GenericParamConstraint` to `System.Delegate` |
   | `'a : unmanaged` | value-type flags + `IsUnmanagedAttribute` on the parameter |
   | `'a : null` / `not null` | none; nullability attributes are the only carrier, step 9 |
   | `'a : equality` / `comparison` | none; F# has no CLI encoding for these either |
   | SRTP member trait | none; an `inline` binding resolves it at the splice |

6. **`MeasureAtom.Typar`.** `MeasureTerm`'s atom widens from `TypeKey` to `MeasureAtom`;
   `[<Measure>]` on a member or binding typar is read at `mkMethodTypars` and at `Infer`'s
   binding-typar read, the two sites `measure-resolution-plan.md` step 2 left type-kinded by
   construction. Measure-generic abbreviations (`type Meters<[<Measure>] 'u> = float<'u>`)
   leave `NotYetSupported`.

7. **Import.** A foreign generic's constraints are read off its metadata into the typar's
   `ConstraintSet`, so enforcement against a BCL or third-party generic exists. The
   `enum<'u>` constraint reads `ExternalTypeShape.Enum`'s underlying key; the
   `delegate<_,_>` constraint waits on `delegates-plan.md` stage 2.

   The project-local half lands first: `FrozenSignature.addValue` publishes a binding's
   `ExternalSymbol` with `[]` constraints, so a cross-file call to a constrained generic is
   unenforced although the scheme holds the constraints. The published scheme and its
   constraints are both in the binding's `ModuleFunction` scope, so no re-scoping is needed
   between them. Verify with a two-file program whose second file violates the first file's
   `'a : struct` constraint, refused with the diagnostic the single-file `typar-*-violated.fs`
   programs pin.

8. **Deletions.** `TyparAxis`, `ConformanceTypars.normAxisTo` and `toDeclaringAxis`
   (re-expressed as a scope substitution where a caller survives), `SchemeId`,
   `TyparInstantiation.toAxis`. The Extractor and Manifest schema diagnostic code
   `method-axis-typar-erased` renamed, with its `Schema.DiagCode` case.

9. **`unmanaged` and the nullability attributes**, in that order, each with its own scope.

## Verify

Assert `GenericParam` flags and `GenericParamConstraint` rows through the `MetadataStructure`
helpers, per `Codegen.Clr`'s `CLAUDE.md` preference for metadata over reflection. The
`typar-*-violated.fs` programs pin front-end rejection and keep their exact diagnostics. The
`typar-struct`, `typar-struct-record`, `typar-not-struct` and `typar-new` goldens render
`where T0 : struct`, `where T0 : class` and `where T0 : new()`, and step 5 adds the rows'
rendering to them.

**Parity note.** `fsc` accepts `under L.A 3` for `'a : enum<'u>` on an `int64` enum, where
this compiler reports a mismatch on `3`. Both solve `'u` to `int64`; `fsc` then widens the
`int32` literal implicitly, a recent F# addition this compiler does not implement. The
divergence belongs to implicit widening, not to this plan.

## Migration checklist

Before this document is deleted, each row is in code or in a test:

- [ ] `TyparScope` is the only scope representation; `TyparAxis`, `FTLocalTypar` and
      `SchemeId` are gone.
- [ ] A `Type` leaf under a `ModuleFunction` chain is unrepresentable, pinned by a codec
      round-trip test that a `Member` and an `Extension` chain pass.
- [ ] `TyparList.Order` is the only source of typar display and conformance order; the CLR
      encoder indexes `Types` alone, pinned by a `[<Measure>]`-bearing type's `GenericParam`
      row count.
- [ ] Every constraint kind in the encoding table has a `MetadataStructure` assertion or a
      row stating why it has none.
- [ ] The two `fsc` typar-order and FS0064 gaps have tests, green or `ptest` with the gap
      quoted in the name.
- [ ] A cross-file constrained generic is enforced, pinned by the two-file `'a : struct`
      program.
