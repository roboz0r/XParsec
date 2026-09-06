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
   generalisation respectively, stored on the member and binding tables. No consumers.
   `ExtensionKey` waits for the type extension design; the front end rejects a detached
   `type … with` block, so there is nothing to key.

2. **`TyparList`.** `{ Types; Measures; Order }` beside `EqArray<TyparKind>`; every reader of
   the kind array and of `TyparKinds.typeOnly` swaps to it; the array and the helper are
   deleted. A CLR metadata row, a TypeScript declaration and an intrinsic binding build a
   `TyparList` with empty `Measures`.

3. **`TyparScope` and the leaf.** Landed. `FTTypar of scope: TyparScope * index: int` and
   `TyTypar` likewise; `FTLocalTypar` folded in as a `LocalFunction` scope;
   `FrozenTypeBridge.ITyparInstantiation` takes a scope. Format version 7. `TyparAxis`,
   `SchemeId`, `normAxisTo`, `toDeclaringAxis`, `reaxisTo` and `reaxisMethodTypars` are
   deleted, with no shim. `Freeze.schemeBoundVars` reads the `LocalBindingId` recorded at
   generalisation.

   What this step leaves out, and why:
   - `TyparScope.Extension`: no `ExtensionKey` yet (step 1).
   - A keyless module binding (`let (a, b) = …`) has no `ModuleFunction` scope, so its free
     typars freeze to `FTUnknown UnresolvedTypar` instead of being quantified.

   First landed with `Member of owner * MemberOrdinal`, which the review of 2026-09-06
   found to be an identity nothing reads: a `.fsi` and its `.fs` number their members
   independently (`Vesper.Formatter` interleaves private members and extra constructors),
   so every comparison erased the ordinal and the homed signature was re-scoped onto the
   implementation's numbering. Step 3a removed it.

3a. **`Member of owner: TypeKey`.** Landed. The ordinal left the scope;
   `ConformanceTypars.rescopeToImplementation`, `FrozenType.rescopeMemberTypars`,
   `UnificationInferOverload.comparisonScope`, `ClassMemberDeclaring.OrdinalOf`,
   `MetadataSymbols.methodOrdinal`, the manifest translator's `InMember` ordinal and the
   `resolveMember` ordinal parameter are deleted; `checkMembers` compares by equality as
   `schemesAgree` does; `TTypeMemberG.Ordinal` became `Key: MemberKey`, minted at
   elaboration by `frozenUserMemberKey` and interned through the pool's member-key table
   (`FrozenCodecTypes.writeMemberKeyRef`). `MemberOrdinal` is deleted outright: its only
   remaining reader was `TypeBodyMembers.PrimaryCtor.IsSome`, now `HasPrimaryCtor: bool`.
   `ElaborateMembers.DeclaringType` carries the type's key, typars and registered members
   as data, and the member walk resolves each site's `TypeMemberInfo` once for both its
   `MemberKey` and its method typars. Format version 8.

   Carried in the same diff:
   - `TyparScope.IsLocal` beside `IsFunction`; `EmitResolve.paramAccepts` is one typar arm
     and one constructor arm, `EmitClosures.ftNoUnknown` reads the flag.
   - The `sameTyCtor` doc comment sits on `sameTyCtor`.
   - `Elaborate.fs` reads `MemberNames.ofBinding` once: `translateModuleLet` mints the
     `BindingKey` and both `moduleLetQuantEnv` and `exportedBindingInfo` take it.
   - `ConformanceTests.fs`'s helper was already `mTypar`; the two tests pinning the
     ordinal erasure and the re-scoping are deleted, and the plain-equality test remains.
   - `UnificationInferOverload.levelSignature` still takes one shared scope, the head
     level's `Member` scope, so a base's and a derived's `M<'a>('a)` dedupe as one
     signature.

3e. **Scope plumbing cleanups.** Landed. Each deleted a runtime check or a duplicate type:
   - `EmitTypes.EnclosingScopes` and `EmitConstruct.scopeOf` are gone. `EnclosingTypars`,
     the enclosing type's key and typar count plus the enclosing member's or module
     function's scope and own count, is built at each discovery root (`Layout`'s member and
     preamble roots, `discoverClosures`'s static-fn root) and stored on `Closure.Enclosing`.
     `Closure.Typars` and `Closure.DeclaringTypars` are members over it, and the
     construction site passes its `Instantiation` to `UserClosureMemberRef`. Step 3c
     replaces the record with the owner chain. `StaticFn`, `ModuleValue` and
     `MethodKey.StaticFn` / `FieldKey.ModuleValue` carry a `BindingKey`; the provider's
     cross-file tables still key by the `SymbolKey` a reference spells.
   - `TsManifestTypes.TranslateCtx` carries one `Scope: TyparScope voption`. Under
     `Member owner`, `Typar i` resolves to `Type owner` and `MethodTypar j` to the member;
     elsewhere a `MethodTypar` is a corrupt manifest.
   - `SignatureResolutionContext.TyparOwner` is deleted; `scopedEnv ctx scope typars` and
     `memberEnv ctx owner declaring own` replace `typarEnv`.
   - `FrozenTypeBridge.TyparInstantiation.ofScopes onType onMember onFunction onLocal`, one
     arm per scope kind, builds every instantiation: `declaringOnly`, `openMethod`,
     `atCallSite`, `identity` and `InlineThaw.bodyAtPath`; `mintLocals` is the memoised
     local-typar arm the last two share.
   - A module tuple binding (`let (f, g) = …`) generalises per name, as `fsc` does:
     `InferGeneralize.generalisedKeys` yields every name the pattern binds, and
     `Elaborate.moduleLetValues` quantifies each under a `ModuleFunction` scope keyed by
     its own name, recording a `GenericFnScheme` per name. Pinned by
     `ElaborateTests.moduleTupleBindingTests` and by the `bindings/module-tuple-poly`
     conformance program, which JS runs through a new module-scope destructuring
     (`EmitJs`, closing the `TypeAbbreviationTests` GAP). The CLR is `pending`: it lowers a
     generic module VALUE to a generic static method only when the value is not
     function-typed (`collectGenericModuleValues` defers `let f : 'T -> 'T = id`), and a
     tuple binding has no per-name declaration to lower that way; until then the program
     reaches `Main` with open typars and fails to load. One explicit `'a` written on two
     names of a tuple pattern (`let (f: 'a -> 'a, g: 'a -> 'a) = …`) has one freeze target in
     the decl but two scopes to quantify under; `Elaborate.declQuantEnv` reports it as
     `NotYetSupported`, pinned in `ElaborateTests.moduleTupleBindingTests`.
   - `MethodScopeGenericTests.fs`, `MethodScopeSingleCandidateTests.fs` and the wire string
     `method-scope-typar-erased` (`Schema.DiagCode.MethodScopeTyparErased`) carry the new
     word; the `es2015` fixture manifest and the extractor burndown ranking were re-spelled.

3b. **`LocalOwners`.** `LocalOwner` and the `LocalBindingId -> LocalOwner` table on
   `FrozenPools`, per the design doc's *Lexical ownership*. Written where the local
   generalises: `Unification` is inside exactly one declaration at that moment, and the
   scheme entry that already records `LocalBindingId` records the owner beside it, so
   `Freeze` is a reader. A member's `MemberKey` is resolved by `LocalMemberKeys` before
   the freeze, so the owner can carry it. The per-declaration freeze is no longer a
   prerequisite. Pinned by a test that a module function's local is owned by its
   `BindingKey`, a member's by its `MemberKey`, and a nested local by the outer local; and
   by the codec round-trip.

3c. **Generic locals on the CLR.** A local whose own typar survives generalisation is
   lifted to a generic static method with the local's typars as method typars, as `fsc`
   emits, instead of a closure class with a fixed `Invoke`. The lifted method's enclosing
   typars are the owner chain from `LocalOwners`, which replaces the counts and scopes
   `EmitTypes.ClosureClass` threads today (`Typars`, `DeclaringTypars`, `Enclosing`).
   `ClrEncoder.encodeType`'s `LocalFunction` arm stops being reachable from a lifted
   local's own signature; it stays for a `Vesper.Fun`-boxed value. Turns green:
   `locals/local-poly` on the CLR (`pending` in the manifest), and the CLR half of the
   same-file cases in `inline/inline-local-poly`.

3d. **A served local generalises again.** `InlineThaw.bodyAtPath` mints one cell per
   `(LocalBindingId, index)` for the whole body, so a served local used at two types
   fails to unify. The thawed local is run through the host's ordinary generalisation
   at its `let`, under the host's level discipline, so a typar the local captures from
   the outer function (`f3` in the design doc) stays un-generalised. The re-generalised
   local gets a host `LocalBindingId` and an owner entry in the host's `LocalOwners`.
   Turns green: `inline/inline-local-poly` on the CLR, and `CrossFileTests`'s `ptest`
   "file 2 expands file 1's inline body whose LOCAL is used at two types". JS is already
   green on both programs because it erases types; the thaw defect is unobservable there
   until a JS test reads the thawed scheme.

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

8. **Deletions and the deferred parts of step 3.** `TyparScope.Extension` once
   `ExtensionKey` exists. `TyparAxis`, `normAxisTo`, `toDeclaringAxis` and `SchemeId` are
   already gone (step 3), `MemberOrdinal` with step 3a, and the `method-axis-typar-erased`
   diagnostic code was renamed in step 3e.

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

- [x] `TyparScope` is the only scope representation; `TyparAxis`, `FTLocalTypar` and
      `SchemeId` are gone.
- [x] Every `TyparScope` case round-trips through the codec, pinned by
      `FrozenCodecRoundTripTests`.
- [x] `TyparScope.Member` carries no ordinal; `rescopeToImplementation`,
      `rescopeMemberTypars` and `comparisonScope` are gone, and `Vesper.Formatter` still
      conforms in the CLR suite (step 3a).
- [ ] `LocalOwners` records every generalised local's owner, pinned per owner kind and by
      the codec round-trip (step 3b).
- [ ] A `Type` leaf never freezes under a module function's body, pinned on a module
      function with a local (step 3b).
- [ ] `locals/local-poly` runs on the CLR (step 3c).
- [ ] `inline/inline-local-poly` runs on the CLR and the `CrossFileTests` served-local
      `ptest` is a `test` (step 3d).
- [x] `EnclosingScopes`, `TranslateCtx.MethodTyparScope` and `TyparOwner` are gone, and
      no `scopeOf` failwith remains in `EmitConstruct` or `TsManifestTypes` (step 3e).
- [x] `let (f, g) = (id, id)` at module level quantifies both typars, pinned by
      `ElaborateTests.moduleTupleBindingTests` and by `bindings/module-tuple-poly`, which
      calls each name at two types; JS runs it, the CLR is `pending` (step 3e).
- [x] A `.fsi`-declared generic member and its `.fs` implementation agree on scope once
      homed, pinned by `ConformanceTests` (`MemberTyparConformance`) and by
      `Vesper.Formatter` in the CLR suite. Re-pinned as plain equality in step 3a.
- [ ] `TyparList.Order` is the only source of typar display and conformance order; the CLR
      encoder indexes `Types` alone, pinned by a `[<Measure>]`-bearing type's `GenericParam`
      row count.
- [ ] Every constraint kind in the encoding table has a `MetadataStructure` assertion or a
      row stating why it has none.
- [ ] The two `fsc` typar-order and FS0064 gaps have tests, green or `ptest` with the gap
      quoted in the name.
- [ ] A cross-file constrained generic is enforced, pinned by the two-file `'a : struct`
      program.
