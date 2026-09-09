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

- Measure resolution has landed and merged. This plan reads `TyparKind`, `DeclaredTypar`,
  `MeasureTerm` and `FTMeasure`, whose atom is a `MeasureAtom` since step 4a.
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

2. **`TyparList`.** `{ Types; Measures; Order }` beside `Block<TyparKind>`; every reader of
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

3b. **`LocalOwners`.** Landed. `LocalOwner` and the `LocalBindingId -> LocalOwner` table on
   `FrozenPools` and on `TastFileG`, per the design doc's *Lexical ownership*. Written where
   the local generalises: `PassContext.PushLocalOwner` carries the declaration whose body is
   being typed, `inferBindingGroup` reads it once at group entry as the group's enclosing
   owner, and passes it explicitly to `EnsureBindingId` / `RecordScheme`, which record it on
   `BindingScheme` beside the `LocalBindingId`. `LocalOwner` is `LocalOwnerG<'m>` over the
   member identity: a member's `MemberKey` is not stable until its body is typed — an
   unannotated parameter settles the signature the key freezes — so the scheme entry holds a
   `LocalOwnerSite = LocalOwnerG<NominalMember>` naming the member by its registration, and
   `Elaborate.localOwners` maps it through `frozenNominalMemberKey` once the file is typed.
   Format version 9.

   Carried in the same diff:
   - `inferBindingGroup` takes a `BindingGroupHome`, so a module-level group's bindings are
     declarations (owner `ValueNone`) and an expression-level group's are locals, which
     require a pushed owner. A single-name local's id is minted BEFORE its right-hand side is
     typed, which is what lets a nested local name it.
   - `TypeMembersFill` carries the `NominalDecl` being filled, so a member's owner site is the
     registry's own `NominalMember`. A member binding whose member is unregistered is an
     internal error, which surfaced a rejected duplicate type declaration being walked against
     the first declaration's registration: `tryDeclaredClass` / `tryDeclaredNonClassHost` now
     resolve only to the registration made from the declaration itself.
   - `LocalOwner.Initialiser`, beyond the design doc's three cases, for a body with no key of
     its own: a module-level `do`, a module-level tuple binding's right-hand side, an
     auto-property initialiser, and a type's `let` / `do` preamble or secondary constructor.
     Step 3c decides what enclosing typars such a body contributes to a lifted local.
   - `FrozenTypeTableBuilder.InternBindingKey` and the `BindingKeyId` indexer, so a
     `ModuleFunction` owner writes as a binding-table row.

3c. **Generic locals on the CLR.** Landed. A local whose own typar survives generalisation
   is lifted to a generic static method on the Program class, `<name>@<n>`, with its
   captures as leading parameters and its typars as method typars, as `fsc` emits, instead
   of a closure class with a fixed `Invoke`. A non-saturated reference is eta-bridged to a
   closure over a saturated call (`EmitClosures.bridgeLiftedLocalEscapes`, member bodies
   included); a generalised local bound to a value (`let g = id`) is a parameterless
   generic method called as a generic module value is. Turns green: `locals/local-poly` on
   the CLR, pinned by the conformance run and the byte-identity goldens, and the same-file
   cases of `inline/inline-local-poly`, pinned in `LiftedLocalTests`; the served case is
   step 3d's.

   What landed differs from the sketch above in three places:
   - `FrozenPools.LocalSchemes`, a `BoundVarId -> LocalScheme` table (the local's
     `LocalBindingId` and own typar count), written by `Elaborate.localSchemes` off the
     scheme table. The backend reads a local's own scope off this row rather than off its
     leaves, and does not read `LocalOwners`: the enclosing scopes are the frame the
     discovery walk carries top-down, which is what closures already used. Format
     version 10.
   - `TyparFrame` (`TyparMarkers.fs`), an ordered list of `(scope, count)`, replaces
     `EnclosingTypars` and the `DeclaringTypars` offset. `Closure.Frame` and
     `LiftedLocal.Enclosing` carry it; the encoder resolves a leaf through
     `TyparSlots`: `Declared` (`!i` / `!!j`), `ClosureClass frame` (every scope a class
     slot) or `LiftedMethod frame` (every scope a method slot), replacing the integer
     `ClosureTyparScope`. A generic nominal's member-ref signature encodes under `Declared`
     whatever the ambient slots (`ClrGenerics.encodeDeclared`), which the lifted frame
     surfaced: under a closure frame the declaring type happened to sit at offset 0.
   - A same-file inline splice freshens the template's bound-variable keys, so the copy's
     local had no scheme row. `InlineReduction.freshenBody` files the entry under the fresh
     key too (`PassContext.ShareBindingEntry`), and `InlineExpand.FreshenedBoundVars`
     carries the row to each call site's copy in the backend.

   A local's captures are filed before its body is walked for closures, so a closure in the
   body that references the local, a generic `let rec` local's own recursive reference
   included, captures the local's captures and `call`s the lifted method; pinned in
   `LiftedLocalTests`. `fillTypeMembers` is a roughly 200-line nested closure that ought to
   be refactored.

3d. **A served local generalises again.** Landed. `InlineThaw.bodyAtPath` minted one cell per
   `(LocalBindingId, index)` for the whole body and quantified none of them, so a served
   local's typar reached `Freeze` unbound and degraded to `FTUnknown UnresolvedTypar`, which
   the CLR encoder then refused. The thawed local is generalised again in the host:
   `InlineReduction.lookupExternal` files a `BindingScheme` per thawed local through
   `PassContext.AdoptSplicedLocal`, quantifying the cells that local's leaves thawed to,
   under a host `LocalBindingId` minted per call so two call sites of one template are two
   locals. Turns green: `inline/inline-local-poly` on the CLR, pinned by the conformance run
   and the byte-identity goldens, and `CrossFileTests`'s served-local test. JS is already
   green on both programs because it erases types.

   Which typars a served local quantifies is read off the frozen leaves, not re-derived
   under a level discipline: `FTTypar(LocalFunction _, i)` is exactly the set the declaring
   file generalised, and a typar the local captured from the outer function (`f3` in the
   design doc) freezes under the enclosing `ModuleFunction` scope instead. What the wire was
   missing is which `let` each scope belongs to, so `InlineBody` gained `LocalSchemes`, the
   declaring file's rows for the served decl, built by `TastPoolBuilder.declTreeWithLocals`
   as it re-mints the body's bound variables.

   A spliced local's owner is `LocalOwner.Spliced template`, a new case: the declaration that
   owns it is in the declaring file, so the host's owner chain stops there. This matches the
   same-file splice, which keeps the template's own owner through `ShareBindingEntry`.
   Format version 11.

4. **Per-typar `ConstraintSet` and `FunctionScheme`.** Landed. `TypeTyparG<'ty>.Constraints`
   is a `ConstraintSetG<'ty>`: the `TyparConstraintKindG` cases as an `EqSet` plus the
   `default` targets in source order. `TyparListG<'ty>` is generic so `TTypeDeclG`,
   `TTypeMemberG` and `TAbstractMethodG` carry their typars as one list with the constraints
   on them (`TypeParams` / `MethodTypars`), and `TyparList = TyparListG<FrozenType>`.
   `TyparConstraintG`, `FrozenConstraint`, `TTypeParam`, the flat `EqSet`s and the `'ty`
   carrier of `MethodTypeParams` are deleted: the member walk adds each generic member's
   `TyTypar(Member _, i)` markers to the decl's freeze env directly, and `DeclaringType.ThisTy`
   is the full self type, so the per-member elaborator is gone.
   `FunctionScheme = { Typars; Traits: Block<MemberTrait> }` replaces `GenericFnScheme`;
   `ExternalConstraint` is deleted, its `Encodable` / `Default` cases living on the typar and
   `MemberTrait` on the scheme, which `ExternalSymbol.Generics` carries beside the `Scheme`
   template. `MemberKey.MethodTyparArity` is minted from `Types.Length`. The CLR reads `Types`
   alone for its `GenericParam` rows and arity names, pinned in `GenericParamFlagsTests` by a
   `[<Measure>]`-bearing record. Format version 12.

   Carried in the same diff, the project-local half of step 7: `FrozenSignature.addValue`
   publishes the binding's scheme, so a cross-file call is checked against its constraints,
   pinned by `CrossFileTests` (`'a : not struct` across two files on the CLR; the
   SemanticAnalysis suite's `none` target settles no layout, so the pin lives there).

   The two `fsc` parity gaps are pinned as `ptest`s in `FrozenConstraintTests.fscParityTests`
   with the gap quoted in the name. `fsc` orders typars by first appearance including the
   constraint clauses (`TFunc, T, U, S, E` for `StructSeq.map`, where this compiler gives
   `TFunc, S, E, T, U`), which needs the implicit typars' source positions at generalisation,
   which `mkMethodQuantTypars` does not track; and two coercions on one typar to the same generic
   interface unify their arguments under FS0064, where `addConstraintByKind` only dedupes
   structurally equal constraints.

4a. **Typar slots.** Landed, and it absorbed step 6. Step 4's review found two producers
   still numbering a leaf by signature slot (`mkDeclTyparEnv`, `publishedScheme`'s trait
   indices) against a `Types`-indexed consumer, and that a measure typar had no leaf to
   freeze to. The three numberings are now measure-tagged (`sigSlot`, `typeSlot`,
   `measureSlot`) over a `BlockM`, the tag sits on `FTTypar` / `TyTypar`, `MeasureAtom.Typar`
   exists, a `TypeKey` spells a `KeyArity`, and a measure argument is a `RootState`. The
   model is in `typar-scope.md` (*Leaves*, *Slot numbering*, *Measure arguments*). Format
   version 22; step 5's rows index a type slot.

5. **`GenericParamConstraint` rows.** Landed. `GenericParamRow.Constraints` carries the
   coercion targets read off the typar's `ConstraintSet`, and the assembler's finalise pass
   adds each row directly after its `GenericParam` row, so the table needs no row-order
   prediction: `AddGenericParameter` returns the owner handle, and adding in `GenericParam`
   order keeps the constraint table sorted by owner as SRM validates. A target resolves
   through `ClrEncoder.TypeDefOrRefOf`, shared with `InterfaceImpl`, under
   `TyparSlots.Declared`: a bare nominal is its `TypeDef` / `TypeRef`, an instantiation a
   `TypeSpec` whose sibling typars spell `!i` / `!!j`. `'a :> obj` adds no row, as `fsc`
   writes it. Pinned in `GenericParamConstraintTests` through the `MetadataStructure` readers
   `typeGenericParamConstraintsOf` / `methodGenericParamConstraintsOf`, which decode a
   `TypeSpec` target to its IL spelling (`` System.IComparable`1<!!0> ``).

   The encoding per constraint is `fsc`'s, read off FSharp.Core's metadata
   (`Option.ofNullable`, `Operators.using`, `Operators.isNull`, `NativePtr.stackalloc`,
   `FSharpEvent`2`) and an `fsi`-compiled probe of each clause in isolation. Three rows of
   the table as first drafted were not what `fsc` writes and were corrected: a plain `struct`
   is the value-type bit alone (the `DefaultConstructorConstraint` and the `System.ValueType`
   row on `ofNullable` come from `Nullable<'T>`'s imported constraints), `null` is the
   reference-type bit, and `enum<_>` / `delegate<_,_>` add no row.

   | Source constraint | CLI encoding |
   | --- | --- |
   | `'a : struct` | `NotNullableValueTypeConstraint` |
   | `'a : not struct` | `ReferenceTypeConstraint` |
   | `'a : null` | `ReferenceTypeConstraint` |
   | `'a : not null` | none; nullability attributes are the only carrier, step 9 |
   | `'a : (new : unit -> 'a)` | `DefaultConstructorConstraint` |
   | `'a :> Ty` | `GenericParamConstraint` to `Ty`, class or interface alike |
   | `'a : enum<'u>` | none |
   | `'a : delegate<_,_>` | none |
   | `'a : unmanaged` | `NotNullableValueTypeConstraint` + a `GenericParamConstraint` to `System.ValueType modreq(UnmanagedType)` + `IsUnmanagedAttribute` on the parameter, step 9 |
   | `'a : equality` / `comparison` | none; F# has no CLI encoding for these either |
   | SRTP member trait | none; an `inline` binding resolves it at the splice |

   Carried in the same diff: `CstTypeWalk.iterTypeMemberSig` stamps an abstract slot's own
   typar constraints (`abstract Only<'a when 'a :> IShape> : 'a -> 'a`), which reached
   `Unification.linkAbstractSlot` unstamped. A closure class's and a lifted local's rows stay
   positional and unconstrained, where `fsc` copies the enclosing constraints onto the
   closure class; nothing consumes those rows yet.

6. **`MeasureAtom.Typar`.** Landed under step 4a. The atom carries a `measureSlot`-tagged
   index and is minted at translation from the live `ScopedTypar` scope. `[<Measure>]` on a
   member or binding typar is read at `TypeBodyExtraction.memberTypars` and at `Infer`'s
   binding-typar read, so a module `let` and a member kind the same source the same way.
   Measure inference and measure-generic abbreviations
   (`type Meters<[<Measure>] 'u> = float<'u>`) remain out; `typar-scope.md` *Not yet
   inferred* holds the gaps and names their pins.

7. **Import.** A foreign generic's constraints are read off its metadata into the typar's
   `ConstraintSet`, so enforcement against a BCL or third-party generic exists. The
   `enum<'u>` constraint reads `ExternalTypeShape.Enum`'s underlying key; the
   `delegate<_,_>` constraint waits on `delegates-plan.md` stage 2.

   The project-local half landed with step 4: `FrozenSignature.addValue` publishes the
   binding's `FunctionScheme`, in the binding's `ModuleFunction` scope like the template, so
   a cross-file call is refused with the diagnostic the single-file `typar-*-violated.fs`
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
`typar-struct`, `typar-struct-record`, `typar-not-struct`, `typar-new`, `typar-null` and
`typar-null-allownull` goldens render `where T0 : struct`, `where T0 : class` and
`where T0 : new()`; no conformance program writes a coercion constraint, so the rows'
rendering is pinned by `GenericParamConstraintTests` alone.

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
- [x] `LocalOwners` records every generalised local's owner, pinned per owner kind and by
      the codec round-trip (step 3b), in `LocalOwnerTests`.
- [x] A `Type` leaf never freezes under a module function's body, pinned on a module
      function with a local (step 3b), in `LocalOwnerTests`.
- [x] `locals/local-poly` runs on the CLR (step 3c), pinned by the conformance run, the
      byte-identity goldens and `LiftedLocalTests`.
- [x] `inline/inline-local-poly` runs on the CLR and the `CrossFileTests` served-local
      `ptest` is a `test` (step 3d), with the served local's re-generalisation pinned in
      `LocalOwnerTests`.
- [x] `EnclosingScopes`, `TranslateCtx.MethodTyparScope` and `TyparOwner` are gone, and
      no `scopeOf` failwith remains in `EmitConstruct` or `TsManifestTypes` (step 3e).
- [x] `let (f, g) = (id, id)` at module level quantifies both typars, pinned by
      `ElaborateTests.moduleTupleBindingTests` and by `bindings/module-tuple-poly`, which
      calls each name at two types; JS runs it, the CLR is `pending` (step 3e).
- [x] A `.fsi`-declared generic member and its `.fs` implementation agree on scope once
      homed, pinned by `ConformanceTests` (`MemberTyparConformance`) and by
      `Vesper.Formatter` in the CLR suite. Re-pinned as plain equality in step 3a.
- [x] `TyparList.Order` is the only source of typar display order (`TyparListG.Names`);
      the CLR encoder indexes `Types` alone, pinned by a `[<Measure>]`-bearing type's
      `GenericParam` row count in `GenericParamFlagsTests` (step 4).
- [x] Every constraint kind in the encoding table has a `MetadataStructure` assertion or a
      row stating why it has none: the flag bits in `GenericParamFlagsTests`, the rows and
      the row-less kinds in `GenericParamConstraintTests`, `unmanaged` deferred to step 9
      (step 5).
- [x] The two `fsc` typar-order and FS0064 gaps have tests, `ptest` with the gap quoted in
      the name, in `FrozenConstraintTests` (step 4).
- [x] A cross-file constrained generic is enforced, pinned by the two-file `'a : not struct`
      program in `CrossFileTests` (step 4).
