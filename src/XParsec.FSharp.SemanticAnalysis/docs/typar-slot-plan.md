# Typar slots — plan

Working document. Ephemeral: delete it when the work lands. The model it implements is
`typar-scope.md` (*Leaves*, *Typars and constraints*, *Measure arguments*), which is durable
and wins where the two disagree.

Line numbers are deliberately absent — they rot. Constructs and file names only.

This plan is sequenced inside `typar-scope-plan.md`: it lands after that plan's step 4 and
before its step 5, and it absorbs its step 6. The ordering note there points here.

## Preconditions

- `typar-scope-plan.md` step 4 has landed (format version 12). This plan reads
  `TyparListG<'ty>`, `TypeTyparG`, `ConstraintSetG`, `TyparSlot`, `FunctionScheme` and
  `MemberTrait`.
- Nothing else is open in SemanticAnalysis. Steps 2 and 3 each change a `FrozenType` leaf
  and bump the format; two format bumps never interleave.

## Root cause

A generic declaration numbers its typars three ways, and every site that holds an `int`
chooses one silently:

| Numbering | Indexes | Counted by |
| --- | --- | --- |
| signature slot | `TyparList.Order`: every typar, type- and measure-kinded, in canonical signature order | `TypeKey.TyparArity`, name resolution, a written `<'a, 'u>` argument list |
| type slot | `TyparList.Types` | the CLR `GenericParam` rows, `FTTypar` / `TyTypar` leaves, `MemberTrait.TyparIndices`, `FunctionScheme.TyparArity`, `instantiateSymbol`'s fresh variables |
| measure slot | `TyparList.Measures` | measure resolution only; erased by the CLR |

The three coincide whenever a declaration has no measure typar, which is every test but one,
so producers were free to treat `Order` and `Types` as the same array. Step 4 wrote the
convention down ("a typar leaf indexes `Types`") but two of its producers still number by
signature slot: `ElaborateTypars.mkDeclTyparEnv` mints `TyTypar(scope, i)` at the `protos`
position, measures included, and `SignatureResolutionContext.publishedScheme` derives
`MemberTrait.TyparIndices` from `tryFindIndex` over the declared list. `TyparList.ofKinded`
takes a signature-slot callback beside `positionalWith`'s type-slot callback in the same
module. A `let f<[<Measure>] 'u, 'a when 'a : equality>` published from a signature attaches
the constraint to the wrong typar or faults in `FunctionScheme.create`.

The deeper reason the confusion could persist is that a measure typar has no leaf. `MeasureTerm`
is a normalised `(TypeKey * Rational) list`, so `float<'u>` in a measure-generic record's
field has nothing to freeze to; `Translate.translateMeasure` reports `Measure.Typar` as
`NotYetSupported`, and `GenericParamFlagsTests`' measure-bearing record declares its `'u`
and leaves it unused. Once a measure typar has its own leaf, every producer must emit a type
leaf or a measure leaf per `Order` entry, and the two numberings cannot be conflated.

The fix is the one `ImmutableArrayM<'T, 'M>` in `XParsec.FSharp/UtilTypes.fs` already
applies to `token` and `line`: the index is a measure-tagged `int`, the array only accepts its
own tag, and a conversion between tags is a function with a name.

## Steps

Each step lands green on its own. Additive where a widely-used shape changes: the new shape
lands beside the old behind a central alias, readers swap, the old one is deleted in a
separate change.

1a. **`Vesper.Block`.** LANDED. `BlockM<'T, [<Measure>] 'M>` in `src/Vesper.Block`, a struct
   over `'T[]` carrying `Block`'s structural equality and comparison, with `Item`,
   `tryItem`, `init`, `mapi`, `iteri`, `tryFindIndex` and `Length` over `int<'M>`. The whole
   module is measure-generic, so there are no tagged counterparts and no untagged escape.
   `Block<'T> = BlockM<'T, 1>` indexes by a plain `int`. `Block<'T>` is an alias for
   `Block<'T>` and `module Block` delegates; both go with the last call site.

   The backing array is internal to the assembly: `AsSpan`, the indexer, enumeration and
   `Block.toArray` are the reads, and `Block.unsafeOfArray` is the one entry that does not
   copy. `sort`, `distinct` and `contains` keep their `Comparer`/`EqualityComparer` bodies
   rather than delegating to `Array`, which would impose `comparison` / `equality` on
   callers; `forall2`, `tryFind`, `tryFindIndex` and `mapPreserve` keep theirs for their
   return shapes.

1. **The three measures.** `[<Measure>] type sigSlot`, `typeSlot` and `measureSlot` in
   `SemanticScalars.fs` beside `TyparKind`.

   `TyparListG<'ty>` becomes `{ Types: BlockM<TypeTyparG<'ty>, typeSlot>; Measures:
   BlockM<MeasureTypar, measureSlot>; Order: BlockM<TyparSlot, sigSlot> }` with
   `TyparSlot.Type of int<typeSlot>` and `TyparSlot.Measure of int<measureSlot>`. Members:
   `TypeArity: int<typeSlot>`, `MeasureArity: int<measureSlot>`, and `Length` stays the
   untagged signature count because `TypeKey.TyparArity` reads it at sixty sites and is
   re-tagged in a later change. `TyparList.typeSlotOf: TyparListG<'ty> -> int<sigSlot> ->
   int<typeSlot> voption` and its measure twin are the only bridges from signature order;
   the reverse direction, needed by `Names` and the codec, is `sigSlotOf`.

   Constructors keep one index convention each, in the signature: `ofDeclared` keys its
   callback by `DeclaredTypar`; `positionalWith` keys by `int<typeSlot>`; `ofKinded`'s
   `int -> ConstraintSet` callback is deleted and `publishedScheme` buckets its `when`
   clauses by `DeclaredTypar` instead. `MemberTrait.TyparIndices: Block<int<typeSlot>>`;
   `publishedScheme`'s `indexOf` returns a `TyparSlot` through `Order`, and a measure typar
   in an SRTP support set is a diagnostic rather than a silently dropped index.
   `FunctionScheme.TyparArity: int<typeSlot>`. `TyparListG.TypeArity` and `HasTypeTypars`
   take the tag; they already replace every `.Types.Length` / `.Types.IsEmpty` read, so the
   change is compiler-led.

   Landed ahead of this plan, from the step 4 review, so step 1's diff is about the tags:
   `ConstraintSetG.Kinds` (was `Constraints`); `TyparListG.TypeArity` / `HasTypeTypars` at
   every CLR and front-end arity read; `Elaborate.recordFunctionScheme` reading the quantified
   typars by position instead of re-deriving the index from `TyTypar` markers (since step 2, it
   freezes the `GeneralizedTypars` through `methodTyparList`, the member path); `OpenSignature.OpenMethodSignature`
   deleted, `CodegenSymbols.TryLookupOpenSignature` reading `sym.Scheme` and `sym.Generics`
   directly; `SignatureResolutionContext.memberTraitOf` out of `publishedScheme`;
   `TyparList.coercions` under `TastLower.solvePhantomTypars`; `GenericParamRow.metadataName`
   over `TyparName.Display`.

   No format bump: the codec writes `int i` for every slot and the shapes on the wire are
   unchanged.

   Landed from the step 1 review. A tag is erased only where the consumer structurally
   requires an untagged `int`; a cast at a call site into repo-owned code means the callee's
   parameter is mistyped. The tagged surface now reaches: `ExternalSignature`
   (`DeclaringTyparArity`, `MethodTypars`, `MethodTyparArity`, every constructor),
   `ValReprG.Typars`, `LocalScheme.TyparArity`, `TyparName.Positional`, `TyparList.typeNames`,
   `TyparList.coercions` (a `Block`), `LocalTyparRoots.At`, `TastLower.matchInstantiation`
   and its siblings, and on the CLR side `declaringMarkers`, `FrameScope.Count`,
   `TyparFrame` (`Count`, `TryOffset`, the constructors), `payloadTyDeclaring`,
   `UnionPlacements.Fields`, `EmittedMember.MethodTyparCount`, `UserMemberKind.Member`,
   `RecoverOpenTypars`, the generic signature encoders, `Closure.Typars`, the
   `GenericUnionShape` / `GenericRecordShape` / `GenericClassShape` typar name blocks,
   `arityOfMetaName` and `methodTyparArityOf`. The remaining `int` casts sit at:
   - `System.Reflection.Metadata` (`genericParameterCount`, `GenericInstantiation`,
     `GenericTypeParameter`), the codec writers, and reflection / TypeScript-manifest reads.
   - `FTTypar` / `TyTypar` construction and the arrays they index, which step 2 tags.
   - `MemberKey.MethodTyparArity` and `TypeKey.TyparArity`, re-tagged with the key types in
     step 5; `SymbolKeyOps.arityName`, which formats either numbering.
   - `positionalWith`'s `Order` line, the one place the coincident numbering is asserted.

   Also landed here: `publishedScheme` buckets its `when` clauses by `int<typeSlot>` over the
   `Order` of `TyparList.unconstrained typeParams` and fills `Types` with one `mapi`; a `val`
   signature's explicit typars are kinded (`SignatureResolutionContext.explicitTypars`,
   `mkDeclaredTypars`), so `val f<[<Measure>] 'u, 'a when 'a : equality>` publishes the
   constraint on type slot 0 and an SRTP support set naming `'u` is FS0703. A member's own
   typars stayed type-kinded until step 2 kinded them at `TypeBodyExtraction.memberTypars`,
   because `InferOverload.frozenScopeEnv` and `MemberKey.MethodTyparArity` numbered by
   signature position. `translateConstraints`
   takes the declared typar block and reports FS0703 for a `when` clause on a measure-kinded
   typar of a type declaration; the impl-side binding path (`Infer.inferBinding`) still kinds
   every binding typar `Type`. `mkDeclTyparEnv` walks `Order` and mints one
   `TyTypar(scope, i)` per `Type` slot, so a measure typar has no env entry; this is step 2's
   `mkDeclTyparEnv` change, landed early because `FunctionScheme.create` refuses a
   signature-slot index at or past `TypeArity`.

2. **The type leaf carries `typeSlot`.** `FTTypar of scope: TyparScope * index:
   int<typeSlot>` and `TyTypar` likewise; `ITyparInstantiation.Typar`, the
   `FTFunctionTypar` / `TyFunctionTypar` active patterns, `TyparFrame`'s counts and every
   `TyparSlots` resolution in the CLR encoder take the tag. Pattern matches and codec writes
   need `int i` at most; the sites that stop compiling are the ones this step exists for.
   `mkDeclTyparEnv` already walks `Order` and skips a `Measure` slot (landed under step 1);
   here its `TyTypar` index takes the tag. `GeneralizedTypars.canonical` keeps a declared
   measure typar's kind, so `methodTyparList` files it under `Measures` and the type-slot
   numbering of the rest stays dense. `ExternalSymbols.instantiateSymbol` already allocates
   one fresh variable per `Types` entry through a tagged `Block`.

   Format version 18, because a `TypeRow.Typar` index that once meant a signature slot on a
   measure-generic type now means a type slot.

   Landed from the step 2 review. The tag reaches `TypeRow.Typar`, `SemTypeWalks.mapTypars`,
   `TyparRoots.At`, `Freeze.schemeBoundVars`, `InferOverload`'s `TrialBindings.MethodTypars`
   and `frozenScopeEnv`, and `InferExternalCall`'s method-typar seed. The remaining `int`
   erasures sit at the codec writer, `System.Reflection.Metadata`'s
   `GenericTypeParameter` / `GenericMethodTypeParameter`, reflection's
   `GenericParameterPosition`, the TypeScript manifest reader, and the mutable
   recovery arrays (`TastLower.matchTyparsPartial`, `ClrEncoder`'s slot arrays,
   `EmitCall`'s `instArr`), whose length is a key arity that step 5 re-tags.

   `frozenScopeEnv`, `GeneralizedTypars.methodEnv` and `ElaborateTypars.mkDeclTyparEnv` all
   number through `DeclaredTypar.typeKinded`, the one definition of which `Types` slot a
   declared typar takes; `DeclaredTypar.typeArity` is its length. `Infer.inferBinding` and
   `TypeBodyExtraction.memberTypars` kind a typar from its `[<Measure>]` attribute, so a module
   `let` and a member number the same source the same way. `ElaborateTypars.mkMethodQuantTypars`
   returns a `GeneralizedTypars` that `methodEnv` numbers and `methodTyparList` freezes, the
   same path a member takes, so a measure-kinded typar reaches the frozen `TyparList`'s
   `Measures` while taking no type slot. Its leaf waits on step 3.

3. **`MeasureAtom.Typar`.** Absorbs `typar-scope-plan.md` step 6. `MeasureTerm`'s atom widens
   from `TypeKey` to `MeasureAtom = Named of TypeKey | Typar of scope: TyparScope * index:
   int<measureSlot>`, per `typar-scope.md` *Leaves*; normalisation is unchanged, since an
   atom is still an ordered key with a rational exponent. `Translate.translateMeasure`'s
   `Measure.Typar` arm resolves the written name against the enclosing declaration's
   measure-kinded `DeclaredTypar`s, the way `Type.VarType(Typar.Named)` resolves a type
   typar, and yields the atom; `Measure.Anonymous` stays `NotYetSupported`.
   `writeMeasureTerm` gains the atom tag. Format version 19.

   What this step leaves out, and why:
   - Measure inference. A declared measure typar is a rigid atom within its declaration,
     equal only to itself, which `MeasureTerm` equality already gives. Generalising an
     unannotated measure (`let f x = x * 1.0<m>`) needs measure variables in the store and
     Abelian-group unification, and is its own plan.
   - Measure-generic abbreviations (`type Meters<[<Measure>] 'u> = float<'u>`) stay
     `NotYetSupported`, as `typar-scope-plan.md` step 6 said.

   Landed from the step 3 review. The atom resolves at TRANSLATE time, not through
   `mkDeclTyparEnv` as this step originally said: a measured type is a `TyVar` whose term the
   store holds in `Units`, so the term sits outside the `SemType` tree the deferred
   `TyVar -> TyTypar` cut walks, and no remap reaches it.
   The live typar scope maps a source name to a `ScopedTypar`: `Type of TyVarId` for a
   type-kinded typar, `Measure of MeasureAtom` for a measure-kinded one. `ScopedTypar.declare`
   is the one constructor of an entry, and assigns the measure slots; every site that pushes a
   declaration's typars passes the declaration's `TyparScope` to it, directly or through
   `PassContext.PushTyparScope(groups, strict)`. `translateMeasure` reads the atom from the
   scope by name; `translateType` reports a `Measure` entry in type position as FS0703.
   `CstKeys.measureOfType` widens to `Type.VarType`, because the parser spells the `'u` of
   `float<'u>` as a type argument.

   `underTypars` takes `(TyparScope * Block<DeclaredTypar>) list`, outer-to-inner, in place of
   the `outer`/`own` pair, so each group's measure typars take their atoms under their own
   scope.

   `Kind.MeasureParameterExpected` (FS0702) is new: a type-kinded typar in measure position.
   `Kind.ExplicitTyparsOnLocalBinding` (FS0665) is new: explicit typars on a binding whose
   owner is neither a module `let` nor a member, which is the one case with no `TyparScope`
   to declare under. An undeclared measure typar (`float<'zz>`, which fsc generalises) and a
   wildcard (`float<_>`, `float<'_>`) are `NotYetSupported`.

   A rigid atom is not instantiated at a call site, so `scale 1.0<m>` on
   `let scale<[<Measure>] 'u> (x: float<'u>) = x` reports `Measure mismatch: <m> vs <'m0>`.
   Pinned as a `ptest` in `MeasureResolutionTests`; it waits on the measure-inference plan.

4. **Backends erase, the front end does not.** The CLR encoder drops a measure argument when
   it expands a measured nominal's abbreviation body, so a `MeasureAtom.Typar` inside one is
   never encoded, and reads `Types` alone for its `GenericParam` rows. The sentence on
   `TyparListG` saying so moves out of `SemanticScalars.fs` and onto the CLR encoder, where
   it is a decision. The JS backend already erases every type. Neither backend reads
   `Measures`, `MeasureArity` or a measure atom; `TastFileG`, `FrozenPools` and the codec
   keep all three.

   Landed from the step 4 review. `MeasureErasure` sits in `XParsec.FSharp.Codegen.Common`,
   not in SemanticAnalysis: the reading is a backend one, and both backends take it. The
   decision itself is sited on `ClrEncoder.encodeType`'s measured arm and on
   `GenericParamRow.ofTypars`; `MeasureErasureTests` in `Codegen.Clr.Tests` pins the
   expansion.

   `FrozenTypeBridge.substituteDeclaring` and `pickInterfaceWitness` take a
   `BlockM<FrozenType, typeSlot>`, and `FrozenType.typeSlotArgs` is the one projection from a
   nominal's signature-order argument list to it. The untyped array they took before put a
   measure argument at type slot 0 at every caller. `FrozenType.MeasuredNominal` yields a
   `MeasuredClaim` carrying the claim's type-slot args, which `IMeasuredThaw.Measured` and
   `MeasureErasure.expandClaim` both instantiate the body over, so the front end and the
   backends read one shape. `ClrEncoder.encodeNominal` projects a user nominal's args the same
   way, one per `GenericParam` row. A source program cannot reach it yet: `Translate.apply`
   builds `Pair<m, int>` as a measured `TyVar` over `Pair<_, int>`, and `freezeWith` accepts a
   primitive carrier alone. `GenericParamFlagsTests` pins the by-name instantiation as a
   `ptest` for step 5.

5. **`` `N `` is written from one numbering and read as another.** `TypeKey.TyparArity` counts
   signature slots: `NameResolutionTypeRegistration.arityOfTypeName` is
   `typarSlotsOfTypeName |> List.length` with measures included, and it is the arity a source
   name must be written at (FS0033). Emission and reference then take that count from opposite
   numberings. `LayoutNodes` builds a `TypeDef`'s `MetaName` from `TypeParams.TypeArity`, so
   `type Pair<[<Measure>] 'u, 'a>` emits as `` Pair`1 `` with one `GenericParam` row, which
   `GenericParamFlagsTests` pins. `SymbolKeyOps.typeSegmentName` and `typeMetaName` render
   `` `N `` from `TypeKey.TyparArity`, so every `TypeRef` `ClrEnv` spells for that type —
   `typeRefOfKey`, `externalRecordRef`, `externalUnionRef`, `externalClassRef` — says
   `` Pair`2 ``, and so does the provider-store key. `typeRefOfKey`'s doc claims the emitted
   row and the matched identity cannot denote different types; for a measure-generic type they
   already do. `ClrEnv.arityOfMetaName` reads a suffix back as `int<typeSlot>`, which holds for
   a metadata name and fails for anything `typeSegmentName` produced.

   A key needs both numberings: the signature count keys resolution and FS0033, the type count
   spells the metadata name. Whether that is a second field on `TypeKey` or a `TyparList` shape
   reachable from it is open, and settles when the key types take their tags — the same change
   that re-tags `MemberKey.MethodTyparArity`, which conflates the two the same way.

   No test fails today: the measure-generic declaration in `GenericParamFlagsTests` is never
   referenced by name, so the `TypeDef` and the `TypeRef` spellings are never compared.

   Landed from the step 5 review, then reworked twice after review. `TypeKey.TyparArity` is
   a `KeyArity`, a two-case DU in `TyparSlots.fs`: `Compiled of int<typeSlot>` is the count
   a nominal type's metadata name spells, so a key minted from a CLR metadata row equals one
   minted from source, which is what lets a signature file overlay a referenced assembly's
   type; `Written of int<sigSlot>` is an abbreviation's written count, because an
   abbreviation has no compiled name and keys at every parameter, as F# does (`dotnet fsi`
   rejects `type T<'a> = 'a list` beside `type T<[<Measure>] 'u> = int` as a duplicate
   `` T`1 ``). `Compiled 1` and `Written 1` are distinct keys, so `float` and `float<'u>`
   stay apart with no cast between the two numberings. `TypeDeclKind.keyArity` is the rule
   for a claim and `ExternalTypeShape.KeyArity` for a published shape, which
   `PublishedSurfaceBuilder.addShape` checks against the key; `Translate.apply` reads the
   measure site off the key's case. `Elaborate` reads a declaration's claim back by decl
   site (`TypeRegistry.tryIdentityByDeclSite`) rather than re-deriving its key.
   `MemberKey.MethodTyparArity` is `int<typeSlot>`. The measures live in `TyparSlots.fs`,
   which moved out of `SemanticScalars.fs` so `CstKeys` can tag a written arity.

   The written count is a fact of the declaration, `TyparList.Order.Length`, read as
   `TypeIdentity.TyparArity` on a claim and `ExternalTypeShape.TyparArity` on a shape, and
   is not part of the identity: a backend that needs it reads the declaration. A claim is
   unique at its written count, so two nominals differing only in measure parameters are
   refused at declaration, although F# admits them, because no use site can tell them apart;
   `DuplicateTypeNameTests` pins the refusal.

   `typeSegmentName` renders the key's count, so `type Pair<[<Measure>] 'u, 'a>` is
   `` Pair`1 `` in the `TypeDef` row, in every `TypeRef` `ClrEnv` spells for it and in the
   provider-store key. A measured primitive's claim (`Vesper.float<'u>`) is `` Vesper.float`1 ``.

   The written arity carries `int<sigSlot>` from `CstKeys.TypeRef.TyparArity` through
   `NameResolutionLongIdent.resolveType`, `TypeRegistry`'s claim lookups,
   `NameResolutionContainers.WrittenArity.Exact`, `TypeIdentity.TyparArity`,
   `ResolvedItem.AmbiguousTypeArity` and each `ExternalTypeShape` arity member. Format
   version 22: a key's count crosses the wire with its case tag.

   Also landed here, because the `` `N `` agreement is only observable through a use site: a
   written measure argument stays an argument. `Translate.apply` places it at its signature
   slot as a metavar carrying the term and no carrier (`PassContext.MeasureTy`), and attaches
   it to the expansion instead only for a transparent abbreviation, which is what
   `MeasureSite` names. Such a root freezes to `FTMeasure` and thaws through
   `IMeasuredThaw.Measure`; `InferGeneralize.generalise` and `ResolvedTypes.addFreeRoots` read
   it as ground rather than as a free type variable. `FrozenType.openArgs` is the one
   projection from a declaration's `TyparList` to its open self-type's signature-order
   arguments. A use that instantiates a referenced declaration fresh reads the declaration's
   `TyparList` from the resolution that found it (`ExternalUnionCase.UnionTypars`,
   `ResolvedStamps.tryStaticQualifier`) rather than re-resolving the key.

   The erasure leaves one gap, unfixed: `type Pair<[<Measure>] 'u, 'a>` and `type Pair<'a>`
   emit one metadata name between them.

6. **A measure argument is a store state, not a type.** `PassContext.MeasureTy` mints a
   root with `Units = ValueSome` and `Link = ValueNone`, a third state beside a free root
   (`ValueNone, ValueNone`) and a measured carrier (`ValueSome, ValueSome`). The state is
   recognised by a guard at each consumer: `InferGeneralize.generalise` skips it,
   `ResolvedTypes.addFreeRoots` skips it, `FrozenTypeBridge.toFrozenWith` freezes it to
   `FTMeasure`. `UnificationEngine.unify`'s `TyVar tv, other` arm does not recognise it: a
   measure root meeting any non-`TyConst` type is silently linked and becomes a measured
   record or function. Only the `TyConst` case reports `DimensionlessMeasureMismatch`.

   Fix: make the root's kind a value. Either a `SemType` case for a measure argument, or a
   store-level root kind, `Free | Measure of MeasureTerm | Measured of MeasureTerm * carrier`,
   replacing the `Units` and `Link` pair. Each consumer's match becomes total, and the
   `unify` hole becomes a compile error at the arm that must report a measure meeting a type.
   `EngineCore.mergeUnits` and the `TyVar, TyVar` arm of `unify` are the merge rule and
   move onto the kind. Verify with a program unifying a measure argument against a record
   and against a function type, each pinned to report rather than link.

   Landed from the step 6 review. `RootState` in `TypeStore.fs` is the root's kind: `Free`,
   `Linked of target`, `Measure of units` and `Measured of units * carrier`, replacing the
   `Link` and `Units` arrays; `PassContext.MeasureTy` and `MeasuredTy` mint the two measure
   states, `TypeStore.SetLink` takes a bare `SemType`, and `TypeStore.IsFree` is the one
   "still a type variable" test, and `TypeStore.ErasedTarget` the one "type behind the
   root with its measure erased" projection (an alias target or a measured carrier), which
   `zonkErased` and the nominal / dot-access / validation walks recurse through. `resolveStep`
   is `UnionFind.zonkShallow`, so a `TyVar` reaching `unify`'s arms is never `Linked`;
   `substituteWith` follows `Linked` alone, and a `Measure` root is a leaf everywhere. A
   reader that treats a free root and a measure argument differently matches `RootState`
   totally. `GeneralizedTypars.canonical` had read a measure argument as a free root and
   quantified it as an inferred type-kinded typar, which is what hid the `FTMeasure` from
   the CLR encoder; `ClrGenerics.userTypeSpec` is the one user `TypeSpec` builder and
   projects `FrozenType.typeSlotArgs` like `encodeNominal`. `TypeRefVerdict.IsMeasure` is
   the one FS0704 classifier, applied in `translateTypeRef` before the verdict is applied
   and in `readTypeArgs` for a lone measure name in a type slot.

   `unify`'s `TyVar, TyVar` arm merges on the state pair: a measure argument meeting a
   measured value or, in the `TyVar, other` arm, any type reports FS0704 and leaves the
   classes apart; two measure arguments merge through `mergeUnits`, which is now
   `MeasureTerm -> MeasureTerm -> MeasureTerm`; a measured value meeting a type unifies its
   carrier against the type instead of overwriting it. `Translate.readTypeArgs` reports
   FS0704 for a lone measure name in a type slot (the parser spells `Box<m>`'s `m` as a type
   argument) and recovers the whole reference, so `let x: float<m> = 1.0<m>` over a local
   `type float<'a>` reports once, as fsc does. Pinned in `MeasureResolutionTests`, "a
   measure argument is a store state": the engine is driven directly, because translation
   refuses every written measure-meets-type program before `unify` sees it.

7. **The CLR backend's type arguments are a `Block`.** `FrozenNominal.Args` is a
   `Block<FrozenType>` and `FrozenType.typeSlotArgs` takes one, but the CLR backend passes
   a nominal's type arguments as `FrozenType list`. `EmitLower.keyAndTyArgs` converts the
   `Block` to a list at the top of the emitter, the list threads through
   `ClrProvider.GenericMemberRef` / `GenericClosureMemberRef`, `ClrGenerics.userTypeSpec`
   and the `genericXxxMemberRef` builders, `ClrExternalMembers.externalRecordCtor`,
   `externalUnionFactory`, `externalCaseSpec`, `externalUnionCaseTest`,
   `externalUnionCaseType` and the member-resolution entry points, `ClrEncoder.externalTypeSpec`
   and `methodSpec`, and each consumer converts back with `Block.ofList` to project the
   type slots. `TyparMarkers.declaringMarkers` builds a list that four of its seven callers
   immediately wrap in `Block.ofList`. A list is never iterated as a stack here, so the
   shape is wrong at both ends.

   Scope is the type-argument position only. A parameter list, a local signature and a
   closure's capture list (`paramTys`, `locals`, `captures`, `CaptureSigs`) are a different
   concept and stay as they are.

   Fix, additively as the working agreement prescribes for a widely used type: change
   `declaringMarkers` to return a `Block` and give every type-argument parameter the
   `Block<FrozenType>` type from `keyAndTyArgs` down to `typeSlotArgs`, deleting each
   `Block.toList` / `Block.ofList` pair as the two ends meet. `ICodegenProvider`'s members
   that carry type arguments (`TryEmitUnionCons`, `TryEmitRecordCons`, `UserGenericMemberRef`,
   `UserClosureMemberRef`, `ExternalUnionCaseTest`, `ExternalUnionCaseType` and
   `StaticFnMethodSpec`) change signature with the chain; the JS backend and
   `Codegen.Common` carry no `FrozenType list` type arguments and are untouched. Score the
   change by
   the conversions deleted: none should remain between `FrozenNominal.Args` and
   `typeSlotArgs`.

   Verify with the CLR conformance goldens byte-identical and `Codegen.Clr.Tests` green; a
   grep for `FrozenType list` in `Codegen.Clr` then names only parameter, local and capture
   lists.

   Not started.

## Verify

- `TyparListTests`: a list with a measure typar before a type typar reports `TypeArity`,
  `MeasureArity` and `Length` apart, and `typeSlotOf` / `sigSlotOf` round-trip through
  `Order`.
- `FrozenConstraintTests`: `let f<[<Measure>] 'u, 'a when 'a : equality> (x: float<'u>)
  (y: 'a)` freezes the constraint on `Types.[0]`, and the same binding published from a
  `.fsi` agrees (`SignatureResolutionTests`). An SRTP trait naming a measure typar in its
  support set is a diagnostic, quoted in the test name. Step 2 pins the half without the
  `float<'u>` parameter, which waits on step 3.
- `GenericParamFlagsTests`: the measure-bearing record test's `'u` is used, `{ V: float<'u> }`,
  and the row count stays one. Its comment about `float<'u>` being unresolved is deleted.
- `FrozenCodecRoundTripTests`: a `MeasureAtom.Typar` atom and every `TyparSlot` case
  round-trip.
- `MeasureResolutionTests`: `float<'u>` in a measure-generic record field, class field and
  member signature analyses without diagnostics; a measure wildcard stays `NotYetSupported`,
  pinned with the gap quoted in the name.
- `GenericParamFlagsTests`: a measure-generic type USED by name from another declaration
  resolves, so the `TypeRef` and the `TypeDef` agree on `` `N ``; a cross-package reference to
  one loads (step 5), pinned in `MeasureResolutionTests` against a referenced package whose
  key carries the two counts apart.
- The `typar-*` conformance programs and goldens are byte-identical after steps 1 and 2.

## Migration checklist

Before this document is deleted, each row is in code or in a test:

- [x] `BlockM` carries the tag, `Block` is its alias, and `Vesper.Block.Tests` pins the
      uninitialised value, ownership, equality and comparison (step 1a).
- [x] `EqArray` and its module are deleted; every call site names `Block` (step 1c).
- [x] `TyparList.Types`, `Measures` and `Order` accept only their own tag; no `int` indexes
      any of them (step 1).
- [x] `TyparList.ofKinded`'s index callback is gone; `publishedScheme` buckets by
      `int<typeSlot>` and `MemberTrait.TyparIndices` is `int<typeSlot>` (step 1).
- [x] A `val`'s measure-kinded typar publishes a later `when` clause on the type slot and an
      SRTP support set naming it is FS0703, pinned in `SignatureResolutionTests`; a type
      declaration's `when` clause on a measure typar is FS0703, pinned in
      `MeasureResolutionTests` (step 1).
- [x] Every `int` erasure of a slot tag sits at a metadata, codec, reflection or manifest
      boundary, at an `FTTypar` / `TyTypar` leaf, or at a key arity (step 1).
- [x] `TypeArity` is the only CLR arity read; no `.Types.Length` or `.Types.IsEmpty` remains
      outside `SemanticScalars.fs` (step 1).
- [x] `FTTypar` and `TyTypar` carry `int<typeSlot>`, pinned by the measure-before-type
      binding in `FrozenConstraintTests` (step 2).
- [x] `MeasureAtom.Typar` round-trips through the codec and `float<'u>` resolves in a field
      and a member, pinned in `MeasureResolutionTests`, `FrozenCodecRoundTripTests` and
      `FrozenCodecTreeRoundTripTests` (step 3).
- [x] The erasure rule lives on the CLR encoder, not on `TyparListG` (step 4).
- [x] A metadata name's `` `N `` is a type-slot count wherever it is written and wherever it is
      read; `TypeKey.TyparArity` is a `KeyArity` whose `Compiled` case is the type count and
      whose `Written` case is an abbreviation's signature count, with no cast between them;
      `MemberKey.MethodTyparArity` is the type count (step 5).
- [x] `typar-scope-plan.md` step 6 is struck and points here.
- [x] A measure argument root is a kind the store states, and `unify` reports a measure
      meeting a type rather than linking it (step 6).
- [ ] A nominal's type arguments reach `FrozenType.typeSlotArgs` as the `Block` that
      `FrozenNominal.Args` holds, with no list conversion between; `FrozenType list` in
      `Codegen.Clr` names only parameter, local and capture lists (step 7).
