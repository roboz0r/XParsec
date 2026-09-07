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

1. **The three measures and `EqArrayM`.** `[<Measure>] type sigSlot`, `typeSlot` and
   `measureSlot` in `SemanticScalars.fs` beside `TyparKind`. `EqArrayM<'T, [<Measure>] 'M>`
   in `EqArray.fs`, a struct over `EqArray<'T>` mirroring `ImmutableArrayM`: `Item` and
   `tryItem` over `int<'M>`, `Length: int<'M>`, `IsEmpty`, enumeration, and `Untagged` for
   the existing `EqArray` module functions. Only `init` and `mapi` get measured
   counterparts, because they are the functions that hand out an index; the module does not
   otherwise duplicate `EqArray`.

   `TyparListG<'ty>` becomes `{ Types: EqArrayM<TypeTyparG<'ty>, typeSlot>; Measures:
   EqArrayM<MeasureTypar, measureSlot>; Order: EqArrayM<TyparSlot, sigSlot> }` with
   `TyparSlot.Type of int<typeSlot>` and `TyparSlot.Measure of int<measureSlot>`. Members:
   `TypeArity: int<typeSlot>`, `MeasureArity: int<measureSlot>`, and `Length` stays the
   untagged signature count because `TypeKey.TyparArity` reads it at sixty sites and is
   re-tagged in a later change. `TyparList.typeSlotOf: TyparListG<'ty> -> int<sigSlot> ->
   int<typeSlot> voption` and its measure twin are the only bridges from signature order;
   the reverse direction, needed by `Names` and the codec, is `sigSlotOf`.

   Constructors keep one index convention each, in the signature: `ofDeclared` keys its
   callback by `DeclaredTypar`; `positionalWith` keys by `int<typeSlot>`; `ofKinded`'s
   `int -> ConstraintSet` callback is deleted and `publishedScheme` buckets its `when`
   clauses by `DeclaredTypar` instead. `MemberTrait.TyparIndices: EqArray<int<typeSlot>>`;
   `publishedScheme`'s `indexOf` returns a `TyparSlot` through `Order`, and a measure typar
   in an SRTP support set is a diagnostic rather than a silently dropped index.
   `FunctionScheme.TyparArity: int<typeSlot>`. `TyparListG.TypeArity` and `HasTypeTypars`
   take the tag; they already replace every `.Types.Length` / `.Types.IsEmpty` read, so the
   change is compiler-led.

   Landed ahead of this plan, from the step 4 review, so step 1's diff is about the tags:
   `ConstraintSetG.Kinds` (was `Constraints`); `TyparListG.TypeArity` / `HasTypeTypars` at
   every CLR and front-end arity read; `ElaborateTypars.quantEnvTyparList`, so
   `Elaborate.recordFunctionScheme` reads a `mkMethodQuantEnv` result by position instead of
   re-deriving the index from its `TyTypar` markers; `OpenSignature.OpenMethodSignature`
   deleted, `CodegenSymbols.TryLookupOpenSignature` reading `sym.Scheme` and `sym.Generics`
   directly; `SignatureResolutionContext.memberTraitOf` out of `publishedScheme`;
   `TyparList.coercions` under `TastLower.solvePhantomTypars`; `GenericParamRow.metadataName`
   over `TyparName.Display`.

   No format bump: the codec writes `int i` for every slot and the shapes on the wire are
   unchanged.

2. **The type leaf carries `typeSlot`.** `FTTypar of scope: TyparScope * index:
   int<typeSlot>` and `TyTypar` likewise; `ITyparInstantiation.Typar`, the
   `FTFunctionTypar` / `TyFunctionTypar` active patterns, `TyparFrame`'s counts and every
   `TyparSlots` resolution in the CLR encoder take the tag. Pattern matches and codec writes
   need `int i` at most; the sites that stop compiling are the ones this step exists for.
   `mkDeclTyparEnv` walks `Order`, mints a `TyTypar(scope, i<typeSlot>)` per `Type` slot,
   and skips a `Measure` slot until step 3 gives it a leaf. `GeneralizedTypars.canonical`
   keeps a declared measure typar's kind, so `methodTyparList` files it under `Measures` and
   the type-slot numbering of the rest stays dense. `ExternalSymbols.instantiateSymbol`
   allocates one fresh variable per `Types` entry, as it already assumes.

   Format version 13, because a `TypeRow.Typar` index that once meant a signature slot on a
   measure-generic type now means a type slot.

3. **`MeasureAtom.Typar`.** Absorbs `typar-scope-plan.md` step 6. `MeasureTerm`'s atom widens
   from `TypeKey` to `MeasureAtom = Named of TypeKey | Typar of scope: TyparScope * index:
   int<measureSlot>`, per `typar-scope.md` *Leaves*; normalisation is unchanged, since an
   atom is still an ordered key with a rational exponent. `Translate.translateMeasure`'s
   `Measure.Typar` arm resolves the written name against the enclosing declaration's
   measure-kinded `DeclaredTypar`s, the way `Type.VarType(Typar.Named)` resolves a type
   typar, and yields the atom; `Measure.Anonymous` stays `NotYetSupported`. `mkDeclTyparEnv`
   pairs each `Measure` slot with its atom, so `FTMeasure` over a typar atom freezes and
   thaws through `IMeasuredThaw.Measured` as a named atom does. `writeMeasureTerm` gains the
   atom tag. Format version 14.

   What this step leaves out, and why:
   - Measure inference. A declared measure typar is a rigid atom within its declaration,
     equal only to itself, which `MeasureTerm` equality already gives. Generalising an
     unannotated measure (`let f x = x * 1.0<m>`) needs measure variables in the store and
     Abelian-group unification, and is its own plan.
   - Measure-generic abbreviations (`type Meters<[<Measure>] 'u> = float<'u>`) stay
     `NotYetSupported`, as `typar-scope-plan.md` step 6 said.

4. **Backends erase, the front end does not.** The CLR encoder drops a measure argument when
   it expands a measured nominal's abbreviation body, so a `MeasureAtom.Typar` inside one is
   never encoded, and reads `Types` alone for its `GenericParam` rows. The sentence on
   `TyparListG` saying so moves out of `SemanticScalars.fs` and onto the CLR encoder, where
   it is a decision. The JS backend already erases every type. Neither backend reads
   `Measures`, `MeasureArity` or a measure atom; `TastFileG`, `FrozenPools` and the codec
   keep all three.

## Verify

- `TyparListTests`: a list with a measure typar before a type typar reports `TypeArity`,
  `MeasureArity` and `Length` apart, and `typeSlotOf` / `sigSlotOf` round-trip through
  `Order`.
- `FrozenConstraintTests`: `let f<[<Measure>] 'u, 'a when 'a : equality> (x: float<'u>)
  (y: 'a)` freezes the constraint on `Types.[0]`, and the same binding published from a
  `.fsi` agrees (`SignatureResolutionTests`). An SRTP trait naming a measure typar in its
  support set is a diagnostic, quoted in the test name.
- `GenericParamFlagsTests`: the measure-bearing record test's `'u` is used, `{ V: float<'u> }`,
  and the row count stays one. Its comment about `float<'u>` being unresolved is deleted.
- `FrozenCodecRoundTripTests`: a `MeasureAtom.Typar` atom and every `TyparSlot` case
  round-trip.
- `MeasureResolutionTests`: `float<'u>` in a measure-generic record field, class field and
  member signature analyses without diagnostics; a measure wildcard stays `NotYetSupported`,
  pinned with the gap quoted in the name.
- The `typar-*` conformance programs and goldens are byte-identical after steps 1 and 2.

## Migration checklist

Before this document is deleted, each row is in code or in a test:

- [ ] `TyparList.Types`, `Measures` and `Order` accept only their own tag; no `int` indexes
      any of them (step 1).
- [ ] `TyparList.ofKinded`'s index callback is gone; `publishedScheme` buckets by
      `DeclaredTypar` and `MemberTrait.TyparIndices` is `int<typeSlot>` (step 1).
- [ ] `TypeArity` is the only CLR arity read; no `.Types.Length` or `.Types.IsEmpty` remains
      outside `SemanticScalars.fs` (step 1).
- [ ] `FTTypar` and `TyTypar` carry `int<typeSlot>`, pinned by the measure-before-type
      binding in `FrozenConstraintTests` (step 2).
- [ ] `MeasureAtom.Typar` round-trips through the codec and `float<'u>` resolves in a field
      and a member, pinned in `MeasureResolutionTests` and `FrozenCodecRoundTripTests`
      (step 3).
- [ ] The erasure rule lives on the CLR encoder, not on `TyparListG` (step 4).
- [ ] `typar-scope-plan.md` step 6 is struck and points here.
