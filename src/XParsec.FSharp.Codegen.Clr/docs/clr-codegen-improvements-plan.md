# CLR codegen improvements — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the decompiled-C# conformance goldens (`test/XParsec.FSharp.Codegen.Clr.Tests/goldens/*.clr.cs`).
Landed and removed from this document: A1 (ctor-param backing field `initonly`, `FieldAttrSets`),
A2 (record field as private backing field with accessors, `readonly struct`), A4 (residue field
at assembly visibility, `FieldVisibilitySweepTests`), A5 (source parameter names,
`ParamNaming.paramNames`), B1 (`InlineExpand.reduceLets`), B2 (`ExprPos`, statement-position
`unit`) and C1 (`Decompile.ReferenceResolver`). Their history is in the git log.

What remains is A3 (typar constraints into IL), A6 (delegate types) and C2 (wider goldens).

**Decided representations.** A record field emits as a private `initonly` field with a public
getter. A typar constraint is enforced across assemblies and written into IL wherever the CLI can
express it. A delegate is a modelled type category on both targets.

---

# A3. Typar constraints are never emitted

`typar-struct.clr.cs`, for `let onlyStruct<'a when 'a: struct> (x: 'a) = x`:

```csharp
public static T0 onlyStruct<T0>(T0 arg0)
```

No `where T0 : struct`. The same holds in `typar-not-struct`, `typar-null` and `typar-not-null`.

## Where it stands

Stages 1 and 2 have landed. A bound is `TyparConstraintG<'ty>` in `SideTypes`: a `TyparIndex` on
the owner's axis and a `TyparConstraintKindG<'ty>` over `Equality`, `Comparison`, `Struct`,
`ReferenceType`, `Nullness`, `NotNull`, `Coercion of 'ty`, `DefaultConstructor`, `Unmanaged`,
`Enum of underlying` and `Delegate of args * ret`. It is carried as an `EqSet` on
`GenericFnSchemes`, `TTypeDeclG.TyparConstraints`, `TTypeMemberG.MethodTyparConstraints`,
`TAbstractMethodG.MethodTyparConstraints` and `CodegenOpenSignature.Constraints`. Stored order is
source order, so the rows stage 4 emits are deterministic. `FrozenCodec.FormatVersion` is 5.

The verdicts live in `UnificationConstraintCheck` over the `NominalDecl` view; the `delegate`
clause reports `NotYetSupported` until A6 stage 1. The SRTP member trait and `Default` are
deliberately absent from `TyparConstraintKindG`: an `inline` binding publishes a trait through
`ExternalConstraint.MemberTrait` beside its `InlineBody`, and a non-`inline` binding with a trait
is FS0670.

## Owed from the stage 2 review

`LocalNominal.fieldTypes` now answers a class with its primary-ctor parameters, `val` fields
and instance preamble `let`s, so a `[<Struct>]` class holding a reference or a function through
a ctor parameter is refused under `unmanaged` and `equality`, as `fsc` refuses it whether or
not a member reads the parameter. `ConstraintsTests` pins both.

The enum underlying type across an assembly boundary has landed: `ExternalTypeShape.Enum`
carries the underlying `TypeKey`, written from `TEnumCases.underlyingTypeKey` by
`SignatureResolution` and `FrozenSignature`, and derived as `int`, `string` or `obj` by the TS
manifest provider. `enumUnderlyingType`, `AttributeFold` and `Unmanagedness` read it.
`ConstraintsTests` pins an imported `int64` enum under `enum<'u>`. No format bump: the shape
is projected from the frozen `TTypeKindG.Enum` cases, whose `TConstValue` already carries the
width, so the blob layout is unchanged and `FrozenCodec.FormatVersion` stays 5.

## Target encoding

One row per source constraint, calibrated against what `fsc` emits for the same source before
it is committed to. `dotnet fsi` plus a decompile of an `fsc` output is the oracle.

| Source constraint | CLI encoding |
| --- | --- |
| `'a : struct` | `NotNullableValueTypeConstraint ||| DefaultConstructorConstraint` + a `GenericParamConstraint` to `System.ValueType` |
| `'a : not struct` | `ReferenceTypeConstraint` |
| `'a : (new : unit -> 'a)` | `DefaultConstructorConstraint` |
| `'a :> Ty` | `GenericParamConstraint` to `Ty`, class or interface alike |
| `'a : enum<'u>` | `GenericParamConstraint` to `System.Enum` — confirm against `fsc` |
| `'a : delegate<_,_>` | `GenericParamConstraint` to `System.Delegate` — confirm against `fsc` |
| `'a : unmanaged` | value-type flags + `IsUnmanagedAttribute` on the parameter |
| `'a : null` / `not null` | none; nullability attributes are the only carrier, and they belong with the wider nullability work |
| `'a : equality` / `comparison` | none — F# has no CLI encoding for these either |
| SRTP member trait | none — an `inline` binding resolves it at the splice, so no generic parameter survives to carry it |

## Remaining stages

3. ~~**Flag bits on the existing `GenericParam` row**: `struct`, `not struct`, `new()`.~~
   Landed. `GenericParamRow` in `GenericParamRows` pairs a row's name with its
   `GenericParameterAttributes`, folded from the owner's constraint set through
   `CliConstraintEncoding.ofKind`, the single classification of every bound kind into its CLI
   encoding (flag bits, a `GenericParamConstraint` row, the unmanaged attribute, or none),
   which stage 4 extends by consuming the row cases;
   `PreparedMethod.MethodTypars` and `TypeSlot.Typars` carry it for every owner (module
   function, type declaration and its union case types, member, interface slot), and
   `AddGenericParameter` writes it. `GenericParamFlagsTests` asserts the bits per owner through
   `MetadataStructure.typeGenericParamsOf` / `methodGenericParamsOf`, and the `typar-struct`,
   `typar-struct-record`, `typar-not-struct` and `typar-new` goldens render `where T0 : struct`,
   `where T0 : class` and `where T0 : new()`.
4. **A closed scheme per generic function.** A function's scheme is one record of its typar
   arity and its bounds, where every `TyparIndex` and every `FTTypar(Method, i)` inside a bound
   is below the arity, checked by the constructor. Today the two halves are the separate side
   tables `GenericFnSchemes` and `BindingTyparArities`, and the CLR backend ignores the second:
   `Emit.staticFnTypars` re-derives arity as the maximum method index over params, result and
   body. The record replaces both tables and the sweep, `Emit.StaticFn` carries it, and
   `GenericParamRow.ofTypars` takes it in place of a name sequence and a bare `EqSet`. Its
   guard on an out-of-range `TyparIndex` goes with the sweep.

   Prerequisite, landed with this plan revision: a nominal instantiation substitutes the
   instance's args through each copied bound's target. `freshNamedInstance` and
   `freshConstrainedTyVar` copied the prototype's bounds verbatim, so a caller's typar acquired
   bounds over a foreign declaration's typars and `mkMethodQuantEnv`'s dependent-typar fixpoint
   quantified them. `StructSeq.map` recorded ten bounds over ten method typars for a five-typar
   method. `ExternalSymbols` already substituted; the local paths now match it.

   Two parity gaps surfaced by the same `fsc` probe, deferred to stage 5 where they first change
   output: `fsc` orders typars by first appearance including the constraint clauses
   (`TFunc, T, U, S, E` for `map`, where this compiler gives `TFunc, S, E, T, U`), and two
   coercions on one typar to the same generic interface unify their arguments under FS0064
   where `addConstraintByKind` only dedupes structurally equal bounds.
5. **`GenericParamConstraint` rows.** No code in `src/` emits one. The table is added to the
   assembler with its row-order prediction, the same prefix-sum discipline as every other table
   here. The `System.Delegate` row reads the kind alone, so it does not wait on A6. The rows
   are read off the stage 4 scheme, so a row's typar index is in range by construction.
6. **Import.** `CodegenOpenSignature.Constraints` in `ExternalSymbols` carries every kind, but a
   foreign generic's constraint is not read off its metadata, so enforcement against a BCL or
   third-party generic is absent. Needs the enum width above and A6 stage 2 for the
   `enum<'u>` and `delegate<_,_>` bounds.
7. **`unmanaged` and the nullability attributes**, in that order, each with its own scope.

**Verify.** Assert `GenericParam` flags and `GenericParamConstraint` rows through the
`MetadataStructure` helpers, per this project's `CLAUDE.md` preference for metadata over
reflection. The `typar-*-violated.fs` programs already pin front-end rejection and must keep
their exact diagnostics. The goldens re-render into `where T0 : struct`, which is the readable
check that stages 3 and 5 agree.

**Parity note.** `fsc` accepts `under L.A 3` for `'a : enum<'u>` on an `int64` enum, where this
compiler reports a mismatch on `3`. Both solve `'u` to `int64`; `fsc` then widens the `int32`
literal implicitly, a recent F# addition that this compiler does not implement. The divergence
belongs to implicit widening, not to A3.

---

# A6. A delegate type is unmodelled

No golden shows this, because no program in the corpus can declare or use a delegate:
`type D = delegate of int -> int` registers under `UnmodelledReason.Delegate`,
`SignatureResolution` publishes it as `ExternalTypeShape.Unmodelled`, and every use is a
diagnostic. A3's `delegate<_,_>` bound therefore has no type it can hold at, and a foreign
generic bounded by `System.Delegate` cannot be instantiated from this compiler.

**Decided.** A sealed `MulticastDelegate` subclass on the CLR, a function value on JS.

**What the model carries.** A delegate declaration is its `Invoke` signature: one uncurried
argument group and a return type, over the declaration's typars. Construction is
`D(fun x -> …)` or `D(f)` and takes any function of the `Invoke` shape; invocation is
`d.Invoke(args)`. The front end types both against the signature alone, so the runtime
`.ctor(object, native int)` and the `BeginInvoke` / `EndInvoke` pair are emission facts, not
front-end ones.

**Fix, staged.**

1. Front end. `TypeRegistration` registers a `DelegateTypeInfo` with the `Invoke` signature,
   and `UnmodelledReason.Delegate` goes. A delegate is a `TyClass` whose info is the delegate's,
   so subsumption to `System.Delegate` and `MulticastDelegate` falls out of the class `inherit`
   chain and every class-shaped verdict reads it without a new arm. `Infer` types construction
   from a lambda or a function value and invocation through `.Invoke`.
   `UnificationConstraintCheck` then answers `delegate<args, ret>` by unifying the `Invoke`
   signature with `args -> ret`, closing A3's review item. No format change.
2. Publication. `ExternalTypeShape.Delegate` with a frozen `Invoke` signature, through
   `SignatureResolution`, `FrozenSignature` and the pool codecs, and the TS extractor maps a
   function type alias to it. Format version 7, landing alone.
3. CLR emission. A sealed class extending `MulticastDelegate`, with `runtime managed` `.ctor`,
   `Invoke`, `BeginInvoke` and `EndInvoke` rows, calibrated against a decompiled `fsc` output.
   Construction emits `ldftn` + `newobj`; invocation is a `callvirt` to `Invoke`. A closure
   passed at construction is the existing closure class's `Invoke`.
4. JS emission. Construction is the function value itself and `.Invoke` is a call, so a
   delegate erases; the golden pins that no wrapper survives.

**Verify.** `MetadataStructureTests` assert the four method rows and their `runtime managed`
implementation flags. The corpus gains `delegates/declare-invoke.fs` accepted on both targets,
`delegates/typar-delegate.fs` accepted on both once stage 1 lands, and a `-violated` pair where
a class and a function of the wrong arity are refused under `delegate<_,_>`.

---

# C2. Extending the decompiled goldens past the conformance corpus

The conformance render cost nothing measurable and produced Part A on the first read. The next
candidates, in order of value per unit of work:

- `StructTests` and `StructSeqTests`, whose programs already live in `data/`, so a corpus entry
  and a type name are the only additions.
- `ClassTests`, where roughly 60 tests assert metadata shape alongside a runtime `Invoke`, and
  where near-identical programs are compiled two and three times over under different assembly
  names to assert different facets of the same emission.

A golden replaces a shape assertion, never a behavioural one. It cannot see IL prefixes, opcode
choice, local signatures, table row order, duplicate mints, or assembly references, so the
digest gate and the `expectNoFSharpCore` checks stay.

---

# Sequencing

A format change lands alone, a metadata change lands after every codec change it could race,
and an import stage lands after the shape it imports. The codec is at format version 5.

1. ~~The enum underlying type on `ExternalTypeShape.Enum`.~~ Landed without a format bump.
2. ~~A3 stage 3, the `GenericParam` flag bits.~~ Landed.
3. A3 stage 4, the closed function scheme. It removes two frozen side tables, so it is a
   format change and lands alone: format version 6.
4. A3 stage 5, the `GenericParamConstraint` rows.
5. A6 stage 1, the delegate front end, and with it A3's `delegate<_,_>` verdict.
6. A6 stage 2, the published delegate shape. Format version 7, alone.
7. A3 stage 6, import. It needs the width from step 1 and the shape from step 6.
8. A6 stages 3 and 4, delegate emission on each target.
9. A3 stage 7, `unmanaged` and the nullability attributes.
10. C2, at any point, independent of the above.
