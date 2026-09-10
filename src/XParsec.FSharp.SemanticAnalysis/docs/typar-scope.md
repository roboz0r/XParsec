# Type parameter scopes

*Decision record, agreed 2026-09-05, revised 2026-09-06 and 2026-09-09. Durable: it
describes the domain model the code is to match, and it is revised rather than deleted when
the code lands. F# verdicts were probed with `dotnet fsi` on the same days.*

This document retires the terms **declaring-axis** and **method-axis**, and the
`TyparAxis = Declaring | Method` type at `SemanticScalars.fs`, in favour of **typar scope**.

## The model

A type parameter belongs to exactly one scope. A scope is a *kind* of declaration and the
type it hangs off, which is what every consumer of a leaf reads: the CLR encoder chooses
`!i` or `!!j` by it, the wildcard rules and the instantiation seams branch on it. A scope
is not an identity. The identity of the declaration a typar belongs to lives on that
declaration's row, keyed as every other row is.

```fsharp
type TyparScope =
    /// A type declaration's own typars.
    | Type of TypeKey
    /// A member's own typars, nested in `owner`'s `Type` scope.
    | Member of owner: TypeKey
    /// A module-level `let`, including a top-level `let` in the implicit program module.
    | ModuleFunction of BindingKey
    /// An optional extension member's own typars (`type List<'T> with member xs.F<'U>`
    /// in another module or assembly). The extended type's typars are `Type extended`
    /// leaves.
    | Extension of extended: TypeKey * ExtensionKey
    /// A generalised body-local `let`, by the identity minted when it generalised.
    | LocalFunction of LocalBindingId
```

`Extension` is not yet in code: it waits for `ExtensionKey` and the type extension design.

Two of a leaf's consumers compare it for equality: the frozen tree's structural equality
and the codec. Neither needs `Member` to distinguish two members of one type. A member's
own typars never coexist un-instantiated with another member's in one tree: a served
inline body is instantiated before the host is frozen, and the CLR reads a member's own
typar as `!!j` whatever the member. A `Member` scope that carried the member's declaration
index was tried (`MemberOrdinal`, 2026-09-06) and retired: a signature file numbers its
members independently of its implementation, so every comparison had to erase the index
again, and the homed signature had to be re-scoped onto the implementation's numbering.
`MemberOrdinal` is deleted outright.

### Lexical ownership

The nesting is fixed by the language, so the chains are short:

- `Type > Member > LocalFunction*`
- `Type > Extension > LocalFunction*`
- `ModuleFunction > LocalFunction*`

The chain is recorded as ownership on the row, not as a parent inside the leaf. A local
function is private to the member or module function whose body declares it, and it leaves
that body only as part of it, so its owner is fixed at generalisation and never changes:

```fsharp
[<RequireQualifiedAccess>]
type LocalOwner =
    | Member of MemberKey
    | ModuleFunction of BindingKey
    | Local of LocalBindingId
    /// A body with no key of its own: a module-level `do`, a module-level tuple binding's
    /// right-hand side, an auto-property initialiser, or a type's `let` / `do` preamble or
    /// secondary constructor.
    | Initialiser
    /// A local of another file's splice template, copied in at a call site. The owning
    /// declaration belongs to the declaring file, so this file's chain stops here.
    | Spliced of template: SymbolKey

/// Every generalised local of the file, by owner. Recorded when the local generalises,
/// carried by `FrozenPools`.
LocalOwners : LocalBindingId -> LocalOwner
```

`MemberKey` can key the owner because the key is on the row, not in the leaf: `ArgSig`
embeds the member's own leaves, and a leaf embedding the key would be a cycle, which is
what `MemberOrdinal` was minted to avoid. The table answers two questions. Walked upward,
which typars are visible at a local: its owner's, its owner's owner's, up to the `Type`.
Walked downward from a member, which locals it owns: the capture and escape analysis that
decides how a local is lowered reads this direction.

F# forbids a type definition nested in a type, and a module carries no typars, so `Type`
never nests in anything with typars, and a `Type` leaf is valid only under a `Member` or
`Extension` chain. The leaf shape does not express this; the freeze walk does, because a
module function's body is frozen under a scope stack with no `Type` entry. A test pins it
on a module function with a local.

### Local functions

A body-local `let` is generalised at its own `let`. F# forbids explicit typars on a local
(`let g<'a> (y: 'a) = y` is FS0665), so a local's polymorphism is always implicit, and an
annotated `'a` on a local is a generalisable placeholder rather than a declaration. All of
these are accepted by `fsc` and print a two-typed pair:

```fsharp
let inline f1 (x: int) = let g y = y in (g x, g "a")
let inline f2 (x: int) = let g (y: 'a) : 'a = y in (g x, g "a")
let inline f3 (x: 'b)  = let g y = (y, x) in (g 1, g "a")     // 'b captured, not generalised
let f4 (x: int)        = let g y = y in (g x, g "a")
```

Two consequences for this compiler. A served inline body carries its locals frozen under
`LocalFunction` leaves, and the host must generalise each one again: a cell minted for such
a leaf and left unquantified reaches `Freeze` unbound and degrades to `FTUnknown`, which a
typed backend refuses. Which typars each local quantifies is the leaf set itself, so the
published body carries the declaring file's `LocalScheme` rows to say which `let` owns each
scope. And a backend with typed signatures cannot closure-convert a generic local into a
class with a fixed `Invoke`, because the local's typar has no slot there. The CLR lifts such
a local to a generic static method whose method typars are the local's own, which is what
`fsc` emits. The corpus programs `locals/local-poly.fs` and `inline/inline-local-poly.fs`
pin both.

### Leaves

One leaf shape serves every scope. `FTLocalTypar of SchemeId * index` retires with it; the
`SchemeId` it carried was re-derived at freeze time by sorting the bindings table, and a
local scope key is instead minted when the binding is generalised.

```fsharp
| FTTypar of scope: TyparScope * index: int<typeSlot>     // indexes the scope's Types
```

A measure variable is an atom of `MeasureTerm`, so the atom widens from `TypeKey` to:

```fsharp
type MeasureAtom =
    | Named of TypeKey
    | Typar of scope: TyparScope * index: int<measureSlot> // indexes the scope's Measures
```

A measure atom is minted at translation: a measured type's term lives in the store's root
state, outside the `SemType` tree walked by the deferred `TyVar -> TyTypar` cut. The live
scope entry is a `ScopedTypar` — `Type of TyVarId` for a
type-kinded parameter, `Measure of MeasureAtom` for a measure-kinded one — and
`ScopedTypar.declare` is its one constructor, assigning the measure slots as a declaration
pushes its parameters. `Translate.translateMeasure` reads the atom out by source name;
a `Measure` entry in type position is FS0703 and a type-kinded name in measure position is
FS0702.

## Typars and constraints

Type-kinded and measure-kinded parameters are different concepts. Type typars are solved by
union-find inference and carry constraints; measure typars are solved as an Abelian group and
carry none. They share only their lexical position, which matters for signature conformance
and display.

```fsharp
type TypeTypar    = { Name: TyparName; Constraints: ConstraintSet }
type MeasureTypar = { Name: TyparName }

[<RequireQualifiedAccess>]
type TyparSlot =
    | Type of typeSlot: int<typeSlot>
    | Measure of measureSlot: int<measureSlot>

type TyparList =
    {
        Types: BlockM<TypeTypar, typeSlot>
        Measures: BlockM<MeasureTypar, measureSlot>
        /// Source order, one slot per parameter.
        Order: BlockM<TyparSlot, sigSlot>
    }
```

`ConstraintSet` holds the existing `TyparConstraintKindG` cases as `Kinds`, plus `Defaults`,
per typar.
`TyparConstraintG.TyparIndex` retires: a constraint's typar is the record holding it. A
constraint's embedded type may reference sibling typars, as `Coercion` and `Default` already
do.

Measure typars keep a scope even where a backend erases them. The CLR erases them entirely:
`typeof<D<1, int>>.GetGenericArguments()` on `type D<[<Measure>] 'u, 'T>` reports one
argument, so the CLR index of a type typar is its index into `Types`, never into `Order`. The
TAST keeps the measure scope regardless, because a future backend may want it.

### Slot numbering

Three numberings run over one declaration's parameters. Each is a measure-tagged `int`
declared in `TyparSlots.fs`, and a `BlockM` accepts only its own tag:

| Tag | Indexes | Read by |
| --- | --- | --- |
| `sigSlot` | `Order`, every parameter in source order | name resolution, a written `<'a, 'u>` argument list, FS0033 |
| `typeSlot` | `Types` | the CLR `GenericParam` rows, `FTTypar` / `TyTypar` leaves, `MemberTrait.TyparIndices`, `FunctionScheme.TyparArity` |
| `measureSlot` | `Measures` | measure resolution and `MeasureAtom.Typar`; erased by both backends |

`TyparList.typeSlotOf` and `measureSlotOf` project a signature slot onto its kind's
numbering and `sigSlotOf` is the reverse; these three are the only bridges. An `int` erasure
belongs at a metadata, codec, reflection or manifest boundary, and a cast anywhere else
names a mistyped parameter.

A `TypeKey` spells one of the two counts and its case says which. `KeyArity.Compiled` is a
nominal type's type-slot count, the `` `N `` of its metadata name, so a key minted from a
metadata row equals one minted from source, which is what lets a signature file overlay a
referenced assembly's type. `KeyArity.Written` is an abbreviation's signature-slot count,
because an abbreviation has no compiled name and F# keys it at every parameter.
`Compiled 1` and `Written 1` are distinct keys, so `float` and `float<'u>` stay apart.

A declaration's written arity is `Order.Length`, carried on the claim as
`TypeIdentity.TyparArity` and outside the identity, so a backend that needs the written
count reads the declaration. Two nominals differing only in measure parameters share one
key, and the second is refused at declaration although F# admits both.

## Schemes and traits

A type has typars and no traits. `type C< ^T when ^T : (static member Zero : ^T)>` is
FS0670, so a trait is only ever on a function-like scheme.

```fsharp
type TypeScheme = { Typars: TyparList }

/// A member's, module function's or local function's scheme.
type FunctionScheme =
    {
        Typars: TyparList
        /// Statically resolved member constraints. A trait references one or more of
        /// `Typars.Types` on its left-hand side.
        Traits: Block<MemberTrait>
    }
```

`MemberTrait` is the shape `ExternalConstraint.MemberTrait` has today, with
`TyparIndices: Block<int<typeSlot>>` resolved against the scheme's own `Types`; a measure
typar in a support set is FS0703. `GenericFnScheme` at `SideTypes.fs` becomes
`FunctionScheme`, whose `TyparArity` is `int<typeSlot>`, and `MemberKey.MethodTyparArity`
is the same count rather than a parallel integer.

## Class preamble lets

A `let` in a class body before the members is one of two things:

- a local binding of the primary constructor, a `LocalFunction` owned by the constructor's
  `MemberKey`, when nothing after the constructor references it;
- a `private member` in its own right, with its own `Member` scope, when another member
  references it.

## Extension members and interface implementations

An intrinsic extension, declared in the extended type's own unit, is a true member of that
type. An optional extension is its own TAST node, a type extension. Each member of the
`type … with` block has an `Extension` scope keyed by the block, mirroring `Member`, and
its own `MemberKey` on its row. Its `'T` is the extended type's `Type` leaf. Lowering it to a
static method on a non-generic class, with the extended type's typars and the member's own
both lifted onto the method, is the CLR encoder's concern, as the closure flattening at
`ClrEncoder.fs` is; a future backend may lower it differently.

An explicit interface implementation (`interface IProcess with member _.Run<'T>()`) is a
`Member` of the implementing type. The interface's abstract slot is a `Member` of the
interface; matching the two is conformance, not scoping.

## What changes, by site

- `TyparAxis` and both `FTTypar` axes: replaced by `TyparScope`. The Extractor's and Manifest
  schema's diagnostic code is `method-scope-typar-erased`.
- `FTLocalTypar` and `SchemeId`: folded into `FTTypar` with a `LocalFunction` scope.
- `MemberOrdinal`: deleted. `TTypeMemberG.Ordinal` becomes `Key: MemberKey`;
  the codec field, `ClassMemberDeclaring.OrdinalOf`, `MetadataSymbols.methodOrdinal`, the
  manifest translator's `InMember` and the `resolveMember` ordinal parameter are deleted,
  with `ConformanceTypars.rescopeToImplementation`, `FrozenType.rescopeMemberTypars` and
  `UnificationInferOverload.comparisonScope`.
- `LocalOwners` on `FrozenPools`, written at generalisation.
- `InlineThaw`: a served local is generalised again in the host, not instantiated once.
- The CLR closure conversion: a generic local lifts to a generic static method.
- `TyparConstraintG.TyparIndex` and the flat `EqSet<TyparConstraintG>` on decls
  (`TastDecl.fs` `MethodTyparConstraints`, `TyparConstraints`): replaced by per-typar
  `ConstraintSet`.
- `Block<TyparKind>` and `TyparKinds.typeOnly`: replaced by `TyparList`.
- `MeasureTerm` atoms: `TypeKey` widens to `MeasureAtom`.
- `ExternalConstraint`: `Encodable` and `Default` move into the typar's `ConstraintSet`,
  `MemberTrait` into `FunctionScheme.Traits`.
- `ConformanceTypars.normAxisTo` and `toDeclaringAxis`: deleted. A `.fsi` scheme and its
  `.fs` scheme are both written under the binding's `ModuleFunction` scope, so conformance
  is equality.
- `FrozenCodec` rows for typar axis and kind: re-encoded over the new shapes.

## Measure arguments

A measure in argument position is a `FrozenType` case, `FTMeasure of MeasureTerm`, usable
only as an argument of a nominal (`FTConst(Vesper.float1, [| FTMeasure term |])`,
`FTRecord(Vec, [| FTMeasure term |])`). Argument position `i` is measure-kinded exactly when
the claim's `Order.[i]` is `Measure`. A `FTMeasure` in type position is FS0704 and is refused
by the front end, so it is unrepresentable past `Freeze`. `FTMeasure` never reaches `unify`.

A measure typar is a `MeasureAtom.Typar` inside that term, so the measure leaf needs no case of
its own.

Before `Freeze` a measure argument is a store state rather than a type. `RootState` is a
union-find root's kind — `Free`, `Linked of target`, `Measure of units`,
`Measured of units * carrier` — and `PassContext.MeasureTy` mints the third of them.
`unify` matches on the state pair: two measure arguments merge through `mergeUnits`, which
requires equal terms and reports `MeasureMismatch` otherwise; a measured value meeting a type
unifies its carrier against that type; a measure argument meeting a type reports FS0704 and
leaves the two classes apart.

A written measure argument stays an argument, placed at its signature slot, and attaches to
the expansion instead for a transparent abbreviation, whose body carries no slot for it. The
two placements are `Translate.MeasureSite`, chosen by the `KeyArity` case.

Both backends erase measures. `MeasureErasure` in `Codegen.Common` expands a measured
nominal's abbreviation body (`type float<[<Measure>] 'M> = float`) and drops the measure
arguments, and the CLR `GenericParam` rows and every `GENERICINST` read `Types` alone,
through `FrozenType.typeSlotArgs`.

### Not yet inferred

A declared measure typar is a rigid atom within its declaration, equal only to itself.
Generalising an unannotated measure (`let f x = x * 1.0<m>`) and instantiating a declared one
at a call site (`scale 1.0<m>` on `let scale<[<Measure>] 'u> (x: float<'u>) = x`, which
reports `Measure mismatch: <m> vs <'m0>`) each need measure variables in the store and
Abelian-group unification over them, which is its own plan. Measure-generic abbreviations
(`type Meters<[<Measure>] 'u> = float<'u>`), a measure wildcard (`float<_>`) and an
undeclared measure typar (`float<'zz>`, which fsc generalises) are `NotYetSupported`.
`MeasureResolutionTests` and `GenericParamFlagsTests` pin each gap, quoted in the test name.

## CLI encoding of constraints

The CLR backend writes each constraint as `fsc` does, read off FSharp.Core's metadata and an
`fsi`-compiled probe of each clause in isolation. `GenericParamFlagsTests` pins the flag bits
and `GenericParamConstraintTests` the rows and the row-less kinds; `MetadataSymbols.typarConstraints`
reads the same encoding back on import.

| Source constraint | CLI encoding |
| --- | --- |
| `'a : struct` | `NotNullableValueTypeConstraint` |
| `'a : not struct` | `ReferenceTypeConstraint` |
| `'a : null` | `ReferenceTypeConstraint` |
| `'a : not null` | none; nullability attributes are the only carrier, not yet written |
| `'a : (new : unit -> 'a)` | `DefaultConstructorConstraint` |
| `'a :> Ty` | `GenericParamConstraint` to `Ty`, class or interface alike; `'a :> obj` adds no row |
| `'a : enum<'u>` | none |
| `'a : delegate<_,_>` | none |
| `'a : unmanaged` | `NotNullableValueTypeConstraint` + a `GenericParamConstraint` to `System.ValueType modreq(UnmanagedType)` + `IsUnmanagedAttribute`; only the flag is written today |
| `'a : equality` / `comparison` | none; F# has no CLI encoding for these either |
| SRTP member trait | none; an `inline` binding resolves it at the splice |

A closure class's and a lifted local's rows are positional and unconstrained, where `fsc`
copies the enclosing constraints onto the closure class; nothing consumes those rows yet.

## Status

The model has landed at format version 22: every scope but `Extension`, the slot tags,
`MeasureAtom.Typar`, per-typar `ConstraintSet`, `FunctionScheme`, generic locals lifted to
generic methods on the CLR, `GenericParamConstraint` rows, and constraint import and
enforcement on a BCL, `.fsi` and cross-file generic alike. Every identifier the model retires
is absent from `src`.

Open, each its own piece of work:

- `TyparScope.Extension` waits on `ExtensionKey` and the type extension design.
  `TypeRegistration.rejectDetachedTypeExtension` refuses a detached `type … with` as
  `NotYetSupported`, so there is no block to key yet.
- `unmanaged` writes only its value-type flag, and `not null` writes nothing: the
  `modreq(UnmanagedType)` row, `IsUnmanagedAttribute` and the nullability attributes are not
  emitted, and reflection cannot read them, so `unmanaged` imports as `struct`.
- The `'a : delegate<_,_>` constraint import waits on `delegates-plan.md` stage 2; a C#
  `where T : Delegate` imports as the coercion it is written as.
- The CLR reader models a BCL enum as a `Class` shape, so `enum<'u>` checks only a published
  or manifest enum (`ConstraintCheck.enumUnderlyingType`).
- Two `fsc` parity gaps are `ptest`s in `FrozenConstraintTests.fscParityTests`: `fsc` orders
  typars by first appearance including the constraint clauses, and two coercions on one typar
  to the same generic interface unify their arguments under FS0064.
- Implicit widening: `fsc` accepts `under L.A 3` for `'a : enum<'u>` on an `int64` enum and
  this compiler reports a mismatch on `3`. Both solve `'u` to `int64`; the divergence belongs
  to implicit widening, not to this model.
- A module tuple binding (`let (f, g) = …`) generalises per name and runs on JS;
  `bindings/module-tuple-poly` is `pending` on the CLR, which lowers a generic module value
  to a generic static method only when it is not function-typed.
