# Type parameter scopes

*Decision record, agreed 2026-09-05, revised 2026-09-06. Durable: it describes the domain
model the code is to match, and it is revised rather than deleted when the code lands. F#
verdicts were probed with `dotnet fsi` on the same days.*

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
`MemberOrdinal` survives only as the registration index inside NameResolution.

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
`LocalFunction` leaves, and the host must generalise each one again rather than mint one
inference cell per local typar for the whole body, or the second use fails to unify. And a
backend with typed signatures cannot closure-convert a generic local into a class with a
fixed `Invoke`, because the local's typar has no slot there. The CLR lifts such a local to
a generic static method whose method typars are the local's own, which is what `fsc`
emits. The corpus programs `locals/local-poly.fs` and `inline/inline-local-poly.fs` pin
both, with the CLR pending on them.

### Leaves

One leaf shape serves every scope. `FTLocalTypar of SchemeId * index` retires with it; the
`SchemeId` it carried was re-derived at freeze time by sorting the bindings table, and a
local scope key is instead minted when the binding is generalised.

```fsharp
| FTTypar of scope: TyparScope * index: int     // indexes the scope's Types
```

A measure variable is an atom of `MeasureTerm`, so the atom widens from `TypeKey` to:

```fsharp
type MeasureAtom =
    | Named of TypeKey
    | Typar of scope: TyparScope * index: int   // indexes the scope's Measures
```

## Typars and constraints

Type-kinded and measure-kinded parameters are different concepts. Type typars are solved by
union-find inference and carry constraints; measure typars are solved as an Abelian group and
carry none. They share only their lexical position, which matters for signature conformance
and display.

```fsharp
type TypeTypar    = { Name: string; Constraints: ConstraintSet }
type MeasureTypar = { Name: string }

[<RequireQualifiedAccess>]
type TyparSlot =
    | Type of int       // index into Types
    | Measure of int    // index into Measures

type TyparList =
    {
        Types: EqArray<TypeTypar>
        Measures: EqArray<MeasureTypar>
        /// Source order, one slot per typar.
        Order: EqArray<TyparSlot>
    }
```

`ConstraintSet` holds the existing `TyparConstraintKindG` cases plus `Default`, per typar.
`TyparConstraintG.TyparIndex` retires: a constraint's typar is the record holding it. A
constraint's embedded type may reference sibling typars, as `Coercion` and `Default` already
do.

Measure typars keep a scope even where a backend erases them. The CLR erases them entirely:
`typeof<D<1, int>>.GetGenericArguments()` on `type D<[<Measure>] 'u, 'T>` reports one
argument, so the CLR index of a type typar is its index into `Types`, never into `Order`. The
TAST keeps the measure scope regardless, because a future backend may want it.

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
        Traits: EqArray<MemberTrait>
    }
```

`MemberTrait` is the shape `ExternalConstraint.MemberTrait` has today, with the
`typarIndices` resolved against the scheme's own `Types`. `GenericFnScheme` at
`SideTypes.fs` becomes `FunctionScheme`; `MemberKey.MethodTyparArity` reads off the scheme's
`Types.Length` rather than being a parallel integer.

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
- `MemberOrdinal` outside NameResolution: `TTypeMemberG.Ordinal` becomes `Key: MemberKey`;
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
- `EqArray<TyparKind>` and `TyparKinds.typeOnly`: replaced by `TyparList`.
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
its own. A backend lowers a measured nominal by expanding its abbreviation body
(`type float<[<Measure>] 'M> = float`) and dropping the measure arguments.

This case is introduced by `measure-resolution-plan.md` step 6 and is inherited unchanged.

## Sequencing

The implementation steps are in `typar-scope-plan.md`, which interleaves the remaining
typar-constraint emission and import stages with the model change. The ordering constraints
are:

- `measure-resolution-plan.md` lands and merges first. Typar-scope reads its outputs:
  `TyparKind`, `DeclaredTypar`, `MeasureTerm` keyed by `TypeKey`, `FTMeasure`.
- Typar-scope rewrites `SemanticInfo.fs`, `SideTypes.fs`, the frozen codecs and every
  backend's typar encoding, so it lands on a branch from `main` with nothing else open in
  SemanticAnalysis.
- Typar-scope is a format bump and lands alone. `delegates-plan.md` stage 2 is another; the
  two never interleave, and the `delegate<_,_>` constraint import waits on it.
