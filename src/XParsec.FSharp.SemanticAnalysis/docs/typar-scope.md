# Type parameter scopes

*Decision record, agreed 2026-09-05. Durable: it describes the domain model the code is to
match, and it is revised rather than deleted when the code lands. F# verdicts were probed
with `dotnet fsi` on the same day.*

This document retires the terms **declaring-axis** and **method-axis**, and the
`TyparAxis = Declaring | Method` type at `SemanticScalars.fs`, in favour of **typar scope**.

## The model

A type parameter belongs to exactly one scope. Scopes nest lexically, in the same way
`SymbolKey` nests containment.

```fsharp
type TyparScope =
    /// A type declaration's own typars.
    | Type of TypeKey
    /// A member's own typars. Always nested in a `Type` scope.
    | Member of owner: TypeKey * member: MemberOrdinal
    /// A module-level `let`, including a top-level `let` in the implicit program module.
    | ModuleFunction of BindingKey
    /// An optional extension member's own typars (`type List<'T> with member xs.F<'U>`
    /// in another module or assembly). `extension` is the `type … with` block; `member`
    /// is the member's position within it. The extended type's typars are `Type extended`
    /// leaves.
    | Extension of extended: TypeKey * extension: ExtensionKey * member: MemberOrdinal
    /// A generalised body-local `let`. `parent` is `Member`, `ModuleFunction`,
    /// `Extension` or `LocalFunction`, never `Type`.
    | LocalFunction of parent: TyparScope * LocalBindingId
```

The nesting is fixed by the language, so the chains are short:

- `Type > Member > LocalFunction*`
- `Type > Extension > LocalFunction*`
- `ModuleFunction > LocalFunction*`

F# forbids a type definition nested in a type, and a module carries no typars, so `Type`
never nests in anything with typars. A leaf referring to a `Type` scope is therefore only
valid under a `Member` or `Extension` chain, and the scope type makes the invalid case
unrepresentable.

### MemberOrdinal

`MemberKey.ArgSig` is written in open typars, and a generic member's own typars appear in
its own signature, so a scope key embedding `MemberKey` would embed the leaf that references
it. `MemberOrdinal` is the member's stable declaration index within its type. Declaration
order is stable for source, CLR `MethodDef` rows and TypeScript manifest entries. The primary
constructor is a member and takes an ordinal like any other.

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

- a local binding of the primary constructor, in a `LocalFunction` scope under the
  constructor's `Member` scope, when nothing after the constructor references it;
- a `private member` in its own right, with its own `Member` scope, when another member
  references it.

## Extension members and interface implementations

An intrinsic extension, declared in the extended type's own unit, is a true member and takes
a `MemberOrdinal` in that type's declaration order. An optional extension has no slot in that
order, so it is its own TAST node, a type extension. Each member of the `type … with` block
is an `Extension` scope keyed by the block and the member's ordinal within it, mirroring
`Member`. Its `'T` is the extended type's `Type` leaf. Lowering it to a
static method on a non-generic class, with the extended type's typars and the member's own
both lifted onto the method, is the CLR encoder's concern, as the closure flattening at
`ClrEncoder.fs` is; a future backend may lower it differently.

An explicit interface implementation (`interface IProcess with member _.Run<'T>()`) is a
`Member` of the implementing type, with an ordinal in that type's declaration order. The
interface's abstract slot is a separate `Member` of the interface; matching the two is
conformance, not scoping.

## What changes, by site

- `TyparAxis` and both `FTTypar` axes: replaced by `TyparScope`. The Extractor and Manifest
  schema carry the old words in diagnostic codes (`method-axis-typar-erased`) and comments.
- `FTLocalTypar` and `SchemeId`: folded into `FTTypar` with a `LocalFunction` scope.
- `TyparConstraintG.TyparIndex` and the flat `EqSet<TyparConstraintG>` on decls
  (`TastDecl.fs` `MethodTyparConstraints`, `TyparConstraints`): replaced by per-typar
  `ConstraintSet`.
- `EqArray<TyparKind>` and `TyparKinds.typeOnly`: replaced by `TyparList`.
- `MeasureTerm` atoms: `TypeKey` widens to `MeasureAtom`.
- `ExternalConstraint`: `Encodable` and `Default` move into the typar's `ConstraintSet`,
  `MemberTrait` into `FunctionScheme.Traits`.
- `ConformanceTypars.normAxisTo` and `toDeclaringAxis`: re-expressed as a scope substitution.
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
