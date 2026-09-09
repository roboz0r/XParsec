namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open Vesper
open XParsec.FSharp.SemanticAnalysis

/// The CLI encoding of one typar constraint, as `fsc` writes it.
[<RequireQualifiedAccess>]
type internal CliConstraintEncoding =
    /// Flag bits on the owner's `GenericParam` row.
    | Flags of GenericParameterAttributes
    /// A `GenericParamConstraint` row against the coercion target.
    | CoercionRow of target: FrozenType
    /// The `IsUnmanagedAttribute` marker on the `GenericParam` row.
    | UnmanagedAttribute
    /// Enforced by the F# compiler alone.
    | NoEncoding

module internal CliConstraintEncoding =

    let ofKind (kind: TyparConstraintKindG<FrozenType>) : CliConstraintEncoding =
        match kind with
        | TyparConstraintKindG.Struct ->
            CliConstraintEncoding.Flags GenericParameterAttributes.NotNullableValueTypeConstraint
        | TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness ->
            CliConstraintEncoding.Flags GenericParameterAttributes.ReferenceTypeConstraint
        | TyparConstraintKindG.DefaultConstructor ->
            CliConstraintEncoding.Flags GenericParameterAttributes.DefaultConstructorConstraint
        // `fsc` omits the `System.Object` row because `'a :> obj` holds for every type.
        | TyparConstraintKindG.Coercion FTObj -> CliConstraintEncoding.NoEncoding
        | TyparConstraintKindG.Coercion target -> CliConstraintEncoding.CoercionRow target
        | TyparConstraintKindG.Unmanaged -> CliConstraintEncoding.UnmanagedAttribute
        | TyparConstraintKindG.Enum _
        | TyparConstraintKindG.Delegate _
        | TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison
        | TyparConstraintKindG.NotNull -> CliConstraintEncoding.NoEncoding

/// One `GenericParam` row: the metadata name, the flag bits of the typar's constraints and
/// the targets of its `GenericParamConstraint` rows.
type internal GenericParamRow =
    {
        Name: string
        Attrs: GenericParameterAttributes
        /// One `GenericParamConstraint` row per target, in constraint order. A target
        /// referencing a sibling typar carries it as an `FTTypar` of the declaring scope.
        Constraints: Block<FrozenType>
    }

module internal GenericParamRow =

    /// The metadata name of a typar: its display name without the leading quote (`'T` →
    /// `T`, `'T0` → `T0`).
    let private metadataName (name: TyparName) : string = name.Display.TrimStart('\'', '^')

    let private ofTypar (t: TypeTyparG<FrozenType>) : GenericParamRow =
        let mutable attrs = GenericParameterAttributes.None
        let constraints = ResizeArray<FrozenType>()

        for kind in t.Constraints.Kinds do
            match CliConstraintEncoding.ofKind kind with
            | CliConstraintEncoding.Flags bits -> attrs <- attrs ||| bits
            | CliConstraintEncoding.CoercionRow target -> constraints.Add target
            | CliConstraintEncoding.UnmanagedAttribute
            | CliConstraintEncoding.NoEncoding -> ()

        {
            Name = metadataName t.Name
            Attrs = attrs
            Constraints = Block.ofSeq constraints
        }

    /// One row per type-kinded typar, in `Types` order. The CLR erases a measure-kinded
    /// typar.
    let ofTypars (typars: TyparList) : GenericParamRow list = [ for t in typars.Types -> ofTypar t ]

    /// The rows of a module function's scheme.
    let ofScheme (scheme: FunctionScheme) : GenericParamRow list = ofTypars scheme.Typars

/// A `GenericParam` row bound to its owner, a `TypeDefinition` or `MethodDefinition`.
type internal GenericParamEntry =
    {
        Owner: EntityHandle
        Index: int
        Row: GenericParamRow
    }
