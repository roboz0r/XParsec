namespace XParsec.FSharp.Codegen.Clr

open System.Reflection
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// The CLI encoding of one typar constraint.
[<RequireQualifiedAccess>]
type internal CliConstraintEncoding<'ty> =
    /// Flag bits on the owner's `GenericParam` row.
    | Flags of GenericParameterAttributes
    /// A `GenericParamConstraint` row against the coercion target.
    | CoercionRow of target: 'ty
    /// A `GenericParamConstraint` row against `System.Enum`.
    | EnumRow
    /// A `GenericParamConstraint` row against `System.Delegate`.
    | DelegateRow
    /// The `IsUnmanagedAttribute` marker on the `GenericParam` row.
    | UnmanagedAttribute
    /// Enforced by the F# compiler alone.
    | NoEncoding

module internal CliConstraintEncoding =

    let ofKind (kind: TyparConstraintKindG<'ty>) : CliConstraintEncoding<'ty> =
        match kind with
        | TyparConstraintKindG.Struct ->
            CliConstraintEncoding.Flags(
                GenericParameterAttributes.NotNullableValueTypeConstraint
                ||| GenericParameterAttributes.DefaultConstructorConstraint
            )
        | TyparConstraintKindG.ReferenceType ->
            CliConstraintEncoding.Flags GenericParameterAttributes.ReferenceTypeConstraint
        | TyparConstraintKindG.DefaultConstructor ->
            CliConstraintEncoding.Flags GenericParameterAttributes.DefaultConstructorConstraint
        | TyparConstraintKindG.Coercion target -> CliConstraintEncoding.CoercionRow target
        | TyparConstraintKindG.Enum _ -> CliConstraintEncoding.EnumRow
        | TyparConstraintKindG.Delegate _ -> CliConstraintEncoding.DelegateRow
        | TyparConstraintKindG.Unmanaged -> CliConstraintEncoding.UnmanagedAttribute
        | TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull -> CliConstraintEncoding.NoEncoding

    /// The flag bits of the constraint; `None` for every other encoding.
    let flags (kind: TyparConstraintKindG<'ty>) : GenericParameterAttributes =
        match ofKind kind with
        | CliConstraintEncoding.Flags attrs -> attrs
        | CliConstraintEncoding.CoercionRow _
        | CliConstraintEncoding.EnumRow
        | CliConstraintEncoding.DelegateRow
        | CliConstraintEncoding.UnmanagedAttribute
        | CliConstraintEncoding.NoEncoding -> GenericParameterAttributes.None

/// One `GenericParam` row: the metadata name and the flag bits of the typar's constraints.
type internal GenericParamRow =
    {
        Name: string
        Attrs: GenericParameterAttributes
    }

module internal GenericParamRow =

    /// The metadata name of a typar: its display name without the leading quote (`'T` →
    /// `T`, `'T0` → `T0`).
    let private metadataName (name: TyparName) : string = name.Display.TrimStart('\'', '^')

    /// One row per type-kinded typar, in `Types` order. The CLR erases a measure-kinded
    /// typar.
    let ofTypars (typars: TyparList) : GenericParamRow list =
        [
            for t in typars.Types ->
                {
                    Name = metadataName t.Name
                    Attrs =
                        (GenericParameterAttributes.None, t.Constraints.Kinds)
                        ||> EqSet.fold (fun attrs kind -> attrs ||| CliConstraintEncoding.flags kind)
                }
        ]

    /// The rows of a module function's scheme.
    let ofScheme (scheme: FunctionScheme) : GenericParamRow list = ofTypars scheme.Typars

/// A `GenericParam` row bound to its owner, a `TypeDefinition` or `MethodDefinition`.
type internal GenericParamEntry =
    {
        Owner: EntityHandle
        Index: int
        Row: GenericParamRow
    }
