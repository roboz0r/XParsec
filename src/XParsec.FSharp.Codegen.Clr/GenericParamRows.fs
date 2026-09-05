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

    /// The rows of an owner's typars in `TyparIndex` order. The metadata name drops a
    /// leading quote (`'T` → `T`).
    let ofTypars (names: string seq) (constraints: EqSet<FrozenConstraint>) : GenericParamRow list =
        let names = Array.ofSeq names
        let attrs = Array.create names.Length GenericParameterAttributes.None

        for c in constraints do
            attrs.[c.TyparIndex] <- attrs.[c.TyparIndex] ||| CliConstraintEncoding.flags c.Kind

        [
            for i in 0 .. names.Length - 1 ->
                {
                    Name = names.[i].TrimStart('\'')
                    Attrs = attrs.[i]
                }
        ]

    /// The positional typar names `T0 .. T(count-1)` of a synthesised owner, such as a
    /// closure class or a module function.
    let positionalNames (count: int) : string seq = Seq.init count (sprintf "T%d")

    /// The rows of a module function's scheme, under positional names.
    let ofScheme (scheme: GenericFnScheme) : GenericParamRow list =
        ofTypars (positionalNames scheme.TyparArity) scheme.Constraints

/// A `GenericParam` row bound to its owner, a `TypeDefinition` or `MethodDefinition`.
type internal GenericParamEntry =
    {
        Owner: EntityHandle
        Index: int
        Row: GenericParamRow
    }
