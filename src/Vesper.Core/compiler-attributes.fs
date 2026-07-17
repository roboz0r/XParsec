namespace Vesper

[<Sealed>]
type CallAtMostOnceAttribute() =
    inherit Attribute()

[<Sealed>]
type StructuralEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type StructuralComparisonAttribute() =
    inherit Attribute()

[<Sealed>]
type ReferenceEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type NoEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type CustomEqualityAttribute() =
    inherit Attribute()

[<Sealed>]
type CustomComparisonAttribute() =
    inherit Attribute()

[<Sealed>]
type NoComparisonAttribute() =
    inherit Attribute()
