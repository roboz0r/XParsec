namespace Vesper

// Impl side of `compiler-attributes.fsi`. These are zero-runtime recognition
// markers: the front end reads them by SHORT NAME off the `.fsi` to gate
// structural `Equals`/`GetHashCode`/`CompareTo` synthesis and to stamp a type's
// equality/comparison verdict (see the `.fsi` header). The runtime bodies exist
// only so the contract is backed by a real `.fs` (T8 G1): each is a sealed class
// inheriting `Attribute` (`= (# "System.Attribute" #)`, prim-types-attr) with a
// parameterless ctor that chains to the base. No fields, no members.
//
// The `[<AttributeUsage(...)>]` placement metadata lives ONLY in the `.fsi`
// contract; it is not re-emitted here.

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
