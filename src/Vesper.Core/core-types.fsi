namespace Vesper

/// <summary>The type of mutable references.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
// Deliberately diverges from FSharp.Core's `[<StructuralEquality; StructuralComparison>]`.
[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> =
    {  /// The current value of the reference cell
        mutable contents: 'T }

/// <summary>The type of mutable references. Same backing record as `Ref<'T>`.</summary>
/// <category>Basic Types</category>
and 'T ref = Ref<'T>
