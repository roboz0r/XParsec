namespace Vesper

/// <summary>The type of mutable references. The captured-mutable promotion pass
/// (Passes/RefCellPromotion) reads and writes the `contents` field directly; the
/// `Value` property and `!` / `:=` / `incr` / `decr` operators of FSharp.Core's
/// `FSharpRef` are deferred.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
// A mutable cell is reference-keyed and not orderable — diverges from
// FSharp.Core's `[<StructuralEquality; StructuralComparison>]`.
[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> =
    {  /// The current value of the reference cell
        mutable contents: 'T }

/// <summary>The type of mutable references. Same backing record as `Ref<'T>`.</summary>
/// <category>Basic Types</category>
and 'T ref = Ref<'T>

