namespace Vesper

open System

/// <summary>The type of mutable references. Use the functions [!] and [:=] to get and
/// set values of this type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
// State, not Data: a mutable cell is reference-keyed and not orderable
// (operators-plan.md O8). Diverges from FSharp.Core's ported
// `[<StructuralEquality; StructuralComparison>]`.
[<ReferenceEquality>]
[<NoComparison>]
[<CompiledName("FSharpRef`1")>]
type Ref<'T> =
    {  /// The current value of the reference cell
        mutable contents: 'T }

    /// <summary>The current value of the reference cell</summary>
    member Value: 'T with get,set
        
/// <summary>The type of mutable references. Use the functions [!] and [:=] to get and
/// set values of this type.</summary>
/// <category>Basic Types</category>
and 'T ref = Ref<'T>

// `Option<'T>` / `'T option` + the `Option` module moved to `src/Vesper.Option/`,
// `Result<'T,'TError>` + the `Result` module to `src/Vesper.Result/`, and
// `Collections.List<'T>` / `'T list` + the `List` module (plus the
// `ResizeArray` / `seq` abbrevs) to `src/Vesper.List/` (package-split-plan PS1).
// `Vesper.List` depends on `Vesper.Option` so its `List.GetSlice` can name
// `int option`. `Ref` stays here for now. `ValueOption` / `voption` were removed
// as redundant — `Vesper.Option`'s `Option` is already a struct
// (operators-plan.md O9).
