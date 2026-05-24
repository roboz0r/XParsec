namespace Vesper

open System

/// <summary>The type of mutable references. Use the functions [!] and [:=] to get and
/// set values of this type.</summary>
///
/// <category>Basic Types</category>
/// <exclude />
[<StructuralEquality; StructuralComparison>]
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
// and `Collections.List<'T>` / `'T list` + the `List` module (plus the
// `ResizeArray` / `seq` abbrevs) to `src/Vesper.List/` (package-split-plan PS1).
// `Vesper.List` depends on `Vesper.Option` so its `List.GetSlice` can name
// `int option`. `Ref` / `ValueOption` / `voption` / `Result` stay here for now.

/// <summary>The type of optional values, represented as structs.</summary>
///
/// <remarks>Use the constructors <c>ValueSome</c> and <c>ValueNone</c> to create values of this type.
/// Use the values in the <c>ValueOption</c> module to manipulate values of this type,
/// or pattern match against the values directly.</remarks>
///
/// <category>Options</category>
/// <exclude />
[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpValueOption`1")>]
[<Struct>]
type ValueOption<'T> =
    /// <summary>The representation of "No value"</summary>
    | ValueNone: 'T voption

    /// <summary>The representation of "Value of type 'T"</summary>
    ///
    /// <param name="Item">The input value.</param>
    ///
    /// <returns>An option representing the value.</returns>
    | ValueSome: Item:'T -> 'T voption

    /// <summary>Get the value of a 'ValueSome' option. An InvalidOperationException is raised if the option is 'ValueNone'.</summary>
    member Value: 'T

    /// <summary>Create a value option value that is a 'ValueNone' value.</summary>
    /// <exclude />
    static member None: 'T voption

    /// <summary>Create a value option value that is a 'Some' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <returns>A value option representing the value.</returns>
    /// <exclude />
    static member Some: value: 'T -> 'T voption
    
    /// <summary>Return 'true' if the value option is a 'ValueSome' value.</summary>
    member IsSome: bool

    /// <summary>Return 'true' if the value option is a 'ValueNone' value.</summary>
    member IsNone: bool
    
    /// <summary>Implicitly converts a value into an optional that is a 'ValueSome' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <remarks>The F# compiler ignored this method when determining possible type-directed conversions. Instead, use <c>Some</c> or <c>None</c> explicitly.</remarks>
    ///
    /// <returns>A voption representing the value.</returns>
    /// <exclude />
    static member op_Implicit: value: 'T -> 'T voption

/// <summary>The type of optional values, represented as structs.</summary>
///
/// <remarks>Use the constructors <c>ValueSome</c> and <c>ValueNone</c> to create values of this type.
/// Use the values in the <c>ValueOption</c> module to manipulate values of this type,
/// or pattern match against the values directly.</remarks>
///
/// <category>Options</category>
and 'T voption = ValueOption<'T>

/// <summary>Helper type for error handling without exceptions.</summary>
///
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpResult`2")>]
[<Struct>]
type Result<'T,'TError> = 

    /// Represents an OK or a Successful result. The code succeeded with a value of 'T.
    | Ok of ResultValue:'T 

    /// Represents an Error or a Failure. The code failed with a value of 'TError representing what went wrong.
    | Error of ErrorValue:'TError
