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

/// <summary>The type of optional values. When used from other CLI languages the
/// empty option is the <c>null</c> value. </summary>
///
/// <remarks>Use the constructors <c>Some</c> and <c>None</c> to create values of this type.
/// Use the values in the <c>Option</c> module to manipulate values of this type,
/// or pattern match against the values directly.
///
/// <c>None</c> values will appear as the value <c>null</c> to other CLI languages.
/// Instance methods on this type will appear as static methods to other CLI languages
/// due to the use of <c>null</c> as a value representation.</remarks>
///
/// <category>Options</category>
/// <exclude />
[<DefaultAugmentation(false)>]
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpOption`1")>]
type Option<'T> =

    /// <summary>The representation of "No value"</summary>
    | None:       'T option

    /// <summary>The representation of "Value of type 'T"</summary>
    ///
    /// <param name="Value">The input value.</param>
    ///
    /// <returns>An option representing the value.</returns>
    | Some: Value:'T -> 'T option 

    /// <summary>Create an option value that is a 'None' value.</summary>
    /// <exclude />
    static member None: 'T option

    /// <summary>Create an option value that is a 'Some' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <returns>An option representing the value.</returns>
    /// <exclude />
    static member Some: value: 'T -> 'T option

    /// <summary>Implicitly converts a value into an optional that is a 'Some' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <remarks>The F# compiler ignored this method when determining possible type-directed conversions. Instead, use <c>Some</c> or <c>None</c> explicitly.</remarks>
    ///
    /// <returns>An option representing the value.</returns>
    /// <exclude />
    static member op_Implicit: value: 'T -> 'T option

    /// <summary>Get the value of a 'Some' option. A NullReferenceException is raised if the option is 'None'.</summary>
    [<CompilationRepresentation(CompilationRepresentationFlags.Instance)>]
    member Value: 'T

    /// <summary>Return 'true' if the option is a 'Some' value.</summary>
    member IsSome: bool

    /// <summary>Return 'true' if the option is a 'None' value.</summary>
    member IsNone: bool

/// <summary>The type of optional values. When used from other CLI languages the
/// empty option is the <c>null</c> value. </summary>
///
/// <remarks>Use the constructors <c>Some</c> and <c>None</c> to create values of this type.
/// Use the values in the <c>Option</c> module to manipulate values of this type,
/// or pattern match against the values directly.
///
/// 'None' values will appear as the value <c>null</c> to other CLI languages.
/// Instance methods on this type will appear as static methods to other CLI languages
/// due to the use of <c>null</c> as a value representation.</remarks>
///
/// <category index="3">Options</category>
and 'T option = Option<'T>

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


namespace Vesper.Collections


    /// <summary>The type of immutable singly-linked lists.</summary>
    ///
    /// <remarks>Use the constructors <c>[]</c> and <c>::</c> (infix) to create values of this type, or
    /// the notation <c>[1;2;3]</c>. Use the values in the <c>List</c> module to manipulate 
    /// values of this type, or pattern match against the values directly.
    /// </remarks>
    ///
    /// <exclude />
#if NETSTANDARD2_1_OR_GREATER
    [<System.Runtime.CompilerServices.CollectionBuilder(typeof<List>, "Create")>]
#endif
    [<DefaultAugmentation(false)>]
    [<StructuralEquality; StructuralComparison>]
    [<CompiledName("FSharpList`1")>]
    type List<'T> =
        | ([]): 'T list
        | (::): Head: 'T * Tail: 'T list -> 'T list

        /// <summary>Returns an empty list of a particular type</summary>
        static member Empty: 'T list
        
        /// <summary>Gets the number of items contained in the list</summary>
        member Length: int

        /// <summary>Gets a value indicating if the list contains no entries</summary>
        member IsEmpty: bool

        /// <summary>Gets the first element of the list</summary>
        member Head: 'T

        /// <summary>Gets the tail of the list, which is a list containing all the elements of the list, excluding the first element </summary>
        member Tail: 'T list

        /// <summary>Gets the element of the list at the given position.</summary>
        /// <remarks>Lists are represented as linked lists so this is an O(n) operation.</remarks>
        /// <param name="index">The index.</param>
        ///
        /// <returns>The value at the given index.</returns>
        member Item: index: int -> 'T with get 
        
        /// <summary>Gets a slice of the list, the elements of the list from the given start index to the given end index.</summary>
        ///
        /// <param name="startIndex">The start index.</param>
        /// <param name="endIndex">The end index.</param>
        ///
        /// <returns>The sub list specified by the input indices.</returns>
        member GetSlice: startIndex: int option * endIndex: int option -> 'T list  

        /// <summary>Get the index for the element offset elements away from the end of the collection.</summary>
        ///
        /// <param name="rank">The rank of the index.</param>
        /// <param name="offset">The offset from the end.</param>
        ///
        /// <returns>The corresponding index from the start.</returns>
        [<Experimental("Experimental library feature, requires '--langversion:preview'")>]
        member GetReverseIndex: rank: int * offset: int -> int

        /// <summary>Returns a list with <c>head</c> as its first element and <c>tail</c> as its subsequent elements</summary>
        ///
        /// <param name="head">A new head value for the list.</param>
        /// <param name="tail">The existing list.</param>
        ///
        /// <returns>The list with head appended to the front of tail.</returns>
        ///
        /// <remarks>This is an O(1) operation.</remarks>
        static member Cons: head: 'T * tail: 'T list -> 'T list

        interface IEnumerable<'T>
        interface IEnumerable
        interface IReadOnlyCollection<'T>
        interface IReadOnlyList<'T>

    /// <summary>The type of immutable singly-linked lists. </summary>
    ///
    /// <remarks>See the <see cref="T:Microsoft.FSharp.Collections.ListModule"/> module for further operations related to lists.
    ///
    /// Use the constructors <c>[]</c> and <c>::</c> (infix) to create values of this type, or
    /// the notation <c>[1; 2; 3]</c>. Use the values in the <c>List</c> module to manipulate 
    /// values of this type, or pattern match against the values directly.
    ///
    ///  See also <a href="https://learn.microsoft.com/dotnet/fsharp/language-reference/lists">F# Language Guide - Lists</a>.
    /// </remarks>
    and 'T list = List<'T>

#if NETSTANDARD2_1_OR_GREATER
    /// <summary>Contains methods for compiler use related to lists.</summary>
    and [<CompilerMessage("This type is for compiler use and should not be used directly", 1204, IsHidden=true);
          Sealed;
          AbstractClass;
          CompiledName("FSharpList")>] List =
        /// <summary>Creates a list with the specified items.</summary>
        ///
        /// <param name="items">The items to store in the list.</param>
        ///
        /// <returns>A list containing the specified items.</returns>
        [<CompilerMessage("This method is for compiler use and should not be used directly", 1204, IsHidden=true)>]
        static member Create: [<System.Runtime.CompilerServices.ScopedRef>] items: ReadOnlySpan<'T> -> 'T list
#endif

    /// <summary>An abbreviation for the CLI type <see cref="T:System.Collections.Generic.List`1"/></summary>
    type ResizeArray<'T> = System.Collections.Generic.List<'T>

    /// <summary>An abbreviation for the CLI type <see cref="T:System.Collections.Generic.IEnumerable`1"/></summary>
    ///
    /// <remarks>
    ///  See the <see cref="T:Microsoft.FSharp.Collections.SeqModule"/> module for further operations related to sequences.
    ///
    ///  See also <a href="https://learn.microsoft.com/dotnet/fsharp/language-reference/sequences">F# Language Guide - Sequences</a>.
    ///</remarks>
    type seq<'T> = IEnumerable<'T>

    /// Operations over `'T list`. `fold` first — it is what the canonical sample
    /// (`minimal-core-lib-plan` acceptance criteria) exercises; the rest of the
    /// module (`map`/`filter`/`iter`/`length`/`rev`/`append`/…) is additive, each
    /// a contract + impl pair added as the language grows. The `ModuleSuffix`
    /// representation lets the module share the `List` name with the type and
    /// gives it the compiled name `ListModule`. The folder's arrow desugars to
    /// `Vesper.Fun`.
    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =

        /// `fold f s [a; b; c]` computes `f (f (f s a) b) c`.
        val fold: folder: ('State -> 'T -> 'State) -> state: 'State -> list: 'T list -> 'State
