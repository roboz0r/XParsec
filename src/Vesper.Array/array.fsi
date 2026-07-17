namespace Vesper.Collections

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =

    /// <summary>Creates an array whose elements are all initially the default value.</summary>
    ///
    /// <param name="count">The length of the array to create.</param>
    ///
    /// <returns>The created array.</returns>
    val zeroCreate: count: int -> 'T[]

    /// <summary>Returns the length of an array.</summary>
    ///
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The length of the array.</returns>
    val length: array: 'T[] -> int

    /// <summary>Returns true if the given array is empty, otherwise false.</summary>
    ///
    /// <param name="array">The input array.</param>
    ///
    /// <returns>True if the array is empty.</returns>
    val isEmpty: array: 'T[] -> bool

    /// <summary>Gets an element from an array.</summary>
    ///
    /// <param name="array">The input array.</param>
    /// <param name="index">The input index.</param>
    ///
    /// <returns>The value of the array at the given index.</returns>
    val get: array: 'T[] -> index: int -> 'T

    /// <summary>Sets an element of an array.</summary>
    ///
    /// <param name="array">The input array.</param>
    /// <param name="index">The input index.</param>
    /// <param name="value">The input value.</param>
    val set: array: 'T[] -> index: int -> value: 'T -> unit

    /// <summary>Creates an array whose elements are all initially the given value.</summary>
    ///
    /// <param name="count">The length of the array to create.</param>
    /// <param name="value">The value for the elements.</param>
    ///
    /// <returns>The created array.</returns>
    val create: count: int -> value: 'T -> 'T[]

    /// <summary>Creates an array by calling the given generator on each index.</summary>
    ///
    /// <param name="count">The number of elements to initialize.</param>
    /// <param name="initializer">The function to generate the initial values for each index.</param>
    ///
    /// <returns>The created array.</returns>
    val init: count: int -> initializer: (int -> 'T) -> 'T[]

    /// <summary>Builds a new array that contains the elements of the given array.</summary>
    ///
    /// <param name="array">The input array.</param>
    ///
    /// <returns>A copy of the input array.</returns>
    val copy: array: 'T[] -> 'T[]

    /// <summary>Builds a new array that contains the elements of the first array
    /// followed by the elements of the second array.</summary>
    ///
    /// <param name="array1">The first input array.</param>
    /// <param name="array2">The second input array.</param>
    ///
    /// <returns>The resulting array.</returns>
    val append: array1: 'T[] -> array2: 'T[] -> 'T[]

    /// <summary>Returns a new array with the elements in reverse order.</summary>
    ///
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The reversed array.</returns>
    val rev: array: 'T[] -> 'T[]

    /// <summary>Builds a new array whose elements are the results of applying the given
    /// function to each of the elements of the array.</summary>
    ///
    /// <param name="mapping">The function to transform elements of the array.</param>
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The array of transformed elements.</returns>
    val map<'T, 'U> : mapping: ('T -> 'U) -> array: 'T[] -> 'U[]

    /// <summary>Builds a new array whose elements are the results of applying the given
    /// function to each of the elements of the array. The integer index passed to the
    /// function indicates the index of element being transformed.</summary>
    ///
    /// <param name="mapping">The function to transform elements and their indices.</param>
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The array of transformed elements.</returns>
    val mapi<'T, 'U> : mapping: (int -> 'T -> 'U) -> array: 'T[] -> 'U[]

    /// <summary>Applies the given function to each element of the array.</summary>
    ///
    /// <param name="action">The function to apply to each element of the array.</param>
    /// <param name="array">The input array.</param>
    val iter: action: ('T -> unit) -> array: 'T[] -> unit

    /// <summary>Applies the given function to each element of the array. The integer
    /// passed to the function indicates the index of element.</summary>
    ///
    /// <param name="action">The function to apply to each index and element.</param>
    /// <param name="array">The input array.</param>
    val iteri: action: (int -> 'T -> unit) -> array: 'T[] -> unit

    /// <summary>Applies a function to each element of the array, threading an accumulator
    /// argument through the computation. Apply the function to the first two elements of the
    /// array. Then feed this result into the function along with the third element and so on.
    /// Return the final result.</summary>
    ///
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The final state.</returns>
    val fold<'T, 'State> : folder: ('State -> 'T -> 'State) -> state: 'State -> array: 'T[] -> 'State

    /// <summary>Applies a function to each element of the array, starting from the end,
    /// threading an accumulator argument through the computation.</summary>
    ///
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="array">The input array.</param>
    /// <param name="state">The initial state.</param>
    ///
    /// <returns>The final state.</returns>
    val foldBack<'T, 'State> : folder: ('T -> 'State -> 'State) -> array: 'T[] -> state: 'State -> 'State
