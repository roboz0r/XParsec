namespace Vesper.Collections

// Vesper.List contract — the cons-list type + the `List` module, carved out of
// Vesper.Core's `core-types.fsi` (package-split-plan PS1: one package per type).
// Like the rest of the Vesper tree this is the front-end symbol contract: parsed
// by XParsec.FSharp and walked into an IExternalSymbolProvider. The runtime impl
// is `list.fs` (→ Vesper.List.dll, BCL-only, our own backend) — the verbatim
// `[]`/`::` + `module List` form, compilable since the front end lowers cons
// patterns/construction (the cutover).
//
// Depends on Vesper.Core (`Fun`, `unit`, `int`, `bool`) and Vesper.Option — the
// `List.GetSlice` member below names `int option`, resolving the forward-reference
// that `src/Vesper.Option/README.md` documented while `List` still lived in Core.
//
// Per package-split-plan PS5 the package is named `Vesper.List` but it
// contributes type `List` into namespace `Vesper.Collections`, not `Vesper.List`.
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): Fantomas strips the
// `[]`/`::` operator-union-case payloads. Authored to a fixed shape; parser
// coverage is the golden `.parsed` snapshot.

open System
open System.Collections.Generic


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

        /// `length list` returns the number of elements in the list.
        val length: list: 'T list -> int

        /// `isEmpty list` returns true when the list contains no elements.
        val isEmpty: list: 'T list -> bool

        /// `head list` returns the first element. Raises when the list is empty.
        val head: list: 'T list -> 'T

        /// `tail list` returns the list without its first element. Raises when the
        /// list is empty.
        val tail: list: 'T list -> 'T list

        /// `map mapping list` builds a new list by applying `mapping` to each element.
        val map: mapping: ('T -> 'U) -> list: 'T list -> 'U list

        /// `filter predicate list` keeps the elements for which `predicate` returns true.
        val filter: predicate: ('T -> bool) -> list: 'T list -> 'T list

        /// `append list1 list2` returns the elements of `list1` followed by those of `list2`.
        val append: list1: 'T list -> list2: 'T list -> 'T list

        /// `rev list` returns the list with its elements in reverse order.
        val rev: list: 'T list -> 'T list

        /// `ofSeq source` builds a new list from the given enumerable object.
        /// Depends on `for x in IEnumerable` (vesper-set-sprint Phase 4).
        val ofSeq: source: seq<'T> -> 'T list

        /// `toSeq list` views the given list as a sequence.
        val toSeq: list: 'T list -> seq<'T>
