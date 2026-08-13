namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Char"/>.</summary>
///
/// <category>Basic Types</category>
type char = extern with

    interface equatable<char>
    interface comparable<char>

/// <summary>An abbreviation for the CLI type <see cref="T:System.String"/>.</summary>
///
/// <category>Basic Types</category>
type string = extern with

    interface equatable<string>
    interface comparable<string>

    /// <summary>Concatenation: <c>"Hello " + "World"</c>.</summary>
    static member inline (+): x: string * y: string -> string

    /// <summary>The UTF-16 code unit at <c>index</c>: <c>s.[i]</c>.</summary>
    member inline Item: index: int -> char with get
