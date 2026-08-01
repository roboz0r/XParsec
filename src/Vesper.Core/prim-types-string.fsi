namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Char"/>.</summary>
///
/// <category>Basic Types</category>
type char = extern

/// <summary>An abbreviation for the CLI type <see cref="T:System.String"/>.</summary>
///
/// <category>Basic Types</category>
type string = extern with

    /// <summary>Concatenation — the one non-numeric operand the arithmetic family
    /// admits, and the only member here whose body is a BCL CALL rather than a
    /// mnemonic. Spliced at the use site like every other width's.</summary>
    static member inline (+): x: string * y: string -> string
