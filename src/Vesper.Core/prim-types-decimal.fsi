namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Decimal"/>.</summary>
///
/// <category>Basic Types</category>
type decimal =
    extern with

    interface equatable<decimal>
    interface comparable<decimal>

    static member inline (+): x: decimal * y: decimal -> decimal

    static member inline (-): x: decimal * y: decimal -> decimal

    static member inline ( * ): x: decimal * y: decimal -> decimal

    static member inline (/): x: decimal * y: decimal -> decimal

    static member inline (%): x: decimal * y: decimal -> decimal

    static member inline (~+): value: decimal -> decimal

    static member inline (~-): n: decimal -> decimal
