namespace Vesper

/// <summary>An abbreviation for the CLI type <see cref="T:System.Numerics.BigInteger"/> —
/// the arbitrary-precision signed integer.</summary>
///
/// <category>Basic Types</category>
type bigint =
    extern with

    static member inline (+): x: bigint * y: bigint -> bigint

    static member inline (-): x: bigint * y: bigint -> bigint

    static member inline ( * ): x: bigint * y: bigint -> bigint

    static member inline (/): x: bigint * y: bigint -> bigint

    static member inline (%): x: bigint * y: bigint -> bigint

    static member inline (~-): n: bigint -> bigint
