namespace Vesper

// The floats state the arithmetic family and `~-`, no bitwise family: `1.0 &&& 2.0` is
// an error because `float` declares no such member.

/// <summary>An abbreviation for the CLI type <see cref="T:System.Single"/>.</summary>
///
/// <category>Basic Types</category>
type float32 =
    extern with

    interface equatable<float32>
    interface comparable<float32>

    static member inline (+): x: float32 * y: float32 -> float32

    static member inline (-): x: float32 * y: float32 -> float32

    static member inline ( * ): x: float32 * y: float32 -> float32

    static member inline (/): x: float32 * y: float32 -> float32

    static member inline (%): x: float32 * y: float32 -> float32

    static member inline (~+): value: float32 -> float32

    static member inline (~-): n: float32 -> float32

/// <summary>An abbreviation for the CLI type <see cref="T:System.Double"/>.</summary>
///
/// <category>Basic Types</category>
type float =
    extern with

    interface equatable<float>
    interface comparable<float>

    static member inline (+): x: float * y: float -> float

    static member inline (-): x: float * y: float -> float

    static member inline ( * ): x: float * y: float -> float

    static member inline (/): x: float * y: float -> float

    static member inline (%): x: float * y: float -> float

    static member inline (~+): value: float -> float

    static member inline (~-): n: float -> float

/// <summary>An abbreviation for the CLI type <see cref="T:System.Single"/>. Identical to <see cref="T:Microsoft.FSharp.Core.float32"/>.</summary>
///
/// <category>Basic Types</category>
type single = float32

/// <summary>An abbreviation for the CLI type <see cref="T:System.Double"/>. Identical to <see cref="T:Microsoft.FSharp.Core.float"/>.</summary>
///
/// <category>Basic Types</category>
type double = float
