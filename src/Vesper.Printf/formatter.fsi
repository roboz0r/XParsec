namespace Vesper

open System.IO
open System.Text
open System.Runtime.CompilerServices

/// <summary>Stack-only handler that accumulates formatted text and flushes it to
/// a sink. Constructed and driven by the backend; users never write it.</summary>
[<Struct; IsByRefLike>]
type Formatter =
    /// Write-through sink: appended text is buffered, and <c>Flush</c> writes it to
    /// <c>writer</c>.
    new: literalLength: int * formattedCount: int * writer: TextWriter -> Formatter

    /// Builder sink (<c>bprintf</c>): buffered text is flushed to <c>builder</c>
    /// via <c>StringBuilder.Append</c>.
    new: literalLength: int * formattedCount: int * builder: StringBuilder -> Formatter

    /// String sink: <c>ToStringAndClear</c> returns the accumulated text.
    new: literalLength: int * formattedCount: int -> Formatter

    /// Append a literal chunk of the format string.
    member AppendLiteral: value: string -> unit

    /// Append a typed hole, formatted with the invariant culture. A <c>null</c> value
    /// appends <c>""</c>.
    member AppendFormatted: value: 'T -> unit

    /// Append a typed hole with a format specifier (e.g. <c>"F2"</c>, <c>"x"</c>).
    member AppendFormatted: value: 'T * format: string -> unit

    /// Append a typed hole with an alignment (field width); negative left-justifies.
    member AppendFormatted: value: 'T * alignment: int -> unit

    /// Append a typed hole with both alignment and a format specifier.
    member AppendFormatted: value: 'T * alignment: int * format: string -> unit

    /// Append an F# <c>%b</c> hole as lowercase <c>true</c>/<c>false</c> in a
    /// field of <c>alignment</c> chars (negative ⇒ left-justify; 0 ⇒ no padding).
    /// Dedicated because <c>bool.ToString()</c> capitalises.
    member AppendBool: value: bool * alignment: int -> unit

    /// Append an F# <c>%o</c> hole as 32-bit two's-complement octal in a field of
    /// <c>alignment</c> chars. Dedicated because .NET has no octal format string.
    member AppendOctal: value: int * alignment: int -> unit

    /// Append an F# <c>%u</c> hole as unsigned decimal in a field of
    /// <c>alignment</c> chars (the source <c>int</c> bits reinterpreted as <c>uint</c>).
    member AppendUnsigned: value: uint * alignment: int -> unit

    /// Append an F# <c>%08o</c> hole: 32-bit two's-complement octal, zero-padded to a
    /// total field of <c>width</c> chars. Dedicated because .NET has no octal format
    /// string; overflowing digits are not truncated (<c>%08o</c> -1 ⇒ 11 digits).
    member AppendZeroPaddedOctal: value: int * width: int -> unit

    /// Append an F# <c>%05u</c> hole: unsigned decimal, zero-padded to a total field
    /// of <c>width</c> chars (the source <c>int</c> bits reinterpreted as <c>uint</c>).
    /// Overflowing digits are not truncated (<c>%05u</c> -1 ⇒ 10 digits, no pad).
    member AppendZeroPaddedUnsigned: value: uint * width: int -> unit

    /// Append an F# <c>%0w.pf</c> hole: format the float via <c>format</c> (an
    /// <c>"F&lt;prec&gt;"</c> string), then zero-pad — after any sign — to a field of
    /// <c>width</c> chars. Dedicated because no .NET float format zero-pads to a total width.
    member AppendZeroPaddedFloat: value: float * format: string * width: int -> unit

    /// Append an F# <c>%-0w.pf</c> hole: F#'s left-align + zero-pad fills the RIGHT with
    /// zeros, past the digits, to a field of <c>width</c> chars (<c>%-08.2f</c> of
    /// <c>1.5</c> ⇒ <c>1.500000</c>). Overflow is a no-op; no truncation.
    member AppendRightZeroPaddedFloat: value: float * format: string * width: int -> unit

    /// Append an F# <c>%A</c> hole as copy-pasteable Vesper source, within a column
    /// budget of <c>width</c> chars (0 ⇒ never break — the <c>%0A</c> flat mode) and a
    /// node budget of <c>size</c> (F# PrintSize — nodes past it render as <c>...</c>).
    member AppendStructured: value: 'T * width: int * size: int -> unit

    /// Append a float hole with a runtime precision (<c>%.*f</c>/<c>%*.*f</c>/<c>%.*e</c>/
    /// <c>%.*g</c>), justified in a field of <c>alignment</c> chars. <c>typeChar</c> is the
    /// source type letter (<c>'f'</c>/<c>'e'</c>/<c>'E'</c>/<c>'g'</c>/<c>'G'</c>).
    member AppendDynamicPrecisionFloat: value: float * typeChar: char * precision: int * alignment: int -> unit

    /// A forced-sign float with a runtime precision (<c>%+.*f</c>/<c>% .*f</c>): a
    /// non-negative number gets a leading <c>+</c> (or <c> </c> when <c>space</c>)
    /// before justification.
    member AppendDynamicPrecisionSignedFloat:
        value: float * typeChar: char * precision: int * alignment: int * space: bool -> unit

    /// Guard a <c>%*d</c>-style runtime field width: a negative width throws
    /// <c>ArgumentOutOfRangeException</c> (<c>ParamName = "totalWidth"</c>), as F#'s
    /// <c>PadLeft</c>/<c>PadRight</c> do; a non-negative width returns unchanged.
    static member GuardTotalWidth: totalWidth: int -> int

    /// Clamp a <c>%*A</c> runtime column budget: a negative width renders flat rather
    /// than throwing, so it clamps to <c>0</c> (the <c>%0A</c> mode).
    static member ClampWidth: width: int -> int

    /// Clamp a runtime precision to <c>0..99</c> (F#'s <c>normalizePrecision</c>).
    /// Reached ONLY on the width=*+prec=* path; the prec=*-only paths keep the raw
    /// precision (<c>printf.fs:632</c> against <c>:649-657</c>).
    static member NormalizePrecision: precision: int -> int

    /// Flush buffered text to the write-through sink and release the buffer.
    member Flush: unit -> unit

    /// Return the accumulated text (string sink) and release the buffer.
    member ToStringAndClear: unit -> string
