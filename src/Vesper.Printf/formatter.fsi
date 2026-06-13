namespace Vesper

open System.IO
open System.Runtime.CompilerServices

// The write-through format handler (vesper-printf-plan P1): a stack-only ref
// struct that accumulates formatted output into a pooled / stack buffer and
// flushes to its sink. The backend emits construction + `AppendLiteral` /
// `AppendFormatted` calls inline for a fully-applied literal format (P2); it is
// shared with the `$"..."` interpolation surface (minimal-core-lib-plan D9).
// Signature-only here (the contract); the impl lands with the self-host rungs.

/// <summary>Stack-only handler that accumulates formatted text and flushes it to
/// a sink. Constructed and driven by the backend; users never name it.</summary>
[<Struct; IsByRefLike>]
type Formatter =
    /// Write-through sink: appended text is buffered and flushed to <c>writer</c>
    /// (via <c>TextWriter.Write(ReadOnlySpan&lt;char&gt;)</c>, no result string).
    new: literalLength: int * formattedCount: int * writer: TextWriter -> Formatter

    /// String sink: <c>ToStringAndClear</c> returns the accumulated text.
    new: literalLength: int * formattedCount: int -> Formatter

    /// Append a literal chunk of the format string.
    member AppendLiteral: value: string -> unit

    /// Append a typed hole. Generic ⇒ no boxing; an <c>ISpanFormattable</c> value
    /// formats straight into the buffer span.
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

    /// Append an F# <c>%0w.pf</c> hole: format the float via <c>format</c>
    /// (an <c>"F&lt;prec&gt;"</c> string), then zero-pad — after any sign — to a
    /// field of <c>width</c> chars. Dedicated because no .NET float format
    /// zero-pads to a total width.
    member AppendZeroPaddedFloat: value: float * format: string * width: int -> unit

    /// Append an F# <c>%A</c> hole as copy-pasteable Vesper source, laid out
    /// within a column budget of <c>width</c> chars (0 ⇒ never break — the
    /// <c>%0A</c> flat mode). Dedicated because <c>%A</c> drives the reflection-free
    /// structural engine rather than an <c>IFormattable</c> call.
    member AppendStructured: value: 'T * width: int -> unit

    /// Flush buffered text to the write-through sink and release the buffer.
    member Flush: unit -> unit

    /// Return the accumulated text (string sink) and release the buffer.
    member ToStringAndClear: unit -> string
