namespace Vesper

// The printf family. Signature-only contract: the backend lowers a fully-applied
// literal call inline to `Formatter` calls (vesper-printf-plan P2), so on the
// happy path these have no runtime body — like the arithmetic operators. The
// `'Printer` type parameter is derived from the format literal by `PrintfSpec`.
// `[<AutoOpen>]` so `printfn` resolves unqualified, as in FSharp.Core's
// `ExtraTopLevelOperators`.

[<AutoOpen>]
module Printf =

    /// <summary>Print to stdout. <c>printf "%d" 42</c>.</summary>
    val printf: format: PrintfFormat<'Printer, unit, string, unit> -> 'Printer

    /// <summary>Print to stdout followed by a newline. <c>printfn "%d" 42</c>.</summary>
    val printfn: format: PrintfFormat<'Printer, unit, string, unit> -> 'Printer

    /// <summary>Format to a string. <c>sprintf "%d" 42 : string</c>.</summary>
    val sprintf: format: PrintfFormat<'Printer, unit, string, string> -> 'Printer
