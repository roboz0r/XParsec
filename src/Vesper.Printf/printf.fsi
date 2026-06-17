namespace Vesper

// The printf family. The backend lowers fully-applied literal calls inline to
// `Formatter` calls. `[<AutoOpen>]` so `printfn` resolves unqualified.

[<AutoOpen>]
module Printf =

    /// <summary>Print to stdout. <c>printf "%d" 42</c>.</summary>
    val printf: format: PrintfFormat<'Printer, unit, string, unit> -> 'Printer

    /// <summary>Print to stdout followed by a newline. <c>printfn "%d" 42</c>.</summary>
    val printfn: format: PrintfFormat<'Printer, unit, string, unit> -> 'Printer

    /// <summary>Format to a string. <c>sprintf "%d" 42 : string</c>.</summary>
    val sprintf: format: PrintfFormat<'Printer, unit, string, string> -> 'Printer
