namespace Vesper

// The parsed-format type. On the fully-applied literal happy path (vesper-printf-
// plan P2) no value of this type is constructed — the format lowers inline. It
// exists in the contract to *type the format literal*: `PrintfSpec` derives
// `'Printer` (the curried hole types) from the literal, which type-checks the
// args and drives the lowering. A value is materialised only in the static-field
// / cold representation (P4), where it is the one place D4's `PrintfFormat`
// survives.

/// <summary>Type of a parsed format string.</summary>
///
/// <typeparam name="Printer">The curried function the format applies to, e.g.
///   <c>"%d"</c> ⇒ <c>int -> 'Result</c>. Derived from the literal.</typeparam>
/// <typeparam name="State">The state threaded by the formatter (the sink, e.g.
///   <c>TextWriter</c>, or <c>unit</c>).</typeparam>
/// <typeparam name="Residue">The residue type for <c>%t</c>-style callbacks.</typeparam>
/// <typeparam name="Result">The final result, e.g. <c>unit</c> for <c>printfn</c>,
///   <c>string</c> for <c>sprintf</c>.</typeparam>
type PrintfFormat<'Printer, 'State, 'Residue, 'Result> =
    /// Construct a format object from its source string.
    new: value: string -> PrintfFormat<'Printer, 'State, 'Residue, 'Result>

    /// The original format string.
    member Value: string

/// <summary>Type of a parsed format string, carrying the captured argument
/// tuple type as a fifth parameter.</summary>
type PrintfFormat<'Printer, 'State, 'Residue, 'Result, 'Tuple> =
    inherit PrintfFormat<'Printer, 'State, 'Residue, 'Result>

    /// Construct a format object from its source string.
    new: value: string -> PrintfFormat<'Printer, 'State, 'Residue, 'Result, 'Tuple>

/// <summary>Abbreviation for <see cref="T:Vesper.PrintfFormat`4"/>.</summary>
type Format<'Printer, 'State, 'Residue, 'Result> = PrintfFormat<'Printer, 'State, 'Residue, 'Result>
