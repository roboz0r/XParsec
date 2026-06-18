namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm

/// The CLR-only projection of a printf hole's semantic `FieldFormat` back to the
/// `(HoleKind, .NET-format-string, alignment)` triple the `Vesper.Formatter` ABI
/// consumes. A .NET format string (`"x8"`, `"F2"`, `"+0;-0"`) is a CLR-runtime
/// dialect — the literal argument handed to `AppendFormatted<T>(v, format)` — so it
/// belongs *only* here, in the CLR backend; the JS backend reads `FieldFormat`
/// directly and never reconstructs one. See printf-shared-core-plan.md ("`.NET`
/// format strings are a CLR dialect").
module ClrHoleFormat =

    /// Project a `FieldFormat` (+ its field alignment) to the legacy
    /// `(HoleKind, .NET-format-string, alignment)` triple. The zero-pad forms carry
    /// their width inside the `FieldFormat`, so the returned alignment slot is
    /// repurposed for it on `FixedZeroPad` (matching the legacy
    /// `AppendZeroPaddedFloat` ABI). `%A` (`HoleForm.PercentA`) has no .NET format
    /// string and isn't a `FieldFormat`, so it never reaches here — `EmitFormat`
    /// emits the `AppendStructured` handler from the `PercentA` fields directly.
    let toDotNetFormat (fmt: FieldFormat) (alignment: int option) : PrintfSpec.HoleKind * string option * int option =
        match fmt with
        | FieldFormat.Verbatim -> PrintfSpec.HoleKind.Formatted, None, alignment
        | FieldFormat.DecimalZeroPad w -> PrintfSpec.HoleKind.Formatted, Some("D" + string w), None
        | FieldFormat.IntRadix(radix, zeroPad) ->
            match radix with
            | Radix.Octal -> PrintfSpec.HoleKind.Octal, None, alignment
            | Radix.Hex upper ->
                let letter = if upper then "X" else "x"

                match zeroPad with
                | Some w -> PrintfSpec.HoleKind.Formatted, Some(letter + string w), None
                | None -> PrintfSpec.HoleKind.Formatted, Some letter, alignment
            | Radix.Binary ->
                match zeroPad with
                | Some w -> PrintfSpec.HoleKind.Formatted, Some("B" + string w), None
                | None -> PrintfSpec.HoleKind.Formatted, Some "B", alignment
        | FieldFormat.Unsigned -> PrintfSpec.HoleKind.Unsigned, None, alignment
        | FieldFormat.Bool -> PrintfSpec.HoleKind.BoolText, None, alignment
        | FieldFormat.Fixed prec -> PrintfSpec.HoleKind.Formatted, Some("F" + string prec), alignment
        | FieldFormat.FixedZeroPad(prec, w) -> PrintfSpec.HoleKind.ZeroPaddedFloat, Some("F" + string prec), Some w
        | FieldFormat.Exponential(prec, upper) ->
            PrintfSpec.HoleKind.Formatted, Some((if upper then "E" else "e") + string prec), alignment
        | FieldFormat.Compact(prec, upper) ->
            PrintfSpec.HoleKind.Formatted, Some((if upper then "G" else "g") + string prec), alignment
        | FieldFormat.ForcedSign(space, prec) ->
            let sign = if space then " " else "+"
            let body = if prec <= 0 then "0" else "0." + System.String('0', prec)
            PrintfSpec.HoleKind.Formatted, Some(sign + body + ";-" + body), alignment
