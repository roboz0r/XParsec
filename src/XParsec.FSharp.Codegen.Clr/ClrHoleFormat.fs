namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm

/// The CLR-only projection of a printf hole's semantic `FieldFormat` back to the
/// `(HoleKind, .NET-format-string, alignment)` triple the `Vesper.Formatter` ABI
/// consumes. A .NET format string (`"x8"`, `"F2"`, `"+0;-0"`) is a CLR-runtime
/// dialect — the literal argument handed to `AppendFormatted<T>(v, format)` — so it
/// belongs *only* here, in the CLR backend; the JS backend reads `FieldFormat`
/// directly and never reconstructs one.
module ClrHoleFormat =

    /// Project a `FieldFormat` (+ its field alignment) to the legacy
    /// `(HoleKind, .NET-format-string, alignment)` triple. The zero-pad forms carry
    /// their width inside the `FieldFormat`, so the returned alignment slot is
    /// repurposed for it on `FixedZeroPad` (matching the legacy
    /// `AppendZeroPaddedFloat` ABI). `%A` (`HoleForm.PercentA`) has no .NET format
    /// string and isn't a `FieldFormat`, so it never reaches here — `EmitFormat`
    /// emits the `AppendStructured` handler from the `PercentA` fields directly.
    /// The alignment slot is carried through as an `Alignment`: `Star` (a runtime
    /// `%*d` width, supplied by the star-width lowering) and `Const` pass through
    /// where a field width lands; the zero-pad forms carry their width inside the
    /// `FieldFormat` and so return `Alignment.None` (the alignment slot is free),
    /// except `FixedZeroPad` and the zero-pad octal (`%08o`) / unsigned (`%05u`)
    /// forms, which repurpose the slot for their total width as a `Const` (the
    /// `AppendZeroPadded*` ABI). A `Star` never reaches the zero-pad forms —
    /// `tryClassify` defers `%0*d`.
    let toDotNetFormat (fmt: FieldFormat) (alignment: Alignment) : PrintfSpec.HoleKind * string option * Alignment =
        // A static .NET format string can only be built from a compile-time precision.
        // `Prec.Star` (`%.*f`, …) has no digits to embed — it routes through the
        // dynamic-precision handler member in `EmitFormat`, never here.
        let constPrec (p: Prec) : int =
            match p with
            | Prec.Const n -> n
            | Prec.Star ->
                failwith "ClrHoleFormat: star precision reached the static .NET-format projection (invariant broken)"

        match fmt with
        | FieldFormat.Verbatim -> PrintfSpec.HoleKind.Formatted, None, alignment
        | FieldFormat.DecimalZeroPad w -> PrintfSpec.HoleKind.Formatted, Some("D" + string w), Alignment.None
        | FieldFormat.IntRadix(radix, zeroPad) ->
            match radix with
            | Radix.Octal ->
                match zeroPad with
                | Some w -> PrintfSpec.HoleKind.OctalZeroPad, None, Alignment.Const w
                | None -> PrintfSpec.HoleKind.Octal, None, alignment
            | Radix.Hex upper ->
                let letter = if upper then "X" else "x"

                match zeroPad with
                | Some w -> PrintfSpec.HoleKind.Formatted, Some(letter + string w), Alignment.None
                | None -> PrintfSpec.HoleKind.Formatted, Some letter, alignment
            | Radix.Binary ->
                match zeroPad with
                | Some w -> PrintfSpec.HoleKind.Formatted, Some("B" + string w), Alignment.None
                | None -> PrintfSpec.HoleKind.Formatted, Some "B", alignment
        | FieldFormat.Unsigned zeroPad ->
            match zeroPad with
            | Some w -> PrintfSpec.HoleKind.UnsignedZeroPad, None, Alignment.Const w
            | None -> PrintfSpec.HoleKind.Unsigned, None, alignment
        | FieldFormat.Bool -> PrintfSpec.HoleKind.BoolText, None, alignment
        | FieldFormat.Fixed prec -> PrintfSpec.HoleKind.Formatted, Some("F" + string (constPrec prec)), alignment
        | FieldFormat.FixedZeroPad(prec, w) ->
            PrintfSpec.HoleKind.ZeroPaddedFloat, Some("F" + string prec), Alignment.Const w
        | FieldFormat.FixedRightZeroPad(prec, w) ->
            PrintfSpec.HoleKind.RightZeroPaddedFloat, Some("F" + string prec), Alignment.Const w
        | FieldFormat.Exponential(prec, upper) ->
            PrintfSpec.HoleKind.Formatted, Some((if upper then "E" else "e") + string (constPrec prec)), alignment
        | FieldFormat.Compact(prec, upper) ->
            PrintfSpec.HoleKind.Formatted, Some((if upper then "G" else "g") + string (constPrec prec)), alignment
        | FieldFormat.ExpCompactZeroPad(prec, w, typeChar) ->
            // Reuses the `%0w.Nf` zero-pad-after-sign handler over the `"e6"`/`"g6"` body.
            PrintfSpec.HoleKind.ZeroPaddedFloat, Some(string typeChar + string prec), Alignment.Const w
        | FieldFormat.ForcedSign(space, _prec, typeChar, zeroPad) ->
            // Only the INTEGER `'d'` forced-sign forms (`%+d`/`% d`/`%+05d`) ride a .NET
            // section format — integers carry no rounding, so the section format's
            // half-away midpoint behaviour is moot. Every FLOAT form (`'f'`/`'e'`…`'G'`,
            // and any runtime precision) is routed to the signed dynamic /
            // forced-sign-zero-pad handler in `EmitFormat` (which formats a half-to-even
            // `"F<prec>"`/`"e<prec>"` body then composes the sign), never this projection.
            if typeChar <> 'd' then
                failwithf
                    "ClrHoleFormat: forced-sign float %%%c reached the section-format projection (EmitFormat routes floats to the signed handler)"
                    typeChar

            let sign = if space then " " else "+"

            match zeroPad with
            | Option.None -> PrintfSpec.HoleKind.Formatted, Some(sign + "0;-" + "0"), alignment
            | Option.Some w ->
                // Zero-pad *through* the sign: a `%+05d` is the section format
                // `"+0000;-0000"` (digit count `w-1`), the sign supplied by the literal.
                let digits = System.String('0', max 1 (w - 1))
                PrintfSpec.HoleKind.Formatted, Some(sign + digits + ";-" + digits), alignment
