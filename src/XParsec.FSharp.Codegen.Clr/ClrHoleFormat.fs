namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm

/// The CLR-only projection of a printf hole's semantic `FieldFormat` to the
/// `(HoleKind, .NET-format-string, alignment)` triple the `Vesper.Formatter` ABI
/// consumes. A .NET format string (`"x8"`, `"F2"`, `"+0;-0"`) is a CLR dialect.
module ClrHoleFormat =

    /// The third slot carries a total WIDTH for the zero-pad `HoleKind`s that take one
    /// — `%08o` → `(OctalZeroPad, None, Const 8)` — and the field's own alignment
    /// otherwise. A zero-pad width is a `Const`; only a plain field alignment can be `Star`.
    let toDotNetFormat (fmt: FieldFormat) (alignment: Alignment) : PrintfSpec.HoleKind * string option * Alignment =
        // A static .NET format string needs a compile-time precision; `%.*f`
        // (`Prec.Star`) routes to the dynamic-precision handler instead.
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
            // Only the INTEGER `'d'` forms (`%+d` / `% d` / `%+05d`) ride a .NET section
            // format — integers carry no rounding, so the section format's half-away
            // midpoint behaviour is moot. A float form would round differently.
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
