namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis.PrintfHoleForm

/// The CLR-only projection of a printf hole's semantic `FieldFormat` to the
/// `Vesper.Formatter` call that renders it. A .NET format string (`"x8"`, `"F2"`,
/// `"+0;-0"`) is a CLR dialect.
module ClrHoleFormat =

    /// The `Vesper.Formatter` append member a non-`%A` hole dispatches to, with the operands
    /// that follow the value. The member is a property of the specifier rather than of the
    /// value type, since `%o` and `%u` share the integer family. `%A` lowers separately.
    [<RequireQualifiedAccess>]
    type HoleCall =
        /// `AppendFormatted<Ty>(v [,alignment] [,format])`. The overload is the one whose
        /// optional parameters are present.
        | Formatted of format: string option * align: Alignment
        /// `AppendBool(v, alignment)` — lowercase `true`/`false`, where `bool.ToString`
        /// capitalises.
        | BoolText of align: Alignment
        /// `AppendUnsigned(v, alignment)` — unsigned decimal of the argument's bits at its
        /// own width, zero-extended to 64 (F# `%u`).
        | Unsigned of align: Alignment
        /// `AppendOctal(v, alignment)` — two's-complement octal (.NET has no octal format
        /// string), matching F#.
        | Octal of align: Alignment
        /// `AppendZeroPaddedUnsigned(v, width)` — F# `%05u`: unsigned decimal, zero-padded
        /// to a total field of `width`.
        | UnsignedZeroPad of width: int
        /// `AppendZeroPaddedOctal(v, width)` — F# `%08o`: two's-complement octal,
        /// zero-padded to a total field of `width`.
        | OctalZeroPad of width: int
        /// `AppendZeroPaddedFloat<Ty>(v, body, width)` — F# `%0w.pf`: format via `body`,
        /// then zero-pad AFTER ANY SIGN to a total field of `width`.
        | ZeroPaddedFloat of body: string * width: int
        /// `AppendRightZeroPaddedFloat<Ty>(v, body, width)` — F# `%-0w.Nf`: F#'s left-align
        /// plus zero-pad fills the RIGHT, past the digits, out to `width`.
        | RightZeroPaddedFloat of body: string * width: int
        /// `AppendForcedSignZeroPaddedFloat<Ty>(v, body, width, space)` — F# `%+0w.pf`: the
        /// sign is composed first, then the zeros fill after it.
        | ForcedSignZeroPaddedFloat of body: string * width: int * space: bool
        /// `AppendDynamicPrecisionFloat<Ty>(v, typeChar, precision, alignment)` — F# `%.*f`
        /// and friends, whose .NET format string is built in-handler.
        | DynamicPrecisionFloat of typeChar: char * precision: Prec * align: Alignment
        /// `AppendDynamicPrecisionSignedFloat<Ty>(v, typeChar, precision, alignment, space)`:
        /// every forced-sign float form. A static precision routes here too, since the
        /// `"F<prec>"` body rounds half-to-even where a .NET section format rounds half-away.
        | DynamicPrecisionSignedFloat of typeChar: char * precision: Prec * align: Alignment * space: bool

    /// The .NET format string for a float body: the source type letter and its digit count
    /// (`"F2"`, `"e6"`, `"G3"`). `%f` has no case variant, so it takes the canonical `"F"`;
    /// the others keep the case the placeholder was written in.
    let private floatBody (typeChar: char) (prec: int) : string =
        (if typeChar = 'f' then "F" else string typeChar) + string prec

    /// The `Vesper.Formatter` call a classified field hole lowers to.
    let holeCall (fmt: FieldFormat) (align: Alignment) : HoleCall =
        match fmt with
        | FieldFormat.Verbatim -> HoleCall.Formatted(None, align)
        | FieldFormat.DecimalZeroPad w -> HoleCall.Formatted(Some("D" + string w), Alignment.None)
        | FieldFormat.Bool -> HoleCall.BoolText align

        | FieldFormat.IntRadix(Radix.Octal, Some w) -> HoleCall.OctalZeroPad w
        | FieldFormat.IntRadix(Radix.Octal, None) -> HoleCall.Octal align
        | FieldFormat.IntRadix(Radix.Hex upper, zeroPad) ->
            let letter = if upper then "X" else "x"

            match zeroPad with
            | Some w -> HoleCall.Formatted(Some(letter + string w), Alignment.None)
            | None -> HoleCall.Formatted(Some letter, align)
        | FieldFormat.IntRadix(Radix.Binary, zeroPad) ->
            match zeroPad with
            | Some w -> HoleCall.Formatted(Some("B" + string w), Alignment.None)
            | None -> HoleCall.Formatted(Some "B", align)

        | FieldFormat.Unsigned(Some w) -> HoleCall.UnsignedZeroPad w
        | FieldFormat.Unsigned None -> HoleCall.Unsigned align

        // A static precision composes a .NET format string; a runtime one (`%.*f`) reaches
        // the handler that builds the string from `typeChar` + `precision` instead.
        | FieldFormat.Fixed Prec.Star -> HoleCall.DynamicPrecisionFloat('f', Prec.Star, align)
        | FieldFormat.Fixed(Prec.Const n) -> HoleCall.Formatted(Some(floatBody 'f' n), align)
        | FieldFormat.Exponential(prec, upper) ->
            let tc = if upper then 'E' else 'e'

            match prec with
            | Prec.Star -> HoleCall.DynamicPrecisionFloat(tc, Prec.Star, align)
            | Prec.Const n -> HoleCall.Formatted(Some(floatBody tc n), align)
        | FieldFormat.Compact(prec, upper) ->
            let tc = if upper then 'G' else 'g'

            match prec with
            | Prec.Star -> HoleCall.DynamicPrecisionFloat(tc, Prec.Star, align)
            | Prec.Const n -> HoleCall.Formatted(Some(floatBody tc n), align)

        | FieldFormat.FixedZeroPad(prec, w) -> HoleCall.ZeroPaddedFloat(floatBody 'f' prec, w)
        | FieldFormat.FixedRightZeroPad(prec, w) -> HoleCall.RightZeroPaddedFloat(floatBody 'f' prec, w)
        // `%014e` reuses the `%0w.Nf` zero-pad-after-sign handler over an `"e6"`/`"g6"` body.
        | FieldFormat.ExpCompactZeroPad(prec, w, typeChar) -> HoleCall.ZeroPaddedFloat(floatBody typeChar prec, w)

        | FieldFormat.ForcedSign(space, Prec.Star, typeChar, _) ->
            HoleCall.DynamicPrecisionSignedFloat(typeChar, Prec.Star, align, space)
        // Only the INTEGER `'d'` forms (`%+d` / `% d` / `%+05d`) use a .NET section format.
        // Integers carry no rounding, so the section format's half-away midpoint behaviour
        // is moot, where a float form would round differently.
        | FieldFormat.ForcedSign(space, Prec.Const _, 'd', zeroPad) ->
            let sign = if space then " " else "+"

            match zeroPad with
            | None -> HoleCall.Formatted(Some(sign + "0;-0"), align)
            | Some w ->
                // Zero-pad *through* the sign: a `%+05d` is the section format
                // `"+0000;-0000"` (digit count `w-1`), the sign supplied by the literal.
                let digits = System.String('0', max 1 (w - 1))
                HoleCall.Formatted(Some(sign + digits + ";-" + digits), align)
        | FieldFormat.ForcedSign(space, Prec.Const n, typeChar, Some w) ->
            HoleCall.ForcedSignZeroPaddedFloat(floatBody typeChar n, w, space)
        | FieldFormat.ForcedSign(space, Prec.Const n, typeChar, None) ->
            HoleCall.DynamicPrecisionSignedFloat(typeChar, Prec.Const n, align, space)
