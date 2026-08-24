namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

/// Target-neutral *semantic* model of a printf format hole, shared by the lowering gate
/// (structured `Format` node, or generic printf call?) and both backends. Projecting one
/// back to a .NET format string is a CLR-only dialect.
module PrintfHoleForm =

    /// `%A` print-*width* budget; F# repurposes the field-alignment slot for it. `Never` ⇒
    /// `%0A` (flat, never break), `Cols n` ⇒ `%NA` (break at column n).
    [<RequireQualifiedAccess>]
    type PrintWidth =
        | Default
        | Never
        | Cols of int
        /// `%*A`: the column budget is a runtime argument (the leading star `int`), so
        /// there is no static column count.
        | Star

    /// `%A` print-*size* budget (F#'s `PrintSize` node count): `Cols n` ⇒ `%.NA`,
    /// `Star` ⇒ `%.*A` (a runtime `int` following any width).
    [<RequireQualifiedAccess>]
    type PrintSize =
        | Default
        | Cols of int
        | Star

    /// A precision dimension for the float field forms. `Const` is a static digit count (the
    /// `%.Nf` / default); `Star` (`%.*f`, `%*.*f`) defers it to the `int` following any star width.
    [<RequireQualifiedAccess>]
    type Prec =
        | Const of int
        | Star

    /// The base / case of an integer rendered in a non-decimal radix.
    [<RequireQualifiedAccess>]
    type Radix =
        /// `%x` / `%X`: the `upper` flag is the digit case (`'a'` vs `'A'`).
        | Hex of upper: bool
        /// `%B`: binary; two's-complement for negatives.
        | Binary
        /// `%o`: base 8; two's-complement for negatives.
        | Octal

    /// A non-`%A` hole's per-value formatting, semantically.
    [<RequireQualifiedAccess>]
    type FieldFormat =
        /// `%s` `%O` `%c` `%M` `%d`/`%i` (no zero-pad): stringify verbatim.
        | Verbatim
        /// `%0wd`: signed decimal integer, zeros inserted *after any sign* to a
        /// total field of `width` (`(-42)` ⇒ `"-00042"`).
        | DecimalZeroPad of width: int
        /// `%x`/`%X`/`%B`/`%o`: unsigned integer in `radix`. `zeroPad = Some w`
        /// zero-pads to a total field of `w` (F# zero-pads octal too, e.g. `%08o`).
        | IntRadix of radix: Radix * zeroPad: int option
        /// `%u`: the source integer's bits, at its own width, reinterpreted unsigned.
        /// `zeroPad = Some w` zero-pads to a total field of `w` (`%05u`; overflowing digits
        /// are not truncated).
        | Unsigned of zeroPad: int option
        /// `%b`: lowercase `true` / `false`.
        | Bool
        /// `%f` / `%.Nf` / `%.*f`: fixed-point with `precision` fraction digits
        /// (`Star` ⇒ runtime precision).
        | Fixed of precision: Prec
        /// `%0w.Nf`: fixed-point, then zeros after any sign to a total field of `width`.
        /// A star precision (`%0w.*f`) is declined rather than reaching this case.
        | FixedZeroPad of precision: int * width: int
        /// `%-0w.Nf`: fixed-point, then zeros on the RIGHT (past the point) to a total field
        /// of `width`, because F#'s left-align plus zero-pad on a float fills the right
        /// (`%-05.2f` 3.14159 ⇒ `"3.140"`).
        | FixedRightZeroPad of precision: int * width: int
        /// `%e` / `%E` / `%.*e`: scientific, `precision` fraction digits (`Star` ⇒ runtime),
        /// `upper` exponent case. JS `toExponential` gives a minimal exponent width, not
        /// .NET's 3-digit zero-pad, so the two targets are not byte-identical.
        | Exponential of precision: Prec * upper: bool
        /// `%g` / `%G` / `%.*g`: compact, `precision` significant digits (`Star` ⇒ runtime),
        /// `upper` case. JS `toPrecision` keeps trailing zeros and switches to exponential at
        /// a different threshold, so the two targets are not byte-identical.
        | Compact of precision: Prec * upper: bool
        /// `%0we`/`%0wE`/`%0wg`/`%0wG`: scientific / compact, then zeros after any sign to a
        /// total field of `width` (`%014e` 1234.5 ⇒ `"01.234500e+003"`). `typeChar` is the
        /// source letter (`'e'`/`'E'`/`'g'`/`'G'`); `precision` its static digit count.
        | ExpCompactZeroPad of precision: int * width: int * typeChar: char
        /// `%+d`/`% d`/`%+05d`/`%+.Nf`/`%+08.2f`/`%+.*f`/`%+e`/`%+g`: forced-sign. `space` ⇒ a
        /// leading space on a non-negative value (else a `+`); a negative keeps its `-`.
        /// `zeroPad = Some w` pads *through* the sign to a total field of `w` (`'d'`/`'f'` only).
        | ForcedSign of space: bool * precision: Prec * typeChar: char * zeroPad: int option

    /// A `Field` hole's alignment-slot intent. `Const` is a signed width (negative ⇒
    /// left-justify); `Star` (`%*d`, `%-*d`) defers the width to a runtime argument.
    [<RequireQualifiedAccess>]
    type Alignment =
        | None
        | Const of int
        | Star of leftJustify: bool

    /// A classified format hole. `Field` carries the field alignment (the `%5d` / `%-5d`
    /// width) alongside the formatting, but the zero-pad forms set it `None`, their width
    /// riding inside the `FieldFormat`.
    [<RequireQualifiedAccess>]
    type HoleForm =
        | PercentA of width: PrintWidth * size: PrintSize
        | Field of fmt: FieldFormat * alignment: Alignment
        /// `%a` (`hasValue = true`) / `%t` (`hasValue = false`): a printer callback hole.
        /// `%a` consumes a callback `('State -> 'T -> 'Residue)` AND a value `'T`; `%t` just
        /// `('State -> 'Residue)`.
        | Callback of hasValue: bool

    /// Resolve a *static* `%A` width budget to the concrete column count the engines take
    /// (80 by default). `Star` (`%*A`) has none, because the width arrives as a runtime argument.
    let percentAWidth (w: PrintWidth) : int voption =
        match w with
        | PrintWidth.Default -> ValueSome 80
        | PrintWidth.Never -> ValueSome 0
        | PrintWidth.Cols n -> ValueSome n
        | PrintWidth.Star -> ValueNone

    /// Resolve a *static* `%A` size budget (F#'s `PrintSize` node count) to a concrete count
    /// (10000 by default). `Star` (`%.*A`) has none, because the size arrives as a runtime argument.
    let percentASize (size: PrintSize) : int voption =
        match size with
        | PrintSize.Default -> ValueSome 10000
        | PrintSize.Cols n -> ValueSome n
        | PrintSize.Star -> ValueNone

    /// The runtime clamp F# applies to a form's *star* width (`%*d` / `%*A`).
    /// `Guard` (every padding `Field` form) throws `ArgumentOutOfRangeException` on a
    /// negative width; `Clamp` (`%*A`) renders a negative budget flat (→ 0) instead.
    [<RequireQualifiedAccess>]
    type StarWidthClamp =
        | Guard
        | Clamp

    let starWidthClamp (form: HoleForm) : StarWidthClamp voption =
        match form with
        | HoleForm.Field(_, Alignment.Star _) -> ValueSome StarWidthClamp.Guard
        | HoleForm.PercentA(PrintWidth.Star, _) -> ValueSome StarWidthClamp.Clamp
        | _ -> ValueNone

    /// True iff a float `Field` carries a *runtime* precision (`%.*f`/`%.*e`/`%.*g`/`%+.*f`).
    let isDynamicPrecisionFloat (fmt: FieldFormat) : bool =
        match fmt with
        | FieldFormat.Fixed Prec.Star
        | FieldFormat.Exponential(Prec.Star, _)
        | FieldFormat.Compact(Prec.Star, _)
        | FieldFormat.ForcedSign(_, Prec.Star, _, _) -> true
        | _ -> false

    /// True iff F# clamps a runtime star precision to `0..99` on this form: ONLY the two-star
    /// float-field path. The prec-star-only `Field` paths and `%.*A` keep the raw precision.
    let normalizesStarPrecision (form: HoleForm) : bool =
        match form with
        | HoleForm.Field(fmt, Alignment.Star _) -> isDynamicPrecisionFloat fmt
        | _ -> false

    /// Reconstruct a placeholder's source text (`"%0*d"`, `"%+08.2f"`, `"%-*A"`) for
    /// diagnostics. Not a round-trip: the lexer canonicalises flag order and duplication.
    let renderPlaceholder (p: FormatPlaceholder) : string =
        let dim (d: FormatDim) =
            match d with
            | FormatDim.Absent -> ""
            | FormatDim.Star -> "*"
            | FormatDim.Literal n -> string n

        let prec =
            match p.Precision with
            | FormatDim.Absent -> ""
            | d -> "." + dim d

        "%" + p.Flags + dim p.Width + prec + string p.TypeChar

    /// The target-neutral `HoleForm` of a placeholder, or `ValueNone` for a specifier no
    /// backend renders faithfully. Parity with F# `printf` under `InvariantCulture` is the
    /// bar for accepting.
    let private tryLowerableForm (p: FormatPlaceholder) : HoleForm voption =
        let precIsStar = p.Precision = FormatDim.Star

        let flags = p.Flags
        let has (c: char) = flags.IndexOf c >= 0
        let zeroPad = has '0'
        let leftAlign = has '-'
        let plusSign = has '+'
        let spaceSign = has ' '

        let widthIsStar = p.Width = FormatDim.Star

        let width =
            match p.Width with
            | FormatDim.Literal w -> Some(int w)
            | FormatDim.Absent
            | FormatDim.Star -> None

        // A star counts as a width for the flag-sanity gates below.
        let hasWidth = width.IsSome || widthIsStar

        // `-` and `0` are inert without a width: `%-d ≡ %d`, `%0d ≡ %d`, `%-.2f ≡ %.2f`.
        // Dropping them here also makes every `width.Value` read under `zeroPad` safe.
        let leftAlign = leftAlign && hasWidth
        let zeroPad = zeroPad && hasWidth

        let isFloatLike =
            match p.Type with
            | FormatType.FloatDecimal
            | FormatType.FloatExponential
            | FormatType.FloatCompact
            | FormatType.Decimal -> true
            | _ -> false

        // The field alignment for every non-zero-pad form.
        let alignment: Alignment =
            if zeroPad then
                Alignment.None
            elif widthIsStar then
                Alignment.Star leftAlign
            else
                match width with
                | Some w -> Alignment.Const(if leftAlign then -w else w)
                | None -> Alignment.None

        // Only reached in the float arms, because every other type defers a star precision first.
        let precDim (dflt: int) : Prec =
            match p.Precision with
            | FormatDim.Star -> Prec.Star
            | FormatDim.Literal pr -> Prec.Const(int pr)
            | FormatDim.Absent -> Prec.Const dflt

        let sizeDim: PrintSize =
            match p.Precision with
            | FormatDim.Star -> PrintSize.Star
            | FormatDim.Literal pr -> PrintSize.Cols(int pr)
            | FormatDim.Absent -> PrintSize.Default

        if p.Type = FormatType.Structured then
            // `%A`: the `0` flag forces flat (width 0) ahead of any explicit width, and
            // `+`/`-`/` ` are no-ops. `%0*A` is declined: in F# the `0` flag also discards
            // the runtime column budget, so the hole would consume a star argument and ignore it.
            if widthIsStar then
                if zeroPad then
                    ValueNone
                else
                    ValueSome(HoleForm.PercentA(PrintWidth.Star, sizeDim))
            else
                let pw =
                    // `%0A`/`%05A` force flat regardless of width, so this reads the
                    // un-normalized flag (normalization drops a width-less `0`).
                    if has '0' then
                        PrintWidth.Never
                    else
                        match width with
                        | Some n -> PrintWidth.Cols n
                        | None -> PrintWidth.Default

                ValueSome(HoleForm.PercentA(pw, sizeDim))
        elif widthIsStar && zeroPad then
            // `%0*d`: the zero-pad forms embed the width *inside* `FieldFormat` as a
            // compile-time `int`, and a star width has none, so it stays cold.
            ValueNone
        elif plusSign || spaceSign then
            // Forced-sign. `+` wins over a space flag when both are present, and this arm
            // runs only when one of them is set, so `not plusSign` ⇒ render a space.
            let space = not plusSign

            // A literal precision as a static digit count (default `dflt`), or `ValueNone`
            // for a star precision, because these forms build a `Prec.Const` only.
            let constPrecOr (dflt: int) : int voption =
                match p.Precision with
                | FormatDim.Star -> ValueNone
                | FormatDim.Literal pr -> ValueSome(int pr)
                | FormatDim.Absent -> ValueSome dflt

            match p.Type with
            | FormatType.DecimalInt ->
                // Integer forced sign has no precision slot, so a star precision (`%+.*d`)
                // is deferred. `%+05d` zero-pads through the sign, so the width rides inside
                // the `FieldFormat`, leaving `Alignment.None`.
                if precIsStar then
                    ValueNone
                else
                    let zp = if zeroPad then Some width.Value else None
                    let align = if zeroPad then Alignment.None else alignment
                    ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, Prec.Const 0, 'd', zp), align))
            | FormatType.FloatDecimal ->
                // `%+.Nf` / `% .Nf` and `%+08.2f` / `% 08.2f` (sign + zero-pad) both lower to
                // a `ForcedSign` fixed-float form. A star precision under zero-pad
                // (`%+08.*f`) has no handler, so decline it.
                if zeroPad && precIsStar then
                    ValueNone
                else
                    let prec =
                        match p.Precision with
                        | FormatDim.Star -> Prec.Star
                        | FormatDim.Literal pr -> Prec.Const(let n = int pr in if n <= 0 then 0 else n)
                        | FormatDim.Absent -> Prec.Const 6

                    let zp = if zeroPad then Some width.Value else None
                    let align = if zeroPad then Alignment.None else alignment
                    ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, prec, 'f', zp), align))
            | FormatType.FloatExponential ->
                // `%+e`/`% e`/`%+E`: sign+zero-pad (`%+08e`) and star precision (`%+.*e`)
                // have no faithful form, so only a literal precision is accepted.
                if zeroPad then
                    ValueNone
                else
                    match constPrecOr 6 with
                    | ValueNone -> ValueNone
                    | ValueSome pr ->
                        let tc = if p.TypeChar = 'E' then 'E' else 'e'
                        ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, Prec.Const pr, tc, None), alignment))
            | FormatType.FloatCompact ->
                if zeroPad then
                    ValueNone
                else
                    match constPrecOr 6 with
                    | ValueNone -> ValueNone
                    | ValueSome pr ->
                        let tc = if p.TypeChar = 'G' then 'G' else 'g'
                        ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, Prec.Const pr, tc, None), alignment))
            | _ -> ValueNone
        elif leftAlign && zeroPad && isFloatLike then
            // Left-align + zero-pad on a float zero-pads on the RIGHT (`%-05.2f` 3.14159 ⇒
            // `"3.140"`). Only fixed-point has a faithful right-zero-pad form. `widthIsStar
            // && zeroPad` is declined above, so `width` is a literal here.
            match p.Type with
            | FormatType.FloatDecimal when not precIsStar ->
                let prec =
                    match p.Precision with
                    | FormatDim.Literal pr -> int pr
                    | FormatDim.Absent
                    | FormatDim.Star -> 6

                ValueSome(HoleForm.Field(FieldFormat.FixedRightZeroPad(prec, width.Value), Alignment.None))
            | _ -> ValueNone
        else
            // Left-align wins over zero-pad for the non-float forms (`%-05d ≡ %-5d`): drop
            // the zero-pad and let the negative `alignment` pad with spaces on the right.
            let zeroPad = zeroPad && not leftAlign

            let alignment: Alignment =
                if zeroPad then
                    Alignment.None
                elif widthIsStar then
                    Alignment.Star leftAlign
                else
                    match width with
                    | Some w -> Alignment.Const(if leftAlign then -w else w)
                    | None -> Alignment.None

            let field f = ValueSome(HoleForm.Field(f, alignment))
            // Reached only when `zeroPad` ⇒ `width` guaranteed present.
            let zpWidth () = width.Value
            // The non-float forms have no runtime-precision slot, so a star precision reaches
            // them only as an argument with no consumer. Defer it.
            let deferIfStarPrec r = if precIsStar then ValueNone else r

            match p.Type with
            | FormatType.String
            | FormatType.Object
            | FormatType.Char ->
                if zeroPad then
                    ValueNone
                else
                    deferIfStarPrec (field FieldFormat.Verbatim)
            | FormatType.DecimalInt ->
                deferIfStarPrec (
                    if zeroPad then
                        field (FieldFormat.DecimalZeroPad(zpWidth ()))
                    else
                        field FieldFormat.Verbatim
                )
            | FormatType.UnsignedHex ->
                deferIfStarPrec (
                    field (
                        FieldFormat.IntRadix(Radix.Hex(p.TypeChar = 'X'), (if zeroPad then Some(zpWidth ()) else None))
                    )
                )
            | FormatType.UnsignedBinary ->
                deferIfStarPrec (
                    field (FieldFormat.IntRadix(Radix.Binary, (if zeroPad then Some(zpWidth ()) else None)))
                )
            | FormatType.FloatDecimal ->
                if zeroPad then
                    // `FixedZeroPad` holds a static precision only, so `%0w.*f` is deferred.
                    if precIsStar then
                        ValueNone
                    else
                        let prec =
                            match p.Precision with
                            | FormatDim.Literal pr -> int pr
                            | FormatDim.Absent
                            | FormatDim.Star -> 6

                        ValueSome(HoleForm.Field(FieldFormat.FixedZeroPad(prec, zpWidth ()), Alignment.None))
                else
                    field (FieldFormat.Fixed(precDim 6))
            | FormatType.FloatExponential ->
                if zeroPad then
                    // `%08e`/`%014e`: zero-pad after any sign, static precision only, so a
                    // star precision (`%08.*e`) is deferred.
                    if precIsStar then
                        ValueNone
                    else
                        let prec =
                            match p.Precision with
                            | FormatDim.Literal pr -> int pr
                            | FormatDim.Absent
                            | FormatDim.Star -> 6

                        let tc = if p.TypeChar = 'E' then 'E' else 'e'
                        ValueSome(HoleForm.Field(FieldFormat.ExpCompactZeroPad(prec, zpWidth (), tc), Alignment.None))
                else
                    field (FieldFormat.Exponential(precDim 6, p.TypeChar = 'E'))
            | FormatType.FloatCompact ->
                if zeroPad then
                    if precIsStar then
                        ValueNone
                    else
                        let prec =
                            match p.Precision with
                            | FormatDim.Literal pr -> int pr
                            | FormatDim.Absent
                            | FormatDim.Star -> 6

                        let tc = if p.TypeChar = 'G' then 'G' else 'g'
                        ValueSome(HoleForm.Field(FieldFormat.ExpCompactZeroPad(prec, zpWidth (), tc), Alignment.None))
                else
                    field (FieldFormat.Compact(precDim 6, p.TypeChar = 'G'))
            | FormatType.UnsignedDecimalInt ->
                deferIfStarPrec (field (FieldFormat.Unsigned(if zeroPad then Some(zpWidth ()) else None)))
            | FormatType.UnsignedOctal ->
                deferIfStarPrec (
                    field (FieldFormat.IntRadix(Radix.Octal, (if zeroPad then Some(zpWidth ()) else None)))
                )
            | FormatType.Bool ->
                if zeroPad then
                    ValueNone
                else
                    deferIfStarPrec (field FieldFormat.Bool)
            | FormatType.Decimal ->
                // F# silently IGNORES a `%M` precision (`%.2M` 3.14159m ⇒ `"3.14159"`), so a
                // literal / absent precision is inert. A star precision still consumes a
                // runtime arg with no consumer, and zero-pad has no faithful mapping.
                if zeroPad || precIsStar then
                    ValueNone
                else
                    field FieldFormat.Verbatim
            | FormatType.Structured -> ValueNone
            // Whether the hole actually lowers is the gate's call, because only it sees the
            // provider-resolved sink type.
            | FormatType.FormatFunction -> ValueSome(HoleForm.Callback true)
            | FormatType.Text -> ValueSome(HoleForm.Callback false)

    /// What the lowering does with one placeholder.
    [<RequireQualifiedAccess>]
    type HoleVerdict =
        | Lowerable of HoleForm
        /// No backend renders the specifier faithfully; the generic printf call shape is kept.
        | Residual
        /// A forced sign (`+` or space) combined with both `-` and `0` (`%+-08.2f`): F#
        /// renders the sign then right-zero-pads, and this compiler rejects the specifier.
        | SignLeftAlignZeroPad

    let classify (p: FormatPlaceholder) : HoleVerdict =
        let has (c: char) = p.Flags.IndexOf c >= 0

        if (has '+' || has ' ') && has '-' && has '0' then
            HoleVerdict.SignLeftAlignZeroPad
        else
            match tryLowerableForm p with
            | ValueSome form -> HoleVerdict.Lowerable form
            | ValueNone -> HoleVerdict.Residual
