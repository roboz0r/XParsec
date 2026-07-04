namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

/// Target-neutral *semantic* model of a printf format hole — the single
/// classification of a `FormatPlaceholder` every consumer shares: the Freeze
/// lowering gate (does this hole lower to a structured `Format` node, or stay a
/// generic printf call?) and both codegen backends.
///
/// WHY it lives in `SemanticAnalysis` (not a codegen assembly): the decision
/// `tryClassify` makes — *is this specifier faithfully renderable as a structured
/// format?* — is a statement about printf *meaning*, not about any one target.
/// Both backends defer the same specifiers (`%a`/`%t`, the `%A` space flag, …), so
/// the gate is shared, and `FreezeExpr` (upstream of every codegen assembly) must
/// be able to consult it. The one genuinely target-specific projection — back to a
/// *.NET format string* (`"x8"`, `"F2"`, `"+0;-0"`) — is a CLR dialect and lives
/// only in `Codegen.Clr` (`ClrHoleFormat.toDotNetFormat`); no other backend ever
/// reconstructs one.
///
/// `HoleForm` carries the per-hole semantics directly. The old
/// `PrintfSpec.tryHoleFormat` baked them into a .NET format string — a CLR-runtime
/// artifact (the literal argument handed to `Formatter.AppendFormatted<T>(v,
/// format)`) — forcing the JS backend to *re-parse* that string (`match fmt.[0]`,
/// section-string surgery, recovering precision by counting characters after the
/// `.`) to recover semantics the `FormatPlaceholder` already held. The JS backend
/// now reads `FieldFormat` / `HoleForm` directly and never sees a .NET format
/// string.
module PrintfHoleForm =

    /// `%A` (`Structured`) print-*width* budget intent. F# repurposes the
    /// field-alignment slot for it; modelled explicitly here so no consumer reads
    /// an `Alignment` field that secretly means "width". `Default` ⇒ 80 at emit,
    /// `Never` ⇒ `%0A` (flat, never break), `Cols n` ⇒ `%NA` (break at column n).
    [<RequireQualifiedAccess>]
    type PrintWidth =
        | Default
        | Never
        | Cols of int
        /// `%*A` — the column budget is a runtime argument (the leading star `int`),
        /// so there is no static column count. The star-width lowering supplies the
        /// clamped runtime value (negative renders flat; F# does not throw for `%A`).
        | Star

    /// `%A` (`Structured`) print-*size* budget (F#'s `PrintSize` node count).
    /// `Default` ⇒ 10000 at emit, `Cols n` ⇒ `%.NA`, `Star` ⇒ `%.*A` (the size is a
    /// runtime argument — the star `int` following any width). Mirrors `PrintWidth`
    /// so no consumer reads a bare `int option` that secretly means "size".
    [<RequireQualifiedAccess>]
    type PrintSize =
        | Default
        | Cols of int
        | Star

    /// A precision dimension for the float field forms. `Const` is a static digit
    /// count (the `%.Nf` / default). `Star` (`%.*f`, `%*.*f`) defers it to a runtime
    /// argument — the `int` following any star width — mirroring `Alignment`; the
    /// precision *expression* rides on the segment's `DynHole` case, constructed from
    /// the same placeholder, so the two agree by construction.
    [<RequireQualifiedAccess>]
    type Prec =
        | Const of int
        | Star

    /// The base / case of an integer rendered in a non-decimal radix.
    [<RequireQualifiedAccess>]
    type Radix =
        /// `%x` / `%X` — the `upper` flag is the digit case (`'a'` vs `'A'`).
        | Hex of upper: bool
        /// `%B` — .NET 8 binary; two's-complement for negatives.
        | Binary
        /// `%o` — `Convert.ToString(v, 8)`; two's-complement for negatives.
        | Octal

    /// A non-`%A` hole's per-value formatting, semantically. The CLR backend maps
    /// this back to its `Formatter` member + format string
    /// (`ClrHoleFormat.toDotNetFormat`); the JS backend emits a JS expression
    /// straight from these fields.
    [<RequireQualifiedAccess>]
    type FieldFormat =
        /// `%s` `%O` `%c` `%M` `%d`/`%i` (no zero-pad): stringify verbatim.
        | Verbatim
        /// `%0wd`: signed decimal integer, zeros inserted *after any sign* to a
        /// total field of `width` (`(-42)` ⇒ `"-00042"`).
        | DecimalZeroPad of width: int
        /// `%x`/`%X`/`%B`/`%o`: unsigned integer in `radix`. `zeroPad = Some w`
        /// zero-pads to a total field of `w` (`%o` never zero-pads — always `None`).
        | IntRadix of radix: Radix * zeroPad: int option
        /// `%u`: the source `int`'s bits reinterpreted unsigned.
        | Unsigned
        /// `%b`: lowercase `true` / `false`.
        | Bool
        /// `%f` / `%.Nf` / `%.*f`: fixed-point with `precision` fraction digits
        /// (`Star` ⇒ runtime precision).
        | Fixed of precision: Prec
        /// `%0w.Nf`: fixed-point, then zeros after any sign to a total field of
        /// `width`. (.NET has no float format that zero-pads to a total width.)
        /// Precision stays a static `int` — zero-pad star stays cold, so no `Star`.
        | FixedZeroPad of precision: int * width: int
        /// `%e` / `%E` / `%.*e`: scientific, `precision` fraction digits (`Star` ⇒
        /// runtime), `upper` exponent case. CLR is byte-exact; JS approximates via
        /// `toExponential` (minimal exponent width, not .NET's 3-digit zero-pad).
        | Exponential of precision: Prec * upper: bool
        /// `%g` / `%G` / `%.*g`: compact, `precision` significant digits (`Star` ⇒
        /// runtime), `upper` case. CLR is byte-exact; JS approximates via
        /// `toPrecision` (keeps trailing zeros, different exponential threshold).
        | Compact of precision: Prec * upper: bool
        /// `%+d`/`% d`/`%+.Nf`/`% .Nf`/`%+.*f`: forced-sign. `space` ⇒ a leading space
        /// on a non-negative value (else a `+`); `precision` is the fraction digits
        /// (`Const 0` ⇒ integer / `%+.0f`, `Star` ⇒ runtime). A negative value keeps
        /// its `-`.
        | ForcedSign of space: bool * precision: Prec

    /// A `Field` hole's alignment-slot intent. `Const` keeps today's signed
    /// convention (negative ⇒ left-justify). `Star` (`%*d`, `%-*d`, `%+*d`) defers
    /// the width to a runtime argument, carrying only the `-`-flag left-justify
    /// decision; the width *expression* rides on the segment's `StarWidthHole` case,
    /// constructed from the same placeholder — so the two agree by construction.
    [<RequireQualifiedAccess>]
    type Alignment =
        | None
        | Const of int
        | Star of leftJustify: bool

    /// A classified format hole. `Field` carries the field alignment (the `%5d` /
    /// `%-5d` width; negative ⇒ left-justify) alongside the formatting — the
    /// zero-pad forms set it `None` (their width rides inside the `FieldFormat`).
    [<RequireQualifiedAccess>]
    type HoleForm =
        | PercentA of width: PrintWidth * size: PrintSize
        | Field of fmt: FieldFormat * alignment: Alignment

    /// Resolve a *static* `%A` width budget to the concrete column count the engines
    /// take (the 80-column default lives here, not duplicated across backends).
    /// `Star` (`%*A`) has no static budget — the width arrives as a runtime argument
    /// — so it returns `ValueNone`; the star-width lowering supplies the (clamped)
    /// runtime value in its place, keeping this function total for every consumer.
    let percentAWidth (w: PrintWidth) : int voption =
        match w with
        | PrintWidth.Default -> ValueSome 80
        | PrintWidth.Never -> ValueSome 0
        | PrintWidth.Cols n -> ValueSome n
        | PrintWidth.Star -> ValueNone

    /// Resolve a *static* `%A` size budget (F#'s `PrintSize` node count) to the
    /// concrete count (10000-node default). `Star` (`%.*A`) has no static budget — the
    /// size arrives as a runtime argument — so it returns `ValueNone`; the
    /// star-precision lowering supplies the runtime value in its place, keeping this
    /// function total for every consumer (mirroring `percentAWidth`).
    let percentASize (size: PrintSize) : int voption =
        match size with
        | PrintSize.Default -> ValueSome 10000
        | PrintSize.Cols n -> ValueSome n
        | PrintSize.Star -> ValueNone

    /// Classify a placeholder into its target-neutral `HoleForm`, or `ValueNone`
    /// for a specifier no backend renders faithfully (the lowering gate — the
    /// caller then keeps the generic printf call shape, which the CLR backend
    /// lowers via the FSharp.Core reflective path). Parity with F# `printf` under
    /// `InvariantCulture` is the gate for each accepted specifier.
    let tryClassify (p: FormatPlaceholder) : HoleForm voption =
        // Star *precision* (`%.*f`, `%*.*f`, `%.*e`, `%.*g`, `%+.*f`, `%.*A`) consumes
        // an extra runtime argument. It is admitted below only where a float field form
        // (`Prec.Star`) or `%A` (`PrintSize.Star`) has a runtime slot to hold it; every
        // other type defers it (`deferIfStarPrec` / explicit `precIsStar` gates), since
        // its accepted forms carry no precision. Star *width* is likewise admitted only
        // where a literal width lands in the alignment slot (not zero-pad, where the
        // width rides inside `FieldFormat`), and for `%A` bare-or-star-width.
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

        // `-` and `0` are inert without a width: there is nothing to justify or
        // pad, so `%-d ≡ %d`, `%0d ≡ %d`, `%-.2f ≡ %.2f`. Dropping them here lets
        // the per-type match treat the specifier as its plain form (and keeps every
        // `zeroPad` branch's `width.Value` read safe — `zeroPad` now implies a width).
        let leftAlign = leftAlign && hasWidth
        let zeroPad = zeroPad && hasWidth

        // Float/decimal forms zero-pad on the *right* under left-align (`%-05.2f`
        // 3.14159 ⇒ `"3.140"`), an F#-specific subtlety with no faithful structured
        // mapping — those `leftAlign && zeroPad` cases stay deferred below.
        let isFloatLike =
            match p.Type with
            | FormatType.FloatDecimal
            | FormatType.FloatExponential
            | FormatType.FloatCompact
            | FormatType.Decimal -> true
            | _ -> false

        // The field alignment shared by every non-zero-pad form: a static signed
        // width (`-` ⇒ left-justify), or a runtime `Star` carrying only the
        // `-`-flag decision; zero-pad is mutually exclusive with it.
        let alignment: Alignment =
            if zeroPad then
                Alignment.None
            elif widthIsStar then
                Alignment.Star leftAlign
            else
                match width with
                | Some w -> Alignment.Const(if leftAlign then -w else w)
                | None -> Alignment.None

        // The precision dimension for a float field form: `Star` (`%.*f`), a literal
        // count, or the type's default. Only reached in the float arms — every other
        // type defers a star precision first, so `Star` never lands where it has no slot.
        let precDim (dflt: int) : Prec =
            match p.Precision with
            | FormatDim.Star -> Prec.Star
            | FormatDim.Literal pr -> Prec.Const(int pr)
            | FormatDim.Absent -> Prec.Const dflt

        // The `%A` size budget (`PrintSize` node count): `Star` (`%.*A`), a literal
        // `%.NA`, or the default.
        let sizeDim: PrintSize =
            match p.Precision with
            | FormatDim.Star -> PrintSize.Star
            | FormatDim.Literal pr -> PrintSize.Cols(int pr)
            | FormatDim.Absent -> PrintSize.Default

        if p.Type = FormatType.Structured then
            // `%A`: the structural engine renders. `0` flag forces flat (width 0),
            // taking precedence over an explicit width (F# `printf.fs:947`). The
            // sign flags `+`/`-`/` ` are all no-ops here: `GenericToString`
            // (`printf.fs:1085`) consults only plus, zero-pad, width, and
            // precision — never the space flag — so `% A` renders identically to
            // `%A`. A bare `%*A` (or `%*.*A`) takes the runtime column budget
            // (`PrintWidth.Star`); a *flagged* star form's flag-vs-star layout is
            // unverified, so it stays cold. The size budget carries a star (`%.*A` /
            // `%*.*A`) with no clamp — F#'s `%A` sets `PrintSize` raw (`printf.fs:1114`).
            if widthIsStar then
                if zeroPad || leftAlign || plusSign then
                    ValueNone
                else
                    ValueSome(HoleForm.PercentA(PrintWidth.Star, sizeDim))
            else
                let pw =
                    // Raw `0` flag: `%0A`/`%05A` force flat regardless of width, so this
                    // reads the un-normalized flag (normalization drops a width-less `0`).
                    if has '0' then
                        PrintWidth.Never
                    else
                        match width with
                        | Some n -> PrintWidth.Cols n
                        | None -> PrintWidth.Default

                ValueSome(HoleForm.PercentA(pw, sizeDim))
        elif widthIsStar && zeroPad then
            // `%0*d`: a star zero-pad width rides *inside* `FieldFormat` (no
            // compile-time value to embed) — stays cold.
            ValueNone
        elif plusSign || spaceSign then
            // Forced-sign: only the signed decimal-integer and fixed-point-float
            // forms section-format faithfully; sign+zero-pad stays cold.
            if zeroPad then
                ValueNone
            else
                // `+` wins over space when both flags are present (legacy
                // `if plusSign then "+" else " "`); this arm runs only when
                // `plusSign || spaceSign`, so `not plusSign` ⇒ render a space.
                let space = not plusSign

                match p.Type with
                | FormatType.DecimalInt ->
                    // Integer forced sign has no precision slot; a star precision
                    // (`%+.*d`) has no consumer, so defer it.
                    if precIsStar then
                        ValueNone
                    else
                        ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, Prec.Const 0), alignment))
                | FormatType.FloatDecimal ->
                    let prec =
                        match p.Precision with
                        | FormatDim.Star -> Prec.Star
                        | FormatDim.Literal pr -> Prec.Const(let n = int pr in if n <= 0 then 0 else n)
                        | FormatDim.Absent -> Prec.Const 6

                    ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, prec), alignment))
                | _ -> ValueNone
        elif leftAlign && zeroPad && isFloatLike then
            // Left-align + zero-pad on a float/decimal zero-pads on the right — no
            // faithful structured mapping — defer.
            ValueNone
        else
            // Left-align wins over zero-pad for the non-float forms (`%-05d ≡ %-5d`):
            // drop the zero-pad and let the negative `alignment` pad with spaces on
            // the right. (`not leftAlign` ⇒ zero-pad already dropped above.)
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
            // The non-float forms have no runtime-precision slot; a star precision
            // reaches them only as an argument with no consumer, so defer it.
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
                    // `%0w.*f`: a star precision rides inside the static `FixedZeroPad`
                    // — zero-pad star stays cold, so defer it.
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
                    ValueNone
                else
                    field (FieldFormat.Exponential(precDim 6, p.TypeChar = 'E'))
            | FormatType.FloatCompact ->
                if zeroPad then
                    ValueNone
                else
                    field (FieldFormat.Compact(precDim 6, p.TypeChar = 'G'))
            | FormatType.UnsignedDecimalInt ->
                if zeroPad then
                    ValueNone
                else
                    deferIfStarPrec (field FieldFormat.Unsigned)
            | FormatType.UnsignedOctal ->
                if zeroPad then
                    ValueNone
                else
                    deferIfStarPrec (field (FieldFormat.IntRadix(Radix.Octal, None)))
            | FormatType.Bool ->
                if zeroPad then
                    ValueNone
                else
                    deferIfStarPrec (field FieldFormat.Bool)
            | FormatType.Decimal ->
                // F# `%M` precision semantics are unusual; zero-pad has no faithful
                // mapping — defer both.
                if zeroPad || (p.Precision <> FormatDim.Absent) then
                    ValueNone
                else
                    field FieldFormat.Verbatim
            | FormatType.Structured
            | FormatType.FormatFunction
            | FormatType.Text -> ValueNone
