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
        /// zero-pads to a total field of `w` (F# zero-pads octal too, e.g. `%08o`).
        | IntRadix of radix: Radix * zeroPad: int option
        /// `%u`: the source `int`'s bits reinterpreted unsigned. `zeroPad = Some w`
        /// zero-pads to a total field of `w` (`%05u`; overflowing digits are not truncated).
        | Unsigned of zeroPad: int option
        /// `%b`: lowercase `true` / `false`.
        | Bool
        /// `%f` / `%.Nf` / `%.*f`: fixed-point with `precision` fraction digits
        /// (`Star` ⇒ runtime precision).
        | Fixed of precision: Prec
        /// `%0w.Nf`: fixed-point, then zeros after any sign to a total field of
        /// `width`. (.NET has no float format that zero-pads to a total width.)
        /// Precision stays a static `int` — zero-pad star stays cold, so no `Star`.
        | FixedZeroPad of precision: int * width: int
        /// `%-0w.Nf`: fixed-point, then zeros on the RIGHT (after the digits, past the
        /// point) to a total field of `width` — F#'s left-align + zero-pad on a float
        /// (`%-05.2f` 3.14159 ⇒ `"3.140"`). No .NET format nor space-alignment does
        /// this; overflow (already ≥ `width`) is a no-op, like every zero-pad form.
        | FixedRightZeroPad of precision: int * width: int
        /// `%e` / `%E` / `%.*e`: scientific, `precision` fraction digits (`Star` ⇒
        /// runtime), `upper` exponent case. CLR is byte-exact; JS approximates via
        /// `toExponential` (minimal exponent width, not .NET's 3-digit zero-pad).
        | Exponential of precision: Prec * upper: bool
        /// `%g` / `%G` / `%.*g`: compact, `precision` significant digits (`Star` ⇒
        /// runtime), `upper` case. CLR is byte-exact; JS approximates via
        /// `toPrecision` (keeps trailing zeros, different exponential threshold).
        | Compact of precision: Prec * upper: bool
        /// `%0we`/`%0wE`/`%0wg`/`%0wG`: scientific / compact, then zeros after any sign
        /// to a total field of `width` (`%014e` 1234.5 ⇒ `"01.234500e+003"`). `typeChar`
        /// is the source letter (`'e'`/`'E'`/`'g'`/`'G'`); `precision` its static digit
        /// count. Reuses the `%0w.Nf` zero-pad-after-sign handler over the `"e6"`/`"g6"`
        /// body — .NET zero-pads no float to a total width. CLR is byte-exact; JS
        /// inherits the `toExponential`/`toPrecision` approximation.
        | ExpCompactZeroPad of precision: int * width: int * typeChar: char
        /// `%+d`/`% d`/`%+05d`/`%+.Nf`/`% .Nf`/`%+.*f`/`%+e`/`%+g`: forced-sign. `space`
        /// ⇒ a leading space on a non-negative value (else a `+`); a negative keeps its
        /// `-`. `precision` is the fraction / significant digits (`Const 0` ⇒ integer
        /// `%+.0f`, `Star` ⇒ runtime `%+.*f`). `typeChar` is the source letter
        /// (`'d'`/`'f'`/`'e'`/`'E'`/`'g'`/`'G'`): the section-format-expressible fixed
        /// forms (`'d'`/`'f'`) lower to a .NET section format; the scientific / compact
        /// forms (`'e'`…`'G'`) and any runtime precision route to the signed dynamic
        /// handler instead (no section format can express them). `zeroPad = Some w`
        /// zero-pads *through* the sign to a total field of `w` (`%+05d`; the section
        /// format's digit count is `w-1`) — only the fixed integer form uses it.
        | ForcedSign of space: bool * precision: Prec * typeChar: char * zeroPad: int option

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
        /// `%a` (`hasValue = true`) / `%t` (`hasValue = false`) — a printer callback
        /// hole. `%a` consumes a callback `('State -> 'T -> 'Residue)` AND a value
        /// `'T`; `%t` just `('State -> 'Residue)`. Classified purely syntactically
        /// (no flags/width/precision — F# `%a`/`%t` carry none); whether it actually
        /// *lowers* on a given target is decided at the gate by whether the family's
        /// `'State` sink type is available on that target's provider.
        | Callback of hasValue: bool

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

    /// The runtime clamp F# applies to a form's *star* width (`%*d` / `%*A`).
    /// `Guard` (every padding `Field` form) throws `ArgumentOutOfRangeException` on a
    /// negative width; `Clamp` (`%*A`) renders a negative budget flat (→ 0) instead.
    [<RequireQualifiedAccess>]
    type StarWidthClamp =
        | Guard
        | Clamp

    /// Which runtime clamp a form's star width takes, or `ValueNone` when the form
    /// carries no star width. The single owner of the guard-vs-clamp rule: both
    /// backends dispatch on this instead of re-matching the classified form (so the
    /// two can't drift, and neither carries a `_ -> invariant broken` catch-all).
    let starWidthClamp (form: HoleForm) : StarWidthClamp voption =
        match form with
        | HoleForm.Field(_, Alignment.Star _) -> ValueSome StarWidthClamp.Guard
        | HoleForm.PercentA(PrintWidth.Star, _) -> ValueSome StarWidthClamp.Clamp
        | _ -> ValueNone

    /// True iff a float `Field` carries a *runtime* (`Prec.Star`) precision
    /// (`%.*f`/`%.*e`/`%.*g`/`%+.*f`) — routed through the dynamic-precision handler
    /// rather than a static .NET format string / `toFixed` literal.
    let isDynamicPrecisionFloat (fmt: FieldFormat) : bool =
        match fmt with
        | FieldFormat.Fixed Prec.Star
        | FieldFormat.Exponential(Prec.Star, _)
        | FieldFormat.Compact(Prec.Star, _)
        | FieldFormat.ForcedSign(_, Prec.Star, _, _) -> true
        | _ -> false

    /// True iff F# clamps a runtime star precision to `0..99` (`normalizePrecision`)
    /// on this form: ONLY the two-star float-field path (`printf.fs:632`). The
    /// prec-star-only `Field` paths and `%.*A` keep the raw precision (`:649-657`,
    /// `:1114`), so a static width (bare `Alignment`) or `%A` returns `false`. The
    /// single owner of the normalize rule — consumed by both backends.
    let normalizesStarPrecision (form: HoleForm) : bool =
        match form with
        | HoleForm.Field(fmt, Alignment.Star _) -> isDynamicPrecisionFloat fmt
        | _ -> false

    /// Reconstruct a placeholder's source text (`"%0*d"`, `"%+08.2f"`, `"%-*A"`)
    /// for diagnostics. Not a round-trip of the raw token (flag order/duplication
    /// is canonicalised by the lexer), but faithful enough to name the offending
    /// specifier in an error message.
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
            // (`PrintWidth.Star`); `%-*A` / `%+*A` render byte-identically (the
            // `-`/`+` flags are pure no-ops for `%A`, verified against F#), so they
            // take the same `PrintWidth.Star`. `%0*A` is declined: F#'s `0` flag
            // forces flat AND discards the runtime column budget — an unintended
            // quirk of F#'s format parsing, not worth reproducing — so it stays a
            // diagnosed residual (the gate re-errors it). The size budget carries a
            // star (`%.*A` / `%*.*A`) with no clamp — F#'s `%A` sets `PrintSize` raw
            // (`printf.fs:1114`).
            if widthIsStar then
                if zeroPad then
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
            // Forced-sign. The signed decimal-integer (incl. `%+05d` zero-pad through
            // the sign) and fixed-point-float forms section-format faithfully; the
            // scientific / compact forms route to the signed dynamic handler; every
            // other sign+zero-pad combination stays cold.
            //
            // `+` wins over space when both flags are present (legacy
            // `if plusSign then "+" else " "`); this arm runs only when
            // `plusSign || spaceSign`, so `not plusSign` ⇒ render a space.
            let space = not plusSign

            // A literal precision as a static digit count (default `dflt`), or `ValueNone`
            // for a star precision (no consumer on these forms — defer it).
            let constPrecOr (dflt: int) : int voption =
                match p.Precision with
                | FormatDim.Star -> ValueNone
                | FormatDim.Literal pr -> ValueSome(int pr)
                | FormatDim.Absent -> ValueSome dflt

            match p.Type with
            | FormatType.DecimalInt ->
                // Integer forced sign has no precision slot; a star precision
                // (`%+.*d`) has no consumer, so defer it. `%+05d` zero-pads through the
                // sign — the width rides inside the `FieldFormat` (`Alignment.None`); a
                // star zero-pad width (`%+0*d`) is already declined above.
                if precIsStar then
                    ValueNone
                else
                    let zp = if zeroPad then Some width.Value else None
                    let align = if zeroPad then Alignment.None else alignment
                    ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, Prec.Const 0, 'd', zp), align))
            | FormatType.FloatDecimal ->
                // `%+08.2f` (sign + zero-pad float) has no faithful section format — cold.
                if zeroPad then
                    ValueNone
                else
                    let prec =
                        match p.Precision with
                        | FormatDim.Star -> Prec.Star
                        | FormatDim.Literal pr -> Prec.Const(let n = int pr in if n <= 0 then 0 else n)
                        | FormatDim.Absent -> Prec.Const 6

                    ValueSome(HoleForm.Field(FieldFormat.ForcedSign(space, prec, 'f', None), alignment))
            | FormatType.FloatExponential ->
                // `%+e`/`% e`/`%+E`: scientific notation can't ride a .NET section
                // format, so a *literal* precision routes to the signed dynamic handler.
                // Sign+zero-pad (`%+08e`) and star precision (`%+.*e`) stay cold.
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
            // Left-align + zero-pad on a float zero-pads on the RIGHT (`%-05.2f`
            // 3.14159 ⇒ `"3.140"`). Only fixed-point (`FloatDecimal`) has a faithful
            // right-zero-pad handler; the scientific / compact / decimal variants keep
            // deferring. A star precision (`%-0*.*f`) has no static width to embed —
            // `widthIsStar && zeroPad` is declined above, so `width` is a literal here.
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
                    // `%08e`/`%014e`: zero-pad after any sign over the `"e6"` body — the
                    // static-precision zero-pad reuses the `%0w.Nf` handler; a star
                    // precision rides no static body, so defer it.
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
                // F# silently IGNORES a `%M` precision (`%.2M` 3.14159m ⇒ `"3.14159"`),
                // so a literal / absent precision is inert — the plain `Verbatim` form.
                // A star precision (`%.*M`) still consumes a runtime arg with no
                // consumer, and zero-pad has no faithful mapping — defer both.
                if zeroPad || precIsStar then
                    ValueNone
                else
                    field FieldFormat.Verbatim
            | FormatType.Structured -> ValueNone
            // `%a` / `%t` callback holes: classified syntactically. `%a`
            // (`FormatFunction`) carries a value arg, `%t` (`Text`) does not. The gate
            // (which holds the provider) decides whether the family's sink type is
            // available on this target and so whether the hole actually lowers.
            | FormatType.FormatFunction -> ValueSome(HoleForm.Callback true)
            | FormatType.Text -> ValueSome(HoleForm.Callback false)
