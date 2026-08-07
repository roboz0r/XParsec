namespace Vesper

open System
open System.Buffers
open System.Globalization
open System.IO
open System.Text

open Vesper.IntComparison

[<Struct; IsByRefLike>]
type Formatter =

    /// Smallest pooled buffer rented, from the BCL handler. The ctors inline it — and
    /// the 11-chars-per-hole guess — as literals: a ctor's field-init block cannot read
    /// a `static let`.
    static let MinimumArrayPoolLength = 256

    /// Max array length the BCL handler clamps growth to (`string.MaxLength`).
    static let MaxChars = 0x3FFFFFDFu

    /// Culture for all hole formatting — Invariant, to match F# `printf`.
    static let provider: IFormatProvider = CultureInfo.InvariantCulture

    /// Write-through `TextWriter` sink, or `null` (a string / builder sink).
    val mutable private Writer: TextWriter

    /// Write-through `StringBuilder` sink (`bprintf`), or `null` (a string /
    /// writer sink). `Flush` appends the buffered text to it.
    val mutable private Builder: StringBuilder

    /// The pooled array backing `Chars`, returned on clear.
    val mutable private Pool: char[]

    /// The span to write into; always points at `Pool`.
    val mutable private Chars: Span<char>

    val mutable private Pos: int

    /// Write-through ctor: buffered text is flushed to `writer`.
    new(literalLength: int, formattedCount: int, writer: TextWriter) =
        let buf =
            ArrayPool<char>.Shared.Rent(Math.Max(256, literalLength + formattedCount * 11))

        {
            Writer = writer
            Builder = null
            Pool = buf
            Chars = Span<char>(buf)
            Pos = 0
        }

    /// Builder ctor (`bprintf`): buffered text is flushed to `builder`.
    new(literalLength: int, formattedCount: int, builder: StringBuilder) =
        let buf =
            ArrayPool<char>.Shared.Rent(Math.Max(256, literalLength + formattedCount * 11))

        {
            Writer = null
            Builder = builder
            Pool = buf
            Chars = Span<char>(buf)
            Pos = 0
        }

    /// String ctor: `ToStringAndClear` returns the accumulated text.
    new(literalLength: int, formattedCount: int) =
        let buf =
            ArrayPool<char>.Shared.Rent(Math.Max(256, literalLength + formattedCount * 11))

        {
            Writer = null
            Builder = null
            Pool = buf
            Chars = Span<char>(buf)
            Pos = 0
        }

    member this.AppendLiteral(value: string) =
        if value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos)) then
            this.Pos <- this.Pos + value.Length
        else
            this.GrowThenCopyString(value)

    /// A `null` value appends `""`, never the word `null`; a field width then pads that
    /// to all spaces.
    member this.AppendFormatted(value: 'T) =
        let o = box value

        let s =
            match o with
            | :? IFormattable as f -> f.ToString(null, provider)
            | null -> ""
            | _ -> o.ToString()

        this.AppendLiteral(s)

    member this.AppendFormatted(value: 'T, format: string) =
        let o = box value

        let s =
            match o with
            | :? IFormattable as f -> f.ToString(format, provider)
            | null -> ""
            | _ -> o.ToString()

        this.AppendLiteral(s)

    member this.AppendFormatted(value: 'T, alignment: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value)

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    member this.AppendFormatted(value: 'T, alignment: int, format: string) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format)

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// Dedicated because `bool.ToString()` gives `True` and F# `%b` gives `true`.
    member this.AppendBool(value: bool, alignment: int) =
        let startingPos = this.Pos
        this.AppendLiteral(if value then "true" else "false")

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// 32-bit two's-complement octal. Dedicated because .NET has no octal format string.
    member this.AppendOctal(value: int, alignment: int) =
        let startingPos = this.Pos
        this.AppendLiteral(Convert.ToString(value, 8))

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    member this.AppendUnsigned(value: uint, alignment: int) = this.AppendFormatted(value, alignment)

    /// Zero-pads the text written since `startingPos` to a total field of `width`,
    /// inserting the zeros AFTER any leading sign: `%+08.2f` of `-1.5` ⇒ `-0001.50`.
    /// Overflow (already ≥ `width`) is a no-op; F# never truncates a zero-pad field.
    member private this.ZeroPadAfterSign(startingPos: int, width: int) =
        let charsWritten = this.Pos - startingPos
        let paddingNeeded = width - charsWritten

        if paddingNeeded > 0 then
            let signOffset =
                if charsWritten > 0 then
                    match this.Chars.[startingPos] with
                    | '-'
                    | '+'
                    | ' ' -> 1
                    | _ -> 0
                else
                    0

            let insertAt = startingPos + signOffset

            this.EnsureCapacityForAdditionalChars(paddingNeeded)

            this.Chars
                .Slice(insertAt, this.Pos - insertAt)
                .CopyTo(this.Chars.Slice(insertAt + paddingNeeded, this.Chars.Length - (insertAt + paddingNeeded)))

            this.Chars.Slice(insertAt, paddingNeeded).Fill('0')
            this.Pos <- this.Pos + paddingNeeded

    /// Dedicated because no .NET float format zero-pads to a total width.
    member this.AppendZeroPaddedFloat(value: float, format: string, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format) // the "F<prec>" body, no padding
        this.ZeroPadAfterSign(startingPos, width)

    /// `%+0w.pf` / `% 0w.pf`: a forced `+` (or space) on a non-negative number, then a
    /// zero-pad after it. The section format that could do both (`"+0.00;-0.00"`) rounds
    /// half-away, where the `"F<prec>"` body rounds half-to-even.
    member this.AppendForcedSignZeroPaddedFloat(value: float, format: string, width: int, space: bool) =
        let startingPos = this.Pos

        let str =
            match box value with
            | :? IFormattable as f -> f.ToString(format, provider)
            | _ -> value.ToString()

        let isNumber = not (Double.IsNaN value) && not (Double.IsInfinity value)
        let isNegative = str.Length > 0 && str.[0] = '-'

        let prefixed =
            if isNumber && not isNegative then
                (if space then " " else "+") + str
            else
                str

        this.AppendLiteral(prefixed)
        this.ZeroPadAfterSign(startingPos, width)

    /// Dedicated because F#'s left-align + zero-pad fills the RIGHT with zeros
    /// (`%-08.2f` of `1.5` ⇒ `1.500000`), which no .NET format or alignment reproduces.
    member this.AppendRightZeroPaddedFloat(value: float, format: string, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format) // the "F<prec>" body, no padding
        let paddingNeeded = width - (this.Pos - startingPos)

        // Overflow (already ≥ width) is a no-op — F# never truncates a zero-pad field.
        if paddingNeeded > 0 then
            this.EnsureCapacityForAdditionalChars(paddingNeeded)
            this.Chars.Slice(this.Pos, paddingNeeded).Fill('0')
            this.Pos <- this.Pos + paddingNeeded

    /// `%o` output carries no sign, so the pad is a plain left-fill; digits past `width`
    /// are not truncated (`%08o` of `-1` ⇒ 11 digits).
    member this.AppendZeroPaddedOctal(value: int, width: int) =
        let startingPos = this.Pos
        this.AppendLiteral(Convert.ToString(value, 8))
        this.ZeroPadAfterSign(startingPos, width)

    /// `%u` output carries no sign, so the pad is a plain left-fill; digits past `width`
    /// are not truncated (`%05u` of `-1` ⇒ 10 digits).
    member this.AppendZeroPaddedUnsigned(value: uint, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value)
        this.ZeroPadAfterSign(startingPos, width)

    member this.AppendStructured(value: 'T, width: int, size: int) =
        this.AppendLiteral(StructuralPrinter.Print(value, width, size))

    /// The .NET format string is `typeChar.ToString() + precision.ToString()`, as
    /// FSharp.Core's `getFormatForFloat` (`printf.fs:606`), so a garbage precision falls
    /// to .NET's custom-format path byte-for-byte: `%.*f` with `-1` ⇒ `"f-1"`.
    member this.AppendDynamicPrecisionFloat(value: float, typeChar: char, precision: int, alignment: int) =
        this.AppendFormatted(value, alignment, typeChar.ToString() + precision.ToString())

    /// A forced-sign float (`%+.*f` / `% .*f` / `%+*.*f`): a non-negative number gets a
    /// leading `+` (or ` ` when `space`) before justification. The section format that
    /// would do it (`"+0.000;-0.000"`) cannot take a runtime precision.
    member this.AppendDynamicPrecisionSignedFloat
        (value: float, typeChar: char, precision: int, alignment: int, space: bool)
        =
        let fmt = typeChar.ToString() + precision.ToString()

        let str =
            match box value with
            | :? IFormattable as f -> f.ToString(fmt, provider)
            | _ -> value.ToString()

        // NaN / ±∞ take no sign prefix (FSharp.Core's `isNumber`, `printf.fs:966`), and a
        // negative value already carries its own `-` — read off the formatted text, since
        // the `>=` operator is int-only here.
        let isNumber = not (Double.IsNaN value) && not (Double.IsInfinity value)
        let isNegative = str.Length > 0 && str.[0] = '-'

        let prefixed =
            if isNumber && not isNegative then
                (if space then " " else "+") + str
            else
                str

        let startingPos = this.Pos
        this.AppendLiteral(prefixed)

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// The parity bar is the exception type + `ParamName` F#'s `PadLeft`/`PadRight`
    /// throw, not the message.
    static member GuardTotalWidth(totalWidth: int) : int =
        if totalWidth < 0 then
            raise (new ArgumentOutOfRangeException("totalWidth"))
        else
            totalWidth

    /// A negative `%A` width renders flat rather than throwing.
    static member ClampWidth(width: int) : int = if width < 0 then 0 else width

    /// F#'s `normalizePrecision` (`printf.fs:608`), reached only when a hole has BOTH
    /// star dimensions: the prec=*-only paths keep the raw precision (`:649-657`).
    static member NormalizePrecision(precision: int) : int =
        if precision < 0 then 0
        elif precision > 99 then 99
        else precision

    /// At most one sink is set; a string sink flushes nothing. Both paths go through
    /// `.ToString()` — F# has no implicit `Span` → `ReadOnlySpan` conversion, so the
    /// no-alloc `Write` / `Append(ReadOnlySpan<char>)` overloads are unreachable.
    member this.Flush() =
        match this.Writer with
        | null ->
            match this.Builder with
            | null -> ()
            | sb -> sb.Append(this.Chars.Slice(0, this.Pos).ToString()) |> ignore
        | w -> w.Write(this.Chars.Slice(0, this.Pos).ToString())

        this.Clear()

    member this.ToStringAndClear() : string =
        let result = this.Chars.Slice(0, this.Pos).ToString()
        this.Clear()
        result

    /// Returns the pooled buffer. The handler must not be used after this.
    member private this.Clear() =
        let toReturn = this.Pool

        // Defensive clear (mirrors the BCL handler; Writer is left as-is).
        this.Pool <- null
        this.Chars <- Span<char>()
        this.Pos <- 0

        match toReturn with
        | null -> ()
        | arr -> ArrayPool<char>.Shared.Return(arr)

    // ---- Alignment + buffer growth (mirrors DefaultInterpolatedStringHandler) ----

    member private this.AppendOrInsertAlignmentIfNeeded(startingPos: int, alignment: int) =
        let charsWritten = this.Pos - startingPos

        let mutable leftAlign = false
        let mutable width = alignment

        if width < 0 then
            leftAlign <- true
            width <- -width

        let paddingNeeded = width - charsWritten

        if paddingNeeded > 0 then
            this.EnsureCapacityForAdditionalChars(paddingNeeded)

            if leftAlign then
                this.Chars.Slice(this.Pos, paddingNeeded).Fill(' ')
            else
                this.Chars
                    .Slice(startingPos, charsWritten)
                    .CopyTo(
                        this.Chars.Slice(startingPos + paddingNeeded, this.Chars.Length - (startingPos + paddingNeeded))
                    )

                this.Chars.Slice(startingPos, paddingNeeded).Fill(' ')

            this.Pos <- this.Pos + paddingNeeded

    member private this.EnsureCapacityForAdditionalChars(additionalChars: int) =
        if this.Chars.Length - this.Pos < additionalChars then
            this.Grow(additionalChars)

    member private this.GrowThenCopyString(value: string) =
        this.Grow(value.Length)
        value.CopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos))
        this.Pos <- this.Pos + value.Length

    member private this.Grow(additionalChars: int) =
        this.GrowCore(uint this.Pos + uint additionalChars)

    member private this.GrowCore(requiredMinCapacity: uint) =
        // Max of the required capacity and a doubling, clamped below the max
        // array length and floored at the minimum pool rental — the BCL policy.
        let newCapacity =
            Math.Max(requiredMinCapacity, Math.Min(uint this.Chars.Length * 2u, MaxChars))

        let arraySize =
            int (Math.Clamp(newCapacity, uint MinimumArrayPoolLength, 2147483647u))

        let newArray = ArrayPool<char>.Shared.Rent(arraySize)
        this.Chars.Slice(0, this.Pos).CopyTo(Span<char>(newArray))

        let toReturn = this.Pool
        this.Pool <- newArray
        this.Chars <- Span<char>(newArray)

        match toReturn with
        | null -> ()
        | arr -> ArrayPool<char>.Shared.Return(arr)
