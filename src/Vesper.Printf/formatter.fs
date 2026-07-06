namespace Vesper

open System
open System.Buffers
open System.Globalization
open System.IO
open System.Text

open Vesper.IntComparison

// Vesper-compiled implementation of `formatter.fsi` — the printf write-through
// handler. Two spots take a text-equivalent path rather than the BCL no-alloc
// fast path (same bytes out, no `Span` fast-path):
//   * `AppendFormatted` uses the `IFormattable.ToString(format, provider)` path
//     rather than the no-alloc `ISpanFormattable.TryFormat` span fast-path; same
//     text output (same culture, same format string).
//   * `Flush` uses `TextWriter.Write(string)` rather than `Write(ReadOnlySpan<char>)`
//     — F# has no implicit `Span<char>` → `ReadOnlySpan<char>` conversion.

/// Stack-only handler that accumulates formatted text and flushes it to a sink.
/// Constructed and driven by the backend; users never name it.
[<Struct; IsByRefLike>]
type Formatter =

    /// Smallest pooled buffer rented; inherited from the BCL handler. (The
    /// per-hole reservation `GuessedLengthPerHole = 11` and this floor are
    /// inlined as literals in the ctors — a secondary ctor's field-init block
    /// can't read a `static let` binding.)
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

    /// Position at which to write the next character.
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

    /// Writes the specified literal chunk to the handler.
    member this.AppendLiteral(value: string) =
        if value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos)) then
            this.Pos <- this.Pos + value.Length
        else
            this.GrowThenCopyString(value)

    /// Writes the specified value to the handler. (C#'s null-reference `s` skip
    /// becomes `AppendLiteral ""` — a no-op, byte-identical — so the hole's
    /// result string is always grounded, never a bare `null` value.)
    member this.AppendFormatted(value: 'T) =
        let o = box value

        let s =
            match o with
            | :? IFormattable as f -> f.ToString(null, provider)
            | null -> ""
            | _ -> o.ToString()

        this.AppendLiteral(s)

    /// Writes the specified value to the handler, with a .NET format specifier.
    member this.AppendFormatted(value: 'T, format: string) =
        let o = box value

        let s =
            match o with
            | :? IFormattable as f -> f.ToString(format, provider)
            | null -> ""
            | _ -> o.ToString()

        this.AppendLiteral(s)

    /// Writes the specified value, right-justified (or, if `alignment` is
    /// negative, left-justified) in a field of `alignment` chars.
    member this.AppendFormatted(value: 'T, alignment: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value)

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// Writes the specified value, with both an alignment and a format.
    member this.AppendFormatted(value: 'T, alignment: int, format: string) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format)

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// Writes `true`/`false` (lowercase) for an F# `%b` hole, justified in a
    /// field of `alignment` chars. Dedicated because `bool.ToString()`
    /// capitalises and F# `%b` is lowercase.
    member this.AppendBool(value: bool, alignment: int) =
        let startingPos = this.Pos
        this.AppendLiteral(if value then "true" else "false")

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// Writes `value` as 32-bit two's-complement octal for an F# `%o` hole.
    /// Dedicated because .NET has no octal format string.
    member this.AppendOctal(value: int, alignment: int) =
        let startingPos = this.Pos
        this.AppendLiteral(Convert.ToString(value, 8))

        if alignment <> 0 then
            this.AppendOrInsertAlignmentIfNeeded(startingPos, alignment)

    /// Writes `value` as unsigned decimal for an F# `%u` hole. The backend
    /// passes the source `int`'s bits unchanged (the CLI stack treats
    /// `int32`/`uint32` alike), reproducing F# `%u`'s reinterpretation.
    member this.AppendUnsigned(value: uint, alignment: int) = this.AppendFormatted(value, alignment)

    /// Zero-pads the text written since `startingPos` — inserting zeros *after any
    /// leading sign* — to a total field of `width` chars. Shared by the zero-pad
    /// float / octal / unsigned handlers. Overflow (already ≥ `width`) is a no-op:
    /// F# never truncates a zero-pad field.
    member private this.ZeroPadAfterSign(startingPos: int, width: int) =
        let charsWritten = this.Pos - startingPos
        let paddingNeeded = width - charsWritten

        if paddingNeeded > 0 then
            // The sign (if any) stays at the field's left edge; the zeros fill
            // the gap between it and the digits. A `-` from the formatted magnitude,
            // or a forced `+` / space sign composed by `AppendForcedSignZeroPaddedFloat`
            // (the unsigned / octal callers never produce a leading `+`/space).
            // NB: written as `||` equality rather than a char OR-pattern
            // (`| '-' | '+' | ' ' -> …`) because the self-host compiler mis-lowers that
            // OR-pattern (it fell through to the `_` arm) — keep this boolean form.
            let lead = if charsWritten > 0 then this.Chars.[startingPos] else '0'

            let signOffset = if lead = '-' || lead = '+' || lead = ' ' then 1 else 0

            let insertAt = startingPos + signOffset

            this.EnsureCapacityForAdditionalChars(paddingNeeded)

            this.Chars
                .Slice(insertAt, this.Pos - insertAt)
                .CopyTo(this.Chars.Slice(insertAt + paddingNeeded, this.Chars.Length - (insertAt + paddingNeeded)))

            this.Chars.Slice(insertAt, paddingNeeded).Fill('0')
            this.Pos <- this.Pos + paddingNeeded

    /// Writes `value` for an F# `%0w.pf` hole: formatted via `format` (an
    /// `"F<precision>"` string), then zero-padded — after any leading sign — to
    /// a total field of `width` chars. Dedicated because no .NET float format
    /// zero-pads to a total width.
    member this.AppendZeroPaddedFloat(value: float, format: string, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format) // the "F<prec>" body, no padding
        this.ZeroPadAfterSign(startingPos, width)

    /// Writes `value` for an F# `%+0w.pf` / `% 0w.pf` hole: formatted via `format`
    /// (an `"F<precision>"` string — a *standard* format, so round-half-to-even), a
    /// forced sign composed on a non-negative number (`+`, or a space when `space`;
    /// NaN/±∞ get none, mirroring `AppendDynamicPrecisionSignedFloat`), then zero-padded
    /// AFTER that sign to a total field of `width` chars. Dedicated because no .NET
    /// float format both forces a sign and zero-pads to a total width — and because the
    /// custom *section* format that could (`"+0.00;-0.00"`) rounds half-away rather than
    /// the half-to-even the `"F<prec>"` body gives.
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

    /// Writes `value` for an F# `%-0w.pf` hole: formatted via `format` (an
    /// `"F<precision>"` string), then zero-padded on the RIGHT (past the digits) to a
    /// total field of `width` chars. Dedicated because F#'s left-align + zero-pad fills
    /// the right with zeros, which no .NET float format nor field alignment reproduces.
    member this.AppendRightZeroPaddedFloat(value: float, format: string, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format) // the "F<prec>" body, no padding
        let paddingNeeded = width - (this.Pos - startingPos)

        // Overflow (already ≥ width) is a no-op — F# never truncates a zero-pad field.
        if paddingNeeded > 0 then
            this.EnsureCapacityForAdditionalChars(paddingNeeded)
            this.Chars.Slice(this.Pos, paddingNeeded).Fill('0')
            this.Pos <- this.Pos + paddingNeeded

    /// Writes `value` as 32-bit two's-complement octal for an F# `%08o` hole, then
    /// zero-pads to a total field of `width` chars. `%o` output carries no sign, so
    /// the padding is a plain left-fill; overflowing digits are not truncated.
    member this.AppendZeroPaddedOctal(value: int, width: int) =
        let startingPos = this.Pos
        this.AppendLiteral(Convert.ToString(value, 8))
        this.ZeroPadAfterSign(startingPos, width)

    /// Writes `value` as unsigned decimal for an F# `%05u` hole, then zero-pads to a
    /// total field of `width` chars. `%u` output carries no sign, so the padding is a
    /// plain left-fill; overflowing digits are not truncated.
    member this.AppendZeroPaddedUnsigned(value: uint, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value)
        this.ZeroPadAfterSign(startingPos, width)

    /// Writes `value` as copy-pasteable Vesper source for an F# `%A` hole, laid
    /// out within a column budget of `width` chars (0 ⇒ never break) and a node
    /// budget of `size` (F# PrintSize).
    member this.AppendStructured(value: 'T, width: int, size: int) =
        this.AppendLiteral(StructuralPrinter.Print(value, width, size))

    /// Writes a float hole with a *runtime* precision (`%.*f`/`%*.*f`/`%.*e`/`%.*g`),
    /// justified in a field of `alignment` chars (negative ⇒ left-justify; 0 ⇒ none).
    /// Builds the .NET format string exactly as FSharp.Core's `getFormatForFloat`
    /// (`printf.fs:606`) — `typeChar.ToString() + precision.ToString()` — so a garbage
    /// precision reproduces the .NET *custom*-format fallback byte-for-byte (e.g.
    /// `%.*f -1` ⇒ `"f-1"`). `typeChar` is the SOURCE type letter (`'f'`/`'e'`/`'E'`/
    /// `'g'`/`'G'`) so the fallback's case matches. The two-star clamp
    /// (`normalizePrecision`, `printf.fs:632`) is applied by the emitter, not here —
    /// the asymmetry against the prec-star-only paths (`:649-657`) is load-bearing.
    member this.AppendDynamicPrecisionFloat(value: float, typeChar: char, precision: int, alignment: int) =
        this.AppendFormatted(value, alignment, typeChar.ToString() + precision.ToString())

    /// As `AppendDynamicPrecisionFloat`, but for a forced-sign float (`%+.*f`/`% .*f`/
    /// `%+*.*f`): a non-negative number gets a leading `+` (or ` ` when `space`) before
    /// justification, mirroring FSharp.Core's `noJustificationCore` /
    /// `rightJustifyWithSpaceAsPadChar` (`printf.fs:801`/`:771`). The compile-time
    /// section-format lowering (`ClrHoleFormat`, `"+0.000;-0.000"`) can't take a runtime
    /// precision, so the sign is composed here.
    member this.AppendDynamicPrecisionSignedFloat
        (value: float, typeChar: char, precision: int, alignment: int, space: bool)
        =
        let fmt = typeChar.ToString() + precision.ToString()

        let str =
            match box value with
            | :? IFormattable as f -> f.ToString(fmt, provider)
            | _ -> value.ToString()

        // NaN / ±∞ are not numbers (`isNumber`, `printf.fs:966`): no sign prefix. A
        // negative value already carries its own `-`; detect it from the formatted text's
        // leading `-` (the `>=` operator is int-only here, and this is what
        // `AppendZeroPaddedFloat` / the JS backend already do for sign detection).
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

    /// Guards a `%*d`-style runtime field width: throws
    /// `ArgumentOutOfRangeException("totalWidth")` on a negative width — the same
    /// exception type + `ParamName` F#'s `PadLeft`/`PadRight` throw — and returns a
    /// non-negative width unchanged. Not built on `PadLeft` (which would allocate);
    /// the guard is the throw parity, not a real pad.
    static member GuardTotalWidth(totalWidth: int) : int =
        if totalWidth < 0 then
            raise (new ArgumentOutOfRangeException("totalWidth"))
        else
            totalWidth

    /// Clamps a `%*A` runtime column budget to `0` on a negative width — F# renders
    /// a negative `%A` width flat (never breaking) rather than throwing.
    static member ClampWidth(width: int) : int = if width < 0 then 0 else width

    /// Clamps a runtime precision to `0..99` — F#'s `normalizePrecision`
    /// (`printf.fs:608`). Applied ONLY on the width=*+prec=* path (`printf.fs:632`);
    /// the prec=*-only paths use the raw precision (`:649-657`), so the emitter calls
    /// this only when a hole has BOTH star dimensions.
    static member NormalizePrecision(precision: int) : int =
        if precision < 0 then 0
        elif precision > 99 then 99
        else precision

    /// Flushes buffered text to the write-through sink (a `TextWriter` or a
    /// `StringBuilder`) and releases the buffer. At most one sink is set; the
    /// string sink (`ToStringAndClear`) never flushes. The `.ToString()` on the
    /// span mirrors the writer path — F# has no implicit `Span` → `ReadOnlySpan`
    /// conversion for the no-alloc `StringBuilder.Append(ReadOnlySpan<char>)`.
    member this.Flush() =
        match this.Writer with
        | null ->
            match this.Builder with
            | null -> ()
            | sb -> sb.Append(this.Chars.Slice(0, this.Pos).ToString()) |> ignore
        | w -> w.Write(this.Chars.Slice(0, this.Pos).ToString())

        this.Clear()

    /// Returns the accumulated text (string sink) and releases the buffer.
    member this.ToStringAndClear() : string =
        let result = this.Chars.Slice(0, this.Pos).ToString()
        this.Clear()
        result

    /// Clears the handler, returning the pooled buffer. The handler must not be
    /// used after this; it is the last operation performed on it.
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
