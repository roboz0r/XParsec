namespace Vesper

open System
open System.Buffers
open System.Globalization
open System.IO

// formatter.fs — the Vesper-compiled implementation of `formatter.fsi`, ported
// from the hand-written C# `Formatter.cs` (the printf write-through handler,
// vesper-printf-plan §P1). This is the PP5f integration of the printf-port-steps
// rungs: the field block (PP4 / PP2a), the buffer-copy surface (PP5a), unsigned
// grow arithmetic (PP5b), the `IFormattable` hole dispatch (PP5c), and the
// `null` pattern (PP4).
//
// STATUS (2026-06-15): this is the *intended* full port; it does **not** yet
// compile end-to-end through this repo's backend. PP5f proved the non-overloaded
// core compiles + runs byte-identically (StructTests "PP5f: Formatter core …")
// and landed the struct self-call codegen fix the bodies require, but stitching
// the *whole* file surfaced several front-end gaps — the four overloaded
// `AppendFormatted<'T>`, `TextWriter.Write` / `string.CopyTo` overload picks, the
// parameterless `Span<char>()` ctor, the `uint`→`int` conversion — tracked in
// `../XParsec.FSharp.SemanticAnalysis/docs/overload-resolution-bug.md`. PP6
// (adding this file to `manifest.toml` and compiling it) is gated on those.
//
// Deviations from `Formatter.cs`, each byte-identical:
//   * `AppendFormatted` uses the `IFormattable.ToString(format, provider)` path
//     rather than the no-alloc `ISpanFormattable.TryFormat` span fast-path. The
//     fast-path is the PP5d optimization (proven independently); the ToString
//     path produces identical text (same culture, same format string) and keeps
//     the hot member free of the grow-retry loop. printf-port-steps PP5f
//     sanctions this ("without [PP5d] AppendFormatted uses the PP5c
//     IFormattable.ToString path — still byte-identical").
//   * `Flush` writes `this.Chars.Slice(0, this.Pos).ToString()` via
//     `TextWriter.Write(string)` rather than `TextWriter.Write(ReadOnlySpan<char>)`
//     — F# has no implicit `Span<char>` → `ReadOnlySpan<char>` conversion; the
//     string overload is byte-identical (printf-port-steps PP5f).
//   * C#'s `?.` / `??` become explicit `null` matches (PP4).

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

    /// Write-through sink, or `null` for a string sink.
    val mutable private Writer: TextWriter

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

    /// Writes `value` for an F# `%0w.pf` hole: formatted via `format` (an
    /// `"F<precision>"` string), then zero-padded — after any leading sign — to
    /// a total field of `width` chars. Dedicated because no .NET float format
    /// zero-pads to a total width.
    member this.AppendZeroPaddedFloat(value: float, format: string, width: int) =
        let startingPos = this.Pos
        this.AppendFormatted(value, format) // the "F<prec>" body, no padding

        let charsWritten = this.Pos - startingPos
        let paddingNeeded = width - charsWritten

        if paddingNeeded > 0 then
            // The sign (if any) stays at the field's left edge; the zeros fill
            // the gap between it and the digits.
            let signOffset =
                if charsWritten > 0 && this.Chars.[startingPos] = '-' then
                    1
                else
                    0

            let insertAt = startingPos + signOffset

            this.EnsureCapacityForAdditionalChars(paddingNeeded)

            this.Chars
                .Slice(insertAt, this.Pos - insertAt)
                .CopyTo(this.Chars.Slice(insertAt + paddingNeeded, this.Chars.Length - (insertAt + paddingNeeded)))

            this.Chars.Slice(insertAt, paddingNeeded).Fill('0')
            this.Pos <- this.Pos + paddingNeeded

    /// Writes `value` as copy-pasteable Vesper source for an F# `%A` hole, laid
    /// out within a column budget of `width` chars (0 ⇒ never break) and a node
    /// budget of `size` (F# PrintSize). Drives the reflection-free structural
    /// engine (`StructuralPrinter`, still C# — PP7).
    member this.AppendStructured(value: 'T, width: int, size: int) =
        this.AppendLiteral(StructuralPrinter.Print(value, width, size))

    /// Flushes buffered text to the write-through sink and releases the buffer.
    member this.Flush() =
        match this.Writer with
        | null -> ()
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
            int (Math.Clamp(newCapacity, uint MinimumArrayPoolLength, uint Int32.MaxValue))

        let newArray = ArrayPool<char>.Shared.Rent(arraySize)
        this.Chars.Slice(0, this.Pos).CopyTo(Span<char>(newArray))

        let toReturn = this.Pool
        this.Pool <- newArray
        this.Chars <- Span<char>(newArray)

        match toReturn with
        | null -> ()
        | arr -> ArrayPool<char>.Shared.Return(arr)
