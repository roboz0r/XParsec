using System;
using System.Buffers;
using System.Globalization;
using System.IO;
using System.Runtime.CompilerServices;

namespace Vesper;

/// <summary>
/// Non-ref-struct anchor for reading this assembly's identity. The CLR backend
/// (<c>ClrProvider</c>) mints its <c>AssemblyReference</c> from the live loaded
/// <c>Vesper.Printf.dll</c> (like FSharp.Core); it can't use
/// <c>typeof(Formatter)</c> because a byref-like type is not a permitted generic
/// argument (CLI rules), so it anchors on this type instead.
/// </summary>
public static class PrintfRuntime
{
}

/// <summary>
/// Write-through format handler for Vesper.Printf P1 (see
/// <c>../XParsec.FSharp.SemanticAnalysis/docs/vesper-printf-plan.md</c>). A
/// stack-only <c>ref struct</c> that accumulates formatted text into a pooled
/// buffer and either flushes it to a <see cref="TextWriter"/> sink
/// (<c>printf</c>/<c>printfn</c>/<c>eprintf</c>) or returns it as a string
/// (<c>sprintf</c>). The CLR backend emits the construction +
/// <c>AppendLiteral</c> / <c>AppendFormatted&lt;T&gt;</c> + flush sequence
/// inline at the call site for a fully-applied literal format.
/// </summary>
/// <remarks>
/// Hand-mirrored from the BCL <c>DefaultInterpolatedStringHandler</c>
/// (<c>../XParsec.FSharp.SemanticAnalysis/ref/DefaultInterpolatedStringHandler.cs</c>):
/// same buffer-management / growth policy, the same hole-formatting policy
/// (<c>IFormattable</c> outer check, then <c>ISpanFormattable.TryFormat</c>
/// straight into the span — no box for value types — then
/// <c>IFormattable.ToString</c>, else <c>object.ToString</c>), and the same
/// alignment handling.
///
/// <para>Deviations, each with a reason:</para>
/// <list type="bullet">
/// <item>A write-through <see cref="Formatter(int,int,TextWriter)"/> ctor +
/// <see cref="Flush"/>: there is no <c>TextWriter.Write(handler)</c> BCL
/// overload, so a zero-string <c>printfn</c> needs its own sink.</item>
/// <item>The string ctor + <see cref="ToStringAndClear"/> stay; the BCL's
/// <c>IFormatProvider</c> / <c>Span&lt;char&gt;</c> initial-buffer ctors are
/// dropped — the backend never supplies either.</item>
/// <item>Formatting is fixed to <see cref="CultureInfo.InvariantCulture"/> to
/// match F# <c>printf</c> (culture-invariant). The custom-<c>ICustomFormatter</c>
/// path is therefore dropped (Invariant supplies none).</item>
/// <item>The enum fast path (<c>Enum.TryFormatUnconstrained</c>) is dropped —
/// it is BCL-internal; enums fall through to <c>IFormattable.ToString</c>.</item>
/// </list>
///
/// <para>The <c>[InterpolatedStringHandler]</c> attribute lets C# target this
/// type for <c>$"..."</c> directly — a free interpolation test harness, and the
/// D9 unification (one handler, two surface syntaxes).</para>
/// </remarks>
[InterpolatedStringHandler]
public ref struct Formatter
{
    /// <summary>Heuristic per-hole reservation; inherited from the BCL handler.</summary>
    private const int GuessedLengthPerHole = 11;

    /// <summary>Smallest pooled buffer rented; inherited from the BCL handler.</summary>
    private const int MinimumArrayPoolLength = 256;

    /// <summary>Max array length the BCL handler clamps growth to (<c>string.MaxLength</c>).</summary>
    private const uint MaxChars = 0x3FFFFFDFu;

    /// <summary>Culture for all hole formatting — Invariant, to match F# <c>printf</c>.</summary>
    private static readonly IFormatProvider Provider = CultureInfo.InvariantCulture;

    /// <summary>Write-through sink, or <c>null</c> for a string sink.</summary>
    private readonly TextWriter? _writer;

    /// <summary>The pooled array backing <see cref="_chars"/>, returned on clear.</summary>
    private char[]? _arrayToReturnToPool;

    /// <summary>The span to write into; always points at <see cref="_arrayToReturnToPool"/>.</summary>
    private Span<char> _chars;

    /// <summary>Position at which to write the next character.</summary>
    private int _pos;

    /// <summary>Write-through ctor: buffered text is flushed to <paramref name="writer"/>.</summary>
    public Formatter(int literalLength, int formattedCount, TextWriter writer)
    {
        _writer = writer;
        _chars = _arrayToReturnToPool = ArrayPool<char>.Shared.Rent(GetDefaultLength(literalLength, formattedCount));
        _pos = 0;
    }

    /// <summary>String ctor: <see cref="ToStringAndClear"/> returns the accumulated text.</summary>
    public Formatter(int literalLength, int formattedCount)
    {
        _writer = null;
        _chars = _arrayToReturnToPool = ArrayPool<char>.Shared.Rent(GetDefaultLength(literalLength, formattedCount));
        _pos = 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)] // becomes a constant when inputs are constant
    private static int GetDefaultLength(int literalLength, int formattedCount) =>
        Math.Max(MinimumArrayPoolLength, literalLength + (formattedCount * GuessedLengthPerHole));

    /// <summary>Writes the specified literal chunk to the handler.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void AppendLiteral(string value)
    {
        if (value.TryCopyTo(_chars.Slice(_pos)))
        {
            _pos += value.Length;
        }
        else
        {
            GrowThenCopyString(value);
        }
    }

    /// <summary>Writes the specified value to the handler.</summary>
    public void AppendFormatted<T>(T value)
    {
        // IFormattable is checked first: ISpanFormattable requires it, and for
        // value types the type checks devolve into JIT-time constants.
        string? s;
        if (value is IFormattable)
        {
            if (value is ISpanFormattable)
            {
                int charsWritten;
                while (!((ISpanFormattable)value).TryFormat(_chars.Slice(_pos), out charsWritten, default, Provider)) // constrained call avoiding boxing for value types
                {
                    Grow();
                }

                _pos += charsWritten;
                return;
            }

            s = ((IFormattable)value).ToString(format: null, Provider); // constrained call avoiding boxing for value types
        }
        else
        {
            s = value?.ToString();
        }

        if (s is not null)
        {
            AppendLiteral(s);
        }
    }

    /// <summary>Writes the specified value to the handler, with a .NET format specifier.</summary>
    public void AppendFormatted<T>(T value, string? format)
    {
        string? s;
        if (value is IFormattable)
        {
            if (value is ISpanFormattable)
            {
                int charsWritten;
                while (!((ISpanFormattable)value).TryFormat(_chars.Slice(_pos), out charsWritten, format, Provider)) // constrained call avoiding boxing for value types
                {
                    Grow();
                }

                _pos += charsWritten;
                return;
            }

            s = ((IFormattable)value).ToString(format, Provider); // constrained call avoiding boxing for value types
        }
        else
        {
            s = value?.ToString();
        }

        if (s is not null)
        {
            AppendLiteral(s);
        }
    }

    /// <summary>Writes the specified value to the handler, right-justified (or, if the
    /// alignment is negative, left-justified) in a field of <paramref name="alignment"/> chars.</summary>
    public void AppendFormatted<T>(T value, int alignment)
    {
        int startingPos = _pos;
        AppendFormatted(value);

        if (alignment != 0)
        {
            AppendOrInsertAlignmentIfNeeded(startingPos, alignment);
        }
    }

    /// <summary>Writes the specified value to the handler, with both an alignment and a format.</summary>
    public void AppendFormatted<T>(T value, int alignment, string? format)
    {
        int startingPos = _pos;
        AppendFormatted(value, format);

        if (alignment != 0)
        {
            AppendOrInsertAlignmentIfNeeded(startingPos, alignment);
        }
    }

    // ---- Dedicated members for specifiers that have no AppendFormatted<T>(…, fmt)
    //      shape (P2). Each is alignment-aware (so `%5b` / `%-5o` work) and
    //      InvariantCulture, matching F# printf. ----

    /// <summary>Writes <c>true</c>/<c>false</c> (lowercase) for an F# <c>%b</c>
    /// hole, justified in a field of <paramref name="alignment"/> chars
    /// (negative ⇒ left-justify; 0 ⇒ no padding).</summary>
    /// <remarks>Deviation from the BCL handler: a dedicated member rather than
    /// <see cref="AppendFormatted{T}(T)"/>, because <c>bool.ToString()</c>
    /// capitalises ("True"/"False") and F# <c>%b</c> is lowercase.</remarks>
    public void AppendBool(bool value, int alignment = 0)
    {
        int startingPos = _pos;
        AppendLiteral(value ? "true" : "false");

        if (alignment != 0)
        {
            AppendOrInsertAlignmentIfNeeded(startingPos, alignment);
        }
    }

    /// <summary>Writes <paramref name="value"/> as 32-bit two's-complement octal
    /// for an F# <c>%o</c> hole, justified in a field of
    /// <paramref name="alignment"/> chars.</summary>
    /// <remarks>Deviation: a dedicated member because .NET has no octal format
    /// string. <c>Convert.ToString(int, 8)</c> renders negatives as 32-bit
    /// two's-complement, matching F# <c>%o</c> on <c>int</c>; it allocates the
    /// digits string (acceptable — <c>%o</c> is rare and off the common
    /// path).</remarks>
    public void AppendOctal(int value, int alignment = 0)
    {
        int startingPos = _pos;
        AppendLiteral(Convert.ToString(value, 8));

        if (alignment != 0)
        {
            AppendOrInsertAlignmentIfNeeded(startingPos, alignment);
        }
    }

    /// <summary>Writes <paramref name="value"/> as unsigned decimal for an F#
    /// <c>%u</c> hole, justified in a field of <paramref name="alignment"/>
    /// chars. The backend passes the source <c>int</c>'s bits unchanged (the
    /// CLI stack treats <c>int32</c>/<c>uint32</c> alike), reproducing F#
    /// <c>%u</c>'s two's-complement reinterpretation with no conversion.</summary>
    public void AppendUnsigned(uint value, int alignment = 0) => AppendFormatted(value, alignment);

    /// <summary>Writes <paramref name="value"/> for an F# <c>%0w.pf</c> hole:
    /// formatted via <paramref name="format"/> (an <c>"F&lt;precision&gt;"</c>
    /// string), then zero-padded — <em>after</em> any leading sign — to a total
    /// field of <paramref name="width"/> chars.</summary>
    /// <remarks>Deviation: a dedicated member because .NET has no float format
    /// that zero-pads to a total width (F# <c>%08.2f</c> of <c>-3.14159</c> is
    /// <c>"-0003.14"</c> — the <c>0</c>s sit between the sign and the digits).
    /// With the <c>0</c> flag alone the only possible leading sign is <c>-</c>
    /// (the <c>+</c>/space sign flags combined with <c>0</c> stay on the cold
    /// path). When the formatted body already meets or exceeds
    /// <paramref name="width"/>, nothing is padded.</remarks>
    public void AppendZeroPaddedFloat(double value, string format, int width)
    {
        int startingPos = _pos;
        AppendFormatted(value, format); // the "F<prec>" body, no padding

        int charsWritten = _pos - startingPos;
        int paddingNeeded = width - charsWritten;
        if (paddingNeeded > 0)
        {
            // The sign (if any) stays at the field's left edge; the zeros fill
            // the gap between it and the digits.
            int signOffset = (charsWritten > 0 && _chars[startingPos] == '-') ? 1 : 0;
            int insertAt = startingPos + signOffset;

            EnsureCapacityForAdditionalChars(paddingNeeded);
            _chars.Slice(insertAt, _pos - insertAt).CopyTo(_chars.Slice(insertAt + paddingNeeded));
            _chars.Slice(insertAt, paddingNeeded).Fill('0');
            _pos += paddingNeeded;
        }
    }

    /// <summary>Writes <paramref name="value"/> as copy-pasteable Vesper source for
    /// an F# <c>%A</c> hole, laid out within a column budget of
    /// <paramref name="width"/> chars (0 ⇒ never break — the <c>%0A</c> flat mode)
    /// and a node budget of <paramref name="size"/> (F# PrintSize — nodes past it
    /// render as <c>...</c>, the <c>%.NA</c> mode; 10000 for plain <c>%A</c>).</summary>
    /// <remarks>Deviation: a dedicated member because <c>%A</c> has no
    /// <see cref="AppendFormatted{T}(T)"/> shape — it drives the reflection-free
    /// structural engine (<see cref="StructuralPrinter"/>) rather than an
    /// <c>IFormattable</c> call. The value is boxed inside <c>Print</c>; acceptable
    /// on the structural heavy path (F#'s reflection <c>%A</c> boxes everything).</remarks>
    public void AppendStructured<T>(T value, int width, int size) => AppendLiteral(StructuralPrinter.Print(value, width, size));

    /// <summary>Flushes buffered text to the write-through sink and releases the buffer.</summary>
    public void Flush()
    {
        _writer?.Write(_chars.Slice(0, _pos));
        Clear();
    }

    /// <summary>Returns the accumulated text (string sink) and releases the buffer.</summary>
    public string ToStringAndClear()
    {
        string result = _chars.Slice(0, _pos).ToString();
        Clear();
        return result;
    }

    /// <summary>Clears the handler, returning the pooled buffer. The handler must not
    /// be used after this; it is the last operation performed on it.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void Clear()
    {
        char[]? toReturn = _arrayToReturnToPool;

        // Defensive clear (mirrors the BCL handler; _writer is readonly and left as-is).
        _arrayToReturnToPool = null;
        _chars = default;
        _pos = 0;

        if (toReturn is not null)
        {
            ArrayPool<char>.Shared.Return(toReturn);
        }
    }

    // ---- Alignment + buffer growth (mirrors DefaultInterpolatedStringHandler) ----

    private void AppendOrInsertAlignmentIfNeeded(int startingPos, int alignment)
    {
        int charsWritten = _pos - startingPos;

        bool leftAlign = false;
        if (alignment < 0)
        {
            leftAlign = true;
            alignment = -alignment;
        }

        int paddingNeeded = alignment - charsWritten;
        if (paddingNeeded > 0)
        {
            EnsureCapacityForAdditionalChars(paddingNeeded);

            if (leftAlign)
            {
                _chars.Slice(_pos, paddingNeeded).Fill(' ');
            }
            else
            {
                _chars.Slice(startingPos, charsWritten).CopyTo(_chars.Slice(startingPos + paddingNeeded));
                _chars.Slice(startingPos, paddingNeeded).Fill(' ');
            }

            _pos += paddingNeeded;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void EnsureCapacityForAdditionalChars(int additionalChars)
    {
        if (_chars.Length - _pos < additionalChars)
        {
            Grow(additionalChars);
        }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private void GrowThenCopyString(string value)
    {
        Grow(value.Length);
        value.CopyTo(_chars.Slice(_pos));
        _pos += value.Length;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] // keep consumers as streamlined as possible
    private void Grow(int additionalChars) => GrowCore((uint)_pos + (uint)additionalChars);

    [MethodImpl(MethodImplOptions.NoInlining)] // keep consumers as streamlined as possible
    private void Grow() => GrowCore((uint)_chars.Length + 1);

    [MethodImpl(MethodImplOptions.AggressiveInlining)] // reuse this grow logic directly in both grow routines
    private void GrowCore(uint requiredMinCapacity)
    {
        // Max of the required capacity and a doubling, clamped below the max array
        // length and floored at the minimum pool rental — the BCL growth policy.
        uint newCapacity = Math.Max(requiredMinCapacity, Math.Min((uint)_chars.Length * 2, MaxChars));
        int arraySize = (int)Math.Clamp(newCapacity, MinimumArrayPoolLength, int.MaxValue);

        char[] newArray = ArrayPool<char>.Shared.Rent(arraySize);
        _chars.Slice(0, _pos).CopyTo(newArray);

        char[]? toReturn = _arrayToReturnToPool;
        _chars = _arrayToReturnToPool = newArray;

        if (toReturn is not null)
        {
            ArrayPool<char>.Shared.Return(toReturn);
        }
    }
}
