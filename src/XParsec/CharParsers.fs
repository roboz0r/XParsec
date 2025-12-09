/// Defines parsers for characters and strings.
/// These parsers are more optimized for working with strings than the general parsers in `XParsec.Parsers`.
module XParsec.CharParsers

open System
open System.Text
open Parsers

// TODO: Look at using SearchValues https://learn.microsoft.com/en-us/dotnet/api/system.buffers.searchvalues-1?view=net-9.0

[<AutoOpen>]
module internal Patterns =
    let (|CharsEqualCI|_|) (s: string) (span: ReadOnlySpan<char>) =
        if MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.OrdinalIgnoreCase) then
#if FABLE_COMPILER
            Some()
#else
            true
#endif
        else
#if FABLE_COMPILER
            None
#else
            false
#endif

    let (|CharsEqual|_|) (s: string) (span: ReadOnlySpan<char>) =
        if MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.Ordinal) then
#if FABLE_COMPILER
            Some()
#else
            true
#endif
        else
#if FABLE_COMPILER
            None
#else
            false
#endif

    let (|Empty|_|) (span: ReadOnlySpan<char>) =
        if span.IsEmpty then
#if FABLE_COMPILER
            Some()
#else
            true
#endif
        else
#if FABLE_COMPILER
            None
#else
            false
#endif

[<RequireQualifiedAccess>]
module ParseError =
    let asciiLetter = Message "Expected Char in range 'A' - 'Z' or 'a' - 'z'."
    let digit = Message "Expected Char in range '0' - '9'."
    let expectedNewline = Message "Expected Newline."
    let intOutOfRange = Message "Integer out of range."
    let intInvalid = Message "Invalid integer format."
    let floatInvalid = Message "Invalid float format."
    let spaces = "Zero or more whitespace characters"
    let spaces1 = "One or more whitespace characters"

    let infiniteLoop =
        Message
            "A parser succeeded in a loop without consuming any input, which is likely an infinite loop. This is a bug in the parser."

    let expectedStringLiteral = Message "Expected string literal"

let isLetter c = Char.IsLetter(c)

let isAsciiLetter c =
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

let isDigit c = c >= '0' && c <= '9'

/// Succeeds if the next char in the input is equal to the given char, and consumes one char. Returns the char, otherwise fails with the Expected char.
let pchar (c: char) (reader: Reader<char, 'State, 'Input, 'InputSlice>) = pitem c reader

/// Succeeds if the next char in the input is equal to the given char, and consumes one char. Returns unit, otherwise fails with the Expected char.
let skipChar (c: char) (reader: Reader<char, 'State, 'Input, 'InputSlice>) = skipItem c reader

/// Succeeds if the next char in the input is equal to the given char, and consumes one char. Returns the `result`, otherwise fails with the Expected char.
let charReturn (c: char) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) = itemReturn c result reader

/// Succeeds if the Reader position is not at the end of the input, and consumes one char.
let anyChar (reader: Reader<char, 'State, 'Input, 'InputSlice>) = pid reader

/// Succeeds if the Reader position is not at the end of the input, and consumes one char. Returns unit.
let skipAnyChar (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome _ ->
        reader.Skip()
        preturn () reader
    | _ -> fail EndOfInput reader

/// Succeeds if the next characters in the reader match the given string, and consumes the characters. Returns the string, otherwise fails with the Expected string.
let pstring (s: string) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail EndOfInput reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.Ordinal) then
        reader.SkipN(s.Length)
        preturn s reader
    else
        fail (ExpectedSeq s) reader

/// Succeeds if the next characters in the reader match the given string (case insensitive), and consumes the characters. Returns `result`.
let stringCIReturn (s: string) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail EndOfInput reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.OrdinalIgnoreCase) then
        reader.SkipN(s.Length)
        preturn result reader
    else
        fail (ExpectedSeq s) reader

/// Succeeds if the next characters in the reader match the given string (case insensitive), and consumes the characters. Returns `result`.
let stringReturn (s: string) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail EndOfInput reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.Ordinal) then
        reader.SkipN(s.Length)
        preturn result reader
    else
        fail (ExpectedSeq s) reader

/// Succeeds if the next character in the reader is an ASCII letter, and consumes one char. Returns the char.
let asciiLetter (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome c ->
        if isAsciiLetter c then
            reader.Skip()
            preturn c reader
        else
            fail ParseError.asciiLetter reader
    | _ -> fail EndOfInput reader

/// Succeeds if the next character in the reader is an ASCII digit, and consumes one char. Returns the char.
let digit (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(c) ->
        if isDigit c then
            reader.Skip()
            preturn c reader
        else
            fail ParseError.digit reader
    | _ -> fail EndOfInput reader

[<RequireQualifiedAccess>]
type internal ManyMode =
    | Continue
    | Stop
    | InfiniteLoop

/// Matches zero or more characters that satisfy the given parser `p1`, and returns the string of matched characters.
/// This parser always succeeds, even if no characters are matched, returning an empty string.
let manyChars (p1: Parser<_, char, _, _, _>) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let pos = reader.Position

    match p1 reader with
    | Ok s1 ->
        let sb = StringBuilder()
        let inline append (c: char) = sb.Append(c) |> ignore
        append s1.Parsed
        let mutable ok = ManyMode.Continue

        while ok = ManyMode.Continue do
            let pos = reader.Position

            match p1 reader with
            | Ok sx ->
                if pos = reader.Position then
                    ok <- ManyMode.InfiniteLoop

                append sx.Parsed
            | Error _ ->
                reader.Position <- pos
                ok <- ManyMode.Stop

        match ok with
        | ManyMode.InfiniteLoop -> fail ParseError.infiniteLoop reader
        | ManyMode.Stop -> preturn (sb.ToString()) reader
        | ManyMode.Continue ->
            // This should never happen, but we handle it just in case.
            invalidOp "Unexpected state in manyChars parser."
    | Error err ->
        reader.Position <- pos
        preturn "" reader

/// Matches one or more characters that satisfy the given parser `p1`, and returns the string of matched characters.
/// This parser fails if no characters are matched.
let many1Chars (p1: Parser<_, char, _, _, _>) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match p1 reader with
    | Ok s1 ->
        let sb = StringBuilder()
        let inline append (c: char) = sb.Append(c) |> ignore
        append s1.Parsed
        let mutable ok = ManyMode.Continue

        while ok = ManyMode.Continue do
            let pos = reader.Position

            match p1 reader with
            | Ok sx ->
                if pos = reader.Position then
                    ok <- ManyMode.InfiniteLoop

                append sx.Parsed
            | Error _ ->
                reader.Position <- pos
                ok <- ManyMode.Stop

        match ok with
        | ManyMode.InfiniteLoop -> fail ParseError.infiniteLoop reader
        | ManyMode.Stop -> preturn (sb.ToString()) reader
        | ManyMode.Continue ->
            // This should never happen, but we handle it just in case.
            invalidOp "Unexpected state in manyChars parser."
    | Error err -> Error err

/// Matches zero or more whitespace characters (space, tab, carriage return, newline) and returns unit.
let spaces (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    (manyChars (
        satisfyL
            (function
            | ' '
            | '\t'
            | '\r'
            | '\n' -> true
            | _ -> false)
            ParseError.spaces
    ))
    >>% ()
    <| reader

/// Matches one or more whitespace characters (space, tab, carriage return, newline) and returns unit.
let spaces1 (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    (many1Chars (
        satisfyL
            (function
            | ' '
            | '\t'
            | '\r'
            | '\n' -> true
            | _ -> false)
            ParseError.spaces1
    ))
    >>% ()
    <| reader

/// Matches one character that satisfies the parser `p1`, and then zero or more characters that satisfy the parser `p`.
/// Returns the string of matched characters.
/// This parser fails if no characters are matched by `p1`.
let many1Chars2
    (p1: Parser<_, char, _, _, _>)
    (p: Parser<_, char, _, _, _>)
    (reader: Reader<char, 'State, 'Input, 'InputSlice>)
    =
    match p1 reader with
    | Ok s1 ->
        let sb = StringBuilder()
        let inline append (c: char) = sb.Append(c) |> ignore
        append s1.Parsed
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p reader with
            | Ok sx -> append sx.Parsed
            | Error _ ->
                reader.Position <- pos
                ok <- false

        preturn (sb.ToString()) reader
    | Error err -> Error err

/// Matches zero or more characters that satisfy the parser `p`, until the parser `pEnd` succeeds.
/// Returns the string of matched characters and the parsed value of `pEnd`.
let manyCharsTill
    (p: Parser<'A, _, _, _, _>)
    (pEnd: Parser<'B, _, _, _, _>)
    (reader: Reader<char, 'State, 'Input, 'InputSlice>)
    =
    let xs = StringBuilder()
    // let mutable reader = reader
    let mutable endTok = None
    let mutable err = None

    while endTok.IsNone && err.IsNone do
        match pEnd reader with
        | Ok s -> endTok <- Some s.Parsed
        | Error _ ->
            match p reader with
            | Ok s -> xs.Append(s.Parsed) |> ignore
            | Error e -> err <- Some e

    match err with
    | None -> preturn (xs.ToString(), endTok.Value) reader
    | Some err -> Error err

/// Matches any of "\n", "\r", or "\r\n" (newline) and returns the provided `result`.
let newlineReturn result (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let s = reader.PeekN(2)

    if s.IsEmpty then
        fail EndOfInput reader
    else
        match s[0] with
        | '\n' ->
            reader.Skip()
            preturn result reader
        | '\r' ->
            if s.Length = 2 && s[1] = '\n' then
                reader.SkipN(2)
                preturn result reader
            else
                reader.Skip()
                preturn result reader
        | _ -> fail ParseError.expectedNewline reader

/// Matches any of "\n", "\r", or "\r\n" (newline) and returns the value '\n'.
let newline (reader: Reader<char, 'State, 'Input, 'InputSlice>) = newlineReturn '\n' reader

/// Matches any of "\n", "\r", or "\r\n" (newline) and returns unit.
let skipNewline (reader: Reader<char, 'State, 'Input, 'InputSlice>) = newlineReturn () reader

/// Matches any of the characters in the given sequence, and returns the character.
let anyOf (chars: char seq) =
    let chars =
        match chars with
        | :? string as s -> s
        | _ -> new String(Array.ofSeq chars)

    let err = $"Any character: '{chars}'"

#if NET5_0_OR_GREATER
    satisfyL (chars.Contains: char -> bool) err
#else
    satisfyL (fun c -> chars.Contains(string c)) err
#endif

let private pint minValue maxValue (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let int0 = int '0'
    let inta = int 'a'
    let intA = int 'A'

    let maybeNext parse next =
        if next > 0L || next < minValue then
            fail ParseError.intOutOfRange reader
        else
            parse next

    // We add the negative value as Int64.MinValue is -9223372036854775808L, which is the smallest value we can parse.
    // and the largest value we can parse is Int64.MaxValue, which is 9223372036854775807L.
    // so the absolute value of the largest negative number is 9223372036854775808L, larger than Int64.MaxValue.

    let rec parseHexDigits value =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '9') ->
            reader.Skip()
            let next = value * 16L - int64 (int c - int0)
            maybeNext parseHexDigits next

        | ValueSome c when (c >= 'a' && c <= 'f') ->
            reader.Skip()
            let next = value * 16L - int64 (int c - inta + 10)
            maybeNext parseHexDigits next

        | ValueSome c when (c >= 'A' && c <= 'F') ->
            reader.Skip()
            let next = value * 16L - int64 (int c - intA + 10)
            maybeNext parseHexDigits next

        | _ -> preturn value reader

    let rec parseOctalDigits value =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '7') ->
            reader.Skip()
            let next = value * 8L - int64 (int c - int0)
            maybeNext parseOctalDigits next
        | _ -> preturn value reader

    let rec parseBinaryDigits value =
        match reader.Peek() with
        | ValueSome '0' ->
            reader.Skip()
            let next = value * 2L
            maybeNext parseBinaryDigits next
        | ValueSome '1' ->
            reader.Skip()
            let next = value * 2L - 1L
            maybeNext parseBinaryDigits next
        | _ -> preturn value reader

    let rec parseDecimalDigits value =
        match reader.Peek() with
        | ValueSome c when isDigit c ->
            reader.Skip()
            let next = value * 10L - int64 (int c - int0)
            maybeNext parseDecimalDigits next
        | _ -> preturn value reader

    let sign =
        match reader.Peek() with
        | ValueSome '-' ->
            reader.Skip()
            1L
        | ValueSome '+' ->
            reader.Skip()
            -1L
        | _ -> -1L

    if reader.AtEnd then
        fail EndOfInput reader
    else

        let pos = reader.Position

        let value =
            match reader.Peek() with
            | ValueSome '0' ->
                reader.Skip()

                match reader.Peek() with
                | ValueSome('x' | 'X') ->
                    reader.Skip()
                    parseHexDigits 0L
                | ValueSome('o' | 'O') ->
                    reader.Skip()
                    parseOctalDigits 0L
                | ValueSome('b' | 'B') ->
                    reader.Skip()
                    parseBinaryDigits 0L
                | _ -> parseDecimalDigits 0L
            | _ -> parseDecimalDigits 0L

        if reader.Position = pos then
            fail ParseError.intInvalid reader
        else
            match value with
            | Ok v ->
                if sign = -1L && v.Parsed = minValue then
                    // Special case for the minimum value, which is negative.
                    // We need to check if the sign is positive and the value is the minimum value.
                    fail ParseError.intOutOfRange reader
                else
                    let v = v.Parsed * sign

                    if v < minValue || v > maxValue then
                        fail ParseError.intOutOfRange reader
                    else
                        preturn v reader
            | Error e -> Error e

let private puint maxValue (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let int0 = int '0'
    let inta = int 'a'
    let intA = int 'A'

    let maybeNext parse prev next =
        if next < prev || next > maxValue then
            fail ParseError.intOutOfRange reader
        else
            parse next

    let rec parseHexDigits value =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '9') ->
            reader.Skip()
            let next = value * 16UL + uint64 (int c - int0)
            maybeNext parseHexDigits value next

        | ValueSome c when (c >= 'a' && c <= 'f') ->
            reader.Skip()
            let next = value * 16UL + uint64 (int c - inta + 10)
            maybeNext parseHexDigits value next

        | ValueSome c when (c >= 'A' && c <= 'F') ->
            reader.Skip()
            let next = value * 16UL + uint64 (int c - intA + 10)
            maybeNext parseHexDigits value next

        | _ -> preturn value reader

    let rec parseOctalDigits value =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '7') ->
            reader.Skip()
            let next = value * 8UL + uint64 (int c - int0)
            maybeNext parseOctalDigits value next
        | _ -> preturn value reader

    let rec parseBinaryDigits value =
        match reader.Peek() with
        | ValueSome '0' ->
            reader.Skip()
            let next = value * 2UL
            maybeNext parseBinaryDigits value next
        | ValueSome '1' ->
            reader.Skip()
            let next = value * 2UL + 1UL
            maybeNext parseBinaryDigits value next
        | _ -> preturn value reader

    let rec parseDecimalDigits value =
        match reader.Peek() with
        | ValueSome c when isDigit c ->
            reader.Skip()
            let next = value * 10UL + uint64 (int c - int0)
            maybeNext parseDecimalDigits value next
        | _ -> preturn value reader

    if reader.AtEnd then
        fail EndOfInput reader
    else


        let pos = reader.Position

        let value =
            match reader.Peek() with
            | ValueSome '0' ->
                reader.Skip()

                match reader.Peek() with
                | ValueSome('x' | 'X') ->
                    reader.Skip()
                    parseHexDigits 0UL
                | ValueSome('o' | 'O') ->
                    reader.Skip()
                    parseOctalDigits 0UL
                | ValueSome('b' | 'B') ->
                    reader.Skip()
                    parseBinaryDigits 0UL
                | _ -> parseDecimalDigits 0UL
            | _ -> parseDecimalDigits 0UL

        if reader.Position = pos then
            fail ParseError.intInvalid reader
        else
            match value with
            | Ok v ->
                let v = v.Parsed

                if v > maxValue then
                    fail ParseError.intOutOfRange reader
                else
                    preturn v reader
            | Error e -> Error e

/// <summary>
/// Parses a 64‐bit signed integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than Int64.MaxValue or less than Int64.MinValue.
/// </remarks>
let pint64 reader =
    pint Int64.MinValue Int64.MaxValue reader

/// <summary>
/// Parses a 32‐bit signed integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than Int32.MaxValue or less than Int32.MinValue.
/// </remarks>
let pint32 reader =
    pint (int64 Int32.MinValue) (int64 Int32.MaxValue) |>> int <| reader

/// <summary>
/// Parses a 16‐bit signed integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than Int16.MaxValue or less than Int16.MinValue.
/// </remarks>
let pint16 reader =
    pint (int64 Int16.MinValue) (int64 Int16.MaxValue) |>> int16 <| reader

/// <summary>
/// Parses an 8‐bit signed integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than SByte.MaxValue or less than SByte.MinValue.
/// </remarks>
let pint8 reader =
    pint (int64 SByte.MinValue) (int64 SByte.MaxValue) |>> int8 <| reader

/// <summary>
/// Parses a 64‐bit unsigned integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than System.Int64.MaxValue or less than System.Int64.MinValue.
/// </remarks>
let puint64 reader = puint UInt64.MaxValue reader

/// <summary>
/// Parses a 32‐bit unsigned integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than System.Int64.MaxValue or less than System.Int64.MinValue.
/// </remarks>
let puint32 reader =
    puint (uint64 UInt32.MaxValue) |>> uint32 <| reader

/// <summary>
/// Parses a 16‐bit unsigned integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than System.Int64.MaxValue or less than System.Int64.MinValue.
/// </remarks>
let puint16 reader =
    puint (uint64 UInt16.MaxValue) |>> uint16 <| reader

/// <summary>
/// Parses an 8‐bit unsigned integer, accepting decimal, hexadecimal (0[xX]), octal (0[oO]) and binary (0[bB]) formats.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than System.Int64.MaxValue or less than System.Int64.MinValue.
/// </remarks>
let puint8 reader =
    puint (uint64 Byte.MaxValue) |>> uint8 <| reader

module internal BigIntParsers =
    let int0 = int '0'
    let inta = int 'a'
    let intA = int 'A'

    let psign (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome '-' ->
            reader.Skip()
            -1I
        | ValueSome '+' ->
            reader.Skip()
            1I
        | _ -> 1I

    let rec parseHexDigits value (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '9') ->
            reader.Skip()
            let next = value * 16I + bigint (int c - int0)
            parseHexDigits next reader

        | ValueSome c when (c >= 'a' && c <= 'f') ->
            reader.Skip()
            let next = value * 16I + bigint (int c - inta + 10)
            parseHexDigits next reader

        | ValueSome c when (c >= 'A' && c <= 'F') ->
            reader.Skip()
            let next = value * 16I + bigint (int c - intA + 10)
            parseHexDigits next reader

        | _ -> value

    let rec parseOctalDigits value (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome c when (c >= '0' && c <= '7') ->
            reader.Skip()
            let next = value * 8I + bigint (int c - int0)
            parseOctalDigits next reader
        | _ -> value

    let rec parseBinaryDigits value (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome '0' ->
            reader.Skip()
            let next = value * 2I
            parseBinaryDigits next reader
        | ValueSome '1' ->
            reader.Skip()
            let next = value * 2I + 1I
            parseBinaryDigits next reader
        | _ -> value

    let rec parseDecimalDigits value (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome c when isDigit c ->
            reader.Skip()
            let next = value * 10I + bigint (int c - int0)
            parseDecimalDigits next reader
        | _ -> value

/// <summary>
/// Parses a signed or unsigned integer in decimal, hexadecimal, octal or binary format.
/// </summary>
/// <remarks>
/// The parser fails
/// - in place, if not at least one digit (including the 0 in the format specifiers 0x etc.) can be parsed,
/// - if no digit comes after the format specifier,
/// - if the value represented by the input string is greater than System.Int64.MaxValue or less than System.Int64.MinValue.
/// </remarks>
let pbigint (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let sign = BigIntParsers.psign reader

    if reader.AtEnd then
        fail EndOfInput reader
    else

        let pos = reader.Position

        let value =
            match reader.Peek() with
            | ValueSome '0' ->
                reader.Skip()

                match reader.Peek() with
                | ValueSome('x' | 'X') ->
                    reader.Skip()
                    BigIntParsers.parseHexDigits 0I reader
                | ValueSome('o' | 'O') ->
                    reader.Skip()
                    BigIntParsers.parseOctalDigits 0I reader
                | ValueSome('b' | 'B') ->
                    reader.Skip()
                    BigIntParsers.parseBinaryDigits 0I reader
                | _ -> BigIntParsers.parseDecimalDigits 0I reader
            | _ -> BigIntParsers.parseDecimalDigits 0I reader

        if reader.Position = pos then
            fail ParseError.intInvalid reader
        else
            let v = value * sign
            preturn v reader

// val pfloat: Parser<float,'u>
// Parses a floating point number in the decimal format (in regular expression notation)

// [0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?
// or the hexadecimal format

// 0[xX][0-9a-fA-F]+(\.[0-9a-fA-F]*)?([pP][+-]?[0-9]+)?
// (as supported by IEEE 754r, C99 and Java, where e.g. 0x1f.cP-5 represents 31.75 * 2‒5).

// The special values NaN and Inf(inity)? (case‐insensitive) are also recognized. All recognized numbers may be prefixed with a plus or minus sign.

// Fractions without a leading digit, as for example “.5”, are not supported.

// The parser fails

// without consuming input, if not at least one digit (including the 0 in 0x) can be parsed,
// after consuming input, if no digit comes after an exponent marker or no hex digit comes after 0x.

module internal FloatParsers =
    let convertToFloat (significand: bigint) (exponent: int) reader =
        let value = $"{significand}e{exponent}" // We use a string and the .NET parse method to avoid precision loss due to rounding.

        match Double.TryParse value with
        | true, v -> preturn v reader
        | _ -> invalidOp $"Failed to parse float from '{value}'. This is likely an issue with XParsec."

    let int0 = int '0'
    let inta = int 'a'
    let intA = int 'A'

    let parseDecimalFloat reader =
        let sign = BigIntParsers.psign reader

        if reader.AtEnd then
            fail EndOfInput reader
        else

            let pos = reader.Position

            let significand = BigIntParsers.parseDecimalDigits 0I reader

            if reader.Position = pos then
                fail ParseError.floatInvalid reader
            else
                let significand, sigExponent =
                    match reader.Peek() with
                    | ValueSome '.' ->
                        reader.Skip()
                        let i = reader.Index
                        let value = BigIntParsers.parseDecimalDigits significand reader
                        // The exponent is the number of digits after the decimal point.
                        // as we include all fractional digits in the significand to maintain precision.
                        let exp = i - reader.Index
                        value, int exp
                    | _ -> significand, 0

                match reader.Peek() with
                | ValueSome('e' | 'E') ->
                    reader.Skip()

                    if reader.AtEnd then
                        fail EndOfInput reader
                    else
                        let exponent = pint32 reader

                        match exponent with
                        | Ok { Parsed = exponent } ->
                            convertToFloat (sign * significand) (sigExponent + exponent) reader
                        | Error e -> fail ParseError.floatInvalid reader
                | _ -> convertToFloat (sign * significand) sigExponent reader

    let parseHexOrDecFloat (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        let pos = reader.Position
        let sign = BigIntParsers.psign reader

        match reader.PeekN 2 with
        | CharsEqualCI "0x" ->
            reader.SkipN 2

            if reader.AtEnd then
                fail EndOfInput reader
            else
                let pos = reader.Position
                let significand = BigIntParsers.parseHexDigits 0I reader

                let significand =
                    if reader.Position = pos then
                        fail ParseError.floatInvalid reader
                    else
                        preturn significand reader

                if reader.Position = pos then
                    fail ParseError.floatInvalid reader
                else
                    match significand with
                    | Ok { Parsed = significand } ->
                        let significand, sigExponent =
                            match reader.Peek() with
                            | ValueSome '.' ->
                                reader.Skip()
                                let pos = reader.Position
                                let significand = BigIntParsers.parseHexDigits significand reader

                                if reader.Position = pos then
                                    significand, 0
                                else
                                    let sigExponent = int (pos.Index - reader.Index) // Exponent is the number of digits after the decimal point (base 16)
                                    let sigExponent = sigExponent * 4 // Convert to base 2
                                    significand, sigExponent
                            | _ -> significand, 0

                        match reader.Peek() with
                        | ValueSome('p' | 'P') ->
                            reader.Skip()

                            if reader.AtEnd then
                                fail EndOfInput reader
                            else
                                let pos = reader.Position
                                let exponent = pint32 reader

                                if reader.Position = pos then
                                    fail ParseError.floatInvalid reader
                                else
                                    match exponent with
                                    | Ok { Parsed = exponent } ->
#if NET8_0_OR_GREATER
                                        preturn
                                            (float (sign * significand) * Double.Exp2(float (sigExponent + exponent)))
                                            reader
#else
                                        preturn
                                            (float (sign * significand) * Math.Pow(2.0, float (sigExponent + exponent)))
                                            reader
#endif
                                    | Error _ -> fail ParseError.floatInvalid reader
                        | _ ->
#if NET8_0_OR_GREATER
                            preturn (float (sign * significand) * Double.Exp2(float sigExponent)) reader
#else
                            preturn (float (sign * significand) * Math.Pow(2.0, float sigExponent)) reader
#endif
                    | Error e -> Error e
        | CharsEqualCI "0" ->
            reader.Skip()
            preturn 0.0 reader
        | Empty -> fail EndOfInput reader
        | _ ->
            reader.Position <- pos
            parseDecimalFloat reader

    let parseFloat (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
        if reader.AtEnd then
            fail EndOfInput reader
        else
            choice
                [
                    stringCIReturn "nan" Double.NaN
                    stringCIReturn "infinity" Double.PositiveInfinity
                    stringCIReturn "inf" Double.PositiveInfinity
                    stringCIReturn "-nan" Double.NaN
                    stringCIReturn "-infinity" Double.NegativeInfinity
                    stringCIReturn "-inf" Double.NegativeInfinity
                    stringCIReturn "+nan" Double.NaN
                    stringCIReturn "+infinity" Double.PositiveInfinity
                    stringCIReturn "+inf" Double.PositiveInfinity
                ]
                reader
            |> function
                | Ok v -> Ok v
                | Error _ -> parseHexOrDecFloat reader

let pfloat (reader: Reader<char, 'State, 'Input, 'InputSlice>) = FloatParsers.parseFloat reader

let private anyStringByReturnImpl (comp: StringComparison) (xs: (string * 'T) seq) (maybeMessage: string option) =
    // Sort by length (greedy first), then by lexicographic order
    // TODO: Consider a Trie (or compact DAWG) rather than linear search
    // This could improve performance for large sets of strings, likely worse for small sets
    let sorted =
        xs
        |> Seq.sortWith (fun (s1, _) (s2, _) ->
            let lenComp = String.length s2 - String.length s1

            if lenComp = 0 then
                String.Compare(s1, s2, comp)
            else
                sign lenComp
        )
        |> Array.ofSeq

    if Array.isEmpty sorted then
        invalidArg (nameof xs) "The input sequence must not be empty"

    if String.IsNullOrEmpty(sorted |> Array.last |> fst) then
        invalidArg (nameof xs) "The input sequence must not contain null or empty strings"

    let maxLen = sorted[0] |> fst |> String.length

    let err =
        let payload =
            match maybeMessage with
            | Some msg -> Message msg
            | None -> sorted |> Seq.map (fun (s, _) -> s :> char seq) |> ExpectedSeqOneOf

        fun pos -> ParseError.create payload pos

    fun (reader: Reader<char, 'State, 'Input, 'InputSlice>) ->
        let span = reader.PeekN maxLen

        if span.IsEmpty then
            fail EndOfInput reader
        else
            let mutable found = ValueNone
            let mutable i = 0

            while i < sorted.Length && found.IsNone do
                let candidate, result = sorted.[i]

                if span.StartsWith(candidate.AsSpan(), comp) then
                    found <- ValueSome(candidate, result)

                i <- i + 1

            match found with
            | ValueSome(s, result) ->
                reader.SkipN s.Length
                preturn result reader
            | ValueNone -> err reader.Position

/// Succeeds if the next characters in the reader match any of the given strings (with the given comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with a detailed error message.
let anyStringByReturn (comp: StringComparison) (xs: (string * 'T) seq) = anyStringByReturnImpl comp xs None

/// Succeeds if the next characters in the reader match any of the given strings (with the given comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with the provided message.
let anyStringByReturnL (comp: StringComparison) (xs: (string * 'T) seq) message =
    anyStringByReturnImpl comp xs (Some message)

/// Succeeds if the next characters in the reader match any of the given strings (with the given comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with a detailed error message.
let anyStringBy (comp: StringComparison) (xs: string seq) =
    anyStringByReturnImpl comp (xs |> Seq.map (fun s -> s, s)) None

/// Succeeds if the next characters in the reader match any of the given strings (with the given comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with the provided message.
let anyStringByL (comp: StringComparison) (xs: string seq) message =
    anyStringByReturnImpl comp (xs |> Seq.map (fun s -> s, s)) (Some message)

/// Succeeds if the next characters in the reader match any of the given strings (with ordinal comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with a detailed error message.
let anyString (xs: string seq) = anyStringBy StringComparison.Ordinal xs

/// Succeeds if the next characters in the reader match any of the given strings (with ordinal comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with the provided message.
let anyStringL (xs: string seq) message =
    anyStringByL StringComparison.Ordinal xs message

/// Succeeds if the next characters in the reader match any of the given strings (case-insensitive ordinal comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with a detailed error message.
let anyStringCI (xs: string seq) =
    anyStringBy StringComparison.OrdinalIgnoreCase xs

/// Succeeds if the next characters in the reader match any of the given strings (case-insensitive ordinal comparison),
/// and consumes the characters. Returns the matched string.
/// If no match is found, fails with the provided message.
let anyStringCIL (xs: string seq) message =
    anyStringByL StringComparison.OrdinalIgnoreCase xs message

/// Succeeds if the next characters in the reader match any of the given strings (with ordinal comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with a detailed error message.
let anyStringReturn (xs: (string * 'T) seq) =
    anyStringByReturn StringComparison.Ordinal xs

/// Succeeds if the next characters in the reader match any of the given strings (with ordinal comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with the provided message.
let anyStringReturnL (xs: (string * 'T) seq) message =
    anyStringByReturnL StringComparison.Ordinal xs message

/// Succeeds if the next characters in the reader match any of the given strings (case-insensitive ordinal comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with a detailed error message.
let anyStringCIReturn (xs: (string * 'T) seq) =
    anyStringByReturn StringComparison.OrdinalIgnoreCase xs

/// Succeeds if the next characters in the reader match any of the given strings (case-insensitive ordinal comparison),
/// and consumes the characters. Returns the associated result.
/// If no match is found, fails with the provided message.
let anyStringCIReturnL (xs: (string * 'T) seq) message =
    anyStringByReturnL StringComparison.OrdinalIgnoreCase xs message
