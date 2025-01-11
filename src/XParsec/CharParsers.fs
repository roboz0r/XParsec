module XParsec.CharParsers

open System
open System.Text
open Parsers

// TODO: Look at using SearchValues https://learn.microsoft.com/en-us/dotnet/api/system.buffers.searchvalues-1?view=net-9.0

let inline isLetter c = Char.IsLetter(c)
let isDigit c = c >= '0' && c <= '9'

let pchar (c: char) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(x) ->
        if x = c then
            reader.Skip()
            preturn c reader
        else
            fail (Expected(c)) reader

    | _ -> fail EndOfInput reader

let skipChar (c: char) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(x) ->
        if x = c then
            reader.Skip()
            preturn () reader
        else
            fail (Expected(c)) reader

    | _ -> fail EndOfInput reader

let charReturn (c: char) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(x) ->
        if x = c then
            reader.Skip()
            preturn result reader
        else
            fail (Expected(c)) reader

    | _ -> fail EndOfInput reader

let anyChar (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.TryRead() with
    | ValueSome(c) -> preturn c reader
    | _ -> fail EndOfInput reader

let pstring (s: string) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail (EndOfInput) reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.Ordinal) then
        reader.SkipN(s.Length)
        preturn s reader
    else
        fail (ExpectedSeq s) reader


let stringCIReturn (s: string) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail (EndOfInput) reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.OrdinalIgnoreCase) then
        reader.SkipN(s.Length)
        preturn result reader
    else
        fail (ExpectedSeq s) reader

let stringReturn (s: string) (result) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let span = reader.PeekN(s.Length)

    if span.IsEmpty then
        fail (EndOfInput) reader
    elif MemoryExtensions.Equals(s.AsSpan(), span, StringComparison.Ordinal) then
        reader.SkipN(s.Length)
        preturn result reader
    else
        fail (ExpectedSeq s) reader

let asciiLetter (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(c) ->
        if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
            reader.Skip()
            preturn c reader
        else
            fail (Message "Expected Char in range 'A' - 'Z' or 'a' - 'z'.") reader
    | _ -> fail EndOfInput reader

let digit (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match reader.Peek() with
    | ValueSome(c) ->
        if c >= '0' && c <= '9' then
            reader.Skip()
            preturn c reader
        else
            fail (Message "Expected Char in range '0' - '9'") reader
    | _ -> fail EndOfInput reader

let manyChars (p1: Parser<_, char, _, _, _>) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let pos = reader.Position

    match p1 reader with
    | Ok s1 ->
        let sb = StringBuilder()
        let inline append (c: char) = sb.Append(c) |> ignore
        append s1.Parsed
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p1 reader with
            | Ok sx -> append sx.Parsed
            | Error _ ->
                reader.Position <- pos
                ok <- false

        preturn (sb.ToString()) reader
    | Error err ->
        reader.Position <- pos
        preturn "" reader

let many1Chars (p1: Parser<_, char, _, _, _>) (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    match p1 reader with
    | Ok s1 ->
        let sb = StringBuilder()
        let inline append (c: char) = sb.Append(c) |> ignore
        append s1.Parsed
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p1 reader with
            | Ok sx -> append sx.Parsed
            | Error _ ->
                reader.Position <- pos
                ok <- false

        preturn (sb.ToString()) reader
    | Error err -> Error err

let spaces (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    (manyChars (
        satisfyL
            (function
            | ' '
            | '\t'
            | '\r'
            | '\n' -> true
            | _ -> false)
            ("Zero or more whitespace characters")
    ))
    >>% ()
    <| reader

let spaces1 (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    (many1Chars (
        satisfyL
            (function
            | ' '
            | '\t'
            | '\r'
            | '\n' -> true
            | _ -> false)
            ("One or more whitespace characters")
    ))
    >>% ()
    <| reader

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


let newline (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let s = reader.PeekN(2)

    if s.IsEmpty then
        fail (Message "Expected Newline") reader
    else
        match s[0] with
        | '\n' ->
            reader.Skip()
            preturn '\n' reader
        | '\r' ->
            if s.Length = 2 && s[1] = '\n' then
                reader.SkipN(2)
                preturn '\n' reader
            else
                reader.Skip()
                preturn '\n' reader
        | _ -> fail (Message "Expected Newline") reader

let skipNewline (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let s = reader.PeekN(2)

    if s.IsEmpty then
        fail (Message "Expected Newline") reader
    else
        match s[0] with
        | '\n' ->
            reader.Skip()
            preturn () reader
        | '\r' ->
            if s.Length = 2 && s[1] = '\n' then
                reader.SkipN(2)
                preturn () reader
            else
                reader.Skip()
                preturn () reader
        | _ -> fail (Message "Expected Newline") reader

let newlineReturn x (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
    let s = reader.PeekN(2)

    if s.IsEmpty then
        fail (Message "Expected Newline") reader
    else
        match s[0] with
        | '\n' ->
            reader.Skip()
            preturn x reader
        | '\r' ->
            if s.Length = 2 && s[1] = '\n' then
                reader.SkipN(2)
                preturn x reader
            else
                reader.Skip()
                preturn x reader
        | _ -> fail (Message "Expected Newline") reader

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
