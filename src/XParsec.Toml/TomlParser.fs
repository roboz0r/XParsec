/// TOML 1.0.0 compliant parser built with XParsec.
/// BCL-minimal design using F# derived types.
/// See: https://toml.io/en/v1.0.0
namespace XParsec.Toml

open XParsec
open XParsec.Parsers
open XParsec.Combinators
open XParsec.CharParsers

#nowarn "40" // Recursive value definitions

/// Parser implementation for TOML.
module TomlParser =

    // ============================================================================
    // Whitespace and Comments
    // ============================================================================

    /// Whitespace: space or tab (no newlines).
    let private pWs = skipManySatisfies (fun c -> c = ' ' || c = '\t')

    /// Newline: \n or \r\n
    let private pNewline = (pstring "\r\n" >>% ()) <|> (pchar '\n' >>% ())

    /// Comment: # followed by anything until newline
    let private pComment =
        pchar '#' >>. skipManySatisfies (fun c -> c <> '\n' && c <> '\r')

    /// Optional whitespace and/or comment before newline
    let private pWsComment = pWs >>. optional pComment

    /// Newline or end of input
    let private pNewlineOrEof = (pNewline >>% ()) <|> eof

    /// Optional whitespace/comment followed by newline or eof
    let private pLineEnd = pWsComment >>. pNewlineOrEof

    /// Skip whitespace, comments, and newlines in arrays/multiline contexts
    let private pSkipWsAndComments reader =
        let pWsNl = skipManySatisfies (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r')

        let rec loop () =
            // Skip whitespace
            match pWsNl reader with
            | Ok _ ->
                // Check for comment
                match pComment reader with
                | Ok _ -> loop () // Comment found, continue skipping
                | Error _ -> preturn () reader // No comment, done
            | Error e -> Error e

        loop ()

    /// Skip blank/comment lines
    let private pSkipBlankLines = skipMany (pWsComment >>. pNewline)

    // ============================================================================
    // Basic String Parsing
    // ============================================================================

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        else int c - int 'A' + 10

    /// Hex digit parser
    let private pHexDigit =
        satisfy (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

    /// Parse 4-digit unicode escape \uXXXX
    let private pUnicode4 =
        parser {
            let! h0 = pHexDigit
            let! h1 = pHexDigit
            let! h2 = pHexDigit
            let! h3 = pHexDigit

            let value =
                (hexValue h0 <<< 12)
                ||| (hexValue h1 <<< 8)
                ||| (hexValue h2 <<< 4)
                ||| hexValue h3

            return string (char value)
        }

    /// Parse 8-digit unicode escape \UXXXXXXXX
    let private pUnicode8 =
        parser {
            let! h0 = pHexDigit
            let! h1 = pHexDigit
            let! h2 = pHexDigit
            let! h3 = pHexDigit
            let! h4 = pHexDigit
            let! h5 = pHexDigit
            let! h6 = pHexDigit
            let! h7 = pHexDigit

            let value =
                (hexValue h0 <<< 28)
                ||| (hexValue h1 <<< 24)
                ||| (hexValue h2 <<< 20)
                ||| (hexValue h3 <<< 16)
                ||| (hexValue h4 <<< 12)
                ||| (hexValue h5 <<< 8)
                ||| (hexValue h6 <<< 4)
                ||| hexValue h7
            // Handle surrogate pairs for values > 0xFFFF
            if value <= 0xFFFF then
                return string (char value)
            else
                let adjusted = value - 0x10000
                let high = char (0xD800 + (adjusted >>> 10))
                let low = char (0xDC00 + (adjusted &&& 0x3FF))
                return string high + string low
        }

    /// Escape sequence in basic string
    let private pEscape =
        pchar '\\'
        >>. choice
                [
                    pchar '"' >>% "\""
                    pchar '\\' >>% "\\"
                    pchar 'b' >>% "\b"
                    pchar 'f' >>% "\f"
                    pchar 'n' >>% "\n"
                    pchar 'r' >>% "\r"
                    pchar 't' >>% "\t"
                    pchar 'u' >>. pUnicode4
                    pchar 'U' >>. pUnicode8
                ]

    /// Non-escape character in basic string
    let private pBasicChar =
        satisfy (fun c -> c <> '"' && c <> '\\' && (c >= '\u0020' || c = '\t'))
        |>> string

    /// Basic string content (between double quotes)
    let private pBasicStringContent =
        many (pEscape <|> pBasicChar) |>> String.concat ""

    /// Basic string: "..."
    let private pBasicString = between (pchar '"') (pchar '"') pBasicStringContent

    // ============================================================================
    // Multiline Basic String
    // ============================================================================

    /// Newline in multiline string (normalized to \n)
    let private pMlNewline = (pstring "\r\n" >>% "\n") <|> (pchar '\n' >>% "\n")

    /// Line ending backslash (trim following whitespace and newlines)
    let private pLineEndingBackslash =
        pchar '\\'
        >>. pWs
        >>. pMlNewline
        >>. skipManySatisfies (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r')
        >>% ""

    /// Multiline basic string content
    let private pMlBasicStringContent =
        many (
            choice
                [
                    pLineEndingBackslash
                    pMlNewline
                    pEscape
                    pBasicChar
                    (pstring "\"\"" .>> notFollowedBy (pchar '"')) >>% "\"\""
                    (pchar '"' .>> notFollowedBy (pchar '"')) >>% "\""
                ]
        )
        |>> String.concat ""

    /// Multiline basic string: """..."""
    let private pMlBasicString =
        pstring "\"\"\"" >>. optional pMlNewline >>. pMlBasicStringContent
        .>> pstring "\"\"\""

    // ============================================================================
    // Literal Strings
    // ============================================================================

    /// Literal string content (no escapes, no single quote)
    let private pLiteralChar =
        satisfy (fun c -> c <> '\'' && (c >= '\u0020' || c = '\t'))

    /// Literal string: '...'
    let private pLiteralString =
        between (pchar '\'') (pchar '\'') (manyChars pLiteralChar)

    /// Multiline literal string: '''...'''
    let private pMlLiteralString =
        parser {
            let! _ = pstring "'''"
            let! _ = optional pMlNewline
            let! content, _ = manyCharsTill anyChar (pstring "'''")
            return content
        }

    // ============================================================================
    // String (all types)
    // ============================================================================

    /// Any TOML string
    let private pString =
        choice [ pMlBasicString; pMlLiteralString; pBasicString; pLiteralString ]

    // ============================================================================
    // Integers
    // ============================================================================

    let private pDecDigit = satisfy (fun c -> c >= '0' && c <= '9')

    /// Decimal integer (with optional sign and underscores). Folds digits
    /// directly into an int64 — no intermediate Replace allocation.
    let private pDecInteger =
        parser {
            let! sign = opt (pchar '+' <|> pchar '-')
            let! digits = many1Chars (pDecDigit <|> pchar '_')
            let mutable value = 0L

            for c in digits do
                if c <> '_' then
                    value <- value * 10L + int64 (int c - int '0')

            return
                match sign with
                | ValueSome '-' -> -value
                | _ -> value
        }

    /// Hex integer: 0x...
    let private pHexInteger =
        pstring "0x" >>. many1Chars (pHexDigit <|> pchar '_')
        |>> fun s ->
            let mutable result = 0L

            for c in s do
                if c <> '_' then
                    result <- result * 16L + int64 (hexValue c)

            result

    /// Octal integer: 0o...
    let private pOctInteger =
        let pOctDigit = satisfy (fun c -> c >= '0' && c <= '7')

        pstring "0o" >>. many1Chars (pOctDigit <|> pchar '_')
        |>> fun s ->
            let mutable result = 0L

            for c in s do
                if c <> '_' then
                    result <- result * 8L + int64 (int c - int '0')

            result

    /// Binary integer: 0b...
    let private pBinInteger =
        let pBinDigit = satisfy (fun c -> c = '0' || c = '1')

        pstring "0b" >>. many1Chars (pBinDigit <|> pchar '_')
        |>> fun s ->
            let mutable result = 0L

            for c in s do
                if c <> '_' then
                    result <- result * 2L + int64 (int c - int '0')

            result

    /// Any TOML integer
    let private pInteger = choice [ pHexInteger; pOctInteger; pBinInteger; pDecInteger ]

    // ============================================================================
    // Floats
    // ============================================================================

    /// Float with exponent and/or fraction. Assembles a single canonical
    /// decimal-string in one StringBuilder (skipping underscores inline)
    /// for `float` to parse — avoids the 3-4 intermediate Replace strings
    /// the prior implementation built.
    let private pFloat =
        parser {
            let! sign = opt (pchar '+' <|> pchar '-')
            let! intPart = many1Chars (pDecDigit <|> pchar '_')
            let! fracPart = opt (pchar '.' >>. many1Chars (pDecDigit <|> pchar '_'))

            let! expPart =
                opt (
                    parser {
                        let! _ = pchar 'e' <|> pchar 'E'
                        let! expSign = opt (pchar '+' <|> pchar '-')
                        let! expDigits = many1Chars (pDecDigit <|> pchar '_')
                        return (expSign, expDigits)
                    }
                )

            // Must have either fraction or exponent (or both) to be a float
            match fracPart, expPart with
            | ValueNone, ValueNone -> return! fail (Message "Expected float")
            | _ ->
                let sb = System.Text.StringBuilder(intPart.Length + 12)

                if sign = ValueSome '-' then
                    sb.Append '-' |> ignore

                for c in intPart do
                    if c <> '_' then
                        sb.Append c |> ignore

                match fracPart with
                | ValueSome f ->
                    sb.Append '.' |> ignore

                    for c in f do
                        if c <> '_' then
                            sb.Append c |> ignore
                | ValueNone -> ()

                match expPart with
                | ValueSome(expSign, digits) ->
                    sb.Append 'e' |> ignore

                    if expSign = ValueSome '-' then
                        sb.Append '-' |> ignore

                    for c in digits do
                        if c <> '_' then
                            sb.Append c |> ignore
                | ValueNone -> ()

                return float (sb.ToString())
        }

    /// Special float values
    let private pSpecialFloat =
        choice
            [
                pstring "inf" >>% infinity
                pstring "+inf" >>% infinity
                pstring "-inf" >>% -infinity
                pstring "nan" >>% nan
                pstring "+nan" >>% nan
                pstring "-nan" >>% nan
            ]

    /// Any TOML float
    let private pFloatValue = pSpecialFloat <|> pFloat

    // ============================================================================
    // Boolean
    // ============================================================================

    let private pBoolean reader =
        ((pstring "true" >>% true) <|> (pstring "false" >>% false)) reader

    // ============================================================================
    // Date/Time
    // ============================================================================

    let private p2Digits =
        parser {
            let! d1 = pDecDigit
            let! d2 = pDecDigit
            return (int d1 - int '0') * 10 + (int d2 - int '0')
        }

    let private p4Digits =
        parser {
            let! d1 = pDecDigit
            let! d2 = pDecDigit
            let! d3 = pDecDigit
            let! d4 = pDecDigit

            return
                (int d1 - int '0') * 1000
                + (int d2 - int '0') * 100
                + (int d3 - int '0') * 10
                + (int d4 - int '0')
        }

    /// Date: YYYY-MM-DD
    let private pDate =
        parser {
            let! year = p4Digits
            let! _ = pchar '-'
            let! month = p2Digits
            let! _ = pchar '-'
            let! day = p2Digits

            return
                ({
                    Year = year
                    Month = month
                    Day = day
                }
                : TomlLocalDate)
        }

    /// Time: HH:MM:SS[.fraction]
    let private pTime =
        parser {
            let! hour = p2Digits
            let! _ = pchar ':'
            let! minute = p2Digits
            let! _ = pchar ':'
            let! second = p2Digits
            let! frac = opt (pchar '.' >>. many1Chars pDecDigit)

            let nanos =
                match frac with
                | ValueSome f ->
                    let padded =
                        if f.Length >= 9 then
                            f.Substring(0, 9)
                        else
                            f.PadRight(9, '0')

                    int padded
                | ValueNone -> 0

            return
                ({
                    Hour = hour
                    Minute = minute
                    Second = second
                    Nanosecond = nanos
                }
                : TomlLocalTime)
        }

    /// Timezone offset: Z/z or +/-HH:MM
    let private pOffset =
        choice
            [
                (pchar 'Z' <|> pchar 'z') >>% ValueSome 0
                parser {
                    let! sign = (pchar '+' >>% 1) <|> (pchar '-' >>% -1)
                    let! hours = p2Digits
                    let! _ = pchar ':'
                    let! minutes = p2Digits
                    return ValueSome(sign * (hours * 60 + minutes))
                }
            ]

    /// Offset date-time
    let private pOffsetDateTime =
        parser {
            let! date = pDate
            let! _ = pchar 'T' <|> pchar 't' <|> pchar ' '
            let! time = pTime
            let! offset = pOffset

            return
                TomlValue.OffsetDateTime
                    {
                        Year = date.Year
                        Month = date.Month
                        Day = date.Day
                        Hour = time.Hour
                        Minute = time.Minute
                        Second = time.Second
                        Nanosecond = time.Nanosecond
                        OffsetMinutes = offset
                    }
        }

    /// Local date-time
    let private pLocalDateTime =
        parser {
            let! date = pDate
            let! _ = pchar 'T' <|> pchar 't' <|> pchar ' '
            let! time = pTime

            return
                TomlValue.LocalDateTime
                    {
                        Year = date.Year
                        Month = date.Month
                        Day = date.Day
                        Hour = time.Hour
                        Minute = time.Minute
                        Second = time.Second
                        Nanosecond = time.Nanosecond
                        OffsetMinutes = ValueNone
                    }
        }

    /// Local date
    let private pLocalDate = pDate |>> TomlValue.LocalDate

    /// Local time
    let private pLocalTime = pTime |>> TomlValue.LocalTime

    // ============================================================================
    // Keys
    // ============================================================================

    /// Bare key: alphanumeric, underscore, dash
    let private pBareKey =
        many1Chars (
            satisfy (fun c ->
                (c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z')
                || (c >= '0' && c <= '9')
                || c = '_'
                || c = '-'
            )
        )

    /// Quoted key: basic or literal string
    let private pQuotedKey = pBasicString <|> pLiteralString

    /// Simple key: bare or quoted
    let private pSimpleKey = pBareKey <|> pQuotedKey

    /// Dotted key: simple keys separated by dots
    let private pDottedKey =
        sepBy1 (pWs >>. pSimpleKey .>> pWs) (pchar '.')
        |>> fun struct (keys, _) -> keys |> Seq.toList

    // ============================================================================
    // Values (forward reference for recursion)
    // ============================================================================

    let private pValueRef = RefParser<TomlValue, char, unit, _>()

    /// Array: [value, value, ...]
    let private pArray =
        parser {
            let! _ = pchar '['
            let! _ = pSkipWsAndComments
            let! values, _ = sepBy (pSkipWsAndComments >>. pValueRef.Parser .>> pSkipWsAndComments) (pchar ',')
            let! _ = optional (pchar ',')
            let! _ = pSkipWsAndComments
            let! _ = pchar ']'
            return TomlValue.Array(values |> Seq.toList)
        }

    /// Key-value pair for inline table
    let private pInlineKeyValue =
        parser {
            let! _ = pWs
            let! key = pSimpleKey
            let! _ = pWs
            let! _ = pchar '='
            let! _ = pWs
            let! value = pValueRef.Parser
            let! _ = pWs
            return (key, value)
        }

    /// Inline table: { key = value, key = value }
    let private pInlineTable =
        parser {
            let! _ = pchar '{'
            let! _ = pWs
            let! pairs, _ = sepBy pInlineKeyValue (pchar ',')
            let! _ = pWs
            let! _ = pchar '}'
            let table = pairs |> Seq.fold (fun t (k, v) -> Map.add k v t) Map.empty
            return TomlValue.InlineTable table
        }

    /// Any TOML value
    let private pValue =
        fun (reader: Reader<char, unit, _>) ->
            match reader.Peek() with
            | ValueSome '"' -> (pMlBasicString <|> pBasicString |>> TomlValue.String) reader
            | ValueSome '\'' -> (pMlLiteralString <|> pLiteralString |>> TomlValue.String) reader
            | ValueSome 't' -> (pstring "true" >>% TomlValue.Boolean true) reader
            | ValueSome 'f' -> (pstring "false" >>% TomlValue.Boolean false) reader
            | ValueSome '[' -> pArray reader
            | ValueSome '{' -> pInlineTable reader
            | ValueSome c when c = 'i' || c = 'n' -> (pSpecialFloat |>> TomlValue.Float) reader
            | ValueSome c when c = '+' || c = '-' || (c >= '0' && c <= '9') ->
                (choice
                    [
                        pOffsetDateTime
                        pLocalDateTime
                        pLocalDate
                        pLocalTime
                        pFloatValue |>> TomlValue.Float
                        pInteger |>> TomlValue.Integer
                    ])
                    reader
            | _ -> fail (Message "Expected value") reader

    do pValueRef.Set(pValue)

    // ============================================================================
    // Key-Value Pairs and Tables
    // ============================================================================

    /// Key-value pair: key = value
    let private pKeyValuePair =
        parser {
            let! keys = pDottedKey
            let! _ = pWs
            let! _ = pchar '='
            let! _ = pWs
            let! value = pValue
            return (keys, value)
        }

    /// Table header: [table.name]
    let private pTableHeader =
        parser {
            let! _ = pchar '['
            let! _ = pWs
            let! keys = pDottedKey
            let! _ = pWs
            let! _ = pchar ']'
            return keys
        }

    /// Array of tables header: [[table.name]]
    let private pArrayOfTablesHeader =
        parser {
            let! _ = pstring "[["
            let! _ = pWs
            let! keys = pDottedKey
            let! _ = pWs
            let! _ = pstring "]]"
            return keys
        }

    // ============================================================================
    // Document Parsing
    // ============================================================================

    type private Section =
        | TableSection of path: string list * pairs: (string list * TomlValue) list
        | ArrayOfTablesSection of path: string list * pairs: (string list * TomlValue) list

    /// Parse the entire document into sections
    let private pSections =
        parser {
            let! _ = pSkipBlankLines

            // Parse root key-value pairs
            let! rootPairs =
                many (
                    parser {
                        let! pair = pKeyValuePair
                        let! _ = pLineEnd
                        let! _ = pSkipBlankLines
                        return pair
                    }
                )

            // Parse table sections
            let! sections =
                many (
                    parser {
                        let! header =
                            (pArrayOfTablesHeader |>> fun k -> (k, true))
                            <|> (pTableHeader |>> fun k -> (k, false))

                        let! _ = pLineEnd
                        let! _ = pSkipBlankLines

                        let! pairs =
                            many (
                                parser {
                                    let! pair = pKeyValuePair
                                    let! _ = pLineEnd
                                    let! _ = pSkipBlankLines
                                    return pair
                                }
                            )

                        let (keys, isArray) = header

                        if isArray then
                            return ArrayOfTablesSection(keys, pairs |> Seq.toList)
                        else
                            return TableSection(keys, pairs |> Seq.toList)
                    }
                )

            let! _ = pWsComment
            let! _ = eof

            return (rootPairs |> Seq.toList, sections |> Seq.toList)
        }

    /// Set a nested value in a table
    let rec private setNested (keys: string list) (value: TomlValue) (table: TomlTable) : Result<TomlTable, string> =
        match keys with
        | [] -> Error "Empty key path"
        | [ key ] ->
            if Map.containsKey key table then
                Error $"Duplicate key: {key}"
            else
                Ok(Map.add key value table)
        | key :: rest ->
            match Map.tryFind key table with
            | Some(TomlValue.Table nested) ->
                match setNested rest value nested with
                | Ok newNested -> Ok(Map.add key (TomlValue.Table newNested) table)
                | Error e -> Error e
            | Some _ -> Error $"Cannot extend non-table key: {key}"
            | None ->
                match setNested rest value Map.empty with
                | Ok nested -> Ok(Map.add key (TomlValue.Table nested) table)
                | Error e -> Error e

    /// Ensure a table path exists
    let rec private ensureTablePath (keys: string list) (table: TomlTable) : Result<TomlTable, string> =
        match keys with
        | [] -> Ok table
        | key :: rest ->
            match Map.tryFind key table with
            | Some(TomlValue.Table nested) ->
                match ensureTablePath rest nested with
                | Ok newNested -> Ok(Map.add key (TomlValue.Table newNested) table)
                | Error e -> Error e
            | Some(TomlValue.InlineTable _) -> Error $"Cannot extend inline table: {key}"
            | Some _ -> Error $"Key is not a table: {key}"
            | None ->
                match ensureTablePath rest Map.empty with
                | Ok nested -> Ok(Map.add key (TomlValue.Table nested) table)
                | Error e -> Error e

    /// Add an array of tables entry
    let rec private addArrayOfTablesEntry
        (keys: string list)
        (tableValue: TomlTable)
        (doc: TomlTable)
        : Result<TomlTable, string> =
        match keys with
        | [] -> Error "Empty key path"
        | [ key ] ->
            match Map.tryFind key doc with
            | Some(TomlValue.Array arr) ->
                let newArr = arr @ [ TomlValue.Table tableValue ]
                Ok(Map.add key (TomlValue.Array newArr) doc)
            | Some _ -> Error $"Key already exists and is not an array: {key}"
            | None -> Ok(Map.add key (TomlValue.Array [ TomlValue.Table tableValue ]) doc)
        | key :: rest ->
            match Map.tryFind key doc with
            | Some(TomlValue.Table nested) ->
                match addArrayOfTablesEntry rest tableValue nested with
                | Ok newNested -> Ok(Map.add key (TomlValue.Table newNested) doc)
                | Error e -> Error e
            | Some(TomlValue.Array arr) when not (List.isEmpty arr) ->
                match List.last arr with
                | TomlValue.Table lastTable ->
                    match addArrayOfTablesEntry rest tableValue lastTable with
                    | Ok newLast ->
                        let newArr = (List.take (List.length arr - 1) arr) @ [ TomlValue.Table newLast ]
                        Ok(Map.add key (TomlValue.Array newArr) doc)
                    | Error e -> Error e
                | _ -> Error $"Array element is not a table: {key}"
            | Some _ -> Error $"Key is not a table or array of tables: {key}"
            | None ->
                match addArrayOfTablesEntry rest tableValue Map.empty with
                | Ok nested -> Ok(Map.add key (TomlValue.Table nested) doc)
                | Error e -> Error e

    /// Build the document from parsed sections
    let private buildDocument
        (rootPairs: (string list * TomlValue) list)
        (sections: Section list)
        : Result<TomlDocument, string> =
        let mutable doc: TomlTable = Map.empty
        let mutable error: string option = None

        // Add root pairs
        for (keys, value) in rootPairs do
            if error.IsNone then
                match setNested keys value doc with
                | Ok newDoc -> doc <- newDoc
                | Error e -> error <- Some e

        // Process sections
        for section in sections do
            if error.IsNone then
                match section with
                | TableSection(path, pairs) ->
                    match ensureTablePath path doc with
                    | Ok newDoc -> doc <- newDoc
                    | Error e -> error <- Some e

                    if error.IsNone then
                        for (keys, value) in pairs do
                            if error.IsNone then
                                match setNested (path @ keys) value doc with
                                | Ok newDoc -> doc <- newDoc
                                | Error e -> error <- Some e

                | ArrayOfTablesSection(path, pairs) ->
                    let mutable entryTable: TomlTable = Map.empty

                    for (keys, value) in pairs do
                        if error.IsNone then
                            match setNested keys value entryTable with
                            | Ok newTable -> entryTable <- newTable
                            | Error e -> error <- Some e

                    if error.IsNone then
                        match addArrayOfTablesEntry path entryTable doc with
                        | Ok newDoc -> doc <- newDoc
                        | Error e -> error <- Some e

        match error with
        | Some e -> Error e
        | None -> Ok doc

    /// Parse a TOML string into a document
    let parse (input: string) : Result<TomlDocument, string> =
        let reader = Reader.ofString input ()

        match pSections reader with
        | Ok(rootPairs, sections) -> buildDocument rootPairs sections
        | Error err ->
            let pos = err.Position.Index
            Error $"Parse error at position {pos}: {err.Errors}"
