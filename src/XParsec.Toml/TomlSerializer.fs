/// TOML Serializer — converts TomlDocument/TomlValue back to valid TOML text.
///
/// Follows the DU→text pattern: match each TomlValue case, emit the appropriate
/// TOML representation. Round-trips correctly with TomlParser (modulo comments
/// and whitespace, which the AST does not preserve).
///
/// Design: Pure F#, no dependencies beyond the types in TomlTypes.fs.
namespace XParsec.Toml

open System
open System.Text

module TomlSerializer =

    // =========================================================================
    // Key Serialization
    // =========================================================================

    /// A bare key may contain only ASCII letters, digits, dashes, and underscores.
    let private isBareKey (key: string) : bool =
        key.Length > 0
        && key
           |> Seq.forall (fun c ->
               (c >= 'a' && c <= 'z')
               || (c >= 'A' && c <= 'Z')
               || (c >= '0' && c <= '9')
               || c = '-'
               || c = '_'
           )

    /// Serialize a TOML key. Bare keys pass through; others get basic-string quoting.
    let serializeKey (key: string) : string =
        if isBareKey key then
            key
        else
            let sb = StringBuilder()
            sb.Append('"') |> ignore

            for c in key do
                match c with
                | '\\' -> sb.Append("\\\\") |> ignore
                | '"' -> sb.Append("\\\"") |> ignore
                | '\n' -> sb.Append("\\n") |> ignore
                | '\r' -> sb.Append("\\r") |> ignore
                | '\t' -> sb.Append("\\t") |> ignore
                | c when c < '\u0020' && c <> '\t' -> sb.Append(sprintf "\\u%04X" (int c)) |> ignore
                | c -> sb.Append(c) |> ignore

            sb.Append('"') |> ignore
            sb.ToString()

    // =========================================================================
    // String Serialization
    // =========================================================================

    /// Escape a string for basic string context (inside double quotes).
    let private escapeBasicString (s: string) : string =
        let sb = StringBuilder(s.Length)

        for c in s do
            match c with
            | '\\' -> sb.Append("\\\\") |> ignore
            | '"' -> sb.Append("\\\"") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\u000C' -> sb.Append("\\f") |> ignore // formfeed
            | '\r' -> sb.Append("\\r") |> ignore
            | c when c < '\u0020' -> sb.Append(sprintf "\\u%04X" (int c)) |> ignore
            | c -> sb.Append(c) |> ignore

        sb.ToString()

    /// Serialize a string value. Uses basic strings for simple values,
    /// multiline basic strings for values containing newlines.
    let private serializeString (s: string) : string =
        if s.Contains('\n') || s.Contains('\r') then
            // Multiline basic string — escape everything except actual newlines
            let sb = StringBuilder()
            sb.Append("\"\"\"\n") |> ignore

            for c in s do
                match c with
                | '\\' -> sb.Append("\\\\") |> ignore
                | '"' -> sb.Append("\\\"") |> ignore
                | '\b' -> sb.Append("\\b") |> ignore
                | '\t' -> sb.Append("\\t") |> ignore
                | '\n' -> sb.Append('\n') |> ignore
                | '\u000C' -> sb.Append("\\f") |> ignore
                | '\r' -> sb.Append("\\r") |> ignore
                | c when c < '\u0020' -> sb.Append(sprintf "\\u%04X" (int c)) |> ignore
                | c -> sb.Append(c) |> ignore

            sb.Append("\"\"\"") |> ignore
            sb.ToString()
        else
            sprintf "\"%s\"" (escapeBasicString s)

    // =========================================================================
    // Number Serialization
    // =========================================================================

    /// Serialize an integer value.
    let private serializeInteger (i: int64) : string = sprintf "%d" i

    /// Serialize a float value, ensuring TOML-valid output.
    let private serializeFloat (f: float) : string =
        if Double.IsPositiveInfinity f then
            "inf"
        elif Double.IsNegativeInfinity f then
            "-inf"
        elif Double.IsNaN f then
            "nan"
        else
            let s = sprintf "%.17g" f
            // TOML requires floats to have a decimal point or exponent
            // to distinguish from integers
            if s.Contains('.') || s.Contains('e') || s.Contains('E') then
                s
            else
                s + ".0"

    // =========================================================================
    // DateTime Serialization
    // =========================================================================

    /// Format nanoseconds, trimming trailing zeros. Returns empty string if zero.
    let private formatNanos (nanos: int) : string =
        if nanos = 0 then
            ""
        else
            let s = sprintf "%09d" nanos
            let trimmed = s.TrimEnd('0')
            "." + trimmed

    /// Serialize a time component (HH:MM:SS[.nanos]).
    let private serializeTime (hour: int) (minute: int) (second: int) (nanos: int) : string =
        sprintf "%02d:%02d:%02d%s" hour minute second (formatNanos nanos)

    /// Serialize a date component (YYYY-MM-DD).
    let private serializeDate (year: int) (month: int) (day: int) : string = sprintf "%04d-%02d-%02d" year month day

    let private serializeOffsetDateTime (dt: TomlDateTime) : string =
        let date = serializeDate dt.Year dt.Month dt.Day
        let time = serializeTime dt.Hour dt.Minute dt.Second dt.Nanosecond

        let offset =
            match dt.OffsetMinutes with
            | ValueSome 0 -> "Z"
            | ValueSome mins ->
                let sign = if mins >= 0 then "+" else "-"
                let absMins = abs mins
                sprintf "%s%02d:%02d" sign (absMins / 60) (absMins % 60)
            | ValueNone -> "Z" // Shouldn't happen for OffsetDateTime, but safe default

        sprintf "%sT%s%s" date time offset

    let private serializeLocalDateTime (dt: TomlDateTime) : string =
        let date = serializeDate dt.Year dt.Month dt.Day
        let time = serializeTime dt.Hour dt.Minute dt.Second dt.Nanosecond
        sprintf "%sT%s" date time

    let private serializeLocalDate (d: TomlLocalDate) : string = serializeDate d.Year d.Month d.Day

    let private serializeLocalTime (t: TomlLocalTime) : string =
        serializeTime t.Hour t.Minute t.Second t.Nanosecond

    // =========================================================================
    // Value Serialization
    // =========================================================================

    /// Check if a TomlValue array is an array of tables (for [[header]] syntax).
    let private isArrayOfTables (items: TomlValue list) : bool =
        items.Length > 0
        && items
           |> List.forall (fun v ->
               match v with
               | TomlValue.Table _ -> true
               | _ -> false
           )

    /// Serialize a single value (inline representation, no key prefix).
    let rec serializeValue (value: TomlValue) : string =
        match value with
        | TomlValue.String s -> serializeString s
        | TomlValue.Integer i -> serializeInteger i
        | TomlValue.Float f -> serializeFloat f
        | TomlValue.Boolean b -> if b then "true" else "false"
        | TomlValue.OffsetDateTime dt -> serializeOffsetDateTime dt
        | TomlValue.LocalDateTime dt -> serializeLocalDateTime dt
        | TomlValue.LocalDate d -> serializeLocalDate d
        | TomlValue.LocalTime t -> serializeLocalTime t
        | TomlValue.Array items -> serializeArray items
        | TomlValue.InlineTable table -> serializeInlineTable table
        | TomlValue.Table table -> serializeInlineTable table // Fallback for inline context

    /// Serialize an array value.
    and private serializeArray (items: TomlValue list) : string =
        if items.IsEmpty then
            "[]"
        else
            let serialized = items |> List.map serializeValue
            let inline' = sprintf "[%s]" (String.concat ", " serialized)
            // Use multi-line format if the inline version is long
            if inline'.Length > 80 then
                let lines = serialized |> List.map (fun s -> sprintf "    %s," s)
                sprintf "[\n%s\n]" (String.concat "\n" lines)
            else
                inline'

    /// Serialize an inline table value.
    and private serializeInlineTable (table: TomlTable) : string =
        if Map.isEmpty table then
            "{ }"
        else
            let entries =
                table
                |> Map.toList
                |> List.map (fun (k, v) -> sprintf "%s = %s" (serializeKey k) (serializeValue v))

            sprintf "{ %s }" (String.concat ", " entries)

    // =========================================================================
    // Document Serialization
    // =========================================================================

    /// Classify a value as "simple" (emitted as key = value) or "complex" (needs section header).
    let private isSimpleValue (value: TomlValue) : bool =
        match value with
        | TomlValue.Table _ -> false
        | TomlValue.Array items when isArrayOfTables items -> false
        | _ -> true

    /// Emit a table's contents to a StringBuilder, using section headers for nested tables.
    /// Always emits LF (`\n`) line endings, not `Environment.NewLine` — TOML files are
    /// LF by convention and the round-trip tests assert exact byte content.
    let rec private emitTable (sb: StringBuilder) (path: string list) (table: TomlTable) : unit =
        let entries = Map.toList table

        // Phase 1: Emit simple key-value pairs
        for (key, value) in entries do
            if isSimpleValue value then
                sb.Append(serializeKey key).Append(" = ").Append(serializeValue value).Append('\n')
                |> ignore

        // Phase 2: Emit sub-tables with [section] headers
        for (key, value) in entries do
            match value with
            | TomlValue.Table subTable ->
                let subPath = path @ [ serializeKey key ]
                let header = String.concat "." subPath
                sb.Append('\n') |> ignore
                sb.Append('[').Append(header).Append("]\n") |> ignore
                emitTable sb subPath subTable
            | _ -> ()

        // Phase 3: Emit arrays of tables with [[section]] headers
        for (key, value) in entries do
            match value with
            | TomlValue.Array items when isArrayOfTables items ->
                let subPath = path @ [ serializeKey key ]
                let header = String.concat "." subPath

                for item in items do
                    match item with
                    | TomlValue.Table subTable ->
                        sb.Append('\n') |> ignore
                        sb.Append("[[").Append(header).Append("]]\n") |> ignore
                        emitTable sb subPath subTable
                    | _ -> () // Shouldn't happen — guarded by isArrayOfTables
            | _ -> ()

    /// Serialize a complete TOML document to a string.
    let serializeDocument (doc: TomlDocument) : string =
        let sb = StringBuilder()
        emitTable sb [] doc
        let result = sb.ToString()
        // Trim leading blank line if present (from empty root + first section)
        if result.StartsWith("\n") then
            result.Substring(1)
        else
            result
