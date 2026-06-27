namespace XParsec.Json

open System.Text
open System.Globalization

/// Serialises a `JsonValue` back to JSON text. `XParsec.Json` is otherwise
/// parse-only; this is the inverse, so the same `JsonValue` model round-trips.
/// Used by the TS-manifest extractor to emit, and the loader could re-emit for
/// golden-diffing. Kept dependency-light (StringBuilder + FSharp.Core) so it
/// compiles under Fable as well as .NET.
[<RequireQualifiedAccess>]
module JsonWriter =

    let private escapeInto (sb: StringBuilder) (s: string) =
        sb.Append('"') |> ignore

        for c in s do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\f' -> sb.Append("\\f") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when System.Char.IsControl c ->
                sb.Append("\\u").Append((int c).ToString("x4", CultureInfo.InvariantCulture))
                |> ignore
            | c -> sb.Append(c) |> ignore

        sb.Append('"') |> ignore

    let private numberToString (n: float) : string =
        if System.Double.IsNaN n || System.Double.IsInfinity n then
            "null" // JSON has no NaN/Infinity; emit null rather than invalid JSON.
        elif System.Math.Floor n = n && abs n < 1e15 then
            (int64 n).ToString(CultureInfo.InvariantCulture)
        else
#if FABLE_COMPILER
            // JS Number.toString is invariant and round-trippable; .NET's "R"
            // specifier isn't supported by Fable.
            string n
#else
            n.ToString("R", CultureInfo.InvariantCulture)
#endif

    let rec private writeInto (sb: StringBuilder) (indent: int) (level: int) (value: JsonValue) =
        let newlineAt depth =
            if indent > 0 then
                sb.Append('\n').Append(System.String(' ', indent * depth)) |> ignore

        match value with
        | JsonValue.String s -> escapeInto sb s
        | JsonValue.Number n -> sb.Append(numberToString n) |> ignore
        | JsonValue.True -> sb.Append("true") |> ignore
        | JsonValue.False -> sb.Append("false") |> ignore
        | JsonValue.Null -> sb.Append("null") |> ignore
        | JsonValue.Array items ->
            if items.Length = 0 then
                sb.Append("[]") |> ignore
            else
                sb.Append('[') |> ignore

                for i in 0 .. items.Length - 1 do
                    if i > 0 then
                        sb.Append(',') |> ignore

                    newlineAt (level + 1)
                    writeInto sb indent (level + 1) items.[i]

                newlineAt level
                sb.Append(']') |> ignore
        | JsonValue.Object members ->
            if members.Length = 0 then
                sb.Append("{}") |> ignore
            else
                sb.Append('{') |> ignore

                for i in 0 .. members.Length - 1 do
                    if i > 0 then
                        sb.Append(',') |> ignore

                    newlineAt (level + 1)
                    let m = members.[i]
                    escapeInto sb m.Name
                    sb.Append(if indent > 0 then ": " else ":") |> ignore
                    writeInto sb indent (level + 1) m.Value

                newlineAt level
                sb.Append('}') |> ignore

    /// Compact, single-line JSON.
    let write (value: JsonValue) : string =
        let sb = StringBuilder(256)
        writeInto sb 0 0 value
        sb.ToString()

    /// Pretty-printed JSON, `indent` spaces per nesting level.
    let writeIndented (indent: int) (value: JsonValue) : string =
        let sb = StringBuilder(256)
        writeInto sb indent 0 value
        sb.ToString()
