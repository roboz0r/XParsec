open System
open System.Globalization

let provider: IFormatProvider = CultureInfo.InvariantCulture

let rec hasDot (s: string) (i: int) : bool =
    if i >= s.Length then
        false
    else
        let c = s.[i]

        if c = '.' || c = 'e' || c = 'E' then
            true
        else
            hasDot s (i + 1)

let fixFloat (s: string) (finite: bool) (suffix: string) : string =
    if not finite then
        if s = "NaN" then "nan" + suffix
        elif s = "Infinity" then "infinity" + suffix
        elif s = "-Infinity" then "-infinity" + suffix
        else s
    elif hasDot s 0 then
        s + suffix
    else
        s + ".0" + suffix

let formatFloat (value: obj) : string =
    match value with
    | :? double as d -> fixFloat (d.ToString(null, CultureInfo.InvariantCulture)) (Double.IsFinite d) ""
    | :? single as f -> fixFloat (f.ToString(null, provider)) (Single.IsFinite f) "f"
    | _ -> value.ToString()

let appendEscaped (acc: string) (c: char) (quote: char) : string =
    if c = '\\' then acc + "\\\\"
    elif c = '\n' then acc + "\\n"
    elif c = '\r' then acc + "\\r"
    elif c = '\t' then acc + "\\t"
    elif c = quote then acc + "\\" + c.ToString()
    else acc + c.ToString()

let rec escapeInto (acc: string) (s: string) (i: int) (quote: char) : string =
    if i >= s.Length then
        acc
    else
        escapeInto (appendEscaped acc s.[i] quote) s (i + 1) quote

let quoteString (s: string) : string = "\"" + escapeInto "" s 0 '\"' + "\""
let quoteChar (c: char) : string = "'" + appendEscaped "" c '\'' + "'"
printfn "%s" (formatFloat (box 3.0))
printfn "%s" (formatFloat (box 3.5))
printfn "%s" (formatFloat (box (0.0 / 0.0)))
printfn "%s" (formatFloat (box 3.0f))
printfn "%s" (formatFloat (box (0.0f / 0.0f)))
printfn "%s" (formatFloat (box (1.0f / 0.0f)))
printfn "%s" (quoteString "a\"b\nc")
printfn "%s" (quoteChar 'c')
printfn "%s" (quoteChar '\n')
