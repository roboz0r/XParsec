open System
open System.Globalization

let provider: IFormatProvider = CultureInfo.InvariantCulture

let formatPrimitive (value: obj) : string =
    let s =
        match value with
        | :? IFormattable as f -> f.ToString(null, provider)
        | _ -> value.ToString()

    let suffix =
        match value with
        | :? sbyte -> "y"
        | :? byte -> "uy"
        | :? int16 -> "s"
        | :? uint16 -> "us"
        | :? uint32 -> "u"
        | :? int64 -> "L"
        | :? uint64 -> "UL"
        | :? decimal -> "M"
        | _ -> ""

    s + suffix

printfn "%s" (formatPrimitive (box 5y))
printfn "%s" (formatPrimitive (box 5uy))
printfn "%s" (formatPrimitive (box 5s))
printfn "%s" (formatPrimitive (box 5us))
printfn "%s" (formatPrimitive (box 5u))
printfn "%s" (formatPrimitive (box 5L))
printfn "%s" (formatPrimitive (box 5UL))
printfn "%s" (formatPrimitive (box 5M))
