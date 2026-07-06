open System
open System.Globalization

let format (value: 'T) : string =
    let o = box value

    match o with
    | :? IFormattable as f -> f.ToString(null, CultureInfo.InvariantCulture)
    | null -> ""
    | _ -> o.ToString()

printfn "%s" (format 42)
printfn "%s" (format 3.14)
