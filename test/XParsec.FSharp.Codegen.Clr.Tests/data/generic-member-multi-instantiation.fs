open System
open System.Globalization

[<Struct; IsByRefLike>]
type Holder(seed: int) =
    static let provider: IFormatProvider = CultureInfo.InvariantCulture

    member this.AppendFormatted(value: 'T) : string =
        let o = box value

        match o with
        | :? IFormattable as f -> f.ToString(null, CultureInfo.InvariantCulture)
        | null -> ""
        | _ -> o.ToString()

let h = Holder(0)
printfn "%s" (h.AppendFormatted 42)
printfn "%s" (h.AppendFormatted 3.14)
printfn "%s" (h.AppendFormatted "hi")
