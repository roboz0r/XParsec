open System.Runtime.CompilerServices
open System
open System.Globalization

[<Struct; IsByRefLike>]
type Box(seed: int) =
    static let provider: IFormatProvider = CultureInfo.InvariantCulture

    member this.Fmt(value: 'T) : string =
        let o = box value

        match o with
        | :? IFormattable as f -> f.ToString(null, provider)
        | null -> ""
        | _ -> o.ToString()

    member this.Fmt(value: 'T, format: string) : string =
        let o = box value

        match o with
        | :? IFormattable as f -> f.ToString(format, provider)
        | null -> ""
        | _ -> o.ToString()

    member this.Fmt(value: 'T, pad: int) : string =
        let o = box value

        match o with
        | :? IFormattable as f -> f.ToString(null, provider)
        | null -> ""
        | _ -> o.ToString()

let b = Box(0)
printfn "%s" (b.Fmt 42)
