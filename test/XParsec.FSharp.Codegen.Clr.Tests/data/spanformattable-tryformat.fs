open System
open System.Buffers
open System.Globalization

[<Struct; IsByRefLike>]
type Holder(seed: int) =
    static let provider: IFormatProvider = CultureInfo.InvariantCulture

    member this.AppendFormatted(value: 'T) : string =
        let buf = ArrayPool<char>.Shared.Rent(64)
        let dest = Span<char>(buf)
        let o = box value

        let result =
            match o with
            | :? ISpanFormattable as sf ->
                let mutable cw = 0

                if sf.TryFormat(dest, &cw, ReadOnlySpan<char>.Empty, provider) then
                    dest.Slice(0, cw).ToString()
                else
                    ""
            | :? IFormattable as f -> f.ToString(null, provider)
            | null -> ""
            | _ -> o.ToString()

        ArrayPool<char>.Shared.Return(buf)
        result

let h = Holder(0)
printfn "%s" (h.AppendFormatted 42)
printfn "%s" (h.AppendFormatted 3.14)
printfn "%s" (h.AppendFormatted "hi")
