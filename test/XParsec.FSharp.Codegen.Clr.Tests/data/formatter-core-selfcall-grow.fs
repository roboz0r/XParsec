open System
open System.Buffers
open System.Globalization

[<Struct; IsByRefLike>]
type Handler =
    static let MinimumArrayPoolLength = 256
    static let MaxChars = 0x3FFFFFDF
    static let provider: IFormatProvider = CultureInfo.InvariantCulture
    val mutable private Pool: char[]
    val mutable private Chars: Span<char>
    val mutable private Pos: int

    new(literalLength: int, formattedCount: int) =
        let buf =
            ArrayPool<char>.Shared.Rent(Math.Max(256, literalLength + formattedCount * 11))

        {
            Pool = buf
            Chars = Span<char>(buf)
            Pos = 0
        }

    member this.AppendLiteral(value: string) =
        if value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos)) then
            this.Pos <- this.Pos + value.Length
        else
            this.GrowThenCopyString(value)

    member this.AppendFormatted(value: 'T) =
        let o = box value

        let s =
            match o with
            | :? IFormattable as f -> f.ToString(null, provider)
            | null -> ""
            | _ -> o.ToString()

        this.AppendLiteral(s)

    member this.ToStringAndClear() : string =
        let result = this.Chars.Slice(0, this.Pos).ToString()
        this.Clear()
        result

    member private this.Clear() =
        let toReturn = this.Pool
        this.Pool <- null
        this.Pos <- 0

        match toReturn with
        | null -> ()
        | arr -> ArrayPool<char>.Shared.Return(arr)

    member private this.GrowThenCopyString(value: string) =
        this.Grow(value.Length)
        let _ok = value.TryCopyTo(this.Chars.Slice(this.Pos, this.Chars.Length - this.Pos))
        this.Pos <- this.Pos + value.Length

    member private this.Grow(additionalChars: int) =
        this.GrowCore(this.Pos + additionalChars)

    member private this.GrowCore(requiredMinCapacity: int) =
        let newCapacity =
            Math.Max(requiredMinCapacity, Math.Min(this.Chars.Length * 2, MaxChars))

        let arraySize = Math.Max(newCapacity, MinimumArrayPoolLength)
        let newArray = ArrayPool<char>.Shared.Rent(arraySize)
        this.Chars.Slice(0, this.Pos).CopyTo(Span<char>(newArray))
        let toReturn = this.Pool
        this.Pool <- newArray
        this.Chars <- Span<char>(newArray)

        match toReturn with
        | null -> ()
        | arr -> ArrayPool<char>.Shared.Return(arr)

let basic () =
    let mutable f = Handler(0, 2)
    f.AppendLiteral("x=")
    f.AppendFormatted(42)
    f.AppendLiteral(", pi=")
    f.AppendFormatted(3.14)
    f.ToStringAndClear()

let grown () =
    let mutable f = Handler(0, 1)
    let mutable i = 0

    while i < 200 do
        f.AppendLiteral("ab")
        i <- i + 1

    let s = f.ToStringAndClear()
    s.Length

printfn "%s" (basic ())
printfn "%d" (grown ())
