open System.Runtime.CompilerServices
open System
open System.IO
open System.Globalization

let GuessedLengthPerHole = 11
let MinimumArrayPoolLength = 256

[<Struct; IsByRefLike>]
type FieldBlock =
    static let provider: IFormatProvider = CultureInfo.InvariantCulture
    val mutable private Writer: TextWriter
    val mutable private Pool: char[]
    val mutable private Chars: Span<char>
    val mutable private Pos: int

    new(w: TextWriter, buf: char[]) =
        {
            Writer = w
            Pool = buf
            Chars = Span<char>(buf)
            Pos = 0
        }

    member this.HasWriter =
        match this.Writer with
        | null -> false
        | _ -> true

    member this.Cap = this.Chars.Length

    member this.PoolLen =
        match this.Pool with
        | null -> -1
        | arr -> arr.Length

    member this.ProviderOk =
        match box provider with
        | null -> false
        | _ -> true

let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]
let a = FieldBlock(Console.Out, arr)
let b = FieldBlock(null, null)
printfn "%b" a.HasWriter
printfn "%d" a.Cap
printfn "%d" a.PoolLen
printfn "%b" b.HasWriter
printfn "%d" b.PoolLen
printfn "%b" a.ProviderOk
