open System.Runtime.CompilerServices

[<Struct; IsByRefLike>]
type Res =
    member this.Dispose() = printfn "disposed"

let run () =
    use r = Res()
    printfn "body"

run ()
