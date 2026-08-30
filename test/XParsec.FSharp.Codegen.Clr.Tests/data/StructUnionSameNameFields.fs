// F# rejects same-name different-type fields across cases (FS3585) because its layout
// collapses same-name slots; the flat per-(case, index) layout gives each field its own
// slot, so the pair is representable here — deliberately more permissive than F#.
[<Struct>]
type Mixed =
    | I of x: int
    | S of x: string

let a = I 5
let b = S "hello"

let showI (m: Mixed) : int =
    match m with
    | I x -> x
    | S _ -> -1

let showS (m: Mixed) : string =
    match m with
    | S x -> x
    | I _ -> "?"

printfn "%d" (showI a)
printfn "%s" (showS b)
printfn "%b" (a = I 5)
printfn "%b" (a = b)
