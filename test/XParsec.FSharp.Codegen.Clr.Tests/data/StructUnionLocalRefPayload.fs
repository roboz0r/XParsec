// A generic `[<Struct>]` union carrying a `string` and a reference record declared in
// this compilation. Both are GC references, so both erase into the one `object` slot,
// while the case factories and the match arms keep the declared types.
type Node = { Label: string; Weight: int }

[<Struct>]
type Holder<'T> =
    | Val of v: 'T
    | Text of s: string
    | Rec of r: Node

let show (h: Holder<int>) : string =
    match h with
    | Val v -> if v = 3 then "three" else "other"
    | Text s -> s
    | Rec r -> r.Label

let a: Holder<int> = Val 3
let b: Holder<int> = Text "hi"
let c: Holder<int> = Rec { Label = "n"; Weight = 2 }

printfn "%s" (show a)
printfn "%s" (show b)
printfn "%s" (show c)
printfn "%b" (b = Text "hi")
printfn "%b" (b = c)
