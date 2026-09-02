// A `[<Struct>]` union exercising every storage kind in one type: unmanaged scalars and a
// local struct record of scalars in the `_data` overlay, a string in the `object` slot, a
// struct record holding a string in an exact slot, a BCL value type the provider cannot
// enumerate (`Guid`) in another, and a case mixing an unmanaged field with a reference.
[<Struct>]
type Inner = { A: int; B: float }

[<Struct>]
type Tagged = { Label: string; N: int }

[<Struct>]
type Storage =
    | Scalars of x: int * y: bool
    | Nested of inner: Inner
    | Text of s: string
    | Labelled of t: Tagged
    | Id of id: System.Guid
    | Both of k: int * name: string

let score (m: Storage) : int =
    match m with
    | Scalars(x, y) -> if y then x else -x
    | Nested i -> i.A + int i.B
    | Text s -> s.Length
    | Labelled t -> t.N + t.Label.Length
    | Id _ -> -1
    | Both(k, name) -> k + name.Length

let a = Scalars(5, true)
let b = Nested { A = 2; B = 3.5 }
let c = Text "hello"
let d = Labelled { Label = "ab"; N = 10 }
let e = Both(7, "xyz")

printfn "%d" (score a)
printfn "%d" (score (Scalars(5, false)))
printfn "%d" (score b)
printfn "%d" (score c)
printfn "%d" (score d)
printfn "%d" (score e)
printfn "%b" (a = Scalars(5, true))
printfn "%b" (a = Scalars(5, false))
printfn "%b" (e = Both(7, "xyz"))
printfn "%b" (b = c)
