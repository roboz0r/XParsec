// A `[<Struct>]` union covering all three unmanagedness classifications: scalars and a local
// struct record (unmanaged), a string (managed), and BCL value types the provider enumerates
// no fields for (undetermined). Analysed by `UnmanagednessTests`' census only.
[<Struct>]
type Inner = { A: int; B: float }

[<Struct>]
type External =
    | Scalars of x: int * y: bool
    | Nested of inner: Inner
    | Text of s: string
    | Id of id: System.Guid
    | Stamp of at: System.DateTime

let describe (p: External) : int =
    match p with
    | Scalars(x, _) -> x
    | Nested i -> i.A
    | Text _ -> -1
    | Id _ -> -2
    | Stamp _ -> -3

printfn "%d" (describe (Scalars(1, true)))
