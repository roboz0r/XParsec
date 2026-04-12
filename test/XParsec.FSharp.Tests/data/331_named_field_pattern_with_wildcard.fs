// Mixed named-field + wildcard pattern in DU / ctor patterns.
// Used extensively in the F# compiler source to deconstruct one
// named field from a DU case with many fields.

type Ctor = Ctor of a: int * b: string * c: bool

let f x =
    match x with
    | Ctor(a = value, _, _) -> value
    | _ -> 0

// Nested named-field patterns (FileContentMapping.fs style)
type Wrap = W of field: Ctor * rest: int

let g y =
    match y with
    | W(field = Ctor(a = av, _, _); rest = r) -> av + r
