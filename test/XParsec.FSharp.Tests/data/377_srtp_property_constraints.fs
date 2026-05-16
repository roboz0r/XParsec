module Test

// SRTP member-trait constraints with property accessors (with get / with set).
// pars.fsy member-sig production includes `with get`, `with set`, `with get,set`,
// and `with set,get`. See recommended-changes.md §1.4.

// Read-only property trait
let inline lengthOf (x: ^T) =
    (^T: (member Length: int with get) x)

// Set-only property trait
let inline setName (x: ^T) (v: string) =
    (^T: (member Name: string with set) (x, v))

// Read-write, both orders
let inline value1 (x: ^T) =
    (^T: (member Value: int with get, set) x)

let inline value2 (x: ^T) =
    (^T: (member Value: int with set, get) x)

// Static member property trait
let inline defaultOf< ^T when ^T: (static member Default: ^T with get)> () =
    (^T: (static member Default: ^T with get) ())

// Indexer-shaped (function-typed) signature with getter
let inline itemAt (x: ^T) (i: int) =
    (^T: (member Item: int -> 'V with get) (x, i))
