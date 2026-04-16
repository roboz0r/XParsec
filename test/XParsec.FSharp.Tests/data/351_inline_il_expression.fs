// Inline IL expression `(# "op" args : type #)` — FSharp.Core internal syntax
// for emitting raw IL opcodes. Only valid inside FSharp.Core.
// Affects: prim-types.fs
type Attr() =
    member _.Flags = 0

let Flag_PermitNull = 8

let test (reprAttr: Attr) =
    if (# "and" reprAttr.Flags Flag_PermitNull : int #) = 0
    then 1
    else 2

let testUnbox (o: obj) : int =
    (# "unbox.any !0" type (int) o : int #)
