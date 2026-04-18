module Repro

// FSharp.Core-internal surface syntax: DU cases whose case name is a
// parenthesized operator (op_Nil = `[]`, op_ColonColon = `::`), combined
// with GADT-style explicit case type signatures. See FSharp.Core/prim-types.fs
// Microsoft.FSharp.Core.List<'T>.

type List<'T> =
    | ([])  :                  'T list
    | ( :: )  : Head: 'T * Tail: 'T list -> 'T list
