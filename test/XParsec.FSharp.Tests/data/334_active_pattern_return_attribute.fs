// Active pattern with `[<return: Struct>]` attribute on the return value.
// Used extensively in F# compiler (IlxGen.fs etc.) to avoid allocating
// Option<_> for partial active patterns.

let (|IsZero|_|) x =
    if x = 0 then Some() else None

[<return: Struct>]
let (|IsZeroStruct|_|) x =
    if x = 0 then ValueSome() else ValueNone

// And the `and`-bound form that appears in mutually recursive active patterns:
let rec foo x = bar x
and [<return: Struct>] (|BoolExpr|_|) expr =
    match expr with
    | true -> ValueSome true
    | false -> ValueSome false
and bar x = x + 1
