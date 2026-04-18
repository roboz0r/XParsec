// SRTP static-optimization clause with a bare `struct` constraint (no `: Type`)
// and a multi-line `match` body. From FSharp.Core/prim-types.fs
// `anyToString` at Ln ~5197.
//
// Known-good path (see data/361_inline_il_expression.fs): `when 'T : Type = expr`.
// This fixture exercises two differences from that path:
//   1. The constraint is bare `struct`, not `'T : SomeType`.
//   2. The RHS is a multi-line `match` expression whose arms undent relative
//      to the `when` column.
//
// Affects: prim-types.fs (2 errors at Ln 5198/5199 — MissingModuleElem +
// UnexpectedTopLevel cascade to EOF).

let inline anyToString nullStr (value: 'T) =
    GenericIntrinsic value
    when 'T : int     = "int"
    when 'T struct    =
       match box value with
       | :? System.IFormattable as f -> f.ToString()
       | _ -> value.ToString()
    when 'T : string  = (# "" value : string #)
