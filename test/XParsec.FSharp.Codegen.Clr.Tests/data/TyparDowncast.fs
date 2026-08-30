// `:?>` and `:? _ as _` against an OPEN typar. `isinst` and `castclass` both leave a boxed
// reference, so a value-type instantiation needs `unbox.any` to reach the value. `castclass !!T`
// type-checks, verifies and JITs, then hands the caller the object address as its `T`.
let castTo (x: obj) : 'T = x :?> 'T

let orDefault (x: obj) (fallback: 'T) : 'T =
    match x with
    | :? 'T as v -> v
    | _ -> fallback
