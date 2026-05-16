// Active pattern invoked with a typed quotation `<@ ... @>` as a curried
// argument in match-arm pattern position. Spec patterns:60-61 — the quotation
// is parsed as an atomic pattern; the type checker reinterprets it back to an
// expression at active-pattern parameter binding time.
let classify e =
    match e with
    | (|Const|_|) <@ 1 + 1 @> x -> Some x
    | _ -> None
