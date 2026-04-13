// Parameterized active pattern invocation in match arms.
// Affects: CheckComputationExpressions.fs, quotations.fs
let rec (|NLambdas|_|) n (e: obj) = None

let test e =
    match e with
    | NLambdas ((-) n 1) (vs, b) -> Some(vs, b)
    | _ -> None
