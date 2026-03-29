module Test

type App = App of isInfix: bool * funcExpr: string * argExpr: int

let test x =
    match x with
    | App(
        isInfix = false
        funcExpr = f
        argExpr = a) -> (f, a)
    | App(isInfix = true; funcExpr = f; argExpr = a) -> (f, a)
    | _ -> ("", 0)
