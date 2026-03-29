module Test

type Expr =
    | Sequential of expr1: Expr * expr2: Expr
    | Literal of int

let test e =
    match e with
    | Sequential(expr1 = e1; expr2 = Sequential _ as e2) -> Some(e1, e2)
    | _ -> None

let test2 e =
    match e with
    | Sequential(expr1 = Literal 1 | Literal 2; expr2 = e2) -> Some e2
    | Sequential(expr1 = Literal _ | Sequential _ as e1; expr2 = _) -> Some e1
    | _ -> None
