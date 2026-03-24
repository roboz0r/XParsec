module Test

type Item =
    | Property of info: string list * tag: int

let test item =
    match item with
    | Property(info = x :: _, tag = t) -> x
    | Property(info = []) -> "none"
    | _ -> "other"
