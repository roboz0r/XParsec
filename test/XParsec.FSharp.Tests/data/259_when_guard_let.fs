module Test

// Simple when guard
let test1 (x: obj) =
    match x with
    | :? string as s when s.Length > 0 -> s
    | _ -> "other"

// Multiline when guard
let test2 (x: obj) =
    match x with
    | :? string as s when
        s.Length > 0
        ->
        s
    | _ -> "other"
