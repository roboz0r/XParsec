module TestNegativeLiteralPatternContexts

// After comma in tuple pattern
let g x =
    match x with
    | (-1, _) -> "a"
    | _ -> "b"

// In list pattern
let h x =
    match x with
    | [ -1; -2 ] -> "a"
    | _ -> "b"

// As-pattern
let i x =
    match x with
    | -1 as n -> n
    | n -> n

// Or-pattern with negative literals on both sides
let j x =
    match x with
    | -1
    | -2 -> "a"
    | _ -> "b"

// Cons pattern
let k x =
    match x with
    | -1 :: _ -> "a"
    | _ -> "b"

// Active-pattern argument in pattern position
let (|Neg|_|) n = if n < 0 then Some -n else None

let m x =
    match x with
    | Neg -1 -> "a"
    | _ -> "b"

// function-keyword
let n =
    function
    | -1 -> "a"
    | _ -> "b"

// Negative float
let p x =
    match x with
    | -1.5 -> "a"
    | _ -> "b"

// Record-field pattern with negative literal value
type R = { v: int }

let r x =
    match x with
    | { v = -1 } -> "a"
    | _ -> "b"

// Function argument pattern (atomic binding arg position)
let s -1 = "a"
