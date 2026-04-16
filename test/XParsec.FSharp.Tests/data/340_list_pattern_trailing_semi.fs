// List pattern with trailing semicolon before closing bracket.
// Affects: CheckExpressions.fs
let test xs =
    match xs with
    | [1; 2;] -> "two"
    | [1; 2; 3;] -> "three"
    | _ -> "other"
