module Test

// Equality operator as first-class function
let eq = (=)
let result = [1;2;3] |> List.forall2 (=) [1;2;3]
