module TestDynamicLookup

// Basic dynamic member lookup (e.g. FSharp.Data row accessor)
let name = row?Name

// Dynamic setter (composes via Assignment over DynamicLookup)
let setIt () = obj?Member <- 42

// Chained dynamic lookups
let chained = obj?A?B

// Mixed with dot member access
let mixed = obj.foo?Bar

// Right side of a pipe should also work
let piped = data |> fun r -> r?Value
