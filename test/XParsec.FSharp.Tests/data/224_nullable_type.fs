module Test

// Nullable reference type annotation (F# 9)
let x: (string | null) = null

// | binds tighter than * (tuple)
// int * string | null = int * (string | null)
let y: int * string | null = (1, null)

// | binds tighter than -> (function)
// int -> string | null = int -> (string | null)
let f: int -> string | null = fun _ -> null

// Nested in generic type args
let g: Map<string, int | null> = Map.empty

// Without parens
let h: string | null = null
