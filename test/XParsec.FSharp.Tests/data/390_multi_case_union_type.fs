module Test

// Anonymous structural union with >2 cases (left-associative chain)
let a: int | string | bool = Unchecked.defaultof<_>

// With null / undefined members
let b: int | string | null = null

// As a parameter annotation
let f (x: int | string | bool) = x

// Nested in generic type args
let g: Map<string, int | string | bool> = Map.empty

// Mixed with tuple precedence: | binds tighter than *
let h: int * string | bool | int = Unchecked.defaultof<_>
