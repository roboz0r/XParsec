module Test

// ML-style type parameter (prefix position)
type 'T pickler = 'T -> int

// Multiple ML-style type params
type ('a, 'b) mapping = 'a -> 'b

// Regular F# style (should still work)
type Wrapper<'T> = { Value: 'T }
