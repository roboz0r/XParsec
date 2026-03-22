module Test

// not null constraint
let f (x: 'a when 'a: not null) = x

// null constraint
let g (x: 'a when 'a: null) = x

// struct constraint
let h (x: 'a when 'a: struct) = x

// multiple constraints with 'and'
let i (x: 'a when 'a: not null and 'a: comparison) = x
