module TestModuleParenOp

let x = NonStructuralComparison.(=) 1 2
let y = Checked.(+) 1 2
let z = Operators.(|||) a b
let w = NullableOperators.( ?= ) x y

let f () =
    let result = Checked.(+) a b
    result
