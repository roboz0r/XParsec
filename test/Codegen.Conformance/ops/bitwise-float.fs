// A bitwise operator on a non-integral operand. `float` declares no `&&&`, so this is
// an ordinary "type does not support the operator" rejection — the same verdict, and
// for the same reason, as `~-` on an unsigned width in `arith-negate-unsigned.fs`.
//
// It used to compile silently and emit CIL `and` over two float64s: the trait synthesis
// admitted every numeric name and the operator's single IL body asked no questions.
let x = 1.0 &&& 2.0
ignore x
