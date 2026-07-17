namespace Vesper

#nowarn "42"

type undefined = (# "undefined" #)

// The value-level companion of the `undefined` type. JS `undefined` is a runtime
// global with no CLR analog, so this is the single honest source of the absence
// value — the type's value-level twin. Its body is EXACTLY one zero-operand intrinsic
// template, which the JS backend treats as a compile-time alias
// (`Inline.nullaryIntrinsicValueBody`): no lowered `const undefined = undefined`
// definition is emitted, and every reference splices the bare `undefined` the template
// carries. The inner `: undefined` ascription types the node — a bare `(# "undefined"
// #)` with no operands and no annotation would infer `unit` (and then fail to unify
// with the `undefined` return type). `[<AutoOpen>]` keeps the value usable unqualified,
// like the type.
[<AutoOpen>]
module Undefined =

    let undefined: undefined = (# "undefined": undefined #)
