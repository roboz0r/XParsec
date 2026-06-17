namespace Vesper

#nowarn "42"

// JS-target intrinsic bindings for the integer primitives (intrinsic-runtime-type-plan.md).
// Platform (codegen/runtime) face only — the `canon` identity is the `.fsi` name itself,
// so each integer type keeps a distinct identity while the ≤32-bit ones all project to the
// JS `number` tag.
//
// `int64`/`uint64` cannot use `number` (a JS `number` is an IEEE-754 double and loses
// precision past 53 bits), so they project to the JS `bigint` tag — matching their literal
// emission (`5n`) and the `BigInt.asIntN(64, …)` arithmetic templates in `ops-platform.js.fs`.
// `bigint` (lowercase) is the `typeof`/TS-primitive tag, like `number`/`boolean`; `BigInt`
// is the constructor, not a value type.

type sbyte = (# "number" #)
type byte = (# "number" #)
type int16 = (# "number" #)
type uint16 = (# "number" #)
type uint32 = (# "number" #)
type int64 = (# "bigint" #)
type uint64 = (# "bigint" #)
