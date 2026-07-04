namespace Vesper

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

// Impl (`.fs`) side: the representation of each primitive as an inline-IL
// intrinsic string. This `.fs` is the per-target binding layer — retargeting a
// primitive (e.g. `int` to 64-bit) is a one-line edit here, no codegen change.

type int = (# "System.Int32" #)
type bool = (# "System.Boolean" #)
type unit = (# "System.ValueTuple" #)
type 'T ``[]`` = (# "!0[]" #)
type 'T array = 'T[]

type Fun<'A, 'B> =
    abstract member Invoke: arg: 'A -> 'B

type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
