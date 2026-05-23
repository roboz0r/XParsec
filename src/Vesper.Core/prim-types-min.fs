namespace Vesper

#nowarn "42" // This construct is deprecated: it is only for use in the F# library
open System

type int = Int32
type bool = Boolean
type Fun<'A, 'B> =
    abstract member Invoke : arg:'A -> 'B
type 'T ``[]`` = (# "!0[]" #)
type 'T array = 'T[]
type Unit = ValueTuple
type unit = Unit
