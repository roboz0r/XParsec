module Test

// Chained multi-constraint static optimization, trimmed from FSharp.Core prim-types.fs
// Each `when` layers another type-specialized dispatch over the generic default.

let inline (+) (x: ^T) (y: ^U) : ^V =
    AdditionDynamic<(^T),(^U),(^V)>  x y
    when ^T : int32    and ^U : int32    = (# "add" x y : int32 #)
    when ^T : float    and ^U : float    = (# "add" x y : float #)
    when ^T : int64    and ^U : int64    = (# "add" x y : int64 #)
    when ^T : ^T = ((^T or ^U): (static member (+) : ^T * ^U -> ^V) (x,y))
