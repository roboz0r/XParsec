namespace Vesper

open System.Collections.Generic

[<AutoOpen>]
module ArithmeticOperators =

    let inline (+) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (-) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) (x, y))

    /// Written `( * )` — `(*` opens a block comment.
    let inline ( * ) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (/) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (/): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (%) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (%): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (~-) (n: ^T) : ^T = (^T: (static member (~-): ^T -> ^T) n)
    let inline (~+) (value: ^T) : ^T = (^T: (static member (~+): ^T -> ^T) value)

[<AutoOpen>]
module EqualityOperators =

    let inline (=) (x: ^T) (y: ^T) : bool =
        EqualityComparer< ^T >.Default.Equals(x, y)
        when ^T: int = (# "ceq" x y : bool #)
        when ^T: int64 = (# "ceq" x y : bool #)
        when ^T: float = (# "ceq" x y : bool #)
        when ^T: float32 = (# "ceq" x y : bool #)
        when ^T: bool = (# "ceq" x y : bool #)
        when ^T: char = (# "ceq" x y : bool #)
        when ^T: byte = (# "ceq" x y : bool #)

    let inline (<>) (x: ^T) (y: ^T) : bool =
        (# "ceq" (EqualityComparer< ^T >.Default.Equals(x, y)) false : bool #)
        when ^T: int = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: int64 = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: float = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: float32 = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: bool = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: char = (# "ceq" (# "ceq" x y : bool #) false : bool #)
        when ^T: byte = (# "ceq" (# "ceq" x y : bool #) false : bool #)

[<AutoOpen>]
module Operators =

    let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj

    let inline not (value: bool) : bool = (# "ceq" value false : bool #)

    let inline ignore (value: 'T) : unit = ()

    let inline isNull (value: 'T when 'T: null) : bool = (# "ceq" value null : bool #)

    let inline box (value: 'T) : obj = (# "box !0" type ('T) value : obj #)

    /// A same-width `int32`/`uint32` conversion is `(# "" … #)` — a sign-only reinterpret,
    /// a stack no-op per ECMA-335 III §1.5.
    let inline uint32 (value: ^T) : uint32 =
        (# "conv.u4" value : uint32 #)
        when ^T: int32 = (# "" value : uint32 #)
        when ^T: uint32 = (# "" value : uint32 #)
        when ^T: int64 = (# "conv.u4" value : uint32 #)
        when ^T: uint64 = (# "conv.u4" value : uint32 #)
        when ^T: float = (# "conv.u4" value : uint32 #)
        when ^T: float32 = (# "conv.u4" value : uint32 #)
        when ^T: char = (# "conv.u4" value : uint32 #)
        when ^T: byte = (# "conv.u4" value : uint32 #)

    let inline uint (value: ^T) : uint32 = uint32 value

    let inline int32 (value: ^T) : int32 =
        (# "conv.i4" value : int32 #)
        when ^T: int32 = (# "" value : int32 #)
        when ^T: uint32 = (# "" value : int32 #)
        when ^T: int64 = (# "conv.i4" value : int32 #)
        when ^T: uint64 = (# "conv.i4" value : int32 #)
        when ^T: float = (# "conv.i4" value : int32 #)
        when ^T: float32 = (# "conv.i4" value : int32 #)
        when ^T: char = (# "conv.i4" value : int32 #)
        when ^T: byte = (# "conv.i4" value : int32 #)

    let inline int (value: ^T) : int = int32 value

    /// `BigInteger` is not a CIL primitive, so the widening is a BCL call.
    let inline bigint (value: int32) : bigint = System.Numerics.BigInteger.op_Implicit(value)

    let inline raise (e: 'TException) : 'T = (# "throw" e : 'T #)

    /// The explicit `new` is required — a bare `System.Exception(msg)` is only an application.
    let inline failwith (message: string) : 'T = raise (new System.Exception(message))

    /// The BCL ctor takes `(message, paramName)` — hence the swap against this signature.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        raise (new System.ArgumentException(message, argumentName))

[<AutoOpen>]
module IndexIntrinsics =

    let inline GetIndex (target: 'T) (key: 'K) : 'V = failwith "GetIndex is a JS-target intrinsic"

    let inline SetIndex (target: 'T) (key: 'K) (value: 'V) : unit =
        failwith "SetIndex is a JS-target intrinsic"

module Unchecked =

    let inline defaultof<'T> : 'T = (# "ilzero" type ('T) : 'T #)
