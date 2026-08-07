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
module BitwiseOperators =

    let inline (&&&) (x: ^T) (y: ^T) : ^T = (^T: (static member (&&&): ^T * ^T -> ^T) (x, y))

    let inline (|||) (x: ^T) (y: ^T) : ^T = (^T: (static member (|||): ^T * ^T -> ^T) (x, y))

    let inline (^^^) (x: ^T) (y: ^T) : ^T = (^T: (static member (^^^): ^T * ^T -> ^T) (x, y))

    let inline (~~~) (value: ^T) : ^T = (^T: (static member (~~~): ^T -> ^T) value)

    let inline (<<<) (value: ^T) (shift: int32) : ^T = (^T: (static member (<<<): ^T * int32 -> ^T) (value, shift))

    let inline (>>>) (value: ^T) (shift: int32) : ^T = (^T: (static member (>>>): ^T * int32 -> ^T) (value, shift))

[<AutoOpen>]
module EqualityOperators =

    /// The aggregate base and `hash` share one comparer, so equal values hash equal.
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

    /// `!0` is the element-type placeholder; codegen emits `box <T>` from the
    /// argument's static type.
    let inline box (value: 'T) : obj = (# "box !0" type ('T) value : obj #)

    /// A same-width `int32`→`uint32` is `(# "" … #)` — a sign-only reinterpret, a stack
    /// no-op per ECMA-335 III §1.5.
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

    /// A same-width `uint32`→`int32` is `(# "" … #)` — a sign-only reinterpret, a stack
    /// no-op per ECMA-335 III §1.5.
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

    /// `BigInteger` is not a CIL primitive, so the widening is a BCL call — `op_Implicit`,
    /// since the BCL offers no non-special-name sibling for it.
    let inline bigint (value: int32) : bigint = System.Numerics.BigInteger.op_Implicit(value)

    /// The desugaring target for `arr.[i]`.
    let inline GetArray (array: 'T[]) (index: int) : 'T = (# "ldelem.any !0" type ('T) array index : 'T #)

    /// The desugaring target for `arr.[i] <- value`.
    let inline SetArray (array: 'T[]) (index: int) (value: 'T) : unit =
        (# "stelem.any !0" type ('T) array index value : unit #)

    /// The desugaring target for `arr.Length`.
    let inline GetArrayLength (array: 'T[]) : int = (# "ldlen" array : int #)

    /// `throw` terminates the path, so the `'T` result is never realised.
    let inline raise (e: 'TException) : 'T = (# "throw" e : 'T #)

    /// The explicit `new` is required: bare `System.Exception(msg)` parses as an
    /// application, not a construction.
    let inline failwith (message: string) : 'T = raise (new System.Exception(message))

    /// Argument order follows FSharp.Core — name first, message second — while the BCL
    /// ctor takes `(message, paramName)`.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        raise (new System.ArgumentException(message, argumentName))

[<AutoOpen>]
module StringIntrinsics =

    /// Contract-only here: `s.[i]` resolves to the BCL `get_Chars`, which the front end
    /// prefers over this intrinsic — so the body is not recursive.
    let inline GetString (s: string) (index: int) : char = s.[index]

[<AutoOpen>]
module IndexIntrinsics =

    /// Contract-only here: a .NET indexer resolves through `get_Item`/`set_Item`
    /// metadata, so `x.[k]` never routes to this body.
    let inline GetIndex (target: 'T) (key: 'K) : 'V = failwith "GetIndex is a JS-target intrinsic"

    let inline SetIndex (target: 'T) (key: 'K) (value: 'V) : unit =
        failwith "SetIndex is a JS-target intrinsic"

module Unchecked =

    /// The zero-operand `ilzero` intrinsic, spliced at each reference: the CLR backend
    /// lowers it to a zeroed scratch local (`ldloca; initobj; ldloc`) — null for a
    /// reference type, all-zeroes for a value type.
    let inline defaultof<'T> : 'T = (# "ilzero" type ('T) : 'T #)
