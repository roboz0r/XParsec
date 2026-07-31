namespace Vesper

open System.Collections.Generic

[<AutoOpen>]
module ArithmeticOperators =

    /// Overloaded addition. Base: the user type's own `static member (+)`.
    let inline (+) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "add" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "add" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "add" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "add" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "add" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "add" x y : uint64 #)
        when ^T1: nativeint and ^T2: nativeint and ^T3: nativeint = (# "add" x y : nativeint #)
        when ^T1: unativeint and ^T2: unativeint and ^T3: unativeint = (# "add" x y : unativeint #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "conv.u1" (# "add" x y : int32 #) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "conv.i1" (# "add" x y : int32 #) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "conv.i2" (# "add" x y : int32 #) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "conv.u2" (# "add" x y : int32 #) : uint16 #)
        // String concatenation: `add` on string references is a garbage pointer.
        // The `(# "" … #)` coercions bridge the free typars ↔ `string` at extraction
        // (clause must type-check while they are unpinned; selected only when `string`).
        when ^T1: string and ^T2: string and ^T3: string =
            (# "" (System.String.Concat((# "" x: string #), (# "" y: string #))) : ^T3 #)

    /// Overloaded subtraction. `sub` is sign-agnostic; narrow widths need truncation.
    let inline (-) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "sub" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "sub" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "sub" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "sub" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "sub" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "sub" x y : uint64 #)
        when ^T1: nativeint and ^T2: nativeint and ^T3: nativeint = (# "sub" x y : nativeint #)
        when ^T1: unativeint and ^T2: unativeint and ^T3: unativeint = (# "sub" x y : unativeint #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "conv.u1" (# "sub" x y : int32 #) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "conv.i1" (# "sub" x y : int32 #) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "conv.i2" (# "sub" x y : int32 #) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "conv.u2" (# "sub" x y : int32 #) : uint16 #)

    /// Overloaded multiplication. Written `( * )` — `(*` opens a block comment.
    let inline ( * ) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "mul" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "mul" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "mul" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "mul" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "mul" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "mul" x y : uint64 #)
        when ^T1: nativeint and ^T2: nativeint and ^T3: nativeint = (# "mul" x y : nativeint #)
        when ^T1: unativeint and ^T2: unativeint and ^T3: unativeint = (# "mul" x y : unativeint #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "conv.u1" (# "mul" x y : int32 #) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "conv.i1" (# "mul" x y : int32 #) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "conv.i2" (# "mul" x y : int32 #) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "conv.u2" (# "mul" x y : int32 #) : uint16 #)

    /// Overloaded division. `div` is the signed form; the unsigned widths need
    /// `div.un`, and sub-int32 widths additionally truncate.
    let inline (/) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (/): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "div" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "div" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "div" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "div" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "div.un" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "div.un" x y : uint64 #)
        when ^T1: nativeint and ^T2: nativeint and ^T3: nativeint = (# "div" x y : nativeint #)
        when ^T1: unativeint and ^T2: unativeint and ^T3: unativeint = (# "div.un" x y : unativeint #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "conv.u1" (# "div.un" x y : int32 #) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "conv.i1" (# "div" x y : int32 #) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "conv.i2" (# "div" x y : int32 #) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "conv.u2" (# "div.un" x y : int32 #) : uint16 #)

    /// Overloaded remainder. Unsigned widths need `rem.un`.
    let inline (%) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (%): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "rem" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "rem" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "rem" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "rem" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "rem.un" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "rem.un" x y : uint64 #)
        when ^T1: nativeint and ^T2: nativeint and ^T3: nativeint = (# "rem" x y : nativeint #)
        when ^T1: unativeint and ^T2: unativeint and ^T3: unativeint = (# "rem.un" x y : unativeint #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "conv.u1" (# "rem.un" x y : int32 #) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "conv.i1" (# "rem" x y : int32 #) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "conv.i2" (# "rem" x y : int32 #) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "conv.u2" (# "rem.un" x y : int32 #) : uint16 #)

    /// Overloaded unary negation.
    /// `neg` is the two's-complement negation, and where the CIL stack type IS the width
    /// (int32 / int64 / native int / the floats) that is the whole clause. The sub-32-bit
    /// widths compute on the int32 stack and must truncate back, exactly as every other
    /// operator's narrow clauses do — without the `conv`, `-(-128y)` answers 128 rather
    /// than wrapping to -128y.
    let inline (~-) (n: ^T) : ^T =
        (^T: (static member (~-): ^T -> ^T) n)
        when ^T: int = (# "neg" n : int #)
        when ^T: int64 = (# "neg" n : int64 #)
        when ^T: float = (# "neg" n : float #)
        when ^T: float32 = (# "neg" n : float32 #)
        when ^T: nativeint = (# "neg" n : nativeint #)
        when ^T: sbyte = (# "conv.i1" (# "neg" n : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "neg" n : int32 #) : int16 #)

    /// Overloaded unary plus — identity.
    let inline (~+) (value: ^T) : ^T = value

[<AutoOpen>]
module BitwiseOperators =

    // Each is the bare trait call. WHICH operand types support the operator, and the IL
    // each one lowers to, are stated on the primitives themselves (`prim-types-*.fs`) —
    // so there is no clause list here that could drift from that one, and a type that
    // states no member (`float`) is rejected rather than handed a width-blind opcode.
    let inline (&&&) (x: ^T) (y: ^T) : ^T = (^T: (static member (&&&): ^T * ^T -> ^T) (x, y))

    let inline (|||) (x: ^T) (y: ^T) : ^T = (^T: (static member (|||): ^T * ^T -> ^T) (x, y))

    let inline (^^^) (x: ^T) (y: ^T) : ^T = (^T: (static member (^^^): ^T * ^T -> ^T) (x, y))

    let inline (~~~) (value: ^T) : ^T = (^T: (static member (~~~): ^T -> ^T) value)

    let inline (<<<) (value: ^T) (shift: int32) : ^T = (^T: (static member (<<<): ^T * int32 -> ^T) (value, shift))

    let inline (>>>) (value: ^T) (shift: int32) : ^T = (^T: (static member (>>>): ^T * int32 -> ^T) (value, shift))

[<AutoOpen>]
module EqualityOperators =

    /// Structural equality. Primitive operands lower to `ceq`; aggregates use
    /// `EqualityComparer<^T>.Default.Equals` (same comparer as `hash`).
    let inline (=) (x: ^T) (y: ^T) : bool =
        EqualityComparer< ^T >.Default.Equals(x, y)
        when ^T: int = (# "ceq" x y : bool #)
        when ^T: int64 = (# "ceq" x y : bool #)
        when ^T: float = (# "ceq" x y : bool #)
        when ^T: float32 = (# "ceq" x y : bool #)
        when ^T: bool = (# "ceq" x y : bool #)
        when ^T: char = (# "ceq" x y : bool #)
        when ^T: byte = (# "ceq" x y : bool #)

    /// Structural inequality. Each form negates a `ceq` via `ceq(b, false)`.
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

    /// Generate a hash value. Rides `EqualityComparer<'T>`, so `hash` and `=` agree.
    let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj

    /// Boolean negation via `ceq(value, false)` — same shape as the `(<>)` base.
    let inline not (value: bool) : bool = (# "ceq" value false : bool #)

    /// Ignore the passed value.
    let inline ignore (value: 'T) : unit = ()

    /// Test whether a reference value is `null`. Lowers to `ceq(value, null)`.
    let inline isNull (value: 'T when 'T: null) : bool = (# "ceq" value null : bool #)

    /// Box a value to `obj`. The `!0` placeholder encodes the element type;
    /// codegen emits `box <T>` from the argument's static type.
    let inline box (value: 'T) : obj = (# "box !0" type ('T) value : obj #)

    /// Convert a value to `uint32`. A same-width `int32`→`uint32` is `(# "" … #)`
    /// (sign-only reinterpret, stack no-op per ECMA-335 III §1.5).
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

    /// `uint` abbreviation of `uint32`.
    let inline uint (value: ^T) : uint32 = uint32 value

    /// Convert a value to `int32`. A same-width `uint32`→`int32` is `(# "" … #)`
    /// (sign-only reinterpret, stack no-op per ECMA-335 III §1.5).
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

    /// `int` abbreviation of `int32`.
    let inline int (value: ^T) : int = int32 value

    /// Indexed array read — desugaring target for `arr.[i]`.
    /// The `ldelem.any` mnemonic lives in this per-target file.
    let inline GetArray (array: 'T[]) (index: int) : 'T = (# "ldelem.any !0" type ('T) array index : 'T #)

    /// Indexed array write — desugaring target for `arr.[i] <- value`.
    /// The `stelem.any` mnemonic lives in this per-target file.
    let inline SetArray (array: 'T[]) (index: int) (value: 'T) : unit =
        (# "stelem.any !0" type ('T) array index value : unit #)

    /// Array length — desugaring target for `arr.Length`. `ldlen` + `conv.i4`.
    let inline GetArrayLength (array: 'T[]) : int = (# "ldlen" array : int #)

    /// Raise the given exception. The `'TException :> exn` bound is enforced at
    /// each call site; `throw` terminates the path and the typar is not emitted.
    let inline raise (e: 'TException) : 'T = (# "throw" e : 'T #)

    /// Raise a `System.Exception` with the given message.
    /// The explicit `new` keyword is required: bare `System.Exception(msg)` is parsed
    /// as `Expr.App`; `new` routes through `Expr.New` → `inferNew`.
    let inline failwith (message: string) : 'T = raise (new System.Exception(message))

    /// Raise a `System.ArgumentException` naming the offending argument.
    /// Argument order follows FSharp.Core: name first, message second;
    /// the BCL ctor takes `(message, paramName)`.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        raise (new System.ArgumentException(message, argumentName))

/// String indexing intrinsics — see `ops-platform.fsi`.
[<AutoOpen>]
module StringIntrinsics =

    /// String indexing. On CLR the front end never routes here (the BCL
    /// `get_Chars` path wins whenever it resolves), so this body exists only to
    /// satisfy the contract; its `s.[i]` resolves to `get_Chars`, NOT recursively
    /// to GetString (the front end prefers `get_Chars` over this intrinsic).
    let inline GetString (s: string) (index: int) : char = s.[index]

/// Index-signature intrinsics — see `ops-platform.fsi`.
[<AutoOpen>]
module IndexIntrinsics =

    /// Indexed read of an index-signature object. These desugar `x.[k]` / `x.[k] <- v`
    /// on a receiver whose EXTERNAL (TS) type carries an index signature — a JS-target
    /// concept with no CLR analog (a `.NET` indexer resolves via `get_Item`/`set_Item`
    /// metadata), so like the JS-only `GetString` path the CLR body exists ONLY to
    /// satisfy the contract and is never routed to on CLR.
    let inline GetIndex (target: 'T) (key: 'K) : 'V = failwith "GetIndex is a JS-target intrinsic"

    /// Indexed write of an index-signature object — the `SetIndex` sibling of `GetIndex`
    /// (contract-only on CLR, see `GetIndex`).
    let inline SetIndex (target: 'T) (key: 'K) (value: 'V) : unit =
        failwith "SetIndex is a JS-target intrinsic"

/// The default-value primitive — see `ops-platform.fsi`.
module Unchecked =

    /// `defaultof` — a nullary value whose body is the zero-operand `ilzero` intrinsic, spliced
    /// at each `Unchecked.defaultof` reference. The CLR backend lowers `ilzero` to a
    /// zeroed scratch local (`ldloca; initobj; ldloc`): null for a reference type, all-zeroes for
    /// a value type. The `type ('T)` clause matches the F# idiom; the backend recovers the type
    /// from the result.
    let inline defaultof<'T> : 'T = (# "ilzero" type ('T) : 'T #)
