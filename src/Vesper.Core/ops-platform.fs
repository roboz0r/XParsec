namespace Vesper

open System.Collections.Generic

[<AutoOpen>]
module ArithmeticOperators =

    // Each is the bare trait call, as the bitwise family below is. WHICH operand types
    // support the operator, and the IL each one lowers to, are stated on the primitives
    // themselves (`prim-types-*.fs`), so there is no clause list here that could drift
    // from that one.
    //
    // The three typars are the `.fsi`'s (`(^T1 or ^T2)` support set), so a heterogeneous
    // user operator keeps its operand types distinct through the splice.

    /// `string` is the family's one surviving clause: every numeric width states its own
    /// `(+)` on the type, but concatenation cannot yet — see the remark on `type string`
    /// in `prim-types-string.fsi`. The `(# "" … #)` coercions bridge the free typars ↔
    /// `string` (the clause must type-check while they are unpinned; selected only when
    /// `string`).
    let inline (+) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: string and ^T2: string and ^T3: string =
            (# "" (System.String.Concat((# "" x: string #), (# "" y: string #))) : ^T3 #)

    let inline (-) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) (x, y))

    /// Written `( * )` — `(*` opens a block comment.
    let inline ( * ) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (/) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (/): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (%) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (%): ^T1 * ^T2 -> ^T3) (x, y))

    /// Overloaded unary negation. Declared only at the signed widths, so `-a` on an
    /// unsigned one is the ordinary "does not support the operator" rejection.
    let inline (~-) (n: ^T) : ^T = (^T: (static member (~-): ^T -> ^T) n)

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
