namespace Vesper

[<AutoOpen>]
module ArithmeticOperators =

    let inline (+) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) (x, y))

    let inline (-) (x: ^T1) (y: ^T2) : ^T3 = ((^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) (x, y))

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

    let inline (=) (x: ^T) (y: ^T) : bool =
        structuralEquals x y
        when ^T: int = (# "$0 === $1" x y : bool #)
        when ^T: int64 = (# "$0 === $1" x y : bool #)
        when ^T: float = (# "$0 === $1" x y : bool #)
        when ^T: float32 = (# "$0 === $1" x y : bool #)
        when ^T: bool = (# "$0 === $1" x y : bool #)
        when ^T: char = (# "$0 === $1" x y : bool #)

    let inline (<>) (x: ^T) (y: ^T) : bool =
        (# "!$0" (structuralEquals x y) : bool #)
        when ^T: int = (# "$0 !== $1" x y : bool #)
        when ^T: int64 = (# "$0 !== $1" x y : bool #)
        when ^T: float = (# "$0 !== $1" x y : bool #)
        when ^T: float32 = (# "$0 !== $1" x y : bool #)
        when ^T: bool = (# "$0 !== $1" x y : bool #)
        when ^T: char = (# "$0 !== $1" x y : bool #)

[<AutoOpen>]
module Operators =

    /// JS has no `EqualityComparer`, so the primitive/aggregate split lives in
    /// `structuralHash` — a non-inline `Vesper.Core.mjs` entry that `(=)` also walks,
    /// so equal values hash equal.
    let inline hash (obj: 'T) : int = structuralHash obj

    let inline not (value: bool) : bool = (# "!$0" value : bool #)

    /// `void $0` yields `undefined`, which IS the `unit` repr, and still evaluates the
    /// operand for its effects.
    let inline ignore (value: 'T) : unit = (# "void $0" value : unit #)

    /// STRICT `===`, not the nullish `==`: `undefined` is a separate type here with its
    /// own value, so `isNull undefined` is `false`.
    let inline isNull (value: 'T when 'T: null) : bool = (# "$0 === null" value : bool #)

    /// The empty template is an erasing identity cast — JS has no unboxed
    /// representation to move.
    let inline box (value: 'T) : obj = (# "" value : obj #)

    /// `$0 >>> 0` — a zero-fill shift by 0 is JS's coercion to a 32-bit UNSIGNED integer.
    let inline uint32 (value: ^T) : uint32 = (# "$0 >>> 0" value : uint32 #)

    let inline uint (value: ^T) : uint32 = uint32 value

    /// `$0 | 0` — a bitwise OR with 0 is JS's coercion to a 32-bit SIGNED integer.
    let inline int32 (value: ^T) : int32 = (# "$0 | 0" value : int32 #)

    let inline int (value: ^T) : int = int32 value

    /// `number` and `bigint` are disjoint JS types that no operator mixes, so the
    /// widening is the explicit `BigInt(…)`.
    let inline bigint (value: int32) : bigint = (# "BigInt($0)" value : bigint #)

    /// The desugaring target for `arr.[i]`. The `ldelem.any` mnemonic is target-neutral —
    /// the JS backend emits the computed member read `arr[i]`.
    let inline GetArray (array: 'T[]) (index: int) : 'T = (# "ldelem.any !0" type ('T) array index : 'T #)

    /// The desugaring target for `arr.[i] <- value`; `stelem.any` emits `arr[i] = value`.
    let inline SetArray (array: 'T[]) (index: int) (value: 'T) : unit =
        (# "stelem.any !0" type ('T) array index value : unit #)

    /// The desugaring target for `arr.Length`; `ldlen` emits `arr.length`.
    let inline GetArrayLength (array: 'T[]) : int = (# "ldlen" array : int #)

    /// An expression-position IIFE, so a `raise` composes anywhere a value is expected;
    /// the `'T` result is never realised.
    let inline raise (e: 'TException) : 'T = (# "(() => { throw $0; })()" e : 'T #)

    /// Throws a native `new Error($0)`; the message becomes that `Error`'s `.message`.
    let inline failwith (message: string) : 'T =
        (# "(() => { throw new Error($0); })()" message : 'T #)

    /// Every exception erases to a JS `Error`, so the argument name survives only in the
    /// message text — worded as the BCL's `ArgumentException` words it.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        failwith (message + " (Parameter '" + argumentName + "')")

[<AutoOpen>]
module StringIntrinsics =

    /// The desugaring target for `s.[i]`: a JS string is indexable and `char` is a
    /// length-1 string, so the native `s[i]` answers directly.
    let inline GetString (s: string) (index: int) : char = (# "$0[$1]" s index : char #)

[<AutoOpen>]
module IndexIntrinsics =

    /// The desugaring target for `x.[k]` where `'T`'s external type carries a TS index
    /// signature (`process.env`, `Record<K,V>`). A JS object has no `get_Item` method,
    /// so the computed-member read `x[k]` is the only form.
    let inline GetIndex (target: 'T) (key: 'K) : 'V = (# "$0[$1]" target key : 'V #)

    /// The `x.[k] <- value` counterpart — the computed-member assignment `x[k] = value`.
    let inline SetIndex (target: 'T) (key: 'K) (value: 'V) : unit =
        (# "$0[$1] = $2" target key value : unit #)

module Unchecked =

    /// JS has no per-type zero, so the default IS `null` — a hole-less template that
    /// substitutes to the bare literal at each reference.
    let inline defaultof<'T> : 'T = (# "null" : 'T #)
