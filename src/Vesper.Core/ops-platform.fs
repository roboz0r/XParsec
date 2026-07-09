namespace Vesper

open System.Collections.Generic

// Per-target implementation of `ops-platform.fsi`. Each inline body is read across
// the package boundary by `SymbolProviders.inlineBodies` and spliced at each use
// site by the `Passes.InlineExpansion` pass.
//
// Equality (`=` / `<>`): each per-primitive clause lowers to `(# "ceq" … #)` IL;
// the base (aggregate operand) delegates to `EqualityComparer<^T>.Default.Equals`.
// An unpinned generic operand falls back to `Emit.BuiltinOps`'s `ceq` via the
// `isGround` guard.
//
// Arithmetic (`+ - * / %`): the base `(# "add" x y : ^T #)` covers every wide
// signed/float type; sub-`int32` widths add a `conv.*` to truncate the int32-on-stack
// result, and unsigned `/`/`%` use the `*.un` opcodes.

[<AutoOpen>]
module ArithmeticOperators =

    /// Overloaded addition. The base `(# "add" … #)` covers all wide types;
    /// sub-int32 widths truncate with `conv.*`.
    let inline (+) (x: ^T) (y: ^T) : ^T =
        (# "add" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "add" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "add" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "add" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "add" x y : int32 #) : uint16 #)
        // String concatenation: `add` on string references is a garbage pointer.
        // The `(# "" … #)` coerces coerce the free `^T` ↔ `string` at extraction
        // (clause must type-check while `^T` is unpinned; selected only when `string`).
        when ^T: string =
            (# "" (System.String.Concat((# "" x: string #), (# "" y: string #))) : ^T #)
        when ^T: ^T = (^T: (static member (+): ^T * ^T -> ^T) (x, y))

    /// Overloaded subtraction. `sub` is sign-agnostic; narrow widths need truncation.
    let inline (-) (x: ^T) (y: ^T) : ^T =
        (# "sub" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "sub" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "sub" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "sub" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "sub" x y : int32 #) : uint16 #)
        when ^T: ^T = (^T: (static member (-): ^T * ^T -> ^T) (x, y))

    /// Overloaded multiplication. Written `( * )` — `(*` opens a block comment.
    let inline ( * ) (x: ^T) (y: ^T) : ^T =
        (# "mul" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "mul" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "mul" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "mul" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "mul" x y : int32 #) : uint16 #)
        when ^T: ^T = (^T: (static member ( * ): ^T * ^T -> ^T) (x, y))

    /// Overloaded division. Base is signed `div`; unsigned widths use `div.un`,
    /// sub-int32 widths additionally truncate.
    let inline (/) (x: ^T) (y: ^T) : ^T =
        (# "div" x y : ^T #)
        when ^T: uint32 = (# "div.un" x y : uint32 #)
        when ^T: uint64 = (# "div.un" x y : uint64 #)
        when ^T: byte = (# "conv.u1" (# "div.un" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "div" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "div" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "div.un" x y : int32 #) : uint16 #)
        when ^T: ^T = (^T: (static member (/): ^T * ^T -> ^T) (x, y))

    /// Overloaded remainder. Unsigned widths need `rem.un`.
    let inline (%) (x: ^T) (y: ^T) : ^T =
        (# "rem" x y : ^T #)
        when ^T: uint32 = (# "rem.un" x y : uint32 #)
        when ^T: uint64 = (# "rem.un" x y : uint64 #)
        when ^T: byte = (# "conv.u1" (# "rem.un" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "rem" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "rem" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "rem.un" x y : int32 #) : uint16 #)
        when ^T: ^T = (^T: (static member (%): ^T * ^T -> ^T) (x, y))

    /// Overloaded unary negation.
    let inline (~-) (n: ^T) : ^T = (# "neg" n : ^T #)

    /// Overloaded unary plus — identity.
    let inline (~+) (value: ^T) : ^T = value

[<AutoOpen>]
module BitwiseOperators =

    /// Bitwise AND. No `conv.*` needed: in-range operands stay in range.
    /// (`and` is a CIL mnemonic inside `(# … #)`, not an F# keyword.)
    let inline (&&&) (x: ^T) (y: ^T) : ^T = (# "and" x y : ^T #)

    let inline (|||) (x: ^T) (y: ^T) : ^T = (# "or" x y : ^T #)

    let inline (^^^) (x: ^T) (y: ^T) : ^T = (# "xor" x y : ^T #)

    /// Bitwise complement. Sub-int32 widths keep the meaningful low bits; no truncation needed.
    let inline (~~~) (value: ^T) : ^T = (# "not" value : ^T #)

    /// Left shift. `shl` is sign-agnostic.
    let inline (<<<) (value: ^T) (shift: int32) : ^T = (# "shl" value shift : ^T #)

    /// Right shift. Base is signed `shr`; unsigned widths use `shr.un` (zero-fill).
    let inline (>>>) (value: ^T) (shift: int32) : ^T =
        (# "shr" value shift : ^T #)
        when ^T: uint32 = (# "shr.un" value shift : uint32 #)
        when ^T: uint64 = (# "shr.un" value shift : uint64 #)
        when ^T: byte = (# "shr.un" value shift : byte #)
        when ^T: uint16 = (# "shr.un" value shift : uint16 #)

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
[<AutoOpen>]
module Unchecked =

    /// `defaultof` — a nullary value whose body is the zero-operand `ilzero` intrinsic, spliced
    /// at each bare reference (the `undefined`-value path). The CLR backend lowers `ilzero` to a
    /// zeroed scratch local (`ldloca; initobj; ldloc`): null for a reference type, all-zeroes for
    /// a value type. The `type ('T)` clause matches the F# idiom; the backend recovers the type
    /// from the result.
    let inline defaultof<'T> : 'T = (# "ilzero" type ('T) : 'T #)
