namespace Vesper

open System.Collections.Generic

// ops-platform.fs — the per-target *implementation* of `ops-platform.fsi`
// (symbol-resolution-plan §5.2: the `.fsi` is the target-agnostic contract, the
// `.fs` is the binding). Each inline body here is read across the package
// boundary by the codegen inline-body loader (`SymbolProviders.inlineBodies`)
// and spliced at each use site by `Emit.lowerWith`'s `External`→inline-body
// routing — the same cross-package-inline mechanism `hash` introduced
// (milestone M, symbol-resolution-handoff.md).
//
// The EQUALITY family (`=` / `<>`) is the first OPERATOR family sourced from here
// rather than the codegen `Emit.BuiltinOps` stopgap (operators-plan.md
// "Implementation status"): closing the operator-named-binding freeze gap lets these `.fs`
// bodies freeze, be read by `SymbolProviders.inlineBodies`, and be spliced at use
// sites — the operator surface now flows through the same cross-package-inline +
// static-optimization machinery as `hash`, not a hard-coded codegen table.
//
// Each is an F# static-optimization over inline IL, *simplified* from
// FSharp.Core's `prim-types.fs` `(=)`/`(<>)`: the per-primitive clauses lower to
// `(# "ceq" … #)` IL the codegen `EmitIntrinsic` path interprets. The fall-clause
// (the static-opt *base*, taken when no primitive clause matches — an aggregate
// operand) is the structural `EqualityComparer<^T>.Default.Equals(x, y)` — the
// same family `hash` and the generated DU triple use, so `=`/`hash` agree by
// construction. (This routes a 2-arg external instance call through the
// cross-package inline; the tupled-arg member-emit + `recoverTypeArgs` fixes that
// makes it work are type-args-bug.md Layers 1+3.) An **unpinned** generic operand
// (`let f a b = a = b`, where `^T` is a free typar `EqualityComparer<!0>` can't
// encode) falls back to `Emit.BuiltinOps`'s `ceq` via the codegen `isGround` guard
// — the comparer can't encode a free `!0` without the deferred generic-member
// machinery.
//
// The ARITHMETIC / UNARY OPERATOR bodies (`+ - * / %`, unary `~-`) now live here
// too (`module ArithmeticOperators`). Unlike equality (every clause returns
// `bool`), these return `^T`, so each `when ^T : …` clause body has its own type
// — `byte`/`int16`/… — which the per-clause static-opt return typing now allows
// (`Unification.inferLibraryOnlyStaticOptimization` no longer cross-unifies clause
// bodies; see operators-plan.md). The static-opt *base* `(# "add" x y :
// ^T #)` already covers every wide signed/float type (the CIL arithmetic opcodes
// are type-polymorphic over the eval stack), so only the cases that need DIFFERENT
// IL carry a clause: the sub-`int32` widths need a `conv.*` to truncate the
// int32-on-stack result back to their width (otherwise `200uy + 100uy` wouldn't
// wrap to 44), and unsigned `/`/`%` need the `*.un` opcodes. An unpinned generic
// operand (`let f a b = a + b`) still falls back to `Emit.BuiltinOps` (the codegen
// `isGround` guard), exactly like the equality family.

[<AutoOpen>]
module ArithmeticOperators =

    /// Overloaded addition. The base `(# "add" … #)` serves int/int64/float/
    /// float32/native/wide-unsigned; the sub-int32 widths truncate the
    /// int32-on-stack sum back to their width with a `conv.*`.
    let inline (+) (x: ^T) (y: ^T) : ^T =
        (# "add" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "add" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "add" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "add" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "add" x y : int32 #) : uint16 #)

    /// Overloaded subtraction. Same shape as `(+)` — `sub` is sign-agnostic
    /// (two's complement), only the narrow result width needs truncation.
    let inline (-) (x: ^T) (y: ^T) : ^T =
        (# "sub" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "sub" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "sub" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "sub" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "sub" x y : int32 #) : uint16 #)

    /// Overloaded multiplication. Same shape as `(+)`. Written `( * )` (spaces
    /// required — `(*` opens a block comment).
    let inline ( * ) (x: ^T) (y: ^T) : ^T =
        (# "mul" x y : ^T #)
        when ^T: byte = (# "conv.u1" (# "mul" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "mul" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "mul" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "mul" x y : int32 #) : uint16 #)

    /// Overloaded division. The base is the SIGNED `div` (int/int64/float/
    /// native); the unsigned widths need `div.un` (signed `div` reads their high
    /// bit as a sign), and the sub-int32 widths additionally truncate.
    let inline (/) (x: ^T) (y: ^T) : ^T =
        (# "div" x y : ^T #)
        when ^T: uint32 = (# "div.un" x y : uint32 #)
        when ^T: uint64 = (# "div.un" x y : uint64 #)
        when ^T: byte = (# "conv.u1" (# "div.un" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "div" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "div" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "div.un" x y : int32 #) : uint16 #)

    /// Overloaded remainder. Same shape as `(/)` — unsigned widths need `rem.un`.
    let inline (%) (x: ^T) (y: ^T) : ^T =
        (# "rem" x y : ^T #)
        when ^T: uint32 = (# "rem.un" x y : uint32 #)
        when ^T: uint64 = (# "rem.un" x y : uint64 #)
        when ^T: byte = (# "conv.u1" (# "rem.un" x y : int32 #) : byte #)
        when ^T: sbyte = (# "conv.i1" (# "rem" x y : int32 #) : sbyte #)
        when ^T: int16 = (# "conv.i2" (# "rem" x y : int32 #) : int16 #)
        when ^T: uint16 = (# "conv.u2" (# "rem.un" x y : int32 #) : uint16 #)

    /// Overloaded unary negation. `neg` is two's-complement on every integral
    /// width and IEEE sign-flip on floats; the base covers all of them.
    let inline (~-) (n: ^T) : ^T = (# "neg" n : ^T #)

    /// Overloaded unary plus — the identity. No opcode: it just yields its operand.
    let inline (~+) (value: ^T) : ^T = value

[<AutoOpen>]
module BitwiseOperators =

    /// Bitwise AND/OR/XOR. No narrow-int `conv.*` is needed: a bitwise op of two
    /// in-range operands stays in range, so the int32-on-stack result already holds
    /// the correct sub-int32 value. (`and`/`or`/`xor` are CIL mnemonics, not F#
    /// keywords here — they're the operand of `(# … #)`.)
    let inline (&&&) (x: ^T) (y: ^T) : ^T = (# "and" x y : ^T #)

    let inline (|||) (x: ^T) (y: ^T) : ^T = (# "or" x y : ^T #)

    let inline (^^^) (x: ^T) (y: ^T) : ^T = (# "xor" x y : ^T #)

    /// Bitwise complement. `not` flips every bit of the int32-on-stack value; a
    /// sub-int32 width keeps the meaningful low bits, so no truncation is needed.
    let inline (~~~) (value: ^T) : ^T = (# "not" value : ^T #)

    /// Left shift by `shift` bits. `shl` is sign-agnostic, so the base covers every
    /// width. (`shift: int32` dealiases through `int32 = int` to the `int`
    /// intrinsic — `tryResolveExternalType`'s abbreviation arm.)
    let inline (<<<) (value: ^T) (shift: int32) : ^T = (# "shl" value shift : ^T #)

    /// Right shift by `shift` bits. The base is the SIGNED `shr` (arithmetic shift,
    /// sign-extending — correct for the signed widths); the unsigned widths need
    /// the LOGICAL `shr.un` (zero-fill).
    let inline (>>>) (value: ^T) (shift: int32) : ^T =
        (# "shr" value shift : ^T #)
        when ^T: uint32 = (# "shr.un" value shift : uint32 #)
        when ^T: uint64 = (# "shr.un" value shift : uint64 #)
        when ^T: byte = (# "shr.un" value shift : byte #)
        when ^T: uint16 = (# "shr.un" value shift : uint16 #)

[<AutoOpen>]
module EqualityOperators =

    /// Structural equality. A primitive operand lowers to a CIL `ceq` through its
    /// `when ^T : …` clause; an aggregate operand falls to the structural
    /// `EqualityComparer<^T>.Default.Equals(x, y)` base (the same comparer `hash`
    /// and the generated DU triple use). See the module comment for the unpinned
    /// generic fallback.
    let inline (=) (x: ^T) (y: ^T) : bool =
        EqualityComparer< ^T >.Default.Equals(x, y)
        when ^T: int = (# "ceq" x y : bool #)
        when ^T: int64 = (# "ceq" x y : bool #)
        when ^T: float = (# "ceq" x y : bool #)
        when ^T: float32 = (# "ceq" x y : bool #)
        when ^T: bool = (# "ceq" x y : bool #)
        when ^T: char = (# "ceq" x y : bool #)
        when ^T: byte = (# "ceq" x y : bool #)

    /// Structural inequality — the negation of `(=)`. `<>` has no CIL opcode of
    /// its own, so each form negates a `ceq` by comparing it to `false`
    /// (`ceq(b, false)` is `not b`); the base negates the structural comparer
    /// result the same way.
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

    /// Generate a hash value for the given value. No dedicated runtime member: it
    /// rides the BCL `EqualityComparer<'T>` — the same family the generated DU
    /// equality triple hashes its fields through — so `hash` and `=` agree by
    /// construction (equal values hash equal). BCL-only (no FSharp.Core, no Vesper
    /// runtime library).
    let inline hash (obj: 'T) = EqualityComparer<'T>.Default.GetHashCode obj

    /// Raise the given exception. The parameter is a typar bounded by `:> exn`,
    /// matching F#'s `raise: 'e :> exn -> 'a` — only an exception type can be
    /// passed. The contract extractor captures the `:> exn` coercion constraint
    /// (`VesperLibTypeTranslate.captureConstraints`) and the unifier enforces it
    /// at each call (`UnificationEngine.checkConstraint` via `subsumes`); the
    /// `exn === System.Exception` identity comes from `prim-types-exn.fs`'s
    /// intrinsic-repr binding, and `subsumes` walks external `inherit` chains so
    /// a derived BCL exception (`InvalidOperationException`) satisfies the bound.
    /// The `(# "throw" e : 'T #)` lowers through the terminal `throw` arm in
    /// `Emit.ILIntrinsic` — no balanced result is left on the stack, the path
    /// terminates — and the typar never reaches emitted IL (`throw` does not
    /// reference its operand's static type).
    let inline raise (e: 'TException) : 'T = (# "throw" e : 'T #)

    /// Throw a `System.Exception` with the given message. Same shape as
    /// FSharp.Core `prim-types.fs:4513`, but constructs the exception with the
    /// BCL `System.Exception(string)` ctor directly (Vesper has no `Failure`
    /// recipe). The inline-body splice at each use site lowers the body
    /// through `TExpr.New("System.Exception", [message], _)` (resolved by
    /// `inferNew`'s external-class fallback, emitted by
    /// `ClrProvider.externalCtor`) and the `raise` body above. The explicit
    /// `new` keyword is required: bare `System.Exception(msg)` is parsed as
    /// `Expr.App` and Freeze's `ClassRef` recognises only single-segment
    /// user-defined class names — `new` routes through `Expr.New` →
    /// `inferNew`, which has the external-class fallback.
    let inline failwith (message: string) : 'T = raise (new System.Exception(message))

    /// Raise a `System.ArgumentException` naming the offending argument. Sugar
    /// for `raise (new System.ArgumentException(message, argumentName))` — the
    /// FSharp.Core shape (`prim-types.fs`), with the BCL two-string ctor
    /// `(message, paramName)` selected by the external-ctor overload pick in
    /// `Infer.inferNew` (arity 2 + both operands `string`). Like `raise` /
    /// `failwith` this is a cross-package inline whose body is spliced at each
    /// use site, so it pins no Vesper runtime dependency.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        raise (new System.ArgumentException(message, argumentName))
