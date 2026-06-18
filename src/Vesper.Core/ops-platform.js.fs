namespace Vesper

// ops-platform.js.fs — the JS-target *implementation* of `ops-platform.fsi`
// (the `.fsi` is the target-agnostic contract; this is the JS binding). Selected
// over the CLR `ops-platform.fs` by the backend via
// `ReferencedProject.resolveInlineBodies (Some "js")` (manifest key
// `inline-bodies-js`); each `let inline` body is read across the package boundary
// and spliced at every use site, exactly like the CLR file. See
// ../XParsec.FSharp.SemanticAnalysis/docs/codegen-js-steps.md step F1.
//
// THE TEMPLATE IDIOM (codegen-js-steps.md step F0). The inline-IL string position
// carries a JS-expression template with `$N` operand holes (zero-indexed against
// the operand list in source order: `(# "$0 + $1" x y #)` ⇒ `$0`=x, `$1`=y; `$$`
// escapes a literal `$`). Semantic analysis treats the string as an opaque,
// trusted payload (`inferILIntrinsic` types the operands and takes the result
// from the annotation; `stitchIlInstruction` folds it verbatim) — interpretation
// is wholly the JS backend's concern, which substitutes operands and emits an
// expression node. Templates carry their own grouping parens where precedence
// needs it, so they stay correct under any backend substitution strategy.
//
// WHY THIS DIVERGES FROM THE CLR BODIES. The CLR base ops are CIL mnemonics that
// are stack-type-polymorphic — one `(# "add" #)` serves int32 / int64 / float /
// native because the CIL arithmetic opcodes read the eval-stack type. JS has no
// such polymorphism: `+` is float (and string) addition, with NO integer
// semantics. So the JS bodies need EXPLICIT per-width clauses where the CLR base
// sufficed:
//   - int32  : `| 0`  truncates the IEEE-double result back to signed 32-bit
//              (`Math.imul` for `*`, because `a * b | 0` loses precision before
//              the mask once the product exceeds 2^53).
//   - int64  : `BigInt.asIntN(64, …)` — int64 is a JS BigInt; BigInt arithmetic is
//              arbitrary-precision, so it must be wrapped back to 64-bit. BigInt
//              shifts also require a BigInt shift amount (`BigInt($1)`).
//   - float / float32 / bool / char go through the polymorphic JS operator base.
//
// SCOPE (F1). This file covers the int32 / int64 / float / bool / char widths that
// backend Step 1 exercises (and can execute under Node), plus the SRTP `when ^T :
// ^T` user-type fallback (a user type's own static operator — target-neutral,
// kept identical to the CLR file). The narrow/unsigned masking clauses
// (byte/sbyte/int16/uint16/uint32/uint64 — `& 0xFF`, `<< 24 >> 24`, `>>> 0`, …)
// are deferred to the F-step gated by the backend step that first exercises those
// widths, so each width's mask lands WITH an execution test rather than as
// untested template surface. `hash` is re-authored here as part of Step 6
// (equality + hashing) — for an aggregate it delegates to the non-inline
// `Vesper.Core` runtime entry `structuralHash` (imported from `Vesper.Core.mjs`
// through the ordinary external-call path; see `module StructuralRuntime` /
// `module Operators` below), as `=` / `<>` delegate to `structuralEquals`.
// `raise` / `failwith` ARE re-authored here (a
// Step 7 adjacent slice) as FFI `throw` templates: `failwith` builds its own native
// `Error`, so a `failwith` use site (e.g. `list.fs`'s `head`/`tail`) lowers with no
// per-call template. `box`, the array ops, and `invalidArg` are not re-authored here
// yet (the last needs external-`new`); a use site needing one simply finds no JS
// inline body until its F-step lands.

[<AutoOpen>]
module ArithmeticOperators =

    /// Overloaded addition. JS `+` is the float base; int32 truncates with `| 0`,
    /// int64 (BigInt) wraps to 64 bits. The `when ^T : ^T` clause dispatches a
    /// user type to its own `static member (+)` (target-neutral, as in the CLR body).
    let inline (+) (x: ^T) (y: ^T) : ^T =
        (# "$0 + $1" x y : ^T #)
        when ^T: int = (# "($0 + $1) | 0" x y : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 + $1)" x y : int64 #)
        when ^T: ^T = (^T: (static member (+): ^T * ^T -> ^T) (x, y))

    /// Overloaded subtraction. Same shape as `(+)`.
    let inline (-) (x: ^T) (y: ^T) : ^T =
        (# "$0 - $1" x y : ^T #)
        when ^T: int = (# "($0 - $1) | 0" x y : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 - $1)" x y : int64 #)
        when ^T: ^T = (^T: (static member (-): ^T * ^T -> ^T) (x, y))

    /// Overloaded multiplication. int32 uses `Math.imul` (a plain `$0 * $1 | 0`
    /// loses precision before the truncation once the product exceeds 2^53);
    /// int64 wraps the BigInt product. Written `( * )` (spaces required — `(*`
    /// opens a block comment).
    let inline ( * ) (x: ^T) (y: ^T) : ^T =
        (# "$0 * $1" x y : ^T #)
        when ^T: int = (# "Math.imul($0, $1)" x y : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 * $1)" x y : int64 #)
        when ^T: ^T = (^T: (static member ( * ): ^T * ^T -> ^T) (x, y))

    /// Overloaded division. int32 truncates toward zero with `| 0` (JS `/` is
    /// always true division); int64 BigInt `/` already truncates toward zero, then
    /// wraps to 64 bits.
    let inline (/) (x: ^T) (y: ^T) : ^T =
        (# "$0 / $1" x y : ^T #)
        when ^T: int = (# "($0 / $1) | 0" x y : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 / $1)" x y : int64 #)
        when ^T: ^T = (^T: (static member (/): ^T * ^T -> ^T) (x, y))

    /// Overloaded remainder. JS `%` is the truncated remainder (sign of the
    /// dividend), matching F#; int32 re-truncates, int64 wraps. The bare `%` rides
    /// through verbatim: a plain/IL-intrinsic string is not a printf format, so the
    /// front end keeps the format-scanned `%` as literal text (`parsePlainStringLiteral`).
    let inline (%) (x: ^T) (y: ^T) : ^T =
        (# "$0 % $1" x y : ^T #)
        when ^T: int = (# "($0 % $1) | 0" x y : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 % $1)" x y : int64 #)
        when ^T: ^T = (^T: (static member (%): ^T * ^T -> ^T) (x, y))

    /// Overloaded unary negation. JS unary `-` is the float base; int32 wraps the
    /// negation (`-(Int32.MinValue)` overflows to itself), int64 wraps the BigInt.
    let inline (~-) (n: ^T) : ^T =
        (# "-$0" n : ^T #)
        when ^T: int = (# "(-$0) | 0" n : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, -$0)" n : int64 #)

    /// Overloaded unary plus — the identity. No template: it just yields its
    /// operand (target-neutral, identical to the CLR body).
    let inline (~+) (value: ^T) : ^T = value

[<AutoOpen>]
module BitwiseOperators =

    /// Bitwise AND. JS `&` coerces a number operand to signed int32 and a BigInt
    /// operand to its BigInt bitwise-and, so the single base covers both int32 and
    /// int64 — the result of AND-ing two in-range operands stays in range.
    let inline (&&&) (x: ^T) (y: ^T) : ^T = (# "$0 & $1" x y : ^T #)

    let inline (|||) (x: ^T) (y: ^T) : ^T = (# "$0 | $1" x y : ^T #)

    let inline (^^^) (x: ^T) (y: ^T) : ^T = (# "$0 ^ $1" x y : ^T #)

    /// Bitwise complement. `~` on a number is int32 complement; on a BigInt it is
    /// the BigInt complement (which `(&&&)`'s reasoning keeps in 64-bit range for
    /// a wrapped int64 operand).
    let inline (~~~) (value: ^T) : ^T = (# "~$0" value : ^T #)

    /// Left shift by `shift` bits. JS `<<` on a number is the int32 shift; a BigInt
    /// shift needs a BigInt shift amount, so int64 converts `$1` and wraps.
    let inline (<<<) (value: ^T) (shift: int32) : ^T =
        (# "$0 << $1" value shift : ^T #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 << BigInt($1))" value shift : int64 #)

    /// Right shift by `shift` bits. The base is the arithmetic (sign-extending)
    /// `>>`, correct for the signed widths; int64 shifts the BigInt and re-wraps.
    let inline (>>>) (value: ^T) (shift: int32) : ^T =
        (# "$0 >> $1" value shift : ^T #)
        when ^T: int64 = (# "BigInt.asIntN(64, $0 >> BigInt($1))" value shift : int64 #)

[<AutoOpen>]
module EqualityOperators =

    /// Structural equality. A primitive operand lowers to a JS strict `===`
    /// through its `when ^T : …` clause (BigInt / number / boolean / string `===`
    /// are all value comparisons); an aggregate operand delegates to
    /// `structuralEquals`, the non-inline `Vesper.Core` runtime entry the backend
    /// imports from `Vesper.Core.mjs` through the ordinary external-call path — no
    /// bare-name template token. So a program over primitives pulls in no import.
    let inline (=) (x: ^T) (y: ^T) : bool =
        structuralEquals x y
        when ^T: int = (# "$0 === $1" x y : bool #)
        when ^T: int64 = (# "$0 === $1" x y : bool #)
        when ^T: float = (# "$0 === $1" x y : bool #)
        when ^T: float32 = (# "$0 === $1" x y : bool #)
        when ^T: bool = (# "$0 === $1" x y : bool #)
        when ^T: char = (# "$0 === $1" x y : bool #)

    /// Structural inequality — the negation of `(=)`. Each primitive form is a
    /// strict `!==`; the base negates the `structuralEquals` runtime call (wrapped
    /// in a `!$0` template because `not` is defined later in this file).
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

    /// Generate a hash value. The CLR body rides `EqualityComparer<'T>.Default`;
    /// JS has no such facility, so the primitive/aggregate split lives wholly in
    /// the JS-runtime `structuralHash` (a primitive hashes by `typeof`, an
    /// aggregate recurses over its own-property / array shape). A single call
    /// therefore covers every width, and `hash` agrees with `(=)` by construction
    /// — both walk the same structural shape, so equal values hash equal.
    /// `structuralHash` is the non-inline `Vesper.Core` runtime entry the backend
    /// imports from `Vesper.Core.mjs` through the ordinary external-call path.
    let inline hash (obj: 'T) : int = structuralHash obj

    /// Boolean negation — JS logical `!`. (The CLR body negates via `ceq value
    /// false`; the JS template is the direct operator.)
    let inline not (value: bool) : bool = (# "!$0" value : bool #)

    /// Convert to `uint32`. The JS idiom `$0 >>> 0` coerces any number to a
    /// 32-bit *unsigned* integer (zero-fill right shift by 0), the counterpart of
    /// the CLR `conv.u4` / sign-only reinterpret.
    let inline uint32 (value: ^T) : uint32 = (# "$0 >>> 0" value : uint32 #)

    /// `uint` abbreviation of `uint32`.
    let inline uint (value: ^T) : uint32 = uint32 value

    /// Convert to `int32`. The JS idiom `$0 | 0` coerces any number to a 32-bit
    /// *signed* integer (bitwise-OR with zero), the signed counterpart of
    /// `uint32`'s `$0 >>> 0` and of the CLR `conv.i4` / sign-only reinterpret.
    let inline int32 (value: ^T) : int32 = (# "$0 | 0" value : int32 #)

    /// `int` abbreviation of `int32`.
    let inline int (value: ^T) : int = int32 value

    /// Raise the given exception. The CLR body is the bare `(# "throw" e #)`
    /// mnemonic (the terminal `throw` arm leaves no balanced stack value); the JS
    /// form is an expression-position IIFE that `throw`s the operand
    /// (`(() => { throw e; })()`), so it composes anywhere a value is expected —
    /// the `'T` result type is never realised, exactly as on the CLR. The operand
    /// must already be a thrown-able value; constructing a BCL exception
    /// (`raise (InvalidOperationException …)`) still awaits the external-`new`
    /// arm — until then prefer `failwith`, which builds its own `Error`.
    let inline raise (e: 'TException) : 'T = (# "(() => { throw $0; })()" e : 'T #)

    /// Throw with the given message. The CLR body is
    /// `raise (new System.Exception(message))`; JS has no `System.Exception`, so
    /// this throws a native `new Error($0)` directly — self-contained, needing
    /// neither the `raise` chain nor external-`new` (the same FFI-throw shape
    /// `option.js.fs` / `list.js.fs` inlined by hand, now the library body so a
    /// `failwith` use site lowers with no per-call template). The message is the
    /// thrown `Error`'s `.message`, surfaced by Node as the uncaught-error text.
    let inline failwith (message: string) : 'T =
        (# "(() => { throw new Error($0); })()" message : 'T #)
