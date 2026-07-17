namespace Vesper

[<AutoOpen>]
module ArithmeticOperators =

    /// Overloaded addition. Base: the user type's own `static member (+)`
    /// (target-neutral, as in the CLR body). Every numeric clause is the JS `+` under
    /// its width mask; JS `+` already concatenates two strings.
    /// The three typars are the `.fsi`'s (`(^T1 or ^T2)` support set), so a
    /// heterogeneous user operator keeps its operand types distinct through the splice.
    let inline (+) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "($0 + $1) | 0" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "BigInt.asIntN(64, $0 + $1)" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "$0 + $1" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "Math.fround($0 + $1)" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "($0 + $1) >>> 0" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "BigInt.asUintN(64, $0 + $1)" x y : uint64 #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "($0 + $1) & 0xFF" x y : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "($0 + $1) << 24 >> 24" x y : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "($0 + $1) << 16 >> 16" x y : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "($0 + $1) & 0xFFFF" x y : uint16 #)
        when ^T1: string and ^T2: string and ^T3: string = (# "$0 + $1" x y : string #)

    /// Overloaded subtraction. Same shape as `(+)`, minus the string clause. The masks
    /// are what keep an unsigned width unsigned: `10uy - 20uy` is -10 on the JS wire
    /// and 246 after `& 0xFF`.
    let inline (-) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (-): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "($0 - $1) | 0" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "BigInt.asIntN(64, $0 - $1)" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "$0 - $1" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "Math.fround($0 - $1)" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "($0 - $1) >>> 0" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "BigInt.asUintN(64, $0 - $1)" x y : uint64 #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "($0 - $1) & 0xFF" x y : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "($0 - $1) << 24 >> 24" x y : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "($0 - $1) << 16 >> 16" x y : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "($0 - $1) & 0xFFFF" x y : uint16 #)

    /// Overloaded multiplication. Written `( * )` (spaces required — `(*` opens a block
    /// comment).
    ///
    /// The 32-bit widths take `Math.imul` — the mask can only be applied to a product
    /// JS has already computed, and a full 32×32 product reaches ~2^64, losing its low
    /// bits (the ones the mask keeps) past 2^53. `Math.imul` computes the product mod
    /// 2^32 directly, which is what both signed and unsigned 32-bit `*` want: it is the
    /// same bit pattern either way, so uint32 differs only in reading it back unsigned.
    /// The narrow widths need no such care — a byte/sbyte/int16/uint16 product cannot
    /// exceed 2^32, so the double is exact and the mask is faithful.
    let inline ( * ) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member ( * ): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "Math.imul($0, $1)" x y : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 = (# "BigInt.asIntN(64, $0 * $1)" x y : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "$0 * $1" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "Math.fround($0 * $1)" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "Math.imul($0, $1) >>> 0" x y : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 = (# "BigInt.asUintN(64, $0 * $1)" x y : uint64 #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "($0 * $1) & 0xFF" x y : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "($0 * $1) << 24 >> 24" x y : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "($0 * $1) << 16 >> 16" x y : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "($0 * $1) & 0xFFFF" x y : uint16 #)

    /// Overloaded division. Two things beyond the width mask, both of which the mask
    /// happens to be the right place for:
    ///
    /// TRUNCATION. JS `/` is true division — `10uy / 3uy` is 3.333…, not an integer at
    /// all — and the fraction survives into the next operation, so a report-site `int (…)`
    /// cannot launder it. Each integral mask is a bitwise coercion (`| 0`, `>>> 0`,
    /// `& 0xFF`, `<< 24 >> 24`), and those truncate toward zero, which is exactly F#'s
    /// rule. int64's BigInt `/` already truncates; only the 64-bit wrap is left.
    ///
    /// THE ZERO DIVISOR. CIL `div` faults; JS yields `Infinity`, and `Infinity | 0` is
    /// `0` — a silently wrong answer of precisely the kind the masks exist to remove. So
    /// every integral divisor passes through `checkedDivisor` (which throws, and returns
    /// its argument so the mask still wraps it), and the operand is read once — a
    /// template repeating `$1` would evaluate it twice. `float` / `float32` are NOT
    /// guarded: `Infinity` is IEEE's correct answer, and F#'s.
    let inline (/) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (/): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "($0 / $1) | 0" x (checkedDivisor y) : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 =
            (# "BigInt.asIntN(64, $0 / $1)" x (checkedDivisor y) : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "$0 / $1" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "Math.fround($0 / $1)" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "($0 / $1) >>> 0" x (checkedDivisor y) : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 =
            (# "BigInt.asUintN(64, $0 / $1)" x (checkedDivisor y) : uint64 #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "($0 / $1) & 0xFF" x (checkedDivisor y) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "($0 / $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "($0 / $1) << 16 >> 16" x (checkedDivisor y) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "($0 / $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

    /// Overloaded remainder. JS `%` is the truncated remainder (sign of the dividend),
    /// matching F#, and on integral operands it is already exact — so the mask is only
    /// re-asserting the width. The zero divisor is guarded exactly as in `(/)`: CIL `rem`
    /// faults, JS `%` yields `NaN` (and `NaN | 0` is `0`).
    ///
    /// The bare `%` rides through the template verbatim: a plain/IL-intrinsic string is
    /// not a printf format, so the front end keeps the format-scanned `%` as literal text
    /// (`parsePlainStringLiteral`).
    let inline (%) (x: ^T1) (y: ^T2) : ^T3 =
        ((^T1 or ^T2): (static member (%): ^T1 * ^T2 -> ^T3) (x, y))
        when ^T1: int and ^T2: int and ^T3: int = (# "($0 % $1) | 0" x (checkedDivisor y) : int #)
        when ^T1: int64 and ^T2: int64 and ^T3: int64 =
            (# "BigInt.asIntN(64, $0 % $1)" x (checkedDivisor y) : int64 #)
        when ^T1: float and ^T2: float and ^T3: float = (# "$0 % $1" x y : float #)
        when ^T1: float32 and ^T2: float32 and ^T3: float32 = (# "Math.fround($0 % $1)" x y : float32 #)
        when ^T1: uint32 and ^T2: uint32 and ^T3: uint32 = (# "($0 % $1) >>> 0" x (checkedDivisor y) : uint32 #)
        when ^T1: uint64 and ^T2: uint64 and ^T3: uint64 =
            (# "BigInt.asUintN(64, $0 % $1)" x (checkedDivisor y) : uint64 #)
        when ^T1: byte and ^T2: byte and ^T3: byte = (# "($0 % $1) & 0xFF" x (checkedDivisor y) : byte #)
        when ^T1: sbyte and ^T2: sbyte and ^T3: sbyte = (# "($0 % $1) << 24 >> 24" x (checkedDivisor y) : sbyte #)
        when ^T1: int16 and ^T2: int16 and ^T3: int16 = (# "($0 % $1) << 16 >> 16" x (checkedDivisor y) : int16 #)
        when ^T1: uint16 and ^T2: uint16 and ^T3: uint16 = (# "($0 % $1) & 0xFFFF" x (checkedDivisor y) : uint16 #)

    /// Overloaded unary negation — the JS unary `-` under each width's mask.
    ///
    /// The mask is the whole point at the wrapping widths: `-(Int32.MinValue)` overflows
    /// to itself, and `-(-128y)` wraps back to -128y rather than answering 128.
    let inline (~-) (n: ^T) : ^T =
        (^T: (static member (~-): ^T -> ^T) n)
        when ^T: int = (# "(-$0) | 0" n : int #)
        when ^T: int64 = (# "BigInt.asIntN(64, -$0)" n : int64 #)
        when ^T: float = (# "-$0" n : float #)
        when ^T: float32 = (# "Math.fround(-$0)" n : float32 #)
        when ^T: sbyte = (# "(-$0) << 24 >> 24" n : sbyte #)
        when ^T: int16 = (# "(-$0) << 16 >> 16" n : int16 #)

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

    /// Indexed array read — desugaring target for `arr.[i]`. The `ldelem.any`
    /// mnemonic is target-neutral (Elaborate drops the element-type operand on JS); the
    /// JS backend emits the computed member read `arr[i]`. Identical to the CLR body.
    let inline GetArray (array: 'T[]) (index: int) : 'T = (# "ldelem.any !0" type ('T) array index : 'T #)

    /// Indexed array write — desugaring target for `arr.[i] <- value`. `stelem.any`
    /// is target-neutral; the JS backend emits the computed-member assignment
    /// `arr[i] = value`. Identical to the CLR body.
    let inline SetArray (array: 'T[]) (index: int) (value: 'T) : unit =
        (# "stelem.any !0" type ('T) array index value : unit #)

    /// Array length — desugaring target for `arr.Length`. `ldlen` is target-neutral;
    /// the JS backend emits `arr.length`. Identical to the CLR body.
    let inline GetArrayLength (array: 'T[]) : int = (# "ldlen" array : int #)

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

/// String indexing intrinsics — see `ops-platform.fsi`.
[<AutoOpen>]
module StringIntrinsics =

    /// String indexing — `s.[i]` → the native bracket index `s[i]` (a JS string is
    /// indexable; F# `char` is a length-1 string). The desugaring target for `s.[i]`
    /// on the JS target, where `string` carries no BCL `get_Chars`. Mirrors `GetArray`.
    let inline GetString (s: string) (index: int) : char = (# "$0[$1]" s index : char #)

/// Index-signature intrinsics — see `ops-platform.fsi`.
[<AutoOpen>]
module IndexIntrinsics =

    /// Indexed read of an index-signature object — `x.[k]` → the native computed-member
    /// read `x[k]`. Carries the SAME proven `$0[$1]` template the JS backend already
    /// lowers for `GetString` and the `dynamic` `(?)`, so an index-signature object
    /// (`process.env`, `NodeJS.Dict<T>`, an instantiated `Record<K,V>`) reads with a
    /// precise `'K`/`'V` typing and NO `dynamic` escape — and there is no JS
    /// `get_Item` method to route through (a JS object has none; bracket IS the form).
    let inline GetIndex (target: 'T) (key: 'K) : 'V = (# "$0[$1]" target key : 'V #)

    /// Indexed write of an index-signature object — `x.[k] <- value` → the
    /// computed-member assignment `x[k] = value` (the `$0[$1] = $2` template `(?<-)`
    /// and `SetArray` already lower). The `SetIndex` sibling of `GetIndex`.
    let inline SetIndex (target: 'T) (key: 'K) (value: 'V) : unit =
        (# "$0[$1] = $2" target key value : unit #)

/// The default-value primitive — see `ops-platform.fsi`.
module Unchecked =

    /// `defaultof` — a nullary value spliced at each `Unchecked.defaultof` reference.
    /// JS has no per-type zero, so the default IS `null`, emitted by the ordinary
    /// template idiom: a hole-less `"null"` template substitutes to the literal, no bespoke
    /// backend arm. This DIVERGES from the CLR body (`ilzero` → `initobj`) exactly as the
    /// arithmetic bodies diverge — a JS template here, a CIL mnemonic there.
    let inline defaultof<'T> : 'T = (# "null" : 'T #)
