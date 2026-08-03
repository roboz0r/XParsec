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

    /// Discard a value. `void` is JS's own discard operator and it yields
    /// `undefined`, which IS the `unit` repr — so the operand is still evaluated
    /// for its effects. (The CLR body is the bare `()`.)
    let inline ignore (value: 'T) : unit = (# "void $0" value : unit #)

    /// Test whether a reference is `null`. STRICT `===`, not the nullish `==`:
    /// `undefined` is a separate type here with its own value, and `null` alone is the
    /// JS default of a reference type, so the two stay distinguishable.
    let inline isNull (value: 'T when 'T: null) : bool = (# "$0 === null" value : bool #)

    /// Box a value to `obj`. JS has no unboxed representation to move — every value
    /// already sits where an `unknown` can hold it — so this is the erasing identity
    /// cast, not the CLR's allocating `box`.
    let inline box (value: 'T) : obj = (# "" value : obj #)

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

    /// Widen an `int32` to arbitrary precision. `number` and `bigint` are disjoint JS
    /// types that no operator mixes, so the widening is the explicit `BigInt(…)`.
    let inline bigint (value: int32) : bigint = (# "BigInt($0)" value : bigint #)

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

    /// Raise an argument error. Every exception erases to a JS `Error`, so the
    /// argument name survives only in the text — folded in with the BCL's own
    /// `ArgumentException` wording so both targets report the same message.
    let inline invalidArg (argumentName: string) (message: string) : 'T =
        failwith (message + " (Parameter '" + argumentName + "')")

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
