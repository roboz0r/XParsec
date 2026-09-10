namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open XParsec.FSharp

// Post-lexing interpretation of a numeric-literal token: its value, read from the
// `NumericBase` / `NumericKind` flags the lexer has already classified onto it.

/// One of F#'s integral primitive types, by its width and signedness. Shared by the literal
/// reader, the semantic constant model and both backends.
[<RequireQualifiedAccess>]
type IntKind =
    | SByte
    | Byte
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    /// Pointer-width. Has no JS representation, and cannot underlie a CLR enum.
    | NativeInt
    | UNativeInt

/// Per-kind facts: width, signedness and the literal suffix.
module IntKind =

    let all: IntKind list =
        [
            IntKind.SByte
            IntKind.Byte
            IntKind.Int16
            IntKind.UInt16
            IntKind.Int32
            IntKind.UInt32
            IntKind.Int64
            IntKind.UInt64
            IntKind.NativeInt
            IntKind.UNativeInt
        ]

    /// The F# literal suffix (`10y`, `10UL`). `Int32` is unsuffixed.
    let suffix (k: IntKind) : string =
        match k with
        | IntKind.SByte -> "y"
        | IntKind.Byte -> "uy"
        | IntKind.Int16 -> "s"
        | IntKind.UInt16 -> "us"
        | IntKind.Int32 -> ""
        | IntKind.UInt32 -> "u"
        | IntKind.Int64 -> "L"
        | IntKind.UInt64 -> "UL"
        | IntKind.NativeInt -> "n"
        | IntKind.UNativeInt -> "un"

    let isSigned (k: IntKind) : bool =
        match k with
        | IntKind.SByte
        | IntKind.Int16
        | IntKind.Int32
        | IntKind.Int64
        | IntKind.NativeInt -> true
        | IntKind.Byte
        | IntKind.UInt16
        | IntKind.UInt32
        | IntKind.UInt64
        | IntKind.UNativeInt -> false

    /// Pointer-width: `nativeint` / `unativeint`, a distinct CIL stack type.
    let isNative (k: IntKind) : bool =
        match k with
        | IntKind.NativeInt
        | IntKind.UNativeInt -> true
        | IntKind.SByte
        | IntKind.Byte
        | IntKind.Int16
        | IntKind.UInt16
        | IntKind.Int32
        | IntKind.UInt32
        | IntKind.Int64
        | IntKind.UInt64 -> false

    /// The integral kind a lexed token's `NumericKind` denotes, `ValueNone` for the
    /// non-integral ones: the floats, `decimal`, and the custom `NumericLiteral` suffixes.
    let ofNumericKind (kind: NumericKind) : IntKind voption =
        match kind with
        | NumericKind.SByte -> ValueSome IntKind.SByte
        | NumericKind.Byte -> ValueSome IntKind.Byte
        | NumericKind.Int16 -> ValueSome IntKind.Int16
        | NumericKind.UInt16 -> ValueSome IntKind.UInt16
        | NumericKind.Int32 -> ValueSome IntKind.Int32
        | NumericKind.UInt32 -> ValueSome IntKind.UInt32
        | NumericKind.Int64 -> ValueSome IntKind.Int64
        | NumericKind.UInt64 -> ValueSome IntKind.UInt64
        | NumericKind.NativeInt -> ValueSome IntKind.NativeInt
        | NumericKind.UNativeInt -> ValueSome IntKind.UNativeInt
        | _ -> ValueNone

[<RequireQualifiedAccess>]
type IntValue =
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | NativeInt of int64
    | UNativeInt of uint64

/// A per-bit operation on two integral values of one kind. The result depends on the
/// operands' bits alone, so a pointer-width operand folds.
[<RequireQualifiedAccess>]
type IntBitwiseOp =
    | Or
    | And
    | Xor

/// Wrapping arithmetic on two integral values of one kind, computed at that kind's width.
[<RequireQualifiedAccess>]
type IntArithOp =
    | Add
    | Subtract
    | Multiply

/// A bit shift. The right operand is an `int32` count rather than a value of the shifted
/// kind, and a signed kind shifts right arithmetically.
[<RequireQualifiedAccess>]
type IntShiftOp =
    | Left
    | Right

/// Why an integral operation yields no value.
[<RequireQualifiedAccess>]
type IntFoldRejection =
    /// The operands are of two kinds; a binary integral operation takes one.
    | KindMismatch
    /// The result depends on the operand's width, and a pointer-width kind takes the
    /// target's.
    | TargetWidth
    /// The shift count lies outside `[0, width)`, which the targets mask differently.
    | ShiftCount

module IntValue =

    let kind (v: IntValue) : IntKind =
        match v with
        | IntValue.SByte _ -> IntKind.SByte
        | IntValue.Byte _ -> IntKind.Byte
        | IntValue.Int16 _ -> IntKind.Int16
        | IntValue.UInt16 _ -> IntKind.UInt16
        | IntValue.Int32 _ -> IntKind.Int32
        | IntValue.UInt32 _ -> IntKind.UInt32
        | IntValue.Int64 _ -> IntKind.Int64
        | IntValue.UInt64 _ -> IntKind.UInt64
        | IntValue.NativeInt _ -> IntKind.NativeInt
        | IntValue.UNativeInt _ -> IntKind.UNativeInt

    /// `n` wrapped onto `k`'s width: the low bits of the two's-complement pattern.
    let ofInt64 (k: IntKind) (n: int64) : IntValue =
        match k with
        | IntKind.SByte -> IntValue.SByte(sbyte n)
        | IntKind.Byte -> IntValue.Byte(byte n)
        | IntKind.Int16 -> IntValue.Int16(int16 n)
        | IntKind.UInt16 -> IntValue.UInt16(uint16 n)
        | IntKind.Int32 -> IntValue.Int32(int32 n)
        | IntKind.UInt32 -> IntValue.UInt32(uint32 n)
        | IntKind.Int64 -> IntValue.Int64 n
        | IntKind.UInt64 -> IntValue.UInt64(uint64 n)
        | IntKind.NativeInt -> IntValue.NativeInt n
        | IntKind.UNativeInt -> IntValue.UNativeInt(uint64 n)

    let zero (k: IntKind) : IntValue = ofInt64 k 0L

    /// Read a magnitude at `k`'s width, range-checked by the `Convert` overload
    /// (`Convert.ToByte("300", 10)` throws). Throws `OverflowException` / `FormatException`
    /// for a magnitude or sign the kind cannot hold.
    let parse (k: IntKind) (digits: string) (radix: int) : IntValue =
        match k with
        | IntKind.SByte -> IntValue.SByte(Convert.ToSByte(digits, radix))
        | IntKind.Byte -> IntValue.Byte(Convert.ToByte(digits, radix))
        | IntKind.Int16 -> IntValue.Int16(Convert.ToInt16(digits, radix))
        | IntKind.UInt16 -> IntValue.UInt16(Convert.ToUInt16(digits, radix))
        | IntKind.Int32 -> IntValue.Int32(Convert.ToInt32(digits, radix))
        | IntKind.UInt32 -> IntValue.UInt32(Convert.ToUInt32(digits, radix))
        | IntKind.Int64 -> IntValue.Int64(Convert.ToInt64(digits, radix))
        | IntKind.UInt64 -> IntValue.UInt64(Convert.ToUInt64(digits, radix))
        | IntKind.NativeInt -> IntValue.NativeInt(Convert.ToInt64(digits, radix))
        | IntKind.UNativeInt -> IntValue.UNativeInt(Convert.ToUInt64(digits, radix))

    /// The value's decimal text, as the .NET primitive's `ToString()` prints it.
    let render (v: IntValue) : string =
        match v with
        | IntValue.SByte n -> string n
        | IntValue.Byte n -> string n
        | IntValue.Int16 n -> string n
        | IntValue.UInt16 n -> string n
        | IntValue.Int32 n -> string n
        | IntValue.UInt32 n -> string n
        | IntValue.Int64 n -> string n
        | IntValue.UInt64 n -> string n
        | IntValue.NativeInt n -> string n
        | IntValue.UNativeInt n -> string n

    /// The value boxed at its own .NET primitive type, as a CLR enum-case field constant must
    /// carry it: the metadata writer reads the box's runtime type.
    let boxed (v: IntValue) : obj =
        match v with
        | IntValue.SByte n -> box n
        | IntValue.Byte n -> box n
        | IntValue.Int16 n -> box n
        | IntValue.UInt16 n -> box n
        | IntValue.Int32 n -> box n
        | IntValue.UInt32 n -> box n
        | IntValue.Int64 n -> box n
        | IntValue.UInt64 n -> box n
        | IntValue.NativeInt n -> box (nativeint n)
        | IntValue.UNativeInt n -> box (unativeint n)

    /// Two's-complement negation at the value's own width: `-(-128y)` is `-128y` and
    /// `-Int32.MinValue` is itself. `ValueNone` for an unsigned value, since F# defines no
    /// negation on one.
    let negate (v: IntValue) : IntValue voption =
        match v with
        | IntValue.SByte n -> ValueSome(IntValue.SByte(-n))
        | IntValue.Int16 n -> ValueSome(IntValue.Int16(-n))
        | IntValue.Int32 n -> ValueSome(IntValue.Int32(-n))
        | IntValue.Int64 n -> ValueSome(IntValue.Int64(-n))
        | IntValue.NativeInt n -> ValueSome(IntValue.NativeInt(-n))
        | IntValue.Byte _
        | IntValue.UInt16 _
        | IntValue.UInt32 _
        | IntValue.UInt64 _
        | IntValue.UNativeInt _ -> ValueNone

    /// `l op r` at the operands' shared kind; `ValueNone` where the kinds differ.
    let bitwise (op: IntBitwiseOp) (l: IntValue) (r: IntValue) : IntValue voption =
        let inline apply (a: ^a) (b: ^a) : ^a =
            match op with
            | IntBitwiseOp.Or -> a ||| b
            | IntBitwiseOp.And -> a &&& b
            | IntBitwiseOp.Xor -> a ^^^ b

        match l, r with
        | IntValue.SByte a, IntValue.SByte b -> ValueSome(IntValue.SByte(apply a b))
        | IntValue.Byte a, IntValue.Byte b -> ValueSome(IntValue.Byte(apply a b))
        | IntValue.Int16 a, IntValue.Int16 b -> ValueSome(IntValue.Int16(apply a b))
        | IntValue.UInt16 a, IntValue.UInt16 b -> ValueSome(IntValue.UInt16(apply a b))
        | IntValue.Int32 a, IntValue.Int32 b -> ValueSome(IntValue.Int32(apply a b))
        | IntValue.UInt32 a, IntValue.UInt32 b -> ValueSome(IntValue.UInt32(apply a b))
        | IntValue.Int64 a, IntValue.Int64 b -> ValueSome(IntValue.Int64(apply a b))
        | IntValue.UInt64 a, IntValue.UInt64 b -> ValueSome(IntValue.UInt64(apply a b))
        | IntValue.NativeInt a, IntValue.NativeInt b -> ValueSome(IntValue.NativeInt(apply a b))
        | IntValue.UNativeInt a, IntValue.UNativeInt b -> ValueSome(IntValue.UNativeInt(apply a b))
        | _ -> ValueNone

    /// `l op r` at the operands' shared kind, wrapping at that kind's width. A pointer-width
    /// operand is refused, since the wrap is the target's.
    let arithmetic (op: IntArithOp) (l: IntValue) (r: IntValue) : Result<IntValue, IntFoldRejection> =
        let inline apply (a: ^a) (b: ^a) : ^a =
            match op with
            | IntArithOp.Add -> a + b
            | IntArithOp.Subtract -> a - b
            | IntArithOp.Multiply -> a * b

        match l, r with
        | IntValue.SByte a, IntValue.SByte b -> Ok(IntValue.SByte(apply a b))
        | IntValue.Byte a, IntValue.Byte b -> Ok(IntValue.Byte(apply a b))
        | IntValue.Int16 a, IntValue.Int16 b -> Ok(IntValue.Int16(apply a b))
        | IntValue.UInt16 a, IntValue.UInt16 b -> Ok(IntValue.UInt16(apply a b))
        | IntValue.Int32 a, IntValue.Int32 b -> Ok(IntValue.Int32(apply a b))
        | IntValue.UInt32 a, IntValue.UInt32 b -> Ok(IntValue.UInt32(apply a b))
        | IntValue.Int64 a, IntValue.Int64 b -> Ok(IntValue.Int64(apply a b))
        | IntValue.UInt64 a, IntValue.UInt64 b -> Ok(IntValue.UInt64(apply a b))
        | IntValue.NativeInt _, IntValue.NativeInt _
        | IntValue.UNativeInt _, IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth
        | _ -> Error IntFoldRejection.KindMismatch

    /// `v` shifted by `count` bits, logically for an unsigned kind and arithmetically for a
    /// signed one. A count within the kind's width shifts identically on every target.
    let shift (op: IntShiftOp) (v: IntValue) (count: int) : Result<IntValue, IntFoldRejection> =
        let inline at (width: int) (mk: ^a -> IntValue) (a: ^a) : Result<IntValue, IntFoldRejection> =
            if count < 0 || count >= width then
                Error IntFoldRejection.ShiftCount
            else
                match op with
                | IntShiftOp.Left -> Ok(mk (a <<< count))
                | IntShiftOp.Right -> Ok(mk (a >>> count))

        match v with
        | IntValue.SByte a -> at 8 IntValue.SByte a
        | IntValue.Byte a -> at 8 IntValue.Byte a
        | IntValue.Int16 a -> at 16 IntValue.Int16 a
        | IntValue.UInt16 a -> at 16 IntValue.UInt16 a
        | IntValue.Int32 a -> at 32 IntValue.Int32 a
        | IntValue.UInt32 a -> at 32 IntValue.UInt32 a
        | IntValue.Int64 a -> at 64 IntValue.Int64 a
        | IntValue.UInt64 a -> at 64 IntValue.UInt64 a
        | IntValue.NativeInt _
        | IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth

    /// `~~~v`, every bit of the value's own width inverted.
    let complement (v: IntValue) : Result<IntValue, IntFoldRejection> =
        match v with
        | IntValue.SByte a -> Ok(IntValue.SByte(~~~a))
        | IntValue.Byte a -> Ok(IntValue.Byte(~~~a))
        | IntValue.Int16 a -> Ok(IntValue.Int16(~~~a))
        | IntValue.UInt16 a -> Ok(IntValue.UInt16(~~~a))
        | IntValue.Int32 a -> Ok(IntValue.Int32(~~~a))
        | IntValue.UInt32 a -> Ok(IntValue.UInt32(~~~a))
        | IntValue.Int64 a -> Ok(IntValue.Int64(~~~a))
        | IntValue.UInt64 a -> Ok(IntValue.UInt64(~~~a))
        | IntValue.NativeInt _
        | IntValue.UNativeInt _ -> Error IntFoldRejection.TargetWidth

[<RequireQualifiedAccess>]
type NumericLiteralValue =
    | Integral of IntValue
    | Float of double
    | Float32 of single
    | Decimal of decimal

/// Why a literal token carries no primitive constant.
[<RequireQualifiedAccess>]
type NumericLiteralRejection =
    /// A custom numeric literal, whose suffix F# translates to a call into the
    /// `NumericLiteral<suffix>` module in scope (`52I` = `NumericLiteralI.FromInt32 52`), or
    /// a suffix F# reserves and gives no meaning to. Neither projects to a constant.
    | CustomLiteral
    /// The lexed magnitude or sign does not fit the classified kind: an out-of-range
    /// magnitude (`300uy`), or a negative-signed *unsigned* literal (`-1uy`, formed by the
    /// negative-literal merge).
    | OutOfRange
    /// The token is not a numeric literal, or carries a `NumericKind` outside the declared
    /// set (a 5-bit field, so F# cannot prove the match exhaustive). A producer bug at every
    /// call site, not a user error.
    | NotNumeric

module NumericLiterals =

    /// The .NET radix for a classified base: 2/8/10/16 is the only set the
    /// `Convert.To…(string, int)` overloads accept, decimal included.
    let private baseRadix (numBase: NumericBase) : int =
        match numBase with
        | NumericBase.Hex -> 16
        | NumericBase.Octal -> 8
        | NumericBase.Binary -> 2
        | _ -> 10

    /// The bare digit span of an integer magnitude: drop the trailing width-suffix letters
    /// (`u`/`l`/`y`/`s`/`n`, any case, none of which collide with the hex digits `a`–`f`),
    /// then the two-char radix prefix for a non-decimal base, then the digit-group underscores.
    let private intDigits (numBase: NumericBase) (text: string) : string =
        let mutable hi = text.Length

        while hi > 0
              && (
                  match Char.ToLowerInvariant text.[hi - 1] with
                  | 'u'
                  | 'l'
                  | 'y'
                  | 's'
                  | 'n' -> true
                  | _ -> false
              ) do
            hi <- hi - 1

        let lo = if numBase = NumericBase.Decimal then 0 else 2
        text.Substring(lo, hi - lo).Replace("_", "")

    /// The classified numeric kind of a token, `ValueNone` when it is not a numeric literal.
    /// The public read of the lexer's flag field, whose radix axis is already collapsed:
    /// `10y` and `0x0Ay` are both `NumericKind.SByte`.
    let numericKindOf (token: Token) : NumericKind voption =
        if TokenInfo.isNumeric token then
            ValueSome(TokenInfo.numericKind token)
        else
            ValueNone

    /// Strip a single trailing suffix, case-insensitively: the float (`f`) and decimal (`m`)
    /// markers.
    let private stripSuffix (suffix: string) (text: string) : string =
        if text.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    /// Parse a numeric literal's value from its raw source `text`, at the kind and base the
    /// lexer recorded on `token`. A non-decimal magnitude is read as its two's-complement bit
    /// pattern (`0xFFFFFFFF` is `-1`), matching F#'s wrap semantics for radix literals.
    let parseNumericLiteral (token: Token) (text: string) : Result<NumericLiteralValue, NumericLiteralRejection> =
        if not (TokenInfo.isNumeric token) then
            Error NumericLiteralRejection.NotNumeric
        else
            let numBase = TokenInfo.numericBase token
            let radix = baseRadix numBase
            let kind = TokenInfo.numericKind token

            // An integral kind reads its magnitude through `IntValue.parse`, whose
            // `Convert` overload is that kind's range check. The kinds `ofNumericKind`
            // declines are the non-integral ones, and each parses its own way.
            let project () =
                match IntKind.ofNumericKind kind with
                | ValueSome k -> Ok(NumericLiteralValue.Integral(IntValue.parse k (intDigits numBase text) radix))
                | ValueNone ->

                    match kind with
                    | NumericKind.IEEE64 ->
                        Ok(NumericLiteralValue.Float(Double.Parse(text, CultureInfo.InvariantCulture)))
                    | NumericKind.IEEE32 ->
                        Ok(
                            NumericLiteralValue.Float32(
                                Single.Parse(stripSuffix "f" text, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.Decimal ->
                        Ok(
                            NumericLiteralValue.Decimal(
                                Decimal.Parse(stripSuffix "m" text, NumberStyles.Float, CultureInfo.InvariantCulture)
                            )
                        )
                    | NumericKind.BigIntegerQ
                    | NumericKind.BigIntegerR
                    | NumericKind.BigIntegerZ
                    | NumericKind.BigIntegerI
                    | NumericKind.BigIntegerN
                    | NumericKind.BigIntegerG
                    | NumericKind.ReservedNumericLiteral -> Error NumericLiteralRejection.CustomLiteral
                    // `NumericKind` is a 5-bit field, so a hand-built token can carry a value
                    // outside the declared cases. That is a corrupt token, not a literal.
                    | _ -> Error NumericLiteralRejection.NotNumeric

            try
                project ()
            with
            | :? OverflowException
            | :? FormatException -> Error NumericLiteralRejection.OutOfRange
