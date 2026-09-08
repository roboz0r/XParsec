namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

module internal LiteralTypes =

    let private unknown = TyUnknown UnknownReason.NoValueType

    let semType (intrinsics: IntrinsicSet) (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> intrinsics.Bool
        | Token.CharLiteral -> intrinsics.Char
        | tok ->

            match NumericLiterals.numericKindOf tok with
            | ValueNone -> unknown
            | ValueSome kind ->
                // `NumericKind` because radix is not part of a literal's type:
                // `10y` / `0x0Ay` / `0o12y` / `0b1010y` are all `sbyte`.
                match kind with
                | NumericKind.SByte -> intrinsics.SByte
                | NumericKind.Byte -> intrinsics.Byte
                | NumericKind.Int16 -> intrinsics.Int16
                | NumericKind.UInt16 -> intrinsics.UInt16
                | NumericKind.Int32 -> intrinsics.Int
                | NumericKind.UInt32 -> intrinsics.UInt32
                | NumericKind.Int64 -> intrinsics.Int64
                | NumericKind.UInt64 -> intrinsics.UInt64
                | NumericKind.NativeInt -> intrinsics.NativeInt
                | NumericKind.UNativeInt -> intrinsics.UNativeInt
                | NumericKind.IEEE32 -> intrinsics.Float32
                | NumericKind.IEEE64 -> intrinsics.Float
                | NumericKind.Decimal -> intrinsics.Decimal
                | NumericKind.BigIntegerI -> intrinsics.BigInt
                // TODO: Individually kinded custom integers
                | NumericKind.BigIntegerQ
                | NumericKind.BigIntegerR
                | NumericKind.BigIntegerZ
                | NumericKind.BigIntegerN
                | NumericKind.BigIntegerG
                // `NumericKind` is an enum, so the wildcard is required: a reserved or
                // otherwise meaningless suffix does not map to a type.
                | NumericKind.ReservedNumericLiteral -> unknown
                | _ -> unreachable $"NumericKind: {kind} was not handled."
