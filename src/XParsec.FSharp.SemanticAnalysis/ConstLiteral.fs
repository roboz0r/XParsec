namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The scalar a written literal token denotes.
module ConstLiteral =

    /// Decodes the (possibly escaped) single character of a char-literal token.
    let private parseCharLiteral (text: string) : char =
        let inner = text.Substring(1, text.Length - 2)

        if inner.Length = 1 then
            inner.[0]
        else
            match Lexing.decodeCharEscape inner with
            | ValueSome c -> c
            | ValueNone -> failwithf "ConstLiteral.parseCharLiteral: unexpected char literal text %s" text

    /// Projection of a bool / char / numeric `Constant` onto `TConstValue`.
    let tryValue
        (nameOf: SyntaxToken -> string)
        (c: Constant<SyntaxToken>)
        : Result<TConstValue, NumericLiteralRejection> =
        let t =
            match c with
            | Constant.Literal t
            | Constant.MeasuredLiteral(value = t) -> t

        let text = nameOf t

        match t.Token with
        | Token.KWTrue -> Ok(TConstValue.Bool true)
        | Token.KWFalse -> Ok(TConstValue.Bool false)
        | Token.CharLiteral -> Ok(TConstValue.Char(parseCharLiteral text))
        | _ ->
            match NumericLiterals.parseNumericLiteral t.Token text with
            | Ok(NumericLiteralValue.Integral v) -> Ok(TConstValue.Integral v)
            | Ok(NumericLiteralValue.Float n) -> Ok(TConstValue.Float n)
            | Ok(NumericLiteralValue.Float32 n) -> Ok(TConstValue.Float32 n)
            | Ok(NumericLiteralValue.Decimal n) -> Ok(TConstValue.Decimal n)
            | Error e -> Error e
