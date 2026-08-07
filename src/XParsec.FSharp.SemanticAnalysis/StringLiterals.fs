namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// String-literal folding primitives shared by the NameResolution enum-case reader and the
// Elaborate constant parsers. Depends only on `PassContext.NameOf` and the parser's
// string-part shape, so it sits ahead of the passes.

module internal StringLiterals =

    /// Decode one backslash escape body (`inner` starts with `\`) to its char. The set is the
    /// CHAR-literal one; the string lexer also admits `\UXXXXXXXX`, a truncated `\uXX` and any
    /// unknown escape, and each of those reaches the `failwithf` below.
    let decodeEscape (inner: string) : char =
        match inner.[1] with
        | '"' -> '"'
        | '\\' -> '\\'
        | '\'' -> '\''
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'b' -> '\b'
        | 'r' -> '\r'
        | 'a' -> '\a'
        | 'f' -> '\f'
        | 'v' -> '\v'
        | 'u' ->
            char (
                System.UInt16.Parse(
                    inner.Substring(2, 4),
                    System.Globalization.NumberStyles.AllowHexSpecifier,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            )
        | 'x' ->
            char (
                System.Byte.Parse(
                    inner.Substring(2, 2),
                    System.Globalization.NumberStyles.AllowHexSpecifier,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            )
        | d when System.Char.IsDigit d ->
            // Trigraph `\DDD` (decimal byte).
            char (System.Int32.Parse(inner.Substring(1, 3), System.Globalization.CultureInfo.InvariantCulture))
        | other -> failwithf "StringLiterals.decodeEscape: unsupported escape '\\%c' in %s" other inner

    /// Concatenate the literal text of every string part via `ctx.NameOf`, rendering an
    /// interpolation hole (`StringPart.Expr`) through `onHole`.
    let foldStringParts
        (ctx: PassContext)
        (onHole: unit -> string)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            // A `Text` part can carry an escape-sequence TOKEN, whose `ctx.NameOf` is the
            // raw source span — backslash then `n`, two chars. Decoding it here is what
            // makes a literal `"\n"` a newline instead of two characters.
            | StringPart.Text t ->
                match t.Token with
                | Token.EscapeSequence -> sb.Append(decodeEscape (ctx.NameOf t)) |> ignore
                | _ -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.EscapeSequence t -> sb.Append(decodeEscape (ctx.NameOf t)) |> ignore
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()

    /// An enum case VALUE expression as its decoded string constant: peels a value-grouping
    /// paren (`| A = ("auto")`) and admits plain / verbatim / triple-quoted strings.
    /// `ValueNone` for an interpolated string (`$"…"`, no constant value) or a non-string.
    let rec tryEnumCaseStringLiteral (ctx: PassContext) (v: Expr<SyntaxToken>) : string voption =
        match v with
        | Expr.EnclosedBlock(expr = inner) -> tryEnumCaseStringLiteral ctx inner
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            ValueSome(foldStringParts ctx (fun () -> "") parts)
        | _ -> ValueNone
