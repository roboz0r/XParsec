namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// String-literal folding primitives shared by the enum-case value projection and the
// Elaborate constant parsers. Takes the token → text read as a function, so it sits ahead of
// the `.fsi` package extractor as well as the passes.

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

    /// Concatenate the literal text of every string part via `nameOf`, rendering an
    /// interpolation hole (`StringPart.Expr`) through `onHole`.
    let foldStringParts
        (nameOf: SyntaxToken -> string)
        (onHole: unit -> string)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            // A `Text` part can carry an escape-sequence TOKEN, whose `nameOf` is the
            // raw source span: backslash then `n`, two chars. Decoding it here is what
            // makes a literal `"\n"` a newline instead of two characters.
            | StringPart.Text t ->
                match t.Token with
                | Token.EscapeSequence -> sb.Append(decodeEscape (nameOf t)) |> ignore
                | _ -> sb.Append(nameOf t) |> ignore
            | StringPart.EscapeSequence t -> sb.Append(decodeEscape (nameOf t)) |> ignore
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()
