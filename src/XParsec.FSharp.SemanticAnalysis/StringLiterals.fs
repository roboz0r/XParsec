namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// String-literal folding primitives shared across passes so what counts as a
// string constant — and how its escapes decode — cannot drift between the
// NameResolution enum-case reader (`TypeRegistration.registerEnumTypeDefn`),
// the Freeze constant parsers, and `Elaborate.resolveEnumCaseValue`. Depends only
// on `PassContext.NameOf` + the parser string-part shape, so it sits ahead of the
// passes (earlier than Freeze, where these primitives used to live).

module internal StringLiterals =

    /// Decode one backslash escape body (`inner` starts with `\`) to its char.
    /// The escape set mirrors the lexer's `pCharChar` (Lexing.fs) exactly — a
    /// literal that reaches here already lexed clean, so any unexpected shape is a
    /// broken invariant. Shared by `FreezeLiterals.parseCharLiteral` (a `'\n'` char
    /// literal) and `foldStringParts` (a `\n` *string*-part escape) so the two never
    /// diverge.
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

    /// Concatenate the literal text of every string part via `ctx.NameOf`,
    /// rendering an interpolation hole (`StringPart.Expr`) through `onHole`.
    /// Shared by the IL-intrinsic and literal-string stitchers, which differ
    /// only in how a hole renders.
    let foldStringParts
        (ctx: PassContext)
        (onHole: unit -> string)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        for part in parts do
            match part with
            // The parser folds every string fragment — including escape-sequence
            // tokens (`\n`, `\t`, `\"`, `\uXXXX`) — into a `StringPart.Text`
            // carrying the raw 2+-char source span (`ctx.NameOf` = `\n`, two
            // chars). Decode an escape *token* to the single char it denotes; a
            // plain text fragment appends verbatim. Without this a literal `"\n"`
            // value would emit a backslash-n, not a newline.
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

    /// Project an enum case VALUE expression onto its decoded string-literal form.
    /// Peels a value-grouping paren (`| A = ("auto")`) and admits every plain string
    /// kind (plain / verbatim / triple-quoted), folding escapes through
    /// `foldStringParts` — `ValueNone` for an interpolated string (`$"…"`, no
    /// constant value) or any non-string expression. The single reader for BOTH the
    /// early literal-union admission (`registerEnumTypeDefn`) and the authoritative
    /// case-table resolution (`Elaborate.resolveEnumCaseValue`), so the two cannot
    /// disagree on which cases carry a string constant.
    let rec tryEnumCaseStringLiteral (ctx: PassContext) (v: Expr<SyntaxToken>) : string voption =
        match v with
        | Expr.EnclosedBlock(expr = inner) -> tryEnumCaseStringLiteral ctx inner
        | Expr.String(kind = (StringKind.String _ | StringKind.VerbatimString _ | StringKind.String3 _); parts = parts) ->
            ValueSome(foldStringParts ctx (fun () -> "") parts)
        | _ -> ValueNone
