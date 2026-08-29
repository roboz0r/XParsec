namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// String-literal folding primitives shared by the enum-case value projection and the
// Elaborate constant parsers. Takes the token → text read as a function, so it sits ahead of
// every pass that needs it.

/// The characters one literal-content token of a string body denotes.
[<RequireQualifiedAccess>]
type internal DecodedLiteralToken =
    | Text of text: string
    /// The escape denotes no character: the diagnostic to raise at its token, and the raw
    /// source text to keep in its place.
    | Invalid of kind: Kind * raw: string

module internal StringLiterals =

    /// Decode one literal-content token: an escape sequence to the characters it denotes, a
    /// verbatim doubled quote to a single `"`, any other token to its own source text. `%%`
    /// stays two characters — whether a format pass will collapse it is the caller's to know.
    let decodeLiteralToken (nameOf: SyntaxToken -> string) (t: SyntaxToken) : DecodedLiteralToken =
        let raw = nameOf t

        match t.Token with
        | Token.EscapeSequence ->
            match Lexing.decodeStringEscape raw with
            | Lexing.DecodedEscape.Text text -> DecodedLiteralToken.Text text
            | Lexing.DecodedEscape.TrigraphOutOfRange ->
                DecodedLiteralToken.Invalid(Kind.EscapeTrigraphOutOfRange raw, raw)
            | Lexing.DecodedEscape.NotUnicodeScalar -> DecodedLiteralToken.Invalid(Kind.EscapeNotUnicodeScalar raw, raw)
        | Token.VerbatimEscapeQuote -> DecodedLiteralToken.Text "\""
        | _ -> DecodedLiteralToken.Text raw

    /// Concatenate every string part decoded by `decodeLiteralToken`, rendering an
    /// interpolation hole (`StringPart.Expr`) through `onHole`. An escape denoting no
    /// character is surfaced through `onInvalid` at its token and kept verbatim in the fold.
    /// `%%` survives as two characters, which is what a plain string literal means and what
    /// the runtime format engine expects of a `PrintfFormat` body.
    let foldStringParts
        (nameOf: SyntaxToken -> string)
        (onHole: unit -> string)
        (onInvalid: SyntaxToken -> Kind -> unit)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        let appendLiteral (t: SyntaxToken) =
            match decodeLiteralToken nameOf t with
            | DecodedLiteralToken.Text text -> sb.Append text |> ignore
            | DecodedLiteralToken.Invalid(kind, raw) ->
                onInvalid t kind
                sb.Append raw |> ignore

        for part in parts do
            match part with
            // A plain string's body collapses to `Text` parts, so the TOKEN is what
            // distinguishes an escape or a doubled quote from ordinary text.
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t -> appendLiteral t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()
