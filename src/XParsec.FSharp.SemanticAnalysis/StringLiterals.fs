namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// String-literal folding primitives shared by the enum-case value projection and the
// Elaborate constant parsers. Takes the token → text read as a function, so it sits ahead of
// every pass that needs it.

module internal StringLiterals =

    /// Concatenate the literal text of every string part via `nameOf`, rendering an
    /// interpolation hole (`StringPart.Expr`) through `onHole`. An escape denoting no
    /// character (`Lexing.decodeStringEscape`'s non-`Text` cases) is surfaced through
    /// `onInvalid` at its token and kept verbatim in the fold.
    let foldStringParts
        (nameOf: SyntaxToken -> string)
        (onHole: unit -> string)
        (onInvalid: SyntaxToken -> Kind -> unit)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : string =
        let sb = System.Text.StringBuilder()

        let appendEscape (t: SyntaxToken) =
            // The raw source span of the token: backslash then `n`, two chars. Decoding it
            // here is what makes a literal `"\n"` a newline instead of two characters.
            let raw = nameOf t

            match Lexing.decodeStringEscape raw with
            | Lexing.DecodedEscape.Text text -> sb.Append text |> ignore
            | Lexing.DecodedEscape.TrigraphOutOfRange ->
                onInvalid t (Kind.EscapeTrigraphOutOfRange raw)
                sb.Append raw |> ignore
            | Lexing.DecodedEscape.NotUnicodeScalar ->
                onInvalid t (Kind.EscapeNotUnicodeScalar raw)
                sb.Append raw |> ignore

        for part in parts do
            match part with
            // A `Text` part can carry an escape-sequence TOKEN.
            | StringPart.Text t ->
                match t.Token with
                | Token.EscapeSequence -> appendEscape t
                | _ -> sb.Append(nameOf t) |> ignore
            | StringPart.EscapeSequence t -> appendEscape t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(nameOf t) |> ignore
            | StringPart.Expr _ -> sb.Append(onHole ()) |> ignore

        sb.ToString()
