namespace XParsec.FSharp.Parser

open System
open System.Text
open XParsec.FSharp

/// Semantic-projection settings for `StringValues.evaluate`. Controls how source
/// bytes captured verbatim in the CST get cooked into a runtime string value.
[<RequireQualifiedAccess>]
type TripleQuoteIndentMode =
    /// Source fragment text is concatenated as-is.
    | Off
    /// C# 11 raw-string semantics: closing-delimiter column establishes the
    /// margin, that prefix is stripped from each content line, leading/trailing
    /// bracketing newlines are stripped.
    | CSharpStyle

[<RequireQualifiedAccess>]
type NewlineMode =
    /// Source line endings (CRLF, LF, CR) pass through unchanged.
    | Preserve
    /// Source line endings inside fragments are normalised to LF. Explicit `\n`
    /// / `\r` escape sequences are never rewritten — they always mean the byte
    /// they spell.
    | Lf

type EvaluateConfig =
    {
        TripleQuoteIndent: TripleQuoteIndentMode
        NewlineNormalization: NewlineMode
    }

    static member Legacy =
        {
            TripleQuoteIndent = TripleQuoteIndentMode.Off
            NewlineNormalization = NewlineMode.Preserve
        }

    static member FSharp2 =
        {
            TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
            NewlineNormalization = NewlineMode.Lf
        }

/// Diagnostic produced by `StringValues.tryEvaluate` when the cooked value
/// can't be derived under the requested config (e.g. a content line is
/// indented less than the triple-quote closing delimiter).
[<RequireQualifiedAccess>]
type StringValueError =
    /// Content line at the given 0-based offset within the literal is indented
    /// less than the closing delimiter's margin.
    | ContentLineUnderindented of contentIndex: int * expectedMargin: string * actualPrefix: string
    /// The closing delimiter's prefix mixes tabs and spaces; we can't strip a
    /// margin under an exact-prefix-match rule.
    | MixedIndentInClosingDelimiter of margin: string

module StringValues =

    let private cookHexDigit (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else -1

    /// Decode the chars *after* the leading backslash of an escape sequence
    /// into one or two output chars (UTF-16 code units). The fragment text
    /// `esc` always starts with `\\`; behaviour matches the F# lexer's
    /// classification in `pStringEscapeToken` (Lexing.fs).
    let private cookEscape (esc: string) (sb: StringBuilder) =
        if esc.Length < 2 then
            sb.Append(esc) |> ignore
        else
            match esc.[1] with
            | '"' -> sb.Append('"') |> ignore
            | '\\' -> sb.Append('\\') |> ignore
            | '\'' -> sb.Append('\'') |> ignore
            | 'n' -> sb.Append('\n') |> ignore
            | 't' -> sb.Append('\t') |> ignore
            | 'b' -> sb.Append('\b') |> ignore
            | 'r' -> sb.Append('\r') |> ignore
            | 'a' -> sb.Append('\a') |> ignore
            | 'f' -> sb.Append('\f') |> ignore
            | 'v' -> sb.Append('\v') |> ignore
            | 'u' when esc.Length = 6 ->
                let mutable v = 0
                let mutable ok = true

                for i in 2..5 do
                    let d = cookHexDigit esc.[i]
                    if d < 0 then ok <- false else v <- (v <<< 4) ||| d

                if ok then
                    sb.Append(char v) |> ignore
                else
                    sb.Append(esc) |> ignore
            | 'U' when esc.Length = 10 ->
                let mutable v = 0
                let mutable ok = true

                for i in 2..9 do
                    let d = cookHexDigit esc.[i]
                    if d < 0 then ok <- false else v <- (v <<< 4) ||| d

                if ok then
                    sb.Append(Char.ConvertFromUtf32(v)) |> ignore
                else
                    sb.Append(esc) |> ignore
            | 'x' when esc.Length = 4 ->
                let d1 = cookHexDigit esc.[2]
                let d2 = cookHexDigit esc.[3]

                if d1 >= 0 && d2 >= 0 then
                    sb.Append(char ((d1 <<< 4) ||| d2)) |> ignore
                else
                    sb.Append(esc) |> ignore
            | c when c >= '0' && c <= '9' && esc.Length = 4 ->
                let d1 = int esc.[1] - int '0'
                let d2 = int esc.[2] - int '0'
                let d3 = int esc.[3] - int '0'

                if d2 >= 0 && d2 <= 9 && d3 >= 0 && d3 <= 9 then
                    sb.Append(char (d1 * 100 + d2 * 10 + d3)) |> ignore
                else
                    sb.Append(esc) |> ignore
            | _ -> sb.Append(esc) |> ignore

    /// Apply CRLF/CR → LF normalisation to a fragment of source text, copying
    /// into `sb`. When normalisation is off, copies verbatim.
    let private appendNormalised (mode: NewlineMode) (text: string) (sb: StringBuilder) =
        match mode with
        | NewlineMode.Preserve -> sb.Append(text) |> ignore
        | NewlineMode.Lf ->
            let mutable i = 0
            let n = text.Length

            while i < n do
                let c = text.[i]

                if c = '\r' then
                    sb.Append('\n') |> ignore

                    if i + 1 < n && text.[i + 1] = '\n' then
                        i <- i + 2
                    else
                        i <- i + 1
                else
                    sb.Append(c) |> ignore
                    i <- i + 1

    /// Compute the closing-delimiter margin for a triple-quoted literal. Walks
    /// back from the opening `"` of the closing `"""` to the most recent
    /// newline (or start of source) and returns the prefix between them. The
    /// margin string is the exact characters to strip from each content line.
    let private closingMargin (source: string) (closeStart: int) =
        let mutable i = closeStart - 1

        while i >= 0 && source.[i] <> '\n' && source.[i] <> '\r' do
            i <- i - 1

        let prefixStart = i + 1
        // Margin is source[prefixStart .. closeStart-1]; it should be only
        // whitespace under a well-formed input (parser/lexer guarantees the
        // closing `"""` is at column-start or preceded only by indentation).
        if closeStart > prefixStart then
            source.Substring(prefixStart, closeStart - prefixStart)
        else
            ""

    /// True if `s` is composed entirely of space or tab.
    let private isAllWhitespacePrefix (s: string) =
        let mutable i = 0
        let mutable ok = true

        while ok && i < s.Length do
            let c = s.[i]

            if c <> ' ' && c <> '\t' then
                ok <- false

            i <- i + 1

        ok

    /// True if `margin` mixes tabs and spaces.
    let private isMixedTabsSpaces (margin: string) =
        let mutable hasTab = false
        let mutable hasSpace = false

        for c in margin do
            if c = '\t' then
                hasTab <- true
            elif c = ' ' then
                hasSpace <- true

        hasTab && hasSpace

    /// Strip `margin` characters from each line in `cooked`, in place into
    /// `sb`, applying C# 11 indentation rules. Returns ValueNone on success or
    /// ValueSome(error) on the first violation.
    let private appendDedented (margin: string) (cooked: string) (sb: StringBuilder) =
        if margin.Length = 0 then
            sb.Append(cooked) |> ignore
            ValueNone
        else
            let mutable err = ValueNone
            let mutable i = 0
            let mutable contentIndex = 0
            let n = cooked.Length
            let mutable atLineStart = true

            while ValueOption.isNone err && i < n do
                if atLineStart then
                    // Try to consume up to margin.Length chars matching margin
                    // exactly. A blank line (just \n) is allowed even if shorter
                    // than margin.
                    let lineEnd =
                        let mutable j = i

                        while j < n && cooked.[j] <> '\n' && cooked.[j] <> '\r' do
                            j <- j + 1

                        j

                    if lineEnd = i then
                        // Blank line — emit nothing, move to newline handling.
                        atLineStart <- false
                    else
                        // Compare margin to source.[i .. i+margin.Length-1]
                        let available = lineEnd - i
                        let toMatch = min margin.Length available

                        let mutable matched = true
                        let mutable k = 0

                        while matched && k < toMatch do
                            if cooked.[i + k] <> margin.[k] then
                                matched <- false

                            k <- k + 1

                        if not matched then
                            let actual = cooked.Substring(i, toMatch)

                            err <- ValueSome(StringValueError.ContentLineUnderindented(contentIndex, margin, actual))
                        elif toMatch < margin.Length then
                            // Line ended before margin fully matched. If the
                            // whole prefix matched as far as it went and the
                            // rest is just whitespace (blank line), allow it.
                            // Otherwise it's underindented.
                            let actual = cooked.Substring(i, toMatch)

                            err <- ValueSome(StringValueError.ContentLineUnderindented(contentIndex, margin, actual))
                        else
                            i <- i + margin.Length
                            atLineStart <- false
                else
                    let c = cooked.[i]
                    sb.Append(c) |> ignore
                    i <- i + 1

                    if c = '\n' then
                        atLineStart <- true
                        contentIndex <- contentIndex + 1
                    elif c = '\r' then
                        // CR or CRLF — handled by caller's normalisation; here
                        // we treat CR/LF/CRLF as line breaks.
                        atLineStart <- true
                        contentIndex <- contentIndex + 1

            err

    /// Strip the bracketing newline that immediately follows the opening
    /// triple-quote (if any) and the one immediately before the closing
    /// triple-quote (if any). Returns the trimmed string.
    let private stripBracketingNewlines (s: string) =
        let n = s.Length

        let start =
            if n >= 2 && s.[0] = '\r' && s.[1] = '\n' then 2
            elif n >= 1 && (s.[0] = '\n' || s.[0] = '\r') then 1
            else 0

        let mutable stop = n

        if stop >= 1 && s.[stop - 1] = '\n' then
            if stop >= 2 && s.[stop - 2] = '\r' then
                stop <- stop - 2
            else
                stop <- stop - 1
        elif stop >= 1 && s.[stop - 1] = '\r' then
            stop <- stop - 1

        if start = 0 && stop = n then s
        elif start >= stop then ""
        else s.Substring(start, stop - start)

    /// True for the three triple-quoted kinds.
    let private isTripleQuoted (kind: StringKind<'T>) =
        match kind with
        | StringKind.String3 _
        | StringKind.Interpolated3String _ -> true
        | _ -> false

    /// True for the two verbatim kinds.
    let private isVerbatim (kind: StringKind<'T>) =
        match kind with
        | StringKind.VerbatimString _
        | StringKind.VerbatimInterpolatedString _ -> true
        | _ -> false

    /// Compute the runtime string value of a non-interpolated literal. For
    /// interpolated kinds the helper returns `ValueNone` because expression
    /// holes can't be evaluated here — call sites that need interpolation must
    /// walk parts themselves (see `appendFragmentText`).
    ///
    /// `getText` resolves a token to its source substring. `closeStartIndex`
    /// is the source offset of the opening `"` of the closing delimiter; for
    /// non-triple kinds it's unused and may be 0.
    let tryEvaluate
        (getText: 'T -> string)
        (source: string)
        (closeStartIndex: int)
        (config: EvaluateConfig)
        (kind: StringKind<'T>)
        (parts: ImArr<StringPart<'T>>)
        : Result<string, StringValueError> =
        match kind with
        | StringKind.InterpolatedString _
        | StringKind.VerbatimInterpolatedString _
        | StringKind.Interpolated3String _ ->
            // Interpolated strings have Expr holes; evaluation isn't well-defined
            // at this layer. Caller must process parts itself.
            Ok ""
        | _ ->
            let verbatim = isVerbatim kind
            let triple = isTripleQuoted kind
            let sb = StringBuilder()

            for part in parts do
                match part with
                | StringPart.Text t ->
                    let raw = getText t

                    if verbatim || triple then
                        appendNormalised config.NewlineNormalization raw sb
                    else
                        appendNormalised config.NewlineNormalization raw sb
                | StringPart.EscapeSequence t ->
                    // Only plain "..." emits EscapeSequence tokens; verbatim/triple
                    // pass them through as fragment text. Defensive: still cook.
                    cookEscape (getText t) sb
                | StringPart.VerbatimEscapeQuote _ -> sb.Append('"') |> ignore
                | StringPart.FormatSpecifier t
                | StringPart.OrphanFormatSpecifier t -> sb.Append(getText t) |> ignore
                | StringPart.EscapePercent _ -> sb.Append('%') |> ignore
                | StringPart.InvalidText t -> sb.Append(getText t) |> ignore
                | StringPart.Expr _ ->
                    // Unreachable for non-interpolated; ignore.
                    ()

            let cooked = sb.ToString()

            if triple && config.TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle then
                let margin = closingMargin source closeStartIndex

                if margin.Length = 0 then
                    Ok(stripBracketingNewlines cooked)
                elif isMixedTabsSpaces margin then
                    Error(StringValueError.MixedIndentInClosingDelimiter margin)
                elif not (isAllWhitespacePrefix margin) then
                    // Closing delimiter has non-whitespace before it — emit
                    // unchanged rather than error (defensive; parser should
                    // already have rejected this).
                    Ok(stripBracketingNewlines cooked)
                else
                    let outSb = StringBuilder(cooked.Length)

                    match appendDedented margin cooked outSb with
                    | ValueNone -> Ok(stripBracketingNewlines (outSb.ToString()))
                    | ValueSome e -> Error e
            else
                Ok cooked

    /// Format a `StringValueError` as a human-readable diagnostic message.
    /// Consumers can use this directly or render the structured fields
    /// themselves for richer UIs (e.g. squiggles in an editor).
    let formatError (err: StringValueError) =
        match err with
        | StringValueError.ContentLineUnderindented(idx, expected, actual) ->
            $"Triple-quoted string content line {idx} is indented less than the closing delimiter "
            + $"(expected prefix %A{expected}, got %A{actual}). The closing \"\"\" establishes the margin "
            + "that is stripped from each content line."
        | StringValueError.MixedIndentInClosingDelimiter margin ->
            $"The closing \"\"\" of a triple-quoted string is preceded by a mix of tabs and spaces "
            + $"(%A{margin}). The dedent algorithm requires tabs and spaces to match exactly between "
            + "the closing delimiter and each content line; pick one and use it consistently."

    /// Cook a single Text or EscapeSequence fragment in isolation. Useful for
    /// interpolated-string consumers that interleave hole evaluation with text.
    /// Triple-quote dedent is NOT applied here — that operation needs the
    /// whole-literal context (closing-delimiter column).
    let appendFragmentText
        (getText: 'T -> string)
        (config: EvaluateConfig)
        (part: StringPart<'T>)
        (sb: StringBuilder)
        : unit =
        match part with
        | StringPart.Text t -> appendNormalised config.NewlineNormalization (getText t) sb
        | StringPart.EscapeSequence t -> cookEscape (getText t) sb
        | StringPart.VerbatimEscapeQuote _ -> sb.Append('"') |> ignore
        | StringPart.FormatSpecifier t
        | StringPart.OrphanFormatSpecifier t -> sb.Append(getText t) |> ignore
        | StringPart.EscapePercent _ -> sb.Append('%') |> ignore
        | StringPart.InvalidText t -> sb.Append(getText t) |> ignore
        | StringPart.Expr _ -> ()
