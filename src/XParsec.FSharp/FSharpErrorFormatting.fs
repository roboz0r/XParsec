module XParsec.FSharp.Parser.ErrorFormatting

open System.Text

open XParsec
open XParsec.ErrorFormatting
open XParsec.FSharp.Lexer

let formatTokenError (error: ParseError<PositionedToken, ParseState>) =
    let state = error.Position.State
    let input = state.Input
    let tokens = state.Lexed.Tokens

    // 1. Map the token index to the character index in the original string
    let tokenIndex = error.Position.Index

    let stringIndex =
        if tokenIndex < 0 then
            0
        elif tokenIndex < int tokens.Length then
            let tIndex = tokenIndex * 1<token>
            tokens.[tIndex].StartIndex
        else
            // If the parser hits Unexpected EndOfInput, the token index
            // will be out-of-bounds. Default to the end of the string.
            input.Length

    // 2. Setup the existing line/string helpers
    let index = LineIndex.OfString input
    let readable = ReadableString(input, 0, input.Length)

    // 3. Define how to format individual and multiple tokens
    let formatOne (x: PositionedToken) (sb: StringBuilder) =
        // PositionedToken.Token returns the enum, so .ToString() gives a readable name
        sb.Append('`').Append(x.Token.ToString()).Append('`')

    let formatSeq (xs: PositionedToken seq) (sb: StringBuilder) =
        let formattedTokens = xs |> Seq.map (fun x -> $"`{x.Token}`") |> String.concat ", "

        sb.Append(formattedTokens)

    // 4. Build and return the formatted error
    StringBuilder()
    |> formatErrorsLine index readable stringIndex
    |> formatParseError formatOne formatSeq error
    |> _.ToString()

/// Flattens the error tree, groups by Index (furthest first), and formats each group.
let splitAndFormatTokenErrors (error: ParseError<PositionedToken, ParseState>) =

    // 1. Flatten the tree into isolated (Position, ErrorType) pairs
    let rec flatten (e: ParseError<PositionedToken, ParseState>) =
        match e.Errors with
        | Nested(parentType, children) ->
            // Extract the parent message, then recursively extract children
            (e.Position, parentType) :: List.collect flatten children
        | leafType -> [ e.Position, leafType ]

    // 2. Group by Index and sort descending (so the furthest index prints first)
    flatten error
    |> List.groupBy (fun (pos, _) -> pos.Index)
    |> List.sortBy fst
    |> List.map (fun (index, group) ->

        let pos = group |> List.head |> fst
        let errorTypes = group |> List.map snd

        // 3. Reconstruct a localized ParseError for this specific index
        let localizedErrorType =
            match errorTypes with
            | [ single ] -> single
            | multiple ->
                // Re-nest multiple errors occurring at the same index
                let children = multiple |> List.map (fun et -> { Position = pos; Errors = et })
                Nested(Message "Multiple errors:", children)

        let localizedError =
            {
                Position = pos
                Errors = localizedErrorType
            }

        // 4. Format using your single-error formatter (from the previous step)
        formatTokenError localizedError
    )
    // 5. Separate different failure locations with double-newlines
    |> String.concat "\n\n---\n\n"

let formatFurthestTokenError (error: ParseError<PositionedToken, ParseState>) =
    let rec flatten (e: ParseError<PositionedToken, ParseState>) =
        match e.Errors with
        | Nested(parentType, children) -> (e.Position, parentType) :: List.collect flatten children
        | leafType -> [ e.Position, leafType ]

    // 1. Flatten and group by index
    let grouped = flatten error |> List.groupBy (fun (pos, _) -> pos.Index)

    // 2. ONLY take the group that progressed the furthest in the file
    let (furthestIndex, group) = grouped |> List.maxBy fst

    // 3. Reconstruct and format
    let pos = group |> List.head |> fst
    let errorTypes = group |> List.map snd

    let localizedErrorType =
        match errorTypes |> List.distinct with // List.distinct helps remove duplicate generic messages
        | [ single ] -> single
        | multiple ->
            let children = multiple |> List.map (fun et -> { Position = pos; Errors = et })
            Nested(Message "Multiple errors at furthest point:", children)

    formatTokenError
        {
            Position = pos
            Errors = localizedErrorType
        }
