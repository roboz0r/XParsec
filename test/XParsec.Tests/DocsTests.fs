module DocsTests

open System

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.Parsers
open XParsec.CharParsers

// A discriminated union for the different value types we can parse
type ConfigValue =
    | String of string
    | Float of float
    | Bool of bool

// A record to hold a single key-value pair
type KeyValuePair = { Key: string; Value: ConfigValue }

// -- Basic Building Blocks --

// A parser for a valid identifier (our keys), which must have at least one character.
let pIdentifier = many1Chars (satisfy (fun c -> Char.IsLetterOrDigit c || c = '_'))

// A parser for a string literal enclosed in double quotes.
// `between` runs three parsers in sequence and returns the result of the middle one.
let pQuotedString = between (pchar '"') (pchar '"') (manyChars (noneOf [ '"' ]))

// A parser for comments, starting with '#' and consuming to the end of the line.
let pComment = pchar '#' >>. skipMany (satisfy (fun c -> c <> '\n'))

// A helper to parse any whitespace or comments. We'll use this to clean up.
// `skipMany` repeatedly runs a parser, consuming input but returning nothing (unit).
// `<|>` is the "choice" operator: it tries the left parser, and if it fails, tries the right.
let pWhitespaceOrComment = skipMany (spaces1 <|> pComment)

// -- Value Parsers --

// Now we parse the specific values. We use `|>>` (the map operator)
// to transform the parsed result into our `ConfigValue` DU cases.
let pString = pQuotedString |>> ConfigValue.String

let pFloat =
    pfloat .>> notFollowedBy (satisfy (fun c -> Char.IsLetter c))
    |>> ConfigValue.Float

// For booleans, we can be more explicit.
// `pstring "true" >>% true` parses the literal string "true" and returns the boolean `true`.
// We use `<|>` again to choose between the "true" and "false" parsers.
let pBool =
    pstring "true" >>% true <|> (pstring "false" >>% false) |>> ConfigValue.Bool

// The `choice` combinator tries a list of parsers in order until one succeeds.
let pValue = choice [ pString; pFloat; pBool ]

// -- Combining Everything --

// Now we define a parser for a full key-value pair line using a computation expression.
let pKeyValuePair =
    parser {
        let! key = pIdentifier
        do! pWhitespaceOrComment >>. pchar '=' >>. pWhitespaceOrComment // Consume whitespace and the '='
        let! value = pValue
        return { Key = key; Value = value }
    }

// Finally, the parser for the entire file.
// `many` parses zero or more `pKeyValuePair`s, separated by newlines.
// We wrap it all in `pWhitespaceOrComment` to handle leading/trailing space or comments.
// `.>> eof` is a common way to assert the parser should consume all input.
let pConfigFile =
    between pWhitespaceOrComment pWhitespaceOrComment (many (pKeyValuePair .>> skipNewline))
    .>> eof

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "DocsTests"
        [
            test "Index" {

                let configText =
                    """
# My Awesome Config
name = "XParsec"
version = 1.2
is_beta = true
                """

                let result = Reader.ofString configText () |> pConfigFile

                // Handle the result using pattern matching
                match result with
                | Ok success ->
                    printfn "Successfully parsed config:"

                    for kvp in success.Parsed do
                        printfn $"- {kvp.Key}: {kvp.Value}"
                | Error err ->
                    // This case is handled in the next section.
                    printfn "An error occurred."
            }

            test "Index with Error" {

                let invalidConfigText =
                    """
name = "XParsec"
version = 1.2a  # Invalid float
is_beta = true
                """

                let result = Reader.ofString invalidConfigText () |> pConfigFile

                match result with
                | Ok _ -> () // This won't be hit
                | Error err ->
                    // The ErrorFormatting module can create a nicely formatted report.
                    let errorMsg = ErrorFormatting.formatStringError invalidConfigText err
                    printfn "Error parsing config:\n%s" errorMsg
            }
        ]
