# XParsec

XParsec is a modern parser combinator library for F#. It allows you to build powerful, type-safe, and efficient parsers by composing small, reusable functions. It's designed from the ground up to provide excellent performance, great error messages, and a developer-friendly API that works seamlessly in both .NET and Fable projects.

## Getting Started

### Installation

You can add XParsec to your project via NuGet:

```bash
dotnet add package XParsec
```

### Your First Parser

The best way to learn XParsec is to build something. We'll write a parser for a simple configuration file format. The file looks like this:

```ini
# config.txt
name = "XParsec"
version = 1.2
is_beta = true
# Comments should be ignored
```

Our goal is to parse this into a list of key-value pairs, ignoring comments and whitespace.

#### 1. Define the Data Types

First, let's define the F# types that will represent our successfully parsed data. This is a great practice as it makes the goal of our parser clear.

```fsharp
open System

// A discriminated union for the different value types we can parse
type ConfigValue =
    | String of string
    | Float of float
    | Bool of bool

// A record to hold a single key-value pair
type KeyValuePair = { Key: string; Value: ConfigValue }
```

#### 2. Writing the Parsers

Now, we'll build up our parser piece by piece. We'll start with the smallest elements of our format and combine them into a parser for the whole file.

```fsharp
open XParsec
open XParsec.CharParsers
open XParsec.Combinators

// -- Basic Building Blocks --

// A parser for a valid identifier (our keys), which must have at least one character.
let pIdentifier =
    many1Chars (satisfy (fun c -> Char.IsLetterOrDigit c || c = '_'))

// A parser for a string literal enclosed in double quotes.
// `between` runs three parsers in sequence and returns the result of the middle one.
let pQuotedString =
    between (pchar '"') (pchar '"') (manyChars (noneOf ['"']))

// A parser for comments, starting with '#' and consuming to the end of the line.
let pComment = pchar '#' >>. skipMany (satisfy (fun c -> c <> '\n'))

// A helper to parse any whitespace or comments. We'll use this to clean up.
// `skipMany` repeatedly runs a parser, consuming input but returning nothing (unit).
// `<|>` is the "choice" operator: it tries the left parser, and if it fails, tries the right.
let pWhitespace = skipMany (pws1 <|> pComment)

// -- Value Parsers --

// Now we parse the specific values. We use `|>>` (the map operator)
// to transform the parsed result into our `ConfigValue` DU cases.
let pString = pQuotedString |>> ConfigValue.String
let pFloat = pfloat |>> ConfigValue.Float

// For booleans, we can be more explicit.
// `pstring "true" >>% true` parses the literal string "true" and returns the boolean `true`.
// We use `<|>` again to choose between the "true" and "false" parsers.
let pBool =
    (pstring "true" >>% true <|> pstring "false" >>% false)
    |>> ConfigValue.Bool

// The `choice` combinator tries a list of parsers in order until one succeeds.
let pValue = choice [ pString; pFloat; pBool ]

// -- Combining Everything --

// Now we define a parser for a full key-value pair line using a computation expression.
let pKeyValuePair = parser {
    let! key = pIdentifier
    do! pWhitespace >>. pchar '=' >>. pWhitespace // Consume whitespace and the '='
    let! value = pValue
    return { Key = key; Value = value }
}

// Finally, the parser for the entire file.
// `sepBy` parses zero or more `pKeyValuePair`s, separated by newlines.
// We wrap it all in `pWhitespace` to handle leading/trailing space or comments.
let pConfigFile = between pWhitespace pWhitespace (sepBy pKeyValuePair skipNewline)
```

#### 3. Running the Parser

With our `pConfigFile` parser defined, we can run it on our input.

```fsharp
let configText = """
# My Awesome Config
name = "XParsec"
version = 1.2
is_beta = true
"""

// The `run` function is the entry point for executing a parser.
// It takes the parser to run and the input string.
let result = run pConfigFile configText

// Handle the result using pattern matching
match result with
| Ok success ->
    printfn "Successfully parsed config:"
    for kvp in success.Parsed do
        printfn $"- {kvp.Key}: {kvp.Value}"
| Error err ->
    // This case is handled in the next section.
    printfn "An error occurred."
```

This will produce the output:

```log
Successfully parsed config:
- name: String "XParsec"
- version: Float 1.2
- is_beta: Bool true
```

#### 4. Handling Parse Errors

One of XParsec's key features is its ability to generate human-readable error messages. Let's see what happens with invalid input.

```fsharp
let invalidConfigText = """
name = "XParsec"
version = 1.2a  # Invalid float
is_beta = true
"""

let result = run pConfigFile invalidConfigText

match result with
| Ok _ -> () // This won't be hit
| Error err ->
    // The ErrorFormatting module can create a nicely formatted report.
    let errorMsg = ErrorFormatting.formatStringError invalidConfigText err
    printfn "Error parsing config:\n%s" errorMsg
```

// REVIEW: This is a crucial addition. Showing the actual error output is a massive selling point.
This produces a clear, helpful error message pointing directly to the problem:

```log
Error parsing config:

 --> 2:11
  |
2 | version = 1.2a  # Invalid float
  |           ^
  | Unexpected 'a'. Expected digit or '.'.
```

This example demonstrates the core philosophy of XParsec: start with small, simple building blocks, and combine them using powerful operators and functions to create a readable, robust, and type-safe parser for any format.
