---
category: Documentation
categoryindex: 0
index: 5
---

# Handling Errors

A good parser doesn't just know how to succeed; it knows how to fail gracefully. Parsing is often an iterative process, and clear, actionable error messages are crucial for debugging grammars. XParsec is designed with this in mind, making error reporting a first-class feature.

## The `ParseResult` and `ParseError` Types

Every parser in XParsec returns a `ParseResult<'Parsed, 'T, 'State>`, which is an alias for the standard F# `Result` type. On failure, this will be an `Error` case containing a `ParseError` record.

This record gives you the raw data about the failure:

- `Position`: A `Position<'State>` struct indicating where the error occurred (the index in the input stream and the user state at that point).
- `Errors`: An `ErrorType` discriminated union that describes the nature of the failure (e.g., an expected character was not found, the end of input was reached, etc.).

While you can inspect this record manually, the real power comes from turning it into a human-readable report.

For more details on these types, see the [Core Types](./core-types.md) documentation.

## Formatting Errors with `ErrorFormatting`

While the `ParseError` record contains all the necessary information, it's not very readable on its own. The `XParsec.ErrorFormatting` module provides functions to turn these records into helpful, human-readable strings.

The primary function for this is `formatStringError`.

```fsharp
ErrorFormatting.formatStringError (input: string) (error: ParseError<char, 'State>) : string
```

This function takes the original input string and the `ParseError` record and produces a formatted report including:

1. The line of code where the error occurred.
2. A caret (`^`) pointing to the exact column of the error.
3. The line and column number.
4. A clear message explaining what the parser expected or what went wrong.

### Example Usage

Let's see it in action with a basic parser that fails.

```fsharp
open XParsec
open XParsec.CharParsers
open XParsec.Combinators

let text = "The quick brown fox."

// A parser that expects the string "The slow..."
let pTheSlow = pstring "The slow"

// Run the parser on our input text
let result = pTheSlow (Reader.ofString text ())

match result with
| Ok _ ->
    printfn "This should not have succeeded!"
| Error e ->
    // Use the formatter to create a nice error message
    let errorMsg = ErrorFormatting.formatStringError text e
    printfn "%s" errorMsg
```

This will produce the following output:

```text
The quick brown fox.
    ^ At index 4 (Ln 1, Col 5)
Expected "slow"
```

### Nested Errors

Combinators like `choice` or `(<|>)` produce nested errors when all of their child parsers fail. The error formatter will display these in a structured, indented tree, making it easy to see why a complex parser failed.

Consider this example:

```fsharp
let pComplex = pstring "The " .>>. choice [ pchar 'a'; pchar 'b'; pchar 'c' ]

let text = "The quick brown fox."

match pComplex (Reader.ofString text ()) with
| Ok _ -> failwith "Should have failed"
| Error e ->
    let errorMsg = ErrorFormatting.formatStringError text e
    printfn "%s" errorMsg
```

This will output a message showing that all choices failed, and why each one failed individually:

```text
The quick brown fox.
    ^ At index 4 (Ln 1, Col 5)
All choices failed.
├───Expected 'a'
├───Expected 'b'
└───Expected 'c'
```

This structured output is invaluable for debugging complex parsers.

## Custom Error Messages

You can provide custom error messages using the `(<?>)` **label operator**. This will replace the default error message for a specific parser if it fails *without consuming any input*. This is a common and useful constraint that prevents hiding more specific error messages from deeper in the parse.

```fsharp
let pWithCustomError = pchar 'a' <?> "I was really expecting an 'a' here!"

match pWithCustomError (Reader.ofString "test" ()) with
| Ok _ -> failwith "Should have failed"
| Error e ->
    let errorMsg = ErrorFormatting.formatStringError "test" e
    printfn "%s" errorMsg
```

Output:

```text
test
^ At index 0 (Ln 1, Col 1)
I was really expecting an 'a' here!
```
