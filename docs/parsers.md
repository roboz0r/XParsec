---
category: Documentation
categoryindex: 0
index: 1
---

# Common Parsers

Primitive parsers are the fundamental building blocks of XParsec. They are the functions that directly interact with the input stream by consuming items, or with the parser's environment by inspecting its state and position. Every complex parser is ultimately built by combining these primitives.

This page lists the core parsers available in the `XParsec.Parsers` module.

## How to Read the Examples

The examples below show how to call each parser. To see what they do, you can run them with a simple test function like this:

```fsharp
open XParsec

// A helper to run a parser and print its result
let testParser parser input =
    match parser (Reader.ofString input ()) with
    | Ok success -> printfn $"Input: \"{input}\" -> Ok {success.Parsed}"
    | Error err ->
        let errorMsg = ErrorFormatting.formatStringError input err
        printfn $"Input: \"{input}\" -> Error:\n{errorMsg}"

// Example usage:
testParser (pitem 'a') "abc"
// Prints: Input: "abc" -> Ok a
```

---

## General-Purpose Parsers

These are the most basic parsers for controlling success and failure.

| Parser | Description | Example Usage |
|---|---|---|
| `preturn x` | Always succeeds with the value `x` without consuming input. | `preturn 42` |
| `pzero` | Always fails with a generic error. Useful as an identity for `choice`. | `pzero` |
| `fail msg`| Always fails with a specific error message `msg`. | `fail "Something went wrong"` |
| `eof` | Succeeds (returning `unit`) only if at the end of the input stream. | `pIdentifier .>> eof` |
| `pid` | Consumes and returns a single item of any kind. Fails at the end of input. | `pid` |
| `skip` | Consumes a single item of any kind. Returns unit. Fails at the end of input. | `skip` |

---

## Item and Sequence Parsers

These parsers match specific items or sequences in the input.

| Parser | Description | Example Usage |
|---|---|---|
| `pitem i` | Parses a single item `i`. Fails if the next item is different. | `pitem 'a'` |
| `skipItem i` | Parses and discards a single item `i`. Returns `unit`. | `skipItem ' '` |
| `pseq s` | Parses a sequence of items `s` (e.g., a `string` or `array`). | `pseq "hello"` |
| `pseqReturn s x` | Parses sequence `s`, but returns the value `x` on success. | `pstring "true" >>% true` |
| `satisfy p`| Parses a single item that satisfies the predicate `p`. | `satisfy Char.IsDigit`|
| `anyOf cs`| Parses any single item from the sequence `cs`. | `anyOf ['a'; 'e'; 'i'; 'o'; 'u']`|
| `noneOf cs` | Parses any single item *not* in the sequence `cs`. | `noneOf ['\n'; '\r']`|
| `skipAnyOf cs`| Like `anyOf`, but discards the result. | `skipAnyOf " \t"` |
| `anyInRange min max`| Parses any single item within the inclusive range. | `anyInRange 'a' 'z'` |
| `skipAnyInRange min max`| Like `anyInRange`, but discards the result. | `skipAnyInRange '0' '9'`|

---

## State and Position Parsers

These parsers allow you to interact with the parser's environment: the user-defined state and the reader's current position. This is a powerful feature for context-sensitive parsing, such as tracking indentation levels in Python or managing a symbol table in a compiler.

For many parsers with simple grammars, a custom state is unnecessary. In these cases, you can simply use `unit` as the state type, and the state-related functions can be ignored.

| Parser | Description | Example Usage |
|---|---|---|
| `getUserState` | Returns the current user-defined state. | `getUserState` |
| `setUserState s` | Sets the user state to the new value `s`. | `setUserState 10` |
| `updateUserState f` | Applies function `f` to the current state to produce a new state. | `updateUserState ((+) 1)` |
| `userStateSatisfies p`| Succeeds if the current state satisfies predicate `p`. Fails otherwise. | `userStateSatisfies ((=) 0)` |
| `getPosition` | Returns the current `Position` struct, which can be saved for later. | `getPosition` |
| `setPosition pos` | Jumps the parser to a previously saved `Position`. **Note:** This will fail if the `Position` came from a different `Reader`. | `setPosition savedPosition` |

### Example: Using State to Count

Let's create a parser that consumes exactly `N` occurrences of the character 'a', where `N` is read from the initial state.

```fsharp
// This parser uses an int as its state, representing a counter.
let pConsumeAs (state: int) =
    // A parser that consumes a single 'a' and decrements the counter.
    let pSingleA = parser {
        do! pitem 'a'
        do! updateUserState (fun i -> i - 1)
    }

    // A parser that succeeds only when the counter is zero.
    let pCheckCounter = userStateSatisfies ((=) 0)

    // The full parser: repeat pSingleA `state` times, then check the counter.
    skipArray state pSingleA >>. pCheckCounter

// Let's run it with an initial state of 3.
testParser (pConsumeAs 3) "aaa"
// Input: "aaa" -> Ok ()

testParser (pConsumeAs 3) "aa"
// Input: "aa" -> Error:
//  --> 1:3
//   |
// 1 | aa
//   |   ^
//   | Unexpected end of input. Expected 'a'.
```

### Example: Manual Backtracking with Position

`getPosition` and `setPosition` are the tools for manual backtracking. You can "try" a parse and then reset the stream's position if you decide to do something else.

```fsharp
// Let's parse an identifier, but if it's "let", we want to parse "let-binding" instead.
let pIdentifier = many1Chars (satisfy Char.IsLower)

// This parser demonstrates a manual lookahead.
let pLookahead = parser {
    // 1. Save our current position.
    let! startPos = getPosition

    // 2. Try parsing an identifier.
    let! id = pIdentifier

    // 3. Check if the identifier is "let".
    if id = "let" then
        // It is! We don't want the simple identifier.
        // 4. Reset the stream to where we started.
        do! setPosition startPos
        // 5. Run a different parser.
        return! pseq "let-binding"
    else
        // It's not "let", so the simple identifier is fine.
        return id
}

testParser pLookahead "let-binding"
// Input: "let-binding" -> Ok let-binding

testParser pLookahead "variable"
// Input: "variable" -> Ok variable
```
