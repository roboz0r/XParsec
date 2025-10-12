---
category: Documentation
categoryindex: 0
index: 3
---

# The Parser Computation Expression

While operators are powerful, the most readable way to sequence multiple parsing steps is often with a `parser { ... }` computation expression. It allows you to write parsers in a familiar, sequential style.

```fsharp
open XParsec.CharParsers

let pJsonString = between (pchar '"') (pchar '"') (manyChars (pitem '\\' >>. pid <|> noneOf ['"']))
let pJsonNumber = pfloat |>> string

let pKeyValuePair = parser {
    let! key = pJsonString
    do! pchar ':'
    let! value = pJsonString <|> pJsonNumber
    return (key, value)
}
```

This computation expression is the most efficient way to chain parsers. It takes advantage of the monadic form of computation expressions and uses `inline` and the `InlineIfLambda` attribute to compile down to highly efficient code with almost zero overhead.

## Available Operations

The `parser` computation expression builder defines a number of methods that translate keywords within the block into specific parser operations.

| Keyword | Method | Description |
|---|---|---|
| `let!` | `Bind` | Executes a parser and binds its successful result to a variable. This is the primary way to sequence parsers. |
| `do!` | `Bind` | Executes a parser that returns `unit`, either to consume input or to modify the parser state. |
| `return`| `Return` | Wraps a value in a parser that always succeeds with that value, consuming no input. |
| `return!`| `ReturnFrom`| Returns an existing parser, effectively continuing with that parser. |
| `let! ... return`| `BindReturn` | A combination of `Bind` and `Return` that maps the result of a parser. `let! x = p in return (f x)` is compiled to this. |
| `try...with`| `TryWith` | Allows for exception handling within the parser expression. |
| `try...finally`| `TryFinally`| Ensures a piece of code is executed, whether the parser succeeds, fails, or throws an exception. |
| `use` | `Using` | Scopes a disposable resource, ensuring `Dispose()` is called when the parser block completes. |
| `while` | `While` | Executes a parser repeatedly as long as a guard condition is true. The parser must return `unit`. |
| `for` | `For` | Iterates over a sequence, executing a unit-returning parser for each element. |
| (sequencing) | `Combine` | Used implicitly to sequence two parsers where the result of the first is discarded. In particular `while` and `for` operations. |
| (empty) | `Zero` | Represents a parser that always fails. It is used when a branch of a conditional should fail, for example, `if false then return 1`. |
| (lazy) | `Delay` | Allows for the creation of recursive parsers by delaying the construction of the parser function until it's executed. |

### `let!`

Sequences parsers by running one, binding its result, and then using that result to create the next parser in the sequence.

**Example:**

```fsharp
let nameAndAge = parser {
    let! name = manyChars letter
    do! pchar ':'
    let! age = pint32
    return (name, age)
}

// run nameAndAge "Alice:30"
// -> Ok ("Alice", 30)
```

### `do!`

Used to execute a parser when its result is not needed (i.e., it returns `unit`). This is common in two main scenarios:

1. **Consuming Input:** For parsers that match and consume parts of the input stream that are not part of the final result, such as separators, keywords, or whitespace.
2. **Managing State:** For parsers that interact with the user state without consuming input, like `setUserState` or `updateUserState`.

**Example:**
This parser parses a `let` binding and stores the variable in the parser's user state, which is a `Map`.

```fsharp
open XParsec.State

// Parses "let x=10" and updates the state map.
let pLetBinding = parser {
    // 1. Consume input without capturing a result
    do! pstring "let"
    do! skipMany1Satisfies System.Char.IsWhiteSpace

    let! varName = many1Chars letter
    do! pchar '='
    let! value = pint32

    // 2. Modify state without consuming input
    do! updateUserState (Map.add varName value)
}

// To run this parser:
// let initialState = Map.empty<string, int>
// let reader = Reader.ofString "let x=10" initialState
// match run pLetBinding reader with
// | Ok result ->
//     // result.Parsed is ()
//     // reader.State is Map [("x", 10)]
// | Error e -> ...
```

### `return`

Takes a regular value and lifts it into a parser that successfully returns that value without consuming any input.

**Example:**

```fsharp
let pDefault = parser {
    // some complex logic...
    return "default value"
}
```

### `try...with`

Catches exceptions that might be thrown during the parsing process.

**Example:**

```fsharp
let pWithExceptionHandling = parser {
    try
        // A parser that might throw an exception
        let! num = pstring "123" |>> (fun s -> int s / 0)
        return num
    with
    | :? System.DivideByZeroException -> return -1
}
```

### `while`

Repeatedly executes a `unit`-returning parser as long as a condition holds.

**Example:**

```fsharp
let mutable count = 3
let pWhile = parser {
    while count > 0 do
        count <- count - 1
        do! pchar 'a'
}
// run pWhile "aaa"
// -> Ok () and count will be 0
```

### `for`

Iterates over a sequence and executes a unit-returning parser for each element in the sequence. This is useful for parsing repeating structures where the pattern is driven by an external collection. The overall for loop returns unit.

**Example:**

```fsharp
let keywords = [ "begin"; "let"; "end" ]
let pWhitespace = skipMany1Satisfies System.Char.IsWhiteSpace

// A parser that consumes "begin let end" with whitespace between them
let pKeywordSequence = parser {
    let mutable first = true
    for keyword in keywords do
        if not first then
            // All keywords after the first must be preceded by whitespace
            do! pWhitespace
        do! pstring keyword
        first <- false
}

// run pKeywordSequence "begin let end"
// -> Ok ()

// run pKeywordSequence "begin   let   end"
// -> Ok ()
```
