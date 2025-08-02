---
category: Documentation
categoryindex: 0
index: 0
---

# Core Concepts

XParsec is a functional parser combinator library for F#. It's built around a single, powerful idea: **a parser is just a function**.

Instead of creating complex parser objects, you write small, focused functions that parse one piece of your input. You then combine, or "compose," these small functions to build a parser for your entire language or data format. This functional approach makes your parsers modular, reusable, and easy to test.

## The Parser Function

At its heart, a parser is a function that takes the current input state and returns a result indicating success or failure. In its simplest form, you can think of it like this:

```fsharp
type Parser = Reader -> ParseResult
```

This simple `Reader -> ParseResult` signature is the foundation. To make it powerful and flexible, XParsec uses generics to allow you to parse almost any kind of input into any kind of F# type. This leads to the full type definition:

```fsharp
type Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    Reader<'T, 'State, 'Input, 'InputSlice> -> ParseResult<'Parsed, 'T, 'State>
```

Let's break down those generic parameters. They give you complete control over your parsing environment:

- `'Parsed`: The type of the value you want to produce (e.g., `int`, `string`, a custom `User` record).
- `'T`: The type of the items in your input stream (e.g., `char` for a string, `byte` for a binary format).
- `'State`: A user-defined state that you can thread through the parsing process. This is perfect for tasks like managing indentation levels or symbol tables. Defaults to `unit` if you don't need it.
- `'Input`: The type of the input stream itself.
- `'InputSlice`: The type of a slice of the input stream.

Because parsers are just functions, you can use standard F# operators like `>>` for composition, or you can use the rich set of combinators provided by XParsec (like `>>=` for sequencing or `<|>` for choice) to build sophisticated parsers from simple ones.

> **When to use `'InputSlice`**
>
> In most cases, you can ignore the `'InputSlice` parameter. Its main power comes from parsing nested grammars. Imagine a binary format where a 32-bit integer specifies the length of a sub-message that follows. You can use a parser to read the length, then "slice" the input to that length and pass the slice to a *different* set of parsers to process the sub-message.

Now, let's look at the input and output of this function.

## Reader: Parser Input

The input to every parser is a `Reader`. Think of the `Reader` as a smart cursor that moves over your input data. It tracks the current position and holds your custom state.

```fsharp
type Reader<'T, 'State, 'Input, 'InputSlice>
```

> It's important to understand the `Reader`, but, in most cases you won't need to interact with this type directly. Parser and combinator functions implicitly thread the reader through your overall parser.

### Key Members

The `Reader` provides a simple API for navigating the input:

- `State`: Gets or sets the user-defined state.
- `Index`: Gets or sets the current position in the input.
- `Peek()`: Looks at the next item without consuming it.
- `PeekN(count: int)`: Returns a `Span<'T>` containing up to the requested number of items.
- `Skip()`: Advances the position by one.
- `SkipN(count: int)`: Advances the position by the provided value.
- `TryRead()`: Tries to read the next item and advances the position if successful.
- `AtEnd`: Returns `true` if the reader is at the end of the input.
- `Position`: Gets or sets a `Position` struct representing the current position of the reader.

The `Reader` gets its data from an `IReadable` source.

### Creating a Reader

You'll typically start a parsing job by creating a `Reader` from your source data. The `Reader` module provides convenient helper functions for this.

For example, to parse a string, you use `Reader.ofString`:

```fsharp
open XParsec

// 1. Your input data
let input = "hello world"

// 2. A parser to run (here, a primitive that parses the character 'h')
let myParser = pchar 'h'

// 3. Create a reader from the input string with an initial state of `()`
let reader = Reader.ofString input ()

// 4. Run the parser on the reader
let result = myParser reader
// val result : Result<char, ParseError<...>> = Ok { Parsed = 'h' }
```

The `Reader` module includes helpers for the most common input types:

| Function | Description | .NET | Fable |
|---|---|---|---|
| `ofString` | Creates a reader from a `string`. | ✔️ | ✔️ |
| `ofArray` | Creates a reader from an `'T array`. | ✔️ | ✔️ |
| `ofImmutableArray` | Creates a reader from an `ImmutableArray<'T>`. | ✔️ | ✔️ |
| `ofResizeArray` | Creates a reader from a `ResizeArray<'T>`. | .NET 5+ | ✔️ |
| `ofStream` | Creates a reader from a `Stream`. | ✔️ | ❌ |

## ParseResult: Parser Output

A parser returns a `ParseResult`, which is a standard F# `Result` type. This makes it easy to handle both success and failure using pattern matching.

```fsharp
type ParseResult<'Parsed, 'T, 'State> = Result<ParseSuccess<'Parsed>, ParseError<'T, 'State>>
```

### ParseSuccess

On success, the result is `Ok` containing a `ParseSuccess` value.

```fsharp
type ParseSuccess<'Parsed> = { Parsed: 'Parsed }
```

- `Parsed`: The value that was successfully parsed.

### ParseError

On failure, the result is `Error` containing a `ParseError` value. This struct provides rich information about what went wrong and where.

```fsharp
type ParseError<'T, 'State> =
    {
        Position: Position<'State> // Where the error happened
        Errors: ErrorType<'T, 'State> // What the error was
    }
```

## Supporting Types

The core types above rely on a few other important building blocks.

### IReadable: The Input Source

This interface makes XParsec extensible. It defines a contract for readable, sliceable data sources. While XParsec provides implementations for common types, you can implement `IReadable` to make your own custom data structures (like a rope or gap buffer) parsable.

```fsharp
type IReadable<'T, 'Slice when 'Slice :> IReadable<'T, 'Slice>> =
    // 'T is the item type (e.g. char)
    // 'Slice is the type of a readable slice
    abstract Item: int64 -> 'T with get
    abstract TryItem: index: int64 -> 'T voption
    abstract SpanSlice: start: int64 * length: int -> ReadOnlySpan<'T>
    abstract Length: int64
    abstract Slice: newStart: int64 * newLength: int64 -> 'Slice
```

### Position: A Snapshot in Time

A struct representing a snapshot of the reader's state (index and user state) at a specific point. This is crucial for backtracking and for providing precise error locations.

```fsharp
type Position<'State> =
    {
        Id: ReaderId
        Index: int64
        State: 'State
    }
```

### ErrorType: Describing What Went Wrong

This discriminated union represents the different kinds of errors that can occur during parsing.

```fsharp
type ErrorType<'T, 'State> =
    | Expected of 'T
    | ExpectedSeq of 'T seq
    | Unexpected of 'T
    | UnexpectedSeq of 'T seq
    | Message of string
    | EndOfInput
    // Used to wrap errors when multiple sub-parsers have failed to process the input.
    | Nested of parent: ErrorType<'T, 'State> * children: ParseError<'T, 'State> list
```

### InfiniteLoopException

To protect against common mistakes in recursive parser definitions, XParsec automatically detects when a parser consumes no input but also doesn't fail. When this happens, it throws an `InfiniteLoopException` instead of causing a stack overflow, making the bug much easier to find and fix.

```fsharp
type InfiniteLoopException<'State>(pos: Position<'State>, innerException) =
    inherit Exception("Infinite loop detected in parser.", innerException)
    member _.Position = pos
```
