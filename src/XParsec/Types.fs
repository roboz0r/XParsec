namespace XParsec

open System

/// <summary>
/// A type that can be read from, representing the input to a parser.
/// Implement this interface to create your own input types.
/// Use the Reader module for common input types like string, array, etc.
/// </summary>
type IReadable<'T, 'Slice when 'Slice :> IReadable<'T, 'Slice>> =
    abstract Item: int -> 'T with get
    abstract TryItem: index: int -> 'T voption
#if !FABLE_COMPILER
    /// Returns a span over the entire view. Equivalent to `AsSpan(0, Length)`.
    abstract AsSpan: unit -> ReadOnlySpan<'T>
    /// Returns a span over `[start, Length)`. Throws `ArgumentOutOfRangeException`
    /// if `start` is negative or greater than `Length` (matches BCL `AsSpan`).
    abstract AsSpan: start: int -> ReadOnlySpan<'T>
    /// Returns a span over `[start, start + length)`. Throws
    /// `ArgumentOutOfRangeException` if `start` or `length` is negative or
    /// `start + length > Length` (matches BCL `AsSpan`).
    abstract AsSpan: start: int * length: int -> ReadOnlySpan<'T>
#else
    /// Single optional-arg form because JS classes can't dispatch by arity.
    /// Callers may invoke `AsSpan()`, `AsSpan(start)`, or `AsSpan(start, length)`
    /// — same call sites as the .NET branch — and the implementation defaults
    /// missing arguments to `0` / `Length - start`. Throws
    /// `ArgumentOutOfRangeException` on overflow (matches BCL `AsSpan`).
    abstract AsSpan: ?start: int * ?length: int -> ReadOnlySpan<'T>
#endif
    abstract Length: int
    abstract Slice: newStart: int * newLength: int -> 'Slice


[<Struct>]
type ReaderId = internal | ReaderId of int64

module internal ReaderUtils =
    open System.Threading

    let nextId =
        let mutable x = 0L
#if FABLE_COMPILER
        fun () ->
            x <- x + 1L
            ReaderId x
#else
        fun () -> Interlocked.Increment &x |> ReaderId
#endif

open ReaderUtils

[<Struct>]
type Position<'State> =
    {
        Id: ReaderId
        Index: int
        State: 'State
    }

/// <summary>
/// A cursor that tracks the current position in the input and the user state.
/// It is used by the parser to read from the input and manage state.
/// </summary>
[<Sealed>]
type Reader<'T, 'State, 'Input when 'Input :> IReadable<'T, 'Input>>(input: 'Input, state: 'State, index: int) =

    let mutable index = index
    let mutable state = state
    let id = nextId ()

    member _.Id = id
    member _.Input = input

    member _.State
        with get () = state
        and set v = state <- v

    member _.Index
        with get () = index
        and set v = index <- v

    member _.Position
        with get () =
            {
                Id = id
                Index = index
                State = state
            }
        and set (p: Position<'State>) =
            if p.Id <> id then
                invalidOp "Position id does not match Cursor id"

            index <- p.Index
            state <- p.State

    member _.Peek() = input.TryItem(index)

    /// Returns up to `count` items starting at the current position, clamped to the
    /// remaining input. Use this for "peek as many as are there" lookups; for strict
    /// BCL bounds semantics call `Input.AsSpan(index, length)` directly.
    member _.PeekN(count) =
        let remaining = input.Length - index
        let safeCount = if count < remaining then count else remaining
        input.AsSpan(index, safeCount)

    member _.Length = input.Length

    member _.Skip() =
        if index < input.Length then
            index <- index + 1
        else
            invalidOp "Attempted to skip past end of input"

    member _.SkipN(count) =
        if count < 0 then
            invalidArg (nameof count) "count must be non-negative; rewind by setting Index directly."
        elif index + count > input.Length then
            invalidOp "Attempted to skip past end of input"
        else
            index <- index + count

    member _.TryRead() =
        let x = input.TryItem(index)

        match x with
        | ValueSome _ -> index <- index + 1
        | ValueNone -> ()

        x

    member _.AtEnd = index >= input.Length

    /// <summary>
    /// Creates a child Reader over a slice of the current input, starting at
    /// <paramref name="newStart"/> items past the current position and spanning
    /// <paramref name="newLength"/> items. The child reader's state is reset to
    /// <c>unit</c> — use the three-arg overload to seed an explicit state.
    /// </summary>
    /// <remarks>
    /// Slicing is the standard way to run a sub-parser over a bounded region with
    /// a fresh (and possibly differently-typed) state. The child reader has its
    /// own <c>ReaderId</c>; <c>Position</c> values cross between parent and child
    /// readers will be rejected by the position setter.
    /// </remarks>
    member _.Slice(newStart, newLength) =
        Reader(input.Slice(index + newStart, newLength), (), 0)

    /// <summary>
    /// Creates a child Reader over a slice of the current input with an explicit
    /// initial state. The state type may differ from the parent reader's.
    /// </summary>
    /// <remarks>
    /// See the two-arg <c>Slice</c> for slicing semantics. The child reader has
    /// its own <c>ReaderId</c> and is independent of the parent.
    /// </remarks>
    member _.Slice(newStart, newLength, newState) =
        Reader(input.Slice(index + newStart, newLength), newState, 0)

type ErrorType<'T, 'State> =
    /// Unspecified failure — analogous to FParsec's "empty error message list".
    /// Produced by `pzero`. Aggregating combinators filter `Empty` children before
    /// constructing nested errors, and the default formatter renders nothing.
    | Empty
    | Expected of 'T
    | ExpectedSeq of 'T seq
    | ExpectedOneOf of 'T seq
    | ExpectedSeqOneOf of 'T seq seq
    | Unexpected of 'T
    | UnexpectedSeq of 'T seq
    | Message of string
    | EndOfInput
    | Nested of parent: ErrorType<'T, 'State> * children: ParseError<'T, 'State> list

and [<Struct>] ParseError<'T, 'State> =
    {
        Position: Position<'State>
        Errors: ErrorType<'T, 'State>
    }

type ParseResult<'Parsed, 'T, 'State> = Result<'Parsed, ParseError<'T, 'State>>

module ParseError =
    let inline create error position : ParseResult<'Parsed, 'T, 'State> =
        Error { Position = position; Errors = error }

    let inline createNested error children position : ParseResult<'Parsed, 'T, 'State> =
        Error
            {
                Position = position
                Errors = Nested(error, children)
            }

    let wrongUserState = Message "Unexpected user state."
    let shouldConsume = Message "The parser did not consume any input."
    let shouldNotConsume = Message "The parser consumed input."
    let shouldNotSucceed = Message "The parser succeeded unexpectedly."
    let shouldFailInPlace = Message "The parser failed but consumed input."
    let unexpectedEnd = Message "Unexpected end of input"
    let expectedEnd = Message "Expected end of input"
    let expectedAtLeastOne = Message "Expected at least one item."
    /// Unspecified failure — emitted by `pzero`. Aggregating combinators drop
    /// these from nested children; the default formatter renders nothing for them.
    let zero = Empty
    let allChoicesFailed = Message "All choices failed."
    let bothFailed = Message "Both parsers failed."

    /// True if the given error carries no specific information (`Empty`). Used by
    /// aggregating combinators to filter out `pzero`-style siblings before nesting.
    let inline isEmpty (e: ErrorType<'T, 'State>) =
        match e with
        | Empty -> true
        | _ -> false

    /// An "unspecified" error at the given position. Used as a sentinel by
    /// internal soft-error accumulators where every code path needs a `ParseError`
    /// value but most are "no error here yet".
    let inline empty (position: Position<'State>) : ParseError<'T, 'State> = { Position = position; Errors = Empty }

type Parser<'Parsed, 'T, 'State, 'Input when 'Input :> IReadable<'T, 'Input>> =
    Reader<'T, 'State, 'Input> -> ParseResult<'Parsed, 'T, 'State>

type InfiniteLoopException<'State>(pos: Position<'State>, innerException) =
    inherit Exception("Infinite loop detected in parser.", innerException)
    new(pos: Position<'State>) = InfiniteLoopException(pos, null)
    member _.Position = pos
