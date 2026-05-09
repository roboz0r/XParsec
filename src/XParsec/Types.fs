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


/// <summary>
/// An opaque identifier minted per <see cref="Reader{T,S,I}"/> instance. Used by
/// <see cref="Position{S}"/> values to detect cross-reader assignments and reject
/// them with a clear error rather than corrupting parser state.
/// </summary>
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

/// <summary>
/// A snapshot of a Reader's progress: the issuing reader's <c>Id</c>, the
/// current <c>Index</c>, and the user <c>State</c>. Capture with
/// <c>reader.Position</c> and restore by assigning back; assigning a position
/// from a different reader throws.
/// </summary>
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
                invalidOp "Position id does not match Reader id"

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

/// <summary>
/// Describes why a parser failed. The default <see cref="ErrorFormatting.formatParseError"/>
/// renders these in a tree, but consumers are free to pattern-match and emit their own format.
/// </summary>
type ErrorType<'T, 'State> =
    /// Unspecified failure — analogous to FParsec's "empty error message list".
    /// Produced by `pzero`. Aggregating combinators filter `Empty` children before
    /// constructing nested errors, and the default formatter renders nothing.
    | Empty
    /// A specific item was expected at this position.
    | Expected of 'T
    /// A specific sequence of items was expected.
    | ExpectedSeq of 'T seq
    /// One of several alternative items was expected.
    | ExpectedOneOf of 'T seq
    /// One of several alternative sequences was expected.
    | ExpectedSeqOneOf of 'T seq seq
    /// An item was encountered that should not have been.
    | Unexpected of 'T
    /// A sequence of items was encountered that should not have been.
    | UnexpectedSeq of 'T seq
    /// A free-form parser-supplied message.
    | Message of string
    /// The parser hit end-of-input but expected more.
    | EndOfInput
    /// A `parent` error caused by the listed `children` (typical for combinators
    /// like `<|>`, `choice`, and the `*Till` family that aggregate sub-errors).
    | Nested of parent: ErrorType<'T, 'State> * children: ParseError<'T, 'State> list

/// <summary>
/// A parse error: the <see cref="Position{S}"/> at which the parser gave up, plus
/// the structured <see cref="ErrorType{T,S}"/> describing why.
/// </summary>
and [<Struct>] ParseError<'T, 'State> =
    {
        Position: Position<'State>
        Errors: ErrorType<'T, 'State>
    }

/// <summary>
/// The result of running a parser: either the parsed value or a
/// <see cref="ParseError{T,S}"/>. Aliased to the BCL <c>Result</c> type so it
/// integrates with standard `Result.bind`/`Result.map` / `match` workflows.
/// </summary>
type ParseResult<'Parsed, 'T, 'State> = Result<'Parsed, ParseError<'T, 'State>>

/// <summary>
/// Helpers for constructing and inspecting <see cref="ParseError{T,S}"/> values.
/// Also exposes the canonical <c>Message</c> values used by the standard combinators
/// (`shouldConsume`, `bothFailed`, etc.) so user code can pattern-match against them
/// and produce the same error shape.
/// </summary>
module ParseError =
    /// Constructs a <c>Result.Error</c> at the given position from the supplied error type.
    let inline create error position : ParseResult<'Parsed, 'T, 'State> =
        Error { Position = position; Errors = error }

    /// Constructs a <c>Result.Error</c> with a `Nested(parent, children)` body at the given position.
    let inline createNested error children position : ParseResult<'Parsed, 'T, 'State> =
        Error
            {
                Position = position
                Errors = Nested(error, children)
            }

    /// Used by `userStateSatisfies` when the predicate rejects the current state.
    let wrongUserState = Message "Unexpected user state."
    /// Used by `notEmpty` when its inner parser returned `Ok` without advancing.
    let shouldConsume = Message "The parser did not consume any input."
    /// Used by `followedBy*` when its inner parser advanced (it should have peeked, not consumed).
    let shouldNotConsume = Message "The parser consumed input."
    /// Used by `notFollowedBy` when the inner parser unexpectedly succeeded.
    let shouldNotSucceed = Message "The parser succeeded unexpectedly."
    /// Used by `notFollowedBy` when the inner parser failed but had already consumed input.
    let shouldFailInPlace = Message "The parser failed but consumed input."
    /// Generic "we hit EOF" message used in error-formatting fallbacks.
    let unexpectedEnd = Message "Unexpected end of input"
    /// Used by `eof` when there are still items left in the input.
    let expectedEnd = Message "Expected end of input"
    /// Used by `many1`, `sepBy1`, `fold1`, etc. when the first item fails.
    let expectedAtLeastOne = Message "Expected at least one item."
    /// Unspecified failure — emitted by `pzero`. Aggregating combinators drop
    /// these from nested children; the default formatter renders nothing for them.
    let zero = Empty
    /// Wraps the per-branch errors from `choice` when every alternative failed.
    let allChoicesFailed = Message "All choices failed."
    /// Wraps the two errors from `<|>` (or the loop-stuck pair from `manyTill`) when both sides failed.
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

/// <summary>
/// A parser is a function from a <see cref="Reader{T,S,I}"/> to a
/// <see cref="ParseResult{P,T,S}"/>. It mutates the reader's index/state on
/// success, and is expected to leave the reader at the position it was at on
/// entry when it returns <c>Error</c> without intending to consume input.
/// </summary>
/// <remarks>
/// Most combinators in <c>XParsec.Combinators</c> are <c>inline</c> and accept
/// <c>InlineIfLambda</c> arguments, so plain function-typed parsers compose
/// with no allocation overhead in steady-state code.
/// </remarks>
type Parser<'Parsed, 'T, 'State, 'Input when 'Input :> IReadable<'T, 'Input>> =
    Reader<'T, 'State, 'Input> -> ParseResult<'Parsed, 'T, 'State>

/// <summary>
/// Raised by looping combinators (<c>many</c>, <c>sepBy*</c>, <c>manyTill</c>,
/// <c>chainl1</c>, <c>fold</c>, <c>foldUserState</c>, <c>manyChars</c>, …) when
/// the inner parser succeeds without advancing the reader. Carries the
/// <see cref="Position{S}"/> at which the loop got stuck so the bug can be
/// localised.
/// </summary>
/// <remarks>
/// This is a programming error — the inner parser is malformed (e.g. it always
/// succeeds with <c>preturn</c>). User code should fix the parser rather than
/// catch the exception. <c>ParserCE.While</c> / <c>For</c> deliberately do
/// <em>not</em> raise this; termination of CE loops is the user's responsibility.
/// </remarks>
type InfiniteLoopException<'State>(pos: Position<'State>, innerException) =
    inherit Exception("Infinite loop detected in parser.", innerException)
    new(pos: Position<'State>) = InfiniteLoopException(pos, null)

    /// The reader position at which the looping combinator detected zero progress.
    member _.Position = pos
