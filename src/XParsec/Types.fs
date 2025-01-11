namespace XParsec

#if FABLE_COMPILER
type Span<'T> = XParsec.Span<'T>
type ReadOnlySpan<'T> = XParsec.ReadOnlySpan<'T>
type ImmutableArray<'T> = XParsec.ImmutableArray<'T>
#else
type Span<'T> = System.Span<'T>
type ReadOnlySpan<'T> = System.ReadOnlySpan<'T>
type ImmutableArray<'T> = System.Collections.Immutable.ImmutableArray<'T>
#endif

type IReadable<'T, 'U when 'U :> IReadable<'T, 'U>> =
    abstract TryItem: index: int64 -> 'T voption
    abstract SpanSlice: start: int64 * length: int -> ReadOnlySpan<'T>
    abstract Length: int64
    abstract Slice: newStart: int64 * newLength: int64 -> 'U


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
        Index: int64
        State: 'State
    }

[<Sealed>]
type Reader<'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>>
    (input: 'Input, state: 'State, index: int64) =

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
    member _.PeekN(count) = input.SpanSlice(index, count)
    member _.Length = input.Length

    member this.Skip() =
        if index < input.Length then
            index <- index + 1L
        else
            invalidOp "Attempted to skip past end of input"

    member this.SkipN(count) =
        if index + count > input.Length then
            invalidOp "Attempted to skip past end of input"
        else
            index <- index + count

    member this.TryRead() =
        if index < input.Length then
            let x = input.TryItem(index)
            index <- index + 1L
            x
        else
            ValueNone

    member this.Current = input.TryItem(index)
    member this.AtEnd = index >= input.Length


type ErrorType<'T, 'State> =
    | Expected of 'T
    | ExpectedSeq of 'T seq
    | Unexpected of 'T
    | UnexpectedSeq of 'T seq
    | Message of string
    | EndOfInput
    | Nested of parent: ErrorType<'T, 'State> * children: ParseErrors<'T, 'State>

and ErrorMessages<'T, 'State> = ErrorType<'T, 'State> list

and ParseError<'T, 'State> =
    {
        Position: Position<'State>
        Errors: ErrorMessages<'T, 'State>
    }

and ParseErrors<'T, 'State> = ParseError<'T, 'State> list

[<Struct>]
type ParseSuccess<'Parsed> = { Parsed: 'Parsed }

type ParseResult<'Parsed, 'T, 'State> = Result<ParseSuccess<'Parsed>, ParseErrors<'T, 'State>>

module ParseSuccess =
    let inline create tokens cursor = Ok { Parsed = tokens }

    let inline map f x = { Parsed = f x.Parsed }

module ParseError =
    let create error (cursor: Reader<_, _, _, _>) : Result<_, ParseErrors<'T, 'State>> =
        Error
            [
                {
                    Position = cursor.Position
                    Errors = [ error ]
                }
            ]

    let createList errors (cursor: Reader<_, _, _, _>) : Result<_, ParseErrors<'T, 'State>> =
        Error
            [
                {
                    Position = cursor.Position
                    Errors = errors
                }
            ]

    let createNested error children (cursor: Reader<_, _, _, _>) : Result<_, ParseErrors<'T, 'State>> =
        Error
            [
                {
                    Position = cursor.Position
                    Errors = [ Nested(error, children) ]
                }
            ]

    let createNestedP error children position : Result<_, ParseErrors<'T, 'State>> =
        Error
            [
                {
                    Position = position
                    Errors = [ Nested(error, children) ]
                }
            ]

    let wrongUserState = Message "Unexpected user state."
    let shouldConsume = Message "The parser did not consume any input."
    let shouldNotConsume = Message "The parser consumed input."
    let shouldNotSucceed = Message "The parser succeeded unexpectedly."
    let shouldFailInPlace = Message "The parser failed but consumed input."
    let unexpectedEnd = Message "Unexpected end of input"
    let expectedEnd = Message "Expected end of input"
    let refParserInit = Message "RefParser was not initialized."
    let expectedAtLeastOne = Message "Expected at least one item."

type Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>> =
    Reader<'T, 'State, 'Input, 'InputSlice> -> ParseResult<'Parsed, 'T, 'State>
