namespace XParsec

open System
open System.Collections.Immutable

module Parsers =

    let preturn x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseSuccess.create x reader

    let pzero (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create ParseError.zero reader

    let fail x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create x reader

    let getUserState (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = preturn reader.State reader

    let setUserState state (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.State <- state
        preturn () reader

    let updateUserState mapper (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        let state = reader.State
        let newState = mapper state
        reader.State <- newState
        preturn () reader

    let inline userStateSatisfies ([<InlineIfLambda>] predicate) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        if predicate reader.State then
            preturn () reader
        else
            fail ParseError.wrongUserState reader

    let getPosition (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = preturn reader.Position reader

    let setPosition (position: Position<_>) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.Position <- position
        preturn () reader

    let inline eof (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        if reader.AtEnd then
            preturn () reader
        else
            fail ParseError.expectedEnd reader

    let inline pid (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.TryRead() with
        | ValueSome(x) -> preturn x reader
        | ValueNone -> fail EndOfInput reader

    let inline satisfy ([<InlineIfLambda>] f: 'T -> bool) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(t) ->
            if f t then
                reader.Skip()
                preturn t reader
            else
                fail (Unexpected t) reader
        | ValueNone -> fail EndOfInput reader

    let inline satisfyL ([<InlineIfLambda>] f: 'T -> bool) msg (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(t) ->
            if f t then
                reader.Skip()
                preturn t reader
            else
                fail (Message msg) reader
        | ValueNone -> fail EndOfInput reader

    let inline itemReturn (x: 'T) (result) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(xI) ->
            if x = xI then
                reader.Skip()
                preturn result reader
            else
                fail (Expected x) reader
        | ValueNone -> fail EndOfInput reader

    let inline skipItem (x: 'T) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = itemReturn x () reader

    let inline pitem (x: 'T) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = itemReturn x x reader

    let inline anyOf (xs: 'T seq) =
        let xs =
            match xs with
            | :? ('T array) as res -> res
            | _ -> Seq.toArray xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            match reader.Peek() with
            | ValueSome(c) ->
                if Array.contains c xs then
                    reader.Skip()
                    preturn c reader
                else
                    fail (Unexpected c) reader
            | ValueNone -> fail EndOfInput reader

    let inline skipAnyOf (xs: 'T seq) =
        let xs =
            match xs with
            | :? ('T array) as res -> res
            | _ -> Seq.toArray xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            match reader.Peek() with
            | ValueSome(c) ->
                if Array.contains c xs then
                    reader.Skip()
                    preturn () reader
                else
                    fail (Unexpected c) reader
            | ValueNone -> fail EndOfInput reader

    let inline noneOf (xs: 'T seq) =
        let xs =
            match xs with
            | :? ('T array) as res -> res
            | _ -> Seq.toArray xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            match reader.Peek() with
            | ValueSome(c) ->
                if Array.contains c xs then
                    fail (Unexpected c) reader
                else
                    reader.Skip()
                    preturn c reader
            | ValueNone -> fail EndOfInput reader

    let inline skipNoneOf (xs: 'T seq) =
        let xs =
            match xs with
            | :? ('T array) as res -> res
            | _ -> Seq.toArray xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            match reader.Peek() with
            | ValueSome(c) ->
                if Array.contains c xs then
                    fail (Unexpected c) reader
                else
                    reader.Skip()
                    preturn () reader
            | ValueNone -> fail EndOfInput reader

    let inline anyInRange xMin xMax (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(c) ->
            if c >= xMin && c <= xMax then
                reader.Skip()
                preturn c reader
            else
                fail (Unexpected c) reader
        | ValueNone -> fail EndOfInput reader

    let inline skipAnyInRange xMin xMax (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(c) ->
            if c >= xMin && c <= xMax then
                reader.Skip()
                preturn () reader
            else
                fail (Unexpected c) reader
        | ValueNone -> fail EndOfInput reader

    let pseq (xs: 'T seq) =
        let xs =
            match xs with
            | :? (ImmutableArray<'T>) as res -> res
            | _ -> ImmutableArray.CreateRange xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            let span = reader.PeekN xs.Length

            if span.IsEmpty then
                fail (ExpectedSeq xs) reader
            else if MemoryExtensions.SequenceEqual(xs.AsSpan(), span) then
                reader.SkipN xs.Length
                preturn xs reader
            else
                fail (ExpectedSeq xs) reader

    let pseqReturn (xs: 'T seq) result =
        let xs =
            match xs with
            | :? (ImmutableArray<'T>) as res -> res
            | _ -> ImmutableArray.CreateRange xs

        fun (reader: Reader<'T, 'State, 'Input, 'InputSlice>) ->
            let span = reader.PeekN xs.Length

            if span.IsEmpty then
                fail (ExpectedSeq xs) reader
            else if MemoryExtensions.SequenceEqual(xs.AsSpan(), span) then
                reader.SkipN xs.Length
                preturn result reader
            else
                fail (ExpectedSeq xs) reader

/// Holds a mutable reference to a parser allowing the creation of forward-reference or mutually recursive parsers
[<Sealed>]
type RefParser<'Parsed, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>>(pInner) =
    let mutable p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice> = pInner

    new() = RefParser(Parsers.fail ParseError.refParserInit)

    member _.Set(parser) = p <- parser
    member _.Parser(reader: Reader<'T, 'State, 'Input, 'InputSlice>) = p reader
