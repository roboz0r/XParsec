namespace XParsec

open System
open System.Collections.Immutable

module Parsers =

    /// Always succeeds and returns the given value.
    /// This parser does not consume any input.
    let preturn x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseSuccess.create x

    /// Always fails with the zero error.
    /// This parser does not consume any input.
    let pzero (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create ParseError.zero reader.Position

    /// Always fails with the given error.
    /// This parser does not consume any input.
    let fail x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create x reader.Position

    /// Return the current user state.
    /// This parser does not consume any input.
    let getUserState (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = preturn reader.State reader

    /// Set the user state to the given value.
    /// This parser does not consume any input.
    let setUserState state (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.State <- state
        preturn () reader

    /// Update the user state using the given function.
    /// This parser does not consume any input.
    let updateUserState mapper (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        let state = reader.State
        let newState = mapper state
        reader.State <- newState
        preturn () reader

    /// Succeeds if the user state satisfies the given predicate.
    /// This parser does not consume any input.
    let inline userStateSatisfies ([<InlineIfLambda>] predicate) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        if predicate reader.State then
            preturn () reader
        else
            fail ParseError.wrongUserState reader

    /// Return the current Reader position.
    /// This parser does not consume any input.
    let getPosition (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = preturn reader.Position reader

    /// Set the Reader position to the given value.
    /// An exception is thrown if the ReaderId is inconsistent with the Reader.
    let setPosition (position: Position<_>) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.Position <- position
        preturn () reader

    /// Succeeds if the Reader position is at the end of the input.
    /// This parser does not consume any input.
    let inline eof (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        if reader.AtEnd then
            preturn () reader
        else
            fail ParseError.expectedEnd reader

    /// Succeeds if the Reader position is not at the end of the input, consumes and returns one item.
    let inline pid (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.TryRead() with
        | ValueSome(x) -> preturn x reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the Reader position is not at the end of the input, and consumes one item, returning unit.
    let skip (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        if reader.AtEnd then
            fail EndOfInput reader
        else
            reader.Skip()
            preturn () reader

    /// Contains generalized parser implementations, not intended to be used directly.
    [<RequireQualifiedAccess>]
    module Internal =
        let inline anyOf ([<InlineIfLambda>] contains) xs (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            match reader.Peek() with
            | ValueSome c ->
                if contains c xs then
                    reader.Skip()
                    preturn c reader
                else
                    fail (Unexpected c) reader
            | ValueNone -> fail EndOfInput reader

        let inline skipAnyOf ([<InlineIfLambda>] contains) xs (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            match reader.Peek() with
            | ValueSome c ->
                if contains c xs then
                    reader.Skip()
                    preturn () reader
                else
                    fail (Unexpected c) reader
            | ValueNone -> fail EndOfInput reader

        let inline noneOf ([<InlineIfLambda>] contains) xs (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            match reader.Peek() with
            | ValueSome c ->
                if contains c xs then
                    fail (Unexpected c) reader
                else
                    reader.Skip()
                    preturn c reader
            | ValueNone -> fail EndOfInput reader

        let inline skipNoneOf ([<InlineIfLambda>] contains) xs (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            match reader.Peek() with
            | ValueSome c ->
                if contains c xs then
                    fail (Unexpected c) reader
                else
                    reader.Skip()
                    preturn () reader
            | ValueNone -> fail EndOfInput reader

        let inline pArrayReturn (xs: 'T array) ret (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            let span = reader.PeekN xs.Length

            if span.IsEmpty then
                fail EndOfInput reader
            else if MemoryExtensions.SequenceEqual(xs.AsSpan(), span) then
                reader.SkipN xs.Length
                preturn ret reader
            else
                fail (ExpectedSeq xs) reader

        let inline pImmArrayReturn (xs: ImmutableArray<'T>) ret (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            let span = reader.PeekN xs.Length

            if span.IsEmpty then
                fail EndOfInput reader
            else if MemoryExtensions.SequenceEqual(xs.AsSpan(), span) then
                reader.SkipN xs.Length
                preturn ret reader
            else
                fail (ExpectedSeq xs) reader

        let inline pResizeArrayReturn (xs: ResizeArray<'T>) ret (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
            let span = reader.PeekN xs.Count

            if span.IsEmpty then
                fail EndOfInput reader
            else
#if !FABLE_COMPILER && NET5_0_OR_GREATER
                let span1 = System.Runtime.InteropServices.CollectionsMarshal.AsSpan xs

                if MemoryExtensions.SequenceEqual(span1, span) then
                    reader.SkipN xs.Count
                    preturn ret reader
                else
                    fail (ExpectedSeq xs) reader
#else
                let len = xs.Count

                if span.Length <> len then
                    fail (ExpectedSeq xs) reader
                else
                    let mutable i = 0
                    let mutable success = true

                    while success && i < xs.Count do
                        if span.[i] <> xs.[i] then
                            success <- false

                        i <- i + 1

                    if success then
                        reader.SkipN len
                        preturn ret reader
                    else
                        fail (ExpectedSeq xs) reader
#endif

    /// Succeeds if the predicate is satisfied by the next item in the input, and consumes one item.
    /// Returns the item if the predicate is satisfied, otherwise fails with the Unexpected item.
    let inline satisfy ([<InlineIfLambda>] f: 'T -> bool) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(t) ->
            if f t then
                reader.Skip()
                preturn t reader
            else
                fail (Unexpected t) reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the predicate is satisfied by the next item in the input, and consumes one item.
    /// Returns the item if the predicate is satisfied, otherwise fails with the given message.
    let inline satisfyL ([<InlineIfLambda>] f: 'T -> bool) msg (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(t) ->
            if f t then
                reader.Skip()
                preturn t reader
            else
                fail (Message msg) reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the next item in the input is equal to the given item, and consumes one item.
    /// Returns the item if it is equal to the given item, otherwise fails with the Expected item.
    let inline itemReturn (x: 'T) result (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome(xI) ->
            if x = xI then
                reader.Skip()
                preturn result reader
            else
                fail (Expected x) reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the next item in the input is equal to the given item, and consumes one item.
    /// Returns unit, otherwise fails with the Expected item.
    let inline skipItem (x: 'T) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = itemReturn x () reader

    /// Succeeds if the next item in the input is equal to the given item, and consumes one item.
    /// Returns the item, otherwise fails with the Expected item.
    let inline pitem (x: 'T) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) = itemReturn x x reader

    /// Succeeds if the next item in the input is equal to any of the given items, and consumes one item.
    /// Returns the item, otherwise fails with the Unexpected item.
    let inline anyOf (xs: 'T seq) =
        match xs with
        | :? ('T array) as xs -> Internal.anyOf Array.contains xs
        | :? ('T ResizeArray) as xs -> Internal.anyOf (fun c (xs: ResizeArray<_>) -> xs.Contains c) xs
        | :? (ImmutableArray<'T>) as xs -> Internal.anyOf (fun c (xs: ImmutableArray<_>) -> xs.Contains c) xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.anyOf Array.contains arr


    /// Succeeds if the next item in the input is equal to any of the given items, and consumes one item.
    /// Returns unit, otherwise fails with the Unexpected item.
    let inline skipAnyOf (xs: 'T seq) =
        match xs with
        | :? ('T array) as xs -> Internal.skipAnyOf Array.contains xs
        | :? ('T ResizeArray) as xs -> Internal.skipAnyOf (fun c (xs: ResizeArray<_>) -> xs.Contains c) xs
        | :? (ImmutableArray<'T>) as xs -> Internal.skipAnyOf (fun c (xs: ImmutableArray<_>) -> xs.Contains c) xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.skipAnyOf Array.contains arr


    /// Succeeds if the next item in the input is equal to none of the given items, and consumes one item.
    /// Returns the item, otherwise fails with the Unexpected item.
    let inline noneOf (xs: 'T seq) =
        match xs with
        | :? ('T array) as xs -> Internal.noneOf Array.contains xs
        | :? ('T ResizeArray) as xs -> Internal.noneOf (fun c (xs: ResizeArray<_>) -> xs.Contains c) xs
        | :? (ImmutableArray<'T>) as xs -> Internal.noneOf (fun c (xs: ImmutableArray<_>) -> xs.Contains c) xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.noneOf Array.contains arr


    /// Succeeds if the next item in the input is equal to none of the given items, and consumes one item.
    /// Returns unit, otherwise fails with the Unexpected item.
    let inline skipNoneOf (xs: 'T seq) =
        match xs with
        | :? ('T array) as xs -> Internal.skipNoneOf Array.contains xs
        | :? ('T ResizeArray) as xs -> Internal.skipNoneOf (fun c (xs: ResizeArray<_>) -> xs.Contains c) xs
        | :? (ImmutableArray<'T>) as xs -> Internal.skipNoneOf (fun c (xs: ImmutableArray<_>) -> xs.Contains c) xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.skipNoneOf Array.contains arr


    /// Succeeds if the next item in the input is in the given range (inclusive), and consumes one item.
    /// Returns the item, otherwise fails with the Unexpected item.
    let inline anyInRange xMin xMax (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome c ->
            if c >= xMin && c <= xMax then
                reader.Skip()
                preturn c reader
            else
                fail (Unexpected c) reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the next item in the input is in the given range (inclusive), and consumes one item.
    /// Returns unit, otherwise fails with the Unexpected item.
    let inline skipAnyInRange xMin xMax (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        match reader.Peek() with
        | ValueSome c ->
            if c >= xMin && c <= xMax then
                reader.Skip()
                preturn () reader
            else
                fail (Unexpected c) reader
        | ValueNone -> fail EndOfInput reader

    /// Succeeds if the next items in the input are equal to the given items, and consumes them.
    /// Returns the items, otherwise fails with the Expected items.
    let pseq (xs: #seq<'T>) =
        match box xs with
        | :? ('T array) as arr -> Internal.pArrayReturn arr xs
        | :? ('T ResizeArray) as arr -> Internal.pResizeArrayReturn arr xs
        | :? (ImmutableArray<'T>) as arr -> Internal.pImmArrayReturn arr xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.pArrayReturn arr xs

    /// Succeeds if the next items in the input are equal to the given items, and consumes them.
    /// Returns the result, otherwise fails with the Expected items.
    let pseqReturn (xs: #seq<'T>) result =
        match box xs with
        | :? ('T array) as arr -> Internal.pArrayReturn arr result
        | :? ('T ResizeArray) as arr -> Internal.pResizeArrayReturn arr result
        | :? (ImmutableArray<'T>) as arr -> Internal.pImmArrayReturn arr result
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.pArrayReturn arr result

    /// Applies the `folder` function to each result of the given parser and the initial `state` zero or more times,
    /// accumulating a final result.
    /// The parser is applied until it fails, and the final state is returned.
    let inline fold
        state
        ([<InlineIfLambda>] folder)
        ([<InlineIfLambda>] p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<'T, 'State, 'Input, 'InputSlice>)
        =
        let mutable keepGoing = true
        let mutable state = state

        while keepGoing do
            let pos = reader.Position

            match p reader with
            | Ok { Parsed = x } ->
                if pos = reader.Position then
                    raise (InfiniteLoopException pos)

                state <- folder state x
            | Error _ ->
                reader.Position <- pos
                keepGoing <- false

        ParseSuccess.create state

    /// Applies the `folder` function to each result of the given parser and the initial `state` one or more times,
    /// accumulating a final result.
    /// The parser is applied until it fails, and the final state is returned.
    let inline fold1
        state
        ([<InlineIfLambda>] folder)
        ([<InlineIfLambda>] p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<'T, 'State, 'Input, 'InputSlice>)
        =
        let pos = reader.Position

        match p reader with
        | Ok { Parsed = x } -> fold (folder state x) folder p reader
        | Error e -> ParseError.createNested ParseError.expectedAtLeastOne [ e ] pos

    /// Applies a `folder` function to each result of the given parser and the current user state zero or more times,
    /// updating the user state.
    /// The parser is applied until it fails. Returns unit.
    let inline foldUserState
        ([<InlineIfLambda>] folder)
        ([<InlineIfLambda>] p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<'T, 'State, 'Input, 'InputSlice>)
        =
        let mutable keepGoing = true

        while keepGoing do
            let pos = reader.Position

            match p reader with
            | Ok { Parsed = x } ->
                if pos = reader.Position then
                    raise (InfiniteLoopException pos)

                reader.State <- folder reader.State x
            | Error _ ->
                reader.Position <- pos
                keepGoing <- false

        ParseSuccess.create ()

    /// Applies a `folder` function to each result of the given parser and the current user state one or more times,
    /// updating the user state.
    /// The parser is applied until it fails. Returns unit.
    let inline foldUserState1
        ([<InlineIfLambda>] folder)
        ([<InlineIfLambda>] p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<'T, 'State, 'Input, 'InputSlice>)
        =
        let pos = reader.Position

        match p reader with
        | Ok { Parsed = x } ->
            reader.State <- folder reader.State x
            foldUserState folder p reader
        | Error e -> ParseError.createNested ParseError.expectedAtLeastOne [ e ] pos

/// Holds a mutable reference to a parser allowing the creation of forward-reference or mutually recursive parsers
[<Sealed>]
type RefParser<'Parsed, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>>(pInner) =
    let mutable p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice> = pInner

    new() = RefParser(Parsers.fail ParseError.refParserInit)

    member _.Set(parser) = p <- parser
    member _.Parser(reader: Reader<'T, 'State, 'Input, 'InputSlice>) = p reader
