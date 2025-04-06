namespace XParsec

open System
open System.Collections.Immutable

module Parsers =

    let preturn x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseSuccess.create x

    let pzero (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create ParseError.zero reader.Position

    let fail x (reader: Reader<'T, 'State, 'Input, 'InputSlice>) : ParseResult<'Parsed, 'T, 'State> =
        ParseError.create x reader.Position

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
            match xs with
        | :? ('T array) as xs -> Internal.anyOf Array.contains xs
        | :? ('T ResizeArray) as xs -> Internal.anyOf (fun c (xs: ResizeArray<_>) -> xs.Contains c) xs
        | :? (ImmutableArray<'T>) as xs -> Internal.anyOf (fun c (xs: ImmutableArray<_>) -> xs.Contains c) xs
        | _ ->
            // Convert to array for better performance
            // as we expect the parser is called multiple times
            let arr = Seq.toArray xs
            Internal.anyOf Array.contains arr


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

/// Holds a mutable reference to a parser allowing the creation of forward-reference or mutually recursive parsers
[<Sealed>]
type RefParser<'Parsed, 'T, 'State, 'Input, 'InputSlice
    when 'Input :> IReadable<'T, 'InputSlice> and 'InputSlice :> IReadable<'T, 'InputSlice>>(pInner) =
    let mutable p: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice> = pInner

    new() = RefParser(Parsers.fail ParseError.refParserInit)

    member _.Set(parser) = p <- parser
    member _.Parser(reader: Reader<'T, 'State, 'Input, 'InputSlice>) = p reader
