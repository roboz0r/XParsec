namespace XParsec.FSharp.Lexer

open System
open XParsec
open XParsec.Parsers


[<RequireQualifiedAccess>]
module ResizeArray =
    let tryLastV (arr: ResizeArray<'T>) =
        if arr.Count = 0 then
            ValueNone
        else
            ValueSome(arr.[arr.Count - 1])

[<AutoOpen>]
module MoreParsers =
    let rec allEqual (span: ReadOnlySpan<'T>) x i =
        if i >= span.Length then true
        elif span.[i] <> x then false
        else allEqual span x (i + 1)

    let manySatisfiesSlice
        (predicate: 'T -> bool)
        (sliceState: 'SliceState)
        (reader: Reader<'T, 'State, 'Input, 'InputSlice>)
        =
        let startPos = reader.Position
        let mutable length = 0
        let mutable doContinue = true

        while doContinue do
            match reader.Peek() with
            | ValueSome c when predicate c ->
                length <- length + 1
                reader.Skip()
            | _ -> doContinue <- false

        preturn (reader.Slice(startPos.Index, length, sliceState)) reader

    let skip (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.Skip()
        preturn () reader

    let skipN (n: int) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        reader.SkipN n
        preturn () reader

    let skipNOf (n: int) (item: 'T) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        let span = reader.PeekN n

        if span.Length = 0 then
            fail EndOfInput reader
        elif span.Length < n then
            fail (UnexpectedSeq(span.ToArray())) reader
        elif allEqual span item 0 then
            reader.SkipN n
            preturn () reader
        else
            fail (UnexpectedSeq(span.ToArray())) reader

    let skipMany (predicate: 'T -> bool) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        let mutable length = 0
        let mutable doContinue = true

        while doContinue do
            match reader.Peek() with
            | ValueSome c when predicate c -> length <- length + 1
            | _ -> doContinue <- false

        reader.SkipN length
        Ok { Parsed = () }

    let peekAnyOf (candidates: 'T seq) (reader: Reader<'T, 'State, 'Input, 'InputSlice>) =
        let candidate = Array.ofSeq candidates

        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome x ->
            if Array.contains x candidate then
                preturn x reader
            else
                fail (Unexpected x) reader

    // The original bind operator isn't inlining properly
    let inline (>>=) ([<InlineIfLambda>] p) ([<InlineIfLambda>] binder) = bind p binder

    type ParserCE with

        member inline this.Using
            (resource: 'disposable :> IDisposable, [<InlineIfLambda>] binder: 'disposable -> Parser<unit, 'T, 'State, 'Input, 'InputSlice>)
            : Parser<unit, 'T, 'State, 'Input, 'InputSlice> =
            fun reader ->
                try
                    binder resource reader
                finally
                    if not (obj.ReferenceEquals(resource, null)) then
                        resource.Dispose()

        member inline this.While
            (
                [<InlineIfLambda>] guard: unit -> bool,
                [<InlineIfLambda>] generator: Parser<unit, 'T, 'State, 'Input, 'InputSlice>
            ) =
            let mutable doContinue = true
            let mutable result = Ok { Parsed = () }

            fun reader ->
                while doContinue && guard () do
                    match generator reader with
                    | Ok { Parsed = () } -> ()
                    | Error e ->
                        doContinue <- false
                        result <- Error e

                result

        member inline this.For
            (sequence: #seq<'T>, [<InlineIfLambda>] binder: 'T -> Parser<unit, 'U, 'State, 'Input, 'InputSlice>)
            : Parser<unit, 'U, 'State, 'Input, 'InputSlice> =
            this.Using(
                sequence.GetEnumerator(),
                fun enum -> this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> binder enum.Current))
            )


        member inline this.Combine
            (
                [<InlineIfLambda>] first: Parser<unit, 'T, 'State, 'Input, 'InputSlice>,
                [<InlineIfLambda>] second: Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>
            ) =
            fun reader ->
                match first reader with
                | Ok { Parsed = () } -> second reader
                | Error e -> Error e
