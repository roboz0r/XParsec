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

[<RequireQualifiedAccess>]
module Array =
    let inline tryFindIndexV ([<InlineIfLambda>] predicate: 'T -> bool) (array: 'T[]) =
        let rec loop i =
            if i >= array.Length then ValueNone
            else if predicate array[i] then ValueSome i
            else loop (i + 1)

        loop 0

[<AutoOpen>]
module MoreParsers =
    let rec allEqual (span: ReadOnlySpan<'T>) x i =
        if i >= span.Length then true
        elif span.[i] <> x then false
        else allEqual span x (i + 1)

    // let manySatisfiesSlice
    //     (predicate: 'T -> bool)
    //     (sliceState: 'SliceState)
    //     (reader: Reader<'T, 'State, 'Input>)
    //     =
    //     let startPos = reader.Position
    //     let mutable length = 0
    //     let mutable doContinue = true

    //     while doContinue do
    //         match reader.Peek() with
    //         | ValueSome c when predicate c ->
    //             length <- length + 1
    //             reader.Skip()
    //         | _ -> doContinue <- false

    //     preturn (reader.Slice(startPos.Index, length, sliceState)) reader

    let skip (reader: Reader<'T, 'State, 'Input>) =
        reader.Skip()
        preturn () reader

    let skipN (n: int) (reader: Reader<'T, 'State, 'Input>) =
        reader.SkipN n
        preturn () reader

    let skipNOf (n: int) (item: 'T) (reader: Reader<'T, 'State, 'Input>) =
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

    let peekAnyOf (candidates: 'T seq) (reader: Reader<'T, 'State, 'Input>) =
        let candidate = Array.ofSeq candidates

        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome x ->
            if Array.contains x candidate then
                preturn x reader
            else
                fail (Unexpected x) reader
