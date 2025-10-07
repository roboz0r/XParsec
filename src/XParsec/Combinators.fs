namespace XParsec

open System
open System.Collections.Immutable

[<AutoOpen>]
module Combinators =
    open Parsers

    /// Applies the parser `p` and, if it succeeds, computes the parser `p2` of `binder` with the result of `p`.
    /// Finally, it returns the result of `p2`.
    let inline bind
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] binder: 'A -> Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success ->
            let p2 = binder success.Parsed
            p2 reader
        | Error err -> Error err

    /// Applies the parser `p` and, if it succeeds, computes the parser `p2` of `binder` with the result of `p`.
    /// Finally, it returns the result of `p2`.
    let inline (>>=) ([<InlineIfLambda>] p) ([<InlineIfLambda>] binder) = bind p binder

    /// Applies the parser `p` and, if it succeeds, returns the value `x`.
    let inline (>>%)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        x
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success -> preturn x reader
        | Error err -> Error err

    /// Applies the parsers `p1` and `p2` in sequence. If both succeed, returns the result of `p2`.
    let inline (>>.)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok success -> p2 reader
        | Error err -> Error err

    /// Applies the parsers `p1` and `p2` in sequence. If both succeed, returns the result of `p1`.
    let inline (.>>)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok success1 ->
            match p2 reader with
            | Ok success2 -> preturn success1.Parsed reader
            | Error err -> Error err
        | Error err -> Error err

    /// Applies the parsers `p1` and `p2` in sequence. If both succeed, returns the result of `p1` and `p2`.
    let inline (.>>.)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok success1 ->
            match p2 reader with
            | Ok success2 -> preturn struct (success1.Parsed, success2.Parsed) reader
            | Error err -> Error err
        | Error err -> Error err

    /// Applies the parsers `pOpen`, `p` and `pClose` in sequence. If all succeed, returns the result of `p`.
    let inline between
        ([<InlineIfLambda>] pOpen: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] pClose: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match pOpen reader with
        | Ok s1 ->
            match p reader with
            | Ok s2 ->
                match pClose reader with
                | Ok s3 -> preturn s2.Parsed reader
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err

    /// Applies the parser `p` and, if it succeeds, computes the function `mapping` with the result of `p`.
    let inline (|>>)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] mapping: 'A -> 'B)
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success -> preturn (mapping success.Parsed) reader
        | Error err -> Error err

    [<Sealed>]
    type ParserCE() =

        member inline _.Bind([<InlineIfLambda>] p, [<InlineIfLambda>] binder) = p >>= binder
        member inline _.Return(x) = preturn x
        member inline _.ReturnFrom(p) = p
        member inline _.BindReturn([<InlineIfLambda>] p, [<InlineIfLambda>] map) = p |>> map
        member inline _.Delay([<InlineIfLambda>] f) = fun reader -> (f ()) reader
        member inline _.Zero() = pzero

        member inline _.TryWith ([<InlineIfLambda>] p, [<InlineIfLambda>] cf) (reader: Reader<_, _, _, _>) =
            try
                p reader
            with e ->
                (cf e) reader

        member inline _.TryFinally ([<InlineIfLambda>] p, [<InlineIfLambda>] ff) (reader: Reader<_, _, _, _>) =
            try
                p reader
            finally
                ff ()

    /// A computation expression builder for parsers.
    let parser = ParserCE()

    /// Applies the parsers `p1` and `p2` in sequence. If both succeed, returns the result of `f` applied to the results of `p1` and `p2`.
    let inline pipe2
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] f: 'A -> 'B -> 'C)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok s1 ->
            match p2 reader with
            | Ok s2 -> preturn (f s1.Parsed s2.Parsed) reader
            | Error err -> Error err
        | Error err -> Error err

    /// Applies the 3 parsers in sequence. If both succeed, returns the result of `f` applied to the results of the parsers.
    let inline pipe3
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] f: 'A -> 'B -> 'C -> 'D)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok s1 ->
            match p2 reader with
            | Ok s2 ->
                match p3 reader with
                | Ok s3 -> preturn (f s1.Parsed s2.Parsed s3.Parsed) reader
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err

    /// Applies the 4 parsers in sequence. If all succeed, returns the result of `f` applied to the results of the parsers.
    let inline pipe4
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p4: Parser<'D, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] f: 'A -> 'B -> 'C -> 'D -> 'E)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            let! s4 = p4
            return f s1 s2 s3 s4
        }

    /// Applies the 5 parsers in sequence. If all succeed, returns the result of `f` applied to the results of the parsers.
    let inline pipe5
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p4: Parser<'D, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p5: Parser<'E, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] f: 'A -> 'B -> 'C -> 'D -> 'E -> 'F)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            let! s4 = p4
            let! s5 = p5
            return f s1 s2 s3 s4 s5
        }

    /// Applies the parser `p1` and, if it fails, applies the parser `p2`. Returns the result of the first parser that succeeds, or both errors if both fail.
    let inline (<|>)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> Ok s1
        | Error err1 ->
            reader.Position <- p

            match p2 reader with
            | Ok s2 -> Ok s2
            | Error err2 -> ParseError.createNested ParseError.bothFailed [ err1; err2 ] p

    /// <summary>
    /// Applies the parsers `ps` in order. Returns the result of the first parser that succeeds, or all errors if all fail.
    /// </summary>
    /// <remarks>
    /// Accumulating all errors may be costly, prefer `choiceL` if the errors are not needed.
    /// </remarks>
    let choice (ps: Parser<'A, 'T, 'State, 'Input, 'InputSlice> seq) : Parser<'A, 'T, 'State, 'Input, 'InputSlice> =
        let parsers = ps |> Seq.toArray

        fun (reader: Reader<_, _, _, _>) ->
            let mutable success = ValueNone
            let errs = ResizeArray<_>()
            let p = reader.Position
            let mutable i = 0

            while success.IsNone && i < parsers.Length do
                match parsers.[i] reader with
                | Ok x -> success <- ValueSome x
                | Error err ->
                    reader.Position <- p
                    errs.Add(err)

                i <- i + 1

            match success with
            | ValueNone -> ParseError.createNested ParseError.allChoicesFailed (List.ofSeq errs) p
            | ValueSome x -> Ok x

    /// Applies the parsers `ps` in order. Returns the result of the first parser that succeeds, or fails with the given message if all fail.
    let choiceL
        (ps: Parser<'A, 'T, 'State, 'Input, 'InputSlice> seq)
        message
        : Parser<'A, 'T, 'State, 'Input, 'InputSlice> =
        let parsers = ps |> Seq.toArray

        fun (reader: Reader<_, _, _, _>) ->
            let mutable success = ValueNone
            let p = reader.Position
            let mutable i = 0

            while success.IsNone && i < parsers.Length do
                match parsers.[i] reader with
                | Ok x -> success <- ValueSome x
                | Error _ -> reader.Position <- p

                i <- i + 1

            match success with
            | ValueNone -> fail (Message message) reader
            | ValueSome x -> Ok x

    /// Applies the parser `p1` and, if it fails, returns the value `x`.
    /// If `p1` succeeds, the input is consumed. If `p1` fails, no input is consumed.
    let inline (<|>%)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        x
        (reader: Reader<_, _, _, _>)
        =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> Ok s1
        | Error _ ->
            reader.Position <- p
            preturn x reader

    /// Applies the parser `p1` and, if it fails, returns `ValueNone`.
    /// If `p1` succeeds, the input is consumed. If `p1` fails, no input is consumed.
    let inline opt ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> preturn (ValueSome s1.Parsed) reader
        | Error _ ->
            reader.Position <- p
            preturn ValueNone reader

    /// Attempts to apply the parser `p1`. Always succeeds, returning `()`.
    /// If `p1` succeeds, the input is consumed. If `p1` fails, no input is consumed.
    let inline optional
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> preturn () reader
        | Error _ ->
            reader.Position <- p
            preturn () reader

    /// Applies the parser `p1`, if it succeeds, ensures that the parser has changed the input position.
    let inline notEmpty
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            if reader.Position = pos then
                fail ParseError.shouldConsume reader
            else
                Ok s1
        | Error err -> Error err

    /// Applies the parser `p1`, if it succeeds, ensures that the parser has not changed the input position.
    /// If `p1` fails, no input is consumed.
    let inline followedBy
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            if reader.Position = pos then
                preturn () reader
            else
                reader.Position <- pos
                fail ParseError.shouldNotConsume reader
        | Error err ->
            reader.Position <- pos
            Error err

    /// Applies the parser `p1`, if it succeeds, ensures that the parser has not changed the input position.
    /// If `p1` fails, no input is consumed.
    /// If `p1` succeeds and consumes input, the parser fails with the given `message`.
    let inline followedByL
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        message
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            if reader.Position = pos then
                preturn () reader
            else
                reader.Position <- pos
                fail (Message message) reader
        | Error err ->
            reader.Position <- pos
            Error err

    /// Applies the parser `p1`, if it succeeds, this parser fails without consuming input.
    /// If `p1` fails without consuming input, the parser succeeds.
    /// If `p1` fails and consumes input, this parser fails without consuming input.
    let inline notFollowedBy
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            reader.Position <- pos
            fail ParseError.shouldNotSucceed reader
        | Error err ->
            if reader.Position = pos then
                preturn () reader
            else
                reader.Position <- pos
                ParseError.createNested ParseError.shouldFailInPlace [ err ] pos

    /// Applies the parser `p1`, if it succeeds, this parser fails without consuming input, with the given `message`.
    /// If `p1` fails without consuming input, the parser succeeds.
    /// If `p1` fails and consumes input, this parser fails without consuming input, with the given `message`.
    let inline notFollowedByL
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        message
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            reader.Position <- pos
            fail (Message message) reader
        | Error err ->
            if reader.Position = pos then
                preturn () reader
            else
                reader.Position <- pos
                ParseError.createNested (Message message) [ err ] pos

    /// Applies the parser `p1`, if it succeeds, returns the result of `p1` without consuming input.
    let inline lookAhead
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            reader.Position <- pos
            Ok s1
        | Error err ->
            reader.Position <- pos
            Error err

    /// Applies the parser `p1`, if it succeeds, returns the result of `p1`.
    /// If `p1` fails without consuming input, the parser fails with the given `message`.
    /// If `p1` fails and consumes input, the parser fails with the result of `p1`.
    let inline (<?>)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        message
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 -> Ok s1
        | Error err ->
            if pos = reader.Position then
                fail (Message message) reader
            else
                Error err

    /// Applies the parser `p1`, if it succeeds, returns the result of `p1`.
    /// If `p1` fails without consuming input, the parser fails with the given `message`.
    /// If `p1` fails and consumes input, the parser fails with a nested error of the given `message` and the result of `p1`.
    let inline (<??>)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        message
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 -> Ok s1
        | Error err ->
            if pos = reader.Position then
                fail (Message message) reader
            else
                ParseError.createNested (Message message) [ err ] pos

    /// Applies the parsers `p1` and `p2` in sequence. If both succeed, returns the results in a tuple.
    let inline tuple2
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            return struct (s1, s2)
        }

    /// Applies the parsers `p1`, `p2` and `p3` in sequence. If all succeed, returns the results in a tuple.
    let inline tuple3
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            return struct (s1, s2, s3)
        }

    /// Applies the parsers `p1`, `p2`, `p3` and `p4` in sequence. If all succeed, returns the results in a tuple.
    let inline tuple4
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p4: Parser<'D, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            let! s4 = p4
            return struct (s1, s2, s3, s4)
        }

    /// Applies the parsers `p1`, `p2`, `p3`, `p4` and `p5` in sequence. If all succeed, returns the results in a tuple.
    let inline tuple5
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p4: Parser<'D, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p5: Parser<'E, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            let! s4 = p4
            let! s5 = p5
            return struct (s1, s2, s3, s4, s5)
        }

    /// Applies the parser `p` `n` times, if it always succeeds, returns the result of `p` as an ImmutableArray of size `n`.
    let parray n (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let xs = ImmutableArray.CreateBuilder n
        let mutable i = 0
        let mutable err = ValueNone

        while err.IsNone && i < n do
            match p reader with
            | Ok s ->
                xs.Add(s.Parsed)
                i <- i + 1
            | Error e -> err <- ValueSome e

        if i = n then
            preturn (xs.MoveToImmutable()) reader
        else
            match err with
            | ValueNone -> failwith "Unreachable"
            | ValueSome err -> Error err

    /// Applies the parser `p` `n` times, if it always succeeds, returns unit.
    let skipArray n (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let mutable i = 0
        let mutable err = ValueNone

        while err.IsNone && i < n do
            match p reader with
            | Ok s -> i <- i + 1
            | Error e -> err <- ValueSome e

        if i = n then
            preturn () reader
        else
            match err with
            | ValueNone -> failwith "Unreachable"
            | ValueSome err -> Error err

    let inline private pOneThen
        ([<InlineIfLambda>] p1: Parser<_, _, _, _, _>)
        ([<InlineIfLambda>] andThen: _ -> Parser<_, _, _, _, _>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 -> andThen s1 reader
        | Error e -> ParseError.createNested ParseError.expectedAtLeastOne [ e ] pos

    /// Applies the parser `p` zero or more times. If it succeeds, returns the results as an ImmutableArray.
    /// This parser always succeeds.
    let many (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let xs = ImmutableArray.CreateBuilder()
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p reader with
            | Ok s ->
                if reader.Position = pos then
                    raise (InfiniteLoopException pos)

                xs.Add(s.Parsed)
            | Error e ->
                reader.Position <- pos
                ok <- false

        preturn (xs.ToImmutable()) reader

    /// Applies the parser `p` one or more times. If it succeeds, returns the results as an ImmutableArray.
    /// If `p` fails on the first attempt, this parser fails.
    let many1 (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        pOneThen
            p
            (fun x0 ->
                let xs = ImmutableArray.CreateBuilder()
                xs.Add(x0.Parsed)
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)

                        xs.Add(s.Parsed)
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn (xs.ToImmutable())
            )
            reader

    /// Applies the parser `p` zero or more times. If it succeeds, returns unit.
    /// This parser always succeeds.
    let skipMany (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p reader with
            | Ok s ->
                if reader.Position = pos then
                    raise (InfiniteLoopException pos)
            | Error e ->
                reader.Position <- pos
                ok <- false

        preturn () reader

    /// Applies the parser `p` one or more times. If it succeeds, returns unit.
    /// If `p` fails on the first attempt, this parser fails.
    let skipMany1 (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        pOneThen
            p
            (fun x0 ->
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ()
            )
            reader

    /// Applies the parser `p` zero or more times, separated by `pSep`. If it succeeds, returns the results as an ImmutableArray.
    /// This parser always succeeds.
    let sepBy
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p reader with
        | Ok s ->
            let xs = ImmutableArray.CreateBuilder()
            let seps = ImmutableArray.CreateBuilder()
            xs.Add(s.Parsed)

            let mutable ok = true

            while ok do
                let pos = reader.Position

                match pSep reader with
                | Ok sep ->
                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)

                        seps.Add(sep.Parsed)
                        xs.Add(s.Parsed)
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false
                | Error e ->
                    reader.Position <- pos
                    ok <- false

            preturn struct (xs.ToImmutable(), seps.ToImmutable()) reader
        | Error e ->
            reader.Position <- pos
            preturn struct (ImmutableArray.Empty, ImmutableArray.Empty) reader

    /// Applies the parser `p` one or more times, separated by `pSep`. If it succeeds, returns the results as an ImmutableArray.
    /// If `p` fails on the first attempt, this parser fails.
    let sepBy1
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun s ->
                let xs = ImmutableArray.CreateBuilder()
                let seps = ImmutableArray.CreateBuilder()
                xs.Add(s.Parsed)

                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pSep reader with
                    | Ok sep ->
                        match p reader with
                        | Ok s ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)

                            seps.Add(sep.Parsed)
                            xs.Add(s.Parsed)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn struct (xs.ToImmutable(), seps.ToImmutable())
            )
            reader

    /// Applies the parser `p` zero or more times, separated by `pSep`. If it succeeds, returns unit.
    /// This parser always succeeds.
    let skipSepBy
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p reader with
        | Ok s ->
            let mutable ok = true

            while ok do
                let pos = reader.Position

                match pSep reader with
                | Ok sep ->
                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false
                | Error e ->
                    reader.Position <- pos
                    ok <- false

            preturn () reader
        | Error e ->
            reader.Position <- pos
            preturn () reader

    /// Applies the parser `p` one or more times, separated by `pSep`. If it succeeds, returns unit.
    /// If `p` fails on the first attempt, this parser fails.
    let skipSepBy1
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun s ->
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pSep reader with
                    | Ok sep ->
                        match p reader with
                        | Ok s ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ()
            )
            reader

    /// Applies the parser `p` zero or more times, separated and optionally ended by `pSep`. If it succeeds, returns the results as an ImmutableArray.
    /// This parser always succeeds.
    let sepEndBy
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p reader with
        | Ok s ->
            let xs = ImmutableArray.CreateBuilder()
            let seps = ImmutableArray.CreateBuilder()
            xs.Add(s.Parsed)

            let mutable ok = true

            while ok do
                let pos = reader.Position

                match pSep reader with
                | Ok sep ->
                    seps.Add(sep.Parsed)
                    let pos = reader.Position

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)

                        xs.Add(s.Parsed)
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false
                | Error e ->
                    reader.Position <- pos
                    ok <- false

            preturn struct (xs.ToImmutable(), seps.ToImmutable()) reader
        | Error e ->
            reader.Position <- pos
            preturn struct (ImmutableArray.Empty, ImmutableArray.Empty) reader

    /// Applies the parser `p` one or more times, separated and optionally ended by `pSep`. If it succeeds, returns the results as an ImmutableArray.
    /// If `p` fails on the first attempt, this parser fails.
    let sepEndBy1
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun s ->
                let xs = ImmutableArray.CreateBuilder()
                let seps = ImmutableArray.CreateBuilder()
                xs.Add(s.Parsed)

                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pSep reader with
                    | Ok sep ->
                        seps.Add(sep.Parsed)
                        let posSep = reader.Position

                        match p reader with
                        | Ok s ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)

                            xs.Add(s.Parsed)
                        | Error _ ->
                            reader.Position <- posSep
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn struct (xs.ToImmutable(), seps.ToImmutable())
            )
            reader

    /// Applies the parser `p` zero or more times, separated and optionally ended by `pSep`. If it succeeds, returns unit.
    /// This parser always succeeds.
    let skipSepEndBy
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p reader with
        | Ok s ->
            let mutable ok = true

            while ok do
                let pos = reader.Position

                match pSep reader with
                | Ok sep ->
                    let posSep = reader.Position

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)
                    | Error _ ->
                        reader.Position <- posSep
                        ok <- false
                | Error e ->
                    reader.Position <- pos
                    ok <- false

            preturn () reader
        | Error e ->
            reader.Position <- pos
            preturn () reader

    /// Applies the parser `p` one or more times, separated and optionally ended by `pSep`. If it succeeds, returns unit.
    /// If `p` fails on the first attempt, this parser fails.
    let skipSepEndBy1
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pSep: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun s ->
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match pSep reader with
                    | Ok sep ->
                        let posSep = reader.Position

                        match p reader with
                        | Ok s ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)
                        | Error _ ->
                            reader.Position <- posSep
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ()
            )
            reader

    /// Applies the parser `p` zero or more times, until `pEnd` succeeds. If it succeeds, returns the results as an ImmutableArray and the result of `pEnd`.
    /// This parser fails if `pEnd` fails.
    let manyTill
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pEnd: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p reader with
        | Ok s1 ->
            let xs = ImmutableArray.CreateBuilder()
            xs.Add(s1.Parsed)
            let mutable endTok = ValueNone
            let mutable err = []

            while endTok.IsNone && err = [] do
                let pos = reader.Position

                match pEnd reader with
                | Ok s -> endTok <- ValueSome s.Parsed
                | Error eEnd ->
                    reader.Position <- pos

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)

                        xs.Add(s.Parsed)
                    | Error e ->
                        reader.Position <- pos
                        err <- [ eEnd; e ]

            match err with
            | [] -> preturn struct (xs.ToImmutable(), endTok.Value) reader
            | err -> ParseError.createNested ParseError.bothFailed err pos
        | Error _ ->
            reader.Position <- pos

            match pEnd reader with
            | Ok s -> preturn struct (ImmutableArray.Empty, s.Parsed) reader
            | Error eEnd -> Error eEnd

    /// Applies the parser `p` one or more times, until `pEnd` succeeds. If it succeeds, returns the results as an ImmutableArray and the result of `pEnd`.
    /// If `p` fails on the first attempt, this parser fails.
    /// This parser also fails if `pEnd` fails.
    let many1Till
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pEnd: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun s1 reader ->
                let xs = ImmutableArray.CreateBuilder()
                xs.Add(s1.Parsed)
                let mutable endTok = ValueNone
                let mutable err = []
                let errPos = reader.Position

                while endTok.IsNone && err = [] do
                    let pos = reader.Position

                    match pEnd reader with
                    | Ok s -> endTok <- ValueSome s.Parsed
                    | Error eEnd ->
                        reader.Position <- pos

                        match p reader with
                        | Ok s ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)

                            xs.Add(s.Parsed)
                        | Error e ->
                            reader.Position <- pos
                            err <- [ eEnd; e ]

                match err with
                | [] -> preturn struct (xs.ToImmutable(), endTok.Value) reader
                | err -> ParseError.createNested ParseError.bothFailed err errPos
            )
            reader

    /// Applies the parser `p` zero or more times, until `pEnd` succeeds. If it succeeds, returns unit.
    /// This parser fails if `pEnd` fails.
    let skipManyTill
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pEnd: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        : ParseResult<unit, 'T, 'State> =
        let pos = reader.Position

        match p reader with
        | Ok s1 ->
            let mutable endTok = ValueNone
            let mutable err = []

            while endTok.IsNone && err = [] do
                let pos = reader.Position

                match pEnd reader with
                | Ok s -> endTok <- ValueSome s.Parsed
                | Error eEnd ->
                    reader.Position <- pos

                    match p reader with
                    | Ok s ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)
                    | Error e ->
                        reader.Position <- pos
                        err <- [ eEnd; e ]

            match err with
            | [] -> preturn () reader
            | err -> ParseError.createNested ParseError.bothFailed err pos
        | Error _ ->
            reader.Position <- pos

            match pEnd reader with
            | Ok s -> preturn () reader
            | Error eEnd -> Error eEnd

    /// Applies the parser `p` one or more times, until `pEnd` succeeds. If it succeeds, returns unit.
    /// If `p` fails on the first attempt, this parser fails.
    /// This parser also fails if `pEnd` fails.
    let skipMany1Till
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (pEnd: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p
            (fun _ reader ->
                let mutable endTok = ValueNone
                let mutable err = []
                let errPos = reader.Position

                while endTok.IsNone && err = [] do
                    let pos = reader.Position

                    match pEnd reader with
                    | Ok s -> endTok <- ValueSome s.Parsed
                    | Error eEnd ->
                        reader.Position <- pos

                        match p reader with
                        | Ok _ ->
                            if reader.Position = pos then
                                raise (InfiniteLoopException pos)
                        | Error e ->
                            reader.Position <- pos
                            err <- [ eEnd; e ]

                match err with
                | [] -> preturn () reader
                | err -> ParseError.createNested ParseError.bothFailed err errPos
            )
            reader

    /// Applies the parser `p` one or more times, separated by `pOp`. If `pOp` succeeds, combines the results of `p` before and after in a left-associative manner.
    /// If `p` fails on the first attempt, this parser fails.
    let inline chainl1
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        : ParseResult<'A, 'T, 'State> =
        let rec parseLeft acc (reader: Reader<_, _, _, _>) =
            let pos = reader.Position

            match pOp reader with
            | Ok sOp ->
                match p reader with
                | Ok s ->
                    if reader.Position = pos then
                        raise (InfiniteLoopException pos)

                    let acc' = sOp.Parsed acc s.Parsed
                    parseLeft acc' reader
                | Error e -> Error e
            | Error _ ->
                reader.Position <- pos
                preturn acc reader

        match p reader with
        | Ok s -> parseLeft s.Parsed reader
        | Error e -> Error e

    /// Applies the parser `p` zero or more times, separated by `pOp`. If `pOp` succeeds, combines the results of `p` before and after in a left-associative manner.
    /// If `p` fails on the first attempt, this parser returns the result of `orElse`.
    /// This parser always succeeds.
    let chainl p pOp orElse reader = (chainl1 p pOp <|>% orElse) reader

    /// Applies the parser `p` one or more times, separated by `pOp`. If `pOp` succeeds, combines the results of `p` before and after in a right-associative manner.
    /// If `p` fails on the first attempt, this parser fails.
    let inline chainr1
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        : ParseResult<'A, 'T, 'State> =
        let rec fold acc pLast =
            match acc with
            | [] -> preturn pLast
            | (op, p) :: rest -> fold rest (op p pLast)

        let rec parseRight prevPos acc (reader: Reader<_, _, _, _>) =
            match p reader with
            | Ok s ->
                if reader.Position = prevPos then
                    raise (InfiniteLoopException prevPos)

                let pos = reader.Position

                match pOp reader with
                | Ok sOp ->
                    let acc = (sOp.Parsed, s.Parsed) :: acc
                    parseRight reader.Position acc reader
                | Error _ ->
                    reader.Position <- pos
                    preturn (acc, s.Parsed) reader
            | Error e -> Error e

        match p reader with
        | Ok s ->
            let pos = reader.Position

            match pOp reader with
            | Ok sOp ->
                let acc = [ (sOp.Parsed, s.Parsed) ]

                match parseRight reader.Position acc reader with
                | Ok stack ->
                    let (acc, pLast) = stack.Parsed
                    (fold acc pLast) reader
                | Error e -> Error e
            | Error _ ->
                reader.Position <- pos
                preturn s.Parsed reader
        | Error e -> Error e

    /// Applies the parser `p` zero or more times, separated by `pOp`. If `pOp` succeeds, combines the results of `p` before and after in a right-associative manner.
    /// If `p` fails on the first attempt, this parser returns the result of `orElse`.
    /// This parser always succeeds.
    let chainr p pOp orElse reader = (chainr1 p pOp <|>% orElse) reader

    /// Applies the parser `p1` followed by `p` zero or more times. If it succeeds, returns the results as an ImmutableArray.
    /// If `p1` fails, this parser fails.
    let many1Items2
        (p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        pOneThen
            p1
            (fun s1 ->
                let xs = ImmutableArray.CreateBuilder()
                let inline append (c) = xs.Add(c)
                append s1.Parsed
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match p reader with
                    | Ok sx ->
                        if reader.Position = pos then
                            raise (InfiniteLoopException pos)

                        append sx.Parsed
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false

                preturn (xs.ToImmutable())
            )
            reader
