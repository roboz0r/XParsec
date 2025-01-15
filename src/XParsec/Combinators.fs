namespace XParsec


[<AutoOpen>]
module Combinators =
    open System.Collections.Immutable
    open Parsers

    let inline (>>=)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] binder: 'A -> Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success ->
            let p2 = binder success.Parsed
            p2 reader
        | Error err -> Error err

    let inline (>>%)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        x
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success -> preturn x reader
        | Error err -> Error err

    let inline (>>.)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        match p1 reader with
        | Ok success -> p2 reader
        | Error err -> Error err

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

    let inline (|>>)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] map: 'A -> 'B)
        (reader: Reader<_, _, _, _>)
        =
        match p reader with
        | Ok success -> preturn (map success.Parsed) reader
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

    let parser = ParserCE()

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
            | Error err2 -> ParseError.createNestedP (Message "Both failed") [ err1; err2 ] p

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
            | ValueNone -> ParseError.createNestedP ParseError.allChoicesFailed [ yield! errs ] p
            | ValueSome x -> Ok x

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

    let inline (<|>%)
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (x)
        (reader: Reader<_, _, _, _>)
        =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> Ok s1
        | Error _ ->
            reader.Position <- p
            preturn x reader

    let inline opt ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let p = reader.Position

        match p1 reader with
        | Ok s1 -> preturn (ValueSome s1.Parsed) reader
        | Error _ ->
            reader.Position <- p
            preturn ValueNone reader

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
                ParseError.createNested ParseError.shouldFailInPlace [ err ] reader

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
                ParseError.createNested (Message message) [ err ] reader

    let inline lookAhead
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        =
        let pos = reader.Position

        match p1 reader with
        | Ok s1 ->
            reader.Position <- pos
            preturn (s1.Parsed) reader
        | Error err ->
            reader.Position <- pos
            Error err

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
                ParseError.createNestedP (Message message) [ err ] pos

    let inline tuple2
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            return s1, s2
        }


    let inline tuple3
        ([<InlineIfLambda>] p1: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p2: Parser<'B, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] p3: Parser<'C, 'T, 'State, 'Input, 'InputSlice>)
        =
        parser {
            let! s1 = p1
            let! s2 = p2
            let! s3 = p3
            return s1, s2, s3
        }


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
            return s1, s2, s3, s4
        }


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
            return s1, s2, s3, s4, s5
        }

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
        | Error e -> ParseError.createNestedP ParseError.expectedAtLeastOne [ e ] pos

    let many (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let xs = ImmutableArray.CreateBuilder()
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p reader with
            | Ok s -> xs.Add(s.Parsed)
            | Error e ->
                reader.Position <- pos
                ok <- false

        preturn (xs.ToImmutable()) reader

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
                    | Ok s -> xs.Add(s.Parsed)
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn (xs.ToImmutable()))
            reader

    let skipMany (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        let mutable ok = true

        while ok do
            let pos = reader.Position

            match p reader with
            | Ok s -> ()
            | Error e ->
                reader.Position <- pos
                ok <- false

        preturn () reader

    let skipMany1 (p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>) (reader: Reader<_, _, _, _>) =
        pOneThen
            p
            (fun x0 ->
                let mutable ok = true

                while ok do
                    let pos = reader.Position

                    match p reader with
                    | Ok s -> ()
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ())
            reader

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
                            seps.Add(sep.Parsed)
                            xs.Add(s.Parsed)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn struct (xs.ToImmutable(), seps.ToImmutable()))
            reader

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
                    | Ok s -> ()
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
                        | Ok s -> ()
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ())
            reader

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
                    | Ok s -> xs.Add(s.Parsed)
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
                        let pos = reader.Position

                        match p reader with
                        | Ok s -> xs.Add(s.Parsed)
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn struct (xs.ToImmutable(), seps.ToImmutable()))
            reader

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
                    let pos = reader.Position

                    match p reader with
                    | Ok s -> ()
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
                        let pos = reader.Position

                        match p reader with
                        | Ok s -> ()
                        | Error _ ->
                            reader.Position <- pos
                            ok <- false
                    | Error e ->
                        reader.Position <- pos
                        ok <- false

                preturn ())
            reader

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
                    | Ok s -> xs.Add(s.Parsed)
                    | Error e ->
                        reader.Position <- pos
                        err <- [ eEnd; e ]

            match err with
            | [] -> preturn struct (xs.ToImmutable(), endTok.Value) reader
            | err -> ParseError.createNestedP (Message "Both failed") err pos
        | Error _ ->
            reader.Position <- pos

            match pEnd reader with
            | Ok s -> preturn struct (ImmutableArray.Empty, s.Parsed) reader
            | Error eEnd -> Error eEnd

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
                        | Ok s -> xs.Add(s.Parsed)
                        | Error e ->
                            reader.Position <- pos
                            err <- [ eEnd; e ]

                match err with
                | [] -> preturn struct (xs.ToImmutable(), endTok.Value) reader
                | err -> ParseError.createNestedP (Message "Both failed") err errPos)
            reader

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
                    | Ok s -> ()
                    | Error e ->
                        reader.Position <- pos
                        err <- [ eEnd; e ]

            match err with
            | [] -> preturn () reader
            | err -> ParseError.createNestedP (Message "Both failed") err pos
        | Error _ ->
            reader.Position <- pos

            match pEnd reader with
            | Ok s -> preturn () reader
            | Error eEnd -> Error eEnd

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
                        | Ok _ -> ()
                        | Error e ->
                            reader.Position <- pos
                            err <- [ eEnd; e ]

                match err with
                | [] -> preturn () reader
                | err -> ParseError.createNestedP (Message "Both failed") err errPos)
            reader

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
                    let acc' = sOp.Parsed acc s.Parsed
                    parseLeft acc' reader
                | Error e -> Error e
            | Error _ ->
                reader.Position <- pos
                preturn acc reader

        match p reader with
        | Ok s -> parseLeft s.Parsed reader
        | Error e -> Error e

    let chainl p pOp orElse reader = ((chainl1 p pOp) <|>% orElse) reader

    let inline chainr1
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input, 'InputSlice>)
        ([<InlineIfLambda>] pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input, 'InputSlice>)
        (reader: Reader<_, _, _, _>)
        : ParseResult<'A, 'T, 'State> =
        let rec fold acc pLast =
            match acc with
            | [] -> preturn pLast
            | (op, p) :: rest -> fold rest (op p pLast)

        let rec parseRight acc (reader: Reader<_, _, _, _>) =
            match p reader with
            | Ok s ->
                let pos = reader.Position

                match pOp reader with
                | Ok sOp ->
                    let acc = (sOp.Parsed, s.Parsed) :: acc
                    parseRight acc reader
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

                match parseRight acc reader with
                | Ok stack ->
                    let (acc, pLast) = stack.Parsed
                    (fold acc pLast) reader
                | Error e -> Error e
            | Error _ ->
                reader.Position <- pos
                preturn s.Parsed reader
        | Error e -> Error e

    let chainr p pOp orElse reader = ((chainr1 p pOp) <|>% orElse) reader

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
                    | Ok sx -> append sx.Parsed
                    | Error _ ->
                        reader.Position <- pos
                        ok <- false

                preturn (xs.ToImmutable()))
            reader
