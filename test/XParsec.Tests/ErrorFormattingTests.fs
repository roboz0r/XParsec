module ErrorFormattingTests

open System
open System.Text

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.CharParsers
open XParsec.Parsers

let formatCharError e sb =
    ErrorFormatting.formatParseError
        (fun (x: char) sb -> sb.Append(''').Append(x).Append('''))
        (fun (xs: char seq) sb ->
            sb.Append('"') |> ignore
            (sb, xs) ||> Seq.fold (fun sb x -> sb.Append x) |> ignore
            sb.Append('"')
        )
        e
        sb

let sample = "The quick brown fox jumps over the lazy dog."

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "ErrorFormattingTests"
        [
            test "Parse line endings" {
                let inputSample =
                    [ "Line 1"; "Line 22"; "Line 333"; "Line 4444"; "Line 55555"; "Line 666666" ]
                    |> String.concat Environment.NewLine

                let index = LineIndex.OfString inputSample
                let nlLength = Environment.NewLine.Length

                let actual = index.Indices |> Seq.map int |> Seq.toList

                let expected =
                    [ 5; 12; 20; 29; 39 ] |> List.mapi (fun i x -> x + (i + 1) * nlLength)

                "" |> Expect.equal actual expected

                let expteced = [ '\n'; '\n'; '\n'; '\n'; '\n' ]
                let actual = actual |> List.map (fun i -> inputSample.[i])
                "" |> Expect.equal actual expteced

                let expectedIndices =
                    [
                        0, struct (1, 1)
                        1, (1, 2)
                        5 + nlLength, (1, 6 + nlLength)
                        6 + nlLength, (2, 1)
                        7 + nlLength, (2, 2)
                        8 + nlLength, (2, 3)
                        12 + (2 * nlLength), (2, 7 + nlLength)
                        13 + (2 * nlLength), (3, 1)
                        14 + (2 * nlLength), (3, 2)
                        21 + (2 * nlLength), (3, 9)
                        20 + (3 * nlLength), (3, 8 + nlLength)
                        21 + (3 * nlLength), (4, 1)
                        29 + (3 * nlLength), (4, 9)
                        30 + (3 * nlLength), (4, 10)
                        29 + (4 * nlLength), (4, 9 + nlLength)
                        30 + (4 * nlLength), (5, 1)
                        31 + (4 * nlLength), (5, 2)
                        45 + (5 * nlLength), (6, 6)
                    ]

                let actualIndices =
                    expectedIndices
                    |> List.map (fun (i, _) ->
                        let struct (l, c) = index.GetLineCol(i)
                        i, struct (l, int c)
                    )

                "Can calculate line and column numbers"
                |> Expect.equal actualIndices expectedIndices

                let actualIndices =
                    expectedIndices
                    |> List.map (fun (_, (ln, col)) -> int (index.GetIndex(ln, col)), struct (ln, col))

                "Can calculate index from line and column numbers"
                |> Expect.equal actualIndices expectedIndices
            }

            test "Parse line endings sliced" {
                let inputSample =
                    [ "Line 1"; "Line 22"; "Line 333"; "Line 4444"; "Line 55555"; "Line 666666" ]
                    |> String.concat Environment.NewLine

                let nlLength = Environment.NewLine.Length
                let index = LineIndex.OfString(inputSample, 32 + (4 * nlLength))

                let actual = index.Indices |> Seq.map int |> Seq.toList

                let expected = [ 5; 12; 20; 29 ] |> List.mapi (fun i x -> x + (i + 1) * nlLength)

                "" |> Expect.equal actual expected

                let expteced = [ '\n'; '\n'; '\n'; '\n' ]
                let actual = actual |> List.map (fun i -> inputSample.[i])
                "" |> Expect.equal actual expteced

                let expectedIndices =
                    [
                        0, struct (1, 1)
                        1, (1, 2)
                        5 + nlLength, (1, 6 + nlLength)
                        6 + nlLength, (2, 1)
                        7 + nlLength, (2, 2)
                        8 + nlLength, (2, 3)
                        12 + (2 * nlLength), (2, 7 + nlLength)
                        13 + (2 * nlLength), (3, 1)
                        14 + (2 * nlLength), (3, 2)
                        21 + (2 * nlLength), (3, 9)
                        20 + (3 * nlLength), (3, 8 + nlLength)
                        21 + (3 * nlLength), (4, 1)
                        29 + (3 * nlLength), (4, 9)
                        30 + (3 * nlLength), (4, 10)
                        29 + (4 * nlLength), (4, 9 + nlLength)
                        30 + (4 * nlLength), (5, 1)
                        31 + (4 * nlLength), (5, 2)
                    ]

                let actualIndices =
                    expectedIndices
                    |> List.map (fun (i, _) ->
                        let struct (l, c) = index.GetLineCol(i)
                        i, struct (l, int c)
                    )

                "Can calculate line and column numbers"
                |> Expect.equal actualIndices expectedIndices

                let actualIndices =
                    expectedIndices
                    |> List.map (fun (_, (ln, col)) -> int (index.GetIndex(ln, col)), struct (ln, col))

                "Can calculate index from line and column numbers"
                |> Expect.equal actualIndices expectedIndices
            }

            test "Format Message" {
                let input = Reader.ofString "test" ()
                let p = (pchar 'a') <?> "Error message"

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()
                    let expected = "Error message"
                    "" |> Expect.equal actual expected
            }

            test "Format Expected" {
                let input = Reader.ofString "test" ()
                let p = pchar 'a'

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()
                    let expected = "Expected 'a'"
                    "" |> Expect.equal actual expected
            }

            test "Format Expected Seq" {
                let input = Reader.ofString "test" ()
                let p = pstring "ab"

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()
                    let expected = "Expected \"ab\""
                    "" |> Expect.equal actual expected
            }

            test "Format Unxpected" {
                let input = Reader.ofString "test" ()
                let p = satisfy (fun x -> x = 'a')

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()
                    let expected = "Unexpected 't'"
                    "" |> Expect.equal actual expected
            }

            test "Format EndOfInput" {
                let input = Reader.ofString "" ()
                let p = pchar 'a'

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()
                    let expected = "Unexpected end of input"
                    "" |> Expect.equal actual expected
            }

            test "Format Nested" {
                let input = Reader.ofString "test" ()
                let p = choice [ pchar 'a'; pchar 'b' ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()

                    let expected =
                        [ "All choices failed."; "├───Expected 'a'"; "└───Expected 'b'" ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format Doubly Nested" {
                let input = Reader.ofString "test" ()

                let p =
                    choice
                        [
                            pchar 'a'
                            pchar 'b'

                            choice [ satisfy (fun c -> c = 'c'); pchar 'd' ]
                            pchar 'e'
                        ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()

                    let expected =
                        [
                            "All choices failed."
                            "├───Expected 'a'"
                            "├───Expected 'b'"
                            "├───All choices failed."
                            "│   ├───Unexpected 't'"
                            "│   └───Expected 'd'"
                            "└───Expected 'e'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format triply nested" {
                let input = Reader.ofString "test" ()

                let p =
                    choice
                        [
                            pchar 'a'
                            pstring "bb" >>% 'b'
                            choice [ satisfy (fun c -> c = 'c'); choice [ pchar 'd'; pchar 'e' ]; pchar 'f' ]
                            pchar 'g'
                        ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()

                    let expected =
                        [
                            "All choices failed."
                            "├───Expected 'a'"
                            "├───Expected \"bb\""
                            "├───All choices failed."
                            "│   ├───Unexpected 't'"
                            "│   ├───All choices failed."
                            "│   │   ├───Expected 'd'"
                            "│   │   └───Expected 'e'"
                            "│   └───Expected 'f'"
                            "└───Expected 'g'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format triply nested 2" {
                let input = Reader.ofString "test" ()

                let p =
                    choice
                        [
                            pchar 'a'
                            pstring "bb" >>% 'b'
                            choice [ satisfy (fun c -> c = 'c'); choice [ pchar 'd'; pchar 'e' ] ]
                            pchar 'g'
                        ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let sb = StringBuilder()
                    let sb = formatCharError e sb
                    let actual = sb.ToString()

                    let expected =
                        [
                            "All choices failed."
                            "├───Expected 'a'"
                            "├───Expected \"bb\""
                            "├───All choices failed."
                            "│   ├───Unexpected 't'"
                            "│   └───All choices failed."
                            "│       ├───Expected 'd'"
                            "│       └───Expected 'e'"
                            "└───Expected 'g'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format String Error 1" {
                let input = Reader.ofString sample ()
                let p = (pchar 'a') <?> "Error message"

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let actual = ErrorFormatting.formatStringError sample e

                    let expected =
                        [
                            "The quick brown fox jumps over the lazy d"
                            "^ At index 0 (Ln 1, Col 1)"
                            "Error message"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format String Error Nested" {
                let input = Reader.ofString sample ()

                let p =
                    pstring "The "
                    .>>. choice [ pchar 'a'; choice [ satisfy (fun c -> c = 'b'); pchar 'c' ]; pchar 'd' ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let actual = ErrorFormatting.formatStringError sample e

                    let expected =
                        [
                            "The quick brown fox jumps over the lazy dog."
                            "    ^ At index 4 (Ln 1, Col 5)"
                            "All choices failed."
                            "├───Expected 'a'"
                            "├───All choices failed."
                            "│   ├───Unexpected 'q'"
                            "│   └───Expected 'c'"
                            "└───Expected 'd'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format String Error Nested 2" {
                let input = Reader.ofString sample ()

                let p =
                    pstring "The "
                    .>>. choice [ pchar 'a'; choice [ satisfy (fun c -> c = 'b'); pchar 'c' ] ]

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let actual = ErrorFormatting.formatStringError sample e

                    let expected =
                        [
                            "The quick brown fox jumps over the lazy dog."
                            "    ^ At index 4 (Ln 1, Col 5)"
                            "All choices failed."
                            "├───Expected 'a'"
                            "└───All choices failed."
                            "    ├───Unexpected 'q'"
                            "    └───Expected 'c'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }

            test "Format String Error Line 2" {
                let inputSample = $"1{sample}{Environment.NewLine}2{sample}"
                let input = Reader.ofString inputSample ()

                let p = tuple5 (pchar '1') (pstring sample) newline (pstring "2The ") (pchar 'a')

                match p input with
                | Ok _ -> failwith "Expected parse error"
                | Error e ->
                    let actual = ErrorFormatting.formatStringError inputSample e
                    let nlLength = Environment.NewLine.Length

                    let expected =
                        [
                            // This is broken -- Fix tomorrow
                            "2The quick brown fox jumps over the lazy dog."
                            $"     ^ At index {50 + nlLength} (Ln 2, Col 6)"
                            "Expected 'a'"
                        ]
                        |> String.concat Environment.NewLine

                    "" |> Expect.equal actual expected
            }
        ]
