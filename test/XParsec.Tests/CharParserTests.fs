module CharParserTests

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open System
open XParsec
open XParsec.CharParsers
open XParsec.Parsers

let confirm msg input expected (parser: Parser<_, _, _, _, _>) =
    let reader = Reader.ofString input ()

    match expected with
    | Ok expected ->
        match parser reader with
        | Ok { Parsed = value } ->
            msg |> Expect.equal value expected
            $"{msg}: Reader should be at end after parsing." |> Expect.isTrue (reader.AtEnd)
        | Error err -> failwithf "%s: %A" msg err
    | Error expected ->
        match parser reader with
        | Ok _ -> failwithf "%s: Expected error %A but got a successful parse." msg expected
        | Error err -> msg |> Expect.equal err.Errors expected

let confirmAt msg input expected endIndex (parser: Parser<_, _, _, _, _>) =
    let reader = Reader.ofString input ()

    match expected with
    | Ok expected ->
        match parser reader with
        | Ok { Parsed = value } ->
            msg |> Expect.equal value expected

            $"{msg}: Reader should be at {endIndex} after parsing."
            |> Expect.equal (reader.Index) (int64 endIndex)
        | Error err -> failwithf "%s: %A" msg err
    | Error expected ->
        match parser reader with
        | Ok _ -> failwithf "%s: Expected error %A but got a successful parse." msg expected
        | Error err ->
            msg |> Expect.equal err.Errors expected

            $"{msg}: Reader should be at {endIndex} after parsing."
            |> Expect.equal (reader.Index) (int64 endIndex)

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "CharParserTests"
        [
            test "pchar" {
                let cases = [ "A", Ok 'A'; "a", Error(Expected 'A'); "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with pchar failed."
                    confirm msg input expected (pchar 'A')
            }

            test "skipChar" {
                let cases = [ "A", Ok(); "a", Error(Expected 'A'); "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with skipChar failed."
                    confirm msg input expected (skipChar 'A')
            }

            test "charReturn" {
                let cases = [ "A", Ok 1; "a", Error(Expected 'A'); "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with charReturn failed."
                    confirm msg input expected (charReturn 'A' 1)
            }

            test "anyChar" {
                let cases = [ "A", Ok 'A'; "a", Ok 'a'; "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with anyChar failed."
                    confirm msg input expected anyChar
            }

            test "skipAnyChar" {
                let cases = [ "A", Ok(); "a", Ok(); "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with skipAnyChar failed."
                    confirm msg input expected skipAnyChar
            }

            test "pstring" {
                let cases =
                    [
                        "Hello", Ok "Hello"
                        "hello", Error(ExpectedSeq "Hello")
                        "He", Error(ExpectedSeq "Hello")
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with pstring failed."
                    confirm msg input expected (pstring "Hello")
            }

            test "stringCIReturn" {
                let cases =
                    [
                        "Hello", Ok 1
                        "hello", Ok 1
                        "He", Error(ExpectedSeq "Hello")
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with stringCIReturn failed."
                    confirm msg input expected (stringCIReturn "Hello" 1)
            }

            test "stringReturn" {
                let cases =
                    [
                        "Hello", Ok 1
                        "hello", Error(ExpectedSeq "Hello")
                        "He", Error(ExpectedSeq "Hello")
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with stringReturn failed."
                    confirm msg input expected (stringReturn "Hello" 1)
            }

            test "asciiLetter" {
                let cases =
                    [
                        "A", Ok 'A'
                        "a", Ok 'a'
                        "1", Error ParseError.asciiLetter
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with asciiLetter failed."
                    confirm msg input expected asciiLetter
            }

            test "digit" {
                let cases =
                    [ "1", Ok '1'; "0", Ok '0'; "A", Error ParseError.digit; "", Error EndOfInput ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with digit failed."
                    confirm msg input expected digit
            }

            test "manyChars" {
                let cases = [ "Hello", Ok "Hello"; "hello", Ok "hello"; "He", Ok "He"; "", Ok "" ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with manyChars failed."
                    confirm msg input expected (manyChars anyChar)
            }

            test "manyChars infinite loop protection" {
                let cases = [ "Hello", Error ParseError.infiniteLoop ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with manyChars failed."

                    let p =
                        parser {
                            let! pos = getPosition
                            let! result = anyChar
                            do! setPosition pos
                            return result
                        }

                    confirm msg input expected (manyChars p)
            }

            test "many1Chars" {
                let cases =
                    [
                        "Hello", Ok "Hello"
                        "hello", Ok "hello"
                        "He", Ok "He"
                        "1", Error ParseError.asciiLetter
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with many1Chars failed."
                    confirm msg input expected (many1Chars asciiLetter)
            }

            test "spaces" {
                let cases = [ "   ", Ok(), 3; "  \n\r\t", Ok(), 5; "Hello", Ok(), 0; "", Ok(), 0 ]

                for input, expected, endAt in cases do
                    let msg = $"Parsing '{input}' with spaces failed."
                    confirmAt msg input expected endAt spaces
            }

            test "spaces1" {
                let cases =
                    [
                        "   ", Ok(), 3
                        "  \n\r\t", Ok(), 5
                        "Hello", Error(Message ParseError.spaces1), 0
                        "", Error EndOfInput, 0
                    ]

                for input, expected, endAt in cases do
                    let msg = $"Parsing '{input}' with spaces1 failed."
                    confirmAt msg input expected endAt spaces1
            }

            test "many1Chars2" {
                let cases =
                    [
                        "Hello", Ok "Hello", 5
                        "hello", Ok "hello", 5
                        "He", Ok "He", 2
                        "1", Error ParseError.asciiLetter, 0
                        "H1", Ok "H", 1
                        "", Error EndOfInput, 0
                    ]

                for input, expected, endAt in cases do
                    let msg = $"Parsing '{input}' with many1Chars2 failed."
                    confirmAt msg input expected endAt (many1Chars2 asciiLetter asciiLetter)
            }

            test "manyCharsTill" {
                let cases =
                    [
                        "Hello World", Ok("Hello", ' '), 6
                        "hello world", Ok("hello", ' '), 6
                        "He", Error EndOfInput, 2
                        "", Error EndOfInput, 0
                    ]

                for input, expected, endAt in cases do
                    let msg = $"Parsing '{input}' with manyCharsTill failed."
                    confirmAt msg input expected endAt (manyCharsTill anyChar (pchar ' '))
            }

            test "newlineReturn" {
                let cases =
                    [
                        "\n", Ok 1, 1
                        "\r\n", Ok 1, 2
                        "Hello", Error ParseError.expectedNewline, 0
                        "", Error EndOfInput, 0
                    ]

                for input, expected, endAt in cases do
                    let msg = $"Parsing '{input}' with newlineReturn failed."
                    confirmAt msg input expected endAt (newlineReturn 1)

                    let expected =
                        match expected with
                        | Ok v -> Ok '\n'
                        | Error e -> Error e

                    confirmAt msg input expected endAt newline

                    let expected =
                        match expected with
                        | Ok v -> Ok()
                        | Error e -> Error e

                    confirmAt msg input expected endAt skipNewline
            }

            test "anyOf" {
                let chars = "Aa1"

                let cases =
                    [
                        "A", Ok 'A'
                        "a", Ok 'a'
                        "1", Ok '1'
                        "", Error EndOfInput
                        "B", Error(Message $"Any character: '{chars}'")
                    ]

                for input, expected in cases do
                    let msg = $"Parsing '{input}' with anyOf failed."
                    confirm msg input expected (CharParsers.anyOf chars)
                    confirm msg input expected (CharParsers.anyOf [ 'A'; 'a'; '1' ])
            }
        ]
