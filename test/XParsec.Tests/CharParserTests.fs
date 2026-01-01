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


let expectedOneOf (candidates: string list) =
    candidates |> Seq.map (fun s -> s :> char seq) |> ExpectedSeqOneOf

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
        | Error err ->

            match expected with
            | ExpectedSeqOneOf xss ->
                match err.Errors with
                | ExpectedSeqOneOf yss ->
                    // Compare sequences of sequences isn't automatic, so do it manually
                    let expectedSets =
                        xss |> Seq.map (fun xs -> xs |> Seq.toArray |> String) |> Seq.toArray

                    let actualSets =
                        yss |> Seq.map (fun ys -> ys |> Seq.toArray |> String) |> Seq.toArray

                    msg |> Expect.equal actualSets expectedSets
                | _ -> failwithf "%s: Expected ExpectedSeqOneOf error but got %A" msg err.Errors
            | _ -> msg |> Expect.equal err.Errors expected

let confirmAt msg input expected endIndex (parser: Parser<_, _, _, _, _>) =
    let reader = Reader.ofString input ()

    match expected with
    | Ok expected ->
        match parser reader with
        | Ok { Parsed = value } ->
            msg |> Expect.equal value expected

            $"{msg}: Reader should be at {endIndex} after parsing."
            |> Expect.equal (reader.Index) endIndex
        | Error err -> failwithf "%s: %A" msg err
    | Error expected ->
        match parser reader with
        | Ok _ -> failwithf "%s: Expected error %A but got a successful parse." msg expected
        | Error err ->
            msg |> Expect.equal err.Errors expected

            $"{msg}: Reader should be at {endIndex} after parsing."
            |> Expect.equal (reader.Index) endIndex

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

            test "anyString - Greedy matching" {
                // Crucial Test: Ensure "==" is matched before "="
                // even though "=" appears first in the list, "==" is longer.
                let p = anyString [ "="; "=="; "==="; "!=" ]

                let errorExpected =
                    // Note: The parser sorts candidates by length desc, then ordinal
                    expectedOneOf [ "==="; "!="; "=="; "=" ]

                let cases =
                    [
                        "===", Ok "==="
                        "==", Ok "=="
                        "=", Ok "="
                        "!=", Ok "!="
                        "!", Error errorExpected
                        "", Error EndOfInput
                    ]

                for input, expected in cases do
                    confirm $"Greedy matching failed for '{input}'" input expected p
            }

            test "anyStringCI - Case Insensitivity and Normalization" {
                // Tests that we match case-insensitively,
                // BUT return the canonical string defined in the list.
                let p = anyStringCI [ "Select"; "From"; "Where" ]

                let errorExpected = expectedOneOf [ "Select"; "Where"; "From" ] // Sorted by Length Desc

                let cases =
                    [
                        "select", Ok "Select"
                        "SELECT", Ok "Select"
                        "SeLeCt", Ok "Select"
                        "from", Ok "From"
                        "sel", Error errorExpected
                    ]

                for input, expected in cases do
                    confirm $"CI matching failed for '{input}'" input expected p
            }

            test "anyStringReturn - Value Mapping" {
                let p = anyStringReturn [ "true", true; "false", false; "yes", true; "no", false ]

                let cases =
                    [
                        "true", Ok true
                        "false", Ok false
                        "yes", Ok true
                        "no", Ok false
                        "yep", Error(expectedOneOf [ "false"; "true"; "yes"; "no" ])
                    ]

                for input, expected in cases do
                    confirm $"Value mapping failed for '{input}'" input expected p
            }

            test "anyStringL / anyStringReturnL - Custom Error Messages" {
                // 1. Test string return with label
                let p1 = anyStringL [ "foo"; "bar" ] "expected identifier"

                let cases1 = [ "foo", Ok "foo"; "baz", Error(Message "expected identifier") ]

                for input, expected in cases1 do
                    confirm $"anyStringL failed for '{input}'" input expected p1

                // 2. Test value return with label
                let p2 = anyStringReturnL [ "+", "Add"; "-", "Sub" ] "arithmetic operator"

                let cases2 = [ "+", Ok "Add"; "*", Error(Message "arithmetic operator") ]

                for input, expected in cases2 do
                    confirm $"anyStringReturnL failed for '{input}'" input expected p2
            }

            test "anyString - Edge Cases" {
                // Test logic with overlapping prefixes but different lengths
                let p = anyString [ "abc"; "ab"; "a" ]

                let cases = [ "abc", Ok "abc"; "ab", Ok "ab"; "a", Ok "a" ]

                for input, expected in cases do
                    confirm $"Edge case overlap failed for '{input}'" input expected p
            }
        ]
