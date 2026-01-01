module ParserTests

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open System
open XParsec
open XParsec.Parsers

type TestState = { Count: int; Log: string list }

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "ParserTests"
        [
            test "PReturn" {
                let input = "input"

                let p = preturn true
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.isTrue result.Parsed
                    "" |> Expect.equal reader.Index 0
                | Error e -> failwithf "%A" e
            }

            test "PZero" {
                let input = "input"

                let p = pzero
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    "" |> Expect.equal e.Position.Index 0
                    "" |> Expect.equal e.Errors ParseError.zero
                    "" |> Expect.equal reader.Index 0
            }

            test "UserState" {
                let input = "input"

                let p =
                    parser {
                        let! state = getUserState
                        do! setUserState "state"

                        do!
                            updateUserState (
                                function
                                | "state" -> "STATE"
                                | _ -> failwith "Invalid state"
                            )

                        do! userStateSatisfies (fun s -> s = "STATE")
                        return state
                    }

                let reader = Reader.ofString input "state"
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "state"
                    "" |> Expect.equal reader.Index 0
                    "" |> Expect.equal reader.State "STATE"
                | Error e -> failwithf "%A" e
            }

            test "EoF" {
                let input = ""

                let p = eof
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "" |> Expect.equal reader.Index 0
                | Error e -> failwithf "%A" e
            }

            test "pId" {
                let input = "input"

                let p = pid
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed 'i'
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "Skip" {
                let input = "input"

                let p = skip
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "Satisfy" {
                let input = "input"

                let p = satisfy (fun c -> c = 'i')
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed 'i'
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "SatisfyL" {
                let input = "input"

                let p = satisfyL (fun c -> c = 'i') "Expected 'i'"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed 'i'
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "ItemReturn" {
                let input = "input"

                let p = itemReturn 'i' true
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.isTrue result.Parsed
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "AnyOf" {
                let input = "input"

                let p = anyOf [ 'i'; 'n' ]
                let reader = Reader.ofString input ()
                let result = (p .>>. p) reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ('i', 'n')
                    "" |> Expect.equal reader.Index 2
                | Error e -> failwithf "%A" e
            }

            test "SkipAnyOf" {
                let input = "input"

                let p = skipAnyOf [ 'i'; 'n' ]
                let reader = Reader.ofString input ()
                let result = (p .>>. p) reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ((), ())
                    "" |> Expect.equal reader.Index 2
                | Error e -> failwithf "%A" e
            }

            test "NoneOf" {
                let input = "input"

                let p = noneOf "nput"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed 'i'
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "SkipNoneOf" {
                let input = "input"

                let p = skipNoneOf "nput"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 1
                | Error e -> failwithf "%A" e
            }

            test "AnyInRange" {
                let input = "input"

                let p = anyInRange 'a' 'z'
                let reader = Reader.ofString input ()
                let result = (p .>>. p) reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ('i', 'n')
                    "" |> Expect.equal reader.Index 2
                | Error e -> failwithf "%A" e
            }

            test "SkipAnyInRange" {
                let input = "input"

                let p = skipAnyInRange 'a' 'z'
                let reader = Reader.ofString input ()
                let result = (p .>>. p) reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ((), ())
                    "" |> Expect.equal reader.Index 2
                | Error e -> failwithf "%A" e
            }

            test "PSeq" {
                let input = "input"

                let p = pseq "input"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
#if FABLE_COMPILER
                    for i in 0..4 do
                        "" |> Expect.equal (result.Parsed[i]) (input[i])
#else
                    "" |> Expect.sequenceEqual result.Parsed "input"
#endif
                    "" |> Expect.equal reader.Index 5
                | Error e -> failwithf "%A" e
            }

            test "PSeqReturn" {
                let input = "input"

                let p = pseqReturn "input" true
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.isTrue result.Parsed
                    "" |> Expect.equal reader.Index 5
                | Error e -> failwithf "%A" e
            }
            test "fold - Summation" {
                // Inputs: "123"
                // Behavior: Parse digits as ints and sum them up
                // Initial state: 0
                let pDigit = satisfy Char.IsDigit |>> fun c -> int (string c)
                let p = fold 0 (+) pDigit

                let cases =
                    [
                        "123", Ok 6 // 1+2+3
                        "9", Ok 9
                        "", Ok 0 // Zero occurrences is valid
                        "12a", Ok 3 // Parses 1, then 2, fails on 'a', backtracks, returns 3
                    ]

                for input, expected in cases do
                    let reader = Reader.ofString input ()

                    match p reader, expected with
                    | Ok result, Ok expected -> "" |> Expect.equal result.Parsed expected
                    | Error actual, Error expected -> "" |> Expect.equal actual expected
                    | actual, exp -> failwith $"Input '{input}': Expected {exp}, got {actual}"
            }

            test "fold - Infinite Loop Protection" {
                // pstring "" always succeeds and consumes nothing
                // fold should detect this and throw
                let p = fold 0 (fun s _ -> s + 1) (lookAhead (pseq "input"))

                try
                    let _ = p (Reader.ofString "input" ())
                    failwith "Should have thrown InfiniteLoopException"
                with
                | :? InfiniteLoopException<unit> -> () // Pass
                | ex ->
#if FABLE_COMPILER
                    failwith $"Wrong exception {ex}"
#else
                    failwith $"Wrong exception {ex.GetType()} {ex.Message}"
#endif
            }

            test "fold1 - At least one required" {
                let pDigit = satisfy Char.IsDigit |>> fun c -> int (string c)
                let p = fold1 0 (+) pDigit

                let cases =
                    [
                        "123", Ok 6
                        "1", Ok 1
                        "", Error(ParseError.expectedAtLeastOne) // Simplified error check
                        "a", Error(ParseError.expectedAtLeastOne)
                    ]

                for input, expected in cases do
                    // Note: For Error checking in tests, you might need to adjust
                    // to match your Nested error structure
                    match p (Reader.ofString input ()), expected with
                    | Ok a, Ok b -> "" |> Expect.equal a.Parsed b
                    | Error {
                                Position = _
                                Errors = Nested(actual, _)
                            },
                      Error expected -> "" |> Expect.equal actual expected
                    | Error actual, Error expected -> "" |> Expect.equal actual.Errors expected
                    | actual, exp -> failwith $"Input '{input}': Expected {exp}, got {actual}"
            }

            test "foldUserState - Modifies Context" {
                let pDigit = satisfy Char.IsDigit |>> fun c -> int (string c)

                // Folder: Increments Count, adds digit to Log
                let folder state parsedInt =
                    { state with
                        Count = state.Count + parsedInt
                        Log = (sprintf "Saw %d" parsedInt) :: state.Log
                    }

                let initialState = { Count = 0; Log = [] }
                let reader = Reader.ofString "123" initialState

                let parser = foldUserState folder pDigit
                let result = parser reader

                match result with
                | Ok _ ->
                    "State not updated" |> Expect.equal reader.State.Count 6
                    "Log incorrect" |> Expect.equal reader.State.Log [ "Saw 3"; "Saw 2"; "Saw 1" ]

                | Error e -> failwith $"Parser failed: {e}"
            }

            test "foldUserState1 - Fails on empty" {
                let folder s _ = s // No-op
                let parser = foldUserState1 folder (pitem 'a')

                let reader = Reader.ofString "b" 0 // Int state

                match parser reader with
                | Error _ -> () // Success
                | Ok _ -> failwith "Should have failed"
            }

            test "foldUserState - Infinite Loop Check" {
                let folder s _ = s
                // Empty string always succeeds without consuming
                let parser = foldUserState folder (pitem 'a' <|> preturn '!')
                let reader = Reader.ofString "abc" 0

                try
                    let _ = parser reader
                    failwith "Should have thrown"
                with
                | :? InfiniteLoopException<int> -> ()
                | ex ->
#if FABLE_COMPILER
                    failwith $"Wrong exception {ex}"
#else
                    failwith $"Wrong exception {ex.GetType()} {ex.Message}"
#endif
            }

            test "RefParser" {
                let input = "input"

                let p = RefParser(fun reader -> pseq "input" reader)
                let reader = Reader.ofString input ()
                let result = p.Parser reader

                match result with
                | Ok result ->
#if FABLE_COMPILER
                    for i in 0..4 do
                        "" |> Expect.equal (result.Parsed[i]) (input[i])
#else
                    "" |> Expect.sequenceEqual result.Parsed "input"
#endif
                    "" |> Expect.equal reader.Index 5
                | Error e -> failwithf "%A" e
            }
        ]
