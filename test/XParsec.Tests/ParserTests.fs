module ParserTests

open System
open System.Collections.Immutable

open Expecto

open XParsec
open XParsec.Parsers

[<Tests>]
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
                    "" |> Expect.equal reader.Index 0L
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
                    "" |> Expect.equal e.Position.Index 0L
                    "" |> Expect.equal e.Errors ParseError.zero
                    "" |> Expect.equal reader.Index 0L
            }

            test "UserState" {
                let input = "input"

                let p =
                    parser {
                        let! state = getUserState
                        do! setUserState "state"

                        do!
                            updateUserState (function
                                | "state" -> "STATE"
                                | _ -> failwith "Invalid state")

                        do! userStateSatisfies (fun s -> s = "STATE")
                        return state
                    }

                let reader = Reader.ofString input "state"
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "state"
                    "" |> Expect.equal reader.Index 0L
                    "" |> Expect.equal reader.State "STATE"
                | Error e -> failwithf "%A" e
            }

            test "EoF" {
                let input = ""

                let p = eof
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "" |> Expect.equal reader.Index 0L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 2L
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
                    "" |> Expect.equal reader.Index 2L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 1L
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
                    "" |> Expect.equal reader.Index 2L
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
                    "" |> Expect.equal reader.Index 2L
                | Error e -> failwithf "%A" e
            }

            test "PSeq" {
                let input = "input"

                let p = pseq "input"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (imm { yield! "input" })
                    "" |> Expect.equal reader.Index 5L
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
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "RefParser" {
                let input = "input"

                let p = RefParser(fun reader -> pseq "input" reader)
                let reader = Reader.ofString input ()
                let result = p.Parser reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (imm { yield! "input" })
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }
        ]
