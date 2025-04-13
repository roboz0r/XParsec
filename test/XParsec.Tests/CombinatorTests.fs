module CombinatorTests

open System
open System.Collections.Immutable

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.CharParsers
open XParsec.Parsers

module ParseErrors =
    open System.Text

    let summarize (errors: ParseError<'State, 'T>) =
        let sb = StringBuilder()

        let rec f i (errors: ParseError<'State, 'T>) =

            errors
            |> (fun x ->
#if FABLE_COMPILER
                // TODO: Fable doesn't support Append(char, int) overload
                let spaces = String.replicate (i * 2) " "
                sb.Append(spaces).Append(string x.Position.Index).Append(": ") |> ignore
#else
                sb.Append(' ', i * 2).Append(x.Position.Index).Append(": ") |> ignore
#endif
                x.Errors
                |> (function
                | ErrorType.Nested(e, es) ->
                    sb.AppendLine(sprintf "%A" e) |> ignore
                    es |> List.iter (f (i + 1))
                | m -> sb.AppendLine(sprintf " %A" m) |> ignore)
            )

        f 0 errors

#if FABLE_COMPILER
        // TODO: Fable doesn't support set_Length
        sb.ToString().TrimEnd()
#else
        let rec trimEnd (sb: StringBuilder) =
            if sb.Length > 0 && Char.IsWhiteSpace(sb.[sb.Length - 1]) then
                sb.Length <- sb.Length - 1
                trimEnd sb

        trimEnd sb
        sb.ToString()
#endif

type Expr =
    | Add of Expr * Expr
    | Mul of Expr * Expr
    | Num of int

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "CombinatorTests"
        [
            test "Bind" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "input"
                let binder = fun x -> preturn (x = "input")
                let p = p1 >>= binder
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.isTrue result.Parsed
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Use Result" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p = pstring "input" >>% true
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.isTrue result.Parsed
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Take Right" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "inp"
                let p2 = pstring "ut"
                let p = p1 >>. p2
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "ut"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Take Left" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "inp"
                let p2 = pstring "ut"
                let p = p1 .>> p2
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Take Both" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "inp"
                let p2 = pstring "ut"
                let p = p1 .>>. p2
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ("inp", "ut")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Between" {
                let input = "(input)"

                let reader = Reader.ofString input ()
                let pOpen = pchar '('
                let px = pstring "input"
                let pClose = pchar ')'
                let p = between pOpen pClose px
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 7L
                | Error e -> failwithf "%A" e
            }

            test "Map" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p = pstring "input" |>> fun x -> x.Length
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed 5
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Pipe2" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "inp"
                let p2 = pstring "ut"
                let p = pipe2 p1 p2 (fun x y -> x + y)
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Pipe3" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "in"
                let p2 = pstring "pu"
                let p3 = pstring "t"
                let p = pipe3 p1 p2 p3 (fun x y z -> x + y + z)
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Pipe4" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "i"
                let p2 = pstring "n"
                let p3 = pstring "p"
                let p4 = pstring "ut"
                let p = pipe4 p1 p2 p3 p4 (fun w x y z -> w + x + y + z)
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Pipe5" {
                let input = "input"

                let reader = Reader.ofString input ()
                let p1 = pstring "i"
                let p2 = pstring "n"
                let p3 = pstring "p"
                let p4 = pstring "u"
                let p5 = pstring "t"
                let p = pipe5 p1 p2 p3 p4 p5 (fun v w x y z -> v + w + x + y + z)
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Either" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = p1 <|> p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p = p2 <|> p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Choice" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = choice [ p1; p2 ]
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "ChoiceL" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = choiceL [ p1; p2 ] ("Expected input")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "ParseOrResult" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = p1 <|>% "false"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p = p2 <|>% "true"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "true"
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e
            }

            test "Opt(ional)" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = opt p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (ValueSome "input")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p = opt p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ValueNone
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e
            }

            test "Optional" {
                let input = "input"

                let p1 = pstring "input"
                let p2 = pstring "output"
                let p = optional p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p = optional p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e
            }

            test "NotEmpty" {
                let input = "input"

                let p1 = pstring "input"
                let p = notEmpty p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p = notEmpty (preturn ())
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (0L, ParseError.shouldConsume)
            }

            test "FollowedBy" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "ut"

                let p2Revert =
                    parser {
                        let! pos = getPosition
                        let! x = p2
                        return! setPosition pos
                    }

                let p = p1 .>> (followedBy p2Revert)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 3L
                | Error e -> failwithf "%A" e

                let p = p1 .>> (followedBy p2)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors
                    "" |> Expect.equal err (3L, ParseError.shouldNotConsume)
            }

            test "FollowedByL" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "ut"

                let p2Revert =
                    parser {
                        let! pos = getPosition
                        let! x = p2
                        return! setPosition pos
                    }

                let p = p1 .>> (followedByL p2Revert "")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 3L
                | Error e -> failwithf "%A" e

                let p = p1 .>> (followedByL p2 "Test")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors
                    "" |> Expect.equal err (3L, Message "Test")
            }

            test "NotFollowedBy" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "UT"

                let p = p1 .>> (notFollowedBy p2)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 3L
                | Error e -> failwithf "%A" e


                let p3 = pstring "ut"
                let p = p1 .>> (notFollowedBy p3)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (3L, ParseError.shouldNotSucceed)

                let p4 = pchar 'u' .>>. pchar 'X'
                let p = p1 .>> (notFollowedBy p4)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail with two messages" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""3: {ParseError.shouldFailInPlace}
  4:  {Expected 'X'}"""

                    "" |> Expect.equal msg expected
            }

            test "NotFollowedByL" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "UT"

                let p = p1 .>> (notFollowedByL p2 "")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 3L
                | Error e -> failwithf "%A" e

                let p3 = pstring "ut"
                let p = p1 .>> (notFollowedByL p3 "Test")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (3L, Message "Test")

                let p4 = pchar 'u' .>>. pchar 'X'
                let p = p1 .>> (notFollowedByL p4 "Test")
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail with two messages" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""3: {Message "Test"}
  4:  {Expected 'X'}"""

                    "" |> Expect.equal msg expected
            }

            test "LookAhead" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "ut"

                let p = p1 .>> (lookAhead p2)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "inp"
                    "" |> Expect.equal reader.Index 3L
                | Error e -> failwithf "%A" e

                let p = p1 .>> (lookAhead p1)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (3L, ExpectedSeq "inp")
            }

            test "ReplaceError" {
                let input = "input"

                let p1 = pstring "input"

                let p = p1 <?> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p2 = pstring "output"
                let p = p2 <?> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (0L, Message "Test")


                let p3 = pstring "in" .>>. pstring "X"
                let p = p3 <?> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (2L, ExpectedSeq "X")
            }

            test "ReplaceErrorNested" {
                let input = "input"

                let p1 = pstring "input"

                let p = p1 <??> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed "input"
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e

                let p2 = pstring "output"
                let p = p2 <??> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let err = e.Position.Index, e.Errors

                    "" |> Expect.equal err (0L, Message "Test")

                let p3 = pstring "in" .>>. pstring "X"
                let p = p3 <??> "Test"
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {Message "Test"}
  2:  {ExpectedSeq "X"}"""

                    "" |> Expect.equal msg expected
            }

            test "Tuple2" {
                let input = "input"

                let p1 = pstring "inp"
                let p2 = pstring "ut"

                let p = tuple2 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ("inp", "ut")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Tuple3" {
                let input = "input"

                let p1 = pstring "i"
                let p2 = pstring "n"
                let p3 = pstring "put"

                let p = tuple3 p1 p2 p3
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ("i", "n", "put")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Tuple4" {
                let input = "input"

                let p1 = pstring "i"
                let p2 = pstring "n"
                let p3 = pstring "p"
                let p4 = pstring "ut"

                let p = tuple4 p1 p2 p3 p4
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ("i", "n", "p", "ut")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "Tuple5" {
                let input = "input"

                let p1 = pstring "i"
                let p2 = pstring "n"
                let p3 = pstring "p"
                let p4 = pstring "u"
                let p5 = pstring "t"

                let p = tuple5 p1 p2 p3 p4 p5
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ("i", "n", "p", "u", "t")
                    "" |> Expect.equal reader.Index 5L
                | Error e -> failwithf "%A" e
            }

            test "ParseArray" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = parray 3 p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        (imm {
                            "input"
                            "input"
                            "input"
                        })

                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e
            }

            test "SkipArray" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = skipArray 3 p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e
            }

            test "Many" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = many p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        (imm {
                            "input"
                            "input"
                            "input"
                        })

                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (ImmutableArray.Empty)
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = many p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "Many1" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = many1 p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        (imm {
                            "input"
                            "input"
                            "input"
                        })

                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = many1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipMany" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = skipMany p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = skipMany p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipMany1" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p = skipMany1 p1
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 15L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = skipMany1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SepBy" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = sepBy p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        ([| "input"; "input"; "input" |].ToImmutableArray(), ImmutableArray.CreateRange([| ','; ',' |]))

                    "" |> Expect.equal reader.Index 17L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (ImmutableArray.Empty, ImmutableArray.Empty)
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = sepBy p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SepBy1" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = sepBy1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        ([| "input"; "input"; "input" |].ToImmutableArray(), ImmutableArray.CreateRange([| ','; ',' |]))

                    "" |> Expect.equal reader.Index 17L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = sepBy1 p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipSepBy" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = skipSepBy p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 17L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = skipSepBy p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipSepBy1" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = skipSepBy1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 17L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = skipSepBy1 p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SepEndBy" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = sepEndBy p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        ([| "input"; "input"; "input" |].ToImmutableArray(),
                         ImmutableArray.CreateRange([| ','; ','; ',' |]))

                    "" |> Expect.equal reader.Index 18L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (ImmutableArray.Empty, ImmutableArray.Empty)
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = sepEndBy p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SepEndBy1" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = sepEndBy1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal
                        result.Parsed
                        ([| "input"; "input"; "input" |].ToImmutableArray(),
                         ImmutableArray.CreateRange([| ','; ','; ',' |]))

                    "" |> Expect.equal reader.Index 18L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = sepEndBy1 p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipSepEndBy" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = skipSepEndBy p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 18L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = skipSepEndBy p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipSepEndBy1" {
                let input = "input,input,input,X"

                let p1 = pstring "input"
                let p2 = pchar ','
                let p = skipSepEndBy1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 18L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = skipSepEndBy1 p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "ManyTill" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p2 = pstring "X"
                let p = manyTill p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal result.Parsed ([| "input"; "input"; "input" |].ToImmutableArray(), "X")

                    "" |> Expect.equal reader.Index 16L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (ImmutableArray.Empty, "X")
                    "" |> Expect.equal reader.Index 1L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = manyTill p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "Many1Till" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p2 = pstring "X"
                let p = many1Till p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal result.Parsed ([| "input"; "input"; "input" |].ToImmutableArray(), "X")

                    "" |> Expect.equal reader.Index 16L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = many1Till p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipManyTill" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p2 = pstring "X"
                let p = skipManyTill p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 16L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 1L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p = skipManyTill p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "SkipMany1Till" {
                let input = "inputinputinputX"

                let p1 = pstring "input"
                let p2 = pstring "X"
                let p = skipMany1Till p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed ()
                    "" |> Expect.equal reader.Index 16L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = skipMany1Till p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "ChainL1" {
                let input = "1+2+3+4"
                let p1 = digit |>> (string >> Int32.Parse >> Num)
                let p2 = pchar '+' |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                // let p2 = pchar '+' >>= (fun _ -> preturn (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainl1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Add(Add(Add(Num 1, Num 2), Num 3), Num 4))
                    "" |> Expect.equal reader.Index 7L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p2 = lookAhead (pchar '1') |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainl1 p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "ChainL" {
                let input = "1+2+3+4"
                let p1 = digit |>> (string >> Int32.Parse >> Num)
                let p2 = pchar '+' |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainl p1 p2 (Num 0)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Add(Add(Add(Num 1, Num 2), Num 3), Num 4))
                    "" |> Expect.equal reader.Index 7L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Num 0)
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p2 = lookAhead (pchar '1') |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainl p1 p2 (Num 0)
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "ChainR1" {
                let input = "1+2+3+4"
                let p1 = digit |>> (string >> Int32.Parse >> Num)
                let p2 = pchar '+' |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainr1 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Add(Num 1, Add(Num 2, Add(Num 3, Num 4))))
                    "" |> Expect.equal reader.Index 7L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p2 = lookAhead (pchar '1') |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainr1 p1 p2
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "ChainR" {
                let input = "1+2+3+4"
                let p1 = digit |>> (string >> Int32.Parse >> Num)
                let p2 = pchar '+' |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainr p1 p2 (Num 0)
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Add(Num 1, Add(Num 2, Add(Num 3, Num 4))))
                    "" |> Expect.equal reader.Index 7L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed (Num 0)
                    "" |> Expect.equal reader.Index 0L
                | Error e -> failwithf "%A" e

                let p1 = lookAhead p1
                let p2 = lookAhead (pchar '1') |>> (fun _ -> (fun expr1 expr2 -> Add(expr1, expr2)))
                let p = chainr p1 p2 (Num 0)
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }

            test "Many1Items2" {
                let input = "input1input2input2X"

                let p1 = pstring "input1"
                let p2 = pstring "input2"
                let p = many1Items2 p1 p2
                let reader = Reader.ofString input ()
                let result = p reader

                match result with
                | Ok result ->
                    ""
                    |> Expect.equal result.Parsed ([| "input1"; "input2"; "input2" |].ToImmutableArray())

                    "" |> Expect.equal reader.Index 18L
                | Error e -> failwithf "%A" e

                let reader = Reader.ofString "X" ()
                let result = p reader

                match result with
                | Ok result -> "Parser should fail" |> Expect.isFalse true
                | Error e ->
                    let msg = ParseErrors.summarize e

                    let expected =
                        $"""0: {ParseError.expectedAtLeastOne}
  0:  {ExpectedSeq "input1"}"""

                    "" |> Expect.equal msg expected

                let p1 = lookAhead p1
                let p = many1Items2 p1 p1
                let reader = Reader.ofString input ()
#if FABLE_COMPILER
                "Inf Loop" |> Expect.throws (fun () -> p reader |> ignore)
#else
                "Inf Loop"
                |> Expect.throwsT<InfiniteLoopException<unit>> (fun () -> p reader |> ignore)
#endif
            }
        ]
