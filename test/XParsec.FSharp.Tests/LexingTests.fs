module XParsec.FSharp.Lexer.Tests.LexingTests

open System
open System.IO

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing


[<Tests>]
let tests =
    testList
        "LexingTests"
        [
            test "Index" {

                let snippet =
                    [ "module Test"; "let html s = s"; "html $\"<p>Hello, World!</p>\"" ]
                    |> String.concat "\n"

                let expected =
                    [
                        0, Token.KWModule
                        6, Token.Whitespace
                        7, Token.Identifier
                        11, Token.Newline
                        12, Token.KWLet
                        15, Token.Whitespace
                        16, Token.Identifier
                        20, Token.Whitespace
                        21, Token.Identifier
                        22, Token.Whitespace
                        23, Token.OpEquality
                        24, Token.Whitespace
                        25, Token.Identifier
                        26, Token.Newline
                        27, Token.Identifier
                        31, Token.Whitespace
                        32, Token.InterpolatedStringOpen
                        34, Token.InterpolatedStringFragment
                        54, Token.InterpolatedStringClose
                        55, Token.EOF
                    ]
                    |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                testLexed snippet expected
            }

            for file in testData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFile file }

            ptest "Temp test file" {
                let file = testData.Value |> Seq.skip 0 |> Seq.head
                let fileName = IO.Path.GetFileName file
                testLexFile file
            }
            ptest "Temp test" {
                let fileName = "03_simple_triple_dollar.fs"
                let file = IO.Path.Combine(testDataDir.Value, fileName)
                let snippet = File.ReadAllText file
                let snippet = snippet.Replace("\r\n", "\n")
                printfn "Lexing\n%s\n-----\n%s\n-----" fileName snippet

                match lexString snippet with
                | Ok lexed ->
                    printLexed snippet lexed
                    let expectedFile = file + ".lexed"
                    let expected = readLexed expectedFile

                    "Lexed tokens should match expected"
                    |> Expect.equal (lexed.Tokens |> List.ofSeq) expected

                | Error err ->
                    let lexed = LexBuilder.complete err.Position.Index err.Position.State
                    printLexed snippet lexed
                    printfn "-----"
                    let s = XParsec.ErrorFormatting.formatStringError snippet err

                    printfn "%s" s
                    failwith "Lexing failed"
            }
        ]
