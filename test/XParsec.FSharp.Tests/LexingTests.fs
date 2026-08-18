module XParsec.FSharp.Tests.LexingTests

open System
open System.IO

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing


let private lexOrFail (source: string) =
    match lexString source with
    | Ok lexed -> lexed
    | Error e -> failtestf "Lexing failed: %A" e

[<Tests>]
let tests =
    testList
        "LexingTests"
        [
            testList
                "MentionedDefines"
                [
                    test "A source without #if mentions nothing" {
                        let lexed = lexOrFail "let x = 1"

                        Expect.isEmpty lexed.MentionedDefines "No #if line, so no symbol to mention"

                        Expect.equal
                            (lexed.WithDefines(set [ "DEBUG"; "FABLE_COMPILER" ]))
                            (lexed.WithDefines Set.empty)
                            "Neither symbol appears, so neither reaches the parse"
                    }

                    test "Every symbol on a #if line is mentioned" {
                        let lexed = lexOrFail "#if A || B && !C\n1\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A"; "B"; "C" ]) "All three are terms"
                    }

                    test "A symbol only reachable through a skipped branch is mentioned" {
                        // `#if B` sits inside a branch that is never active, so a parse-time
                        // collection would never read it.
                        let lexed = lexOrFail "#if A\n#if B\n1\n#endif\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A"; "B" ]) "Lexing sees the whole file"

                        Expect.notEqual
                            (lexed.WithDefines(set [ "B" ]))
                            (lexed.WithDefines Set.empty)
                            "B is mentioned, so defining it is a different parse"
                    }

                    test "A symbol the file never references is dropped" {
                        let lexed = lexOrFail "#if A\n1\n#endif\n"

                        Expect.equal
                            (lexed.WithDefines(set [ "A"; "Z" ]))
                            (lexed.WithDefines(set [ "A" ]))
                            "Z is not mentioned, so it cannot change this parse"
                    }

                    test "A trailing comment on the #if line stays comment text" {
                        let lexed = lexOrFail "#if A //B\n1\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A" ]) "B is comment text"
                    }

                    test "A #if inside a block comment stays comment text" {
                        let lexed = lexOrFail "(*\n#if A\n*)\n"

                        Expect.isEmpty lexed.MentionedDefines "The directive's tokens are flagged in-comment"
                    }
                ]

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

            for file in lexOnlyTestData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFile file }
        ]
