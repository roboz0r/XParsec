module XParsec.FSharp.Tests.LexingTests

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

            for file in lexOnlyTestData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFile file }

            // Phase 4 smoke tests for N-dollar single-quoted interpolated strings.
            // These only succeed when the lexer is given FSharp2 config; with Legacy
            // the lexer rejects the literal and falls through to operator-then-string.
            test "FSharp2: $$\"... {{x}} ...\" lexes as level-2 interpolated" {
                let input = "$$\"hi {{x}} world\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    let expected =
                        [
                            Token.InterpolatedStringOpen
                            Token.InterpolatedStringFragment // "hi "
                            Token.InterpolatedExpressionOpen // {{
                            Token.Identifier // x
                            Token.InterpolatedExpressionClose // }}
                            Token.InterpolatedStringFragment // " world"
                            Token.InterpolatedStringClose
                            Token.EOF
                        ]

                    Expect.equal tokens expected "Token sequence"
            }

            test "Legacy: $$\"... fails to open as N-dollar (lexes $$ as operator)" {
                let input = "$$\"hi\""

                match lexStringWith LexerConfig.Legacy input with
                | Error _ -> () // legacy may reject — that's fine
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq
                    // Should NOT begin with InterpolatedStringOpen — first non-EOF token
                    // should be an operator or invalid (the legacy fallback when $$ doesn't
                    // form a valid interp prefix).
                    Expect.notEqual tokens.Head Token.InterpolatedStringOpen "Should not lex as N-dollar interp"
            }

            test "FSharp2: $$@\"... ...\" lexes as level-2 verbatim interpolated" {
                let input = "$$@\"hi {{x}} world\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let first = lexed.Tokens.[0<_>]
                    Expect.equal first.Token Token.VerbatimInterpolatedStringOpen "Opens as verb-interp"
            }

            // Phase 2 smoke tests for single-line string enforcement under FSharp2.
            // Under Legacy, the same input is accepted (newline becomes part of the fragment).
            test "FSharp2: newline inside \"...\" emits NewlineInSingleLineString" {
                let input = "\"hello\nworld\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.contains tokens Token.NewlineInSingleLineString "NewlineInSingleLineString in output"
            }

            test "Legacy: newline inside \"...\" lexes as fragment (multiline allowed)" {
                let input = "\"hello\nworld\""

                match lexStringWith LexerConfig.Legacy input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.isFalse
                        (List.contains Token.NewlineInSingleLineString tokens)
                        "No NewlineInSingleLineString in legacy"

                    Expect.contains tokens Token.StringClose "Properly terminated"
            }

            test "FSharp2: newline inside @\"...\" is rejected when verbatim flag off" {
                let input = "@\"path\nthing\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.contains tokens Token.NewlineInSingleLineString "Verbatim newline rejected"
            }

            test "FSharp2: newline inside $\"...\" emits NewlineInSingleLineString" {
                let input = "$\"hi\nthere\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.contains tokens Token.NewlineInSingleLineString "Interp newline rejected"
            }

            test "FSharp2: newline inside \"\"\"...\"\"\" is allowed (triple-quoted always multiline)" {
                let input = "\"\"\"hi\nthere\"\"\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.isFalse
                        (List.contains Token.NewlineInSingleLineString tokens)
                        "Triple-quoted newline allowed"

                    Expect.contains tokens Token.String3Close "Triple close present"
            }

            test "FSharp2: CRLF inside \"...\" is also caught" {
                let input = "\"hello\r\nworld\""

                match lexStringWith LexerConfig.FSharp2 input with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    let tokens = lexed.Tokens |> Seq.map (fun t -> t.Token) |> List.ofSeq

                    Expect.contains tokens Token.NewlineInSingleLineString "CRLF rejected"
            }
        ]
