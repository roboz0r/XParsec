module XParsec.FSharp.Tests.LexingTests

open System
open System.IO

open Expecto
open FsCheck

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing

/// The lexemes that open, close or escape a lexer context. A source drawn from these reaches
/// the string, interpolation, comment, directive and numeric contexts, at the depths and in the
/// truncations a generator over ordinary text would take astronomically long to produce.
let private lexemes =
    [|
        // String and interpolation openers, closers and escapes.
        "\""
        "\"\"\""
        "@\""
        "$\""
        "$@\""
        "@$\""
        "$\"\"\""
        "\"B"
        "\\"
        "\\n"
        "\\u0041"
        "\\x41"
        "\\065"
        "{"
        "}"
        "{{"
        "}}"
        "%d"
        "%s"
        "%*d"
        "%A"
        "%%"
        // Comments and directives.
        "(*"
        "*)"
        "//"
        "#if"
        "#else"
        "#endif"
        "#nowarn"
        // Identifiers, type parameters and literals.
        "`"
        "``"
        "'"
        "'a"
        "0x1F"
        "0b01"
        "0o7"
        "1e"
        "1.0"
        "1UL"
        "let"
        "a"
        "A"
        "1"
        "_"
        // Whitespace and delimiters.
        " "
        "\n"
        "\r\n"
        "\t"
        "("
        ")"
        "["
        "]"
        "[|"
        "|]"
        "{|"
        "|}"
        "->"
        "<-"
        "|>"
        ":"
        ";"
        ","
        "."
        "|"
        "&&"
        "!"
        "?"
        "$"
        "@"
        "^"
    |]

/// A source string concatenated from `lexemes`.
type LexerSource = | LexerSource of string

/// Generates from `lexemes` and shrinks as an ordinary string, so a counterexample reduces to
/// the shortest text that still reproduces it whether or not that text is a whole lexeme.
let private arbLexerSource =
    Arb.fromGenShrink (Gen.listOf (Gen.elements lexemes) |> Gen.map (String.concat ""), Arb.shrink)
    |> Arb.convert LexerSource (fun (LexerSource source) -> source)

/// FsCheck discovers registered generators by reflecting over public static members.
type LexerArbitraries =
    static member LexerSource() = arbLexerSource

let private lexerConfig =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<LexerArbitraries> ]
        maxTest = 2000
    }


[<Tests>]
let tests =
    testList
        "LexingTests"
        [
            testList
                "MentionedDefines"
                [
                    test "A source without #if mentions nothing" {
                        let lexed = lexString "let x = 1"

                        Expect.isEmpty lexed.MentionedDefines "No #if line, so no symbol to mention"

                        Expect.equal
                            (lexed.WithDefines(set [ "DEBUG"; "FABLE_COMPILER" ]))
                            (lexed.WithDefines Set.empty)
                            "Neither symbol appears, so neither reaches the parse"
                    }

                    test "Every symbol on a #if line is mentioned" {
                        let lexed = lexString "#if A || B && !C\n1\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A"; "B"; "C" ]) "All three are terms"
                    }

                    test "A symbol only reachable through a skipped branch is mentioned" {
                        // `#if B` sits inside a branch that is never active, so a parse-time
                        // collection would never read it.
                        let lexed = lexString "#if A\n#if B\n1\n#endif\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A"; "B" ]) "Lexing sees the whole file"

                        Expect.notEqual
                            (lexed.WithDefines(set [ "B" ]))
                            (lexed.WithDefines Set.empty)
                            "B is mentioned, so defining it is a different parse"
                    }

                    test "A symbol the file never references is dropped" {
                        let lexed = lexString "#if A\n1\n#endif\n"

                        Expect.equal
                            (lexed.WithDefines(set [ "A"; "Z" ]))
                            (lexed.WithDefines(set [ "A" ]))
                            "Z is not mentioned, so it cannot change this parse"
                    }

                    test "A trailing comment on the #if line stays comment text" {
                        let lexed = lexString "#if A //B\n1\n#endif\n"

                        Expect.equal lexed.MentionedDefines (set [ "A" ]) "B is comment text"
                    }

                    test "A #if inside a block comment stays comment text" {
                        let lexed = lexString "(*\n#if A\n*)\n"

                        Expect.isEmpty lexed.MentionedDefines "The directive's tokens are flagged in-comment"
                    }
                ]

            testList
                "Totality"
                [
                    // `lexString` returns a `Lexed` for any string, so every truncation of a
                    // source file is a token stream. A parser that must not fail on a prefix
                    // rests on this, and so does every caller that holds a `Lexed` without a
                    // failure arm.
                    test "A backslash ending an interpolated string closes it as unterminated" {
                        let expected =
                            [
                                0, Token.InterpolatedStringOpen
                                2, Token.EscapeSequence
                                3, Token.UnterminatedInterpolatedString
                                3, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"\\" expected
                    }

                    test "Every prefix of a source using each string form lexes" {
                        let source =
                            [
                                "let a = \"plain \\n \\u0041\""
                                "let b = @\"verbatim \"\" tail\""
                                "let c = \"\"\"triple \" tail\"\"\""
                                "let d = $\"{a}: %d{1} \\\" tail\""
                                "let e = $@\"{a} tail\""
                                "let f = $\"\"\"{a} tail\"\"\""
                                "let g = 'x'"
                                "let h = ``quoted name``"
                                "// %s trailing comment"
                                "(* %d block comment *)"
                                "#if A"
                                "let i = 0x1F"
                                "#endif"
                            ]
                            |> String.concat "\n"

                        for i in 0 .. source.Length do
                            let prefix = source.Substring(0, i)
                            let lexed = lexString prefix
                            Expect.equal lexed.Input prefix $"the prefix of length {i} lexed"
                    }

                    // Every prefix, because truncation is what puts EOF inside each lexer
                    // context, and a context that cannot end is how totality breaks.
                    testPropertyWithConfig lexerConfig "Every prefix of any source lexes to a stream ending at EOF"
                    <| fun (LexerSource source) ->
                        for i in 0 .. source.Length do
                            let prefix = source.Substring(0, i)
                            let lexed = lexString prefix
                            let tokens = List.ofSeq lexed.Tokens

                            Expect.equal lexed.Input prefix "the stream carries the source it was built from"

                            match tokens with
                            | [] -> failtestf "%A lexed to no tokens, not even EOF" prefix
                            | _ ->
                                let last = List.last tokens

                                Expect.equal last.TokenWithoutCommentFlags Token.EOF $"%A{prefix} ends at EOF"
                                Expect.equal (int last.StartIndex) i $"%A{prefix} puts EOF at its end"

                                Expect.isTrue
                                    (tokens
                                     |> List.pairwise
                                     |> List.forall (fun (a, b) -> a.StartIndex <= b.StartIndex))
                                    $"%A{prefix} runs its start indices forward"
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

            testList
                "EscapeDecoding"
                [
                    test "Ordinary escapes decode to their characters" {
                        let cases =
                            [
                                "\\\"", "\""
                                "\\\\", "\\"
                                "\\'", "'"
                                "\\n", "\n"
                                "\\t", "\t"
                                "\\b", "\b"
                                "\\r", "\r"
                                "\\a", "\a"
                                "\\f", "\f"
                                "\\v", "\v"
                                "\\u0041", "A"
                                "\\x41", "A"
                                "\\065", "A"
                                "\\255", "\u00FF"
                            ]

                        for raw, expected in cases do
                            Expect.equal
                                (decodeStringEscape raw)
                                (DecodedEscape.Text expected)
                                $"%A{raw} decodes to %A{expected}"
                    }

                    test "An astral \\U escape decodes to a surrogate pair" {
                        Expect.equal
                            (decodeStringEscape "\\U0001F600")
                            (DecodedEscape.Text "\U0001F600")
                            "U+1F600 is two UTF-16 code units"
                    }

                    test "A surrogate code point decodes to U+FFFD, as fsc's string context does" {
                        Expect.equal (decodeStringEscape "\\uD800") (DecodedEscape.Text "\uFFFD") "\\uD800"
                        Expect.equal (decodeStringEscape "\\U0000DFFF") (DecodedEscape.Text "\uFFFD") "\\U0000DFFF"
                    }

                    test "An unknown or truncated escape stays raw text verbatim" {
                        for raw in [ "\\q"; "\\u12"; "\\u"; "\\x2"; "\\U0001F60"; "\\12"; "\\" ] do
                            Expect.equal (decodeStringEscape raw) (DecodedEscape.Text raw) $"%A{raw} stays verbatim"
                    }

                    test "A trigraph above 255 and a \\U beyond the scalar range are refused" {
                        Expect.equal (decodeStringEscape "\\256") DecodedEscape.TrigraphOutOfRange "\\256"
                        Expect.equal (decodeStringEscape "\\999") DecodedEscape.TrigraphOutOfRange "\\999"
                        Expect.equal (decodeStringEscape "\\U00110000") DecodedEscape.NotUnicodeScalar "\\U00110000"
                        Expect.equal (decodeStringEscape "\\UFFFFFFFF") DecodedEscape.NotUnicodeScalar "\\UFFFFFFFF"
                    }

                    test "A char-literal escape keeps a lone surrogate" {
                        Expect.equal (decodeCharEscape "\\uD800") (ValueSome '\uD800') "'\\uD800' is U+D800"
                        Expect.equal (decodeCharEscape "\\n") (ValueSome '\n') "'\\n'"
                        Expect.equal (decodeCharEscape "\\q") ValueNone "'\\q' is refused by the char lexer"
                    }

                    test "A malformed long-form escape lexes as the 2-char unknown escape, keeping the close quote" {
                        // "\u12" — fsc keeps the four chars verbatim; the escape token must
                        // not swallow the closing quote.
                        let expected =
                            [
                                0, Token.StringOpen
                                1, Token.EscapeSequence
                                3, Token.StringFragment
                                5, Token.StringClose
                                6, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "\"\\u12\"" expected
                    }

                    test "An interpolated string escape is its own token" {
                        // $"\n"
                        let expected =
                            [
                                0, Token.InterpolatedStringOpen
                                2, Token.EscapeSequence
                                4, Token.InterpolatedStringClose
                                5, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"\\n\"" expected
                    }

                    test "An interpolated string escape splits the fragments around it" {
                        // $"a\tb{x}cA"
                        let expected =
                            [
                                0, Token.InterpolatedStringOpen
                                2, Token.InterpolatedStringFragment
                                3, Token.EscapeSequence
                                5, Token.InterpolatedStringFragment
                                6, Token.InterpolatedExpressionOpen
                                7, Token.Identifier
                                8, Token.InterpolatedExpressionClose
                                9, Token.InterpolatedStringFragment
                                11, Token.InterpolatedStringClose
                                12, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"a\\tb{x}cA\"" expected
                    }

                    test "A brace keeps its interpolation meaning after a backslash" {
                        // $"a\{x}b" — fsc keeps the backslash literal and opens the hole.
                        let expected =
                            [
                                0, Token.InterpolatedStringOpen
                                2, Token.InterpolatedStringFragment
                                3, Token.EscapeSequence
                                4, Token.InterpolatedExpressionOpen
                                5, Token.Identifier
                                6, Token.InterpolatedExpressionClose
                                7, Token.InterpolatedStringFragment
                                8, Token.InterpolatedStringClose
                                9, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"a\\{x}b\"" expected
                    }

                    test "A format specifier keeps its meaning after a backslash" {
                        // $"a\%d{x}"
                        let expected =
                            [
                                0, Token.InterpolatedStringOpen
                                2, Token.InterpolatedStringFragment
                                3, Token.EscapeSequence
                                4, Token.FormatPlaceholder
                                6, Token.InterpolatedExpressionOpen
                                7, Token.Identifier
                                8, Token.InterpolatedExpressionClose
                                9, Token.InterpolatedStringClose
                                10, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"a\\%d{x}\"" expected
                    }

                    test "A verbatim interpolated string keeps a backslash in its fragment" {
                        // $@"\n"
                        let expected =
                            [
                                0, Token.VerbatimInterpolatedStringOpen
                                3, Token.VerbatimInterpolatedStringFragment
                                5, Token.VerbatimInterpolatedStringClose
                                6, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$@\"\\n\"" expected
                    }

                    test "A triple-quoted interpolated string keeps a backslash in its fragment" {
                        // $"""\n"""
                        let expected =
                            [
                                0, Token.Interpolated3StringOpen
                                4, Token.Interpolated3StringFragment
                                6, Token.Interpolated3StringClose
                                9, Token.EOF
                            ]
                            |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                        testLexed "$\"\"\"\\n\"\"\"" expected
                    }
                ]

            for file in testData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFile file }

            for file in lexOnlyTestData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFile file }
        ]
