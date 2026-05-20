module XParsec.FSharp.Tests.StringValuesTests

open System.Collections.Immutable

open Expecto

open XParsec.FSharp.Parser

let private getText (s: string) = s
let private toParts (xs: StringPart<string> list) = ImmutableArray.CreateRange xs

let private evalPlain (parts: StringPart<string> list) (config: EvaluateConfig) =
    StringValues.tryEvaluate getText "" 0 config (StringKind.String "") (toParts parts)

let private evalVerbatim (parts: StringPart<string> list) (config: EvaluateConfig) =
    StringValues.tryEvaluate getText "" 0 config (StringKind.VerbatimString "") (toParts parts)

let private evalTriple
    (source: string)
    (closeStartIndex: int)
    (parts: StringPart<string> list)
    (config: EvaluateConfig)
    =
    StringValues.tryEvaluate getText source closeStartIndex config (StringKind.String3 "") (toParts parts)

[<Tests>]
let tests =
    testList
        "StringValues"
        [
            test "plain string concatenates fragments" {
                let parts = [ StringPart.Text "hello "; StringPart.Text "world" ]
                let r = evalPlain parts EvaluateConfig.Legacy
                Expect.equal r (Ok "hello world") "Concat"
            }

            test "plain string cooks \\n escape" {
                let parts =
                    [ StringPart.Text "a"; StringPart.EscapeSequence "\\n"; StringPart.Text "b" ]

                let r = evalPlain parts EvaluateConfig.Legacy
                Expect.equal r (Ok "a\nb") "Newline escape"
            }

            test "plain string cooks \\uXXXX escape" {
                let parts = [ StringPart.EscapeSequence "\\u00e9" ]
                let r = evalPlain parts EvaluateConfig.Legacy
                Expect.equal r (Ok "é") "Unicode hex 4"
            }

            test "plain string cooks \\xHH escape" {
                let parts = [ StringPart.EscapeSequence "\\x41" ]
                let r = evalPlain parts EvaluateConfig.Legacy
                Expect.equal r (Ok "A") "Hex byte"
            }

            test "plain string cooks decimal trigraph" {
                let parts = [ StringPart.EscapeSequence "\\065" ]
                let r = evalPlain parts EvaluateConfig.Legacy
                Expect.equal r (Ok "A") "Decimal trigraph"
            }

            test "verbatim escape quote cooks to single quote" {
                let parts =
                    [
                        StringPart.Text "say "
                        StringPart.VerbatimEscapeQuote "\"\""
                        StringPart.Text "hi"
                        StringPart.VerbatimEscapeQuote "\"\""
                    ]

                let r = evalVerbatim parts EvaluateConfig.Legacy
                Expect.equal r (Ok "say \"hi\"") "Verbatim quote"
            }

            test "newline normalization rewrites source CRLF to LF" {
                let parts = [ StringPart.Text "a\r\nb\r\nc" ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        NewlineNormalization = NewlineMode.Lf
                    }

                let r = evalPlain parts cfg
                Expect.equal r (Ok "a\nb\nc") "CRLF to LF"
            }

            test "newline normalization leaves \\r escape alone" {
                let parts = [ StringPart.EscapeSequence "\\r" ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        NewlineNormalization = NewlineMode.Lf
                    }

                let r = evalPlain parts cfg
                Expect.equal r (Ok "\r") "Escape \\r unchanged"
            }

            test "triple-quote off concatenates verbatim" {
                let parts = [ StringPart.Text "\n  hello\n  world\n  " ]
                let r = evalTriple "" 0 parts EvaluateConfig.Legacy
                Expect.equal r (Ok "\n  hello\n  world\n  ") "Off mode passthrough"
            }

            test "triple-quote dedent strips margin (2-space)" {
                // Source: """[\n]  hello[\n]  world[\n]  """
                // Closing delimiter is at column 2 (after 2 spaces).
                let source = "\"\"\"\n  hello\n  world\n  \"\"\""
                // close-start = index of the opening " of the closing """
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\n  hello\n  world\n  " ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
                    }

                let r = evalTriple source closeIdx parts cfg
                Expect.equal r (Ok "hello\nworld") "Dedent 2-space"
            }

            test "triple-quote dedent strips margin (4-space, multiple lines)" {
                let source = "\"\"\"\n    line1\n    line2\n    line3\n    \"\"\""
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\n    line1\n    line2\n    line3\n    " ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
                    }

                let r = evalTriple source closeIdx parts cfg
                Expect.equal r (Ok "line1\nline2\nline3") "Dedent 4-space"
            }

            test "triple-quote dedent reports underindented content line" {
                let source = "\"\"\"\n    line1\n  bad\n    line3\n    \"\"\""
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\n    line1\n  bad\n    line3\n    " ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
                    }

                let r = evalTriple source closeIdx parts cfg

                match r with
                | Error(StringValueError.ContentLineUnderindented _) -> ()
                | other -> failtestf "Expected ContentLineUnderindented, got %A" other
            }

            test "triple-quote dedent rejects mixed tabs and spaces in margin" {
                // Margin = "\t " — mix of tab + space
                let source = "\"\"\"\n\t hello\n\t \"\"\""
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\n\t hello\n\t " ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
                    }

                let r = evalTriple source closeIdx parts cfg

                match r with
                | Error(StringValueError.MixedIndentInClosingDelimiter _) -> ()
                | other -> failtestf "Expected MixedIndentInClosingDelimiter, got %A" other
            }

            test "triple-quote dedent + LF normalization composes" {
                // Source has CRLF; dedent + normalize together.
                let source = "\"\"\"\r\n  hi\r\n  \"\"\""
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\r\n  hi\r\n  " ]
                let cfg = EvaluateConfig.FSharp2
                let r = evalTriple source closeIdx parts cfg
                Expect.equal r (Ok "hi") "Dedent + LF"
            }

            test "triple-quote dedent: zero margin is no-op" {
                let source = "\"\"\"\nhi\n\"\"\""
                let closeIdx = source.LastIndexOf("\"\"\"")
                let parts = [ StringPart.Text "\nhi\n" ]

                let cfg =
                    { EvaluateConfig.Legacy with
                        TripleQuoteIndent = TripleQuoteIndentMode.CSharpStyle
                    }

                let r = evalTriple source closeIdx parts cfg
                Expect.equal r (Ok "hi") "Zero-margin strips brackets only"
            }

            test "interpolated string returns empty (caller must walk parts)" {
                let kind = StringKind.InterpolatedString ""
                let parts = ImmutableArray.CreateRange [ StringPart.Text "abc" ]
                let r = StringValues.tryEvaluate getText "" 0 EvaluateConfig.Legacy kind parts
                Expect.equal r (Ok "") "Interpolated yields empty"
            }

            // End-to-end: lex a real triple-quoted F# source under FSharp2 config,
            // then use StringValues to derive the cooked value.
            test "end-to-end: lex + StringValues.evaluate FSharp2 dedent" {
                let source = "let x = \"\"\"\n    hello\n    world\n    \"\"\""

                match XParsec.FSharp.Lexer.Lexing.lexStringWith XParsec.FSharp.Lexer.LexerConfig.FSharp2 source with
                | Error e -> failtestf "Lex failed: %A" e
                | Ok lexed ->
                    // Find the String3Open and String3Close, gather fragments between.
                    let tokens = lexed.Tokens
                    let mutable openIdx = -1
                    let mutable closeIdx = -1

                    for i in 0 .. tokens.Length - 1 do
                        let t = tokens.[i * 1<XParsec.FSharp.Lexer.token>]

                        if openIdx = -1 && t.Token = XParsec.FSharp.Lexer.Token.String3Open then
                            openIdx <- i
                        elif
                            openIdx >= 0
                            && closeIdx = -1
                            && t.Token = XParsec.FSharp.Lexer.Token.String3Close
                        then
                            closeIdx <- i

                    Expect.notEqual openIdx -1 "Found String3Open"
                    Expect.notEqual closeIdx -1 "Found String3Close"

                    // Concatenate fragment text between open and close.
                    let getTokText (i: int) =
                        lexed.GetTokenString(i * 1<XParsec.FSharp.Lexer.token>, source)

                    let parts =
                        [
                            for i in (openIdx + 1) .. (closeIdx - 1) do
                                let t = tokens.[i * 1<XParsec.FSharp.Lexer.token>]

                                if t.Token = XParsec.FSharp.Lexer.Token.StringFragment then
                                    yield StringPart.Text(getTokText i)
                        ]

                    let closeStartIndex =
                        int tokens.[closeIdx * 1<XParsec.FSharp.Lexer.token>].StartIndex

                    let r =
                        StringValues.tryEvaluate
                            id
                            source
                            closeStartIndex
                            EvaluateConfig.FSharp2
                            (StringKind.String3 "")
                            (ImmutableArray.CreateRange parts)

                    Expect.equal r (Ok "hello\nworld") "Dedent + cook"
            }
        ]
