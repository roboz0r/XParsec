module XParsec.FSharp.Lexer.Tests.ParserTests

open System
open System.IO
open System.CodeDom.Compiler

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Files (relative to testDataDir) that the parser can handle, together with any
/// preprocessor symbols that must be defined.  Files that require features not yet
/// implemented (e.g. interpolated strings) are intentionally omitted.
let private parseTestFiles: (string * Set<string>) list =
    [
        "00_simple_expr.fs", Set.empty
        "01_simple_arithmetic.fs", Set.empty
        "02_simple_tuple.fs", Set.empty
        "03_simple_list.fs", Set.empty
        "04_simple_application.fs", Set.empty
        "05_simple_array.fs", Set.empty
        "06_simple_struct_tuple.fs", Set.empty
        "07_simple_let.fs", Set.empty
        "08_simple_begin_end.fs", Set.empty
        "09_simple_long_ident.fs", Set.empty
        "10_simple_typed_ident.fs", Set.empty
        "11_simple_if.fs", Set.empty
        "24_if_directive.fs", Set.empty
        "24_if_directive.fs", Set.ofList [ "A" ]
        "25_implicit_in.fs", Set.empty
        "26_sequential.fs", Set.empty
        "27_begin_end_seq.fs", Set.empty
        "28_nested_lets.fs", Set.empty
        "29_let_body_seq.fs", Set.empty
        "30_let_in_body_seq.fs", Set.empty
        "31_if_then_else_seq.fs", Set.empty
        "32_match_seq.fs", Set.empty
        "33_fun_expr.fs", Set.empty
        "34_try_with.fs", Set.empty
        "35_try_finally.fs", Set.empty
        "36_while.fs", Set.empty
        "37_for_to.fs", Set.empty
        "38_for_in.fs", Set.empty
        "39_use_expr.fs", Set.empty
    ]

[<Tests>]
let tests =
    testList
        "ParserTests"
        [
            for (fileName, symbols) in parseTestFiles do
                let path = Path.Combine(testDataDir.Value, fileName)

                let name =
                    if symbols.IsEmpty then
                        $"Parsing {fileName}"
                    else
                        let syms = symbols |> String.concat ","
                        $"Parsing {fileName} ({syms})"

                test name { testParseFileWith symbols path }

            ptest "Temp test file" {
                // Note: this test is intended for quick iteration during development.
                // It will be skipped by default, but you can temporarily change `ptest` to `ftest` to run it.
                let fileName = "34_try_with.fs"
                let path = Path.Combine(testDataDir.Value, fileName)

                try
                    testLexFile path
                with ex ->
                    printfn "Lexing failed: %s" ex.Message
                    testParseFile path

                testParseFile path
            }
        ]
