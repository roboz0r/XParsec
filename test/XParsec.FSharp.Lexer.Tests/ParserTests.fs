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

[<Tests>]
let tests =
    testList
        "ParserTests"
        [
            testCase
                "Simple Expression"
                (fun () ->
                    let path = Path.Combine(testDataDir.Value, "00_simple_expr.fs")
                    testParseFile path
                )
            testCase
                "Simple Arithmetic"
                (fun () ->
                    let path = Path.Combine(testDataDir.Value, "01_simple_arithmetic.fs")
                    testParseFile path
                )
        ]
