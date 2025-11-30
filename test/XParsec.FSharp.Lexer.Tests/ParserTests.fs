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
            test "Simple Expression" {
                let path = Path.Combine(testDataDir.Value, "00_simple_expr.fs")
                testParseFile path
            }
            test "Simple Arithmetic" {
                let path = Path.Combine(testDataDir.Value, "01_simple_arithmetic.fs")
                testParseFile path
            }
            test "Simple Tuple" {
                let path = Path.Combine(testDataDir.Value, "02_simple_tuple.fs")
                testParseFile path
            }
            test "Simple List" {
                let path = Path.Combine(testDataDir.Value, "03_simple_list.fs")
                testParseFile path
            }
            test "Simple Application" {
                let path = Path.Combine(testDataDir.Value, "04_simple_application.fs")
                testParseFile path
            }
            test "Simple Array" {
                let path = Path.Combine(testDataDir.Value, "05_simple_array.fs")
                testParseFile path
            }
            test "Simple Struct Tuple" {
                let path = Path.Combine(testDataDir.Value, "06_simple_struct_tuple.fs")
                testParseFile path
            }
            test "Simple Let" {
                let path = Path.Combine(testDataDir.Value, "07_simple_let.fs")
                testParseFile path
            }
        ]
