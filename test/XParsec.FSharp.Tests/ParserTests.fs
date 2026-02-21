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
            test "Simple Begin-End Block" {
                let path = Path.Combine(testDataDir.Value, "08_simple_begin_end.fs")
                testParseFile path
            }
            test "Simple Long Ident" {
                let path = Path.Combine(testDataDir.Value, "09_simple_long_ident.fs")
                testParseFile path
            }
            test "Simple Typed Ident" {
                let path = Path.Combine(testDataDir.Value, "10_simple_typed_ident.fs")
                testParseFile path
            }
            test "Simple If Expression" {
                let path = Path.Combine(testDataDir.Value, "11_simple_if.fs")
                testParseFile path
            }
            test "If Directives (inactive branch)" {
                let path = Path.Combine(testDataDir.Value, "24_if_directive.fs")
                testParseFile path
            }

            test "If Directives (active branch)" {
                let path = Path.Combine(testDataDir.Value, "24_if_directive.fs")
                testParseFileWithSymbols [ "A" ] path
            }
            test "Implicit In" {
                let path = Path.Combine(testDataDir.Value, "25_implicit_in.fs")
                testParseFile path
            }
            test "Sequential" {
                let path = Path.Combine(testDataDir.Value, "26_sequential.fs")
                testParseFile path
            }
            test "Begin-End Sequential" {
                let path = Path.Combine(testDataDir.Value, "27_begin_end_seq.fs")
                testParseFile path
            }
            test "Nested Lets" {
                let path = Path.Combine(testDataDir.Value, "28_nested_lets.fs")
                testParseFile path
            }
            test "Let Body Sequential" {
                let path = Path.Combine(testDataDir.Value, "29_let_body_seq.fs")
                testParseFile path
            }
        ]
