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

//let x = 1
//x

let exprsToTest =
    [
        "1"
        "x"
        "let x = 1 in x"
        """let x = 1
x"""
    ]


[<Tests>]
let tests =
    testList
        "ParserTests"
        [
            ftest "Temp test expr" {
                let source = """let x = 1
x"""

                match Lexing.lexString source with
                | Error e -> failtestf "Lexing failed: %A" e
                | Ok { Parsed = lexed } ->
                    let reader = Reader.ofLexed lexed

                    match Expr.parse reader with
                    | Error e -> failtestf "Parsing failed: %A" e
                    | Ok { Parsed = expr} ->
                        let tw = new IndentedTextWriter(new StringWriter(), "  ")
                        Debug.printExpr tw source lexed expr
                        let str = tw.InnerWriter.ToString()
                        printfn "Parsed expression:\n%s" str
            }
        ]
