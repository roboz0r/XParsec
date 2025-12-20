module XParsec.FSharp.Lexer.Tests.BlocksTests

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
        "BlocksTests"
        [
            for file in blocksTestData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" { testLexFileBlocks file }

            ptest "Temp test file" {
                let file = blocksTestData.Value |> Seq.skip 12 |> Seq.head
                let fileName = IO.Path.GetFileName file
                testLexFileBlocks file
            }
        ]
