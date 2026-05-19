/// Tests for TOML string parsing.
/// Covers basic strings, literal strings, multiline variants, and escape sequences.
module XParsec.Toml.Tests.StringTests

open Expecto
open XParsec.Toml

[<Tests>]
let basicStringTests =
    testList
        "Basic Strings"
        [
            test "empty basic string" {
                let input = """key = "" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "") "Should be empty string"
            }

            test "simple basic string" {
                let input = """key = "hello world" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "hello world") "Should match"
            }

            test "escape sequences" {
                let input = """key = "tab:\there\nnewline" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "tab:\there\nnewline") "Should have escapes"
            }

            test "backslash escape" {
                let input = """key = "path\\to\\file" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "path\\to\\file") "Should have backslashes"
            }

            test "quote escape" {
                let input = """key = "say \"hello\"" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "say \"hello\"") "Should have quotes"
            }

            test "unicode escape 4 digit" {
                let input = """key = "\u0048\u0065\u006C\u006C\u006F" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "Hello") "Should decode unicode"
            }
        ]

[<Tests>]
let literalStringTests =
    testList
        "Literal Strings"
        [
            test "empty literal string" {
                let input = """key = '' """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "") "Should be empty string"
            }

            test "simple literal string" {
                let input = """key = 'hello world' """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "hello world") "Should match"
            }

            test "literal string preserves backslashes" {
                let input = """key = 'C:\path\to\file' """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "C:\\path\\to\\file") "Should preserve backslashes"
            }

            test "literal string with quotes inside" {
                let input = """key = 'say "hello"' """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "say \"hello\"") "Should preserve quotes"
            }
        ]

[<Tests>]
let multilineBasicStringTests =
    testList
        "Multiline Basic Strings"
        [
            test "simple multiline" {
                let input = "key = \"\"\"\nhello\nworld\"\"\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "hello\nworld") "Should have newline"
            }

            test "multiline trims first newline" {
                let input = "key = \"\"\"\nline1\nline2\"\"\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "line1\nline2") "First newline trimmed"
            }

            test "line ending backslash" {
                let input = "key = \"\"\"\nhello \\\n    world\"\"\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "hello world") "Should join lines"
            }
        ]

[<Tests>]
let multilineLiteralStringTests =
    testList
        "Multiline Literal Strings"
        [
            test "simple multiline literal" {
                let input = "key = '''\nhello\nworld'''"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "hello\nworld") "Should have newline"
            }

            test "preserves backslashes" {
                let input = "key = '''\nC:\\path\\file'''"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "C:\\path\\file") "Should preserve"
            }
        ]
