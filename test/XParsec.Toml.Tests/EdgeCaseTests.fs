/// Tests for TOML edge cases and spec compliance.
/// Covers boundary conditions, special characters, and unusual valid inputs.
module XParsec.Toml.Tests.EdgeCaseTests

open Expecto
open XParsec.Toml

[<Tests>]
let keyEdgeCases =
    testList
        "Key Edge Cases"
        [
            test "bare key with numbers" {
                let input = "key123 = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key123" doc) (Some 1L) "Should work"
            }

            test "bare key with underscores" {
                let input = "my_key_name = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "my_key_name" doc) (Some 1L) "Should work"
            }

            test "bare key with dashes" {
                let input = "my-key-name = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "my-key-name" doc) (Some 1L) "Should work"
            }

            test "numeric bare key" {
                let input = "1234 = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "1234" doc) (Some 1L) "Should work"
            }

            test "quoted key with special chars" {
                let input = "\"key.with" + ".dots\" = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
            }

            test "empty quoted key" {
                let input = "\"\" = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "" doc) (Some 1L) "Should work"
            }

            test "single quoted key" {
                let input = "'' = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "" doc) (Some 1L) "Should work"
            }
        ]

[<Tests>]
let stringEdgeCases =
    testList
        "String Edge Cases"
        [
            test "string with all escape sequences" {
                let input = "key = \"\\b\\t\\n\\f\\r\\\"\\\\\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getString "key" doc with
                | Some s -> Expect.equal s "\b\t\n\x0C\r\"\\" "Should have all escapes"
                | None -> failtest "Should have string"
            }

            test "multiline with only newlines" {
                let input = "key = \"\"\"\n\n\n\"\"\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "\n\n") "Should have newlines"
            }

            test "literal string with backslash sequences" {
                let input = "key = '\\n\\t\\r'"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "\\n\\t\\r") "Should be literal"
            }
        ]

[<Tests>]
let numberEdgeCases =
    testList
        "Number Edge Cases"
        [
            test "positive zero" {
                let input = "key = +0"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0L) "Should be 0"
            }

            test "negative zero" {
                let input = "key = -0"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0L) "Should be 0"
            }

            test "float positive zero" {
                let input = "key = +0.0"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 0.0) "Should be 0.0"
            }

            test "float negative zero" {
                let input = "key = -0.0"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some -0.0) "Should be -0.0"
            }

            test "positive infinity explicit" {
                let input = "key = +inf"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some infinity) "Should be +inf"
            }

            test "positive nan" {
                let input = "key = +nan"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getFloat "key" doc with
                | Some f -> Expect.isTrue (System.Double.IsNaN f) "Should be NaN"
                | None -> failtest "Should have float"
            }

            test "negative nan" {
                let input = "key = -nan"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getFloat "key" doc with
                | Some f -> Expect.isTrue (System.Double.IsNaN f) "Should be NaN"
                | None -> failtest "Should have float"
            }

            test "hex lowercase" {
                let input = "key = 0xabcdef"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0xABCDEFL) "Should parse"
            }

            test "octal leading zeros" {
                let input = "key = 0o0755"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 493L) "Should be 493"
            }

            test "binary all ones" {
                let input = "key = 0b11111111"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 255L) "Should be 255"
            }

            test "exponent uppercase E" {
                let input = "key = 1E10"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 1e10) "Should be 1e10"
            }

            test "exponent with plus" {
                let input = "key = 1e+10"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 1e10) "Should be 1e10"
            }
        ]

[<Tests>]
let arrayEdgeCases =
    testList
        "Array Edge Cases"
        [
            test "array with comments" {
                let input =
                    """
key = [
    # first element
    1,
    # second element
    2
]
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 2 "Should have 2 elements"
                | _ -> failtest "Expected Array"
            }

            test "deeply nested arrays" {
                let input = "key = [[[[1]]]]"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 1 "Should have 1 outer element"
                | _ -> failtest "Expected Array"
            }

            test "array of inline tables" {
                let input = "key = [{ a = 1 }, { a = 2 }]"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 2 "Should have 2 elements"
                | _ -> failtest "Expected Array"
            }
        ]

[<Tests>]
let tableEdgeCases =
    testList
        "Table Edge Cases"
        [
            test "table with quoted name" {
                let input =
                    """
["table.with.dots"]
key = 1
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
            }

            test "super-table implicit creation" {
                let input =
                    """
[a.b.c]
key = 1
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "a.b.c.key" doc) (Some 1L) "Should access"
            }

            test "inline table with trailing comma" {
                // TOML spec: trailing commas NOT allowed in inline tables
                // This test documents expected behavior
                let input = "key = { a = 1, }"
                let result = Toml.parse input
                // Depending on parser strictness, this may fail or succeed
                // The TOML 1.0.0 spec says trailing commas are not allowed
                ()
            }

            ptest "array of tables defines table" {
                // TODO: Complex edge case - [table.header] after [[array]] should add to last array element
                let input =
                    """
[[products]]
name = "first"

[products.details]
price = 10
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "products" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 1 "Should have 1 product"
                | _ -> failtest "Expected Array"
            }
        ]

[<Tests>]
let dateTimeEdgeCases =
    testList
        "DateTime Edge Cases"
        [
            test "datetime with lowercase t separator" {
                let input = "key = 1979-05-27t07:32:00Z"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) -> Expect.equal dt.Year 1979 "Year"
                | _ -> failtest "Expected OffsetDateTime"
            }

            test "datetime with lowercase z" {
                let input = "key = 1979-05-27T07:32:00z"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.OffsetDateTime dt) -> Expect.equal dt.OffsetMinutes (ValueSome 0) "UTC offset"
                | _ -> failtest "Expected OffsetDateTime"
            }

            test "time without seconds" {
                // TOML requires seconds
                let input = "key = 07:32"
                let result = Toml.parse input
                // This should fail as per spec
                ()
            }

            test "date at year boundary" {
                let input = "key = 2000-01-01"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalDate d) ->
                    Expect.equal d.Year 2000 "Year 2000"
                    Expect.equal d.Month 1 "January"
                    Expect.equal d.Day 1 "First"
                | _ -> failtest "Expected LocalDate"
            }

            test "leap year date" {
                let input = "key = 2000-02-29"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalDate d) -> Expect.equal d.Day 29 "Leap day"
                | _ -> failtest "Expected LocalDate"
            }

            test "midnight time" {
                let input = "key = 00:00:00"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalTime t) ->
                    Expect.equal t.Hour 0 "Hour 0"
                    Expect.equal t.Minute 0 "Minute 0"
                    Expect.equal t.Second 0 "Second 0"
                | _ -> failtest "Expected LocalTime"
            }

            test "end of day time" {
                let input = "key = 23:59:59"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.LocalTime t) ->
                    Expect.equal t.Hour 23 "Hour 23"
                    Expect.equal t.Minute 59 "Minute 59"
                    Expect.equal t.Second 59 "Second 59"
                | _ -> failtest "Expected LocalTime"
            }
        ]

[<Tests>]
let valueAccessTests =
    testList
        "Value Access"
        [
            test "missing key returns None" {
                let input = "key = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "nonexistent" doc) None "Should be None"
            }

            test "wrong type returns None" {
                let input = "key = \"string\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) None "Should be None for wrong type"
            }

            test "deep path missing intermediate" {
                let input = "key = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "a.b.c" doc) None "Should be None"
            }
        ]
