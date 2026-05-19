/// Tests for TOML array parsing.
module XParsec.Toml.Tests.ArrayTests

open Expecto
open XParsec.Toml

[<Tests>]
let arrayTests =
    testList
        "Arrays"
        [
            test "empty array" {
                let input = "key = []"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.isEmpty arr "Should be empty"
                | _ -> failtest "Expected Array"
            }

            test "integer array" {
                let input = "key = [1, 2, 3]"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 3 "Should have 3 elements"
                | _ -> failtest "Expected Array"
            }

            test "string array" {
                let input = """key = ["a", "b", "c"]"""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getStringArray "key" doc) (Some [ "a"; "b"; "c" ]) "Should match"
            }

            test "array with trailing comma" {
                let input = "key = [1, 2, 3,]"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 3 "Should have 3 elements"
                | _ -> failtest "Expected Array"
            }

            test "multiline array" {
                let input =
                    """
key = [
    1,
    2,
    3
]
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 3 "Should have 3 elements"
                | _ -> failtest "Expected Array"
            }

            test "nested array" {
                let input = "key = [[1, 2], [3, 4]]"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array outer) ->
                    Expect.equal (List.length outer) 2 "Should have 2 inner arrays"

                    match outer with
                    | [ TomlValue.Array inner1; TomlValue.Array inner2 ] ->
                        Expect.equal (List.length inner1) 2 "First inner"
                        Expect.equal (List.length inner2) 2 "Second inner"
                    | _ -> failtest "Expected nested arrays"
                | _ -> failtest "Expected Array"
            }

            test "mixed type array" {
                let input = """key = ["string", 42, 3.14]"""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 3 "Should have 3 elements"
                | _ -> failtest "Expected Array"
            }
        ]
