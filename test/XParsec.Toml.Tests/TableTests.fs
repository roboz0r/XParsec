/// Tests for TOML table parsing.
/// Covers tables, inline tables, dotted keys, and array of tables.
module XParsec.Toml.Tests.TableTests

open Expecto
open XParsec.Toml

[<Tests>]
let tableTests =
    testList
        "Tables"
        [
            test "simple table" {
                let input =
                    """
[table]
key = "value"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "table.key" doc) (Some "value") "Should access nested"
            }

            test "nested table" {
                let input =
                    """
[parent]
name = "parent"

[parent.child]
name = "child"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "parent.name" doc) (Some "parent") "Parent name"
                Expect.equal (Toml.getString "parent.child.name" doc) (Some "child") "Child name"
            }

            test "dotted key in root" {
                let input = "a.b.c = 1"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "a.b.c" doc) (Some 1L) "Should access via path"
            }

            test "dotted key in table" {
                let input =
                    """
[table]
a.b = 1
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "table.a.b" doc) (Some 1L) "Should access via path"
            }

            test "quoted key" {
                let input = """"key with spaces" = "value" """
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key with spaces" doc) (Some "value") "Should work"
            }
        ]

[<Tests>]
let inlineTableTests =
    testList
        "Inline Tables"
        [
            test "simple inline table" {
                let input = "key = { a = 1, b = 2 }"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key.a" doc) (Some 1L) "Should access a"
                Expect.equal (Toml.getInt "key.b" doc) (Some 2L) "Should access b"
            }

            test "nested inline table" {
                let input = "key = { inner = { x = 1 } }"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key.inner.x" doc) (Some 1L) "Should access nested"
            }

            test "empty inline table" {
                let input = "key = {}"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "key" doc with
                | Some(TomlValue.InlineTable t) -> Expect.isEmpty (Map.toList t) "Should be empty"
                | _ -> failtest "Expected InlineTable"
            }
        ]

[<Tests>]
let arrayOfTablesTests =
    testList
        "Array of Tables"
        [
            test "simple array of tables" {
                let input =
                    """
[[products]]
name = "Hammer"
price = 9.99

[[products]]
name = "Nail"
price = 0.05
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "products" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 2 "Should have 2 products"
                | _ -> failtest "Expected Array"
            }

            test "nested array of tables" {
                let input =
                    """
[[fruits]]
name = "apple"

[[fruits.varieties]]
name = "red delicious"

[[fruits.varieties]]
name = "granny smith"

[[fruits]]
name = "banana"

[[fruits.varieties]]
name = "plantain"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getValue "fruits" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 2 "Should have 2 fruits"
                | _ -> failtest "Expected Array"
            }
        ]
