/// Tests for TOML number parsing.
/// Covers integers (decimal, hex, octal, binary) and floats.
module XParsec.Toml.Tests.NumberTests

open Expecto
open XParsec.Toml

[<Tests>]
let integerTests =
    testList
        "Integers"
        [
            test "simple positive integer" {
                let input = "key = 42"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 42L) "Should be 42"
            }

            test "negative integer" {
                let input = "key = -17"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some -17L) "Should be -17"
            }

            test "positive integer with plus" {
                let input = "key = +99"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 99L) "Should be 99"
            }

            test "zero" {
                let input = "key = 0"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0L) "Should be 0"
            }

            test "integer with underscores" {
                let input = "key = 1_000_000"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 1000000L) "Should be 1000000"
            }

            test "hexadecimal integer" {
                let input = "key = 0xDEADBEEF"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0xDEADBEEFL) "Should be DEADBEEF"
            }

            test "octal integer" {
                let input = "key = 0o755"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 493L) "Should be 493 (0o755)"
            }

            test "binary integer" {
                let input = "key = 0b11010110"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 214L) "Should be 214"
            }

            test "hex with underscores" {
                let input = "key = 0xdead_beef"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key" doc) (Some 0xDEADBEEFL) "Should be DEADBEEF"
            }
        ]

[<Tests>]
let floatTests =
    testList
        "Floats"
        [
            test "simple float" {
                let input = "key = 3.14"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 3.14) "Should be 3.14"
            }

            test "negative float" {
                let input = "key = -0.01"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some -0.01) "Should be -0.01"
            }

            test "float with exponent" {
                let input = "key = 5e10"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 5e10) "Should be 5e10"
            }

            test "float with negative exponent" {
                let input = "key = 1e-10"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 1e-10) "Should be 1e-10"
            }

            test "float with fraction and exponent" {
                let input = "key = 6.022e23"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 6.022e23) "Should be Avogadro"
            }

            test "float with underscores" {
                let input = "key = 9_224_617.445_991"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some 9224617.445991) "Should parse underscores"
            }

            test "positive infinity" {
                let input = "key = inf"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some infinity) "Should be infinity"
            }

            test "negative infinity" {
                let input = "key = -inf"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getFloat "key" doc) (Some -infinity) "Should be -infinity"
            }

            test "nan" {
                let input = "key = nan"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result

                match Toml.getFloat "key" doc with
                | Some f -> Expect.isTrue (System.Double.IsNaN f) "Should be NaN"
                | None -> failtest "Should have float"
            }
        ]

[<Tests>]
let booleanTests =
    testList
        "Booleans"
        [
            test "true" {
                let input = "key = true"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getBool "key" doc) (Some true) "Should be true"
            }

            test "false" {
                let input = "key = false"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getBool "key" doc) (Some false) "Should be false"
            }
        ]
