/// Round-trip tests for TOML serialization.
/// Strategy: parse input → serialize → parse again → compare documents.
module XParsec.Toml.Tests.SerializerTests

open Expecto
open XParsec.Toml

/// Helper: parse, serialize, parse again, verify structural equality.
let private roundTrip (input: string) =
    let doc1 = Toml.parseOrFail input
    let serialized = Toml.serialize doc1
    let doc2 = Toml.parseOrFail serialized
    doc1, doc2, serialized

[<Tests>]
let scalarRoundTripTests =
    testList
        "Serializer - Scalar Round-trips"
        [
            test "simple string" {
                let doc1, doc2, _ = roundTrip "key = \"hello world\""
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "string with escapes" {
                let doc1, doc2, _ = roundTrip "key = \"hello\\nworld\\t!\""
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "string with backslash" {
                let doc1, doc2, _ = roundTrip "key = \"C:\\\\Users\\\\test\""
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "string with quotes" {
                let doc1, doc2, _ = roundTrip "key = \"say \\\"hello\\\"\""
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "empty string" {
                let doc1, doc2, _ = roundTrip "key = \"\""
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "positive integer" {
                let doc1, doc2, _ = roundTrip "key = 42"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "negative integer" {
                let doc1, doc2, _ = roundTrip "key = -17"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "zero integer" {
                let doc1, doc2, _ = roundTrip "key = 0"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "simple float" {
                let doc1, doc2, _ = roundTrip "key = 3.14"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "negative float" {
                let doc1, doc2, _ = roundTrip "key = -0.5"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "float with exponent" {
                let doc1, doc2, _ = roundTrip "key = 1e10"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "positive infinity" {
                let _, doc2, serialized = roundTrip "key = inf"
                Expect.stringContains serialized "inf" "Should contain inf"
                Expect.equal (Toml.getFloat "key" doc2) (Some infinity) "Should be infinity"
            }

            test "negative infinity" {
                let _, doc2, serialized = roundTrip "key = -inf"
                Expect.stringContains serialized "-inf" "Should contain -inf"
                Expect.equal (Toml.getFloat "key" doc2) (Some(-infinity)) "Should be -infinity"
            }

            test "nan" {
                let _, doc2, serialized = roundTrip "key = nan"
                Expect.stringContains serialized "nan" "Should contain nan"

                match Toml.getFloat "key" doc2 with
                | Some f -> Expect.isTrue (System.Double.IsNaN f) "Should be NaN"
                | None -> failtest "Expected float"
            }

            test "boolean true" {
                let doc1, doc2, _ = roundTrip "key = true"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "boolean false" {
                let doc1, doc2, _ = roundTrip "key = false"
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let dateTimeRoundTripTests =
    testList
        "Serializer - DateTime Round-trips"
        [
            test "offset datetime UTC" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27T07:32:00Z"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "offset datetime with positive offset" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27T07:32:00+05:30"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "offset datetime with negative offset" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27T07:32:00-05:00"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "offset datetime with fractional seconds" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27T07:32:00.123456Z"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "local datetime" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27T07:32:00"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "local date" {
                let doc1, doc2, _ = roundTrip "key = 1979-05-27"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "local time" {
                let doc1, doc2, _ = roundTrip "key = 07:32:00"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "local time with fractional seconds" {
                let doc1, doc2, _ = roundTrip "key = 07:32:00.123"
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let arrayRoundTripTests =
    testList
        "Serializer - Array Round-trips"
        [
            test "empty array" {
                let doc1, doc2, _ = roundTrip "key = []"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "integer array" {
                let doc1, doc2, _ = roundTrip "key = [1, 2, 3]"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "string array" {
                let doc1, doc2, _ = roundTrip "key = [\"a\", \"b\", \"c\"]"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "nested array" {
                let doc1, doc2, _ = roundTrip "key = [[1, 2], [3, 4]]"
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "mixed type array" {
                let doc1, doc2, _ = roundTrip "key = [1, \"two\", true]"
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let tableRoundTripTests =
    testList
        "Serializer - Table Round-trips"
        [
            test "simple table" {
                let input =
                    """
[server]
host = "localhost"
port = 8080
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "nested tables" {
                let input =
                    """
[a]
x = 1

[a.b]
x = 2

[a.b.c]
x = 3
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "multiple top-level tables" {
                let input =
                    """
[server]
host = "localhost"

[database]
host = "db.example.com"
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "inline table" {
                let input = "key = { a = 1, b = \"hello\" }"
                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "empty inline table" {
                let input = "key = { }"
                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let arrayOfTablesRoundTripTests =
    testList
        "Serializer - Array of Tables Round-trips"
        [
            test "array of tables" {
                let input =
                    """
[server]
name = "main"

[[server.endpoints]]
path = "/api/users"
method = "GET"

[[server.endpoints]]
path = "/api/health"
method = "GET"
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let documentRoundTripTests =
    testList
        "Serializer - Full Document Round-trips"
        [
            test "cargo-style config" {
                let input =
                    """
[package]
name = "my-project"
version = "0.1.0"
authors = ["Alice", "Bob"]

[dependencies]
serde = "1.0"
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "fidproj-style config" {
                let input =
                    """
[package]
name = "HelloWorld"
version = "0.1.0"

[compilation]
memory_model = "stack_only"
target = "native"

[build]
sources = ["Main.fs"]
output = "hello"
output_kind = "freestanding"
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }

            test "root values with tables" {
                let input =
                    """
title = "My App"
version = "1.0.0"

[server]
host = "0.0.0.0"
port = 3000

[features]
dark_mode = true
"""

                let doc1, doc2, _ = roundTrip input
                Expect.equal doc2 doc1 "Should round-trip"
            }
        ]

[<Tests>]
let keyQuotingTests =
    testList
        "Serializer - Key Quoting"
        [
            test "bare key passes through" {
                let serialized = Toml.serialize (Toml.parseOrFail "simple-key_123 = \"value\"")
                Expect.stringContains serialized "simple-key_123" "Bare key should not be quoted"
            }

            test "key with dot gets quoted" {
                let doc = Map.ofList [ ("key.with.dots", TomlValue.String "value") ]
                let serialized = Toml.serialize doc
                Expect.stringContains serialized "\"key.with.dots\"" "Dotted key should be quoted"
            }

            test "key with space gets quoted" {
                let doc = Map.ofList [ ("key with spaces", TomlValue.String "value") ]
                let serialized = Toml.serialize doc
                Expect.stringContains serialized "\"key with spaces\"" "Key with spaces should be quoted"
            }
        ]

[<Tests>]
let formattingTests =
    testList
        "Serializer - Formatting"
        [
            test "sections have blank line separator" {
                let input =
                    """
[a]
x = 1

[b]
y = 2
"""

                let _, _, serialized = roundTrip input
                // Sections should be separated by blank lines
                Expect.stringContains serialized "\n\n[" "Sections should be separated by blank lines"
            }

            test "serialize value - string" {
                let s = Toml.serializeValue (TomlValue.String "hello")
                Expect.equal s "\"hello\"" "Should serialize string"
            }

            test "serialize value - integer" {
                let s = Toml.serializeValue (TomlValue.Integer 42L)
                Expect.equal s "42" "Should serialize integer"
            }

            test "serialize value - boolean" {
                let s = Toml.serializeValue (TomlValue.Boolean true)
                Expect.equal s "true" "Should serialize boolean"
            }

            test "serialize value - array" {
                let s =
                    Toml.serializeValue (TomlValue.Array [ TomlValue.Integer 1L; TomlValue.Integer 2L ])

                Expect.equal s "[1, 2]" "Should serialize array"
            }
        ]
