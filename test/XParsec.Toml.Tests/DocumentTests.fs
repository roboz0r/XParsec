/// Tests for complete TOML document parsing.
/// Covers multi-section documents, comments, and whitespace handling.
module XParsec.Toml.Tests.DocumentTests

open Expecto
open XParsec.Toml

[<Tests>]
let commentTests =
    testList
        "Comments"
        [
            test "line comment" {
                let input =
                    """
# This is a comment
key = "value"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "value") "Should parse value"
            }

            test "inline comment" {
                let input = """key = "value" # inline comment"""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "value") "Should parse value"
            }

            test "comment only document" {
                let input =
                    """
# Just a comment
# Another comment
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.isEmpty (Map.toList doc) "Should be empty"
            }
        ]

[<Tests>]
let whitespaceTests =
    testList
        "Whitespace"
        [
            test "empty document" {
                let input = ""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.isEmpty (Map.toList doc) "Should be empty"
            }

            test "whitespace only document" {
                let input = "   \n\n   \n"
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.isEmpty (Map.toList doc) "Should be empty"
            }

            test "multiple blank lines" {
                let input =
                    """
key1 = 1


key2 = 2
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "key1" doc) (Some 1L) "key1"
                Expect.equal (Toml.getInt "key2" doc) (Some 2L) "key2"
            }

            test "spaces around equals" {
                let input = "key   =   \"value\""
                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "key" doc) (Some "value") "Should parse"
            }
        ]

[<Tests>]
let multiSectionTests =
    testList
        "Multi-Section Documents"
        [
            test "root and table" {
                let input =
                    """
root_key = "root"

[section]
section_key = "section"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "root_key" doc) (Some "root") "Root key"
                Expect.equal (Toml.getString "section.section_key" doc) (Some "section") "Section key"
            }

            test "multiple tables" {
                let input =
                    """
[server]
host = "localhost"
port = 8080

[database]
host = "db.example.com"
port = 5432
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "server.host" doc) (Some "localhost") "Server host"
                Expect.equal (Toml.getInt "server.port" doc) (Some 8080L) "Server port"
                Expect.equal (Toml.getString "database.host" doc) (Some "db.example.com") "DB host"
                Expect.equal (Toml.getInt "database.port" doc) (Some 5432L) "DB port"
            }

            test "deeply nested tables" {
                let input =
                    """
[a]
x = 1

[a.b]
x = 2

[a.b.c]
x = 3

[a.b.c.d]
x = 4
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getInt "a.x" doc) (Some 1L) "a.x"
                Expect.equal (Toml.getInt "a.b.x" doc) (Some 2L) "a.b.x"
                Expect.equal (Toml.getInt "a.b.c.x" doc) (Some 3L) "a.b.c.x"
                Expect.equal (Toml.getInt "a.b.c.d.x" doc) (Some 4L) "a.b.c.d.x"
            }

            test "mixed root and tables with comments" {
                let input =
                    """
# Application configuration
title = "My App"
version = "1.0.0"

# Server settings
[server]
host = "0.0.0.0"
port = 3000

# Feature flags
[features]
dark_mode = true
beta = false
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "title" doc) (Some "My App") "Title"
                Expect.equal (Toml.getString "version" doc) (Some "1.0.0") "Version"
                Expect.equal (Toml.getString "server.host" doc) (Some "0.0.0.0") "Server host"
                Expect.equal (Toml.getInt "server.port" doc) (Some 3000L) "Server port"
                Expect.equal (Toml.getBool "features.dark_mode" doc) (Some true) "Dark mode"
                Expect.equal (Toml.getBool "features.beta" doc) (Some false) "Beta"
            }
        ]

[<Tests>]
let realWorldTests =
    testList
        "Real World Examples"
        [
            test "cargo-style package config" {
                let input =
                    """
[package]
name = "my-project"
version = "0.1.0"
authors = ["Alice <alice@example.com>", "Bob <bob@example.com>"]

[dependencies]
serde = "1.0"

[dev-dependencies]
test-framework = { version = "2.0", features = ["async"] }
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "package.name" doc) (Some "my-project") "Name"
                Expect.equal (Toml.getString "package.version" doc) (Some "0.1.0") "Version"

                Expect.equal
                    (Toml.getStringArray "package.authors" doc)
                    (Some [ "Alice <alice@example.com>"; "Bob <bob@example.com>" ])
                    "Authors"

                Expect.equal (Toml.getString "dependencies.serde" doc) (Some "1.0") "Serde dep"
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

[dependencies]
alloy = { path = "../Alloy/src" }

[build]
sources = ["Main.fs"]
output = "hello"
output_kind = "freestanding"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "package.name" doc) (Some "HelloWorld") "Name"
                Expect.equal (Toml.getString "compilation.memory_model" doc) (Some "stack_only") "Memory model"
                Expect.equal (Toml.getStringArray "build.sources" doc) (Some [ "Main.fs" ]) "Sources"
                Expect.equal (Toml.getString "build.output_kind" doc) (Some "freestanding") "Output kind"
            }

            test "config with arrays of tables" {
                let input =
                    """
[server]
name = "main"

[[server.endpoints]]
path = "/api/users"
method = "GET"

[[server.endpoints]]
path = "/api/users"
method = "POST"

[[server.endpoints]]
path = "/api/health"
method = "GET"
"""

                let result = Toml.parse input
                Expect.isOk result "Should parse"
                let doc = Result.defaultValue Map.empty result
                Expect.equal (Toml.getString "server.name" doc) (Some "main") "Server name"

                match Toml.getValue "server.endpoints" doc with
                | Some(TomlValue.Array arr) -> Expect.equal (List.length arr) 3 "Should have 3 endpoints"
                | _ -> failtest "Expected Array"
            }
        ]
