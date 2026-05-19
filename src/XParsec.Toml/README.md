# XParsec.Toml

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![TOML](https://img.shields.io/badge/TOML-1.0.0-orange.svg)](https://toml.io/en/v1.0.0)

A fully TOML 1.0.0 compliant parser for F#, built with XParsec.

## Origin

This is a fork of [Fidelity.Toml](https://github.com/FidelityFramework/Fidelity.Toml) (MIT, 2026). The
original author archived their copy after pivoting to [Clef](https://github.com/FidelityFramework),
their own F# fork that uses `.clef` configuration files instead of TOML. Both copyrights are preserved
in [LICENSE](LICENSE). The XParsec fork tracks the in-tree XParsec project so the two evolve together.

## Overview

XParsec.Toml is a pure F# implementation of a [TOML 1.0.0](https://toml.io/en/v1.0.0) parser using
parser combinators:

- **Spec-compliant**: Full TOML 1.0.0 support including all data types, datetime formats, and
  structural features.
- **Pure F#**: No external parsing dependencies beyond XParsec.
- **BCL-minimal**: Uses F# derived types (`Map`, lists, structs) for the document model.

## Usage

```fsharp
open XParsec.Toml

let toml = """
[package]
name = "my-project"
version = "1.0.0"

[build]
sources = ["Main.fs", "Lib.fs"]
output = "myapp"
"""

match Toml.parse toml with
| Ok document ->
    let name = Toml.getString "package.name" document  // Some "my-project"
    let sources = Toml.getStringArray "build.sources" document  // Some ["Main.fs"; "Lib.fs"]
    printfn "Project: %A" name

| Error msg ->
    eprintfn "Parse error: %s" msg
```

## Supported TOML Features

### Data Types

| Type | Example |
|------|---------|
| String (basic) | `"hello\nworld"` |
| String (literal) | `'C:\path\to\file'` |
| String (multiline) | `"""..."""` |
| Integer (decimal) | `42`, `1_000_000` |
| Integer (hex) | `0xDEADBEEF` |
| Integer (octal) | `0o755` |
| Integer (binary) | `0b11010110` |
| Float | `3.14`, `5e10`, `inf`, `nan` |
| Boolean | `true`, `false` |
| Offset Date-Time | `1979-05-27T07:32:00Z` |
| Local Date-Time | `1979-05-27T07:32:00` |
| Local Date | `1979-05-27` |
| Local Time | `07:32:00` |

### Structural Features

| Feature | Example |
|---------|---------|
| Tables | `[section]` |
| Nested Tables | `[section.subsection]` |
| Inline Tables | `point = { x = 1, y = 2 }` |
| Arrays | `ports = [80, 443]` |
| Array of Tables | `[[products]]` |
| Dotted Keys | `physical.color = "red"` |
| Comments | `# comment` |

## API Reference

### Parsing

```fsharp
/// Parse a TOML string into a document
val parse : string -> Result<TomlDocument, string>
```

### Value Access

```fsharp
/// Get a string value by dotted key path
val getString : string -> TomlDocument -> string option

/// Get an integer value
val getInt : string -> TomlDocument -> int64 option

/// Get a float value
val getFloat : string -> TomlDocument -> float option

/// Get a boolean value
val getBool : string -> TomlDocument -> bool option

/// Get an array of strings
val getStringArray : string -> TomlDocument -> string list option

/// Get a table (sub-document)
val getTable : string -> TomlDocument -> TomlTable option
```

### Serialization

```fsharp
/// Serialize a document back to TOML text.
/// Round-trips with parse, modulo comments and original whitespace
/// (the AST does not preserve either).
val serialize : TomlDocument -> string
```

## License

MIT — see [LICENSE](LICENSE) for the full text and attribution.
