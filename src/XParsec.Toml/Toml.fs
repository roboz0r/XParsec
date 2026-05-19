/// Public API for XParsec.Toml.
/// Provides functions for parsing TOML documents and accessing values.
namespace XParsec.Toml

/// Main entry point for TOML parsing and value access.
[<RequireQualifiedAccess>]
module Toml =

    /// Parses a TOML string into a document.
    /// Returns Ok with the document on success, or Error with a message on failure.
    let parse (input: string) : Result<TomlDocument, string> = TomlParser.parse input

    /// Parses a TOML string, throwing an exception on failure.
    let parseOrFail (input: string) : TomlDocument =
        match parse input with
        | Ok doc -> doc
        | Error msg -> failwith msg

    /// Gets a value by dotted key path.
    let getValue (path: string) (doc: TomlDocument) : TomlValue option = TomlDocument.tryGetPath path doc

    /// Gets a string value by dotted key path.
    let getString (path: string) (doc: TomlDocument) : string option = TomlDocument.tryGetString path doc

    /// Gets an integer value by dotted key path.
    let getInt (path: string) (doc: TomlDocument) : int64 option = TomlDocument.tryGetInt path doc

    /// Gets a float value by dotted key path.
    let getFloat (path: string) (doc: TomlDocument) : float option = TomlDocument.tryGetFloat path doc

    /// Gets a boolean value by dotted key path.
    let getBool (path: string) (doc: TomlDocument) : bool option = TomlDocument.tryGetBool path doc

    /// Gets a string array by dotted key path.
    let getStringArray (path: string) (doc: TomlDocument) : string list option = TomlDocument.tryGetStringArray path doc

    /// Gets a table by dotted key path.
    let getTable (path: string) (doc: TomlDocument) : TomlTable option = TomlDocument.tryGetTable path doc

    /// Gets an inline table as key-value pairs by dotted key path.
    let getInlineTable (path: string) (doc: TomlDocument) : (string * TomlValue) list option =
        TomlDocument.tryGetInlineTable path doc

    /// Gets all keys at the root level of the document.
    let keys (doc: TomlDocument) : string seq = TomlTable.keys doc

    /// Serializes a TOML document to a valid TOML string.
    let serialize (doc: TomlDocument) : string = TomlSerializer.serializeDocument doc

    /// Serializes a single TOML value to its string representation.
    let serializeValue (value: TomlValue) : string = TomlSerializer.serializeValue value
