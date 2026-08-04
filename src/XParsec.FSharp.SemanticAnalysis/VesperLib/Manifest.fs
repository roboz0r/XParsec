namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Per-file parsing for the `.fsi` contract extractor. The `parseFileFull` shim
/// routes `.fsi` through the signature-file parser and the (rare) `.fs` through
/// the implementation parser. The files themselves are resolved by the caller —
/// `ReferencedProject` reads each package's `manifest.toml` and hands the results
/// here one at a time.
module VesperLibManifest =

    /// One file handed to the extractor: the identity every anchor into it will name, and
    /// where to read its bytes. The identity is the `OriginPath` itself and not a copy of its
    /// fields, so a manifest entry and the anchor domain it becomes cannot come apart.
    ///
    /// `Path.Relative` also selects the parser (`.fsi` ⇒ signature, otherwise implementation)
    /// and names the file in diagnostics.
    type LibFile =
        {
            Path: OriginPath
            /// Where this build found the file. NOT part of the identity — two invocations
            /// that mount the package differently are reading the same file — so it stays
            /// here, with the read, rather than riding into every tree anchored in it.
            Absolute: string
        }

    /// The lexer's token table and source text are retained so subsequent
    /// passes can extract identifier text off a `SyntaxToken`.
    type ParsedFile =
        {
            File: LibFile
            Input: string
            Lexed: Lexed
            Ast: FSharpAst<SyntaxToken>
        }

    // Force-load the parser's `ObjectConstruction` ref so attribute
    // parsing succeeds even when the only entry points hit are
    // signature-file parsers. The init lives behind a `do` at the head
    // of `ImplementationFile.pNamedModule`, which a pure-signature path
    // may never touch.
    do ObjectConstruction.init ()

    /// Parse one `.fsi` file via XParsec.FSharp's signature-file parser, or `.fs`
    /// file via the implementation parser. Both paths are live: most modules ship
    /// `.fsi` (extracted as the signature contract), but a package's per-target
    /// primitive companions (`prim-types-int.clr.fs`) and operator bodies
    /// (`ops-platform.clr.fs`) have no companion signature and route through
    /// `FSharpAst.parse` so their reprs / bodies feed the intrinsic extraction and the
    /// cross-package inline-expansion pipeline.
    let parseFileFull (file: LibFile) : Result<ParsedFile, string> =
        let input = SourceText.normalise (File.ReadAllText file.Absolute)

        match Lexing.lexString input with
        | Error _ -> Error(sprintf "Lex error in %s" file.Path.Relative)
        | Ok lexed ->
            let reader = Reader.ofLexed lexed input Set.empty

            let result =
                if file.Path.Relative.EndsWith ".fsi" then
                    FSharpAst.parseSignature reader
                else
                    FSharpAst.parse reader

            match result with
            | Ok ast ->
                Ok
                    {
                        File = file
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }
            | Error e -> Error(ErrorFormatting.splitAndFormatTokenErrors e)
