namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

module VesperLibManifest =

    type LibFile =
        {
            Path: OriginPath
            /// Where this build found the file. Not part of the identity: two invocations
            /// that mount the package at different absolute paths read the same file.
            Absolute: string
        }

    /// The file a manifest of `bucketName` names as `relative`, mounted at `dir`.
    let libFile (bucketName: string) (dir: string) (relative: string) : LibFile =
        {
            Path =
                {
                    BucketName = bucketName
                    Relative = AssemblyFileId.ofPathUnder dir relative
                }
            Absolute = Path.Combine(dir, relative)
        }

    /// The lexer's token table and source text are retained so subsequent
    /// passes can extract identifier text off a `SyntaxToken`.
    type ParsedFile =
        {
            File: LibFile
            Lexed: Lexed
            Ast: FSharpAst<SyntaxToken>
        }

    // Force-load the parser's `ObjectConstruction` ref: attribute parsing reads it, and
    // the parser's own initialisers sit on paths a pure-signature run may never touch.
    do ObjectConstruction.init ()

    let parseFileFull (file: LibFile) : Result<ParsedFile, string> =
        let input = File.ReadAllText file.Absolute

        match Lexing.lexString input with
        | Error _ -> Error(sprintf "Lex error in %s" file.Path.Relative.Name)
        | Ok lexed ->
            let reader = Reader.ofLexed lexed Set.empty

            let result =
                if file.Path.Relative.Name.EndsWith ".fsi" then
                    FSharpAst.parseSignature reader
                else
                    FSharpAst.parse reader

            match result with
            | Ok ast ->
                Ok
                    {
                        File = file
                        Lexed = lexed
                        Ast = ast
                    }
            | Error e -> Error(ErrorFormatting.splitAndFormatTokenErrors e)
