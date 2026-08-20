namespace XParsec.FSharp.SemanticAnalysis

open System.Text
open System.IO.Hashing
open XParsec.FSharp.Lexer

/// Content hashes for SOURCE IDENTITY: what an `FileStamp` carries so two files with the
/// same path and different text are distinct anchors' domains.
module Hashing =

    let hashBytes (bytes: byte[]) : InputHash =
        InputHash.ofBytes (XxHash128.Hash bytes)

    /// UTF-8 so the digest is culture- and platform-independent.
    let hashString (s: string) : InputHash = hashBytes (Encoding.UTF8.GetBytes s)

    /// The content hash is taken off the `Lexed`'s own text, not a re-read of the path, which
    /// can already disagree with what the token indices address.
    let lexedFile (path: AssemblyFilePath) (lexed: Lexed) : LexedFile =
        {
            Stamp =
                {
                    Path = path
                    ContentHash = hashString lexed.Input
                }
            Lexed = lexed
        }

    /// The identity of a source handed over as TEXT with no file behind it: a script fragment,
    /// a driver given a string, a test. The content hash stands in for the path.
    let textAssemblyFilePath (input: string) : AssemblyFilePath =
        {
            Assembly = ""
            Relative = AssemblyFileId.ofRelative (sprintf "<text:%s>" (hashString input).Hex)
        }

    let lexedFileOfText (lexed: Lexed) : LexedFile =
        lexedFile (textAssemblyFilePath lexed.Input) lexed
