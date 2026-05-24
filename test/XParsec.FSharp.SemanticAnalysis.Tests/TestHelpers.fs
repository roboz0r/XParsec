module XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser

/// Lex + parse a source string and return Lexed + an ImplementationFile
/// (script fragments are wrapped as AnonymousModule). Raises on failure.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match Lexing.lexString input with
    | Error e -> failwithf "lex failed: %A" e
    | Ok lexed ->
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parse reader with
        | Error e -> failwithf "parse failed: %A" e
        | Ok(FSharpAst.ImplementationFile f) -> lexed, f
        | Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
            lexed, ImplementationFile.AnonymousModule elems
        | Ok ast -> failwithf "unexpected AST: %A" ast

/// Lex + parse a signature (`.fsi`) source string and return Lexed + a
/// SignatureFile. Raises on failure.
let parseSigFile (input: string) : Lexed * SignatureFile<SyntaxToken> =
    match Lexing.lexString input with
    | Error e -> failwithf "lex failed: %A" e
    | Ok lexed ->
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parseSignature reader with
        | Error e -> failwithf "parse failed: %A" e
        | Ok(FSharpAst.SignatureFile f) -> lexed, f
        | Ok ast -> failwithf "unexpected AST: %A" ast
