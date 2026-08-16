namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Lex → reader → AST, with the parser's findings translated into the semantic layer's
/// `Diagnostic`.
module ParseChain =

    type Diagnostic = XParsec.FSharp.SemanticAnalysis.Diagnostic

    // Force-load the parser's `ObjectConstruction` ref: attribute parsing reads it, and
    // the parser's own initialisers sit on paths a pure-signature run may never touch.
    do ObjectConstruction.init ()

    /// A parsed file: the token stream and tree every pass runs on, and the diagnostics
    /// RECOVERY raised producing them. A COMPLETE tree can still have had every delimiter
    /// inserted and every missing expression stubbed, so `Ok` routinely carries diagnostics.
    type ParsedFile =
        {
            Lexed: Lexed
            File: ImplementationFile<SyntaxToken>
            Diagnostics: Diagnostic list
        }

    /// A file no tree came out of. `Lexed` is present iff LEXING succeeded, so the recovery
    /// diagnostics raised before the parser gave up still have the token stream their
    /// positions resolve against.
    type ParseFailure =
        {
            Lexed: Lexed voption
            Diagnostics: Diagnostic list
        }

    /// The parser's diagnostics as the semantic layer sees them, put back into SOURCE order
    /// because the parser accumulates them reversed.
    let private ofParseDiagnostics (diagnostics: XParsec.FSharp.Parser.Diagnostic list) : Diagnostic list =
        // Both delimiter diagnostics point back at the delimiter left open.
        let openedHere (openedAt: Site) : Label list =
            [
                {
                    Site = openedAt
                    Message = DiagnosticCode.openedHereLabel
                }
            ]

        [
            for d in List.rev diagnostics do
                let site, related =
                    match d.Code with
                    // The close was never written: the parser SYNTHESISED one, so the
                    // mistake is the hole it went into, not the token that exposed it.
                    | DiagnosticCode.UnclosedDelimiter(openedAt = openedAt) ->
                        Site.gapBefore (Site.ofToken d.Token), openedHere openedAt
                    // The close IS written, just the wrong one, and nothing was inserted,
                    // so that token is the mistake.
                    | DiagnosticCode.MismatchedDelimiter(openedAt = openedAt) ->
                        Site.ofToken d.Token, openedHere openedAt
                    | _ ->
                        match d.TokenEnd with
                        | Some last -> Site.spanning [ d.Token; last ], []
                        | None -> Site.ofToken d.Token, []

                Diagnostic.create (Kind.Parse d.Code) site related
        ]

    /// The front-end parse chain, lex → reader → AST: a bare-expression `ScriptFragment`
    /// wraps as an `AnonymousModule`, and lex/parse failures surface as `Diagnostic`s
    /// (never exceptions). The parser's recovery diagnostics ride out on BOTH arms.
    let parse (source: string) : Result<ParsedFile, ParseFailure> =
        match Lexing.lexString source with
        | Result.Error e ->
            Error
                {
                    Lexed = ValueNone
                    // A whole-file lex failure has no place in the file to point at.
                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure(sprintf "%A" e)) ]
                }
        | Result.Ok lexed ->
            let reader = Reader.ofLexed lexed Set.empty

            let failed (kind: Kind) =
                Error
                    {
                        Lexed = ValueSome lexed
                        Diagnostics = Diagnostic.nowhere kind :: ofParseDiagnostics reader.State.Diagnostics
                    }

            let parsed (file: ImplementationFile<SyntaxToken>) =
                Ok
                    {
                        Lexed = lexed
                        File = file
                        Diagnostics = ofParseDiagnostics reader.State.Diagnostics
                    }

            match FSharpAst.parse reader with
            | Result.Error e -> failed (Kind.ParseFailure(sprintf "%A" e))
            | Result.Ok(FSharpAst.ImplementationFile f) -> parsed f
            | Result.Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
                parsed (ImplementationFile.AnonymousModule elems)
            | Result.Ok other -> failed (Kind.ParseFailure(sprintf "unexpected AST: %A" other))

    /// A parsed SIGNATURE file: the tree the `.fsi` front end walks, and the token table every
    /// `SyntaxToken` in it indexes into.
    type ParsedSignature =
        {
            Lexed: Lexed
            File: SignatureFile<SyntaxToken>
            Diagnostics: Diagnostic list
        }

    /// `parse` for a `.fsi`. A signature file has no bare-expression form to wrap.
    let parseSignature (source: string) : Result<ParsedSignature, ParseFailure> =
        match Lexing.lexString source with
        | Result.Error e ->
            Error
                {
                    Lexed = ValueNone
                    Diagnostics = [ Diagnostic.nowhere (Kind.LexFailure(sprintf "%A" e)) ]
                }
        | Result.Ok lexed ->
            let reader = Reader.ofLexed lexed Set.empty

            let failed (kind: Kind) =
                Error
                    {
                        Lexed = ValueSome lexed
                        Diagnostics = Diagnostic.nowhere kind :: ofParseDiagnostics reader.State.Diagnostics
                    }

            match FSharpAst.parseSignature reader with
            | Result.Error e -> failed (Kind.ParseFailure(sprintf "%A" e))
            | Result.Ok(FSharpAst.SignatureFile f) ->
                Ok
                    {
                        Lexed = lexed
                        File = f
                        Diagnostics = ofParseDiagnostics reader.State.Diagnostics
                    }
            | Result.Ok other -> failed (Kind.ParseFailure(sprintf "unexpected AST: %A" other))

    /// `parse`, refusing a tree the parser had to PATCH: every inserted delimiter and every
    /// `Expr.Missing` is a hole the source did not fill, so a recovered parse is not a
    /// compilable one.
    let parseUnrecovered (source: string) : Result<ParsedFile, Diagnostic list> =
        match parse source with
        | Error f -> Error f.Diagnostics
        | Ok parsed ->
            match parsed.Diagnostics with
            | [] -> Ok parsed
            | recovered -> Error recovered
