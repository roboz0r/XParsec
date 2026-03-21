namespace XParsec.FSharp.Parser

open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.Parser.ParseState

[<RequireQualifiedAccess>]
module ImplementationFile =
    /// Parses: [attributes] module [access] LongIdent <module-elems>
    /// Distinguished from module abbreviation (module X = Y.Z) by the absence of '=' after the LongIdent.
    let private pNamedModule =
        let notFollowedByEquals = notFollowedByNonTriviaToken Token.OpEquality

        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            // Committed after 'module' — recover with virtual ident if LongIdent fails
            let! longIdent = recoverLongIdent "Expected module identifier" LongIdent.parse
            // Distinguish named module (module Foo.Bar <elems>) from module abbreviation (module X = Y.Z)
            do! notFollowedByEquals
            let! elems = ModuleElem.parseElemsWithRecovery
            return NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems)
        }

    let parse =
        dispatchNextNonTriviaTokenFallback
            [
                Token.KWNamespace, many1 NamespaceDeclGroup.parse |>> (List.ofSeq >> ImplementationFile.Namespaces)
                Token.KWModule,
                // Try named module first (module LongIdent <elems>),
                // fall back to anonymous module (e.g. module abbreviation as first elem)
                choiceL
                    [
                        pNamedModule |>> ImplementationFile.NamedModule
                        ModuleElem.parseElems |>> ImplementationFile.AnonymousModule
                    ]
                    "ImplementationFile"
            ]
            // No namespace/module keyword → try named module (with attrs) or anonymous module
            (choiceL
                [
                    pNamedModule |>> ImplementationFile.NamedModule
                    ModuleElem.parseElems |>> ImplementationFile.AnonymousModule
                ]
                "ImplementationFile")

[<RequireQualifiedAccess>]
module FSharpAst =
    /// Succeeds at end-of-file by consuming the EOF sentinel token (skipping trivia and directives).
    /// We also check that the reader is indeed at EOF to avoid accepting spurious tokens after a successful parse.
    let private pEof = nextNonTriviaTokenIsL Token.EOF "Expected end of file" .>> eof

    let private pNormal =
        choiceL
            [
                ImplementationFile.parse .>> pEof |>> FSharpAst.ImplementationFile
                Expr.parse .>> pEof
                |>> (fun e -> FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment [ ModuleElem.Expression e ]))
            ]
            "FSharpAst"

    /// Skips all remaining non-EOF tokens, collecting them into a list.
    let private skipToEof (reader: Reader<PositionedToken, ParseState, _, _>) =
        let skipped = ResizeArray<SyntaxToken>()
        let mutable keepGoing = true

        while keepGoing do
            match peekNextNonTriviaToken reader with
            | Error _ -> keepGoing <- false
            | Ok tok ->
                if tok.Token = Token.EOF then
                    keepGoing <- false
                else
                    match consumePeeked tok reader with
                    | Ok t -> skipped.Add(t)
                    | Error _ -> keepGoing <- false

        List.ofSeq skipped

    /// Infallible top-level parser. Always returns Ok with errors captured as diagnostics.
    let parse: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _, _> =
        fun reader ->
            let startPos = reader.Position

            match pNormal reader with
            | Ok result -> Ok result
            | Error topErr ->
                // Normal parsing failed. Reset and try ImplementationFile without pEof,
                // then consume leftover tokens as diagnostics.
                reader.Position <- startPos

                match ImplementationFile.parse reader with
                | Ok implFile ->
                    // Parsed something but didn't reach EOF. Collect leftover tokens.
                    let skipped = skipToEof reader

                    if not skipped.IsEmpty then
                        let startTok = skipped.Head

                        reader.State <-
                            addDiagnostic
                                (DiagnosticCode.UnexpectedTopLevel topErr)
                                DiagnosticSeverity.Error
                                startTok.PositionedToken
                                None
                                reader.State

                    Ok(FSharpAst.ImplementationFile implFile)

                | Error _ ->
                    // Even ImplementationFile.parse failed. Reset and try just Expr.parse.
                    reader.Position <- startPos

                    match Expr.parse reader with
                    | Ok expr ->
                        // Parsed an expression. Collect leftover tokens.
                        let skipped = skipToEof reader

                        if not skipped.IsEmpty then
                            let startTok = skipped.Head

                            reader.State <-
                                addDiagnostic
                                    (DiagnosticCode.UnexpectedTopLevel topErr)
                                    DiagnosticSeverity.Error
                                    startTok.PositionedToken
                                    None
                                    reader.State

                        Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment [ ModuleElem.Expression expr ]))

                    | Error _ ->
                        // Nothing parseable at all. Skip all tokens and return empty module.
                        reader.Position <- startPos
                        let skipped = skipToEof reader

                        let elems =
                            if not skipped.IsEmpty then
                                let startTok = skipped.Head

                                reader.State <-
                                    addDiagnostic
                                        (DiagnosticCode.UnexpectedTopLevel topErr)
                                        DiagnosticSeverity.Error
                                        startTok.PositionedToken
                                        None
                                        reader.State

                                [ ModuleElem.SkipsTokens(skipped) ]
                            else
                                []

                        Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elems))
