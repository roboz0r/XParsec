namespace XParsec.FSharp.Parser

open System.Collections.Immutable
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
        do ObjectConstruction.init () // Force static constructor to run and initialize refObjectConstruction

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
                Token.KWNamespace, many1 NamespaceDeclGroup.parse |>> ImplementationFile.Namespaces
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
                |>> (fun e ->
                    FSharpAst.ScriptFragment(
                        ScriptFragment.ScriptFragment(ImmutableArray.Create(ModuleElem.Expression e))
                    )
                )
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

        ImmutableArray.CreateRange(skipped)

    /// Appends skipped tokens as a ModuleElem.SkipsTokens to the appropriate container
    /// in an ImplementationFile, so leftover tokens are visible in the AST.
    let private appendSkippedToImpl (skipped: ImmutableArray<SyntaxToken>) (implFile: ImplementationFile<SyntaxToken>) =
        let skipElem = ModuleElem.SkipsTokens(skipped)

        match implFile with
        | ImplementationFile.AnonymousModule elems -> ImplementationFile.AnonymousModule(elems.Add(skipElem))
        | ImplementationFile.NamedModule(NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems)) ->
            ImplementationFile.NamedModule(
                NamedModule.NamedModule(attrs, modTok, access, isRec, longIdent, elems.Add(skipElem))
            )
        | ImplementationFile.Namespaces decls when decls.Length > 0 ->
            let lastIdx = decls.Length - 1

            let updatedLast =
                match decls[lastIdx] with
                | NamespaceDeclGroup.Named(ns, isRec, ident, elems) ->
                    NamespaceDeclGroup.Named(ns, isRec, ident, elems.Add(skipElem))
                | NamespaceDeclGroup.Global(ns, g, elems) -> NamespaceDeclGroup.Global(ns, g, elems.Add(skipElem))

            ImplementationFile.Namespaces(decls.SetItem(lastIdx, updatedLast))
        | ImplementationFile.Namespaces _ -> ImplementationFile.AnonymousModule(ImmutableArray.Create(skipElem))

    /// Infallible top-level parser. Always returns Ok with errors captured as diagnostics.
    let parse: Parser<FSharpAst<SyntaxToken>, PositionedToken, ParseState, _, _> =
        fun reader ->
            let startPos = reader.Position

            // Push a file-level SeqBlock context so that collection/CE undentation rules
            // (checkCollectionUndent) always find a base offside line at the outermost level.
            let fileIndent =
                match peekNextNonTriviaToken reader with
                | Ok tok ->
                    match tok.Index with
                    | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                    | TokenIndex.Virtual -> 0
                | Error _ -> 0

            let entry: Offside =
                {
                    Context = OffsideContext.SeqBlock
                    Indent = fileIndent
                    Token = PositionedToken.Create(Token.EOF, 0)
                }

            reader.State <- ParseState.pushOffside entry reader.State

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

                    let implFile' =
                        if skipped.IsEmpty then
                            implFile
                        else
                            let startTok = skipped.[0]

                            reader.State <-
                                addErrorDiagnosticWithError
                                    DiagnosticCode.UnexpectedTopLevel
                                    startTok.PositionedToken
                                    topErr
                                    reader.State

                            appendSkippedToImpl skipped implFile

                    Ok(FSharpAst.ImplementationFile implFile')

                | Error _ ->
                    // Even ImplementationFile.parse failed. Reset and try just Expr.parse.
                    reader.Position <- startPos

                    match Expr.parse reader with
                    | Ok expr ->
                        // Parsed an expression. Collect leftover tokens.
                        let skipped = skipToEof reader
                        let elems = ImmutableArray.CreateBuilder(if skipped.IsEmpty then 1 else 2)
                        elems.Add(ModuleElem.Expression expr)

                        if not skipped.IsEmpty then
                            let startTok = skipped.[0]

                            reader.State <-
                                addErrorDiagnosticWithError
                                    DiagnosticCode.UnexpectedTopLevel
                                    startTok.PositionedToken
                                    topErr
                                    reader.State

                            elems.Add(ModuleElem.SkipsTokens(skipped))

                        Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment(elems.ToImmutable())))

                    | Error _ ->
                        // Nothing parseable at all. Skip all tokens and return empty module.
                        reader.Position <- startPos
                        let skipped = skipToEof reader

                        let elems =
                            if not skipped.IsEmpty then
                                let startTok = skipped.[0]

                                reader.State <-
                                    addErrorDiagnosticWithError
                                        DiagnosticCode.UnexpectedTopLevel
                                        startTok.PositionedToken
                                        topErr
                                        reader.State

                                ImmutableArray.Create(ModuleElem.SkipsTokens(skipped))
                            else
                                ImmutableArray.Empty

                        Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elems))
