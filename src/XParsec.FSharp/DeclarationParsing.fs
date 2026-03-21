namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

[<RequireQualifiedAccess>]
module ImportDecl =
    let parse =
        parser {
            let! openTok = pOpen
            let! ident = LongIdent.parse
            return ImportDecl(openTok, ident)
        }

[<RequireQualifiedAccess>]
module ModuleAbbrev =
    let parse =
        parser {
            let! modTok = pModule
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Expected identifier"
            let! eq = pEquals
            let! lid = LongIdent.parse
            return ModuleAbbrev.ModuleAbbrev(modTok, ident, eq, lid)
        }

[<RequireQualifiedAccess>]
module CompilerDirectiveDecl =
    let parse: Parser<CompilerDirectiveDecl<SyntaxToken>, _, _, ReadableImmutableArray<_>, _> =
        parser {
            let! hash = pHash
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Directive identifier"
            let! strings = many (nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsText) "String argument")
            return CompilerDirectiveDecl(hash, ident, List.ofSeq strings)
        }

[<RequireQualifiedAccess>]
module ModuleFunctionOrValueDefn =

    let private pLetBinding attrs letTok =
        parser {
            let! isRec = opt pRec

            match isRec with
            | ValueSome recTok ->
                let! bindings = Binding.parseSepByAnd1 attrs
                return ModuleFunctionOrValueDefn.Let(attrs, letTok, ValueSome recTok, bindings)
            | ValueNone ->
                let! binding = Binding.parse attrs
                return ModuleFunctionOrValueDefn.Let(attrs, letTok, ValueNone, [ binding ])
        }

    let parse =
        parser {
            let! attrs = opt Attributes.parse

            let! token = nextNonTriviaToken

            match token.Token with
            | Token.KWDo ->
                let! expr = Expr.parse
                return ModuleFunctionOrValueDefn.Do(attrs, token, expr)

            | Token.KWLet -> return! pLetBinding attrs token

            | _ -> return! fail (Message "Expected 'let' or 'do'")
        }

[<RequireQualifiedAccess>]
module ModuleDefn =

    let parseBody (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>) =
        parser {
            let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
            let! elems = withContext OffsideContext.Module (opt elementParser)
            let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
            return ModuleDefnBody(beginTok, elems, endTok)
        }

    let parse (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>) =
        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Module identifier"
            let! eq = pEquals
            let! body = parseBody elementParser
            return ModuleDefn(attrs, modTok, access, isRec, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleElem =
    let private refModuleElem = RefParser<ModuleElem<SyntaxToken>, _, _, _, _>()

    // Forward reference setup to handle: ModuleElem -> ModuleDefn -> ModuleElem
    /// Plain parseElems: no recovery. Used in AnonymousModule where backtracking must remain possible.
    let parseElems = many refModuleElem.Parser |>> List.ofSeq

    /// parseElems with recovery: after `many` stops on failure, skips tokens to the next
    /// module-elem boundary and resumes. Only safe in committed contexts (after namespace/module keyword).
    /// Recovery happens *outside* `many` to avoid zero-width infinite loops.
    let parseElemsWithRecovery: Parser<ModuleElems<SyntaxToken>, _, _, _, _> =
        fun reader ->
            let result = ResizeArray<ModuleElem<SyntaxToken>>()
            let mutable keepGoing = true

            while keepGoing do
                // Run many to collect as many module elements as possible
                match many refModuleElem.Parser reader with
                | Ok elems ->
                    for e in elems do
                        result.Add(e)
                | Error _ -> ()

                // Check if there are leftover non-EOF tokens
                match peekNextNonTriviaToken reader with
                | Error _ -> keepGoing <- false
                | Ok tok ->
                    if tok.Token = Token.EOF then
                        keepGoing <- false
                    else
                        // Skip tokens until the next module-elem-starting keyword or EOF
                        let skipped = ResizeArray<SyntaxToken>()
                        let mutable skipping = true

                        while skipping do
                            match peekNextNonTriviaToken reader with
                            | Error _ -> skipping <- false
                            | Ok tok ->
                                if StoppingTokens.afterModuleElem tok then
                                    skipping <- false
                                else
                                    match consumePeeked tok reader with
                                    | Ok t -> skipped.Add(t)
                                    | Error _ -> skipping <- false

                        if skipped.Count > 0 then
                            let startTok = skipped.[0]

                            let parseErr: ParseError<PositionedToken, ParseState> =
                                {
                                    Position = reader.Position
                                    Errors = Message "Unexpected token(s) in module"
                                }

                            reader.State <-
                                ParseState.addDiagnostic
                                    (DiagnosticCode.MissingModuleElem parseErr)
                                    DiagnosticSeverity.Error
                                    startTok.PositionedToken
                                    None
                                    reader.State

                            result.Add(ModuleElem.SkipsTokens(List.ofSeq skipped, ModuleElem.Missing))
                        else
                            // Can't skip anything and can't parse — avoid infinite loop
                            keepGoing <- false

            Ok(List.ofSeq result)

    let parse =
        dispatchNextNonTriviaTokenFallback
            [
                Token.KWOpen, ImportDecl.parse |>> ModuleElem.Import
                Token.KWHash, CompilerDirectiveDecl.parse |>> ModuleElem.CompilerDirective
            ]
            // Fallback: parsers that might be preceded by Attributes
            // (Module, Type, Exception, Let, Do, bare expressions)
            (choiceL
                [
                    // Type Definitions (with mutual recursion via 'and')
                    parser {
                        let! first = TypeDefn.parse
                        let! rest = many TypeDefn.parseAndContinuation
                        return ModuleElem.Type(first :: List.ofSeq rest)
                    }

                    // Exception Definitions
                    ExceptionDefn.parse |>> ModuleElem.Exception

                    // Module Abbreviation (must precede ModuleDefn so `module X = A.B.C` isn't consumed as a nested module)
                    ModuleAbbrev.parse |>> ModuleElem.ModuleAbbrev

                    // Module Definition (Recursive)
                    (ModuleDefn.parse parseElemsWithRecovery) |>> ModuleElem.Module

                    // Let / Do bindings
                    ModuleFunctionOrValueDefn.parse |>> ModuleElem.FunctionOrValue

                    // Bare expressions (scripts, interactive)
                    Expr.parse |>> ModuleElem.Expression
                ]
                "ModuleElem")

    do refModuleElem.Set parse


[<RequireQualifiedAccess>]
module NamespaceDeclGroup =
    let parse =
        parser {
            let! nsTok = pNamespace
            let! globalTok = opt pGlobal

            match globalTok with
            | ValueSome gTok ->
                // namespace global
                let! elems = ModuleElem.parseElemsWithRecovery
                return NamespaceDeclGroup.Global(nsTok, gTok, elems)

            | ValueNone ->
                // namespace [rec] LongIdent
                let! isRec = opt pRec
                let! ident = LongIdent.parse
                let! elems = ModuleElem.parseElemsWithRecovery
                return NamespaceDeclGroup.Named(nsTok, isRec, ident, elems)
        }
