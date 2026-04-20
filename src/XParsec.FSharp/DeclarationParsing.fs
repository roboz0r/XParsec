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
            let! typeTok = opt pType

            // Committed after consuming 'open' — recover with virtual ident if LongIdent fails
            let! ident = recoverLongIdent "Expected identifier after 'open'" LongIdent.parse

            match typeTok with
            | ValueSome typeTok -> return ImportDeclType(openTok, typeTok, ident)
            | ValueNone -> return ImportDecl(openTok, ident)
        }

[<RequireQualifiedAccess>]
module ModuleAbbrev =
    let parse =
        parser {
            let! modTok = pModule
            let! ident = nextNonTriviaTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Expected identifier"
            let! eq = pEquals
            let! lid = LongIdent.parse
            return ModuleAbbrev.ModuleAbbrev(modTok, ident, eq, lid)
        }

[<RequireQualifiedAccess>]
module CompilerDirectiveDecl =
    let parse: Parser<CompilerDirectiveDecl<SyntaxToken>, _, _, ReadableImmutableArray<_>, _> =
        parser {
            let! hash = pHash
            // Committed after consuming '#' — recover with virtual identifier if missing
            let! ident =
                recoverWithVirtualToken
                    Token.Identifier
                    "Expected directive identifier after '#'"
                    (nextNonTriviaTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Directive identifier")

            let! strings = many (nextNonTriviaTokenSatisfiesLMsg (fun t -> t.Token.IsText) "String argument")
            return CompilerDirectiveDecl(hash, ident, strings)
        }

[<RequireQualifiedAccess>]
module ModuleFunctionOrValueDefn =

    // Consume a trailing `in` only when another token follows. If the `in` is the last
    // meaningful token in the file, leave it for the existing missing-body diagnostic
    // to fire via the script-expression fallback in FSharpAst.parse.
    let private pTrailingIn =
        parser {
            let! inTok = pIn

            match! peekNextNonTriviaToken with
            | t when t.Token <> Token.EOF -> return inTok
            | _ -> return! fail (Message "trailing 'in' at end of file has no body")
        }

    let private pLetBinding attrs letTok =
        parser {
            let! isRec = opt pRec

            let! recTok, bindings, ands =
                match isRec with
                | ValueSome recTok ->
                    parser {
                        let! bindings, ands = Binding.parseSepByAnd1 attrs
                        return ValueSome recTok, bindings, ands
                    }
                | ValueNone ->
                    parser {
                        let! binding = Binding.parse attrs
                        return ValueNone, ImmutableArray.Create(binding), ImmutableArray.Empty
                    }

            // F# allows an ML-style trailing `in` to terminate a module-level let binding
            // (LexFilter turns it into ODECLEND in the reference compiler). Capture it so
            // the AST preserves the source token.
            let! inTok = opt pTrailingIn
            return ModuleFunctionOrValueDefn.Let(attrs, letTok, recTok, bindings, ands, inTok)
        }

    let parse =
        parser {
            let! attrs = opt Attributes.parse

            let! token = nextNonTriviaToken

            match token.Token with
            | Token.KWDo ->
                // Committed after 'do' — recover with Expr.Missing if expression fails.
                // Grammar: moduleDoBinding: opt_attributes DO typedSeqExprBlock
                let! expr =
                    recoverWith
                        StoppingTokens.afterExpr
                        DiagnosticSeverity.Error
                        DiagnosticCode.MissingExpression
                        (fun toks ->
                            if toks.IsEmpty then
                                Expr.Missing
                            else
                                Expr.SkipsTokens(toks)
                        )
                        Expr.pTypedSeqExprBlock

                let! inTok = opt pTrailingIn
                return ModuleFunctionOrValueDefn.Do(attrs, token, expr, inTok)

            | Token.KWLet -> return! pLetBinding attrs token

            | _ -> return! fail (Message "Expected 'let' or 'do'")
        }

[<RequireQualifiedAccess>]
module ModuleDefn =

    let parseBody
        (modTok: SyntaxToken)
        (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>)
        : Parser<_, _, _, _, _> =
        parser {
            let! state = getUserState

            let indent =
                match modTok.Index with
                | TokenIndex.Regular iT -> ParseState.getIndent state iT
                | TokenIndex.Virtual -> 0

            let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
            // Module body at module_col + 1, anchored to the keyword's column
            let! elems = withContextAt OffsideContext.Module (indent + 1) modTok.PositionedToken (opt elementParser)
            let! endTok = nextNonTriviaTokenVirtualIfNot Token.KWEnd
            return ModuleDefnBody(beginTok, elems, endTok)
        }

    let parse (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>) =
        parser {
            let! attrs = opt Attributes.parse
            let! modTok = pModule
            let! access = opt Access.parse
            let! isRec = opt pRec
            // Committed after 'module' — recover ident and '=' with virtuals if missing
            let! ident =
                recoverWithVirtualToken
                    Token.Identifier
                    "Expected module identifier"
                    (nextNonTriviaTokenSatisfiesLMsg (fun t -> t.Token.IsIdentifier) "Module identifier")

            let! eq = recoverWithVirtualToken Token.OpEquality "Expected '=' after module identifier" pEquals

            let! body = parseBody modTok elementParser
            return ModuleDefn(attrs, modTok, access, isRec, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleElem =
    let private refModuleElem = RefParser<ModuleElem<SyntaxToken>, _, _, _, _>()

    // Forward reference setup to handle: ModuleElem -> ModuleDefn -> ModuleElem
    /// Plain parseElems: no recovery. Used in AnonymousModule where backtracking must remain possible.
    let parseElems = many refModuleElem.Parser

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
                                ParseState.addErrorDiagnosticWithError
                                    DiagnosticCode.MissingModuleElem
                                    startTok.PositionedToken
                                    parseErr
                                    reader.State

                            result.Add(ModuleElem.SkipsTokens(ImmutableArray.CreateRange(skipped)))
                        else
                            // Can't skip anything and can't parse — avoid infinite loop
                            keepGoing <- false

            Ok(ImmutableArray.CreateRange(result))

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
                        let builder = ImmutableArray.CreateBuilder(rest.Length + 1)
                        builder.Add(first)
                        builder.AddRange(rest)
                        return ModuleElem.Type(builder.ToImmutable())
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
                // namespace [rec] LongIdent — committed after 'namespace'
                let! isRec = opt pRec
                let! ident = recoverLongIdent "Expected namespace identifier" LongIdent.parse
                let! elems = ModuleElem.parseElemsWithRecovery
                return NamespaceDeclGroup.Named(nsTok, isRec, ident, elems)
        }
