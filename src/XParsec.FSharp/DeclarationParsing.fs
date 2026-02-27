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

    // Helper to distinguish LetValue vs LetFunction based on lookahead or backtracking
    // Assuming FunctionDefn/ValueDefn handle the body after 'let'
    let private pLetBinding attrs letTok =
        parser {
            let! isRec = opt pRec

            match isRec with
            | ValueSome recTok ->
                // let rec ...
                // Parses a list of 'and' connected definitions
                let! defns, _ = sepBy1 FunctionOrValueDefn.parse pAnd
                return ModuleFunctionOrValueDefn.LetRec(attrs, letTok, ValueSome recTok, List.ofSeq defns)

            | ValueNone ->
                // let ...
                // Try parsing as a function (params present), if that fails/backtracks, try value.
                // Note: In a real parser, we might peek for parameters to decide.
                return!
                    choiceL
                        [
                            parser {
                                let! fn = FunctionDefn.parse
                                return ModuleFunctionOrValueDefn.LetFunction(attrs, letTok, fn)
                            }
                            parser {
                                let! valDef = ValueDefn.parse
                                return ModuleFunctionOrValueDefn.LetValue(attrs, letTok, valDef)
                            }
                        ]
                        ""
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
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Module identifier"
            let! eq = pEquals
            let! body = parseBody elementParser
            return ModuleDefn(attrs, modTok, access, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleElem =
    let private refModuleElem = RefParser<ModuleElem<SyntaxToken>, _, _, _, _>()

    // Forward reference setup to handle: ModuleElem -> ModuleDefn -> ModuleElem
    let parseElems = many refModuleElem.Parser |>> List.ofSeq

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
                    (ModuleDefn.parse parseElems) |>> ModuleElem.Module

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

            // Check for 'global' keyword
            let! globalTok = opt pGlobal

            match globalTok with
            | ValueSome gTok ->
                // namespace global
                let! elems = ModuleElem.parseElems
                return NamespaceDeclGroup.Global(nsTok, gTok, elems)

            | ValueNone ->
                // namespace LongIdent
                let! ident = LongIdent.parse
                let! elems = ModuleElem.parseElems
                return NamespaceDeclGroup.Named(nsTok, ident, elems)
        }
