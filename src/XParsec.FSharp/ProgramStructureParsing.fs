namespace XParsec.FSharp.Parser

open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

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
            let! longIdent = LongIdent.parse
            // Distinguish named module (module Foo.Bar <elems>) from module abbreviation (module X = Y.Z)
            do! notFollowedByEquals
            let! elems = ModuleElem.parseElems
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

    let parse =
        choiceL
            [
                ImplementationFile.parse .>> pEof |>> FSharpAst.ImplementationFile
                Expr.parse .>> pEof
                |>> (fun e -> FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment [ ModuleElem.Expression e ]))
            ]
            "FSharpAst"
