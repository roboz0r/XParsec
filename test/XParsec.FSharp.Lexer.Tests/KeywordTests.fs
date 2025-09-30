module XParsec.FSharp.Lexer.Tests.KeywordTests

open System
open System.IO

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing

let keywords =
    [
        // IsKeyword
        "abstract", Token.KWAbstract
        "and", Token.KWAnd
        "and!", Token.KWAndBang
        "as", Token.KWAs
        "base", Token.KWBase
        "begin", Token.KWBegin
        "class", Token.KWClass
        "const", Token.KWConst
        "default", Token.KWDefault
        "delegate", Token.KWDelegate
        "do", Token.KWDo
        "do!", Token.KWDoBang
        "done", Token.KWDone
        "downcast", Token.KWDowncast
        "downto", Token.KWDownto
        "elif", Token.KWElif
        "else", Token.KWElse
        "end", Token.KWEnd
        "event", Token.KWEvent
        "exception", Token.KWException
        "extern", Token.KWExtern
        "external", Token.KWExternal
        "false", Token.KWFalse
        "finally", Token.KWFinally
        "fixed", Token.KWFixed
        "for", Token.KWFor
        "global", Token.KWGlobal
        "in", Token.KWIn
        "inherit", Token.KWInherit
        "inline", Token.KWInline
        "interface", Token.KWInterface
        "internal", Token.KWInternal
        "module", Token.KWModule
        "mutable", Token.KWMutable
        "namespace", Token.KWNamespace
        "new", Token.KWNew
        "of", Token.KWOf
        "open", Token.KWOpen
        "override", Token.KWOverride
        "private", Token.KWPrivate
        "public", Token.KWPublic
        "rec", Token.KWRec
        "return", Token.KWReturn
        "return!", Token.KWReturnBang
        "select", Token.KWContextualSelect
        "sig", Token.KWSig
        "static", Token.KWStatic
        "struct", Token.KWStruct
        "then", Token.KWThen
        "to", Token.KWTo
        "true", Token.KWTrue
        "try", Token.KWTry
        "type", Token.KWType
        "val", Token.KWVal
        "void", Token.KWVoid
        "when", Token.KWWhen
        "with", Token.KWWith
        "'", Token.SingleQuote

        // IsKeywordOperator
        "assert", Token.KWAssert
        "fun", Token.KWFun
        "function", Token.KWFunction
        "if", Token.KWIf
        "lazy", Token.KWLazy
        "let", Token.KWLet
        "let!", Token.KWLetBang
        "match", Token.KWMatch
        "match!", Token.KWMatchBang
        "upcast", Token.KWUpcast
        "use", Token.KWUse
        "use!", Token.KWUseBang
        "while", Token.KWWhile
        "yield", Token.KWYield
        "yield!", Token.KWYieldBang
        "|", Token.OpBar
        "->", Token.OpArrowRight
        "<-", Token.OpArrowLeft
        ".", Token.OpDot
        ":", Token.OpColon
        ",", Token.OpComma
        "(", Token.OpParenLeft
        ")", Token.OpParenRight
        "[", Token.OpBracketLeft
        "]", Token.OpBracketRight
        "[<", Token.OpAttributeBracketLeft
        ">]", Token.OpAttributeBracketRight
        "[|", Token.OpArrayBracketLeft
        "|]", Token.OpArrayBracketRight
        "{", Token.OpBraceLeft
        "}", Token.OpBraceRight
        "#", Token.OpHash
        ":?", Token.OpTypeTest
        ":>", Token.OpUpcast
        ":?>", Token.OpDowncast
        "..", Token.OpRange
        ";;", Token.OpDoubleSemicolon
        ";", Token.OpSemicolon
        "??", Token.OpDoubleQuestion
        "[]", Token.OpNil
        "!", Token.OpDereference
        "<@", Token.OpQuotationTypedLeft
        "@>", Token.OpQuotationTypedRight
        "<@@", Token.OpQuotationUntypedLeft
        "@@>", Token.OpQuotationUntypedRight
        ".()<-", Token.OpIndexSetParenIdentifier
        ".()", Token.OpIndexGetParenIdentifier
        ".[", Token.OpIndexLeft

        // IsKeywordOperatorIdentifier & IsKeywordIdentifier
        "mod", Token.KWMod
        "not", Token.KWNot
        "null", Token.KWNull
        "or", Token.KWOr
        "::", Token.OpCons
        "_", Token.Wildcard
        "()", Token.Unit

        // IsReservedKeyword
        "break", Token.KWReservedBreak
        "checked", Token.KWReservedChecked
        "component", Token.KWReservedComponent
        "constraint", Token.KWReservedConstraint
        "continue", Token.KWReservedContinue
        "fori", Token.KWReservedFori
        "include", Token.KWReservedInclude
        "mixin", Token.KWReservedMixin
        "parallel", Token.KWReservedParallel
        "params", Token.KWReservedParams
        "process", Token.KWReservedProcess
        "protected", Token.KWReservedProtected
        "pure", Token.KWReservedPure
        "sealed", Token.KWReservedSealed
        "tailcall", Token.KWReservedTailcall
        "trait", Token.KWReservedTrait
        "virtual", Token.KWReservedVirtual

        // IsReservedKeywordOperator
        "~", Token.KWReservedTwiddle

        // IsDeprecatedKeywordOperatorIdentifier & IsDeprecatedKeywordOperator
        "asr", Token.KWAsr
        "land", Token.KWLand
        "lor", Token.KWLor
        "lsl", Token.KWLsl
        "lsr", Token.KWLsr
        "lxor", Token.KWLxor
        ":=", Token.OpAssignment
        ".[]<-", Token.OpIndexSetIdentifier
        ".[]", Token.OpIndexGetIdentifier
        ".[,]<-", Token.OpIndexSet2Identifier
        ".[,]", Token.OpIndexGet2Identifier
        ".[,,]<-", Token.OpIndexSet3Identifier
        ".[,,]", Token.OpIndexGet3Identifier
        ".[,,,]<-", Token.OpIndexSet4Identifier
        ".[,,,]", Token.OpIndexGet4Identifier
        ".(", Token.OpIndexLeftParen
    ]

[<Tests>]
let tests =
    testList
        "KeywordTests"
        [
            for (kw, tok) in keywords do
                test $"Lexing keyword '{kw}'" {
                    // printfn "Testing keyword: '%s' (Token: %O)" kw tok
                    let expected =
                        [ PositionedToken.Create(tok, 0); PositionedToken.Create(Token.EOF, kw.Length) ]

                    testLexed kw expected
                }
        ]
