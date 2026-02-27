namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.FSharp.Lexer

[<AutoOpen>]
module internal Keywords =
    type private KWParser =
        Parser<
            SyntaxToken,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         >

    let isAccessModifier (token: SyntaxToken) =
        match token.Token with
        | Token.KWPrivate
        | Token.KWPublic
        | Token.KWInternal -> true
        | _ -> false

    let pMutable: KWParser =
        nextNonTriviaTokenIsL Token.KWMutable "Expected 'mutable' keyword"

    let pWith: KWParser = nextNonTriviaTokenIsL Token.KWWith "Expected 'with' keyword"

    let pThen: KWParser = nextNonTriviaTokenIsL Token.KWThen "Expected 'then' keyword"

    let pElse: KWParser = nextNonTriviaTokenIsL Token.KWElse "Expected 'else' keyword"

    let pDo: KWParser = nextNonTriviaTokenIsL Token.KWDo "Expected 'do' keyword"

    let pAccessModifier: KWParser =
        nextNonTriviaTokenSatisfiesL isAccessModifier "Expected access modifier (private, public or internal)"

    let pEquals: KWParser = nextNonTriviaTokenIsL Token.OpEquality "Expected '=' symbol"

    let pInVirt: KWParser = nextNonTriviaTokenVirtualIfNot Token.KWIn

    let pLet: KWParser =
        // TODO: Where does VirtualLet get inserted?
        nextNonTriviaTokenIsL Token.KWLet "Expected 'let' keyword"

    let pFor: KWParser = nextNonTriviaTokenIsL Token.KWFor "for"

    let pArrowRight: KWParser = nextNonTriviaTokenIsL Token.OpArrowRight "->"

    let pLetBang: KWParser = nextNonTriviaTokenIsL Token.KWLetBang "let!"

    let pUseBang: KWParser = nextNonTriviaTokenIsL Token.KWUseBang "use!"

    let pUse: KWParser = nextNonTriviaTokenIsL Token.KWUse "use"
    let pDoBang: KWParser = nextNonTriviaTokenIsL Token.KWDoBang "do!"

    let pYieldBang: KWParser = nextNonTriviaTokenIsL Token.KWYieldBang "yield!"
    let pReturnBang: KWParser = nextNonTriviaTokenIsL Token.KWReturnBang "return!"

    let pMatchBang: KWParser = nextNonTriviaTokenIsL Token.KWMatchBang "match!"

    let pYield: KWParser = nextNonTriviaTokenIsL Token.KWYield "yield"

    let pReturn: KWParser = nextNonTriviaTokenIsL Token.KWReturn "return"

    let pMatch: KWParser = nextNonTriviaTokenIsL Token.KWMatch "match"
    let pIf: KWParser = nextNonTriviaTokenIsL Token.KWIf "if"
    let pLazy: KWParser = nextNonTriviaTokenIsL Token.KWLazy "lazy"
    let pAssert: KWParser = nextNonTriviaTokenIsL Token.KWAssert "assert"
    let pTry: KWParser = nextNonTriviaTokenIsL Token.KWTry "try"
    let pFinally: KWParser = nextNonTriviaTokenIsL Token.KWFinally "finally"

    let pWhile: KWParser = nextNonTriviaTokenIsL Token.KWWhile "while"
    let pFun: KWParser = nextNonTriviaTokenIsL Token.KWFun "fun"
    let pFunction: KWParser = nextNonTriviaTokenIsL Token.KWFunction "function"

    let pDone: KWParser = nextNonTriviaTokenVirtualIfNot Token.KWDone

    let pIdent: KWParser = nextNonTriviaTokenIsL Token.Identifier "identifier"

    let pToOrDownTo: KWParser =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWTo || t.Token = Token.KWDownto) "to/downto"

    let pLParen: KWParser = nextNonTriviaTokenIsL Token.KWLParen "("
    let pRParen: KWParser = nextNonTriviaTokenIsL Token.KWRParen ")"
    let pOpConcatenate: KWParser = nextNonTriviaTokenIsL Token.OpConcatenate "^"
    let pOpMultiply: KWParser = nextNonTriviaTokenIsL Token.OpMultiply "*"

    // Symbol parsers
    let pComma: KWParser = nextNonTriviaTokenIsL Token.OpComma ","
    let pSemi: KWParser = nextNonTriviaTokenIsL Token.OpSemicolon ";"
    let pColon: KWParser = nextNonTriviaTokenIsL Token.OpColon ":"
    let pDot: KWParser = nextNonTriviaTokenIsL Token.OpDot "."
    let pBar: KWParser = nextNonTriviaTokenIsL Token.OpBar "|"
    let pLessThan: KWParser = nextNonTriviaTokenIsL Token.OpLessThan "<"
    let pGreaterThan: KWParser = nextNonTriviaTokenIsL Token.OpGreaterThan ">"
    let pLBracket: KWParser = nextNonTriviaTokenIsL Token.KWLBracket "["
    let pRBracket: KWParser = nextNonTriviaTokenIsL Token.KWRBracket "]"
    let pLBrace: KWParser = nextNonTriviaTokenIsL Token.KWLBrace "{"
    let pRBrace: KWParser = nextNonTriviaTokenIsL Token.KWRBrace "}"
    let pRange: KWParser = nextNonTriviaTokenIsL Token.OpRange ".."
    let pHash: KWParser = nextNonTriviaTokenIsL Token.KWHash "#"
    let pWildcard: KWParser = nextNonTriviaTokenIsL Token.Wildcard "_"

    // Additional keyword parsers
    let pBegin: KWParser = nextNonTriviaTokenIsL Token.KWBegin "begin"
    let pEnd: KWParser = nextNonTriviaTokenIsL Token.KWEnd "end"
    let pStruct: KWParser = nextNonTriviaTokenIsL Token.KWStruct "struct"
    let pClass: KWParser = nextNonTriviaTokenIsL Token.KWClass "class"
    let pInterface: KWParser = nextNonTriviaTokenIsL Token.KWInterface "interface"
    let pInherit: KWParser = nextNonTriviaTokenIsL Token.KWInherit "inherit"
    let pNew: KWParser = nextNonTriviaTokenIsL Token.KWNew "new"
    // Uncomment when needed
    let pModule: KWParser = nextNonTriviaTokenIsL Token.KWModule "module"
    let pRec: KWParser = nextNonTriviaTokenIsL Token.KWRec "rec"
    let pNamespace: KWParser = nextNonTriviaTokenIsL Token.KWNamespace "namespace"
    let pGlobal: KWParser = nextNonTriviaTokenIsL Token.KWGlobal "global"
    let pOpen: KWParser = nextNonTriviaTokenIsL Token.KWOpen "open"
    let pAs: KWParser = nextNonTriviaTokenIsL Token.KWAs "as"
    let pWhen: KWParser = nextNonTriviaTokenIsL Token.KWWhen "when"
    let pAnd: KWParser = nextNonTriviaTokenIsL Token.KWAnd "and"
    let pOr: KWParser = nextNonTriviaTokenIsL Token.KWOr "or"
    let pStatic: KWParser = nextNonTriviaTokenIsL Token.KWStatic "static"
    let pMember: KWParser = nextNonTriviaTokenIsL Token.KWMember "member"
    let pOverride: KWParser = nextNonTriviaTokenIsL Token.KWOverride "override"
    let pAbstract: KWParser = nextNonTriviaTokenIsL Token.KWAbstract "abstract"
    let pDefault: KWParser = nextNonTriviaTokenIsL Token.KWDefault "default"
    let pVal: KWParser = nextNonTriviaTokenIsL Token.KWVal "val"
    let pType: KWParser = nextNonTriviaTokenIsL Token.KWType "type"
    let pException: KWParser = nextNonTriviaTokenIsL Token.KWException "exception"
    let pDelegate: KWParser = nextNonTriviaTokenIsL Token.KWDelegate "delegate"
    let pOf: KWParser = nextNonTriviaTokenIsL Token.KWOf "of"
    let pNull: KWParser = nextNonTriviaTokenIsL Token.KWNull "null"
    let pElif: KWParser = nextNonTriviaTokenIsL Token.KWElif "elif"
    let pQuestionMark: KWParser = nextNonTriviaTokenIsL Token.OpDynamic "?"
    let pSingleQuote: KWParser = nextNonTriviaTokenIsL Token.KWSingleQuote "'"

    let pQuotedExprLeft: KWParser =
        nextNonTriviaTokenIsL Token.OpQuotationTypedLeft "<@"

    let pQuotedExprRight: KWParser =
        nextNonTriviaTokenIsL Token.OpQuotationTypedRight "@>"

    let pNil: KWParser = nextNonTriviaTokenIsL Token.OpNil "[]"
    let pLArrayBracket: KWParser = nextNonTriviaTokenIsL Token.KWLArrayBracket "[|"
    let pRArrayBracket: KWParser = nextNonTriviaTokenIsL Token.KWRArrayBracket "|]"
    let pTypeTest: KWParser = nextNonTriviaTokenIsL Token.OpTypeTest ":?"
