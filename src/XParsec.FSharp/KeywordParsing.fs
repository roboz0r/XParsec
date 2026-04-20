namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.ParseState

[<AutoOpen>]
module internal Keywords =
    type private KWParser = Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>>

    // Construct the ErrorType.Message statically per binding so each `pX` allocates
    // it exactly once at module load and captures it in the returned closure.
    // Without this, `fail (Message msg)` re-allocated the Message on every failure.
    let inline private mkKW (t: Token) (msg: string) : KWParser =
        let err: ErrorType<PositionedToken, ParseState> = Message msg
        nextNonTriviaTokenIsL t err

    let inline private mkKWPred (pred: SyntaxToken -> bool) (msg: string) : KWParser =
        let err: ErrorType<PositionedToken, ParseState> = Message msg
        nextNonTriviaTokenSatisfiesL pred err

    let inline private mkKWIdent (msg: string) : KWParser =
        let err: ErrorType<PositionedToken, ParseState> = Message msg
        nextNonTriviaIdentifierL err

    let isAccessModifier (token: SyntaxToken) =
        match token.Token with
        | Token.KWPrivate
        | Token.KWPublic
        | Token.KWInternal -> true
        | _ -> false

    let pMutable: KWParser = mkKW Token.KWMutable "Expected 'mutable' keyword"

    let pWith: KWParser = mkKW Token.KWWith "Expected 'with' keyword"

    let pThen: KWParser = mkKW Token.KWThen "Expected 'then' keyword"

    let pElse: KWParser = mkKW Token.KWElse "Expected 'else' keyword"

    let pDo: KWParser = mkKW Token.KWDo "Expected 'do' keyword"

    let pAccessModifier: KWParser =
        mkKWPred isAccessModifier "Expected access modifier (private, public or internal)"

    let pEquals: KWParser = mkKW Token.OpEquality "Expected '=' symbol"

    let pIn: KWParser = mkKW Token.KWIn "Expected 'in' keyword"

    let pLet: KWParser =
        // TODO: Where does VirtualLet get inserted?
        mkKW Token.KWLet "Expected 'let' keyword"

    let pFor: KWParser = mkKW Token.KWFor "for"

    let pArrowRight: KWParser = mkKW Token.OpArrowRight "->"

    let pLetBang: KWParser = mkKW Token.KWLetBang "let!"

    let pUseBang: KWParser = mkKW Token.KWUseBang "use!"

    let pUse: KWParser = mkKW Token.KWUse "use"
    let pDoBang: KWParser = mkKW Token.KWDoBang "do!"

    let pYieldBang: KWParser = mkKW Token.KWYieldBang "yield!"
    let pReturnBang: KWParser = mkKW Token.KWReturnBang "return!"

    let pMatchBang: KWParser = mkKW Token.KWMatchBang "match!"

    let pYield: KWParser = mkKW Token.KWYield "yield"

    let pReturn: KWParser = mkKW Token.KWReturn "return"

    let pMatch: KWParser = mkKW Token.KWMatch "match"
    let pIf: KWParser = mkKW Token.KWIf "if"
    let pLazy: KWParser = mkKW Token.KWLazy "lazy"
    let pAssert: KWParser = mkKW Token.KWAssert "assert"
    let pTry: KWParser = mkKW Token.KWTry "try"
    let pFinally: KWParser = mkKW Token.KWFinally "finally"

    let pWhile: KWParser = mkKW Token.KWWhile "while"
    let pFun: KWParser = mkKW Token.KWFun "fun"
    let pFunction: KWParser = mkKW Token.KWFunction "function"

    let pDone: KWParser = mkKW Token.KWDone "done"

    let pIdent: KWParser = mkKWIdent "identifier"

    let pToOrDownTo: KWParser =
        mkKWPred (fun t -> t.Token = Token.KWTo || t.Token = Token.KWDownto) "to/downto"

    let pLParen: KWParser = mkKW Token.KWLParen "("
    let pRParen: KWParser = mkKW Token.KWRParen ")"
    let pOpConcatenate: KWParser = mkKW Token.OpConcatenate "^"
    let pOpMultiply: KWParser = mkKW Token.OpMultiply "*"

    // Symbol parsers
    let pComma: KWParser = mkKW Token.OpComma ","
    let pSemi: KWParser = mkKW Token.OpSemicolon ";"
    let pColon: KWParser = mkKW Token.OpColon ":"
    let pDot: KWParser = mkKW Token.OpDot "."
    let pBar: KWParser = mkKW Token.OpBar "|"

    let pLessThan: KWParser =
        // Hoisted err — re-used across every invocation.
        let err: ErrorType<PositionedToken, ParseState> = Message "<"

        parser {
            let! state = getUserState
            return! nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpLessThan && tokenStringIs "<" t state) err
        }

    let pGreaterThan: KWParser =
        let err: ErrorType<PositionedToken, ParseState> = Message ">"

        parser {
            let! state = getUserState

            return!
                nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpGreaterThan && tokenStringIs ">" t state) err
        }

    let pLBracket: KWParser = mkKW Token.KWLBracket "["
    let pRBracket: KWParser = mkKW Token.KWRBracket "]"
    let pLBrace: KWParser = mkKW Token.KWLBrace "{"
    let pRBrace: KWParser = mkKW Token.KWRBrace "}"
    let pRange: KWParser = mkKW Token.OpRange ".."
    let pHash: KWParser = mkKW Token.KWHash "#"
    let pWildcard: KWParser = mkKW Token.Wildcard "_"

    // Additional keyword parsers
    let pBegin: KWParser = mkKW Token.KWBegin "begin"
    let pEnd: KWParser = mkKW Token.KWEnd "end"
    let pStruct: KWParser = mkKW Token.KWStruct "struct"
    let pClass: KWParser = mkKW Token.KWClass "class"
    let pInterface: KWParser = mkKW Token.KWInterface "interface"
    let pInherit: KWParser = mkKW Token.KWInherit "inherit"
    let pNew: KWParser = mkKW Token.KWNew "new"
    // Uncomment when needed
    let pModule: KWParser = mkKW Token.KWModule "module"
    let pRec: KWParser = mkKW Token.KWRec "rec"
    let pNamespace: KWParser = mkKW Token.KWNamespace "namespace"
    let pGlobal: KWParser = mkKW Token.KWGlobal "global"
    let pOpen: KWParser = mkKW Token.KWOpen "open"
    let pAs: KWParser = mkKW Token.KWAs "as"
    let pWhen: KWParser = mkKW Token.KWWhen "when"
    let pAnd: KWParser = mkKW Token.KWAnd "and"
    let pOr: KWParser = mkKW Token.KWOr "or"
    let pStatic: KWParser = mkKW Token.KWStatic "static"
    let pInline: KWParser = mkKW Token.KWInline "inline"
    let pMember: KWParser = mkKW Token.KWMember "member"
    let pOverride: KWParser = mkKW Token.KWOverride "override"
    let pAbstract: KWParser = mkKW Token.KWAbstract "abstract"
    let pDefault: KWParser = mkKW Token.KWDefault "default"
    let pVal: KWParser = mkKW Token.KWVal "val"
    let pType: KWParser = mkKW Token.KWType "type"
    let pException: KWParser = mkKW Token.KWException "exception"
    let pDelegate: KWParser = mkKW Token.KWDelegate "delegate"
    let pOf: KWParser = mkKW Token.KWOf "of"
    let pNull: KWParser = mkKW Token.KWNull "null"
    let pElif: KWParser = mkKW Token.KWElif "elif"
    let pQuestionMark: KWParser = mkKW Token.OpDynamic "?"
    let pSingleQuote: KWParser = mkKW Token.KWSingleQuote "'"

    let pQuotationTypedLeft: KWParser = mkKW Token.OpQuotationTypedLeft "<@"

    let pQuotationTypedRight: KWParser = mkKW Token.OpQuotationTypedRight "@>"

    let pQuotationUntypedLeft: KWParser = mkKW Token.OpQuotationUntypedLeft "<@@"

    let pQuotationUntypedRight: KWParser = mkKW Token.OpQuotationUntypedRight "@@>"

    //let pNil: KWParser = mkKW Token.OpNil "[]"
    let pLArrayBracket: KWParser = mkKW Token.KWLArrayBracket "[|"
    let pRArrayBracket: KWParser = mkKW Token.KWRArrayBracket "|]"
    let pLBraceBar: KWParser = mkKW Token.KWLBraceBar "{|"
    let pRBraceBar: KWParser = mkKW Token.KWRBraceBar "|}"
    let pTypeTest: KWParser = mkKW Token.OpTypeTest ":?"
