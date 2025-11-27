namespace XParsec.FSharp.Lexer

open System

module TokenRepresentation =
    [<Literal>]
    let TokenMask = 0xFFFFUL // Lower 16 bits for token ID and flags

    [<Literal>]
    let IsIdentifier = 0b00000_0001_0000000us

    [<Literal>]
    let IsLiteral = 0b00000_0010_0000000us

    [<Literal>]
    let IsOperator = 0b00000_0100_0000000us

    [<Literal>]
    let IsKeyword = 0b00000_1000_0000000us

    [<Literal>]
    let IsDeprecated = 0b00001_0000_0000000us

    [<Literal>]
    let IsReserved = 0b00010_0000_0000000us

    [<Literal>]
    let IsInvalid = 0b00100_0000_0000000us

    [<Literal>]
    let InBlockComment = 0b01000_0000_0000000us

    [<Literal>]
    let InOCamlBlockComment = 0b10000_0000_0000000us

    // **Bit 6's meaning depends on the flags above.**
    // For literals, it indicates textual vs. numeric literal
    [<Literal>]
    let IsText = 0b00000_0000_1000000us // For literals, indicates a text literal (string, char)

    [<Literal>]
    let internal IsTextLiteral = IsLiteral ||| IsText // For literals, indicates a text literal (string, char)

    [<Literal>]
    let internal IsNumericLiteral = IsLiteral // For literals, indicates a numeric literal (int, float, etc.)

    [<Literal>]
    let NumericBaseMask = 0b00110000us // Bits 5-4

    [<Literal>]
    let NumericTypeIDMask = 0b00001111us // Bits 3-0

    [<Literal>]
    let TextTypeIDMask = 0b00111111us // Bits 5-0

    [<Literal>]
    let internal NumericBaseHex = 0b00010000us

    [<Literal>]
    let internal NumericBaseOctal = 0b00100000us

    [<Literal>]
    let internal NumericBaseBinary = 0b00110000us

    // This bit indicates that the token was synthesized by the parser
    // and does not exist in the original source text.
    [<Literal>]
    let IsVirtual = 0b00000_0000_1000000us // Bit 6


    // Operator flags
    // The lower 7 bits (0-6) are reserved for the token ID (up to 128 tokens)
    // But F# allows arbitrary custom operators
    // So, for operators we use these bits to store operator properties for all custom operators
    [<Literal>]
    let CanBePrefix = 0b00000_0000_1000000us // Can be used as a prefix operator (e.g., unary minus)

    // Bits 5-0: A numeric field for Precedence
    [<Literal>]
    let PrecedenceMask = 0b00000_0000_0111111us // Bits 5-0

    // --- General Masks ---
    [<Literal>]
    let TokenIdMask = 0b00000_0000_1111111us // The lower 7 bits

    [<Literal>]
    let IsReservedIdentifier = IsIdentifier ||| IsReserved

    [<Literal>]
    let IsKeywordIdentifier = IsIdentifier ||| IsKeyword

    [<Literal>]
    let IsReservedKeyword = IsKeyword ||| IsReserved

    [<Literal>]
    let IsReservedKeywordIdentifier = IsIdentifier ||| IsKeyword ||| IsReserved

    [<Literal>]
    let IsReservedKeywordOperator = IsOperator ||| IsKeyword ||| IsReserved

    [<Literal>]
    let IsInvalidIdentifier = IsIdentifier ||| IsInvalid

    [<Literal>]
    let IsKeywordOperator = IsKeyword ||| IsOperator

    [<Literal>]
    let IsReservedOperator = IsOperator ||| IsReserved

    [<Literal>]
    let IsInvalidOperator = IsOperator ||| IsInvalid

    [<Literal>]
    let IsDeprecatedOperator = IsDeprecated ||| IsOperator

    [<Literal>]
    let IsDeprecatedKeywordOperator = IsDeprecated ||| IsKeyword ||| IsOperator

    [<Literal>]
    let IsKeywordOperatorIdentifier = IsIdentifier ||| IsKeyword ||| IsOperator

    [<Literal>]
    let IsReservedKeywordOperatorIdentifier =
        IsIdentifier ||| IsKeyword ||| IsOperator ||| IsReserved

    [<Literal>]
    let IsDeprecatedKeywordOperatorIdentifier =
        IsIdentifier ||| IsKeyword ||| IsOperator ||| IsDeprecated

    [<Literal>]
    let IsInBlockComment = InBlockComment ||| InOCamlBlockComment


    module internal KW =
        [<Literal>]
        let Abstract = 5us

        [<Literal>]
        let And = 6us

        [<Literal>]
        let As = 8us

        [<Literal>]
        let Assert = 10us

        [<Literal>]
        let Base = 11us

        [<Literal>]
        let Begin = 12us

        [<Literal>]
        let Class = 15us

        [<Literal>]
        let Const = 17us

        [<Literal>]
        let Default = 20us

        [<Literal>]
        let Delegate = 21us

        [<Literal>]
        let Do = 22us

        [<Literal>]
        let Done = 24us

        [<Literal>]
        let Downcast = 25us

        [<Literal>]
        let Downto = 26us

        [<Literal>]
        let Elif = 27us

        [<Literal>]
        let Else = 28us

        [<Literal>]
        let End = 29us

        [<Literal>]
        let Event = 30us

        [<Literal>]
        let Exception = 31us

        [<Literal>]
        let Extern = 32us

        [<Literal>]
        let External = 33us

        [<Literal>]
        let False = 34us

        [<Literal>]
        let Finally = 35us

        [<Literal>]
        let Fixed = 36us

        [<Literal>]
        let For = 37us

        [<Literal>]
        let Fun = 38us

        [<Literal>]
        let Function = 39us

        [<Literal>]
        let Global = 40us

        [<Literal>]
        let If = 41us

        [<Literal>]
        let In = 42us

        [<Literal>]
        let Inherit = 44us

        [<Literal>]
        let Inline = 45us

        [<Literal>]
        let Interface = 46us

        [<Literal>]
        let Internal = 47us

        [<Literal>]
        let Lazy = 49us

        [<Literal>]
        let Let = 50us

        [<Literal>]
        let Match = 56us

        [<Literal>]
        let Member = 58us

        [<Literal>]
        let Mod = 60us

        [<Literal>]
        let Module = 61us

        [<Literal>]
        let Mutable = 62us

        [<Literal>]
        let Namespace = 63us

        [<Literal>]
        let New = 64us

        [<Literal>]
        let Not = 65us

        [<Literal>]
        let Null = 66us

        [<Literal>]
        let Of = 67us

        [<Literal>]
        let Open = 68us

        [<Literal>]
        let Or = 69us

        [<Literal>]
        let Override = 70us

        [<Literal>]
        let Private = 72us

        [<Literal>]
        let Public = 75us

        [<Literal>]
        let Rec = 77us

        [<Literal>]
        let Return = 78us

        [<Literal>]
        let ContextualSelect = 81us

        [<Literal>]
        let Sig = 82us

        [<Literal>]
        let Static = 83us

        [<Literal>]
        let Struct = 84us

        [<Literal>]
        let Then = 86us

        [<Literal>]
        let To = 87us

        [<Literal>]
        let True = 89us

        [<Literal>]
        let Try = 90us

        [<Literal>]
        let Type = 91us

        [<Literal>]
        let Upcast = 92us

        [<Literal>]
        let Use = 93us

        [<Literal>]
        let Val = 95us

        [<Literal>]
        let Void = 97us

        [<Literal>]
        let When = 98us

        [<Literal>]
        let While = 99us

        [<Literal>]
        let With = 100us

        [<Literal>]
        let Yield = 101us

        [<Literal>]
        let ReservedBreak = 103us

        [<Literal>]
        let ReservedChecked = 104us

        [<Literal>]
        let ReservedComponent = 105us

        [<Literal>]
        let ReservedConstraint = 106us

        [<Literal>]
        let ReservedContinue = 107us

        [<Literal>]
        let ReservedFori = 108us

        [<Literal>]
        let ReservedInclude = 109us

        [<Literal>]
        let ReservedMixin = 110us

        [<Literal>]
        let ReservedParallel = 111us

        [<Literal>]
        let ReservedParams = 112us

        [<Literal>]
        let ReservedProcess = 113us

        [<Literal>]
        let ReservedProtected = 114us

        [<Literal>]
        let ReservedPure = 115us

        [<Literal>]
        let ReservedSealed = 116us

        [<Literal>]
        let ReservedTailcall = 117us

        [<Literal>]
        let ReservedTrait = 118us

        [<Literal>]
        let ReservedVirtual = 119us

        [<Literal>]
        let ReservedTwiddle = 120us

        [<Literal>]
        let ReservedBacktick = 121us


        [<Literal>]
        let Asr = 122us

        [<Literal>]
        let Land = 123us

        [<Literal>]
        let Lor = 124us

        [<Literal>]
        let Lsl = 125us

        [<Literal>]
        let Lsr = 126us

        [<Literal>]
        let Lxor = 127us

        [<Literal>]
        let AndBang = 7us

        [<Literal>]
        let DoBang = 23us

        [<Literal>]
        let LetBang = 51us

        [<Literal>]
        let MatchBang = 57us

        [<Literal>]
        let ReturnBang = 79us

        [<Literal>]
        let UseBang = 94us

        [<Literal>]
        let YieldBang = 102us


// 4.4.2 Precedence of Symbolic Operators and Pattern/Expression Constructs

module internal Precedence =
    // Used to define the precedence in the Token enum
    [<Literal>]
    let As = 1us

    [<Literal>]
    let When = 2us

    [<Literal>]
    let Pipe = 3us

    [<Literal>]
    let Semicolon = 4us

    [<Literal>]
    let Let = 5us

    [<Literal>]
    let Function = 6us

    [<Literal>]
    let If = 7us

    [<Literal>]
    let Not = 8us

    [<Literal>]
    let Arrow = 9us

    [<Literal>]
    let Assignment = 10us

    [<Literal>]
    let Comma = 11us

    [<Literal>]
    let BooleanOr = 12us

    [<Literal>]
    let BooleanAnd = 13us

    [<Literal>]
    let Cast = 14us

    [<Literal>]
    let LogicalAndBitwise = 15us

    [<Literal>]
    let Concatenate = 16us

    [<Literal>]
    let Cons = 17us

    [<Literal>]
    let TypeTest = 18us

    [<Literal>]
    let InfixAdd = 19us

    [<Literal>]
    let InfixMultiply = 20us

    [<Literal>]
    let Exponentiation = 21us

    [<Literal>]
    let Application = 22us

    [<Literal>]
    let PatternMatchBar = 23us

    [<Literal>]
    let Prefix = 24us

    [<Literal>]
    let Dot = 25us

    [<Literal>]
    let HighApplication = 26us

    [<Literal>]
    let HighTypeApplication = 27us

/// Represents the precedence levels of F# operators, from lowest to highest.
/// The integer values match the official F# documentation.
type PrecedenceLevel =
    | As = 1
    | When = 2
    | Pipe = 3
    | Semicolon = 4
    | Let = 5
    | Function = 6
    | If = 7
    | Not = 8
    | Arrow = 9
    | Assignment = 10
    | Comma = 11
    | LogicalOr = 12
    | LogicalAnd = 13
    | Cast = 14
    | LogicalAndBitwise = 15
    | Caret = 16
    | Cons = 17
    | TypeTest = 18
    | InfixAdd = 19
    | InfixMultiply = 20
    | Power = 21
    | Application = 22
    | PatternMatchBar = 23
    | Prefix = 24
    | Dot = 25
    | HighApplication = 26
    | HighTypeApplication = 27

[<Struct; RequireQualifiedAccess>]
type Associativity =
    | Non
    | Right
    | Left

// --- Numeric Literal Specifics (Bits 5-0, when Bit 6 is 0) ---
type NumericBase =
    | Decimal = 0 // 123
    | Hex = 1 // 0x123
    | Octal = 2 // 0o123
    | Binary = 3 // 0b101

type NumericKind =
    | SByte = 2
    | Byte = 3
    | Int16 = 4
    | UInt16 = 5
    | Int32 = 6
    | UInt32 = 7
    | Int64 = 8
    | UInt64 = 9
    | NativeInt = 10
    | UNativeInt = 11
    | IEEE32 = 12
    | IEEE64 = 13
    | Decimal = 14
    | BigInteger = 15
    | Reserved = 16

open TokenRepresentation

type Token =
    // Should not be used, only for default initialization
    | None = 0us
    // 3.1, 3.2 Whitespace and control tokens
    | EOF = (1us) // End of file/input
    | Newline = (2us) // \n or \r\n
    | Whitespace = (3us) // Spaces only
    | Tab = (4us) // \t
    | LineComment = (5us) // // line comment
    | BlockCommentStart = (6us) // (* start of block comment
    | BlockCommentEnd = (7us) // *) end of block comment
    | Indent = (8us) // Indentation (whitespace at start of line)

    // 3.3 Conditional Compilation
    | IfDirective = (9us) // #if
    | ElseDirective = (10us) // #else
    | EndIfDirective = (11us) // #endif

    // 3.8.4 Shebang
    | Shebang = (12us) // #!/bin/usr/env fsharpi --exec

    // 3.9 Line Directives
    | LineDirective = (13us) // #line
    | LineIntDirective = (14us) // #line
    // 12.4 Compiler Directives
    | NoWarnDirective = (15us) // #nowarn
    | WarnOnDirective = (16us) // #warnon
    | ReferenceDirective = (17us) // #r, #reference
    | IncludePathDirective = (18us) // #I, #Include
    | LoadDirective = (19us) // #load
    | TimeDirective = (20us) // #time
    | HelpDirective = (21us) // #help
    | QuitDirective = (22us) // #q, #quit
    // 19.4 File Extensions and Lexical Matters
    | IndentDirective = (23us) // #indent
    | InvalidDirective = (IsInvalid ||| 24us) // Any other invalid directive starting with #

    // 19.1 ML Compatibility
    | KWStartFSharpBlockComment = 100us // "(*IF-FSHARP" | "(*F#" starts a block comment ignored by F#
    | KWEndFSharpBlockComment = 101us // "ENDIF-FSHARP*)" | "F#*)" ends a block comment ignored by F#
    | KWStartOCamlBlockComment = 102us // "(*IF-CAML*)" | "(*IF-OCAML*)" starts a block comment ignored by OCaml
    | KWEndOCamlBlockComment = 103us // "ENDIF-CAML*)" | "ENDIF-OCAML*)" ends a block comment ignored by OCaml

    | OtherUnlexed = (127us) // Any other unlexed character, when the lexer is done we shouldn't have any of these left

    // 3.4 Identifiers
    | Identifier = (IsIdentifier ||| 1us)
    | BacktickedIdentifier = (IsIdentifier ||| 2us)
    | ReservedIdentifierBang = (IsReservedIdentifier ||| 3us)
    | ReservedIdentifierHash = (IsReservedIdentifier ||| 4us)
    | UnterminatedBacktickedIdentifier = (IsInvalidIdentifier ||| 5us)
    | TypeParameter = (IsIdentifier ||| 6us)

    // 3.11 Identifier Replacements
    | SourceDirectoryIdentifier = (IsIdentifier ||| 7us)
    | SourceFileIdentifier = (IsIdentifier ||| 8us)
    | LineIdentifier = (IsIdentifier ||| 9us)

    // 3.4 Keywords and Keyword Identifiers
    | KWAbstract = (IsKeyword ||| KW.Abstract)
    | KWAnd = (IsKeyword ||| KW.And)
    | KWAs = (IsKeyword ||| KW.As)
    | KWAssert = (IsKeywordOperator ||| KW.Assert)
    | KWBase = (IsKeyword ||| KW.Base)
    | KWBegin = (IsKeyword ||| KW.Begin)
    | KWClass = (IsKeyword ||| KW.Class)
    | KWConst = (IsKeyword ||| KW.Const)
    | KWDefault = (IsKeyword ||| KW.Default)
    | KWDelegate = (IsKeyword ||| KW.Delegate)
    | KWDo = (IsKeyword ||| KW.Do)
    | KWDone = (IsKeyword ||| KW.Done)
    | KWDowncast = (IsKeyword ||| KW.Downcast)
    | KWDownto = (IsKeyword ||| KW.Downto)
    | KWElif = (IsKeyword ||| KW.Elif)
    | KWElse = (IsKeyword ||| KW.Else)
    | KWEnd = (IsKeyword ||| KW.End)
    | KWEvent = (IsKeyword ||| KW.Event)
    | KWException = (IsKeyword ||| KW.Exception)
    | KWExtern = (IsKeyword ||| KW.Extern)
    | KWExternal = (IsKeyword ||| KW.External)
    | KWFalse = (IsKeyword ||| KW.False)
    | KWFinally = (IsKeyword ||| KW.Finally)
    | KWFixed = (IsKeyword ||| KW.Fixed)
    | KWFor = (IsKeyword ||| KW.For)
    | KWFun = (IsKeywordOperator ||| KW.Fun)
    | KWFunction = (IsKeywordOperator ||| KW.Function)
    | KWGlobal = (IsKeyword ||| KW.Global)
    | KWIf = (IsKeywordOperator ||| KW.If)
    | KWIn = (IsKeyword ||| KW.In)
    | KWInherit = (IsKeyword ||| KW.Inherit)
    | KWInline = (IsKeyword ||| KW.Inline)
    | KWInterface = (IsKeyword ||| KW.Interface)
    | KWInternal = (IsKeyword ||| KW.Internal)
    | KWLazy = (IsKeywordOperator ||| KW.Lazy)
    | KWLet = (IsKeywordOperator ||| KW.Let)
    | KWMatch = (IsKeywordOperator ||| KW.Match)
    | KWMember = (IsKeyword ||| KW.Member)
    | KWMod = (IsKeywordOperatorIdentifier ||| KW.Mod)
    | KWModule = (IsKeyword ||| KW.Module)
    | KWMutable = (IsKeyword ||| KW.Mutable)
    | KWNamespace = (IsKeyword ||| KW.Namespace)
    | KWNew = (IsKeyword ||| KW.New)
    | KWNot = (IsKeywordOperatorIdentifier ||| KW.Not)
    | KWNull = (IsKeywordIdentifier ||| KW.Null)
    | KWOf = (IsKeyword ||| KW.Of)
    | KWOpen = (IsKeyword ||| KW.Open)
    | KWOr = (IsKeywordOperatorIdentifier ||| KW.Or)
    | KWOverride = (IsKeyword ||| KW.Override)
    | KWPrivate = (IsKeyword ||| KW.Private)
    | KWPublic = (IsKeyword ||| KW.Public)
    | KWRec = (IsKeyword ||| KW.Rec)
    | KWReturn = (IsKeyword ||| KW.Return)
    /// Used in query expressions to specify what fields or columns to extract.
    /// Note that this is a contextual keyword, which means that it is not actually
    /// a reserved word and it only acts like a keyword in appropriate context.
    | KWContextualSelect = (IsKeywordIdentifier ||| KW.ContextualSelect)
    | KWSig = (IsKeyword ||| KW.Sig)
    | KWStatic = (IsKeyword ||| KW.Static)
    | KWStruct = (IsKeyword ||| KW.Struct)
    | KWThen = (IsKeyword ||| KW.Then)
    | KWTo = (IsKeyword ||| KW.To)
    | KWTrue = (IsKeyword ||| KW.True)
    | KWTry = (IsKeyword ||| KW.Try)
    | KWType = (IsKeyword ||| KW.Type)
    | KWUpcast = (IsKeywordOperator ||| KW.Upcast)
    | KWUse = (IsKeywordOperator ||| KW.Use)
    | KWVal = (IsKeyword ||| KW.Val)
    | KWVoid = (IsKeyword ||| KW.Void)
    | KWWhen = (IsKeyword ||| KW.When)
    | KWWhile = (IsKeywordOperator ||| KW.While)
    | KWWith = (IsKeyword ||| KW.With)
    | KWYield = (IsKeywordOperator ||| KW.Yield)
    | KWReservedBreak = (IsReservedKeyword ||| KW.ReservedBreak)
    | KWReservedChecked = (IsReservedKeyword ||| KW.ReservedChecked)
    | KWReservedComponent = (IsReservedKeyword ||| KW.ReservedComponent)
    | KWReservedConstraint = (IsReservedKeyword ||| KW.ReservedConstraint)
    | KWReservedContinue = (IsReservedKeyword ||| KW.ReservedContinue)
    | KWReservedFori = (IsReservedKeyword ||| KW.ReservedFori)
    | KWReservedInclude = (IsReservedKeyword ||| KW.ReservedInclude)
    | KWReservedMixin = (IsReservedKeyword ||| KW.ReservedMixin)
    | KWReservedParallel = (IsReservedKeyword ||| KW.ReservedParallel)
    | KWReservedParams = (IsReservedKeyword ||| KW.ReservedParams)
    | KWReservedProcess = (IsReservedKeyword ||| KW.ReservedProcess)
    | KWReservedProtected = (IsReservedKeyword ||| KW.ReservedProtected)
    | KWReservedPure = (IsReservedKeyword ||| KW.ReservedPure)
    | KWReservedSealed = (IsReservedKeyword ||| KW.ReservedSealed)
    | KWReservedTailcall = (IsReservedKeyword ||| KW.ReservedTailcall)
    | KWReservedTrait = (IsReservedKeyword ||| KW.ReservedTrait)
    | KWReservedVirtual = (IsReservedKeyword ||| KW.ReservedVirtual)
    | KWReservedTwiddle = (IsReservedKeywordOperator ||| KW.ReservedTwiddle) // ~ symbol is reserved
    | KWReservedBacktick = (IsReservedKeyword ||| KW.ReservedBacktick) // ` symbol is reserved


    // 15.1.2 Inserted Tokens
    // virtual keywords inserted by the parser
    | VirtualIn = (IsVirtual ||| IsKeyword ||| KW.In)
    | VirtualDone = (IsVirtual ||| IsKeyword ||| KW.Done)
    | VirtualBegin = (IsVirtual ||| IsKeyword ||| KW.Begin)
    | VirtualEnd = (IsVirtual ||| IsKeyword ||| KW.End)
    | VirtualSep = (IsVirtual ||| IsKeyword ||| 1024us)
    | VirtualApp = (IsVirtual ||| IsKeyword ||| 1025us)
    | VirtualTyApp = (IsVirtual ||| IsKeyword ||| 1026us)
    | VirtualLet = (IsVirtual ||| IsKeywordOperator ||| KW.Let)
    | VirtualUse = (IsVirtual ||| IsKeywordOperator ||| KW.Use)
    | VirtualLetBang = (IsVirtual ||| IsKeywordOperator ||| KW.LetBang)
    | VirtualUseBang = (IsVirtual ||| IsKeywordOperator ||| KW.UseBang)
    | VirtualDo = (IsVirtual ||| IsKeyword ||| KW.Do)
    | VirtualDoBang = (IsVirtual ||| IsKeyword ||| KW.DoBang)
    | VirtualThen = (IsVirtual ||| IsKeyword ||| KW.Then)
    | VirtualElse = (IsVirtual ||| IsKeyword ||| KW.Else)
    | VirtualWith = (IsVirtual ||| IsKeyword ||| KW.With)
    | VirtualFunction = (IsVirtual ||| IsKeywordOperator ||| KW.Function)
    | VirtualFun = (IsVirtual ||| IsKeywordOperator ||| KW.Fun)

    // 19.2 ML Compatibility Keywords
    | KWAsr = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Asr)
    | KWLand = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Land)
    | KWLor = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Lor)
    | KWLsl = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Lsl)
    | KWLsr = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Lsr)
    | KWLxor = (IsDeprecatedKeywordOperatorIdentifier ||| KW.Lxor)

    // 3.6 Symbolic Keywords and Operators
    // Computation Expressions (CE)
    | KWAndBang = (IsKeyword ||| KW.AndBang)
    | KWDoBang = (IsKeyword ||| KW.DoBang)
    | KWLetBang = (IsKeywordOperator ||| KW.LetBang)
    | KWMatchBang = (IsKeywordOperator ||| KW.MatchBang)
    | KWReturnBang = (IsKeyword ||| KW.ReturnBang)
    | KWUseBang = (IsKeywordOperator ||| KW.UseBang)
    | KWYieldBang = (IsKeywordOperator ||| KW.YieldBang)
    // Other symbolic keywords
    | OpBar = (IsKeywordOperator ||| 1us) // |
    | OpArrowRight = (IsKeywordOperator ||| 2us) // ->
    | OpArrowLeft = (IsKeywordOperator ||| 3us) // <-
    | OpDot = (IsKeywordOperator ||| 4us) // .
    | OpColon = (IsKeywordOperator ||| 5us) // :
    | OpComma = (IsKeywordOperator ||| 6us) // ,
    | OpParenLeft = (IsKeywordOperator ||| 7us) // (
    | OpParenRight = (IsKeywordOperator ||| 8us) // )
    | OpBracketLeft = (IsKeywordOperator ||| 9us) // [
    | OpBracketRight = (IsKeywordOperator ||| 10us) // ]
    | OpAttributeBracketLeft = (IsKeywordOperator ||| 11us) // [<
    | OpAttributeBracketRight = (IsKeywordOperator ||| 12us) // >]
    | OpArrayBracketLeft = (IsKeywordOperator ||| 13us) // [|
    | OpArrayBracketRight = (IsKeywordOperator ||| 14us) // |]
    | OpBraceLeft = (IsKeywordOperator ||| 15us) // {
    | OpBraceRight = (IsKeywordOperator ||| 16us) // }
    | SingleQuote = (IsKeyword ||| 17us) // ' a lone single quote (there doesn't seem to be a way to use it in F#, but it's listed in the spec)
    | OpTypeTest = (IsKeywordOperator ||| 19us) // :?
    | OpUpcast = (IsKeywordOperator ||| 20us) // :?>
    | OpDowncast = (IsKeywordOperator ||| 21us) // :>
    | OpRange = (IsKeywordOperator ||| 65us) // ..
    | OpRangeStep = (IsKeywordOperator ||| 66us) // .. ..
    | OpCons = (IsKeywordOperatorIdentifier ||| 22us) // :: spec says op_ColonColon but MSFT says op_Cons?
    | OpAssignment = (IsDeprecatedKeywordOperator ||| 23us) // := Reference cell operators (deprecated)
    | OpDoubleSemicolon = (IsKeywordOperator ||| 24us) // ;;
    | OpSemicolon = (IsKeywordOperator ||| 25us) // ;
    | OpEquality = (IsKeywordOperator ||| 18us) // =
    | Wildcard = (IsKeywordIdentifier ||| 26us) // _
    | OpDoubleQuestion = (IsKeywordOperator ||| 27us) // ??
    | OpDeclareMultiply = (IsKeywordOperator ||| 28us) // (*)
    | OpNil = (IsKeywordOperator ||| 29us) // []
    | OpDereference = (IsKeywordOperator ||| 67us) // !
    | OpQuotationTypedLeft = (IsKeywordOperator ||| 57us) // <@
    | OpQuotationTypedRight = (IsKeywordOperator ||| 58us) // @>
    | OpQuotationUntypedLeft = (IsKeywordOperator ||| 59us) // <@@
    | OpQuotationUntypedRight = (IsKeywordOperator ||| 60us) // @@>
    | Unit = (IsKeywordIdentifier ||| 61us) // ()
    | ReservedOperator = (IsReservedOperator ||| 62us) // A reserved operator (i.e. containing a reserved symbol $ or :)
    | InvalidOperator = (IsInvalidOperator ||| 63us) // An invalid operator (e.g., contains only ignored prefix characters)
    | InvalidPrefixOperator = (IsInvalidOperator ||| CanBePrefix ||| 64us) // An invalid prefix operator (i.e. starts with ~ but is not a valid prefix operator)
    | OpIndexSetIdentifier = (IsDeprecatedKeywordOperator ||| 68us) // op_IndexSet .[]<-
    | OpIndexGetIdentifier = (IsDeprecatedKeywordOperator ||| 69us) // op_IndexGet .[]
    | OpIndexSet2Identifier = (IsDeprecatedKeywordOperator ||| 68us) // op_IndexSet .[,]<-
    | OpIndexGet2Identifier = (IsDeprecatedKeywordOperator ||| 69us) // op_IndexGet .[,]
    | OpIndexSet3Identifier = (IsDeprecatedKeywordOperator ||| 68us) // op_IndexSet .[,,]<-
    | OpIndexGet3Identifier = (IsDeprecatedKeywordOperator ||| 69us) // op_IndexGet .[,,]
    | OpIndexSet4Identifier = (IsDeprecatedKeywordOperator ||| 68us) // op_IndexSet .[,,,]<-
    | OpIndexGet4Identifier = (IsDeprecatedKeywordOperator ||| 69us) // op_IndexGet .[,,,]
    | OpIndexSetParenIdentifier = (IsKeywordOperator ||| 70us) // op_IndexSetParen .()<-
    | OpIndexGetParenIdentifier = (IsKeywordOperator ||| 71us) // op_IndexGetParen .()
    | OpIndexLeft = (IsKeywordOperator ||| 72us) // op_IndexLeft .[
    | OpIndexLeftParen = (IsDeprecatedKeywordOperator ||| 73us) // op_IndexLeftParen .(

    // 4.1 Operator Names
    // These operators are not keywords, but regular operators with names
    // The values in this section define their precedence and properties, they are not unique IDs
    | OpAddition = (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd) // +
    | OpSubtraction = (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd) // -
    | OpMultiply = (IsOperator ||| Precedence.InfixMultiply) // *
    | OpDivision = (IsOperator ||| Precedence.InfixMultiply) // /
    | OpExponentiation = (IsOperator ||| Precedence.Exponentiation) // **
    | OpAppend = (IsOperator ||| Precedence.Concatenate) // @ precedence is not documented, assuming Concatenate level
    | OpConcatenate = (IsOperator ||| Precedence.Concatenate) // ^
    | OpModulus = (IsOperator ||| CanBePrefix ||| Precedence.InfixMultiply) // %
    | OpBitwiseAnd = (IsOperator ||| Precedence.LogicalAndBitwise) // &&&
    | OpBitwiseOr = (IsOperator ||| Precedence.LogicalAndBitwise) // |||
    | OpExclusiveOr = (IsOperator ||| Precedence.LogicalAndBitwise) // ^^^
    | OpLeftShift = (IsOperator ||| Precedence.LogicalAndBitwise) // <<<
    | OpLogicalNot = (IsOperator ||| CanBePrefix ||| Precedence.LogicalAndBitwise) // ~~~
    | OpRightShift = (IsOperator ||| Precedence.LogicalAndBitwise) // >>>
    | OpUnaryPlus = (IsOperator ||| CanBePrefix ||| Precedence.Prefix) // ~+
    | OpUnaryNegation = (IsOperator ||| CanBePrefix ||| Precedence.Prefix) // ~-
    | OpInequality = (IsOperator ||| Precedence.LogicalAndBitwise) // <>
    | OpLessThanOrEqual = (IsOperator ||| Precedence.LogicalAndBitwise) // <=
    | OpGreaterThanOrEqual = (IsOperator ||| Precedence.LogicalAndBitwise) // >=
    | OpLessThan = (IsOperator ||| Precedence.LogicalAndBitwise) // <
    | OpGreaterThan = (IsOperator ||| Precedence.LogicalAndBitwise) // >
    | OpDynamic = (IsOperator ||| Precedence.Dot) // ?
    | OpDynamicAssignment = (IsOperator ||| Precedence.Dot) // ?<-
    | OpPipeRight = (IsOperator ||| Precedence.Pipe) // |>
    | OpPipeRight2 = (IsOperator ||| Precedence.Pipe) // ||>
    | OpPipeRight3 = (IsOperator ||| Precedence.Pipe) // |||>
    | OpPipeLeft = (IsOperator ||| Precedence.LogicalAndBitwise) // <| didn't find official precedence, assuming LogicalAndBitwise level as first char is <
    | OpPipeLeft2 = (IsOperator ||| Precedence.LogicalAndBitwise) // <||
    | OpPipeLeft3 = (IsOperator ||| Precedence.LogicalAndBitwise) // <|||
    | OpComposeRight = (IsOperator ||| Precedence.LogicalAndBitwise) // >>
    | OpComposeLeft = (IsOperator ||| Precedence.LogicalAndBitwise) // <<
    | OpSplice = (IsOperator ||| CanBePrefix ||| Precedence.Dot) // ~% (used in F# quotations)
    | OpSpliceUntyped = (IsOperator ||| CanBePrefix ||| Precedence.Dot) // ~%%
    | OpAddressOf = (IsOperator ||| CanBePrefix ||| Precedence.Dot) // ~&
    | OpIntegerAddressOf = (IsOperator ||| CanBePrefix ||| Precedence.Dot) // ~&&
    | OpBooleanOr = (IsOperator ||| Precedence.BooleanOr) // ||
    | OpBooleanAnd = (IsOperator ||| Precedence.BooleanAnd) // &&
    | OpAdditionAssignment = (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd) // +=
    | OpSubtractionAssignment = (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd) // -=
    | OpMultiplyAssignment = (IsOperator ||| Precedence.InfixMultiply) // *=
    | OpDivisionAssignment = (IsOperator ||| Precedence.InfixMultiply) // /=
    | OpNotEqual = (IsOperator ||| Precedence.LogicalAndBitwise) // != (F# uses <> but != is in the spec too)

    // 3.8 Numeric Literals
    | NumSByte = (IsNumericLiteral ||| 2us) // 0y
    | NumSByteHex = (IsNumericLiteral ||| NumericBaseHex ||| 2us) // 0x0y
    | NumSByteOctal = (IsNumericLiteral ||| NumericBaseOctal ||| 2us) // 0o0y
    | NumSByteBinary = (IsNumericLiteral ||| NumericBaseBinary ||| 2us) // 0b0000y

    | NumByte = (IsNumericLiteral ||| 3us) // 0uy
    | NumByteHex = (IsNumericLiteral ||| NumericBaseHex ||| 3us) // 0x0uy
    | NumByteOctal = (IsNumericLiteral ||| NumericBaseOctal ||| 3us) // 0o0uy
    | NumByteBinary = (IsNumericLiteral ||| NumericBaseBinary ||| 3us) // 0b0000uy

    | NumInt16 = (IsNumericLiteral ||| 4us) // 0s
    | NumInt16Hex = (IsNumericLiteral ||| NumericBaseHex ||| 4us) // 0x0s
    | NumInt16Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 4us) // 0o0s
    | NumInt16Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 4us) // 0b0000s

    | NumUInt16 = (IsNumericLiteral ||| 5us) // 0us
    | NumUInt16Hex = (IsNumericLiteral ||| NumericBaseHex ||| 5us) // 0x0us
    | NumUInt16Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 5us) // 0o0us
    | NumUInt16Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 5us) // 0b0000us

    | NumInt32 = (IsNumericLiteral ||| 6us) // 0
    | NumInt32Hex = (IsNumericLiteral ||| NumericBaseHex ||| 6us) // 0x0
    | NumInt32Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 6us) // 0o0
    | NumInt32Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 6us) // 0b0000

    | NumUInt32 = (IsNumericLiteral ||| 7us) // 0u
    | NumUInt32Hex = (IsNumericLiteral ||| NumericBaseHex ||| 7us) // 0x0u
    | NumUInt32Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 7us) // 0o0u
    | NumUInt32Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 7us) // 0b0000u

    | NumInt64 = (IsNumericLiteral ||| 8us) // 0L
    | NumInt64Hex = (IsNumericLiteral ||| NumericBaseHex ||| 8us) // 0x0L
    | NumInt64Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 8us) // 0o0L
    | NumInt64Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 8us) // 0b0000L

    | NumUInt64 = (IsNumericLiteral ||| 9us) // 0UL
    | NumUInt64Hex = (IsNumericLiteral ||| NumericBaseHex ||| 9us) // 0x0UL
    | NumUInt64Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 9us) // 0o0UL
    | NumUInt64Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 9us) // 0b0000UL

    | NumNativeInt = (IsNumericLiteral ||| 10us) // 0n
    | NumNativeIntHex = (IsNumericLiteral ||| NumericBaseHex ||| 10us) // 0x0n
    | NumNativeIntOctal = (IsNumericLiteral ||| NumericBaseOctal ||| 10us) // 0o0n
    | NumNativeIntBinary = (IsNumericLiteral ||| NumericBaseBinary ||| 10us) // 0b0000n

    | NumUNativeInt = (IsNumericLiteral ||| 11us) // 0un
    | NumUNativeIntHex = (IsNumericLiteral ||| NumericBaseHex ||| 11us) // 0x0un
    | NumUNativeIntOctal = (IsNumericLiteral ||| NumericBaseOctal ||| 11us) // 0o0un
    | NumUNativeIntBinary = (IsNumericLiteral ||| NumericBaseBinary ||| 11us) // 0b0000un

    | NumIEEE32 = (IsNumericLiteral ||| 12us) // 0.0f
    | NumIEEE32Hex = (IsNumericLiteral ||| NumericBaseHex ||| 12us) // 0x0.0f
    | NumIEEE32Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 12us) // 0o0.0f
    | NumIEEE32Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 12us) // 0b0000.0f

    | NumIEEE64 = (IsNumericLiteral ||| 13us) // 0.0
    | NumIEEE64Hex = (IsNumericLiteral ||| NumericBaseHex ||| 13us) // 0x0.0
    | NumIEEE64Octal = (IsNumericLiteral ||| NumericBaseOctal ||| 13us) // 0o0.0
    | NumIEEE64Binary = (IsNumericLiteral ||| NumericBaseBinary ||| 13us) // 0b0000.0

    | NumDecimal = (IsNumericLiteral ||| 14us) // ( float | int ) [Mm]
    | NumDecimalHex = (IsNumericLiteral ||| NumericBaseHex ||| 14us) // 0x0M
    | NumDecimalOctal = (IsNumericLiteral ||| NumericBaseOctal ||| 14us) // 0o0M
    | NumDecimalBinary = (IsNumericLiteral ||| NumericBaseBinary ||| 14us) // 0b0000M

    // BigInteger formats only support decimal base
    | NumBigInteger = (IsNumericLiteral ||| 15us) // int ('Q' | ' R' | 'Z' | 'I' | 'N' | 'G')

    | ReservedNumericLiteral = (IsReserved ||| IsNumericLiteral ||| 16us) // (xint | ieee32 | ieee64) ident-char+
    | ReservedNumericLiteralHex = (IsReserved ||| IsNumericLiteral ||| NumericBaseHex ||| 16us) // 0x0ident-char+
    | ReservedNumericLiteralOctal = (IsReserved ||| IsNumericLiteral ||| NumericBaseOctal ||| 16us) // 0o0ident-char+
    | ReservedNumericLiteralBinary = (IsReserved ||| IsNumericLiteral ||| NumericBaseBinary ||| 16us) // 0b0000ident-char+

    // 3.5 Strings and Characters
    | CharLiteral = (IsTextLiteral ||| 1us)
    | InvalidCharTrigraphLiteral = (IsInvalid ||| IsTextLiteral ||| 2us) // > 255
    | InvalidCharLiteral = (IsInvalid ||| IsTextLiteral ||| 3us)
    | InvalidLongUnicodeCharLiteral = (IsInvalid ||| IsTextLiteral ||| 4us)
    | UnterminatedCharLiteral = (IsInvalid ||| IsTextLiteral ||| 5us)
    | StringLiteral = (IsTextLiteral ||| 6us)
    | ByteArrayLiteral = (IsTextLiteral ||| 7us) // e.g. "abc"B
    | UnterminatedStringLiteral = (IsInvalid ||| IsTextLiteral ||| 8us)
    | VerbatimStringLiteral = (IsTextLiteral ||| 9us)
    | VerbatimByteArrayLiteral = (IsTextLiteral ||| 10us) // e.g. @"abc"B
    | UnterminatedVerbatimStringLiteral = (IsInvalid ||| IsTextLiteral ||| 11us)
    | String3Literal = (IsTextLiteral ||| 12us) // Triple-quoted string
    | UnterminatedString3Literal = (IsInvalid ||| IsTextLiteral ||| 13us)
    | UnterminatedInterpolatedString = (IsInvalid ||| IsTextLiteral ||| 14us)
    | InterpolatedStringOpen = (IsTextLiteral ||| 15us) // $"
    | InterpolatedStringClose = (IsTextLiteral ||| 16us)
    | VerbatimInterpolatedStringOpen = (IsTextLiteral ||| 17us)
    | VerbatimInterpolatedStringClose = (IsTextLiteral ||| 18us)
    | Interpolated3StringOpen = (IsTextLiteral ||| 19us)
    | Interpolated3StringClose = (IsTextLiteral ||| 20us)
    | InterpolatedStringFragment = (IsTextLiteral ||| 21us) // Constant string fragment
    | Interpolated3StringFragment = (IsTextLiteral ||| 22us) // Constant string fragment
    | TooManyLBracesInInterpolated3String = (IsInvalid ||| IsTextLiteral ||| 23us)
    | TooManyRBracesInInterpolated3String = (IsInvalid ||| IsTextLiteral ||| 24us)
    | VerbatimInterpolatedStringFragment = (IsTextLiteral ||| 25us)
    | InterpolatedExpressionOpen = (IsTextLiteral ||| 26us) // one or more {
    | InterpolatedExpressionClose = (IsTextLiteral ||| 27us) // one or more }
    | FormatPlaceholder = (IsTextLiteral ||| 28us)
    | InvalidFormatPlaceholder = (IsInvalid ||| IsTextLiteral ||| 29us)
    | InvalidFormatPercents = (IsInvalid ||| IsTextLiteral ||| 30us)
    | EscapePercent = (IsTextLiteral ||| 31us)
    | EscapeLBrace = (IsTextLiteral ||| 32us)
    | EscapeRBrace = (IsTextLiteral ||| 33us)
    | UnmatchedInterpolatedRBrace = (IsInvalid ||| IsTextLiteral ||| 34us) // Single } is invalid outside an expression
    | VerbatimEscapeQuote = (IsTextLiteral ||| 35us) // "" inside a verbatim string


module internal Token =
    let inline ofUInt16 i =
        LanguagePrimitives.EnumOfValue<uint16, Token> i

    // Characters that are ignored at the start of a custom operator for precedence purposes
    // See 4.4.1 Categorization of Symbolic Operators
    let ignoredChars = ".$?"

    let ofCustomOperator (span: ReadOnlySpan<char>) =
        let trimIgnored = span.TrimStart(ignoredChars)

        if trimIgnored.Length = 0 then
            Token.InvalidOperator // Operator cannot consist solely of ignored characters

        elif span.Contains '$' || (not (trimIgnored.StartsWith(">")) && span.Contains ':') then
            // '$' is not permitted as a character in operator names and is reserved for future use
            // ':' is not permitted as a character in operator names and is reserved for future use
            // Except when it starts with '>' after trimming ignored chars
            // https://github.com/dotnet/fsharp/pull/15923
            // https://github.com/fsharp/fslang-suggestions/issues/1446
            Token.ReservedOperator

        else
            //!%&*+-./<=>@^|~
            match trimIgnored[0] with
            | '!' ->
                if trimIgnored.Length >= 2 && trimIgnored[1] = '=' then
                    ofUInt16 (IsOperator ||| Precedence.LogicalAndBitwise) // !=
                else
                    ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.Prefix) // !
            | '%' -> ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.InfixMultiply)
            | '&' -> ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.LogicalAndBitwise)
            | '*' -> ofUInt16 (IsOperator ||| Precedence.InfixMultiply)
            | '+' -> ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd)
            | '-' -> ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.InfixAdd)
            | '/' -> ofUInt16 (IsOperator ||| Precedence.InfixMultiply)
            | '<' -> ofUInt16 (IsOperator ||| Precedence.LogicalAndBitwise)
            | '=' -> ofUInt16 (IsOperator ||| Precedence.LogicalAndBitwise)
            | '>' -> ofUInt16 (IsOperator ||| Precedence.LogicalAndBitwise)
            | '@' -> ofUInt16 (IsOperator ||| Precedence.Concatenate) // TODO https://github.com/fsharp/fslang-spec/issues/70
            | '^' -> ofUInt16 (IsOperator ||| Precedence.Concatenate)
            | '|' -> ofUInt16 (IsOperator ||| Precedence.LogicalAndBitwise)
            | '~' ->
                if
                    trimIgnored.Length = 1 // ~ alone is not a valid operator
                    || trimIgnored.Length < span.Length // prefix operators cannot start with ignored chars
                then
                    if span.Length > 3 && span.Trim('~').Length = 0 then
                        // any number of ~ is a valid prefix operator
                        ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.Prefix)
                    else
                        let s = span.ToString()
                        // 4.4.1 Categorization of Symbolic Operators
                        // Only these prefix operators are valid, the spec doesn't list ~%% ~?+ ~?- but they will compile
                        // TODO: Use SearchValues?
                        match s with
                        | "~+"
                        | "~-"
                        | "~%"
                        | "~&"
                        | "~~"
                        | "~?+"
                        | "~?-"
                        | "~+."
                        | "~-."
                        | "~~~"
                        | "~%%"
                        | "~&&" -> ofUInt16 (IsOperator ||| CanBePrefix ||| Precedence.Prefix)
                        | _ -> Token.InvalidPrefixOperator
                else
                    Token.InvalidPrefixOperator

            | _ -> invalidArg "span" (sprintf "Invalid custom operator: %s" (span.ToString()))

module internal TokenInfo =

    let hasFlag (token: Token) (flag: uint16) = (uint16 token &&& flag) <> 0us

    let isIdentifier (token: Token) = hasFlag token IsIdentifier
    let isLiteral (token: Token) = hasFlag token IsLiteral
    let isOperator (token: Token) = hasFlag token IsOperator
    let isKeyword (token: Token) = hasFlag token IsKeyword
    let isDeprecated (token: Token) = hasFlag token IsDeprecated
    let isReserved (token: Token) = hasFlag token IsReserved
    let isInvalid (token: Token) = hasFlag token IsInvalid

    let inBlockComment (token: Token) = hasFlag token InBlockComment

    let inOCamlBlockComment (token: Token) = hasFlag token InOCamlBlockComment

    let isNumeric (token: Token) = hasFlag token IsNumericLiteral

    let isText (token: Token) = hasFlag token IsTextLiteral

    let isVirtualKeyword (token: Token) =
        isKeyword token && hasFlag token IsVirtual

[<Struct>]
type PositionedToken =
    val private value: uint64
    private new(value: uint64) = { value = value }

    static member Create(tokenValue: Token, startIndex: int64) =
        let indexPart = uint64 startIndex <<< 16
        let tokenPart = uint64 tokenValue
        PositionedToken(indexPart ||| tokenPart)

    member this.Token: Token =
        this.value &&& TokenMask |> uint16 |> LanguagePrimitives.EnumOfValue

    member this.StartIndex: int64 = int64 (this.value >>> 16)

    override this.ToString() =
        if System.Enum.IsDefined<Token> this.Token then
            sprintf "%d, %O @ " this.StartIndex this.Token
        else
            sprintf "%d, 016%B @ " this.StartIndex (uint16 this.Token)

[<AutoOpen>]
module TokenExtensions =
    type Token with
        member this.IsIdentifier = TokenInfo.isIdentifier this
        member this.IsLiteral = TokenInfo.isLiteral this
        member this.IsOperator = TokenInfo.isOperator this
        member this.IsKeyword = TokenInfo.isKeyword this
        member this.IsDeprecated = TokenInfo.isDeprecated this
        member this.IsReserved = TokenInfo.isReserved this
        member this.IsInvalid = TokenInfo.isInvalid this
        member this.InBlockComment = TokenInfo.inBlockComment this
        member this.InOCamlBlockComment = TokenInfo.inOCamlBlockComment this
        member this.IsNumeric = TokenInfo.isNumeric this
        member this.IsText = TokenInfo.isText this

        member this.IsCommentedOut =
            TokenInfo.inBlockComment this || TokenInfo.inOCamlBlockComment this

        member this.WithoutCommentFlags: Token =
            let mask = ~~~(InBlockComment ||| InOCamlBlockComment)
            uint16 this &&& mask |> uint16 |> LanguagePrimitives.EnumOfValue

    type PositionedToken with
        member this.IsIdentifier = TokenInfo.isIdentifier this.Token
        member this.IsLiteral = TokenInfo.isLiteral this.Token
        member this.IsOperator = TokenInfo.isOperator this.Token
        member this.IsKeyword = TokenInfo.isKeyword this.Token
        member this.IsDeprecated = TokenInfo.isDeprecated this.Token
        member this.IsReserved = TokenInfo.isReserved this.Token
        member this.IsInvalid = TokenInfo.isInvalid this.Token
        member this.InBlockComment = TokenInfo.inBlockComment this.Token
        member this.InOCamlBlockComment = TokenInfo.inOCamlBlockComment this.Token
        member this.IsNumeric = TokenInfo.isNumeric this.Token
        member this.IsText = TokenInfo.isText this.Token

        member this.IsCommentedOut =
            TokenInfo.inBlockComment this.Token || TokenInfo.inOCamlBlockComment this.Token

        member this.TokenWithoutCommentFlags: Token =
            let mask = ~~~(InBlockComment ||| InOCamlBlockComment)
            uint16 this.Token &&& mask |> uint16 |> LanguagePrimitives.EnumOfValue


[<Struct>]
type NumericInfo =
    internal
        {
            _token: PositionedToken
        }

    member this.PositionedToken = this._token
    member this.Token: Token = this._token.Token
    member this.StartIndex: int64 = this._token.StartIndex

    member this.Base: NumericBase =
        uint16 this._token.Token &&& NumericBaseMask >>> 4 |> int |> enum

    member this.Kind: NumericKind =
        uint16 this._token.Token &&& NumericTypeIDMask |> int |> enum

    static member TryCreate(token: PositionedToken) =
        if TokenInfo.isNumeric token.Token then
            ValueSome { _token = token }
        else
            ValueNone

    static member Create(token: PositionedToken) =
        if TokenInfo.isNumeric token.Token then
            { _token = token }
        else
            invalidArg "token" (sprintf "Token %A is not a numeric literal." token)

module internal OperatorInfo =
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    // 4.4.2 Precedence of Symbolic Operators and Pattern/Expression Constructs

    let precedence (this: PositionedToken) : PrecedenceLevel =
        if this.Token.IsKeyword then
            // All keyword operators are left associative
            match this.Token.WithoutCommentFlags with
            | Token.KWAs -> PrecedenceLevel.As
            | Token.KWWhen -> PrecedenceLevel.When
            | Token.OpSemicolon -> PrecedenceLevel.Semicolon
            | Token.KWLet -> PrecedenceLevel.Let
            | Token.KWFunction
            | Token.KWFun
            | Token.KWMatch
            | Token.KWTry -> PrecedenceLevel.Function
            | Token.KWIf -> PrecedenceLevel.If
            | Token.KWNot -> PrecedenceLevel.Not
            | Token.OpArrowRight -> PrecedenceLevel.Arrow
            | Token.OpAssignment -> PrecedenceLevel.Assignment
            | Token.OpComma -> PrecedenceLevel.Comma
            | Token.KWOr -> PrecedenceLevel.LogicalOr
            | Token.OpDowncast
            | Token.OpUpcast -> PrecedenceLevel.Cast
            | Token.OpCons -> PrecedenceLevel.Cons
            | Token.OpTypeTest -> PrecedenceLevel.TypeTest
            | Token.KWLazy
            | Token.KWAssert -> PrecedenceLevel.Function // same as function application
            | Token.OpBar -> PrecedenceLevel.PatternMatchBar // pattern match bar
            | Token.OpDot -> PrecedenceLevel.Dot
            | _ -> raise (new NotImplementedException())
        else
            uint16 this.Token &&& PrecedenceMask |> int |> enum

    let associativity (p: PrecedenceLevel) : Associativity =
        match p with
        | PrecedenceLevel.As -> Associativity.Right
        | PrecedenceLevel.When -> Associativity.Right
        | PrecedenceLevel.Pipe -> Associativity.Left
        | PrecedenceLevel.Semicolon -> Associativity.Right
        | PrecedenceLevel.Let -> Associativity.Non
        | PrecedenceLevel.Function -> Associativity.Non
        | PrecedenceLevel.If -> Associativity.Non
        | PrecedenceLevel.Not -> Associativity.Right
        | PrecedenceLevel.Arrow -> Associativity.Right
        | PrecedenceLevel.Assignment -> Associativity.Right
        | PrecedenceLevel.Comma -> Associativity.Non
        | PrecedenceLevel.LogicalOr -> Associativity.Left
        | PrecedenceLevel.LogicalAnd -> Associativity.Left
        | PrecedenceLevel.Cast -> Associativity.Right
        | PrecedenceLevel.LogicalAndBitwise -> Associativity.Left
        | PrecedenceLevel.Caret -> Associativity.Right
        | PrecedenceLevel.Cons -> Associativity.Right
        | PrecedenceLevel.TypeTest -> Associativity.Non
        | PrecedenceLevel.InfixAdd -> Associativity.Left
        | PrecedenceLevel.InfixMultiply -> Associativity.Left
        | PrecedenceLevel.Power -> Associativity.Right
        | PrecedenceLevel.Application -> Associativity.Left
        | PrecedenceLevel.PatternMatchBar -> Associativity.Right
        | PrecedenceLevel.Prefix -> Associativity.Left
        | PrecedenceLevel.Dot -> Associativity.Left
        | PrecedenceLevel.HighApplication -> Associativity.Left
        | PrecedenceLevel.HighTypeApplication -> Associativity.Left
        | _ -> invalidOp $"Unknown precedence level {p}."

[<Struct>]
type OperatorInfo =
    internal
        {
            _token: PositionedToken
            _precedence: PrecedenceLevel
        }

    member this.PositionedToken = this._token
    member this.Token: Token = this._token.Token
    member this.StartIndex: int64 = this._token.StartIndex
    member this.CanBePrefix: bool = TokenInfo.hasFlag this.Token CanBePrefix
    member this.Associativity = OperatorInfo.associativity this.Precedence
    member this.Precedence = this._precedence

    static member TryCreate(token: PositionedToken) =
        if TokenInfo.isOperator token.Token then
            ValueSome
                {
                    _token = token
                    _precedence = OperatorInfo.precedence token
                }
        else
            ValueNone

    static member Create(token: PositionedToken) =
        if TokenInfo.isOperator token.Token then
            {
                _token = token
                _precedence = OperatorInfo.precedence token
            }
        else
            invalidArg "token" (sprintf "Token %A is not an operator." token)
