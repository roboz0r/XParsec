namespace XParsec.FSharp.Lexer

open System

type TokenKind =
    | Keyword = 0
    | Identifier = 1
    | TextLiteral = 2
    | NumericLiteral = 3
    | Operator = 4
    | Special = 5
    | Invalid = 6
    | Spare = 7

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
    | Parens = 28 // Parentheses have the highest precedence for grouping

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
    | SByte = 1
    | Byte = 2
    | Int16 = 3
    | UInt16 = 4
    | Int32 = 5
    | UInt32 = 6
    | Int64 = 7
    | UInt64 = 8
    | NativeInt = 9
    | UNativeInt = 10
    | IEEE32 = 11
    | IEEE64 = 12
    | Decimal = 13
    | BigIntegerQ = 14
    | BigIntegerR = 15
    | BigIntegerZ = 16
    | BigIntegerI = 17
    | BigIntegerN = 18
    | BigIntegerG = 19
    | ReservedNumericLiteral = 31


module TokenRepresentation =
    [<Literal>]
    let TokenMask = 0xFFFFUL // Lower 16 bits for token ID and flags

    // Bits: 15-13 (Kind) | 12-11 (Flags) | 10-8 (Spare) | 7-0 (Payload)
    //  000          | 00            | 000          | 00010101 (ID = 21)

    // ==========================================================
    // SECTION 1: Token Kind (Bits 15-13)
    // ==========================================================
    [<Literal>]
    let KindMask = 0xE000us

    [<Literal>]
    let KindShift = 13

    // 000...
    [<Literal>]
    let KindKeyword = 0x0000us
    // 001...
    [<Literal>]
    let KindIdentifier = 0x2000us
    // 010... (Textual Literal: String, Char)
    [<Literal>]
    let KindTextLiteral = 0x4000us
    // 011... (Numeric Literal: Int, Float)
    [<Literal>]
    let KindNumericLiteral = 0x6000us
    // 100... (Symbolic Operators)
    [<Literal>]
    let KindOperator = 0x8000us
    // 101... (Whitespace, Comments, Directives)
    [<Literal>]
    let KindSpecial = 0xA000us
    // 110...
    [<Literal>]
    let KindInvalid = 0xC000us
    // 111... (Available for specialized internal markers)
    [<Literal>]
    let KindSpare = 0xE000us

    // ==========================================================
    // SECTION 2: Global Flags (Bits 12-11)
    // ==========================================================

    // Bit 12: Indicates the token was found inside a comment block
    // (Replaces InBlockComment and InOCamlBlockComment)
    [<Literal>]
    let InComment = 0x1000us // Bit 12

    // Bit 11: Virtual / Synthesized
    // (Kept as a flag per your existing logic, but can be moved to Payload
    // if you assign distinct TokenIDs for Virtual tokens)
    [<Literal>]
    let IsVirtual = 0x0800us // Bit 11

    // ==========================================================
    // SECTION 3: Reserved / Extended (Bit 10-8)
    // ==========================================================

    // Bit 10-8: Currently Unused.
    // Can be used as a flag, or to extend Payload to 9 bits (512 values).
    [<Literal>]
    let ReservedFlag10 = 0x0400us

    [<Literal>]
    let ReservedFlag9 = 0x0200us

    [<Literal>]
    let ReservedFlag8 = 0x0100us

    // ==========================================================
    // SECTION 4: Payload (Bits 7-0)
    // ==========================================================
    // 8 bits = 256 unique values.

    [<Literal>]
    let PayloadMask = 0x00FFus

    // --- Helpers for Kind Checks ---

    // A token is a literal if it is String (0x4000) or Number (0x6000)
    // We check this by masking the top 2 bits (0xC000) and checking for 0x4000 bit.
    [<Literal>]
    let internal LiteralGroupMask = 0xC000us

    [<Literal>]
    let internal LiteralGroupCheck = 0x4000us

    // --- Payload Definitions ---

    // 1. Numeric Literals (Payload Bits 0-6)
    // Base Mask: Bits 6-5
    [<Literal>]
    let NumericBaseMask = 0b0110_0000us
    // Type ID: Bits 4-0
    [<Literal>]
    let NumericKindMask = 0b0001_1111us

    [<Literal>]
    let NumericBaseShift = 5

    [<Literal>]
    let internal NumericBaseHex = 0b0010_0000us

    [<Literal>]
    let internal NumericBaseOctal = 0b0100_0000us

    [<Literal>]
    let internal NumericBaseBinary = 0b0110_0000us

    // 2. String/Text Literals (Payload Bits 0-5)
    // Fits your requirement of ~60 types within 6 bits.
    [<Literal>]
    let TextTypeIDMask = 0b00111111us

    // 3. Operators (Payload Bits 0-6)
    // Bit 6: Prefix Flag
    [<Literal>]
    let CanBePrefix = 0b0100_0000us
    // Bits 0-5: Precedence (0-63)
    [<Literal>]
    let PrecedenceMask = 0b0011_1111us


    module internal KW =
        // Keywords get a unique ID starting from 1us to 127us (lower 7 bits)
        // 3.4 token ident-keyword
        [<Literal>]
        let Abstract = 1us

        [<Literal>]
        let And = 2us

        [<Literal>]
        let As = 3us

        [<Literal>]
        let Assert = 4us

        [<Literal>]
        let Base = 5us

        [<Literal>]
        let Begin = 6us

        [<Literal>]
        let Class = 7us

        [<Literal>]
        let Const = 8us

        [<Literal>]
        let Default = 9us

        [<Literal>]
        let Delegate = 10us

        [<Literal>]
        let Do = 11us

        [<Literal>]
        let Done = 12us

        [<Literal>]
        let Downcast = 13us

        [<Literal>]
        let Downto = 14us

        [<Literal>]
        let Elif = 15us

        [<Literal>]
        let Else = 16us

        [<Literal>]
        let End = 17us

        [<Literal>]
        let Exception = 18us

        [<Literal>]
        let Extern = 19us

        [<Literal>]
        let False = 20us

        [<Literal>]
        let Finally = 21us

        [<Literal>]
        let Fixed = 22us

        [<Literal>]
        let For = 23us

        [<Literal>]
        let Fun = 24us

        [<Literal>]
        let Function = 25us

        [<Literal>]
        let Global = 26us

        [<Literal>]
        let If = 27us

        [<Literal>]
        let In = 28us

        [<Literal>]
        let Inherit = 29us

        [<Literal>]
        let Inline = 30us

        [<Literal>]
        let Interface = 31us

        [<Literal>]
        let Internal = 32us

        [<Literal>]
        let Lazy = 33us

        [<Literal>]
        let Let = 34us

        [<Literal>]
        let Match = 35us

        [<Literal>]
        let Member = 36us

        [<Literal>]
        let Module = 38us

        [<Literal>]
        let Mutable = 39us

        [<Literal>]
        let Namespace = 40us

        [<Literal>]
        let New = 41us

        [<Literal>]
        let Null = 42us

        [<Literal>]
        let Of = 43us

        [<Literal>]
        let Open = 44us

        [<Literal>]
        let Or = 45us

        [<Literal>]
        let Override = 46us

        [<Literal>]
        let Private = 47us

        [<Literal>]
        let Public = 48us

        [<Literal>]
        let Rec = 49us

        [<Literal>]
        let Return = 50us

        [<Literal>]
        let Sig = 51us

        [<Literal>]
        let Static = 52us

        [<Literal>]
        let Struct = 53us

        [<Literal>]
        let Then = 54us

        [<Literal>]
        let To = 55us

        [<Literal>]
        let True = 56us

        [<Literal>]
        let Try = 57us

        [<Literal>]
        let Type = 58us

        [<Literal>]
        let Upcast = 59us

        [<Literal>]
        let Use = 60us

        [<Literal>]
        let Val = 61us

        [<Literal>]
        let Void = 62us

        [<Literal>]
        let When = 63us

        [<Literal>]
        let While = 64us

        [<Literal>]
        let With = 65us

        [<Literal>]
        let Yield = 66us

        // ==========================================================
        // Available 67-69us
        // ==========================================================

        // 3.4 token reserved-ident-keyword

        // [<Literal>]
        // let ReservedAtomic = 70us Unreserved in FS-1016

        [<Literal>]
        let ReservedBreak = 71us

        [<Literal>]
        let ReservedChecked = 72us

        [<Literal>]
        let ReservedComponent = 73us

        [<Literal>]
        let ReservedConstraint = 74us

        // [<Literal>]
        // let ReservedConstructor = 75us Unreserved in FS-1016

        [<Literal>]
        let ReservedContinue = 76us

        // [<Literal>]
        // let ReservedEager = 77us Unreserved in FS-1016

        [<Literal>]
        let ReservedFori = 78us

        // [<Literal>]
        // let ReservedFunctor = 79us Unreserved in FS-1016

        [<Literal>]
        let ReservedInclude = 80us

        // [<Literal>]
        // let ReservedMeasure = 81us Unreserved in FS-1016

        // [<Literal>]
        // let ReservedMethod = 82us Unreserved in FS-1016

        [<Literal>]
        let ReservedMixin = 83us

        // [<Literal>]
        // let ReservedObject = 84us Unreserved in FS-1016

        [<Literal>]
        let ReservedParallel = 85us

        [<Literal>]
        let ReservedParams = 86us

        [<Literal>]
        let ReservedProcess = 87us

        [<Literal>]
        let ReservedProtected = 88us

        [<Literal>]
        let ReservedPure = 89us

        // [<Literal>]
        // let ReservedRecursive = 90us Unreserved in FS-1016

        [<Literal>]
        let ReservedSealed = 91us

        [<Literal>]
        let ReservedTailcall = 92us

        [<Literal>]
        let ReservedTrait = 93us

        [<Literal>]
        let ReservedVirtual = 94us

        // [<Literal>]
        // let ReservedVolatile = 95us Unreserved in FS-1016

        // ==========================================================
        // Available 103-120us
        // ==========================================================

        // 19.2 token ocaml-ident-keyword
        [<Literal>]
        let Asr = 121us

        [<Literal>]
        let Land = 122us

        [<Literal>]
        let Lor = 123us

        [<Literal>]
        let Lsl = 124us

        [<Literal>]
        let Lsr = 125us

        [<Literal>]
        let Lxor = 126us

        [<Literal>]
        let Mod = 127us

        // 128us reserved as lower 7 bits all 0

        // 3.6 symbolic keywords for computation expressions
        [<Literal>]
        let AndBang = 129us

        [<Literal>]
        let DoBang = 130us

        [<Literal>]
        let LetBang = 131us

        [<Literal>]
        let MatchBang = 132us

        [<Literal>]
        let ReturnBang = 133us

        [<Literal>]
        let UseBang = 134us

        [<Literal>]
        let YieldBang = 135us

        // ------
        // 3.6 Symbolic Keywords

        [<Literal>]
        let Bar = 136us

        [<Literal>]
        let RightArrow = 137us

        [<Literal>]
        let LeftArrow = 138us

        [<Literal>]
        let Dot = 139us

        [<Literal>]
        let Colon = 140us

        [<Literal>]
        let LParen = 141us

        [<Literal>]
        let RParen = 142us

        [<Literal>]
        let LBracket = 143us

        [<Literal>]
        let RBracket = 144us

        [<Literal>]
        let LAttrBracket = 145us

        [<Literal>]
        let RAttrBracket = 146us

        [<Literal>]
        let LArrayBracket = 149us

        [<Literal>]
        let RArrayBracket = 150us

        [<Literal>]
        let LBrace = 151us

        [<Literal>]
        let RBrace = 152us

        [<Literal>]
        let SingleQuote = 153us

        [<Literal>]
        let Hash = 154us

        [<Literal>]
        let OpDowncast = 155us // :?>

        [<Literal>]
        let OpTypeTest = 156us // :?

        [<Literal>]
        let OpUpcast = 157us // :>

        [<Literal>]
        let OpRange = 158us // ..

        [<Literal>]
        let ColonColon = 159us // ::

        [<Literal>]
        let ColonEquals = 160us // :=

        [<Literal>]
        let SemiSemi = 161us // ;;

        [<Literal>]
        let Semi = 162us // ;

        [<Literal>]
        let Equals = 163us // =

        [<Literal>]
        let QMark = 164us // ?

        [<Literal>]
        let QMarkQMark = 165us // ??

        [<Literal>]
        let OpDeclareMultiply = 166us // (*)

        [<Literal>]
        let LQuoteTyped = 167us // <@

        [<Literal>]
        let RQuoteTyped = 168us // @>

        [<Literal>]
        let LQuoteUntyped = 169us // <@@

        [<Literal>]
        let RQuoteUntyped = 170us // @@>

        [<Literal>]
        let Underscore = 171us // _

        [<Literal>]
        let Unit = 172us // ()

        [<Literal>]
        let OpRangeStep = 173us // .. ..

        // ==========================================================
        // Available 174-179us
        // ==========================================================

        // 3.7 Symbolic Operators

        [<Literal>]
        let QMarkLeftArrow = 180us // ?<-

        [<Literal>]
        let Nil = 181us // []

        [<Literal>]
        let Dereference = 182us // !

        [<Literal>]
        let Comma = 183us // , (Tuples, Arguments)

        // ==========================================================
        // Available 184-199us
        // ==========================================================

        [<Literal>]
        let OpIndexSetIdentifier = 200us // op_IndexSet .[]<-

        [<Literal>]
        let OpIndexGetIdentifier = 201us // op_IndexGet .[]

        [<Literal>]
        let OpIndexSet2Identifier = 202us // op_IndexSet .[,]<-

        [<Literal>]
        let OpIndexGet2Identifier = 203us // op_IndexGet .[,]

        [<Literal>]
        let OpIndexSet3Identifier = 204us // op_IndexSet .[,,]<-

        [<Literal>]
        let OpIndexGet3Identifier = 205us // op_IndexGet .[,,]

        [<Literal>]
        let OpIndexSet4Identifier = 206us // op_IndexSet .[,,,]<-

        [<Literal>]
        let OpIndexGet4Identifier = 207us // op_IndexGet .[,,,]

        [<Literal>]
        let OpIndexSetParenIdentifier = 208us // op_IndexSetParen .()<-

        [<Literal>]
        let OpIndexGetParenIdentifier = 209us // op_IndexGetParen .()

        // let OpIndexLeft = 210us // op_IndexLeft .[ Lexer emits DOT + LBRACK
        // let OpIndexLeftParen = 211us // op_IndexLeftParen .( Lexer emits DOT + LPAREN

        // ==========================================================
        // Available 212-250us
        // ==========================================================

        // 3.6 token reserved-symbolic-sequence
        [<Literal>]
        let ReservedTwiddle = 251us

        [<Literal>]
        let ReservedBacktick = 252us

        [<Literal>]
        let InvalidPrefixOperator = 253us // Only a limited set of prefix operators are valid

        [<Literal>]
        let InvalidOperator = 254us // An invalid operator (e.g., contains only ignored prefix characters)

        [<Literal>]
        let ReservedOperator = 255us // A reserved operator (i.e. containing a reserved symbol $ or :)


    // ==========================================================
    // HELPER: String/Text Subtypes (Must fit in 8 bits: 0-255)
    // ==========================================================
    module internal StringType =
        [<Literal>]
        let Regular = 0us

        [<Literal>]
        let Verbatim = 1us

        [<Literal>]
        let TripleQuote = 2us

        [<Literal>]
        let ByteArray = 3us

        [<Literal>]
        let VerbatimByteArray = 4us

        [<Literal>]
        let Char = 5us

        // Interpolation parts
        [<Literal>]
        let InterpolatedOpen = 10us // $"

        [<Literal>]
        let InterpolatedClose = 11us // "

        [<Literal>]
        let InterpolatedPart = 12us // text inside

        [<Literal>]
        let InterpolatedExprOpen = 13us // {

        [<Literal>]
        let InterpolatedExprClose = 14us // }

        // Triple Interpolation
        [<Literal>]
        let TripleInterpolatedOpen = 20us // $"""

        [<Literal>]
        let TripleInterpolatedClose = 21us

        // Verbatim Interpolation
        [<Literal>]
        let VerbatimInterpolatedOpen = 30us // $@"

        // Formatting / Escapes
        [<Literal>]
        let FormatPlaceholder = 50us

        [<Literal>]
        let EscapeChar = 51us

    module internal Special =
        [<Literal>]
        let EOF = 1us

        [<Literal>]
        let Newline = 2us

        [<Literal>]
        let Whitespace = 3us

        [<Literal>]
        let Tab = 4us

        [<Literal>]
        let LineComment = 5us

        [<Literal>]
        let BlockCommentStart = 6us

        [<Literal>]
        let BlockCommentEnd = 7us

        [<Literal>]
        let Indent = 8us

        // Preprocessor Directives
        [<Literal>]
        let DirectiveIf = 10us

        [<Literal>]
        let DirectiveElse = 11us

        [<Literal>]
        let DirectiveEndIf = 12us

        [<Literal>]
        let DirectiveLine = 13us

        [<Literal>]
        let DirectiveNoWarn = 14us

        [<Literal>]
        let DirectiveWarnOn = 15us

        [<Literal>]
        let DirectiveLoad = 16us

        [<Literal>]
        let DirectiveReference = 17us

        [<Literal>]
        let DirectiveIncludePath = 18us

        [<Literal>]
        let DirectiveTime = 20us

        [<Literal>]
        let DirectiveHelp = 21us

        [<Literal>]
        let DirectiveQuit = 22us

        [<Literal>]
        let DirectiveShebang = 23us

        [<Literal>]
        let DirectiveLineInt = 24us

        [<Literal>]
        let DirectiveIndent = 25us


        [<Literal>]
        let StartFSharpBlockComment = 100us // "(*IF-FSHARP" | "(*F#" starts a block comment ignored by F#

        [<Literal>]
        let EndFSharpBlockComment = 101us // "ENDIF-FSHARP*)" | "F#*)" ends a block comment ignored by F#

        [<Literal>]
        let StartOCamlBlockComment = 102us // "(*IF-CAML*)" | "(*IF-OCAML*)" starts a block comment ignored by OCaml

        [<Literal>]
        let EndOCamlBlockComment = 103us // "ENDIF-CAML*)" | "ENDIF-OCAML*)" ends a block comment ignored by OCaml


        // Fallback for others
        [<Literal>]
        let DirectiveInvalid = 255us

    module internal Identifier =
        [<Literal>]
        let Identifier = 1us

        [<Literal>]
        let BacktickedIdentifier = 2us

        [<Literal>]
        let ReservedIdentifierHash = 4us

        [<Literal>]
        let UnterminatedBacktickedIdentifier = 5us

        [<Literal>]
        let TypeParameter = 6us

        [<Literal>]
        let SourceDirectoryIdentifier = 7us

        [<Literal>]
        let SourceFileIdentifier = 8us

        [<Literal>]
        let LineIdentifier = 9us

    module internal Text =

        [<Literal>]
        let CharLiteral = 1us

        [<Literal>]
        let StringLiteral = 2us

        [<Literal>]
        let ByteArrayLiteral = 3us

        [<Literal>]
        let VerbatimStringLiteral = 4us

        [<Literal>]
        let VerbatimByteArrayLiteral = 5us

        [<Literal>]
        let String3Literal = 6us

        [<Literal>]
        let InterpolatedStringOpen = 7us

        [<Literal>]
        let InterpolatedStringClose = 8us

        [<Literal>]
        let VerbatimInterpolatedStringOpen = 9us

        [<Literal>]
        let VerbatimInterpolatedStringClose = 10us

        [<Literal>]
        let Interpolated3StringOpen = 11us

        [<Literal>]
        let Interpolated3StringClose = 12us

        [<Literal>]
        let InterpolatedStringFragment = 13us

        [<Literal>]
        let Interpolated3StringFragment = 14us

        [<Literal>]
        let VerbatimInterpolatedStringFragment = 15us

        [<Literal>]
        let InterpolatedExpressionOpen = 16us

        [<Literal>]
        let InterpolatedExpressionClose = 17us

        [<Literal>]
        let FormatPlaceholder = 18us

        [<Literal>]
        let EscapePercent = 19us

        [<Literal>]
        let EscapeLBrace = 20us

        [<Literal>]
        let EscapeRBrace = 21us

        [<Literal>]
        let VerbatimEscapeQuote = 22us

    module internal Numeric =

        [<Literal>]
        let NumSByte = 1us

        [<Literal>]
        let NumByte = 2us

        [<Literal>]
        let NumInt16 = 3us

        [<Literal>]
        let NumUInt16 = 4us

        [<Literal>]
        let NumInt32 = 5us

        [<Literal>]
        let NumUInt32 = 6us

        [<Literal>]
        let NumInt64 = 7us

        [<Literal>]
        let NumUInt64 = 8us

        [<Literal>]
        let NumNativeInt = 9us

        [<Literal>]
        let NumUNativeInt = 10us

        [<Literal>]
        let NumIEEE32 = 11us

        [<Literal>]
        let NumIEEE64 = 12us

        [<Literal>]
        let NumDecimal = 13us

        [<Literal>]
        let NumBigIntegerQ = 14us

        [<Literal>]
        let NumBigIntegerR = 15us

        [<Literal>]
        let NumBigIntegerZ = 16us

        [<Literal>]
        let NumBigIntegerI = 17us

        [<Literal>]
        let NumBigIntegerN = 18us

        [<Literal>]
        let NumBigIntegerG = 19us

        [<Literal>]
        let ReservedNumericLiteral = 31us


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

    module internal Invalid =


        [<Literal>]
        let InvalidCharTrigraphLiteral = 1us

        [<Literal>]
        let InvalidCharLiteral = 2us

        [<Literal>]
        let InvalidLongUnicodeCharLiteral = 3us

        [<Literal>]
        let UnterminatedCharLiteral = 4us

        [<Literal>]
        let UnterminatedStringLiteral = 5us

        [<Literal>]
        let UnterminatedVerbatimStringLiteral = 6us

        [<Literal>]
        let UnterminatedString3Literal = 7us

        [<Literal>]
        let UnterminatedInterpolatedString = 8us

        [<Literal>]
        let TooManyLBracesInInterpolated3String = 9us

        [<Literal>]
        let TooManyRBracesInInterpolated3String = 10us

        [<Literal>]
        let InvalidFormatPlaceholder = 11us

        [<Literal>]
        let InvalidFormatPercents = 12us

        [<Literal>]
        let UnmatchedInterpolatedRBrace = 13us

        [<Literal>]
        let Operator = 14us

        [<Literal>]
        let PrefixOperator = 15us


        [<Literal>]
        let Other = 255us

open TokenRepresentation

// ==========================================================
// THE TOKEN ENUM
// ==========================================================
type Token =
    /// Should not be used, only for default initialization
    | None = 0us

    // ==============================================================================
    // 1. SPECIAL / CONTROL (KindSpecial 0xE000)
    // 3.1, 3.2 Whitespace and control tokens
    // ==============================================================================
    | EOF = (KindSpecial ||| Special.EOF)
    | Newline = (KindSpecial ||| Special.Newline)
    | Whitespace = (KindSpecial ||| Special.Whitespace)
    | Tab = (KindSpecial ||| Special.Tab)

    // Comments (Using Special IDs we defined or mapped)
    | LineComment = (KindSpecial ||| Special.LineComment)
    | BlockCommentStart = (KindSpecial ||| Special.BlockCommentStart) // Start marker
    | BlockCommentEnd = (KindSpecial ||| Special.BlockCommentEnd) // End marker (Parser logic usually handles nesting)
    | Indent = (KindSpecial ||| Special.Indent)

    // 3.3 Conditional Compilation
    | IfDirective = (KindSpecial ||| Special.DirectiveIf)
    | ElseDirective = (KindSpecial ||| Special.DirectiveElse)
    | EndIfDirective = (KindSpecial ||| Special.DirectiveEndIf)

    // 3.8.4 Shebang
    | Shebang = (KindSpecial ||| Special.DirectiveShebang) // #!/bin/usr/env fsharpi --exec

    // 3.9 Line Directives
    | LineDirective = (KindSpecial ||| Special.DirectiveLine)
    | LineIntDirective = (KindSpecial ||| Special.DirectiveLineInt)

    // 12.4 Compiler Directives
    | NoWarnDirective = (KindSpecial ||| Special.DirectiveNoWarn)
    | WarnOnDirective = (KindSpecial ||| Special.DirectiveWarnOn)
    | ReferenceDirective = (KindSpecial ||| Special.DirectiveReference)
    | IncludePathDirective = (KindSpecial ||| Special.DirectiveIncludePath)
    | LoadDirective = (KindSpecial ||| Special.DirectiveLoad)
    | TimeDirective = (KindSpecial ||| Special.DirectiveTime)
    | HelpDirective = (KindSpecial ||| Special.DirectiveHelp)
    | QuitDirective = (KindSpecial ||| Special.DirectiveQuit)

    // 19.4 File Extensions and Lexical Matters
    | IndentDirective = (KindSpecial ||| Special.DirectiveIndent) // #indent
    | InvalidDirective = (KindSpecial ||| Special.DirectiveInvalid) // Any other invalid directive starting with #

    // 19.1 ML Compatibility
    | StartFSharpBlockComment = (KindSpecial ||| Special.StartFSharpBlockComment) // "(*IF-FSHARP" | "(*F#" starts a block comment ignored by F#
    | EndFSharpBlockComment = (KindSpecial ||| Special.EndFSharpBlockComment) // "ENDIF-FSHARP*)" | "F#*)" ends a block comment ignored by F#
    | StartOCamlBlockComment = (KindSpecial ||| Special.StartOCamlBlockComment) // "(*IF-CAML*)" | "(*IF-OCAML*)" starts a block comment ignored by OCaml
    | EndOCamlBlockComment = (KindSpecial ||| Special.EndOCamlBlockComment) // "ENDIF-CAML*)" | "ENDIF-OCAML*)" ends a block comment ignored by OCaml


    // ==============================================================================
    // 2. IDENTIFIERS (KindIdentifier 0x2000)
    // ==============================================================================

    // 3.4 Identifiers
    | Identifier = (KindIdentifier ||| Identifier.Identifier)
    | BacktickedIdentifier = (KindIdentifier ||| Identifier.BacktickedIdentifier)
    | ReservedIdentifierHash = (KindIdentifier ||| Identifier.ReservedIdentifierHash)
    | UnterminatedBacktickedIdentifier = (KindIdentifier ||| Identifier.UnterminatedBacktickedIdentifier)
    | TypeParameter = (KindIdentifier ||| Identifier.TypeParameter)

    // 3.11 Identifier Replacements
    | SourceDirectoryIdentifier = (KindIdentifier ||| Identifier.SourceDirectoryIdentifier)
    | SourceFileIdentifier = (KindIdentifier ||| Identifier.SourceFileIdentifier)
    | LineIdentifier = (KindIdentifier ||| Identifier.LineIdentifier)

    // ==============================================================================
    // 3. KEYWORDS & SYMBOLS (KindKeyword 0x0000)
    // ==============================================================================

    // 3.4 token ident-keyword
    | KWAbstract = (KindKeyword ||| KW.Abstract)
    | KWAnd = (KindKeyword ||| KW.And)
    | KWAs = (KindKeyword ||| KW.As)
    | KWAssert = (KindKeyword ||| KW.Assert)
    | KWBase = (KindKeyword ||| KW.Base)
    | KWBegin = (KindKeyword ||| KW.Begin)
    | KWClass = (KindKeyword ||| KW.Class)
    | KWConst = (KindKeyword ||| KW.Const)
    | KWDefault = (KindKeyword ||| KW.Default)
    | KWDelegate = (KindKeyword ||| KW.Delegate)
    | KWDo = (KindKeyword ||| KW.Do)
    | KWDone = (KindKeyword ||| KW.Done)
    | KWDowncast = (KindKeyword ||| KW.Downcast)
    | KWDownto = (KindKeyword ||| KW.Downto)
    | KWElif = (KindKeyword ||| KW.Elif)
    | KWElse = (KindKeyword ||| KW.Else)
    | KWEnd = (KindKeyword ||| KW.End)
    | KWException = (KindKeyword ||| KW.Exception)
    | KWExtern = (KindKeyword ||| KW.Extern)
    | KWFalse = (KindKeyword ||| KW.False)
    | KWFinally = (KindKeyword ||| KW.Finally)
    | KWFixed = (KindKeyword ||| KW.Fixed)
    | KWFor = (KindKeyword ||| KW.For)
    | KWFun = (KindKeyword ||| KW.Fun)
    | KWFunction = (KindKeyword ||| KW.Function)
    | KWGlobal = (KindKeyword ||| KW.Global)
    | KWIf = (KindKeyword ||| KW.If)
    | KWIn = (KindKeyword ||| KW.In)
    | KWInherit = (KindKeyword ||| KW.Inherit)
    | KWInline = (KindKeyword ||| KW.Inline)
    | KWInterface = (KindKeyword ||| KW.Interface)
    | KWInternal = (KindKeyword ||| KW.Internal)
    | KWLazy = (KindKeyword ||| KW.Lazy)
    | KWLet = (KindKeyword ||| KW.Let)
    | KWMatch = (KindKeyword ||| KW.Match)
    | KWMember = (KindKeyword ||| KW.Member)
    | KWModule = (KindKeyword ||| KW.Module)
    | KWMutable = (KindKeyword ||| KW.Mutable)
    | KWNamespace = (KindKeyword ||| KW.Namespace)
    | KWNew = (KindKeyword ||| KW.New)
    | KWNull = (KindKeyword ||| KW.Null)
    | KWOf = (KindKeyword ||| KW.Of)
    | KWOpen = (KindKeyword ||| KW.Open)
    | KWOr = (KindKeyword ||| KW.Or)
    | KWOverride = (KindKeyword ||| KW.Override)
    | KWPrivate = (KindKeyword ||| KW.Private)
    | KWPublic = (KindKeyword ||| KW.Public)
    | KWRec = (KindKeyword ||| KW.Rec)
    | KWReturn = (KindKeyword ||| KW.Return)
    | KWSig = (KindKeyword ||| KW.Sig)
    | KWStatic = (KindKeyword ||| KW.Static)
    | KWStruct = (KindKeyword ||| KW.Struct)
    | KWThen = (KindKeyword ||| KW.Then)
    | KWTo = (KindKeyword ||| KW.To)
    | KWTrue = (KindKeyword ||| KW.True)
    | KWTry = (KindKeyword ||| KW.Try)
    | KWType = (KindKeyword ||| KW.Type)
    | KWUpcast = (KindKeyword ||| KW.Upcast)
    | KWUse = (KindKeyword ||| KW.Use)
    | KWVal = (KindKeyword ||| KW.Val)
    | KWVoid = (KindKeyword ||| KW.Void)
    | KWWhen = (KindKeyword ||| KW.When)
    | KWWhile = (KindKeyword ||| KW.While)
    | KWWith = (KindKeyword ||| KW.With)
    | KWYield = (KindKeyword ||| KW.Yield)

    // 3.4 token reserved-ident-keyword
    // let ReservedAtomic = 70us Unreserved in FS-1016
    | KWReservedBreak = (KindKeyword ||| KW.ReservedBreak)
    | KWReservedChecked = (KindKeyword ||| KW.ReservedChecked)
    | KWReservedComponent = (KindKeyword ||| KW.ReservedComponent)
    | KWReservedConstraint = (KindKeyword ||| KW.ReservedConstraint)
    // | KWReservedConstructor = (KindKeyword ||| KW.ReservedConstructor) Unreserved in FS-1016
    | KWReservedContinue = (KindKeyword ||| KW.ReservedContinue)
    // | KWReservedEager = (KindKeyword ||| KW.ReservedEager) Unreserved in FS-1016
    | KWReservedFori = (KindKeyword ||| KW.ReservedFori)
    // | KWReservedFunctor = (KindKeyword ||| KW.ReservedFunctor) Unreserved in FS-1016
    | KWReservedInclude = (KindKeyword ||| KW.ReservedInclude)
    // | KWReservedMeasure = (KindKeyword ||| KW.ReservedMeasure) Unreserved in FS-1016
    // | KWReservedMethod = (KindKeyword ||| KW.ReservedMethod) Unreserved in FS-1016
    | KWReservedMixin = (KindKeyword ||| KW.ReservedMixin)
    // | KWReservedObject = (KindKeyword ||| KW.ReservedObject) Unreserved in FS-1016
    | KWReservedParallel = (KindKeyword ||| KW.ReservedParallel)
    | KWReservedParams = (KindKeyword ||| KW.ReservedParams)
    | KWReservedProcess = (KindKeyword ||| KW.ReservedProcess)
    | KWReservedProtected = (KindKeyword ||| KW.ReservedProtected)
    | KWReservedPure = (KindKeyword ||| KW.ReservedPure)
    // | KWReservedRecursive = (KindKeyword ||| KW.ReservedRecursive) Unreserved in FS-1016
    | KWReservedSealed = (KindKeyword ||| KW.ReservedSealed)
    | KWReservedTailcall = (KindKeyword ||| KW.ReservedTailcall)
    | KWReservedTrait = (KindKeyword ||| KW.ReservedTrait)
    | KWReservedVirtual = (KindKeyword ||| KW.ReservedVirtual)
    // | KWReservedVolatile = (KindKeyword ||| KW.ReservedVolatile) Unreserved in FS-1016

    // 19.2 token ocaml-ident-keyword
    | KWAsr = (KindKeyword ||| KW.Asr)
    | KWLand = (KindKeyword ||| KW.Land)
    | KWLor = (KindKeyword ||| KW.Lor)
    | KWLsl = (KindKeyword ||| KW.Lsl)
    | KWLsr = (KindKeyword ||| KW.Lsr)
    | KWLxor = (KindKeyword ||| KW.Lxor)
    | KWMod = (KindKeyword ||| KW.Mod)

    // 3.6 symbolic keywords for computation expressions
    | KWAndBang = (KindKeyword ||| KW.AndBang)
    | KWDoBang = (KindKeyword ||| KW.DoBang)
    | KWLetBang = (KindKeyword ||| KW.LetBang)
    | KWMatchBang = (KindKeyword ||| KW.MatchBang)
    | KWReturnBang = (KindKeyword ||| KW.ReturnBang)
    | KWUseBang = (KindKeyword ||| KW.UseBang)
    | KWYieldBang = (KindKeyword ||| KW.YieldBang)
    | OpBar = (KindKeyword ||| KW.Bar)
    | OpArrowRight = (KindKeyword ||| KW.RightArrow)
    | OpArrowLeft = (KindKeyword ||| KW.LeftArrow)
    | OpDot = (KindKeyword ||| KW.Dot)
    | OpColon = (KindKeyword ||| KW.Colon)
    | KWLParen = (KindKeyword ||| KW.LParen)
    | KWRParen = (KindKeyword ||| KW.RParen)
    | KWLBracket = (KindKeyword ||| KW.LBracket)
    | KWRBracket = (KindKeyword ||| KW.RBracket)
    | KWLAttrBracket = (KindKeyword ||| KW.LAttrBracket)
    | KWRAttrBracket = (KindKeyword ||| KW.RAttrBracket)
    | KWLArrayBracket = (KindKeyword ||| KW.LArrayBracket)
    | KWRArrayBracket = (KindKeyword ||| KW.RArrayBracket)
    | KWLBrace = (KindKeyword ||| KW.LBrace)
    | KWRBrace = (KindKeyword ||| KW.RBrace)
    | KWSingleQuote = (KindKeyword ||| KW.SingleQuote)
    | KWHash = (KindKeyword ||| KW.Hash)
    | OpDowncast = (KindKeyword ||| KW.OpDowncast) // :?>
    | OpTypeTest = (KindKeyword ||| KW.OpTypeTest) // :?
    | OpUpcast = (KindKeyword ||| KW.OpUpcast) // :>
    | OpRange = (KindKeyword ||| KW.OpRange) // ..
    | OpRangeStep = (KindKeyword ||| KW.OpRangeStep) // ..
    | KWColonColon = (KindKeyword ||| KW.ColonColon) // ::
    | OpAssignment = (KindKeyword ||| KW.ColonEquals) // :=
    | OpDoubleSemicolon = (KindKeyword ||| KW.SemiSemi) // ;;
    | OpSemicolon = (KindKeyword ||| KW.Semi) // ;
    | OpEquality = (KindKeyword ||| KW.Equals) // =
    | OpDynamic = (KindKeyword ||| KW.QMark) // ?
    | OpQMarkQMark = (KindKeyword ||| KW.QMarkQMark) // ??
    | KWOpDeclareMultiply = (KindKeyword ||| KW.OpDeclareMultiply) // (*)
    | OpQuotationTypedLeft = (KindKeyword ||| KW.LQuoteTyped) // <@
    | OpQuotationTypedRight = (KindKeyword ||| KW.RQuoteTyped) // @>
    | OpQuotationUntypedLeft = (KindKeyword ||| KW.LQuoteUntyped) // <@@
    | OpQuotationUntypedRight = (KindKeyword ||| KW.RQuoteUntyped) // @@>
    | Wildcard = (KindKeyword ||| KW.Underscore) // _
    | Unit = (KindKeyword ||| KW.Unit) // ()

    // 3.6 token reserved-symbolic-sequence
    | KWReservedTwiddle = (KindKeyword ||| KW.ReservedTwiddle)
    | KWReservedBacktick = (KindKeyword ||| KW.ReservedBacktick)
    | KWInvalidPrefixOperator = (KindKeyword ||| KW.InvalidPrefixOperator) // Only a limited set of prefix operators are valid
    | KWInvalidOperator = (KindKeyword ||| KW.InvalidOperator) // An invalid operator (e.g., contains only ignored prefix characters)
    | KWReservedOperator = (KindKeyword ||| KW.ReservedOperator) // A reserved operator (i.e. containing a reserved symbol $ or :)

    // 3.7 Symbolic Operators
    | OpDynamicAssignment = (KindKeyword ||| KW.QMarkLeftArrow) // ?<-
    | OpNil = (KindKeyword ||| KW.Nil) // []
    | OpDereference = (KindKeyword ||| KW.Dereference) // !
    | OpComma = (KindKeyword ||| KW.Comma) // , (Tuples, Arguments)
    | ReservedOperator = (KindKeyword ||| KW.ReservedOperator) // A reserved operator (i.e. containing a reserved symbol $ or :)

    // Indexer Operators, deprecated and undocumented but still recognized by the lexer
    | OpIndexSetIdentifier = (KindKeyword ||| KW.OpIndexSetIdentifier) // op_IndexSet .[]<-
    | OpIndexGetIdentifier = (KindKeyword ||| KW.OpIndexGetIdentifier) // op_IndexGet .[]
    | OpIndexSet2Identifier = (KindKeyword ||| KW.OpIndexSet2Identifier) // op_IndexSet .[,]<-
    | OpIndexGet2Identifier = (KindKeyword ||| KW.OpIndexGet2Identifier) // op_IndexGet .[,]
    | OpIndexSet3Identifier = (KindKeyword ||| KW.OpIndexSet3Identifier) // op_IndexSet .[,,]<-
    | OpIndexGet3Identifier = (KindKeyword ||| KW.OpIndexGet3Identifier) // op_IndexGet .[,,]
    | OpIndexSet4Identifier = (KindKeyword ||| KW.OpIndexSet4Identifier) // op_IndexSet .[,,,]<-
    | OpIndexGet4Identifier = (KindKeyword ||| KW.OpIndexGet4Identifier) // op_IndexGet .[,,,]
    | OpIndexSetParenIdentifier = (KindKeyword ||| KW.OpIndexSetParenIdentifier) // op_IndexSetParen .()<-
    | OpIndexGetParenIdentifier = (KindKeyword ||| KW.OpIndexGetParenIdentifier) // op_IndexGetParen .()
    // | OpIndexLeft = (KindKeyword ||| KW.OpIndexLeft) // op_IndexLeft .[ Lexer emits DOT + LBRACK
    // | OpIndexLeftParen = (KindKeyword ||| KW.OpIndexLeftParen) // op_IndexLeftParen .( Lexer emits DOT + LPAREN

    // ==============================================================================
    // 4. VIRTUAL KEYWORDS (Synthesized)
    // ==============================================================================
    // 15.1.2 Inserted Tokens
    // virtual keywords inserted by the parser
    | VirtualIn = (IsVirtual ||| KindKeyword ||| KW.In)
    | VirtualDone = (IsVirtual ||| KindKeyword ||| KW.Done)
    | VirtualBegin = (IsVirtual ||| KindKeyword ||| KW.Begin)
    | VirtualEnd = (IsVirtual ||| KindKeyword ||| KW.End)
    | VirtualSep = (IsVirtual ||| KindKeyword ||| 1024us)
    | VirtualApp = (IsVirtual ||| KindKeyword ||| 1025us)
    | VirtualTyApp = (IsVirtual ||| KindKeyword ||| 1026us)
    | VirtualLet = (IsVirtual ||| KindKeyword ||| KW.Let)
    | VirtualUse = (IsVirtual ||| KindKeyword ||| KW.Use)
    | VirtualLetBang = (IsVirtual ||| KindKeyword ||| KW.LetBang)
    | VirtualUseBang = (IsVirtual ||| KindKeyword ||| KW.UseBang)
    | VirtualDo = (IsVirtual ||| KindKeyword ||| KW.Do)
    | VirtualDoBang = (IsVirtual ||| KindKeyword ||| KW.DoBang)
    | VirtualThen = (IsVirtual ||| KindKeyword ||| KW.Then)
    | VirtualElse = (IsVirtual ||| KindKeyword ||| KW.Else)
    | VirtualWith = (IsVirtual ||| KindKeyword ||| KW.With)
    | VirtualFunction = (IsVirtual ||| KindKeyword ||| KW.Function)
    | VirtualFun = (IsVirtual ||| KindKeyword ||| KW.Fun)

    // ==============================================================================
    // 5. OPERATORS (KindOperator 0x8000)
    // ==============================================================================
    // Payload = Precedence (bits 0-5) | CanBePrefix (bit 6)

    // Custom / Generic Operator
    // (Used when the lexer finds an unknown sequence like +*+)
    // Standard Operators (Lexer usually calculates these, but here are the prototypes)
    // Note: We use the helper module `Precedence` defined above

    // 4.1 Operator Names
    // These operators are not keywords, but regular operators with names
    // The values in this section define their precedence and properties, they are not unique IDs
    | OpAddition = (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd) // +
    | OpSubtraction = (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd) // -
    | OpMultiply = (KindOperator ||| Precedence.InfixMultiply) // *
    | OpDivision = (KindOperator ||| Precedence.InfixMultiply) // /
    | OpExponentiation = (KindOperator ||| Precedence.Exponentiation) // **
    | OpAppend = (KindOperator ||| Precedence.Concatenate) // @ precedence is not documented, assuming Concatenate level
    | OpConcatenate = (KindOperator ||| Precedence.Concatenate) // ^
    | OpModulus = (KindOperator ||| CanBePrefix ||| Precedence.InfixMultiply) // %
    | OpBitwiseAnd = (KindOperator ||| Precedence.LogicalAndBitwise) // &&&
    | OpBitwiseOr = (KindOperator ||| Precedence.LogicalAndBitwise) // |||
    | OpExclusiveOr = (KindOperator ||| Precedence.LogicalAndBitwise) // ^^^
    | OpLeftShift = (KindOperator ||| Precedence.LogicalAndBitwise) // <<<
    | OpLogicalNot = (KindOperator ||| CanBePrefix ||| Precedence.LogicalAndBitwise) // ~~~
    | OpRightShift = (KindOperator ||| Precedence.LogicalAndBitwise) // >>>
    | OpUnaryPlus = (KindOperator ||| CanBePrefix ||| Precedence.Prefix) // ~+
    | OpUnaryNegation = (KindOperator ||| CanBePrefix ||| Precedence.Prefix) // ~-
    | OpInequality = (KindOperator ||| Precedence.LogicalAndBitwise) // <>
    | OpLessThanOrEqual = (KindOperator ||| Precedence.LogicalAndBitwise) // <=
    | OpGreaterThanOrEqual = (KindOperator ||| Precedence.LogicalAndBitwise) // >=
    | OpLessThan = (KindOperator ||| Precedence.LogicalAndBitwise) // <
    | OpGreaterThan = (KindOperator ||| Precedence.LogicalAndBitwise) // >
    | OpPipeRight = (KindOperator ||| Precedence.Pipe) // |>
    | OpPipeRight2 = (KindOperator ||| Precedence.Pipe) // ||>
    | OpPipeRight3 = (KindOperator ||| Precedence.Pipe) // |||>
    | OpPipeLeft = (KindOperator ||| Precedence.LogicalAndBitwise) // <| didn't find official precedence, assuming LogicalAndBitwise level as first char is <
    | OpPipeLeft2 = (KindOperator ||| Precedence.LogicalAndBitwise) // <||
    | OpPipeLeft3 = (KindOperator ||| Precedence.LogicalAndBitwise) // <|||
    | OpComposeRight = (KindOperator ||| Precedence.LogicalAndBitwise) // >>
    | OpComposeLeft = (KindOperator ||| Precedence.LogicalAndBitwise) // <<
    | OpSplice = (KindOperator ||| CanBePrefix ||| Precedence.Dot) // ~% (used in F# quotations)
    | OpSpliceUntyped = (KindOperator ||| CanBePrefix ||| Precedence.Dot) // ~%%
    | OpAddressOf = (KindOperator ||| CanBePrefix ||| Precedence.Dot) // ~&
    | OpIntegerAddressOf = (KindOperator ||| CanBePrefix ||| Precedence.Dot) // ~&&
    | OpBooleanOr = (KindOperator ||| Precedence.BooleanOr) // ||
    | OpBooleanAnd = (KindOperator ||| Precedence.BooleanAnd) // &&
    | OpAdditionAssignment = (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd) // +=
    | OpSubtractionAssignment = (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd) // -=
    | OpMultiplyAssignment = (KindOperator ||| Precedence.InfixMultiply) // *=
    | OpDivisionAssignment = (KindOperator ||| Precedence.InfixMultiply) // /=
    | OpNotEqual = (KindOperator ||| Precedence.LogicalAndBitwise) // != (F# uses <> but != is in the spec too)


    | OpCons = (KindKeyword ||| KW.ColonColon) // :: is Structural (KindKeyword), not Operator

    // ==============================================================================
    // 6. NUMERIC LITERALS (KindNumber 0x6000)
    // ==============================================================================
    // KindNumber | Base (2 bits) | Type (4 bits)

    // 3.8 Numeric Literals
    | NumSByte = (KindNumericLiteral ||| Numeric.NumSByte) // 0y
    | NumSByteHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumSByte) // 0x0y
    | NumSByteOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumSByte) // 0o0y
    | NumSByteBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumSByte) // 0b0000y

    | NumByte = (KindNumericLiteral ||| Numeric.NumByte) // 0uy
    | NumByteHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumByte) // 0x0uy
    | NumByteOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumByte) // 0o0uy
    | NumByteBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumByte) // 0b0000uy

    | NumInt16 = (KindNumericLiteral ||| Numeric.NumInt16) // 0s
    | NumInt16Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumInt16) // 0x0s
    | NumInt16Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumInt16) // 0o0s
    | NumInt16Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumInt16) // 0b0000s

    | NumUInt16 = (KindNumericLiteral ||| Numeric.NumUInt16) // 0us
    | NumUInt16Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumUInt16) // 0x0us
    | NumUInt16Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumUInt16) // 0o0us
    | NumUInt16Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumUInt16) // 0b0000us

    | NumInt32 = (KindNumericLiteral ||| Numeric.NumInt32) // 0
    | NumInt32Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumInt32) // 0x0
    | NumInt32Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumInt32) // 0o0
    | NumInt32Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumInt32) // 0b0000

    | NumUInt32 = (KindNumericLiteral ||| Numeric.NumUInt32) // 0u
    | NumUInt32Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumUInt32) // 0x0u
    | NumUInt32Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumUInt32) // 0o0u
    | NumUInt32Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumUInt32) // 0b0000u

    | NumInt64 = (KindNumericLiteral ||| Numeric.NumInt64) // 0L
    | NumInt64Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumInt64) // 0x0L
    | NumInt64Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumInt64) // 0o0L
    | NumInt64Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumInt64) // 0b0000L

    | NumUInt64 = (KindNumericLiteral ||| Numeric.NumUInt64) // 0UL
    | NumUInt64Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumUInt64) // 0x0UL
    | NumUInt64Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumUInt64) // 0o0UL
    | NumUInt64Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumUInt64) // 0b0000UL

    | NumNativeInt = (KindNumericLiteral ||| Numeric.NumNativeInt) // 0n
    | NumNativeIntHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumNativeInt) // 0x0n
    | NumNativeIntOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumNativeInt) // 0o0n
    | NumNativeIntBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumNativeInt) // 0b0000n

    | NumUNativeInt = (KindNumericLiteral ||| Numeric.NumUNativeInt) // 0un
    | NumUNativeIntHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumUNativeInt) // 0x0un
    | NumUNativeIntOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumUNativeInt) // 0o0un
    | NumUNativeIntBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumUNativeInt) // 0b0000un

    | NumIEEE32 = (KindNumericLiteral ||| Numeric.NumIEEE32) // 0.0f
    | NumIEEE32Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumIEEE32) // 0x0.0f
    | NumIEEE32Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumIEEE32) // 0o0.0f
    | NumIEEE32Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumIEEE32) // 0b0000.0f

    | NumIEEE64 = (KindNumericLiteral ||| Numeric.NumIEEE64) // 0.0
    | NumIEEE64Hex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumIEEE64) // 0x0.0
    | NumIEEE64Octal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumIEEE64) // 0o0.0
    | NumIEEE64Binary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumIEEE64) // 0b0000.0

    | NumDecimal = (KindNumericLiteral ||| Numeric.NumDecimal) // ( float | int ) [Mm]
    | NumDecimalHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.NumDecimal) // 0x0M
    | NumDecimalOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.NumDecimal) // 0o0M
    | NumDecimalBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.NumDecimal) // 0b0000M

    // BigInteger formats only support decimal base
    // int ('Q' | ' R' | 'Z' | 'I' | 'N' | 'G')
    | NumBigIntegerQ = (KindNumericLiteral ||| Numeric.NumBigIntegerQ)
    | NumBigIntegerR = (KindNumericLiteral ||| Numeric.NumBigIntegerR)
    | NumBigIntegerZ = (KindNumericLiteral ||| Numeric.NumBigIntegerZ)
    | NumBigIntegerI = (KindNumericLiteral ||| Numeric.NumBigIntegerI)
    | NumBigIntegerN = (KindNumericLiteral ||| Numeric.NumBigIntegerN)
    | NumBigIntegerG = (KindNumericLiteral ||| Numeric.NumBigIntegerG)

    | ReservedNumericLiteral = (KindNumericLiteral ||| Numeric.ReservedNumericLiteral) // (xint | ieee32 | ieee64) ident-char+
    | ReservedNumericLiteralHex = (KindNumericLiteral ||| NumericBaseHex ||| Numeric.ReservedNumericLiteral) // 0x0ident-char+
    | ReservedNumericLiteralOctal = (KindNumericLiteral ||| NumericBaseOctal ||| Numeric.ReservedNumericLiteral) // 0o0ident-char+
    | ReservedNumericLiteralBinary = (KindNumericLiteral ||| NumericBaseBinary ||| Numeric.ReservedNumericLiteral) // 0b0000ident-char+


    // ==============================================================================
    // 7. TEXT LITERALS (KindString 0x4000)
    // ==============================================================================

    // 3.5 Strings and Characters
    | CharLiteral = (KindTextLiteral ||| Text.CharLiteral)
    | StringLiteral = (KindTextLiteral ||| Text.StringLiteral)
    | ByteArrayLiteral = (KindTextLiteral ||| Text.ByteArrayLiteral) // e.g. "abc"B
    | VerbatimStringLiteral = (KindTextLiteral ||| Text.VerbatimStringLiteral)
    | VerbatimByteArrayLiteral = (KindTextLiteral ||| Text.VerbatimByteArrayLiteral) // e.g. @"abc"B
    | String3Literal = (KindTextLiteral ||| Text.String3Literal) // Triple-quoted string
    | InterpolatedStringOpen = (KindTextLiteral ||| Text.InterpolatedStringOpen) // $"
    | InterpolatedStringClose = (KindTextLiteral ||| Text.InterpolatedStringClose)
    | VerbatimInterpolatedStringOpen = (KindTextLiteral ||| Text.VerbatimInterpolatedStringOpen)
    | VerbatimInterpolatedStringClose = (KindTextLiteral ||| Text.VerbatimInterpolatedStringClose)
    | Interpolated3StringOpen = (KindTextLiteral ||| Text.Interpolated3StringOpen)
    | Interpolated3StringClose = (KindTextLiteral ||| Text.Interpolated3StringClose)
    | InterpolatedStringFragment = (KindTextLiteral ||| Text.InterpolatedStringFragment) // Constant string fragment
    | Interpolated3StringFragment = (KindTextLiteral ||| Text.Interpolated3StringFragment) // Constant string fragment
    | VerbatimInterpolatedStringFragment = (KindTextLiteral ||| Text.VerbatimInterpolatedStringFragment)
    | InterpolatedExpressionOpen = (KindTextLiteral ||| Text.InterpolatedExpressionOpen) // one or more {
    | InterpolatedExpressionClose = (KindTextLiteral ||| Text.InterpolatedExpressionClose) // one or more }
    | FormatPlaceholder = (KindTextLiteral ||| Text.FormatPlaceholder)
    | EscapePercent = (KindTextLiteral ||| Text.EscapePercent)
    | EscapeLBrace = (KindTextLiteral ||| Text.EscapeLBrace)
    | EscapeRBrace = (KindTextLiteral ||| Text.EscapeRBrace)
    | VerbatimEscapeQuote = (KindTextLiteral ||| Text.VerbatimEscapeQuote) // "" inside a verbatim string


    // ==============================================================================
    // 8. INVALID / ERRORS (KindInvalid 0xC000)
    // ==============================================================================

    // Invalid text literals
    | InvalidCharTrigraphLiteral = (KindInvalid ||| Invalid.InvalidCharTrigraphLiteral) // > 255
    | InvalidCharLiteral = (KindInvalid ||| Invalid.InvalidCharLiteral)
    | InvalidLongUnicodeCharLiteral = (KindInvalid ||| Invalid.InvalidLongUnicodeCharLiteral)
    | UnterminatedCharLiteral = (KindInvalid ||| Invalid.UnterminatedCharLiteral)
    | UnterminatedStringLiteral = (KindInvalid ||| Invalid.UnterminatedStringLiteral)
    | UnterminatedVerbatimStringLiteral = (KindInvalid ||| Invalid.UnterminatedVerbatimStringLiteral)
    | UnterminatedString3Literal = (KindInvalid ||| Invalid.UnterminatedString3Literal)
    | UnterminatedInterpolatedString = (KindInvalid ||| Invalid.UnterminatedInterpolatedString)
    | TooManyLBracesInInterpolated3String = (KindInvalid ||| Invalid.TooManyLBracesInInterpolated3String)
    | TooManyRBracesInInterpolated3String = (KindInvalid ||| Invalid.TooManyRBracesInInterpolated3String)
    | InvalidFormatPlaceholder = (KindInvalid ||| Invalid.InvalidFormatPlaceholder)
    | InvalidFormatPercents = (KindInvalid ||| Invalid.InvalidFormatPercents)
    | UnmatchedInterpolatedRBrace = (KindInvalid ||| Invalid.UnmatchedInterpolatedRBrace) // Single } is invalid outside an expression

    // Invalid operators
    | InvalidOperator = (KindInvalid ||| Invalid.Operator) // An invalid operator (e.g., contains only ignored prefix characters)
    | InvalidPrefixOperator = (KindInvalid ||| Invalid.PrefixOperator) // An invalid prefix operator (i.e. starts with ~ but is not a valid prefix operator)

    /// Any other unlexed character, when the lexer is done we shouldn't have any of these left
    | OtherUnlexed = (KindInvalid ||| Invalid.Other)


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
                    ofUInt16 (KindOperator ||| Precedence.LogicalAndBitwise) // !=
                else
                    ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.Prefix) // !
            | '%' -> ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.InfixMultiply)
            | '&' -> ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.LogicalAndBitwise)
            | '*' -> ofUInt16 (KindOperator ||| Precedence.InfixMultiply)
            | '+' -> ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd)
            | '-' -> ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.InfixAdd)
            | '/' -> ofUInt16 (KindOperator ||| Precedence.InfixMultiply)
            | '<' -> ofUInt16 (KindOperator ||| Precedence.LogicalAndBitwise)
            | '=' -> ofUInt16 (KindOperator ||| Precedence.LogicalAndBitwise)
            | '>' -> ofUInt16 (KindOperator ||| Precedence.LogicalAndBitwise)
            | '@' -> ofUInt16 (KindOperator ||| Precedence.Concatenate) // TODO https://github.com/fsharp/fslang-spec/issues/70
            | '^' -> ofUInt16 (KindOperator ||| Precedence.Concatenate)
            | '|' -> ofUInt16 (KindOperator ||| Precedence.LogicalAndBitwise)
            | '~' ->
                if
                    trimIgnored.Length = 1 // ~ alone is not a valid operator
                    || trimIgnored.Length < span.Length // prefix operators cannot start with ignored chars
                then
                    if span.Length > 3 && span.Trim('~').Length = 0 then
                        // any number of ~ is a valid prefix operator
                        ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.Prefix)
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
                        | "~&&" -> ofUInt16 (KindOperator ||| CanBePrefix ||| Precedence.Prefix)
                        | _ -> Token.InvalidPrefixOperator
                else
                    Token.InvalidPrefixOperator

            | _ -> invalidArg "span" (sprintf "Invalid custom operator: %s" (span.ToString()))

module internal TokenInfo =

    let private hasFlag (token: Token) (flag: uint16) = (uint16 token &&& flag) <> 0us

    let kind (token: Token) =
        (uint16 token) >>> KindShift |> int |> enum<TokenKind>

    let isIdentifier (token: Token) = token |> kind = TokenKind.Identifier

    let isLiteral (token: Token) =
        let tKind = kind token
        tKind = TokenKind.NumericLiteral || tKind = TokenKind.TextLiteral

    let isNumeric (token: Token) =
        token |> kind = TokenKind.NumericLiteral

    let isText (token: Token) = token |> kind = TokenKind.TextLiteral
    let isOperator (token: Token) = token |> kind = TokenKind.Operator
    let isKeyword (token: Token) = token |> kind = TokenKind.Keyword
    let isInvalid (token: Token) = token |> kind = TokenKind.Invalid

    let isDeprecated (token: Token) = failwith "TODO: Implement isDeprecated"
    let isReserved (token: Token) = failwith "TODO: Implement isReserved"
    let inComment (token: Token) = hasFlag token InComment
    let isVirtual (token: Token) = hasFlag token IsVirtual

    let isVirtualKeyword (token: Token) =
        isKeyword token && hasFlag token IsVirtual

    let withoutCommentFlags (token: Token) : Token =
        let mask = ~~~InComment
        uint16 token &&& mask |> uint16 |> LanguagePrimitives.EnumOfValue

    let withoutFlags (token: Token) : Token =
        let mask = ~~~(InComment ||| IsVirtual)
        uint16 token &&& mask |> uint16 |> LanguagePrimitives.EnumOfValue

    let canBePrefix token = hasFlag token CanBePrefix
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    // 4.4.2 Precedence of Symbolic Operators and Pattern/Expression Constructs

    let isOperatorKeyword (token: Token) =
        if isKeyword token then
            match withoutCommentFlags token with
            | Token.OpSemicolon
            | Token.OpArrowRight
            | Token.OpAssignment
            | Token.OpComma
            | Token.OpCons
            | Token.OpTypeTest
            | Token.OpBar
            | Token.OpDot
            | Token.KWOr
            | Token.KWDowncast
            | Token.KWUpcast
            | Token.OpDowncast
            | Token.OpUpcast -> true
            | _ -> false
        else
            false

    let operatorPrecedence (token: Token) : PrecedenceLevel =
        if isKeyword token then
            // All keyword operators are left associative
            match withoutCommentFlags token with
            | Token.KWAs -> PrecedenceLevel.As
            | Token.KWWhen -> PrecedenceLevel.When
            | Token.OpSemicolon -> PrecedenceLevel.Semicolon
            | Token.KWLet -> PrecedenceLevel.Let
            | Token.KWFunction
            | Token.KWFun
            | Token.KWMatch
            | Token.KWTry -> PrecedenceLevel.Function
            | Token.KWIf -> PrecedenceLevel.If
            | Token.OpArrowRight -> PrecedenceLevel.Arrow
            | Token.OpAssignment -> PrecedenceLevel.Assignment
            | Token.OpComma -> PrecedenceLevel.Comma
            | Token.KWOr -> PrecedenceLevel.LogicalOr
            | Token.KWDowncast
            | Token.KWUpcast
            | Token.OpDowncast
            | Token.OpUpcast -> PrecedenceLevel.Cast
            | Token.OpCons -> PrecedenceLevel.Cons
            | Token.OpTypeTest -> PrecedenceLevel.TypeTest
            | Token.KWLazy
            | Token.KWAssert -> PrecedenceLevel.Function // same as function application
            | Token.OpBar -> PrecedenceLevel.PatternMatchBar // pattern match bar
            | Token.OpDot -> PrecedenceLevel.Dot
            | Token.KWLParen
            | Token.KWRParen
            | Token.KWLBracket
            | Token.KWRBracket
            | Token.KWLAttrBracket
            | Token.KWRAttrBracket
            | Token.KWLArrayBracket
            | Token.KWRArrayBracket
            | Token.KWLBrace
            | Token.KWRBrace -> PrecedenceLevel.Parens
            | t -> raise (new NotImplementedException($"{t}"))
        else
            uint16 token &&& PrecedenceMask |> int |> enum

    let numericBase token : NumericBase =
        uint16 token &&& NumericBaseMask >>> NumericBaseShift |> int |> enum

    let numericKind token : NumericKind =
        uint16 token &&& NumericKindMask |> int |> enum

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
        let inComment =
            if TokenInfo.inComment this.Token then
                " (in comment)"
            else
                ""

        let isVirtual = if TokenInfo.isVirtual this.Token then " (virtual)" else ""
        let tokNoFlags = TokenInfo.withoutFlags this.Token

        match TokenInfo.kind tokNoFlags with
        | TokenKind.Keyword
        | TokenKind.Identifier
        | TokenKind.TextLiteral
        | TokenKind.NumericLiteral
        | TokenKind.Special
        | TokenKind.Invalid
        | TokenKind.Spare -> sprintf "%d, %O%s%s" this.StartIndex tokNoFlags inComment isVirtual
        | TokenKind.Operator ->
            // For operators, print the binary representation to see flags
            let precedence = TokenInfo.operatorPrecedence this.Token

            let maybePrefix =
                if TokenInfo.canBePrefix this.Token then
                    " (can be prefix)"
                else
                    ""

            sprintf
                "%d, Operator 0b%016B %O%s%s%s"
                this.StartIndex
                (uint16 tokNoFlags)
                precedence
                maybePrefix
                inComment
                isVirtual
        | _ -> sprintf "%d, %O%s%s" this.StartIndex tokNoFlags inComment isVirtual

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
        member this.InComment = TokenInfo.inComment this
        member this.IsNumeric = TokenInfo.isNumeric this
        member this.IsText = TokenInfo.isText this

        member this.IsCommentedOut = TokenInfo.inComment this

        member this.WithoutCommentFlags: Token =
            let mask = ~~~InComment
            uint16 this &&& mask |> uint16 |> LanguagePrimitives.EnumOfValue

        member this.Kind: TokenKind = TokenInfo.kind this

    type PositionedToken with
        member this.IsIdentifier = TokenInfo.isIdentifier this.Token
        member this.IsLiteral = TokenInfo.isLiteral this.Token
        member this.IsOperator = TokenInfo.isOperator this.Token
        member this.IsKeyword = TokenInfo.isKeyword this.Token
        member this.IsDeprecated = TokenInfo.isDeprecated this.Token
        member this.IsReserved = TokenInfo.isReserved this.Token
        member this.IsInvalid = TokenInfo.isInvalid this.Token
        member this.InComment = TokenInfo.inComment this.Token
        member this.IsNumeric = TokenInfo.isNumeric this.Token
        member this.IsText = TokenInfo.isText this.Token

        member this.IsCommentedOut = TokenInfo.inComment this.Token

        member this.TokenWithoutCommentFlags: Token = TokenInfo.withoutCommentFlags this.Token

[<Struct>]
type NumericInfo =
    internal
        {
            _token: PositionedToken
        }

    member this.PositionedToken = this._token
    member this.Token: Token = this._token.Token
    member this.StartIndex: int64 = this._token.StartIndex

    member this.Base: NumericBase = TokenInfo.numericBase this._token.Token

    member this.Kind: NumericKind = TokenInfo.numericKind this._token.Token

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

    let associativity (p: PrecedenceLevel) : Associativity =
        match p with
        | PrecedenceLevel.As -> Associativity.Right
        | PrecedenceLevel.When -> Associativity.Right
        | PrecedenceLevel.Pipe -> Associativity.Left
        | PrecedenceLevel.Semicolon -> Associativity.Right
        | PrecedenceLevel.Let -> Associativity.Non
        | PrecedenceLevel.Function -> Associativity.Non
        | PrecedenceLevel.If -> Associativity.Non
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
        | PrecedenceLevel.Parens -> Associativity.Non
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
    member this.CanBePrefix: bool = TokenInfo.canBePrefix this.Token
    member this.Associativity = OperatorInfo.associativity this.Precedence
    member this.Precedence = this._precedence

    static member TryCreate(token: PositionedToken) =
        if TokenInfo.isOperator token.Token || TokenInfo.isOperatorKeyword token.Token then
            ValueSome
                {
                    _token = token
                    _precedence = TokenInfo.operatorPrecedence token.Token
                }
        else
            ValueNone

    static member Create(token: PositionedToken) =
        match OperatorInfo.TryCreate token with
        | ValueSome opInfo -> opInfo
        | ValueNone -> invalidArg "token" (sprintf "Token %A is not an operator." token)
