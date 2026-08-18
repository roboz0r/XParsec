namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.FSharp.Lexer


[<RequireQualifiedAccess>]
[<Struct>]
type TokenIndex =
    | Regular of int<token>
    | Virtual

[<Struct>]
type SyntaxToken =
    {
        PositionedToken: PositionedToken
        Index: TokenIndex
    }

    member this.StartIndex = this.PositionedToken.StartIndex
    member this.Token = this.PositionedToken.Token


/// WHERE something is, in the token space of the file that produced it. Declared HERE, in
/// the parser layer, because a position in a token stream is the parser's own notion: a
/// `DiagnosticCode` identifies one, and every later layer (the semantic passes, the frozen
/// format) speaks the same space rather than a translation of it.
[<RequireQualifiedAccess>]
type Site =
    /// No place in the file: a whole-file lex/parse failure, a conformance verdict
    /// about a signature rather than a position, a pass with no node in hand.
    | Nowhere
    /// One token.
    | At of token: int<token>
    /// A run of tokens, INCLUSIVE of both ends. `Between(t, t)` is `At t`; the
    /// module's smart constructor collapses it.
    | Between of first: int<token> * last: int<token>
    /// The GAP after a token — a zero-width position, for something that is
    /// MISSING. A recovery-inserted virtual `)` is here and nowhere else: it has
    /// no token index of its own, so it cannot be `At` anything.
    | After of token: int<token>

[<RequireQualifiedAccess>]
module Site =

    /// The place a token points to. A VIRTUAL token yields `Nowhere` — it carries no lexed
    /// index, so there is nothing to point at. This is deliberately NOT `Anchor.ofToken`,
    /// which faults instead: an anchor may never be virtual, whereas a recovery-inserted
    /// token is exactly what a diagnostic wants to blame. A producer that means "a `)` is
    /// missing here" says `Site.gapBefore` of the REAL token before the gap, which is
    /// information this conversion does not have.
    let ofToken (tok: SyntaxToken) : Site =
        match tok.Index with
        | TokenIndex.Regular i -> Site.At i
        | TokenIndex.Virtual -> Site.Nowhere

    /// The gap immediately BEFORE `site` — where something MISSING from the token stream
    /// belonged. The type spells a gap only as `After` its predecessor, and trivia is
    /// tokenised, so a placed site's predecessor always exists and its gap always ends
    /// where that site starts. Nothing precedes token 0, so a hole at the very start of the
    /// file stays `At 0`.
    ///
    /// Takes a `Site` and not a token because the caller that needs it — the seam that
    /// re-points an unclosed-delimiter diagnostic at the hole rather than at the innocent
    /// token that exposed it — is reading a diagnostic, which carries a place, not a token.
    let gapBefore (site: Site) : Site =
        match site with
        | Site.At i when i > 0<token> -> Site.After(i - 1<token>)
        | placed -> placed

    /// The place `tok` points to, or `fallback` when `tok` has none — for a caller holding an
    /// ENCLOSING span (the declaration it sits in) that is still a real place when
    /// the node itself is a recovery insertion.
    let ofTokenOr (fallback: Site) (tok: SyntaxToken) : Site =
        match ofToken tok with
        | Site.Nowhere -> fallback
        | positioned -> positioned

    /// A run of tokens in the ONE canonical form the type admits: ends in order, and a
    /// one-token run collapsed to `At` so a renderer meets exactly one spelling of "here".
    /// The bare `Between` constructor can express neither rule, so every producer builds a
    /// run through here.
    let between (first: int<token>) (last: int<token>) : Site =
        let lo = min first last
        let hi = max first last

        if lo = hi then Site.At lo else Site.Between(lo, hi)

    /// The canonical form of any `Site`, including one a caller built with the bare
    /// `Between` constructor or a blob decoded from bytes nothing wrote. The codec
    /// normalises through this on BOTH sides, so a degenerate or inverted range cannot
    /// survive a round trip in one form on the way out and another on the way back.
    let normalise (s: Site) : Site =
        match s with
        | Site.Between(first, last) -> between first last
        | Site.Nowhere
        | Site.At _
        | Site.After _ -> s

    /// The run a SEQUENCE of tokens covers, from the leftmost placed token to the
    /// rightmost. Recovery insertions are skipped rather than faulting the whole span — a
    /// run containing one is still somewhere — and `Nowhere` when every token is virtual.
    /// THE span builder: `between` takes indices, this takes what a caller holds.
    let spanning (toks: SyntaxToken seq) : Site =
        let mutable lo = ValueNone
        let mutable hi = ValueNone

        for tok in toks do
            match tok.Index with
            | TokenIndex.Regular i ->
                lo <-
                    ValueSome(
                        match lo with
                        | ValueSome l -> min l i
                        | ValueNone -> i
                    )

                hi <-
                    ValueSome(
                        match hi with
                        | ValueSome h -> max h i
                        | ValueNone -> i
                    )
            | TokenIndex.Virtual -> ()

        match lo, hi with
        | ValueSome l, ValueSome h -> between l h
        | _ -> Site.Nowhere

    /// The run a written long-ident covers (`A.B.T` — first segment to last).
    let ofLongIdent (li: LongIdent<SyntaxToken>) : Site = spanning li.Idents


/// Represents #if expressions used in conditional compilation
[<RequireQualifiedAccess>]
type IfExpr<'T> =
    | Term of 'T
    | And of IfExpr<'T> * 'T * IfExpr<'T> // &&
    | Or of IfExpr<'T> * 'T * IfExpr<'T> // ||
    | Not of 'T * IfExpr<'T> // !
    | Paren of 'T * IfExpr<'T> * 'T // ( ... )

[<RequireQualifiedAccess>]
type IfOp =
    | And
    | Or
    | Not
    | LParen
    | RParen


// https://fsharp.github.io/fslang-spec/lexical-filtering/#1516-full-list-of-offside-contexts

[<RequireQualifiedAccess>]
type OffsideContext =
    | Let
    | If
    | Try
    | Lazy
    | Fun
    | Function
    /// The `with` keyword as part of a record expression or an object expression whose members use the syntax `{ new Foo with M() = 1 and N() = 2 }`
    | WithLet
    /// The `with` keyword as part of an extension, interface, or object expression whose members use the syntax `{ new Foo member x.M() = 1 member x. N() = 2 }`
    | WithAugment
    | Match
    /// Between the `for` and `do` or `->` in a `for` expression. The pattern and iterable must be indented
    /// past the `for` keyword, but the body can be at the same indent as `for` or indented further.
    /// The body is represented by a `Do` context, which is pushed after the `do` or `->`.
    | For
    | While
    | Then
    | Else
    | Do
    | Type
    | Namespace
    | Module
    /// The `member`, `abstract`, `default`, or `override` keyword, if the Member context is not already active, because multiple tokens may be present.
    /// - or -
    /// `(` is the next token after the `new` keyword. This distinguishes the member declaration `new(x) = ...` from the expression `new x()`
    | Member
    | Paren
    | Bracket
    | Brace
    | BracketBar
    | BraceBar
    | Begin
    | Struct
    | Sig
    | Quote
    /// The `with` keyword in a Try or Match context immediately after a `function` keyword.
    | MatchClauses
    /// An otherwise unprocessed keyword in a SeqBlock context.
    | Vanilla
    /// A sequence of items that must be column-aligned (the primary offside context).
    /// Pushed after `=` in Let/Member, after Then/Else/Try/Finally/Do contexts, after `->` in MatchClauses, etc.
    | SeqBlock

type Offside =
    {
        Context: OffsideContext
        Indent: int
        Token: PositionedToken
    }

[<RequireQualifiedAccess>]
module Offside =
    /// Is this frame a module-declaration BLOCK — a body whose column-aligned
    /// elements are module declarations (pars.fsy `moduleDefns`), not a `seqExpr`?
    /// Two producers exist: a nested `module X =` body pushes `Module`
    /// (`ModuleDefn.parseBody`), and the FILE-level entry frame is the `SeqBlock`
    /// pushed by `ProgramStructureParsing.parse`/`parseSignature`, identified by its
    /// `Token.EOF` anchor — every other `SeqBlock` is anchored on the real token
    /// that opened it, so an EOF anchor is unique to the file entry. Consumed by
    /// `pSepVirt`'s binding-start OBLOCKSEP rule.
    let isDeclBlock (frame: Offside) : bool =
        match frame.Context with
        | OffsideContext.Module -> true
        | OffsideContext.SeqBlock -> frame.Token.Token = Token.EOF
        | _ -> false

/// WHAT the parser could not accept. Every payload here is a `Token`, a `Site` or a string
/// — never a CST node. That is what lets a consumer forward the code whole (the semantic
/// layer wraps it as its own `Kind.Parse`) and freeze it alongside the rest of a
/// diagnostic: a node would drag raw char offsets and virtual tokens across a boundary
/// built to keep them out. The tokens a code is ABOUT are identified by `Site`, in the same
/// token space every later layer speaks.
[<RequireQualifiedAccess>]
type DiagnosticCode =
    // TODO: Use F# error codes
    | Other of string
    /// A unit-of-measure on a constant mentioning a type parameter. Payload-free: the
    /// offending typar is what the diagnostic's own `Site` points at, and nothing ever read
    /// the subtree this used to carry.
    | TyparInConstant
    // Recovery-specific:
    | MissingExpression
    | MissingPattern
    | MissingType
    | MissingRule
    | MissingTypeDefn
    | MissingModuleElem
    | UnexpectedTopLevel
    | ExpectedEnd
    | ExpectedRParen
    | ExpectedRBracket
    | ExpectedRArrayBracket
    | ExpectedRBraceBar
    | ExpectedQuotationTypedRight
    | ExpectedQuotationUntypedRight
    /// A close delimiter that never appeared: the parser SYNTHESISED a virtual one, so the
    /// mistake is a hole in the token stream and the token that exposed it is innocent.
    ///
    /// `opened` is the opening delimiter's TOKEN (what the message spells it as) and
    /// `openedAt` is WHERE it was written (what a secondary label points at) — the two
    /// halves of the `SyntaxToken` this used to carry, and the only two anything read.
    | UnclosedDelimiter of opened: Token * openedAt: Site * expected: Token
    /// A close delimiter that is PRESENT but wrong (`{| … }`). The parser accepts the token
    /// as the close rather than inserting anything, so that token IS the mistake and there
    /// is no hole to point at — which is why this is not `UnclosedDelimiter`.
    | MismatchedDelimiter of opened: Token * openedAt: Site * expected: Token

/// A parse diagnostic. Every one is an error — recovery only ever reports something the
/// grammar could not accept — so there is no severity to carry.
///
/// `Token`/`TokenEnd` are `SyntaxToken`, not `PositionedToken`: a consumer outside the
/// parser needs the token INDEX to point at the place, and a `PositionedToken` carries only a
/// char offset, which it could only turn back into a token by searching. A diagnostic
/// raised where the input offers no token to blame carries `SyntaxToken.nowhere`.
and Diagnostic =
    {
        Code: DiagnosticCode
        Token: SyntaxToken
        TokenEnd: SyntaxToken option
        Error: ParseError<PositionedToken, ParseState> option
    }

and [<RequireQualifiedAccess>] Syntax =
    | Light
    | Verbose

/// Receives structured parse-trace events. Attached to `ParseState.Trace` as a
/// nullable reference (`null` = no tracing). `ParseState.ifTrace` is the only
/// blessed access path: it performs a single null check and skips the callback
/// entirely when unset, so every hot-path trace site pays at most a test + branch
/// and zero virtual-call overhead in the common case. A reference type so it
/// doesn't affect ParseState equality and is shared across immutable record copies.
and [<AllowNullLiteral>] TraceCallback() =
    abstract ContextPush: context: OffsideContext * indent: int * token: PositionedToken * stackDepth: int -> unit

    default _.ContextPush(_, _, _, _) = ()

    abstract ContextPop: context: OffsideContext * stackDepth: int -> unit
    default _.ContextPop(_, _) = ()

    abstract TokenConsumed: token: PositionedToken * index: int * col: int -> unit
    default _.TokenConsumed(_, _, _) = ()

    abstract TokenPeeked: token: PositionedToken * index: int * col: int -> unit
    default _.TokenPeeked(_, _, _) = ()

    abstract VirtualToken: token: Token * atStartIndex: int -> unit
    default _.VirtualToken(_, _) = ()

    abstract OffsideOk: token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext -> unit

    default _.OffsideOk(_, _, _, _) = ()

    abstract OffsideFail: token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext -> unit

    default _.OffsideFail(_, _, _, _) = ()

    abstract PermittedUndentation: token: PositionedToken * tokenCol: int * contextIndent: int * rule: string -> unit

    default _.PermittedUndentation(_, _, _, _) = ()

    abstract DiagnosticEmitted: code: DiagnosticCode * token: SyntaxToken -> unit

    default _.DiagnosticEmitted(_, _) = ()

    abstract SplitRAttrBracketSet: atStartIndex: int -> unit
    default _.SplitRAttrBracketSet(_) = ()

    abstract SplitRAttrBracketConsumed: atStartIndex: int -> unit
    default _.SplitRAttrBracketConsumed(_) = ()

    abstract SplitPowerMinusSet: atStartIndex: int -> unit
    default _.SplitPowerMinusSet(_) = ()

    abstract SplitPowerMinusConsumed: atStartIndex: int -> unit
    default _.SplitPowerMinusConsumed(_) = ()

    abstract Message: message: string -> unit
    default _.Message(_) = ()

and [<Struct>] WarnDirective =
    {
        Line: int<line>
        WarningNumber: int
        Suppress: bool
    }

and [<ReferenceEquality; NoComparison>] ParseState =
    {
        Lexed: Lexed
        Context: Offside list
        Diagnostics: Diagnostic list
        ActiveDefines: ActiveDefines
        IndentationMode: Syntax
        mutable LastLine: int<line> // ok to be mutable since it's only used as a guess
        // ReprocessOpAfterTypeDeclaration: bool
        /// Number of characters consumed since the last type parameter, used to
        /// allow procesing of `>.` or `>>=` as single characters to close type parameters without prematurely treating `>` as an operator.
        CharsConsumedAfterTypeParams: int
        ConditionalCompilationStack: PositionedToken list
        /// When true, the next `KWRAttrBracket` token encountered by `nextNonTriviaTokenImpl`
        /// is rewritten to `KWRBracket`. Set by the measure parser when it splits `>]` into
        /// a virtual `>` (for the measure close) and a real `]` (for the enclosing indexer).
        SplitRAttrBracket: bool
        /// When true, the next custom operator token starting with `^-` (which the lexer fuses
        /// as a single operator at Append precedence) is rewritten to `OpSubtraction` at
        /// `StartIndex + 1`. Set by the measure parser when it splits `^-N` into a virtual `^`
        /// (for the power operator) and a real `-` followed by the numeric exponent.
        SplitPowerMinus: bool
        /// Accumulated #nowarn / #warnon directives (most recent first).
        WarnDirectives: WarnDirective list
        /// Callback for structured parse tracing. `null` (the default) means no
        /// tracing — each call site is a single null check via `ifTrace`. Assigned
        /// via `createWithTracing`. Shared across immutable record copies.
        Trace: TraceCallback
    }

/// The concrete Readable slice type the F# parser reads from.
/// Future input-representation swaps should only change this alias.
type FSReadable = ReadableArray<PositionedToken>

/// An F# parser: produces 'T from the token stream.
type FSParser<'T> = Parser<'T, PositionedToken, ParseState, FSReadable>

/// An F# reader over the token stream.
type FSReader = Reader<PositionedToken, ParseState, FSReadable>


module DiagnosticCode =

    /// The source spelling of `t` as an error message refers to it. There is no general
    /// token→text table, so this covers the delimiters and keywords a diagnostic can
    /// demand and falls back to the token's own name, which is what the pre-existing
    /// `Other $"Expected '{t}'"` sites already print.
    let private spelling (t: Token) =
        match TokenInfo.withoutFlags t with
        | Token.KWLParen -> "("
        | Token.KWRParen -> ")"
        | Token.KWLBracket -> "["
        | Token.KWRBracket -> "]"
        | Token.KWLArrayBracket -> "[|"
        | Token.KWRArrayBracket -> "|]"
        | Token.KWLBrace -> "{"
        | Token.KWRBrace -> "}"
        | Token.KWLBraceBar -> "{|"
        | Token.KWRBraceBar -> "|}"
        | Token.KWLAttrBracket -> "[<"
        | Token.KWRAttrBracket -> ">]"
        | Token.OpQuotationTypedLeft -> "<@"
        | Token.OpQuotationTypedRight -> "@>"
        | Token.OpQuotationUntypedLeft -> "<@@"
        | Token.OpQuotationUntypedRight -> "@@>"
        | Token.KWEnd -> "end"
        | other -> string other

    /// "Expected 'X'", spelling `t` the way a reader wrote it. THE one phrasing of that
    /// sentence, so a diagnostic built ad hoc from a token cannot print the enum name
    /// (`Expected 'KWRParen'`) where the seam prints the glyph.
    let expecting (t: Token) = $"Expected '{spelling t}'"

    // How a parse diagnostic presents on the FAR side of the parser boundary. THE SEAM —
    // the parser owns its own error vocabulary, and these are the one place that vocabulary
    // becomes the two things a consumer still spells a diagnostic with. TWO functions and
    // not one pair-returning function: every caller wants one half, and for the delimiter
    // cases the discarded half is an interpolated string that would be built anyway.

    /// The stable code a consumer filters on.
    let code (c: DiagnosticCode) : string =
        match c with
        | DiagnosticCode.Other _ -> "Other"
        | DiagnosticCode.TyparInConstant -> "TyparInConstant"
        | DiagnosticCode.MissingExpression -> "MissingExpression"
        | DiagnosticCode.MissingPattern -> "MissingPattern"
        | DiagnosticCode.MissingType -> "MissingType"
        | DiagnosticCode.MissingRule -> "MissingRule"
        | DiagnosticCode.MissingTypeDefn -> "MissingTypeDefn"
        | DiagnosticCode.MissingModuleElem -> "MissingModuleElem"
        | DiagnosticCode.UnexpectedTopLevel -> "UnexpectedTopLevel"
        | DiagnosticCode.ExpectedEnd -> "ExpectedEnd"
        | DiagnosticCode.ExpectedRParen -> "ExpectedRParen"
        | DiagnosticCode.ExpectedRBracket -> "ExpectedRBracket"
        | DiagnosticCode.ExpectedRArrayBracket -> "ExpectedRArrayBracket"
        | DiagnosticCode.ExpectedRBraceBar -> "ExpectedRBraceBar"
        | DiagnosticCode.ExpectedQuotationTypedRight -> "ExpectedQuotationTypedRight"
        | DiagnosticCode.ExpectedQuotationUntypedRight -> "ExpectedQuotationUntypedRight"
        | DiagnosticCode.UnclosedDelimiter _ -> "UnclosedDelimiter"
        | DiagnosticCode.MismatchedDelimiter _ -> "MismatchedDelimiter"

    /// The English it renders.
    let message (c: DiagnosticCode) : string =
        match c with
        | DiagnosticCode.Other msg -> msg
        | DiagnosticCode.TyparInConstant -> "A unit-of-measure on a constant cannot mention a type parameter"
        | DiagnosticCode.MissingExpression -> "Expected an expression"
        | DiagnosticCode.MissingPattern -> "Expected a pattern"
        | DiagnosticCode.MissingType -> "Expected a type"
        | DiagnosticCode.MissingRule -> "Expected a match rule"
        | DiagnosticCode.MissingTypeDefn -> "Expected a type definition"
        | DiagnosticCode.MissingModuleElem -> "Expected a module declaration"
        | DiagnosticCode.UnexpectedTopLevel -> "Unexpected token(s) at the top level"
        | DiagnosticCode.ExpectedEnd -> expecting Token.KWEnd
        | DiagnosticCode.ExpectedRParen -> expecting Token.KWRParen
        | DiagnosticCode.ExpectedRBracket -> expecting Token.KWRBracket
        | DiagnosticCode.ExpectedRArrayBracket -> expecting Token.KWRArrayBracket
        | DiagnosticCode.ExpectedRBraceBar -> expecting Token.KWRBraceBar
        | DiagnosticCode.ExpectedQuotationTypedRight -> expecting Token.OpQuotationTypedRight
        | DiagnosticCode.ExpectedQuotationUntypedRight -> expecting Token.OpQuotationUntypedRight
        | DiagnosticCode.UnclosedDelimiter(opened = opened; expected = expected) ->
            $"Unclosed '{spelling opened}': {expecting expected}"
        | DiagnosticCode.MismatchedDelimiter(opened = opened; expected = expected) ->
            $"Wrong close for '{spelling opened}': {expecting expected}"

    /// What the secondary label on the OPENING delimiter says. Both delimiter diagnostics
    /// point back at the same thing, so the wording is decided once rather than per code.
    let openedHereLabel = "unclosed delimiter"

module SyntaxToken =

    /// The name `tok` spells, read out of the file that produced it, a backtick-escaped
    /// identifier reading as the name inside the quotes. A VIRTUAL token spells the empty
    /// string, the one string an identifier can never be.
    let nameIn (lexed: Lexed) (tok: SyntaxToken) : string =
        match tok.Index with
        | TokenIndex.Regular i -> lexed.GetTokenName i
        | TokenIndex.Virtual -> ""

    let syntaxToken token (index: int) =
        {
            PositionedToken = token
            Index = TokenIndex.Regular(index * 1<token>)
        }

    let virtualToken token =
        {
            PositionedToken = token
            Index = TokenIndex.Virtual
        }

    /// The token a diagnostic blames when the input offers none: the reader is past the
    /// end, or the next token is offside and so is not part of the construct being
    /// diagnosed. Virtual, so it carries NO index and points nowhere — the alternative is
    /// to invent an offset and point the diagnostic at whatever happens to sit there.
    let nowhere = virtualToken (PositionedToken.Create(Token.EOF, 0))

module ParseState =
    /// Invokes `action` with the attached trace callback when one is present.
    /// The `null` fast path compiles to a single test + branch on the reference
    /// field, and `[<InlineIfLambda>]` inlines the action body so no closure
    /// allocates per call.
    let inline ifTrace (state: ParseState) ([<InlineIfLambda>] action: TraceCallback -> unit) =
        if not (isNull state.Trace) then
            action state.Trace

    let createWithTracing (input: ParseInput) (trace: TraceCallback) =
        {
            Lexed = input.Lexed
            Context = []
            Diagnostics = []
            ActiveDefines = input.ActiveDefines
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            // ReprocessOpAfterTypeDeclaration = false
            CharsConsumedAfterTypeParams = 0
            ConditionalCompilationStack = []
            SplitRAttrBracket = false
            SplitPowerMinus = false
            WarnDirectives = []
            Trace = trace
        }

    let create (input: ParseInput) =
        {
            Lexed = input.Lexed
            Context = []
            Diagnostics = []
            ActiveDefines = input.ActiveDefines
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            CharsConsumedAfterTypeParams = 0
            ConditionalCompilationStack = []
            SplitRAttrBracket = false
            SplitPowerMinus = false
            WarnDirectives = []
            Trace = null
        }

    let setIndentOn (state: ParseState) =
        { state with
            IndentationMode = Syntax.Light
        }

    let setIndentOff (state: ParseState) =
        { state with
            IndentationMode = Syntax.Verbose
        }

    let pushOffside offsideCtx (state: ParseState) =
        let newState =
            { state with
                Context = offsideCtx :: state.Context
            }

        let stackDepth = newState.Context.Length

        ifTrace newState (fun t -> t.ContextPush(offsideCtx.Context, offsideCtx.Indent, offsideCtx.Token, stackDepth))

        newState

    let popOffside current (state: ParseState) =
        match state.Context with
        | [] -> invalidOp "Attempted to pop empty context"
        | top :: tail ->
            if top <> current then
                invalidOp $"Attempted to pop context {current} but top of stack was {top}"

            ifTrace state (fun t -> t.ContextPop(top.Context, state.Context.Length))
            { state with Context = tail }

    let addDiagnostic code startToken endToken error (state: ParseState) =
        ifTrace state (fun t -> t.DiagnosticEmitted(code, startToken))

        let diag =
            {
                Code = code
                Token = startToken
                TokenEnd = endToken
                Error = error
            }

        { state with
            Diagnostics = diag :: state.Diagnostics
        }

    /// Shortcut for the common case of a diagnostic at a single token with no end-token
    /// range and no underlying parser error.
    let addDiagnosticAt code startToken state =
        addDiagnostic code startToken None None state

    /// Shortcut for the common case of a diagnostic at a single token that wraps an
    /// underlying parser error.
    let addDiagnosticWithError code startToken err state =
        addDiagnostic code startToken None (Some err) state

    let private findLineNumberImpl (lexed: Lexed) (guess: int<line>) (index: int<token>) =
        if index < 0<token> || index >= lexed.Tokens.LengthM then
            invalidArg (nameof index) "Index out of range"

        let lineStarts = lexed.LineStarts
        let lineCount = lineStarts.LengthM

        // Precondition (maintained through recursion): lineStarts[low] <= index.
        // This holds initially because every initial range produced below either
        // already satisfies it (guess or guess+1 paths) or starts at low = 0, and
        // lineStarts[0] = 0 <= index for any non-negative token index.
        // Returns the largest i in [low, high] with lineStarts[i] <= index.
        let rec search (low: int<line>) (high: int<line>) =
            if low >= high then
                low
            else
                let mid = low + (high - low + 1<line>) / 2 // upper-biased to avoid infinite loop when low + 1 = high

                if lineStarts.[mid] <= index then
                    search mid high
                else
                    search low (mid - 1<line>)

        // Use the guess to narrow the initial search range
        let low, high =
            if guess >= 0<line> && guess < lineCount then
                if
                    lineStarts.[guess] <= index
                    && (guess = lineCount - 1<line> || lineStarts.[guess + 1<line>] > index)
                then
                    // The guess is correct, we can determine the result directly
                    (guess, guess) // Set low and high to the same value to end the search
                // The target is after the guess
                else if lineStarts.[guess] < index then
                    (guess, lineCount - 1<line>)
                else
                    // The target is before the guess
                    (0<line>, guess)
            else
                // Default to a full search if the guess is out of range
                (0<line>, lineCount - 1<line>)

        // If the guess was correct, low and high will be the same,
        // and the search will correctly return that value.
        search low high

    let findLineNumber (state: ParseState) (index: int<token>) =
        let lineNo = findLineNumberImpl state.Lexed state.LastLine index
        state.LastLine <- lineNo
        lineNo

    let rec private getIndentViaLineStarts (state: ParseState) (index: int<token>) =
        let currentLineTokenIndex = state.Lexed.LineStarts[state.LastLine]

        if index = currentLineTokenIndex then
            0
        elif index > currentLineTokenIndex then
            let nextLine = state.LastLine + 1<_>

            if nextLine < state.Lexed.LineStarts.LengthM then
                let nextLineTokenIndex = state.Lexed.LineStarts[nextLine]

                if index < nextLineTokenIndex then
                    let token = state.Lexed.Tokens[index]
                    let lineStartToken = state.Lexed.Tokens[currentLineTokenIndex]
                    token.StartIndex - lineStartToken.StartIndex
                else
                    state.LastLine <- findLineNumberImpl state.Lexed nextLine index
                    getIndentViaLineStarts state index
            else
                // Last line
                let token = state.Lexed.Tokens[index]
                token.StartIndex - state.Lexed.Tokens[currentLineTokenIndex].StartIndex
        else
            state.LastLine <- findLineNumberImpl state.Lexed (state.LastLine - 1<_>) index
            getIndentViaLineStarts state index

    /// The token's column.
    let getIndent (state: ParseState) (index: int<token>) =
        let indent = state.Lexed.Indents[index]

        match indent with
        | Indents.UseLineStarts -> getIndentViaLineStarts state index
        | _ -> int indent

    let isTriviaToken (state: ParseState) (token: PositionedToken) =
        if token.InComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Indent
            | Token.Whitespace
            | Token.BlockCommentStart
            | Token.BlockCommentEnd
            | Token.StartFSharpBlockComment
            | Token.EndFSharpBlockComment
            | Token.StartOCamlBlockComment
            | Token.EndOCamlBlockComment
            | Token.Newline -> true
            | Token.Tab -> state.IndentationMode = Syntax.Verbose
            | _ -> false

    let tokenString (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> ""
        | TokenIndex.Regular iT -> state.Lexed.GetTokenString(iT)

    let tokenStringIs (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT)
            span.SequenceEqual(s.AsSpan())

    let tokenStringStartsWith (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT)
            span.StartsWith(s.AsSpan())

    let isDefined (state: ParseState) (symbolToken: SyntaxToken) =
        let symbol = tokenString symbolToken state
        state.ActiveDefines.Contains(symbol)

    /// Check if a warning number is suppressed at a given line.
    let isWarningSuppressed (warningNumber: int) (line: int<line>) (state: ParseState) =
        state.WarnDirectives
        |> List.tryFind (fun d -> d.WarningNumber = warningNumber && d.Line <= line)
        |> Option.map (fun d -> d.Suppress)
        |> Option.defaultValue false

/// TraceCallback that formats each event as a line of text and routes it through
/// the virtual `Write` hook (default: the supplied TextWriter). Subclasses can override
/// individual event methods to capture structured data, or override `Write` to tee the
/// formatted lines elsewhere (e.g. an in-memory ring buffer for test diagnostics).
type WriterTraceCallback(lexed: Lexed, writer: System.IO.TextWriter) =
    inherit TraceCallback()

    /// Emit a formatted trace line. Default writes to the supplied TextWriter.
    abstract Write: line: string -> unit
    default _.Write(line) = writer.WriteLine(line)

    override this.ContextPush(ctx, indent, token, depth) =
        this.Write($"PUSH {ctx} indent={indent} token={token.Token} @{token.StartIndex} depth={depth}")

    override this.ContextPop(ctx, depth) = this.Write($"POP {ctx} depth={depth}")

    override this.TokenConsumed(token, index, col) =
        let line = lexed.GetLineForToken(index * 1<token>)
        this.Write($"CONSUME {token.Token} @{token.StartIndex} index={index} col={col} line={line}")

    override this.TokenPeeked(token, index, col) =
        let line = lexed.GetLineForToken(index * 1<token>)
        this.Write($"PEEK {token.Token} @{token.StartIndex} index={index} col={col} line={line}")

    override this.VirtualToken(token, startIndex) =
        this.Write($"VIRTUAL {token} @{startIndex}")

    override this.OffsideOk(token, tokenCol, contextIndent, ctx) =
        this.Write($"OFFSIDE_OK {token.Token} col={tokenCol} >= indent={contextIndent} ctx={ctx}")

    override this.OffsideFail(token, tokenCol, contextIndent, ctx) =
        this.Write($"OFFSIDE_FAIL {token.Token} col={tokenCol} < indent={contextIndent} ctx={ctx}")

    override this.PermittedUndentation(token, tokenCol, contextIndent, rule) =
        this.Write($"UNDENT_OK {token.Token} col={tokenCol} < indent={contextIndent} rule={rule}")

    override this.DiagnosticEmitted(code, token) =
        this.Write($"DIAGNOSTIC {code} @{token.StartIndex}")

    override this.SplitRAttrBracketSet(startIndex) =
        this.Write($"SPLIT_RATTR_SET @{startIndex}")

    override this.SplitRAttrBracketConsumed(startIndex) =
        this.Write($"SPLIT_RATTR_CONSUMED @{startIndex}")

    override this.SplitPowerMinusSet(startIndex) =
        this.Write($"SPLIT_POW_MINUS_SET @{startIndex}")

    override this.SplitPowerMinusConsumed(startIndex) =
        this.Write($"SPLIT_POW_MINUS_CONSUMED @{startIndex}")

    override this.Message(msg) = this.Write($"MSG: {msg}")

[<RequireQualifiedAccess>]
module Reader =
    let ofParseInput (input: ParseInput) : Reader<_, ParseState, _> =
        let initialState = ParseState.create input
        Reader((input.Lexed.Tokens.AsReadableArray()), initialState, 0)

    let ofParseInputWithTracing (input: ParseInput) (trace: TraceCallback) : Reader<_, ParseState, _> =
        let initialState = ParseState.createWithTracing input trace
        Reader((input.Lexed.Tokens.AsReadableArray()), initialState, 0)
