namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Wire format: bit 63 synthetic, bits 62..48 reserved (always zero), bits 47..32 kind, bits
// 31..0 offset. The offset slot is SIGNED and the sign is load-bearing: negative is the
// uniqueness-counter space, a domain no genuine source offset inhabits.

/// A real NodeKey is unique on (offset, kind), not on offset alone: two CST node types can
/// start at the same source offset (a LetBinding and its Pattern both start at the `let`).
type NodeKind =
    | Unknown = 0us

    | ExprConst = 1us
    | ExprIdent = 2us
    | ExprLongIdent = 3us
    | ExprApp = 4us
    | ExprLambda = 5us
    | ExprLet = 6us
    | ExprMatch = 7us
    | ExprIfThenElse = 8us
    | ExprPipeline = 9us
    | ExprTuple = 10us
    | ExprRecord = 11us
    | ExprSequential = 12us
    | ExprComputation = 13us
    | ExprInfixApp = 14us
    | ExprPrefixApp = 15us
    | ExprEnclosedBlock = 16us
    | ExprTypeAnnotation = 17us
    | ExprEmptyBlock = 18us
    | ExprWhile = 19us
    | ExprForTo = 20us
    | ExprForIn = 21us
    | ExprString = 22us
    | ExprFunction = 24us
    | ExprTryWith = 25us
    | ExprTryFinally = 26us
    | ExprAssignment = 27us
    | ExprHighPrecApp = 28us
    | ExprRange = 29us
    | ExprSteppedRange = 30us
    | ExprNull = 31us
    | ExprDotLookup = 32us
    | ExprRecordClone = 33us
    | ExprNew = 34us
    | ExprILIntrinsic = 35us
    | ExprStaticOptimization = 36us
    | ExprStaticUpcast = 37us
    | ExprDynamicTypeTest = 38us
    | ExprDynamicDowncast = 39us
    /// Indexed array lookup (`arr.[i]`), keyed off the `[` token, not the object argument's.
    | ExprIndexedLookup = 40us
    | ExprStaticMemberInvocation = 41us

    | PatConst = 100us
    | PatIdent = 101us
    | PatLongIdent = 102us
    | PatTuple = 103us
    | PatRecord = 104us
    | PatWildcard = 105us
    | PatAs = 106us
    | PatEnclosedBlock = 107us
    | PatTyped = 108us
    | PatOr = 109us
    /// `for i = …` loop variable; the `i` token has no surrounding `Pat` in the CST.
    | PatForToVar = 110us
    | PatEmptyBlock = 111us
    /// Operator-named binding (`let (=) x y = …`), keyed off the `IdentOrOp`'s `(`, not the
    /// operator token.
    | PatOp = 112us
    /// Cons pattern (`h :: t`), keyed off the `::` token, not its head sub-pattern's.
    | PatCons = 113us
    /// Attribute-decorated parameter (`([<CallAtMostOnce>] x)`), keyed distinctly from the
    /// wrapped pattern (which shares the same first token).
    | PatAttributed = 114us
    /// Type-test pattern (`:? T as x`), keyed off the `:?` token, not the inner bound variable's.
    | PatTypeTestAs = 115us
    /// `null` literal pattern, keyed off the unique `null` keyword token.
    | PatNull = 116us
    /// Bare type-test pattern (`:? T`, no `as`-bound variable), keyed off the `:?` token; binds nothing.
    | PatTypeTest = 117us

    | TypeNamed = 200us
    | TypeVarRef = 201us
    | TypeFunction = 202us
    | TypeTuple = 203us
    | TypeGeneric = 204us

    | DeclLetBinding = 300us
    | DeclModule = 301us
    | DeclNamespace = 302us
    | DeclType = 303us
    | DeclOpen = 304us

    // Synthetic-only: real CST nodes never carry these kinds.
    | SynthCEMethodCall = 1000us
    | SynthLambdaBody = 1001us
    | SynthDesugaredApp = 1002us
    /// `this` (or `as self`) bound variable inside class member bodies. One per class, shared across members.
    | SynthThisBinding = 1003us
    // 1004 is retired; do not reuse it for another kind.
    /// Anchor for a "not yet supported" diagnostic on an unkeyed CST shape (`ModuleElem.Missing`,
    /// `ModuleElem.SkipsTokens`). Offset is the spawning token's, or `0`.
    | SynthUnsupportedDecl = 1005us
    /// `base` bound variable inside a derived class's member bodies. One per class with
    /// `inherit Base(...)`, shared across members.
    | SynthBaseBinding = 1006us
    /// Freshened bound variable of an inline template, so independent call sites don't alias each
    /// other's bound names. Counter-minted; it has no source position.
    | SynthPreFreezeInline = 1007us
    /// BoundVar of a template UNPOOLED onto the cross-file wire, whose slot means nothing in the
    /// consuming file and so is re-minted. Counter-minted, on its own counter.
    | SynthUnpooledBoundVar = 1008us
    /// The object argument and per-element bound variables a tupled member call's destructured
    /// argument needs. Counter-minted on its own counter: one construct mints several at one offset.
    | SynthElaborateBoundVar = 1009us

[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source offset for real nodes / the spawning offset for a synthetic one. NEGATIVE for a
    /// counter-minted key, which has no source position.
    member this.Offset: int = int (uint32 this.Raw)

    /// False only in the uniqueness-counter space.
    member this.IsSourcePosition: bool = this.Offset >= 0

    /// The offset slot with the counter flag masked off, for NAMING a bound variable (`_s7`,
    /// `value@7`), but never for scoping: counter `7` and spawning offset `7` render alike.
    member this.NameIndex: int = int (uint32 this.Raw &&& 0x7FFFFFFFu)

    member this.Kind: NodeKind =
        let kindBits = (this.Raw &&& 0x0000FFFF00000000UL) >>> 32
        LanguagePrimitives.EnumOfValue(uint16 kindBits)

    member this.IsSynthetic: bool = (this.Raw &&& 0x8000000000000000UL) <> 0UL

    override this.ToString() =
        let tag = if this.IsSynthetic then "syn" else "src"
        sprintf "%s@%d:%A" tag this.Offset this.Kind

/// WHERE in the file a name is being looked up FROM. F# declaration scoping is file-ordered, so
/// a by-NAME registry read is answerable only against a position: a declaration is visible at a
/// use iff its `VisibleFrom` offset is at or before the use.
[<Struct>]
type SourcePos =
    private
        {
            /// `System.Int32.MaxValue` for `SourcePos.unbounded`; a real source offset otherwise.
            /// The sentinel sits above every 32-bit offset, so "sees everything" needs no case.
            Pos: int
        }

    /// The offset a visibility test compares a claim's `VisibleFrom` against.
    member this.Offset: int = this.Pos

module SourcePos =

    /// The whole-file view, for a query with no source position to scope by.
    let unbounded: SourcePos = { Pos = System.Int32.MaxValue }

    /// Fails on a counter-minted key: it has no source position, so a pass needing a scoped read from
    /// such a node must carry the position of the SOURCE construct that spawned it.
    let ofNodeKey (key: NodeKey) : SourcePos =
        if key.IsSourcePosition then
            { Pos = key.Offset }
        else
            failwithf "Internal error: NodeKey %O is counter-minted and has no source position" key

module NodeKey =

    let private synBit = 0x8000000000000000UL

    let ofSource (offset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 offset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k)

    /// A synthetic node that HAS a place in the file, so it stays scopable: a desugared
    /// application resolves names where its source was written.
    let ofSynthetic (spawningOffset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 spawningOffset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    /// Uniqueness from a monotone COUNTER rather than a place in the file. Packed into the
    /// NEGATIVE half of the offset slot, so scoping by it is refused on the value alone.
    let ofSyntheticCounter (counter: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (0x80000000u ||| uint32 counter)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey = ofSource firstToken.StartIndex kind

/// A key's number IS its token's character offset, but a key cannot be inverted back to a
/// token, so the token travels beside it.
[<Struct>]
type NodeSite = { Key: NodeKey; Tok: SyntaxToken }

[<RequireQualifiedAccess>]
module NodeSite =

    let ofToken (kind: NodeKind) (tok: SyntaxToken) : NodeSite =
        {
            Key = NodeKey.ofToken tok kind
            Tok = tok
        }

/// Identity of a source LAMBDA expression: the INDEX of its anchor token, not the character
/// offset a `NodeKey` carries, because one integer identifies a different node in each space.
[<Struct>]
type LambdaKey = | LambdaKey of anchor: Anchor
