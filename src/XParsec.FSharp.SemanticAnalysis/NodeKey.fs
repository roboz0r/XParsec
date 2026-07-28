namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Wire format:
//   bit 63     bit 62..48     bit 47..32   bit 31..0
//   +--------+---------------+------------+-----------+
//   | syn:1  | reserved:15   | kind:16    | offset:32 |
//   +--------+---------------+------------+-----------+
//
// The 15 reserved bits (a future per-spawning-construct counter) are always zero today; `Kind`
// ignores them.
//
// THE OFFSET SLOT IS SIGNED, and the sign is load-bearing:
//   * `Offset >= 0` — a genuine source index. Every real node has one, as does a synthetic node
//     minted from a SPAWNING construct (`ofSynthetic`), so it stays scopable (`SourcePos`).
//   * `Offset < 0`  — the UNIQUENESS-COUNTER space (`ofSyntheticCounter`). No source offset is
//     negative, so this domain holds no real position — a checkable fact about the VALUE, not a
//     convention about which kinds are counter-minted. `SynthLambdaBody` is minted BOTH ways
//     (spawning offset in `ElaborateExpr`, counter in `TastLower`), so kind cannot discriminate.

/// (offset, kind) — not offset alone — makes a real NodeKey unique: two CST node types can
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
    /// Indexed array lookup (`arr.[i]`), keyed off the `[` token so it never collides with its
    /// receiver sub-expression — the operator-token choice `ExprInfixApp` also makes.
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
    /// `for i = …` loop variable. The `i` token has no surrounding `Pat` in the CST, but
    /// NameResolution still needs a stable NodeKey for it.
    | PatForToVar = 110us
    | PatEmptyBlock = 111us
    /// Operator-named binding head (`let (=) x y = …`) — the `IdentOrOp` carries the operator token.
    | PatOp = 112us
    /// Cons pattern (`h :: t`), keyed off the `::` token so it never collides with its head sub-pattern.
    | PatCons = 113us
    /// Attribute-decorated parameter (`([<CallAtMostOnce>] x)`), keyed distinctly from the
    /// wrapped pattern (which shares the same first token).
    | PatAttributed = 114us
    /// Type-test pattern (`:? T as x`), keyed off the `:?` token so it never collides with the
    /// inner binder.
    | PatTypeTestAs = 115us
    /// `null` literal pattern, keyed off the unique `null` keyword token.
    | PatNull = 116us
    /// Bare type-test pattern (`:? T`, no `as`-binder), keyed off the `:?` token; binds nothing.
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
    /// `this` (or `as self`) binder inside class member bodies. One per class, shared across members.
    | SynthThisBinding = 1003us
    // 1004 was `SynthInlineExpansion` (removed); not reused — a retired value must not come to
    // mean another.
    /// Anchor for a "not yet supported" diagnostic on an unkeyed CST shape (`ModuleElem.Missing`,
    /// `ModuleElem.SkipsTokens`). Offset is the spawning token's, or `0`; the distinct kind keeps
    /// it from colliding with real source keys at the same offset.
    | SynthUnsupportedDecl = 1005us
    /// `base` binder inside a derived class's member bodies. One per class with `inherit Base(...)`,
    /// shared across members; mirrors `SynthThisBinding`.
    | SynthBaseBinding = 1006us
    /// Freshened binder of an inline template, minted by `Inline.freshen` so independent call
    /// sites don't alias each other's bound names. Counter-minted (`ofSyntheticCounter`) — it
    /// names no source position.
    | SynthPreFreezeInline = 1007us
    /// Binder of a template DRAINED from the pools onto the cross-unit wire
    /// (`TastPoolBuilder.declTree`), whose slot means nothing in the consuming unit and so is
    /// re-minted. Counter-minted like `SynthPreFreezeInline`, and a KIND of its own precisely
    /// because it is: the two counters are independent, so sharing a kind would let a drain's
    /// nth binder and a freshen's nth binder be one key. They meet — a drained body is
    /// freshened at the splice — so that must be unrepresentable rather than merely unlikely.
    | SynthDrainedBinder = 1008us

[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source offset for real nodes / the spawning offset for a synthetic one. NEGATIVE for a
    /// counter-minted key, which names no source position (see the wire-format note above).
    member this.Offset: int = int (uint32 this.Raw)

    /// Does this key name a place in the source string? True for every real node and for a
    /// spawning-minted synthetic; false only in the uniqueness-counter space. What
    /// `SourcePos.ofNodeKey` admits a key by — a fact about the value, not about the kind.
    member this.IsSourcePosition: bool = this.Offset >= 0

    /// The offset slot with the counter flag masked off. For NAMING a binder a backend must
    /// invent an identifier for (`_s7`, `value@7`) — never for scoping (ask `IsSourcePosition` /
    /// go through `SourcePos`). Not injective across the two spaces (counter `7` and spawning
    /// offset `7` render alike).
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
/// use iff its `VisibleFrom` offset is at or before the use. Carrying that position on the QUERY
/// (rather than as ambient walk state) means a pass cannot silently forget to scope a read —
/// there is only an argument to pass — and the answer depends on where a node IS, not when it
/// is visited.
///
/// The representation is private, so `SourcePos.ofNodeKey` and `SourcePos.unbounded` are the
/// only ways to obtain one, and `ofNodeKey` admits exactly the keys whose offset IS a source
/// position (`NodeKey.IsSourcePosition`). A counter-minted key's offset is a uniqueness token,
/// not a place in the file, so the type makes scoping by it unrepresentable. The test is on the
/// VALUE, deliberately: a synthetic node spawned from a real construct HAS a position and must
/// resolve names at it, while `SynthLambdaBody` is minted both ways — so any kind-based
/// allowlist would be unsound.
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

    /// A read that sees EVERY declaration, wherever it sits — the whole-unit view, for a query
    /// with no source position to scope by (an observer of the finished registry, a consumer
    /// that already holds a resolved key).
    let unbounded: SourcePos = { Pos = System.Int32.MaxValue }

    /// The place in the file a node sits at. Fails on a key from the uniqueness-counter space (a
    /// negative offset): it names no place, so there is nothing to scope by. A pass needing a
    /// scoped read from such a node must carry the position of the SOURCE construct that spawned it.
    let ofNodeKey (key: NodeKey) : SourcePos =
        if key.IsSourcePosition then
            { Pos = key.Offset }
        else
            failwithf "Internal error: NodeKey %O is counter-minted and names no source position" key

module NodeKey =

    let private synBit = 0x8000000000000000UL

    let ofSource (offset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 offset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k)

    /// A synthetic node that HAS a place in the file: `spawningOffset` is the source offset of
    /// the construct that produced it. Non-negative by construction, so the node stays scopable
    /// (a desugared application resolves names where its source was written); uniqueness at that
    /// offset comes from `kind`. A synthetic node with no such construct uses `ofSyntheticCounter`.
    let ofSynthetic (spawningOffset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 spawningOffset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    /// A synthetic node whose uniqueness comes from a monotone COUNTER rather than a place in
    /// the file (a freshened inline binder, a placeholder lambda-param slot). Packed into the
    /// NEGATIVE half of the offset slot — a domain no source offset inhabits — so
    /// `SourcePos.ofNodeKey` refuses the key on its value alone. Counters stay far below 2^31,
    /// so the low 31 bits carry them intact.
    let ofSyntheticCounter (counter: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (0x80000000u ||| uint32 counter)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey = ofSource firstToken.StartIndex kind

/// Identity of a source LAMBDA expression: the INDEX of its anchor token.
///
/// A token index and not the character offset a `NodeKey` carries, which is why this is its
/// own type rather than a `NodeKey` of some lambda kind. The two spaces number differently,
/// so one integer names a different node in each, and only the type keeps a key of one from
/// being read as a key of the other. What that buys is two facts held by construction: a
/// lambda's identity can never anchor a diagnostic or a span (both take a `NodeKey`, whose
/// number IS a place in the source), and it can never be confused with a definition site
/// (a lambda expression binds nothing, so it has no `BinderId` either). Distinctness and
/// equality are the whole of what it supports.
///
/// The producer and the two consumers are in three different domains and none of them holds
/// the others' representation: `InferApp` files the verdict off a CST pattern token,
/// `TastPools.toPools` stamps the pooled lambda's id space off the frozen row's anchor, and
/// `TastUnpool.rebuildFile` inverts that id back off the `ExprToks` column. All three speak
/// the same `Anchor` — the frozen spine stores anchors, so there is nothing left to resolve
/// at any of them. A verdict filed under one spelling and sought under another does not
/// fault — it silently resolves to no lambda, and the closure it was about is emitted as if
/// no verdict existed.
[<Struct>]
type LambdaKey = | LambdaKey of anchor: Anchor
