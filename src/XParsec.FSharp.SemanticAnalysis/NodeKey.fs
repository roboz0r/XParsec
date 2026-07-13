namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Parser

// Wire format:
//   bit 63        bit 62..48          bit 47..32      bit 31..0
//   +----------+----------------------+--------------+----------------------+
//   | syn:1    | reserved:15          | kind:16      | offset:32            |
//   +----------+----------------------+--------------+----------------------+
//
// The 15 reserved bits are available for a future per-spawning-construct
// counter; today they're always
// zero and `Kind` ignores them.
//
// THE OFFSET SLOT IS SIGNED, and the sign is load-bearing:
//
//   * `Offset >= 0` — a genuine index into the source string. Every real node has
//     one, and so does a synthetic node minted from a SPAWNING construct
//     (`ofSynthetic`), which is why it can still be scoped (`SourcePos`).
//   * `Offset < 0`  — the UNIQUENESS-COUNTER space (`ofSyntheticCounter`). A source
//     offset can never be negative, so this domain is uninhabited by real positions:
//     a key minted there names no place in the file and can never be mistaken for
//     one. That is a checkable fact about the VALUE, not a convention about which
//     `NodeKind`s happen to be counter-minted — `SynthLambdaBody` is minted BOTH
//     ways (from a spawning offset in `ElaborateExpr`, from a counter in
//     `TastLower`), so kind cannot discriminate them and must not be asked to.

/// (offset, kind) — not offset alone — is what makes a real NodeKey unique:
/// two CST node types can start at the same source offset (a LetBinding and
/// its Pattern both start at the `let` keyword).
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
    /// Indexed array lookup (`arr.[i]`). Keyed off the `[` token (not the
    /// receiver's first token) so a lookup node never collides with its
    /// receiver sub-expression's key — same rationale as `ExprInfixApp`.
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
    /// Binding site of a `for i = …` loop variable. The `i` token has no
    /// surrounding `Pat` in the CST, but NameResolution still needs a
    /// stable NodeKey to attach it to.
    | PatForToVar = 110us
    | PatEmptyBlock = 111us
    /// Operator-named binding head (`let (=) x y = …`); the `IdentOrOp` carries
    /// the operator token, not a plain ident.
    | PatOp = 112us
    /// Cons pattern (`h :: t`). Keyed off the `::` token (not the head's first
    /// token) so a cons node never collides with its head sub-pattern's key —
    /// same rationale as the `ExprInfixApp` operator-token choice.
    | PatCons = 113us
    /// Attribute-decorated parameter (`([<CallAtMostOnce>] x)`). Keyed distinctly
    /// from the wrapped pattern (which shares the same first token) so the
    /// `Attributed` node and its inner `NamedSimple` never collide.
    | PatAttributed = 114us
    /// Type-test pattern (`:? T as x`). Keyed off the `:?` token (not the inner
    /// binder's first token) so the test node never collides with its inner
    /// `NamedSimple` sub-pattern's key — same rationale as `PatCons`.
    | PatTypeTestAs = 115us
    /// `null` literal pattern (`match x with null -> …`). Keyed off the `null`
    /// keyword token, which is unique to it.
    | PatNull = 116us
    /// Bare type-test pattern (`:? T`, no `as`-binder). Keyed off the `:?` token,
    /// like `PatTypeTestAs`; binds nothing.
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
    /// Synthetic binder for `this` (or `as self`) inside class member
    /// bodies. One per class, shared across every member.
    | SynthThisBinding = 1003us
    /// Freshened binder produced when an `inline` body is expanded at a call
    /// site (`Inline.freshen`). The distinct kind keeps freshened keys from
    /// colliding with source keys (sign bit) or other synthetics; uniqueness comes
    /// from a monotone counter (`ofSyntheticCounter`), so the key names no source
    /// position.
    | SynthInlineExpansion = 1004us
    /// Anchor for a "not yet supported" diagnostic on a CST shape whose own
    /// keying isn't yet implemented (e.g. `ModuleElem.Missing`,
    /// `ModuleElem.SkipsTokens`). The offset is the spawning token's source
    /// offset where available, or `0` for shapes that carry no token. The
    /// distinct kind keeps these placeholder keys from colliding with real
    /// source keys at the same offset.
    | SynthUnsupportedDecl = 1005us
    /// Synthetic binder for `base` inside a derived class's member bodies.
    /// One per class with `inherit Base(...)`; shared across every member.
    /// Mirrors `SynthThisBinding`. Wired into Unification / Elaborate when inheritance is active.
    | SynthBaseBinding = 1006us
    /// Freshened binder produced when the *pre-freeze* inline-expansion pass
    /// (`InlineExpansion`) splices an `inline` body. A
    /// distinct kind from `SynthInlineExpansion` so the pass's baked keys can
    /// never collide with the keys codegen's (now-redundant) eta-expansion still
    /// mints from the `SynthInlineExpansion` space during beat (a).
    | SynthPreFreezeInline = 1007us

[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source offset for real nodes; spawning-construct offset for a synthetic node
    /// minted from one. NEGATIVE for a counter-minted key (`ofSyntheticCounter`), which
    /// names no source position at all — see the wire-format note above.
    member this.Offset: int = int (uint32 this.Raw)

    /// Does this key name a place in the source string? True for every real node and for
    /// a synthetic node minted from a spawning construct; false only in the
    /// uniqueness-counter space, which is uninhabited by real positions. THE test
    /// `SourcePos.ofNodeKey` admits a key by — a fact about the value, not about the kind.
    member this.IsSourcePosition: bool = this.Offset >= 0

    /// The offset slot with the counter flag masked off: the source offset for a
    /// positioned key, the bare counter for a counter-minted one. For NAMING a binder a
    /// backend must invent an identifier for (`_s7`, `value@7`) — never for scoping, which
    /// must ask `IsSourcePosition` / go through `SourcePos`. It is not injective across the
    /// two spaces (a counter `7` and a spawning offset `7` render alike), exactly as they
    /// did when both inhabited the positive half.
    member this.NameIndex: int = int (uint32 this.Raw &&& 0x7FFFFFFFu)

    member this.Kind: NodeKind =
        let kindBits = (this.Raw &&& 0x0000FFFF00000000UL) >>> 32
        LanguagePrimitives.EnumOfValue(uint16 kindBits)

    member this.IsSynthetic: bool = (this.Raw &&& 0x8000000000000000UL) <> 0UL

    override this.ToString() =
        let tag = if this.IsSynthetic then "syn" else "src"
        sprintf "%s@%d:%A" tag this.Offset this.Kind

/// WHERE in the file a name is being looked up FROM. F# declaration scoping is
/// file-ordered, so a by-NAME registry read is only answerable against a position: a
/// declaration is visible at a use iff the declaration's `VisibleFrom` offset is at or
/// before the use. Carrying that position on the QUERY (rather than as ambient walk
/// state) means a pass cannot silently forget to scope a read — there is no bound to
/// set, only an argument to pass — and the answer depends on where a node IS, not on
/// when it happens to be visited.
///
/// The representation is private, so `SourcePos.ofNodeKey` and `SourcePos.unbounded` are
/// the ONLY two ways to obtain one — and `ofNodeKey` admits exactly the keys whose offset
/// IS a source position (`NodeKey.IsSourcePosition`, i.e. non-negative). A counter-minted
/// key's offset is a uniqueness token in the negative half of the slot, not a place in the
/// file, so scoping a lookup by it would compare garbage against a claim's `VisibleFrom`;
/// the type makes that unrepresentable rather than merely discouraged.
///
/// The test is on the VALUE, deliberately: neither the syn bit nor the `NodeKind` can
/// answer it. A synthetic node spawned from a real construct (a desugared application, a
/// `this` binder) HAS a source position and must be able to resolve names at it, while
/// `SynthLambdaBody` is minted both from a spawning offset and from a counter — so any
/// kind-based allowlist would be both over-broad and unsound.
[<Struct>]
type SourcePos =
    private
        {
            /// `System.Int32.MaxValue` for `SourcePos.unbounded`; a real source offset
            /// otherwise. The sentinel sits above every offset a 32-bit source position
            /// can hold, so "sees everything" needs no case in the comparison.
            Pos: int
        }

    /// The offset a visibility test compares a claim's `VisibleFrom` against.
    member this.Offset: int = this.Pos

module SourcePos =

    /// A read that sees EVERY declaration, wherever it sits — the whole-unit view, for a
    /// query that has no source position to scope by (an observer of the finished
    /// registry, a consumer that already holds a resolved key).
    let unbounded: SourcePos = { Pos = System.Int32.MaxValue }

    /// The place in the file a node sits at. Fails on a key from the uniqueness-counter
    /// space (a negative offset): it names no place, so there is nothing to scope by. A
    /// pass that needs a scoped read from such a node must carry the position of the
    /// SOURCE construct that spawned it.
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

    /// A synthetic node that HAS a place in the file: `spawningOffset` is the source
    /// offset of the construct that produced it. Non-negative by construction (it comes
    /// from a token, or from another key's source offset), so the node stays scopable —
    /// a desugared application resolves names exactly where the expression it desugars
    /// was written. Uniqueness against other keys at that offset comes from `kind`, not
    /// from the offset. A synthetic node with NO such construct must use
    /// `ofSyntheticCounter`.
    let ofSynthetic (spawningOffset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 spawningOffset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    /// A synthetic node whose uniqueness comes from a monotone COUNTER rather than from a
    /// place in the file (a freshened inline binder, a placeholder lambda-param slot). The
    /// counter is packed into the NEGATIVE half of the offset slot — a domain no source
    /// offset can inhabit — so `SourcePos.ofNodeKey` can refuse the key on its value alone
    /// and no scoped lookup can ever compare a counter against a claim's position. Counters
    /// stay far below 2^31, so the low 31 bits carry them intact and uniqueness is
    /// unaffected.
    let ofSyntheticCounter (counter: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (0x80000000u ||| uint32 counter)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey = ofSource firstToken.StartIndex kind
