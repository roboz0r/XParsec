namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Parser

// Wire format (see docs/nodekey.md):
//   bit 63        bit 62..48          bit 47..32      bit 31..0
//   +----------+----------------------+--------------+----------------------+
//   | syn:1    | reserved:15          | kind:16      | offset:32            |
//   +----------+----------------------+--------------+----------------------+
//
// The 15 reserved bits are available for a future per-spawning-construct
// counter (see docs/nodekey.md "Synthetic NodeKeys"); today they're always
// zero and `Kind` ignores them.

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
    /// colliding with source keys (sign bit) or other synthetics; the minter
    /// packs a monotone per-build counter into the offset slot for uniqueness.
    | SynthInlineExpansion = 1004us
    /// Anchor for a "not yet supported" diagnostic on a CST shape whose own
    /// keying isn't yet implemented (e.g. `ModuleElem.Missing`,
    /// `ModuleElem.SkipsTokens`). The offset is the spawning token's source
    /// offset where available, or `0` for shapes that carry no token. The
    /// distinct kind keeps these placeholder keys from colliding with real
    /// source keys at the same offset.
    | SynthUnsupportedDecl = 1005us

[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source offset for real nodes; spawning-construct offset for synthetic ones.
    member this.Offset: int = int (uint32 this.Raw)

    member this.Kind: NodeKind =
        let kindBits = (this.Raw &&& 0x0000FFFF00000000UL) >>> 32
        LanguagePrimitives.EnumOfValue(uint16 kindBits)

    member this.IsSynthetic: bool = (this.Raw &&& 0x8000000000000000UL) <> 0UL

    override this.ToString() =
        let tag = if this.IsSynthetic then "syn" else "src"
        sprintf "%s@%d:%A" tag this.Offset this.Kind

module NodeKey =

    let private synBit = 0x8000000000000000UL

    let ofSource (offset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 offset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k)

    /// `spawningOffset` is the source offset of the construct that produced
    /// this synthetic node — used purely for debugging and locality-grouped
    /// sorting, not for uniqueness.
    let ofSynthetic (spawningOffset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 spawningOffset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey = ofSource firstToken.StartIndex kind
