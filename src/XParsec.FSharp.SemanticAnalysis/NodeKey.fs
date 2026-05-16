namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Parser

// 64-bit key used to index every semantic side table. Wire format:
//
//   bit 63        bit 62..32                      bit 31..0
//   +----------+---------------------------------+----------------------+
//   | syn:1    | kind:31                         | offset:32            |
//   +----------+---------------------------------+----------------------+
//
// See docs/nodekey.md for the full rationale (including why we use
// (offset, kind) and not a sequential ID, and how synthetic keys work).

/// Enumerates every CST node kind that can be the target of a NodeKey.
///
/// Two different CST node types CAN start at the same source offset (e.g. a
/// LetBinding and its Pattern), so (offset, kind) is what makes a real key
/// unique — not offset alone.
///
/// This enum is intentionally flat. F#'s CST has on the order of ~200
/// distinct node constructors; a single uint16-wide enum keeps the NodeKey
/// packing logic simple. Add new entries as the constraint generator grows
/// to handle more node kinds — the assignment order does not matter for
/// correctness, only that values are stable within a single compilation.
type NodeKind =
    | Unknown = 0us

    // Expressions — sketch only; expand as Passes/Desugar.fs and
    // Passes/Unification.fs start consuming the real Expr<_> DU.
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

    // Patterns
    | PatConst = 100us
    | PatIdent = 101us
    | PatLongIdent = 102us
    | PatTuple = 103us
    | PatRecord = 104us
    | PatWildcard = 105us
    | PatAs = 106us

    // Types
    | TypeNamed = 200us
    | TypeVarRef = 201us
    | TypeFunction = 202us
    | TypeTuple = 203us
    | TypeGeneric = 204us

    // Declarations / top-level
    | DeclLetBinding = 300us
    | DeclModule = 301us
    | DeclNamespace = 302us
    | DeclType = 303us
    | DeclOpen = 304us

    // Synthetic-only kinds — used exclusively as the "kind" field of a
    // synthetic NodeKey. Real CST nodes never carry these.
    | SynthCEMethodCall = 1000us
    | SynthLambdaBody = 1001us
    | SynthDesugaredApp = 1002us

/// 64-bit key identifying a CST node (real or synthetic) within one
/// compilation. Equality and hashing are pure integer ops.
[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source character offset for real nodes; spawning-construct offset for synthetic ones.
    member this.Offset: int = int (uint32 this.Raw)

    /// 31-bit CST node-kind tag.
    member this.Kind: NodeKind =
        // mask off the syn bit (63) before shifting kind down by 32
        let kindBits = (this.Raw &&& 0x7FFFFFFF00000000UL) >>> 32
        LanguagePrimitives.EnumOfValue (uint16 kindBits)

    /// True for synthetic keys (nodes minted during desugaring that have no
    /// source position of their own).
    member this.IsSynthetic: bool = (this.Raw &&& 0x8000000000000000UL) <> 0UL

    override this.ToString() =
        let tag = if this.IsSynthetic then "syn" else "src"
        sprintf "%s@%d:%A" tag this.Offset this.Kind

module NodeKey =

    let private synBit = 0x8000000000000000UL

    /// Build a real NodeKey from a source offset + CST node kind.
    let ofSource (offset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 offset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k)

    /// Build a synthetic NodeKey. `spawningOffset` is the source offset of
    /// the construct that produced this synthetic node — used purely for
    /// debugging and locality-grouped sorting. See docs/nodekey.md.
    let ofSynthetic (spawningOffset: int) (kind: NodeKind) : NodeKey =
        let off = uint64 (uint32 spawningOffset)
        let k = (uint64 (LanguagePrimitives.EnumToValue kind)) <<< 32
        NodeKey(off ||| k ||| synBit)

    /// Extract a NodeKey from any CST node that carries a leading SyntaxToken.
    /// Callers pass the node's first token and its kind. Helpers per CST DU
    /// case will live next to the passes that need them (e.g. in Desugar.fs
    /// when it walks Expr<SyntaxToken>).
    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey =
        ofSource firstToken.StartIndex kind
