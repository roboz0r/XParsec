namespace XParsec.FSharp.SemanticAnalysis

open System
open XParsec.FSharp.Parser

// Wire format (see docs/nodekey.md):
//   bit 63        bit 62..32                      bit 31..0
//   +----------+---------------------------------+----------------------+
//   | syn:1    | kind:31                         | offset:32            |
//   +----------+---------------------------------+----------------------+

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

    | PatConst = 100us
    | PatIdent = 101us
    | PatLongIdent = 102us
    | PatTuple = 103us
    | PatRecord = 104us
    | PatWildcard = 105us
    | PatAs = 106us
    | PatEnclosedBlock = 107us

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

[<Struct>]
type NodeKey =
    val Raw: uint64
    new(raw: uint64) = { Raw = raw }

    /// Source offset for real nodes; spawning-construct offset for synthetic ones.
    member this.Offset: int = int (uint32 this.Raw)

    member this.Kind: NodeKind =
        let kindBits = (this.Raw &&& 0x7FFFFFFF00000000UL) >>> 32
        LanguagePrimitives.EnumOfValue (uint16 kindBits)

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

    // TODO: per-CST-DU-case helpers (ofExpr, ofPat, ofDecl) live next to the
    // passes that walk those DUs.
    let ofToken (firstToken: SyntaxToken) (kind: NodeKind) : NodeKey =
        ofSource firstToken.StartIndex kind
