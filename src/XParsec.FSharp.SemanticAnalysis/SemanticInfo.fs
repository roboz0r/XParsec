namespace XParsec.FSharp.SemanticAnalysis

// See docs/typevar.md for the 3-axis design.

/// Sequential ints. Revisit if region analysis ever wants union-find
/// (it shouldn't — regions are inequality, not equality).
[<Struct>]
type RegionId =
    val Raw: int
    new(raw) = { Raw = raw }
    static member Unknown = RegionId(-1)

/// Maps to Phase 4.6 target lowering: LocalStack -> `ref struct` (.NET) / `&T`
/// (Rust); HeapShared -> `Rc<T>` / `Arc<T>` (Rust).
type EscapeState =
    | LocalStack
    | CallerStack
    | HeapShared

/// Mutually recursive with TypeVar — every TyVar is a pointer into the
/// union-find graph. Will grow to include generics, units.
type SemType =
    /// Call UnionFind.find then read the representative's Link to dereference.
    | TyVar of TypeVar
    | TyConst of name: string
    /// Curried; multi-arg functions nest TyFun.
    | TyFun of arg: SemType * result: SemType
    /// Flat n-ary tuple. Unifies pairwise with same-arity TyTuple; arity
    /// mismatch is a diagnostic in Unification.
    | TyTuple of items: SemType list

/// TODO: `(UnitName * int) list` representing an abelian-group exponent vector.
and MeasureTerm = | MeasurePlaceholder

/// TODO: real shape when SRTPs come online.
and MemberSignature = | MemberSignaturePlaceholder

/// TODO: real shape when IWSAMs come online.
and InterfaceBound = | InterfaceBoundPlaceholder

and [<Sealed>] TypeVar() =
    /// Authoritative only on the representative — call UnionFind.find first.
    member val Link: SemType voption = ValueNone with get, set
    member val Units: MeasureTerm list = [] with get, set
    member val Region: RegionId = RegionId.Unknown with get, set
    /// Fires when Link is set (on-unified callback in Unification).
    member val IfaceBounds: InterfaceBound list = [] with get, set
    /// Fires when Link is set (on-unified callback in Unification).
    member val SrtpBounds: MemberSignature list = [] with get, set
    // Owned by UnionFind; do not mutate directly.
    member val Parent: TypeVar voption = ValueNone with get, set
    member val Rank: int = 0 with get, set

/// BindingSite is the NodeKey of the LetBinding / lambda parameter /
/// TypeMember that introduced the name — NOT the use site.
type ResolvedBinding =
    {
        BindingSite: NodeKey
        IsInline: bool
        IsMutable: bool
    }

/// A thin view, not a rewritten tree: Desugar attaches this without ever
/// mutating CST shape.
[<RequireQualifiedAccess>]
type DesugaredForm =
    /// On an InfixApp / PrefixApp node, the operator's compiled name
    /// ("op_Addition", "op_Subtraction", …). Unification looks the name up
    /// via the provider and types the application as if it were a normal
    /// function call.
    | OpName of compiledName: string
