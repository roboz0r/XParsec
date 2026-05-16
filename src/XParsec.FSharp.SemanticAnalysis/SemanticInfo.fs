namespace XParsec.FSharp.SemanticAnalysis

// Semantic-info value types attached to CST nodes via the side tables in
// SideTables.fs. See docs/typevar.md for the design rationale (the 3-axis
// TypeVar, deferred SRTP/IWSAM bounds, why we use voption everywhere).
//
// This file is intentionally light on structure. Each type is a placeholder
// that compiles and gives the pass authors a name to write to. Real layout
// decisions (e.g. MeasureTerm as sorted list vs dictionary, whether
// InterfaceBound and SrtpBound should share a representation) get made when
// the pass that owns the type starts being implemented.

/// Identifier for an allocation region. Sequential ints for now; revisit if
/// region analysis ever wants union-find (it shouldn't — regions are
/// inequality, not equality).
[<Struct>]
type RegionId =
    val Raw: int
    new(raw) = { Raw = raw }
    static member Unknown = RegionId(-1)

/// The escape state assigned to every TypeVar after Regions runs. Maps to
/// Phase 4.6 target-specific lowering: LocalStack -> ref struct (.NET) / &T
/// (Rust); HeapShared -> Rc<T> / Arc<T> (Rust).
type EscapeState =
    | LocalStack
    | CallerStack
    | HeapShared

/// The semantic type. Will become a real DU as Unification grows — function
/// types, generic instantiations, primitive types, etc. Kept as a single
/// placeholder case for now so the file compiles.
type SemType =
    | TyPlaceholder

/// A single (unit, exponent) term in a measure expression. The full measure
/// is `MeasureTerm list`, treated as an abelian group: order doesn't matter,
/// like exponents cancel. Representation TBD — see docs/typevar.md
/// "Open questions".
type MeasureTerm =
    | MeasurePlaceholder

/// A deferred SRTP bound: "this TypeVar must support member with this signature."
/// Resolved by Unification's on-unified callback once the TypeVar's Link is set.
type MemberSignature =
    | MemberSignaturePlaceholder

/// A deferred IWSAM / interface / trait bound: "this TypeVar must implement
/// this interface." Resolved by interface-lookup once Link is set.
type InterfaceBound =
    | InterfaceBoundPlaceholder

/// The 3-axis inference variable. Mutated in place by Unification; class
/// rather than record so identity (each `new TypeVar()` is its own variable)
/// reads naturally.
[<Sealed>]
type TypeVar() =
    member val Link: SemType voption = ValueNone with get, set
    member val Units: MeasureTerm list = [] with get, set
    member val Region: RegionId = RegionId.Unknown with get, set
    member val IfaceBounds: InterfaceBound list = [] with get, set
    member val SrtpBounds: MemberSignature list = [] with get, set
    // Union-find fields
    member val Parent: TypeVar voption = ValueNone with get, set
    member val Rank: int = 0 with get, set

/// What NameResolution attaches per ident-use site: the binding site it
/// resolved to. The binding site itself is identified by a NodeKey (the
/// NodeKey of the LetBinding / parameter / TypeMember that introduced the
/// name).
type ResolvedBinding =
    { BindingSite: NodeKey
      IsInline: bool
      IsMutable: bool }

/// What Desugar attaches for nodes whose semantics differ from their surface
/// form (computation expressions, list comprehensions, pipeline operators,
/// for-in-do over arbitrary IEnumerable, …). The DesugaredForm is a thin
/// view, not a rewritten tree — it references either other CST nodes by
/// NodeKey or synthetic NodeKeys minted during this pass.
type DesugaredForm =
    | DesugaredPlaceholder
