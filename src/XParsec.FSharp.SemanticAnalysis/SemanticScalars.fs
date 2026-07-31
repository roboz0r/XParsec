namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

/// Revisit if region analysis ever wants union-find (it shouldn't — regions
/// are inequality, not equality).
[<Struct>]
type RegionId =
    val Raw: int
    new(raw) = { Raw = raw }
    static member Unknown = RegionId(-1)

/// Arbitrary-precision rational. Always stored in canonical form:
/// `gcd(|Numerator|, Denominator) = 1` and `Denominator > 0`. Construct
/// via `Rational.create`; equality and hashing are structural over the
/// canonical representation, so two rationals built from non-reduced
/// fractions compare equal iff they denote the same value.
[<Struct; CustomEquality; CustomComparison>]
type Rational =
    val Numerator: bigint
    val Denominator: bigint
    new(n: bigint, d: bigint) = { Numerator = n; Denominator = d }

    static member create(n: bigint, d: bigint) : Rational =
        if d.IsZero then
            invalidArg "d" "Rational denominator must be nonzero"

        let sign = if d.Sign < 0 then bigint -1 else bigint 1
        let n' = n * sign
        let d' = d * sign
        let g = BigInteger.GreatestCommonDivisor(BigInteger.Abs n', d')
        Rational(n' / g, d' / g)

    static member ofInt(n: int) : Rational = Rational(bigint n, bigint 1)

    static member Zero = Rational(bigint 0, bigint 1)
    static member One = Rational(bigint 1, bigint 1)

    member this.IsZero = this.Numerator.IsZero
    member this.IsOne = this.Numerator = bigint 1 && this.Denominator = bigint 1

    static member (+)(a: Rational, b: Rational) : Rational =
        Rational.create (a.Numerator * b.Denominator + b.Numerator * a.Denominator, a.Denominator * b.Denominator)

    static member (-)(a: Rational, b: Rational) : Rational =
        Rational.create (a.Numerator * b.Denominator - b.Numerator * a.Denominator, a.Denominator * b.Denominator)

    static member (~-)(a: Rational) : Rational = Rational(-a.Numerator, a.Denominator)

    static member (*)(a: Rational, b: Rational) : Rational =
        Rational.create (a.Numerator * b.Numerator, a.Denominator * b.Denominator)

    override this.Equals(other: obj) =
        match other with
        | :? Rational as r -> this.Numerator = r.Numerator && this.Denominator = r.Denominator
        | _ -> false

    override this.GetHashCode() =
        let h1 = this.Numerator.GetHashCode()
        let h2 = this.Denominator.GetHashCode()
        (h1 * 397) ^^^ h2

    interface System.IComparable with
        member this.CompareTo(other: obj) =
            match other with
            | :? Rational as r ->
                let lhs = this.Numerator * r.Denominator
                let rhs = r.Numerator * this.Denominator
                compare lhs rhs
            | _ -> invalidArg "other" "Cannot compare Rational to a different type"

    override this.ToString() =
        if this.Denominator = bigint 1 then
            string this.Numerator
        else
            sprintf "%O/%O" this.Numerator this.Denominator

/// Roslyn's *ref-safe-context* tiers (ratified C# spec) — the CLR emission
/// target's vocabulary, in widest-escape-first order. `EscapeState`
/// coarsens onto these via `EscapeState.toClrRefSafe`. The C# compiler and
/// the CLR verifier enforce exactly this relation and the .NET 9 `allows ref
/// struct` rules are layered on top.
[<RequireQualifiedAccess>]
type SafeContext =
    /// Roslyn's "beyond the lattice": must live on the heap, never a
    /// `ref struct`. The `EscapeState.HeapShared` image.
    | Heap
    /// `CallingMethod` — escapes to the caller's frame (e.g. via a
    /// caller-provided `ref`/`out`).
    | CallingMethod
    /// `ReturnOnly` (.NET 7+) — may be returned *by value* (sret), but not
    /// stored into a caller-visible ref.
    | ReturnOnly
    /// `CurrentMethod` — confined to this frame; the unconditional
    /// ref-struct green-light (modulo the Axis-2 representation check).
    | CurrentMethod

/// Tofte–Talpin coarsening of `EscapeState` for a future native backend
/// (MLIR / LLVM). Not consumed yet.
[<RequireQualifiedAccess>]
type NativeRegionTier =
    /// `alloca` + `nocapture` / `noalias` parameter attributes.
    | Stack
    /// Caller-provided return slot (`sret`) / out-param.
    | ReturnSlot
    /// Arena / bump region (Tofte–Talpin `letregion`) or, when unbounded /
    /// shared, `Rc` / `Arc` / GC.
    | Heap

/// `LocalStack` → `ref struct` (.NET) / `&T` (Rust); `HeapShared` → `Rc<T>` /
/// `Arc<T>` (Rust). Ordered widest-escape-first: `HeapShared > CallerStack >
/// ReturnOnly > LocalStack`; the two coarsening maps (`toClrRefSafe`,
/// `toNativeRegionTier`) live on the companion module.
type EscapeState =
    | LocalStack
    /// May be returned *by value* but not captured by a caller's refs —
    /// Roslyn's `ReturnOnly` tier. More permissive than `CallerStack`; minted
    /// on a returned-but-non-escaping closure. v1 lays the tier down but does
    /// not act on it for emission.
    | ReturnOnly
    | CallerStack
    | HeapShared

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EscapeState =

    let toClrRefSafe (s: EscapeState) : SafeContext =
        match s with
        | LocalStack -> SafeContext.CurrentMethod
        | ReturnOnly -> SafeContext.ReturnOnly
        | CallerStack -> SafeContext.CallingMethod
        | HeapShared -> SafeContext.Heap

    let toNativeRegionTier (s: EscapeState) : NativeRegionTier =
        match s with
        | LocalStack -> NativeRegionTier.Stack
        | ReturnOnly
        | CallerStack -> NativeRegionTier.ReturnSlot
        | HeapShared -> NativeRegionTier.Heap

/// Axis-2 representation requirement for a region, orthogonal to the
/// `EscapeState` *lifetime* axis. A closure can be frame-confined by lifetime
/// yet still pinned to a heap representation by a containment / boxing channel —
/// held in a non-`ref struct` aggregate (including a `System.ValueTuple`, which
/// cannot carry a ref-struct field), captured by a heap-class closure, escaping
/// to the heap, or upcast to `Vesper.Fun<_,_>` / `obj`. The ref-struct-closure
/// eligibility predicate is `LocalStack ∧ StackOnlyEligible`; this axis supplies
/// the second conjunct, computed by a forward fixpoint over the same region graph.
[<RequireQualifiedAccess>]
type RegionRepr =
    /// No heap-repr channel reaches this region — eligible for the deferred
    /// readonly-struct closure shape (modulo the Axis-1 lifetime check).
    | StackOnlyEligible
    /// A containment / boxing / heap-escape channel pins this region to a
    /// reference-type representation.
    | RequiresHeapRepr

/// Codegen-facing stack-vs-heap verdict for one closure: the conjunction of
/// `EscapeState.LocalStack` (Axis 1) and `RegionRepr.StackOnlyEligible` (Axis 2).
/// Snapshotted per closure binder onto `TastFile.ClosureReprs`. `Heap` is the only
/// shape emitted today; `Stack` flags a closure the deferred readonly-struct work
/// may lower onto a `valuetype`. Inert in v1 — emission still forces heap.
[<RequireQualifiedAccess>]
type ClosureRepr =
    /// The reference-type closure shape emitted today; the only verdict acted on.
    | Heap
    /// Frame-confined (Axis 1) and free of any heap-repr channel (Axis 2) —
    /// eligible for the deferred struct-closure shape. Carried but not yet emitted.
    | Stack

/// Per-type decision on whether the structural-equality triple
/// (`GetHashCode()` / `Equals(object)` / `IEquatable<Self>::Equals(Self)`) ships
/// on a record / union. Computed by `Passes/Attributes.fs` off
/// `[<StructuralEquality>]` / `[<ReferenceEquality>]` / `[<NoEquality>]`
/// declarations. Default: an
/// all-immutable record or any union ⇒ `Structural`; a record with any mutable
/// field ⇒ `Reference`. An interface ignores it (no triple is ever synthesised).
[<RequireQualifiedAccess>]
type EqualityVerdict =
    /// Emit the structural-equality triple + the `IEquatable<Self>`
    /// `InterfaceImpl`. Default for a union and an all-immutable record.
    | Structural
    /// Emit no triple; `Object.Equals` / `Object.GetHashCode` (reference
    /// identity) suffice. Default for a record with any mutable field; also
    /// the `[<ReferenceEquality>]`-attributed case.
    | Reference
    /// The type provides its own equality (`[<CustomEquality>]`): no triple is
    /// synthesised; the user's `Equals`/`GetHashCode`/`IEquatable<Self>` members
    /// are authoritative. An equality use site is SATISFIED. Validation (a later
    /// phase) requires the type to implement `IEquatable<Self>`.
    | Custom
    /// Emit no triple AND mark the type as forbidding equality; a `=` /
    /// `<>` use site against this type is a diagnostic (driven through the
    /// `Equality` typar-constraint check in `Unification`).
    | NoEquality

/// Per-type decision on whether the structural-comparison pair
/// (`int CompareTo(Self)` / `int CompareTo(object)` + `IComparable<Self>` /
/// `IComparable` `InterfaceImpl`s) ships on a record / union. Computed by
/// `Passes/Attributes.fs` off `[<StructuralComparison>]` /
/// `[<NoComparison>]` declarations. Default is
/// **opt-in**: an unannotated record / union is `NoComparison`, so ordering
/// use sites (`r1 < r2`) are rejected unless `[<StructuralComparison>]` is
/// present.
[<RequireQualifiedAccess>]
type ComparisonVerdict =
    /// Emit the structural-comparison pair + the `IComparable<Self>` /
    /// `IComparable` `InterfaceImpl`s. Requires an explicit
    /// `[<StructuralComparison>]` attribute on the type.
    | Structural
    /// The type provides its own comparison (`[<CustomComparison>]`): no pair is
    /// synthesised; the user's `CompareTo`/`IComparable<Self>` members are
    /// authoritative. A comparison use site is SATISFIED. Validation (a later
    /// phase) requires the type to implement `IComparable<Self>`.
    | Custom
    /// Emit no pair; `<` / `>` / `<=` / `>=` against this type is a diagnostic
    /// (driven through the `Comparison` typar-constraint check in
    /// `Unification`). Default for unannotated records / unions.
    | NoComparison

/// The axis a `FrozenType.FTTypar` indexes into: the declaring type's own
/// generic parameters (`!i` in CLI metadata) versus a method's own generic
/// parameters (`!!i`). There is deliberately no `Closure` axis — closure
/// typars are a codegen-synthesis concept, never expressed in a frozen
/// signature or a provider descriptor.
[<RequireQualifiedAccess>]
type TyparAxis =
    | Declaring
    | Method

/// A generalized scheme bound INSIDE one frozen body, numbered densely within that body —
/// the identity `FrozenType.FTLocalTypar` names, and the reason it needs no key. It is
/// OPAQUE and BODY-RELATIVE: nothing may resolve it against a file, a pool, or a side
/// table; the only operations it supports are the two `FTLocalTypar` needs, distinctness
/// and equality, and both are interpreted only against the body carrying the leaf.
///
/// Being file-scoped is the point. A `NodeKey` addresses a node of SOME file with no file
/// id in it, so two files' local schemes collide silently and a reader is always one
/// careless lookup away from resolving one; a dense per-body int cannot be mistaken for
/// anything resolvable, because it names nothing outside the body it came with.
[<Struct>]
type SchemeId = | SchemeId of int

/// A dense pool index into `FrozenPools`' binder columns — the positional identity a frozen
/// binder takes on once kind dissolves. A binder is a definition site the tree INTRODUCES —
/// a `NamedSimple` pattern, a `ForTo` loop variable, or one of a type declaration's
/// pattern-less key slots (the `BinderKey` projections); the cross-references that named it
/// by 64-bit content key during analysis (`Var.binding`, the side-table keys) name it by
/// this id in the pool form.
///
/// Minted by exactly ONE thing — interning a `BinderKey` (`TastPools`' sink) — which is why
/// this space, unlike the node space a `NodeKey` addresses, holds definition sites and
/// nothing else. `BinderKey.ofInterned` is what that buys.
///
/// Declared with the other dense scalars rather than with the pools it indexes, because the
/// binder-key projections are typed over it and compile long before them.
[<Struct>]
type BinderId = | BinderId of int

/// A dense index into a file's RESOLVED-SPECIALIZATION table
/// (`TastFileG.Specializations` / `FrozenPools.Specializations`) — the identity a
/// `TExprG.InlineCall` names the body it calls by. An entry IS its slot, exactly as a binder
/// is, so nothing beside the index is stored at the edge.
///
/// Its own type and not a bare `int`, for the reason every other dense id here has one: an
/// index is meaningful only against the array it indexes, and the tree already carries four
/// other index spaces an untyped one would silently interchange with.
///
/// Declared with the other dense scalars because `TExprG` is typed over it and compiles long
/// before the table.
[<Struct>]
type SpecializationId = | SpecializationId of int

/// The constant value a structural LITERAL type carries (`FTLiteral`/`TyLiteral`).
/// String first (`"GET"`); `Int` falls out for numeric literal unions. No `bool`
/// (design §"Literal types stay structural … string first; skip bool"). A literal
/// type is external-vocabulary ONLY — Vesper inference never mints one (the
/// nominalism invariant), so this is produced solely by instantiating an external
/// signature. It ERASES to `BaseName` (its base primitive) on both backends.
[<RequireQualifiedAccess>]
type LiteralConst =
    | String of string
    | Int of int64

    /// The base primitive a literal of this value erases to — a member of the
    /// canonical FRONT-END type vocabulary (`string` / `int`), NOT a backend repr.
    member this.BaseName: string =
        match this with
        | LiteralConst.String _ -> "string"
        | LiteralConst.Int _ -> "int"

    /// The one literal SPELLING (`"GET"` quoted, `42` bare) — the human-facing form
    /// for diagnostics (`InferApp`'s allowed-literal message). Overload identity no
    /// longer renders: a `MemberKey` argSig interns the `FTLiteral` structurally.
    member this.Render: string =
        match this with
        | LiteralConst.String s -> "\"" + s + "\""
        | LiteralConst.Int n -> string n
