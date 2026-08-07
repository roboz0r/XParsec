namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

[<Struct>]
type RegionId =
    val Raw: int
    new(raw) = { Raw = raw }
    static member Unknown = RegionId(-1)

/// `Rational.create` canonicalises (`gcd(|Numerator|, Denominator) = 1`, `Denominator > 0`);
/// the raw constructor does not, and `Equals` compares fields, so only canonical values match.
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

/// Roslyn's *ref-safe-context* tiers (ratified C# spec), widest-escape-first.
[<RequireQualifiedAccess>]
type SafeContext =
    /// Must live on the heap, never a `ref struct`.
    | Heap
    /// Escapes to the caller's frame (e.g. via a caller-provided `ref`/`out`).
    | CallingMethod
    /// (.NET 7+) May be returned *by value* (sret), but not stored into a caller-visible ref.
    | ReturnOnly
    /// Confined to this frame — the ref-struct green-light, modulo the representation axis.
    | CurrentMethod

/// Tofte–Talpin coarsening of `EscapeState` for a native backend (MLIR / LLVM).
[<RequireQualifiedAccess>]
type NativeRegionTier =
    /// `alloca` + `nocapture` / `noalias` parameter attributes.
    | Stack
    /// Caller-provided return slot (`sret`) / out-param.
    | ReturnSlot
    /// Arena / bump region (Tofte–Talpin `letregion`), or `Rc` / `Arc` / GC when shared.
    | Heap

/// Declared narrowest-escape-first, so the derived comparison orders them
/// `LocalStack < ReturnOnly < CallerStack < HeapShared`.
type EscapeState =
    | LocalStack
    /// May be returned *by value* but not captured by a caller's refs.
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

/// Orthogonal to the `EscapeState` *lifetime* axis: a frame-confined closure is still pinned
/// to the heap by a boxing channel — a class capture, an `obj` upcast.
[<RequireQualifiedAccess>]
type RegionRepr =
    /// No heap-repr channel reaches this region.
    | StackOnlyEligible
    /// A boxing / heap-escape channel pins this region to a reference type.
    | RequiresHeapRepr

/// Stack-vs-heap verdict for one closure: the conjunction of `EscapeState.LocalStack`
/// (lifetime) and `RegionRepr.StackOnlyEligible` (representation).
[<RequireQualifiedAccess>]
type ClosureRepr =
    /// The reference-type closure shape.
    | Heap
    /// Frame-confined and free of any heap-repr channel.
    | Stack

/// Whether the structural-equality triple (`GetHashCode()` / `Equals(object)` /
/// `IEquatable<Self>::Equals(Self)`) ships on a record / union. An interface ignores it.
[<RequireQualifiedAccess>]
type EqualityVerdict =
    /// Emit the structural-equality triple + the `IEquatable<Self>`
    /// `InterfaceImpl`. Default for a union and an all-immutable record.
    | Structural
    /// Emit no triple; `Object.Equals` / `Object.GetHashCode` (reference identity)
    /// suffice. Default for a record with any mutable field, and `[<ReferenceEquality>]`.
    | Reference
    /// The type provides its own equality (`[<CustomEquality>]`): no triple is synthesised
    /// and an equality use site is SATISFIED.
    | Custom
    /// Emit no triple and forbid equality: a `=` / `<>` use site is a diagnostic.
    | NoEquality

/// Whether the structural-comparison pair (`int CompareTo(Self)` / `int CompareTo(object)` +
/// `IComparable<Self>` / `IComparable` `InterfaceImpl`s) ships on a record / union. Opt-in:
/// an unannotated record / union is `NoComparison`.
[<RequireQualifiedAccess>]
type ComparisonVerdict =
    /// Emit the structural-comparison pair + the `IComparable<Self>` / `IComparable`
    /// `InterfaceImpl`s. Requires an explicit `[<StructuralComparison>]` attribute.
    | Structural
    /// The type provides its own comparison (`[<CustomComparison>]`): no pair is synthesised
    /// and a comparison use site is SATISFIED.
    | Custom
    /// Emit no pair; `<` / `>` / `<=` / `>=` against this type is a diagnostic.
    /// Default for unannotated records / unions.
    | NoComparison

/// The axis a `FrozenType.FTTypar` indexes into: the declaring type's own generic
/// parameters (`!i` in CLI metadata) versus a method's own (`!!i`).
[<RequireQualifiedAccess>]
type TyparAxis =
    | Declaring
    | Method

/// A generalized scheme bound INSIDE one frozen body, numbered densely within that body.
/// OPAQUE and BODY-RELATIVE: it resolves against no file, pool or side table.
[<Struct>]
type SchemeId = | SchemeId of int

/// A dense index into `FrozenPools`' binder columns. A binder is a definition site the tree
/// INTRODUCES: a `NamedSimple` pattern, a `ForTo` loop variable, a type's key slots.
[<Struct>]
type BinderId = | BinderId of int

/// A dense index into a file's `Specializations` table — how a `TExprG.InlineCall` names the
/// body it calls. An entry IS its slot.
[<Struct>]
type SpecializationId = | SpecializationId of int

/// The constant value a structural LITERAL type carries: `"GET"`, or an `Int` for a numeric
/// literal union. External vocabulary ONLY — inference never mints one.
[<RequireQualifiedAccess>]
type LiteralConst =
    | String of string
    | Int of int64

    /// The front-end primitive a literal erases to (`string` / `int`), not a backend repr.
    member this.BaseName: string =
        match this with
        | LiteralConst.String _ -> "string"
        | LiteralConst.Int _ -> "int"

    /// The literal SPELLING (`"GET"` quoted, `42` bare), for diagnostics.
    member this.Render: string =
        match this with
        | LiteralConst.String s -> "\"" + s + "\""
        | LiteralConst.Int n -> string n
