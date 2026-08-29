namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

[<Struct>]
type RegionId =
    val Raw: int
    new(raw) = { Raw = raw }
    static member Unknown = RegionId(-1)

/// A canonical rational: `gcd(|Numerator|, Denominator) = 1` and `Denominator > 0`. The
/// zero-initialised struct is `0/1`, so `Unchecked.defaultof<Rational>` equals `Rational.Zero`
/// and an array of them starts canonical.
[<Struct; CustomEquality; CustomComparison>]
type Rational =
    val Numerator: bigint
    /// One less than the denominator, which is what puts `0/1` at the default. A bijection
    /// onto the denominators, so equality over the stored fields IS value equality.
    val private denominatorLess1: bigint

    /// Reduces `n`/`d` to lowest terms with a positive denominator. Raises `ArgumentException`
    /// when `d` is zero.
    private new(n: bigint, d: bigint) =
        if d.IsZero then
            invalidArg "d" "Rational denominator must be nonzero"

        let sign = if d.Sign < 0 then BigInteger.MinusOne else BigInteger.One
        let n' = n * sign
        let d' = d * sign
        let g = BigInteger.GreatestCommonDivisor(BigInteger.Abs n', d')

        {
            Numerator = n' / g
            denominatorLess1 = (d' / g) - BigInteger.One
        }

    member this.Denominator: bigint = this.denominatorLess1 + BigInteger.One

    static member create(n: bigint, d: bigint) : Rational = Rational(n, d)

    static member ofInt(n: int) : Rational = Rational(bigint n, BigInteger.One)

    static member Zero = Rational(BigInteger.Zero, BigInteger.One)
    static member One = Rational(BigInteger.One, BigInteger.One)

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
        | :? Rational as r -> this.Numerator = r.Numerator && this.denominatorLess1 = r.denominatorLess1
        | _ -> false

    override this.GetHashCode() =
        let h1 = this.Numerator.GetHashCode()
        let h2 = this.denominatorLess1.GetHashCode()
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
    /// Confined to this frame: the ref-struct green-light, modulo the representation axis.
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
/// to the heap by a boxing channel such as a class capture or an `obj` upcast.
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

/// A dense index into `FrozenPools`' bound variable columns. A bound variable is a definition site the tree
/// INTRODUCES: a `NamedSimple` pattern, a `ForTo` loop variable, a type's key slots.
[<Struct>]
type BoundVarId = | BoundVarId of int

/// A dense index into a file's `Specializations` table: how a `TExprG.InlineCall` identifies the
/// body it calls. An entry IS its slot.
[<Struct>]
type SpecializationId = | SpecializationId of int

/// Why a type position carries no type shape, which is what makes `freeze` total. A reason
/// minted while EXTRACTING a contract carries the text its diagnostic needs, because a later
/// compilation reads that contract back without the source; an in-process one carries none.
[<RequireQualifiedAccess>]
type UnknownReason =
    /// A type name written in a source annotation or an extracted contract, with no definition.
    /// The only case the unifier's `UndefinedTypeNames` suppression applies to.
    | UndefinedName of name: string
    /// An external declaration whose body did not translate; `what` is the extractor's own
    /// phrase for the construct it could not model.
    | UnfreezableExternal of what: string
    /// A metavar the front end never resolved.
    | UnresolvedTypar
    /// An extraction-time placeholder for a body that may forward-reference a type registered
    /// later in the same package; filled before that pass ends.
    | Deferred
    /// A type argument index past the instantiation it was applied to.
    | ArityMismatch
    /// A construct with no first-class value: `1..10`, or a literal whose suffix F# reserves.
    /// Already diagnosed at elaboration.
    | NoValueType

    /// Display text only. Never an identity: `<deferred>` and `<arity-mismatch>` are not names
    /// anything can be looked up by.
    member this.Render: string =
        match this with
        | UndefinedName name -> name
        | UnfreezableExternal what -> "<unfreezable: " + what + ">"
        | UnresolvedTypar -> "?unresolved-typar"
        | Deferred -> "<deferred>"
        | ArityMismatch -> "<arity-mismatch>"
        | NoValueType -> "<no-value-type>"

/// The constant value a structural LITERAL type carries: `"GET"`, or an `Int` for a numeric
/// literal union. External vocabulary ONLY, so inference never mints one; it subsumes to its
/// base primitive, and the CLR backend encodes it as that base.
[<RequireQualifiedAccess>]
type LiteralConst =
    | String of string
    | Int of int64

    /// The literal SPELLING (`"GET"` quoted, `42` bare), for diagnostics.
    member this.Render: string =
        match this with
        | LiteralConst.String s -> "\"" + s + "\""
        | LiteralConst.Int n -> string n
