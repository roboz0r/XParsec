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

// `EscapeState` and `RegionRepr` are optimisation metadata: `Passes/Regions.fs` computes the
// lifetime and representation axes once, for any backend to read when it picks a lowering. The
// two enums below are per-target readings of the lifetime axis, projected by the converters on
// `module EscapeState`.
//
// UNBUILT: `RegionsTests` is the only consumer of either projection. Today's readers take the
// axes direct — `RefCellPromotion` reads `ctx.Bindings.Escape`, and the CLR closure emitter
// reads the `ClosureRepr` collapse — so nothing asks for a ref-safe tier. A native target must
// place every allocation itself, so `NativeRegionTier` is what a GC-free backend will read; it
// stays because that backend is intended, not written.

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

    /// The widest ref-safe context an escape state permits. UNBUILT — see `SafeContext`.
    let toClrRefSafe (s: EscapeState) : SafeContext =
        match s with
        | LocalStack -> SafeContext.CurrentMethod
        | ReturnOnly -> SafeContext.ReturnOnly
        | CallerStack -> SafeContext.CallingMethod
        | HeapShared -> SafeContext.Heap

    /// The region an escape state allocates into on a native target. `ReturnOnly` and
    /// `CallerStack` share `ReturnSlot`, so the caller's frame owns both. UNBUILT — see
    /// `NativeRegionTier`.
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

/// What a type parameter ranges over: a type, or a unit of measure (`[<Measure>] 'u`).
[<RequireQualifiedAccess>]
type TyparKind =
    | Type
    | Measure

/// A type parameter's name.
[<RequireQualifiedAccess>]
type TyparName =
    /// Source text, leading `'`/`^` included (`'a`).
    | Written of string
    /// The `i`th parameter of a declaration known only by arity: a TypeScript manifest export,
    /// an intrinsic binding, an unresolved reference.
    | Positional of int

    /// The name as source spells it: the written text, or `'T<i>` for a positional parameter.
    member this.Display: string =
        match this with
        | Written s -> s
        | Positional i -> sprintf "'T%d" i

/// The kind of a constraint on a type-kinded parameter.
[<RequireQualifiedAccess>]
type TyparConstraintKindG<'ty> =
    /// `when 'a : equality`.
    | Equality
    /// `when 'a : comparison`.
    | Comparison
    /// `when 'a : struct`.
    | Struct
    /// `when 'a : not struct`.
    | ReferenceType
    /// `when 'a : null`.
    | Nullness
    /// `when 'a : not null`.
    | NotNull
    /// `when 'a :> <ty>`.
    | Coercion of target: 'ty
    /// `when 'a : (new : unit -> 'a)`.
    | DefaultConstructor
    /// `when 'a : unmanaged`.
    | Unmanaged
    /// `when 'a : enum<underlying>`.
    | Enum of underlying: 'ty
    /// `when 'a : delegate<args, ret>`.
    | Delegate of args: 'ty * ret: 'ty

[<RequireQualifiedAccess>]
module TyparConstraintKind =

    let map (f: 'a -> 'b) (kind: TyparConstraintKindG<'a>) : TyparConstraintKindG<'b> =
        match kind with
        | TyparConstraintKindG.Equality -> TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison -> TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Struct -> TyparConstraintKindG.Struct
        | TyparConstraintKindG.ReferenceType -> TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness -> TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull -> TyparConstraintKindG.NotNull
        | TyparConstraintKindG.Coercion target -> TyparConstraintKindG.Coercion(f target)
        | TyparConstraintKindG.DefaultConstructor -> TyparConstraintKindG.DefaultConstructor
        | TyparConstraintKindG.Unmanaged -> TyparConstraintKindG.Unmanaged
        | TyparConstraintKindG.Enum underlying -> TyparConstraintKindG.Enum(f underlying)
        | TyparConstraintKindG.Delegate(args, ret) -> TyparConstraintKindG.Delegate(f args, f ret)

    /// Applies `f` to each embedded type.
    let iter (f: 'a -> unit) (kind: TyparConstraintKindG<'a>) : unit =
        match kind with
        | TyparConstraintKindG.Coercion target
        | TyparConstraintKindG.Enum target -> f target
        | TyparConstraintKindG.Delegate(args, ret) ->
            f args
            f ret
        | TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Struct
        | TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull
        | TyparConstraintKindG.DefaultConstructor
        | TyparConstraintKindG.Unmanaged -> ()

    /// The coercion supertype, for a `Coercion` constraint only.
    let tryCoercion (kind: TyparConstraintKindG<'ty>) : 'ty voption =
        match kind with
        | TyparConstraintKindG.Coercion target -> ValueSome target
        | TyparConstraintKindG.Equality
        | TyparConstraintKindG.Comparison
        | TyparConstraintKindG.Struct
        | TyparConstraintKindG.ReferenceType
        | TyparConstraintKindG.Nullness
        | TyparConstraintKindG.NotNull
        | TyparConstraintKindG.DefaultConstructor
        | TyparConstraintKindG.Unmanaged
        | TyparConstraintKindG.Enum _
        | TyparConstraintKindG.Delegate _ -> ValueNone

/// The constraints on one type-kinded parameter. An embedded type may reference a sibling
/// parameter of the same declaration.
type ConstraintSetG<'ty> =
    {
        /// Declared or inferred, deduplicated structurally, in source order.
        Kinds: EqSet<TyparConstraintKindG<'ty>>
        /// `default ^T : <ty>` targets in source order; generalisation takes the first that
        /// resolves.
        Defaults: EqArray<'ty>
    }

    member this.IsEmpty: bool = this.Kinds.IsEmpty && this.Defaults.IsEmpty

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConstraintSet =

    let empty<'ty> : ConstraintSetG<'ty> =
        {
            Kinds = EqSet.empty
            Defaults = EqArray.empty
        }

    let ofKinds (kinds: seq<TyparConstraintKindG<'ty>>) : ConstraintSetG<'ty> =
        {
            Kinds = EqSet.ofSeq kinds
            Defaults = EqArray.empty
        }

    let map (f: 'a -> 'b) (set: ConstraintSetG<'a>) : ConstraintSetG<'b> =
        {
            Kinds = EqSet.map (TyparConstraintKind.map f) set.Kinds
            Defaults = EqArray.map f set.Defaults
        }

    /// Applies `f` to each embedded type.
    let iter (f: 'a -> unit) (set: ConstraintSetG<'a>) : unit =
        for c in set.Kinds do
            TyparConstraintKind.iter f c

        EqArray.iter f set.Defaults

/// A type-kinded parameter with its constraints.
type TypeTyparG<'ty> =
    {
        Name: TyparName
        Constraints: ConstraintSetG<'ty>
    }

/// A measure-kinded parameter (`[<Measure>] 'u`).
type MeasureTypar = { Name: TyparName }

/// One source-order position of a `TyparList`: an index into `Types` or into `Measures`.
[<RequireQualifiedAccess>]
type TyparSlot =
    | Type of int
    | Measure of int

    member this.Kind: TyparKind =
        match this with
        | Type _ -> TyparKind.Type
        | Measure _ -> TyparKind.Measure

/// A declaration's type parameters, type-kinded and measure-kinded apart. A typar leaf
/// indexes `Types`; the CLR encodes `Types` alone.
type TyparListG<'ty> =
    {
        Types: EqArray<TypeTyparG<'ty>>
        Measures: EqArray<MeasureTypar>
        /// Source order, one slot per parameter.
        Order: EqArray<TyparSlot>
    }

    /// The arity: one per parameter of either kind.
    member this.Length: int = this.Order.Length

    member this.IsEmpty: bool = this.Order.IsEmpty

    /// The type-kinded count: the arity a backend that erases measures emits.
    member this.TypeArity: int = this.Types.Length

    member this.HasTypeTypars: bool = not this.Types.IsEmpty

    /// Whether any type-kinded parameter carries a constraint or a default.
    member this.HasConstraints: bool =
        this.Types |> EqArray.exists (fun t -> not t.Constraints.IsEmpty)

    /// Each parameter's display name, in source order.
    member this.Names: EqArray<string> =
        this.Order
        |> EqArray.map (fun slot ->
            match slot with
            | TyparSlot.Type i -> this.Types.[i].Name.Display
            | TyparSlot.Measure i -> this.Measures.[i].Name.Display
        )

/// A type parameter as a declaration binds it. `TyVar` is a prototype; every use site
/// substitutes a fresh variable for it.
type DeclaredTypar =
    {
        /// Source-text name, leading `'`/`^` included (`'a`).
        Name: string
        TyVar: TyVarId
        Kind: TyparKind
    }

[<RequireQualifiedAccess>]
module DeclaredTypar =

    /// The prototype variables, in declaration order.
    let protos (typars: EqArray<DeclaredTypar>) : EqArray<TyVarId> =
        typars |> EqArray.map (fun t -> t.TyVar)

    let names (typars: EqArray<DeclaredTypar>) : EqArray<string> = typars |> EqArray.map (fun t -> t.Name)

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TyparList =

    let empty<'ty> : TyparListG<'ty> =
        {
            Types = EqArray.empty
            Measures = EqArray.empty
            Order = EqArray.empty
        }

    /// Each parameter's kind, in source order.
    let kinds (typars: TyparListG<'ty>) : EqArray<TyparKind> =
        typars.Order |> EqArray.map (fun s -> s.Kind)

    /// The display names of the type-kinded parameters, by `Types` index.
    let typeNames (typars: TyparListG<'ty>) : EqArray<string> =
        typars.Types |> EqArray.map (fun t -> t.Name.Display)

    /// The list over `(name, kind)` pairs in source order, every type-kinded parameter
    /// constrained by `constraintsAt` its source position.
    let ofKinded (constraintsAt: int -> ConstraintSetG<'ty>) (typars: seq<string * TyparKind>) : TyparListG<'ty> =
        let types = ResizeArray<TypeTyparG<'ty>>()
        let measures = ResizeArray<MeasureTypar>()
        let order = ResizeArray<TyparSlot>()

        for (name, kind) in typars do
            match kind with
            | TyparKind.Type ->
                order.Add(TyparSlot.Type types.Count)

                types.Add
                    {
                        Name = TyparName.Written name
                        Constraints = constraintsAt (order.Count - 1)
                    }
            | TyparKind.Measure ->
                order.Add(TyparSlot.Measure measures.Count)

                measures.Add
                    {
                        MeasureTypar.Name = TyparName.Written name
                    }

        {
            Types = EqArray.ofSeq types
            Measures = EqArray.ofSeq measures
            Order = EqArray.ofSeq order
        }

    /// The list over written, unconstrained parameters in source order.
    let ofSeq (typars: seq<string * TyparKind>) : TyparListG<'ty> =
        ofKinded (fun _ -> ConstraintSet.empty) typars

    /// The list over a declaration's typars, each type-kinded one constrained by
    /// `constraintsOf` its declaration.
    let ofDeclared
        (constraintsOf: DeclaredTypar -> ConstraintSetG<'ty>)
        (ts: EqArray<DeclaredTypar>)
        : TyparListG<'ty> =
        ofKinded (fun i -> constraintsOf ts.[i]) (seq { for t in ts -> t.Name, t.Kind })

    /// The list over a declaration's typars, unconstrained.
    let unconstrained (ts: EqArray<DeclaredTypar>) : TyparListG<'ty> =
        ofDeclared (fun _ -> ConstraintSet.empty) ts

    /// Type-kinded parameters under the written names, unconstrained.
    let typeOnly (names: seq<string>) : TyparListG<'ty> =
        ofSeq (seq { for n in names -> n, TyparKind.Type })

    /// `n` type-kinded parameters, positionally named, each constrained by `constraintsAt`
    /// its index.
    let positionalWith (constraintsAt: int -> ConstraintSetG<'ty>) (n: int) : TyparListG<'ty> =
        match n with
        | 0 -> empty
        | n ->
            {
                Types =
                    EqArray.init
                        n
                        (fun i ->
                            {
                                Name = TyparName.Positional i
                                Constraints = constraintsAt i
                            }
                        )
                Measures = EqArray.empty
                Order = EqArray.init n TyparSlot.Type
            }

    /// `n` type-kinded parameters, positionally named and unconstrained.
    let positional (n: int) : TyparListG<'ty> =
        positionalWith (fun _ -> ConstraintSet.empty) n

    let map (f: 'a -> 'b) (typars: TyparListG<'a>) : TyparListG<'b> =
        {
            Types =
                typars.Types
                |> EqArray.map (fun t ->
                    {
                        Name = t.Name
                        Constraints = ConstraintSet.map f t.Constraints
                    }
                )
            Measures = typars.Measures
            Order = typars.Order
        }

    /// Applies `f` to each type embedded in a constraint.
    let iter (f: 'a -> unit) (typars: TyparListG<'a>) : unit =
        for t in typars.Types do
            ConstraintSet.iter f t.Constraints

    /// Every `Coercion` supertype, paired with the `Types` index of the typar it constrains.
    let coercions (typars: TyparListG<'ty>) : (int * 'ty) list =
        [
            for i in 0 .. typars.Types.Length - 1 do
                for kind in typars.Types.[i].Constraints.Kinds do
                    match TyparConstraintKind.tryCoercion kind with
                    | ValueSome target -> i, target
                    | ValueNone -> ()
        ]

/// A generalised binding's identity within its file, minted when the binding generalises
/// and dense in generalisation order. Stable across re-generalisation of the same binding.
[<Struct>]
type LocalBindingId = | LocalBindingId of int

/// The declaration a type parameter belongs to. A `TyTypar` / `FTTypar` leaf carries its
/// scope and its index into the scope's `TyparList.Types`.
[<RequireQualifiedAccess>]
type TyparScope =
    /// A type declaration's own typars: `!i` in CLI metadata.
    | Type of TypeKey
    /// A member's own typars, nested in the owner's `Type` scope: `!!j` in CLI metadata.
    | Member of owner: TypeKey
    /// A module-level `let`'s own typars, a top-level `let` of the implicit program module
    /// included: `!!j` on the static method it compiles to.
    | ModuleFunction of BindingKey
    /// A generalised body-local `let`'s own typars: `!!j` on the generic method the local is
    /// lifted to, after every enclosing scope's.
    | LocalFunction of LocalBindingId

    /// A member's or a module function's scope: the typars a call site instantiates.
    member this.IsFunction: bool =
        match this with
        | Member _
        | ModuleFunction _ -> true
        | Type _
        | LocalFunction _ -> false

    member this.IsLocal: bool =
        match this with
        | LocalFunction _ -> true
        | Type _
        | Member _
        | ModuleFunction _ -> false

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
