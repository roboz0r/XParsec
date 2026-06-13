namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

// See docs/typevar.md for the 3-axis design.

// --- Symbol identity ---------------------------------------------------------
//
// `SymbolOrigin` / `SymbolKey` / `MemberKind` live here (ahead of `SemType`)
// because the nominal `SemType` cases (`TyUnion` / `TyRecord` / `TyClass`) now
// carry a `SymbolKey` as their identity. They
// are pure string/EqArray records with no `SemType` dependency, so the ordering
// is one-way: identity first, then the type graph that references it.
// `ExternalSymbols.fs` (which mints/decomposes these) compiles after this file.

/// Where a resolved symbol physically lives — enough for codegen to mint a ref
/// without re-resolving. `Assembly` is a simple name keyed into a `ProjectInfo`'s
/// resolved reference set; `None` ⇒ defined in the project being compiled.
type SymbolOrigin =
    {
        Assembly: string option
        Namespace: string
        DeclaringType: string option
    }

    /// The default carried by symbols that don't (yet) record an origin —
    /// project-local, root namespace, no declaring type. P0 stamps this
    /// everywhere; later phases fill it from the resolving source.
    static member Empty =
        {
            Assembly = None
            Namespace = ""
            DeclaringType = None
        }

    /// Strip the namespace prefix off a fully-qualified compiled name to
    /// produce the simple type/value name the metadata layer keys on. Given
    /// the symbol's declared namespace `ns` (from manifest / `Type.Namespace`)
    /// and its full compiled name (`Foo.Bar.Baz`), returns `Baz` when
    /// `Foo.Bar` is the recorded namespace and the prefix matches; otherwise
    /// returns `fullName` unchanged. Treats `null`/empty `ns` as "no prefix
    /// to strip" — the metadata `declTypeKey` path passes a possibly-null
    /// `Type.Namespace`, so this hides the null check at the seam. Used by
    /// codegen `TypeRef` minting (`externalClassRef` / `externalRecordRef`)
    /// and metadata `SymbolKey.TypeKey` decomposition (`declTypeKey`) —
    /// vesper-set-sprint-plan §0.4 / M5. Phase 1's user-class emit needs
    /// the same split for its `TypeDefinition` row construction.
    static member StripNamespace (ns: string) (fullName: string) : string =
        if not (System.String.IsNullOrEmpty ns) && fullName.StartsWith(ns + ".") then
            fullName.Substring(ns.Length + 1)
        else
            fullName

/// Platform-agnostic, scope-unambiguous symbol identity. All strings/ints —
/// never a CLR `EntityHandle` or `System.Type` (those
/// are per-context and target-specific). The discriminator is the *origin*
/// (assembly, namespace[, declaring type]), not the bare name, so a project-local
/// `List` and `System.Collections.Generic.List`1` get different keys by
/// construction. Keyed on the open generic *definition* (`name` includes the
/// `` `arity `` suffix); instantiation is the cheap per-use substitution.
[<RequireQualifiedAccess>]
type SymbolKey =
    /// A type definition. `name` includes the arity suffix (`` IEnumerable`1 ``).
    | TypeKey of asm: string option * ns: string * name: string
    /// A value (module-level binding / operator).
    | ValueKey of asm: string option * ns: string * name: string
    /// A member on a type. `argSig` is written in the declaring type's OPEN
    /// typars (`!0`, …) and disambiguates overloads (`GetHashCode()` vs
    /// `GetHashCode(!0)`). `EqArray` (not `list` / not `string[]`) so the
    /// containing `SymbolKey` keeps the structural `=` interning relies on.
    /// `kind` distinguishes a plain method from a property or an
    /// interface-method / explicit interface implementation (the latter two
    /// carry the interface's own `SymbolKey` so codegen can write the matching
    /// `.override` row — pre-sprint-recommendations H3).
    ///
    /// TODO (method overloading): `argSig` is currently a *lossy* string rendering
    /// — it only disambiguates overloads and is **never re-parsed** (see
    /// `MetadataSymbols.openTyparSig`), and project-local `MemberKey`s are minted
    /// with placeholder contents (`LocalSymbolKey.ofMember` fills it with empty
    /// strings) because locals have no overload set yet — only its *length* (the
    /// member's value-parameter arity) carries meaning, read by codegen's external
    /// member-ref param-flatten. Resolving real overloads — externals *by argument-type
    /// betterness*, and local overloaded members at all — needs argument-type
    /// *identity*, not a display string. A per-arg `EqArray<SymbolKey>` is the
    /// natural candidate (it would carry the full nominal identity of each
    /// parameter type instead of a name), **but** is insufficient on its own: a
    /// parameter can be a typar (`!0`), a function/tuple/array, or a constructed
    /// generic — shapes a single `SymbolKey` can't express. The genuinely complete
    /// representation is `SemType`, which can't be embedded here directly (`SemType`
    /// already references `SymbolKey` — that would be a definitional cycle). So the
    /// eventual shape is an open design question (a structural arg-type encoding, a
    /// `SemType`-keyed side table, or breaking the cycle), not a drop-in field swap.
    | MemberKey of decl: SymbolKey * memberName: string * argSig: EqArray<string> * kind: MemberKind

/// What kind of member a `SymbolKey.MemberKey` denotes (pre-sprint-recommendations
/// H3). `Method` and `Property` are the today-resolvable shapes; `InterfaceMethod`
/// and `ExplicitInterfaceImpl` land their consumers with B-2 (interface conformance
/// + `(this :> iface).M()` syntax) — until then both are unused, but the field
/// is wide enough to carry the interface's `SymbolKey` so B-2 doesn't have to
/// re-shape the key.
and [<RequireQualifiedAccess>] MemberKind =
    | Method
    | Property
    /// An abstract method on an interface; `iface` is the declaring interface's
    /// `SymbolKey.TypeKey`. Distinct from `Method` so a call site can resolve
    /// the right vtable slot when several interfaces inherit a like-named
    /// method (`IEnumerable<'T>::GetEnumerator()` vs
    /// `IEnumerable::GetEnumerator()`).
    | InterfaceMethod of iface: SymbolKey
    /// An explicit interface implementation on a class (B-2):
    /// `Set<'T>::System.Collections.IEnumerable.GetEnumerator`. `iface` pins
    /// which interface's slot is being overridden, the token codegen needs to
    /// emit the `.override` row.
    | ExplicitInterfaceImpl of iface: SymbolKey

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

/// Maps to Phase 4.6 target lowering: LocalStack -> `ref struct` (.NET) / `&T`
/// (Rust); HeapShared -> `Rc<T>` / `Arc<T>` (Rust).
type EscapeState =
    | LocalStack
    | CallerStack
    | HeapShared

/// Per-type decision on whether the structural-equality triple
/// (`GetHashCode()` / `Equals(object)` / `IEquatable<Self>::Equals(Self)`) ships
/// on a record / union. Driven by C-Attr (`Passes/Attributes.fs`) off the
/// type's `[<StructuralEquality>]` / `[<ReferenceEquality>]` / `[<NoEquality>]`
/// declarations and, when no attribute is present, the default rule
/// (brainstorm-structural-equality §8): an all-immutable
/// record or any union ⇒ `Structural`; a record with any mutable field ⇒
/// `Reference`. An interface ignores it (no triple is ever synthesised).
[<RequireQualifiedAccess>]
type EqualityVerdict =
    /// Emit the structural-equality triple + the `IEquatable<Self>`
    /// `InterfaceImpl`. Default for a union and an all-immutable record.
    | Structural
    /// Emit no triple; `Object.Equals` / `Object.GetHashCode` (reference
    /// identity) suffice. Default for a record with any mutable field; also
    /// the `[<ReferenceEquality>]`-attributed case.
    | Reference
    /// Emit no triple AND mark the type as forbidding equality; a `=` /
    /// `<>` use site against this type is a diagnostic (driven through the
    /// `Equality` typar-constraint check in `Unification`).
    | NoEquality

/// Per-type decision on whether the structural-comparison pair
/// (`int CompareTo(Self)` / `int CompareTo(object)` + `IComparable<Self>` /
/// `IComparable` `InterfaceImpl`s) ships on a record / union. Driven by C-Attr
/// (`Passes/Attributes.fs`) off the type's `[<StructuralComparison>]` /
/// `[<NoComparison>]` declarations. Per brainstorm-comparison §9 the default
/// is **opt-in**: an unannotated record / union is `NoComparison`, so
/// ordering use sites (`r1 < r2`) are rejected unless the author writes
/// `[<StructuralComparison>]`. See
/// [`docs/brainstorm-comparison.md`](docs/brainstorm-comparison.md) §9.
[<RequireQualifiedAccess>]
type ComparisonVerdict =
    /// Emit the structural-comparison pair + the `IComparable<Self>` /
    /// `IComparable` `InterfaceImpl`s. Requires an explicit
    /// `[<StructuralComparison>]` attribute on the type.
    | Structural
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

/// The immutable, *elaborated* type representation — the codomain of `freeze`
/// and the type the TAST carries into Codegen, distinct from the mutable
/// inference `SemType`. Its defining property is the **absence of a `TyVar`
/// case**: a `FrozenType` never holds a union-find unification variable, so a
/// metavar reaching the backend is unrepresentable rather than a convention to
/// assert against. Open type parameters — a generic definition's own typars in
/// their uninstantiated form — are the explicit, self-describing `FTTypar` node
/// (carrying its axis + index), replacing the marker-`TypeVar` mechanism codegen
/// used to fake them. Structural equality is value-based (no `TypeVar` leaf), so
/// a `FrozenType` is a sound dictionary key — this is what lets it serve as the
/// overload-identity key that retires the lossy `SymbolKey.MemberKey.argSig`
/// string. Constructors mirror `SemType`'s shape under an `FT` prefix to avoid
/// ambiguity when both types are in scope.
///
/// NOTE (naming): `FrozenType` / `FT*` are
/// provisional; revisit before the representation is widely consumed.
type FrozenType =
    /// A nominal constant in two roles (the `SemType.TyConst` declaring-typar
    /// marker role moves to `FTTypar`): an argless primitive / intrinsic
    /// (`FTConst("int", [])`) and a generic intrinsic forwarding its args
    /// (`'T[]` ≡ `FTConst("[]", [elem])`).
    | FTConst of name: string * args: EqArray<FrozenType>
    /// Curried; multi-arg functions nest `FTFun`.
    | FTFun of arg: FrozenType * result: FrozenType
    /// Flat n-ary tuple — mirrors `SemType.TyTuple`.
    | FTTuple of items: EqArray<FrozenType>
    | FTRecord of key: SymbolKey * args: EqArray<FrozenType>
    | FTUnion of key: SymbolKey * args: EqArray<FrozenType>
    | FTClass of key: SymbolKey * args: EqArray<FrozenType>
    /// An open type parameter of the enclosing generic definition: `axis`
    /// selects the declaring-type vs method axis; `index` is its position in
    /// that axis's typar list — the order `freeze` quantifies in, which is the
    /// single index-minting point.
    | FTTypar of axis: TyparAxis * index: int
    /// Mirror of `SemType.TyUnknown`: a nominal head that resolved to no type
    /// shape. Carried so `freeze` is total; whether it may legitimately reach
    /// the backend is an open question (likely a hard error).
    | FTUnknown of name: string

/// Mutually recursive with TypeVar — every TyVar is a pointer into the
/// union-find graph. Will grow to include generics, units.
type SemType =
    /// Call UnionFind.find then read the representative's Link to dereference.
    | TyVar of TypeVar
    /// A nominal constant spanning three roles: (a) an argless primitive /
    /// intrinsic binding (`TyConst("int", [])`), (b) a declaring-type typar
    /// marker the backend's typar encoder consumes (`TyConst("'A", [])` — always
    /// argless), and (c) a *generic intrinsic* that forwards its type arguments
    /// (`'T[]` ≡ `TyConst("[]", [elem])` — the array repr `!0[]` is a
    /// backend-specific encoding, the args are backend-agnostic structure). So `args ≠ []` does NOT
    /// imply a registry nominal — arrays are the only generic intrinsic in v1.
    /// Args participate in unification (same arity rule as `TyRecord`).
    | TyConst of name: string * args: EqArray<SemType>
    /// Curried; multi-arg functions nest TyFun.
    | TyFun of arg: SemType * result: SemType
    /// Flat n-ary tuple. Unifies pairwise with same-arity TyTuple; arity
    /// mismatch is a diagnostic in Unification.
    | TyTuple of items: EqArray<SemType>
    /// Field types are not stored inline — look up the record's shape via its
    /// `key` (and the declared `TypeParams` used to substitute `args` into each
    /// field). Two TyRecords unify iff their `key`s are equal AND their args
    /// unify pairwise. Identity is the resolved `SymbolKey` (minted once in
    /// NameResolution / Translate), not a bare string. The `key`'s `ns` distinguishes same-named records in different
    /// namespaces; its `name` carries the arity suffix.
    | TyRecord of key: SymbolKey * args: EqArray<SemType>
    /// Same shape as TyRecord. Cases / TypeParams live in the union registry,
    /// reachable by `key` (`TypeRegistry.tryUnionByKey`).
    | TyUnion of key: SymbolKey * args: EqArray<SemType>
    /// Same shape as `TyRecord` / `TyUnion`; member lookup is a side-channel on
    /// the class registry. Two `TyClass` unify iff their `key`s are equal AND
    /// their args unify pairwise.
    | TyClass of key: SymbolKey * args: EqArray<SemType>
    /// A nominal reference that resolved to no in-scope type shape during extraction.
    /// It never unifies with anything; Unification reports it at the use site and
    /// recovers, so one broken contract head doesn't cascade. Distinct from
    /// `TyConst` (a known intrinsic/primitive) and from a fresh `TyVar` (an
    /// inference hole). Must never reach the backend — `ClrEncoder` treats it as
    /// an internal error.
    | TyUnknown of name: string
    /// An elaborated open type parameter — the `SemType` counterpart of
    /// `FrozenType.FTTypar` (the same axis + index). It is the canonical
    /// representation of an open typar on the post-freeze `SemType` subset:
    /// `freeze` rewrites every surviving `TyVar` to one, so afterwards no `TyVar`
    /// remains in any TAST `.ty` field — every open typar is a `TyTypar`, and a
    /// `TyVar` reaching Codegen is a bug. It replaces the old declaring-typar
    /// `TyConst "'A"` markers and the leftover-`TyVar` static-fn typars. It also
    /// rides the inference-side template helpers that work in `SemType` but must
    /// name an open typar (`ofFrozen`, `ExternalSymbols.openSignature`).
    ///
    /// **Invariant: never produced during inference.** Unification / generalisation
    /// never see it (they run before `freeze`); their match arms treat it as
    /// impossible (`failwith`) — a free invariant check. Only `freeze` mints it
    /// (the single index-minting point, Edge A) and only Codegen + post-freeze
    /// walks read it.
    | TyTypar of axis: TyparAxis * index: int

/// Abelian-group expression over named unit atoms. Always stored in a
/// normalised form: each exponent is in canonical Rational form, zero
/// exponents are dropped, and entries are sorted by unit name. Equality
/// is structural list equality after normalise. `Empty` is the group
/// identity (dimensionless).
and [<Sealed>] MeasureTerm private (exponents: (string * Rational) list) =
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    static member Empty = MeasureTerm([])

    /// Normalises a raw list: duplicate units are merged (exponents summed),
    /// zero exponents dropped, result sorted by unit name.
    static member ofList(raw: (string * Rational) list) : MeasureTerm =
        raw
        |> List.groupBy fst
        |> List.map (fun (n, xs) -> n, xs |> List.fold (fun acc (_, r) -> acc + r) Rational.Zero)
        |> List.filter (fun (_, e) -> not e.IsZero)
        |> List.sortBy fst
        |> fun normalised -> MeasureTerm(normalised)

    override this.Equals(other) =
        match other with
        | :? MeasureTerm as other -> this.Exponents = other.Exponents
        | _ -> false

    override this.GetHashCode() = hash exponents

    override this.ToString() =
        if List.isEmpty exponents then
            "1"
        else
            // Format like F#: positive exponents in numerator, negative in
            // denominator: `<m s^-1>` renders as `m/s`, `<m s>` as `m s`.
            let positives = exponents |> List.filter (fun (_, e) -> e > Rational.Zero)

            let negatives =
                exponents
                |> List.filter (fun (_, e) -> e < Rational.Zero)
                |> List.map (fun (n, e) -> n, -e)

            let renderEntry (n, e: Rational) =
                if e.IsOne then n else sprintf "%s^%O" n e

            let sb = System.Text.StringBuilder()

            let renderList xs =
                xs |> List.map renderEntry |> String.concat " "

            match positives, negatives with
            | [], ns -> sb.Append("1/").Append(renderList ns) |> ignore
            | ps, [] -> sb.Append(renderList ps) |> ignore
            | ps, ns -> sb.Append(renderList ps).Append("/").Append(renderList ns) |> ignore

            sb.ToString()

/// Captured SRTP member-trait clause attached to a `TypeVar`'s
/// `SrtpBounds`. `MemberName` is the compiled name (`"op_Addition"`,
/// `"Zero"`); `ArgTypes` / `ReturnType` are the trait's expected member
/// signature, instantiated against the fresh TyVars allocated for the
/// containing val's typar list. `Unification.drainSrtpBounds` fires when
/// any participating TyVar's `Link` is set and dispatches against either
/// a built-in primitive table (for `TyConst "int"` etc.) or the candidate
/// type's `ClassTypes` entry (for `TyClass`).
and MemberSignature =
    {
        MemberName: string
        ArgTypes: EqArray<SemType>
        ReturnType: SemType
        /// Shared across every stamp of the *same* trait (one per
        /// participating typar) by reference identity: all participating
        /// TyVars' `SrtpBounds` lists hold the same record instance.
        /// First successful dispatch flips this so other typars' drain
        /// paths no-op when their `Link` is later set.
        mutable Resolved: bool
    }

/// Type-parameter constraint attached to a `TypeVar`. Built from
/// `Constraint<'T>` CST nodes by `Unification.translateConstraints` and
/// drained by `Unification.unify` when the TyVar is linked to a concrete
/// shape. See `docs/constraints-plan.md`. v1 covers the trait-table
/// subset (`equality`, `comparison`, `struct`, `not struct`, `: null`,
/// `: not null`) plus `Coercion` (`:> T` subtype bounds, checked via
/// `subsumes`); `MemberTrait`, `DefaultConstructor`, `Enum`, `Unmanaged`,
/// `Delegate`, and `Default` are deferred.
and [<RequireQualifiedAccess>] SemanticConstraintKind =
    | Equality
    | Comparison
    | Struct
    | ReferenceType
    | Nullness
    | NotNull
    /// `when 'e :> exn` — `target` is the required supertype, resolved to a
    /// `SemType` at the point the typar's fresh TyVar is minted (local binding:
    /// `translateConstraint`; external symbol: `Instantiate`). Checked by
    /// `checkConstraint` via the read-only `subsumes` relation. The `exn ≡
    /// System.Exception` identity it leans on comes from `IntrinsicReprTypes`
    /// (prim-types-exn.fs), not the unifier.
    | Coercion of target: SemType

and [<Struct>] SemanticConstraint =
    {
        Kind: SemanticConstraintKind
        /// Source location of the `when 'a : ...` clause that introduced
        /// the constraint. Used by the constraint-violation diagnostic so
        /// the message can point back at the declaration site, not just
        /// the unification call site.
        DeclKey: NodeKey
    }

/// One element of a TypeVar's `PendingDotAccess` list. `MemberName` is the
/// field-or-member name in `receiver.X`; `UseKey` is the access expression's
/// NodeKey (used for diagnostics); `ResultTv` is the access expression's own
/// TyVar — unified with the field/member's declared type when the receiver
/// resolves.
and [<NoEquality; NoComparison>] DeferredMemberAccess =
    {
        MemberName: string
        UseKey: NodeKey
        ResultTv: TypeVar
    }

and [<Sealed>] TypeVar() =
    /// Authoritative only on the representative — call UnionFind.find first.
    member val Link: SemType voption = ValueNone with get, set
    /// Measure constraint on this variable, when known to be a numeric
    /// type. Authoritative on the union-find root — call `UnionFind.find`
    /// before reading. `union` merges measures via abelian-group equality;
    /// a mismatch on union is a diagnostic. Most TypeVars never get a
    /// measure (function types, tuples, non-numeric values) and stay
    /// `ValueNone`. `ValueSome MeasureTerm.Empty` means "dimensionless
    /// numeric"; `ValueSome <non-empty>` means measured.
    member val Units: MeasureTerm voption = ValueNone with get, set
    member val Region: RegionId = RegionId.Unknown with get, set
    /// Type-parameter constraints attached to this TyVar at declaration
    /// or use sites. Drained by `Unification.unify` when `Link` is set
    /// (on-unified callback); merged on union-find via `migrateBounds`.
    /// Empty for the overwhelming majority of TyVars.
    member val Constraints: SemanticConstraint list = [] with get, set
    /// Fires when Link is set (on-unified callback in Unification).
    member val SrtpBounds: MemberSignature list = [] with get, set
    // Owned by UnionFind; do not mutate directly.
    member val Parent: TypeVar voption = ValueNone with get, set
    member val Rank: int = 0 with get, set
    /// Let-depth at which this TyVar was minted (Rémy's levels). Lowered by
    /// `unify` when this TyVar becomes reachable from a shallower scope.
    /// `generalise` quantifies TyVars whose level strictly exceeds the
    /// enclosing scope's level. Authoritative on the union-find root — call
    /// UnionFind.find before reading. `union` propagates `min` of the two
    /// roots' levels to the survivor.
    member val Level: int = 0 with get, set
    /// Pending dot-access constraints accumulated while this TyVar was
    /// free. Drained by `unify` when the TyVar's `Link` becomes a
    /// `TyRecord _`, `TyClass _`, or another shape that supports dotted
    /// dispatch. The drain code branches on the link-target shape to
    /// resolve against record fields vs class members. Authoritative on
    /// the union-find root.
    member val PendingDotAccess: DeferredMemberAccess list = [] with get, set
    /// Default-constraint chain for this TyVar (Phase 5b). Built from
    /// `ExternalConstraint.Default` clauses captured on external symbols
    /// (notably `(+)`, `(-)` etc.): `default ^T3 : ^T1` records `TyVar t1`
    /// here, `default ^T1 : int` records `TyConst "int"`. Order matches
    /// the source clause order; generalisation walks the list, chasing
    /// each target through union-find, and links the TyVar to the first
    /// concrete shape it reaches. Migrated on union-find via
    /// `migrateBounds`. Empty for the overwhelming majority of TyVars.
    member val Defaults: SemType list = [] with get, set

module MeasureTerm =
    let empty = MeasureTerm.Empty
    let isDimensionless (m: MeasureTerm) = m.IsDimensionless

    let mul (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm =
        MeasureTerm.ofList (a.Exponents @ b.Exponents)

    let inv (m: MeasureTerm) : MeasureTerm =
        m.Exponents |> List.map (fun (n, e) -> n, -e) |> MeasureTerm.ofList

    let div (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm = mul a (inv b)

    /// `k` is `Rational` so `pow m (Rational.create (bigint 1, bigint 2))`
    /// (square root) is expressible once a callsite produces one. Surface
    /// syntax only ever passes integer `k` today.
    let pow (m: MeasureTerm) (k: Rational) : MeasureTerm =
        if k.IsZero then
            empty
        else
            m.Exponents |> List.map (fun (n, e) -> n, e * k) |> MeasureTerm.ofList

/// The `SemType` ↔ `FrozenType` bridge. `toFrozen` is
/// the Edge-A sink-side conversion; `ofFrozen` its inverse. On the *post-freeze*
/// `SemType` subset (`{TyConst, TyFun, TyTuple, TyRecord, TyUnion, TyClass,
/// TyTypar, TyUnknown}`) the two are mutual inverses — the cases are 1:1 with
/// `{FTConst, FTFun, FTTuple, FTRecord, FTUnion, FTClass, FTTypar, FTUnknown}`.
/// `TyVar` is the sole case with no `FrozenType` counterpart (the point of the
/// split): `toFrozen` rejects it with a hard error mirroring `ClrEncoder`'s
/// existing `cannot encode SemType: TyVar` crash, so a stray metavar fails here
/// — one hop out from where the catch-all failed before. AutoOpen so 3B-2's
/// boundary callers can wrap a `.ty` in `toFrozen` unqualified.
[<AutoOpen>]
module FrozenTypeBridge =
    /// `SemType -> FrozenType`. Total on the post-freeze subset; a hard error on
    /// `TyVar` (an inference metavar must never reach the frozen boundary).
    let rec toFrozen (ty: SemType) : FrozenType =
        match ty with
        | TyConst(name, args) -> FTConst(name, EqArray.map toFrozen args)
        | TyFun(arg, result) -> FTFun(toFrozen arg, toFrozen result)
        | TyTuple items -> FTTuple(EqArray.map toFrozen items)
        | TyRecord(key, args) -> FTRecord(key, EqArray.map toFrozen args)
        | TyUnion(key, args) -> FTUnion(key, EqArray.map toFrozen args)
        | TyClass(key, args) -> FTClass(key, EqArray.map toFrozen args)
        | TyTypar(axis, index) -> FTTypar(axis, index)
        | TyUnknown name -> FTUnknown name
        | TyVar _ -> failwithf "FrozenType.toFrozen: cannot freeze SemType: %A" ty

    /// Realise a `FrozenType` template, resolving its open typars via the two
    /// supplied callbacks: `declaring i` yields the declaring type's i-th arg;
    /// `methodVar j` yields the method axis's j-th instantiation. Every other
    /// case maps structurally. Callers that span more than one template of the
    /// *same* signature (a split parameter/return `ExternalSignature`) must share
    /// one `methodVar` memo so a repeated method index resolves to the same var
    /// across the whole signature. `ofFrozen` is the identity case (both
    /// placeholders map straight back to their `TyTypar` markers).
    let rec instantiateWith (declaring: int -> SemType) (methodVar: int -> SemType) (template: FrozenType) : SemType =
        let go = instantiateWith declaring methodVar

        match template with
        | FTConst(name, args) -> TyConst(name, EqArray.map go args)
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(EqArray.map go items)
        | FTRecord(key, args) -> TyRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> TyUnion(key, EqArray.map go args)
        | FTClass(key, args) -> TyClass(key, EqArray.map go args)
        | FTTypar(TyparAxis.Declaring, i) -> declaring i
        | FTTypar(TyparAxis.Method, j) -> methodVar j
        | FTUnknown name -> TyUnknown name

    /// `FrozenType -> SemType`. Total — every `FrozenType` case has a `SemType`
    /// counterpart (`FTTypar` lands on the post-freeze-only `TyTypar`). The
    /// identity realisation of `instantiateWith`: each placeholder maps straight
    /// back to its self-describing `TyTypar` marker.
    let ofFrozen (ft: FrozenType) : SemType =
        instantiateWith (fun i -> TyTypar(TyparAxis.Declaring, i)) (fun j -> TyTypar(TyparAxis.Method, j)) ft

    // --- Template freshening ----------------
    //
    // A `FrozenType` template is an external descriptor's body with its open
    // typars baked as `FTTypar(Declaring,i)` / `FTTypar(Method,j)` placeholders.
    // The realiser family below resolves declaring placeholders to the caller's
    // fresh declaring args and method placeholders to fresh metavars, all over
    // the shared `instantiateWith` walk. It is the data form of the legacy
    // `SemType[] -> SemType` closures (`BuildSignature` / `BuildType` / …):
    // inference reads templates here, codegen reads them directly. Constraint
    // stamping is NOT part of this — it stays in `ExternalSymbol.Instantiate`,
    // applied *after* freshening (the type-shape half carries no constraints).

    /// The placeholder a contract-layer descriptor carries between extraction and
    /// the `ExtractCtx.toProvider` finalize pass.
    /// A body's `FrozenType` can't be built at extraction time — it may forward-
    /// reference a type registered later in the same package — so the shape holds
    /// this until `VesperLib.finalizeDeferred` translates the stashed CST and
    /// overwrites it. Never observed by a consumer.
    let deferredTemplate: FrozenType = FTUnknown "<deferred>"

    /// The standard method-typar freshener: a fresh `TyVar` at `level` per
    /// distinct index, memoised in `cache` so repeated occurrences of the same
    /// method index share one var. Mirrors `Infer.instantiateMethodTypars`.
    let methodFreshener (cache: System.Collections.Generic.Dictionary<int, SemType>) (level: int) (j: int) : SemType =
        match cache.TryGetValue j with
        | true, v -> v
        | _ ->
            let tv = TypeVar()
            tv.Level <- level
            let v = TyVar tv
            cache.[j] <- v
            v

    /// Realise a *declaring-only* template (a type-shape descriptor — a record
    /// field, union-case field, interface arg, base type, or abbreviation body):
    /// `FTTypar(Declaring,i) → declaringArgs.[i]`. These descriptors carry no
    /// method axis (only members do), so a `FTTypar(Method,_)` here is a producer
    /// bug — it fails loud rather than fabricating a var. An out-of-range declaring
    /// index degrades to `TyUnknown` rather than crashing — the `SemType`
    /// counterpart of `substituteDeclaring`'s arity-mismatch arm — so a template
    /// that names more typars than the use site supplies (an under-applied generic
    /// abbrev, a body referencing an undeclared typar) surfaces as a use-site
    /// diagnostic instead of an `IndexOutOfRange`. Needs no `level`.
    let instantiateDeclaring (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith
            (fun i ->
                if i < declaringArgs.Length then
                    declaringArgs.[i]
                else
                    TyUnknown "<arity-mismatch>"
            )
            (fun j ->
                failwithf
                    "FrozenTypeBridge.instantiateDeclaring: unexpected method typar %d in a type-shape template"
                    j
            )
            template

    /// The largest declaring-typar index a template references, or `-1` if it
    /// references none. `freezeMemberSig` uses this to DROP a member whose
    /// signature names a typar beyond the declaring type's arity
    /// (`maxDeclaringIndex >= declaringArity`): such a member can't be instantiated
    /// from the receiver's declaring args alone, so it's removed rather than
    /// surfaced with an unrealisable slot. This is a policy choice — drop vs.
    /// degrade — not crash-avoidance: both realisers (`instantiateDeclaring`,
    /// `substituteDeclaring`) degrade an out-of-range declaring index to `Unknown`
    /// on their own. A method typar is a producer bug here (type-shape / contract
    /// templates carry no method axis).
    let rec maxDeclaringIndex (template: FrozenType) : int =
        let maxOf (args: EqArray<FrozenType>) =
            let mutable m = -1

            for i in 0 .. args.Length - 1 do
                m <- max m (maxDeclaringIndex args.[i])

            m

        match template with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> maxOf args
        | FTFun(arg, result) -> max (maxDeclaringIndex arg) (maxDeclaringIndex result)
        | FTTuple items -> maxOf items
        | FTTypar(TyparAxis.Declaring, i) -> i
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.maxDeclaringIndex: unexpected method typar %d in a type-shape template" j
        | FTUnknown _ -> -1

    /// `true` when the type is fully ground: no open typar on either axis and no
    /// `FTUnknown` (a leaked inference metavar the front end never resolved). The
    /// `FrozenType` sibling of `Passes.InlineExpansion`'s `SemType` `isGroundType`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        | FTUnknown _ -> false
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> args |> EqArray.forall ftIsGround
        | FTFun(a, b) -> ftIsGround a && ftIsGround b
        | FTTuple items -> items |> EqArray.forall ftIsGround

    /// The `FrozenType → FrozenType` use-site substitution codegen applies to a
    /// type-shape template directly: codegen reads the template and does its own
    /// `FTTypar(Declaring,i) ↦ tyArgs.[i]` substitution — a trivial total walk on
    /// `FrozenType`, touching no `SemType` and no inference state. The frozen
    /// sibling of `instantiateDeclaring`; a method typar is a producer bug
    /// (type-shape templates carry no method axis), so it fails loud.
    ///
    /// Used to expand an abbreviation body against use-site args. Because
    /// `resolveTypeName` deliberately tolerates an arity mismatch (an under-applied
    /// generic abbrev still resolves), a declaring index can land past the provided
    /// args; that leaf degrades to `FTUnknown` rather than crashing — the frozen
    /// counterpart of `translateType`'s unresolved-name → `FTUnknown` arm.
    let rec substituteDeclaring (declaringArgs: FrozenType[]) (template: FrozenType) : FrozenType =
        let go = substituteDeclaring declaringArgs

        match template with
        | FTConst(name, args) -> FTConst(name, EqArray.map go args)
        | FTFun(arg, result) -> FTFun(go arg, go result)
        | FTTuple items -> FTTuple(EqArray.map go items)
        | FTRecord(key, args) -> FTRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> FTUnion(key, EqArray.map go args)
        | FTClass(key, args) -> FTClass(key, EqArray.map go args)
        | FTTypar(TyparAxis.Declaring, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown "<abbrev-arity-mismatch>"
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected method typar %d in a type-shape template" j
        | FTUnknown name -> FTUnknown name

/// `∀ Quantified . Body`. Built by `Unification.generalise` and stored in
/// `PassContext.Bindings.Scheme` keyed by the binding's headPat NodeKey. Each
/// `inferIdent` of a generalised binding instantiates the scheme — mints a
/// fresh TyVar at the current level for every entry in `Quantified` and
/// walks `Body` substituting them, so independent use sites get independent
/// variables. Mirrors `ExternalSymbol.Instantiate` for the finitely many
/// `'a`s that come out of a user-written `let`. Quantified TyVars stay live
/// in the union-find graph; they are simply no longer "free" with respect
/// to the outer scope.
[<Sealed>]
type TypeScheme(quantified: TypeVar list, body: SemType, constraints: (TypeVar * SemanticConstraint) list) =
    new(quantified: TypeVar list, body: SemType) = TypeScheme(quantified, body, [])
    member _.Quantified = quantified
    member _.Body = body
    /// Constraints captured at generalisation time. Each entry pairs the
    /// constraint with the *quantified* TyVar it constrained at that
    /// point; `instantiate` swaps the TyVar through the substitution
    /// before re-stamping. Empty for the overwhelming majority of
    /// schemes — only `let f<'a when 'a : C> ...` populates this list.
    member _.Constraints = constraints

/// One resolved `when ^T : …` constraint of an F# library-only static
/// optimization clause. Lives here (not in `Tast.fs`) because the side table
/// that carries it is declared before `Tast.fs` in the compile order, and the
/// constraint references only `SemType` — the clause *body* (a `TExpr`) is
/// rebuilt by Freeze, not stored. The typar is a `TyVar` over the inline
/// binding's quantified root, so `Inline.inlineExpand`'s typar substitution
/// turns it into the call site's concrete type before the clause is tested.
/// See docs/operators-plan.md (prereq 3).
[<RequireQualifiedAccess>]
type TStaticOptConstraint =
    /// `when ^T : SomeType` — holds when the type substituted for `typar` equals
    /// `required`. The catch-all `when ^T : ^T` is this case with `required`
    /// equal to `typar`, so after substitution both sides are the same concrete
    /// type and it matches unconditionally.
    | TyconEquals of typar: SemType * required: SemType
    /// `when ^T : struct` — holds when the substituted `typar` is a value type.
    | IsStruct of typar: SemType

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
    /// ("op_Addition", "op_Subtraction", "op_PipeRight", …). Unification
    /// looks the name up via the provider and types the application as if
    /// it were a normal function call. Polymorphic operators (`|>`, `>>`)
    /// are resolved this way too — the provider returns a fresh
    /// instantiation of the polymorphic scheme on each lookup.
    | OpName of compiledName: string
    /// On an `Expr.EnclosedBlock(ParenKind.List, …)` /
    /// `Expr.EmptyBlock(ParenKind.List, …)` node — `[1; 2; 3]` or `[]`.
    /// Unification types as `Microsoft.FSharp.Collections.list<'elem>`
    /// (single element-TyVar shared by every item); Freeze projects the
    /// chain into nested `TExpr.UnionCons("Cons", [hd; tl])` /
    /// `UnionCons("Nil", [])` nodes.
    | ListLiteral
    /// On an `Expr.EnclosedBlock(ParenKind.Array, …)` /
    /// `Expr.EmptyBlock(ParenKind.Array, …)` node — `[|1; 2; 3|]` or
    /// `[||]`. Same element-typing rule as `ListLiteral`; Freeze wraps
    /// the lowered list chain in an `Array.ofList` external call so
    /// the same nested `UnionCons` shape feeds both literal forms.
    | ArrayLiteral
    /// On an `Expr.InfixApp(_, ::, _)` node — cons construction `h :: t`. The
    /// `::` operator is not a provider-resolved function (unlike `+`/`|>`); it
    /// builds the list union directly. Unification types `h :: t` as the list
    /// type carrying `h`'s element type (`tail` unified to the same list);
    /// Freeze projects it to `TExpr.UnionCons("Cons", [hd; tl])` against the
    /// resolved list union — the same shape `ListLiteral` lowers to.
    | ConsExpr
