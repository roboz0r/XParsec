namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

// See docs/typevar.md for the 3-axis design.

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
    /// Field types are not stored inline — look up `ctx.RecordTypes[name]` for
    /// the field-shape (and the declared `TypeParams` used to substitute `args`
    /// into each field). Two TyRecords unify iff their names match AND their
    /// args unify pairwise. v1 single-segment names; qualified names land with
    /// namespaces.
    | TyRecord of name: string * args: SemType list
    /// Same shape as TyRecord. Cases / TypeParams live in `ctx.UnionTypes[name]`.
    | TyUnion of name: string * args: SemType list
    /// Same shape as `TyRecord` / `TyUnion`; member lookup is a side-channel on
    /// `ctx.ClassTypes`. Two `TyClass` unify iff their names match AND their
    /// args unify pairwise.
    | TyClass of name: string * args: SemType list

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
        ArgTypes: SemType list
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
/// `: not null`); `Coercion`, `MemberTrait`, `DefaultConstructor`,
/// `Enum`, `Unmanaged`, `Delegate`, and `Default` are deferred.
and [<RequireQualifiedAccess>] SemanticConstraintKind =
    | Equality
    | Comparison
    | Struct
    | ReferenceType
    | Nullness
    | NotNull

and [<Struct>] SemanticConstraint =
    {
        Kind: SemanticConstraintKind
        /// Source location of the `when 'a : ...` clause that introduced
        /// the constraint. Used by the constraint-violation diagnostic so
        /// the message can point back at the declaration site, not just
        /// the unification call site.
        DeclKey: NodeKey
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
    /// dispatch. Tuple shape: (memberName, useKey, resultTyVar). The
    /// `useKey` is the dot-access expression's NodeKey for diagnostics;
    /// `resultTyVar` is the access expression's own TyVar that needs to be
    /// unified with the field/member's declared type when the receiver
    /// resolves. The drain code branches on the link-target shape to
    /// resolve against record fields vs class members. Authoritative on
    /// the union-find root.
    member val PendingDotAccess: (string * NodeKey * TypeVar) list = [] with get, set
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

/// `∀ Quantified . Body`. Built by `Unification.generalise` and stored in
/// `PassContext.Scheme` keyed by the binding's headPat NodeKey. Each
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
/// See docs/core-operators-handoff.md (prereq 3).
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
