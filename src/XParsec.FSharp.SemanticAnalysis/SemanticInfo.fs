namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

// See docs/typevar.md for the 3-axis design.

/// Sequential ints. Revisit if region analysis ever wants union-find
/// (it shouldn't — regions are inequality, not equality).
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
    /// Named record type. Field types are not stored inline — look up
    /// `ctx.RecordTypes[name]` for the field list. Two TyRecords unify
    /// iff their names match. v1 uses single-segment names; qualified
    /// names land with namespaces.
    | TyRecord of name: string

/// Abelian-group expression over named unit atoms. Always stored in a
/// normalised form: each exponent is in canonical Rational form, zero
/// exponents are dropped, and entries are sorted by unit name. Equality
/// is structural list equality after normalise. `Empty` is the group
/// identity (dimensionless).
and [<Sealed>] MeasureTerm private (exponents: (string * Rational) list) =
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    static member Empty = MeasureTerm([])

    /// Build a `MeasureTerm` from a raw, possibly un-normalised list of
    /// `(unit, exponent)` pairs. Duplicate units are merged (their
    /// exponents summed), zero exponents are dropped, and the result is
    /// sorted by unit name.
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

/// TODO: real shape when SRTPs come online.
and MemberSignature = | MemberSignaturePlaceholder

/// TODO: real shape when IWSAMs come online.
and InterfaceBound = | InterfaceBoundPlaceholder

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
    /// Fires when Link is set (on-unified callback in Unification).
    member val IfaceBounds: InterfaceBound list = [] with get, set
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
    /// Pending field-access constraints accumulated while this TyVar was
    /// free. Drained by `unify` when the TyVar's `Link` becomes a
    /// `TyRecord _`. Tuple shape: (fieldName, useKey, resultTyVar). The
    /// `useKey` is the field-access expression's NodeKey for diagnostics;
    /// `resultTyVar` is the access expression's own TyVar that needs to be
    /// unified with the field's declared type when the receiver resolves.
    /// Authoritative on the union-find root.
    member val PendingFieldAccess: (string * NodeKey * TypeVar) list = [] with get, set

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
type TypeScheme(quantified: TypeVar list, body: SemType) =
    member _.Quantified = quantified
    member _.Body = body

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
