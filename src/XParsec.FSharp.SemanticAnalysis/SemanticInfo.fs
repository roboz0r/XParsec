namespace XParsec.FSharp.SemanticAnalysis

/// Abelian-group expression over measure atoms, each the declaring measure's `TypeKey`, so
/// same-named measures in different modules stay distinct. Always stored normalised (duplicates
/// merged, zero exponents dropped, entries sorted), so equality is structural list equality.
[<Sealed>]
type MeasureTerm private (exponents: (TypeKey * Rational) list) =
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    static member Empty = MeasureTerm([])

    static member OfList(raw: (TypeKey * Rational) list) : MeasureTerm =
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
            // `<m s^-1>` renders as `m/s`, `<m s>` as `m s`.
            let positives = exponents |> List.filter (fun (_, e) -> e > Rational.Zero)

            let negatives =
                exponents
                |> List.filter (fun (_, e) -> e < Rational.Zero)
                |> List.map (fun (n, e) -> n, -e)

            let renderEntry (k: TypeKey, e: Rational) =
                if e.IsOne then
                    k.DeclaredPath
                else
                    sprintf "%s^%O" k.DeclaredPath e

            let sb = System.Text.StringBuilder()

            let renderList xs =
                xs |> List.map renderEntry |> String.concat " "

            match positives, negatives with
            | [], ns -> sb.Append("1/").Append(renderList ns) |> ignore
            | ps, [] -> sb.Append(renderList ps) |> ignore
            | ps, ns -> sb.Append(renderList ps).Append("/").Append(renderList ns) |> ignore

            sb.ToString()

/// `ArgSig` is written in the declaring type's OPEN typars (`FTTypar(Type Decl, i)`), never
/// an instantiation: a key minted at a `C<int>` use site equals one from the open declaration.
/// The member's own typars are `FTTypar(Member Decl, j)`.
type MemberKey =
    {
        Decl: TypeKey
        Name: string
        ArgSig: EqArray<FrozenType>
        /// `M<'a>()` vs `M<'a,'b>()` — distinct overloads with identical empty `ArgSig`.
        MethodTyparArity: int
        Kind: MemberKind
    }

and [<RequireQualifiedAccess>] MemberKind =
    | Method
    | Property
    /// `iface` picks the slot when several interfaces declare a like-named method:
    /// `IEnumerable<'T>::GetEnumerator()` vs `IEnumerable::GetEnumerator()`.
    | InterfaceMethod of iface: TypeKey
    /// `Set<'T>::System.Collections.IEnumerable.GetEnumerator` — `iface` is the overridden slot.
    | ExplicitInterfaceImpl of iface: TypeKey

/// Identity is the containment chain (namespace → module* → type → member), so a project-local
/// `List` and `System.Collections.Generic.List` differ by construction. No home assembly.
and [<RequireQualifiedAccess>] SymbolKey =
    | Type of TypeKey
    | Binding of BindingKey
    | Member of MemberKey

/// The elaborated type representation: `SemType` minus `TyVar`, so a metavar reaching the
/// backend is unrepresentable. Cases mirror `SemType`'s under an `FT` prefix.
and FrozenType =
    /// An argless primitive (`FTConst(RuntimeNames.intKey, [])`) or a generic intrinsic
    /// forwarding its args (`'T[]` ≡ `FTConst(RuntimeNames.arrayKey 1, [elem])`).
    | FTConst of key: TypeKey * args: EqArray<FrozenType>
    | FTFun of arg: FrozenType * result: FrozenType
    | FTTuple of items: EqArray<FrozenType>
    | FTRecord of key: TypeKey * args: EqArray<FrozenType>
    | FTUnion of key: TypeKey * args: EqArray<FrozenType>
    | FTClass of key: TypeKey * args: EqArray<FrozenType>
    /// No `args`, because enums are never generic. A distinct nominal, NOT its underlying `int`.
    | FTEnum of key: TypeKey
    /// An anonymous (structural) union: `A | B ≡ B | A`, and `FTOr []` is `never`.
    | FTOr of disjuncts: FTDisjuncts
    /// A structural LITERAL type (`"GET"`, `42`): ground, no children. A plain Vesper literal
    /// does not make one: `"ping"` types as `string`.
    | FTLiteral of value: LiteralConst
    /// `keyof T`, `T[K]`, `C extends E ? T : F` — carried from the manifest as inert nodes with
    /// CHILDREN, which every structural walk must thread, until a call site grounds them.
    | FTKeyOf of ty: FrozenType
    | FTIndexedAccess of objTy: FrozenType * index: FrozenType
    | FTConditional of FTConditionalPayload
    /// An open type parameter: `index` is its position in `scope`'s type-kinded typars, the
    /// order `freeze` quantifies in.
    | FTTypar of scope: TyparScope * index: int
    /// A position that resolved to no type shape, carried so `freeze` is total. `reason`
    /// identifies the producer; only some of them report a diagnostic at the source.
    | FTUnknown of reason: UnknownReason
    /// A measure filling a MEASURE-KINDED type parameter: `float<m>` is
    /// `FTConst(Vesper.float`1, [FTMeasure m])`, the arity-1 claim of the carrier applied to
    /// the term. Argument position only; a measure in type position is FS0704 at the front end.
    | FTMeasure of units: MeasureTerm

    /// A one-disjunct set collapses to the bare disjunct; `MkUnion []` is `never` (bottom).
    static member MkUnion(disjuncts: FrozenType seq) : FrozenType =
        let canonical = FTDisjuncts.OfSeq disjuncts

        if canonical.Disjuncts.Length = 1 then
            canonical.Disjuncts.[0]
        else
            FTOr canonical

/// `Check extends Extends ? WhenTrue : WhenFalse`.
and FTConditionalPayload =
    {
        Check: FrozenType
        Extends: FrozenType
        WhenTrue: FrozenType
        WhenFalse: FrozenType
    }

/// The disjuncts of an `FTOr`: flattened and deduped, so `string | int` = `int | string`.
and [<Sealed>] FTDisjuncts private (disjuncts: EqSet<FrozenType>) =
    member _.Disjuncts: EqSet<FrozenType> = disjuncts

    static member OfSeq(xs: FrozenType seq) : FTDisjuncts =
        let acc = ResizeArray<FrozenType>()

        let rec add (t: FrozenType) =
            match t with
            | FTOr ds -> EqSet.iter add ds.Disjuncts
            | _ -> acc.Add t

        for x in xs do
            add x

        FTDisjuncts(EqSet.ofSeq acc)

    /// Instantiation can collapse the set (`'T | string` with `'T := string` → `string`), so
    /// the result is a `FrozenType`, not an `FTDisjuncts`.
    member _.Map(f: FrozenType -> FrozenType) : FrozenType =
        FrozenType.MkUnion(seq { for d in disjuncts -> f d })

    override _.Equals(other) =
        match other with
        | :? FTDisjuncts as o -> disjuncts = o.Disjuncts
        | _ -> false

    override _.GetHashCode() = hash disjuncts

/// The declaration whose body declares a generalised body-local `let`. `'m` identifies a
/// member: its `MemberKey` once the file is typed, its registration while inference is still
/// settling the key's signature.
[<RequireQualifiedAccess>]
type LocalOwnerG<'m> =
    | Member of 'm
    | ModuleFunction of BindingKey
    /// An enclosing body-local `let`.
    | Local of LocalBindingId
    /// A body with no key of its own: a module-level `do`, a module-level tuple binding's
    /// right-hand side, an auto-property initialiser, or a type's `let` / `do` preamble or
    /// secondary constructor.
    | Initialiser
    /// A local of `template`, another file's splice template, copied in at a call site. The
    /// owning declaration belongs to the declaring file, terminating the owner chain.
    | Spliced of template: SymbolKey

module LocalOwnerG =
    let mapMember (f: 'a -> 'b) (o: LocalOwnerG<'a>) : LocalOwnerG<'b> =
        match o with
        | LocalOwnerG.Member m -> LocalOwnerG.Member(f m)
        | LocalOwnerG.ModuleFunction key -> LocalOwnerG.ModuleFunction key
        | LocalOwnerG.Local id -> LocalOwnerG.Local id
        | LocalOwnerG.Initialiser -> LocalOwnerG.Initialiser
        | LocalOwnerG.Spliced template -> LocalOwnerG.Spliced template

/// `LocalOwnerG` as the frozen file records it, a member identified by its `MemberKey`.
type LocalOwner = LocalOwnerG<MemberKey>

/// The constraints on one type-kinded parameter, with typar leaves `FTTypar(scope, i)`.
type ConstraintSet = ConstraintSetG<FrozenType>

type TypeTypar = TypeTyparG<FrozenType>

/// A declaration's type parameters as the frozen contract carries them.
type TyparList = TyparListG<FrozenType>

/// A generalised body-local `let`'s own typars: the scope its `FTTypar(LocalFunction Id, i)`
/// leaves carry, with `i < TyparArity`.
///
/// TODO(local-constraints): arity alone, so a splice of the declaring template regeneralises
/// the local UNCONSTRAINED. A body-local carrying a member or trait constraint needs the
/// constraints frozen here too.
type LocalScheme = { Id: LocalBindingId; TyparArity: int }

/// The mutable inference type IR. Every `TyVar` is a dense `TyVarId` index into the
/// per-file `TypeStore` union-find graph.
type SemType =
    | TyVar of TyVarId
    /// An argless primitive (`TyConst(RuntimeNames.intKey, [])`) or a generic intrinsic
    /// forwarding its args (`'T[]` ≡ `TyConst(RuntimeNames.arrayKey 1, [elem])`).
    | TyConst of key: TypeKey * args: EqArray<SemType>
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: EqArray<SemType>
    /// Field types are not stored inline, so look up the record's shape via `key`, and its
    /// declared `TypeParams` to substitute `args` into each field.
    | TyRecord of key: TypeKey * args: EqArray<SemType>
    /// Cases and `TypeParams` live in the union registry, reachable by `key`.
    | TyUnion of key: TypeKey * args: EqArray<SemType>
    | TyClass of key: TypeKey * args: EqArray<SemType>
    /// `type E = | C1 = v1 | …`. No `args`, because enums are never generic; `E` is a DISTINCT
    /// nominal, NOT structurally its underlying `int`.
    | TyEnum of key: TypeKey
    /// An anonymous (structural) union: `X | Y | null`, no key and no nominal identity.
    /// `TyOr []` is `never` (bottom); assignability is `subsumes`, not `unify`.
    | TyOr of disjuncts: TyDisjuncts
    /// A structural LITERAL type (`"GET"`, `42`); ground, and widens OUTWARD to its base
    /// primitive.
    | TyLiteral of value: LiteralConst
    /// Inert nodes with CHILDREN until the front end ground-evaluates them; every structural
    /// traversal must recurse them.
    | TyKeyOf of ty: SemType
    | TyIndexedAccess of objTy: SemType * index: SemType
    | TyConditional of TyConditionalPayload
    /// A position that resolved to no in-scope type shape. It unifies with nothing, so one
    /// broken contract type doesn't cascade.
    | TyUnknown of reason: UnknownReason
    /// An elaborated open type parameter. `freeze` rewrites every surviving `TyVar` to one, so
    /// afterwards no `TyVar` remains in any TAST `.ty` field.
    | TyTypar of scope: TyparScope * index: int

    /// A one-disjunct set collapses to the bare disjunct; `MkUnion []` is `never` (bottom).
    static member MkUnion(disjuncts: SemType seq) : SemType =
        let canonical = TyDisjuncts.OfSeq disjuncts

        if canonical.Disjuncts.Length = 1 then
            canonical.Disjuncts.[0]
        else
            TyOr canonical

/// `Check extends Extends ? WhenTrue : WhenFalse`.
and TyConditionalPayload =
    {
        Check: SemType
        Extends: SemType
        WhenTrue: SemType
        WhenFalse: SemType
    }

/// The disjuncts of a `TyOr`: flattened and deduped, so `string | int` = `int | string`.
and [<Sealed>] TyDisjuncts private (disjuncts: EqSet<SemType>) =
    member _.Disjuncts: EqSet<SemType> = disjuncts

    static member OfSeq(xs: SemType seq) : TyDisjuncts =
        let acc = ResizeArray<SemType>()

        let rec add (t: SemType) =
            match t with
            | TyOr ds -> EqSet.iter add ds.Disjuncts
            | _ -> acc.Add t

        for x in xs do
            add x

        TyDisjuncts(EqSet.ofSeq acc)

    /// `f` applied to each disjunct, or `ValueNone` when every disjunct maps reference-equal.
    /// Substituting can collapse the set (`'T | string` with `'T := string` → `string`), so a
    /// mapped result is a `SemType`, not a `TyDisjuncts`.
    member _.MapPreserve(f: SemType -> SemType) : SemType voption =
        match EqArray.mapPreserve f (EqArray.ofImmutable disjuncts.Underlying) with
        | ValueNone -> ValueNone
        | ValueSome mapped -> ValueSome(SemType.MkUnion mapped)

    override _.Equals(other) =
        match other with
        | :? TyDisjuncts as o -> disjuncts = o.Disjuncts
        | _ -> false

    override _.GetHashCode() = hash disjuncts

[<AutoOpen>]
module TyparLeafPatterns =

    /// A member's or a module function's own typar at `index`: the leaf a call site
    /// instantiates and the CLR encodes as `!!index`.
    [<return: Struct>]
    let (|FTFunctionTypar|_|) (t: FrozenType) : int voption =
        match t with
        | FTTypar(scope, index) when scope.IsFunction -> ValueSome index
        | _ -> ValueNone

    /// The inference-side form of `FTFunctionTypar`.
    [<return: Struct>]
    let (|TyFunctionTypar|_|) (t: SemType) : int voption =
        match t with
        | TyTypar(scope, index) when scope.IsFunction -> ValueSome index
        | _ -> ValueNone

/// Captured SRTP member-trait clause; `MemberName` is the compiled name (`"op_Addition"`,
/// `"Zero"`). `SupportTys` is the declared `(^T1 or ^T2)` support set, instantiated for
/// the use site; the trait stays undischarged until every element is pinned.
type MemberSignature =
    {
        MemberName: string
        SupportTys: EqArray<SemType>
        ArgTypes: EqArray<SemType>
        ReturnType: SemType
    }

/// Type-parameter constraint on a metavar, built from `Constraint<'T>` CST nodes and
/// discharged when the metavar links to a concrete shape.
[<RequireQualifiedAccess>]
type SemanticConstraintKind =
    | Equality
    | Comparison
    | Struct
    | ReferenceType
    | Nullness
    | NotNull
    /// `when 'e :> exn` — `target` is the required supertype, checked by `subsumes`.
    | Coercion of target: SemType
    /// `when 'a : (new : unit -> 'a)`: a public parameterless constructor. Every value type
    /// has one.
    | DefaultConstructor
    /// `when 'a : unmanaged`: a fixed-width scalar, an enum, or a non-generic struct whose
    /// every field is unmanaged.
    | Unmanaged
    /// `when 'a : enum<underlying>`: an enum type, whose underlying primitive is unified
    /// with `underlying` when the typar grounds.
    | Enum of underlying: SemType
    /// `when 'a : delegate<args, ret>`: a delegate whose `Invoke` is `obj * args -> ret`.
    | Delegate of args: SemType * ret: SemType
    /// The metavar ranges over a fixed set of arity-0 primitives, listed in the order a
    /// diagnostic lists them. Printf's flexible format families are its only source: `%d`
    /// accepts any integer type, `%f` any float type.
    | OneOf of choices: EqArray<TypeKey>

[<RequireQualifiedAccess>]
module SemanticConstraintKind =

    let mapTypes (f: SemType -> SemType) (kind: SemanticConstraintKind) : SemanticConstraintKind =
        match kind with
        | SemanticConstraintKind.Coercion target -> SemanticConstraintKind.Coercion(f target)
        | SemanticConstraintKind.Enum underlying -> SemanticConstraintKind.Enum(f underlying)
        | SemanticConstraintKind.Delegate(args, ret) -> SemanticConstraintKind.Delegate(f args, f ret)
        | SemanticConstraintKind.Equality
        | SemanticConstraintKind.Comparison
        | SemanticConstraintKind.Struct
        | SemanticConstraintKind.ReferenceType
        | SemanticConstraintKind.Nullness
        | SemanticConstraintKind.NotNull
        | SemanticConstraintKind.DefaultConstructor
        | SemanticConstraintKind.Unmanaged
        | SemanticConstraintKind.OneOf _ -> kind

    let iterTypes (f: SemType -> unit) (kind: SemanticConstraintKind) : unit =
        match kind with
        | SemanticConstraintKind.Coercion target -> f target
        | SemanticConstraintKind.Enum underlying -> f underlying
        | SemanticConstraintKind.Delegate(args, ret) ->
            f args
            f ret
        | SemanticConstraintKind.Equality
        | SemanticConstraintKind.Comparison
        | SemanticConstraintKind.Struct
        | SemanticConstraintKind.ReferenceType
        | SemanticConstraintKind.Nullness
        | SemanticConstraintKind.NotNull
        | SemanticConstraintKind.DefaultConstructor
        | SemanticConstraintKind.Unmanaged
        | SemanticConstraintKind.OneOf _ -> ()

[<Struct>]
type SemanticConstraint =
    {
        Kind: SemanticConstraintKind
        /// Source location of the `when 'a : …` clause, so a violation reports at the
        /// declaration.
        DeclKey: NodeKey
    }

/// An `objArg.X` access parked on a still-free object argument: `ResultTv` is the access's
/// own metavar, unified with `X`'s type once `objArg` resolves.
[<NoEquality; NoComparison>]
type DeferredMemberAccess =
    {
        MemberName: string
        Use: NodeSite
        ResultTv: TyVarId
    }

/// The nominal family a `NominalG` rebuilds as: a `Union`-flavoured nominal rebuilds as a
/// `TyUnion` / `FTUnion`, not a class.
[<RequireQualifiedAccess>]
type NominalFlavour =
    | Class
    | Const
    | Record
    | Union

/// A type known to denote an applied nominal type constructor: its flavour, key and
/// arguments. Required where the construct emits the type itself: an `interface <ty>`
/// reference, an `inherit` parent, a construction, a member access's object argument.
type NominalG<'ty> =
    private
        {
            Flav: NominalFlavour
            RefKey: TypeKey
            RefArgs: EqArray<'ty>
        }

    member this.Flavour: NominalFlavour = this.Flav

    member this.Key: TypeKey = this.RefKey

    member this.Args: EqArray<'ty> = this.RefArgs

/// A frozen nominal: the shape the backends and the external surface consume.
type FrozenNominal = NominalG<FrozenType>

/// An inference-side nominal: a `TyClass` or `TyConst` with its head destructured.
type SemNominal = NominalG<SemType>

[<RequireQualifiedAccess>]
module NominalG =

    let ofClass (key: TypeKey) (args: EqArray<'ty>) : NominalG<'ty> =
        {
            Flav = NominalFlavour.Class
            RefKey = key
            RefArgs = args
        }

    let ofConst (key: TypeKey) (args: EqArray<'ty>) : NominalG<'ty> =
        {
            Flav = NominalFlavour.Const
            RefKey = key
            RefArgs = args
        }

    /// The flavour and key are preserved.
    let map (f: 'a -> 'b) (n: NominalG<'a>) : NominalG<'b> =
        {
            Flav = n.Flav
            RefKey = n.RefKey
            RefArgs = EqArray.map f n.RefArgs
        }

[<RequireQualifiedAccess>]
module FrozenNominal =

    /// Rebuild the whole frozen type at a use site.
    let ty (n: FrozenNominal) : FrozenType =
        match n.Flavour with
        | NominalFlavour.Class -> FTClass(n.Key, n.Args)
        | NominalFlavour.Const -> FTConst(n.Key, n.Args)
        | NominalFlavour.Record -> FTRecord(n.Key, n.Args)
        | NominalFlavour.Union -> FTUnion(n.Key, n.Args)

    let tryOfFrozen (ft: FrozenType) : FrozenNominal voption =
        match ft with
        | FTClass(k, args) ->
            ValueSome
                {
                    Flav = NominalFlavour.Class
                    RefKey = k
                    RefArgs = args
                }
        | FTUnion(k, args) ->
            ValueSome
                {
                    Flav = NominalFlavour.Union
                    RefKey = k
                    RefArgs = args
                }
        | FTRecord(k, args) ->
            ValueSome
                {
                    Flav = NominalFlavour.Record
                    RefKey = k
                    RefArgs = args
                }
        // An intrinsic (`seq<'T>` on JS) freezes as `FTConst`, and its key is as nominal as
        // the other three.
        | FTConst(k, args) ->
            ValueSome
                {
                    Flav = NominalFlavour.Const
                    RefKey = k
                    RefArgs = args
                }
        | _ -> ValueNone

    /// `what` is a bare noun phrase the failure completes: "an `interface` clause" reads
    /// "an `interface` clause does not denote a type constructor: FTFun (…)".
    let ofFrozen (what: string) (ft: FrozenType) : FrozenNominal =
        match tryOfFrozen ft with
        | ValueSome n -> n
        | ValueNone -> failwithf "%s does not denote a type constructor: %A" what ft

[<RequireQualifiedAccess>]
module SemNominal =

    /// Rebuild the whole inference type at a use site.
    let ty (n: SemNominal) : SemType =
        match n.Flavour with
        | NominalFlavour.Class -> TyClass(n.Key, n.Args)
        | NominalFlavour.Const -> TyConst(n.Key, n.Args)
        | NominalFlavour.Record -> TyRecord(n.Key, n.Args)
        | NominalFlavour.Union -> TyUnion(n.Key, n.Args)

/// An admitted `inherit` parent, carrying the flavour `BaseEligibility.classify` proved.
[<RequireQualifiedAccess>]
type BaseParentG<'ty> =
    /// A non-interface class.
    | Class of NominalG<'ty>
    /// A heritable primitive's canon (`exn`): the CLR backend resolves it to its
    /// platform base class.
    | PrimitiveCanon of NominalG<'ty>

    member this.Nominal: NominalG<'ty> =
        match this with
        | BaseParentG.Class n
        | BaseParentG.PrimitiveCanon n -> n

    member this.Key: TypeKey = this.Nominal.Key

    member this.Args: EqArray<'ty> = this.Nominal.Args

/// An inference-side `inherit` parent.
type BaseParent = BaseParentG<SemType>

[<RequireQualifiedAccess>]
module BaseParentG =

    let map (f: 'a -> 'b) (p: BaseParentG<'a>) : BaseParentG<'b> =
        match p with
        | BaseParentG.Class n -> BaseParentG.Class(NominalG.map f n)
        | BaseParentG.PrimitiveCanon n -> BaseParentG.PrimitiveCanon(NominalG.map f n)

[<RequireQualifiedAccess>]
module BaseParent =

    /// Rebuild the parent's whole inference type.
    let ty (p: BaseParent) : SemType = SemNominal.ty p.Nominal

module MeasureTerm =
    let empty = MeasureTerm.Empty

    /// One base measure, to the first power.
    let atom (key: TypeKey) : MeasureTerm =
        MeasureTerm.OfList [ key, Rational.One ]

    let mul (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm =
        MeasureTerm.OfList(a.Exponents @ b.Exponents)

    let inv (m: MeasureTerm) : MeasureTerm =
        m.Exponents |> List.map (fun (n, e) -> n, -e) |> MeasureTerm.OfList

    let div (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm = mul a (inv b)

    /// `k` is `Rational`, so a fractional power is expressible: `pow m (1/2)` is a square root.
    let pow (m: MeasureTerm) (k: Rational) : MeasureTerm =
        if k.IsZero then
            empty
        else
            m.Exponents |> List.map (fun (n, e) -> n, e * k) |> MeasureTerm.OfList
