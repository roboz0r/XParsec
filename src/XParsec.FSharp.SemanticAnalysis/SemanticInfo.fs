namespace XParsec.FSharp.SemanticAnalysis

/// A member on a type. `Decl` is a `TypeKey`, so "the declaring key is not a type" is
/// unrepresentable by construction rather than a `failwithf` runtime guard a consumer runs.
///
/// `ArgSig` is the member's value-parameter signature as `FrozenType`s, written in the
/// declaring type's OPEN typars (`FTTypar(Declaring, i)`, never an instantiation — so a
/// key minted from a `C<int>` use site equals one minted from the open declaration). It
/// is the STRUCTURAL, value-equal form that makes a `MemberKey` a TOTAL overload identity:
/// it disambiguates overloads by argument TYPE (`GetHashCode()` vs `GetHashCode(!0)`,
/// `M(x:int)` vs `M(x:string)` vs `M(x:'T)`), not a lossy display string. `MethodTyparArity`
/// is the member's OWN generic arity (`M<'a>()` vs `M<'a,'b>()` — identical empty `ArgSig`,
/// distinct overloads), the second identity axis. Together with `Decl`/`Name`/`Kind` this
/// is the complete identity: the declaring type's own generic arity rides `Decl`
/// (`TypeKey.Name`'s `` `n `` suffix) and the return type is NOT an axis
/// (return-type-only overloading is illegal). `EqArray` (not `list`) so the containing
/// `SymbolKey` keeps the structural `=` interning relies on.
type MemberKey =
    {
        Decl: TypeKey
        Name: string
        ArgSig: EqArray<FrozenType>
        MethodTyparArity: int
        Kind: MemberKind
    }

/// What kind of member a `MemberKey` denotes. `Method` and `Property` are the
/// today-resolvable shapes; `InterfaceMethod` and `ExplicitInterfaceImpl` land their
/// consumers with interface conformance + `(this :> iface).M()` syntax.
and [<RequireQualifiedAccess>] MemberKind =
    | Method
    | Property
    /// An abstract method on an interface; `iface` is the declaring interface's
    /// `TypeKey`. Distinct from `Method` so a call site can resolve
    /// the right vtable slot when several interfaces inherit a like-named
    /// method (`IEnumerable<'T>::GetEnumerator()` vs
    /// `IEnumerable::GetEnumerator()`).
    | InterfaceMethod of iface: TypeKey
    /// An explicit interface implementation on a class:
    /// `Set<'T>::System.Collections.IEnumerable.GetEnumerator`. `iface` pins
    /// which interface's slot is being overridden, the token codegen needs to
    /// emit the `.override` row.
    | ExplicitInterfaceImpl of iface: TypeKey

/// Platform-agnostic, scope-unambiguous symbol identity. Strings + containment —
/// never a CLR `EntityHandle` or `System.Type` (those are per-context and
/// target-specific). The discriminator is the *containment chain* (namespace →
/// module* → type → member), not the bare name, so a project-local
/// `List` and `System.Collections.Generic.List`1` get different keys by
/// construction. Keyed on the open generic *definition* (a `TypeKey`'s `Name`
/// includes the `` `arity `` suffix); instantiation is the cheap per-use substitution.
///
/// The home ASSEMBLY is deliberately NOT here. Identity is nominal; the assembly is a
/// physical location. Within one compilation a fully-qualified name names at most one
/// type, and no lookup anywhere disambiguates on the assembly — so a key minted from a
/// bare compiled name (which is all ten of the string-fed mint sites have) compares
/// equal to one minted from a fully resolved shape, by construction rather than by
/// assertion. Where a backend genuinely needs the physical home (an `AssemblyRef`
/// scope, a JS import path) it reads `SymbolOrigin.Home.AssemblyOption` off the resolved shape.
///
/// There is deliberately NO `Module` case: a module appears only in HOLDER position.
/// A standalone module symbol has no reader (`OpenScope` is kind-blind by design).
///
/// The type IR's NOMINAL heads (`SemType.TyClass/TyRecord/TyUnion/TyEnum` and their
/// `FrozenType` mirrors) do NOT carry a `SymbolKey` — a nominal head is ALWAYS a type, so
/// they carry the narrow `TypeKey` and no consumer re-narrows at run time.
and [<RequireQualifiedAccess>] SymbolKey =
    | Type of TypeKey
    | Binding of BindingKey
    | Member of MemberKey

/// The immutable, *elaborated* type representation — the codomain of `freeze`
/// and the type the TAST carries into Codegen, distinct from the mutable
/// inference `SemType`. Its defining property is the **absence of a `TyVar`
/// case**: a `FrozenType` never holds a union-find unification variable, so a
/// metavar reaching the backend is unrepresentable rather than a convention to
/// assert against. Open type parameters — a generic definition's own typars in
/// their uninstantiated form — are the explicit, self-describing `FTTypar` node
/// (carrying its axis + index), so codegen reads a typar's axis + index off the node
/// rather than from a marker-`TypeVar` convention. Structural equality is value-based
/// (no `TypeVar` leaf), so a `FrozenType` is a sound dictionary key — which is what lets
/// it back a `MemberKey`'s `ArgSig` as a structural overload identity rather than a lossy
/// display string. Constructors mirror `SemType`'s shape under an `FT` prefix to avoid
/// ambiguity when both types are in scope.
///
/// NOTE (naming): `FrozenType` / `FT*` are
/// provisional; revisit before the representation is widely consumed.
and FrozenType =
    /// A nominal constant in two roles (the `SemType.TyConst` declaring-typar
    /// marker role is `FTTypar`): an argless primitive / intrinsic
    /// (`FTConst(RuntimeNames.intKey, [])`) and a generic intrinsic forwarding its args
    /// (`'T[]` ≡ `FTConst(RuntimeNames.arrayKey 1, [elem])`). Carries the same qualified
    /// `SymbolKey` its `SemType.TyConst` source does; codegen recognises a well-known
    /// intrinsic by KEY (`FTUnit`/`FTObj`/`FTArray`/`FTByref`, `IntrinsicTypePatterns`) and
    /// reaches its platform repr through the key-addressed forward axis.
    | FTConst of key: SymbolKey * args: EqArray<FrozenType>
    /// Curried; multi-arg functions nest `FTFun`.
    | FTFun of arg: FrozenType * result: FrozenType
    /// Flat n-ary tuple — mirrors `SemType.TyTuple`.
    | FTTuple of items: EqArray<FrozenType>
    | FTRecord of key: TypeKey * args: EqArray<FrozenType>
    | FTUnion of key: TypeKey * args: EqArray<FrozenType>
    | FTClass of key: TypeKey * args: EqArray<FrozenType>
    /// A nominal enum reference — the frozen mirror of `SemType.TyEnum`; see it for
    /// the full rationale. Niladic (no `args` — enums are never generic), a distinct
    /// nominal NOT its underlying `int`; the case→literal table rides the frozen
    /// `TDecl` node by `key`, and the per-variant repr is a backend decision.
    | FTEnum of key: TypeKey
    /// Frozen anonymous (structural) union — mirror of `SemType.TyOr`. Members
    /// live in an `EqSet` (insertion-ordered storage so the declared `.d.ts` order
    /// survives into diagnostics, SET-semantic equality/hash so `A | B ≡ B | A`),
    /// flattened/deduped/singleton-collapsed by the `MkUnion` smart constructor —
    /// the ONLY sanctioned producer (every rebuild site routes through it, never a
    /// raw member re-map, because instantiation can introduce duplicates). The
    /// backend lowers it to its universal-supertype primitive (`obj`+`isinst` on
    /// the CLR, erased on JS); no nominal identity. `FTOr []` is `never`.
    | FTOr of members: EqSet<FrozenType>
    /// A structural LITERAL type (`"GET"`, `42`) — the frozen mirror of
    /// `SemType.TyLiteral`. External-vocabulary ONLY (design §"Literal types stay
    /// structural … the nominalism invariant"): Vesper inference never mints one, it
    /// arises solely by instantiating an external signature. Ground, no children, no
    /// typars. Composes with `FTOr` (`FTOr [FTLiteral "ping"; FTLiteral "pong"]`); it
    /// erases to a base primitive, which is `LiteralConst`'s concern.
    | FTLiteral of value: LiteralConst
    /// The three TS type-level COMPUTATIONS the front end ground-EVALUATES (design
    /// §"keyof … ride on top"), carried FAITHFULLY from the manifest as inert nodes
    /// until a call site grounds their children: `keyof T`, `T[K]`, and
    /// `check extends extends_ ? whenTrue : whenFalse`. They have CHILDREN (unlike the
    /// ground `FTLiteral`), so every structural walk must thread them — a fresh method
    /// `TyVar` can live inside `objTy`/`index`/… after instantiation. External-
    /// vocabulary only; Vesper inference never mints one. Erase to `obj` on the CLR
    /// (they only arise on the JS seam and must be evaluated before codegen).
    | FTKeyOf of ty: FrozenType
    | FTIndexedAccess of objTy: FrozenType * index: FrozenType
    | FTConditional of FTConditionalPayload
    /// An open type parameter of the enclosing generic definition: `axis`
    /// selects the declaring-type vs method axis; `index` is its position in
    /// that axis's typar list — the order `freeze` quantifies in, which is the
    /// single index-minting point.
    | FTTypar of axis: TyparAxis * index: int
    /// Typar #`index` of the generalized scheme `scheme` — a body-local `let`'s OWN
    /// scheme. The root is NOT free: it is BOUND, just by a binder that is not the
    /// enclosing method. `let g = fun x -> x` inside a decl is its own declaration
    /// with its own generalized scheme; `Elaborate.mkMethodQuantEnv` fails to map
    /// `g`'s root not because the root is unbound but because it is looking at the
    /// WRONG binder's axis (it derives its remap by walking the ENCLOSING decl's
    /// type, in which `g`'s own root does not occur — every use of `g` instantiates
    /// away from it). So the leaf names the scheme that binds it. `index` is scoped
    /// to `scheme` (position within that local scheme, minted by `freeze` in
    /// first-occurrence pre-order — the same single-minting-point discipline
    /// `FTTypar` indices follow), so two distinct local schemes cannot collide even
    /// before their schemes are compared. NEVER equate two local typars by anything
    /// other than the `(scheme, index)` PAIR.
    ///
    /// Naming the scheme at all preserves an association a future GENERIC-CLOSURE
    /// lowering needs, rather than erasing it and forcing it to be reconstructed.
    /// Real F# compiles `let f () = let g = fun x -> x in (g, g)` to `f<'a,'b>` (its
    /// two USE-SITE instantiations, implicitly generalized onto `f`'s own method
    /// typar list) plus a separate GENERIC closure class `g@2T<'c>` for `g`'s own
    /// root — it does NOT append `'c` to `f`'s typars, which would change `f`'s ABI
    /// and force callers to pass a third type argument. `scheme` is the handle on
    /// that separate home.
    ///
    /// It is a DISTINCT case rather than a third `TyparAxis` because
    /// `Declaring`/`Method` indices are positions in a *declared* typar list on the
    /// enclosing decl, and every consumer realises them from an argument vector.
    /// A local typar has no position in that list and must NEVER be instantiated
    /// from one. A separate case makes F#'s incomplete-match check force every
    /// `FrozenType` walk to decide what it means; a third axis would ride the
    /// existing `FTTypar` arms silently — which is exactly how the predecessor
    /// `FTUnknown "?free-typar"` hack conflated every local typar into one
    /// name-equal leaf.
    ///
    /// **`scheme` is BODY-RELATIVE and must never be resolved against anything.**
    /// A `SchemeId` is a bare ordinal minted per frozen body, so there is nothing in
    /// the program it addresses: no node, no pool slot, no side-table key. That is
    /// what the type buys — the leaf cannot be resolved even by accident, whereas a
    /// `NodeKey` here was always one lookup away from being resolved against the
    /// consuming file's own tree (keys from different files collide freely, by
    /// design: cross-file references resolve by NAME against prior views). What is
    /// left to uphold:
    ///
    /// - It is interpreted only against the TEMPLATE that carries it, exactly as
    ///   `FTTypar`'s index is. Two leaves from different files comparing structurally
    ///   equal is no more a bug than `FTTypar(Declaring, 0)` from two files doing so.
    /// - It is CONSUMED AT THAW: the leaf becomes a fresh consumer-owned `TyVar` and
    ///   the id does not survive into the spliced tree.
    ///
    /// Therefore: NEVER use `scheme` for cross-file (or any) resolution. It
    /// identifies a scheme WITHIN one frozen body and nothing else.
    | FTLocalTypar of scheme: SchemeId * index: int
    /// Mirror of `SemType.TyUnknown`: a nominal head that resolved to no type
    /// shape. Carried so `freeze` is total; whether it may legitimately reach
    /// the backend is an open question (likely a hard error).
    | FTUnknown of name: string

    /// The smart constructor for a frozen anonymous union — the ONLY sanctioned
    /// producer of `FTOr`. Owns TS's semantic union rules: flatten nested `FTOr`,
    /// dedupe (via `EqSet`, keeping first occurrence / declared order), and collapse
    /// a singleton set to its bare member. The frozen mirror of `SemType.MkUnion`.
    /// EVERY rebuild site (`toFrozen`, freshen/reaxis walks, `substituteDeclaring`,
    /// the TS provider's `toFrozen`) MUST route through here — duplicates arise
    /// POST-construction when a member instantiates to another member's value, so a
    /// raw member re-map would leave a stale `FTOr [string; string]`.
    static member MkUnion(members: FrozenType seq) : FrozenType =
        let acc = ResizeArray<FrozenType>()

        let rec add (t: FrozenType) =
            match t with
            | FTOr ms -> EqSet.iter add ms
            | _ -> acc.Add t

        for m in members do
            add m

        let canonical = EqSet.ofSeq acc

        if canonical.Length = 1 then
            canonical.[0]
        else
            FTOr canonical

/// Named payload of `FrozenType.FTConditional` (`Check extends Extends ? WhenTrue
/// : WhenFalse`). All four fields are the same type, so a positional tuple lets a
/// `WhenTrue`/`WhenFalse` swap typecheck silently — the record makes each branch's
/// identity nominal.
and FTConditionalPayload =
    {
        Check: FrozenType
        Extends: FrozenType
        WhenTrue: FrozenType
        WhenFalse: FrozenType
    }

/// Every `TyVar` is a dense `TyVarId` index into the per-file `TypeStore`
/// union-find graph. Will grow to include generics, measures.
type SemType =
    /// Call `UnionFind.find` then read the representative's `Link` to dereference.
    /// The payload is the raw `TyVarId` — the store keys every metavar cell by it,
    /// so `SemType` is fully value-comparable (no reference-identity leaf).
    | TyVar of TyVarId
    /// A nominal constant in two roles, both carrying a qualified `SymbolKey`
    /// identity (like `TyRecord`/`TyUnion`/`TyClass` — an intrinsic is no longer the
    /// one identity class that drops its namespace): (a) an argless primitive /
    /// intrinsic binding (`TyConst(RuntimeNames.intKey, [])`, ns `Vesper`), and (b) a
    /// *generic intrinsic* that forwards its type arguments (`'T[]` ≡
    /// `TyConst(RuntimeNames.arrayKey 1, [elem])`, byref `&` likewise — the array repr
    /// `!0[]` is a backend-specific encoding, the args are backend-agnostic
    /// structure). So `args ≠ []` does NOT imply a registry nominal — array/byref are
    /// the only generic intrinsics in v1. The `key`'s `name` component is the verbatim
    /// bare identity string (`"int"`, `"[]"`). The semantic passes recognise a
    /// well-known intrinsic by KEY IDENTITY — the `TyBool`/`TyUnit`/`TyObj`/`TyString`/
    /// `TyArray`/`TyByref`/… active patterns (`IntrinsicTypePatterns`) — never by a
    /// stringified name; the codegen/repr axis (canon→platform maps) reads the bare
    /// name via `SymbolKeyOps.intrinsicName` (non-lossy) or `simpleName` (display). Args
    /// participate in unification (same arity rule as `TyRecord`). A declaring-type typar
    /// is a `TyTypar`, never this case — `TyConst` is a nominal head only.
    | TyConst of key: SymbolKey * args: EqArray<SemType>
    /// Curried; multi-arg functions nest TyFun.
    | TyFun of arg: SemType * result: SemType
    /// Flat n-ary tuple. Unifies pairwise with same-arity TyTuple; arity
    /// mismatch is a diagnostic in Unification.
    | TyTuple of items: EqArray<SemType>
    /// Field types are not stored inline — look up the record's shape via its
    /// `key` (and the declared `TypeParams` used to substitute `args` into each
    /// field). Two TyRecords unify iff their `key`s are equal AND their args
    /// unify pairwise. Identity is the resolved `TypeKey` (minted once in
    /// NameResolution / Translate), not a bare string — a nominal head is ALWAYS a type,
    /// so the payload is the narrow key, never the wider `SymbolKey`. The `key`'s holder
    /// distinguishes same-named records in different namespaces; its `TyparArity` is part of it.
    | TyRecord of key: TypeKey * args: EqArray<SemType>
    /// Same shape as TyRecord. Cases / TypeParams live in the union registry,
    /// reachable by `key` (`TypeRegistry.tryUnionByKey`).
    | TyUnion of key: TypeKey * args: EqArray<SemType>
    /// Same shape as `TyRecord` / `TyUnion`; member lookup is a side-channel on
    /// the class registry. Two `TyClass` unify iff their `key`s are equal AND
    /// their args unify pairwise.
    | TyClass of key: TypeKey * args: EqArray<SemType>
    /// A nominal enum reference (`type E = | C1 = v1 | …`). Enums are never
    /// generic, so — unlike `TyUnion` / `TyRecord` / `TyClass` — there is NO
    /// `args` field (illegal states unrepresentable): an enum is a niladic
    /// nominal identified solely by its `TypeKey`. The ordered case→literal
    /// table is reached off the frozen `TDecl` node by `key` (it already rides
    /// the node, like union cases — no new carrier). `E` is a DISTINCT nominal
    /// type, NOT structurally its underlying `int`, which is what a faithful
    /// `System.Enum` emission and the closed-set semantic model require; the
    /// per-variant representation (numeric→`System.Enum`, string→struct-wrapper,
    /// mixed→`obj`-box, JS→object map) stays a backend decision read off the case
    /// table + `TEnumCases.classify`. Two `TyEnum` unify iff their `key`s match.
    | TyEnum of key: TypeKey
    /// An anonymous (structural) union — TypeScript-style `X | Y | null`. Distinct
    /// from the nominal `TyUnion` (a declared `type Foo = A | B`): it has no key,
    /// no nominal identity, and its members are an order-insensitive, deduped,
    /// flattened **set** held in EqSet set-semantic form — INSERTION-ordered, NOT
    /// sorted (no total order on `SemType` is imposed; declared member order is
    /// preserved). The smart constructor `mkUnion` is the ONLY sanctioned producer —
    /// it enforces that set form, so the raw case is never built directly outside
    /// `mkUnion` (the private `UnionMembers` payload makes this structural — see
    /// below). Set semantics are what make `string | int` ≡ `int | string` under the
    /// equality layer's `n1 = n2` discipline (EqSet set-equality, order-independent),
    /// NOT a shared sort order. `TyOr []` is `never` (bottom). Unions enter the graph
    /// only at annotation sites — inference never synthesises one (the principality
    /// rule); membership/assignability lives in the directional `subsumes` layer,
    /// never in symmetric `unify`.
    ///
    /// The payload is a private-ctor `UnionMembers`, so the set form is
    /// type-enforced: the raw case cannot be built with an arbitrary `EqArray`.
    /// `SemType.MkUnion` (aliased as `mkUnion`) is the sole producer.
    | TyOr of members: UnionMembers
    /// A structural LITERAL type (`"GET"`, `42`) — see `FrozenType.FTLiteral`.
    /// External-vocabulary ONLY: Vesper inference NEVER mints one (the nominalism
    /// invariant — `"ping"` types as `string`, always); it arises solely by
    /// instantiating an external signature, and matters only DIRECTIONALLY at the
    /// external-arg seam (the `subsumes` layer). Ground, no children, no typars;
    /// widens OUTWARD to its base primitive.
    | TyLiteral of value: LiteralConst
    /// The `SemType` mirror of `FrozenType.FTKeyOf`/`FTIndexedAccess`/`FTConditional`:
    /// the three TS type-level COMPUTATIONS (`keyof T`, `T[K]`, conditional) carried
    /// as inert nodes with children until the front end ground-EVALUATES them (design
    /// §"keyof … ride on top"). External-vocabulary only; inference never mints one.
    /// Every structural traversal MUST recurse their children — a fresh method `TyVar`
    /// can live inside after an external signature is instantiated.
    | TyKeyOf of ty: SemType
    | TyIndexedAccess of objTy: SemType * index: SemType
    | TyConditional of TyConditionalPayload
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
    /// `TyVar` reaching Codegen is a bug. A declaring-typar marker and a static-fn typar
    /// are both this single case — there is no `TyConst "'A"` marker form. It also
    /// rides the inference-side template helpers that work in `SemType` but must
    /// name an open typar (`ofFrozen`, `ExternalSymbols.openSignature`).
    ///
    /// **Invariant: never produced during inference.** Unification / generalisation
    /// never see it (they run before `freeze`); their match arms treat it as
    /// impossible (`failwith`) — a free invariant check. Only `freeze` mints it
    /// (the single index-minting point, Edge A) and only Codegen + post-freeze
    /// walks read it.
    | TyTypar of axis: TyparAxis * index: int

    /// The smart constructor for anonymous (structural) unions — the ONLY
    /// sanctioned producer of `TyOr` (aliased as `mkUnion` in `SemTypeOps`).
    /// `UnionMembers.OfSeq` owns flatten / dedup (the `EqSet` set-semantic
    /// identity, NOT a canonical sort — a total order on `SemType`/`FrozenType`
    /// does not exist, per the EqSet design decision); `MkUnion` adds the
    /// SemType-level **collapse**: a one-member set is the bare member, never a
    /// degenerate `TyOr`. `MkUnion []` is `TyOr (empty)` = `never` (bottom).
    static member MkUnion(members: SemType seq) : SemType =
        let canonical = UnionMembers.OfSeq members

        if canonical.Members.Length = 1 then
            canonical.Members.[0]
        else
            TyOr canonical

/// Named payload of `SemType.TyConditional` — the mirror of `FTConditionalPayload`
/// (`Check extends Extends ? WhenTrue : WhenFalse`). Same-typed branches, so the
/// record makes a `WhenTrue`/`WhenFalse` swap a compile error rather than a silent
/// positional mistake.
and TyConditionalPayload =
    {
        Check: SemType
        Extends: SemType
        WhenTrue: SemType
        WhenFalse: SemType
    }

/// The member set of an anonymous union (`SemType.TyOr`): an order-insensitive,
/// deduped, flattened `EqSet` — insertion-ordered storage (declared order
/// survives into diagnostics) with SET-semantic equality/hash, so `string | int`
/// and `int | string` are the SAME value WITHOUT a canonical sort (rejected — no
/// total order on `SemType` exists; see the EqSet design decision). Private
/// constructor — the only way in is `OfSeq`, so a non-canonical `UnionMembers`
/// cannot exist; this makes the canonical set form a *type-enforced* invariant
/// rather than a `mkUnion`-only convention. Collapse to a single member lives one
/// level up in `SemType.MkUnion` (a one-member set is a `SemType`, not a
/// `UnionMembers`).
and [<Sealed>] UnionMembers private (members: EqSet<SemType>) =
    /// The canonical (flattened / deduped) member set. A genuine union has ≥ 2
    /// here; `OfSeq` may yield 0 (never) or 1 (which `MkUnion` collapses before it
    /// ever becomes a `TyOr`).
    member _.Members: EqSet<SemType> = members

    /// Canonicalise an arbitrary member sequence: splice nested unions, then drop
    /// structural duplicates via `EqSet` (insertion order preserved — declared
    /// order survives). The sole normaliser; NO sort (set-semantic identity).
    static member OfSeq(xs: SemType seq) : UnionMembers =
        let acc = ResizeArray<SemType>()

        let rec add (t: SemType) =
            match t with
            | TyOr ms -> EqSet.iter add ms.Members
            | _ -> acc.Add t

        for x in xs do
            add x

        UnionMembers(EqSet.ofSeq acc)

    /// Map each member, then re-canonicalise — the single home for the
    /// rebuild-and-recanonicalise pattern. Resolving / substituting / remapping a
    /// member can collapse the set (`'T | string` with `'T := string` → `string`),
    /// so the result routes back through `MkUnion` and is a `SemType` (a post-map
    /// collapse is a bare member, not a `UnionMembers`).
    member _.Map(f: SemType -> SemType) : SemType =
        SemType.MkUnion(seq { for m in members -> f m })

    // Delegate equality/hash to `EqSet`'s SET-semantic implementation, so
    // `string | int` and `int | string` are equal and hash identically.
    override _.Equals(other) =
        match other with
        | :? UnionMembers as o -> members = o.Members
        | _ -> false

    override _.GetHashCode() = hash members

/// Abelian-group expression over named measure atoms. Always stored in a
/// normalised form: each exponent is in canonical Rational form, zero
/// exponents are dropped, and entries are sorted by measure name. Equality
/// is structural list equality after normalise.
and [<Sealed>] MeasureTerm private (exponents: (string * Rational) list) =
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    /// The group identity (dimensionless).
    static member Empty = MeasureTerm([])

    /// Normalises a raw list: duplicate measures are merged (exponents summed),
    /// zero exponents dropped, result sorted by measure name.
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
/// containing val's typar list. `Unification.dischargeSrtpBounds` fires when
/// any participating TyVar's `Link` is set and dispatches against either
/// a built-in primitive table (for `TyConst "int"` etc.) or the candidate
/// type's `ClassTypes` entry (for `TyClass`).
and MemberSignature =
    {
        MemberName: string
        ArgTypes: EqArray<SemType>
        ReturnType: SemType
    }

/// Type-parameter constraint attached to a `TypeVar`. Built from
/// `Constraint<'T>` CST nodes by `Unification.translateConstraints` and
/// discharged by `Unification.unify` when the TyVar is linked to a concrete
/// shape. v1 covers the trait-table subset (`equality`, `comparison`, `struct`,
/// `not struct`, `: null`, `: not null`) plus `Coercion` (`:> T` subtype bounds,
/// checked via `subsumes`); `MemberTrait`, `DefaultConstructor`, `Enum`,
/// `Unmanaged`, `Delegate`, and `Default` are deferred.
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

/// One element of a TyVar's `PendingDotAccess` list. `MemberName` is the
/// field-or-member name in `receiver.X`; `Use` is the access expression's site — its key
/// records the resolved access for Elaborate, its token places the diagnostic, and one
/// projection answers both so they cannot name different use sites. `ResultTv` is the
/// access expression's own TyVar id — unified with the field/member's declared type when
/// the receiver resolves.
and [<NoEquality; NoComparison>] DeferredMemberAccess =
    {
        MemberName: string
        Use: NodeSite
        ResultTv: TyVarId
    }

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
