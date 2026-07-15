namespace XParsec.FSharp.SemanticAnalysis

open System.Numerics

// `SymbolOrigin` / `SymbolKey` / `MemberKind` live here (ahead of `SemType`)
// because the nominal `SemType` cases (`TyUnion` / `TyRecord` / `TyClass` / `TyEnum`) carry
// a `TypeKey` as their identity — pure string/EqArray records with no `SemType`
// dependency. `ExternalSymbols.fs` (which mints/decomposes these) compiles after.

/// A simple assembly name (the `ProjectInfo` reference-set key), wrapped so it can
/// never be confused with a namespace or a type name at the type level.
[<Struct>]
type AssemblyName =
    | AssemblyName of name: string

    member this.Name = let (AssemblyName n) = this in n

/// Where a symbol PHYSICALLY lives. This is not part of any key: nominal identity is
/// the containment chain + namespace + name, and within one compilation a fully
/// qualified name names at most one type. The assembly is a *function of* the identity,
/// carried on the resolved SHAPE (`SymbolOrigin`) and consulted only where a backend
/// needs a physical location (a CLR `AssemblyRef` scope, a JS import path).
/// `Unstamped` is the "no home / placeholder" state: the compilation being analysed,
/// plus the front-end-only / contract-scrape paths that have no home assembly at all,
/// and the pre-stamp placeholder a stamping wrapper (`ExternalSymbolProviders.stack`,
/// `ReferencedProject.wrap`) overwrites before the shape reaches a backend.
[<RequireQualifiedAccess>]
type Origin =
    | Unstamped
    | InAssembly of asm: AssemblyName

    /// The home assembly's simple name, or `ValueNone` on an `Unstamped` origin (no
    /// home: the compilation being analysed, a front-end-only / contract-scrape path, or
    /// a pre-stamp placeholder a stamping wrapper overwrites). TOTAL — the caller decides
    /// what "no home" means (fall back to local, or fail with its OWN import-path
    /// message), rather than a throwing accessor deciding for every consumer.
    member this.AssemblyOption: string voption =
        match this with
        | Origin.Unstamped -> ValueNone
        | Origin.InAssembly a -> ValueSome a.Name

    /// Whether this origin names a home assembly — the local-vs-external discriminator a
    /// backend guard reads (`InAssembly _` ⇒ external / importable, `Unstamped` ⇒
    /// project-local).
    member this.IsStamped: bool =
        match this with
        | Origin.Unstamped -> false
        | Origin.InAssembly _ -> true

/// A standalone utility (wired to NOTHING in the compile chain) for a caller that holds
/// a genuinely anonymous origin and wants a stable, deterministic name for it.
module AnonymousOrigin =
    /// Deterministic, collision-resistant assembly name from arbitrary content.
    /// NOT called anywhere in the compile chain — a utility a caller with a genuinely
    /// anonymous origin opts into for a stable name. The `$anon.` prefix guarantees no
    /// clash with a real assembly/module name (a real assembly simple name never begins
    /// with `$`; cf. the `@struct` synthetic home in `TsManifestTypes`).
    let nameOfContent (content: string) : AssemblyName =
        // FNV-1a over the UTF-8 bytes — a stable, non-cryptographic content hash that,
        // unlike `String.GetHashCode`, is identical across runs, processes and platforms.
        let mutable h = 0xcbf29ce484222325UL
        let bytes = System.Text.Encoding.UTF8.GetBytes content

        for b in bytes do
            h <- (h ^^^ uint64 b) * 0x100000001b3UL

        AssemblyName(sprintf "$anon.%016x" h)

/// A namespace — the root holder. `Path` is SEGMENTED (`["System"; "Collections"]`),
/// never a dotted string: the prefix relations the codebase needs (`StartsWith(ns + ".")`)
/// are segment-list prefix tests. A dotted string can only approximate them with a
/// last-dot cut, which mis-splits a name like `Vesper.Collections.seq`.
/// An EMPTY path is the global namespace — a real thing (CLR types live there), not a
/// sentinel: the global namespace is spelled by the empty path, never by `ns = ""`.
type NamespaceKey =
    {
        Path: EqArray<string>
    }

    /// The dotted rendering (`"System.Collections"`; `""` for the global namespace).
    /// The BOUNDARY projection — for CLR `TypeRef` namespace slots, provider store
    /// keys and diagnostics. Identity comparisons use the segmented `Path`.
    member this.Dotted: string = System.String.Join(".", this.Path.Underlying)

    /// The global namespace.
    static member Global = { Path = EqArray.empty }

/// What holds a module: a namespace, or an enclosing module (modules nest).
///
/// Also what holds a `BindingKey`. `InNamespace` in *that* position means the binding
/// has NO declaring module — it sits directly in the namespace. F# source cannot
/// produce one (a namespace cannot hold a value), but the EXTERNAL vocabulary does:
/// a TS package's top-level export (`TsManifestProvider`, `nsPath = ""`) and a
/// flat-package contract extern (`ExternalSymbols.monoFrozen "printfn"`) are both
/// exactly that.
[<RequireQualifiedAccess>]
type ModuleHolder =
    | InNamespace of ns: NamespaceKey
    | InModule of parent: ModuleKey

    /// Walk to the namespace at the root of the chain.
    member this.Namespace: NamespaceKey =
        match this with
        | ModuleHolder.InNamespace ns -> ns
        | ModuleHolder.InModule parent -> parent.Namespace

    /// How many `module`s deep this scope is — a namespace body is 0. F# enters an inner
    /// scope AFTER its enclosing one, so a deeper scope's contribution to the name
    /// environment outranks everything the scopes above it added: this int is the whole of
    /// "a bare name resolves innermost-outward".
    member this.Depth: int =
        match this with
        | ModuleHolder.InNamespace _ -> 0
        | ModuleHolder.InModule parent -> parent.Holder.Depth + 1

    /// The scopes a bare name written HERE is searched in, innermost FIRST: this scope,
    /// then each enclosing module, then the namespace at the root.
    member this.SelfAndAncestors: ModuleHolder list =
        match this with
        | ModuleHolder.InNamespace _ -> [ this ]
        | ModuleHolder.InModule parent -> this :: parent.Holder.SelfAndAncestors

/// A module. NO arity — modules are not generic. That asymmetry with `TypeKey` is
/// the point: a module is simpler (no generics, no overloading) and richer in
/// containment (it holds modules *and* types).
and ModuleKey =
    {
        Holder: ModuleHolder
        Name: string
    }

    /// Walk the holder chain to the namespace at its root.
    member this.Namespace: NamespaceKey = this.Holder.Namespace

/// What holds a type.
[<RequireQualifiedAccess>]
type TypeHolder =
    | InNamespace of ns: NamespaceKey
    /// A type declared inside a `module` — `namespace N` + `module M` + `type T`.
    /// `parent` names the module's COMPILED holder type (the `…Module` suffix already
    /// applied), so the chain reads as the containment the CLR will eventually emit:
    /// `T` nested in `M`, `M` in namespace `N`.
    ///
    /// The holder is part of the CLAIM a declaration holds (`TypeIdentity.Holder`), so
    /// `N.A.T` and `N.B.T` are two distinct types — not one name contested twice.
    | InModule of parent: ModuleKey
    /// EXTERNAL ONLY — a CLR *nested* type. Unconstructible from Vesper source (the
    /// parser cannot declare a nested type); required to name
    /// `` System.Collections.Generic.List`1+Enumerator ``, the shape the duck-typed
    /// struct-enumerator path consumes. This case decodes the nesting STRUCTURALLY: the `+`
    /// in that display name is a reflection *display* convention, not a metadata name, so a
    /// nested `TypeRef` must chain through its enclosing type's `TypeRef` as ResolutionScope
    /// rather than be recovered by splitting a string on `+`.
    | InType of outer: TypeKey

/// A type definition — a nominal identity, and for a project-local type its
/// `TypeRegistry` key. Identity is the containment chain + the plain name + the generic
/// ARITY, and nothing else: F# overloads a type name on arity alone, so that int is the
/// whole of the discriminator a name needs. (NOT the typars themselves — a typar's name
/// and constraints are not identity-bearing, and a key minted from a contract, which has
/// no constraint detail, must still compare equal to one minted from source. The typar
/// DETAIL lives on `RecordTypeInfo` / `ClassTypeInfo` / …, the type ARGUMENTS on the
/// `SemType` — `TyUnion(key, args)`.)
///
/// `Name` is the PLAIN source name (`List`, `seq`, `[]`) — never the CLR `` `N ``-mangled
/// metadata spelling, which is a *rendering* of `(Name, Arity)` and lives only at the
/// metadata boundary (`SymbolKeyOps.typeMetaName`, the one renderer; `typeKeyOf`, the one
/// parser — inverses on the `InNamespace`/`InType` sublattice, which is the whole of what
/// a bare metadata name can express; see `typeKeyOf`). Because the arity is a FIELD, a
/// producer cannot forget to state it, and no consumer can be arity-blind by accident.
///
/// For a NESTED type `Name`/`TyparArity` are the innermost segment's own (`Enumerator`, 0); the
/// `+`-mangled reflection spelling is produced on demand, each segment rendering its OWN
/// count (`` List`1+Enumerator ``), which is the CLR rule.
///
/// CAUTION for type-identity comparisons in the unifier: structural `=` on two
/// `TypeKey`s does NOT reconcile a language capability's two nominal faces (its BCL
/// platform key vs. its canonical key — e.g. `IEnumerable`1` vs. `Vesper.Collections.seq`).
/// When comparing nominal heads for "same type" at a unify / subsume / overload seam, go
/// through `UnificationEngineCore.sameNominalKey` (or `capabilityCanonKey`), never a bare
/// `=`, or a capability spelled as its BCL face will read as a distinct type.
and TypeKey =
    {
        Holder: TypeHolder
        Name: string
        /// This segment's OWN generic-parameter count (0 ⇒ non-generic, and no `` `N ``
        /// when rendered). A nested type's outer carries its own count; the CLR spells
        /// each segment's separately (`` Outer`1+Inner`1 `` = one typar each).
        TyparArity: int
    }

    /// Walk the holder chain to the namespace at its root. A nested type reports its
    /// OUTER's namespace — which is exactly what the CLR does.
    member this.Namespace: NamespaceKey =
        match this.Holder with
        | TypeHolder.InNamespace ns -> ns
        | TypeHolder.InModule parent -> parent.Namespace
        | TypeHolder.InType outer -> outer.Namespace

/// WHERE a candidate binding ENTERS the name environment as seen from one use site. F#
/// builds that environment by descending the module tree and ADDING, in source order, each
/// declaration and each `open` — and the LAST thing added wins. So precedence is not a
/// hand-ordered cascade of special cases (innermost-first, open-beats-outer-decl,
/// last-open-wins); it is a single ordering, and this is it:
///
///   * `Depth` — how many `module`s enclose the scope that added the binding. An inner
///     scope is entered after its enclosing one, so it outranks everything above it. This
///     is what makes a bare name resolve innermost-outward, and it is what keeps a
///     `module rec` (where every declaration in the scope shares one offset) ordered.
///   * `Offset` — where in that scope the binding was added: a declaration's own
///     `VisibleFrom`, or the offset of the `open` that brought it in. Within one scope a
///     declaration and an `open` are ordered by nothing but the text, which is exactly how
///     F# orders them.
///
/// Comparison is structural and field-ordered — `Depth`, then `Offset` — so `max` IS the
/// resolution rule.
[<Struct>]
type BindingRank = { Depth: int; Offset: int }

/// WHERE a by-NAME lookup speaks FROM. A name is not an identity on its own — it is one
/// only as seen from somewhere — and "somewhere" in F# is three facts, so they travel as
/// one value rather than as arguments a caller can supply some of:
///
///   * `Pos` — the place in the file. Declaration scoping is file-ordered, so a claim
///     answers only at offsets at or after it (`TypeIdentity.VisibleFrom`).
///   * `Holder` — the module / namespace chain the use is nested in, INNERMOST last (the
///     `ModuleHolder` chain `ModuleRules.holderChain` builds from the use's containment).
///     A bare name resolves innermost-outward, so a use inside `module A` is not the same
///     use site as one at namespace level even at the same offset. A SIBLING module
///     contributes nothing to it: `module A`'s types are simply not in scope in `module B`.
///   * `Opens` — the `open`s in scope, which is how a sibling module's types get in.
///
/// `Pos` is a `SourcePos`, whose representation is private: a use site can therefore only
/// be pinned to a node that HAS a place in the file, and a counter-minted key cannot mint
/// one. `Holder` is `ValueNone` exactly for a read that speaks from nowhere — the
/// whole-unit view (`UseSite.unbounded`).
[<NoComparison>]
type UseSite =
    {
        Pos: SourcePos
        Holder: ModuleHolder voption
        Opens: LocalOpen list
    }

    /// The offset a visibility test compares a claim's `VisibleFrom` against.
    member this.Offset: int = this.Pos.Offset

module UseSite =

    /// A read that sees EVERY declaration, wherever it sits and whatever holds it — for a
    /// query with no position and no enclosing module to speak from (an observer of the
    /// finished registry, a consumer that already holds a resolved key).
    let unbounded: UseSite =
        {
            Pos = SourcePos.unbounded
            Holder = ValueNone
            Opens = []
        }

/// A type name AS WRITTEN at a use site, split where the syntax splits it: the dotted SOURCE
/// path of the scope that QUALIFIES the name (`"A"` in `A.T`, `"N.A"` in `N.A.T`, EMPTY for a
/// bare `T`), and the short name itself. The two facts a by-name type lookup needs from the
/// syntax, and the only two — a written head says which SCOPE to look in and which NAME to
/// find there.
///
/// The path is a SOURCE path (what an `open` and a qualifier write), never a compiled holder
/// name (`ListModule`): the scope it names is recovered by resolving it against the scopes
/// this unit declares, exactly as an `open`'s path is.
///
/// A BARE name is the empty path — not a separate case. A bare name and a qualified one
/// resolve by ONE rule (`TypeRegistry.claimRank`): the name enters the environment through a
/// scope, and the empty path names the enclosing scope itself.
[<Struct>]
type WrittenTypeName =
    {
        /// The qualifying scope's dotted SOURCE path; empty for a bare name.
        Path: string
        /// The short type name (the last segment as written, no arity suffix).
        Name: string
    }

    /// The name as the source spells it — for diagnostics.
    member this.Written: string =
        if this.Path.Length = 0 then
            this.Name
        else
            this.Path + "." + this.Name

module WrittenTypeName =

    /// A name written with no qualifier.
    let bare (name: string) : WrittenTypeName = { Path = ""; Name = name }

/// A module-level binding / operator. No `ArgSig`: modules do not overload.
///
/// `Decl` is a `ModuleHolder`, so the two states are distinguished BY CASE, not by an
/// empty string: `InModule m` is the ordinary module-qualified binding
/// (`Vesper.Unchecked.defaultof`), and `InNamespace ns` is the UNQUALIFIED one — a flat
/// package's export, which has no declaring module. A consumer matches on that case,
/// never on a `when ns <> ""` string guard.
///
/// A namespace holding a value is not F# — but this key names the EXTERNAL vocabulary
/// too, where a TS module's top-level export and a flat-package contract extern are
/// exactly that.
type BindingKey = { Decl: ModuleHolder; Name: string }

/// A key's name AS SHOWN TO A HUMAN — the result of `SymbolKeyOps.simpleName`, which
/// drops the containment chain and the generic arity. A LOSSY projection OUT of an
/// identity, and never a route back INTO one: nothing mints a key from it, and no table
/// is keyed by it.
///
/// A single-case DU, not an abbreviation, precisely so that recovering the string takes an
/// explicit `let (DisplayName s) = …`. Two consumers legitimately do:
///   * HUMAN — a diagnostic, an error message, display text;
///   * BACKEND NAME EMISSION — mangling an identifier the target actually emits (a JS
///     identifier, a CLR member name), whose names carry no generic arity.
/// Every other unwrap is treating a display string as an identity — the bug this type
/// exists to make greppable. To ask a table, ask the KEY (`TypeRegistry`'s `*ByKey`
/// helpers, `IntrinsicReprKeys` / `IntrinsicForwardRepr`); to recognise a well-known
/// intrinsic, match the KEY (`IntrinsicTypePatterns`).
[<Struct>]
type DisplayName = | DisplayName of string

/// Where a resolved symbol physically lives — enough for codegen to mint a ref
/// without re-resolving. It is the `key -> home` ORACLE: a `SymbolKey` names *what* a
/// symbol is, and the shape a provider resolves for that key carries, here, *where* it
/// is. Nothing else in the pipeline knows a symbol's assembly.
///
/// A symbol's DECLARING TYPE is not here: it is `MemberKey.Decl : TypeKey`, on the
/// key itself. An origin names a *place* (assembly + namespace); containment is the
/// key's job.
type SymbolOrigin =
    {
        Home: Origin
        Namespace: NamespaceKey
    }

    /// The default carried by symbols that don't (yet) record an origin —
    /// project-local / pre-stamp placeholder, global namespace.
    static member Empty =
        {
            Home = Origin.Unstamped
            Namespace = NamespaceKey.Global
        }

/// How an external member is STORED/accessed — the storage-and-shape axis,
/// orthogonal to the key-identity `MemberKind` above (which interns vtable slots).
/// `Field` and `Property` are both *value members* (no parameters, value in the
/// signature's `Return`); they diverge only at CLR emission — a `Field` reads via a
/// `FieldRef` + `ldfld`/`ldsfld`, a `Property` via its `get_X` getter `MemberRef` +
/// `call`. On JS both are a plain value access (a `Field` adds only `readonly`
/// fidelity, not yet modelled). `Method` is an arrow member (`call`/JS call).
/// Consumers that only care about value-vs-arrow read `IsValueMember` (here, or the
/// forwarding `ExternalMember.IsValueMember` / `ResolvedExternalMember.IsValueMember`)
/// rather than matching this directly.
[<RequireQualifiedAccess>]
type MemberStorage =
    | Field
    | Property
    | Method

    /// A value member (`Field`/`Property` — no parameters, value in the signature's
    /// `Return`) vs an arrow `Method`. The canonical value-vs-arrow predicate; the
    /// `ExternalMember` / `ResolvedExternalMember` members of the same name forward here.
    member s.IsValueMember = s <> MemberStorage.Method

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
    /// typars. Composes with `FTOr` (`FTOr [FTLiteral "ping"; FTLiteral "pong"]`) and
    /// ERASES to its base primitive on both backends.
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
    /// Typar #`index` of the generalized scheme bound at `binder` — a body-local
    /// `let`'s OWN scheme. The root is NOT free: it is BOUND, just by a binder that
    /// is not the enclosing method. `let g = fun x -> x` inside a decl is its own
    /// declaration with its own generalized scheme; `Elaborate.mkMethodQuantEnv`
    /// fails to map `g`'s root not because the root is unbound but because it is
    /// looking at the WRONG binder's axis (it derives its remap by walking the
    /// ENCLOSING decl's type, in which `g`'s own root does not occur — every use of
    /// `g` instantiates away from it). So the leaf names the scheme that binds it.
    /// `index` is scoped to `binder` (position within that local scheme, minted by
    /// `freeze` in first-occurrence pre-order — the same single-minting-point
    /// discipline `FTTypar` indices follow), so two distinct local schemes cannot
    /// collide even before their binders are compared. NEVER equate two local
    /// typars by anything other than the `(binder, index)` PAIR.
    ///
    /// Carrying `binder` preserves an association a future GENERIC-CLOSURE lowering
    /// needs, rather than erasing it and forcing it to be reconstructed. Real F#
    /// compiles `let f () = let g = fun x -> x in (g, g)` to `f<'a,'b>` (its two
    /// USE-SITE instantiations, implicitly generalized onto `f`'s own method typar
    /// list) plus a separate GENERIC closure class `g@2T<'c>` for `g`'s own root —
    /// it does NOT append `'c` to `f`'s typars, which would change `f`'s ABI and
    /// force callers to pass a third type argument. `binder` is the handle on that
    /// separate home.
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
    /// **`binder` is BODY-RELATIVE and must never be resolved against anything.**
    /// A `NodeKey` is `(offset, kind)` with NO file id (`NodeKey.fs`), so keys from
    /// different files collide freely — deliberately: cross-file references resolve
    /// by NAME against prior views, never by `NodeKey`. This leaf is safe under that
    /// rule, and stays safe only if the following hold:
    ///
    /// - It is the SAME CLASS of key a frozen body already carries: every
    ///   `TPatG.NamedSimple(k, …)` inside an inline body is a file-local `NodeKey`
    ///   that already crosses the package boundary, and `Inline.freshen` rewrites
    ///   them at the splice (what the `SynthPreFreezeInline` kind exists for).
    /// - It is interpreted only against the TEMPLATE that carries it, exactly as
    ///   `FTTypar`'s index is. Two leaves from different units comparing structurally
    ///   equal is no more a bug than `FTTypar(Declaring, 0)` from two units doing so.
    /// - It is CONSUMED AT THAW: the leaf becomes a fresh consumer-owned `TyVar` and
    ///   the key does not survive into the spliced tree.
    ///
    /// Therefore: NEVER use `binder` for cross-file (or any) resolution, and NEVER
    /// merge it into a `NodeKey`-keyed side table. It identifies a scheme WITHIN one
    /// frozen body and nothing else.
    | FTLocalTypar of binder: NodeKey * index: int
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

/// Mutually recursive with TypeVar — every TyVar is a pointer into the
/// union-find graph. Will grow to include generics, units.
type SemType =
    /// Call UnionFind.find then read the representative's Link to dereference.
    | TyVar of TypeVar
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
    /// Default-constraint chain for this TyVar. Built from
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

/// The variance a `FrozenType` position carries, threaded by `FrozenType.mapVariant`:
/// COVARIANT (a value read / result), CONTRAVARIANT (a parameter), INVARIANT (a
/// generic type ARGUMENT — a slot that admits both reads and writes, so neither the
/// covariant nor the contravariant face alone is sound for it). A general type-system
/// concept, not a backend one — the walk names no concrete type; a caller's leaf owns
/// any policy.
[<RequireQualifiedAccess>]
type Variance =
    | Co
    | Contra
    | Inv

    /// Flip co/contra; invariant is self-dual. Applied at each `FTFun` DOMAIN — a
    /// parameter position inverts the enclosing variance.
    member this.Flip =
        match this with
        | Variance.Co -> Variance.Contra
        | Variance.Contra -> Variance.Co
        | Variance.Inv -> Variance.Inv

/// One-level structural walks over `FrozenType`'s DIRECT children — THE answer to
/// the "every new constructor fans out into N hand-written walker arms" tax: a
/// generic walk keeps only its leaf-specific arms and delegates every
/// child-carrying case here, so the next constructor addition touches this module
/// instead of twenty walk sites. Walks with per-arm SEMANTICS (encoders,
/// renderers, `freeze`) stay explicit exhaustive matches by design — these
/// skeletons are only for walks where child recursion is definitionally correct
/// for any child-carrying arm.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FrozenType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf returns unchanged.
    /// `FTOr` rebuilds through `MkUnion` — a mapped member set can collapse or
    /// splice — so every mapping walk inherits the canonical-form invariant
    /// STRUCTURALLY instead of by per-site convention.
    let mapChildren (f: FrozenType -> FrozenType) (t: FrozenType) : FrozenType =
        match t with
        | FTConst(key, args) -> FTConst(key, EqArray.map f args)
        | FTFun(arg, result) -> FTFun(f arg, f result)
        | FTTuple items -> FTTuple(EqArray.map f items)
        | FTRecord(key, args) -> FTRecord(key, EqArray.map f args)
        | FTUnion(key, args) -> FTUnion(key, EqArray.map f args)
        | FTClass(key, args) -> FTClass(key, EqArray.map f args)
        | FTOr members -> FrozenType.MkUnion(seq { for m in members -> f m })
        | FTKeyOf ty -> FTKeyOf(f ty)
        | FTIndexedAccess(objTy, index) -> FTIndexedAccess(f objTy, f index)
        | FTConditional c ->
            FTConditional
                {
                    Check = f c.Check
                    Extends = f c.Extends
                    WhenTrue = f c.WhenTrue
                    WhenFalse = f c.WhenFalse
                }
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> t

    /// Variance-tracking rebuild — the reusable skeleton for any walk whose per-arm
    /// action depends on POSITION (a covariant value read vs a contravariant parameter
    /// vs an invariant generic slot). `leaf v node` is consulted FIRST at every node:
    /// `ValueSome replacement` replaces `node` at variance `v` and STOPS the recursion
    /// (the leaf owns that subtree); `ValueNone` recurses under the structural variance
    /// rule. That rule is the type system's own and is FIXED here so no variance-sensitive
    /// walk re-derives it: variance FLIPS at each `FTFun` domain (a parameter is
    /// contravariant) and is KEPT for the result; a nominal / applied-constructor type
    /// ARGUMENT drops to INVARIANT (a generic slot admits both reads and writes);
    /// structural operators (tuple, anonymous union, keyof, indexed access, conditional)
    /// CARRY the enclosing variance into their children. Unlike `mapChildren`, the arms
    /// carry SEMANTICS (the variance decision), so — apart from the structural-operator
    /// arm, whose children are unconditionally same-variance — this is an EXHAUSTIVE match
    /// with no `mapChildren` catch-all: a new child-carrying constructor must force a
    /// variance decision here rather than silently inherit the enclosing one.
    let rec mapVariant (leaf: Variance -> FrozenType -> FrozenType voption) (v: Variance) (t: FrozenType) : FrozenType =
        match leaf v t with
        | ValueSome replaced -> replaced
        | ValueNone ->
            match t with
            // A parameter is contravariant: flip for the domain, keep variance for the result.
            | FTFun(a, b) -> FTFun(mapVariant leaf v.Flip a, mapVariant leaf v b)
            // Nominal / applied-constructor type ARGUMENTS are invariant slots.
            | FTConst(key, args) -> FTConst(key, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTClass(k, args) -> FTClass(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTRecord(k, args) -> FTRecord(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTUnion(k, args) -> FTUnion(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            // Structural operators carry the ENCLOSING variance into their children.
            | FTTuple _
            | FTOr _
            | FTKeyOf _
            | FTIndexedAccess _
            | FTConditional _ -> mapChildren (mapVariant leaf v) t
            // Childless leaves.
            | FTEnum _
            | FTLiteral _
            | FTTypar _
            | FTLocalTypar _
            | FTUnknown _ -> t

    let iterChildren (f: FrozenType -> unit) (t: FrozenType) : unit =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> EqArray.iter f args
        | FTFun(arg, result) ->
            f arg
            f result
        | FTTuple items -> EqArray.iter f items
        | FTOr members -> EqSet.iter f members
        | FTKeyOf ty -> f ty
        | FTIndexedAccess(objTy, index) ->
            f objTy
            f index
        | FTConditional c ->
            f c.Check
            f c.Extends
            f c.WhenTrue
            f c.WhenFalse
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> ()

    /// `p` holds for EVERY direct child (vacuously true at a leaf). Short-circuits.
    let forallChildren (p: FrozenType -> bool) (t: FrozenType) : bool =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> EqArray.forall p args
        | FTFun(arg, result) -> p arg && p result
        | FTTuple items -> EqArray.forall p items
        | FTOr members -> EqSet.forall p members
        | FTKeyOf ty -> p ty
        | FTIndexedAccess(objTy, index) -> p objTy && p index
        | FTConditional c -> p c.Check && p c.Extends && p c.WhenTrue && p c.WhenFalse
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> true

    /// `p` holds for SOME direct child (vacuously false at a leaf). Short-circuits.
    let existsChild (p: FrozenType -> bool) (t: FrozenType) : bool =
        not (forallChildren (fun c -> not (p c)) t)

    /// True when `a` and `b` present the SAME head — same case, and for a nominal
    /// the same `key`; child structure is ignored (that is what the pairwise descent
    /// recovers). An `FTTypar` is a WILDCARD that heads-matches anything: an open
    /// template slot accepts any instantiated shape. Used to test whether an `FTOr`'s
    /// members line up POSITIONALLY, and to head-key the fallback pairing when they do
    /// not.
    let private sameHead (a: FrozenType) (b: FrozenType) : bool =
        match a, b with
        | FTTypar _, _
        | _, FTTypar _ -> true
        | FTConst(k1, _), FTConst(k2, _) -> k1 = k2
        | FTRecord(k1, _), FTRecord(k2, _)
        | FTUnion(k1, _), FTUnion(k2, _)
        | FTClass(k1, _), FTClass(k2, _) -> k1 = k2
        | FTEnum k1, FTEnum k2 -> k1 = k2
        | FTFun _, FTFun _ -> true
        | FTTuple _, FTTuple _ -> true
        | FTOr _, FTOr _ -> true
        | FTLiteral v1, FTLiteral v2 -> v1 = v2
        | FTKeyOf _, FTKeyOf _ -> true
        | FTIndexedAccess _, FTIndexedAccess _ -> true
        | FTConditional _, FTConditional _ -> true
        | FTUnknown n1, FTUnknown n2 -> n1 = n2
        // NOT a wildcard (unlike `FTTypar`): a local typar is an identity-bearing
        // leaf that no argument vector instantiates, so it only heads-matches the
        // same `(binder, index)` PAIR — the leaf-identity rule `FTUnknown`/`FTLiteral`
        // follow. Never equate two local typars by index alone.
        | FTLocalTypar(b1, i1), FTLocalTypar(b2, i2) -> b1 = b2 && i1 = i2
        | _ -> false

    /// PAIRWISE descent: when `a` and `b` share the same head (same case, same
    /// child count — nominal KEYS are deliberately not compared, mirroring the
    /// open-vs-instantiated template matching this serves), invoke `f` on each
    /// corresponding child pair; any head mismatch is a silent no-op (the caller
    /// decides what a mismatch means). `FTOr` members are a SET (`EqSet`), so their
    /// storage order is NOT a semantic invariant across instantiation. When the
    /// members line up positionally (the common case — instantiation maps in order),
    /// pair by position; otherwise pair each open member to the instantiated member
    /// sharing its HEAD KEY (a wildcard `FTTypar` open member takes any leftover). A
    /// concrete open member whose head matches TWO unused instantiated members is
    /// genuinely ambiguous — fail loudly rather than guess; no match declines
    /// silently (like a head mismatch). A length mismatch declines wholesale.
    let iterChildren2 (f: FrozenType -> FrozenType -> unit) (a: FrozenType) (b: FrozenType) : unit =
        let pairwise (xs: EqArray<FrozenType>) (ys: EqArray<FrozenType>) =
            if xs.Length = ys.Length then
                for i in 0 .. xs.Length - 1 do
                    f xs.[i] ys.[i]

        match a, b with
        | FTFun(a1, r1), FTFun(a2, r2) ->
            f a1 a2
            f r1 r2
        | FTTuple xs, FTTuple ys
        | FTConst(_, xs), FTConst(_, ys)
        | FTRecord(_, xs), FTRecord(_, ys)
        | FTUnion(_, xs), FTUnion(_, ys)
        | FTClass(_, xs), FTClass(_, ys) -> pairwise xs ys
        | FTOr xs, FTOr ys when xs.Length = ys.Length ->
            let n = xs.Length
            let mutable positionalOk = true

            for i in 0 .. n - 1 do
                positionalOk <- positionalOk && sameHead xs.[i] ys.[i]

            if positionalOk then
                for i in 0 .. n - 1 do
                    f xs.[i] ys.[i]
            else
                // Members were reordered (or freshly re-set-ified) by instantiation:
                // recover the pairing by head key instead of trusting position.
                let used = Array.zeroCreate<bool> n
                let wildcards = ResizeArray<FrozenType>()

                for i in 0 .. n - 1 do
                    match xs.[i] with
                    | FTTypar _ -> wildcards.Add xs.[i]
                    | x ->
                        let candidates =
                            [
                                for j in 0 .. n - 1 do
                                    if not used.[j] && sameHead x ys.[j] then
                                        yield j
                            ]

                        match candidates with
                        | [ j ] ->
                            used.[j] <- true
                            f x ys.[j]
                        | [] -> () // no partner: decline, mirroring a head mismatch
                        | _ ->
                            failwithf
                                "FrozenType.iterChildren2: ambiguous FTOr member pairing — open member %A matches multiple instantiated members in %A"
                                x
                                ys

                // Leftover instantiated members go to the wildcard open members. With a
                // SINGLE wildcard (the only shape any producer reaches today) this is
                // exact. With TWO+ wildcards the pairing is index-order ARBITRARY — head
                // keys can't disambiguate one bare typar from another — so if a future
                // reachable producer can emit a reordered `FTOr` with multiple bare-typar
                // members, this needs a real assignment, not first-come.
                let mutable wi = 0

                for j in 0 .. n - 1 do
                    if not used.[j] && wi < wildcards.Count then
                        f wildcards.[wi] ys.[j]
                        wi <- wi + 1
        | FTKeyOf x1, FTKeyOf x2 -> f x1 x2
        | FTIndexedAccess(o1, i1), FTIndexedAccess(o2, i2) ->
            f o1 o2
            f i1 i2
        | FTConditional c1, FTConditional c2 ->
            f c1.Check c2.Check
            f c1.Extends c2.Extends
            f c1.WhenTrue c2.WhenTrue
            f c1.WhenFalse c2.WhenFalse
        | _ -> ()

/// SemType-level active patterns that read naturally in `match` arms, auto-opened
/// with the rest of `SemanticInfo`.
[<AutoOpen>]
module SemTypePatterns =

    /// A NOMINAL registry type that carries type arguments and can hold instance
    /// members — class, union, OR record. NOT `TyEnum` (niladic, no `args`) and NOT
    /// `TyConst` (an intrinsic head, not a member-bearing registry type). Yields the
    /// declaring `TypeKey` and the receiver's type arguments.
    ///
    /// This is the single unification vehicle for kind-blind instance-member
    /// dispatch: a `match` arm on `TyNominal(key, args)` treats the three kinds
    /// identically (the declaring-key lookup, the member-key registry read, the
    /// `MethodCall`/`PropertyGet` lowering), and every such site is greppable. The
    /// three cases stay DISTINCT in the representation — this pattern is the only
    /// sanctioned way to say "these three, identically", and it keeps the arms
    /// source-compatible with a future real `TyNominal` DU case (only construction
    /// sites would change). A site where a kind adds behaviour ON TOP of member
    /// dispatch — a record's field-by-name access, or a construction / tag / field
    /// site where the kind genuinely forks — keeps its explicit
    /// `TyRecord`/`TyUnion`/`TyClass` arm, ordered BEFORE this one so the
    /// kind-specific behaviour wins. Does NOT zonk — match on an already-resolved
    /// type (`Unification.zonk` first where the receiver may be a link).
    [<return: Struct>]
    let (|TyNominal|_|) (ty: SemType) : struct (TypeKey * EqArray<SemType>) voption =
        match ty with
        | TyClass(key, args)
        | TyUnion(key, args)
        | TyRecord(key, args) -> ValueSome(struct (key, args))
        | _ -> ValueNone

/// `SemType` sibling of the `FrozenType` child-walk module above — the same
/// one-level skeletons, PURELY structural: no `resolveStep`/`zonk` here (a walk
/// dispatches on its own resolved view first, then delegates the child-carrying
/// remainder). `TyVar` is a leaf from this module's viewpoint.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SemType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf (incl. `TyVar`)
    /// returns unchanged. `TyOr` rebuilds through `UnionMembers.Map`/`MkUnion`, so
    /// every mapping walk inherits the canonical-form invariant structurally.
    let mapChildren (f: SemType -> SemType) (t: SemType) : SemType =
        match t with
        | TyConst(name, args) -> TyConst(name, EqArray.map f args)
        | TyFun(arg, result) -> TyFun(f arg, f result)
        | TyTuple items -> TyTuple(EqArray.map f items)
        | TyRecord(key, args) -> TyRecord(key, EqArray.map f args)
        | TyUnion(key, args) -> TyUnion(key, EqArray.map f args)
        | TyClass(key, args) -> TyClass(key, EqArray.map f args)
        | TyOr members -> members.Map f
        | TyKeyOf ty -> TyKeyOf(f ty)
        | TyIndexedAccess(objTy, index) -> TyIndexedAccess(f objTy, f index)
        | TyConditional c ->
            TyConditional
                {
                    Check = f c.Check
                    Extends = f c.Extends
                    WhenTrue = f c.WhenTrue
                    WhenFalse = f c.WhenFalse
                }
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> t

    let iterChildren (f: SemType -> unit) (t: SemType) : unit =
        match t with
        | TyConst(_, args)
        | TyRecord(_, args)
        | TyUnion(_, args)
        | TyClass(_, args) -> EqArray.iter f args
        | TyFun(arg, result) ->
            f arg
            f result
        | TyTuple items -> EqArray.iter f items
        | TyOr members -> EqSet.iter f members.Members
        | TyKeyOf ty -> f ty
        | TyIndexedAccess(objTy, index) ->
            f objTy
            f index
        | TyConditional c ->
            f c.Check
            f c.Extends
            f c.WhenTrue
            f c.WhenFalse
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> ()

    /// `p` holds for EVERY direct child (vacuously true at a leaf). Short-circuits.
    let forallChildren (p: SemType -> bool) (t: SemType) : bool =
        match t with
        | TyConst(_, args)
        | TyRecord(_, args)
        | TyUnion(_, args)
        | TyClass(_, args) -> EqArray.forall p args
        | TyFun(arg, result) -> p arg && p result
        | TyTuple items -> EqArray.forall p items
        | TyOr members -> EqSet.forall p members.Members
        | TyKeyOf ty -> p ty
        | TyIndexedAccess(objTy, index) -> p objTy && p index
        | TyConditional c -> p c.Check && p c.Extends && p c.WhenTrue && p c.WhenFalse
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> true

    /// `p` holds for SOME direct child (vacuously false at a leaf). Short-circuits.
    let existsChild (p: SemType -> bool) (t: SemType) : bool =
        not (forallChildren (fun c -> not (p c)) t)

/// The `SemType` ↔ `FrozenType` bridge. `toFrozen` is
/// the Edge-A sink-side conversion; `ofFrozen` its inverse. On the *post-freeze*
/// `SemType` subset (`{TyConst, TyFun, TyTuple, TyRecord, TyUnion, TyClass,
/// TyTypar, TyUnknown}`) the two are mutual inverses — the cases are 1:1 with
/// `{FTConst, FTFun, FTTuple, FTRecord, FTUnion, FTClass, FTTypar, FTUnknown}`.
/// `TyVar` is the sole case with no `FrozenType` counterpart (the point of the
/// split): `toFrozen` rejects it with a hard error mirroring `ClrEncoder`'s
/// existing `cannot encode SemType: TyVar` crash, so a stray metavar fails here
/// — one hop out from where the catch-all failed before. AutoOpen so the
/// boundary callers can wrap a `.ty` in `toFrozen` unqualified.
[<AutoOpen>]
module FrozenTypeBridge =
    /// `toFrozen` with the `TyVar` leaf as a POLICY parameter — the single
    /// `SemType -> FrozenType` structural fold; `toFrozen` (hard error) and
    /// `Elaborate.freezeTy`'s documented-temporary lenient placeholder are its two
    /// instantiations, so the fold body cannot drift between them.
    let rec toFrozenWith (onVar: SemType -> FrozenType) (ty: SemType) : FrozenType =
        let go = toFrozenWith onVar

        match ty with
        | TyConst(key, args) -> FTConst(key, EqArray.map go args)
        | TyFun(arg, result) -> FTFun(go arg, go result)
        | TyTuple items -> FTTuple(EqArray.map go items)
        | TyRecord(key, args) -> FTRecord(key, EqArray.map go args)
        | TyUnion(key, args) -> FTUnion(key, EqArray.map go args)
        | TyClass(key, args) -> FTClass(key, EqArray.map go args)
        // Enums are niladic nominals (no args, no typars) — a pure key carry-over.
        | TyEnum key -> FTEnum key
        // Rebuild through the smart constructor — freezing members can collapse the
        // set (two distinct `SemType` members freezing equal), so never a raw map.
        | TyOr members -> FrozenType.MkUnion(seq { for m in members.Members -> go m })
        | TyLiteral v -> FTLiteral v
        // The type-level computations carry across as inert nodes; their children
        // freeze structurally (a still-open method var lands on the `onVar` policy).
        | TyKeyOf t -> FTKeyOf(go t)
        | TyIndexedAccess(objTy, index) -> FTIndexedAccess(go objTy, go index)
        | TyConditional c ->
            FTConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | TyTypar(axis, index) -> FTTypar(axis, index)
        | TyUnknown name -> FTUnknown name
        | TyVar _ -> onVar ty

    /// `SemType -> FrozenType`. Total on the post-freeze subset; a hard error on
    /// `TyVar` (an inference metavar must never reach the frozen boundary).
    let toFrozen (ty: SemType) : FrozenType =
        toFrozenWith (fun v -> failwithf "FrozenType.toFrozen: cannot freeze SemType: %A" v) ty

    /// Realise a `FrozenType` template, resolving its open typars via the three
    /// supplied callbacks: `declaring i` yields the declaring type's i-th arg;
    /// `methodVar j` yields the method axis's j-th instantiation; `localTypar binder
    /// k` yields the realisation of typar #`k` of the local scheme bound at `binder`
    /// (`FTLocalTypar`, which — unlike the two declared axes — is NOT a position in
    /// any argument vector, so its policy can only MINT, never index; and which must
    /// be keyed on the `(binder, index)` PAIR, never the index alone). Every other
    /// case maps structurally. Callers that span more than one template of the *same*
    /// signature (a split parameter/return `ExternalSignature`) must share one
    /// `methodVar` memo so a repeated method index resolves to the same var across
    /// the whole signature; the same holds for `localTypar` across a thawed decl.
    /// `ofFrozen` is the identity case (both declared placeholders map straight
    /// back to their `TyTypar` markers).
    let rec instantiateWith
        (declaring: int -> SemType)
        (methodVar: int -> SemType)
        (localTypar: NodeKey -> int -> SemType)
        (template: FrozenType)
        : SemType =
        let go = instantiateWith declaring methodVar localTypar

        match template with
        | FTConst(key, args) -> TyConst(key, EqArray.map go args)
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(EqArray.map go items)
        | FTRecord(key, args) -> TyRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> TyUnion(key, EqArray.map go args)
        | FTClass(key, args) -> TyClass(key, EqArray.map go args)
        // Enums carry no args/typars — the key passes straight through both ways.
        | FTEnum key -> TyEnum key
        // Build through `MkUnion`, not a raw `TyOr`: realising members can collapse
        // the set (a typar member instantiating to another member), and `MkUnion` is
        // the sole producer.
        | FTOr members -> SemType.MkUnion(seq { for m in members -> go m })
        // A literal is a ground leaf — no typars to resolve, maps straight across.
        | FTLiteral v -> TyLiteral v
        // The type-level computations realise their children (which may carry the
        // declaring/method placeholders) but are NOT evaluated here — carried inert.
        | FTKeyOf t -> TyKeyOf(go t)
        | FTIndexedAccess(objTy, index) -> TyIndexedAccess(go objTy, go index)
        | FTConditional c ->
            TyConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | FTTypar(TyparAxis.Declaring, i) -> declaring i
        | FTTypar(TyparAxis.Method, j) -> methodVar j
        | FTLocalTypar(binder, k) -> localTypar binder k
        | FTUnknown name -> TyUnknown name

    /// `FrozenType -> SemType`. Total — every `FrozenType` case has a `SemType`
    /// counterpart (`FTTypar` lands on the post-freeze-only `TyTypar`). The
    /// identity realisation of `instantiateWith`: each DECLARED placeholder maps
    /// straight back to its self-describing `TyTypar` marker.
    ///
    /// `FTLocalTypar` is the one arm with no marker to map to — `SemType` has no
    /// local-typar case — so it MINTS a fresh unlinked `TyVar`, memoised per
    /// `(binder, index)` PAIR so repeated occurrences of one local typar share a
    /// cell across the realised template. So `ofFrozen` is not cell-free in that
    /// arm; the contract it actually owes is intact, because the cells it mints are
    /// the CALLER's, never a producer's (nothing on the other side of a frozen
    /// boundary can hold a reference to one).
    let ofFrozen (ft: FrozenType) : SemType =
        let localCache =
            System.Collections.Generic.Dictionary<struct (NodeKey * int), SemType>()

        instantiateWith
            (fun i -> TyTypar(TyparAxis.Declaring, i))
            (fun j -> TyTypar(TyparAxis.Method, j))
            (fun binder k ->
                let key = struct (binder, k)

                match localCache.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = TyVar(TypeVar())
                    localCache.[key] <- v
                    v
            )
            ft

    // A `FrozenType` template is an external descriptor's body with its open
    // typars baked as `FTTypar(Declaring,i)` / `FTTypar(Method,j)` placeholders.
    // The realiser family below resolves declaring placeholders to the caller's
    // fresh declaring args and method placeholders to fresh metavars, all over
    // the shared `instantiateWith` walk. It is the data form of the legacy
    // `SemType[] -> SemType` closures (`BuildSignature` / `BuildType` / …):
    // inference reads templates here, codegen reads them directly. Constraint
    // stamping is NOT part of this — it stays in `ExternalSymbols.instantiateSymbol`,
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

    /// The `localTypar` policy for a SIGNATURE / type-shape template. Such a template
    /// describes a DECLARED type, and an `FTLocalTypar` only ever arises inside a
    /// decl's BODY (a body-local `let`'s own generalized scheme) — never in the decl's
    /// own type, which is exactly why `mkMethodQuantEnv` cannot map it to a declared
    /// axis. So one reaching a template realiser is a producer bug: fail loud rather
    /// than fabricate a var, mirroring the method-axis arm of `instantiateDeclaring`.
    /// Only a realiser of a whole frozen BODY (the inline-splice thaw) supplies a
    /// minting policy.
    let localTyparInTemplate (site: string) (binder: NodeKey) (k: int) : SemType =
        failwithf "%s: unexpected body-local typar %d of scheme %O in a signature template" site k binder

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
            (localTyparInTemplate "FrozenTypeBridge.instantiateDeclaring")
            template

    /// The largest declaring-typar index a template references, or `-1` if it
    /// references none. `freezeMemberSig` uses this to DROP a member whose
    /// signature names a typar beyond the declaring type's arity
    /// (`maxDeclaringIndex >= declaringTyparArity`): such a member can't be instantiated
    /// from the receiver's declaring args alone, so it's removed rather than
    /// surfaced with an unrealisable slot. This is a policy choice — drop vs.
    /// degrade — not crash-avoidance: both realisers (`instantiateDeclaring`,
    /// `substituteDeclaring`) degrade an out-of-range declaring index to `Unknown`
    /// on their own. A method typar is a producer bug here (type-shape / contract
    /// templates carry no method axis).
    let rec maxDeclaringIndex (template: FrozenType) : int =
        match template with
        | FTTypar(TyparAxis.Declaring, i) -> i
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.maxDeclaringIndex: unexpected method typar %d in a type-shape template" j
        | t ->
            let mutable m = -1
            FrozenType.iterChildren (fun c -> m <- max m (maxDeclaringIndex c)) t
            m

    /// Split a freshly-translated member signature's single typar axis into the
    /// declaring + method axes. The contract-extraction translate (`translateType`)
    /// bakes EVERY typar on the `Declaring` axis — it threads one `TyparCollector`
    /// with no axis notion. A member's collector is seeded with the declaring type's
    /// own typars (indices `0 .. declaringTyparArity-1`) before its signature is walked,
    /// so any typar the member INTRODUCES — explicit `<'a>` or an implicit `'T`
    /// (`Formatter.AppendFormatted: 'T -> unit`) — lands at index `>= declaringTyparArity`.
    /// Those are the member's OWN generic parameters: rewrite each to
    /// `FTTypar(Method, i - declaringTyparArity)`, leaving the genuine declaring typars
    /// untouched. The `.fsi` analogue of `Elaborate.freezeTypars`' `methodEnv` flip;
    /// the single point that gives an extracted member its method axis (so codegen
    /// reads a real `MethodTyparArity` and mints the `MethodSpec`'s generic params).
    let rec reaxisMethodTypars (declaringTyparArity: int) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) when i >= declaringTyparArity ->
            FTTypar(TyparAxis.Method, i - declaringTyparArity)
        // Child recursion reaches a member-introduced typar buried in ANY child —
        // `keyof`/indexed/conditional included; `mapChildren` routes `FTOr` through
        // `MkUnion`, keeping the every-rebuild-canonicalises invariant.
        | t -> FrozenType.mapChildren (reaxisMethodTypars declaringTyparArity) t

    /// `true` when the type is fully ground: no open typar on either axis, no
    /// body-local free typar, and no `FTUnknown` (a leaked inference metavar the
    /// front end never resolved). The `FrozenType` sibling of
    /// `Passes.InlineExpansion`'s `SemType` `isGroundType`.
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        // A body-local scheme's own root is open in exactly the sense that
        // matters here: nothing at a use site has instantiated it.
        | FTLocalTypar _
        | FTUnknown _ -> false
        // Every other node is ground iff every child is (vacuously ground leaves
        // included) — an open typar in any child keeps the whole node non-ground.
        | t -> FrozenType.forallChildren ftIsGround t

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
        match template with
        | FTTypar(TyparAxis.Declaring, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown "<abbrev-arity-mismatch>"
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected method typar %d in a type-shape template" j
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The shared tail of the project-local and external seq-interface witnesses
    /// (`EmitResolve.tryInterfaceWitness` / `ClrRecipes.tryExternalInterfaceWitness`):
    /// find the impl whose compiled name equals `target` (a `qualifiedName`) among
    /// `ifaces` (each `(compiled-name, args-over-declaring-typars)`) and return its
    /// args instantiated at THIS receiver — `FTTypar(Declaring,i) := declArgs.[i]`
    /// via `substituteDeclaring`. `ValueNone` if none matches. Each head reads its
    /// own registry (`env.Classes` vs the codegen symbol provider) and adapts it to
    /// the `(name, args)` shape; this picks + substitutes so the two can't drift.
    let pickInterfaceWitness
        (target: string)
        (declArgs: FrozenType[])
        (ifaces: (string * FrozenType[]) seq)
        : EqArray<FrozenType> voption =
        match
            ifaces
            |> Seq.tryPick (fun (iname, ifaceArgs) ->
                if iname = target then
                    Some(ifaceArgs |> Array.map (substituteDeclaring declArgs) |> EqArray.ofArray)
                else
                    None
            )
        with
        | Some ia -> ValueSome ia
        | None -> ValueNone

/// `∀ Quantified . Body`. Built by `Unification.generalise` and stored in
/// `PassContext.Bindings.Scheme` keyed by the binding's headPat NodeKey. Each
/// `inferIdent` of a generalised binding instantiates the scheme — mints a
/// fresh TyVar at the current level for every entry in `Quantified` and
/// walks `Body` substituting them, so independent use sites get independent
/// variables. Mirrors `ExternalSymbols.instantiateSymbol` for the finitely many
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
/// that carries it is declared before `Tast.fs` in the compile order.
///
/// GENERIC over the type domain, exactly like the clause (`TStaticOptClauseG`)
/// that carries it, so it rides `TastConvert`'s freeze/thaw conversions rather
/// than being copied verbatim across them. On the producer side (`'ty = SemType`,
/// the `PassContext.StaticOpt` side table and the pre-freeze tree) the typar is a
/// `TyVar` over the inline binding's quantified root, so `Inline.inlineExpand`'s
/// typar substitution turns it into the call site's concrete type before the
/// clause is tested. In the frozen domain (`'ty = FrozenType`) it is the same
/// constraint with that root quantified — the whole point being that a frozen
/// clause carries NO `UnionFind` cell, so it can cross an assembly boundary.
[<RequireQualifiedAccess>]
type TStaticOptConstraintG<'ty> =
    /// `when ^T : SomeType` — holds when the type substituted for `typar` equals
    /// `required`. The catch-all `when ^T : ^T` is this case with `required`
    /// equal to `typar`, so after substitution both sides are the same concrete
    /// type and it matches unconditionally.
    | TyconEquals of typar: 'ty * required: 'ty
    /// `when ^T : struct` — holds when the substituted `typar` is a value type.
    | IsStruct of typar: 'ty

/// The producer-domain (inference-side) static-optimization constraint — what the
/// `PassContext.StaticOpt` side table and the pre-freeze TAST carry.
type TStaticOptConstraint = TStaticOptConstraintG<SemType>

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
    /// (single element-TyVar shared by every item); Elaborate projects the
    /// chain into nested `TExpr.UnionCons("Cons", [hd; tl])` /
    /// `UnionCons("Nil", [])` nodes.
    | ListLiteral
    /// On an `Expr.EnclosedBlock(ParenKind.Array, …)` /
    /// `Expr.EmptyBlock(ParenKind.Array, …)` node — `[|1; 2; 3|]` or
    /// `[||]`. Same element-typing rule as `ListLiteral`; Elaborate wraps
    /// the lowered list chain in an `Array.ofList` external call so
    /// the same nested `UnionCons` shape feeds both literal forms.
    | ArrayLiteral
    /// On an `Expr.InfixApp(_, ::, _)` node — cons construction `h :: t`. The
    /// `::` operator is not a provider-resolved function (unlike `+`/`|>`); it
    /// builds the list union directly. Unification types `h :: t` as the list
    /// type carrying `h`'s element type (`tail` unified to the same list);
    /// Elaborate projects it to `TExpr.UnionCons("Cons", [hd; tl])` against the
    /// resolved list union — the same shape `ListLiteral` lowers to.
    | ConsExpr
