namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Where a resolved symbol physically lives — enough for codegen to mint a ref
/// without re-resolving. `Assembly` is a simple name keyed into a `ProjectInfo`'s
/// resolved reference set; `None` ⇒ defined in the project being compiled. See
/// symbol-resolution-plan §4.
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

/// Platform-agnostic, scope-unambiguous symbol identity (symbol-resolution-plan
/// §7.3). All strings/ints — never a CLR `EntityHandle` or `System.Type` (those
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

/// SRTP / trait / default constraint captured on an external symbol's typar
/// list. Member-trait clauses are recorded as opaque markers; default clauses
/// carry a SemBuilder over the symbol's typar list so `Instantiate` can stamp
/// the default target onto the freshly minted TyVar for generalisation-time
/// defaulting.
[<RequireQualifiedAccess>]
type ExternalConstraint =
    /// `when 'T : equality` etc. — directly stamps a `SemanticConstraint` on
    /// the fresh `TypeVar` allocated for the typar at instantiation time.
    | Trait of typarIndex: int * kind: SemanticConstraintKind
    /// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)` — SRTP
    /// member trait. `typarIndices` are the participating typar slots
    /// (the LHS of the trait). `memberName` is the compiled name. At
    /// `Instantiate` time the caller passes the fresh-TyVar array, and the
    /// closures produce the SemTypes describing the trait's expected member
    /// signature. The Unification pass drains the captured signature when any
    /// participating fresh TyVar is linked to a concrete shape — see
    /// `Unification.drainSrtpBounds`.
    | MemberTrait of
        typarIndices: EqArray<int> *
        memberName: string *
        buildArgTypes: (SemType[] -> SemType)[] *
        buildReturnType: (SemType[] -> SemType)
    /// `default ^T : <ty>` — typar defaulting at generalisation. The
    /// `buildTarget` closure takes the symbol's fresh-TyVar array (one
    /// entry per declared typar) and returns the target `SemType` —
    /// usually another fresh TyVar (`default ^T3 : ^T1`) or a concrete
    /// shape (`default ^T1 : int`). `Instantiate` stamps the resolved
    /// target onto the source TyVar's `Defaults` list so generalisation
    /// can chase the chain and pick the first concrete shape it reaches.
    | Default of typarIndex: int * buildTarget: (SemType[] -> SemType)
    /// `when 'e :> <ty>` — coercion. `buildTarget` takes the symbol's
    /// fresh-TyVar array and yields the required supertype; `Instantiate`
    /// stamps a `SemanticConstraintKind.Coercion` onto the constrained fresh
    /// TyVar so the first `Link` fires `checkConstraint`/`subsumes`.
    | Coercion of typarIndex: int * buildTarget: (SemType[] -> SemType)

type ExternalSymbol =
    {
        Name: string
        /// Returns a fresh instantiation of the symbol's type each call.
        /// `level` is the let-depth at which the instantiation happens; fresh
        /// TyVars must be stamped with it so Rémy's level-based generalisation
        /// can decide which to quantify. Monomorphic symbols return the same
        /// SemType every time and ignore the level. Polymorphic symbols
        /// allocate fresh TypeVars at `level` per call so independent use-sites
        /// don't unify with each other through the shared scheme.
        ///
        /// `Instantiate` is also responsible for applying any `Constraints` to
        /// the fresh TyVars it mints; callers don't drain the list separately.
        Instantiate: int -> SemType
        /// Empty for the overwhelming majority of symbols. Surfaced out of the
        /// closure for diagnostic introspection and to let future passes audit
        /// which constraints are still unimplemented.
        Constraints: ExternalConstraint list
        /// Where the symbol lives — the bridge to codegen (symbol-resolution-plan
        /// §4). `SymbolOrigin.Empty` until a resolving source fills it.
        Origin: SymbolOrigin
        /// Interned identity: a `SymbolKey.ValueKey` over the symbol's resolved
        /// origin + simple name (vesper-set-sprint-plan §0.1 / M1). Front-end
        /// passes write it into `Resolution.ExternalValue`; Freeze stamps it
        /// onto `TExpr.External` so codegen can do robust identity checks
        /// (e.g. "is this exactly `Vesper.Printf.printfn`?") instead of
        /// suffix-matching the source-written name.
        Key: SymbolKey
    }

/// Per-field shape inside an `ExternalTypeShape.Record`. Field types are
/// closure-builders parameterised over the enclosing type's typars: callers
/// pass an `SemType[]` (one entry per declared typar, in declaration order)
/// and the builder substitutes them through.
type ExternalFieldShape =
    {
        Name: string
        IsMutable: bool
        BuildType: SemType[] -> SemType
    }

/// Per-case shape inside an `ExternalTypeShape.Union`. `FieldNames` is
/// `ValueNone` for positional fields and `ValueSome name` for `of x: int`-
/// style named fields. The arrays line up: `FieldNames[i]` describes
/// `BuildFieldTypes[i]`'s source-side label.
type ExternalCaseShape =
    {
        Name: string
        FieldNames: string voption[]
        BuildFieldTypes: (SemType[] -> SemType)[]
    }

/// A resolved member (static/instance method or property getter) on an external
/// type (symbol-resolution-plan §4). `BuildSignature` is parameterised over the
/// *enclosing type's* typars, exactly like `ExternalFieldShape.BuildType`:
/// callers pass a `SemType[]` (one entry per declared typar) and the builder
/// substitutes them through, yielding the **tupled** `(p1 * … * pN) → ret`
/// signature (the .NET calling convention; arity ≤ 1 is unchanged — see
/// `MetadataMapping.tryMethodSignature`).
type ExternalMember =
    {
        Name: string
        IsStatic: bool
        IsProperty: bool
        BuildSignature: SemType[] -> SemType
        Origin: SymbolOrigin
        /// The interned identity (symbol-resolution-plan §7.2/§7.3): a
        /// `SymbolKey.MemberKey` over the *open* declaring type (its `argSig` in
        /// `!0`-typars), minted by the resolving source. Freeze stamps it into
        /// `TExpr.ExternalMember` so codegen reads the binding off the node.
        Key: SymbolKey
    }

/// Capability flags on an external class or interface (pre-sprint-recommendations
/// H2). The metadata layer reads them off the .NET `TypeAttributes` plus
/// `[<AllowNullLiteral>]` attribute decoding; the contract layer leaves them at
/// `Default` until a `.fsi` learns to publish them. B-1 (class emission) reads
/// `IsSealed` on the declared base type; B-8 (`[<AllowNullLiteral>]`) reads
/// `AllowNullLiteral` off both user-declared and external classes.
type ExternalClassFlags =
    {
        IsSealed: bool
        IsAbstract: bool
        AllowNullLiteral: bool
        /// `true` for a .NET value type (`struct`) — read off `Type.IsValueType`
        /// by the metadata layer. Codegen needs it to pick value-receiver emission
        /// (`ldloca` + `constrained.`/`call`) over reference `callvirt`; the duck-
        /// typed `for … in` over a struct enumerator (`List<'T>.Enumerator`) is the
        /// first consumer (vesper-set-sprint-phase-4 §4.4). Contract-layer providers
        /// leave it `false` (a `.fsi` doesn't yet publish struct-ness).
        IsValueType: bool
    }

    /// The conservative default the contract layer stamps when a `.fsi` only
    /// commits the type's name + arity + interface-ness.
    static member Default =
        {
            IsSealed = false
            IsAbstract = false
            AllowNullLiteral = false
            IsValueType = false
        }

/// The shape of an external class or interface (pre-sprint-recommendations H2).
/// Lifted out of `ExternalTypeShape.Class` so the DU header stays narrow and the
/// member set is reachable to consumers (B-2's `interface … with member …`
/// conformance check, B-1's base-type lookup, etc.) without having to round-trip
/// through `TryLookupMember` per name.
///
/// `Interfaces`, `BaseType`, and each member's `BuildSignature` are written over
/// the *declaring type's* typars, exactly like `ExternalFieldShape.BuildType`:
/// callers pass a `SemType[]` (one entry per declared typar, in declaration
/// order) and the builders substitute them through.
type ExternalClassShape =
    {
        Arity: int
        IsInterface: bool
        /// All public declared methods + properties whose signature maps via
        /// the §6.1 `tryBuildType`. Sibling members the metadata layer can't
        /// map (e.g. a member with its own generic params, or a by-ref
        /// parameter) are filtered out, not faked. Contract-layer providers
        /// leave this empty until the `.fsi` extractor learns to publish
        /// member sigs.
        Members: ExternalMember[]
        /// The directly-implemented interfaces as `(compiled-name, type-args)`
        /// pairs. Substitutes the declaring type's typars through the recorded
        /// arg builders.
        Interfaces: SemType[] -> (string * SemType[])[]
        /// The declared base type, if any (`ValueNone` for interfaces and for
        /// `System.Object` itself). Substitutes the declaring type's typars.
        BaseType: (SemType[] -> SemType) voption
        Flags: ExternalClassFlags
        Origin: SymbolOrigin
    }

    /// A minimally-populated class shape — the form contract-layer providers
    /// (`VesperLib`, `ReferencedProject`) record when the `.fsi` only commits
    /// to the type's name + arity + interface-ness. Metadata-layer providers
    /// (`MetadataSymbols`) build the rich form directly.
    static member basic(arity: int, isInterface: bool, origin: SymbolOrigin) : ExternalClassShape =
        {
            Arity = arity
            IsInterface = isInterface
            Members = [||]
            Interfaces = fun _ -> [||]
            BaseType = ValueNone
            Flags = ExternalClassFlags.Default
            Origin = origin
        }

/// Type-declaration shape carried by `IExternalSymbolProvider.TryLookupType`.
/// `arity` is the number of declared typars (same length the builder
/// arrays expect at instantiation). enum/delegate types are deferred and
/// currently return `ValueNone` from the provider.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    | Abbrev of arity: int * body: (SemType[] -> SemType)
    /// Field order matches source. `origin` is filled by the layer that knows
    /// where the type lives (`ReferencedProject.wrap` from the manifest's
    /// assembly + namespace); the inner extractor records `SymbolOrigin.Empty`.
    /// Records-handoff Phase 2 follow-up F2 reads it to mint a `TypeRef` for
    /// cross-package record emission (an external `RecordCons` / field access).
    | Record of arity: int * fields: ExternalFieldShape[] * origin: SymbolOrigin
    /// Case order matches source. `origin` is filled by the layer that knows
    /// where the type lives (`ReferencedProject.wrap` from the manifest's
    /// assembly + namespace); the inner extractor records `SymbolOrigin.Empty`.
    /// Codegen reads it to mint a `TypeRef` for the case factories on a
    /// cross-package `Some`/`None` construction (vesper-lib-test-plan Gap 2
    /// Layer B), exactly as `Record` does for `RecordCons`.
    | Union of arity: int * cases: ExternalCaseShape[] * origin: SymbolOrigin
    /// A class or interface (the gap that makes `EqualityComparer<_>` resolve to
    /// `ValueNone` today). The members / interfaces / base-type / flags ride
    /// inside `ExternalClassShape`, lifted out of the DU header so the sprint's
    /// B-2 (interface conformance) and B-1 (base-type lookup) can reach them
    /// directly. Contract-layer providers stamp `ExternalClassShape.basic`; the
    /// metadata layer fills the rich form.
    | Class of shape: ExternalClassShape
    /// A *referenced* package's intrinsic-representation binding: an `extern`
    /// type whose sibling `.fs` carries `type x = (# "<repr>" #)`
    /// (`type exn = (# "System.Exception" #)`, prim-types-exn.fs). `repr` is the
    /// CLI representation string. NON-transparent (unlike `Abbrev`): a use site
    /// resolves to the nominal `TyConst name`, never the expanded `repr`. The
    /// repr is consumed only by codegen (`IntrinsicRepr`) and by `subsumes`'
    /// `canonName` to reconcile the contract name with its metadata type. This
    /// mirrors the *local* `IntrinsicReprTypes` semantics (SideTables.fs) for a
    /// referenced package; arity is always 0 (primitives are non-generic).
    | Intrinsic of repr: string

/// **Thread-safety:** `TryLookup` and `TryLookupType` must be safe to call
/// concurrently from multiple threads. Implementations that cache lazily must
/// guard their internal mutation. Per-file pipelines run independent
/// `PassContext`s in parallel and may hit the same provider from any of them —
/// see [`docs/architecture.md`](docs/architecture.md#parallelism).
type IExternalSymbolProvider =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption
    /// Look up the body of a `type` declaration by canonical compiled name.
    /// Returns `ValueNone` for unknown names or for types whose body shape the
    /// provider doesn't (yet) model — enums, delegates, etc. Consumers fall back
    /// to `TyConst` / `TyRecord` nominal behaviour when this returns `ValueNone`.
    abstract TryLookupType: name: string -> ExternalTypeShape voption
    /// Look up a static/instance member on an external type by the declaring
    /// type's compiled name and the member name. This is what types
    /// `EqualityComparer<'T>.Default` (static property) and `.GetHashCode`
    /// (instance method). Defaults to `ValueNone` for providers that don't model
    /// members (symbol-resolution-plan §4). When several overloads share a name
    /// this collapses to a single best-by-arity pick; the *call site* uses
    /// `TryLookupMembers` instead to resolve by argument types (type-args-bug.md
    /// Layer 2).
    abstract TryLookupMember: typeName: string * memberName: string -> ExternalMember voption

    /// Look up **all** overloads of a member by name — the candidate set the
    /// application-site overload resolver picks from (by arity, then argument-type
    /// betterness; type-args-bug.md Layer 2). Providers that don't model members
    /// return `[||]`. A provider that models members SHOULD return every overload
    /// whose signature maps (the same filter `TryLookupMember` applies, minus the
    /// single-pick collapse).
    abstract TryLookupMembers: typeName: string * memberName: string -> ExternalMember[]

    /// Reverse case-name lookup: a (bare) union-case name → its declaring
    /// union's compiled name, the union's typar arity, and the case shape. The
    /// mirror of `TryLookupMember` for union construction: it lets a consumer
    /// type `Some 5` / `None` against an external union without a type
    /// annotation, exactly as F# brings a non-`RequireQualifiedAccess` union's
    /// cases into scope when its namespace is opened (vesper-lib-test-plan Gap 2
    /// Layer B). v1 is first-declaration-wins on a name collision (the same rule
    /// the short-name type index uses); providers that don't model unions return
    /// `ValueNone`.
    abstract TryLookupUnionCase: caseName: string -> (string * int * ExternalCaseShape) voption

    /// The *ambient* (implicit) open-prefix set this provider contributes — the
    /// prelude / referenced-contract `[<AutoOpen>]` modules. The pipeline seeds
    /// `PassContext.Resolution.AmbientOpenScope` from it, where it is probed
    /// strictly BEHIND explicit `open`s: a short name tries its bare form and
    /// every explicit open first, and only then these ambient prefixes
    /// (symbol-resolution-handoff.md, open-resolution). Dotted prefixes in
    /// priority order (earliest wins on a collision), e.g.
    /// `["Vesper.ArithmeticOperators"; "Vesper"]`. Providers with no implicit
    /// prelude (`MockBuiltins`, inline test fakes) return `[]`. Required (was the
    /// optional `IAmbientOpenScope` cast); folded in alongside the intrinsic
    /// surface, which now rides `TryLookupType` via `ExternalTypeShape.Intrinsic`
    /// (intrinsic-repr-handoff.md — first-cut teardown).
    abstract AmbientOpenPrefixes: string list

module ExternalSymbols =

    /// Re-resolve every nominal head of `ty` through `lookup` so the type's
    /// shapes match the forms a *use-site* type resolves to. The symbol /
    /// abbreviation extractor (`VesperLibTypeTranslate`) bakes EVERY nominal
    /// generic reference as a kind-agnostic `TyRecord(compiled, …)` placeholder.
    /// It has to: each package is extracted into its own isolated `ExtractCtx`
    /// (`ReferencedProject.buildProvider`) holding only that package's own type
    /// shapes — its dependencies are separate providers, stacked into a composite
    /// only later — so at bake time the kind of a *cross-package* type, and the
    /// expansion of an abbreviation, simply aren't knowable. (The real defect is
    /// that per-package scope; see `docs/package-type-extraction-plan.md`.) A
    /// module function's `'T option` parameter therefore comes back as the
    /// unexpanded, mis-kinded `TyRecord("Vesper.option", …)`, which unifies with
    /// neither the `TyUnion("Vesper.Option", …)` a use site resolves to nor
    /// anything else. This walk is the reconciliation step: run at the *consumer*,
    /// where the full composite provider stack IS in scope, it consults `lookup`
    /// per nominal head — a transparent abbreviation expands (then re-normalizes),
    /// a union/class/record re-kinds, an intrinsic collapses to its unqualified
    /// `TyConst` (so an external `int`/`exn` matches the literal-typed form), and
    /// an unknown head keeps its written kind. It brings baked val signatures up
    /// to the parity that type *annotations* already enjoy (those resolve fresh at
    /// the consumer via `tryResolveExternalType`). Shared by the front-end
    /// (`UnificationTranslate.normalizeExternalValueTy` / the abbrev-body
    /// re-kind in `tryResolveExternalType`) and codegen (`ClrRecipes.normalizeSig`),
    /// each passing its own provider's `TryLookupType` (vesper-lib-test-plan Gap 2).
    let rec normalizeNominal (lookup: string -> ExternalTypeShape voption) (ty: SemType) : SemType =
        let resolveNominal (name: string) (args: EqArray<SemType>) (fallback: unit -> SemType) : SemType =
            match lookup name with
            | ValueSome(ExternalTypeShape.Abbrev(_, build)) -> normalizeNominal lookup (build (args.AsSpan().ToArray()))
            | ValueSome(ExternalTypeShape.Union _) -> TyUnion(name, args)
            | ValueSome(ExternalTypeShape.Class _) -> TyClass(name, args)
            | ValueSome(ExternalTypeShape.Record _) -> TyRecord(name, args)
            | ValueSome(ExternalTypeShape.Intrinsic _) -> TyConst(name.Substring(name.LastIndexOf('.') + 1))
            | ValueNone -> fallback ()

        match ty with
        | TyRecord(name, args) ->
            let args' = EqArray.map (normalizeNominal lookup) args
            resolveNominal name args' (fun () -> TyRecord(name, args'))
        | TyUnion(name, args) ->
            let args' = EqArray.map (normalizeNominal lookup) args
            resolveNominal name args' (fun () -> TyUnion(name, args'))
        | TyClass(name, args) ->
            let args' = EqArray.map (normalizeNominal lookup) args
            resolveNominal name args' (fun () -> TyClass(name, args'))
        | TyTuple items -> TyTuple(EqArray.map (normalizeNominal lookup) items)
        | TyFun(a, b) -> TyFun(normalizeNominal lookup a, normalizeNominal lookup b)
        | TyVar _
        | TyConst _
        | TyUnknown _ -> ty

    /// The last `.`-separated segment of a compiled name (`Vesper.Option` ⇒
    /// `Option`), i.e. the simple name with any namespace / declaring-module
    /// prefix dropped. Used to test a union's declaring type against a written
    /// qualifier (`Option.Some`). A name with no `.` is returned unchanged.
    let shortName (compiled: string) : string =
        compiled.Substring(compiled.LastIndexOf '.' + 1)

    /// Mint a `SymbolKey.ValueKey` from an assembly + fully-qualified compiled
    /// name by splitting at the last `.`: everything before becomes the
    /// `ns` (module path), the last segment the simple `name`. For a bare
    /// `printfn` (no `.`) the `ns` is empty. Used by `mono`/`poly`/`polyWith`
    /// to default the symbol's `Key`; `stack`'s `stampSymbol` re-mints the
    /// key with the wrapping package's assembly once it stamps the origin
    /// (vesper-set-sprint-plan §0.1 / M1).
    let valueKeyOf (asm: string option) (compiled: string) : SymbolKey =
        let i = compiled.LastIndexOf '.'

        if i < 0 then
            SymbolKey.ValueKey(asm, "", compiled)
        else
            SymbolKey.ValueKey(asm, compiled.Substring(0, i), compiled.Substring(i + 1))

    let mono (name: string) (ty: SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = fun _ -> ty
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = valueKeyOf None name
        }

    /// `build level` is invoked per lookup so any `TypeVar` it allocates is
    /// fresh and stamped at the caller's let-depth.
    let poly (name: string) (build: int -> SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = valueKeyOf None name
        }

    /// Like `poly` but carries constraints. The `build` closure is responsible
    /// for applying them to the fresh TyVars it allocates; this helper just
    /// records the structured shape on the symbol for introspection use.
    let polyWith (name: string) (build: int -> SemType) (constraints: ExternalConstraint list) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = constraints
            Origin = SymbolOrigin.Empty
            Key = valueKeyOf None name
        }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
            member _.TryLookupType _ = ValueNone
            member _.TryLookupMember(_, _) = ValueNone
            member _.TryLookupMembers(_, _) = [||]
            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []
        }

    /// The single provider-shim primitive: first-hit-wins composition over
    /// `sources`, surfacing `ambient` via `AmbientOpenPrefixes`, optionally
    /// rewriting every resolved `ExternalSymbol` / `ExternalTypeShape` /
    /// `ExternalMember` to carry `stampOrigin`'s `SymbolOrigin`. `composite`
    /// and `ReferencedProject.wrap` both layer on top of this — one TryLookup*
    /// fall-through, one ambient surface, one place to keep the shape
    /// switch in `TryLookupType` honest when a new `ExternalTypeShape` case
    /// learns to carry its `Origin`.
    let stack
        (stampOrigin: SymbolOrigin voption)
        (ambient: string list)
        (sources: IExternalSymbolProvider list)
        : IExternalSymbolProvider =
        // Snapshot to an array so the hot lookup is an index loop, not list
        // traversal, on a provider hit from many parallel PassContexts.
        let sources = List.toArray sources

        let stampSymbol =
            match stampOrigin with
            | ValueNone -> id
            | ValueSome o ->
                // Re-stamp the asm slot on the existing key: the inner provider
                // mints `Key = valueKeyOf None compiledName` (no asm yet); the
                // wrapper knows the asm from the package manifest. Preserve the
                // inner's `(ns, name)` decomposition — a source/compiled alias
                // pair (e.g. `List.fold` + `ListModule.fold`) carries the SAME
                // key (both registered with the compiled-name decomposition by
                // VesperLib), so this asm-only re-stamp keeps the aliases
                // pointing at one identity (vesper-set-sprint-plan §0.1 / M1).
                let restampKey (k: SymbolKey) : SymbolKey =
                    match k with
                    | SymbolKey.ValueKey(_, ns, name) -> SymbolKey.ValueKey(o.Assembly, ns, name)
                    | _ -> k

                fun (s: ExternalSymbol) ->
                    { s with
                        Origin = o
                        Key = restampKey s.Key
                    }

        let stampMember =
            match stampOrigin with
            | ValueNone -> id
            | ValueSome o -> fun (m: ExternalMember) -> { m with Origin = o }

        // The single place that decides which `ExternalTypeShape` cases carry
        // their `Origin`. Class/Record/Union do today; Abbrev doesn't (its
        // cross-package emit path lands later, with the same shape). Extend
        // this match — not three call sites — when a new case learns origin.
        let stampType =
            match stampOrigin with
            | ValueNone -> id
            | ValueSome o ->
                fun shape ->
                    match shape with
                    | ExternalTypeShape.Class info -> ExternalTypeShape.Class { info with Origin = o }
                    | ExternalTypeShape.Record(arity, fields, _) -> ExternalTypeShape.Record(arity, fields, o)
                    | ExternalTypeShape.Union(arity, cases, _) -> ExternalTypeShape.Union(arity, cases, o)
                    | ExternalTypeShape.Abbrev _
                    | ExternalTypeShape.Intrinsic _ -> shape

        { new IExternalSymbolProvider with
            member _.TryLookup name =
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < sources.Length do
                    result <- sources.[i].TryLookup name
                    i <- i + 1

                match result with
                | ValueSome s -> ValueSome(stampSymbol s)
                | ValueNone -> ValueNone

            member _.TryLookupType name =
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < sources.Length do
                    result <- sources.[i].TryLookupType name
                    i <- i + 1

                match result with
                | ValueSome shape -> ValueSome(stampType shape)
                | ValueNone -> ValueNone

            member _.TryLookupMember(typeName, memberName) =
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < sources.Length do
                    result <- sources.[i].TryLookupMember(typeName, memberName)
                    i <- i + 1

                match result with
                | ValueSome m -> ValueSome(stampMember m)
                | ValueNone -> ValueNone

            // First source that knows the type wins the whole overload set — a
            // type's members live in one assembly, so a later source never
            // *adds* overloads to an earlier one's hit (same first-hit-wins
            // shadowing as the singular lookups).
            member _.TryLookupMembers(typeName, memberName) =
                let mutable result = [||]
                let mutable i = 0

                while Array.isEmpty result && i < sources.Length do
                    result <- sources.[i].TryLookupMembers(typeName, memberName)
                    i <- i + 1

                match stampOrigin with
                | ValueNone -> result
                | ValueSome _ -> result |> Array.map stampMember

            // First source that knows a case of this name wins. The result is a
            // bare name + arity + case shape — no `Origin` rides it, so (unlike
            // the type/member lookups) there is nothing to re-stamp; the union's
            // origin is recovered later via `TryLookupType` on the returned name.
            member _.TryLookupUnionCase caseName =
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < sources.Length do
                    result <- sources.[i].TryLookupUnionCase caseName
                    i <- i + 1

                result

            member _.AmbientOpenPrefixes = ambient
        }

    /// The composed ambient prelude: each source's `[<AutoOpen>]` / prelude
    /// prefixes, concatenated in source priority order (so a higher-priority
    /// provider's auto-opens shadow a lower one's on a name collision, same
    /// first-hit-wins ordering as lookups). Providers without an implicit
    /// prelude (`MockBuiltins`, inline test fakes) return `[]` and contribute
    /// nothing.
    let private collectAmbient (sources: IExternalSymbolProvider seq) : string list =
        [
            for s in sources do
                yield! s.AmbientOpenPrefixes
        ]

    /// First-hit-wins down the list; `[]` ⇒ `nullProvider`, a singleton ⇒ that
    /// provider unwrapped. Priority encodes shadowing among *external* sources
    /// (a referenced project beats a referenced assembly — symbol-resolution-plan
    /// §5). Project-local symbols are not here: `PassContext` resolves them
    /// before the provider is ever consulted. Just `stack` with no origin
    /// stamping and ambient computed from each source's `AmbientOpenPrefixes`.
    let composite (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
        match sources with
        | [] -> nullProvider
        | [ single ] -> single
        | _ -> stack ValueNone (collectAmbient sources) sources

/// The primitive `SemType` anchors the type-checker pins literals and built-in
/// constructs to (`Unification` / `Freeze`). Codegen maps each `TyConst` name to
/// its target IL type via `IntrinsicRepr`.
module BuiltinTypes =

    let tyInt: SemType = TyConst "int"
    let tyInt64: SemType = TyConst "int64"
    let tyByte: SemType = TyConst "byte"
    let tyFloat: SemType = TyConst "float"
    let tyBool: SemType = TyConst "bool"
    let tyChar: SemType = TyConst "char"
    let tyDecimal: SemType = TyConst "decimal"
    let tyUnit: SemType = TyConst "unit"
    let tyString: SemType = TyConst "string"
    /// Placeholder for `seq<int>` — the result type of int range expressions
    /// (`1..10`, `1..2..10`). Until generic types are modelled this is an
    /// opaque TyConst that only unifies with itself.
    let tySeqInt: SemType = TyConst "seq<int>"
