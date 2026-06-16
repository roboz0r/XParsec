namespace XParsec.FSharp.SemanticAnalysis

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — see
// [[project_inline_il_target_specific]] for why we don't model it here.

/// Raised by `mkNominal` when a nominal head resolves to a
/// genuinely body-less shape (`ExternalTypeShape.Opaque` — an enum / delegate /
/// type-extension or an unmodelled body). This is the *one* failure the
/// contract-extraction finalize pass (`VesperLib.finalizeDeferred`) tolerates:
/// such a head's template is never read (no use site expands it), so it degrades
/// to `FTUnknown` rather than aborting the whole provider build. Every *other*
/// exception out of the frozen translation is a producer bug and propagates.
/// `compiledName` is the head that could not be kinded.
exception BodylessExternalShape of compiledName: string with
    override this.Message =
        sprintf
            "mkNominal: '%s' is an Opaque (body-less) shape — an enum / delegate / type-extension or an unmodelled body. Model its kind before a contract names it"
            this.compiledName

/// SRTP / trait / default / coercion constraint captured on an external symbol's
/// typar list. Each carries a typar index (or indices) plus, where it has a
/// target, a `FrozenType` template over the symbol's typars; `Instantiate`
/// realises the target against the freshly minted TyVars (`instantiateDeclaring`)
/// and stamps it for inference / generalisation-time defaulting.
[<RequireQualifiedAccess>]
type ExternalConstraint =
    /// `when 'T : equality` etc. — directly stamps a `SemanticConstraint` on
    /// the fresh `TypeVar` allocated for the typar at instantiation time.
    | Trait of typarIndex: int * kind: SemanticConstraintKind
    /// `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)` — SRTP
    /// member trait. `typarIndices` are the participating typar slots
    /// (the LHS of the trait). `memberName` is the compiled name. The arg /
    /// return types are `FrozenType` templates over the symbol's declaring
    /// typars (`FTTypar(Declaring,i)`); `Instantiate` realises them against the
    /// fresh-TyVar array via `instantiateDeclaring`. The Unification pass drains
    /// the captured signature when any participating fresh TyVar is linked to a
    /// concrete shape — see `Unification.drainSrtpBounds`.
    | MemberTrait of typarIndices: EqArray<int> * memberName: string * argTypes: FrozenType[] * returnType: FrozenType
    /// `default ^T : <ty>` — typar defaulting at generalisation. `target` is a
    /// `FrozenType` template over the symbol's declaring typars — usually another
    /// typar (`default ^T3 : ^T1`) or a concrete shape (`default ^T1 : int`).
    /// `Instantiate` realises it against the fresh-TyVar array
    /// (`instantiateDeclaring`) and stamps the result onto the source TyVar's
    /// `Defaults` list so generalisation can chase the chain and pick the first
    /// concrete shape it reaches.
    | Default of typarIndex: int * target: FrozenType
    /// `when 'e :> <ty>` — coercion. `target` is a `FrozenType` template over the
    /// symbol's declaring typars; `Instantiate` realises it
    /// (`instantiateDeclaring`) and stamps a `SemanticConstraintKind.Coercion`
    /// onto the constrained fresh TyVar so the first `Link` fires
    /// `checkConstraint`/`subsumes`.
    | Coercion of typarIndex: int * target: FrozenType

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
        /// Where the symbol lives — the bridge to codegen. `SymbolOrigin.Empty`
        /// until a resolving source fills it.
        Origin: SymbolOrigin
        /// Interned identity: a `SymbolKey.ValueKey` over the symbol's resolved
        /// origin + simple name. Front-end
        /// passes write it into `Resolution.ExternalValue`; Freeze stamps it
        /// onto `TExpr.External` so codegen can do robust identity checks
        /// (e.g. "is this exactly `Vesper.Printf.printfn`?") instead of
        /// suffix-matching the source-written name.
        Key: SymbolKey
    }

/// Per-field shape inside an `ExternalTypeShape.Record`. The field type is the
/// immutable `Frozen` template parameterised over the enclosing type's typars:
/// consumers substitute a `SemType[]` (one entry per declared typar, in
/// declaration order) through the baked `FTTypar(Declaring,i)` placeholders
/// (`ExternalSymbols.instantiateFieldType`).
type ExternalFieldShape =
    {
        Name: string
        IsMutable: bool
        /// The field type with the enclosing type's typars baked as
        /// `FTTypar(Declaring,i)`. Contract-layer (`VesperLib`) producers store the
        /// `deferredTemplate` sentinel here at extraction and the
        /// `ExtractCtx.toProvider` finalize pass fills it once the registry is
        /// complete (a field type may forward-reference a type declared later in
        /// the same package — see the finalize pass); metadata-layer producers fill
        /// it eagerly.
        Frozen: FrozenType
    }

    /// A field whose template is still deferred (the `VesperLib` finalize pass
    /// fills `Frozen` once the registry is complete). Metadata-layer producers
    /// build the record literal with the real `Frozen` directly.
    static member create(name: string, isMutable: bool) : ExternalFieldShape =
        {
            Name = name
            IsMutable = isMutable
            Frozen = deferredTemplate
        }

/// Per-case shape inside an `ExternalTypeShape.Union`. `FieldNames` is
/// `ValueNone` for positional fields and `ValueSome name` for `of x: int`-
/// style named fields. The arrays line up: `FieldNames[i]` describes
/// `BuildFieldTypes[i]`'s source-side label.
type ExternalCaseShape =
    {
        Name: string
        FieldNames: string voption[]
        /// The field types as `FrozenType` templates, index-aligned with
        /// `FieldNames`, the enclosing type's typars baked as `FTTypar(Declaring,i)`.
        /// Contract-layer producers store `deferredTemplate` sentinels here at
        /// extraction; the `ExtractCtx.toProvider` finalize pass fills them once the
        /// registry is complete.
        FrozenFieldTypes: FrozenType[]
    }

    /// A case whose field templates are still deferred — one `deferredTemplate`
    /// sentinel per `fieldNames` entry, filled by the `VesperLib` finalize pass
    /// once the registry is complete.
    static member create(name: string, fieldNames: string voption[]) : ExternalCaseShape =
        {
            Name = name
            FieldNames = fieldNames
            FrozenFieldTypes = fieldNames |> Array.map (fun _ -> deferredTemplate)
        }

/// Result of a reverse union-case lookup (`IExternalSymbolProvider.TryLookupUnionCase`):
/// the declaring union's identity plus the matched case shape. A record rather
/// than a wide tuple so the union's `Origin` can ride alongside the name/arity
/// without re-threading every consumer:
/// a consumer building the union's `TyUnion` node has the data to mint its
/// `SymbolKey.TypeKey` in hand, instead of recovering it via a second
/// `TryLookupType unionName` round-trip.
type ExternalUnionCase =
    {
        /// The declaring union's compiled (arity-suffixed) name (`Vesper.Option`,
        /// `Vesper.Choice`2`).
        UnionName: string
        /// The union's declared typar arity (one fresh TyVar per slot at a use site).
        Arity: int
        /// Where the union is declared — assembly + namespace. `SymbolOrigin.Empty`
        /// for providers that don't model origins (the extractor records `Empty`;
        /// `ExternalSymbols.stack` re-stamps the package origin, mirroring how it
        /// stamps the `ExternalTypeShape.Union` it came from).
        Origin: SymbolOrigin
        /// The matched case's shape (field names + per-field type builders).
        Case: ExternalCaseShape
        /// True when the declaring union is `[<RequireQualifiedAccess>]`: F# forbids
        /// the bare case form (`Red`), accepting only the qualified `Color.Red`. The
        /// resolution-side suppression keys off this — a *bare* hit on an RQA case is
        /// rejected (treated as unresolved), while a qualified reference still
        /// resolves (opens-overhaul-plan Gap 1). Providers that don't model unions
        /// never return an `ExternalUnionCase`, so the default is moot for them.
        IsRequireQualifiedAccess: bool
    }

    /// Does a reference written with `qualifier` resolve to this case? A *bare*
    /// (`ValueNone`) reference to an `[<RequireQualifiedAccess>]` union's case does
    /// not — F# requires `Color.Red`, not `Red` (opens-overhaul-plan Gap 1). A
    /// *qualified* (`ValueSome q`) reference resolves only when `q` is the union's
    /// short name. The single home for the RQA + qualifier-match rule; the resolver,
    /// typer, and projector all defer here rather than re-deriving it inline.
    member uc.ResolvesWith(qualifier: string voption) : bool =
        match qualifier with
        | ValueNone -> not uc.IsRequireQualifiedAccess
        | ValueSome q -> SymbolKeyOps.shortName uc.UnionName = q

/// The immutable, two-axis member descriptor: a
/// member's tupled `(Parameters, Return)` as `FrozenType` templates. `Parameters`
/// is the .NET-tupled argument type (`N ≥ 2` → one `FTTuple`; 0 params →
/// `unit`); `Return` the result. Open typars are baked as `FTTypar(Declaring,i)`
/// (the declaring type's typars) / `FTTypar(Method,j)` (the method's own) — the
/// `DeclaringArity` / `MethodArity` counts give each axis's width. For a
/// property (`ExternalMember.IsProperty`) there are no parameters: `Parameters`
/// is `unit` and the value type lives in `Return`; consumers gate reconstruction
/// on `IsProperty` (see `ExternalSymbols.instantiateSignature`). The two-axis
/// data form is what unblocks generic external static methods (`truncate`).
type ExternalSignature =
    {
        DeclaringArity: int
        MethodArity: int
        Parameters: FrozenType
        Return: FrozenType
    }

    /// The deferred sentinel a contract-layer member carries between extraction
    /// and the `ExtractCtx.toProvider` finalize pass (which fills `Parameters` /
    /// `Return` by translating the stashed signature CST once the registry is
    /// complete). `DeclaringArity` / `MethodArity` are recorded eagerly so the
    /// finalize pass needs only the CST. Metadata-layer (`MetadataSymbols`,
    /// reflection-backed) members skip this and build their template eagerly —
    /// their shapes are total and registry-independent.
    static member deferred(declaringArity: int, methodArity: int) : ExternalSignature =
        {
            DeclaringArity = declaringArity
            MethodArity = methodArity
            Parameters = deferredTemplate
            Return = deferredTemplate
        }

/// A resolved member (static/instance method or property getter) on an external
/// type. `Signature` is the immutable two-axis `ExternalSignature` template
/// parameterised over the *enclosing type's* typars (and the member's own, on
/// the method axis), yielding the **tupled** `(p1 * … * pN) → ret` signature
/// (the .NET calling convention; arity ≤ 1 is unchanged — see
/// `MetadataMapping.tryMethodSignature`).
type ExternalMember =
    {
        Name: string
        IsStatic: bool
        IsProperty: bool
        /// The tupled `(Parameters, Return)` two-axis template, the declaring +
        /// method typars baked as `FTTypar` placeholders.
        /// `ExternalSymbols.instantiateSignature` / `openSignature` realise it.
        Signature: ExternalSignature
        /// The count of the member's *own* generic type parameters — the
        /// method-owned typar axis (`Take<TSource>` ⇒ 1), distinct from the
        /// declaring type's typars `Signature` substitutes. `0` for a
        /// non-generic method, every property, and every constructor. The
        /// `Signature` template already carries these typars as baked
        /// `FTTypar(Method, j)` nodes (the method axis is intrinsic to the
        /// member — there is nothing to pass in, unlike the declaring args); a
        /// consumer instantiates them to fresh inference vars at a call site, and
        /// codegen reads `MethodArity` to mint the `MethodSpec`'s generic-parameter
        /// count.
        MethodArity: int
        Origin: SymbolOrigin
        /// The interned identity: a
        /// `SymbolKey.MemberKey` over the *open* declaring type (its `argSig` in
        /// `!0`-typars), minted by the resolving source. Freeze stamps it into
        /// `TExpr.ExternalMember` so codegen reads the binding off the node.
        Key: SymbolKey
        /// The compile-time-constant default values of this member's *trailing*
        /// optional parameters (`ArrayPool<'T>.Return(array, [<Optional>] clearArray =
        /// false)` ⇒ `[Bool false]`), in declaration order. A call may omit any
        /// suffix of these: the front end (`InferExternalCall.tryFillOptionalCall`)
        /// permits the under-applied arity and Freeze synthesises the omitted defaults
        /// as literal arguments so codegen sees the full tupled call unchanged. Empty
        /// for a member with no omittable optionals — every property, ctor, and the
        /// contract (`.fsi`) layer, which doesn't publish optional defaults yet. Only
        /// constants representable as a `TConstValue` are surfaced; an optional whose
        /// default is `null` / a non-primitive `default(struct)` ends the trailing run
        /// (that parameter stays required), so no call can omit past it.
        OptionalDefaults: TConstValue list
    }

    /// The canonical `.ctor` member shape every layer must agree on: `Name =
    /// ".ctor"`, instance, non-property, `MethodArity = 0`, keyed as a
    /// `MemberKind.Method` over `declKey`. The metadata layer (`MetadataSymbols`),
    /// the `.fsi` contract extractor (`VesperLib`), and the JS-native stubs
    /// (`JsNativeSymbols`) all mint a constructor through this, so the constant
    /// fields stay in one place. Only the per-layer parts vary: the frozen
    /// `signature`, the `argSig` the key interns, the `origin`, and any
    /// `optionalDefaults` (metadata-layer only — the contract layers pass `[]`).
    static member ctor
        (declKey: SymbolKey)
        (signature: ExternalSignature)
        (argSig: EqArray<string>)
        (origin: SymbolOrigin)
        (optionalDefaults: TConstValue list)
        : ExternalMember =
        {
            Name = ".ctor"
            IsStatic = false
            IsProperty = false
            Signature = signature
            MethodArity = 0
            Origin = origin
            Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
            OptionalDefaults = optionalDefaults
        }

/// Capability flags on an external class or interface. The metadata layer reads
/// them off the .NET `TypeAttributes` plus
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
        /// first consumer. Contract-layer providers
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

/// The shape of an external class or interface. Lifted out of `ExternalTypeShape.Class`
/// so the DU header stays narrow and the
/// member set is reachable to consumers (B-2's `interface … with member …`
/// conformance check, B-1's base-type lookup, etc.) without having to round-trip
/// through `TryLookupMember` per name.
///
/// `FrozenInterfaces`, `FrozenBaseType`, and each member's `Signature` are
/// written over the *declaring type's* typars: consumers substitute a
/// `SemType[]` (one entry per declared typar, in declaration order) through the
/// baked `FTTypar(Declaring,i)` placeholders (`ExternalSymbols.instantiateInterfaces`
/// / `instantiateBaseType`).
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
        /// pairs, each interface's type args with the declaring typars baked as
        /// `FTTypar(Declaring,i)`.
        FrozenInterfaces: (string * FrozenType[])[]
        /// The declared base type, if any (`ValueNone` for interfaces and for
        /// `System.Object` itself), with the declaring typars baked as
        /// `FTTypar(Declaring,i)`.
        FrozenBaseType: FrozenType voption
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
            FrozenInterfaces = [||]
            FrozenBaseType = ValueNone
            Flags = ExternalClassFlags.Default
            Origin = origin
        }

/// Type-declaration shape carried by `IExternalSymbolProvider.TryLookupType`.
/// `arity` is the number of declared typars (same length the builder
/// arrays expect at instantiation).
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    /// `frozen` is the abbreviation body as a `FrozenType` template (the
    /// declaring typars baked as `FTTypar(Declaring,i)`); a use site expands it
    /// via `FrozenTypeBridge.instantiateDeclaring`. Contract-layer producers store
    /// the `deferredTemplate` sentinel and the finalize pass fills it once the
    /// registry is complete.
    | Abbrev of arity: int * frozen: FrozenType
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
    /// A nominal type whose *name + arity* the extractor registered but whose
    /// body shape it does not (yet) model: an enum / delegate / type-extension
    /// (v1 defers the body), or a union / record / abbreviation whose body failed
    /// to translate (an unsupported field form, a typar-arity overflow). The skip
    /// reason, when it is an error rather than a deferral, is recorded in
    /// `ExtractCtx.Skipped`; this shape is the *referenceable* residue, so the
    /// name still has a shape (`TryLookupType` is total) instead of leaving the
    /// accidental `ValueNone -> TyRecord` gap.
    ///
    /// It is a body-*less* residue: it carries no kind, so `mkNominal` **refuses**
    /// a signature that names one (a loud `failwith`) rather than minting a
    /// kind-agnostic `TyRecord` placeholder that would flow to codegen. No
    /// shipping contract names an `Opaque` type; modelling its kind
    /// turns it into a real shape
    /// (`Union` / `Class` / …) and a normal `mkNominal` arm. Distinct from a
    /// genuinely *unresolved* name, which never registers and bakes `TyUnknown`.
    | Opaque of arity: int

/// A cross-package `val inline` body plus the compiler attributes on its
/// parameters, positionally aligned to the inline's curried parameters. `Decl`
/// is the retained `let inline` declaration the pre-freeze `Passes.InlineExpansion`
/// splices at each use site; `ParamAttrs` is the cross-package twin of
/// `PassContext.InlineParamAttrs` (`[<CallAtMostOnce>]` &c.) — empty for a body
/// whose parameters carry no recognised attribute. Both are read by the inliner;
/// the attrs gate call-by-name-at-single-use splicing.
type InlineBody =
    {
        Decl: TDecl
        ParamAttrs: ParamAttrs[]
    }

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
    /// members. When several overloads share a name
    /// this collapses to a single best-by-arity pick; the *call site* uses
    /// `TryLookupMembers` instead to resolve by argument types.
    abstract TryLookupMember: typeName: string * memberName: string -> ExternalMember voption

    /// Look up **all** overloads of a member by name — the candidate set the
    /// application-site overload resolver picks from (by arity, then argument-type
    /// betterness). Providers that don't model members
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
    abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption

    /// The *ambient* (implicit) open-prefix set this provider contributes — the
    /// prelude / referenced-contract `[<AutoOpen>]` modules. The pipeline seeds
    /// `PassContext.Resolution.AmbientOpenScope` from it, where it is probed
    /// strictly BEHIND explicit `open`s: a short name tries its bare form and
    /// every explicit open first, and only then these ambient prefixes
    /// Dotted prefixes in
    /// priority order (earliest wins on a collision), e.g.
    /// `["Vesper.ArithmeticOperators"; "Vesper"]`. Providers with no implicit
    /// prelude (`MockBuiltins`, inline test fakes) return `[]`. Required (was the
    /// optional `IAmbientOpenScope` cast); folded in alongside the intrinsic
    /// surface, which rides `TryLookupType` via `ExternalTypeShape.Intrinsic`.
    abstract AmbientOpenPrefixes: string list

    /// A cross-package `val inline` body — a referenced package's `let inline`
    /// whose `.fs` source the pre-freeze `Passes.InlineExpansion` pass *splices*
    /// at each use site rather than calling as a compiled member.
    /// Looked up by the inline value's resolved
    /// `SymbolKey` — the same key `TryLookup` returns and `Freeze` stamps onto a
    /// use-site `TExpr.External`. The primary, identity-robust channel
    /// (disambiguates a referenced package's `hash` from a user shadow).
    /// Providers that carry no inline bodies (the front-end-only paths) return
    /// `ValueNone`.
    abstract TryLookupInlineBody: key: SymbolKey -> InlineBody voption

    /// `TryLookupInlineBody` by source/compiled name — the residual fallback for
    /// use-site `External` heads that still carry `key = ValueNone` (operator /
    /// desugared heads, which `FreezeExpr` does not yet stamp). Shrinks toward
    /// nothing as more head shapes get their key stamped; `ValueNone` once they
    /// all do (and for providers with no inline bodies).
    abstract TryLookupInlineBodyByName: name: string -> InlineBody voption

/// The open signature of an external module-level function as the codegen
/// boundary sees it: the curried
/// `param -> … -> return` template with the function's own typars baked as
/// `FTTypar(Method, i)`, plus the home `Origin` the call's `MemberRef` parent is
/// minted against and the method-typar count for the `MethodSpec`. The immutable-
/// data replacement for codegen reaching `ExternalSymbol` + `Inline.openMethodSignature`:
/// `Signature` is `FrozenType`, so the codegen side never touches a `SemType` or the
/// symbol's mutable `Instantiate` closure.
type CodegenOpenSignature =
    {
        Origin: SymbolOrigin
        Signature: FrozenType
        MethodArity: int
    }

/// The **codegen-facing** view of the external-symbol contract (external-signature
/// -plan: "dual view over one provider"). Where `IExternalSymbolProvider` exposes the
/// inference surface (the `SemType`-returning `Instantiate`, `Constraints`, inline
/// bodies, the ambient-open scope), this exposes **only** what emission needs to mint
/// references: the type/member shapes (whose `FrozenType` templates codegen reads — it
/// never runs the legacy `SemType[] -> SemType` closures) and the open signature of a
/// module-level function. `ClrEnv` holds this instead of `IExternalSymbolProvider`, so
/// the emission code can no longer reach `Instantiate` / constraints / mutable inference
/// state. One backing provider implements both views (`ExternalSymbols.codegenView`).
type ICodegenSymbols =
    /// Look up a `type` declaration's shape by canonical compiled name (the parent
    /// `TypeRef` + the field/case templates codegen encodes).
    abstract TryLookupType: name: string -> ExternalTypeShape voption
    /// The single best-by-arity member overload (the fallback when the front end's
    /// exact key isn't in the candidate set).
    abstract TryLookupMember: typeName: string * memberName: string -> ExternalMember voption
    /// Every overload of a member name — the set codegen filters by the front end's
    /// resolved `SymbolKey` (or re-picks a ctor from, by call-site arg types).
    abstract TryLookupMembers: typeName: string * memberName: string -> ExternalMember[]
    /// The open `FrozenType` signature of a module-level function, or `ValueNone` for
    /// an unknown symbol or one with no home assembly (a project-local symbol the
    /// provider never sees — the caller falls back to its hard error). The data-form
    /// replacement for `TryLookup` + `Inline.openMethodSignature` at the codegen boundary.
    abstract TryLookupOpenSignature: name: string -> CodegenOpenSignature voption

module ExternalSymbols =

    // The generic `SymbolKey` ↔ compiled-name string algebra (`bareName`,
    // `arityName`, `valueKeyOf`, `simpleName`, `qualifiedName`, `externalTypeKey`,
    // …) lives in `module SymbolKeyOps` (compiles before this file, so
    // `RuntimeNames` can route through it without depending on the provider
    // surface). This module keeps only the resolution surface that genuinely
    // needs `IExternalSymbolProvider` / `ExternalSymbol`.

    /// Look an external type up by the `SymbolKey` a front-end consumer already holds.
    /// The provider is string-keyed (its metadata / contract leaves own compiled names —
    /// the genuine string boundary), so this is the single key-accepting front door that
    /// projects to `qualifiedName` once, deleting the per-site `TryLookupType (qualifiedName
    /// key)` re-projection at the consumers whose only use of the string was the lookup.
    /// Callers that need the qualified string for an adjacent purpose (a diagnostic,
    /// `TryLookupMember`) keep projecting it directly.
    let tryLookupType (provider: IExternalSymbolProvider) (key: SymbolKey) : ExternalTypeShape voption =
        provider.TryLookupType(SymbolKeyOps.qualifiedName key)

    /// Realise a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per
    /// index, shared across `Parameters` and `Return`). Reconstructs
    /// `BuildSignature`'s `TyFun(params, ret)` for a method / ctor, or the bare
    /// value type for a property. The data-form replacement for
    /// `member.BuildSignature args` followed by `Infer.instantiateMethodTypars`;
    /// equal to it on the post-freeze subset.
    let instantiateSignature (m: ExternalMember) (declaringArgs: SemType[]) (level: int) : SemType =
        let cache = System.Collections.Generic.Dictionary<int, SemType>()
        let methodVar = methodFreshener cache level
        let decl i = declaringArgs.[i]
        let s = m.Signature

        if m.IsProperty then
            instantiateWith decl methodVar s.Return
        else
            TyFun(instantiateWith decl methodVar s.Parameters, instantiateWith decl methodVar s.Return)

    /// The *open* realisation of a member's `Signature`: declaring typars
    /// substituted from `declaringArgs`, but the member's own method typars left
    /// as `TyTypar(Method,j)` markers — exactly the shape `BuildSignature`
    /// produced. This is the applicability-
    /// filtering / single-pick form; a generic method's `TyTypar(Method,_)`
    /// stays a wildcard for `InferOverload.semTypeEq`, and the bind site that
    /// commits the member freshens them separately (`instantiateSignature`, or
    /// `Infer.instantiateMethodTypars`). For a non-generic member (the common
    /// case) it is byte-identical to `instantiateSignature` at any level.
    let openSignature (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let s = m.Signature

        if m.IsProperty then
            instantiateWith decl methodOpen s.Return
        else
            TyFun(instantiateWith decl methodOpen s.Parameters, instantiateWith decl methodOpen s.Return)

    /// Realise a record field's type at a use site (`FTTypar(Declaring,i) →
    /// declaringArgs.[i]`). The data-form replacement for `field.BuildType args`.
    let instantiateFieldType (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring f.Frozen declaringArgs

    /// Realise a union case's field types at a use site. The data-form
    /// replacement for `case.BuildFieldTypes |> Array.map (fun b -> b args)`.
    let instantiateCaseFieldTypes (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        c.FrozenFieldTypes
        |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs)

    /// Realise a class/interface's directly-implemented interfaces as
    /// `(compiled-name, type-args)` pairs. The data-form replacement for
    /// `shape.Interfaces args`.
    let instantiateInterfaces (shape: ExternalClassShape) (declaringArgs: SemType[]) : (string * SemType[])[] =
        shape.FrozenInterfaces
        |> Array.map (fun (name, fts) -> name, fts |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs))

    /// Realise a class's declared base type, if any. The data-form replacement
    /// for `shape.BaseType |> ValueOption.map (fun b -> b args)`.
    let instantiateBaseType (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType voption =
        shape.FrozenBaseType
        |> ValueOption.map (fun ft -> instantiateDeclaring ft declaringArgs)

    // --- Contract-extraction finalize fallback ----
    //
    // The `VesperLib` finalize pass translates each stashed body / member CST to a
    // `FrozenType` template directly (`translateType`), once the registry is
    // complete. A genuinely body-less head (`byref` / an `Opaque` shape) raises
    // `BodylessExternalShape` during that walk and is never expanded by any use
    // site, so the pass degrades it to this sentinel rather than aborting the whole
    // provider build. (The former `freezeTemplateTolerant` /
    // `signatureOfClosureTolerant` closure-freezers are gone — `VesperLib`
    // tolerates around the CST translation itself.)

    let unfreezable = FTUnknown "<unfreezable external template>"

    let mono (name: string) (ty: SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = fun _ -> ty
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = SymbolKeyOps.valueKeyOf None name
        }

    /// `build level` is invoked per lookup so any `TypeVar` it allocates is
    /// fresh and stamped at the caller's let-depth.
    let poly (name: string) (build: int -> SemType) : ExternalSymbol =
        {
            Name = name
            Instantiate = build
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = SymbolKeyOps.valueKeyOf None name
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
            Key = SymbolKeyOps.valueKeyOf None name
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
            member _.TryLookupInlineBody _ = ValueNone
            member _.TryLookupInlineBodyByName _ = ValueNone
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

        // First-hit-wins fall-through shared by every singular (`voption`) lookup
        // below: scan `sources` in priority order, stop at the first `ValueSome`.
        // `inline` keeps this an index loop with the projection fused at each call
        // site — no list traversal. The array-valued `TryLookupMembers` keeps its
        // own loop (its "empty" sentinel is `[||]`, not `ValueNone`).
        let inline firstHit (f: IExternalSymbolProvider -> 'a voption) : 'a voption =
            let mutable result = ValueNone
            let mutable i = 0

            while result.IsNone && i < sources.Length do
                result <- f sources.[i]
                i <- i + 1

            result

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
                // pointing at one identity.
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
                    | ExternalTypeShape.Intrinsic _
                    | ExternalTypeShape.Opaque _ -> shape

        // Mirror `stampType`'s Union arm: the extractor records the declaring
        // union with `SymbolOrigin.Empty`, so a case reverse-looked-up off it
        // would otherwise carry the empty origin. Overwrite it with the package
        // origin so the union-case's origin agrees with what `TryLookupType`
        // would report for the same union.
        let stampUnionCase =
            match stampOrigin with
            | ValueNone -> id
            | ValueSome o -> fun (uc: ExternalUnionCase) -> { uc with Origin = o }

        { new IExternalSymbolProvider with
            member _.TryLookup name =
                firstHit (fun s -> s.TryLookup name) |> ValueOption.map stampSymbol

            member _.TryLookupType name =
                firstHit (fun s -> s.TryLookupType name) |> ValueOption.map stampType

            member _.TryLookupMember(typeName, memberName) =
                firstHit (fun s -> s.TryLookupMember(typeName, memberName))
                |> ValueOption.map stampMember

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

            // First source that knows a case of this name wins; re-stamp the
            // package origin onto the result exactly as `TryLookupType` does for
            // the union shape it came from (the inner extractor records
            // `SymbolOrigin.Empty`).
            member _.TryLookupUnionCase caseName =
                firstHit (fun s -> s.TryLookupUnionCase caseName)
                |> ValueOption.map stampUnionCase

            member _.AmbientOpenPrefixes = ambient

            // Inline bodies are origin-independent `TDecl`s (no key/origin
            // re-stamp), so these are plain first-hit-wins fall-throughs like the
            // lookups above — a source that serves cross-package inline bodies
            // (the codegen contract stack) surfaces them through the composite.
            member _.TryLookupInlineBody key =
                firstHit (fun s -> s.TryLookupInlineBody key)

            member _.TryLookupInlineBodyByName name =
                firstHit (fun s -> s.TryLookupInlineBodyByName name)
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
    /// (a referenced project beats a referenced assembly). Project-local symbols
    /// are not here: `PassContext` resolves them
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

    let tyInt: SemType = TyConst("int", EqArray.empty)
    let tyInt64: SemType = TyConst("int64", EqArray.empty)
    let tyByte: SemType = TyConst("byte", EqArray.empty)
    let tySByte: SemType = TyConst("sbyte", EqArray.empty)
    let tyInt16: SemType = TyConst("int16", EqArray.empty)
    let tyUInt16: SemType = TyConst("uint16", EqArray.empty)
    let tyUInt32: SemType = TyConst("uint32", EqArray.empty)
    let tyUInt64: SemType = TyConst("uint64", EqArray.empty)
    let tyNativeInt: SemType = TyConst("nativeint", EqArray.empty)
    let tyUNativeInt: SemType = TyConst("unativeint", EqArray.empty)
    let tyBigInt: SemType = TyConst("bigint", EqArray.empty)
    let tyFloat: SemType = TyConst("float", EqArray.empty)
    let tyFloat32: SemType = TyConst("float32", EqArray.empty)
    let tyBool: SemType = TyConst("bool", EqArray.empty)
    let tyChar: SemType = TyConst("char", EqArray.empty)
    let tyDecimal: SemType = TyConst("decimal", EqArray.empty)
    let tyUnit: SemType = TyConst("unit", EqArray.empty)
    let tyString: SemType = TyConst("string", EqArray.empty)
    /// Placeholder for `seq<int>` — the result type of int range expressions
    /// (`1..10`, `1..2..10`). Until generic types are modelled this is an
    /// opaque TyConst that only unifies with itself.
    let tySeqInt: SemType = TyConst("seq<int>", EqArray.empty)
