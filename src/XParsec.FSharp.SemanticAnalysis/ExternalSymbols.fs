namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

// The full FSharp.Core (+) story (SRTPs + static-optimisation clauses +
// per-target inline IL) is firmly future work — the resolved operator's compiled
// name drives target-specific dispatch, so the inline IL stays target-specific and
// is not modelled here.

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
    /// fresh-TyVar array via `instantiateDeclaring`. The Unification pass discharges
    /// the captured signature when any participating fresh TyVar is linked to a
    /// concrete shape — see `Unification.dischargeSrtpBounds`.
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

/// How a symbol's home module EXPORTS it — the fact that decides the JS import
/// statement shape at a use site. A semantic classification of the external package,
/// so it rides the provider seam (like `ExternalClassFlags.MemberLowering`); only the
/// JS backend consumes it (`JsImports.addRef`). The four arms mirror the manifest's
/// `Schema.ImportShape`:
///   • `Named`   → `import { x } from …` (the default for every non-TS producer);
///   • `Default` → `import x from …` (a TS `export default`; a default export cannot
///     be imported by name);
///   • `CommonJs`→ a TS `export =` (CommonJS `module.exports = X`). Under the
///     esModuleInterop lowering every modern ESM/node consumer uses, `export =`
///     binds the whole `module.exports` to a DEFAULT import, so `CommonJs` lowers
///     IDENTICALLY to `Default` today — it stays a distinct arm so the extractor's
///     brand survives to a later CJS-target that would emit `import x = require(…)`;
///   • `Namespace` → `import * as ns from …` with member access `ns.x` (a
///     namespace-object import, e.g. `import * as fs from "fs"`).
[<RequireQualifiedAccess>]
type ImportForm =
    | Named
    | Default
    | CommonJs
    | Namespace

/// A published inline body as the provider serves it: the producing file's own template,
/// unpooled to the wire shape (`Wire.TDecl`) and handed across the boundary VERBATIM.
///
/// Its binder keys are MINTED BY THE UNPOOL (`TastPoolBuilder.declTree`), not the producer's
/// own: a pooled binder is a slot, and a slot means nothing in the consuming file's pool.
/// What the wire needs of them is distinctness within this one template plus equality
/// between a binder and its references, which a counter-minted key gives — and, being
/// counter-minted, it names no position anything could try to resolve it against.
///
/// Its POSITIONS, by contrast, are handed over as-is: every node still carries the index of
/// the token that spells it in the PRODUCER's file. What the consumer lacks is not the indices
/// but the `Lexed` they index, so they arrive intact rather than blanked, and the consumer
/// names the producer file it reads them against (`OriginSources.tokenAt`, which is also where
/// the file's content hash is checked). `Origin` is what it names.
///
/// `FrozenType`, not `SemType` — a `SemType.TyVar` is a mutable `UnionFind` cell, and
/// an oracle that hands one out lets a consumer's inference reach back and mutate a
/// producer's. The consumer THAWS the body when it resolves it, minting its own cells by
/// construction; that thaw is the one immutable→mutable transition, and it sits on the
/// consumer's side of the seam. Do NOT re-widen this to `SemType` to make a consuming site
/// convenient — thaw is the seam.
type InlineBody =
    {
        Decl: Wire.TDecl
        ParamAttrs: ParamAttrs[]
        /// The producer file `Decl`'s anchors index, RETAINED — its text and token table, not
        /// merely its identity.
        ///
        /// Retained rather than named because a served body and the file that gives its
        /// integers a meaning are ONE fact: a provider that hands out the body and drops the
        /// file has published indices nothing can ever read, and there is no later point at
        /// which the file could be found again (the collection that parsed it is the only
        /// thing that ever held it). Carrying it here is what makes that unrepresentable.
        ///
        /// Mandatory, and that is what lets EVERY served body be left behind an edge: a body
        /// whose anchors nothing could resolve would have to be moved onto its call site
        /// instead, which is a second placement for the consumer to decide.
        Origin: OriginSource
    }

[<RequireQualifiedAccess>]
module InlineBody =

    /// A body served WITH the producer file its anchors index — the shape a publisher owes its
    /// consumers, and the only shape there is.
    let anchoredIn (origin: OriginSource) (decl: Wire.TDecl) (paramAttrs: ParamAttrs[]) : InlineBody =
        {
            Decl = decl
            ParamAttrs = paramAttrs
            Origin = origin
        }

type ExternalSymbol =
    {
        Name: string
        /// The symbol's type SCHEME over its own typars, the typars baked as
        /// `FTTypar(Declaring,i)`. The pure-data form of the value/free-function
        /// type: `ExternalSymbols.instantiateSymbol` realises it at a `level`,
        /// minting a fresh `TyVar` (stamped at `level`) per typar so independent
        /// use-sites don't share variables, and stamping `Constraints` onto them.
        /// A monomorphic symbol (`TyparArity = 0`) realises to a fresh structurally-
        /// identical `SemType` each call. Built by `monoFrozen` / `scheme`. This is
        /// the contract surface a provider speaks — `SemType` never crosses it.
        Scheme: FrozenType
        /// Count of the `Scheme`'s own typars (the single axis a value/free-fn has);
        /// `0` for a monomorphic symbol.
        TyparArity: int
        /// Empty for the overwhelming majority of symbols. `instantiateSymbol`
        /// applies them to the fresh TyVars it mints; callers don't apply the list
        /// separately. Also surfaced for diagnostic introspection.
        Constraints: ExternalConstraint list
        /// Where the symbol lives — the bridge to codegen. `SymbolOrigin.Empty`
        /// until a resolving source fills it.
        Origin: SymbolOrigin
        /// Interned identity — a `BindingKey`, the ONE kind a value symbol can have:
        /// the symbol's declaring holder + simple name. Front-end
        /// passes write it into `Resolution.ExternalValue`; Elaborate stamps it
        /// onto `TExpr.External` so codegen can do robust identity checks
        /// (e.g. "is this exactly `Vesper.Printf.printfn`?") instead of
        /// suffix-matching the source-written name. A consumer that needs the wide
        /// `SymbolKey` the IR carries widens at the boundary (`SymbolKey.Binding`);
        /// nothing narrows back, so "what if it isn't a binding?" cannot be asked.
        Key: BindingKey
        /// The SOURCE arity (`ValRepr`) of a module-level FUNCTION, carried across the
        /// assembly boundary so a caller reconciles its call arguments against the
        /// producer's
        /// curried / tupled grouping. The flat, lone-unit-erased, `void`-normalised
        /// `CompiledForm` is derived from it on demand (`TastLower.compiledOf`), never
        /// stored — it is fully determined by the `ValRepr`. `ValueNone` for
        /// everything that is not a contract-extracted function (module values,
        /// operators, the `monoFrozen`/`scheme` builders, metadata-layer symbols) — those
        /// keep the curried-`Scheme` reconstruction at the codegen boundary.
        ValRepr: TastAccessor.ValRepr voption
        /// How the symbol's home module exports it (see `ImportForm`). Stamped
        /// `Default` by the TS-manifest provider for a TS `export default`
        /// (mitt's factory); `Named` for every other producer — the
        /// `monoFrozen`/`scheme` builders default it, so non-TS layers
        /// (VesperLib, MetadataSymbols, JsNativeSymbols) never touch it.
        ImportForm: ImportForm
        /// The symbol's splice TEMPLATE, when it has one — a `val inline` whose home
        /// file published its body. `ValueNone` for every ordinary (compiled) symbol,
        /// and for every provider that carries no inline bodies.
        ///
        /// Folded ONTO the resolved entry rather than served by a sibling by-key
        /// channel: the entry already carries the identity the body is keyed by
        /// (`Key`), so a separate lookup could only re-ask a question this entry has
        /// already answered — and answer it under a key that might disagree. NOT
        /// `Lazy`: the provider builds its symbols FROM the frozen file, so the body is
        /// already in memory and deferring it would defer work already done.
        InlineBody: InlineBody voption
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

/// The compile-time value of an `ExternalEnumCaseShape`, mirroring the manifest
/// wire `LiteralValue` (`Vesper.Ts.Manifest.Schema`): a TS numeric member is an
/// `int64` (the integer subset the extractor admits), a string member its text.
/// The numeric *width* is deliberately ABSENT — a TS import has no width notion;
/// the underlying integral type is the `FrozenType`-layer default (`I32`), a
/// policy decision at the seam, not data read off the wire (see the enum-support
/// plan). The numeric / string / mixed VARIANT likewise falls out of a case
/// table's values (the same "classify, don't bake" rule as the authored
/// `TEnumCases.classify`); it is never stored.
[<RequireQualifiedAccess>]
type ExternalEnumCaseValue =
    | IntVal of int64
    | StringVal of string

/// Per-case shape inside an `ExternalTypeShape.Enum`: the case identifier (the
/// `C1` of `E.C1`) plus its resolved compile-time value. The enum analogue of
/// `ExternalCaseShape`, but an enum case carries a single scalar literal rather
/// than a field list — an enum is a closed set of named constant singletons, not
/// a payload-bearing union. Case order matches the manifest / TS source.
type ExternalEnumCaseShape =
    {
        Name: string
        Value: ExternalEnumCaseValue
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
        TyparArity: int
        /// Where the union is declared — assembly + namespace. `SymbolOrigin.Empty`
        /// for providers that don't model origins (the extractor records `Empty`;
        /// `ExternalSymbolProviders.stack` re-stamps the package origin, mirroring how it
        /// stamps the `ExternalTypeShape.Union` it came from).
        Origin: SymbolOrigin
        /// The matched case's shape (field names + per-field type builders).
        Case: ExternalCaseShape
        /// True when the declaring union is `[<RequireQualifiedAccess>]`: F# forbids
        /// the bare case form (`Red`), accepting only the qualified `Color.Red`. The
        /// resolution-side suppression keys off this — a *bare* hit on an RQA case is
        /// rejected (treated as unresolved), while a qualified reference still
        /// resolves. Providers that don't model unions never return an
        /// `ExternalUnionCase`, so the default is moot for them.
        IsRequireQualifiedAccess: bool
    }

    /// Does a reference written with `qualifier` resolve to this case? A *bare*
    /// (`ValueNone`) reference to an `[<RequireQualifiedAccess>]` union's case does
    /// not — F# requires `Color.Red`, not `Red`. A
    /// *qualified* (`ValueSome q`) reference resolves only when `q` is the union's
    /// short name. The single home for the RQA + qualifier-match rule; the resolver,
    /// typer, and projector all defer here rather than re-deriving it inline.
    member uc.ResolvesWith(qualifier: string voption) : bool =
        match qualifier with
        | ValueNone -> not uc.IsRequireQualifiedAccess
        | ValueSome q -> SymbolKeyOps.shortName uc.UnionName = q

/// One hit from the per-field reverse index (`IExternalSymbolProvider.TryRecordsWithField`):
/// a record that declares the queried field, named by its identity plus everything the
/// unqualified record-literal / record-pattern resolver needs to intersect and tie-break.
/// A record rather than a wide tuple — exactly the `ExternalUnionCase` rationale — so the
/// record's `Origin` and RQA flag ride alongside its name/arity without re-threading every
/// consumer: a resolver that picks this candidate has the data to mint its
/// `SymbolKey.TypeKey` in hand, rather than recovering it via a second `TryLookupType`
/// round-trip. Field *types* are deliberately NOT here — they come from the by-key shape
/// path (`TryLookupType key -> ExternalTypeShape.Record`) at construction; this index only
/// answers identity.
type ExternalRecordCandidate =
    {
        /// The record's REAL identity — the exact `TypeKey` the declaring file minted
        /// (a module-held record's `InModule` holder chain, which no compiled-name
        /// string can reconstruct: `externalTypeKeyOf` would re-cut the `+`-mangled
        /// module segment as an `InType` class holder, yielding a key with the same
        /// metadata NAME but an unequal identity — one that MISSES both the by-key shape
        /// store and codegen's `env.Records` local re-home). Carried whole so both the
        /// `TyRecord` node identity (cross-file re-homing to a local `TypeDef`) and the
        /// by-key field-shape lookup use the producer's authoritative key.
        TypeKey: TypeKey
        /// The record's declared typar arity (one fresh TyVar per slot at a use site).
        TyparArity: int
        /// Where the record is declared — assembly + namespace. `SymbolOrigin.Empty`
        /// for providers that don't model origins; `ExternalSymbolProviders.stack`
        /// re-stamps the package origin, mirroring how it stamps the
        /// `ExternalTypeShape.Record` the candidate was indexed from.
        Origin: SymbolOrigin
        /// Every declared field name — enough for BOTH the per-field intersection and
        /// the count tie-break ("fields determine a unique record type"). Field *types*
        /// come from the by-key shape path at construction, not from here.
        FieldNames: string[]
        /// True when the record is `[<RequireQualifiedAccess>]`: F#'s
        /// `isILOrRequiredQualifiedAccess` guard excludes such records from bare
        /// field-set resolution (`{ X = … }` must be qualified). Threaded from the
        /// declaration's decoded attributes through freeze (`TTypeDecl.IsRequireQualifiedAccess`,
        /// projected by `FrozenSignature`) and honoured by the bare-construction filter
        /// (`InferResolve.admitsBareExternalRecord`). The qualified `{ R.X = … }` path
        /// is unaffected — it resolves `R` by name and stays constructible.
        IsRequireQualifiedAccess: bool
    }

/// The immutable, two-axis member descriptor: a
/// member's tupled `(Parameters, Return)` as `FrozenType` templates. `Parameters`
/// is the .NET-tupled argument type (`N ≥ 2` → one `FTTuple`; 0 params →
/// `unit`); `Return` the result. Open typars are baked as `FTTypar(Declaring,i)`
/// (the declaring type's typars) / `FTTypar(Method,j)` (the method's own) — the
/// `DeclaringTyparArity` / `MethodTyparArity` counts give each axis's width. For a
/// value member (`ExternalMember.IsValueMember` — a field or property) there are no
/// parameters: `Parameters` is `unit` and the value type lives in `Return`; consumers
/// gate reconstruction on `IsValueMember` (see `ExternalSymbols.instantiateSignature`).
/// The two-axis
/// data form is what unblocks generic external static methods (`truncate`).
type ExternalSignature =
    {
        DeclaringTyparArity: int
        MethodTyparArity: int
        Parameters: FrozenType
        Return: FrozenType
        /// Per-method-typar UPPER BOUND (`<Key extends keyof Events>`), aligned to the
        /// method axis: index `j` is the `j`-th method typar's constraint as a CARRIED
        /// `FrozenType` (`keyof(FTTypar(Declaring,0))`), `ValueNone` when unconstrained.
        /// Baked over the DECLARING typars just like `Parameters`/`Return`, so
        /// `ExternalSymbols.instantiateSignatureBounds` realises it against the use-site
        /// declaring args (`keyof Events` at `Emitter<R>` → `TyKeyOf R`). Length is
        /// `MethodTyparArity` when any bound is present, else EMPTY (the churn-free default
        /// for every non-TS producer — reflection/`.fsi`/JS-native carry no TS keyof
        /// bound). Consumed by the call-site literal-grounding rule (R4a step 3 item 2):
        /// a syntactic string constant admits into a method typar whose bound's
        /// keyof-fold contains it, solving that typar to `TyLiteral`. External-vocabulary
        /// only.
        MethodTyparBounds: FrozenType voption[]
    }

    /// The deferred sentinel a contract-layer member carries between extraction
    /// and the `ExtractCtx.toProvider` finalize pass (which fills `Parameters` /
    /// `Return` by translating the stashed signature CST once the registry is
    /// complete). `DeclaringTyparArity` / `MethodTyparArity` are recorded eagerly so the
    /// finalize pass needs only the CST. Metadata-layer (`MetadataSymbols`,
    /// reflection-backed) members skip this and build their template eagerly —
    /// their shapes are total and registry-independent.
    static member deferred(declaringTyparArity: int, methodTyparArity: int) : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            Parameters = deferredTemplate
            Return = deferredTemplate
            MethodTyparBounds = [||]
        }

    /// Smart constructor for the COMMON eager case: a fully-known `Parameters` /
    /// `Return` template with no TS keyof method bounds. Defaults the churn-prone
    /// `MethodTyparBounds` to EMPTY (every non-TS producer — reflection / `.fsi` /
    /// JS-native — carries no bound), so a future signature field is a one-site
    /// addition here rather than an edit at each construction site. The rare
    /// bound-carrying producer (the TS-manifest `signatureOf`) still builds the
    /// record explicitly.
    static member make
        (declaringTyparArity: int, methodTyparArity: int, parameters: FrozenType, return': FrozenType)
        : ExternalSignature =
        {
            DeclaringTyparArity = declaringTyparArity
            MethodTyparArity = methodTyparArity
            Parameters = parameters
            Return = return'
            MethodTyparBounds = [||]
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
        /// The storage/shape axis: `Field` / `Property` (value members) vs `Method`
        /// (function member). `Field` vs `Property` matters only at CLR emission
        /// (`ldfld` vs `call get_X`); consumers that only need value-vs-function read
        /// `IsValueMember`. See `MemberStorage`.
        Storage: MemberStorage
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
        /// codegen reads `MethodTyparArity` to mint the `MethodSpec`'s generic-parameter
        /// count.
        MethodTyparArity: int
        Origin: SymbolOrigin
        /// The interned identity — a `MemberKey`, the ONE kind a member entry can have:
        /// the *open* declaring type (its `argSig` in `!0`-typars) + name + kind, minted
        /// by the resolving source. Elaborate WIDENS it (`SymbolKey.Member`) into
        /// `TExpr.ExternalMember`, where the IR's nominal payloads are still `SymbolKey`;
        /// a consumer holding the entry reads `Key.Decl` / `Key.ArgSig` / `Key.Kind`
        /// directly, so "what if it isn't a member?" cannot be asked here.
        Key: MemberKey
        /// The compile-time-constant default values of this member's *trailing*
        /// optional parameters (`ArrayPool<'T>.Return(array, [<Optional>] clearArray =
        /// false)` ⇒ `[Bool false]`), in declaration order. A call may omit any
        /// suffix of these: the front end (`InferExternalCall.tryFillOptionalCall`)
        /// permits the under-applied arity and Elaborate synthesises the omitted defaults
        /// as literal arguments so codegen sees the full tupled call unchanged. Empty
        /// for a member with no omittable optionals — every property, ctor, and the
        /// contract (`.fsi`) layer, which doesn't publish optional defaults yet. Only
        /// constants representable as a `TConstValue` are surfaced; an optional whose
        /// default is `null` / a non-primitive `default(struct)` ends the trailing run
        /// (that parameter stays required), so no call can omit past it.
        OptionalDefaults: TConstValue list
        /// True for an OPTIONAL interface member (`verbose?: T`) — a structural-width
        /// admission at a foreign-call arg position treats it as not-required. Every
        /// non-interface producer (metadata, .fsi contract, JS-native, ctors) sets `false`.
        IsOptional: bool
        /// The member's splice TEMPLATE, when it has one — a concrete `(# … #)`-bodied
        /// member, lifted `this`-first (`SymbolProviders.liftMemberBody`) so it
        /// splices through the same path a `let inline` value does. `ValueNone` for a
        /// real callable. See `ExternalSymbol.InlineBody` for why it is folded here.
        ///
        /// A splice site must select the member by EXACT `Key`
        /// (`IExternalSymbolStore.TryLookupMemberByKey`), never by a name lookup:
        /// `TryLookupMember` collapses an overload set to a single best-by-arity pick, so
        /// a name re-lookup can hand back a DIFFERENT overload's body than the one the
        /// use-site node's `MemberKey` names.
        InlineBody: InlineBody voption
    }

    /// The KEYED zero every member literal is copied from
    /// (`{ ExternalMember.OfKey key with … }`): identity is an ARGUMENT, so no producer
    /// can mint a member without one, and `Name` is DERIVED from the key rather than
    /// written beside it — the two cannot disagree. Everything a producer has no opinion
    /// on (the inline body, the optional defaults, the optional-member flag, the origin a
    /// stacking wrapper stamps) defaults here, so a future field is a one-site addition
    /// rather than an edit at every construction site. `Signature` is the `deferred`
    /// sentinel: a copy that does not fill it publishes no signature, which is exactly what
    /// the contract layer wants between extraction and its finalize pass.
    static member OfKey(key: MemberKey) : ExternalMember =
        {
            Name = key.Name
            IsStatic = false
            Storage = MemberStorage.Method
            Signature = ExternalSignature.deferred (0, 0)
            MethodTyparArity = 0
            Origin = SymbolOrigin.Empty
            Key = key
            OptionalDefaults = []
            IsOptional = false
            InlineBody = ValueNone
        }

    /// A value member (field or property) — no parameters, the value in `Return` —
    /// as opposed to a `Method`. The single predicate the inference/freeze
    /// consumers gate on; only CLR emission cares about `Field` vs `Property`.
    member m.IsValueMember = m.Storage.IsValueMember

    /// The canonical `.ctor` member shape every layer must agree on: `Name =
    /// ".ctor"`, instance, non-property, `MethodTyparArity = 0`, keyed as a
    /// `MemberKind.Method` over `declKey`. The metadata layer (`MetadataSymbols`),
    /// the `.fsi` contract extractor (`VesperLib`), and the JS-native stubs
    /// (`JsNativeSymbols`) all mint a constructor through this, so the constant
    /// fields stay in one place. Only the per-layer parts vary: the frozen
    /// `signature`, the `argSig` the key interns, the `origin`, and any
    /// `optionalDefaults` (metadata-layer only — the contract layers pass `[]`).
    static member ctor
        (declKey: TypeKey)
        (signature: ExternalSignature)
        (argSig: EqArray<FrozenType>)
        (origin: SymbolOrigin)
        (optionalDefaults: TConstValue list)
        : ExternalMember =
        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey ".ctor" argSig 0 MemberKind.Method) with
            Signature = signature
            Origin = origin
            OptionalDefaults = optionalDefaults
        }

/// HOW an external type's instance-member CALLS lower on the JS backend — a single
/// axis replacing the former `Erased`/`AttachMembers` bool pair (both-true was
/// unrepresentable nonsense the type admitted). Exactly one of these holds.
type MemberLowering =
    /// Instance members live ON the object as genuine prototype/own methods — native
    /// `receiver.member(args)` calls / property reads. Named after F#/Fable's
    /// `[<AttachMembers>]`, whose semantic this is. SET by the TS-manifest provider
    /// for real Interface/Class shapes (the object has native methods); CONSUMED by
    /// JS emit to pick the `receiver.member(args)` lowering.
    | AttachedNative
    /// Vesper's OWN emitted form: members compile to receiver-first FREE FUNCTIONS as
    /// a tree-shaking optimisation (the Fable trick). The DEFAULT, so every existing
    /// provider is unaffected.
    | ReceiverFirst
    /// A SYNTHETIC grouping type that does not exist at runtime — its static members
    /// are bare module-level exports collected under one F#-visible type purely so the
    /// front end can resolve them (F# has no free-function overloading; the TS provider
    /// groups overloaded free functions of a module as static members of a synthetic
    /// type named after the module). A call to such a member (`Util.format(x)`) ERASES
    /// at JS emit to the bare export (`format(x)`) — the real export name is the bare
    /// member name, NOT a mangled `Type_member`. SET by the TS-manifest provider;
    /// CONSUMED by the JS emit erase branch.
    | ErasedBare

/// Capability flags on an external class or interface. The metadata layer reads
/// them off the .NET `TypeAttributes` plus
/// `[<AllowNullLiteral>]` attribute decoding; the contract layer leaves them at
/// `Default` until a `.fsi` learns to publish them. Class emission reads
/// `IsSealed` on the declared base type; `[<AllowNullLiteral>]` support reads
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
        /// HOW instance-member calls lower on JS (attached-native / receiver-first /
        /// erased-bare). Replaces the former `Erased`/`AttachMembers` bool pair.
        MemberLowering: MemberLowering
        /// A GLOBAL (ambient) type — one the JS runtime provides intrinsically
        /// (`Map`, `Set`, `Promise`, …), reachable by its BARE name with NO `import`.
        /// A SEPARATE axis from `MemberLowering`: that decides call-lowering shape,
        /// this decides import emission. Fable-named after `[<Global>]`. STAMPED by
        /// the TS-manifest provider for a type whose HOME is a global pack (a
        /// `TsGlobalHomes.isGlobalHome`, e.g. `es2015` mounted under `Js`) —
        /// Global rides the HOME, not the type, so a real-package home keeps `false`
        /// (normal import). CONSUMED by the JS backend: `JsImports.addRef` skips
        /// recording and the external-new / member-emit sites use the bare name.
        Global: bool
        /// The import-STATEMENT shape for this type's home-module exports — the
        /// type-level analog of `ExternalSymbol.ImportForm` (which a VALUE symbol
        /// carries directly). An overloaded free function is routed to a synthetic
        /// `ErasedBare` grouping type, which drops the per-value `ImportForm`; the
        /// group's uniform form is stamped HERE instead so `JsExternalMembers`'
        /// erased-grouping ref can lower `Util.format(x)` to the right import
        /// (`import { format }` for `Named`, `import format` for `Default`/`CommonJs`,
        /// `import * as util; util.format` for `Namespace`). `Named` for every other
        /// producer and type shape (the churn-free default). A SEPARATE axis from
        /// `Global`: that decides import-vs-no-import, this the statement shape.
        ImportForm: ImportForm
    }

    /// The conservative default the contract layer stamps when a `.fsi` only
    /// commits the type's name + arity + interface-ness.
    static member Default =
        {
            IsSealed = false
            IsAbstract = false
            AllowNullLiteral = false
            IsValueType = false
            MemberLowering = MemberLowering.ReceiverFirst
            Global = false
            ImportForm = ImportForm.Named
        }

/// The shape of an external class or interface. Lifted out of `ExternalTypeShape.Class`
/// so the DU header stays narrow and the
/// member set is reachable to consumers (the `interface … with member …`
/// conformance check, the base-type lookup, etc.) without having to round-trip
/// through `TryLookupMember` per name.
///
/// `FrozenInterfaces`, `FrozenBaseType`, and each member's `Signature` are
/// written over the *declaring type's* typars: consumers substitute a
/// `SemType[]` (one entry per declared typar, in declaration order) through the
/// baked `FTTypar(Declaring,i)` placeholders (`ExternalSymbols.instantiateInterfaces`
/// / `instantiateBaseType`).
type ExternalClassShape =
    {
        TyparArity: int
        IsInterface: bool
        /// All public declared methods + properties whose signature maps via
        /// the `tryBuildType`. Sibling members the metadata layer can't
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
            TyparArity = arity
            IsInterface = isInterface
            Members = [||]
            FrozenInterfaces = [||]
            FrozenBaseType = ValueNone
            Flags = ExternalClassFlags.Default
            Origin = origin
        }

/// The platform-invariant identity axis a scalar / heritable intrinsic carries — the
/// shared payload of `ExternalTypeShape.Intrinsic`, so consumers of "any intrinsic
/// canon" read one record rather than re-matching per kind. (A capability
/// `IntrinsicInterface` does NOT share it — its `Platform` is always present, so it
/// carries a non-optional `string` rather than this record's `option`.)
///
/// **Two names** (a single repr string used to do two unrelated jobs at once):
/// - `Canon` — the platform-INVARIANT nominal-identity key: the qualified
///   **`.fsi` name** the type was declared under (`Vesper.int`, `Vesper.exn`),
///   i.e. the front-end identity itself, NOT a BCL name. `subsumes`' `canonKey`
///   uses THIS key; it is distinct per nominal type so `int` ≠ `float`, and it
///   is the SAME regardless of which backend is compiling — a JS build never
///   needs to know what the BCL calls `int`.
/// - `Platform` — the per-target runtime/codegen repr: the platform's *name*
///   for the type, sourced from the `<base>.<target>.fs` companion's
///   `type x = (# "<repr>" #)` (`Some "System.Int32"` on CLR, `Some "number"`/
///   `Some "Error"` on JS). Codegen emission + the `exnReprOf`/`tryRuntimeType`
///   runtime axis, and the intrinsic-receiver member probe
///   (`tryExternalReceiver`), read THIS name. Many-to-one and directional — it
///   must never drive unification. **`None` on a NULLARY intrinsic means the
///   type is a known scalar primitive but has NO representation on the
///   compiling target** — e.g. `decimal`/`nativeint` on JS, which ship no
///   `.js.fs` companion; `SemanticAnalysis.PlatformTypes` rejects that up front.
///   `None` on a GENERIC intrinsic (`'T []` on JS) is benign — the structural
///   backend path needs no repr string. On CLR every primitive's base `.fs` IS
///   its platform repr, so `Platform` is always `Some` there.
///
/// `TyparArity` — the type's generic parameter count. Usually `0` (the scalar
/// primitives), but NOT always: the structural type constructors are intrinsics
/// too (`type 'T [] = (# "!0[]" #)`, arity 1; `byref`, nd-array). Load-bearing
/// for representability: only an `arity = 0` intrinsic with `Platform = None`
/// is unrepresentable (see `Platform` above).
type IntrinsicIdentity =
    {
        /// A `TypeKey` — an intrinsic is a nominal TYPE, so the identity axis admits no
        /// other kind. The `SemType`/`FrozenType`-facing consumers widen at the boundary
        /// (`SymbolKey.Type`), which is where the IR still speaks the wide key.
        Canon: TypeKey
        TyparArity: int
        Platform: string option
    }

/// The class surface a HERITABLE primitive (`obj`/`exn`, declared
/// `(# class "…" #)` + `extern class with inherit/new:`) carries on top of its
/// intrinsic identity — the surface a scalar primitive lacks:
/// - `BaseType` — the declared `inherit` parent (`exn`'s is `obj`; `obj`'s is
///   `ValueNone`, the root), with the declaring typars baked as
///   `FTTypar(Declaring,i)`.
/// - `Members` — the contract `.ctor`s (`new: string -> exn`), frozen exactly as
///   a `Class`'s ctors: the SINGLE constructible surface both `new exn "…"`
///   (`InferCtor`) and `inherit exn(…)` (`Unification.fillBaseCtorCall`) check
///   against — target-agnostic; the explicit platform spelling
///   (`new System.Exception(…)`) is the opt-in to the platform's wider catalogue.
///
/// Instance members (`obj.ToString` / `exn.Message`) are NOT here — they are
/// per-target and route through the PLATFORM type (`IntrinsicBclMember`),
/// merging with these contract ctors.
type IntrinsicClassSurface =
    {
        BaseType: FrozenType voption
        Members: ExternalMember[]
    }

/// A *referenced* package's intrinsic-representation binding: an `extern` type
/// whose sibling `.fs` carries `type x = (# "<repr>" #)`. NON-transparent
/// (unlike `Abbrev`): a use site resolves to the nominal `TyConst Id.Canon`,
/// never an expanded repr — REGARDLESS of `Class`, so the identity axis is
/// single-pattern by construction.
///
/// `Class = ValueSome` ⇔ a heritable primitive (`obj`/`exn`): the contract's
/// `class` kind tag IS the predicate, and the added surface lets a downstream
/// file `inherit exn` / `new exn` through the ordinary provider paths while the
/// value identity stays `TyConst` (no `TyClass` churn at the pervasive
/// `obj`/`exn` value sites). Distinct from a capability `IntrinsicInterface`
/// (`disposable`) — an INTERFACE, which resolves to `TyClass`.
type IntrinsicShape =
    {
        Id: IntrinsicIdentity
        Class: IntrinsicClassSurface voption
    }

    /// A scalar (non-heritable) intrinsic — the common mint.
    static member Scalar(canon: TypeKey, arity: int, platform: string option) : IntrinsicShape =
        {
            Id =
                {
                    Canon = canon
                    TyparArity = arity
                    Platform = platform
                }
            Class = ValueNone
        }

/// A **capability interface** (`disposable`/`equatable`/`comparable`) — an intrinsic
/// whose identity axis is a `TyClass` CONSTRAINT rather than a `TyConst` value
/// identity, so it earns its own case beside `Intrinsic` (an interface is excluded
/// from the forward-repr extraction and the unrepresentability gate, and it resolves to
/// `TyClass` not `TyConst`). Minted ONLY on a target whose `.fs` binds the platform
/// repr (CLR); on JS a capability surfaces as a plain canon-only interface `Class`.
///
/// Carries its own identity fields rather than a shared `IntrinsicIdentity`: unlike a
/// scalar `Intrinsic` (whose `Platform` is an `option` — `None` = unrepresentable, the
/// gate's reject signal), a capability interface is minted ONLY when its `.fs` binds
/// the repr, so its `Platform` name is always present — a non-optional `string` that
/// keeps the `Some`-unwraps at `resolveAnchor` / `externalClassRef` / `ClrExternalMembers`
/// total and removes the two-polarity hazard of sharing the scalar's optional field.
///
/// - `Canon` — the platform-INVARIANT `.fsi` short-name identity (`Vesper.disposable`):
///   the reconciliation / capability-matching key (`resolveAnchor`'s `CanonKey`) and the
///   pre-split namespace `stampType` homes the `Origin` on. NOT the value-resolution key.
/// - `Platform` — the `.fs` `(# … #)` BCL repr (`"System.IDisposable"`), driving CLR
///   reconciliation + the `ClrEnv` InterfaceImpl redirect.
/// - `TyparArity` — the type's generic parameter count (`equatable<'T>` = 1).
/// - `Members` — the abstract member surface (`Dispose`), read by
///   `Unification.checkInterfaceConformance`. Populated at finalize (after the
///   deferred member loop) via the `PendingCapabilityInterfaces` republish.
/// - `Origin` — the manifest home (assembly + namespace), stamped by
///   `ExternalSymbolProviders.stack`'s `stampType` exactly as a `Class`'s is. The VALUE
///   resolution key uses THIS (`externalTypeKey Origin`) for its namespace; `Canon` is the
///   reconciliation/capability-matching key only.
type IntrinsicInterfaceShape =
    {
        /// A `TypeKey` — a capability is a nominal INTERFACE type; no other kind can name
        /// it, so `Canon.Namespace` is a field read rather than a match with a fallback.
        Canon: TypeKey
        TyparArity: int
        Platform: string
        Members: ExternalMember[]
        /// The capability's directly-inherited interfaces as `(compiled-name, type-args)`
        /// pairs (declaring typars baked as `FTTypar(Declaring,i)`), the capability-interface
        /// analogue of `ExternalClassShape.FrozenInterfaces` / `Union.interfaces`. Lets a
        /// scanner (`Infer.tryExternalDispose`, subsumption) see that `enumerator` inherits
        /// `disposable` — BCL parity for `IEnumerator`1 : IDisposable`. Empty for a leaf
        /// capability (`disposable`/`equatable`/`comparable`).
        Interfaces: (string * FrozenType[])[]
        Origin: SymbolOrigin
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
    /// A follow-up reads it to mint a `TypeRef` for
    /// cross-package record emission (an external `RecordCons` / field access).
    | Record of arity: int * fields: ExternalFieldShape[] * origin: SymbolOrigin
    /// Case order matches source. `origin` is filled by the layer that knows
    /// where the type lives (`ReferencedProject.wrap` from the manifest's
    /// assembly + namespace); the inner extractor records `SymbolOrigin.Empty`.
    /// Codegen reads it to mint a `TypeRef` for the case factories on a
    /// cross-package `Some`/`None` construction exactly as `Record` does for `RecordCons`.
    /// `interfaces` are the union's directly-declared `interface <ty>` impls as
    /// `(compiled-name, type-args)` pairs (each interface's args with the declaring
    /// typars baked as `FTTypar(Declaring,i)`) — the union analogue of
    /// `ExternalClassShape.FrozenInterfaces`. It lets `tryForInEnumerator` admit a
    /// bare cons-list whose `.fsi` union declares `interface seq<'T>`.
    | Union of arity: int * cases: ExternalCaseShape[] * interfaces: (string * FrozenType[])[] * origin: SymbolOrigin
    /// A TS-manifest (or otherwise external) enum: a closed, nominal set of named
    /// constant cases in source order. No `arity` field — enums are never generic
    /// (mirroring `TyEnum` / `FTEnum`). The numeric / string / mixed variant is
    /// DERIVED from `cases` (`TEnumCases.classify`), never baked. `origin` (the
    /// provider stamps the module specifier) mints the enum's `SymbolKey`, so a use
    /// site resolves to `TyEnum` / `FTEnum` and JS IMPORTS the enum object
    /// (`import { E }`) at each `E.Ci` rather than re-emitting its object map. This
    /// is the body the extractor previously dropped behind `Opaque`. External enums
    /// are a JS-target feature and never reach CLR codegen.
    | Enum of cases: ExternalEnumCaseShape[] * origin: SymbolOrigin
    /// A class or interface (the gap that makes `EqualityComparer<_>` resolve to
    /// `ValueNone` today). The members / interfaces / base-type / flags ride
    /// inside `ExternalClassShape`, lifted out of the DU header so interface
    /// conformance and base-type lookup can reach them
    /// directly. Contract-layer providers stamp `ExternalClassShape.basic`; the
    /// metadata layer fills the rich form.
    | Class of shape: ExternalClassShape
    /// A *referenced* package's intrinsic-representation binding: an `extern`
    /// type whose sibling `.fs` carries `type x = (# "<repr>" #)`
    /// (`type exn = (# class "System.Exception" #)`, prim-types-exn.fs) — scalar
    /// (`int`) or heritable class (`obj`/`exn`). Identity, representability, and
    /// the optional class surface all ride `IntrinsicShape` (lifted out like
    /// `ExternalClassShape`, so the DU header stays narrow and the identity axis
    /// is ONE pattern regardless of heritability — see the record's docs for the
    /// canon/platform split).
    ///
    /// The two names **diverge on every target** (CLR: `int`/`Some "System.Int32"`):
    /// `Canon` is `.fsi`-defined, `Platform` is `.fs`-defined. An incoming
    /// BCL/native runtime name on the metadata seam (e.g. `System.Exception`
    /// surfaced by a metadata `inherit` chain) is reconciled back to its `Canon`
    /// through the reverse `{ platform -> canon }` map
    /// (`IExternalSymbolProvider.IntrinsicReverseCanon`), so `int`-as-metadata and
    /// `int`-as-contract still meet at `"int"`. The *local* `IntrinsicReprTypes`
    /// twin (`TypeRegistry.fs`) stays single-string: it holds a self-compiled file's
    /// own `platform` repr keyed by the `.fsi` short name (which is the canon).
    | Intrinsic of shape: IntrinsicShape
    /// A capability interface (`disposable`/`equatable`/`comparable`): an intrinsic on
    /// the `TyClass`-constraint axis. A DISTINCT case from `Intrinsic` because an
    /// interface differs on identity (resolves to `TyClass`, not a `TyConst` value),
    /// is excluded from the forward-repr extraction + the unrepresentability gate, and
    /// carries a member surface + `Origin`. See `IntrinsicInterfaceShape`. CLR-only
    /// (a JS capability is a plain canon-only interface `Class`).
    | IntrinsicInterface of shape: IntrinsicInterfaceShape
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

    /// The shape's syntactic arity — the guard every resolution path applies so a
    /// generic type referenced at the wrong arity isn't mistaken for this type
    /// (and the abbrev/record builders get a right-length arg array).
    member this.TyparArity: int =
        match this with
        | Class info -> info.TyparArity
        | Intrinsic s -> s.Id.TyparArity
        | IntrinsicInterface s -> s.TyparArity
        | Enum _ -> 0 // enums are never generic
        | Record(arity = a)
        | Union(arity = a)
        | Abbrev(arity = a)
        | Opaque(arity = a) -> a

/// The **resolver view** of the external-symbol contract: *spelling → identity*
/// (`string → identity`). Opens-aware — this is where a source spelling is turned
/// into a resolved symbol/type/case. String-keyed is CORRECT here: it is the one
/// layer (with the contract extractor) that owns `string × OpenScope → SymbolKey`
/// resolution. Every pass downstream of NameResolution speaks the key-addressed
/// `IExternalSymbolStore` instead. The oracle doctrine and thread-safety contract
/// on `IExternalSymbolProvider` govern this view too.
type IExternalSymbolResolver =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption
    /// Look up a `type` declaration by canonical compiled name: its REGISTERED identity
    /// AND its body shape, from the one hit. Returns `ValueNone` for unknown names.
    abstract TryLookupType: name: string -> struct (TypeKey * ExternalTypeShape) voption

    /// Reverse case-name lookup: a (bare) union-case name → its declaring
    /// union's compiled name, the union's typar arity, and the case shape. The
    /// mirror of `TryLookupMember` for union construction: it lets a consumer
    /// type `Some 5` / `None` against an external union without a type
    /// annotation, exactly as F# brings a non-`RequireQualifiedAccess` union's
    /// cases into scope when its namespace is opened v1 is first-declaration-wins on a name collision (the same rule
    /// the short-name type index uses); providers that don't model unions return
    /// `ValueNone`.
    abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption

    /// The per-field reverse index (F#'s `eFieldLabels` analogue): a field name →
    /// every record declaring a field of that name. Unqualified record-literal /
    /// record-pattern resolution intersects these per-field sets to pin the type
    /// (`{ X = …; Y = … }` resolves to the record at the intersection). `[||]` = no
    /// record here; providers not backed by Vesper TAST (metadata / JS-native /
    /// TS-manifest / test fakes) return `[||]`, exactly as F# reverse-indexes only F#
    /// record tycons and never imported IL. RQA records are excluded from the index at
    /// the source that builds it (F#'s `isILOrRequiredQualifiedAccess` guard).
    abstract TryRecordsWithField: fieldName: string -> ExternalRecordCandidate[]

    /// The *ambient* (implicit) open-prefix set this provider contributes — the
    /// prelude / referenced-contract `[<AutoOpen>]` modules. The pipeline seeds
    /// `PassContext.Resolution.AmbientOpenScope` from it, where it is probed
    /// strictly BEHIND explicit `open`s: a short name tries its bare form and
    /// every explicit open first, and only then these ambient prefixes
    /// Dotted prefixes in
    /// priority order (earliest wins on a collision), e.g.
    /// `["Vesper.ArithmeticOperators"; "Vesper"]`. Providers with no implicit
    /// prelude (inline test fakes) return `[]`. Required (was the
    /// optional `IAmbientOpenScope` cast); folded in alongside the intrinsic
    /// surface, which rides `TryLookupType` via `ExternalTypeShape.Intrinsic`.
    abstract AmbientOpenPrefixes: string list

/// The **store view** of the external-symbol contract: *identity → payload*
/// (`SymbolKey → payload`). What Unification, Elaborate, InlineExpansion, and codegen
/// speak once identity is already resolved — no consumer re-derives identity from a
/// spelling here. Type / index / member / symbol lookups are addressed by the resolved
/// `SymbolKey` a front-end consumer already holds; a member *name* stays a string
/// (a post-dot member spelling is not opens-sensitive — only the declaring type's
/// identity is). Implementations may satisfy the key-addressed methods by
/// projecting `SymbolKeyOps.qualifiedName` INTERNALLY — the string round-trip is an
/// implementation detail, never a call-site idiom. **Lookup is addressed by
/// `(ns, arity-qualified name)` and NOTHING ELSE** — which is the whole of a key's
/// nominal identity, so a key minted from a bare compiled-name string
/// (`SymbolKeyOps.qualifiedTypeKey`) answers exactly the entries a fully resolved one
/// does. Two same-named types in different assemblies are therefore indistinguishable
/// to the store view (the CS0433 sweep in `ReferencedProject.fs` exists to make that
/// collision impossible upstream rather than to resolve it here).
type IExternalSymbolStore =
    /// Look up a `type` declaration's body by the resolved `SymbolKey` a consumer
    /// already holds — the key-addressed twin of
    /// `IExternalSymbolResolver.TryLookupType`. Returns `ValueNone` on the same
    /// terms (unknown key, or a body shape the provider doesn't model).
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption

    /// Look up a static/instance member on an external type by the declaring
    /// type's resolved `SymbolKey` and the member name. This is what types
    /// `EqualityComparer<'T>.Default` (static property) and `.GetHashCode`
    /// (instance method). Defaults to `ValueNone` for providers that don't model
    /// members. When several overloads share a name
    /// this collapses to a single best-by-arity pick; the *call site* uses
    /// `TryLookupMembers` instead to resolve by argument types.
    abstract TryLookupMember: key: SymbolKey * memberName: string -> ExternalMember voption

    /// Look up **all** overloads of a member by name — the candidate set the
    /// application-site overload resolver picks from (by arity, then argument-type
    /// betterness). Providers that don't model members
    /// return `[||]`. A provider that models members SHOULD return every overload
    /// whose signature maps (the same filter `TryLookupMember` applies, minus the
    /// single-pick collapse).
    abstract TryLookupMembers: key: SymbolKey * memberName: string -> ExternalMember[]

    /// Look up a member by the resolved `MemberKey` a consumer already holds — the
    /// key-addressed twin of `TryLookupMember`, and the channel a MEMBER splice site
    /// reaches an `InlineBody` through (`ExternalSymbolProviders.tryInlineBody`).
    ///
    /// It must be BY KEY, not by name: `TryLookupMember` collapses an overload set to a
    /// single best-by-arity pick, so a name lookup can hand back a DIFFERENT overload's
    /// entry — and hence a different overload's body — than the one the use site's
    /// `MemberKey` names. A member's key is its whole nominal identity (declaring type +
    /// name + `argSig` + kind), so this channel names exactly one entry.
    /// Providers that model no members return `ValueNone`.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption

    /// The TS index signature(s) `{ [k: K]: V }` on an external type, by the type's
    /// resolved `SymbolKey` — the seam `x.[k]` / `x.[k] <- v` reads/writes through (each
    /// entry a `(keyTemplate, valueTemplate)` pair of `FrozenType`s over the type's
    /// DECLARING typars, realised at a use site via `FrozenTypeBridge.instantiateDeclaring`
    /// against the receiver's args, exactly as a member `Signature` is). EMPTY = no
    /// index signature. A type may declare BOTH a string- and a number-index entry, so
    /// the list carries all and the consumer (`inferIndexedLookup`) selects by the index
    /// expression's type. Parallel to `TryLookupMember`; providers that model no index
    /// signatures (metadata, .fsi contract, JS-native, test fakes) return `[]`.
    abstract TryLookupIndexSignature: key: SymbolKey -> (FrozenType * FrozenType) list

    /// Look up a value/free-function symbol by the resolved `SymbolKey` a consumer
    /// already holds — the key-addressed twin of `IExternalSymbolResolver.TryLookup`,
    /// and the channel a VALUE splice site reaches an `InlineBody` through (the
    /// `SymbolKey.Binding` arm of `ExternalSymbolProviders.tryInlineBody`; a member's
    /// body rides `TryLookupMemberByKey`).
    ///
    /// It must be BY KEY, not by name. A splice site holds an identity, not a
    /// resolvable spelling: an inline body's intra-body reference to a SIBLING template
    /// carries the sibling's simple name, which provably does not resolve (the index is
    /// qualified-name keyed and the holder is not auto-opened) — and re-resolving any
    /// spelling at splice time would reintroduce the user-shadow hazard the key channel
    /// exists to kill.
    abstract TryLookupByKey: key: SymbolKey -> ExternalSymbol voption

    /// The reverse intrinsic axis `{ platform-repr -> [canon] }`, so the unifier can
    /// reconcile an incoming BCL/native *runtime* name (the `platform` name, e.g.
    /// `"System.Exception"` surfaced by a metadata `inherit` chain on CLR) back to the
    /// short front-end identity (`canon`, the `.fsi` name, e.g. `"exn"`). ONE-TO-MANY:
    /// a single platform repr can be the target of several canons — on JS `int`,
    /// `float`, and `float32` all share repr `"number"`, so `"number" -> ["int";
    /// "float"; "float32"]`; on CLR each platform name maps to exactly one canon, so
    /// the list is a singleton. Consumers that want a single canon take the head/sole
    /// element. The forward `canon` axis lives on `ExternalTypeShape.Intrinsic` and is
    /// reachable by name via `IExternalSymbolResolver.TryLookupType`; the reverse axis
    /// cannot be (it is keyed by the platform repr, a different string axis than a
    /// source spelling), so it is published as data here. The intrinsic-carrying
    /// providers (`ExtractCtx.toProvider`) and their composite (`ExternalSymbolProviders.stack`)
    /// build a real map; metadata / JS-native / test providers carry no intrinsics and
    /// return `Map.empty`.
    abstract IntrinsicReverseCanon: Map<string, SymbolKey list>

    /// The FORWARD intrinsic axis `{ canon -> platform-repr }` (the `.fsi` short name
    /// -> its `.fs` `(# … #)` repr for the compiling target) — the mirror of
    /// `IntrinsicReverseCanon`. The CLR *codegen* backend reads it to resolve a
    /// primitive's IL representation from the single `.fs` source; a bare canon like
    /// `"int"` is the open-resolved identity codegen carries (opens are a name-resolution
    /// concern, already discharged), and `TryLookupType` can't serve it because intrinsics
    /// are keyed there by qualified compiled name. On JS the codegen backend resolves reprs
    /// by its own path and never reads this axis, but the map is still POPULATED on the JS
    /// contract stack (extracted from the `.js.fs` `(# … #)` bindings, exactly as on CLR):
    /// the JS front-end seams depend on it — `reprSiblings` (structural-width admission) and
    /// `Codegen.Js.NumberCovariance` (the covariant `number → float` target, asserted to
    /// repr to `number`) both read it. The intrinsic-carrying providers
    /// (`ExtractCtx.toProvider`) and their composite build a real map on EVERY target;
    /// metadata / JS-native / test providers return `Map.empty`.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

/// A mechanical metadata / contract ORACLE combining both views: the resolver
/// (spelling → identity) and the store (identity → payload). Every backing object
/// (VesperLib contract, metadata, JS-native, TS-manifest, test fakes) implements
/// THIS combined interface — one object, both duties — so the two views are always
/// free upcasts of the same object (`p :> IExternalSymbolStore`), no forwarding.
///
/// **The oracle doctrine (both views, every channel):** the contract carries **no
/// capability predicates** — no `IsDisposable` / `IsEquatable` here, and there must
/// never be. "Is this type disposable?" is a language-semantics judgment the passes
/// derive from the raw facts (`FrozenInterfaces`, `Members`) against the resolved
/// `CapabilityIds`; folding the verdict in would smuggle a language decision into
/// the metadata layer. The provider's *data* may grow; its *interface* stays a dumb
/// oracle — the absence of those members IS the constraint, do not add them.
///
/// **Thread-safety (both views, every channel):** every lookup — value, type, case,
/// member, index-signature, by-key symbol, and the intrinsic axes — must be safe to
/// call concurrently from multiple threads. Implementations that cache lazily must
/// guard their internal mutation. Per-file pipelines run independent `PassContext`s
/// in parallel and may hit the same provider from any of them — see
/// [`docs/architecture.md`](docs/architecture.md#parallelism).
type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore

/// The open signature of an external module-level function as the codegen
/// boundary sees it: the curried
/// `param -> … -> return` template with the function's own typars baked as
/// `FTTypar(Method, i)`, plus the home `Origin` the call's `MemberRef` parent is
/// minted against and the method-typar count for the `MethodSpec`. Immutable data:
/// `Signature` is `FrozenType`, so the codegen side never touches a `SemType` or the
/// symbol's mutable `Instantiate` closure.
type CodegenOpenSignature =
    {
        Origin: SymbolOrigin
        Signature: FrozenType
        MethodTyparArity: int
        /// The SOURCE arity carried across the assembly boundary: how the producer grouped
        /// curried / tupled parameters (`ValRepr.Groups`). The codegen boundary reads
        /// it to flatten / lone-unit-erase the member-ref parameters and split the
        /// call's applied arguments, and derives the flat `CompiledForm`
        /// (`TastLower.compiledOf`) for the `void`-vs-value decision — replacing the
        /// ambiguous `uncurryFrozen` reconstruction of the curried `Signature` (which
        /// can't tell a tupled group `f (x,y)` from a single tuple param
        /// `f (t:int*int)`). `ValueNone` for a symbol with no captured arity (a value,
        /// a metadata-layer symbol); the boundary then keeps the curried
        /// reconstruction. The compiled form is never stored alongside — it is fully
        /// determined by the `ValRepr`.
        ValRepr: TastAccessor.ValRepr voption
        /// The symbol's `when 'a :> <ty>` bounds, frozen over the method-typar axis
        /// (`FTTypar(Method, i)` leaves), in the SAME `FrozenConstraint` shape the
        /// project-local `EmitCall` phantom-typar solve consumes. Re-opens the channel
        /// the codegen view deliberately stripped: the external head's
        /// phantom-typar solve recovers a phantom slot (`fold`'s `'E`) from
        /// the constrained source's interface witness, exactly as the project-local
        /// head does. Empty for a symbol with no subtype bounds.
        Constraints: FrozenConstraint list
    }

/// The **codegen-facing** view of the external-symbol contract. Where `IExternalSymbolProvider` exposes the
/// inference surface (the `SemType`-returning `Instantiate`, `Constraints`, inline
/// bodies, the ambient-open scope), this exposes **only** what emission needs to mint
/// references: the type/member shapes (whose `FrozenType` templates codegen reads — it
/// never runs the legacy `SemType[] -> SemType` closures) and the open signature of a
/// module-level function. `ClrEnv` holds this instead of `IExternalSymbolProvider`, so
/// the emission code can no longer reach `Instantiate` / constraints / mutable inference
/// state. One backing provider implements both views (`CodegenSymbols.ofProvider`).
///
/// **Every channel is `SymbolKey`/`MemberKey`-addressed, and NONE returns an overload
/// SET.** Codegen never disambiguates overloads: the front end already resolved the
/// member and stamped its `MemberKey` on the node, so a member is fetched by that key
/// (`TryLookupMemberByKey`), never re-picked from a name's candidate array. The two
/// queries that genuinely ENUMERATE — a ctor's arity fallback and a capability member's
/// base-declarer rebase — do their walk BEHIND this seam and hand back a single member /
/// a rebased key, so no `ExternalMember[]` ever crosses to emission.
type ICodegenSymbols =
    /// A `type` declaration's shape by its resolved key (the parent `TypeRef` + the
    /// field/case templates codegen encodes). Single-probe; the bare-vs-arity-suffix
    /// registration split is reconciled once in `CodegenSymbols.lookupTypeByKey`.
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption
    /// The exact member the front end resolved, by the `MemberKey` it stamped — no
    /// overload re-pick. `ValueNone` when the provider models no such member.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
    /// Select the `.ctor` a `new` emits. By the exact `MemberKey` the front end recorded
    /// (`chosen`) when it has one — a total identity, so same-arity overloads
    /// (`ArgumentException(string, string)` vs `(string, Exception)`) are told apart by key,
    /// not re-picked. A heritable primitive's recorded ctor key is already PLATFORM-valid
    /// (`new exn` records a `System.Exception::.ctor`): the provider stamps the platform decl
    /// off `IntrinsicForwardRepr` when it republishes the intrinsic surface, so the backend
    /// never rebases. `chosen` is absent for a ctor node that records no identity — the printf
    /// `%a`/`%t` scratch (`new StringBuilder()`) and an external-base `inherit` chain
    /// (`inherit exn(msg)`, which has no `TExpr.New`) — and the sole ctor of `arity` params is
    /// taken (these have no same-arity ambiguity). Not overload disambiguation; one member out,
    /// never an array.
    abstract TryLookupCtor: declKey: SymbolKey * chosen: SymbolKey voption * arity: int -> ExternalMember voption
    /// Rebase a member call resolved against a capability's platform interface onto its
    /// true base declarer when the member is inherited: `enumerator.MoveNext` is declared
    /// on the non-generic `IEnumerator`, not the `IEnumerator`1` it was called on, and a
    /// member-ref parented on the derived interface would `MissingMethodException`.
    /// `ValueNone` when `key` names no capability member, is declared on that interface
    /// itself, or has no base to rebase onto — declining beats minting a wrong ref.
    abstract TryRebaseCapabilityMember: key: SymbolKey -> SymbolKey voption
    /// The open `FrozenType` signature of a module-level function by its value key, or
    /// `ValueNone` for an unknown symbol or one with no home assembly (a project-local
    /// symbol the provider never sees — the caller falls back to its hard error).
    abstract TryLookupOpenSignature: key: SymbolKey -> CodegenOpenSignature voption
    /// The forward intrinsic axis `{ canon -> platform-repr }` (see
    /// `IExternalSymbolProvider.IntrinsicForwardRepr`): codegen resolves a primitive
    /// canon name (`"int"`) to its `.fs`-declared repr (`"System.Int32"`) here — the
    /// single source of a primitive's representation.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

module ExternalSymbols =

    // The generic `SymbolKey` ↔ compiled-name string algebra (`bareName`,
    // `arityName`, `bindingKeyOf`, `simpleName`, `qualifiedName`, `externalTypeKey`,
    // …) lives in `module SymbolKeyOps` (compiles before this file, so
    // `RuntimeNames` can route through it without depending on the provider
    // surface). This module keeps only the resolution surface that genuinely
    // needs `IExternalSymbolProvider` / `ExternalSymbol`.

    /// The shared empty forward-repr axis for the intrinsic-less providers (metadata /
    /// JS-native / test fakes / `nullProvider`). `SymbolKey` is equatable-but-not-
    /// comparable, so the axis is a read-only `Dictionary`, not a `Map` — this is its
    /// canonical empty value.
    let emptyForwardRepr: IReadOnlyDictionary<SymbolKey, string> =
        Dictionary<SymbolKey, string>() :> IReadOnlyDictionary<_, _>

    /// The identity of a type a NAME-INDEXED leaf resolved: the key its own name index
    /// round-trips to. This is `KeyedLeaf.ofNamed`'s `SymbolKeyOps.qualifiedName` rendering
    /// INVERTED, so it is sound on exactly the condition that already makes such a leaf
    /// answerable by key at all — its type keys are `InNamespace`, where the name and the
    /// key say the same thing. A leaf that mints an `InModule` chain must supply a real key
    /// index (`KeyedLeaf.ofKeyIndexes`) instead, in both directions alike.
    ///
    /// The arity comes from the SHAPE — the type's own — never from the arity a probe
    /// happened to ask for: a bare-keyed generic (`Vesper.Option`, arity 1) answers a bare
    /// probe, and its identity is still arity 1.
    let nameKeyedTypeHit (name: string) (shape: ExternalTypeShape) : struct (TypeKey * ExternalTypeShape) =
        struct (SymbolKeyOps.qualifiedTypeKeyOf name shape.TyparArity, shape)

    /// The shape half of a by-name type hit, for the consumers that resolve a compiler-minted
    /// repr string rather than a written name and so have no use for the identity.
    let typeShapeOf (hit: struct (TypeKey * ExternalTypeShape) voption) : ExternalTypeShape voption =
        hit |> ValueOption.map (fun (struct (_, shape)) -> shape)

    /// Select the ONE entry of a by-NAME overload set whose identity is `key` — how a
    /// store whose member index is name-keyed answers `TryLookupMemberByKey`. Spelled
    /// once, here, so no consumer of the store view ever re-implements "find my overload"
    /// over `TryLookupMembers` (a consumer holds an identity and must be answered under
    /// it, not handed a candidate set to sift).
    let memberByKey (key: MemberKey) (candidates: ExternalMember[]) : ExternalMember voption =
        match candidates |> Array.tryFind (fun m -> m.Key = key) with
        | Some m -> ValueSome m
        | None -> ValueNone

    /// The member surface an external nominal publishes — a `Class` or a capability
    /// `IntrinsicInterface` both carry `ExternalMember[]`, so a consumer reading "the
    /// members of this shape" (`keyof`, interface conformance) treats them identically.
    /// The two arms live HERE, not open-coded at each read site, so a future
    /// member-bearing shape is wired in one place rather than forgotten at one of them.
    [<return: Struct>]
    let (|ExternalMembers|_|) (shape: ExternalTypeShape) : ExternalMember[] voption =
        match shape with
        | ExternalTypeShape.Class shape -> ValueSome shape.Members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// The member surface of an external INTERFACE specifically — an interface-flagged
    /// `Class` or a capability `IntrinsicInterface`. The record→interface structural
    /// widen and `interface … with` dispatch on this; a non-interface `Class` is excluded
    /// (a record cannot widen to a concrete class). The `IntrinsicInterface` arm is shared
    /// with `(|ExternalMembers|_|)` — both centralise it so neither drifts.
    [<return: Struct>]
    let (|ExternalInterfaceMembers|_|) (shape: ExternalTypeShape) : ExternalMember[] voption =
        match shape with
        | ExternalTypeShape.Class {
                                      IsInterface = true
                                      Members = members
                                  } -> ValueSome members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// Whether an external shape is an interface: an interface-flagged `Class` (a metadata
    /// / `.d.ts` interface, or a JS capability) or a CLR capability `IntrinsicInterface`. A
    /// capability is an interface on BOTH targets — only the shape differs (the CLR one
    /// additionally carries the BCL reconciliation name); this predicate erases that shape
    /// split so a consumer asks "is this an interface?" in one place.
    let isInterfaceShape (shape: ExternalTypeShape) : bool =
        match shape with
        | ExternalTypeShape.Class s -> s.IsInterface
        | ExternalTypeShape.IntrinsicInterface _ -> true
        | _ -> false

    /// Resolve an already-RESOLVED intrinsic canon key to its heritable-primitive
    /// surface (`obj`/`exn`: identity + the contract base/`.ctor`s), when the
    /// provider publishes one. A DIRECT qualified lookup — the canon is a resolved
    /// identity, so it must never round-trip through a short-name/ambient re-scan
    /// (a composited provider could resolve the short name to a DIFFERENT entry
    /// than the one that minted the key). `ValueNone` is the honest miss: a scalar
    /// intrinsic, a non-intrinsic key, or a self-host file whose own primitives
    /// publish no provider shape — callers no-op or fall to their ordinary error.
    let tryIntrinsicClass
        (provider: IExternalSymbolStore)
        (canon: SymbolKey)
        : struct (IntrinsicIdentity * IntrinsicClassSurface) voption =
        match provider.TryLookupType canon with
        | ValueSome(ExternalTypeShape.Intrinsic { Id = id; Class = ValueSome surface }) ->
            ValueSome(struct (id, surface))
        | _ -> ValueNone

    /// The **runtime-type** axis of an intrinsic repr — distinct from `canonKey`'s
    /// nominal-identity read. Resolve a bare runtime
    /// repr string (`"Error"`) to the concrete `ExternalTypeShape` it names over the
    /// *assembled* composite: probe the bare name, then each `AmbientOpenPrefixes` entry
    /// (`Error` ⇒ `Vesper.Error`). The repr
    /// is a layer-2 name (`JsNativeSymbols.Error` on JS, `MetadataSymbols` on CLR) only
    /// in scope on the composite, so this resolves lazily there rather than at per-package
    /// extraction. `ValueNone` when the repr is a JS *primitive tag* (`"number"`, `"boolean"`)
    /// that names no class — the caller emits the bare tag — or when no provider models it.
    /// Free function (not a new `IExternalSymbolProvider` member): it derives purely from
    /// the existing `TryLookupType` / `AmbientOpenPrefixes` window, so it adds no interface
    /// churn while keeping the provider the one seam to the outside.
    ///
    /// **A `(# "…" #)` REPR STRING ONLY — never a name written in source.** A repr is minted
    /// by the compiler and is not opens-sensitive, so the prelude window is the whole of its
    /// scope and there is no consumer `OpenScope` to consult (the JS/CLR codegen callers hold
    /// no `PassContext`). A name a USER WROTE — an `inherit` base, a type annotation — must
    /// instead go through `NameResolutionTypeHeadStamp.tryPickExternalType`, which resolves
    /// over the file's own `OpenScope`: its explicit `open`s, the implicit open of its
    /// `namespace N` header, then the ambient. Resolving a written name here would see only
    /// the prelude and miss every `open` in the file.
    ///
    /// The chooser-based core: the bare name, then each
    /// `AmbientOpenPrefixes` candidate, returning the first shape `choose` ACCEPTS —
    /// a rejected shape does not stop the scan. That continuation matters when
    /// several providers are composited and an earlier prefix resolves the same
    /// short name to a different KIND of shape (an FSharp.Core-lib `int`
    /// abbreviation shadowing the Vesper `int` intrinsic): a kind-specific consumer
    /// (the intrinsic resolvers) must keep scanning to the shape it wants, not
    /// stop at the first name hit.
    let tryPickRuntimeType
        (provider: IExternalSymbolResolver)
        (choose: ExternalTypeShape -> 'a voption)
        (repr: string)
        : 'a voption =
        match provider.TryLookupType repr |> typeShapeOf |> ValueOption.bind choose with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            provider.AmbientOpenPrefixes
            |> List.tryPick (fun p ->
                match provider.TryLookupType(p + "." + repr) |> typeShapeOf |> ValueOption.bind choose with
                | ValueSome v -> Some v
                | ValueNone -> None
            )
            |> function
                | Some v -> ValueSome v
                | None -> ValueNone

    let tryRuntimeType (provider: IExternalSymbolResolver) (repr: string) : ExternalTypeShape voption =
        tryPickRuntimeType provider ValueSome repr

    /// Resolve the language-capability identities THROUGH THE PROVIDER, from their
    /// canonical language-level Vesper contract names (`Vesper.disposable` etc. —
    /// §5.0/§6: these names are language-level, hardcoding *them* is intended; the
    /// BCL identities they map to are not). A capability the provider does not name
    /// resolves to `ValueNone` — no silent CLR fallback (§5.4); the consumer site
    /// surfaces a resolve-on-use diagnostic or treats it as a non-match.
    ///
    /// All five are dedicated `extern interface` anchors (`capabilities.fsi`) — see
    /// `resolveAnchor` for the one- and two-name cases. The key is minted with arity 0
    /// because the fqn ALREADY carries the metadata backtick-arity suffix (`arityName` is
    /// a no-op on it): the arity is in the key, which is what lets `CapabilityIdentity`
    /// recognise by `=`. `seq`/`enumerator` live in `Vesper.Core` (not
    /// `Vesper.List`) so resolving them isn't circular when building `Vesper.List`, whose
    /// `List` union implements `seq`. `for-in` resolution stays structural-primary, so a
    /// `ValueNone` Enumerable/Enumerator here is harmless (§5.1).
    let resolveCapabilities (provider: IExternalSymbolProvider) : RuntimeNames.CapabilityIds =
        let ofKey (key: TypeKey) : RuntimeNames.CapabilityIdentity =
            {
                RuntimeNames.CapabilityIdentity.Key = key
                RuntimeNames.CapabilityIdentity.CanonKey = ValueNone
            }

        // Mint a capability's identity from its anchor. On CLR a capability is an
        // `IntrinsicInterface` carrying both names: `Key` is the platform/BCL name
        // (`System.IDisposable`) a metadata or BCL-spelled impl freezes to, `CanonKey` the
        // canonical one `interface disposable` freezes to; both spellings then dispatch.
        // On JS it is a plain `Class` with only the canonical name, and the BCL name is added
        // iff the caller supplies it AND the `capabilities-compat.js.fsi` shim confirms that
        // spelling abbreviates to this canonical. That gives JS the same two names CLR gets,
        // so a TS pack spelling `IEnumerable`1` still reconciles to `seq`.
        //
        // The provider exposes no reverse-abbreviation index, so canon→BCL cannot be derived:
        // `bclName` is supplied and VERIFIED, never trusted. The `Intrinsic` arm covers a
        // build whose anchor is still a bare `extern`.
        let shimConfirms (bcl: string) (lookup: string) : bool =
            match provider.TryLookupType bcl |> typeShapeOf with
            | ValueSome(ExternalTypeShape.Abbrev(_, FTClass(head, _))) -> SymbolKeyOps.typeMetaName head = lookup
            | _ -> false

        let resolveAnchor (lookup: string) (bclName: string voption) : RuntimeNames.CapabilityIdentity voption =
            match provider.TryLookupType lookup |> typeShapeOf with
            | ValueSome(ExternalTypeShape.Intrinsic { Id = { Platform = Some fqn } }) ->
                ValueSome(ofKey (SymbolKeyOps.qualifiedTypeKeyOf fqn 0))
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                ValueSome
                    {
                        RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf platform 0
                        RuntimeNames.CapabilityIdentity.CanonKey = ValueSome(SymbolKeyOps.qualifiedTypeKeyOf lookup 0)
                    }
            | ValueSome(ExternalTypeShape.Class _) ->
                let canonKey = SymbolKeyOps.qualifiedTypeKeyOf lookup 0

                match bclName with
                | ValueSome bcl when shimConfirms bcl lookup ->
                    // JS: pair this canon-only anchor with its shim-confirmed BCL name, mirroring
                    // the CLR polarity (`Key` = platform, `CanonKey` = canonical), so
                    // `capabilityCanonKey` folds either spelling → canon.
                    ValueSome
                        {
                            RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf bcl 0
                            RuntimeNames.CapabilityIdentity.CanonKey = ValueSome canonKey
                        }
                | _ -> ValueSome(ofKey canonKey)
            | _ -> ValueNone

        {
            // The iteration capabilities carry a BCL reconciliation name so a TS pack's
            // BCL-spelled `IEnumerable`1`/`IEnumerator`1` reconciles to `seq`/`enumerator` on JS
            // (shim-verified; a no-op on CLR, where both names come from the `.fs` repr).
            Enumerable = resolveAnchor "Vesper.Collections.seq`1" (ValueSome "System.Collections.Generic.IEnumerable`1")
            Enumerator =
                resolveAnchor "Vesper.Collections.enumerator`1" (ValueSome "System.Collections.Generic.IEnumerator`1")
            // The leaf capabilities' JS `use`/eq/comp paths fold BCL spellings to the canonical
            // at freeze time via the shim, so they need no reconciliation name here.
            Disposable = resolveAnchor "Vesper.disposable" ValueNone
            Equatable = resolveAnchor "Vesper.equatable`1" ValueNone
            Comparable = resolveAnchor "Vesper.comparable`1" ValueNone
        }

    /// Realise a member's `Signature` with SOME method typars PRE-BOUND to a concrete
    /// type (`seed`, index → type) instead of a fresh var — the rest freshen normally
    /// (shared `cache`). The call-site literal-grounding rule (R4a step 3) seeds a method
    /// typar solved from a syntactic constant that appears ONLY inside a non-bare param
    /// position (mitt's no-payload `emit(type: undefined extends Events[Key] ? Key :
    /// never)` — `Key` is never a bare parameter, so plain unification cannot solve it, but
    /// the constant `"tick"` grounds it here, letting the conditional fold). A bare-position
    /// typar is left unseeded (unification already solves it). Realises the member's stored
    /// template VERBATIM — the covariant `number → float` identity is a JS-provider fact
    /// already baked into the member's `FrozenType` before it reaches here
    /// (`Codegen.Js.NumberCovariance`), so this front-end realiser is number-agnostic.
    let instantiateSignatureWith
        (store: TypeStore)
        (seed: (int * SemType) list)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        (level: int)
        : SemType =
        let cache = System.Collections.Generic.Dictionary<int, SemType>()

        for (j, ty) in seed do
            cache.[j] <- ty

        let methodVar = methodFreshener store cache level
        let decl i = declaringArgs.[i]
        let noLocal = localTyparInTemplate "ExternalSymbols.instantiateSignatureWith"
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodVar noLocal s.Return
        else
            TyFun(instantiateWith decl methodVar noLocal s.Parameters, instantiateWith decl methodVar noLocal s.Return)

    /// Realise a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per
    /// index, shared across `Parameters` and `Return`). Reconstructs
    /// `BuildSignature`'s `TyFun(params, ret)` for a method / ctor, or the bare
    /// value type for a property. The data-form replacement for
    /// `member.BuildSignature args` followed by `Infer.instantiateMethodTypars`;
    /// equal to it on the post-freeze subset.
    let instantiateSignature (store: TypeStore) (m: ExternalMember) (declaringArgs: SemType[]) (level: int) : SemType =
        instantiateSignatureWith store [] m declaringArgs level

    /// The *open* realisation of a member's `Signature`: declaring typars
    /// substituted from `declaringArgs`, but the member's own method typars left
    /// as `TyTypar(Method,j)` markers — exactly the shape `BuildSignature`
    /// produced. This is the applicability-
    /// filtering / single-pick form; a generic method's `TyTypar(Method,_)`
    /// stays a wildcard for `InferOverload.matchTypes`, and the bind site that
    /// commits the member freshens them separately (`instantiateSignature`, or
    /// `Infer.instantiateMethodTypars`). For a non-generic member (the common
    /// case) it is byte-identical to `instantiateSignature` at any level. Realises the
    /// stored template VERBATIM — the covariant `number → float` identity is a JS-provider
    /// fact baked into the member's `FrozenType` upstream (`Codegen.Js.NumberCovariance`),
    /// so this realiser is number-agnostic.
    let openSignature (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.openSignature"
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodOpen noLocal s.Return
        else
            TyFun(
                instantiateWith decl methodOpen noLocal s.Parameters,
                instantiateWith decl methodOpen noLocal s.Return
            )

    /// Realise a member's method-typar BOUNDS at a use site — one `SemType voption` per
    /// method-typar index. `FTTypar(Declaring,i)` inside a bound → `declaringArgs.[i]`;
    /// a `FTTypar(Method,j)` ref stays a `TyTypar(Method,j)` marker (a bound over another
    /// method typar is inert until it grounds — mitt's bounds only name declaring typars,
    /// `keyof Events`). Empty for a member with no carried bounds (every non-TS producer).
    /// The call-site literal-grounding rule (R4a step 3 item 2) keyof-folds these to gate
    /// which method typar a syntactic string constant may solve to a `TyLiteral`.
    let instantiateSignatureBounds (m: ExternalMember) (declaringArgs: SemType[]) : SemType voption[] =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.instantiateSignatureBounds"

        m.Signature.MethodTyparBounds
        |> Array.map (ValueOption.map (fun ft -> instantiateWith decl methodOpen noLocal ft))

    /// Realise a record field's type at a use site (`FTTypar(Declaring,i) →
    /// declaringArgs.[i]`). The data-form replacement for `field.BuildType args`.
    let instantiateFieldType (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring f.Frozen declaringArgs

    /// Realise a union case's field types at a use site. The data-form
    /// replacement for `case.BuildFieldTypes |> Array.map (fun b -> b args)`.
    let instantiateCaseFieldTypes (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        c.FrozenFieldTypes
        |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs)

    /// Realise a directly-implemented interface set (`(compiled-name, frozen
    /// type-args)` pairs, the `FrozenInterfaces` of a class or the
    /// `ExternalTypeShape.Union.interfaces` of a union) at a use site as
    /// `(compiled-name, type-args)` pairs.
    let instantiateInterfacesOf
        (interfaces: (string * FrozenType[])[])
        (declaringArgs: SemType[])
        : (string * SemType[])[] =
        interfaces
        |> Array.map (fun (name, fts) -> name, fts |> Array.map (fun ft -> instantiateDeclaring ft declaringArgs))

    /// Realise a class/interface's directly-implemented interfaces. The data-form
    /// replacement for `shape.Interfaces args`.
    let instantiateInterfaces (shape: ExternalClassShape) (declaringArgs: SemType[]) : (string * SemType[])[] =
        instantiateInterfacesOf shape.FrozenInterfaces declaringArgs

    /// Instantiate a declared base `FrozenType` template over the receiver's args —
    /// the shape-agnostic core shared by `ExternalClassShape` (`instantiateBaseType`)
    /// and `IntrinsicClassSurface.BaseType` (the same template form).
    let instantiateBaseTypeFrozen (baseType: FrozenType voption) (declaringArgs: SemType[]) : SemType voption =
        baseType |> ValueOption.map (fun ft -> instantiateDeclaring ft declaringArgs)

    /// Realise a class's declared base type, if any. The data-form replacement
    /// for `shape.BaseType |> ValueOption.map (fun b -> b args)`.
    let instantiateBaseType (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType voption =
        instantiateBaseTypeFrozen shape.FrozenBaseType declaringArgs

    /// The per-parameter `argSig` of a frozen method signature, flattening the
    /// `.NET`-tupled parameter form: a `unit` parameter is zero arguments, a tuple
    /// is one entry per element, anything else is a single argument. Mirrors the
    /// `argCount` decode in codegen's `ExternalMember` arm. Structural — one
    /// `FrozenType` per value parameter, the total overload identity a `MemberKey`
    /// interns (never a rendered string).
    let argSigOfParameters (parameters: FrozenType) : EqArray<FrozenType> =
        match parameters with
        | FTUnit -> EqArray.empty
        | FTTuple items -> items
        | single -> EqArray.singleton single

    /// The unit type as a frozen template head — the `.NET`-tupled `Parameters` form a
    /// member with no value parameters folds to.
    let unitFrozen: FrozenType = FTConst(RuntimeNames.unitKey, EqArray.empty)

    /// Fold a method / ctor's per-parameter frozen types into the single `.NET`-tupled
    /// `Parameters` form an `ExternalSignature` carries: none ⇒ `unit`, one ⇒ itself,
    /// several ⇒ an `FTTuple`. The forward direction of `argSigOfParameters` (which
    /// un-tuples it back to one `FrozenType` per value parameter) — the ONE home every
    /// signature producer (contract extractor, metadata reader, frozen-impl projection)
    /// folds through, so the fold cannot drift between them.
    let tupledParams (ps: FrozenType[]) : FrozenType =
        match ps.Length with
        | 0 -> unitFrozen
        | 1 -> ps.[0]
        | _ -> FTTuple(EqArray.ofArray ps)

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

    /// Realise a value/free-function symbol's `Scheme` at `level`: a fresh `TyVar`
    /// (stamped at `level`) per declaring typar, the `Constraints` stamped onto them
    /// in fixed groups (trait → default → SRTP → coercion), then the scheme realised
    /// against that fresh array via `instantiateDeclaring`. A monomorphic scheme
    /// (`typarCount = 0`) realises directly. The single `FrozenType → SemType`
    /// realiser for the value/`TryLookup` channel (relocated from
    /// `VesperLib.makeInstantiate`); the value-channel twin of `instantiateSignature`.
    let instantiateSymbol (store: TypeStore) (sym: ExternalSymbol) (level: int) : SemType =
        let inst ft fresh =
            FrozenTypeBridge.instantiateDeclaring ft fresh

        let scheme = sym.Scheme
        let constraints = sym.Constraints

        if sym.TyparArity = 0 then
            inst scheme [||]
        else
            let freshTvs =
                Array.init
                    sym.TyparArity
                    (fun _ ->
                        let tv = store.NewTypeVar()
                        store.SetLevel(UnionFind.find store tv, level)
                        tv
                    )

            let fresh = freshTvs |> Array.map TyVar

            // External symbols carry no source-side NodeKey; stamp `Unknown`
            // so diagnostics attribute the constraint to the use site.
            for c in constraints do
                match c with
                | ExternalConstraint.Trait(i, kind) ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    store.Constraints.Prepend(UnionFind.find store freshTvs.[i], cstr)
                | _ -> ()

            // Defaults accumulate newest-last so source order is preserved when
            // generalisation later walks the list for the first concrete shape.
            for c in constraints do
                match c with
                | ExternalConstraint.Default(i, target) ->
                    store.Defaults.Append(UnionFind.find store freshTvs.[i], inst target fresh)
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.MemberTrait(idxs, mName, argFts, retFt) ->
                    let sig_: MemberSignature =
                        {
                            MemberName = mName
                            ArgTypes = EqArray.ofSeq (seq { for ft in argFts -> inst ft fresh })
                            ReturnType = inst retFt fresh
                        }

                    for i in idxs do
                        store.Srtp.Prepend(UnionFind.find store freshTvs.[i], sig_)
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.Coercion(i, target) ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = SemanticConstraintKind.Coercion(inst target fresh)
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    store.Constraints.Prepend(UnionFind.find store freshTvs.[i], cstr)
                | _ -> ()

            inst scheme fresh

    /// The registration `Name` of a value symbol whose identity is `key`: the
    /// module-qualified compiled name (`Vesper.Collections.ListModule.fold`), bare for an
    /// unqualified binding. Deriving it from the key — rather than taking a dotted string
    /// and cutting the key back out of it — is what keeps a producer's `(namespace,
    /// module chain, name)` intact: the name is a RENDERING of the identity, not its source.
    let private valueSymbolName (key: BindingKey) : string =
        SymbolKeyOps.qualifiedName (SymbolKey.Binding key)

    /// The KEYED zero the two symbol builders below copy from: identity is an ARGUMENT, so
    /// no `ExternalSymbol` can exist without a real binding key, and `Name` is DERIVED from
    /// it (`valueSymbolName`) rather than written beside it — the two cannot disagree.
    /// Everything a producer has no opinion on (the inline body, the source `ValRepr`, the
    /// export `ImportForm`, the origin a stacking wrapper stamps, and the `Scheme` its
    /// caller always fills) defaults here, so a future field is a one-site addition rather
    /// than an edit at both builders. A module-level `let`, not a static member on the
    /// type: `valueSymbolName` lives here.
    let private ofBindingKey (key: BindingKey) : ExternalSymbol =
        {
            Name = valueSymbolName key
            Scheme = deferredTemplate
            TyparArity = 0
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = key
            ValRepr = ValueNone
            ImportForm = ImportForm.Named
            InlineBody = ValueNone
        }

    /// A monomorphic value/free-function symbol from a closed `FrozenType` scheme
    /// (no typars). `decl` is the declaring holder — a module chain, or the namespace
    /// itself for an unqualified binding (`SymbolKeyOps.inNamespace ""` for a
    /// flat-package extern such as `printfn`).
    let monoFrozen (decl: ModuleHolder) (name: string) (scheme: FrozenType) : ExternalSymbol =
        { ofBindingKey (SymbolKeyOps.bindingKeyOf decl name) with
            Scheme = scheme
        }

    /// A value/free-function symbol from a `FrozenType` scheme over `arity` declaring
    /// typars, plus optional constraints. A polymorphic symbol is a template with
    /// typars, freshened per use site by `instantiateSymbol` — not a closure.
    let scheme
        (decl: ModuleHolder)
        (name: string)
        (frozen: FrozenType)
        (arity: int)
        (constraints: ExternalConstraint list)
        : ExternalSymbol =
        { ofBindingKey (SymbolKeyOps.bindingKeyOf decl name) with
            Scheme = frozen
            TyparArity = arity
            Constraints = constraints
        }
