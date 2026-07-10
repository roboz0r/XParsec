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
        /// applies them to the fresh TyVars it mints; callers don't drain the list
        /// separately. Also surfaced for diagnostic introspection.
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
        /// The SOURCE arity (`ValRepr`) of a module-level FUNCTION, carried across the
        /// assembly boundary so a caller reconciles its application spine against the
        /// producer's
        /// curried / tupled grouping. The flat, lone-unit-erased, `void`-normalised
        /// `CompiledForm` is derived from it on demand (`TastLower.compiledOf`), never
        /// stored — it is fully determined by the `ValRepr`. `ValueNone` for
        /// everything that is not a contract-extracted function (module values,
        /// operators, the `monoFrozen`/`scheme` builders, metadata-layer symbols) — those
        /// keep the curried-`Scheme` reconstruction at the codegen boundary.
        ValRepr: Frozen.ValRepr voption
        /// How the symbol's home module exports it (see `ImportForm`). Stamped
        /// `Default` by the TS-manifest provider for a TS `export default`
        /// (mitt's factory); `Named` for every other producer — the
        /// `monoFrozen`/`scheme` builders default it, so non-TS layers
        /// (VesperLib, MetadataSymbols, JsNativeSymbols) never touch it.
        ImportForm: ImportForm
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

/// The immutable, two-axis member descriptor: a
/// member's tupled `(Parameters, Return)` as `FrozenType` templates. `Parameters`
/// is the .NET-tupled argument type (`N ≥ 2` → one `FTTuple`; 0 params →
/// `unit`); `Return` the result. Open typars are baked as `FTTypar(Declaring,i)`
/// (the declaring type's typars) / `FTTypar(Method,j)` (the method's own) — the
/// `DeclaringArity` / `MethodArity` counts give each axis's width. For a
/// value member (`ExternalMember.IsValueMember` — a field or property) there are no
/// parameters: `Parameters` is `unit` and the value type lives in `Return`; consumers
/// gate reconstruction on `IsValueMember` (see `ExternalSymbols.instantiateSignature`).
/// The two-axis
/// data form is what unblocks generic external static methods (`truncate`).
type ExternalSignature =
    {
        DeclaringArity: int
        MethodArity: int
        Parameters: FrozenType
        Return: FrozenType
        /// Per-method-typar UPPER BOUND (`<Key extends keyof Events>`), aligned to the
        /// method axis: index `j` is the `j`-th method typar's constraint as a CARRIED
        /// `FrozenType` (`keyof(FTTypar(Declaring,0))`), `ValueNone` when unconstrained.
        /// Baked over the DECLARING typars just like `Parameters`/`Return`, so
        /// `ExternalSymbols.instantiateSignatureBounds` realises it against the use-site
        /// declaring args (`keyof Events` at `Emitter<R>` → `TyKeyOf R`). Length is
        /// `MethodArity` when any bound is present, else EMPTY (the churn-free default
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
        (declaringArity: int, methodArity: int, parameters: FrozenType, return': FrozenType)
        : ExternalSignature =
        {
            DeclaringArity = declaringArity
            MethodArity = methodArity
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
        /// (arrow member). `Field` vs `Property` matters only at CLR emission
        /// (`ldfld` vs `call get_X`); consumers that only need value-vs-arrow read
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
        /// True for an OPTIONAL interface member (`verbose?: T`) — a structural-width
        /// admission at a foreign-call arg position treats it as not-required. Every
        /// non-interface producer (metadata, .fsi contract, JS-native, ctors) sets `false`.
        IsOptional: bool
    }

    /// A value member (field or property) — no parameters, the value in `Return` —
    /// as opposed to an arrow `Method`. The single predicate the inference/freeze
    /// consumers gate on; only CLR emission cares about `Field` vs `Property`.
    member m.IsValueMember = m.Storage.IsValueMember

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
            Storage = MemberStorage.Method
            Signature = signature
            MethodArity = 0
            Origin = origin
            Key = SymbolKey.MemberKey(declKey, ".ctor", argSig, MemberKind.Method)
            OptionalDefaults = optionalDefaults
            IsOptional = false
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
        Arity: int
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
            Arity = arity
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
/// carries a non-optional `string` face rather than this record's `option`.)
///
/// **Two faces** (a single repr string used to do two unrelated jobs at once):
/// - `Canon` — the platform-INVARIANT nominal-identity key: the qualified
///   **`.fsi` name** the type was declared under (`Vesper.int`, `Vesper.exn`),
///   i.e. the front-end identity itself, NOT a BCL name. `subsumes`' `canonKey`
///   uses THIS face; it is distinct per nominal type so `int` ≠ `float`, and it
///   is the SAME regardless of which backend is compiling — a JS build never
///   needs to know what the BCL calls `int`.
/// - `Platform` — the per-target runtime/codegen repr: the platform's *name*
///   for the type, sourced from the `<base>.<target>.fs` companion's
///   `type x = (# "<repr>" #)` (`Some "System.Int32"` on CLR, `Some "number"`/
///   `Some "Error"` on JS). Codegen emission + the `exnReprOf`/`tryRuntimeType`
///   runtime axis, and the intrinsic-receiver member probe
///   (`tryExternalReceiver`), read THIS face. Many-to-one and directional — it
///   must never drive unification. **`None` on a NULLARY intrinsic means the
///   type is a known scalar primitive but has NO representation on the
///   compiling target** — e.g. `decimal`/`nativeint` on JS, which ship no
///   `.js.fs` companion; `SemanticAnalysis.PlatformTypes` rejects that up front.
///   `None` on a GENERIC intrinsic (`'T []` on JS) is benign — the structural
///   backend path needs no repr string. On CLR every primitive's base `.fs` IS
///   its platform repr, so `Platform` is always `Some` there.
///
/// `Arity` — the type's generic parameter count. Usually `0` (the scalar
/// primitives), but NOT always: the structural type constructors are intrinsics
/// too (`type 'T [] = (# "!0[]" #)`, arity 1; `byref`, nd-array). Load-bearing
/// for representability: only an `arity = 0` intrinsic with `Platform = None`
/// is unrepresentable (see `Platform` above).
type IntrinsicIdentity =
    {
        Canon: SymbolKey
        Arity: int
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
/// unit `inherit exn` / `new exn` through the ordinary provider paths while the
/// value identity stays `TyConst` (no `TyClass` churn at the pervasive
/// `obj`/`exn` value sites). Distinct from a capability `IntrinsicInterface`
/// (`disposable`) — an INTERFACE, which resolves to `TyClass`.
type IntrinsicShape =
    {
        Id: IntrinsicIdentity
        Class: IntrinsicClassSurface voption
    }

    /// A scalar (non-heritable) intrinsic — the common mint.
    static member Scalar(canon: SymbolKey, arity: int, platform: string option) : IntrinsicShape =
        {
            Id =
                {
                    Canon = canon
                    Arity = arity
                    Platform = platform
                }
            Class = ValueNone
        }

/// A **capability interface** (`disposable`/`equatable`/`comparable`) — an intrinsic
/// whose identity axis is a `TyClass` CONSTRAINT rather than a `TyConst` value
/// identity, so it earns its own case beside `Intrinsic` (an interface is excluded
/// from the forward-repr harvest and the unrepresentability gate, and it resolves to
/// `TyClass` not `TyConst`). Minted ONLY on a target whose `.fs` binds the platform
/// repr (CLR); on JS a capability surfaces as a plain single-faced interface `Class`.
///
/// Carries its own identity fields rather than a shared `IntrinsicIdentity`: unlike a
/// scalar `Intrinsic` (whose `Platform` is an `option` — `None` = unrepresentable, the
/// gate's reject signal), a capability interface is minted ONLY when its `.fs` binds
/// the repr, so its `Platform` face is always present — a non-optional `string` that
/// keeps the `Some`-unwraps at `resolveAnchor` / `externalClassRef` / `ClrExternalMembers`
/// total and removes the two-polarity hazard of sharing the scalar's optional field.
///
/// - `Canon` — the platform-INVARIANT `.fsi` short-name identity (`Vesper.disposable`),
///   asm-blind: the reconciliation / capability-matching face (`resolveAnchor`'s
///   `CanonKey`) and the pre-split namespace `stampType` homes the `Origin` on. NOT the
///   value-resolution key.
/// - `Platform` — the `.fs` `(# … #)` BCL repr (`"System.IDisposable"`), driving CLR
///   reconciliation + the `ClrEnv` InterfaceImpl redirect.
/// - `Arity` — the type's generic parameter count (`equatable<'T>` = 1).
/// - `Members` — the abstract member surface (`Dispose`), read by
///   `Unification.checkInterfaceConformance`. Populated at finalize (after the
///   deferred member loop) via the `PendingCapabilityInterfaces` republish.
/// - `Origin` — the manifest home (assembly + namespace), stamped by
///   `ExternalSymbols.stack`'s `stampType` exactly as a `Class`'s is. The VALUE
///   resolution key uses THIS (`externalTypeKey Origin`, asm-qualified), keeping the
///   `TyClass` identity byte-identical to the pre-`IntrinsicInterface` faced `Class`;
///   `Canon` (asm-blind) is the reconciliation/capability-matching face only.
type IntrinsicInterfaceShape =
    {
        Canon: SymbolKey
        Arity: int
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
    /// canon/platform two-faces story).
    ///
    /// The two faces **diverge on every target** (CLR: `int`/`Some "System.Int32"`):
    /// `Canon` is `.fsi`-defined, `Platform` is `.fs`-defined. An incoming
    /// BCL/native runtime name on the metadata seam (e.g. `System.Exception`
    /// surfaced by a metadata `inherit` chain) is reconciled back to its `Canon`
    /// through the reverse `{ platform -> canon }` map
    /// (`IExternalSymbolProvider.IntrinsicReverseCanon`), so `int`-as-metadata and
    /// `int`-as-contract still meet at `"int"`. The *local* `IntrinsicReprTypes`
    /// twin (SideTables.fs) stays single-string: it holds a self-compiled unit's
    /// own `platform` repr keyed by the `.fsi` short name (which is the canon).
    | Intrinsic of shape: IntrinsicShape
    /// A capability interface (`disposable`/`equatable`/`comparable`): an intrinsic on
    /// the `TyClass`-constraint axis. A DISTINCT case from `Intrinsic` because an
    /// interface differs on identity (resolves to `TyClass`, not a `TyConst` value),
    /// is excluded from the forward-repr harvest + the unrepresentability gate, and
    /// carries a member surface + `Origin`. See `IntrinsicInterfaceShape`. CLR-only
    /// (a JS capability is a plain single-faced interface `Class`).
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

/// A mechanical metadata / contract ORACLE: members, interfaces, type shapes,
/// intrinsic reprs. By design it carries **no capability predicates** — no
/// `IsDisposable` / `IsEquatable` here, and there must never be. "Is this type
/// disposable?" is a language-semantics judgment the passes derive from the raw
/// facts (`FrozenInterfaces`, `Members`) against the resolved `CapabilityIds`;
/// folding the verdict in would smuggle a language decision into the metadata layer.
/// The provider's *data* may grow; its *interface* stays a dumb oracle — the absence
/// of those members IS the constraint, do not add them.
///
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

    /// The TS index signature(s) `{ [k: K]: V }` on an external type, by its compiled
    /// name — the seam `x.[k]` / `x.[k] <- v` reads/writes through (each entry a
    /// `(keyTemplate, valueTemplate)` pair of `FrozenType`s over the type's DECLARING
    /// typars, realised at a use site via `FrozenTypeBridge.instantiateDeclaring`
    /// against the receiver's args, exactly as a member `Signature` is). EMPTY = no
    /// index signature. A type may declare BOTH a string- and a number-index entry, so
    /// the list carries all and the consumer (`inferIndexedLookup`) selects by the index
    /// expression's type. Parallel to `TryLookupMember`; providers that model no index
    /// signatures (metadata, .fsi contract, JS-native, test fakes) return `[]`.
    abstract TryLookupIndexSignature: typeName: string -> (FrozenType * FrozenType) list

    /// Reverse case-name lookup: a (bare) union-case name → its declaring
    /// union's compiled name, the union's typar arity, and the case shape. The
    /// mirror of `TryLookupMember` for union construction: it lets a consumer
    /// type `Some 5` / `None` against an external union without a type
    /// annotation, exactly as F# brings a non-`RequireQualifiedAccess` union's
    /// cases into scope when its namespace is opened v1 is first-declaration-wins on a name collision (the same rule
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
    /// prelude (inline test fakes) return `[]`. Required (was the
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

    /// The reverse intrinsic axis `{ platform-repr -> [canon] }`, so the unifier can
    /// reconcile an incoming BCL/native *runtime* name (the `platform` face, e.g.
    /// `"System.Exception"` surfaced by a metadata `inherit` chain on CLR) back to the
    /// short front-end identity (`canon`, the `.fsi` name, e.g. `"exn"`). ONE-TO-MANY:
    /// a single platform repr can be the target of several canons — on JS `int`,
    /// `float`, and `float32` all share repr `"number"`, so `"number" -> ["int";
    /// "float"; "float32"]`; on CLR each platform name maps to exactly one canon, so
    /// the list is a singleton. Consumers that want a single canon take the head/sole
    /// element. The forward `canon` axis lives on `ExternalTypeShape.Intrinsic` and is
    /// reachable by name via `TryLookupType`; the reverse axis cannot be (it is keyed
    /// by the platform repr, which is not a provider type key), so it is published as
    /// data here. The intrinsic-carrying providers (`ExtractCtx.toProvider`) and their
    /// composite (`ExternalSymbols.stack`) build a real map; metadata / JS-native /
    /// test providers carry no intrinsics and return `Map.empty`.
    abstract IntrinsicReverseCanon: Map<string, SymbolKey list>

    /// The FORWARD intrinsic axis `{ canon -> platform-repr }` (the `.fsi` short name
    /// -> its `.fs` `(# … #)` repr for the compiling target) — the mirror of
    /// `IntrinsicReverseCanon`. The CLR *codegen* backend reads it to resolve a
    /// primitive's IL representation from the single `.fs` source; a bare canon like
    /// `"int"` is the open-resolved identity codegen carries (opens are a name-resolution
    /// concern, already discharged), and `TryLookupType` can't serve it because intrinsics
    /// are keyed there by qualified compiled name. On JS the codegen backend resolves reprs
    /// by its own path and never reads this axis, but the map is still POPULATED on the JS
    /// contract stack (harvested from the `.js.fs` `(# … #)` bindings, exactly as on CLR):
    /// the JS front-end seams depend on it — `reprSiblings` (structural-width admission) and
    /// `Codegen.Js.NumberCovariance` (the covariant `number → float` target, asserted to
    /// repr to `number`) both read it. The intrinsic-carrying providers
    /// (`ExtractCtx.toProvider`) and their composite build a real map on EVERY target;
    /// metadata / JS-native / test providers return `Map.empty`.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

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
        /// The SOURCE arity carried across the assembly boundary: how the producer grouped
        /// curried / tupled parameters (`ValRepr.Groups`). The codegen boundary reads
        /// it to flatten / lone-unit-erase the member-ref parameters and split the
        /// call's application spine, and derives the flat `CompiledForm`
        /// (`TastLower.compiledOf`) for the `void`-vs-value decision — replacing the
        /// ambiguous `decurryFrozen` reconstruction of the curried `Signature` (which
        /// can't tell a tupled group `f (x,y)` from a single tuple param
        /// `f (t:int*int)`). `ValueNone` for a symbol with no captured arity (a value,
        /// a metadata-layer symbol); the boundary then keeps the curried
        /// reconstruction. The compiled form is never stored alongside — it is fully
        /// determined by the `ValRepr`.
        ValRepr: Frozen.ValRepr voption
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
    /// The forward intrinsic axis `{ canon -> platform-repr }` (see
    /// `IExternalSymbolProvider.IntrinsicForwardRepr`): codegen resolves a primitive
    /// canon name (`"int"`) to its `.fs`-declared repr (`"System.Int32"`) here — the
    /// single source of a primitive's representation.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

module ExternalSymbols =

    // The generic `SymbolKey` ↔ compiled-name string algebra (`bareName`,
    // `arityName`, `valueKeyOf`, `simpleName`, `qualifiedName`, `externalTypeKey`,
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

    /// Look an external type up by the `SymbolKey` a front-end consumer already holds.
    /// The provider is string-keyed (its metadata / contract leaves own compiled names —
    /// the genuine string boundary), so this is the single key-accepting front door that
    /// projects to `qualifiedName` once, deleting the per-site `TryLookupType (qualifiedName
    /// key)` re-projection at the consumers whose only use of the string was the lookup.
    /// Callers that need the qualified string for an adjacent purpose (a diagnostic,
    /// `TryLookupMember`) keep projecting it directly.
    let tryLookupType (provider: IExternalSymbolProvider) (key: SymbolKey) : ExternalTypeShape voption =
        provider.TryLookupType(SymbolKeyOps.qualifiedName key)

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
    /// additionally carries the BCL reconciliation face); this predicate erases that shape
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
    /// intrinsic, a non-intrinsic key, or a self-host unit whose own primitives
    /// publish no provider shape — callers no-op or fall to their ordinary error.
    let tryIntrinsicClass
        (provider: IExternalSymbolProvider)
        (canon: SymbolKey)
        : struct (IntrinsicIdentity * IntrinsicClassSurface) voption =
        match tryLookupType provider canon with
        | ValueSome(ExternalTypeShape.Intrinsic { Id = id; Class = ValueSome surface }) ->
            ValueSome(struct (id, surface))
        | _ -> ValueNone

    /// The **runtime-type** axis of an intrinsic repr — distinct from `canonKey`'s
    /// nominal-identity read. Resolve a bare runtime
    /// repr string (`"Error"`) to the concrete `ExternalTypeShape` it names over the
    /// *assembled* composite: probe the bare name, then each `AmbientOpenPrefixes` entry
    /// (`Error` ⇒ `Vesper.Error`), exactly how an intrinsic base name freezes. The repr
    /// is a layer-2 name (`JsNativeSymbols.Error` on JS, `MetadataSymbols` on CLR) only
    /// in scope on the composite, so this resolves lazily there rather than at per-package
    /// harvest. `ValueNone` when the repr is a JS *primitive tag* (`"number"`, `"boolean"`)
    /// that names no class — the caller emits the bare tag — or when no provider models it.
    /// Free function (not a new `IExternalSymbolProvider` member): it derives purely from
    /// the existing `TryLookupType` / `AmbientOpenPrefixes` window, so it adds no interface
    /// churn while keeping the provider the one seam to the outside.
    /// The chooser-based core of the ambient probe: the bare name, then each
    /// `AmbientOpenPrefixes` candidate, returning the first shape `choose` ACCEPTS —
    /// a rejected shape does not stop the scan. That continuation matters when
    /// several providers are composited and an earlier prefix resolves the same
    /// short name to a different KIND of shape (an FSharp.Core-lib `int`
    /// abbreviation shadowing the Vesper `int` intrinsic): a kind-specific consumer
    /// (the intrinsic resolvers) must keep scanning to the shape it wants, not
    /// stop at the first name hit.
    let tryPickRuntimeType
        (provider: IExternalSymbolProvider)
        (choose: ExternalTypeShape -> 'a voption)
        (repr: string)
        : 'a voption =
        match provider.TryLookupType repr |> ValueOption.bind choose with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            provider.AmbientOpenPrefixes
            |> List.tryPick (fun p ->
                match provider.TryLookupType(p + "." + repr) |> ValueOption.bind choose with
                | ValueSome v -> Some v
                | ValueNone -> None
            )
            |> function
                | Some v -> ValueSome v
                | None -> ValueNone

    let tryRuntimeType (provider: IExternalSymbolProvider) (repr: string) : ExternalTypeShape voption =
        tryPickRuntimeType provider ValueSome repr

    /// Resolve the language-capability identities THROUGH THE PROVIDER, from their
    /// canonical language-level Vesper contract names (`Vesper.disposable` etc. —
    /// §5.0/§6: these names are language-level, hardcoding *them* is intended; the
    /// BCL identities they map to are not). A capability the provider does not name
    /// resolves to `ValueNone` — no silent CLR fallback (§5.4); the consumer site
    /// surfaces a resolve-on-use diagnostic or treats it as a non-match.
    ///
    /// All five are dedicated `extern interface` anchors (`capabilities.fsi`) — see
    /// `resolveAnchor` for the dual/single-faced cases. The key is minted with arity 0
    /// because the fqn already carries the metadata backtick-arity suffix, which `bareName`
    /// strips for asm-blind recognition. `seq`/`enumerator` live in `Vesper.Core` (not
    /// `Vesper.List`) so resolving them isn't circular when building `Vesper.List`, whose
    /// `List` union implements `seq`. `for-in` resolution stays structural-primary, so a
    /// `ValueNone` Enumerable/Enumerator here is harmless (§5.1).
    let resolveCapabilities (provider: IExternalSymbolProvider) : RuntimeNames.CapabilityIds =
        let ofKey (key: SymbolKey) : RuntimeNames.CapabilityIdentity =
            {
                RuntimeNames.CapabilityIdentity.Key = key
                RuntimeNames.CapabilityIdentity.CanonKey = ValueNone
            }

        // Mint a capability's identity from its anchor. On CLR a capability surfaces as a
        // dual-faced `IntrinsicInterface`: its PLATFORM face (`System.IDisposable`) is `Key`
        // — what a metadata or BCL-spelled impl freezes to — and the canonical face is
        // `CanonKey` — what `interface disposable` freezes to; both spellings then dispatch.
        // On JS a capability surfaces as a single-faced interface `Class` (no `(# … #)` repr).
        // The canonical key is always present; the BCL face is added ON JS iff the caller
        // supplies its BCL spelling AND the `capabilities-compat.js.fsi` shim CONFIRMS that
        // spelling abbreviates to this very canonical (`shimConfirms`). This gives the JS
        // iteration capabilities the same dual face the CLR `IntrinsicInterface` carries, so an
        // external TS pack that spells its interface with the BCL name (`System.Collections
        // .Generic.IEnumerable`1`) reconciles to the canonical `seq` through the already-landed
        // `capabilityCanonKey` fold — exactly as on CLR. The BCL spelling is not invented here:
        // it is the reconciliation constant (the same string the CLR `.fs` repr declares) and is
        // only trusted when the shim's own abbreviation verifies the mapping. The `Intrinsic`
        // arm covers any build whose anchor is still a bare `extern`.
        //
        // NOTE: the provider exposes no reverse-abbreviation index, so the canonical→BCL
        // direction cannot be derived from the shim alone — the BCL face is supplied as
        // `bclFace` and VERIFIED (never blindly trusted) against the shim's forward abbreviation.
        let shimConfirms (bcl: string) (lookup: string) : bool =
            match provider.TryLookupType bcl with
            | ValueSome(ExternalTypeShape.Abbrev(_, FTClass(head, _))) -> SymbolKeyOps.qualifiedName head = lookup
            | _ -> false

        let resolveAnchor (lookup: string) (bclFace: string voption) : RuntimeNames.CapabilityIdentity voption =
            match provider.TryLookupType lookup with
            | ValueSome(ExternalTypeShape.Intrinsic { Id = { Platform = Some fqn } }) ->
                ValueSome(ofKey (SymbolKeyOps.qualifiedTypeKeyOf None fqn 0))
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                ValueSome
                    {
                        RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf None platform 0
                        RuntimeNames.CapabilityIdentity.CanonKey =
                            ValueSome(SymbolKeyOps.qualifiedTypeKeyOf None lookup 0)
                    }
            | ValueSome(ExternalTypeShape.Class _) ->
                let canonKey = SymbolKeyOps.qualifiedTypeKeyOf None lookup 0

                match bclFace with
                | ValueSome bcl when shimConfirms bcl lookup ->
                    // JS: dual-face this single-faced anchor with its shim-confirmed BCL face,
                    // mirroring the CLR `IntrinsicInterface` polarity (`Key` = BCL platform face,
                    // `CanonKey` = canonical). `capabilityCanonKey` then folds either spelling → canon.
                    ValueSome
                        {
                            RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf None bcl 0
                            RuntimeNames.CapabilityIdentity.CanonKey = ValueSome canonKey
                        }
                | _ -> ValueSome(ofKey canonKey)
            | _ -> ValueNone

        {
            // The iteration capabilities carry their BCL reconciliation face so an external TS
            // pack's BCL-spelled `IEnumerable`1`/`IEnumerator`1` reconciles to `seq`/`enumerator`
            // on JS (shim-verified; a no-op on CLR, where the dual face comes from the `.fs` repr).
            Enumerable = resolveAnchor "Vesper.Collections.seq`1" (ValueSome "System.Collections.Generic.IEnumerable`1")
            Enumerator =
                resolveAnchor "Vesper.Collections.enumerator`1" (ValueSome "System.Collections.Generic.IEnumerator`1")
            // The leaf capabilities' JS `use`/eq/comp paths fold BCL spellings to the canonical at
            // freeze time via the shim, so they need no extra reconciliation face here (single-faced on JS).
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
        (seed: (int * SemType) list)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        (level: int)
        : SemType =
        let cache = System.Collections.Generic.Dictionary<int, SemType>()

        for (j, ty) in seed do
            cache.[j] <- ty

        let methodVar = methodFreshener cache level
        let decl i = declaringArgs.[i]
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodVar s.Return
        else
            TyFun(instantiateWith decl methodVar s.Parameters, instantiateWith decl methodVar s.Return)

    /// Realise a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per
    /// index, shared across `Parameters` and `Return`). Reconstructs
    /// `BuildSignature`'s `TyFun(params, ret)` for a method / ctor, or the bare
    /// value type for a property. The data-form replacement for
    /// `member.BuildSignature args` followed by `Infer.instantiateMethodTypars`;
    /// equal to it on the post-freeze subset.
    let instantiateSignature (m: ExternalMember) (declaringArgs: SemType[]) (level: int) : SemType =
        instantiateSignatureWith [] m declaringArgs level

    /// The *open* realisation of a member's `Signature`: declaring typars
    /// substituted from `declaringArgs`, but the member's own method typars left
    /// as `TyTypar(Method,j)` markers — exactly the shape `BuildSignature`
    /// produced. This is the applicability-
    /// filtering / single-pick form; a generic method's `TyTypar(Method,_)`
    /// stays a wildcard for `InferOverload.applicabilityMatches`, and the bind site that
    /// commits the member freshens them separately (`instantiateSignature`, or
    /// `Infer.instantiateMethodTypars`). For a non-generic member (the common
    /// case) it is byte-identical to `instantiateSignature` at any level. Realises the
    /// stored template VERBATIM — the covariant `number → float` identity is a JS-provider
    /// fact baked into the member's `FrozenType` upstream (`Codegen.Js.NumberCovariance`),
    /// so this realiser is number-agnostic.
    let openSignature (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let s = m.Signature

        if m.IsValueMember then
            instantiateWith decl methodOpen s.Return
        else
            TyFun(instantiateWith decl methodOpen s.Parameters, instantiateWith decl methodOpen s.Return)

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

        m.Signature.MethodTyparBounds
        |> Array.map (ValueOption.map (fun ft -> instantiateWith decl methodOpen ft))

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

    // --- The overload-identity `argSig` spelling grammar (ONE renderer) ----
    //
    // A `SymbolKey.MemberKey.argSig` entry only DISAMBIGUATES same-name overloads
    // (the same role `MetadataSymbols.openTyparSig` fills on the metadata layer), so
    // an exotic shape rendered by name is harmless — but the spelling must be the
    // SAME wherever a `FrozenType`-shaped member is keyed, or the front end's
    // resolved key misses codegen's re-derived one. Both `FrozenType`-consuming
    // producers (the `.fsi` contract extractor `VesperLib` and the TS-manifest
    // provider) render through here; total over `FrozenType`.

    /// A single parameter type's `argSig` spelling. Never reparsed — identity only.
    let rec argTypeName (t: FrozenType) : string =
        match t with
        | FTConst(key, args) ->
            let n = SymbolKeyOps.intrinsicName key

            if args.IsEmpty then
                n
            else
                n
                + "<"
                + (args |> EqArray.toList |> List.map argTypeName |> String.concat ",")
                + ">"
        | FTClass(key, _)
        | FTRecord(key, _)
        | FTUnion(key, _)
        | FTEnum key -> SymbolKeyOps.qualifiedName key
        | FTTuple items ->
            "("
            + (items |> EqArray.toList |> List.map argTypeName |> String.concat "*")
            + ")"
        | FTFun(a, b) -> argTypeName a + "->" + argTypeName b
        | FTOr members ->
            "("
            + (members |> EqSet.toList |> List.map argTypeName |> String.concat "|")
            + ")"
        // A literal renders as its QUOTED constant (`LiteralConst.Render`), keeping
        // overload identity sharp: two overloads differing only by literal value
        // (mitt's `on(type: Key)` vs `on(type: '*')`) must mint DISTINCT `argSig`s,
        // so this must NOT collapse to the base primitive (design §"argSigOf …
        // quoted-value spelling").
        | FTLiteral l -> l.Render
        // The type-level computations render sharply so an overload differing only
        // by one mints a distinct `argSig`.
        | FTKeyOf t -> "keyof(" + argTypeName t + ")"
        | FTIndexedAccess(objTy, index) -> argTypeName objTy + "[" + argTypeName index + "]"
        | FTConditional c ->
            "("
            + argTypeName c.Check
            + " extends "
            + argTypeName c.Extends
            + " ? "
            + argTypeName c.WhenTrue
            + " : "
            + argTypeName c.WhenFalse
            + ")"
        | FTTypar(axis, i) ->
            (match axis with
             | TyparAxis.Declaring -> "!"
             | _ -> "!!")
            + string i
        | FTUnknown n -> n

    /// The per-parameter `argSig` of a frozen method signature, flattening the
    /// `.NET`-tupled parameter form: a `unit` parameter is zero arguments, a tuple
    /// is one entry per element, anything else is a single argument. Mirrors the
    /// `argCount` decode in codegen's `ExternalMember` arm.
    let argSigOfParameters (parameters: FrozenType) : EqArray<string> =
        match parameters with
        | FTUnit -> EqArray.empty
        | FTTuple items -> items |> EqArray.toList |> List.map argTypeName |> EqArray.ofList
        | single -> EqArray.singleton (argTypeName single)

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
    let instantiateSymbol (sym: ExternalSymbol) (level: int) : SemType =
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
                        let tv = TypeVar()
                        tv.Level <- level
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

                    freshTvs.[i].Constraints <- cstr :: freshTvs.[i].Constraints
                | _ -> ()

            // Defaults accumulate newest-last so source order is preserved when
            // generalisation later walks the list for the first concrete shape.
            for c in constraints do
                match c with
                | ExternalConstraint.Default(i, target) ->
                    let tv = freshTvs.[i]
                    tv.Defaults <- tv.Defaults @ [ inst target fresh ]
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.MemberTrait(idxs, mName, argFts, retFt) ->
                    let sig_: MemberSignature =
                        {
                            MemberName = mName
                            ArgTypes = EqArray.ofSeq (seq { for ft in argFts -> inst ft fresh })
                            ReturnType = inst retFt fresh
                            Resolved = false
                        }

                    for i in idxs do
                        freshTvs.[i].SrtpBounds <- sig_ :: freshTvs.[i].SrtpBounds
                | _ -> ()

            for c in constraints do
                match c with
                | ExternalConstraint.Coercion(i, target) ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = SemanticConstraintKind.Coercion(inst target fresh)
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    freshTvs.[i].Constraints <- cstr :: freshTvs.[i].Constraints
                | _ -> ()

            inst scheme fresh

    /// A monomorphic value/free-function symbol from a closed `FrozenType` scheme
    /// (no typars).
    let monoFrozen (name: string) (scheme: FrozenType) : ExternalSymbol =
        {
            Name = name
            Scheme = scheme
            TyparArity = 0
            Constraints = []
            Origin = SymbolOrigin.Empty
            Key = SymbolKeyOps.valueKeyOf None name
            ValRepr = ValueNone
            ImportForm = ImportForm.Named
        }

    /// A value/free-function symbol from a `FrozenType` scheme over `arity` declaring
    /// typars, plus optional constraints. A polymorphic symbol is a template with
    /// typars, freshened per use site by `instantiateSymbol` — not a closure.
    let scheme
        (name: string)
        (frozen: FrozenType)
        (arity: int)
        (constraints: ExternalConstraint list)
        : ExternalSymbol =
        {
            Name = name
            Scheme = frozen
            TyparArity = arity
            Constraints = constraints
            Origin = SymbolOrigin.Empty
            Key = SymbolKeyOps.valueKeyOf None name
            ValRepr = ValueNone
            ImportForm = ImportForm.Named
        }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup _ = ValueNone
            member _.TryLookupType _ = ValueNone
            member _.TryLookupMember(_, _) = ValueNone
            member _.TryLookupMembers(_, _) = [||]
            member _.TryLookupIndexSignature _ = []
            member _.TryLookupUnionCase _ = ValueNone
            member _.AmbientOpenPrefixes = []
            member _.TryLookupInlineBody _ = ValueNone
            member _.TryLookupInlineBodyByName _ = ValueNone
            member _.IntrinsicReverseCanon = Map.empty
            member _.IntrinsicForwardRepr = emptyForwardRepr
        }

    /// Merge sources' reverse `{ platform-repr -> [canon] }` maps by UNIONING the canon
    /// lists per platform key (dedup, first-seen order preserved). `Array.rev` folds the
    /// earliest source's entries LAST so its canons lead each list — the same
    /// first-source-wins precedence `mergeForwardRepr` gives the forward axis, here
    /// widened to keep every source's canons rather than shadow to one.
    let mergeReverseCanon (sources: IExternalSymbolProvider seq) : Map<string, SymbolKey list> =
        let arr = Seq.toArray sources

        (Map.empty, Array.rev arr)
        ||> Array.fold (fun acc s ->
            (acc, s.IntrinsicReverseCanon)
            ||> Map.fold (fun m platform canons ->
                match Map.tryFind platform m with
                | Some existing -> Map.add platform (canons @ existing |> List.distinct) m
                | None -> Map.add platform (canons |> List.distinct) m
            )
        )

    /// Merge sources' forward `{ canon -> platform-repr }` maps (first-source-wins).
    /// `SymbolKey` is equatable-but-not-comparable, so the merged axis is a read-only
    /// `Dictionary`, not a `Map`. `Array.rev` folds the earliest source LAST so its
    /// entries overwrite later ones — the same first-source-wins precedence the reverse
    /// axis and the singular lookups use.
    let mergeForwardRepr (sources: IExternalSymbolProvider seq) : IReadOnlyDictionary<SymbolKey, string> =
        let arr = Seq.toArray sources
        let d = Dictionary<SymbolKey, string>()

        for s in Array.rev arr do
            for kv in s.IntrinsicForwardRepr do
                d.[kv.Key] <- kv.Value

        d :> IReadOnlyDictionary<_, _>

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

        // Merge the sources' reverse `{ platform -> canon }` and forward
        // `{ canon -> platform-repr }` intrinsic maps (intrinsic-carrying sources only;
        // the rest contribute the empty map). First-source-wins, matching the singular
        // lookups' shadowing order.
        let reverseCanon = mergeReverseCanon sources
        let forwardRepr = mergeForwardRepr sources

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
        // The namespace a Class/Record/Union/Enum extern's `Origin` should carry, given the
        // package-blanket manifest namespace `o.Namespace` and the type's looked-up compiled
        // `name`. Normally the type lives directly in the manifest namespace and the two agree,
        // but a type may live in a SUB-namespace (JS capabilities: `Vesper.Collections.seq` under
        // manifest `Vesper` — on JS a capability is a single-faced `Class`, not the CLR
        // `IntrinsicInterface`). The extractor records `SymbolOrigin.Empty`, so blanket-stamping
        // `o.Namespace` would leave `externalTypeKey` to split `Vesper.Collections.seq` at the
        // wrong dot (`ns = "Vesper"`, `name = "Collections.seq"`), and the mis-split use-site key
        // no longer matches `resolveCapabilities`' `CanonKey` (`ns = "Vesper.Collections"`) — the
        // same failure the `IntrinsicInterface` arm's `Id.Canon` fix repairs on CLR. Derive the
        // namespace from `name` when it STRICTLY EXTENDS the manifest namespace; otherwise keep
        // the blanket (types directly in the namespace are unchanged — their derived ns equals it).
        let originNsFor (name: string) (o: SymbolOrigin) : string =
            let dot = name.LastIndexOf '.'

            if dot < 0 then
                o.Namespace
            else
                let ns = name.Substring(0, dot)

                if o.Namespace <> "" && ns.StartsWith(o.Namespace + ".") then
                    ns
                else
                    o.Namespace

        let stampType (name: string) (shape: ExternalTypeShape) : ExternalTypeShape =
            match stampOrigin with
            | ValueNone -> shape
            | ValueSome o ->
                // Each shape owns its namespace SOURCE (see `originNsFor` above for why the
                // blanket `o.Namespace` mis-splits a sub-namespace capability): a nominal extern
                // derives it from the compiled `name`; a capability `IntrinsicInterface` takes it
                // from its authoritative, pre-split `Id.Canon`. The home ASSEMBLY is always `o`.
                let withNs ns = { o with Namespace = ns }
                let nominalNs = originNsFor name o

                match shape with
                | ExternalTypeShape.Class info -> ExternalTypeShape.Class { info with Origin = withNs nominalNs }
                | ExternalTypeShape.Record(arity, fields, _) ->
                    ExternalTypeShape.Record(arity, fields, withNs nominalNs)
                | ExternalTypeShape.Union(arity, cases, ifaces, _) ->
                    ExternalTypeShape.Union(arity, cases, ifaces, withNs nominalNs)
                | ExternalTypeShape.Enum(cases, _) -> ExternalTypeShape.Enum(cases, withNs nominalNs)
                // Origin-stamped like a `Class` (its value resolution key is asm-qualified via
                // `Origin`; the extractor left it `Empty`), but its namespace comes from the
                // pre-split `Canon` (`Vesper.Collections` for `seq`; `disposable` et al. already
                // sit directly in `Vesper`). `originNsFor name` is only the fallback if the canon
                // isn't a `TypeKey`.
                | ExternalTypeShape.IntrinsicInterface s ->
                    let canonNs =
                        match s.Canon with
                        | SymbolKey.TypeKey(_, ns, _) -> ns
                        | _ -> nominalNs

                    ExternalTypeShape.IntrinsicInterface { s with Origin = withNs canonNs }
                | ExternalTypeShape.Abbrev _
                // An intrinsic carries no `Origin` (its identity is the canon,
                // asm-blind), so origin stamping leaves it unchanged.
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
                firstHit (fun s -> s.TryLookupType name) |> ValueOption.map (stampType name)

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

            // First source with a non-empty index signature wins (a type's index sig
            // lives in one home, like its members). The `(key, value)` templates are
            // origin-independent, so no re-stamp — a plain first-hit-wins fall-through.
            member _.TryLookupIndexSignature typeName =
                let mutable result = []
                let mutable i = 0

                while List.isEmpty result && i < sources.Length do
                    result <- sources.[i].TryLookupIndexSignature typeName
                    i <- i + 1

                result

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

            member _.IntrinsicReverseCanon = reverseCanon
            member _.IntrinsicForwardRepr = forwardRepr
        }

    /// The composed ambient prelude: each source's `[<AutoOpen>]` / prelude
    /// prefixes, concatenated in source priority order (so a higher-priority
    /// provider's auto-opens shadow a lower one's on a name collision, same
    /// first-hit-wins ordering as lookups). Providers without an implicit
    /// prelude (inline test fakes) return `[]` and contribute
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

    /// Rebuild a provider so every VALUE-FLOW `FrozenType` surface it serves is passed
    /// through `transform` AT that surface's variance — the general, content-agnostic
    /// decorator a variance-sensitive rewrite (the JS `number` resolution being the
    /// first) plugs into. It names NO concrete type; the leaf inside `transform` owns
    /// all policy. `transform` is applied at each surface's ROOT variance; a caller
    /// that must thread the decision through nested positions composes
    /// `FrozenType.mapVariant leaf` (which flips/drops variance down the tree). The
    /// surface → root-variance map is fixed here ONCE so no caller re-enumerates where
    /// the types live or which position they occupy:
    ///
    /// - a symbol `Scheme` and a member `Return` are COVARIANT (a value read / result);
    ///   a member's `Parameters` are CONTRAVARIANT (a curried `Scheme`'s own `FTFun`
    ///   flips give its parameters contravariance under `mapVariant` automatically);
    /// - a RECORD field and a UNION-case field are COVARIANT (a field read);
    /// - an interface / base-type type-ARGUMENT is INVARIANT (a generic slot).
    ///
    /// TOTAL over the value-flow surfaces — the reason it exists: a bespoke per-shape
    /// walk keeps missing one (union-case fields, interface args, the base type). The
    /// non-value-flow TEMPLATE positions are deliberately NOT threaded: an `Abbrev` body
    /// inherits its USE SITE's variance (unknowable here), and `MethodTyparBounds` /
    /// `Constraints` are constraint-solve inputs, not value positions — a shape-level
    /// resolution there would be a guess, so they resolve (if ever) at their own
    /// instantiation seam. `TryLookupType`'s shape match is EXHAUSTIVE, so a new
    /// `ExternalTypeShape` case forces a variance decision here.
    let mapProviderTypes
        (transform: Variance -> FrozenType -> FrozenType)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        let co t = transform Variance.Co t
        let contra t = transform Variance.Contra t
        let inv t = transform Variance.Inv t

        // A member's `Return` is a covariant read; its `Parameters` contravariant.
        let mapMember (m: ExternalMember) : ExternalMember =
            { m with
                Signature =
                    { m.Signature with
                        Parameters = contra m.Signature.Parameters
                        Return = co m.Signature.Return
                    }
            }

        // Interface / base-type type-ARGUMENTS are invariant generic slots.
        let mapInterfaces (ifaces: (string * FrozenType[])[]) =
            ifaces |> Array.map (fun (name, args) -> name, args |> Array.map inv)

        // A union-case field is a covariant value read (shared by `TryLookupType`'s
        // `Union` shape and the reverse `TryLookupUnionCase`).
        let mapCase (c: ExternalCaseShape) : ExternalCaseShape =
            { c with
                FrozenFieldTypes = c.FrozenFieldTypes |> Array.map co
            }

        let mapShape (shape: ExternalTypeShape) : ExternalTypeShape =
            match shape with
            | ExternalTypeShape.Class info ->
                ExternalTypeShape.Class
                    { info with
                        Members = info.Members |> Array.map mapMember
                        FrozenInterfaces = mapInterfaces info.FrozenInterfaces
                        FrozenBaseType = info.FrozenBaseType |> ValueOption.map inv
                    }
            | ExternalTypeShape.Record(arity, fields, origin) ->
                // A record field is a covariant value read.
                ExternalTypeShape.Record(arity, fields |> Array.map (fun f -> { f with Frozen = co f.Frozen }), origin)
            | ExternalTypeShape.Union(arity, cases, ifaces, origin) ->
                ExternalTypeShape.Union(arity, cases |> Array.map mapCase, mapInterfaces ifaces, origin)
            // A heritable primitive's class surface has the same value-flow surface as
            // `Class` (a `.ctor`'s params are contravariant reads, the base a covariant
            // chain) — map it identically; a scalar intrinsic has none to map.
            | ExternalTypeShape.Intrinsic({ Class = ValueSome surface } as s) ->
                ExternalTypeShape.Intrinsic
                    { s with
                        Class =
                            ValueSome
                                { surface with
                                    BaseType = surface.BaseType |> ValueOption.map inv
                                    Members = surface.Members |> Array.map mapMember
                                }
                    }
            // A capability interface's abstract members are a value-flow surface (a param
            // is a contravariant read) — map them exactly as a `Class`'s members.
            | ExternalTypeShape.IntrinsicInterface s ->
                ExternalTypeShape.IntrinsicInterface
                    { s with
                        Members = s.Members |> Array.map mapMember
                    }
            // No value-flow FrozenType surface (Abbrev: no intrinsic variance — see header).
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Enum _
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.Opaque _ -> shape

        { new IExternalSymbolProvider with
            member _.TryLookup name =
                inner.TryLookup name
                |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })

            member _.TryLookupType name =
                inner.TryLookupType name |> ValueOption.map mapShape

            member _.TryLookupMember(typeName, memberName) =
                inner.TryLookupMember(typeName, memberName) |> ValueOption.map mapMember

            member _.TryLookupMembers(typeName, memberName) =
                inner.TryLookupMembers(typeName, memberName) |> Array.map mapMember

            // An index KEY is a contravariant position (the supplied index), the VALUE a
            // covariant read — the same variance split as a member's `Parameters`/`Return`.
            member _.TryLookupIndexSignature typeName =
                inner.TryLookupIndexSignature typeName
                |> List.map (fun (k, v) -> contra k, co v)

            member _.TryLookupUnionCase caseName =
                inner.TryLookupUnionCase caseName
                |> ValueOption.map (fun uc -> { uc with Case = mapCase uc.Case })

            member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
            member _.TryLookupInlineBody key = inner.TryLookupInlineBody key
            member _.TryLookupInlineBodyByName name = inner.TryLookupInlineBodyByName name
            member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
            member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

    /// A general MEMOISING decorator: every lookup channel caches on first hit (MISSES
    /// included — the contract is immutable for a compile, so a `ValueNone` / `[||]` is
    /// as stable as a hit). Content-agnostic — it changes no result, only avoids
    /// recomputing it. Apply ONCE atop a composed stack: the per-source `stack`
    /// fall-through and any `mapProviderTypes` rewrite otherwise re-run on EVERY call,
    /// and a hot symbol is looked up many times across the parallel per-file
    /// `PassContext`s. Thread-safe via `ConcurrentDictionary` (the provider contract
    /// requires concurrent-safe lookups; a factory may run more than once under
    /// contention but the inner lookup is pure, so only one result is ever stored). The
    /// intrinsic axes and ambient prefixes are constant fields — passed through uncached.
    let memoize (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        let symbols = ConcurrentDictionary<string, ExternalSymbol voption>()
        let types = ConcurrentDictionary<string, ExternalTypeShape voption>()

        let members =
            ConcurrentDictionary<struct (string * string), ExternalMember voption>()

        let memberSets = ConcurrentDictionary<struct (string * string), ExternalMember[]>()
        let indexSigs = ConcurrentDictionary<string, (FrozenType * FrozenType) list>()
        let unionCases = ConcurrentDictionary<string, ExternalUnionCase voption>()
        let inlineByKey = ConcurrentDictionary<SymbolKey, InlineBody voption>()
        let inlineByName = ConcurrentDictionary<string, InlineBody voption>()

        { new IExternalSymbolProvider with
            member _.TryLookup name =
                symbols.GetOrAdd(name, (fun n -> inner.TryLookup n))

            member _.TryLookupType name =
                types.GetOrAdd(name, (fun n -> inner.TryLookupType n))

            member _.TryLookupMember(typeName, memberName) =
                members.GetOrAdd(struct (typeName, memberName), (fun (struct (t, m)) -> inner.TryLookupMember(t, m)))

            member _.TryLookupMembers(typeName, memberName) =
                memberSets.GetOrAdd(
                    struct (typeName, memberName),
                    (fun (struct (t, m)) -> inner.TryLookupMembers(t, m))
                )

            member _.TryLookupIndexSignature typeName =
                indexSigs.GetOrAdd(typeName, (fun n -> inner.TryLookupIndexSignature n))

            member _.TryLookupUnionCase caseName =
                unionCases.GetOrAdd(caseName, (fun n -> inner.TryLookupUnionCase n))

            member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes

            member _.TryLookupInlineBody key =
                inlineByKey.GetOrAdd(key, (fun k -> inner.TryLookupInlineBody k))

            member _.TryLookupInlineBodyByName name =
                inlineByName.GetOrAdd(name, (fun n -> inner.TryLookupInlineBodyByName n))

            member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
            member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

/// The primitive `SemType` anchors the type-checker pins literals and built-in
/// constructs to (`Unification` / `Freeze`). Codegen maps each `TyConst` name to
/// its target IL type via `IntrinsicRepr`.
module BuiltinTypes =

    let tyInt: SemType = TyConst(RuntimeNames.intKey, EqArray.empty)
    let tyInt64: SemType = TyConst(RuntimeNames.int64Key, EqArray.empty)
    let tyByte: SemType = TyConst(RuntimeNames.byteKey, EqArray.empty)
    let tySByte: SemType = TyConst(RuntimeNames.primitiveKey "sbyte", EqArray.empty)
    let tyInt16: SemType = TyConst(RuntimeNames.primitiveKey "int16", EqArray.empty)
    let tyUInt16: SemType = TyConst(RuntimeNames.primitiveKey "uint16", EqArray.empty)
    let tyUInt32: SemType = TyConst(RuntimeNames.uint32Key, EqArray.empty)
    let tyUInt64: SemType = TyConst(RuntimeNames.primitiveKey "uint64", EqArray.empty)

    let tyNativeInt: SemType =
        TyConst(RuntimeNames.primitiveKey "nativeint", EqArray.empty)

    let tyUNativeInt: SemType =
        TyConst(RuntimeNames.primitiveKey "unativeint", EqArray.empty)

    let tyFloat: SemType = TyConst(RuntimeNames.floatKey, EqArray.empty)
    let tyFloat32: SemType = TyConst(RuntimeNames.primitiveKey "float32", EqArray.empty)
    let tyBool: SemType = TyConst(RuntimeNames.boolKey, EqArray.empty)
    let tyChar: SemType = TyConst(RuntimeNames.charKey, EqArray.empty)
    let tyDecimal: SemType = TyConst(RuntimeNames.decimalKey, EqArray.empty)
    let tyUnit: SemType = TyConst(RuntimeNames.unitKey, EqArray.empty)
    let tyString: SemType = TyConst(RuntimeNames.stringKey, EqArray.empty)
