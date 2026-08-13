namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// The declaration SHAPE a lookup carries, the resolver / store / codegen contracts that
// serve it, and the queries over it.

/// Type-declaration shape carried by a by-name or by-key type lookup.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    /// `frozen` is the abbreviation body as a template, which a use site expands.
    | Abbrev of arity: int * frozen: FrozenType
    /// Field order matches source.
    | Record of arity: int * fields: EqArray<ExternalFieldShape> * origin: SymbolOrigin
    /// Case order matches source. `interfaces` are the union's directly-declared
    /// `interface <ty>` impls.
    | Union of
        arity: int *
        cases: EqArray<ExternalCaseShape> *
        interfaces: EqArray<FrozenInterface> *
        origin: SymbolOrigin
    /// An external enum: named constant cases in source order. No `arity`, because enums are
    /// never generic; the numeric / string / mixed variant is DERIVED from `cases`, never baked.
    | Enum of cases: EqArray<ExternalEnumCaseShape> * origin: SymbolOrigin
    | Class of shape: ExternalClassShape
    /// A referenced package's intrinsic-repr binding (`type exn = (# class
    /// "System.Exception" #)`): scalar (`int`) or heritable class (`obj`/`exn`).
    | Intrinsic of shape: IntrinsicShape
    /// A capability interface (`disposable`/`equatable`/`comparable`). CLR-only, because a JS
    /// capability is a plain canon-only interface `Class`.
    | IntrinsicInterface of shape: IntrinsicInterfaceShape
    /// NAME + ARITY are registered, the body is not modelled. `reason` says which gap, so a
    /// use site can name it rather than degrade silently.
    | Unmodelled of reason: UnmodelledReason * arity: int

    member this.TyparArity: int =
        match this with
        | Class info -> info.TyparArity
        | Intrinsic s -> s.Id.TyparArity
        | IntrinsicInterface s -> s.TyparArity
        | Enum _ -> 0 // enums are never generic
        | Record(arity = a)
        | Union(arity = a)
        | Abbrev(arity = a)
        | Unmodelled(arity = a) -> a

/// The RESOLVER view of the external-symbol contract: spelling → identity, opens-aware.
/// Downstream of name resolution, passes speak the key-addressed store view instead.
type IExternalSymbolResolver =
    /// `name` is the compiled name ("op_Addition", not "(+)").
    abstract TryLookup: name: string -> ExternalSymbol voption
    /// Look up a `type` by canonical compiled name, returning its REGISTERED identity plus
    /// its body shape from the one hit.
    abstract TryLookupType: name: string -> struct (TypeKey * ExternalTypeShape) voption

    /// Reverse case-name lookup: a bare case name → its declaring union, so `Some 5` /
    /// `None` type without an annotation. First declaration wins on a name collision.
    abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption

    /// A field name → every record declaring a field of that name; unqualified
    /// record-literal / record-pattern resolution intersects these sets to pin the type.
    abstract TryRecordsWithField: fieldName: string -> EqArray<ExternalRecordCandidate>

    /// The AMBIENT `[<AutoOpen>]` open prefixes this provider contributes, probed strictly
    /// BEHIND every explicit `open`; earliest wins: `["Vesper.ArithmeticOperators"; "Vesper"]`.
    abstract AmbientOpenPrefixes: string list

/// The STORE view of the external-symbol contract: identity → payload, once identity is
/// resolved. Addressed by `(namespace, arity-qualified name)` and NOTHING ELSE, so
/// same-named types in two assemblies are indistinguishable here.
type IExternalSymbolStore =
    /// A type declaration's body by resolved key; `ValueNone` for an unknown key or a body
    /// shape the provider doesn't model.
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption

    /// A static/instance member by the declaring type's key and the member name. When
    /// several overloads share a name this collapses to a single best-by-arity pick.
    abstract TryLookupMember: key: SymbolKey * memberName: string -> ExternalMember voption

    /// ALL overloads of a member by name: the candidate set the application-site overload
    /// resolver picks from. Empty from providers that don't model members.
    abstract TryLookupMembers: key: SymbolKey * memberName: string -> EqArray<ExternalMember>

    /// A member by resolved `MemberKey`: the channel a MEMBER splice site reaches an
    /// `InlineBody` through. BY KEY: a by-name lookup collapses an overload set to one pick
    /// and can hand back a different overload's entry.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption

    /// The TS index signature(s) `{ [k: K]: V }`: the seam `x.[k]` / `x.[k] <- v` goes
    /// through, `(key, value)` templates over the DECLARING typars. Both kinds may be present.
    abstract TryLookupIndexSignature: key: SymbolKey -> (FrozenType * FrozenType) list

    /// A value/free-function symbol by resolved key: the channel a VALUE splice site
    /// reaches an `InlineBody` through.
    abstract TryLookupByKey: key: SymbolKey -> ExternalSymbol voption

    /// `{ platform-repr -> [canon] }`: an incoming BCL/native runtime name
    /// (`"System.Exception"`) back to the `.fsi` identity (`"exn"`). ONE-TO-MANY, because on
    /// JS `int`, `float` and `float32` all repr to `"number"`.
    abstract IntrinsicReverseCanon: Map<string, SymbolKey list>

    /// `{ canon -> platform-repr }`: the `.fsi` short name to its `.fs` `(# … #)` repr for
    /// the compiling target. Empty from providers carrying no intrinsics.
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

/// Both views on ONE object: raw facts only, with NO capability predicates ("is this type
/// disposable?" is a language judgment the passes make). Every lookup must be thread-safe.
type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore

/// An external module-level function as the codegen boundary sees it: the curried
/// `param -> … -> return` template with the function's own typars baked as
/// `FTTypar(Method, i)`.
type CodegenOpenSignature =
    {
        Origin: SymbolOrigin
        Signature: FrozenType
        MethodTyparArity: int
        /// The producer's SOURCE parameter grouping: the curried `Signature` alone can't
        /// tell a group `f (x,y)` from a tuple param `f (t:int*int)`.
        ValRepr: TastAccessor.ValRepr voption
        /// The symbol's `when 'a :> <ty>` bounds over the method-typar axis, letting a
        /// phantom slot be recovered from the constrained source's interface witness.
        Constraints: FrozenConstraint list
    }

/// The CODEGEN-facing view: only what emission needs to mint references, never the
/// inference surface. Every channel is key-addressed and NONE returns an overload set.
type ICodegenSymbols =
    abstract TryLookupType: key: SymbolKey -> ExternalTypeShape voption
    /// The exact member the front end resolved, by the `MemberKey` it stamped, with no
    /// overload re-pick.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
    /// Select the `.ctor` a `new` emits: by the exact `MemberKey` the front end recorded
    /// when it has one, telling `ArgumentException(string, string)` from
    /// `(string, Exception)`; else the sole ctor of `arity` params.
    abstract TryLookupCtor: declKey: SymbolKey * chosen: SymbolKey voption * arity: int -> ExternalMember voption
    /// Rebase a capability member onto its true base declarer: `enumerator.MoveNext` is
    /// declared on the non-generic `IEnumerator`, and a member-ref parented on the derived
    /// interface would `MissingMethodException`.
    abstract TryRebaseCapabilityMember: key: SymbolKey -> SymbolKey voption
    /// The open `FrozenType` signature of a module-level function by its value key.
    abstract TryLookupOpenSignature: key: SymbolKey -> CodegenOpenSignature voption
    /// `{ canon -> platform-repr }`: a primitive canon name (`"int"`) to its `.fs`-declared
    /// repr (`"System.Int32"`).
    abstract IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>

module ExternalSymbols =

    /// `SymbolKey` is equatable-but-not-comparable, so the axis is a `Dictionary`, not a `Map`.
    let emptyForwardRepr: IReadOnlyDictionary<SymbolKey, string> =
        Dictionary<SymbolKey, string>() :> IReadOnlyDictionary<_, _>

    /// Invert a NAME-INDEXED leaf's qualified name back to a key. Sound only when the
    /// leaf's type keys are `InNamespace`. Arity comes from the SHAPE, never from the arity
    /// probed for.
    let nameKeyedTypeHit (name: string) (shape: ExternalTypeShape) : struct (TypeKey * ExternalTypeShape) =
        struct (SymbolKeyOps.qualifiedTypeKeyOf name shape.TyparArity, shape)

    let typeShapeOf (hit: struct (TypeKey * ExternalTypeShape) voption) : ExternalTypeShape voption =
        hit |> ValueOption.map (fun (struct (_, shape)) -> shape)

    /// The one entry of a by-NAME overload set whose identity is `key`.
    let memberByKey (key: MemberKey) (candidates: EqArray<ExternalMember>) : ExternalMember voption =
        candidates |> EqArray.tryFind (fun m -> m.Key = key)

    /// The member surface an external nominal publishes: a `Class` or a capability
    /// `IntrinsicInterface`.
    [<return: Struct>]
    let (|ExternalMembers|_|) (shape: ExternalTypeShape) : EqArray<ExternalMember> voption =
        match shape with
        | ExternalTypeShape.Class shape -> ValueSome shape.Members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// The member surface of an external INTERFACE specifically. A non-interface `Class` is
    /// excluded, because a record cannot widen to a concrete class.
    [<return: Struct>]
    let (|ExternalInterfaceMembers|_|) (shape: ExternalTypeShape) : EqArray<ExternalMember> voption =
        match shape with
        | ExternalTypeShape.Class {
                                      IsInterface = true
                                      Members = members
                                  } -> ValueSome members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// A capability is an interface on BOTH targets, and only the carried shape differs.
    let isInterfaceShape (shape: ExternalTypeShape) : bool =
        match shape with
        | ExternalTypeShape.Class s -> s.IsInterface
        | ExternalTypeShape.IntrinsicInterface _ -> true
        | _ -> false

    /// The heritable-primitive surface a shape carries (`obj`/`exn`): what an `inherit` may name.
    let intrinsicClassOf (shape: ExternalTypeShape) : struct (IntrinsicIdentity * IntrinsicClassSurface) voption =
        match shape with
        | ExternalTypeShape.Intrinsic {
                                          Id = id
                                          Class = ValueSome({ Heritable = true } as surface)
                                      } -> ValueSome(struct (id, surface))
        | _ -> ValueNone

    /// An already-RESOLVED intrinsic canon key → its heritable-primitive surface. A DIRECT
    /// lookup: a short-name re-scan could hit a different entry of a composited provider
    /// than the one that minted the key.
    let tryIntrinsicClass
        (provider: IExternalSymbolStore)
        (canon: SymbolKey)
        : struct (IntrinsicIdentity * IntrinsicClassSurface) voption =
        provider.TryLookupType canon |> ValueOption.bind intrinsicClassOf

    /// Resolve a `(# "…" #)` REPR STRING to the first shape `choose` ACCEPTS; a rejected
    /// shape does not stop the scan. Never a source-written name, because this would miss
    /// the `open`s it was written under.
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

    /// Resolve the language-capability identities from their canonical contract names
    /// (`Vesper.disposable`). Keys are minted at arity 0, because the fqn already carries the
    /// backtick-arity suffix.
    let resolveCapabilities (provider: IExternalSymbolProvider) : RuntimeNames.CapabilityIds =
        let ofKey (key: TypeKey) : RuntimeNames.CapabilityIdentity =
            {
                RuntimeNames.CapabilityIdentity.Key = key
                RuntimeNames.CapabilityIdentity.CanonKey = ValueNone
            }

        // On CLR a capability anchor is an `IntrinsicInterface` carrying both names; on JS a
        // plain `Class` with only the canonical, which is the only name JS ever keys by.
        let resolveAnchorKey (canon: TypeKey) : RuntimeNames.CapabilityIdentity voption =
            let lookup = SymbolKeyOps.typeMetaName canon

            match provider.TryLookupType lookup |> typeShapeOf with
            | ValueSome(ExternalTypeShape.Intrinsic {
                                                        Id = {
                                                                 Platform = IntrinsicPlatform.Repr fqn
                                                             }
                                                    }) -> ValueSome(ofKey (SymbolKeyOps.qualifiedTypeKeyOf fqn 0))
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                ValueSome
                    {
                        RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf platform 0
                        RuntimeNames.CapabilityIdentity.CanonKey = ValueSome canon
                    }
            | ValueSome(ExternalTypeShape.Class _) -> ValueSome(ofKey canon)
            | _ -> ValueNone

        {
            Enumerable = resolveAnchorKey RuntimeNames.seqKey
            Enumerator = resolveAnchorKey RuntimeNames.enumeratorKey
            Disposable = resolveAnchorKey RuntimeNames.disposableKey
            Equatable = resolveAnchorKey RuntimeNames.equatableKey
            Comparable = resolveAnchorKey RuntimeNames.comparableKey
        }

    /// Realise a member's `Signature` with SOME method typars PRE-BOUND (`seed`, index →
    /// type) instead of fresh; the rest freshen normally. A typar in no bare parameter
    /// position is unsolvable by unification alone.
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
        instantiateWith decl methodVar noLocal (ExternalSignature.openTemplate m.Signature)

    /// Realise a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per index, shared
    /// across the argument groups and `Return`).
    let instantiateSignature (store: TypeStore) (m: ExternalMember) (declaringArgs: SemType[]) (level: int) : SemType =
        instantiateSignatureWith store [] m declaringArgs level

    /// The OPEN realisation of a member's `Signature`: declaring typars substituted from
    /// `declaringArgs`, the member's own method typars left as `TyTypar(Method,j)` markers.
    /// The applicability-filtering form, in which a generic method's marker stays a wildcard.
    let openSignature (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.openSignature"
        instantiateWith decl methodOpen noLocal (ExternalSignature.openTemplate m.Signature)

    /// A member's method-typar BOUNDS at a use site, one per index.
    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`; a `FTTypar(Method,j)` ref stays an
    /// inert marker. Empty when uncarried.
    let instantiateSignatureBounds (m: ExternalMember) (declaringArgs: SemType[]) : EqArray<SemType voption> =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate "ExternalSymbols.instantiateSignatureBounds"

        m.Signature.MethodTyparBounds
        |> EqArray.map (ValueOption.map (fun ft -> instantiateWith decl methodOpen noLocal ft))

    let instantiateFieldType (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring f.Frozen declaringArgs

    let instantiateCaseFieldTypes (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        let fts = c.FrozenFieldTypes
        Array.init fts.Length (fun i -> instantiateDeclaring fts.[i] declaringArgs)

    /// Realise a class's `FrozenInterfaces`, or a union's declared `interface <ty>` impls,
    /// at a use site.
    let instantiateInterfacesOf (interfaces: EqArray<FrozenInterface>) (declaringArgs: SemType[]) : SemType[] =
        Array.init interfaces.Length (fun i -> instantiateDeclaring interfaces.[i].Frozen declaringArgs)

    let instantiateInterfaces (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType[] =
        instantiateInterfacesOf shape.FrozenInterfaces declaringArgs

    /// Shared by a class shape and a heritable primitive's class surface.
    let instantiateBaseTypeFrozen (baseType: FrozenType voption) (declaringArgs: SemType[]) : SemType voption =
        baseType |> ValueOption.map (fun ft -> instantiateDeclaring ft declaringArgs)

    let instantiateBaseType (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType voption =
        instantiateBaseTypeFrozen shape.FrozenBaseType declaringArgs

    /// Realise a value/free-function symbol's `Scheme` at `level`: a fresh `TyVar` per
    /// declaring typar, the `Constraints` stamped onto them, then the scheme realised
    /// against that array.
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

            // Appended, not prepended: generalisation takes the first target in list order
            // that resolves, so the list must stay in source order.
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

    /// The module-qualified compiled name (`Vesper.Collections.ListModule.fold`), bare for
    /// an unqualified binding.
    let private valueSymbolName (key: BindingKey) : string =
        SymbolKeyOps.qualifiedName (SymbolKey.Binding key)

    /// The zero the two builders below copy from; `Name` is derived from `key` and `Scheme`
    /// is the deferred sentinel until filled.
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

    /// A monomorphic symbol from a closed `FrozenType` scheme. `decl` is a module chain, or
    /// the namespace itself for an unqualified binding (a flat-package extern like
    /// `printfn`).
    let monoFrozen (decl: ModuleContainer) (name: string) (scheme: FrozenType) : ExternalSymbol =
        { ofBindingKey (SymbolKeyOps.bindingKeyOf decl name) with
            Scheme = scheme
        }

    /// A symbol from a `FrozenType` scheme over `arity` declaring typars: a template
    /// freshened per use site, not a closure.
    let scheme
        (decl: ModuleContainer)
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
