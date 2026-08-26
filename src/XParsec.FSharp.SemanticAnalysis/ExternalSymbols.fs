namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

// The declaration SHAPE a lookup carries, the resolver / store / codegen contracts that
// serve it, and the queries over it.

/// Type-declaration shape carried by a by-name or by-key type lookup.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    /// `frozen` is the abbreviation body as a template, which a use site expands.
    | Abbrev of arity: int * frozen: FrozenType
    /// Field order matches source. `isValueType` is the `[<Struct>]` the declaration asked
    /// for, carried so a consuming unit reads the same layout the declaring one did.
    /// `requiresQualifiedAccess` is the record's `[<RequireQualifiedAccess>]`, which forces a
    /// consumer to write `{ R.X = … }`.
    | Record of
        arity: int *
        fields: EqArray<ExternalFieldShape> *
        origin: SymbolOrigin *
        isValueType: bool *
        requiresQualifiedAccess: bool
    /// Case order matches source. `interfaces` are the union's directly-declared
    /// `interface <ty>` impls. `requiresQualifiedAccess` is the union's
    /// `[<RequireQualifiedAccess>]`, so a case reached through the type carries the flag.
    | Union of
        arity: int *
        cases: EqArray<ExternalCaseShape> *
        interfaces: EqArray<FrozenNominal> *
        origin: SymbolOrigin *
        requiresQualifiedAccess: bool
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
    /// use site can report it rather than degrade silently.
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

/// The contents of a module or namespace, one segment at a time: what a dotted name's
/// resolution asks after its first segment has been classified. Every answer is scoped to the
/// entity asked about, so no bare-name reverse index takes part.
type IScopeContents =
    /// The module or namespace the dotted SOURCE path denotes.
    abstract TryContainer: sourcePath: string -> ModuleContainer voption
    /// The value `name` declared directly in `container`. A value answers under its compiled
    /// short name and under the short name its source writes, which a `[<CompiledName>]`
    /// makes differ.
    abstract TryValue: container: ModuleContainer * name: string -> ExternalSymbol voption
    /// Every case named `name` of a union declared directly in `container`; each answer
    /// carries its `[<RequireQualifiedAccess>]` flag for the caller to report. Two answers
    /// are two unions of the container declaring the name, which the caller reports as an
    /// ambiguity.
    abstract UnionCasesNamed: container: ModuleContainer * name: string -> EqArray<ExternalUnionCase>
    /// Every type named `name` declared directly in `container`, one per generic arity.
    abstract TypesNamed: container: ModuleContainer * name: string -> EqArray<struct (TypeKey * ExternalTypeShape)>

[<RequireQualifiedAccess>]
module ScopeContents =

    /// Every query a miss: a source with no module structure to expose.
    let empty: IScopeContents =
        { new IScopeContents with
            member _.TryContainer _ = ValueNone
            member _.TryValue(_, _) = ValueNone
            member _.UnionCasesNamed(_, _) = EqArray.empty
            member _.TypesNamed(_, _) = EqArray.empty
        }

    /// A union case's identity: two entries agreeing here are the same case reached twice.
    let private caseIdentity (uc: ExternalUnionCase) = struct (uc.UnionKey, uc.Case.Name)

    /// The nearest-first composition: a container or value is the first source's that
    /// declares it, and a type name is the first source's non-empty arity set. Union cases
    /// are the UNION across sources, one entry per identity: a case name recurs across
    /// packages, and the caller decides between the claims.
    let composite (sources: IScopeContents list) : IScopeContents =
        match sources with
        | [] -> empty
        | [ single ] -> single
        | _ ->
            let sources = List.toArray sources

            let inline firstHit (f: IScopeContents -> 'a voption) : 'a voption =
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < sources.Length do
                    result <- f sources.[i]
                    i <- i + 1

                result

            { new IScopeContents with
                member _.TryContainer path = firstHit (fun s -> s.TryContainer path)
                member _.TryValue(c, name) = firstHit (fun s -> s.TryValue(c, name))

                member _.UnionCasesNamed(c, name) =
                    let seen = HashSet<struct (TypeKey * string)>(HashIdentity.Structural)

                    EqArray.ofSeq
                        [
                            for s in sources do
                                for uc in (s.UnionCasesNamed(c, name)).Underlying do
                                    if seen.Add(caseIdentity uc) then
                                        uc
                        ]

                member _.TypesNamed(c, name) =
                    let mutable result = EqArray.empty
                    let mutable i = 0

                    while result.Length = 0 && i < sources.Length do
                        result <- sources.[i].TypesNamed(c, name)
                        i <- i + 1

                    result
            }

    /// The value a LITERAL dotted spelling denotes: its leading segments are the container
    /// and its last segment the short name. A caller holding the qualifier and the short name
    /// apart passes them apart — resolution does, through `containersAtPath`, because a
    /// binding's own name may hold a dot (`` let ``a.size`` ``).
    let tryValueAt (scope: IScopeContents) (written: string) : ExternalSymbol voption =
        match written.LastIndexOf '.' with
        | i when i > 0 ->
            match scope.TryContainer(written.Substring(0, i)) with
            | ValueSome c -> scope.TryValue(c, written.Substring(i + 1))
            | ValueNone -> ValueNone
        | _ -> scope.TryValue(ModuleContainer.InNamespace NamespaceKey.Global, written)

    /// The containers a BARE name is read against: the root namespace, then each of
    /// `prefixes` that names one, deduplicated. `OpenScope.Prefixes` repeats a prefix a
    /// namespace header and an ambient prelude both carry.
    let openedContainers (scope: IScopeContents) (prefixes: string list) : ModuleContainer list =
        let found = ResizeArray<ModuleContainer>()
        found.Add(ModuleContainer.InNamespace NamespaceKey.Global)

        for p in prefixes do
            match scope.TryContainer p with
            | ValueSome c when not (found.Contains c) -> found.Add c
            | _ -> ()

        List.ofSeq found

    /// `inner` with each answer rewritten: `value` over a value, `case` over a union case,
    /// `shape` over a type shape at the identity it answered under. `TryContainer` passes
    /// through.
    let decorate
        (value: ExternalSymbol -> ExternalSymbol)
        (case: ExternalUnionCase -> ExternalUnionCase)
        (shape: TypeKey -> ExternalTypeShape -> ExternalTypeShape)
        (inner: IScopeContents)
        : IScopeContents =
        { new IScopeContents with
            member _.TryContainer path = inner.TryContainer path

            member _.TryValue(c, name) =
                inner.TryValue(c, name) |> ValueOption.map value

            member _.UnionCasesNamed(c, name) =
                inner.UnionCasesNamed(c, name) |> EqArray.map case

            member _.TypesNamed(c, name) =
                inner.TypesNamed(c, name)
                |> EqArray.map (fun (struct (key, s)) -> struct (key, shape key s))
        }

    /// `inner` with every query cached, MISSES included. The resolver repeats a segment read
    /// once per candidate spelling, and a contract is immutable for a compile.
    let memoize (inner: IScopeContents) : IScopeContents =
        let containers = ConcurrentDictionary<string, ModuleContainer voption>()

        let values =
            ConcurrentDictionary<struct (ModuleContainer * string), ExternalSymbol voption>()

        let cases =
            ConcurrentDictionary<struct (ModuleContainer * string), EqArray<ExternalUnionCase>>()

        let types =
            ConcurrentDictionary<struct (ModuleContainer * string), EqArray<struct (TypeKey * ExternalTypeShape)>>()

        { new IScopeContents with
            member _.TryContainer path =
                containers.GetOrAdd(path, (fun p -> inner.TryContainer p))

            member _.TryValue(c, name) =
                values.GetOrAdd(struct (c, name), (fun (struct (c, n)) -> inner.TryValue(c, n)))

            member _.UnionCasesNamed(c, name) =
                cases.GetOrAdd(struct (c, name), (fun (struct (c, n)) -> inner.UnionCasesNamed(c, n)))

            member _.TypesNamed(c, name) =
                types.GetOrAdd(struct (c, name), (fun (struct (c, n)) -> inner.TypesNamed(c, n)))
        }

/// The RESOLVER view of the external-symbol contract: spelling → identity, opens-aware.
/// Downstream of name resolution, passes speak the key-addressed store view instead.
type IExternalSymbolResolver =
    /// The module structure this source declares, for segment-by-segment resolution. The one
    /// route to a published VALUE: a value is reached through its declaring container.
    abstract Scope: IScopeContents
    /// Look up a `type` by canonical compiled name, returning its REGISTERED identity plus
    /// its body shape from the one hit.
    abstract TryLookupType: name: string -> struct (TypeKey * ExternalTypeShape) voption

    /// A field name → every record declaring a field of that name; unqualified
    /// record-literal / record-pattern resolution intersects these sets to pin the type.
    abstract TryRecordsWithField: fieldName: string -> EqArray<ExternalRecordCandidate>

    /// The AMBIENT `[<AutoOpen>]` open prefixes this provider contributes, probed strictly
    /// BEHIND every explicit `open`; earliest wins: `["Vesper.ArithmeticOperators"; "Vesper"]`.
    abstract AmbientOpenPrefixes: string list

/// What the COMPILING TARGET lays out and encodes, which no `.fsi` can state: `int` is a
/// value type on the CLR and nothing is on JS. Answered only by a source that IS the platform
/// metadata, so the whole interface is what a contract source declines.
type IPlatformFacts =
    /// Does the target lay this type out as a VALUE? `ValueNone` for a key it does not know.
    abstract IsValueType: key: TypeKey -> bool voption

    /// The nominal a tuple of `arity` elements BECOMES: the OUTERMOST constructor, so a target
    /// that nests keeps the nesting in its encoder. `ValueNone` below arity 2, not a tuple.
    abstract TupleType: arity: int -> TypeKey voption

/// The STORE view of the external-symbol contract: identity → payload, once identity is
/// resolved. Addressed by `(namespace, arity-qualified name)` and NOTHING ELSE, so
/// same-named types in two assemblies are indistinguishable here.
type IExternalSymbolStore =
    /// A type declaration's body by resolved key; `ValueNone` for an unknown key or a body
    /// shape the provider doesn't model.
    abstract TryLookupType: key: TypeKey -> ExternalTypeShape voption

    /// ALL overloads of a member by name: the candidate set the application-site overload
    /// resolver picks from. Empty from providers that don't model members.
    abstract TryLookupMembers: key: TypeKey * memberName: string -> EqArray<ExternalMember>

    /// A member by resolved `MemberKey`: the channel a MEMBER splice site reaches an
    /// `InlineBody` through. BY KEY: a by-name lookup collapses an overload set to one pick
    /// and can hand back a different overload's entry.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption

    /// The TS index signature(s) `{ [k: K]: V }`: the seam `x.[k]` / `x.[k] <- v` goes
    /// through, `(key, value)` templates over the DECLARING typars. Both kinds may be present.
    abstract TryLookupIndexSignature: key: TypeKey -> (FrozenType * FrozenType) list

    /// A value/free-function symbol by resolved key: the channel a VALUE splice site
    /// reaches an `InlineBody` through.
    abstract TryLookupByKey: key: BindingKey -> ExternalSymbol voption

    /// The `(canon, platform-repr)` declarations this provider carries, readable both ways:
    /// `int` → `"System.Int32"` and `"System.Exception"` → `exn`. Empty from providers
    /// carrying no intrinsics.
    abstract IntrinsicTypeMap: IntrinsicTypeMap

    /// This source's target facts, `ValueNone` unless it IS the platform metadata.
    abstract Platform: IPlatformFacts voption

/// Both views on ONE object: raw facts only, with NO capability predicates ("is this type
/// disposable?" is a language judgment the passes make). Every lookup must be thread-safe.
type IExternalSymbolProvider =
    inherit IExternalSymbolResolver
    inherit IExternalSymbolStore

[<AutoOpen>]
module IExternalSymbolStoreExtensions =

    type IExternalSymbolStore with

        /// The HEAD of the name's overload set, so the store's own ordering decides which:
        /// most-params first from IL metadata, declaration order elsewhere.
        member this.TryLookupMember(key: TypeKey, memberName: string) : ExternalMember voption =
            match this.TryLookupMembers(key, memberName) with
            | EqEmpty -> ValueNone
            | ms -> ValueSome ms.[0]

        /// "No platform" and "the platform doesn't know" flattened into the one `ValueNone`
        /// a layout question treats alike.
        member this.IsValueType(key: TypeKey) : bool voption =
            this.Platform |> ValueOption.bind (fun p -> p.IsValueType key)

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
    abstract TryLookupType: key: TypeKey -> ExternalTypeShape voption
    /// The exact member the front end resolved, by the `MemberKey` it stamped, with no
    /// overload re-pick.
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
    /// Select the `.ctor` a `new` emits: by the exact `MemberKey` the front end recorded
    /// when it has one, telling `ArgumentException(string, string)` from
    /// `(string, Exception)`; else the sole ctor of `arity` params.
    abstract TryLookupCtor: declKey: TypeKey * chosen: SymbolKey voption * arity: int -> ExternalMember voption
    /// Rebase a capability member onto its true base declarer: `enumerator.MoveNext` is
    /// declared on the non-generic `IEnumerator`, and a member-ref parented on the derived
    /// interface would `MissingMethodException`.
    abstract TryRebaseCapabilityMember: key: SymbolKey -> SymbolKey voption
    /// The open `FrozenType` signature of a module-level function by its value key.
    abstract TryLookupOpenSignature: key: BindingKey -> CodegenOpenSignature voption
    /// The platform spelling emission mints a primitive reference through: `int` →
    /// `"System.Int32"`. `ValueNone` for a canon with no repr on the compiling target.
    abstract TryPlatformRepr: canon: TypeKey -> string voption
    /// Does the compiling target lay this type out as a VALUE? Already SETTLED: the target's
    /// layout, else what the declaration asked for. `ValueNone` when neither states one.
    abstract IsValueType: key: TypeKey -> bool voption
    /// The RAW target facts, unmerged with any declaration, so emission classifies a shape
    /// through the same answers the front end typed it against.
    abstract Platform: IPlatformFacts voption

module ExternalSymbols =

    /// Invert a NAME-INDEXED source's qualified name back to a key. Sound only when its
    /// type keys are `InNamespace`. Arity comes from the SHAPE, never from the arity
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

    /// The `[<Struct>]` a CLASS or RECORD declaration asked for. Every other shape is
    /// `ValueNone`: an intrinsic's layout is the target's alone, and no other shape carries the
    /// request. A caller asks the platform first, because the target may erase the request.
    let declaredValueType (shape: ExternalTypeShape) : bool voption =
        match shape with
        | ExternalTypeShape.Class s -> ValueSome s.Flags.IsValueType
        | ExternalTypeShape.Record(isValueType = isValueType) -> ValueSome isValueType
        | _ -> ValueNone

    /// A capability is an interface on BOTH targets, and only the carried shape differs.
    let isInterfaceShape (shape: ExternalTypeShape) : bool =
        match shape with
        | ExternalTypeShape.Class s -> s.IsInterface
        | ExternalTypeShape.IntrinsicInterface _ -> true
        | _ -> false

    /// The heritable-primitive surface a shape carries (`obj`/`exn`): what an `inherit` may write.
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
        (canon: TypeKey)
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
        let cache = Dictionary<int, SemType>()

        for (j, ty) in seed do
            cache.[j] <- ty

        let methodVar = methodFreshener store cache level
        let decl i = declaringArgs.[i]
        let noLocal = localTyparInTemplate
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
        let noLocal = localTyparInTemplate
        instantiateWith decl methodOpen noLocal (ExternalSignature.openTemplate m.Signature)

    /// A member's method-typar BOUNDS at a use site, one per index.
    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`; a `FTTypar(Method,j)` ref stays an
    /// inert marker. Empty when uncarried.
    let instantiateSignatureBounds (m: ExternalMember) (declaringArgs: SemType[]) : EqArray<SemType voption> =
        let decl i = declaringArgs.[i]
        let methodOpen j = TyTypar(TyparAxis.Method, j)
        let noLocal = localTyparInTemplate

        m.Signature.MethodTyparBounds
        |> EqArray.map (ValueOption.map (fun ft -> instantiateWith decl methodOpen noLocal ft))

    let instantiateFieldType (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring f.Frozen declaringArgs

    let instantiateCaseFieldTypes (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        let fts = c.FrozenFieldTypes
        Array.init fts.Length (fun i -> instantiateDeclaring fts.[i] declaringArgs)

    /// Realise a class's `FrozenInterfaces`, or a union's declared `interface <ty>` impls,
    /// at a use site.
    let instantiateInterfacesOf (interfaces: EqArray<FrozenNominal>) (declaringArgs: SemType[]) : SemType[] =
        Array.init interfaces.Length (fun i -> instantiateDeclaring interfaces.[i].Frozen declaringArgs)

    let instantiateInterfaces (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType[] =
        instantiateInterfacesOf shape.FrozenInterfaces declaringArgs

    /// Shared by a class shape and a heritable primitive's class surface.
    let instantiateBaseTypeFrozen (baseType: FrozenNominal voption) (declaringArgs: SemType[]) : SemType voption =
        baseType
        |> ValueOption.map (fun b -> instantiateDeclaring b.Frozen declaringArgs)

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

    /// The zero the two builders below copy from; `Scheme` is the deferred sentinel until
    /// filled.
    let private ofBindingKey (key: BindingKey) : ExternalSymbol =
        {
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
