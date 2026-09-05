namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

// The declaration SHAPE a lookup carries, the resolver / store / codegen contracts that
// serve it, and the queries over it.

/// Type-declaration shape carried by a by-name or by-key type lookup.
[<RequireQualifiedAccess>]
type ExternalTypeShape =
    /// `frozen` is the abbreviation body as a template, which a use site expands.
    | Abbrev of typars: EqArray<TyparKind> * frozen: FrozenType
    /// A `[<Measure>]` declaration; `term` as `TTypeKindG.Measure` carries it.
    | Measure of term: MeasureTerm
    | Record of shape: ExternalRecordShape
    | Union of shape: ExternalUnionShape
    /// An external enum: named constant cases in source order. The numeric / string / mixed
    /// variant is DERIVED from `cases`, never baked.
    | Enum of cases: EqArray<ExternalEnumCaseShape> * origin: SymbolOrigin
    | Class of shape: ExternalClassShape
    /// A referenced package's intrinsic-repr binding (`type exn = (# class
    /// "System.Exception" #)`): scalar (`int`) or heritable class (`obj`/`exn`).
    | Intrinsic of shape: IntrinsicShape
    /// A capability interface (`disposable`/`equatable`/`comparable`). CLR-only, because a JS
    /// capability is a plain canon-only interface `Class`.
    | IntrinsicInterface of shape: IntrinsicInterfaceShape
    /// NAME + TYPE PARAMETERS are registered, the body is not modelled. `reason` says which
    /// gap, so a use site can report it rather than degrade silently.
    | Unmodelled of reason: UnmodelledReason * typars: EqArray<TyparKind>

    /// The nominal family the DECLARATION commits its name to, which a paired implementation
    /// must agree with. `ValueNone` where the declaration commits to none: an opaque
    /// `type T`, an abbreviation, the `extern` family and an unmodelled form.
    member this.DeclaredFamily: Conformance.TypeKindFamily voption =
        match this with
        | Record _ -> ValueSome Conformance.TypeKindFamily.Record
        | Union _ -> ValueSome Conformance.TypeKindFamily.Union
        | Enum _ -> ValueSome Conformance.TypeKindFamily.Enum
        | Class info ->
            match info.Commitment with
            | ClassCommitment.Class -> ValueSome Conformance.TypeKindFamily.Class
            | ClassCommitment.Interface -> ValueSome Conformance.TypeKindFamily.Interface
            | ClassCommitment.Opaque -> ValueNone
        | Abbrev _
        | Measure _
        | Intrinsic _
        | IntrinsicInterface _
        | Unmodelled _ -> ValueNone

    /// Declared typars in source order.
    member this.TyparKinds: EqArray<TyparKind> =
        match this with
        | Class info -> info.Typars
        | Intrinsic s -> s.Id.Typars
        | IntrinsicInterface s -> s.Typars
        | Enum _ -> EqArray.empty // enums are never generic
        | Measure _ -> EqArray.empty // only a non-generic measure publishes
        | Record r -> r.Typars
        | Union u -> u.Typars
        | Abbrev(typars = ts)
        | Unmodelled(typars = ts) -> ts

    member this.TyparArity: int = this.TyparKinds.Length

    /// The type this abbreviation ALIASES: its body is a keyed type applied to the
    /// abbreviation's own type parameters, each exactly once (`Box<int>` is not an alias).
    member this.AliasedKey: TypeKey voption =
        match this with
        | Abbrev(typars, FTKeyed(key, args)) when args.Length = typars.Length ->
            let arity = typars.Length
            let seen = Array.zeroCreate<bool> arity

            let isAlias =
                args
                |> EqArray.forall (fun a ->
                    match a with
                    | FTTypar(TyparAxis.Declaring, i) when i < arity && not seen.[i] ->
                        seen.[i] <- true
                        true
                    | _ -> false
                )

            if isAlias then ValueSome key else ValueNone
        | _ -> ValueNone

/// The contents of a module or namespace, one segment at a time: the queries dotted-name
/// resolution makes after its first segment has been classified. Every query is scoped to a
/// container.
type IScopeContents =
    /// The module or namespace the dotted SOURCE path denotes.
    abstract TryContainer: sourcePath: string -> ModuleContainer voption
    /// The value identified by `key`, declared directly in `key`'s container.
    abstract TryValue: key: BindingKey -> ExternalSymbol voption
    /// Every case named `name` of a union declared directly in `container`, one entry per
    /// declaring union, each carrying its `[<RequireQualifiedAccess>]` flag. Two entries are
    /// an ambiguity for the caller to report.
    abstract UnionCasesNamed: container: ModuleContainer * name: string -> EqArray<ExternalUnionCase>
    /// Every type named `name` declared directly in `container`, one per generic arity,
    /// ASCENDING by arity: a spelling written without type args takes the narrowest.
    abstract TypesNamed: container: ModuleContainer * name: string -> EqArray<struct (TypeKey * ExternalTypeShape)>
    /// Every declaration of the module `m` this source carries, one per declaring surface,
    /// each with its home. EMPTY from a source that does not declare `m` at all, which an
    /// emitter must treat as an error rather than as a bare `m.Name`.
    abstract DeclarationsOf: m: ModuleKey -> EqArray<ModuleDeclaration>

[<RequireQualifiedAccess>]
module ScopeContents =

    /// Every query a miss: a source with no module structure to expose.
    let empty: IScopeContents =
        { new IScopeContents with
            member _.TryContainer _ = ValueNone
            member _.TryValue _ = ValueNone
            member _.UnionCasesNamed(_, _) = EqArray.empty
            member _.TypesNamed(_, _) = EqArray.empty
            member _.DeclarationsOf _ = EqArray.empty
        }

    /// An `IScopeContents` over TYPES declared in a namespace, and over those alone. `slots`
    /// yields each declared `(namespace, plain name, arity)` once, on first query;
    /// `resolveShape` supplies a slot's body on demand, dropping the slots it declines. A
    /// container is a declared namespace or a dotted prefix of one.
    let typeDirectory
        (slots: unit -> seq<struct (string * string * int)>)
        (resolveShape: TypeKey -> ExternalTypeShape voption)
        : IScopeContents =
        let index =
            lazy
                (let byName = Dictionary<struct (string * string), ResizeArray<int>>()
                 let containers = HashSet<string>(System.StringComparer.Ordinal)

                 for struct (ns, name, arity) in slots () do
                     let arities =
                         match byName.TryGetValue(struct (ns, name)) with
                         | true, a -> a
                         | _ ->
                             let a = ResizeArray<int>()
                             byName.[struct (ns, name)] <- a
                             a

                     arities.Add arity

                     if ns.Length > 0 then
                         containers.Add ns |> ignore
                         let mutable dot = ns.IndexOf '.'

                         while dot >= 0 do
                             containers.Add(ns.Substring(0, dot)) |> ignore
                             dot <- ns.IndexOf('.', dot + 1)

                 for arities in byName.Values do
                     arities.Sort()

                 struct (byName, containers))

        { new IScopeContents with
            member _.TryContainer path =
                let struct (_, containers) = index.Value

                if containers.Contains path then
                    ValueSome(ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey path))
                else
                    ValueNone

            member _.TryValue _ = ValueNone
            member _.UnionCasesNamed(_, _) = EqArray.empty
            // IL declares no modules.
            member _.DeclarationsOf _ = EqArray.empty

            member _.TypesNamed(c, name) =
                match c with
                | ModuleContainer.InModule _ -> EqArray.empty
                | ModuleContainer.InNamespace ns ->
                    let struct (byName, _) = index.Value

                    match byName.TryGetValue(struct (ns.Dotted, name)) with
                    | true, arities ->
                        EqArray.ofSeq
                            [
                                for arity in arities do
                                    let key = SymbolKeyOps.typeKeyOfContainer (TypeContainer.InNamespace ns) name arity

                                    match resolveShape key with
                                    | ValueSome shape -> struct (key, shape)
                                    | ValueNone -> ()
                            ]
                    | _ -> EqArray.empty
        }

    /// A union case's identity: two entries agreeing here are the same case reached twice.
    let private caseIdentity (uc: ExternalUnionCase) = struct (uc.UnionKey, uc.Case.Name)

    /// The nearest-first composition: a container or value is the first source's that
    /// declares it, and a type name is the UNION of the sources' arity sets, the nearest
    /// source winning each arity it declares: `float` at arity 0 and `float<'Measure>` at
    /// arity 1 survive together out of different sources. Union cases and module
    /// declarations are the UNION across sources: a case name recurs across packages,
    /// and the caller decides between the claims; a module PATH is declared by as many
    /// assemblies as write it, and `open` reaches every declaration. A case reached through
    /// two sources appears once.
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
                member _.TryValue key = firstHit (fun s -> s.TryValue key)

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
                    let hits =
                        [
                            for s in sources do
                                match s.TypesNamed(c, name) with
                                | found when found.Length > 0 -> found
                                | _ -> ()
                        ]

                    match hits with
                    | [] -> EqArray.empty
                    | [ single ] -> single
                    | _ ->
                        let byArity = SortedDictionary<int, struct (TypeKey * ExternalTypeShape)>()

                        for found in hits do
                            for struct (key, shape) in found.Underlying do
                                if not (byArity.ContainsKey key.TyparArity) then
                                    byArity.Add(key.TyparArity, struct (key, shape))

                        EqArray.ofSeq byArity.Values

                member _.DeclarationsOf m =
                    EqArray.ofSeq
                        [
                            for s in sources do
                                yield! (s.DeclarationsOf m).Underlying
                        ]
            }

    /// The value a LITERAL dotted spelling denotes: its leading segments are the container
    /// and its last segment the short name. Where the qualifier and the short name are
    /// already separate, use `tryValueIn` instead: a binding's own name may contain a dot
    /// (`` let ``a.size`` ``).
    let tryValueAt (scope: IScopeContents) (written: string) : ExternalSymbol voption =
        match written.LastIndexOf '.' with
        | i when i > 0 ->
            match scope.TryContainer(written.Substring(0, i)) with
            | ValueSome c -> scope.TryValue(SymbolKeyOps.bindingKeyOf c (written.Substring(i + 1)))
            | ValueNone -> ValueNone
        | _ -> scope.TryValue(SymbolKeyOps.bindingKeyOf (ModuleContainer.InNamespace NamespaceKey.Global) written)

    /// The first hit `pick` admits among the types called `name` in `containers`, taking each
    /// container's types narrowest arity first per `TypesNamed`.
    let tryPickTypeIn
        (scope: IScopeContents)
        (containers: ModuleContainer list)
        (pick: TypeKey -> ExternalTypeShape -> 'T voption)
        (name: string)
        : 'T voption =
        let rec go (cs: ModuleContainer list) =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                let declared = (scope.TypesNamed(c, name)).Underlying
                let mutable result = ValueNone
                let mutable i = 0

                while result.IsNone && i < declared.Length do
                    let (struct (key, shape)) = declared.[i]
                    result <- pick key shape
                    i <- i + 1

                match result with
                | ValueSome _ -> result
                | ValueNone -> go rest

        go containers

    /// The value `name` denotes among `containers`: the first of them declaring it.
    let tryValueIn (scope: IScopeContents) (containers: ModuleContainer list) (name: string) : ExternalSymbol voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match scope.TryValue(SymbolKeyOps.bindingKeyOf c name) with
                | ValueSome _ as hit -> hit
                | ValueNone -> go rest

        go containers

    /// `inner` with each result rewritten: `value` over a value symbol, `case` over a union
    /// case, `shape` over a type shape at its resolved key, `declaration` over a module
    /// declaration. `TryContainer` passes through.
    let decorate
        (value: ExternalSymbol -> ExternalSymbol)
        (case: ExternalUnionCase -> ExternalUnionCase)
        (shape: TypeKey -> ExternalTypeShape -> ExternalTypeShape)
        (declaration: ModuleDeclaration -> ModuleDeclaration)
        (inner: IScopeContents)
        : IScopeContents =
        { new IScopeContents with
            member _.TryContainer path = inner.TryContainer path

            member _.TryValue key =
                inner.TryValue key |> ValueOption.map value

            member _.UnionCasesNamed(c, name) =
                inner.UnionCasesNamed(c, name) |> EqArray.map case

            member _.TypesNamed(c, name) =
                inner.TypesNamed(c, name)
                |> EqArray.map (fun (struct (key, s)) -> struct (key, shape key s))

            member _.DeclarationsOf m =
                inner.DeclarationsOf m |> EqArray.map declaration
        }

    /// `inner` with each value symbol rewritten. Every other channel passes through.
    let mapValues (value: ExternalSymbol -> ExternalSymbol) (inner: IScopeContents) : IScopeContents =
        decorate value id (fun _ s -> s) id inner

    /// `inner` with every query cached, MISSES included. The resolver repeats a segment read
    /// once per candidate spelling, and a contract is immutable for a compile.
    let memoize (inner: IScopeContents) : IScopeContents =
        let containers = ConcurrentDictionary<string, ModuleContainer voption>()

        let values = ConcurrentDictionary<BindingKey, ExternalSymbol voption>()

        let cases =
            ConcurrentDictionary<struct (ModuleContainer * string), EqArray<ExternalUnionCase>>()

        let types =
            ConcurrentDictionary<struct (ModuleContainer * string), EqArray<struct (TypeKey * ExternalTypeShape)>>()

        let modules = ConcurrentDictionary<ModuleKey, EqArray<ModuleDeclaration>>()

        { new IScopeContents with
            member _.TryContainer path =
                containers.GetOrAdd(path, (fun p -> inner.TryContainer p))

            member _.TryValue key =
                values.GetOrAdd(key, (fun k -> inner.TryValue k))

            member _.UnionCasesNamed(c, name) =
                cases.GetOrAdd(struct (c, name), (fun (struct (c, n)) -> inner.UnionCasesNamed(c, n)))

            member _.TypesNamed(c, name) =
                types.GetOrAdd(struct (c, name), (fun (struct (c, n)) -> inner.TypesNamed(c, n)))

            member _.DeclarationsOf m =
                modules.GetOrAdd(m, (fun k -> inner.DeclarationsOf k))
        }

/// The RESOLVER view of the external-symbol contract: spelling → identity, opens-aware.
/// Downstream of name resolution, passes consume the key-addressed store view instead.
type IExternalSymbolResolver =
    /// The module structure this source declares, for segment-by-segment resolution. A
    /// published value or type is reached through its declaring container.
    abstract Scope: IScopeContents

    /// A field name → every record declaring a field of that name; unqualified
    /// record-literal / record-pattern resolution intersects these sets to pin the type.
    abstract TryRecordsWithField: fieldName: string -> EqArray<ExternalRecordCandidate>

    /// What this provider opens with no `open` written for it, OUTERMOST first: an `[<AutoOpen>]`
    /// module follows the scope that holds it.
    abstract ImplicitOpens: ImplicitOpen list

/// What the COMPILING TARGET lays out and encodes, which no `.fsi` can state: `int` is a
/// value type on the CLR and nothing is on JS. Implemented only by a source that IS the
/// platform metadata.
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

    /// The `(canon, platform type id)` declarations this provider carries, readable both ways:
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
        /// The name the function emits under, already resolved against the declaring key's
        /// own short name.
        EmittedName: string
        /// The producer's SOURCE parameter grouping: the curried `Signature` alone can't
        /// tell a group `f (x,y)` from a tuple param `f (t:int*int)`.
        ValRepr: TastAccessor.ValRepr voption
        /// The symbol's typar bounds over the method-typar axis. A `Coercion` bound lets a
        /// phantom slot be recovered from the constrained source's interface witness.
        Constraints: EqSet<FrozenConstraint>
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
    /// Every declaration of an external module the referenced surfaces carry, each with its
    /// home. EMPTY where none declares it, which emission must refuse rather than guess a
    /// class name for.
    abstract DeclarationsOf: m: ModuleKey -> EqArray<ModuleDeclaration>
    /// The platform type id emission mints a primitive reference through: `int` →
    /// `"System.Int32"`. `ValueNone` for a canon the compiling target binds no id for.
    abstract TryPlatformTypeId: canon: TypeKey -> PlatformTypeId voption
    /// Does the compiling target lay this type out as a VALUE? Already SETTLED: the target's
    /// layout, else what the declaration asked for. `ValueNone` when neither states one.
    abstract IsValueType: key: TypeKey -> bool voption
    /// The RAW target facts, unmerged with any declaration, so emission classifies a shape
    /// through the same facts the front end typed it against.
    abstract Platform: IPlatformFacts voption

module ExternalSymbols =

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
                                      Commitment = ClassCommitment.Interface
                                      Members = members
                                  } -> ValueSome members
        | ExternalTypeShape.IntrinsicInterface shape -> ValueSome shape.Members
        | _ -> ValueNone

    /// The `[<Struct>]` a CLASS, RECORD or UNION declaration asked for. Every other shape is
    /// `ValueNone`: an intrinsic's layout is the target's alone, and no other shape carries the
    /// request. A caller asks the platform first, because the target may erase the request.
    let declaredValueType (shape: ExternalTypeShape) : bool voption =
        match shape with
        | ExternalTypeShape.Class s -> ValueSome s.Flags.IsValueType
        | ExternalTypeShape.Record r -> ValueSome r.IsValueType
        | ExternalTypeShape.Union u -> ValueSome u.IsValueType
        | _ -> ValueNone

    /// A capability is an interface on BOTH targets, and only the carried shape differs.
    let isInterfaceShape (shape: ExternalTypeShape) : bool =
        match shape with
        | ExternalTypeShape.Class s -> s.IsInterface
        | ExternalTypeShape.IntrinsicInterface _ -> true
        | _ -> false

    /// An intrinsic shape's canonical key.
    let intrinsicCanonOf (shape: ExternalTypeShape) : TypeKey voption =
        match shape with
        | ExternalTypeShape.Intrinsic { Id = { Canon = c } } -> ValueSome c
        | _ -> ValueNone

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

    /// The identity and shape `metaName` denotes at `arity`. `metaName` is an exact metadata
    /// rendering read by key, with no opens applied; a source-written name resolves through
    /// `IScopeContents` under the use site's opens instead.
    let tryMetaTypeAt
        (store: IExternalSymbolStore)
        (metaName: string)
        (arity: int)
        : struct (TypeKey * ExternalTypeShape) voption =
        let key = SymbolKeyOps.qualifiedTypeKeyOf metaName arity

        match store.TryLookupType key with
        | ValueSome shape when shape.TyparArity = key.TyparArity -> ValueSome(struct (key, shape))
        | _ -> ValueNone

    let tryMetaType (store: IExternalSymbolStore) (metaName: string) : ExternalTypeShape voption =
        tryMetaTypeAt store metaName 0
        |> ValueOption.map (fun (struct (_, shape)) -> shape)

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
            match provider.TryLookupType canon with
            | ValueSome(ExternalTypeShape.Intrinsic {
                                                        Id = {
                                                                 Platform = IntrinsicPlatform.Bound fqn
                                                             }
                                                    }) -> ValueSome(ofKey (SymbolKeyOps.qualifiedTypeKeyOf fqn.Value 0))
            | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                ValueSome
                    {
                        RuntimeNames.CapabilityIdentity.Key = SymbolKeyOps.qualifiedTypeKeyOf platform.Value 0
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

    /// Instantiate a member's `Signature` with SOME method typars PRE-BOUND (`seed`, index →
    /// type) instead of fresh; the rest freshen normally. A typar in no bare parameter
    /// position is unsolvable by unification alone.
    let instantiateSignatureWith
        (thaw: IMeasuredThaw)
        (seed: (int * SemType) list)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        (level: int)
        : SemType =
        instantiateWith
            thaw
            (TyparInstantiation.atCallSite thaw.Store level seed declaringArgs)
            (ExternalSignature.openTemplate m.Signature)

    /// Instantiate a member's `Signature` at `level`: `FTTypar(Declaring,i) →
    /// declaringArgs.[i]`, `FTTypar(Method,j) → fresh TyVar at level` (one per index, shared
    /// across the argument groups and `Return`).
    let instantiateSignature
        (thaw: IMeasuredThaw)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        (level: int)
        : SemType =
        instantiateSignatureWith thaw [] m declaringArgs level

    /// The OPEN instantiation of a member's `Signature`: declaring typars substituted from
    /// `declaringArgs`, the member's own method typars left as `TyTypar(Method,j)` markers.
    /// The applicability-filtering form, in which a generic method's marker stays a wildcard.
    let openSignature (thaw: IMeasuredThaw) (m: ExternalMember) (declaringArgs: SemType[]) : SemType =
        instantiateWith thaw (TyparInstantiation.openMethod declaringArgs) (ExternalSignature.openTemplate m.Signature)

    /// A member's method-typar BOUNDS at a use site, one per method typar.
    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`; a `FTTypar(Method,j)` ref stays an
    /// inert marker.
    let instantiateSignatureBounds
        (thaw: IMeasuredThaw)
        (m: ExternalMember)
        (declaringArgs: SemType[])
        : EqArray<SemType voption> =
        let inst = TyparInstantiation.openMethod declaringArgs

        m.Signature.MethodTypars
        |> EqArray.map (ValueOption.map (instantiateWith thaw inst))

    let instantiateFieldType (thaw: IMeasuredThaw) (f: ExternalFieldShape) (declaringArgs: SemType[]) : SemType =
        instantiateDeclaring thaw f.Frozen declaringArgs

    let instantiateCaseFieldTypes (thaw: IMeasuredThaw) (c: ExternalCaseShape) (declaringArgs: SemType[]) : SemType[] =
        let fts = c.FrozenFieldTypes
        Array.init fts.Length (fun i -> instantiateDeclaring thaw fts.[i] declaringArgs)

    /// Instantiate a class's `FrozenInterfaces`, or a union's declared `interface <ty>` impls,
    /// at a use site.
    let instantiateInterfacesOf
        (thaw: IMeasuredThaw)
        (interfaces: EqArray<FrozenNominal>)
        (declaringArgs: SemType[])
        : SemType[] =
        Array.init
            interfaces.Length
            (fun i -> instantiateDeclaring thaw (FrozenNominal.ty interfaces.[i]) declaringArgs)

    let instantiateInterfaces (thaw: IMeasuredThaw) (shape: ExternalClassShape) (declaringArgs: SemType[]) : SemType[] =
        instantiateInterfacesOf thaw shape.FrozenInterfaces declaringArgs

    /// Shared by a class shape and a heritable primitive's class surface.
    let instantiateBaseTypeFrozen
        (thaw: IMeasuredThaw)
        (baseType: FrozenNominal voption)
        (declaringArgs: SemType[])
        : SemType voption =
        baseType
        |> ValueOption.map (fun b -> instantiateDeclaring thaw (FrozenNominal.ty b) declaringArgs)

    let instantiateBaseType
        (thaw: IMeasuredThaw)
        (shape: ExternalClassShape)
        (declaringArgs: SemType[])
        : SemType voption =
        instantiateBaseTypeFrozen thaw shape.FrozenBaseType declaringArgs

    /// Instantiate a value/free-function symbol's `Scheme` at `level`: a fresh `TyVar` per
    /// declaring typar, the `Constraints` stamped onto them, then the scheme instantiated
    /// against that array.
    let instantiateSymbol (thaw: IMeasuredThaw) (sym: ExternalSymbol) (level: int) : SemType =
        let store = thaw.Store

        let inst ft fresh =
            FrozenTypeBridge.instantiateDeclaring thaw ft fresh

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
                | ExternalConstraint.Bound b ->
                    let cstr: SemanticConstraint =
                        {
                            Kind = TyparConstraintKind.toSemantic (fun target -> inst target fresh) b.Kind
                            DeclKey = NodeKey.ofSource 0 NodeKind.Unknown
                        }

                    store.Constraints.Prepend(UnionFind.find store freshTvs.[b.TyparIndex], cstr)
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
                            SupportTys = EqArray.ofSeq (seq { for i in idxs -> fresh.[i] })
                            ArgTypes = EqArray.ofSeq (seq { for ft in argFts -> inst ft fresh })
                            ReturnType = inst retFt fresh
                        }

                    for i in idxs do
                        store.Srtp.Prepend(UnionFind.find store freshTvs.[i], sig_)
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
            CompiledName = ValueNone
            ValRepr = ValueNone
            ImportForm = ImportForm.Named
            InlineBody = ValueNone
            Attributes = EqArray.empty
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
