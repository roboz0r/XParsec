namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

/// Addresses a member by NAME, so it reaches the whole overload set; `MemberKey` addresses
/// one overload by identity.
type KeyedMemberName =
    {
        DeclaringType: SymbolKey
        Name: string
    }

/// `KeyedMemberName` with the declaring type RENDERED: the address a by-name member index
/// is keyed on.
type ExternalMemberName =
    {
        /// The declaring type's qualified compiled name.
        DeclaringType: string
        Name: string
    }

module ExternalMemberName =

    let ofKeyed (key: KeyedMemberName) : ExternalMemberName =
        {
            DeclaringType = SymbolKeyOps.qualifiedName key.DeclaringType
            Name = key.Name
        }

module ExternalSymbolProviders =


    /// Channels whose types' identities are not a field: a name IS the identity, so a type
    /// key is minted from the name and a key lookup is the rendered name lookup. A source
    /// holding `TypeContainer.InModule` types supplies `KeyIndexedChannels` instead.
    type NamedChannels =
        {
            TryLookup: string -> ExternalSymbol voption
            TryLookupType: string -> ExternalTypeShape voption
            TryLookupUnionCase: string -> ExternalUnionCase voption
            TryRecordsWithField: string -> EqArray<ExternalRecordCandidate>
            AmbientOpenPrefixes: string list
            TryLookupMembers: ExternalMemberName -> EqArray<ExternalMember>
            TryLookupIndexSignature: string -> (FrozenType * FrozenType) list
            IntrinsicTypeMap: IntrinsicTypeMap
            IsValueType: TypeKey -> bool voption
        }

    module NamedChannels =

        /// Every channel misses, so override just what the source models.
        let empty: NamedChannels =
            {
                TryLookup = fun _ -> ValueNone
                TryLookupType = fun _ -> ValueNone
                TryLookupUnionCase = fun _ -> ValueNone
                TryRecordsWithField = fun _ -> EqArray.empty
                AmbientOpenPrefixes = []
                TryLookupMembers = fun _ -> EqArray.empty
                TryLookupIndexSignature = fun _ -> []
                IntrinsicTypeMap = IntrinsicTypeMap.empty
                IsValueType = fun _ -> ValueNone
            }

    /// Channels that HOLD their types' identities. Needed when a type is
    /// `TypeContainer.InModule`, whose rendering is not what the source writes.
    type KeyIndexedChannels =
        {
            ShapesByKey: IReadOnlyDictionary<SymbolKey, ExternalTypeShape>
            /// A type's FULL member list, in DECLARATION order, because the by-name overload
            /// scan and the by-key selection both depend on that order.
            MembersByKey: IReadOnlyDictionary<SymbolKey, ResizeArray<ExternalMember>>
            /// Written type name -> registered identity.
            ResolveTypeName: string -> TypeKey voption
            TryLookup: string -> ExternalSymbol voption
            TryLookupUnionCase: string -> ExternalUnionCase voption
            TryRecordsWithField: string -> EqArray<ExternalRecordCandidate>
            AmbientOpenPrefixes: string list
            IntrinsicTypeMap: IntrinsicTypeMap
        }

    module KeyIndexedChannels =

        let empty: KeyIndexedChannels =
            {
                ShapesByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                MembersByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                ResolveTypeName = fun _ -> ValueNone
                TryLookup = fun _ -> ValueNone
                TryLookupUnionCase = fun _ -> ValueNone
                TryRecordsWithField = fun _ -> EqArray.empty
                AmbientOpenPrefixes = []
                IntrinsicTypeMap = IntrinsicTypeMap.empty
            }

    /// Type channels answered BY KEY, whichever way the source came by them:
    /// `ofNamed` renders the key onto a name index, `ofKeyIndexes` reads a real one.
    type KeyedChannels =
        {
            Named: NamedChannels
            /// Identity + shape from one read. Derived by both builders, never supplied.
            TypeByName: string -> struct (TypeKey * ExternalTypeShape) voption
            TypeShapeByKey: SymbolKey -> ExternalTypeShape voption
            TypeMembersByKey: KeyedMemberName -> EqArray<ExternalMember>
        }

    module KeyedChannels =

        let ofNamed (channels: NamedChannels) : KeyedChannels =
            {
                Named = channels
                TypeByName =
                    fun name ->
                        channels.TryLookupType name
                        |> ValueOption.map (ExternalSymbols.nameKeyedTypeHit name)
                TypeShapeByKey = fun key -> channels.TryLookupType(SymbolKeyOps.qualifiedName key)
                TypeMembersByKey = ExternalMemberName.ofKeyed >> channels.TryLookupMembers
            }

        let ofKeyIndexes (channels: KeyIndexedChannels) : KeyedChannels =
            let shapeByKey (key: SymbolKey) : ExternalTypeShape voption =
                match channels.ShapesByKey.TryGetValue key with
                | true, shape -> ValueSome shape
                | _ -> ValueNone

            let membersNamed (key: KeyedMemberName) : EqArray<ExternalMember> =
                match channels.MembersByKey.TryGetValue key.DeclaringType with
                | true, ms ->
                    EqArray.ofSeq
                        [
                            for m in ms do
                                if m.Name = key.Name then
                                    m
                        ]
                | _ -> EqArray.empty

            {
                Named =
                    { NamedChannels.empty with
                        TryLookup = channels.TryLookup
                        TryLookupUnionCase = channels.TryLookupUnionCase
                        TryRecordsWithField = channels.TryRecordsWithField
                        AmbientOpenPrefixes = channels.AmbientOpenPrefixes
                        IntrinsicTypeMap = channels.IntrinsicTypeMap
                    }
                TypeByName =
                    fun name ->
                        match channels.ResolveTypeName name with
                        | ValueSome key ->
                            shapeByKey (SymbolKey.Type key)
                            |> ValueOption.map (fun shape -> struct (key, shape))
                        | ValueNone -> ValueNone
                TypeShapeByKey = shapeByKey
                TypeMembersByKey = membersNamed
            }

    let ofKeyedChannels (channels: KeyedChannels) : IExternalSymbolProvider =
        let named = channels.Named

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name = named.TryLookup name
              member _.TryLookupType(name: string) = channels.TypeByName name
              member _.TryLookupUnionCase caseName = named.TryLookupUnionCase caseName
              member _.TryRecordsWithField fieldName = named.TryRecordsWithField fieldName
              member _.AmbientOpenPrefixes = named.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) = channels.TypeShapeByKey key

              member _.TryLookupMembers(key, memberName) =
                  channels.TypeMembersByKey
                      {
                          DeclaringType = key
                          Name = memberName
                      }

              // These channels index members by (declaring type, member NAME), so a key is
              // the exact-identity selection out of that name's overload set. A
              // first-in-declaration-order pick would answer with a SIBLING overload.
              member _.TryLookupMemberByKey(key: MemberKey) =
                  channels.TypeMembersByKey
                      {
                          DeclaringType = SymbolKey.Type key.Decl
                          Name = key.Name
                      }
                  |> ExternalSymbols.memberByKey key

              member _.TryLookupIndexSignature key =
                  named.TryLookupIndexSignature(SymbolKeyOps.qualifiedName key)

              // A `BindingKey`'s rendering `.`-joins its containment chain, which is how a
              // binding is WRITTEN, so it round-trips through the name index. A TYPE's does not:
              // a module-held type's metadata name `+`-nests where the source dots.
              member _.TryLookupByKey key =
                  named.TryLookup(SymbolKeyOps.qualifiedName key)

              member _.IntrinsicTypeMap = named.IntrinsicTypeMap
              member _.IsValueType key = named.IsValueType key
        }

    let ofNamedChannels (channels: NamedChannels) : IExternalSymbolProvider =
        ofKeyedChannels (KeyedChannels.ofNamed channels)

    /// Every channel a miss.
    let nullProvider: IExternalSymbolProvider = ofNamedChannels NamedChannels.empty

    /// Every channel defaults to forwarding `inner`, so a subclass overrides only what it
    /// changes. The two `TryLookupType` overloads are named apart because an override's
    /// argument type is otherwise all that tells them apart.
    [<AbstractClass>]
    type ProviderDecorator(inner: IExternalSymbolProvider) =

        abstract TryLookup: name: string -> ExternalSymbol voption
        default _.TryLookup name = inner.TryLookup name

        abstract TryLookupTypeByName: name: string -> struct (TypeKey * ExternalTypeShape) voption
        default _.TryLookupTypeByName name = inner.TryLookupType name

        abstract TryLookupUnionCase: caseName: string -> ExternalUnionCase voption
        default _.TryLookupUnionCase caseName = inner.TryLookupUnionCase caseName

        abstract TryRecordsWithField: fieldName: string -> EqArray<ExternalRecordCandidate>
        default _.TryRecordsWithField fieldName = inner.TryRecordsWithField fieldName

        abstract AmbientOpenPrefixes: string list
        default _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes

        abstract TryLookupTypeByKey: key: SymbolKey -> ExternalTypeShape voption
        default _.TryLookupTypeByKey key = inner.TryLookupType key

        abstract TryLookupMembers: key: SymbolKey * memberName: string -> EqArray<ExternalMember>
        default _.TryLookupMembers(key, memberName) = inner.TryLookupMembers(key, memberName)

        abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
        default _.TryLookupMemberByKey key = inner.TryLookupMemberByKey key

        abstract TryLookupIndexSignature: key: SymbolKey -> (FrozenType * FrozenType) list
        default _.TryLookupIndexSignature key = inner.TryLookupIndexSignature key

        abstract TryLookupByKey: key: SymbolKey -> ExternalSymbol voption
        default _.TryLookupByKey key = inner.TryLookupByKey key

        abstract IntrinsicTypeMap: IntrinsicTypeMap
        default _.IntrinsicTypeMap = inner.IntrinsicTypeMap

        abstract IsValueType: key: TypeKey -> bool voption
        default _.IsValueType key = inner.IsValueType key

        interface IExternalSymbolProvider

        interface IExternalSymbolResolver with
            member this.TryLookup name = this.TryLookup name
            member this.TryLookupType(name: string) = this.TryLookupTypeByName name
            member this.TryLookupUnionCase caseName = this.TryLookupUnionCase caseName
            member this.TryRecordsWithField fieldName = this.TryRecordsWithField fieldName
            member this.AmbientOpenPrefixes = this.AmbientOpenPrefixes

        interface IExternalSymbolStore with
            member this.TryLookupType(key: SymbolKey) = this.TryLookupTypeByKey key
            member this.TryLookupMembers(key, memberName) = this.TryLookupMembers(key, memberName)
            member this.TryLookupMemberByKey(key: MemberKey) = this.TryLookupMemberByKey key
            member this.TryLookupIndexSignature(key: SymbolKey) = this.TryLookupIndexSignature key
            member this.TryLookupByKey key = this.TryLookupByKey key
            member this.IntrinsicTypeMap = this.IntrinsicTypeMap
            member this.IsValueType key = this.IsValueType key

    /// The composed intrinsic axis of `sources`, EARLIEST source nearest: what `stack`
    /// publishes, exposed for a caller that must seed a later source with it before composing.
    let mergeIntrinsics (sources: IExternalSymbolProvider seq) : IntrinsicTypeMap =
        sources
        |> Seq.collect (fun s -> IntrinsicTypeMap.entries s.IntrinsicTypeMap)
        |> IntrinsicTypeMap.ofSeq

    /// First-hit-wins composition over `sources`, surfacing `ambient` and stamping
    /// `stampHome` onto each resolved entry's `SymbolOrigin.Home`. The origin's NAMESPACE
    /// is never stamped, because a package spans as many as its files declare.
    let stack
        (stampHome: Origin voption)
        (ambient: string list)
        (sources: IExternalSymbolProvider list)
        : IExternalSymbolProvider =
        // Snapshot to an array so the hot lookup is an index loop, not list
        // traversal, on a provider hit from many parallel PassContexts.
        let sources = List.toArray sources

        let intrinsics = mergeIntrinsics sources

        // The array-valued lookups keep their own loop: their empty sentinel is `[||]`.
        let inline firstHit (f: IExternalSymbolProvider -> 'a voption) : 'a voption =
            let mutable result = ValueNone
            let mutable i = 0

            while result.IsNone && i < sources.Length do
                result <- f sources.[i]
                i <- i + 1

            result

        let foldIntrinsicSurface (key: SymbolKey) (hit: ExternalTypeShape) : ExternalTypeShape =
            match hit with
            | ExternalTypeShape.Intrinsic shape ->
                let mutable surface = shape.Class

                for s in sources do
                    match s.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Intrinsic other) ->
                        surface <- IntrinsicClassSurface.merge surface other.Class
                    | _ -> ()

                ExternalTypeShape.Intrinsic { shape with Class = surface }
            | _ -> hit

        let inline home (origin: SymbolOrigin) (h: Origin) = { origin with Home = h }

        let stampSymbol =
            match stampHome with
            | ValueNone -> id
            | ValueSome h -> fun (s: ExternalSymbol) -> { s with Origin = home s.Origin h }

        let stampMember =
            match stampHome with
            | ValueNone -> id
            | ValueSome h -> fun (m: ExternalMember) -> { m with Origin = home m.Origin h }

        // A shape's key is untouched: the home is a PACKAGE fact, the identity is not.
        let stampType (shape: ExternalTypeShape) : ExternalTypeShape =
            match stampHome with
            | ValueNone -> shape
            | ValueSome h ->
                match shape with
                | ExternalTypeShape.Class info ->
                    ExternalTypeShape.Class
                        { info with
                            Origin = home info.Origin h
                        }
                | ExternalTypeShape.Record(arity, fields, o, isValueType) ->
                    ExternalTypeShape.Record(arity, fields, home o h, isValueType)
                | ExternalTypeShape.Union(arity, cases, ifaces, o) ->
                    ExternalTypeShape.Union(arity, cases, ifaces, home o h)
                | ExternalTypeShape.Enum(cases, o) -> ExternalTypeShape.Enum(cases, home o h)
                | ExternalTypeShape.IntrinsicInterface s ->
                    ExternalTypeShape.IntrinsicInterface { s with Origin = home s.Origin h }
                | ExternalTypeShape.Abbrev _
                // An intrinsic carries no `Origin`: its identity is the canon.
                | ExternalTypeShape.Intrinsic _
                | ExternalTypeShape.Unmodelled _ -> shape

        // An extractor records a declaring union with `SymbolOrigin.Empty`, so a case
        // reverse-looked-up off it must be re-homed to agree with its union's shape.
        let stampUnionCase =
            match stampHome with
            | ValueNone -> id
            | ValueSome h -> fun (uc: ExternalUnionCase) -> { uc with Origin = home uc.Origin h }

        // Likewise for the reverse FIELD index: a candidate comes off a `Record` shape
        // recorded with `SymbolOrigin.Empty`.
        let stampRecordCandidate =
            match stampHome with
            | ValueNone -> id
            | ValueSome h -> fun (c: ExternalRecordCandidate) -> { c with Origin = home c.Origin h }

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  firstHit (fun s -> s.TryLookup name) |> ValueOption.map stampSymbol

              member _.TryLookupType(name: string) =
                  firstHit (fun s -> s.TryLookupType name)
                  |> ValueOption.map (fun (struct (key, shape)) ->
                      struct (key, stampType (foldIntrinsicSurface (SymbolKey.Type key) shape))
                  )

              member _.TryLookupUnionCase caseName =
                  firstHit (fun s -> s.TryLookupUnionCase caseName)
                  |> ValueOption.map stampUnionCase

              // UNION, not first-hit-wins: a field name can recur across records in
              // DIFFERENT packages, and unqualified record resolution must intersect over
              // every candidate, so a later source's records add rather than being shadowed.
              member _.TryRecordsWithField fieldName =
                  EqArray.ofSeq
                      [
                          for s in sources do
                              for c in s.TryRecordsWithField fieldName do
                                  stampRecordCandidate c
                      ]

              member _.AmbientOpenPrefixes = ambient
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  firstHit (fun s -> s.TryLookupType key)
                  |> ValueOption.map (foldIntrinsicSurface key >> stampType)

              // A type's members live in one assembly, so a later source never *adds*
              // overloads and the first source that knows the type wins the whole set.
              member _.TryLookupMembers(key, memberName) =
                  let mutable result = EqArray.empty
                  let mutable i = 0

                  while result.IsEmpty && i < sources.Length do
                      result <- sources.[i].TryLookupMembers(key, memberName)
                      i <- i + 1

                  match stampHome with
                  | ValueNone -> result
                  | ValueSome _ -> result |> EqArray.map stampMember

              member _.TryLookupMemberByKey(key: MemberKey) =
                  firstHit (fun s -> s.TryLookupMemberByKey key) |> ValueOption.map stampMember

              // First source with a non-empty index signature wins. The `(key, value)`
              // templates are origin-independent, so nothing is re-stamped.
              member _.TryLookupIndexSignature(key: SymbolKey) =
                  let mutable result = []
                  let mutable i = 0

                  while List.isEmpty result && i < sources.Length do
                      result <- sources.[i].TryLookupIndexSignature key
                      i <- i + 1

                  result

              member _.TryLookupByKey key =
                  firstHit (fun s -> s.TryLookupByKey key) |> ValueOption.map stampSymbol

              member _.IntrinsicTypeMap = intrinsics

              // Per FACT, not per shape: a source with no opinion abstains, so the platform
              // metadata is reached past every contract source above it.
              member _.IsValueType key = firstHit (fun s -> s.IsValueType key)
        }

    /// Each source's `[<AutoOpen>]` / prelude prefixes, in source priority order,
    /// deduplicated keeping the FIRST sighting: packages share prelude prefixes.
    let private collectAmbient (sources: IExternalSymbolProvider seq) : string list =
        [
            for s in sources do
                yield! s.AmbientOpenPrefixes
        ]
        |> List.distinct

    /// `stack` with no origin stamping. List order encodes shadowing among *external*
    /// sources only (a referenced project beats a referenced assembly); project-local
    /// symbols resolve before the provider is consulted at all.
    let composite (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
        match sources with
        | [] -> nullProvider
        | [ single ] -> single
        | _ -> stack ValueNone (collectAmbient sources) sources

    /// Rebuild a provider so every `FrozenType` a VALUE can have (a parameter, a return, a
    /// field) passes through `transform` at that position's ROOT variance. A caller needing
    /// the decision threaded through NESTED positions composes `FrozenType.mapVariant` itself.
    let mapProviderTypes
        (transform: Variance -> FrozenType -> FrozenType)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        let co t = transform Variance.Co t
        let contra t = transform Variance.Contra t
        let inv t = transform Variance.Inv t

        // A member's `Return` is a covariant read; every argument group contravariant.
        let mapMember (m: ExternalMember) : ExternalMember =
            { m with
                Signature =
                    { m.Signature with
                        ArgGroups = m.Signature.ArgGroups |> EqArray.map contra
                        Return = co m.Signature.Return
                    }
            }

        // An interface's type ARGUMENTS are invariant slots; the reference itself holds no
        // value, so `transform` has nothing to say about it.
        let mapInterfaces (ifaces: EqArray<FrozenInterface>) =
            ifaces |> EqArray.map (fun i -> i.MapArgs inv)

        // A union-case field is a covariant value read.
        let mapCase (c: ExternalCaseShape) : ExternalCaseShape =
            { c with
                FrozenFieldTypes = c.FrozenFieldTypes |> EqArray.map co
            }

        let mapShape (shape: ExternalTypeShape) : ExternalTypeShape =
            match shape with
            | ExternalTypeShape.Class info ->
                ExternalTypeShape.Class
                    { info with
                        Members = info.Members |> EqArray.map mapMember
                        FrozenInterfaces = mapInterfaces info.FrozenInterfaces
                        FrozenBaseType = info.FrozenBaseType |> ValueOption.map inv
                    }
            | ExternalTypeShape.Record(arity, fields, origin, isValueType) ->
                // A record field is a covariant value read.
                ExternalTypeShape.Record(
                    arity,
                    fields |> EqArray.map (fun f -> { f with Frozen = co f.Frozen }),
                    origin,
                    isValueType
                )
            | ExternalTypeShape.Union(arity, cases, ifaces, origin) ->
                ExternalTypeShape.Union(arity, cases |> EqArray.map mapCase, mapInterfaces ifaces, origin)
            // A primitive's class surface maps identically to `Class`, interfaces included.
            | ExternalTypeShape.Intrinsic({ Class = ValueSome surface } as s) ->
                ExternalTypeShape.Intrinsic
                    { s with
                        Class =
                            ValueSome
                                { surface with
                                    BaseType = surface.BaseType |> ValueOption.map inv
                                    Interfaces = mapInterfaces surface.Interfaces
                                    Members = surface.Members |> EqArray.map mapMember
                                }
                    }
            // A capability interface's abstract members and inherited interfaces map exactly
            // as a `Class`'s.
            | ExternalTypeShape.IntrinsicInterface s ->
                ExternalTypeShape.IntrinsicInterface
                    { s with
                        Members = s.Members |> EqArray.map mapMember
                        Interfaces = mapInterfaces s.Interfaces
                    }
            // No members or fields to map. An `Abbrev` body inherits its USE SITE's variance,
            // which is unknowable here.
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Enum _
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.Unmodelled _ -> shape

        // An `ExternalRecordCandidate` carries identity + field NAMES only, and value-ness is
        // a layout not a type: neither channel carries a position to map.
        { new ProviderDecorator(inner) with
            override _.TryLookup name =
                inner.TryLookup name
                |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })

            override _.TryLookupTypeByName name =
                inner.TryLookupType name
                |> ValueOption.map (fun (struct (key, shape)) -> struct (key, mapShape shape))

            override _.TryLookupUnionCase caseName =
                inner.TryLookupUnionCase caseName
                |> ValueOption.map (fun uc -> { uc with Case = mapCase uc.Case })

            override _.TryLookupTypeByKey key =
                inner.TryLookupType key |> ValueOption.map mapShape

            override _.TryLookupMembers(key, memberName) =
                inner.TryLookupMembers(key, memberName) |> EqArray.map mapMember

            override _.TryLookupMemberByKey(key: MemberKey) =
                inner.TryLookupMemberByKey key |> ValueOption.map mapMember

            // An index KEY is a contravariant position (the supplied index), the VALUE a
            // covariant read.
            override _.TryLookupIndexSignature(key: SymbolKey) =
                inner.TryLookupIndexSignature key |> List.map (fun (k, v) -> contra k, co v)

            override _.TryLookupByKey key =
                inner.TryLookupByKey key
                |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })
        }
        :> IExternalSymbolProvider

    /// Fold each symbol's / member's published INLINE BODY onto the entry that carries its
    /// identity: the entry comes from `inner`, and its own `Key` is what `bodies` is asked for.
    let withInlineBodies
        (bodies: SymbolKey -> InlineBody voption)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        let stampSymbol (s: ExternalSymbol) : ExternalSymbol =
            { s with
                InlineBody = bodies (SymbolKey.Binding s.Key)
            }

        let stampMember (m: ExternalMember) : ExternalMember =
            { m with
                InlineBody = bodies (SymbolKey.Member m.Key)
            }

        { new ProviderDecorator(inner) with
            override _.TryLookup name =
                inner.TryLookup name |> ValueOption.map stampSymbol

            override _.TryLookupMembers(key, memberName) =
                inner.TryLookupMembers(key, memberName) |> EqArray.map stampMember

            override _.TryLookupMemberByKey(key: MemberKey) =
                inner.TryLookupMemberByKey key |> ValueOption.map stampMember

            override _.TryLookupByKey key =
                inner.TryLookupByKey key |> ValueOption.map stampSymbol
        }
        :> IExternalSymbolProvider

    /// Cache every lookup channel on first hit, MISSES included: the contract is immutable
    /// for a compile, so a `ValueNone` / empty result is as stable as a hit. Apply ONCE, atop a
    /// composed stack, whose fall-through and rewrites would otherwise re-run per call.
    let memoize (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        let symbols = ConcurrentDictionary<string, ExternalSymbol voption>()

        let typesByName =
            ConcurrentDictionary<string, struct (TypeKey * ExternalTypeShape) voption>()

        let typesByKey = ConcurrentDictionary<SymbolKey, ExternalTypeShape voption>()

        let memberSets =
            ConcurrentDictionary<struct (SymbolKey * string), EqArray<ExternalMember>>()

        let membersByKey = ConcurrentDictionary<MemberKey, ExternalMember voption>()

        let indexSigs = ConcurrentDictionary<SymbolKey, (FrozenType * FrozenType) list>()
        let unionCases = ConcurrentDictionary<string, ExternalUnionCase voption>()

        let recordsByField =
            ConcurrentDictionary<string, EqArray<ExternalRecordCandidate>>()

        let symbolsByKey = ConcurrentDictionary<SymbolKey, ExternalSymbol voption>()
        let valueTypes = ConcurrentDictionary<TypeKey, bool voption>()

        { new ProviderDecorator(inner) with
            override _.TryLookup name =
                symbols.GetOrAdd(name, (fun n -> inner.TryLookup n))

            override _.TryLookupTypeByName name =
                typesByName.GetOrAdd(name, (fun n -> inner.TryLookupType n))

            override _.TryLookupUnionCase caseName =
                unionCases.GetOrAdd(caseName, (fun n -> inner.TryLookupUnionCase n))

            override _.TryRecordsWithField fieldName =
                recordsByField.GetOrAdd(fieldName, (fun n -> inner.TryRecordsWithField n))

            override _.TryLookupTypeByKey key =
                typesByKey.GetOrAdd(key, (fun k -> inner.TryLookupType k))

            override _.TryLookupMembers(key, memberName) =
                memberSets.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMembers(k, m)))

            override _.TryLookupMemberByKey(key: MemberKey) =
                membersByKey.GetOrAdd(key, (fun k -> inner.TryLookupMemberByKey k))

            override _.TryLookupIndexSignature(key: SymbolKey) =
                indexSigs.GetOrAdd(key, (fun k -> inner.TryLookupIndexSignature k))

            override _.TryLookupByKey key =
                symbolsByKey.GetOrAdd(key, (fun k -> inner.TryLookupByKey k))

            override _.IsValueType key =
                valueTypes.GetOrAdd(key, (fun k -> inner.IsValueType k))
        }
        :> IExternalSymbolProvider

    /// The two body-bearing key kinds route to different ENTRY types: a `Binding` rides
    /// `ExternalSymbol`, a `(# … #)`-bodied `Member` rides `ExternalMember`.
    let tryInlineBody (p: IExternalSymbolStore) (key: SymbolKey) : InlineBody voption =
        match key with
        | SymbolKey.Member m -> p.TryLookupMemberByKey m |> ValueOption.bind (fun em -> em.InlineBody)
        | SymbolKey.Binding _ -> p.TryLookupByKey key |> ValueOption.bind (fun s -> s.InlineBody)
        | SymbolKey.Type _ -> ValueNone
