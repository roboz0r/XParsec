namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

/// Addresses a member by NAME, so it reaches the whole overload set; `MemberKey` addresses
/// one overload by identity.
type KeyedMemberName =
    { DeclaringType: TypeKey; Name: string }

/// `KeyedMemberName` with the declaring type RENDERED: the address a by-name member index
/// is keyed on.
type ExternalMemberName =
    {
        /// The declaring type's `typeMetaName` rendering.
        DeclaringType: string
        Name: string
    }

module ExternalMemberName =

    let ofKeyed (key: KeyedMemberName) : ExternalMemberName =
        {
            DeclaringType = SymbolKeyOps.typeMetaName key.DeclaringType
            Name = key.Name
        }

module ExternalSymbolProviders =


    /// The published-surface contract, indexed by identity throughout: a type is reachable
    /// whatever its container renders as.
    type KeyIndexedChannels =
        {
            ShapesByKey: IReadOnlyDictionary<TypeKey, ExternalTypeShape>
            /// A type's FULL member list, in DECLARATION order, because the by-name overload
            /// scan and the by-key selection both depend on that order.
            MembersByKey: IReadOnlyDictionary<TypeKey, EqArray<ExternalMember>>
            /// A type's `{ [k: K]: V }` signatures, in declaration order.
            IndexSignaturesByKey: IReadOnlyDictionary<TypeKey, (FrozenType * FrozenType) list>
            /// Every published value, one entry per identity.
            SymbolsByKey: IReadOnlyDictionary<BindingKey, ExternalSymbol>
            TryRecordsWithField: string -> EqArray<ExternalRecordCandidate>
            Platform: IPlatformFacts voption
            ImplicitOpens: ImplicitOpen list
            IntrinsicTypeMap: IntrinsicTypeMap
            Scope: IScopeContents
        }

    module KeyIndexedChannels =

        let empty: KeyIndexedChannels =
            {
                Scope = ScopeContents.empty
                ShapesByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                MembersByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                IndexSignaturesByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                SymbolsByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                TryRecordsWithField = fun _ -> EqArray.empty
                Platform = ValueNone
                ImplicitOpens = []
                IntrinsicTypeMap = IntrinsicTypeMap.empty
            }

    let ofKeyIndexedChannels (channels: KeyIndexedChannels) : IExternalSymbolProvider =
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

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.Scope = channels.Scope
              member _.TryRecordsWithField fieldName = channels.TryRecordsWithField fieldName
              member _.ImplicitOpens = channels.ImplicitOpens
          interface IExternalSymbolStore with
              member _.TryLookupType(key: TypeKey) =
                  match channels.ShapesByKey.TryGetValue key with
                  | true, shape -> ValueSome shape
                  | _ -> ValueNone

              member _.TryLookupMembers(key, memberName) =
                  membersNamed
                      {
                          DeclaringType = key
                          Name = memberName
                      }

              // These channels index members by (declaring type, member NAME), so a key is
              // the exact-identity selection out of that name's overload set. A
              // first-in-declaration-order pick would return a SIBLING overload.
              member _.TryLookupMemberByKey(key: MemberKey) =
                  membersNamed
                      {
                          DeclaringType = key.Decl
                          Name = key.Name
                      }
                  |> ExternalSymbols.memberByKey key

              member _.TryLookupIndexSignature key =
                  match channels.IndexSignaturesByKey.TryGetValue key with
                  | true, pairs -> pairs
                  | _ -> []

              member _.TryLookupByKey key =
                  match channels.SymbolsByKey.TryGetValue key with
                  | true, sym -> ValueSome sym
                  | _ -> ValueNone

              member _.IntrinsicTypeMap = channels.IntrinsicTypeMap
              member _.Platform = channels.Platform
        }

    /// Every channel a miss.
    let nullProvider: IExternalSymbolProvider =
        ofKeyIndexedChannels KeyIndexedChannels.empty

    /// Every channel defaults to forwarding `inner`.
    [<AbstractClass>]
    type ProviderDecorator(inner: IExternalSymbolProvider) =

        abstract Scope: IScopeContents
        default _.Scope = inner.Scope

        abstract TryRecordsWithField: fieldName: string -> EqArray<ExternalRecordCandidate>
        default _.TryRecordsWithField fieldName = inner.TryRecordsWithField fieldName

        abstract ImplicitOpens: ImplicitOpen list
        default _.ImplicitOpens = inner.ImplicitOpens

        abstract TryLookupType: key: TypeKey -> ExternalTypeShape voption
        default _.TryLookupType key = inner.TryLookupType key

        abstract TryLookupMembers: key: TypeKey * memberName: string -> EqArray<ExternalMember>
        default _.TryLookupMembers(key, memberName) = inner.TryLookupMembers(key, memberName)

        abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
        default _.TryLookupMemberByKey key = inner.TryLookupMemberByKey key

        abstract TryLookupIndexSignature: key: TypeKey -> (FrozenType * FrozenType) list
        default _.TryLookupIndexSignature key = inner.TryLookupIndexSignature key

        abstract TryLookupByKey: key: BindingKey -> ExternalSymbol voption
        default _.TryLookupByKey key = inner.TryLookupByKey key

        abstract IntrinsicTypeMap: IntrinsicTypeMap
        default _.IntrinsicTypeMap = inner.IntrinsicTypeMap

        abstract Platform: IPlatformFacts voption
        default _.Platform = inner.Platform

        interface IExternalSymbolProvider

        interface IExternalSymbolResolver with
            member this.Scope = this.Scope
            member this.TryRecordsWithField fieldName = this.TryRecordsWithField fieldName
            member this.ImplicitOpens = this.ImplicitOpens

        interface IExternalSymbolStore with
            member this.TryLookupType(key: TypeKey) = this.TryLookupType key
            member this.TryLookupMembers(key, memberName) = this.TryLookupMembers(key, memberName)
            member this.TryLookupMemberByKey(key: MemberKey) = this.TryLookupMemberByKey key
            member this.TryLookupIndexSignature(key: TypeKey) = this.TryLookupIndexSignature key
            member this.TryLookupByKey key = this.TryLookupByKey key
            member this.IntrinsicTypeMap = this.IntrinsicTypeMap
            member this.Platform = this.Platform

    /// The composed intrinsic axis of `providers`, EARLIEST source nearest: what `stack`
    /// publishes, exposed for a caller that must seed a later source with it before composing.
    let mergeIntrinsics (providers: IExternalSymbolProvider seq) : IntrinsicTypeMap =
        providers
        |> Seq.collect (fun s -> IntrinsicTypeMap.entries s.IntrinsicTypeMap)
        |> IntrinsicTypeMap.ofSeq

    /// First-hit-wins composition over `providers`, surfacing `ambient` and stamping
    /// `stampHome` onto each resolved entry's `SymbolOrigin.Home`. The origin's NAMESPACE
    /// is never stamped, because a package spans as many as its files declare.
    let stack
        (stampHome: SymbolHome voption)
        (ambient: ImplicitOpen list)
        (providers: IExternalSymbolProvider list)
        : IExternalSymbolProvider =
        // Snapshot to an array so the hot lookup is an index loop, not list
        // traversal, on a provider hit from many parallel PassContexts.
        let providers = List.toArray providers

        let intrinsics = mergeIntrinsics providers

        // The array-valued lookups keep their own loop: their empty sentinel is `[||]`.
        let inline firstHit (f: IExternalSymbolProvider -> 'a voption) : 'a voption =
            let mutable result = ValueNone
            let mutable i = 0

            while result.IsNone && i < providers.Length do
                result <- f providers.[i]
                i <- i + 1

            result

        let foldIntrinsicSurface (key: TypeKey) (hit: ExternalTypeShape) : ExternalTypeShape =
            match hit with
            | ExternalTypeShape.Intrinsic shape ->
                let mutable surface = shape.Class

                for s in providers do
                    match s.TryLookupType key with
                    | ValueSome(ExternalTypeShape.Intrinsic other) ->
                        surface <- IntrinsicClassSurface.merge surface other.Class
                    | _ -> ()

                ExternalTypeShape.Intrinsic { shape with Class = surface }
            | _ -> hit

        let inline home (origin: SymbolOrigin) (h: SymbolHome) = { origin with Home = h }

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
                | ExternalTypeShape.Record r -> ExternalTypeShape.Record { r with Origin = home r.Origin h }
                | ExternalTypeShape.Union u -> ExternalTypeShape.Union { u with Origin = home u.Origin h }
                | ExternalTypeShape.Enum(cases, o) -> ExternalTypeShape.Enum(cases, home o h)
                | ExternalTypeShape.IntrinsicInterface s ->
                    ExternalTypeShape.IntrinsicInterface { s with Origin = home s.Origin h }
                | ExternalTypeShape.Abbrev _
                // An intrinsic carries no `SymbolOrigin`: its identity is the canon.
                | ExternalTypeShape.Intrinsic _
                | ExternalTypeShape.Unmodelled _ -> shape

        let stampHit (key: TypeKey) (shape: ExternalTypeShape) : ExternalTypeShape =
            stampType (foldIntrinsicSurface key shape)

        // The scope exposes the same values and types as the key channels, so it carries
        // the same home stamp and the same intrinsic fold. A union case takes its origin from
        // its declaring union's shape.
        let scope =
            ScopeContents.composite [ for p in providers -> p.Scope ]
            |> ScopeContents.decorate stampSymbol id stampHit

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.Scope = scope

              // UNION, not first-hit-wins: a field name can recur across records in
              // DIFFERENT packages, and unqualified record resolution must intersect over
              // every candidate, so a later source's records add rather than being shadowed.
              member _.TryRecordsWithField fieldName =
                  EqArray.ofSeq
                      [
                          for s in providers do
                              yield! s.TryRecordsWithField fieldName
                      ]

              member _.ImplicitOpens = ambient
          interface IExternalSymbolStore with
              member _.TryLookupType(key: TypeKey) =
                  firstHit (fun s -> s.TryLookupType key) |> ValueOption.map (stampHit key)

              // A type's members live in one assembly, so a later source never *adds*
              // overloads and the first source that knows the type wins the whole set.
              member _.TryLookupMembers(key, memberName) =
                  let mutable result = EqArray.empty
                  let mutable i = 0

                  while result.IsEmpty && i < providers.Length do
                      result <- providers.[i].TryLookupMembers(key, memberName)
                      i <- i + 1

                  match stampHome with
                  | ValueNone -> result
                  | ValueSome _ -> result |> EqArray.map stampMember

              member _.TryLookupMemberByKey(key: MemberKey) =
                  firstHit (fun s -> s.TryLookupMemberByKey key) |> ValueOption.map stampMember

              // First source with a non-empty index signature wins. The `(key, value)`
              // templates are origin-independent, so nothing is re-stamped.
              member _.TryLookupIndexSignature(key: TypeKey) =
                  let mutable result = []
                  let mutable i = 0

                  while List.isEmpty result && i < providers.Length do
                      result <- providers.[i].TryLookupIndexSignature key
                      i <- i + 1

                  result

              member _.TryLookupByKey key =
                  firstHit (fun s -> s.TryLookupByKey key) |> ValueOption.map stampSymbol

              member _.IntrinsicTypeMap = intrinsics

              // Every contract source above it abstains as a WHOLE, so this reaches the one
              // source that is the platform metadata, wherever in the stack it sits.
              member _.Platform = firstHit (fun s -> s.Platform)
        }

    /// Each source's implicit opens, in source priority order, deduplicated keeping the
    /// FIRST sighting: packages share the prelude their common dependency declares.
    let collectImplicitOpens (providers: IExternalSymbolProvider seq) : ImplicitOpen list =
        [
            for s in providers do
                yield! s.ImplicitOpens
        ]
        |> List.distinct

    /// `stack` with no origin stamping. List order encodes shadowing among *external*
    /// sources only (a referenced project beats a referenced assembly); project-local
    /// symbols resolve before the provider is consulted at all.
    let composite (providers: IExternalSymbolProvider list) : IExternalSymbolProvider =
        match providers with
        | [] -> nullProvider
        | [ single ] -> single
        | _ -> stack ValueNone (collectImplicitOpens providers) providers

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

        // An interface's type ARGUMENTS are invariant slots; the reference itself holds no value,
        // so `transform` has nothing to say about it. A base type maps the same way, and for a
        // second reason: it widens `number` at `Inv` to `int|float|…`, and no class inherits a union.
        let mapNominal (n: FrozenNominal) = NominalG.map inv n

        let mapInterfaces (ifaces: EqArray<FrozenNominal>) = ifaces |> EqArray.map mapNominal

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
                        FrozenBaseType = info.FrozenBaseType |> ValueOption.map mapNominal
                    }
            | ExternalTypeShape.Record r ->
                // A record field is a covariant value read.
                ExternalTypeShape.Record
                    { r with
                        Fields = r.Fields |> EqArray.map (fun f -> { f with Frozen = co f.Frozen })
                    }
            | ExternalTypeShape.Union u ->
                ExternalTypeShape.Union
                    { u with
                        Cases = u.Cases |> EqArray.map mapCase
                        Interfaces = mapInterfaces u.Interfaces
                    }
            // A primitive's class surface maps identically to `Class`, interfaces included.
            | ExternalTypeShape.Intrinsic({ Class = ValueSome surface } as s) ->
                ExternalTypeShape.Intrinsic
                    { s with
                        Class =
                            ValueSome
                                { surface with
                                    BaseType = surface.BaseType |> ValueOption.map mapNominal
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

        let mapSymbol (s: ExternalSymbol) = { s with Scheme = co s.Scheme }

        // The scope exposes the same values, cases and types as the key channels, so it
        // carries the same transform.
        let mappedScope =
            inner.Scope
            |> ScopeContents.decorate mapSymbol (fun uc -> { uc with Case = mapCase uc.Case }) (fun _ -> mapShape)

        // An `ExternalRecordCandidate` carries identity + field NAMES only, and value-ness is
        // a layout not a type: neither channel carries a position to map.
        { new ProviderDecorator(inner) with
            override _.Scope = mappedScope

            override _.TryLookupType key =
                inner.TryLookupType key |> ValueOption.map mapShape

            override _.TryLookupMembers(key, memberName) =
                inner.TryLookupMembers(key, memberName) |> EqArray.map mapMember

            override _.TryLookupMemberByKey(key: MemberKey) =
                inner.TryLookupMemberByKey key |> ValueOption.map mapMember

            // An index KEY is a contravariant position (the supplied index), the VALUE a
            // covariant read.
            override _.TryLookupIndexSignature(key: TypeKey) =
                inner.TryLookupIndexSignature key |> List.map (fun (k, v) -> contra k, co v)

            override _.TryLookupByKey key =
                inner.TryLookupByKey key |> ValueOption.map mapSymbol
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

        let scope = inner.Scope |> ScopeContents.mapValues stampSymbol

        { new ProviderDecorator(inner) with
            override _.Scope = scope

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
        let typesByKey = ConcurrentDictionary<TypeKey, ExternalTypeShape voption>()

        let memberSets =
            ConcurrentDictionary<struct (TypeKey * string), EqArray<ExternalMember>>()

        let membersByKey = ConcurrentDictionary<MemberKey, ExternalMember voption>()

        let indexSigs = ConcurrentDictionary<TypeKey, (FrozenType * FrozenType) list>()

        let recordsByField =
            ConcurrentDictionary<string, EqArray<ExternalRecordCandidate>>()

        let symbolsByKey = ConcurrentDictionary<BindingKey, ExternalSymbol voption>()
        let valueTypes = ConcurrentDictionary<TypeKey, bool voption>()

        // Only `IsValueType` is cached: it reaches a metadata name lookup, where `TupleType`
        // is read from the target's own fixed family and is cheaper than the dictionary probe.
        let platform =
            lazy
                (inner.Platform
                 |> ValueOption.map (fun facts ->
                     { new IPlatformFacts with
                         member _.IsValueType key =
                             valueTypes.GetOrAdd(key, (fun k -> facts.IsValueType k))

                         member _.TupleType arity = facts.TupleType arity
                     }
                 ))

        let scope = ScopeContents.memoize inner.Scope

        { new ProviderDecorator(inner) with
            override _.Scope = scope

            override _.TryRecordsWithField fieldName =
                recordsByField.GetOrAdd(fieldName, (fun n -> inner.TryRecordsWithField n))

            override _.TryLookupType key =
                typesByKey.GetOrAdd(key, (fun k -> inner.TryLookupType k))

            override _.TryLookupMembers(key, memberName) =
                memberSets.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMembers(k, m)))

            override _.TryLookupMemberByKey(key: MemberKey) =
                membersByKey.GetOrAdd(key, (fun k -> inner.TryLookupMemberByKey k))

            override _.TryLookupIndexSignature(key: TypeKey) =
                indexSigs.GetOrAdd(key, (fun k -> inner.TryLookupIndexSignature k))

            override _.TryLookupByKey key =
                symbolsByKey.GetOrAdd(key, (fun k -> inner.TryLookupByKey k))

            override _.Platform = platform.Value
        }
        :> IExternalSymbolProvider

    /// The two body-bearing key kinds route to different ENTRY types: a `Binding` to
    /// `ExternalSymbol`, a `(# … #)`-bodied `Member` to `ExternalMember`.
    let tryInlineBody (p: IExternalSymbolStore) (key: SymbolKey) : InlineBody voption =
        match key with
        | SymbolKey.Member m -> p.TryLookupMemberByKey m |> ValueOption.bind (fun em -> em.InlineBody)
        | SymbolKey.Binding b -> p.TryLookupByKey b |> ValueOption.bind (fun s -> s.InlineBody)
        | SymbolKey.Type _ -> ValueNone
