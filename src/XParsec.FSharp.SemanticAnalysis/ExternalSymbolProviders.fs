namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

module ExternalSymbolProviders =


    /// A leaf whose types' identities are not a field: a name IS the identity, so a type
    /// key is minted from the name and a key lookup is the rendered name lookup. A leaf
    /// holding `TypeContainer.InModule` types supplies a `KeyIndexedLeaf` instead.
    type NamedLeaf =
        {
            TryLookup: string -> ExternalSymbol voption
            TryLookupType: string -> ExternalTypeShape voption
            TryLookupUnionCase: string -> ExternalUnionCase voption
            TryRecordsWithField: string -> ExternalRecordCandidate[]
            AmbientOpenPrefixes: string list
            /// `(declaring type's qualified compiled name, member name)`.
            TryLookupMember: string * string -> ExternalMember voption
            TryLookupMembers: string * string -> ExternalMember[]
            TryLookupIndexSignature: string -> (FrozenType * FrozenType) list
            IntrinsicReverseCanon: Map<string, SymbolKey list>
            IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>
        }

    module NamedLeaf =

        /// Every channel misses — override just what the leaf models.
        let empty: NamedLeaf =
            {
                TryLookup = fun _ -> ValueNone
                TryLookupType = fun _ -> ValueNone
                TryLookupUnionCase = fun _ -> ValueNone
                TryRecordsWithField = fun _ -> [||]
                AmbientOpenPrefixes = []
                TryLookupMember = fun _ -> ValueNone
                TryLookupMembers = fun _ -> [||]
                TryLookupIndexSignature = fun _ -> []
                IntrinsicReverseCanon = Map.empty
                IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
            }

    /// A leaf that HOLDS its types' identities. Needed when a type is
    /// `TypeContainer.InModule`, whose rendering is not what the source writes.
    type KeyIndexedLeaf =
        {
            ShapesByKey: IReadOnlyDictionary<SymbolKey, ExternalTypeShape>
            /// A type's FULL member list, in DECLARATION order — the order the by-name
            /// overload scan and the by-key selection both depend on.
            MembersByKey: IReadOnlyDictionary<SymbolKey, ResizeArray<ExternalMember>>
            /// Written type name -> registered identity.
            ResolveTypeName: string -> TypeKey voption
            TryLookup: string -> ExternalSymbol voption
            TryLookupUnionCase: string -> ExternalUnionCase voption
            TryRecordsWithField: string -> ExternalRecordCandidate[]
            AmbientOpenPrefixes: string list
            IntrinsicReverseCanon: Map<string, SymbolKey list>
            IntrinsicForwardRepr: IReadOnlyDictionary<SymbolKey, string>
        }

    module KeyIndexedLeaf =

        let empty: KeyIndexedLeaf =
            {
                ShapesByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                MembersByKey = Dictionary() :> IReadOnlyDictionary<_, _>
                ResolveTypeName = fun _ -> ValueNone
                TryLookup = fun _ -> ValueNone
                TryLookupUnionCase = fun _ -> ValueNone
                TryRecordsWithField = fun _ -> [||]
                AmbientOpenPrefixes = []
                IntrinsicReverseCanon = Map.empty
                IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
            }

    /// A leaf's type channels answered BY KEY, whichever way the leaf came by them:
    /// `ofNamed` renders the key onto a name index, `ofKeyIndexes` reads a real one.
    type KeyedLeaf =
        {
            Named: NamedLeaf
            /// Identity + shape from one read. Derived by both builders, never supplied.
            TypeByName: string -> struct (TypeKey * ExternalTypeShape) voption
            TypeShapeByKey: SymbolKey -> ExternalTypeShape voption
            TypeMemberByKey: SymbolKey * string -> ExternalMember voption
            TypeMembersByKey: SymbolKey * string -> ExternalMember[]
        }

    module KeyedLeaf =

        let ofNamed (leaf: NamedLeaf) : KeyedLeaf =
            {
                Named = leaf
                TypeByName =
                    fun name ->
                        leaf.TryLookupType name
                        |> ValueOption.map (ExternalSymbols.nameKeyedTypeHit name)
                TypeShapeByKey = fun key -> leaf.TryLookupType(SymbolKeyOps.qualifiedName key)
                TypeMemberByKey = fun (key, m) -> leaf.TryLookupMember(SymbolKeyOps.qualifiedName key, m)
                TypeMembersByKey = fun (key, m) -> leaf.TryLookupMembers(SymbolKeyOps.qualifiedName key, m)
            }

        let ofKeyIndexes (leaf: KeyIndexedLeaf) : KeyedLeaf =
            let shapeByKey (key: SymbolKey) : ExternalTypeShape voption =
                match leaf.ShapesByKey.TryGetValue key with
                | true, shape -> ValueSome shape
                | _ -> ValueNone

            let membersNamed (key: SymbolKey) (memberName: string) : ExternalMember[] =
                match leaf.MembersByKey.TryGetValue key with
                | true, ms ->
                    [|
                        for m in ms do
                            if m.Name = memberName then
                                m
                    |]
                | _ -> [||]

            let firstMemberNamed (key: SymbolKey) (memberName: string) : ExternalMember voption =
                match leaf.MembersByKey.TryGetValue key with
                | true, ms ->
                    let mutable found = ValueNone
                    let mutable i = 0

                    while found.IsNone && i < ms.Count do
                        if ms.[i].Name = memberName then
                            found <- ValueSome ms.[i]

                        i <- i + 1

                    found
                | _ -> ValueNone

            {
                Named =
                    { NamedLeaf.empty with
                        TryLookup = leaf.TryLookup
                        TryLookupUnionCase = leaf.TryLookupUnionCase
                        TryRecordsWithField = leaf.TryRecordsWithField
                        AmbientOpenPrefixes = leaf.AmbientOpenPrefixes
                        IntrinsicReverseCanon = leaf.IntrinsicReverseCanon
                        IntrinsicForwardRepr = leaf.IntrinsicForwardRepr
                    }
                TypeByName =
                    fun name ->
                        match leaf.ResolveTypeName name with
                        | ValueSome key ->
                            shapeByKey (SymbolKey.Type key)
                            |> ValueOption.map (fun shape -> struct (key, shape))
                        | ValueNone -> ValueNone
                TypeShapeByKey = shapeByKey
                TypeMemberByKey = fun (key, memberName) -> firstMemberNamed key memberName
                TypeMembersByKey = fun (key, memberName) -> membersNamed key memberName
            }

    let ofKeyedLeaf (leaf: KeyedLeaf) : IExternalSymbolProvider =
        let named = leaf.Named

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name = named.TryLookup name
              member _.TryLookupType(name: string) = leaf.TypeByName name
              member _.TryLookupUnionCase caseName = named.TryLookupUnionCase caseName
              member _.TryRecordsWithField fieldName = named.TryRecordsWithField fieldName
              member _.AmbientOpenPrefixes = named.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) = leaf.TypeShapeByKey key

              member _.TryLookupMember(key, memberName) = leaf.TypeMemberByKey(key, memberName)

              member _.TryLookupMembers(key, memberName) = leaf.TypeMembersByKey(key, memberName)

              // A leaf indexes members by (declaring type, member NAME), so a key is the
              // exact-identity selection out of that name's overload set — a
              // first-in-declaration-order pick would answer with a SIBLING overload.
              member _.TryLookupMemberByKey(key: MemberKey) =
                  leaf.TypeMembersByKey(SymbolKey.Type key.Decl, key.Name)
                  |> ExternalSymbols.memberByKey key

              member _.TryLookupIndexSignature key =
                  named.TryLookupIndexSignature(SymbolKeyOps.qualifiedName key)

              // A `BindingKey`'s rendering `.`-joins its containment chain — how a binding
              // is WRITTEN — so it round-trips through the name index. A TYPE's does not:
              // a module-held type's metadata name `+`-nests where the source dots.
              member _.TryLookupByKey key =
                  named.TryLookup(SymbolKeyOps.qualifiedName key)

              member _.IntrinsicReverseCanon = named.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = named.IntrinsicForwardRepr
        }

    let ofNamedLeaf (leaf: NamedLeaf) : IExternalSymbolProvider = ofKeyedLeaf (KeyedLeaf.ofNamed leaf)

    /// Every channel a miss.
    let nullProvider: IExternalSymbolProvider = ofNamedLeaf NamedLeaf.empty

    /// Merge sources' reverse `{ platform-repr -> [canon] }` maps by UNIONING the canon
    /// lists per platform key (deduped). `Array.rev` folds the earliest source LAST so its
    /// canons lead each list.
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

    /// Merge sources' forward `{ canon -> platform-repr }` maps, first-source-wins:
    /// `Array.rev` folds the earliest source LAST so its entries overwrite later ones.
    /// `SymbolKey` is equatable-but-not-comparable, hence a `Dictionary`, not a `Map`.
    let mergeForwardRepr (sources: IExternalSymbolProvider seq) : IReadOnlyDictionary<SymbolKey, string> =
        let arr = Seq.toArray sources
        let d = Dictionary<SymbolKey, string>()

        for s in Array.rev arr do
            for kv in s.IntrinsicForwardRepr do
                d.[kv.Key] <- kv.Value

        d :> IReadOnlyDictionary<_, _>

    /// First-hit-wins composition over `sources`, surfacing `ambient` and stamping
    /// `stampHome` onto each resolved entry's `SymbolOrigin.Home`. The origin's NAMESPACE
    /// is never stamped — a package spans as many as its files declare.
    let stack
        (stampHome: Origin voption)
        (ambient: string list)
        (sources: IExternalSymbolProvider list)
        : IExternalSymbolProvider =
        // Snapshot to an array so the hot lookup is an index loop, not list
        // traversal, on a provider hit from many parallel PassContexts.
        let sources = List.toArray sources

        let reverseCanon = mergeReverseCanon sources
        let forwardRepr = mergeForwardRepr sources

        // The array-valued lookups keep their own loop: their empty sentinel is `[||]`.
        let inline firstHit (f: IExternalSymbolProvider -> 'a voption) : 'a voption =
            let mutable result = ValueNone
            let mutable i = 0

            while result.IsNone && i < sources.Length do
                result <- f sources.[i]
                i <- i + 1

            result

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
                | ExternalTypeShape.Record(arity, fields, o) -> ExternalTypeShape.Record(arity, fields, home o h)
                | ExternalTypeShape.Union(arity, cases, ifaces, o) ->
                    ExternalTypeShape.Union(arity, cases, ifaces, home o h)
                | ExternalTypeShape.Enum(cases, o) -> ExternalTypeShape.Enum(cases, home o h)
                | ExternalTypeShape.IntrinsicInterface s ->
                    ExternalTypeShape.IntrinsicInterface { s with Origin = home s.Origin h }
                | ExternalTypeShape.Abbrev _
                // An intrinsic carries no `Origin`: its identity is the canon.
                | ExternalTypeShape.Intrinsic _
                | ExternalTypeShape.Opaque _ -> shape

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
                  |> ValueOption.map (fun (struct (key, shape)) -> struct (key, stampType shape))

              member _.TryLookupUnionCase caseName =
                  firstHit (fun s -> s.TryLookupUnionCase caseName)
                  |> ValueOption.map stampUnionCase

              // UNION, not first-hit-wins: a field name can recur across records in
              // DIFFERENT packages, and unqualified record resolution must intersect over
              // every candidate, so a later source's records add rather than being shadowed.
              member _.TryRecordsWithField fieldName =
                  [|
                      for s in sources do
                          for c in s.TryRecordsWithField fieldName do
                              stampRecordCandidate c
                  |]

              member _.AmbientOpenPrefixes = ambient
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  firstHit (fun s -> s.TryLookupType key) |> ValueOption.map stampType

              member _.TryLookupMember(key, memberName) =
                  firstHit (fun s -> s.TryLookupMember(key, memberName))
                  |> ValueOption.map stampMember

              // First source that knows the type wins the whole overload set — a type's
              // members live in one assembly, so a later source never *adds* overloads.
              member _.TryLookupMembers(key, memberName) =
                  let mutable result = [||]
                  let mutable i = 0

                  while Array.isEmpty result && i < sources.Length do
                      result <- sources.[i].TryLookupMembers(key, memberName)
                      i <- i + 1

                  match stampHome with
                  | ValueNone -> result
                  | ValueSome _ -> result |> Array.map stampMember

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

              member _.IntrinsicReverseCanon = reverseCanon
              member _.IntrinsicForwardRepr = forwardRepr
        }

    /// Each source's `[<AutoOpen>]` / prelude prefixes, in source priority order,
    /// deduplicated keeping the FIRST sighting: packages share a prelude tail.
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

    /// Rebuild a provider so every VALUE-FLOW `FrozenType` surface it serves is passed
    /// through `transform` at that surface's ROOT variance; a caller that must thread the
    /// decision through NESTED positions composes `FrozenType.mapVariant` itself.
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

        // A union-case field is a covariant value read.
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
            // A heritable primitive's class surface maps identically to `Class`; a scalar
            // intrinsic has no value-flow surface at all.
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
            // A capability interface's abstract members map exactly as a `Class`'s.
            | ExternalTypeShape.IntrinsicInterface s ->
                ExternalTypeShape.IntrinsicInterface
                    { s with
                        Members = s.Members |> Array.map mapMember
                    }
            // No value-flow surface. An `Abbrev` body inherits its USE SITE's variance,
            // which is unknowable here.
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Enum _
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.Opaque _ -> shape

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  inner.TryLookup name
                  |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })

              member _.TryLookupType(name: string) =
                  inner.TryLookupType name
                  |> ValueOption.map (fun (struct (key, shape)) -> struct (key, mapShape shape))

              member _.TryLookupUnionCase caseName =
                  inner.TryLookupUnionCase caseName
                  |> ValueOption.map (fun uc -> { uc with Case = mapCase uc.Case })

              // An `ExternalRecordCandidate` carries identity + field NAMES only; the field
              // types ride the by-key shape path.
              member _.TryRecordsWithField fieldName = inner.TryRecordsWithField fieldName

              member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  inner.TryLookupType key |> ValueOption.map mapShape

              member _.TryLookupMember(key, memberName) =
                  inner.TryLookupMember(key, memberName) |> ValueOption.map mapMember

              member _.TryLookupMembers(key, memberName) =
                  inner.TryLookupMembers(key, memberName) |> Array.map mapMember

              member _.TryLookupMemberByKey(key: MemberKey) =
                  inner.TryLookupMemberByKey key |> ValueOption.map mapMember

              // An index KEY is a contravariant position (the supplied index), the VALUE a
              // covariant read.
              member _.TryLookupIndexSignature(key: SymbolKey) =
                  inner.TryLookupIndexSignature key |> List.map (fun (k, v) -> contra k, co v)

              member _.TryLookupByKey key =
                  inner.TryLookupByKey key
                  |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })

              member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

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

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  inner.TryLookup name |> ValueOption.map stampSymbol

              member _.TryLookupType(name: string) = inner.TryLookupType name
              member _.TryLookupUnionCase caseName = inner.TryLookupUnionCase caseName
              member _.TryRecordsWithField fieldName = inner.TryRecordsWithField fieldName
              member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) = inner.TryLookupType key

              member _.TryLookupMember(key, memberName) =
                  inner.TryLookupMember(key, memberName) |> ValueOption.map stampMember

              member _.TryLookupMembers(key, memberName) =
                  inner.TryLookupMembers(key, memberName) |> Array.map stampMember

              member _.TryLookupMemberByKey(key: MemberKey) =
                  inner.TryLookupMemberByKey key |> ValueOption.map stampMember

              member _.TryLookupIndexSignature(key: SymbolKey) = inner.TryLookupIndexSignature key

              member _.TryLookupByKey key =
                  inner.TryLookupByKey key |> ValueOption.map stampSymbol

              member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

    /// Cache every lookup channel on first hit, MISSES included: the contract is immutable
    /// for a compile, so a `ValueNone` / `[||]` is as stable as a hit. Apply ONCE, atop a
    /// composed stack, whose fall-through and rewrites would otherwise re-run per call.
    let memoize (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        let symbols = ConcurrentDictionary<string, ExternalSymbol voption>()

        let typesByName =
            ConcurrentDictionary<string, struct (TypeKey * ExternalTypeShape) voption>()

        let typesByKey = ConcurrentDictionary<SymbolKey, ExternalTypeShape voption>()

        let members =
            ConcurrentDictionary<struct (SymbolKey * string), ExternalMember voption>()

        let memberSets =
            ConcurrentDictionary<struct (SymbolKey * string), ExternalMember[]>()

        let membersByKey = ConcurrentDictionary<MemberKey, ExternalMember voption>()

        let indexSigs = ConcurrentDictionary<SymbolKey, (FrozenType * FrozenType) list>()
        let unionCases = ConcurrentDictionary<string, ExternalUnionCase voption>()
        let recordsByField = ConcurrentDictionary<string, ExternalRecordCandidate[]>()
        let symbolsByKey = ConcurrentDictionary<SymbolKey, ExternalSymbol voption>()

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  symbols.GetOrAdd(name, (fun n -> inner.TryLookup n))

              member _.TryLookupType(name: string) =
                  typesByName.GetOrAdd(name, (fun n -> inner.TryLookupType n))

              member _.TryLookupUnionCase caseName =
                  unionCases.GetOrAdd(caseName, (fun n -> inner.TryLookupUnionCase n))

              member _.TryRecordsWithField fieldName =
                  recordsByField.GetOrAdd(fieldName, (fun n -> inner.TryRecordsWithField n))

              member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  typesByKey.GetOrAdd(key, (fun k -> inner.TryLookupType k))

              member _.TryLookupMember(key, memberName) =
                  members.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMember(k, m)))

              member _.TryLookupMembers(key, memberName) =
                  memberSets.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMembers(k, m)))

              member _.TryLookupMemberByKey(key: MemberKey) =
                  membersByKey.GetOrAdd(key, (fun k -> inner.TryLookupMemberByKey k))

              member _.TryLookupIndexSignature(key: SymbolKey) =
                  indexSigs.GetOrAdd(key, (fun k -> inner.TryLookupIndexSignature k))

              member _.TryLookupByKey key =
                  symbolsByKey.GetOrAdd(key, (fun k -> inner.TryLookupByKey k))

              member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

    /// The two body-bearing key kinds route to different ENTRY types: a `Binding` rides
    /// `ExternalSymbol`, a `(# … #)`-bodied `Member` rides `ExternalMember`.
    let tryInlineBody (p: IExternalSymbolStore) (key: SymbolKey) : InlineBody voption =
        match key with
        | SymbolKey.Member m -> p.TryLookupMemberByKey m |> ValueOption.bind (fun em -> em.InlineBody)
        | SymbolKey.Binding _ -> p.TryLookupByKey key |> ValueOption.bind (fun s -> s.InlineBody)
        | SymbolKey.Type _ -> ValueNone
