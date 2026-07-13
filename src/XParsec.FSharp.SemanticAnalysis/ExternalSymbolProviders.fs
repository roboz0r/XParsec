namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Concurrent
open System.Collections.Generic

// Provider construction and composition over the external-symbol contract: the
// by-name leaf builder (`NamedLeaf`/`ofNamedLeaf`), the null provider, the
// layering combinators (`stack`/`composite` + the intrinsic-axis merges), and
// the decorators (`mapProviderTypes`, `memoize`). Split out of
// `ExternalSymbols` (which keeps the data model, the interfaces, and the
// instantiation/realisation helpers): this layer is self-contained over the
// interfaces and is where every new provider shape or decorator lands.
module ExternalSymbolProviders =


    /// A leaf provider expressed as its BY-NAME lookup functions — the single home
    /// for the name-keyed leaf shape. `ofNamedLeaf` derives the full two-faced
    /// provider from one of these: the resolver face calls the functions directly,
    /// and the store face answers a `SymbolKey` by projecting
    /// `SymbolKeyOps.qualifiedName` onto the SAME functions — so a leaf's two
    /// `TryLookupType` faces agree BY CONSTRUCTION, and the string round-trip is
    /// spelled once, here, instead of once per provider. Start from
    /// `NamedLeaf.empty` (all misses) and override the channels the leaf models;
    /// `ofNamedLeaf NamedLeaf.empty` is the null provider.
    type NamedLeaf =
        {
            TryLookup: string -> ExternalSymbol voption
            TryLookupType: string -> ExternalTypeShape voption
            TryLookupUnionCase: string -> ExternalUnionCase voption
            AmbientOpenPrefixes: string list
            /// `(declaring type's qualified compiled name, member name)`.
            TryLookupMember: string * string -> ExternalMember voption
            TryLookupMembers: string * string -> ExternalMember[]
            TryLookupIndexSignature: string -> (FrozenType * FrozenType) list
            /// Key-addressed even on the leaf: an inline body has no by-name channel
            /// (a value key does not round-trip to its spelling).
            TryLookupInlineBody: SymbolKey -> InlineBody voption
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
                AmbientOpenPrefixes = []
                TryLookupMember = fun _ -> ValueNone
                TryLookupMembers = fun _ -> [||]
                TryLookupIndexSignature = fun _ -> []
                TryLookupInlineBody = fun _ -> ValueNone
                IntrinsicReverseCanon = Map.empty
                IntrinsicForwardRepr = ExternalSymbols.emptyForwardRepr
            }

    /// Derive the two-faced provider from a by-name leaf — see `NamedLeaf`.
    let ofNamedLeaf (leaf: NamedLeaf) : IExternalSymbolProvider =
        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name = leaf.TryLookup name
              member _.TryLookupType(name: string) = leaf.TryLookupType name
              member _.TryLookupUnionCase caseName = leaf.TryLookupUnionCase caseName
              member _.AmbientOpenPrefixes = leaf.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  leaf.TryLookupType(SymbolKeyOps.qualifiedName key)

              member _.TryLookupMember(key, memberName) =
                  leaf.TryLookupMember(SymbolKeyOps.qualifiedName key, memberName)

              member _.TryLookupMembers(key, memberName) =
                  leaf.TryLookupMembers(SymbolKeyOps.qualifiedName key, memberName)

              member _.TryLookupIndexSignature key =
                  leaf.TryLookupIndexSignature(SymbolKeyOps.qualifiedName key)

              member _.TryLookupInlineBody key = leaf.TryLookupInlineBody key
              member _.IntrinsicReverseCanon = leaf.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = leaf.IntrinsicForwardRepr
        }

    /// For tests that want to isolate behavior from external-symbol noise.
    let nullProvider: IExternalSymbolProvider = ofNamedLeaf NamedLeaf.empty

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
                // Re-home the existing key: the inner provider builds the containment
                // chain but has no assembly for it yet (`Origin.Local`); the wrapper
                // knows the asm from the package manifest, and `reroot` rewrites the
                // `Origin` at the chain's root, leaving the chain itself untouched. A
                // source/compiled alias pair (e.g. `List.fold` + `ListModule.fold`)
                // carries the SAME key, so this home-only rewrite keeps the aliases
                // pointing at one identity.
                fun (s: ExternalSymbol) ->
                    { s with
                        Origin = o
                        Key = SymbolKeyOps.reroot o.Namespace.Origin s.Key
                    }

        let stampMember =
            match stampOrigin with
            | ValueNone -> id
            | ValueSome o -> fun (m: ExternalMember) -> { m with Origin = o }

        // The single place that decides which `ExternalTypeShape` cases carry
        // their `Origin`. Class/Record/Union do today; Abbrev doesn't (its
        // cross-package emit path lands later, with the same shape). Extend
        // this match — not three call sites — when a new case learns origin.
        //
        // The origin is a PACKAGE fact (home assembly + manifest namespace); a type's own
        // namespace lives in its key, so no per-type namespace repair happens here.
        let stampType (shape: ExternalTypeShape) : ExternalTypeShape =
            match stampOrigin with
            | ValueNone -> shape
            | ValueSome o ->
                match shape with
                | ExternalTypeShape.Class info -> ExternalTypeShape.Class { info with Origin = o }
                | ExternalTypeShape.Record(arity, fields, _) -> ExternalTypeShape.Record(arity, fields, o)
                | ExternalTypeShape.Union(arity, cases, ifaces, _) -> ExternalTypeShape.Union(arity, cases, ifaces, o)
                | ExternalTypeShape.Enum(cases, _) -> ExternalTypeShape.Enum(cases, o)
                // Origin-stamped like a `Class` (its value resolution key is asm-qualified via
                // `Origin`; the extractor left it `Empty`), but its namespace comes from its
                // authoritative canonical key (`Vesper.Collections` for `seq`; `disposable` et al.
                // already sit directly in `Vesper`), which is where the capability's identity is.
                | ExternalTypeShape.IntrinsicInterface s ->
                    // The canon key is asm-blind by convention, so only its PATH is taken;
                    // the home assembly stays the package's.
                    let canonPath =
                        match s.Canon with
                        | SymbolKey.Type t -> t.Namespace.Path
                        | _ -> o.Namespace.Path

                    ExternalTypeShape.IntrinsicInterface
                        { s with
                            Origin =
                                {
                                    Namespace = { o.Namespace with Path = canonPath }
                                }
                        }
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

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  firstHit (fun s -> s.TryLookup name) |> ValueOption.map stampSymbol

              member _.TryLookupType(name: string) =
                  firstHit (fun s -> s.TryLookupType name) |> ValueOption.map stampType

              // First source that knows a case of this name wins; re-stamp the
              // package origin onto the result exactly as `TryLookupType` does for
              // the union shape it came from (the inner extractor records
              // `SymbolOrigin.Empty`).
              member _.TryLookupUnionCase caseName =
                  firstHit (fun s -> s.TryLookupUnionCase caseName)
                  |> ValueOption.map stampUnionCase

              member _.AmbientOpenPrefixes = ambient
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  firstHit (fun s -> s.TryLookupType key) |> ValueOption.map stampType

              member _.TryLookupMember(key, memberName) =
                  firstHit (fun s -> s.TryLookupMember(key, memberName))
                  |> ValueOption.map stampMember

              // First source that knows the type wins the whole overload set — a
              // type's members live in one assembly, so a later source never
              // *adds* overloads to an earlier one's hit (same first-hit-wins
              // shadowing as the singular lookups).
              member _.TryLookupMembers(key, memberName) =
                  let mutable result = [||]
                  let mutable i = 0

                  while Array.isEmpty result && i < sources.Length do
                      result <- sources.[i].TryLookupMembers(key, memberName)
                      i <- i + 1

                  match stampOrigin with
                  | ValueNone -> result
                  | ValueSome _ -> result |> Array.map stampMember

              // First source with a non-empty index signature wins (a type's index sig
              // lives in one home, like its members). The `(key, value)` templates are
              // origin-independent, so no re-stamp — a plain first-hit-wins fall-through.
              member _.TryLookupIndexSignature(key: SymbolKey) =
                  let mutable result = []
                  let mutable i = 0

                  while List.isEmpty result && i < sources.Length do
                      result <- sources.[i].TryLookupIndexSignature key
                      i <- i + 1

                  result

              // Inline bodies are origin-independent `TDecl`s (no key/origin
              // re-stamp), so these are plain first-hit-wins fall-throughs like the
              // lookups above — a source that serves cross-package inline bodies
              // (the codegen contract stack) surfaces them through the composite.
              member _.TryLookupInlineBody key =
                  firstHit (fun s -> s.TryLookupInlineBody key)

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

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  inner.TryLookup name
                  |> ValueOption.map (fun s -> { s with Scheme = co s.Scheme })

              member _.TryLookupType(name: string) =
                  inner.TryLookupType name |> ValueOption.map mapShape

              member _.TryLookupUnionCase caseName =
                  inner.TryLookupUnionCase caseName
                  |> ValueOption.map (fun uc -> { uc with Case = mapCase uc.Case })

              member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  inner.TryLookupType key |> ValueOption.map mapShape

              member _.TryLookupMember(key, memberName) =
                  inner.TryLookupMember(key, memberName) |> ValueOption.map mapMember

              member _.TryLookupMembers(key, memberName) =
                  inner.TryLookupMembers(key, memberName) |> Array.map mapMember

              // An index KEY is a contravariant position (the supplied index), the VALUE a
              // covariant read — the same variance split as a member's `Parameters`/`Return`.
              member _.TryLookupIndexSignature(key: SymbolKey) =
                  inner.TryLookupIndexSignature key |> List.map (fun (k, v) -> contra k, co v)

              member _.TryLookupInlineBody key = inner.TryLookupInlineBody key
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
        let typesByName = ConcurrentDictionary<string, ExternalTypeShape voption>()
        let typesByKey = ConcurrentDictionary<SymbolKey, ExternalTypeShape voption>()

        let members =
            ConcurrentDictionary<struct (SymbolKey * string), ExternalMember voption>()

        let memberSets =
            ConcurrentDictionary<struct (SymbolKey * string), ExternalMember[]>()

        let indexSigs = ConcurrentDictionary<SymbolKey, (FrozenType * FrozenType) list>()
        let unionCases = ConcurrentDictionary<string, ExternalUnionCase voption>()
        let inlineByKey = ConcurrentDictionary<SymbolKey, InlineBody voption>()

        { new IExternalSymbolProvider

          interface IExternalSymbolResolver with
              member _.TryLookup name =
                  symbols.GetOrAdd(name, (fun n -> inner.TryLookup n))

              member _.TryLookupType(name: string) =
                  typesByName.GetOrAdd(name, (fun n -> inner.TryLookupType n))

              member _.TryLookupUnionCase caseName =
                  unionCases.GetOrAdd(caseName, (fun n -> inner.TryLookupUnionCase n))

              member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes
          interface IExternalSymbolStore with
              member _.TryLookupType(key: SymbolKey) =
                  typesByKey.GetOrAdd(key, (fun k -> inner.TryLookupType k))

              member _.TryLookupMember(key, memberName) =
                  members.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMember(k, m)))

              member _.TryLookupMembers(key, memberName) =
                  memberSets.GetOrAdd(struct (key, memberName), (fun (struct (k, m)) -> inner.TryLookupMembers(k, m)))

              member _.TryLookupIndexSignature(key: SymbolKey) =
                  indexSigs.GetOrAdd(key, (fun k -> inner.TryLookupIndexSignature k))

              member _.TryLookupInlineBody key =
                  inlineByKey.GetOrAdd(key, (fun k -> inner.TryLookupInlineBody k))

              member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
              member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }
