namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Shared mutable state threaded through the extractor:
///
///   - `TyparCollector` — order-preserving typar-name → index map. Explicit
///     `<'T>` typars are seeded first by the caller so they keep declared
///     positions; implicit body-only typars pick up indices in source order.
///   - `RawConstraint` / `ConstraintCollector` — captured `when …` clauses
///     before typar names are mapped to indices. Trait constraints flow
///     straight through; SRTP / `default` entries hold their raw `Type`
///     until the body walk has registered every typar they might reference.
///   - `ExtractCtx` — the per-build accumulator: symbol / type-shape tables,
///     diagnostics, ambient open-prefix set. `ExtractCtx.toProvider` lifts
///     it to an `IExternalSymbolProvider` (surfacing the ambient prefixes
///     via its `AmbientOpenPrefixes` member).
module VesperLibTyparCapture =

    [<RequireQualifiedAccess>]
    type TyparKind =
        | Regular
        | Static

    /// Order-preserving typar collector. The first time a name is seen it
    /// gets the next index; subsequent occurrences re-use it. Explicit
    /// `<'T>` typars are added first by the caller before the body walk,
    /// so they keep their declared positions; implicit (body-only) typars
    /// pick up indices in source order.
    [<Sealed>]
    type TyparCollector() =
        let dict = Dictionary<string, int>(StringComparer.Ordinal)
        let order = ResizeArray<string * TyparKind>()

        member _.IndexOf(name: string, kind: TyparKind) : int =
            match dict.TryGetValue name with
            | true, idx -> idx
            | _ ->
                let idx = order.Count
                dict.[name] <- idx
                order.Add((name, kind))
                idx

        /// Index for `name` if already registered, `ValueNone` otherwise.
        /// Used by the constraint resolver to map captured typar names to
        /// their typar-list positions *without* introducing extra typars.
        member _.TryIndexOf(name: string) : int voption =
            match dict.TryGetValue name with
            | true, idx -> ValueSome idx
            | _ -> ValueNone

        member _.Count = order.Count
        member _.Entries = order.ToArray()

    /// The per-body source context the finalize pass needs to run
    /// `translateType` on a stashed CST: the file's lexed tokens + source
    /// text, the in-scope open prefixes, and the typar collector with every body
    /// typar already interned at extraction (so a frozen re-walk reads the same
    /// indices the validation walk assigned). Held off the shape until the
    /// registry is complete.
    type DeferredCtx =
        {
            Lexed: Lexed
            Input: string
            Opens: string list
            Typars: TyparCollector
        }

    /// The deferred **CST** a type-shape body carries between extraction and the
    /// `ExtractCtx.toProvider` finalize pass — held off the shape (which carries
    /// only immutable `FrozenType` data) because a body may
    /// forward-reference a type declared later in the same package, so it can only
    /// be kinded once the registry is complete. The finalize pass runs
    /// `translateType` on it: a single `CST → FrozenType` translation, no
    /// closure-as-IR. Index-aligned with the shape's field / case / nothing.
    [<RequireQualifiedAccess>]
    type DeferredBody =
        /// The abbreviation's RHS CST.
        | Abbrev of DeferredCtx * rhs: Type<SyntaxToken>
        /// One field-type CST per record field, in declaration order.
        | Record of DeferredCtx * fields: Type<SyntaxToken>[]
        /// One field-type CST array per union case (in case order), each indexed
        /// by the case's fields, plus the union's directly-declared `interface
        /// <type>` impl CSTs (frozen into the shape's `interfaces` field — the union
        /// analogue of the `Class` arm's `interfaces`, so a bare cons-list's
        /// `interface seq<'T>` reaches `tryForInEnumerator`). `[]` interfaces ⇒ none.
        | Union of DeferredCtx * cases: Type<SyntaxToken>[][] * interfaces: Type<SyntaxToken> list
        /// A class's `inherit <type>` base CST (if any), its directly-declared
        /// `interface <type>` impl CSTs, and its `new: … -> T` constructor
        /// signatures — frozen by the finalize pass into the shape's
        /// `FrozenBaseType`, `FrozenInterfaces`, and `.ctor` `ExternalMember`s. The
        /// base lets a consumer's subtype walk reconcile through the *contract*
        /// inherit chain (the JS exception hierarchy, `InvalidOperationException :
        /// exn`) independent of BCL metadata; the interfaces let a consumer's
        /// interface-impl witness (`tryInterfaceWitness`' external arm)
        /// recover a phantom typar (`fold`'s `'E`) from a struct seq's
        /// `IStructSeq<'T,'E>` impl; the ctors let a BCL-free provider type the
        /// constructor-as-function application (`InvalidOperationException "msg"`).
        /// Each ctor is `(param-type CSTs, return-type CST)`. `ValueNone` base ⇒ no
        /// declared base; `[]` interfaces ⇒ none; `[]` ctors ⇒ none.
        | Class of
            DeferredCtx *
            baseType: Type<SyntaxToken> voption *
            interfaces: Type<SyntaxToken> list *
            ctors: (Type<SyntaxToken>[] * Type<SyntaxToken>) list

    /// The deferred **CST** for one augmentation member's signature, frozen into
    /// its `ExternalMember.Signature` by the finalize pass. Each member carries
    /// its own `DeferredCtx` (members are walked with a fresh typar collector).
    type DeferredMember =
        {
            Ctx: DeferredCtx
            Signature: CurriedSig<SyntaxToken>
        }

    /// The deferred **CST** for one `val` signature, frozen into a complete
    /// `ExternalSymbol` by the finalize pass. A val
    /// signature may forward-reference a type declared later in the package — and
    /// its constraint targets the same — so it can only be translated once the
    /// registry is complete. `Ctx.Typars` is seeded with the val's explicit `<'T>`
    /// typars at extraction; the finalize frozen walk interns the remaining body
    /// typars and captures the `when` clauses. `Key` is the val's identity — its
    /// declaring holder chain (namespace + enclosing modules, as the `.fsi` declares
    /// them) plus its compiled simple name; the compiled NAME is a rendering of it
    /// (`SymbolKeyOps.qualifiedName`), never the other way round. `Source` is the
    /// optional `ModuleSuffix` source-name alias (registered first-wins, so
    /// `DeferredVals` order must be preserved).
    type DeferredVal =
        {
            Ctx: DeferredCtx
            Key: BindingKey
            Source: string voption
            File: VesperLibManifest.LibFile
            Signature: CurriedSig<SyntaxToken>
        }

    /// Raw `when`-clause capture, before typar names are mapped to indices.
    [<RequireQualifiedAccess>]
    type RawConstraint =
        | Trait of typarName: string * kind: SemanticConstraintKind
        /// Captured `when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)`.
        /// `memberName` is the compiled name. `argTypes` / `returnType` are
        /// raw CST types so translation runs later — they may reference
        /// typars that the body walk hasn't registered yet.
        | MemberTrait of
            typarNames: string list *
            memberName: string *
            argTypes: Type<SyntaxToken> list *
            returnType: Type<SyntaxToken>
        /// `target` is the unresolved RHS of `default ^T : <type>`. Stored
        /// as a `Type<SyntaxToken>` so translation can run later, after the
        /// body walk has registered every typar the target might reference.
        | Default of typarName: string * target: Type<SyntaxToken>
        /// `when 'e :> <type>`. `target` is the raw RHS, translated later
        /// through the same `translateType` path the val signature used
        /// (it may reference typars not yet registered when captured).
        | Coercion of typarName: string * target: Type<SyntaxToken>

    [<Sealed>]
    type ConstraintCollector() =
        let items = ResizeArray<RawConstraint>()
        member _.Add(c: RawConstraint) = items.Add c
        member _.Snapshot() = List.ofSeq items
        member _.Count = items.Count

    /// Mutable accumulator threaded through `extractSymbols` across every
    /// file in `LoadedLib.Files`. Cross-bucket references resolve through
    /// the accumulated tables.
    [<Sealed>]
    type ExtractCtx() =
        member val Symbols = Dictionary<string, ExternalSymbol>(StringComparer.Ordinal) with get
        /// File-level diagnostics — parse failures, AST-shape rejections.
        /// Kept distinct from `Skipped`; tests filter against them to detect
        /// parsing regressions.
        member val Diagnostics = ResizeArray<VesperLibManifest.LibFile * string>() with get
        /// Per-val extraction failures. Captured separately from
        /// `Diagnostics` so file-level health stays visible: an .fsi can
        /// extract 90% of its vals fine and still parse cleanly.
        member val Skipped = ResizeArray<VesperLibManifest.LibFile * string>() with get
        /// Type-name index: short name -> (arity, compiledName). Multiple
        /// entries with the same short name are allowed; first declaration
        /// wins (warn-and-take-first per the plan).
        member val Types = Dictionary<string, int * string>(StringComparer.Ordinal) with get
        /// The IDENTITY of every type this package declares, indexed by its canonical
        /// metadata name — the key minted at `registerTypeDecl` off the containment the
        /// walker descended, and the ONE place a name gets back to a key. The shape tables
        /// below are addressed by that canonical name (`SymbolKeyOps.typeMetaName key`,
        /// which is injective), so this is what lifts them to the key-addressed store view
        /// `toProvider` publishes.
        ///
        /// The name is a RENDERING of the key, never a route back to one: nothing re-cuts a
        /// metadata name into an identity (that absorbs a module into the namespace path).
        /// A name that is not this rendering — the DOTTED spelling a `.fsi` writes for a
        /// module-held type, `Microsoft.FSharp.Core.ByRefKinds.In` — is resolved through the
        /// declared containment instead (`ExtractCtx.tryTypeKey` / `ModuleHolders`).
        member val TypeKeys = Dictionary<string, TypeKey>(StringComparer.Ordinal) with get
        /// Every `module` this package declares, indexed by the DOTTED path the source
        /// writes it as (`Microsoft.FSharp.Core.ByRefKinds`) -> the `TypeHolder` a type
        /// declared in it sits in. The containment the contract itself minted, so a written
        /// name can be resolved against it: `A.B.C` with `A.B` a known module IS the type
        /// `C` that module holds. Source spellings, because that is what a name index
        /// resolves; the holder inside carries the COMPILED (`…Module`-suffixed) chain.
        member val ModuleHolders = Dictionary<string, TypeHolder>(StringComparer.Ordinal) with get
        /// Type-shape index: qualified compiled name -> body shape. Only
        /// records, unions, and abbreviations are populated in v1; classes
        /// and other shapes land later.
        member val TypeShapes = Dictionary<string, ExternalTypeShape>(StringComparer.Ordinal) with get
        /// Instance/static members extracted from a type's augmentation block —
        /// the `member`s a `.fsi` declares inside a union/record/class body
        /// (`Option.IsSome` / `.Value` / `.IsNone`). Keyed by the declaring
        /// type's qualified compiled name (the same key `TypeShapes` uses);
        /// each list preserves declaration order. Surfaced through `toProvider`'s
        /// `TryLookupMember` / `TryLookupMembers` so a cross-package use site can
        /// type `o.IsSome` against the contract. Empty for types with no augmentation members.
        member val TypeMembers = Dictionary<string, ResizeArray<ExternalMember>>(StringComparer.Ordinal) with get
        /// The deferred body **CST** for each `TypeShapes` body
        /// (`Record` / `Union` / `Abbrev`), keyed by the same qualified compiled
        /// name. Held here rather than on the shape (which carries only immutable
        /// `FrozenType` data); the
        /// `toProvider` finalize pass runs `translateType` on it once the
        /// registry is complete. `Class` / `Intrinsic` / `Opaque` bodies carry no
        /// CST and have no entry.
        member val DeferredBodies = Dictionary<string, DeferredBody>(StringComparer.Ordinal) with get
        /// The deferred signature **CST** for each `TypeMembers` entry, keyed by
        /// the declaring type's qualified compiled name and index-aligned with the
        /// member list. Frozen into each member's `Signature` by the `toProvider`
        /// finalize pass.
        member val DeferredMembers = Dictionary<string, ResizeArray<DeferredMember>>(StringComparer.Ordinal) with get
        /// The deferred `val` signatures stashed during extraction, in source order.
        /// The `toProvider` finalize pass translates each to a `FrozenType` template,
        /// builds the complete `ExternalSymbol` (instantiation closure + resolved
        /// constraints), and registers it into `Symbols`. Ordered so the
        /// first-registration-wins `ModuleSuffix` source-name alias keeps the same
        /// semantics the eager extraction had.
        member val DeferredVals = ResizeArray<DeferredVal>() with get
        /// Intrinsic-representation index: *short* type name -> the **platform** repr
        /// string for the COMPILING target, extracted from the package's per-target
        /// `.fs` companion (`prim-types-int.js.fs` ⇒ `"int" -> "number"` on JS; the base
        /// `.fs` itself on CLR, where it IS the platform repr). Populated BEFORE `.fsi`
        /// extraction so the `extern` arm of `extractTypeSig` can publish the `platform`
        /// name of an `ExternalTypeShape.Intrinsic(short, arity, platform)`. A primitive
        /// ABSENT here on a non-base target (e.g. `decimal` on JS — no `.js.fs`) still
        /// publishes as an `Intrinsic` (gated on `IntrinsicBaseReprs` below) but with
        /// `platform = None` ("no representation on this target" — fatal only for a nullary
        /// scalar; a generic constructor like `'T []` is representable structurally). Empty
        /// for callers with no `.fs` companions (a signature-only FSharp.Core port), so
        /// every extern stays a `Class`.
        member val IntrinsicReprs = Dictionary<string, string>(StringComparer.Ordinal) with get
        /// Primitive *marker* index: *short* type name -> its BASE `.fs` `(# … #)` repr,
        /// extracted from the base companion regardless of target. The presence of a
        /// base repr is what makes an `extern` a primitive (publishes as `Intrinsic`,
        /// not an opaque `Class`); the per-target `IntrinsicReprs` then supplies the
        /// `platform` name (or `None` when this target ships no companion for it). Kept
        /// SEPARATE from `IntrinsicReprs` so a target that omits a primitive does not
        /// demote it to a `Class` and lose its `canon` identity in the unifier.
        member val IntrinsicBaseReprs = Dictionary<string, string>(StringComparer.Ordinal) with get

        /// The heritable primitives (`extern class with …`: `obj`/`exn`) awaiting their
        /// one-shot finalize-time republish as an `Intrinsic` with a class surface:
        /// qualified compiled name -> (canon key, platform repr), recorded by the
        /// `TypeSignature.Extern` arm. A side table because the `extern class` kind
        /// tag is the discriminator and it is NOT recoverable at finalize
        /// (`DeferredBody.Class` is shared with capability interfaces and carries no
        /// kind); the base/`.ctor` surfaces are read off the frozen `Class` shape /
        /// `TypeMembers` when the republish runs (`VesperLib.finalizeDeferred`).
        member val PendingIntrinsicClasses =
            Dictionary<string, struct (TypeKey * string)>(StringComparer.Ordinal) with get

        /// The capability interfaces (`disposable`/`equatable`/`comparable`: `extern with
        /// abstract member …` + a `.fs` `(# … #)` repr) awaiting their one-shot finalize-time
        /// republish as an `IntrinsicInterface`: qualified compiled name -> (canon key, platform
        /// repr), recorded by the `TypeSignature.Extern` arm. Deferred (not attached at
        /// extraction) because the interface's member surface is not populated in the shape
        /// until the finalize member-copy loop runs; the republish reads the frozen `Class`
        /// shape's members + origin (`VesperLib.finalizeDeferred`). CLR-only — a JS build binds
        /// no repr, so a capability stays a plain canon-only interface `Class`.
        member val PendingCapabilityInterfaces =
            Dictionary<string, struct (TypeKey * string)>(StringComparer.Ordinal) with get

        /// Qualified names of `[<AutoOpen>]` modules encountered during
        /// extraction, in source order (`"Vesper.ArithmeticOperators"`). A
        /// referenced contract surfaces these as its ambient open-prefix set so a
        /// consumer resolves `op_Addition` / `hash` with no explicit `open`.
        /// The hardcoded FSharp.Core prelude list
        /// stays separate (compiler-magic opens not expressible as `[<AutoOpen>]`).
        member val AutoOpenPrefixes = ResizeArray<string>() with get
        /// Qualified compiled names of `[<RequireQualifiedAccess>]` unions
        /// encountered during extraction. The reverse case-name index reads this to
        /// stamp `ExternalUnionCase.IsRequireQualifiedAccess`, so a consumer's bare
        /// (unqualified) reference to an RQA case is rejected the way F# rejects it

        member val RqaTypes = HashSet<string>(StringComparer.Ordinal) with get
        /// Type shapes contributed by already-extracted dependency packages (dependency-ordered).
        /// Read-only here: extraction never writes a dependency's shape, only
        /// consults it (through `shapeOf`) to kind a cross-package nominal head.
        /// `ReferencedProject.buildProviderWith` seeds it from the composite
        /// `TryLookupType` of this package's dependency providers *and* layer-2
        /// metadata (the BCL) — the same shapes the consumer would see, minus this
        /// package. Layer-2 is included so a contract naming a raw BCL nominal head
        /// not aliased in its own package kinds correctly at bake time instead of
        /// baking a spurious `TyUnknown`.
        /// The default (`fun _ -> ValueNone`) is the dependency-free, no-metadata
        /// case: any caller that builds a context in isolation.
        member val AmbientShapes: (string -> ExternalTypeShape voption) = (fun _ -> ValueNone) with get, set
        /// The implicit open prefixes contributed by this package's dependency
        /// providers (their `AmbientOpenPrefixes` — e.g. Vesper.Core's `"Vesper"`,
        /// where `Fun`2` / `Fun`3` / `Ref` live). Seeded by
        /// `ReferencedProject.buildProviderWith` from the dependency composite, and
        /// used as the lowest-priority open prefixes during this package's own
        /// extraction (`extractSymbols`), so a `.fsi` can name a dependency's
        /// ambiently-available type unqualified — exactly as the consumer's
        /// front end resolves it through the composite's `AmbientOpenPrefixes`.
        /// Without this, a cross-package short name reachable only via a dependency's
        /// prelude (a `'TFunc :> Fun<_,_>` coercion target) froze as `FTUnknown`.
        /// The default `[]` is the dependency-free / isolated-context case.
        member val DependencyAmbientPrefixes: string list = [] with get, set

    module ExtractCtx =
        let empty () = ExtractCtx()

        /// The in-scope type shape for compiled name `compiled` during
        /// extraction: this package's own shapes first (registered as its files
        /// are walked), then the dependency-contributed `AmbientShapes`. The
        /// single lookup `translateType`'s kinding consults — own shapes
        /// shadow a dependency's on a name clash, matching the consumer composite's
        /// first-source-wins priority.
        ///
        /// A *forward* reference within this package (a name whose shape is
        /// registered later in the file walk) misses here unless it is legal: an
        /// intra-package forward reference is only valid inside a `type … and …`
        /// group or a `rec` namespace/module, and those shapes are registered
        /// together before any signature body is kinded. Any other unresolved name
        /// is a genuine `TyUnknown`, not an ordering artefact.
        let shapeOf (ctx: ExtractCtx) (compiled: string) : ExternalTypeShape voption =
            match ctx.TypeShapes.TryGetValue compiled with
            | true, s -> ValueSome s
            | _ -> ctx.AmbientShapes compiled

        /// NAME RESOLUTION: the identity a written type name denotes in this package, or
        /// `ValueNone` for a name it declares no type under. THE by-name entry point — every
        /// path that must accept a name (the extraction-time resolver, the published
        /// provider's resolver view) comes here, and what it gets back is a key.
        ///
        /// Two spellings reach it, and neither is re-cut into an identity:
        ///   * the canonical metadata name (what `typeMetaName` renders, what a probe built
        ///     from a key spells) — a direct hit on the identity index;
        ///   * the DOTTED spelling the source WRITES for a module-held type
        ///     (`byref<'T, ByRefKinds.In>`), which is not that rendering. It resolves through
        ///     the CONTAINMENT: `A.B` is a module this contract declared, so `C` is the type
        ///     that module holds — the same question F# name resolution asks. The candidate
        ///     key it mints is then looked up in the identity index, so the key returned is
        ///     always the REGISTERED one and a spelling that names nothing resolves to nothing.
        let tryTypeKey (ctx: ExtractCtx) (probe: string) : TypeKey voption =
            // The written-name → registered-key containment algorithm is shared with the
            // frozen-impl projector (`FrozenSignature.typeShapeByName`) — the ONE
            // canonicalizer that inverts `typeMetaName`'s dotted flattening through the
            // declared holders, never a re-cut string.
            let exact (name: string) =
                match ctx.TypeKeys.TryGetValue name with
                | true, key -> ValueSome key
                | _ -> ValueNone

            let moduleHolder (path: string) =
                match ctx.ModuleHolders.TryGetValue path with
                | true, holder -> ValueSome holder
                | _ -> ValueNone

            SymbolKeyOps.tryDottedModuleHeld exact moduleHolder probe

        /// Provider over the extracted symbol / type-shape tables, exposing
        /// `ctx.AutoOpenPrefixes` as its ambient. The pipeline seeds the
        /// ambient into the open scope and probes it BEHIND explicit
        /// `open`s — `1 + 2`'s desugared `op_Addition` lives in
        /// `Microsoft.FSharp.Core.Operators`, not at the root, and resolves
        /// through the ambient.
        /// The extractor populates `ctx.AutoOpenPrefixes` from `[<AutoOpen>]`
        /// attributes on modules; `buildProvider` supplements it with any
        /// library-specific prelude (e.g. F#'s implicit `Microsoft.FSharp.*`
        /// namespace opens) before calling `toProvider`. The same mechanism
        /// `ReferencedProject` uses for Vesper packages.
        /// Lift a finalized `ExtractCtx` to an `IExternalSymbolProvider`.
        ///
        /// PRECONDITION: the deferred bodies / members must already be frozen into
        /// the shapes' templates — `VesperLib.finalizeDeferred` runs the
        /// `translateType` pass (it lives a file later, where the
        /// translation is in scope). The `VesperLib.ExtractCtx.toProvider` wrapper
        /// chains the two; nothing else calls this directly.
        let toProvider (ctx: ExtractCtx) : IExternalSymbolProvider =
            // Reverse case-name index for `TryLookupUnionCase`:
            // bare case name -> (declaring union compiled name, arity, case
            // shape). Built once here, after extraction has fully populated
            // `ctx.TypeShapes`. First declaration wins on a name collision (the
            // same warn-and-take-first rule the short-name type index uses); the
            // actual Vesper packages have disjoint case names across unions, so
            // collisions don't arise in practice.
            let unionCaseIndex =
                let d = Dictionary<string, ExternalUnionCase>(StringComparer.Ordinal)

                for kv in ctx.TypeShapes do
                    match kv.Value with
                    // The cons-list's cases are now the canonical `Empty`/`Cons`
                    // (shared `OperatorNames.unionCaseCtorName`), matching `ElaborateExpr`
                    // and codegen. They must NOT enter the bare-ctor-name index: the
                    // list is constructed/matched only via `[]`/`::` (which lower
                    // specially), never by writing `Cons`/`Empty`, so indexing them
                    // would shadow a user union's same-named ctor. This exclusion is
                    // what the old `op_Nil`/`op_ColonColon` extraction form provided.
                    | ExternalTypeShape.Union _ when RuntimeNames.isVesperListName kv.Key -> ()
                    | ExternalTypeShape.Union(arity, cases, _, origin) ->
                        let rqa = ctx.RqaTypes.Contains kv.Key

                        for case in cases do
                            if not (d.ContainsKey case.Name) then
                                d.[case.Name] <-
                                    {
                                        UnionName = kv.Key
                                        TyparArity = arity
                                        Origin = origin
                                        Case = case
                                        IsRequireQualifiedAccess = rqa
                                    }
                    | _ -> ()

                d

            // Reverse intrinsic axis `{ platform-repr -> canon }` (the `.fsi` name),
            // so a metadata BCL/native runtime name (`System.Exception`) reconciles
            // back to the front-end identity (`exn`) at RESOLUTION — in
            // `MetadataSymbols.tryBuildType` (eager canonicalization) — and so the
            // multi-canon entries drive `numericFamilyOr`'s JS `number` widening. Built
            // from the published `Intrinsic` shapes — the same source the forward
            // `canon` axis reads. The `platform <> canon.Name` guard is NOT a fact about the
            // primitive (see the forward axis, which no longer carries it); it survives here
            // because this map's consumer, `Engine.intrinsicUnionOf`, fires on a platform name
            // with ≥2 canons, so admitting JS `string`/`undefined` would newly synthesise
            // `char | string` and `unit | undefined`. That is a front-end semantics change to
            // measure, not to make in passing.
            // A capability INTERFACE (`disposable`) is deliberately NOT emitted here: it
            // resolves to a `TyClass` constraint, not a `TyConst` value identity, so its only
            // possible reverse consumer is `MetadataSymbols.tryBuildType` — which must NOT
            // canonicalize an interface to an `FTConst` and would guard it out. A BCL
            // `System.IDisposable` reconciles to the canonical `disposable` through
            // `CapabilityIdentity` / the `IntrinsicInterface` platform name, NOT this map; an
            // entry here would be dead weight AND force an `IsInterface` guard back into the
            // reverse-map readers (the "change both or drift" seam this omission retires).
            let intrinsicReverse =
                ctx.TypeShapes
                |> Seq.choose (fun kv ->
                    match kv.Value with
                    // Every intrinsic reconciles its platform repr back to canon — the
                    // scalar (`int`/`System.Int32`) and the heritable primitive
                    // (`exn`/`System.Exception`, `obj`/`System.Object`) alike.
                    | ExternalTypeShape.Intrinsic {
                                                      Id = {
                                                               Canon = canon
                                                               Platform = Some platform
                                                           }
                                                  } when platform <> canon.Name -> Some(platform, SymbolKey.Type canon)
                    | _ -> None
                )
                // A platform repr is one-to-many over canons (JS: `number` <- int/float/
                // float32), so group rather than collapse — `Map.ofSeq` would keep only
                // the last canon per key. Dedup, preserve first-seen order.
                |> Seq.groupBy fst
                |> Seq.map (fun (platform, xs) -> platform, xs |> Seq.map snd |> Seq.distinct |> List.ofSeq)
                |> Map.ofSeq

            // Forward intrinsic axis `{ canon -> platform-repr }`, from the SAME published
            // `Intrinsic` shapes `intrinsicReverse` reads — but not its mirror; see the
            // guard note below. Codegen reads it to resolve a primitive canon (`int`) to its
            // `.fs` repr (`System.Int32`) — the single source of a primitive's repr.
            // Only `Intrinsic` (scalar/structural primitives) carry a codegen repr;
            // capability platform interfaces are reconciliation-only (reverse) and are
            // encoded as classes, so they are NOT included here.
            //
            // NO `platform <> canon.Name` guard, unlike `intrinsicReverse`: the two are
            // different string axes (a source spelling and a platform type name), so their
            // coincidence is not a fact about the primitive. On JS `string`, `bigint` and
            // `undefined` all repr to their own spelling and are as represented as any
            // other; dropping them made the axis mean "no repr on this target" and "repr
            // happens to be spelled like the canon" at once, and the second reading is what
            // rendered a `bigint` literal without its `n` suffix.
            let intrinsicForward =
                // `SymbolKey` is equatable-but-not-comparable, so the canon-keyed forward
                // axis is a read-only `Dictionary`, not a `Map`.
                let d = System.Collections.Generic.Dictionary<SymbolKey, string>()

                for kv in ctx.TypeShapes do
                    match kv.Value with
                    // Every intrinsic carries a codegen repr — a heritable primitive
                    // (`obj`/`exn`) IS emitted as a value/type ref
                    // (`System.Object`/`System.Exception`), unlike a capability interface.
                    | ExternalTypeShape.Intrinsic {
                                                      Id = {
                                                               Canon = canon
                                                               Platform = Some platform
                                                           }
                                                  } -> d.[SymbolKey.Type canon] <- platform
                    | _ -> ()

                d :> System.Collections.Generic.IReadOnlyDictionary<_, _>

            // The type channels' index, addressed by the IDENTITY the extractor minted for
            // each type — not by a rendering of it. This is what lets the store answer a
            // module-held type's `InModule` key, which no name the source writes spells. The
            // shape/member tables are addressed by the key's canonical name (injective), so
            // walking `ctx.TypeKeys` lifts them to the key without re-cutting any string.
            let shapesByKey = Dictionary<SymbolKey, ExternalTypeShape>()
            let membersByKey = Dictionary<SymbolKey, ResizeArray<ExternalMember>>()

            for KeyValue(compiled, typeKey) in ctx.TypeKeys do
                let key = SymbolKey.Type typeKey

                match ctx.TypeShapes.TryGetValue compiled with
                | true, shape -> shapesByKey.[key] <- shape
                | _ -> ()

                match ctx.TypeMembers.TryGetValue compiled with
                | true, members -> membersByKey.[key] <- members
                | _ -> ()

            // The written name resolves into a key (`tryTypeKey` — the canonical rendering,
            // or the source's dotted containment); `ofKeyIndexes` derives the by-name shape
            // lookup from it and the key index, so one index answers both ways in and a
            // spelling can no longer be an identity of its own.
            let typeKeyOfName (name: string) : TypeKey voption = tryTypeKey ctx name

            // The extractor's leaf: the TYPE channels are key-addressed (its types carry a
            // module chain a name cannot express), the symbol channel stays name-addressed —
            // a binding's key renders `.`-joined, which is exactly how `ctx.Symbols` is keyed.
            //
            // A `.fsi` contract does not (yet) publish TS index signatures or a record-field
            // index, and the extractor exposes signatures rather than spliceable inline
            // bodies — those are collected separately and served by the codegen contract-stack
            // wrapper layered over this provider. Those channels keep `KeyIndexedLeaf.empty`'s
            // miss.
            ExternalSymbolProviders.ofKeyedLeaf (
                ExternalSymbolProviders.KeyedLeaf.ofKeyIndexes
                    { ExternalSymbolProviders.KeyIndexedLeaf.empty with
                        ShapesByKey = shapesByKey
                        MembersByKey = membersByKey
                        ResolveTypeName = typeKeyOfName
                        TryLookup =
                            fun name ->
                                match ctx.Symbols.TryGetValue name with
                                | true, sym -> ValueSome sym
                                | _ -> ValueNone
                        TryLookupUnionCase =
                            fun caseName ->
                                match unionCaseIndex.TryGetValue caseName with
                                | true, hit -> ValueSome hit
                                | _ -> ValueNone
                        AmbientOpenPrefixes = List.ofSeq ctx.AutoOpenPrefixes
                        IntrinsicReverseCanon = intrinsicReverse
                        IntrinsicForwardRepr = intrinsicForward
                    }
            )
