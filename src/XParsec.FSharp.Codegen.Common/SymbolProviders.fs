namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 tail FACTORY: given the extracted `{ platform-repr → [canon] }` reverse
    /// map (folded from the layer-1 providers' `IntrinsicReverseCanon`), produce the
    /// metadata leaf. A factory rather than a fixed list so the leaf can be seeded with
    /// the reverse map — it canonicalizes a BCL `System.Int32` to the Vesper `int`
    /// through it (`MetadataSymbols.tryBuildType`), driven by the dynamically-analysed
    /// `type int = (# "System.Int32" #)` relationship rather than a static table.
    /// Non-CLR backends inject their own (reverse-independent) factory via
    /// `buildContractWithMetadata`. The dependency-order composition itself lives in
    /// `ReferencedProject.composeOrdered` (the SA layer), shared with the in-assembly
    /// test fixtures; this is the same type.
    type MetaTailFactory = ReferencedProject.MetaTailFactory

    /// Dependency-ordered manifests and each package's transitive `depends-on` closure.
    /// A cycle or missing dependency is a hard error.
    let private orderedManifestsWithDeps (manifestPaths: string list) : string list * (string -> string list) =
        match ReferencedProject.buildClosureWithDeps manifestPaths with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 leaf
    /// FACTORY. Common names no concrete leaf — the CLR backend injects its BCL
    /// `MetadataSymbols` tail (`ClrSymbolProviders.bclMetaTail`), the JS backend its
    /// JS-native tail. Uncached.
    let buildWith (metaTail: MetaTailFactory) (target: string) (manifestPaths: string list) : IExternalSymbolProvider =
        ReferencedProject.composeContract metaTail target manifestPaths

    /// Mint the `this`-first inline `TDecl.Let` for a concrete `member inline` — the
    /// member-sourced twin of the `let inline` value case. An accessor
    /// `member inline _.M p0 p1 = body` IS the inline function
    /// `M this p0 p1 = body`: `this` (the member's `ThisKey` / `ThisTy`) prepended
    /// as the OUTERMOST curried lambda param, then the value params in order; a STATIC
    /// member (`ThisKey = ValueNone`) prepends no `this`. The curried lambda and its
    /// `declTy` (the outer lambda's own function type, carrying the declaring + method
    /// typars in curried-param order) match the exact shape `inlineExpand` consumes.
    /// Only a `member inline` is a splice template; a non-`inline` member is a real
    /// callable and yields `None`. The DECLARATION says which, so a member whose body is
    /// an ordinary expression (`System.String.Concat(x, y)`) publishes exactly as an
    /// inline-IL one does — body shape is not consulted.
    ///
    /// `inline` is the same contract the value half already runs on: `InlineTemplates`
    /// publishes every `let inline` regardless of body shape. What both still owe is the
    /// accessibility check F# spells FS1113 ("marked inline but its implementation makes
    /// use of an internal or private function which is not sufficiently accessible"). A
    /// class's compiler-generated backing storage (primary-ctor params, preamble `let`s,
    /// `static let`s) is emitted `FieldAttributes.Assembly`, so a `FieldGet` on it CANNOT
    /// be read from a consumer assembly — and an `inline` member body, unlike a
    /// module-level `let inline`, is in scope to name one.
    ///
    /// Lifted off the FROZEN member, so the published body is `FrozenType` like every
    /// other thing crossing the provider seam. The wrapping lambdas are minted into the
    /// pool the member's body already lives in — the body is spliced BY ID, so nothing is
    /// copied to wrap it — and the finished declaration is unpooled back to the DU because
    /// that is the form the package wire carries (a pool id is meaningless in the
    /// consumer's own pool).
    ///
    /// `origin` is the file the member's body was written in, which a member cannot say of
    /// itself: it is a node, and only the collection that opened the pool knows which file the
    /// pool is.
    let liftMemberBody (origin: OriginSource) (m: TastAccessor.TypeMember) : InlineBody option =
        if not m.IsInline then
            None
        else
            let pool = m.Body.Pool
            // EVERY node minted below takes this one anchor, so the wrapper adds no position
            // the body did not already have: the finished tree's anchor domain is exactly the
            // member body's own file, and nothing here can index a second one. That is what
            // lets the collection stamp ONE origin over the whole lifted declaration.
            let bodyTok = TastAccessor.exprTok m.Body

            let curried =
                [
                    match m.ThisKey with
                    | ValueSome tk -> yield (tk, m.ThisTy)
                    | ValueNone -> ()

                    for (k, ty) in m.Params do
                        yield (k, ty)
                ]

            let mutable body = m.Body
            let mutable resultTy = m.ReturnTy

            // Fold innermost-last so the outermost lambda's type is the whole curried
            // function (`this -> p0 -> … -> ret`), exactly as `translateFun` folds a
            // source lambda.
            for i = curried.Length - 1 downto 0 do
                let (pk, pty) = curried.[i]
                let lamTy = FTFun(pty, resultTy)
                // The minted `NamedSimple` IS the parameter's definition site, so it is
                // built from the identity the declaration's key slot holds.
                let param = TastAccessor.mintNamedPat pool (BinderKey.identity pk) pty bodyTok
                body <- TastAccessor.mintLambda param body lamTy bodyTok
                resultTy <- lamTy

            let declTy = resultTy
            // The `TDecl.Let` binder is unread by `inlineExpand` (it matches
            // `TDecl.Let(_, value, _, declTy)`); this binder is unread filler that keeps
            // the node total, so it is minted rather than taken from anything.
            let decl =
                TastAccessor.mintLetDecl
                    (TastAccessor.mintNamedPat pool (TastPoolBuilder.mintBinder pool) declTy bodyTok)
                    body
                    true
                    declTy

            // ParamAttrs aligned to curried position: a leading (default) entry for
            // `this` holds value-param attribute indices at their curried offset. A
            // member param carries no decoded compiler attribute today, so every
            // entry is `ParamAttrs.Default`.
            let paramAttrs = Array.create curried.Length ParamAttrs.Default

            Some(InlineBody.anchoredIn origin (TastPoolBuilder.declTree pool decl.Id) paramAttrs)

    type KeyedInlineBody = { Key: SymbolKey; Body: InlineBody }

    let private collectInlineBodies
        (origin: OriginSource)
        (tast: FrozenPools)
        : KeyedInlineBody list * KeyedInlineBody list =
        // The file's trees as columns, with an append-only overlay for the curried lambda
        // chains `liftMemberBody` wraps each lifted body in. The overlay is
        // discarded with this call: what leaves is the unpooled DU template, never an id.
        let pool = TastPoolBuilder.openOver tast

        // Every body this function publishes, whichever list it lands in, is unpooled off THIS
        // pool — which is `tast`, which is `origin` frozen. So the domain is a fact of the
        // call, fixed once here rather than restated per comprehension.
        let anchored = InlineBody.anchoredIn origin

        // The published VALUE templates, unpooled off their own pool roots — the wire form
        // is DU-typed because a pool id means nothing in the consuming file's pool.
        let values =
            [
                for iv in tast.InlineTemplates ->
                    {
                        Key = iv.Key
                        Body = anchored (TastPoolBuilder.declTree pool iv.Decl) iv.ParamAttrs
                    }
            ]

        let members =
            [
                for d in TastAccessor.roots pool do
                    // A `member inline` on ANY member-bearing host (class / union /
                    // record — `TTypeKindG.members`) is a splice template. A non-`inline`
                    // member is a real callable and is skipped by `liftMemberBody`, so a
                    // union/record augmentation with an ordinary member is unaffected.
                    match TastAccessor.declKind d with
                    | DeclShape.Type ->
                        let tdecl = TastAccessor.declType d

                        for m in TTypeKindG.members tdecl.Kind do
                            match liftMemberBody origin m with
                            | Some body ->
                                let kind =
                                    match m.Kind with
                                    | TMemberKind.Method -> MemberKind.Method
                                    | TMemberKind.Property -> MemberKind.Property

                                let key =
                                    SymbolKeyOps.memberKey
                                        tdecl.TypeKey
                                        m.Name
                                        (m.Params |> EqArray.map snd)
                                        m.MethodTypeParams.Length
                                        kind

                                yield { Key = key; Body = body }
                            | None -> ()
                    | _ -> ()
            ]

        values, members

    /// One pass over a manifest set's splice sources: the templates they publish, and the
    /// producer file each was declared in.
    type CollectedInlineBodies =
        {
            Values: KeyedInlineBody list
            Members: KeyedInlineBody list
            Origins: OriginSources
        }

    /// Load cross-package inline bodies from manifests' `impl` files. Type-checked and
    /// frozen once against `provider`. Emitted in manifest/decl order so a later body wins
    /// a clash downstream (`Map.ofList` / `byKey.[k] <-`).
    let inlineBodies
        (target: string)
        (provider: IExternalSymbolProvider)
        (manifestPaths: string list)
        : CollectedInlineBodies =
        let acc = ResizeArray<KeyedInlineBody>()
        let memberAcc = ResizeArray<KeyedInlineBody>()
        let mutable origins = OriginSources.empty

        for manifestPath in manifestPaths do
            match ReferencedProject.loadManifest manifestPath with
            // A malformed manifest already failed `build`; nothing to add here.
            | Result.Error _ -> ()
            | Result.Ok manifest ->
                let dir = Path.GetDirectoryName manifestPath

                for rel in ReferencedProject.resolveImpl target manifest do
                    let file: VesperLib.LibFile =
                        {
                            Path =
                                {
                                    BucketName = manifest.Name
                                    Relative = rel
                                }
                            Absolute = Path.Combine(dir, rel)
                        }

                    match VesperLib.parseFileFull file with
                    | Result.Error _ -> ()
                    | Result.Ok parsed ->
                        // ONE retention, and it is also what every body unpooled below records
                        // as its anchor domain — so the retained file and the file an entry
                        // names cannot come apart.
                        let origin = Hashing.originSource parsed.File.Path parsed.Input parsed.Lexed

                        origins <- OriginSources.add origin origins

                        let implFile =
                            match parsed.Ast with
                            | FSharpAst.ImplementationFile f -> Some f
                            | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
                                Some(ImplementationFile.AnonymousModule elems)
                            | _ -> None

                        match implFile with
                        | None -> ()
                        | Some f ->
                            // The FROZEN file is the publish surface: its `InlineBodies`
                            // carry the minted keys, and every type in a body is
                            // `FrozenType` — no live `UnionFind` cell can cross to a
                            // consumer. `manifest.Name` is the home assembly the keys are
                            // rooted at, the same one `ReferencedProject.wrap` stamps onto
                            // the package's symbols, so the two agree by construction.
                            let _, tast = Pipeline.analyseWithContextFor manifest.Name provider origin f

                            let values, members = collectInlineBodies origin tast

                            acc.AddRange values
                            memberAcc.AddRange members

        {
            Values = List.ofSeq acc
            Members = List.ofSeq memberAcc
            Origins = origins
        }


    /// One manifest set's composed contract: the manifest set itself, the provider stack a
    /// compile resolves against, the collected bodies by simple name, and the producer sources
    /// their anchors index.
    ///
    /// A record and not a tuple because the members stopped being memorable by position;
    /// `Origins` in particular is the half a caller is most likely to forget exists, and a name
    /// is what stops it being dropped on the floor a second time.
    ///
    /// The whole VALUE is what a backend takes, never a pair of members chosen at a call site:
    /// a provider from one manifest set beside an anchor domain from another resolves a served
    /// body's position against a file that was never retained, and the wrong answer is in range
    /// (see `OriginFile`). Travelling as one value is what makes that unrepresentable.
    type Contract =
        {
            /// The normalised manifest paths this contract was built from — the set whose
            /// per-target runtime ASSETS (`runtimeModules`) back the imports
            /// of a program compiled against it. Held so the assets and the symbols a program
            /// resolves cannot be drawn from two different manifest sets.
            ManifestPaths: string list
            Provider: IExternalSymbolProvider
            /// Simple name → body. NOT a provider channel (the provider folds a body onto the
            /// entry that owns its key); the introspection seam tests assert against.
            BodiesByName: Map<string, InlineBody>
            /// The producer files the collected bodies were unpooled from, retained so their
            /// anchors stay readable. Cached WITH the provider: they are the same collection,
            /// and re-parsing to recover them would give a second answer for what each file
            /// contains.
            Origins: OriginSources
        }

    module Contract =

        /// The contract of the EMPTY manifest set: no package resolves, no body is served, and
        /// so nothing is anchored anywhere but the compiling file. The value a compile of a
        /// program that references no external symbol takes — a real contract rather than an
        /// absent one, so no consumer has to carry a "there is no contract" arm.
        let empty: Contract =
            {
                ManifestPaths = []
                Provider = ExternalSymbolProviders.nullProvider
                BodiesByName = Map.empty
                Origins = OriginSources.empty
            }

    /// Cache keyed by normalised manifest set + target + metadata tag. Each set is
    /// parsed, analysed, and composed once.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Lazy<Contract>>(System.StringComparer.Ordinal)

    /// Build and cache the provider stack + inline bodies + producer sources for a manifest
    /// set. `BodiesByName` is exposed via `contractInlineBodies` for tests.
    let private buildContractCached
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string)
        (manifestPaths: string list)
        : Contract =
        let normalised = manifestPaths |> List.map Path.GetFullPath
        // The target AND the metadata-layer tag are part of the cache identity: the JS
        // and CLR collections of the same set freeze different `impl` bodies, and a
        // backend (`cacheTag = "jsnative"`) composes a different layer-2 provider.
        let key = cacheTag + "|" + target + "|" + String.concat ";" normalised

        contractCache
            .GetOrAdd(
                key,
                fun _ ->
                    lazy
                        (let ordered, transitiveDeps = orderedManifestsWithDeps normalised

                         let provider =
                             ReferencedProject.composeOrdered metaTail target ordered transitiveDeps

                         let collected = inlineBodies target provider ordered

                         // A later body wins a clash (the list is in manifest/decl order).
                         let byName =
                             (Map.empty, collected.Values)
                             ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                         // Both channels arrive already keyed by a resolved identity — a
                         // VALUE by the key its home file minted at freeze, a MEMBER by the
                         // key the provider resolved at collection. Nothing here re-derives
                         // an identity from a spelling.
                         for v in collected.Values do
                             byKey.[v.Key] <- v.Body

                         for mb in collected.Members do
                             byKey.[mb.Key] <- mb.Body

                         let served =
                             provider
                             |> ExternalSymbolProviders.withInlineBodies (fun key ->
                                 match byKey.TryGetValue key with
                                 | true, v -> ValueSome v
                                 | _ -> ValueNone
                             )

                         {
                             // The NORMALISED list — the very one the cache key was taken from,
                             // so the set a consumer resolves runtime assets against is the set
                             // this contract's symbols were collected from.
                             ManifestPaths = normalised
                             Provider = served
                             BodiesByName = byName
                             Origins = collected.Origins
                         })
            )
            .Value

    /// Cached contract for a manifest set, over a caller-supplied layer-2 leaf FACTORY.
    /// The seam every backend's convenience layer wraps with its concrete leaf
    /// (`ClrSymbolProviders` injects BCL metadata, the JS backend its JS-native tail).
    /// `cacheTag` keeps each backend's collection of the same manifest set distinct in
    /// `contractCache`.
    let buildContractWith
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string)
        (manifestPaths: string list)
        : Contract =
        buildContractCached cacheTag metaTail target manifestPaths

    /// `buildContractWith` with a backend-injected, reverse-map-independent layer-2
    /// `metaTail` (a non-CLR backend supplies its own leaf), wrapped as a constant
    /// factory. `cacheTag` prevents the backend's entry from aliasing another's.
    let buildContractWithMetadata
        (cacheTag: string)
        (metaTail: IExternalSymbolProvider list)
        (target: string)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        (buildContractCached cacheTag (fun _ -> metaTail) target manifestPaths).Provider
