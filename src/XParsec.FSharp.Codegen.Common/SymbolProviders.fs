namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 tail FACTORY: given the harvested `{ platform-repr → [canon] }` reverse
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
    let buildWith (metaTail: MetaTailFactory) (manifestPaths: string list) : IExternalSymbolProvider =
        ReferencedProject.composeContract metaTail None manifestPaths

    /// Mint the `this`-first inline `TDecl.Let` for a concrete `(# … #)`-bodied
    /// member — the member-sourced twin of the `let inline` value case. A concrete
    /// accessor `member _.M p0 p1 = (# … #)` IS the inline function
    /// `M this p0 p1 = (# … #)`: `this` (the member's `ThisKey` / `ThisTy`) prepended
    /// as the OUTERMOST curried lambda param, then the value params in order; a STATIC
    /// member (`ThisKey = ValueNone`) prepends no `this`. The curried lambda and its
    /// `declTy` (the outer lambda's own arrow type, carrying the declaring + method
    /// typars in curried-param order) match the exact shape `inlineExpand` consumes.
    /// Only an inline-IL (`Frozen.TExpr.ILIntrinsic`) body is a splice template; any
    /// other member body is a real callable and yields `None`.
    ///
    /// That restriction is load-bearing beyond inlining: a class's compiler-generated
    /// backing storage (primary-ctor params, preamble `let`s, `static let`s) is emitted
    /// `FieldAttributes.Assembly`, so a `FieldGet` on it CANNOT be read from a consumer
    /// assembly. Nothing published here can carry such a read — an IL splice has no
    /// `FieldGet` on class storage, and a module-level `let inline` cannot name a class's
    /// ctor param or `let` binding at all (F# scoping forbids it). Widening this to
    /// publish general member bodies would expose exactly that, and would first need the
    /// accessibility check F# spells FS1113 ("marked inline but its implementation makes
    /// use of an internal or private function which is not sufficiently accessible").
    ///
    /// Harvested off the FROZEN member, so the published body is `FrozenType` like every
    /// other thing crossing the provider seam.
    let harvestMemberBody (m: Frozen.TTypeMember) : InlineBody option =
        match m.Body with
        | TExprG.ILIntrinsic(_, _, _, _, bodyTok) ->
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
            // arrow (`this -> p0 -> … -> ret`), exactly as `translateFun` folds a
            // source lambda.
            for i = curried.Length - 1 downto 0 do
                let (pk, pty) = curried.[i]
                let lamTy = FTFun(pty, resultTy)
                body <- TExprG.Lambda(TPatG.NamedSimple(pk, pty, bodyTok), body, lamTy, bodyTok)
                resultTy <- lamTy

            let declTy = resultTy
            // The `TDecl.Let` binder is unread by `inlineExpand` (it matches
            // `TDecl.Let(_, value, _, declTy)`); this synthetic key is unread filler
            // that keeps the node total. `synthLambdaBodyKey` is the sole home of its
            // construction (shared with the ExprLambda recomputes).
            let patKey = TastWalk.synthLambdaBodyKey bodyTok

            let decl =
                TDeclG.Let(TPatG.NamedSimple(patKey, declTy, bodyTok), body, true, declTy)

            // ParamAttrs aligned to curried position: a leading (default) entry for
            // `this` holds value-param attribute indices at their curried offset. A
            // member param carries no decoded compiler attribute today, so every
            // entry is `ParamAttrs.Default`.
            let paramAttrs = Array.create curried.Length ParamAttrs.Default

            Some { Decl = decl; ParamAttrs = paramAttrs }
        | _ -> None

    /// A unit's published inline vocabulary, read off its FROZEN tree.
    ///
    /// The value half is a straight read: `Freeze` already minted each template's
    /// `SymbolKey` from its declaring module chain and published it — that identity is
    /// OWNED, not reconstructed, which is what a multi-file unit (no `.fsi` to recover a
    /// name against) needs.
    ///
    /// The member half is harvested here, and its total `MemberKey` is minted DIRECTLY
    /// from the frozen member `m`: at freeze `m.Params` are already `FrozenType`s and
    /// `m.MethodTypeParams` its own generic arity, so the structural, value-equal key is
    /// in hand with no re-derivation. A name-lookup round-trip (`TryLookupMember`) would
    /// collapse a same-name overload set to a single best-by-arity pick and lose every
    /// sibling body — the second harvested body would overwrite the first under one key
    /// and neither of the others would ever get a body. Because `MemberKey` is now a total
    /// overload identity there is no rendered `argSig` for producer and use site to
    /// disagree on; the key `m` mints here is the same one an external entry / use site
    /// mints from the same frozen signature by construction.
    let private collectInlineBodies (tast: Frozen.TastFile) : Frozen.TInlineValue list * Frozen.TInlineValue list =
        let values = tast.InlineBodies |> EqArray.toList

        let members =
            [
                for d in tast.Decls do
                    match d with
                    // A concrete `(# … #)`-bodied member on ANY member-bearing host
                    // (class / union / record — `TTypeKindG.members`) is a splice
                    // template. A member with a non-inline-IL body is a real callable and
                    // is skipped by `harvestMemberBody`, so a union/record augmentation
                    // with an ordinary member is unaffected.
                    | TDeclG.Type tdecl ->
                        for m in TTypeKindG.members tdecl.Kind do
                            match harvestMemberBody m with
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

    /// Load cross-package inline bodies from manifests' `impl` files. Type-checked
    /// and frozen once against `provider`. Emitted in manifest/decl order so a later
    /// body wins a clash downstream (`Map.ofList` / `byKey.[k] <-`).
    /// `target` selects per-target `inline-bodies-<t>` overrides.
    let inlineBodies
        (target: string option)
        (provider: IExternalSymbolProvider)
        (manifestPaths: string list)
        : Frozen.TInlineValue list * Frozen.TInlineValue list =
        let acc = ResizeArray<Frozen.TInlineValue>()
        let memberAcc = ResizeArray<Frozen.TInlineValue>()

        for manifestPath in manifestPaths do
            match ReferencedProject.loadManifest manifestPath with
            // A malformed manifest already failed `build`; nothing to add here.
            | Result.Error _ -> ()
            | Result.Ok manifest ->
                let dir = Path.GetDirectoryName manifestPath

                for rel in ReferencedProject.resolveInlineBodies target manifest do
                    let file: VesperLib.LibFile =
                        {
                            BucketName = manifest.Name
                            Relative = rel
                            Absolute = Path.Combine(dir, rel)
                        }

                    match VesperLib.parseFileFull file with
                    | Result.Error _ -> ()
                    | Result.Ok parsed ->
                        let implFile =
                            match parsed.Ast with
                            | FSharpAst.ImplementationFile f -> Some f
                            | FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems) ->
                                Some(ImplementationFile.AnonymousModule elems)
                            | _ -> None

                        match implFile with
                        | None -> ()
                        | Some f ->
                            // The FROZEN unit is the publish surface: its `InlineBodies`
                            // carry the minted keys, and every type in a body is
                            // `FrozenType` — no live `UnionFind` cell can cross to a
                            // consumer. `manifest.Name` is the home assembly the keys are
                            // rooted at, the same one `ReferencedProject.wrap` stamps onto
                            // the package's symbols, so the two agree by construction.
                            let _, tast =
                                Pipeline.analyseWithContextFor manifest.Name provider parsed.Input parsed.Lexed f

                            let values, members = collectInlineBodies tast

                            acc.AddRange values
                            memberAcc.AddRange members

        List.ofSeq acc, List.ofSeq memberAcc


    /// Cache keyed by normalised manifest set + target + metadata tag. Each set is
    /// parsed, analysed, and composed once.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<IExternalSymbolProvider * Map<string, InlineBody>>
         >(
            System.StringComparer.Ordinal
        )

    /// Build and cache the provider stack + inline bodies for a manifest set.
    /// The raw `Map` is exposed via `contractInlineBodies` for tests.
    let private buildContractCached
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider * Map<string, InlineBody> =
        let normalised = manifestPaths |> List.map Path.GetFullPath
        // The target AND the metadata-layer tag are part of the cache identity: the JS
        // and CLR collections of the same set freeze different `inline-bodies`, and a
        // backend (`cacheTag = "jsnative"`) composes a different layer-2 provider.
        let key =
            cacheTag + "|" + (defaultArg target "") + "|" + String.concat ";" normalised

        contractCache
            .GetOrAdd(
                key,
                fun _ ->
                    lazy
                        (let ordered, transitiveDeps = orderedManifestsWithDeps normalised

                         let provider =
                             ReferencedProject.composeOrdered metaTail target ordered transitiveDeps

                         let values, memberInlines = inlineBodies target provider ordered

                         // A simple-name → body map — NOT a provider channel (the provider
                         // folds a body onto the entry that owns its key); it is the
                         // introspection seam `contractInlineBodies` returns so tests can
                         // assert a manifest set collected the bodies it should. A later
                         // body wins a clash (list is in manifest/decl order).
                         let byName =
                             (Map.empty, values)
                             ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                         // Both channels arrive already keyed by a resolved identity — a
                         // VALUE by the key its home unit minted at freeze, a MEMBER by the
                         // key the provider resolved at collection. Nothing here re-derives
                         // an identity from a spelling.
                         for v in values do
                             byKey.[v.Key] <- v.Body

                         for mb in memberInlines do
                             byKey.[mb.Key] <- mb.Body

                         let served =
                             provider
                             |> ExternalSymbolProviders.withInlineBodies (fun key ->
                                 match byKey.TryGetValue key with
                                 | true, v -> ValueSome v
                                 | _ -> ValueNone
                             )

                         served, byName)
            )
            .Value

    /// Cached provider stack + raw inline-body map for a manifest set, over a
    /// caller-supplied layer-2 leaf FACTORY. The seam every backend's convenience
    /// layer wraps with its concrete leaf (`ClrSymbolProviders` injects BCL metadata,
    /// the JS backend its JS-native tail). `cacheTag` keeps each backend's collection
    /// of the same manifest set distinct in `contractCache`.
    let buildContractWith
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider * Map<string, InlineBody> =
        buildContractCached cacheTag metaTail target manifestPaths

    /// `buildContractWith` with a backend-injected, reverse-map-independent layer-2
    /// `metaTail` (a non-CLR backend supplies its own leaf), wrapped as a constant
    /// factory. `cacheTag` prevents the backend's entry from aliasing another's.
    let buildContractWithMetadata
        (cacheTag: string)
        (metaTail: IExternalSymbolProvider list)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        buildContractCached cacheTag (fun _ -> metaTail) target manifestPaths |> fst
