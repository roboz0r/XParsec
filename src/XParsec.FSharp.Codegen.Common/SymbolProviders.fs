namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 FACTORY over the intrinsic axis composed from the layer-1 providers, which is
    /// what `type int = (# "System.Int32" #)` declares, both directions. A factory, not a
    /// fixed list, so that axis can seed it.
    type PlatformMetadataFactory = ReferencedProject.PlatformMetadataFactory

    /// A package's manifest for the compiling target, as resolved from its directory.
    type ManifestPath = ReferencedProject.ManifestPath

    /// A parsed package manifest.
    type Manifest = ReferencedProject.Manifest

    /// Dependency-ordered manifests, PARSED, and each package's transitive `depends-on` closure.
    let private orderedManifestsWithDeps
        (manifests: ManifestPath list)
        : Manifest list * (ManifestPath -> ManifestPath list) =
        match ReferencedProject.buildClosureWithDeps manifests with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// The package stack for a compilation that IS a package: its declared references, then the
    /// package's OWN directory last. That `.fsi` route is the only channel a prior file's
    /// `type int32 = int` reaches a later one through: freezing carries no abbreviation.
    let selfStack (selfPackage: string option) (packageDirs: string list) : string list =
        match selfPackage with
        | Some p -> packageDirs @ [ p ]
        | None -> packageDirs

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 FACTORY. Common
    /// names no platform; each backend injects its own reader. Uncached.
    let buildWith
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        ReferencedProject.composeContract platformMetadata (ReferencedProject.resolveAll target packageDirs)

    /// Mint the `this`-first inline `TDecl.Let` for a `member inline`: an accessor
    /// `member inline _.M p0 p1 = body` IS the inline function `M this p0 p1 = body`, `this`
    /// the OUTERMOST curried param (a static member, `ThisKey = ValueNone`, prepends none).
    let liftMemberBody (origin: OriginSource) (m: TastAccessor.TypeMember) : InlineBody option =
        if not m.IsInline then
            None
        else
            let pool = m.Body.Pool
            // Every node minted below takes the body's own anchor, so the lifted tree indexes
            // exactly one file and the collection can stamp ONE origin over the whole thing.
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
            // function (`this -> p0 -> … -> ret`).
            for i = curried.Length - 1 downto 0 do
                let (pk, pty) = curried.[i]
                let lamTy = FTFun(pty, resultTy)
                let param = TastAccessor.mintNamedPat pool (BoundVarKey.identity pk) pty bodyTok
                body <- TastAccessor.mintLambda param body lamTy bodyTok
                resultTy <- lamTy

            let declTy = resultTy
            // `inlineExpand` matches `TDecl.Let(_, value, _, declTy)`, so this bound variable is
            // filler that keeps the node total — minted rather than taken from anything.
            let decl =
                TastAccessor.mintLetDecl
                    (TastAccessor.mintNamedPat pool (TastPoolBuilder.mintBoundVar pool) declTy bodyTok)
                    body
                    true
                    declTy

            // One entry per curried position, so `this` takes a leading default. No member
            // param carries a decoded attribute today, so every entry is `ParamAttrs.Default`.
            let paramAttrs = EqArray.init curried.Length (fun _ -> ParamAttrs.Default)

            Some(InlineBody.anchoredIn origin (TastPoolBuilder.declTree pool decl.Id) paramAttrs)

    type KeyedInlineBody = { Key: SymbolKey; Body: InlineBody }

    let private collectInlineBodies
        (origin: OriginSource)
        (tast: FrozenPools)
        : KeyedInlineBody list * KeyedInlineBody list =
        // The file's trees as columns, plus an append-only overlay for the wrapper lambdas.
        // The overlay dies with this call.
        let pool = TastPoolBuilder.openOver tast

        let anchored = InlineBody.anchoredIn origin

        // Unpooled off their own pool roots: the wire form is DU-typed because a pool id
        // means nothing in the consuming file's pool.
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
                    // A `member inline` on ANY member-bearing host (class / union / record)
                    // is a splice template; `liftMemberBody` skips every other member.
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

    /// One pass over a manifest set's splice sources: the templates published, and the
    /// producer file each was declared in.
    type CollectedInlineBodies =
        {
            Values: KeyedInlineBody list
            Members: KeyedInlineBody list
            Origins: OriginSources
        }

    /// Load cross-package inline bodies from manifests' `impl` files, type-checked and
    /// frozen once against `provider`. Manifest/decl order, so a later body wins a clash.
    let inlineBodies (provider: IExternalSymbolProvider) (manifests: Manifest list) : CollectedInlineBodies =
        let acc = ResizeArray<KeyedInlineBody>()
        let memberAcc = ResizeArray<KeyedInlineBody>()
        let mutable origins = OriginSources.empty

        for manifest in manifests do
            for rel in manifest.Impl do
                let file = VesperLib.libFile manifest.Name manifest.Dir rel

                match VesperLib.parseFileFull file with
                | Result.Error _ -> ()
                | Result.Ok parsed ->
                    let origin = Hashing.originSource parsed.File.Path parsed.Lexed

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
                        // `manifest.Name` is the home assembly the published keys are rooted
                        // at, the same one the package's own symbols are stamped with, so a
                        // served key and a resolved one agree.
                        let _, tast = Pipeline.analyseWithContextFor manifest.Name provider origin f

                        let values, members = collectInlineBodies origin tast

                        acc.AddRange values
                        memberAcc.AddRange members

        {
            Values = List.ofSeq acc
            Members = List.ofSeq memberAcc
            Origins = origins
        }


    /// One manifest set's composed contract. A backend takes the WHOLE value: a provider from
    /// one manifest set beside an anchor domain from another resolves a served body's position
    /// against a file that was never retained, and the wrong answer is in range.
    type Contract =
        {
            /// The `[core] runtime` assets of the whole `depends-on` closure, which back a
            /// compiled program's imports, keyed by package name.
            RuntimeAssets: Map<string, RuntimeAsset>
            Provider: IExternalSymbolProvider
            /// Simple name → body. NOT a resolution channel (the provider folds a body onto
            /// the entry that owns its key); the introspection seam tests assert against.
            BodiesByName: Map<string, InlineBody>
            /// The producer files the collected bodies were unpooled from, retained so their
            /// anchors stay readable. Re-parsing to recover them would give a second answer.
            Origins: OriginSources
        }

    module Contract =

        /// The contract of the EMPTY manifest set: nothing resolves, nothing is served, nothing
        /// is anchored but the compiling file. A real value, so no consumer carries a
        /// "there is no contract" arm.
        let empty: Contract =
            {
                RuntimeAssets = Map.empty
                Provider = ExternalSymbolProviders.nullProvider
                BodiesByName = Map.empty
                Origins = OriginSources.empty
            }

    /// Cache keyed by resolved manifest set + target + metadata tag.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Lazy<Contract>>(System.StringComparer.Ordinal)

    /// Cached contract for a package set, over a caller-supplied layer-2 FACTORY: the seam
    /// each backend wraps with its own platform reader.
    let buildContractWith
        (cacheTag: string)
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : Contract =
        let normalised = ReferencedProject.resolveAll target packageDirs

        // The tag distinguishes each backend's collection of one package set: they freeze
        // different bodies over different platform metadata. The target is in the key in
        // its own right because an EMPTY set contributes no path that could carry it.
        let key =
            cacheTag
            + "|"
            + target
            + "|"
            + (normalised |> List.map (fun m -> m.Path) |> String.concat ";")

        contractCache
            .GetOrAdd(
                key,
                fun _ ->
                    lazy
                        (let ordered, transitiveDeps = orderedManifestsWithDeps normalised

                         let provider =
                             ReferencedProject.composeOrdered platformMetadata ordered transitiveDeps

                         let collected = inlineBodies provider ordered

                         // A later body wins a clash (the list is in manifest/decl order).
                         let byName =
                             (Map.empty, collected.Values)
                             ||> List.fold (fun m v -> Map.add (SymbolKeyOps.intrinsicName v.Key) v.Body m)

                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

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
                             RuntimeAssets = ReferencedProject.runtimeModules ordered
                             Provider = served
                             BodiesByName = byName
                             Origins = collected.Origins
                         })
            )
            .Value

    /// `buildContractWith` over a FIXED provider list, wrapped as a constant factory: for a
    /// backend whose platform metadata reads nothing from the intrinsic axis.
    let buildContractWithMetadata
        (cacheTag: string)
        (platformMetadata: IExternalSymbolProvider list)
        (target: string)
        (packageDirs: string list)
        : IExternalSymbolProvider =
        (buildContractWith cacheTag (fun _ -> platformMetadata) target packageDirs).Provider
