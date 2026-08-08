namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 tail FACTORY over the `{ platform-repr → [canon] }` map folded from the
    /// layer-1 providers' `IntrinsicReverseCanon` — the reverse direction of
    /// `type int = (# "System.Int32" #)`. A factory, not a fixed list, so the leaf is seeded.
    type MetaTailFactory = ReferencedProject.MetaTailFactory

    /// Dependency-ordered manifests, and each package's transitive `depends-on` closure.
    let private orderedManifestsWithDeps (manifestPaths: string list) : string list * (string -> string list) =
        match ReferencedProject.buildClosureWithDeps manifestPaths with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// The manifest stack for a compilation that IS a package: its declared references, then
    /// the package's OWN manifest last. That `.fsi` route is the only channel a prior file's
    /// `type int32 = int` reaches a later one through — freezing carries no abbreviation.
    let selfStack (selfManifest: string option) (manifestPaths: string list) : string list =
        match selfManifest with
        | Some p -> manifestPaths @ [ p ]
        | None -> manifestPaths

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 leaf FACTORY.
    /// Common names no concrete leaf — the CLR backend injects its BCL reflection tail. Uncached.
    let buildWith (metaTail: MetaTailFactory) (target: string) (manifestPaths: string list) : IExternalSymbolProvider =
        ReferencedProject.composeContract metaTail target manifestPaths

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
            let paramAttrs = Array.create curried.Length ParamAttrs.Default

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
            // `orderedManifestsWithDeps` already failed on a malformed manifest.
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
                            // `manifest.Name` is the home assembly the published keys are
                            // rooted at — the same one the package's own symbols are
                            // stamped with, so a served key and a resolved one agree.
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
            /// The normalised manifest paths this contract was built from — the set whose
            /// per-target runtime ASSETS (`runtimeModules`) back a compiled program's imports.
            ManifestPaths: string list
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
                ManifestPaths = []
                Provider = ExternalSymbolProviders.nullProvider
                BodiesByName = Map.empty
                Origins = OriginSources.empty
            }

    /// Cache keyed by normalised manifest set + target + metadata tag.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Lazy<Contract>>(System.StringComparer.Ordinal)

    /// Build and cache the provider stack, inline bodies and producer sources for a manifest set.
    let private buildContractCached
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string)
        (manifestPaths: string list)
        : Contract =
        let normalised = manifestPaths |> List.map Path.GetFullPath
        // Target AND metadata tag are part of the cache identity: the JS and CLR collections
        // of one manifest set freeze different `impl` bodies over different layer-2 leaves.
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
                             ManifestPaths = normalised
                             Provider = served
                             BodiesByName = byName
                             Origins = collected.Origins
                         })
            )
            .Value

    /// Cached contract for a manifest set, over a caller-supplied layer-2 leaf FACTORY — the
    /// seam each backend wraps with its concrete leaf. `cacheTag` keeps each backend's
    /// collection of the same manifest set distinct.
    let buildContractWith
        (cacheTag: string)
        (metaTail: MetaTailFactory)
        (target: string)
        (manifestPaths: string list)
        : Contract =
        buildContractCached cacheTag metaTail target manifestPaths

    /// `buildContractWith` over a FIXED layer-2 leaf, wrapped as a constant factory — for a
    /// backend whose tail reads nothing from the reverse-canon map.
    let buildContractWithMetadata
        (cacheTag: string)
        (metaTail: IExternalSymbolProvider list)
        (target: string)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        (buildContractCached cacheTag (fun _ -> metaTail) target manifestPaths).Provider
