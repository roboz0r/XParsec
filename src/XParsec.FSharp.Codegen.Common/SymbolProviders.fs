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

    /// A harvested member-sourced inline body: the declaring type's (simple)
    /// compiled name, the member name, and the `this`-first inline `Body`. Kept a
    /// SEPARATE channel from the source-name value bodies because a member resolves
    /// to its `SymbolKey` through the provider (`TryLookupMember`) at store time —
    /// so the stored key AGREES with the use-site `TExpr.ExternalMember.Key` (the
    /// finalized member key) rather than a hand-rolled `MemberKey`.
    type MemberInlineBody =
        {
            TypeName: string
            MemberName: string
            Body: InlineBody
        }

    /// A harvested `let inline` VALUE body: its simple compiled name, the resolved
    /// `SymbolKey` its home package interns it under (`ValueNone` if the qualified
    /// name did not resolve), and the `Body`. `Key` is the value-channel twin of the
    /// member channel's `TryLookupMember` result: source spelling is resolved to the
    /// identity ONCE, at collection, so the by-KEY store agrees with the use-site
    /// `TExpr.External.Key` — a module-qualified read (`Unchecked.defaultof`) hits the
    /// identity-robust key channel — and a `SymbolKey`, not a spelling, flows onward.
    /// The simple `Name` still keys the by-name channel (a bare `undefined`, and
    /// intra-body `External` refs, carry it).
    type ValueInlineBody =
        {
            Name: string
            Key: SymbolKey voption
            Body: InlineBody
        }

    /// Mint the `this`-first inline `TDecl.Let` for a concrete `(# … #)`-bodied
    /// member — the member-sourced twin of the `let inline` value case. A concrete
    /// accessor `member _.M p0 p1 = (# … #)` IS the inline function
    /// `M this p0 p1 = (# … #)`: `this` (the member's `ThisKey` / `ThisTy`) prepended
    /// as the OUTERMOST curried lambda param, then the value params in order; a STATIC
    /// member (`ThisKey = ValueNone`) prepends no `this`. The curried lambda and its
    /// `declTy` (the outer lambda's own arrow type, carrying the declaring + method
    /// typars in curried-param order) match the exact shape `inlineExpand` /
    /// `expandExternalAt` consume. Only an inline-IL (`TExpr.ILIntrinsic`) body is a
    /// splice template; any other member body is a real callable and yields `None`.
    let harvestMemberBody (typeName: string) (m: TTypeMember) : MemberInlineBody option =
        match m.Body with
        | TExpr.ILIntrinsic(_, _, _, _, bodyTok) ->
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
                let lamTy = TyFun(pty, resultTy)
                body <- TExpr.Lambda(TPat.NamedSimple(pk, pty, bodyTok), body, lamTy, bodyTok)
                resultTy <- lamTy

            let declTy = resultTy
            // The `TDecl.Let` binder is unread by `inlineExpand` (it matches
            // `TDecl.Let(_, value, _, declTy)`); a synthetic key keeps the node total.
            let patKey = NodeKey.ofSynthetic bodyTok.StartIndex NodeKind.SynthLambdaBody
            let decl = TDecl.Let(TPat.NamedSimple(patKey, declTy, bodyTok), body, true, declTy)

            // ParamAttrs aligned to curried position: a leading (default) entry for
            // `this` holds value-param attribute indices at their curried offset. A
            // member param carries no decoded compiler attribute today, so every
            // entry is `ParamAttrs.Default`.
            let paramAttrs = Array.create curried.Length ParamAttrs.Default

            Some
                {
                    TypeName = typeName
                    MemberName = m.Name
                    Body = { Decl = decl; ParamAttrs = paramAttrs }
                }
        | _ -> None

    /// The fully-qualified compiled name of a module value (`Vesper.Unchecked.defaultof`):
    /// `Namespace.Holder.Name` with empty segments dropped, matching the contract
    /// extractor's `dv.Compiled` (`VesperLib.compiledNameForVal`) — i.e. the name the
    /// provider indexes the value under. The query for the one source-spelling → key
    /// resolution; a wrong reconstruction simply misses (`ValueNone`), never mis-keys.
    let private qualifiedValueName (info: ModuleMemberInfo) : string =
        [
            (match info.Namespace with
             | Some ns -> ns
             | None -> "")
            info.Holder
            info.Name
        ]
        |> List.filter (fun s -> s <> "")
        |> String.concat "."

    /// Cross-package inline bodies. The first channel is `let inline` VALUE bodies
    /// keyed by source name; the second is member-sourced bodies (concrete
    /// `(# … #)`-bodied members on a `Class`), served by member key. Collected once
    /// here, frozen against the same provider stack the consumer uses.
    let private collectInlineBodies (ctx: PassContext) (tast: TastFile) : ValueInlineBody list * MemberInlineBody list =
        let acc = ResizeArray<ValueInlineBody>()
        let memberAcc = ResizeArray<MemberInlineBody>()

        // Pre-pass: build NodeKey → source-name map. Inline bodies that reference a
        // sibling inline carry `TExpr.Var` bound to a key not in scope at a consumer
        // use site; rewrite those to `TExpr.External(name)` so the inliner can splice them.
        let inlineNames = System.Collections.Generic.Dictionary<NodeKey, string>()

        for d in tast.Decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                match Map.tryFind k tast.ModuleMembers with
                | Some info -> inlineNames.[k] <- info.Name
                | None -> ()
            | _ -> ()

        let rewriteInlineVars (e: TExpr) : TExpr =
            let mapper: TastWalk.Mapper =
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun _ e ->
                            match e with
                            | TExpr.Var(k, ty, tok) ->
                                match inlineNames.TryGetValue k with
                                | true, name -> ValueSome(TExpr.External(name, ValueNone, ty, tok))
                                | _ -> ValueNone
                            | _ -> ValueNone
                }

            TastWalk.mapExpr mapper e

        let rewriteDecl (d: TDecl) : TDecl =
            match d with
            | TDecl.Let(pat, value, isInline, ty) -> TDecl.Let(pat, rewriteInlineVars value, isInline, ty)
            | other -> other

        for d in tast.Decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                match Map.tryFind k tast.ModuleMembers with
                | Some info ->
                    // Carry param attrs so the consumer's inliner honours them without re-decoding.
                    let paramAttrs =
                        match ctx.InlineParamAttrs.TryGetValue k with
                        | true, a -> a
                        | _ -> [||]

                    acc.Add(
                        {
                            Name = info.Name
                            Key =
                                ctx.Provider.TryLookup(qualifiedValueName info)
                                |> ValueOption.map (fun s -> s.Key)
                            Body =
                                {
                                    Decl = rewriteDecl d
                                    ParamAttrs = paramAttrs
                                }
                        }
                    )
                | None -> ()
            // A non-inline `let` value bound to a single zero-operand intrinsic
            // (`let undefined = (# "undefined" #)`) is served as an inline body too: it
            // has no CLR-style `let inline`, but the JS backend treats it as a
            // compile-time alias for its intrinsic (`Inline.nullaryIntrinsicValueBody`) —
            // the consumer's `InlineExpansion` splices the intrinsic at each reference so
            // no `const undefined = undefined` definition or import is emitted. No param
            // attrs (a nullary value has no parameters).
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, false, _) when (Inline.nullaryIntrinsicValueBody d).IsSome ->
                match Map.tryFind k tast.ModuleMembers with
                | Some info ->
                    acc.Add(
                        {
                            Name = info.Name
                            Key =
                                ctx.Provider.TryLookup(qualifiedValueName info)
                                |> ValueOption.map (fun s -> s.Key)
                            Body = { Decl = d; ParamAttrs = [||] }
                        }
                    )
                | None -> ()
            // Member-sourced inline bodies: a concrete `(# … #)`-bodied member on ANY
            // member-bearing host (class / union / record — `TTypeKindG.members`) mints
            // a `this`-first inline body (the member-sourced twin of the `let inline`
            // value case). A member with a non-inline-IL body is a real callable and is
            // skipped by `harvestMemberBody`, so a union/record augmentation with an
            // ordinary member is unaffected — only its `(# … #)` members are harvested
            // (not silently dropped as a Class-only match once did).
            | TDecl.Type tdecl ->
                // Key the harvested body by the QUALIFIED compiled name — the store
                // (`buildContractCached`) resolves the finalized member key via
                // `TryLookupMember(typeName, …)`, which matches by the qualified name
                // (`SymbolKeyOps.qualifiedName`), not the simple `tdecl.Name`. A
                // namespaced intrinsic (`Vesper.string`, `Widgets.widget`) would
                // otherwise miss and fall back to a (non-existent) real method call.
                let typeName = SymbolKeyOps.qualifiedName tdecl.Key

                for m in TTypeKindG.members tdecl.Kind do
                    match harvestMemberBody typeName m with
                    | Some mb -> memberAcc.Add mb
                    | None -> ()
            | _ -> ()

        List.ofSeq acc, List.ofSeq memberAcc

    /// Load cross-package inline bodies from manifests' `impl` files. Type-checked
    /// and frozen once against `provider`. Emitted in manifest/decl order so a later
    /// body wins a by-name clash downstream (`Map.ofList` / `byKey.[k] <-`).
    /// `target` selects per-target `inline-bodies-<t>` overrides.
    let inlineBodies
        (target: string option)
        (provider: IExternalSymbolProvider)
        (manifestPaths: string list)
        : ValueInlineBody list * MemberInlineBody list =
        let acc = ResizeArray<ValueInlineBody>()
        let memberAcc = ResizeArray<MemberInlineBody>()

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
                            let ctx, tast =
                                Pipeline.analyseSemWithContextFor manifest.Name provider parsed.Input parsed.Lexed f

                            let values, members = collectInlineBodies ctx tast

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

    /// Wrap `inner` to serve cross-package inline bodies. `byKey` is the primary
    /// channel (resolved `SymbolKey`); `byName` is the source-name fallback for
    /// `External` heads with `key = ValueNone`.
    let private withInlineBodies
        (inner: IExternalSymbolProvider)
        (byKey: System.Collections.Generic.Dictionary<SymbolKey, InlineBody>)
        (byName: Map<string, InlineBody>)
        : IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup name = inner.TryLookup name
            member _.TryLookupType name = inner.TryLookupType name
            member _.TryLookupMember(t, m) = inner.TryLookupMember(t, m)
            member _.TryLookupMembers(t, m) = inner.TryLookupMembers(t, m)
            member _.TryLookupIndexSignature t = inner.TryLookupIndexSignature t
            member _.TryLookupUnionCase c = inner.TryLookupUnionCase c
            member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes

            member _.TryLookupInlineBody key =
                match byKey.TryGetValue key with
                | true, v -> ValueSome v
                | _ -> ValueNone

            member _.TryLookupInlineBodyByName name =
                match Map.tryFind name byName with
                | Some v -> ValueSome v
                | None -> ValueNone

            member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
            member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
        }

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

                         // The by-NAME fallback (`External` heads with `key = ValueNone`
                         // — a bare `undefined`, intra-body refs). Simple-name keyed; a
                         // later body wins a clash (list is in manifest/decl order).
                         let byName = (Map.empty, values) ||> List.fold (fun m v -> Map.add v.Name v.Body m)

                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                         // Value bodies are keyed by the `SymbolKey` resolved ONCE at
                         // collection — the same key a use-site `TExpr.External` carries,
                         // so a module-qualified read (`Unchecked.defaultof`) hits this
                         // identity-robust channel. A resolved identity flows here, not a
                         // spelling: the store builder does no source-name lookup of its
                         // own. (The simple name does not resolve — the index is
                         // qualified-name keyed and the holder is not auto-opened — which
                         // is why the by-name fallback alone once needed a name-rewrite hack.)
                         for v in values do
                             match v.Key with
                             | ValueSome k -> byKey.[k] <- v.Body
                             | ValueNone -> ()

                         // Member-sourced bodies are keyed by the FINALIZED member key
                         // the provider resolves (`TryLookupMember`): a method's argSig
                         // is rewritten from its frozen params, so this key AGREES with
                         // the use-site `TExpr.ExternalMember.Key`. Never hand-roll a
                         // `MemberKey` here — that would risk key disagreement.
                         for mb in memberInlines do
                             match provider.TryLookupMember(mb.TypeName, mb.MemberName) with
                             | ValueSome mem -> byKey.[mem.Key] <- mb.Body
                             | ValueNone -> ()

                         withInlineBodies provider byKey byName, byName)
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
