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

    /// Cross-package `let inline` bodies keyed by source name. Collected once here,
    /// frozen against the same provider stack the consumer uses.
    let private collectInlineBodies (ctx: PassContext) (tast: TastFile) : (string * InlineBody) list =
        let acc = ResizeArray<string * InlineBody>()

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
                        info.Name,
                        {
                            Decl = rewriteDecl d
                            ParamAttrs = paramAttrs
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
                | Some info -> acc.Add(info.Name, { Decl = d; ParamAttrs = [||] })
                | None -> ()
            | _ -> ()

        List.ofSeq acc

    /// Load cross-package inline bodies from manifests' `impl` files. Type-checked
    /// and frozen once against `provider`. A later body wins on a name clash.
    /// `target` selects per-target `inline-bodies-<t>` overrides.
    let inlineBodies
        (target: string option)
        (provider: IExternalSymbolProvider)
        (manifestPaths: string list)
        : Map<string, InlineBody> =
        let mutable acc = Map.empty

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

                            for (name, body) in collectInlineBodies ctx tast do
                                acc <- Map.add name body acc

        acc


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

                         let inlines = inlineBodies target provider ordered

                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                         for KeyValue(name, body) in inlines do
                             match provider.TryLookup name with
                             | ValueSome sym -> byKey.[sym.Key] <- body
                             | ValueNone -> ()

                         withInlineBodies provider byKey inlines, inlines)
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
