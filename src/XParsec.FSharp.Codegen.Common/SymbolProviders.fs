namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 tail FACTORY: given the harvested `{ platform-repr → canon }` reverse
    /// map (folded from the layer-1 providers' `IntrinsicReverseCanon`), produce the
    /// metadata leaf. A factory rather than a fixed list so the leaf can be seeded with
    /// the reverse map — it canonicalizes a BCL `System.Int32` to the Vesper `int`
    /// through it (`MetadataSymbols.tryBuildType`), driven by the dynamically-analysed
    /// `type int = (# "System.Int32" #)` relationship rather than a static table.
    /// Non-CLR backends inject their own (reverse-independent) factory via
    /// `buildContractWithMetadata`.
    type MetaTailFactory = Map<string, string> -> IExternalSymbolProvider list

    /// Fold a provider list's `{ platform → canon }` reverse maps; later wins (matches
    /// `ExternalSymbols.composite`'s fold).
    let private foldReverseCanon (providers: IExternalSymbolProvider list) : Map<string, string> =
        (Map.empty, providers)
        ||> List.fold (fun acc p -> (acc, p.IntrinsicReverseCanon) ||> Map.fold (fun m k v -> Map.add k v m))

    /// BCL reflection over the host runtime. The empty-reverse case (extraction-time
    /// `depComposite`, which never canonicalizes primitives) reuses the shared singleton;
    /// only the seeded final-composite leaf is freshly built.
    let private bclMetaTail: MetaTailFactory =
        fun reverseCanon ->
            if reverseCanon.IsEmpty then
                [ MetadataSymbols.provider ]
            else
                [
                    MetadataSymbols.createWith reverseCanon (MetadataSymbols.runtimeAssemblyPaths ())
                ]

    /// Dependency-ordered manifests and each package's transitive `depends-on` closure.
    /// A cycle or missing dependency is a hard error.
    let private orderedManifestsWithDeps (manifestPaths: string list) : string list * (string -> string list) =
        match ReferencedProject.buildClosureWithDeps manifestPaths with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// Compose layer-1 providers (dependency order) ahead of the layer-2 metadata tail.
    /// Each package is built with access to its declared `depends-on` closure + BCL
    /// metadata, so cross-package nominal heads kind at bake time.
    let private composeProviders
        (metaTail: MetaTailFactory)
        (target: string option)
        (orderedManifestPaths: string list)
        (transitiveDeps: string -> string list)
        : IExternalSymbolProvider =
        // Final stack, in build (topological) order; `byPath` indexes each built
        // provider by its normalised manifest path so a package's dependency
        // providers resolve in O(closure).
        let built = ResizeArray<IExternalSymbolProvider>()

        let byPath =
            System.Collections.Generic.Dictionary<string, IExternalSymbolProvider>(System.StringComparer.Ordinal)

        for path in orderedManifestPaths do
            let key = Path.GetFullPath path

            let depProviders =
                transitiveDeps key
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            // The per-package extraction leaf is the SAME injected `metaTail` factory as
            // the final composite — Common never names a concrete provider. Seeded with
            // the reverse map of the deps built so far (extraction does not canonicalize
            // primitives, so this is the cheap empty-reverse case in practice). (Before,
            // this hardcoded the BCL leaf even on a JS build.)
            let depComposite =
                ExternalSymbols.composite (depProviders @ metaTail (foldReverseCanon depProviders))

            let ambientShapes = (fun name -> depComposite.TryLookupType name)

            // The dependency providers' implicit open prefixes (`Vesper` from Core,
            // where `Fun`/`Fun2`/`Ref` live), so this package's extraction resolves a
            // dependency's ambiently-available type by bare name — mirroring the
            // consumer composite's `AmbientOpenPrefixes`. Dedup, dependency order.
            let depAmbientPrefixes =
                depProviders |> List.collect (fun p -> p.AmbientOpenPrefixes) |> List.distinct

            // Qualify `Result.Ok`/`Error`: `open ...SemanticAnalysis` shadows bare cases.
            match ReferencedProject.buildProviderWith target ambientShapes depAmbientPrefixes path with
            | Result.Ok(provider, _) ->
                built.Add provider
                byPath.[key] <- provider
            | Result.Error e -> failwithf "Failed to load referenced project manifest '%s': %s" path e

        // The final composite's leaf IS seeded with the full harvested reverse map, so a
        // consumer's BCL member sigs canonicalize (`System.Int32 → int`).
        let builtList = List.ofSeq built
        ExternalSymbols.composite (builtList @ metaTail (foldReverseCanon builtList))

    let build (manifestPaths: string list) : IExternalSymbolProvider =
        let ordered, transitiveDeps = orderedManifestsWithDeps manifestPaths
        composeProviders bclMetaTail None ordered transitiveDeps

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
                         let provider = composeProviders metaTail target ordered transitiveDeps
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

    /// Provider stack for a manifest set, including cross-package inline bodies.
    let buildContract (manifestPaths: string list) : IExternalSymbolProvider =
        buildContractCached "bcl" bclMetaTail None manifestPaths |> fst

    /// `buildContract` for a specific target (`Some "js"` selects `inline-bodies-js`
    /// overrides). `None` is identical to `buildContract`.
    let buildContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        buildContractCached "bcl" bclMetaTail target manifestPaths |> fst

    /// `buildContractFor` with a backend-injected layer-2 `metaTail`. `cacheTag`
    /// prevents the backend's entry from aliasing the `"bcl"` entry. The injected tail
    /// is reverse-map-independent (a non-CLR backend supplies its own leaf), so it is
    /// wrapped as a constant factory.
    let buildContractWithMetadata
        (cacheTag: string)
        (metaTail: IExternalSymbolProvider list)
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider =
        buildContractCached cacheTag (fun _ -> metaTail) target manifestPaths |> fst

    /// Raw cross-package inline bodies by source name — introspection seam for tests.
    /// Production code uses the provider's inline-body channel.
    let contractInlineBodies (manifestPaths: string list) : Map<string, InlineBody> =
        buildContractCached "bcl" bclMetaTail None manifestPaths |> snd

    /// `contractInlineBodies` for a specific target — introspection seam for target tests.
    let contractInlineBodiesFor (target: string option) (manifestPaths: string list) : Map<string, InlineBody> =
        buildContractCached "bcl" bclMetaTail target manifestPaths |> snd
