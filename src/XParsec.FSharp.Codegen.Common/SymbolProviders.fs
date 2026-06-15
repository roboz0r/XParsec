namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
/// This is the **single declaration** consumed by *both* the front end and the
/// back end (option A in the P1 handoff): a driver builds the stack once and
/// threads the same `IExternalSymbolProvider` through `Pipeline.analyse` and
/// `Codegen.compile`, replacing today's split where each phase reached for
/// `MockBuiltins.provider` independently. Codegen still ignores the provider in;
/// passing it now is what proves the wiring.
module SymbolProviders =

    /// Compose the layer-1 referenced-project providers (each stood up from its
    /// `manifest.toml`) ahead of the layer-2 referenced-assembly provider:
    ///
    ///   `composite [ layer-1 manifests… ; layer-2 metadata ]`
    ///
    /// Layer 2 (referenced assemblies via `MetadataLoadContext`) is P2: the shared
    /// `MetadataSymbols.provider` reads the host runtime's BCL reflection-only, so a
    /// type like `EqualityComparer`1` and its members resolve here when no manifest
    /// owns them. It answers only namespace-qualified metadata names (and no values).
    ///
    /// **Contract-as-provider demotion is now total.**
    /// `MockBuiltins` is GONE from this stack entirely — its bare-name registrations
    /// (operators, `hash`, `failwith`, the printf family) used to shadow the contract
    /// from behind, and the final `List.fold` backstop is retired too. Every symbol
    /// resolves from the `Vesper.*` `.fsi` contracts via the ambient open scope:
    /// operators + `hash` + `failwith` from `Vesper.Core`, the ordering operators
    /// from `Vesper.Comparison`, the printf family from `Vesper.Printf`, and
    /// `List.fold` from `Vesper.List` — the last needed the `ModuleSuffix` module's
    /// members to be addressable by their *source* name (`List.fold`, not the
    /// compiled `ListModule.fold`; `VesperLib.extractValSig`) and codegen to accept
    /// the contract's `'T list` abbreviation name alongside the union name
    /// (both resolve to the one cons-list `SymbolKey` — `RuntimeNames.isVesperListKey`).
    ///
    /// `ProjectInfo.References` is not yet classified (a flat DLL-path list with no
    /// link to its source manifest), so the layer-1 manifests are supplied
    /// explicitly by the driver (handoff §7), and layer 2 reads the host runtime
    /// (`MetadataSymbols.runtimeAssemblyPaths`, a first cut — §6/§9). The
    /// classification that lets a `forProject : ProjectInfo -> _` derive both the
    /// layer-1 manifests and the layer-2 reference paths from the reference set is
    /// the remaining pairing.
    /// Close `manifestPaths` over `depends-on` and return them in dependency order
    /// A dependency a root only names
    /// transitively is pulled in, and every package is processed *after* the
    /// packages it depends on. A `depends-on` cycle or a missing dependency
    /// manifest is a hard error. Both `build` and `buildContract` thread the same
    /// ordered list so the composite provider and the inline-body loader agree on
    /// the package set.
    /// Dependency-ordered manifests plus each package's transitive `depends-on`
    /// closure (`transitiveDeps key` → its normalised dependency manifest paths).
    /// `composeProviders` scopes a package's ambient to these declared dependencies
    /// rather than all topological predecessors.
    let private orderedManifestsWithDeps (manifestPaths: string list) : string list * (string -> string list) =
        match ReferencedProject.buildClosureWithDeps manifestPaths with
        | Result.Ok(ordered, transitiveDeps) -> ordered, transitiveDeps
        | Result.Error e -> failwithf "Failed to order referenced project manifests: %s" e

    /// Compose the layer-1 providers for an *already dependency-ordered* manifest
    /// list ahead of the layer-2 metadata provider. Each package is built bottom-up
    /// with read access to the type shapes of the packages already built — its
    /// dependencies, which dependency order guarantees precede it
    /// This is what lets extraction kind a
    /// cross-package nominal head at bake time, instead of leaving a
    /// placeholder for the consumer to reconcile.
    ///
    /// Builds via `buildProviderWith` rather than the per-path `ReferencedProject.provider`
    /// cache, because a package's extraction now depends on its dependency shapes;
    /// the whole composite is still memoised per manifest set by `buildContract`'s
    /// `contractCache`, so each set is built once.
    let private composeProviders
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
            // The shapes in scope = the composite `TryLookupType` of *this package's
            // transitive `depends-on` closure* and layer-2 metadata (the BCL
            // via `MetadataSymbols.provider`). Scoping to declared dependencies — not
            // every topological predecessor — keeps a package from silently kinding a
            // cross-package head it never declared a `depends-on` for. The closure is
            // dependency-ordered and every member is already built (dependency order),
            // so this indexed lookup is O(closure). Layer-2 must be in scope too so a
            // contract naming a raw BCL nominal head not aliased in its own package
            // (e.g. `System.Text.StringBuilder` with no `extern` companion) kinds
            // correctly at bake time, rather than baking a spurious `TyUnknown` for a
            // type the consumer resolves through layer-2 anyway. So the ambient is
            // literally "the same `TryLookupType` the consumer would see, restricted
            // to this package's dependency closure".
            let depProviders =
                transitiveDeps key
                |> List.choose (fun dep ->
                    match byPath.TryGetValue dep with
                    | true, p -> Some p
                    | _ -> None
                )

            let depComposite =
                ExternalSymbols.composite (depProviders @ [ MetadataSymbols.provider ])

            let ambientShapes = (fun name -> depComposite.TryLookupType name)

            // `Result.Ok`/`Error` are qualified: `open ...SemanticAnalysis`
            // brings `Severity.Error` into scope, shadowing the bare cases.
            match ReferencedProject.buildProviderWith ambientShapes path with
            | Result.Ok(provider, _) ->
                built.Add provider
                byPath.[key] <- provider
            | Result.Error e -> failwithf "Failed to load referenced project manifest '%s': %s" path e

        ExternalSymbols.composite (List.ofSeq built @ [ MetadataSymbols.provider ])

    let build (manifestPaths: string list) : IExternalSymbolProvider =
        let ordered, transitiveDeps = orderedManifestsWithDeps manifestPaths
        composeProviders ordered transitiveDeps

    /// The inline `val` bindings a referenced project contributes whose `.fs`
    /// bodies must be *spliced* at the consumer's use site — a cross-package
    /// inline (milestone M). Keyed by the binding's
    /// source name (the same name the use-site `TExpr.External` carries); the
    /// value is the frozen `TDecl.Let(isInline=true)` the pre-freeze
    /// `Passes.InlineExpansion` pass expands in place of an `External(name)` call
    /// head (served to it through the provider's `IInlineBodyProvider` channel).
    ///
    /// `hash` is the first such body: `let inline hash (obj: 'T) =
    /// EqualityComparer<'T>.Default.GetHashCode obj` (`ops-platform.fs`). It is a
    /// normal identifier, so it clears the operator-named-binding freeze gap that
    /// still blocks `=`/`+`/… (operators-plan.md). The arithmetic/equality
    /// operators have no `.fs` body yet and stay on the `Emit.BuiltinOps` stopgap.
    let private collectInlineBodies (ctx: PassContext) (tast: TastFile) : (string * InlineBody) list =
        let acc = ResizeArray<string * InlineBody>()

        // Pre-pass: every module-level inline binding's binder NodeKey → its
        // source name. One inline body may reference *another* (failwith calls
        // raise; both are sibling top-level `let inline` in `module Operators`),
        // and the frozen body carries that reference as a plain `TExpr.Var`
        // bound to the binder's source key. Spliced at a cross-package use site
        // those keys aren't in scope; rewriting them to `TExpr.External(name,
        // …)` here lets `Passes.InlineExpansion` route the inner call through the
        // same per-name splice path as the outer
        // one. `hash` is the only inline that pre-dates this case and has no
        // sibling-inline calls, so it round-trips unchanged.
        let inlineNames = System.Collections.Generic.Dictionary<uint64, string>()

        for d in tast.Decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                match Map.tryFind k.Raw tast.ModuleMembers with
                | Some info -> inlineNames.[k.Raw] <- info.Name
                | None -> ()
            | _ -> ()

        let rewriteInlineVars (e: TExpr) : TExpr =
            let mapper: TastWalk.Mapper =
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun _ e ->
                            match e with
                            | TExpr.Var(k, ty, tok) ->
                                match inlineNames.TryGetValue k.Raw with
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
            // A module-level `let inline` resolves its source name through
            // `ModuleMembers` (the same map `Emit.collectStaticFns` names static
            // methods from); a top-level inline with no named-module placement is
            // unaddressable from a use site, so it is skipped.
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                match Map.tryFind k.Raw tast.ModuleMembers with
                | Some info ->
                    // The parameter attributes (`[<CallAtMostOnce>]` &c.) were
                    // validated + recorded against this binder key while the body's
                    // own package was analysed; carry them across the boundary so
                    // the consumer's inliner honours them without re-decoding.
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

    /// Load the cross-package inline bodies declared by the manifests' `impl`
    /// `.fs` files. Each body is type-checked + frozen ONCE here, against the same
    /// `provider` stack the consumer uses, so its `EqualityComparer<'T>` access
    /// already freezes to keyed `TExpr.ExternalMember` nodes (P3) the consumer
    /// emits verbatim (P4) — the consumer never re-resolves them. `provider` MUST
    /// be `build manifestPaths` (it resolves both the contract's own types — `int`
    /// from `prim-types-min.fsi` — and the BCL members the bodies reach).
    ///
    /// A later body wins on a name clash (the manifests are processed in order),
    /// matching `composite`'s first-listed-source priority for symbol lookup. A
    /// parse / impl-file-shape failure contributes no body (it surfaces as the
    /// emit-time "no inline body / no recipe" failure at the use site, not here).
    /// `target` selects the per-target `inline-bodies-<t>` override
    /// (`resolveInlineBodies`): `None` is the base list (the CLR path); `Some "js"`
    /// picks `ops-platform.js.fs` &c. where a package declares it, falling back to
    /// the base list per-package.
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
                            // The inline bodies belong to the referenced package, so its
                            // own assembly name (`manifest.Name`) is the home assembly for
                            // any local nominal keys minted while analysing them.
                            // Cross-package inline bodies must re-enter the *SemType*
                            // inline pass at the consumer, so collect them off the
                            // pre-freeze (`analyseSemFor`) tree, not the frozen output.
                            // With-context variant: `collectInlineBodies` reads the
                            // run's `ctx.InlineParamAttrs` to bake each body's
                            // parameter attributes into its `InlineBody`.
                            let ctx, tast =
                                Pipeline.analyseSemWithContextFor manifest.Name provider parsed.Input parsed.Lexed f

                            for (name, body) in collectInlineBodies ctx tast do
                                acc <- Map.add name body acc

        acc


    /// Lazy cache keyed by the normalised manifest set so a *suite* of compiles
    /// parses + analyses each contract `.fsi`/`.fs` once, not once per compile
    /// `ReferencedProject.provider` already caches each manifest's
    /// `.fsi` parse and `MetadataSymbols.provider` is process-wide, so the only
    /// previously-uncached cost was `inlineBodies` re-analysing each `impl` `.fs`
    /// against the stack on every call — this caches that, plus the per-set
    /// `composite`.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<IExternalSymbolProvider * Map<string, InlineBody>>
         >(
            System.StringComparer.Ordinal
        )

    /// Wrap `inner` so it ALSO serves cross-package inline bodies
    /// every `IExternalSymbolProvider` member delegates
    /// to `inner`, and the two inline-body channels read the pre-built maps.
    /// `byKey` is keyed by the inline value's resolved `SymbolKey` (the
    /// identity-robust primary channel); `byName` is the source-name residue the
    /// front-end pass falls back to for `External` heads still carrying
    /// `key = ValueNone`. The front end reads these straight off `ctx.Provider`
    /// (the `TryLookupInlineBody` / `…ByName` members are now part of
    /// `IExternalSymbolProvider`, so no cast is needed).
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
        }

    /// Build the provider stack AND load its cross-package inline bodies for a
    /// manifest set, caching both. The provider and the inline `Map` are a matched
    /// pair — the bodies were frozen against that exact stack. Production code
    /// reaches the bodies through the provider's inline-body channel
    /// (`buildContract`); the raw `Map` (`contractInlineBodies`) is an
    /// introspection seam for the inline-body collection tests.
    let private buildContractCached
        (target: string option)
        (manifestPaths: string list)
        : IExternalSymbolProvider * Map<string, InlineBody> =
        let normalised = manifestPaths |> List.map Path.GetFullPath
        // The target is part of the cache identity: the JS and CLR collections of
        // the same manifest set freeze different `inline-bodies` files.
        let key = (defaultArg target "") + "|" + String.concat ";" normalised

        contractCache
            .GetOrAdd(
                key,
                fun _ ->
                    lazy
                        // The base provider already surfaces referenced-package
                        // intrinsic reprs as `ExternalTypeShape.Intrinsic` shapes
                        // (the SA-layer `ReferencedProject` extractor pairs each
                        // `.fsi` extern with its sibling `.fs` `(# … #)` binding);
                        // no codegen-layer harvest wrap is needed.
                        // Close + order the manifest set ONCE and thread
                        // the same ordered list into the provider stack and the
                        // inline-body loader, so both see the full `depends-on`
                        // closure (a root's transitive dependency contributes its
                        // contract symbols AND its cross-package inline bodies).
                        (let ordered, transitiveDeps = orderedManifestsWithDeps normalised
                         let provider = composeProviders ordered transitiveDeps
                         let inlines = inlineBodies target provider ordered
                         // Rekey the inline bodies by the resolved `SymbolKey` the
                         // consumer's use-site `TExpr.External` carries — looked up
                         // through this same `provider`, so the body's key is exactly
                         // the key `Resolution.ExternalValue` stamps. A name with no
                         // resolvable symbol contributes only to
                         // the by-name residue. The wrapped provider serves both
                         // channels to the front-end inline pass (the sole consumer
                         // since beat (b) retired codegen's inline expansion); the raw
                         // `inlines` map is cached alongside it for `contractInlineBodies`
                         // (the inline-body collection tests).
                         let byKey =
                             System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                         for KeyValue(name, body) in inlines do
                             match provider.TryLookup name with
                             | ValueSome sym -> byKey.[sym.Key] <- body
                             | ValueNone -> ()

                         withInlineBodies provider byKey inlines, inlines)
            )
            .Value

    /// The provider stack for a manifest set, serving both the contract symbols
    /// and (via its `IInlineBodyProvider` channel) the cross-package inline
    /// bodies. The single entry point the default compile path uses once
    /// `MockBuiltins` is demoted to the backstop.
    let buildContract (manifestPaths: string list) : IExternalSymbolProvider =
        buildContractCached None manifestPaths |> fst

    /// `buildContract` for a specific backend target (`Some Target.Js` selects the
    /// `inline-bodies-js` `.fs` bodies via `ReferencedProject.resolveInlineBodies`),
    /// so the provider's `IInlineBodyProvider` channel splices the target's
    /// templated operator bodies (`($0 + $1) | 0` for int32 `+`) at the consumer's
    /// use site. `None` is identical to `buildContract` (the CLR base list). The
    /// matched inline `Map` is the target-keyed cache entry shared with
    /// `contractInlineBodiesFor`.
    let buildContractFor (target: string option) (manifestPaths: string list) : IExternalSymbolProvider =
        buildContractCached target manifestPaths |> fst

    /// The raw cross-package inline bodies collected for a manifest set, keyed by
    /// source name — an introspection seam for the inline-body collection tests.
    /// Production splices these through the provider's `IInlineBodyProvider`
    /// channel (see `buildContract`), never this map. Shares `buildContract`'s
    /// cache.
    let contractInlineBodies (manifestPaths: string list) : Map<string, InlineBody> =
        buildContractCached None manifestPaths |> snd

    /// `contractInlineBodies` for a specific target (`Some "js"` selects the
    /// `inline-bodies-js` files via `ReferencedProject.resolveInlineBodies`). The
    /// introspection seam the F1 / JS-target inline-body tests use to confirm a
    /// target's bodies freeze with their `$N` templates intact.
    let contractInlineBodiesFor (target: string option) (manifestPaths: string list) : Map<string, InlineBody> =
        buildContractCached target manifestPaths |> snd
