namespace XParsec.FSharp.Codegen.Clr

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack (symbol-resolution-plan §5, P1).
/// This is the **single declaration** consumed by *both* the front end and the
/// back end (option A in the P1 handoff): a driver builds the stack once and
/// threads the same `IExternalSymbolProvider` through `Pipeline.analyse` and
/// `Codegen.compile`, replacing today's split where each phase reached for
/// `MockBuiltins.provider` independently. Codegen still ignores the provider in
/// P1 (it reads it in P4); passing it now is what proves the wiring.
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
    /// **Contract-as-provider demotion is now total (symbol-resolution-handoff.md).**
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
    /// (`ClrProvider.isVesperListName`).
    ///
    /// `ProjectInfo.References` is not yet classified (a flat DLL-path list with no
    /// link to its source manifest), so the layer-1 manifests are supplied
    /// explicitly by the driver (handoff §7), and layer 2 reads the host runtime
    /// (`MetadataSymbols.runtimeAssemblyPaths`, a first cut — §6/§9). The
    /// classification that lets a `forProject : ProjectInfo -> _` derive both the
    /// layer-1 manifests and the layer-2 reference paths from the reference set is
    /// the remaining pairing.
    let build (manifestPaths: string list) : IExternalSymbolProvider =
        let layer1 =
            manifestPaths
            |> List.map (fun path ->
                // `Result.Ok`/`Error` are qualified: `open ...SemanticAnalysis`
                // brings `Severity.Error` into scope, shadowing the bare cases.
                match ReferencedProject.provider path with
                | Result.Ok(provider, _) -> provider
                | Result.Error e -> failwithf "Failed to load referenced project manifest '%s': %s" path e
            )

        ExternalSymbols.composite (layer1 @ [ MetadataSymbols.provider ])

    /// The inline `val` bindings a referenced project contributes whose `.fs`
    /// bodies must be *spliced* at the consumer's use site — a cross-package
    /// inline (milestone M, symbol-resolution-handoff.md). Keyed by the binding's
    /// source name (the same name the use-site `TExpr.External` carries); the
    /// value is the frozen `TDecl.Let(isInline=true)` the codegen `Emit.lower`
    /// expands in place of an `External(name)` call head.
    ///
    /// `hash` is the first such body: `let inline hash (obj: 'T) =
    /// EqualityComparer<'T>.Default.GetHashCode obj` (`ops-platform.fs`). It is a
    /// normal identifier, so it clears the operator-named-binding freeze gap that
    /// still blocks `=`/`+`/… (operators-plan.md). The arithmetic/equality
    /// operators have no `.fs` body yet and stay on the `Emit.BuiltinOps` stopgap.
    let private collectInlineBodies (tast: TastFile) : (string * TDecl) list =
        let acc = ResizeArray<string * TDecl>()

        for d in tast.Decls do
            match d with
            // A module-level `let inline` resolves its source name through
            // `ModuleMembers` (the same map `Emit.collectStaticFns` names static
            // methods from); a top-level inline with no named-module placement is
            // unaddressable from a use site, so it is skipped.
            | TDecl.Let(TPat.NamedSimple(k, _), _, true, _) ->
                match Map.tryFind k.Raw tast.ModuleMembers with
                | Some info -> acc.Add(info.Name, d)
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
    let inlineBodies (provider: IExternalSymbolProvider) (manifestPaths: string list) : Map<string, TDecl> =
        let mutable acc = Map.empty

        for manifestPath in manifestPaths do
            match ReferencedProject.loadManifest manifestPath with
            // A malformed manifest already failed `build`; nothing to add here.
            | Result.Error _ -> ()
            | Result.Ok manifest ->
                let dir = Path.GetDirectoryName manifestPath

                for rel in manifest.Impl do
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
                            let tast = Pipeline.analyse provider parsed.Input parsed.Lexed f

                            for (name, decl) in collectInlineBodies tast do
                                acc <- Map.add name decl acc

        acc

    /// Lazy cache keyed by the normalised manifest set so a *suite* of compiles
    /// parses + analyses each contract `.fsi`/`.fs` once, not once per compile
    /// (symbol-resolution-handoff.md "cache the contract analysis before
    /// flipping"). `ReferencedProject.provider` already caches each manifest's
    /// `.fsi` parse and `MetadataSymbols.provider` is process-wide, so the only
    /// previously-uncached cost was `inlineBodies` re-analysing each `impl` `.fs`
    /// against the stack on every call — this caches that, plus the per-set
    /// `composite`.
    let private contractCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Lazy<IExternalSymbolProvider * Map<string, TDecl>>>(
            System.StringComparer.Ordinal
        )

    /// Build the provider stack AND load its cross-package inline bodies for a
    /// manifest set, caching both. The returned `provider` and inline `Map` are a
    /// matched pair (the bodies were frozen against that exact stack — they MUST
    /// be threaded together to codegen). This is the single entry point the
    /// default compile path uses once `MockBuiltins` is demoted to the backstop.
    let buildContract (manifestPaths: string list) : IExternalSymbolProvider * Map<string, TDecl> =
        let normalised = manifestPaths |> List.map Path.GetFullPath
        let key = String.concat ";" normalised

        contractCache
            .GetOrAdd(
                key,
                fun _ ->
                    lazy
                        (let provider = build normalised
                         let inlines = inlineBodies provider normalised
                         provider, inlines)
            )
            .Value
