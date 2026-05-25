namespace XParsec.FSharp.Codegen.Clr

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
    /// `manifest.toml`) ahead of the layer-2 referenced-assembly provider and the
    /// test-only `MockBuiltins` backstop:
    ///
    ///   `composite [ layer-1 manifests… ; layer-2 metadata ; MockBuiltins ]`
    ///
    /// Layer 2 (referenced assemblies via `MetadataLoadContext`) is P2: the shared
    /// `MetadataSymbols.provider` reads the host runtime's BCL reflection-only, so a
    /// type like `EqualityComparer`1` and its members resolve here when no manifest
    /// owns them. It answers only namespace-qualified metadata names (and no values),
    /// so the front end's short-name / operator probes still fall through to
    /// `MockBuiltins`, which stays lowest-priority until the contract owns those
    /// (symbol-resolution-plan §10).
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

        ExternalSymbols.composite (layer1 @ [ MetadataSymbols.provider; MockBuiltins.provider ])
