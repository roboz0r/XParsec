namespace XParsec.FSharp.Codegen.Js

open System
open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// The ESM module system as the assembly gate checks `[<Import>]` bindings against it.
[<RequireQualifiedAccess>]
module EsmModules =

    // A `[core] runtime` asset is a committed ESM module, read as JS text for its exported
    // names alone.
    let private esmDeclaredExport =
        Text.RegularExpressions.Regex(
            @"\bexport\s+(?:default\s+)?(?:async\s+)?(?:const|let|var|function|class)\b[\s*]*([A-Za-z_$][A-Za-z0-9_$]*)",
            Text.RegularExpressions.RegexOptions.Compiled
        )

    let private esmExportList =
        Text.RegularExpressions.Regex(@"\bexport\s*\{([^}]*)\}", Text.RegularExpressions.RegexOptions.Compiled)

    /// The names an ESM source publishes to an importer.
    let exportedNames (source: string) : Set<string> =
        set
            [
                for m in esmDeclaredExport.Matches source -> m.Groups.[1].Value

                for m in esmExportList.Matches source do
                    for spec in m.Groups.[1].Value.Split(',') do
                        // `a as b` publishes `b`; a bare `a` publishes itself.
                        match spec.Split([| " as " |], StringSplitOptions.None) with
                        | [| _; alias |] -> yield alias.Trim()
                        | _ -> yield spec.Trim()
            ]
        |> Set.remove ""

    /// The ESM implementation of `IRuntimeModules`: a specifier is `./` plus an
    /// extension-carrying file name, resolved against `assets`. An entry the manifest lists
    /// but disk lacks is absent from `assets` too, so an import of it is `NotListed`.
    let create (assets: RuntimeAsset list) : IRuntimeModules =
        let byName = Dictionary<string, RuntimeAsset>(StringComparer.Ordinal)

        for a in assets do
            byName.[a.FileName] <- a

        let provided = Dictionary<string, Set<string>>(StringComparer.Ordinal)

        { new IRuntimeModules with
            member _.Resolve(path: string) : ImportResolution =
                if not (path.StartsWith("./", StringComparison.Ordinal)) then
                    ImportResolution.Malformed
                else
                    let rel = path.Substring 2

                    if rel = "" || IO.Path.GetExtension rel = "" then
                        ImportResolution.Malformed
                    else
                        match byName.TryGetValue rel with
                        | true, asset -> ImportResolution.Resolved asset
                        | _ -> ImportResolution.NotListed

            member _.Provided(asset: RuntimeAsset) : Set<string> =
                match provided.TryGetValue asset.FileName with
                | true, names -> names
                | _ ->
                    let names = exportedNames asset.Source
                    provided.[asset.FileName] <- names
                    names
        }
