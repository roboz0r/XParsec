namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// A single CLR compilation's inputs, MSBuild-shaped: a future `dotnet build`
/// integration hands us these three lists already resolved.
///   - `Project` — the target-generic per-build config (headed for `Codegen.Common`).
///   - `Manifests` — the Vesper package-manifest (`.fsi` contract) stack.
///   - `BclReferences` — the compilation's OWN BCL surface (a TFM ref pack + the
///     `<Reference>`/`<PackageReference>` assemblies), read for `AssemblyRef`
///     identity rather than reflected off the codegen host. This is a DRIVER-level
///     input by design: it is BCL-exclusive and must NOT accrete onto `ProjectInfo`.
type ClrCompilation =
    {
        Project: ProjectInfo
        Manifests: string list
        BclReferences: string list
    }

/// The library-level production CLR driver: parse → analyse → gate → emit against
/// an EXPLICIT reference set, returning front-end errors rather than throwing. The
/// pluggable-backend CLI (`vesperc`) is a separate design effort; this is the
/// callable seam a `dotnet build` integration drives.
module ClrDriver =

    // Front-end failures (lex / parse / analysis) are user errors and surface as
    // `Diagnostic`s. Codegen exceptions are NOT caught: a malformed emission is a
    // compiler bug, not a user error, and must fail loudly.
    let private driverDiagnostic (message: string) : Diagnostic =
        {
            Code = "DRV"
            Message = message
            Severity = Severity.Error
            // A whole-file lex/parse/driver failure names no place in the file.
            Site = Site.Nowhere
        }

    /// Compile `source` to an in-memory PE artifact against the compilation's own
    /// reference set. Front end: `Pipeline.analyseFor` — the frozen production
    /// entry. A driver program is an FSharp.Core-front-end CONSUMER of the Vesper
    /// packages (like the package-consuming test harnesses), so it runs the default
    /// (non-self-host) front end, and it needs only the frozen tree, so it takes the
    /// one production call rather than the `analyseSemFor` + `Freeze.run` split the
    /// tests use to also return the pre-freeze `SemType` tree for assertions.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match Pipeline.parse "DRV" source with
        | Error ds -> Error ds
        | Ok(lexed, file) ->
            let provider =
                ClrSymbolProviders.buildContractWithRefs inputs.BclReferences None inputs.Manifests

            let tast =
                Pipeline.analyseFor inputs.Project.AssemblyName provider source lexed file

            match tast.Residue.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error) with
            | [] -> Ok(Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project tast)
            | errors -> Error errors

    /// `compile`, but routing the per-file front end through the frozen-compile cache
    /// (`store`). Opt-in: a caller enables caching only by passing a store; `compile` stays
    /// cache-free and byte-identical. Behaviour is otherwise identical to `compile` — a HIT
    /// (equal source + equal dependency signatures) skips parse + analyse + freeze and thaws
    /// the cached tree; a MISS runs the front end and stores it; an errored front end is
    /// returned as `Error` and NOT stored (`freezeResult`).
    ///
    /// The provider is built OUTSIDE the cache, on BOTH the hit and miss paths, because
    /// codegen consumes it (`compileWithBclReferences`) even on a hit — the cache elides only
    /// the front end that yields the frozen tree, never emission. This is sound: every
    /// dependency's exported signature is already folded into the cache key
    /// (`Hashing.fileInputHash source inputs.Manifests`), so a hit implies a provider
    /// equivalent to the one that produced the cached tree, hence identical codegen. The key's
    /// `QueryId.Freeze` / `Cache.CodeVersion` guards match the rest of the cache seam.
    let compileCached
        (store: ICacheStore)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.BclReferences None inputs.Manifests

        let key =
            {
                Query = QueryId.Freeze
                CodeVersion = Cache.CodeVersion
                Input = Hashing.fileInputHash source inputs.Manifests
            }

        FrozenCache.freezeResult
            store
            key
            (fun () ->
                match Pipeline.parse "DRV" source with
                | Error ds -> Error ds
                | Ok(lexed, file) ->
                    let tast =
                        Pipeline.analyseFor inputs.Project.AssemblyName provider source lexed file

                    match tast.Residue.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error) with
                    | [] -> Ok tast
                    | errors -> Error errors
            )
        |> Result.map (fun frozen ->
            Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project frozen
        )

    /// THE shared multi-file glue seam: analyse an ordered `(path, source)` list as one
    /// assembly through `analyse` (the front end — `Pipeline.analyseFor` for a package
    /// consumer, `analyseForSelfHost` for a BCL-only package), then emit ONE PE. A file
    /// that fails to parse, or any error-severity front-end diagnostic across the analysed
    /// units, is returned anchored to its own unit (path + in-unit line/col) rather than
    /// throwing. On a clean analysis the units' projected views compose ahead of `external`
    /// (so a cross-file call finds the prior unit's exported open signature, which codegen
    /// re-homes to the LOCAL `MethodDef`) and `Codegen.compileUnitsWithBclReferences` emits
    /// the assembly. `bclReferences` feeds emitted-`AssemblyRef` identity only.
    ///
    /// This bridges the SemanticAnalysis multi-file front end (`AssemblyUnits`) and CLR
    /// emission; the `ClrCompilation`-shaped `compileAssembly` and the package-build
    /// fixtures all route through it, so the front-end/compose/emit core exists once.
    let compileAssemblyWith
        (analyse: AssemblyUnits.AnalyseUnit)
        (bclReferences: string list)
        (external: IExternalSymbolProvider)
        (project: ProjectInfo)
        (files: (string * string) list)
        : Result<ClrArtifact, AssemblyUnits.AnchoredDiagnostic list> =
        let results =
            AssemblyUnits.analyseAssemblyWith analyse project.AssemblyName external files

        // A parse failure is fatal for the whole assembly: surface every failed unit's
        // diagnostics anchored to its own source (looked up by path from `files`).
        let parseFailures =
            results
            |> List.collect (
                function
                | Error e -> AssemblyUnits.unpositionedDiagnostics e.Path e.Diagnostics
                | Ok _ -> []
            )

        match parseFailures with
        | _ :: _ -> Error parseFailures
        | [] ->
            let units =
                results
                |> List.choose (
                    function
                    | Ok u -> Some u
                    | Error _ -> None
                )

            // Every analysed unit's error-severity diagnostics, anchored per-unit.
            let analysisErrors =
                AssemblyUnits.consolidatedDiagnostics units
                |> List.filter (fun a -> a.Diagnostic.Severity = Severity.Error)

            match analysisErrors with
            | _ :: _ -> Error analysisErrors
            | [] ->
                // Each unit's projected view (nearest-first) ahead of the external stack.
                let symbols =
                    ExternalSymbolProviders.composite ([ for u in units -> u.View ] @ [ external ])

                let tasts = [ for u in units -> u.Frozen ]
                Ok(Codegen.compileUnitsWithBclReferences bclReferences symbols project tasts)

    /// Compile an ordered multi-file assembly against the compilation's own reference
    /// set, MSBuild-shaped (`ClrCompilation`). The multi-file counterpart of `compile`:
    /// it runs the default (non-self-host) front end and threads `BclReferences` into
    /// both the contract provider and the emitted-`AssemblyRef` identity map. The
    /// single-`source` `compile` stays for script / fragment callers.
    let compileAssembly
        (inputs: ClrCompilation)
        (files: (string * string) list)
        : Result<ClrArtifact, AssemblyUnits.AnchoredDiagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.BclReferences None inputs.Manifests

        compileAssemblyWith Pipeline.analyseFor inputs.BclReferences provider inputs.Project files

    /// `compile`, then — when `Project.OutputPath` is set — `materialiseApp` to a
    /// runnable framework-dependent bundle (PE + `runtimeconfig.json` + the
    /// referenced assemblies the shared framework does not carry). An in-memory
    /// compilation (`OutputPath = None`) returns the artifact unwritten.
    let compileApp (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        compile inputs source
        |> Result.map (fun artifact ->
            match inputs.Project.OutputPath with
            | Some _ -> Materialise.materialiseApp inputs.Project artifact
            | None -> ()

            artifact
        )

    /// `compile` with `BclReferences` filled from `Project.TargetFramework` via
    /// `RefPack.resolve` — the no-MSBuild CONVENIENCE. Explicit `BclReferences`
    /// (via `compile`) remains the PRIMARY mechanism; a driver handed a resolved
    /// reference list should not route through the resolver. A missing TFM or an
    /// unresolved pack surfaces as a driver diagnostic rather than being swallowed.
    let compileForTfm (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match inputs.Project.TargetFramework with
        | None ->
            Error
                [
                    driverDiagnostic "compileForTfm requires ProjectInfo.TargetFramework to be set"
                ]
        | Some tfm ->
            match RefPack.resolve tfm with
            | Result.Error msg -> Error [ driverDiagnostic msg ]
            | Result.Ok dlls -> compile { inputs with BclReferences = dlls } source
