namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// A single CLR compilation's inputs, MSBuild-shaped: a future `dotnet build`
/// integration hands us these already resolved.
///   - `Project` — the target-generic per-build config (headed for `Codegen.Common`).
///   - `Manifests` — the Vesper package-manifest (`.fsi` contract) stack it REFERENCES.
///   - `BclReferences` — the compilation's OWN BCL surface (a TFM ref pack + the
///     `<Reference>`/`<PackageReference>` assemblies), read for `AssemblyRef`
///     identity rather than reflected off the codegen host. This is a DRIVER-level
///     input by design: it is BCL-exclusive and must NOT accrete onto `ProjectInfo`.
///   - `SelfManifest` — the manifest of the package this compilation IS, when it is
///     one. Distinct from `Manifests` in both direction and effect: a referenced
///     package is RESOLVED against, while the self package is being DEFINED, and its
///     `(# … #)` reprs seed the metadata leaf so a BCL signature presents its own
///     primitives as its own canon identities. `None` for a consumer.
type ClrCompilation =
    {
        Project: ProjectInfo
        Manifests: string list
        BclReferences: string list
        SelfManifest: string option
    }

module ClrCompilation =

    /// A consumer compilation: references packages, defines no primitives of its own.
    /// The overwhelmingly common shape, and the one a caller should reach for unless it
    /// is compiling a package that declares `extern` types.
    let consumer (project: ProjectInfo) (manifests: string list) (bclReferences: string list) : ClrCompilation =
        {
            Project = project
            Manifests = manifests
            BclReferences = bclReferences
            SelfManifest = None
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
        Diagnostic.nowhere (Kind.Driver message)

    /// The ANALYSIS diagnostics that block emission for one source. A recovered parse is
    /// refused upstream by `Pipeline.parseUnrecovered`, so a tree that reaches here was
    /// written as it stands and only the front end can still refuse it. One definition,
    /// because `compile` and `compileCached` claim to gate identically and two copies is
    /// how that stops being true.
    let private blockingErrors (tast: FrozenPools) : Diagnostic list =
        Diagnostic.errors tast.Residue.Diagnostics

    /// Compile `source` to an in-memory PE artifact against the compilation's own
    /// reference set. Front end: `Pipeline.analyseFor` — the frozen production
    /// entry. A driver program is an FSharp.Core-front-end CONSUMER of the Vesper
    /// packages (like the package-consuming test harnesses), so it runs the default
    /// (non-self-host) front end, and it needs only the frozen tree, so it takes the
    /// one production call rather than the `analyseSemFor` + `Freeze.run` split the
    /// tests use to also return the pre-freeze `SemType` tree for assertions.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match Pipeline.parseUnrecovered source with
        | Error diagnostics -> Error diagnostics
        | Ok parsed ->
            let provider =
                ClrSymbolProviders.buildContractWithRefs inputs.SelfManifest inputs.BclReferences None inputs.Manifests

            let tast =
                Pipeline.analyseFor inputs.Project.AssemblyName provider source parsed.Lexed parsed.File

            match blockingErrors tast with
            | [] -> Ok(Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project tast)
            | errors -> Error errors

    /// The CLR target suffix: none. ONE binding for the provider build and for the cache
    /// digest, because `buildContractWithRefs` selects the per-target manifest lists with it
    /// and a digest naming a different one would let two targets' trees alias in the store.
    /// They are computed in two different functions now, which is what makes sharing the
    /// binding load-bearing rather than tidy.
    let private clrTarget: string option = None

    /// This compilation's cache digest — everything a cached front end depends on EXCEPT one
    /// file's text. Fold it once and hand it to `compileCachedWith` for every file of the
    /// compilation: it stats and reads the whole referenced-package source closure, which is
    /// the same order of magnitude as the front end a hit elides, so folding it per file
    /// would spend most of what the cache saves.
    let compilationDigest (inputs: ClrCompilation) : Hashing.CompilationDigest =
        Hashing.compilationDigest
            {
                HomeAssembly = inputs.Project.AssemblyName
                Target = clrTarget
                ReferenceAssemblies = inputs.BclReferences
                Manifests = inputs.Manifests
                SelfManifest = inputs.SelfManifest
            }

    /// `compile`, but routing the per-file front end through the frozen-compile cache
    /// (`store`) against an already-folded compilation `digest`. Opt-in: a caller enables
    /// caching only by passing a store; `compile` stays cache-free and byte-identical.
    /// Behaviour is otherwise identical to `compile` — a HIT (equal source + equal digest)
    /// skips parse + analyse + freeze and thaws the cached tree; a MISS runs the front end
    /// and stores it; an errored front end is returned as `Error` and NOT stored
    /// (`freezeResult`).
    ///
    /// The digest is a PARAMETER so that a driver compiling many files pays for it once. That
    /// is the whole reason this and `compileCached` are two functions.
    ///
    /// The provider is built OUTSIDE the cache, on BOTH the hit and miss paths, because
    /// codegen consumes it (`compileWithBclReferences`) even on a hit — the cache elides only
    /// the front end that yields the frozen tree, never emission. This is sound only because
    /// the key covers every input the provider is built from as well as the source:
    /// `Hashing.CompilationInputs` names them, and `compilationDigest` fills them from
    /// exactly the four arguments handed to `buildContractWithRefs` plus the home assembly
    /// `analyseFor` roots its minted keys at. A hit therefore implies a provider equivalent to
    /// the one that produced the cached tree, hence identical codegen. The key's
    /// `QueryId.Freeze` / `Cache.CodeVersion` guards match the rest of the cache seam.
    ///
    /// A digest folded from OTHER inputs than `inputs` is the one way to misuse this, and it
    /// is the reason `compilationDigest` takes the same `ClrCompilation` this does.
    let compileCachedWith
        (store: ICacheStore)
        (digest: Hashing.CompilationDigest)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        let provider =
            ClrSymbolProviders.buildContractWithRefs inputs.SelfManifest inputs.BclReferences clrTarget inputs.Manifests

        let key =
            {
                Query = QueryId.Freeze
                CodeVersion = Cache.CodeVersion
                Input = Hashing.fileInputHash source digest
            }

        FrozenCache.freezeResult
            store
            key
            (fun () ->
                match Pipeline.parseUnrecovered source with
                | Error diagnostics -> Error diagnostics
                | Ok parsed ->
                    let tast =
                        Pipeline.analyseFor inputs.Project.AssemblyName provider source parsed.Lexed parsed.File

                    match blockingErrors tast with
                    | [] -> Ok tast
                    | errors -> Error errors
            )
        |> Result.map (fun frozen ->
            Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project frozen
        )

    /// `compileCachedWith` for a ONE-FILE compilation, folding the digest inline. Compiling
    /// several files through this would re-read the whole dependency closure per file — hoist
    /// `compilationDigest` and call `compileCachedWith` instead.
    let compileCached
        (store: ICacheStore)
        (inputs: ClrCompilation)
        (source: string)
        : Result<ClrArtifact, Diagnostic list> =
        compileCachedWith store (compilationDigest inputs) inputs source

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
                | Error e -> AssemblyUnits.failureDiagnostics e
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
            ClrSymbolProviders.buildContractWithRefs inputs.SelfManifest inputs.BclReferences None inputs.Manifests

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
