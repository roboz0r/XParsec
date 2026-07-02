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
            // No source anchor exists for a whole-file lex/parse/driver failure.
            Key = NodeKey.ofSynthetic 0 NodeKind.SynthUnsupportedDecl
            Code = "DRV"
            Message = message
            Severity = Severity.Error
        }

    // The production parse chain (lex → reader → `FSharpAst.parse`), calling the
    // parser API directly — the driver takes no dependency on test plumbing. A
    // bare-expression fragment wraps as an `AnonymousModule`, matching the front
    // end's script posture.
    let private parse (source: string) : Result<Lexed * ImplementationFile<SyntaxToken>, Diagnostic list> =
        match Lexing.lexString source with
        | Result.Error e -> Error [ driverDiagnostic (sprintf "lex error: %A" e) ]
        | Result.Ok lexed ->
            let reader = Reader.ofLexed lexed source Set.empty

            match FSharpAst.parse reader with
            | Result.Error e -> Error [ driverDiagnostic (sprintf "parse error: %A" e) ]
            | Result.Ok(FSharpAst.ImplementationFile f) -> Ok(lexed, f)
            | Result.Ok(FSharpAst.ScriptFragment(ScriptFragment.ScriptFragment elems)) ->
                Ok(lexed, ImplementationFile.AnonymousModule elems)
            | Result.Ok other -> Error [ driverDiagnostic (sprintf "unexpected AST: %A" other) ]

    /// Compile `source` to an in-memory PE artifact against the compilation's own
    /// reference set. Front end: `Pipeline.analyseFor` — the frozen production
    /// entry. A driver program is an FSharp.Core-front-end CONSUMER of the Vesper
    /// packages (like the package-consuming test harnesses), so it runs the default
    /// (non-self-host) front end, and it needs only the frozen tree, so it takes the
    /// one production call rather than the `analyseSemFor` + `Freeze.run` split the
    /// tests use to also return the pre-freeze `SemType` tree for assertions.
    let compile (inputs: ClrCompilation) (source: string) : Result<ClrArtifact, Diagnostic list> =
        match parse source with
        | Error ds -> Error ds
        | Ok(lexed, file) ->
            let provider =
                ClrSymbolProviders.buildContractWithRefs inputs.BclReferences None inputs.Manifests

            let tast =
                Pipeline.analyseFor inputs.Project.AssemblyName provider source lexed file

            match tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error) with
            | [] -> Ok(Codegen.compileWithBclReferences inputs.BclReferences provider inputs.Project tast)
            | errors -> Error errors

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
