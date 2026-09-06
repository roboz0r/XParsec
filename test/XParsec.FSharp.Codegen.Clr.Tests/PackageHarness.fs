module XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// A caller lists the Vesper packages a snippet links against; the contract stack, the
// reference DLLs and the whole `depends-on` graph are derived, with the default stack
// unioned in. The per-package wrappers below fix `packages` for one suite each.

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// `defaultPackages` by package name: what every driver implicitly links.
let private defaultPackageNames =
    [ "Vesper.Core"; "Vesper.List"; "Vesper.Comparison"; "Vesper.Printf" ]

/// Transitive `depends-on` closure of `roots`, dependencies before dependents, deduped.
/// Drives both the contract stack and the `References` DLL list.
let private transitivePackages (roots: string list) : string list =
    let acc = System.Collections.Generic.List<string>()

    let rec go (pkg: string) =
        if not (acc.Contains pkg) then
            let m =
                ReferencedProject.resolveManifest Target.Clr (srcPackage pkg)
                |> Result.bind ReferencedProject.loadManifest
                |> PackageFaults.okOrFail (sprintf "transitivePackages %s" pkg)

            m.DependsOn |> List.map dependencyName |> List.iter go

            if not (acc.Contains pkg) then
                acc.Add pkg

    roots |> List.iter go
    List.ofSeq acc

/// Uniquifies a per-call driver assembly name: Expecto runs in parallel and `packageAlc`
/// is process-persistent, so two identically-named loads would collide on identity.
let private driverCounter = ref 0

/// Compile `src` against `packages` unioned with the default stack. Every package in the
/// transitive `depends-on` closure is built once and registered in `packageAlc`; its `.fsi`
/// joins the contract stack, its DLL the `References`.
let compilePackages (packages: string list) (src: string) : ClrArtifact =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let depDlls = allPackages |> List.map packageOutputPath
    let provider = ClrSymbolProviders.buildContract (allPackages |> List.map srcPackage)
    let n = System.Threading.Interlocked.Increment driverCounter

    let project =
        { ProjectInfo.defaults (sprintf "PkgDriver%d" n) with
            References = depDlls
        }

    compileAgainst provider project src

/// The Vesper-compiled `Vesper.Printf`, registered in `packageAlc` once. `buildPackage` leaves
/// it out, so without this a driver run here would reach printf through Default, which
/// implements a DIFFERENT `Vesper.Core` identity than its own package types.
let private packageAlcPrintf: Lazy<unit> =
    lazy
        (use ms =
            new IO.MemoryStream(IO.File.ReadAllBytes(packageOutputPath "Vesper.Printf"))

         packageAlc.Register("Vesper.Printf", packageAlc.LoadFromStream ms))

/// Compile `src` against `packages` and run its entry point inside `packageAlc`, so driver,
/// dependencies and printf share ONE `Vesper.Core` identity. Returns (exitCode, stdout)
/// plus the emitted bytes, for a caller asserting on the IL in the same pass as the run.
let runPackagesInspect (packages: string list) (src: string) : (int * string) * byte[] =
    packageAlcPrintf.Value
    let artifact = compilePackages packages src
    let bytes = Codegen.toBytes artifact
    use ms = new IO.MemoryStream(bytes)
    let asm = packageAlc.LoadFromStream ms
    runLoadedEntryPoint asm, bytes

/// `runPackagesInspect` without the bytes.
let runPackages (packages: string list) (src: string) : int * string = runPackagesInspect packages src |> fst

/// Compile + run `src` against `packages`; assert exit 0 and trimmed, CRLF-normalised
/// stdout equals `expected`.
let runsPackages (packages: string list) (expected: string) (src: string) : unit =
    runPackages packages src |> expectRan Whole expected src

/// `runsPackages` for a multi-line expected block (joined with "\n").
let runsPackagesLines (packages: string list) (expected: string list) (src: string) : unit =
    runsPackages packages (String.concat "\n" expected) src

/// Analyse `src` against the default stack plus `packages`, no codegen, and return the
/// error-severity diagnostics.
let private analysePackagesErrors (packages: string list) (src: string) : Diagnostic list =
    let allPackages = transitivePackages (defaultPackageNames @ packages)

    let provider = ClrSymbolProviders.buildContract (allPackages |> List.map srcPackage)

    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

    tast.Diagnostics |> Diagnostic.errors

/// Analyse `src` against `packages`; assert NO error diagnostics.
let typeChecksPackages (packages: string list) (src: string) : unit =
    match analysePackagesErrors packages src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

/// Analyse `src` against `packages`; assert an error diagnostic whose message
/// contains `fragment`.
let failsWithPackages (packages: string list) (fragment: string) (src: string) : unit =
    match analysePackagesErrors packages src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

// ---- Per-package wrappers over the declarative core --------------------------
// A new package needs one wrapper line.

/// Vesper.Option — the option type + `Option` module (counterpart of `runs`).
let runsOption (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Option" ] expected src

/// `runsOption` for a multi-line expected block.
let runsOptionLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Option" ] expected src

/// Analyse `src` through the default contract stack (no codegen) and return the
/// error-severity diagnostics.
let private analyseErrors (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

    tast.Diagnostics |> Diagnostic.errors

/// Analyse `src`; assert an error diagnostic whose message contains `fragment`.
let failsWith (fragment: string) (src: string) : unit =
    match analyseErrors src with
    | [] -> failwithf "expected an error containing %A but analysis produced none for:\n%s" fragment src
    | errors ->
        if not (errors |> List.exists (fun d -> d.Message.Contains fragment)) then
            failwithf
                "expected an error containing %A but got %A for:\n%s"
                fragment
                (errors |> List.map (fun d -> d.Message))
                src

/// Analyse `src`; assert NO error diagnostics.
let typeChecks (src: string) : unit =
    match analyseErrors src with
    | [] -> ()
    | errors -> failwithf "expected no errors but got %A for:\n%s" (errors |> List.map (fun d -> d.Message)) src

let typeChecksOption (src: string) : unit =
    typeChecksPackages [ "Vesper.Option" ] src

let failsWithOption (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Option" ] fragment src

// ---- Vesper.Result wrappers --------------------------------------------------

/// Compile a `Vesper.Result` consumer and return the `ClrArtifact` without running it, for a
/// caller asserting on the emitted IL.
let compileResultArtifact (src: string) : ClrArtifact = compilePackages [ "Vesper.Result" ] src

/// Vesper.Result — the result type + `Result` module (counterpart of `runsOption`).
let runsResult (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Result" ] expected src

let runsResultLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Result" ] expected src

let typeChecksResult (src: string) : unit =
    typeChecksPackages [ "Vesper.Result" ] src

let failsWithResult (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Result" ] fragment src

// ---- Vesper.Choice wrappers --------------------------------------------------
// Choice is a pure-data struct union with no companion module, so a driver here is limited
// to construction + `match`.

let runsChoice (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Choice" ] expected src

let runsChoiceLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Choice" ] expected src

let typeChecksChoice (src: string) : unit =
    typeChecksPackages [ "Vesper.Choice" ] src

let failsWithChoice (fragment: string) (src: string) : unit =
    failsWithPackages [ "Vesper.Choice" ] fragment src

// ---- Vesper.Array wrappers ---------------------------------------------------
// BCL-only: `arr.[i]` / `arr.Length` / `Array.zeroCreate` / `[| … |]` lower to `ldelem` /
// `ldlen` / `newarr`.

let runsArray (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Array" ] expected src

let runsArrayLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Array" ] expected src

let typeChecksArray (src: string) : unit =
    typeChecksPackages [ "Vesper.Array" ] src

// ---- Vesper.Seq wrappers -----------------------------------------------------
// A driver's `seq<'T>` source is `System.Linq.Enumerable.Range(start, count)`, a real BCL
// `IEnumerable<int>`.

let runsSeq (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Seq" ] expected src

let runsSeqLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Seq" ] expected src

let typeChecksSeq (src: string) : unit = typeChecksPackages [ "Vesper.Seq" ] src

// ---- Vesper.Set wrappers -----------------------------------------------------
// A driver's HOF argument (`Set.fold` / `partition`'s folder) must be written CURRIED:
// `fun s -> fun x -> …`.

let runsSet (expected: string) (src: string) : unit =
    runsPackages [ "Vesper.Set" ] expected src

let runsSetLines (expected: string list) (src: string) : unit =
    runsPackagesLines [ "Vesper.Set" ] expected src
