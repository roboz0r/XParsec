module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// P1 layer-1 gate (symbol-resolution-plan §5.1): stand up `Vesper.Core` from its
// real `manifest.toml` and confirm the contract resolves with a non-empty
// `Origin` stamped from the manifest — the first consumer of the P0 identity
// surface.

/// Locate `src/Vesper.Core/manifest.toml` by walking up from the test assembly.
let private vesperCoreManifest =
    let testDir = Path.GetDirectoryName(typeof<VesperLib.LibFile>.Assembly.Location)
    let mutable dir = DirectoryInfo testDir
    let mutable found = None

    while not (isNull dir) && found.IsNone do
        let candidate = Path.Combine(dir.FullName, "src", "Vesper.Core", "manifest.toml")

        if File.Exists candidate then
            found <- Some candidate

        dir <- dir.Parent

    match found with
    | Some p -> p
    | None -> failwithf "Could not locate src/Vesper.Core/manifest.toml from %s" testDir

/// `src/` directory holding the real Vesper package manifests (the parent of
/// `Vesper.Core`), and the sibling `Vesper.List` manifest (which `depends-on`
/// Vesper.Core) — the fixtures for the `buildClosure` dependency-ordering tests.
let private srcDir = Path.GetDirectoryName(Path.GetDirectoryName vesperCoreManifest)

let private vesperListManifest =
    Path.Combine(srcDir, "Vesper.List", "manifest.toml")

/// A throwaway `src/`-shaped tree under the repo `./tmp` for the synthetic
/// cycle / missing-dependency manifests (`buildClosure` resolves a `depends-on`
/// name to a *sibling* package directory, so the manifests must live side by
/// side). Each package gets `tmpSrc/<name>/manifest.toml`.
let private tmpSrc =
    let repoRoot = Path.GetDirectoryName srcDir
    let d = Path.Combine(repoRoot, "tmp", "buildClosure-tests", "src")
    Directory.CreateDirectory d |> ignore
    d

/// Write `tmpSrc/<name>/manifest.toml` with the given `depends-on` packages and
/// return its path. `files = []` keeps it parse-valid without any `.fsi`.
let private writeSyntheticManifest (name: string) (dependsOn: string list) : string =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    let deps = dependsOn |> List.map (sprintf "\"%s\"") |> String.concat ", "
    let path = Path.Combine(dir, "manifest.toml")

    File.WriteAllText(
        path,
        sprintf "[core]\nname = \"%s\"\nnamespace = \"%s\"\ndepends-on = [%s]\nfiles = []\n" name name deps
    )

    path

/// Build the manifest provider once for the run.
let private builtProvider =
    lazy
        (match ReferencedProject.buildProvider vesperCoreManifest with
         | Result.Error e -> failwithf "buildProvider failed: %s" e
         | Result.Ok(provider, diags) -> provider, diags)

[<Tests>]
let tests =
    testList
        "ReferencedProject"
        [
            test "manifest parses: name, namespace, files in compile order" {
                match ReferencedProject.loadManifest vesperCoreManifest with
                | Result.Error e -> failtestf "loadManifest failed: %s" e
                | Result.Ok m ->
                    // `Vesper.Core`'s `[core]` carries no `name`, so it falls back
                    // to the directory name.
                    Expect.equal m.Name "Vesper.Core" "assembly name from dir"
                    Expect.equal m.Namespace "Vesper" "namespace from [core]"
                    Expect.isNonEmpty m.Files "files listed"
                    Expect.equal (List.head m.Files) "prim-types-min.fsi" "compile order: prim-types-min first"
            }

            test "every listed .fsi parses (no file-level diagnostics)" {
                let _, diags = builtProvider.Value
                Expect.isEmpty diags (sprintf "expected clean parse, got: %A" diags)
            }

            test "int resolves (qualified) as an Intrinsic shape carrying its `.fs` repr" {
                let provider, _ = builtProvider.Value

                // Short names no longer resolve through the provider's own retry
                // (O3): they're resolved by the pipeline via the ambient prefix
                // set. The provider answers the qualified name directly. `int` is an
                // `extern` paired with its sibling `.fs` `(# "System.Int32" #)`
                // binding, so it surfaces as an `Intrinsic` carrying the CLI repr —
                // NOT an opaque `Class`. The repr is what codegen / `subsumes`
                // consume; an intrinsic carries no `Origin` (it keys off the repr
                // string, not an assembly ref) — intrinsic-repr-handoff.md.
                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic repr) ->
                    Expect.equal repr "System.Int32" "int carries its prim-types-min `.fs` representation"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }

            test "Fun resolves (qualified) as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // `type Fun<'A,'B>` carries a single abstract `Invoke`. It parses
                // as an anonymous (`= begin … end`) type, so the extractor records
                // a non-interface `Class` shape — SAM-interface detection from an
                // all-abstract body is a downstream (Freeze/codegen) concern. P1
                // only needs it to resolve with the package `Origin`.
                match provider.TryLookupType "Vesper.Fun" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Arity 2 "Fun has two typars"
                    Expect.equal info.Origin.Assembly (Some "Vesper.Core") "origin assembly = Vesper.Core"
                    Expect.equal info.Origin.Namespace "Vesper" "origin namespace = Vesper"
                | other -> failtestf "expected Vesper.Fun as Class shape, got %A" other
            }

            test "short names no longer resolve through the provider directly (O3)" {
                // The namespace retry moved out of the provider into the ambient
                // open scope (probed by the pipeline behind explicit opens), so a
                // bare `int` is a miss at the provider surface.
                let provider, _ = builtProvider.Value
                Expect.isTrue (provider.TryLookupType "int" |> ValueOption.isNone) "bare int is a provider miss"
            }

            test "the contract surfaces its [<AutoOpen>] modules + namespace as the ambient prefix set" {
                let provider, _ = builtProvider.Value
                let prefixes = provider.AmbientOpenPrefixes
                // `ops-platform.fsi`'s `[<AutoOpen>]` operator modules, plus
                // the package namespace as the trailing implicit prefix.
                Expect.isTrue
                    (List.contains "Vesper.ArithmeticOperators" prefixes)
                    "ArithmeticOperators auto-open surfaced"

                Expect.isTrue (List.contains "Vesper.Operators" prefixes) "Operators (hash) auto-open surfaced"
                Expect.isTrue (List.contains "Vesper" prefixes) "namespace surfaced as the trailing prefix"
                Expect.equal (List.last prefixes) "Vesper" "namespace is last (probed after the AutoOpen modules)"
            }

            test "an unknown type misses" {
                let provider, _ = builtProvider.Value
                Expect.isTrue (provider.TryLookupType "NoSuchType" |> ValueOption.isNone) "unknown type miss"
            }

            // O3 contract-ambient proof (symbol-resolution-handoff.md, open-resolution) and the
            // first step off MockBuiltins: a program resolves `+` and `hash`
            // purely through the contract's `[<AutoOpen>]` operator modules — the
            // `.fsi` source of truth — via the ambient open scope. No MockBuiltins
            // backstop, no explicit `open`.
            test "operators + hash resolve from the Vesper.Core contract, no MockBuiltins" {
                let provider, _ = builtProvider.Value

                let input = "let r = 1 + 2\nlet h = hash 5"
                let lexed, file = parseFile input
                let ctx = PassContext(provider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                // No resolution-failure diagnostics: `op_Addition` and `hash` each
                // found a home in the contract's auto-opened operator modules.
                let failures =
                    ctx.Diagnostics
                    |> Seq.filter (fun d -> d.Severity = Severity.Error)
                    |> Seq.map (fun d -> d.Message)
                    |> List.ofSeq

                Expect.isEmpty failures (sprintf "expected clean resolution against the contract, got: %A" failures)

                // `r : int` via the SRTP `(+)`'s `default ^T : int` chain — typed
                // entirely from the contract, proving the `.fsi` is authoritative.
                let rIdx = input.IndexOf "let r" + 4

                match ctx.Bindings.TypeVar.TryGetValue(NodeKey.ofSource rIdx NodeKind.PatIdent) with
                | ValueSome tv ->
                    match Unification.zonk (TyVar tv) with
                    | TyConst "int" -> ()
                    | other -> failtestf "Expected r : int, got %A" other
                | ValueNone -> failtest "no TypeVar for r"
            }

            // `buildClosure` closes a root manifest set over `depends-on` and orders it dependencies-first.
            testList
                "buildClosure"
                [
                    test "pulls a transitive dependency into the closure (List ⇒ + Core)" {
                        // `Vesper.List` only names `Vesper.Core` via `depends-on`; the
                        // closure resolves it (sibling directory) and includes it.
                        match ReferencedProject.buildClosure [ vesperListManifest ] with
                        | Result.Error e -> failtestf "buildClosure failed: %s" e
                        | Result.Ok ordered ->
                            let coreFull = Path.GetFullPath vesperCoreManifest
                            let listFull = Path.GetFullPath vesperListManifest
                            Expect.contains ordered coreFull "Core pulled into the closure"
                            Expect.contains ordered listFull "List itself present"
                            // dependency before dependent
                            Expect.isLessThan
                                (List.findIndex ((=) coreFull) ordered)
                                (List.findIndex ((=) listFull) ordered)
                                "Core ordered before List"
                    }

                    test "reorders a dependent-first input dependencies-first" {
                        // Caller lists List before its Core dependency; `buildClosure`
                        // returns Core first (post-order topo sort).
                        match ReferencedProject.buildClosure [ vesperListManifest; vesperCoreManifest ] with
                        | Result.Error e -> failtestf "buildClosure failed: %s" e
                        | Result.Ok ordered ->
                            let coreFull = Path.GetFullPath vesperCoreManifest
                            let listFull = Path.GetFullPath vesperListManifest

                            Expect.isLessThan
                                (List.findIndex ((=) coreFull) ordered)
                                (List.findIndex ((=) listFull) ordered)
                                "Core ordered before List despite being listed second"
                    }

                    test "empty root set closes to nothing" {
                        match ReferencedProject.buildClosure [] with
                        | Result.Ok [] -> ()
                        | other -> failtestf "expected Ok [], got %A" other
                    }

                    test "a depends-on cycle is a hard error" {
                        // Two synthetic siblings that depend on each other.
                        let a = writeSyntheticManifest "CycleA" [ "CycleB" ]
                        writeSyntheticManifest "CycleB" [ "CycleA" ] |> ignore

                        match ReferencedProject.buildClosure [ a ] with
                        | Result.Ok ordered -> failtestf "expected a cycle error, got Ok %A" ordered
                        | Result.Error e -> Expect.stringContains e "cycle" "error names the cycle"
                    }

                    test "a missing dependency manifest is a hard error" {
                        let p = writeSyntheticManifest "NeedsGhost" [ "NoSuchPackage" ]

                        match ReferencedProject.buildClosure [ p ] with
                        | Result.Ok ordered -> failtestf "expected a missing-dependency error, got Ok %A" ordered
                        | Result.Error e -> Expect.stringContains e "buildClosure" "error is surfaced from buildClosure"
                    }

                    // `buildClosureWithDeps` also reports each package's *transitive*
                    // `depends-on` closure, so `composeProviders` can scope a package's
                    // ambient to its declared dependencies (not all topological predecessors).
                    test "buildClosureWithDeps reports the transitive depends-on closure" {
                        // A → B → C: A names only B, B names only C. A's transitive
                        // closure must include C even though A never names it directly.
                        writeSyntheticManifest "ClosureC" [] |> ignore
                        writeSyntheticManifest "ClosureB" [ "ClosureC" ] |> ignore
                        let a = writeSyntheticManifest "ClosureA" [ "ClosureB" ]

                        match ReferencedProject.buildClosureWithDeps [ a ] with
                        | Result.Error e -> failtestf "buildClosureWithDeps failed: %s" e
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name = Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))
                            let depsA = transitiveDeps (pathOf "ClosureA")
                            Expect.contains depsA (pathOf "ClosureB") "A's direct dependency B"
                            Expect.contains depsA (pathOf "ClosureC") "A's transitive dependency C"
                            Expect.equal (transitiveDeps (pathOf "ClosureB")) [ pathOf "ClosureC" ] "B depends on C only"
                            Expect.equal (transitiveDeps (pathOf "ClosureC")) [] "C is dependency-free"
                    }

                    test "buildClosureWithDeps excludes a non-dependency that merely sorts earlier" {
                        // D and E are independent roots (neither depends on the other);
                        // D sorts earlier in the closure, but must NOT appear in E's deps.
                        writeSyntheticManifest "IndepD" [] |> ignore
                        let e = writeSyntheticManifest "IndepE" []

                        match ReferencedProject.buildClosureWithDeps [ Path.Combine(tmpSrc, "IndepD", "manifest.toml"); e ] with
                        | Result.Error err -> failtestf "buildClosureWithDeps failed: %s" err
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name = Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))
                            Expect.equal (transitiveDeps (pathOf "IndepE")) [] "E declares no dependency on D despite D sorting earlier"
                    }
                ]
        ]
