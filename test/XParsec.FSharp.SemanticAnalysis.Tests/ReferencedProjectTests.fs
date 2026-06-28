module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Stand up `Vesper.Core` from its
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

/// The same `Vesper.Core` contract built for the JS target — no `.js.fs` capability
/// reprs, plus the `files-js` compat shim (`capabilities-compat.js.fsi`) appended. The
/// fixture for the single-faced-capability + BCL-compat-shim assertions.
let private builtProviderJs =
    lazy
        (match ReferencedProject.buildProviderWith (Some "js") (fun _ -> ValueNone) [] vesperCoreManifest with
         | Result.Error e -> failwithf "buildProviderWith (Some js) failed: %s" e
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
                // binding, so it surfaces as an `Intrinsic`: `canon` is the `.fsi`
                // name `int` (the front-end identity `subsumes` consumes), `platform`
                // is the CLI repr (what codegen consumes). An intrinsic carries no
                // `Origin` (it keys off the name, not an assembly ref).
                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic(canon = canon; platform = Some platform)) ->
                    Expect.equal canon "int" "int's canon identity is the `.fsi` name"

                    Expect.equal
                        platform
                        "System.Int32"
                        "int's platform face is its prim-types-min `.fs` CLI representation"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }

            test "the language-capability anchors resolve as dual-faced interface Classes carrying their CLR repr" {
                // On a CLR-target build `capabilities.fsi` declares
                // `disposable`/`equatable`/`comparable` as `extern with abstract member …`,
                // paired with their `capabilities.fs` `(# "<BCL interface>" #)` reprs. Each
                // surfaces as ONE dual-faced shape: a `Class{IsInterface=true}` with the
                // member surface PLUS a `CapabilityFace { Canon; Platform }` — `Canon` is the
                // `.fsi` short name, `Platform` is the CLR interface name. The generic ones
                // (`equatable`/`comparable`) carry the metadata backtick-arity suffix in BOTH
                // the lookup name (`Vesper.equatable`1`) and the `Platform` repr
                // (``System.IEquatable`1``) — exactly the string a metadata interface name
                // reconciles against for `disposable === System.IDisposable`. The reverse-canon
                // entry (`platform -> canon`) feeds `Engine.canonName`, the same path `exn`
                // rides. (Iteration has no anchor here — it rides the existing `seq`
                // abbreviation; see `ExternalSymbols.resolveCapabilities`.)
                let provider, _ = builtProvider.Value

                let expectCapability (lookup: string) (canonExpected: string) (platformExpected: string) =
                    match provider.TryLookupType lookup with
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        Expect.isTrue shape.IsInterface (sprintf "%s is an interface Class" lookup)

                        match shape.CapabilityFace with
                        | ValueSome face ->
                            Expect.equal face.Canon canonExpected (sprintf "%s canon is its `.fsi` short name" lookup)

                            Expect.equal
                                face.Platform
                                platformExpected
                                (sprintf "%s platform is its `.fs` CLR repr" lookup)
                        | ValueNone -> failtestf "%s must carry a CapabilityFace" lookup

                        Expect.equal
                            (Map.tryFind platformExpected provider.IntrinsicReverseCanon)
                            (Some canonExpected)
                            (sprintf "reverse-canon maps %s -> %s" platformExpected canonExpected)
                    | other -> failtestf "expected %s as a dual-faced Class shape, got %A" lookup other

                expectCapability "Vesper.disposable" "disposable" "System.IDisposable"
                expectCapability "Vesper.equatable`1" "equatable" "System.IEquatable`1"
                expectCapability "Vesper.comparable`1" "comparable" "System.IComparable`1"
            }

            test "JS build: capabilities are single-faced canonical; BCL spellings resolve through the compat shim" {
                // The JS-target build omits the `.js.fs` capability reprs and appends the
                // `capabilities-compat.js.fsi` shim (manifest `files-js`). So each capability
                // surfaces SINGLE-faced — the canonical `Vesper.disposable` interface `Class`
                // with `CapabilityFace = ValueNone` (no BCL type to reconcile to) — and the BCL
                // spelling resolves through the shim as an ABBREVIATION to that canonical, NOT a
                // fabricated `Class`. This is the JS half of the CLR/JS asymmetry: the contract
                // names no BCL type; the BCL spelling is quarantined to the optional shim.
                let provider, _ = builtProviderJs.Value

                let expectSingleFaced (lookup: string) =
                    match provider.TryLookupType lookup with
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        Expect.isTrue shape.IsInterface (sprintf "%s is an interface Class" lookup)

                        Expect.equal
                            shape.CapabilityFace
                            ValueNone
                            (sprintf "%s is canonical-only on JS (no platform face)" lookup)
                    | other -> failtestf "expected %s as a single-faced Class on JS, got %A" lookup other

                expectSingleFaced "Vesper.disposable"
                expectSingleFaced "Vesper.equatable`1"
                expectSingleFaced "Vesper.comparable`1"

                // The BCL spelling resolves through the compat shim — an Abbrev whose head is
                // the canonical capability — so `interface System.IDisposable` records the
                // canonical interface key on JS (the same key `caps.Disposable` resolves to).
                let expectShimAbbrev (bcl: string) (canonQualified: string) =
                    match provider.TryLookupType bcl with
                    | ValueSome(ExternalTypeShape.Abbrev(_, FTClass(key, _))) ->
                        Expect.equal
                            (SymbolKeyOps.qualifiedName key)
                            canonQualified
                            (sprintf "%s shim-abbreviates to the canonical %s" bcl canonQualified)
                    | other -> failtestf "expected %s as a compat-shim Abbrev to %s, got %A" bcl canonQualified other

                expectShimAbbrev "System.IDisposable" "Vesper.disposable"
                expectShimAbbrev "System.IEquatable`1" "Vesper.equatable`1"
                expectShimAbbrev "System.IComparable`1" "Vesper.comparable`1"
            }

            test "Fun resolves (qualified) as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // `type Fun<'A,'B>` carries a single abstract `Invoke`. It parses
                // as an anonymous (`= begin … end`) type, so the extractor records
                // a non-interface `Class` shape — SAM-interface detection from an
                // all-abstract body is a downstream (Freeze/codegen) concern. P1
                // only needs it to resolve with the package `Origin`.
                // Generic compiled names are arity-suffixed (`Fun`2`), matching the
                // emitted metadata name (`Vesper.Fun`2`) and the consumer's probe.
                match provider.TryLookupType "Vesper.Fun`2" with
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

            // O3 contract-ambient proof and the
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
                    | TyConst("int", _) -> ()
                    | other -> failtestf "Expected r : int, got %A" other
                | ValueNone -> failtest "no TypeVar for r"
            }

            // The §3b disposal-model flip: a `use` binder must implement `disposable`
            // (`System.IDisposable`), matching real F#. Resolved against the real
            // Vesper.Core contract so `caps.Disposable` is non-null (`Vesper.disposable`
            // ⇒ `System.IDisposable`). NOTE: this contract-only provider does NOT surface
            // BCL `System.IDisposable` as an interface (no metadata layer), so the
            // non-ref-struct interface-accepted direction can't be exercised here — it is
            // covered by the Codegen.Clr / Codegen.Js `UseTests` (full BCL / JS providers).
            let analyseErrors (provider: IExternalSymbolProvider) (input: string) =
                let lexed, file = parseFile input
                let ctx = PassContext(provider, input, lexed)
                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                ctx.Diagnostics
                |> Seq.filter (fun d -> d.Severity = Severity.Error)
                |> Seq.map (fun d -> d.Message)
                |> List.ofSeq

            let analyseDiagnostics (provider: IExternalSymbolProvider) (input: string) =
                analyseErrors provider input
                |> List.exists (fun m -> m.Contains "implement 'disposable'")

            test "`use` over a type with a `Dispose` method but no `disposable` impl is rejected (interface-required)" {
                let provider, _ = builtProvider.Value

                let input =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    member this.Dispose () = ()"
                            "let run () ="
                            "    use r = Res()"
                            "    ()"
                            "run ()"
                        ]

                Expect.isTrue
                    (analyseDiagnostics provider input)
                    "a duck-typed `Dispose` (no `System.IDisposable`) no longer qualifies for `use`"
            }

            // The ref-struct carve-out: a `[<IsByRefLike>]` type can't be boxed to
            // `IDisposable`, so a duck-typed pattern `Dispose()` is accepted (C#8
            // pattern-`using` parity) and disposed by calling its own method directly.
            // This path is provider-independent (no capability / BCL interface
            // resolution), so it is the unit-testable "accepted" direction here; the
            // non-ref-struct `interface System.IDisposable` accepted path needs a
            // BCL/JS provider and is covered by the Codegen.Clr / Codegen.Js `UseTests`
            // (both type-check it AND dispose it at runtime).
            test "`use` over a `[<IsByRefLike>]` ref struct with a pattern `Dispose` is accepted (carve-out)" {
                let provider, _ = builtProvider.Value

                let input =
                    String.concat
                        "\n"
                        [
                            "[<Struct; System.Runtime.CompilerServices.IsByRefLike>]"
                            "type Res ="
                            "    member this.Dispose () = ()"
                            "let run () ="
                            "    use r = Res()"
                            "    ()"
                            "run ()"
                        ]

                Expect.isFalse
                    (analyseDiagnostics provider input)
                    "a ref struct exposing a pattern `Dispose` qualifies for `use` (the carve-out)"
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
                            let pathOf name =
                                Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))

                            let depsA = transitiveDeps (pathOf "ClosureA")
                            Expect.contains depsA (pathOf "ClosureB") "A's direct dependency B"
                            Expect.contains depsA (pathOf "ClosureC") "A's transitive dependency C"

                            Expect.equal
                                (transitiveDeps (pathOf "ClosureB"))
                                [ pathOf "ClosureC" ]
                                "B depends on C only"

                            Expect.equal (transitiveDeps (pathOf "ClosureC")) [] "C is dependency-free"
                    }

                    test "buildClosureWithDeps excludes a non-dependency that merely sorts earlier" {
                        // D and E are independent roots (neither depends on the other);
                        // D sorts earlier in the closure, but must NOT appear in E's deps.
                        writeSyntheticManifest "IndepD" [] |> ignore
                        let e = writeSyntheticManifest "IndepE" []

                        match
                            ReferencedProject.buildClosureWithDeps
                                [ Path.Combine(tmpSrc, "IndepD", "manifest.toml"); e ]
                        with
                        | Result.Error err -> failtestf "buildClosureWithDeps failed: %s" err
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name =
                                Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))

                            Expect.equal
                                (transitiveDeps (pathOf "IndepE"))
                                []
                                "E declares no dependency on D despite D sorting earlier"
                    }

                    // The directory name is the package identity (`depends-on`
                    // resolves against it). An explicit `[core] name` that diverges from
                    // the directory is rejected at parse time so the two identities
                    // can't drift unnoticed.
                    test "a [core] name diverging from the directory name is rejected" {
                        // Hand-write a manifest whose `name` ("Mismatch") differs from
                        // its directory ("DivergeDir") — `writeSyntheticManifest` always
                        // matches them, so build this one directly.
                        let dir = Path.Combine(tmpSrc, "DivergeDir")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nname = \"Mismatch\"\nnamespace = \"X\"\nfiles = []\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> failtestf "expected a name/dir mismatch error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains e "Mismatch" "error names the declared name"
                            Expect.stringContains e "DivergeDir" "error names the directory"
                    }

                    test "an omitted [core] name falls back to the directory name (no divergence)" {
                        let dir = Path.Combine(tmpSrc, "NoName")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nnamespace = \"X\"\nfiles = []\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> Expect.equal m.Name "NoName" "name falls back to directory name"
                        | Result.Error e -> failtestf "expected Ok, got Error %s" e
                    }
                ]

            // Per-target `impl-<t>` / `inline-bodies-<t>` overrides (codegen-js
            // step F0): the manifest parse captures them inertly by suffix; the
            // *backend* selects via `resolveImpl` / `resolveInlineBodies`. The
            // `.fsi` contract (`files`) is shared and never overridden.
            testList
                "per-target overrides"
                [
                    // A manifest carrying both a base and a `js` override for each key.
                    let withOverrides =
                        let dir = Path.Combine(tmpSrc, "TargetOverrides")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")

                        File.WriteAllText(
                            path,
                            "[core]\n\
                             namespace = \"X\"\n\
                             files = [\"contract.fsi\"]\n\
                             impl = [\"ops.fs\"]\n\
                             impl-js = [\"ops.js.fs\"]\n\
                             inline-bodies = [\"ops.fs\"]\n\
                             inline-bodies-js = [\"ops.js.fs\"]\n\
                             runtime-js = [\"runtime.mjs\"]\n"
                        )

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> m
                        | Result.Error e -> failwithf "loadManifest failed: %s" e

                    test "overrides are captured by bare suffix" {
                        Expect.equal
                            (withOverrides.ImplOverrides |> Map.tryFind "js")
                            (Some [ "ops.js.fs" ])
                            "impl-js captured under \"js\""

                        Expect.equal
                            (withOverrides.InlineBodiesOverrides |> Map.tryFind "js")
                            (Some [ "ops.js.fs" ])
                            "inline-bodies-js captured under \"js\""

                        // `runtime-js` (the platform-support `.mjs` asset) is captured
                        // the same way — by bare suffix, with no base `runtime` key.
                        Expect.equal
                            (withOverrides.RuntimeOverrides |> Map.tryFind "js")
                            (Some [ "runtime.mjs" ])
                            "runtime-js captured under \"js\""
                    }

                    test "resolveImpl/resolveInlineBodies pick the override for a known target" {
                        Expect.equal
                            (ReferencedProject.resolveImpl (Some "js") withOverrides)
                            [ "ops.js.fs" ]
                            "js impl override selected"

                        Expect.equal
                            (ReferencedProject.resolveInlineBodies (Some "js") withOverrides)
                            [ "ops.js.fs" ]
                            "js inline-bodies override selected"
                    }

                    // `resolveRuntime` has NO base list (a runtime asset is inherently
                    // target-specific), so `None` and any target without the key yield
                    // the empty list — unlike `resolveImpl`/`resolveInlineBodies`.
                    test "resolveRuntime selects the target asset, empty for None / unknown" {
                        Expect.equal
                            (ReferencedProject.resolveRuntime (Some "js") withOverrides)
                            [ "runtime.mjs" ]
                            "js runtime asset selected"

                        Expect.isEmpty (ReferencedProject.resolveRuntime None withOverrides) "None ⇒ no runtime"

                        Expect.isEmpty
                            (ReferencedProject.resolveRuntime (Some "wasm") withOverrides)
                            "unknown target ⇒ no runtime (no base fallback)"
                    }

                    // `runtimeModules` reads the resolved asset's contents off disk,
                    // keyed by package/assembly name — the map the JS backend threads
                    // into `JsImports` and materialises beside its output.
                    test "runtimeModules reads the asset contents keyed by package name" {
                        let dir = Path.Combine(tmpSrc, "RuntimeAsset")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "asset.mjs"), "export const k = 1;\n")
                        let path = Path.Combine(dir, "manifest.toml")

                        File.WriteAllText(
                            path,
                            "[core]\n\
                             namespace = \"X\"\n\
                             files = []\n\
                             runtime-js = [\"asset.mjs\"]\n"
                        )

                        let resolved = ReferencedProject.runtimeModules "js" [ path ]

                        Expect.equal
                            (resolved |> Map.tryFind "RuntimeAsset")
                            (Some("asset.mjs", "export const k = 1;\n"))
                            "package RuntimeAsset → (fileName, source) read from disk"

                        Expect.isEmpty
                            (ReferencedProject.runtimeModules "wasm" [ path ])
                            "a target with no runtime asset resolves to an empty map"
                    }

                    test "resolve falls back to the base list for None and for an unknown target" {
                        Expect.equal (ReferencedProject.resolveImpl None withOverrides) [ "ops.fs" ] "None ⇒ base impl"

                        Expect.equal
                            (ReferencedProject.resolveImpl (Some "wasm") withOverrides)
                            [ "ops.fs" ]
                            "unknown target ⇒ base impl"

                        Expect.equal
                            (ReferencedProject.resolveInlineBodies (Some "wasm") withOverrides)
                            [ "ops.fs" ]
                            "unknown target ⇒ base inline-bodies"
                    }

                    test "a manifest with no overrides has empty override maps and resolves to base" {
                        let dir = Path.Combine(tmpSrc, "NoOverrides")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nnamespace = \"X\"\nfiles = []\nimpl = [\"ops.fs\"]\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Error e -> failtestf "loadManifest failed: %s" e
                        | Result.Ok m ->
                            Expect.isEmpty m.ImplOverrides "no impl overrides"
                            Expect.isEmpty m.InlineBodiesOverrides "no inline-bodies overrides"
                            Expect.isEmpty m.RuntimeOverrides "no runtime overrides"
                            // `inline-bodies` itself defaults to `impl` (existing behaviour).
                            Expect.equal
                                (ReferencedProject.resolveInlineBodies (Some "js") m)
                                [ "ops.fs" ]
                                "js ⇒ base (= impl)"
                    }
                ]
        ]
