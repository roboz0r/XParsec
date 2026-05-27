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

            test "int resolves (qualified) as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // Short names no longer resolve through the provider's own retry
                // (O3): they're resolved by the pipeline via the ambient prefix
                // set. The provider answers the qualified name directly, stamping
                // the package `Origin`.
                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Arity 0 "int is non-generic"
                    Expect.isFalse info.IsInterface "int is not an interface"
                    Expect.equal info.Origin.Assembly (Some "Vesper.Core") "origin assembly = Vesper.Core"
                    Expect.equal info.Origin.Namespace "Vesper" "origin namespace = Vesper"
                | other -> failtestf "expected Vesper.int as Class shape, got %A" other
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

                match box provider with
                | :? IAmbientOpenScope as a ->
                    let prefixes = a.AmbientOpenPrefixes
                    // `ops-platform.fsi`'s `[<AutoOpen>]` operator modules, plus
                    // the package namespace as the trailing implicit prefix.
                    Expect.isTrue
                        (List.contains "Vesper.ArithmeticOperators" prefixes)
                        "ArithmeticOperators auto-open surfaced"

                    Expect.isTrue (List.contains "Vesper.Operators" prefixes) "Operators (hash) auto-open surfaced"
                    Expect.isTrue (List.contains "Vesper" prefixes) "namespace surfaced as the trailing prefix"
                    Expect.equal (List.last prefixes) "Vesper" "namespace is last (probed after the AutoOpen modules)"
                | _ -> failtest "the manifest provider should implement IAmbientOpenScope"
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
        ]
