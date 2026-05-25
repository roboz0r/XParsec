module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis

// P1 layer-1 gate (symbol-resolution-plan §5.1): stand up `Vesper.Core` from its
// real `manifest.toml` and confirm the contract resolves with a non-empty
// `Origin` stamped from the manifest — the first consumer of the P0 identity
// surface.

/// Locate `src/Vesper.Core/manifest.toml` by walking up from the test assembly.
let private vesperCoreManifest =
    let testDir = Path.GetDirectoryName(typeof<FSharpLib.LibFile>.Assembly.Location)
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

            test "int resolves as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "int" with
                | ValueSome(ExternalTypeShape.Class(arity, isInterface, origin)) ->
                    Expect.equal arity 0 "int is non-generic"
                    Expect.isFalse isInterface "int is not an interface"
                    Expect.equal origin.Assembly (Some "Vesper.Core") "origin assembly = Vesper.Core"
                    Expect.equal origin.Namespace "Vesper" "origin namespace = Vesper"
                | other -> failtestf "expected int as Class shape, got %A" other
            }

            test "Fun resolves as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // `type Fun<'A,'B>` carries a single abstract `Invoke`. It parses
                // as an anonymous (`= begin … end`) type, so the extractor records
                // a non-interface `Class` shape — SAM-interface detection from an
                // all-abstract body is a downstream (Freeze/codegen) concern. P1
                // only needs it to resolve with the package `Origin`.
                match provider.TryLookupType "Fun" with
                | ValueSome(ExternalTypeShape.Class(arity, _, origin)) ->
                    Expect.equal arity 2 "Fun has two typars"
                    Expect.equal origin.Assembly (Some "Vesper.Core") "origin assembly = Vesper.Core"
                    Expect.equal origin.Namespace "Vesper" "origin namespace = Vesper"
                | other -> failtestf "expected Fun as Class shape, got %A" other
            }

            test "qualified name resolves too (Vesper.int)" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Class(_, _, origin)) ->
                    Expect.equal origin.Assembly (Some "Vesper.Core") "origin on qualified lookup"
                | other -> failtestf "expected Vesper.int as Class shape, got %A" other
            }

            test "an unknown type misses" {
                let provider, _ = builtProvider.Value
                Expect.isTrue (provider.TryLookupType "NoSuchType" |> ValueOption.isNone) "unknown type miss"
            }
        ]
