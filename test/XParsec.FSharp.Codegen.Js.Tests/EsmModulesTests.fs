module XParsec.FSharp.Codegen.Js.Tests.EsmModulesTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The module-system half of the `[<Import>]` check: analysis records the obligations, and
// the assembly gate discharges each against the declaring package's `[core] runtime` assets
// through the ESM resolver the JS driver supplies.

let private asset (fileName: string) (source: string) : RuntimeAsset =
    { FileName = fileName; Source = source }

[<Tests>]
let resolverTests =
    testList
        "EsmModules.resolver"
        [
            test "exportedNames reads declarations, export lists and aliases" {
                let names =
                    EsmModules.exportedNames
                        "export const a = 1;\nexport async function b() {}\nconst c = 2;\nexport { c, d as e };\n"

                Expect.equal names (set [ "a"; "b"; "c"; "e" ]) "declared, listed and aliased exports"
            }

            test "the three resolutions: malformed, not listed, resolved" {
                let modules = EsmModules.create [ asset "Here.mjs" "export const x = 1;" ]

                Expect.equal (modules.Resolve "Here.mjs") ImportResolution.Malformed "a bare specifier is not relative"

                Expect.equal
                    (modules.Resolve "./Here")
                    ImportResolution.Malformed
                    "an ESM specifier carries an extension"

                Expect.equal
                    (modules.Resolve "./Other.mjs")
                    ImportResolution.NotListed
                    "a path outside the manifest's runtime list"

                match modules.Resolve "./Here.mjs" with
                | ImportResolution.Resolved a ->
                    Expect.equal (modules.Provided a) (set [ "x" ]) "the resolved asset's exports"
                | other -> failtestf "expected Resolved, got %A" other
            }
        ]

// ---- The discharge through a real JS package build ----------------------

/// Materialise a throwaway package under `tmp/` and compile it through the production
/// driver, with the repo's Vesper.Core as its dependency so `[<Import>]` and `nativeOnly`
/// resolve.
let private compileSynthetic
    (packageName: string)
    (files: (string * string) list)
    : Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list> =
    let dir = tmpDir (Path.Combine("esm-modules-tests", packageName))

    for stale in Directory.GetFiles dir do
        File.Delete stale

    for name, content in files do
        File.WriteAllText(Path.Combine(dir, name), content)

    let sources =
        ReferencedProject.resolveManifest Target.Js dir
        |> function
            | Ok mp -> mp
            | Error f -> failtestf "resolveManifest: %A" f
        |> AssemblySources.ofManifest
        |> function
            | Ok s -> s
            | Error f -> failtestf "manifest units: %A" f

    JsDriver.compileWith (JsDriver.contractForSelf dir []) sources

let private manifest (packageName: string) (runtime: string list) : string =
    let corePath = vesperCorePackage.Replace('\\', '/')

    sprintf
        "[core]\nname = \"%s\"\ndescription = \"EsmModules discharge fixture\"\ndepends-on = [\"%s\"]\nfiles = [\"imports.js.fs\"]\nruntime = [%s]\n"
        packageName
        corePath
        (runtime |> List.map (sprintf "\"%s\"") |> String.concat ", ")

let private importSource (path: string) : string =
    sprintf
        "namespace Probe\n\nmodule Served =\n\n    [<Import(\"served\", \"%s\")>]\n    let served (x: int) : int = nativeOnly\n"
        path

let private expectRefused (what: string) (result: Result<JsPackage, AssemblyFiles.AnchoredDiagnostic list>) =
    match result with
    | Ok _ -> failtestf "%s must be refused" what
    | Error diags -> diags

[<Tests>]
let dischargeTests =
    testList
        "EsmModules gate discharge"
        [
            test "a served selector the asset exports compiles" {
                let name = "Vesper.EsmOk"

                match
                    compileSynthetic
                        name
                        [
                            "manifest.js.toml", manifest name [ "Probe.Asset.mjs" ]
                            "imports.js.fs", importSource "./Probe.Asset.mjs"
                            "Probe.Asset.mjs", "export const served = (x) => x;\n"
                        ]
                with
                | Ok _ -> ()
                | Error diags ->
                    failtestf
                        "the conforming import must compile:\n%s"
                        (AssemblyFiles.AnchoredDiagnostic.renderAll diags)
            }

            test "a selector the asset does not export reports at the binding" {
                let name = "Vesper.EsmNoExport"

                let diags =
                    compileSynthetic
                        name
                        [
                            "manifest.js.toml", manifest name [ "Probe.Asset.mjs" ]
                            "imports.js.fs", importSource "./Probe.Asset.mjs"
                            "Probe.Asset.mjs", "export const other = 1;\n"
                        ]
                    |> expectRefused "a missing export"

                let d = Expect.wantSome (diags |> List.tryExactlyOne) "one finding"
                Expect.stringContains d.Diagnostic.Message "does not export" "the asset owes the selector"
                Expect.equal (AssemblyFileId.toStored d.Path) "imports.js.fs" "positioned in the declaring .fs"
                Expect.isGreaterThan d.Line 1 "at the binding, not the file head"
            }

            test "a path that is not an ESM module reference reports at the binding" {
                let name = "Vesper.EsmBadPath"

                let diags =
                    compileSynthetic
                        name
                        [
                            "manifest.js.toml", manifest name [ "Probe.Asset.mjs" ]
                            "imports.js.fs", importSource "Probe.Asset.mjs"
                            "Probe.Asset.mjs", "export const served = 1;\n"
                        ]
                    |> expectRefused "a bare specifier"

                let d = Expect.wantSome (diags |> List.tryExactlyOne) "one finding"
                Expect.stringContains d.Diagnostic.Message "not a module reference" "the specifier is not relative"
                Expect.equal (AssemblyFileId.toStored d.Path) "imports.js.fs" "positioned in the declaring .fs"
            }

            test "a path naming no runtime asset reports at the binding" {
                let name = "Vesper.EsmNotListed"

                let diags =
                    compileSynthetic
                        name
                        [
                            "manifest.js.toml", manifest name [ "Probe.Asset.mjs" ]
                            "imports.js.fs", importSource "./Elsewhere.mjs"
                            "Probe.Asset.mjs", "export const served = 1;\n"
                        ]
                    |> expectRefused "an unlisted asset"

                let d = Expect.wantSome (diags |> List.tryExactlyOne) "one finding"
                Expect.stringContains d.Diagnostic.Message "names no '[core] runtime' asset" "outside the runtime list"
                Expect.equal (AssemblyFileId.toStored d.Path) "imports.js.fs" "positioned in the declaring .fs"
            }

            test "a listed asset absent on disk is one manifest fault, not one per import" {
                // The contract's own gate reports the broken `runtime` entry, before any
                // per-binding discharge could repeat it.
                let name = "Vesper.EsmAssetGone"

                let diags =
                    compileSynthetic
                        name
                        [
                            "manifest.js.toml", manifest name [ "Probe.Asset.mjs" ]
                            "imports.js.fs", importSource "./Probe.Asset.mjs"
                        ]
                    |> expectRefused "a missing runtime asset"

                let missing =
                    diags |> List.filter (fun d -> d.Diagnostic.Message.Contains "not on disk")

                Expect.equal (List.length missing) 1 "once against the manifest"
            }
        ]
