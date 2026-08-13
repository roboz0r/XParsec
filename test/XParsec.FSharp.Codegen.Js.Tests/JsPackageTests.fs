module XParsec.FSharp.Codegen.Js.Tests.JsPackageTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A JS package build: one `.mjs` per EMITTING source file inside a directory named for
// the package, plus the generated barrel. A cross-file reference names the DECLARING
// FILE's module, which the package name alone could not identify.

let private packageName = "Test.Pkg"

/// Compile `files` as one JS package through the production driver, failing the test on
/// any front-end diagnostic (anchored to its own file, as the driver reports it).
let private compilePackage (files: AssemblyFiles.SourceFile list) : JsPackage =
    match JsDriver.compileAssemblyWith Pipeline.analyseForSelfHost jsContract.Value packageName files with
    | Ok pkg -> pkg
    | Error diags ->
        failtestf
            "package compile failed:\n%s"
            (diags
             |> List.map (fun d -> sprintf "%s(%d,%d): %s" d.Path.Name d.Line d.Col d.Diagnostic.Message)
             |> String.concat "\n")

let private sourceOf (pkg: JsPackage) (fileName: string) : string =
    match pkg.Modules |> List.tryFind (fun m -> m.Path.FileName = fileName) with
    | Some m -> m.Artifact.Source
    | None ->
        failtestf "no module '%s' in the package; got %A" fileName (pkg.Modules |> List.map (fun m -> m.Path.FileName))

/// Every module the emitted artifacts import must be one the package writes: a surviving
/// per-file module or a runtime asset at the output root. The driver also faults on a
/// violation; stated here as a property rather than only a compiler fault.
let private expectImportsResolvable (pkg: JsPackage) =
    let written =
        (pkg.Modules |> List.map (fun m -> m.Path))
        @ (pkg.RuntimeAssets |> List.map (fun a -> JsModulePath.asset a.FileName))
        |> Set.ofList

    for m in pkg.Modules do
        for target in m.Artifact.ImportedModules do
            Expect.isTrue
                (written.Contains target)
                (sprintf "%s imports %s, which the package does not write" m.Path.FileName target.FileName)

// Two files: the first declares a record and a function over it, the second consumes both.
let private declaringFile =
    "\
namespace Test.Pkg

module Shapes =
    type Point = { X: int; Y: int }

    let sum (p: Point) : int = p.X + p.Y
"

let private consumingFile =
    "\
namespace Test.Pkg

module Use =
    open Test.Pkg.Shapes

    let total () : int = sum { X = 20; Y = 22 }
"

[<Tests>]
let tests =
    testList
        "Codegen.Js package build"
        [
            test "a cross-file reference imports the DECLARING FILE's module, not the package's" {
                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "shapes.fs" declaringFile
                            AssemblyFiles.SourceFile.ofText "use.fs" consumingFile
                        ]

                let js = sourceOf pkg "use.mjs"

                // A sibling in the same package directory: one `./`, no package segment.
                Expect.stringContains js "from \"./shapes.mjs\"" (sprintf "sibling module specifier, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "Test.Pkg.mjs")
                    (sprintf "the package name must not be a module specifier, got:\n%s" js)

                expectImportsResolvable pkg
            }

            test "each emitting file becomes its own module, and the barrel re-exports them" {
                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "shapes.fs" declaringFile
                            AssemblyFiles.SourceFile.ofText "use.fs" consumingFile
                        ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.Path.FileName))
                    [ "shapes.mjs"; "use.mjs" ]
                    "one module per emitting file, in file order"

                Expect.equal
                    pkg.Barrel
                    "export * from \"./shapes.mjs\";\nexport * from \"./use.mjs\";\n"
                    "the barrel re-exports every emitted module"
            }

            // Most of a contract package's files are declaration-only and lower to no JS
            // at all, so they get no `.mjs` rather than an empty one.
            test "a file that emits nothing gets no module and is absent from the barrel" {
                let interfacesOnly =
                    "\
namespace Test.Pkg

type IShape =
    abstract member Area: unit -> int
"

                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "interfaces.fs" interfacesOnly
                            AssemblyFiles.SourceFile.ofText "shapes.fs" declaringFile
                        ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.Path.FileName))
                    [ "shapes.mjs" ]
                    "the declaration-only file contributes no module"

                Expect.isFalse (pkg.Barrel.Contains "interfaces") "and is absent from the barrel"
            }

            test "two sources with the same output file name are a build error, not a silent overwrite" {
                let moduleNamed (name: string) =
                    sprintf "namespace Test.Pkg\n\nmodule %s =\n    let v () : int = 1\n" name

                let files =
                    [
                        AssemblyFiles.SourceFile.ofText "a/one.fs" (moduleNamed "Alpha")
                        AssemblyFiles.SourceFile.ofText "b/one.fs" (moduleNamed "Beta")
                    ]

                match JsDriver.compileAssemblyWith Pipeline.analyseForSelfHost jsContract.Value packageName files with
                | Ok _ -> failtest "the collision must be refused"
                | Error diags ->
                    Expect.equal
                        (diags |> List.map (fun d -> d.Path.Name))
                        [ "a/one.fs"; "b/one.fs" ]
                        "each claimant is blamed, so neither is silently the loser"

                    for d in diags do
                        Expect.stringContains d.Diagnostic.Message "one.mjs" "naming the claimed module path"
                        Expect.stringContains d.Diagnostic.Message "a/one.fs" "and the sources that claimed it"
            }

            // Dropping a module is decided per FILE; naming one in an import is per
            // CONSUMER. A declaration-only file is reachable only at the type level, which
            // JS erases, so the consumer imports nothing from the dropped module.
            test "a consumer of a declaration-only file imports no module the package omits" {
                let contracts =
                    "\
namespace Test.Pkg

type IShape =
    abstract member Area: unit -> int
"

                let user =
                    "\
namespace Test.Pkg

module Shim =
    let passthrough (s: IShape) : IShape = s
"

                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "contracts.fs" contracts
                            AssemblyFiles.SourceFile.ofText "user.fs" user
                        ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.Path.FileName))
                    [ "user.mjs" ]
                    "the declaration-only file contributes no module"

                expectImportsResolvable pkg

                Expect.isEmpty
                    (pkg.Modules |> List.collect (fun m -> m.Artifact.ImportedModules))
                    "an erased type reference costs no import"
            }

            // The package this shape exists for: many contract files, no single one of
            // which could be "the package's module".
            test "Vesper.Core compiles as one package and its modules load under Node" {
                let manifest =
                    match
                        ReferencedProject.resolveManifest Target.Js vesperCorePackage
                        |> Result.bind ReferencedProject.loadManifest
                    with
                    | Result.Ok m -> m
                    | Result.Error e -> failtestf "Vesper.Core manifest: %s" e

                let dir = vesperCorePackage

                let files = manifest.Impl |> List.map (AssemblyFiles.SourceFile.read dir)

                let pkg =
                    match
                        JsDriver.compileAssemblyWith
                            Pipeline.analyseForSelfHost
                            (JsDriver.contractForSelf vesperCorePackage [])
                            manifest.Name
                            files
                    with
                    | Ok pkg -> pkg
                    | Error diags ->
                        failtestf
                            "Vesper.Core JS package compile failed:\n%s"
                            (diags
                             |> List.map (fun d ->
                                 sprintf "%s(%d,%d): %s" d.Path.Name d.Line d.Col d.Diagnostic.Message
                             )
                             |> String.concat "\n")

                let root = tmpDir "js-package-core"
                JsDriver.materialise root pkg

                // Most of Core is intrinsic-repr-only and lowers to no JS: `type int =
                // (# "number" #)` declares a representation the target already has, and
                // its operator members are splice templates. Those files get no `.mjs`.
                let emitted = pkg.Modules |> List.map (fun m -> m.Source.Name)

                for reprOnly in [ "prim-types-min.js.fs"; "prim-types-int.js.fs"; "prim-types-array.fs" ] do
                    Expect.isFalse
                        (List.contains reprOnly emitted)
                        (sprintf "%s declares only representations; it must emit no module (got %A)" reprOnly emitted)

                Expect.isNonEmpty pkg.Modules "the operator bodies do emit modules"

                expectImportsResolvable pkg

                // Loading the barrel evaluates every emitted module, so an unresolved
                // import or a missing export is a load-time failure here.
                let entry = IO.Path.Combine(root, "core-load.mjs")

                IO.File.WriteAllText(
                    entry,
                    sprintf
                        "import * as core from \"./%s/%s\";\nconsole.log(typeof core);\n"
                        pkg.Name
                        JsDriver.BarrelFileName
                )

                match runNode entry with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    let actual = out.Replace("\r", "").Trim()
                    Expect.equal code 0 (sprintf "node loads the package (%s)" actual)
                    Expect.equal actual "object" "the barrel resolves to a module namespace object"
            }

            // A `[<Global>]` value's declaring file emits no definition, so it contributes
            // no module at all, and a sibling reference cannot import from a module that
            // does not exist.
            test "a [<Global>] value emits no definition and a cross-file use imports nothing" {
                let declaring =
                    "\
namespace Test.Pkg

module Ambient =
    [<Global>]
    let globalThis: string = (# \"globalThis\" : string #)
"

                let consuming =
                    "\
namespace Test.Pkg

module Reader =
    open Test.Pkg.Ambient

    let here () : string = globalThis
"

                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "ambient.fs" declaring
                            AssemblyFiles.SourceFile.ofText "reader.fs" consuming
                        ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.Path.FileName))
                    [ "reader.mjs" ]
                    "the declaring file defines nothing, so it contributes no module"

                let js = sourceOf pkg "reader.mjs"

                Expect.isFalse (js.Contains "import") (sprintf "a global pulls in no import, got:\n%s" js)
                Expect.stringContains js "globalThis" (sprintf "the bare global name is emitted, got:\n%s" js)
            }

            test "the emitted package runs under Node through its barrel" {
                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "shapes.fs" declaringFile
                            AssemblyFiles.SourceFile.ofText "use.fs" consumingFile
                        ]

                let root = tmpDir "js-package"
                JsDriver.materialise root pkg

                // The barrel is the single specifier a consumer needs for the whole
                // package, whichever of its files an export came from.
                let entry = IO.Path.Combine(root, "main.mjs")

                IO.File.WriteAllText(entry, "import { total } from \"./Test.Pkg/index.mjs\";\nconsole.log(total());\n")

                match runNode entry with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    let actual = out.Replace("\r", "").Trim()
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" actual)
                    Expect.equal actual "42" "the cross-file call resolved through the emitted sibling module"
            }
        ]
