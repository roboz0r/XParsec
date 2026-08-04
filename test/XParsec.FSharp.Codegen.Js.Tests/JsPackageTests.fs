module XParsec.FSharp.Codegen.Js.Tests.JsPackageTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A JS package build: one `.mjs` per EMITTING source file inside a directory named for
// the package, plus the generated barrel. The point of the shape is that a cross-file
// reference names the DECLARING FILE's module — the assembly alone cannot, and before the
// declaring file rode the symbol's origin there was nothing else to name it by.

let private packageName = "Test.Pkg"

/// Compile `files` as one JS package through the production driver, failing the test on
/// any front-end diagnostic (anchored to its own file, as the driver reports it).
let private compilePackage (files: (string * string) list) : JsPackage =
    match JsDriver.compileAssemblyWith Pipeline.analyseForSelfHost jsContract.Value packageName files with
    | Ok pkg -> pkg
    | Error diags ->
        failtestf
            "package compile failed:\n%s"
            (diags
             |> List.map (fun d -> sprintf "%s(%d,%d): %s" d.Path d.Line d.Col d.Diagnostic.Message)
             |> String.concat "\n")

let private sourceOf (pkg: JsPackage) (fileName: string) : string =
    match pkg.Modules |> List.tryFind (fun m -> m.FileName = fileName) with
    | Some m -> m.Artifact.Source
    | None -> failtestf "no module '%s' in the package; got %A" fileName (pkg.Modules |> List.map (fun m -> m.FileName))

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
                let pkg = compilePackage [ "shapes.fs", declaringFile; "use.fs", consumingFile ]

                let js = sourceOf pkg "use.mjs"

                // A sibling of the same package directory — one `./`, no package segment.
                Expect.stringContains js "from \"./shapes.mjs\"" (sprintf "sibling module specifier, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "Test.Pkg.mjs")
                    (sprintf "the package name must not be a module specifier, got:\n%s" js)
            }

            test "each emitting file becomes its own module, and the barrel re-exports them" {
                let pkg = compilePackage [ "shapes.fs", declaringFile; "use.fs", consumingFile ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.FileName))
                    [ "shapes.mjs"; "use.mjs" ]
                    "one module per emitting file, in file order"

                Expect.equal
                    pkg.Barrel
                    "export * from \"./shapes.mjs\";\nexport * from \"./use.mjs\";\n"
                    "the barrel re-exports every emitted module"
            }

            // The regeneration rule the plan states as an ABSENCE: most of a contract
            // package's files are declaration-only and lower to no JS at all, so they get
            // no `.mjs` rather than an empty one.
            test "a file that emits nothing gets no module and is absent from the barrel" {
                let interfacesOnly =
                    "\
namespace Test.Pkg

type IShape =
    abstract member Area: unit -> int
"

                let pkg =
                    compilePackage [ "interfaces.fs", interfacesOnly; "shapes.fs", declaringFile ]

                Expect.equal
                    (pkg.Modules |> List.map (fun m -> m.FileName))
                    [ "shapes.mjs" ]
                    "the declaration-only file contributes no module"

                Expect.isFalse (pkg.Barrel.Contains "interfaces") "and is absent from the barrel"
            }

            // The package this whole shape exists for: eighteen contract files, no single
            // one of which could be "the package's module".
            test "Vesper.Core compiles as one package and its modules load under Node" {
                let manifest =
                    match ReferencedProject.loadManifest vesperCoreManifest with
                    | Result.Ok m -> m
                    | Result.Error e -> failtestf "Vesper.Core manifest: %s" e

                let dir = IO.Path.GetDirectoryName vesperCoreManifest

                let files =
                    ReferencedProject.resolveImpl Target.Js manifest
                    |> List.map (fun rel -> rel, IO.File.ReadAllText(IO.Path.Combine(dir, rel)))

                let pkg =
                    match
                        JsDriver.compileAssemblyWith
                            Pipeline.analyseForSelfHost
                            (JsDriver.contractForSelf Target.Js vesperCoreManifest [])
                            manifest.Name
                            files
                    with
                    | Ok pkg -> pkg
                    | Error diags ->
                        failtestf
                            "Vesper.Core JS package compile failed:\n%s"
                            (diags
                             |> List.map (fun d -> sprintf "%s(%d,%d): %s" d.Path d.Line d.Col d.Diagnostic.Message)
                             |> String.concat "\n")

                let root = tmpDir "js-package-core"
                JsDriver.materialise root pkg

                // Most of Core is intrinsic-repr-only and lowers to no JS: `type int =
                // (# "number" #)` declares a representation the target already has, and its
                // operator members are splice templates. Those files get no `.mjs` at all —
                // the ABSENCE is the assertion, an empty module would say less.
                let emitted = pkg.Modules |> List.map (fun m -> m.Source)

                for reprOnly in [ "prim-types-min.js.fs"; "prim-types-int.js.fs"; "array-index.js.fs" ] do
                    Expect.isFalse
                        (List.contains reprOnly emitted)
                        (sprintf "%s declares only representations; it must emit no module (got %A)" reprOnly emitted)

                Expect.isNonEmpty pkg.Modules "the operator bodies do emit modules"

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

            test "the emitted package runs under Node through its barrel" {
                let pkg = compilePackage [ "shapes.fs", declaringFile; "use.fs", consumingFile ]

                let root = tmpDir "js-package"
                JsDriver.materialise root pkg

                // A hand-written consumer: the barrel is the ONE specifier a consumer needs
                // for the whole package, whichever of its files an export came from.
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
