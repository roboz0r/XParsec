module XParsec.FSharp.Codegen.Js.Tests.JsPackageTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A JS package build: one `.mjs` per EMITTING source file inside a directory named for
// the package, plus the generated barrel. A cross-file reference resolves to the DECLARING
// FILE's module, which the package name alone could not identify.

let private packageName = "Test.Pkg"

/// `units` as `packageName`'s JS sources. A source here carries no `#if`, so compilation
/// defines are empty.
let private testSources (units: AssemblyFiles.SourceUnit list) : AssemblySources =
    AssemblySources.synthetic packageName Target.Js Set.empty units

/// Compile `units` as one JS package through the production driver, failing the test on
/// any front-end diagnostic (anchored to its own file, as the driver reports it).
let private compileUnits (units: AssemblyFiles.SourceUnit list) : JsPackage =
    match JsDriver.compileWith jsContract.Value (testSources units) with
    | Ok pkg -> pkg
    | Error diags -> failtestf "package compile failed:\n%s" (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

let private compilePackage (files: AssemblyFiles.SourceFile list) : JsPackage =
    compileUnits (files |> List.map AssemblyFiles.SourceUnit.ofImplementation)

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
        @ (pkg.RuntimeAssets |> List.map (fun a -> a.Path))
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

            // A `.fsi` publishes the declarations, but what a backend EMITS for the unit is
            // compiled from the `.fs` — so the import must reference that module, not the signature.
            test "a reference through a `.fsi` imports the IMPLEMENTATION's module" {
                let declaring =
                    "\
namespace Test.Pkg

module Shapes =
    type Point = { X: int; Y: int }

    let scale (n: int) : int = n * 2

    let hidden (p: Point) : int = p.X + p.Y
"

                // `val` before `type`, which is the order a signature module parses in today.
                let declaringSig =
                    "\
namespace Test.Pkg

module Shapes =
    val scale: n: int -> int

    type Point = { X: int; Y: int }
"

                // The record literal carries no type annotation: it resolves through the field-reverse
                // index the SIGNATURE publishes, which is a channel a `.fsi` view must carry.
                let consuming =
                    "\
namespace Test.Pkg

module Use =
    open Test.Pkg.Shapes

    let total () : int = scale 21

    let origin () = { X = 0; Y = 0 }
"

                let pkg =
                    compileUnits
                        [
                            AssemblyFiles.SourceUnit.paired
                                (AssemblyFiles.SourceFile.ofText "shapes.fsi" declaringSig)
                                (AssemblyFiles.SourceFile.ofText "shapes.fs" declaring)
                            AssemblyFiles.SourceUnit.ofImplementation (
                                AssemblyFiles.SourceFile.ofText "use.fs" consuming
                            )
                        ]

                let js = sourceOf pkg "use.mjs"

                Expect.stringContains js "from \"./shapes.mjs\"" (sprintf "imports the `.fs` module, got:\n%s" js)

                Expect.isFalse (js.Contains "shapes.fsi") (sprintf "no signature file is a module, got:\n%s" js)

                expectImportsResolvable pkg
            }

            // `[<CompiledName>]` is a CLR emission fact. A JS module exports the name its
            // source writes, so an import naming the attribute's name would bind an export
            // the declaring module never wrote.
            test "a [<CompiledName>]'d binding is exported and imported under its SOURCE name" {
                let declaring =
                    "\
namespace Test.Pkg

module Bag =
    [<CompiledName(\"Count\")>]
    let count (n: int) : int = n
"

                let declaringSig =
                    "\
namespace Test.Pkg

module Bag =
    [<CompiledName(\"Count\")>]
    val count: n: int -> int
"

                let consuming =
                    "\
namespace Test.Pkg

module Use =
    open Test.Pkg.Bag

    let total () : int = count 21
"

                let pkg =
                    compileUnits
                        [
                            AssemblyFiles.SourceUnit.paired
                                (AssemblyFiles.SourceFile.ofText "bag.fsi" declaringSig)
                                (AssemblyFiles.SourceFile.ofText "bag.fs" declaring)
                            AssemblyFiles.SourceUnit.ofImplementation (
                                AssemblyFiles.SourceFile.ofText "use.fs" consuming
                            )
                        ]

                let declared = sourceOf pkg "bag.mjs"
                let used = sourceOf pkg "use.mjs"

                Expect.stringContains declared "count" "the declaring module exports its source name"

                Expect.isFalse (declared.Contains "Count") (sprintf "and not the attribute's, got:\n%s" declared)

                Expect.stringContains used "count" "the consumer imports the export the declaring module wrote"

                Expect.isFalse (used.Contains "Count") (sprintf "and not the attribute's, got:\n%s" used)

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

            // Most of a declaration-heavy package's files lower to no JS at all, so they get
            // no `.mjs` rather than an empty one.
            test "a file that emits nothing is absent from both the module list and the barrel" {
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
                    |> List.map AssemblyFiles.SourceUnit.ofImplementation

                match JsDriver.compileWith jsContract.Value (testSources files) with
                | Ok _ -> failtest "the collision must be refused"
                | Error diags ->
                    Expect.equal
                        (diags |> List.map (fun d -> AssemblyFileId.toStored d.Path))
                        [ "a/one.fs"; "b/one.fs" ]
                        "the diagnostic is reported at each claimant, so neither is silently the loser"

                    for d in diags do
                        Expect.stringContains d.Diagnostic.Message "one.mjs" "quoting the claimed module path"
                        Expect.stringContains d.Diagnostic.Message "a/one.fs" "and the sources that claimed it"
            }

            // Dropping a module is decided per FILE; importing one is per
            // CONSUMER. A declaration-only file is reachable only at the type level, which
            // JS erases, so the consumer imports nothing from the dropped module.
            test "a consumer of a declaration-only file imports nothing from the dropped module" {
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

            // The package this shape exists for: many signature files, no single one of
            // which could be "the package's module".
            //
            // KNOWN DEFECT this test tolerates: an `[<Import>]` binding's DECLARING module
            // still emits it as a throwing `nativeOnly` function, and the load check passes
            // only because the asset barrel overwrites the generated one (see
            // `JsDriver.materialise`).
            test "Vesper.Core compiles as one package and its modules load under Node" {
                let manifestPath =
                    ReferencedProject.resolveManifest Target.Js vesperCorePackage
                    |> PackageFaults.okOrFail "Vesper.Core manifest"

                let sources =
                    AssemblySources.ofManifest manifestPath
                    |> PackageFaults.okOrFail "Vesper.Core units"

                let pkg =
                    match JsDriver.compileWith (JsDriver.contractForSelf vesperCorePackage []) sources with
                    | Ok pkg -> pkg
                    | Error diags ->
                        failtestf
                            "Vesper.Core JS package compile failed:\n%s"
                            (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

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
            test "a [<Global>] value defines nothing, and a cross-file use imports nothing" {
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

            // A module VALUE, not a function: the consumer reads the declaring file's export
            // both bare (after `open`) and qualified.
            test "a cross-file module VALUE runs under Node through its barrel" {
                let declaring =
                    "\
namespace Test.Pkg

module Consts =
    let v : int = 5
"

                let consuming =
                    "\
namespace Test.Pkg

module Use =
    open Test.Pkg.Consts

    let total () : int = v + 1 + Test.Pkg.Consts.v
"

                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceFile.ofText "consts.fs" declaring
                            AssemblyFiles.SourceFile.ofText "use.fs" consuming
                        ]

                expectImportsResolvable pkg

                let root = tmpDir "js-package-value"
                JsDriver.materialise root pkg
                let entry = IO.Path.Combine(root, "main.mjs")

                IO.File.WriteAllText(entry, "import { total } from \"./Test.Pkg/index.mjs\";\nconsole.log(total());\n")

                match runNode entry with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    let actual = out.Replace("\r", "").Trim()
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" actual)
                    Expect.equal actual "11" "5 + 1 + 5 through the sibling module's exported value"
            }
        ]

/// A `tmp/<name>` package whose two `.fs` files each declare an inline `pick`, in `order`.
let private writePickPackage (name: string) (order: string list) : string =
    let dir = tmpDir name

    let write (file: string) (text: string) =
        IO.File.WriteAllText(IO.Path.Combine(dir, file), text)

    let entries = order |> List.map (sprintf "\"%s\"") |> String.concat ", "

    write
        "manifest.js.toml"
        (sprintf
            "[core]\nname = \"%s\"\ndescription = \"Two bodies for one name, for the clash order.\"\ndepends-on = [\"../../src/Vesper.Core\"]\nfiles = [%s]\n"
            name
            entries)

    write "first.fs" "namespace PickOrder\n\n[<AutoOpen>]\nmodule First =\n    let inline pick () : int = 1\n"

    write "second.fs" "namespace PickOrder\n\n[<AutoOpen>]\nmodule Second =\n    let inline pick () : int = 2\n"

    dir

/// The file that produced the body `jsNativeInlineBodies` serves for `pick` from `dir`'s package.
let private pickWinner (dir: string) : string =
    match JsNativeSymbols.jsNativeInlineBodies [ dir ] |> Map.tryFind "pick" with
    | Some body -> AssemblyFileId.toStored body.File.Path.Relative
    | None -> failtest "no inline body collected for 'pick'"

[<Tests>]
let inlineBodyOrderTests =
    testList
        "JsPackageInlineBodyOrder"
        [
            // The manifest file list is the ONE place `.fs`-to-`.fs` relative order is
            // load-bearing: two bodies under one simple name resolve to the LATER file's.
            // Reversing the list flips the winner, so it is order, not identity.
            test "a name two files bind resolves to the later file's body, by manifest order" {
                Expect.equal
                    (pickWinner (writePickPackage "InlineOrder.AB" [ "first.fs"; "second.fs" ]))
                    "second.fs"
                    "the later entry wins the clash"

                Expect.equal
                    (pickWinner (writePickPackage "InlineOrder.BA" [ "second.fs"; "first.fs" ]))
                    "first.fs"
                    "reversed, the other file is the later entry and wins"
            }
        ]

// ---- Whole-corpus conformance (js) ------------------------------------------
// Every package publishing a `manifest.js.toml`, driven through the production analyse-and-
// gate seam: the same `conformSignature` and `[<Import>]` verdicts a compile takes, including
// the gate's discharge of the import obligations against the committed assets.

let private jsManifestPackages: (string * string) list =
    IO.Directory.GetDirectories(IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src"), "Vesper.*")
    |> Array.filter (fun d -> IO.File.Exists(IO.Path.Combine(d, "manifest.js.toml")))
    |> Array.map (fun d -> IO.Path.GetFileName d, d)
    |> Array.sortBy fst
    |> List.ofArray

[<Tests>]
let jsCorpusConformanceTests =
    testList
        "JsCorpusConformance"
        [
            for package, dir in jsManifestPackages do
                test $"{package}: the js manifest's units analyse and conform, imports discharged" {
                    // `resolveAll` closes the contract over `depends-on`, so the self stack
                    // needs no explicit reference list.
                    let contract = JsDriver.contractForSelf dir []

                    let gated =
                        match PackageProviders.AnalysedManifest.gate contract with
                        | Ok g -> g
                        | Error diags ->
                            failtestf
                                "%s: contract refused:\n%s"
                                package
                                (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

                    let sources =
                        ReferencedProject.resolveManifest Target.Js dir
                        |> PackageFaults.okOrFail (sprintf "%s manifest" package)
                        |> AssemblySources.ofManifest
                        |> PackageFaults.okOrFail (sprintf "%s units" package)

                    let analysed = Frontend.analyse gated.Provider sources

                    match AnalysedAssembly.gate (JsDriver.selfModules contract package) analysed with
                    | Ok _ -> ()
                    | Error diags ->
                        failtestf
                            "%s: the analysed units must conform to their signature contracts; got:\n%s"
                            package
                            (AssemblyFiles.AnchoredDiagnostic.renderAll diags)
                }
        ]
