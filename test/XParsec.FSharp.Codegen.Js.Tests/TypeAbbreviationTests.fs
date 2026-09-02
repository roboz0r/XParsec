module XParsec.FSharp.Codegen.Js.Tests.TypeAbbreviationTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Every use site of a type abbreviation expands to the underlying type, so the emitted JS
// declares nothing, imports nothing and exports nothing under the name, while a program
// written through the name runs.

let private expectNoAliasInSource (js: string) : unit =
    for alias in TypeAbbreviationFixtures.aliasNames do
        Expect.isFalse
            (js.Contains alias)
            (sprintf "the abbreviation `%s` leaves no trace in the emitted JS:\n%s" alias js)

/// Emit `src`, require the aliases absent from the JS, run it under Node and return stdout.
/// `None` when Node is absent.
let private runSource (name: string) (src: string) : string option =
    expectNoAliasInSource (emitJs src)

    match runJs name src with
    | None -> None
    | Some(code, out) ->
        Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
        Some out

let private expectOutput (name: string) (src: string) (expected: string) : unit =
    match runSource name src with
    | None -> skiptest "node not found on PATH"
    | Some out -> Expect.equal out expected "the program written through the abbreviations runs"

let private packageName = "Test.Abbrev"

let private compilePackage (units: AssemblyFiles.SourceUnit list) : JsPackage =
    match JsDriver.compileWith jsContract.Value (AssemblySources.synthetic packageName Target.Js Set.empty units) with
    | Ok pkg -> pkg
    | Error diags -> failtestf "package compile failed:\n%s" (AssemblyFiles.AnchoredDiagnostic.renderAll diags)

let private sourceOf (pkg: JsPackage) (fileName: string) : string =
    match pkg.Modules |> List.tryFind (fun m -> m.Path.FileName = fileName) with
    | Some m -> m.Artifact.Source
    | None ->
        failtestf "no module '%s' in the package; got %A" fileName (pkg.Modules |> List.map (fun m -> m.Path.FileName))

let private declaringFile =
    TypeAbbreviationFixtures.declaringFile "namespace Test.Abbrev\n\nmodule Decls =" "    "

[<Tests>]
let tests =
    testList
        "Codegen.Js TypeAbbreviation (erased name, expanded use)"
        [
            for p in TypeAbbreviationFixtures.programs do
                test p.Description { expectOutput ("js-" + p.Name) p.Source p.Expected }

            // Control for the generic fixture, with no abbreviation: the JS emitter refuses a
            // top-level tuple-pattern binding as an unsupported declaration, so the fixture
            // destructures inside a function.
            ptest "GAP: a top-level tuple-pattern binding is `EmitJs: unsupported declaration` (prints 40)" {
                expectOutput
                    "js-toplevel-tuple-pattern"
                    (String.concat "\n" [ "let (u, v) = (20, 10)"; "printfn \"%d\" (u + v * 2)" ])
                    "40"
            }

            test
                "a package's second file writes the first file's abbreviations, imports only real declarations, and runs" {
                let consumingFile =
                    "\
namespace Test.Abbrev

module Use =
    open Test.Abbrev.Decls

    let sum (p: PointAlias) : myint = p.X + p.Y

    let area (s: ShapeAlias) : int =
        match s with
        | ShapeAlias.Circle r -> 3 * r * r
        | Decls.ShapeAlias.Square w -> w * w

    let both (p: intpair) : int =
        let (a, b) = p
        a + b

    let total () : int =
        let zero: CounterAlias = Decls.CounterAlias.Zero
        sum { X = 3; Y = 4 } + area (ShapeAlias.Square 5) + zero.Value + both (10, 20)
"

                let pkg =
                    compilePackage
                        [
                            AssemblyFiles.SourceUnit.ofImplementation (
                                AssemblyFiles.SourceFile.ofText "decls.fs" declaringFile
                            )
                            AssemblyFiles.SourceUnit.ofImplementation (
                                AssemblyFiles.SourceFile.ofText "use.fs" consumingFile
                            )
                        ]

                let js = sourceOf pkg "use.mjs"
                expectNoAliasInSource js

                Expect.stringContains
                    js
                    "from \"./decls.mjs\""
                    (sprintf "the real declarations are imported, got:\n%s" js)

                let root = tmpDir "js-abbrev-package"
                JsDriver.materialise root pkg

                let entry = IO.Path.Combine(root, "abbrev-entry.mjs")

                IO.File.WriteAllText(
                    entry,
                    sprintf
                        "import { total } from \"./%s/%s\";\nconsole.log(total());\n"
                        pkg.Name
                        JsDriver.BarrelFileName
                )

                match runNode entry with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    let actual = out.Replace("\r", "").Trim()
                    Expect.equal code 0 (sprintf "node runs the package (%s)" actual)
                    // 7 + 25 + 0 + 30
                    Expect.equal actual "62" "every published abbreviation expands in the consuming file"
            }

            // Control with no abbreviation: constructing a class declared in ANOTHER file of the
            // package is refused by the JS emitter, so the fixture above reads a static instead.
            ptest
                "GAP: a package's second file constructs a class the first file declares (`EmitJs: construction of external type … has no JS analogue`)" {
                let consumingFile =
                    "\
namespace Test.Abbrev

module Use =
    let total () : int = Decls.Counter(7).Value
"

                compilePackage
                    [
                        AssemblyFiles.SourceUnit.ofImplementation (
                            AssemblyFiles.SourceFile.ofText "decls.fs" declaringFile
                        )
                        AssemblyFiles.SourceUnit.ofImplementation (
                            AssemblyFiles.SourceFile.ofText "use.fs" consumingFile
                        )
                    ]
                |> ignore
            }
        ]
