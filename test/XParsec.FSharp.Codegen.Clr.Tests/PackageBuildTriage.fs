module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTriage

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// Assert a package builds to a BCL-only DLL.
let private buildsBclOnly (package: string) : unit =
    let _, artifact = (buildPackage package).Value
    expectNoFSharpCore artifact (sprintf "%s must compile to a BCL-only DLL" package)

[<Tests>]
let tests =
    testList
        "PackageBuildTriage"
        [

            test "Vesper.Choice builds BCL-only" { buildsBclOnly "Vesper.Choice" }

            test "Vesper.Option builds BCL-only" { buildsBclOnly "Vesper.Option" }

            test "Vesper.Result builds BCL-only" { buildsBclOnly "Vesper.Result" }

            test "Vesper.Array builds BCL-only" { buildsBclOnly "Vesper.Array" }

            // Vesper.Seq is covered by `PackageBuildTests`, which also loads its public types.

            test "Vesper.Set builds BCL-only" { buildsBclOnly "Vesper.Set" }

            test "Vesper.Printf builds BCL-only (structural-printer.clr.fs + formatter.clr.fs)" {
                buildsBclOnly "Vesper.Printf"
            }
        ]
