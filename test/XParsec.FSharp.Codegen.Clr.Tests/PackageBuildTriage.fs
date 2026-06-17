module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTriage

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// Assert a package builds to a BCL-only DLL (empty `FSharpCoreDependencies`).
let private buildsBclOnly (package: string) : unit =
    let _, artifact = (buildPackage package).Value

    Expect.isEmpty
        artifact.FSharpCoreDependencies
        (sprintf "%s must compile to a BCL-only DLL (FSharp.Core deps: %A)" package artifact.FSharpCoreDependencies)

[<Tests>]
let tests =
    testList
        "PackageBuildTriage"
        [

            test "Vesper.Choice builds BCL-only" { buildsBclOnly "Vesper.Choice" }

            test "Vesper.Option builds BCL-only" { buildsBclOnly "Vesper.Option" }

            test "Vesper.Result builds BCL-only" { buildsBclOnly "Vesper.Result" }

            test "Vesper.Array builds BCL-only" { buildsBclOnly "Vesper.Array" }

            test "Vesper.Seq builds BCL-only" { buildsBclOnly "Vesper.Seq" }

            test "Vesper.Set builds BCL-only" { buildsBclOnly "Vesper.Set" }

            test "Vesper.Printf builds BCL-only (structural-printer.fs + formatter.fs, PP7d)" {
                buildsBclOnly "Vesper.Printf"
            }
        ]
