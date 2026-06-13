module XParsec.FSharp.Codegen.Clr.Tests.UseSetSampleTests

open System.IO
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The Phase-9 exit deliverable: the `samples/UseSet/` consumer demo, gated.
// Rather than duplicate the program
// inline, this reads the on-disk `samples/UseSet/Program.fs` and drives it through
// the same `runsSet` harness the operation table uses (build `Vesper.Set` + its
// eight transitive deps, load into `packageAlc`, run, assert stdout). So the
// committed sample is the *exact* source that runs here — it cannot rot into a
// program that no longer compiles or whose output drifts.

/// `samples/UseSet/Program.fs`, relative to this test file
/// (`test/XParsec.FSharp.Codegen.Clr.Tests/`).
let private useSetProgram: string =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "samples", "UseSet", "Program.fs")

[<Tests>]
let tests =
    testList
        "UseSetSample"
        [
            test "samples/UseSet/Program.fs runs end-to-end against the built Vesper.Set" {
                let src = File.ReadAllText useSetProgram

                runsSetLines
                    [
                        "4" // Set.count primes
                        "true" // Set.contains 5 primes
                        "false" // Set.contains 4 primes
                        "7" // union
                        "1" // intersect
                        "3" // difference
                        "7" // primes + evens
                        "3" // primes - evens
                        "34" // fold over doubled
                        "3" // filter < 6
                        "true" // forall > 0
                        "true" // exists = 7
                        "2" // partition lo count
                        "2" // partition hi count
                        "2" // minElement
                        "7" // maxElement
                        "4" // ofList (toList primes)
                        "4" // ofArray (toArray primes)
                        "3" // iter over difference …
                        "5"
                        "7"
                    ]
                    src
            }
        ]
