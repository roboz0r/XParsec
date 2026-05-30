module XParsec.FSharp.Codegen.Clr.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-sprint-phase-4 Step 4.2 / B-6 backend tests. `for x in src do body`
// over an `IEnumerable<'T>` lowers to the standard enumerator loop:
//   let e = src.GetEnumerator() in
//   try while e.MoveNext() do (let x = e.Current in body)
//   finally if e <> null then e.Dispose()
// The four member slots are resolved through the *interface* declaring types
// (IEnumerable<'T> / IEnumerator<'T> / IEnumerator / IDisposable), so a
// `callvirt` dispatches to the source collection's implementation. The source
// here is a BCL `System.Collections.Generic.List<int>` (the metadata provider
// surfaces its `IEnumerable<int>` interface for the front-end element-type
// probe). Asserting on captured stdout proves the loop walks the elements in
// order and terminates (the empty case prints nothing).

[<Tests>]
let forInTests =
    testList
        "ForIn"
        [
            test "for-in over an empty BCL List<int> runs and prints nothing" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = new System.Collections.Generic.List<int>()"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "ForInEmpty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "empty list yields no iterations"
            }

            test "for-in over a populated IEnumerable<int> prints the elements in order" {
                // Source it from `System.Linq.Enumerable.Range` — a static method
                // returning a non-empty `IEnumerable<int>` directly, so the test
                // exercises a populated walk without depending on external
                // *instance*-method calls (`xs.Add`, a separate resolution gap).
                let src =
                    String.concat "\n" [ "for x in System.Linq.Enumerable.Range(1, 3) do"; "    printfn \"%d\" x" ]

                let _, artifact = compileSource "ForInPopulated" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the sequence in order"
            }
        ]
