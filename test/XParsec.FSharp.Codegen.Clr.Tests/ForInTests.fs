module XParsec.FSharp.Codegen.Clr.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
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

            // vesper-set-sprint-phase-4 Step 4.4 — front end only. The duck-typed
            // (pattern-based `GetEnumerator()`) codegen is deferred (the project's
            // first value-type member-call IL), so this asserts the *analysis*
            // resolves the loop rather than running the program.
            test "for-in over a duck-typed source (no IEnumerable<'T>) type-checks via the pattern GetEnumerator()" {
                // `System.Collections.BitArray` implements only the *non-generic*
                // `IEnumerable`, so the §4.2 `IEnumerable<'T>` interface probe
                // misses it; the §4.4 duck-typed fallback resolves it through its
                // public `GetEnumerator(): IEnumerator`, whose `MoveNext(): bool` +
                // `Current` property drive the loop and pin the element type. Before
                // §4.4 this raised a "source is not a supported enumerable"
                // diagnostic.
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (ba: System.Collections.BitArray) ="
                            "    for x in ba do"
                            "        ()"
                        ]

                let provider, _ = SymbolProviders.buildContract defaultManifests
                let lexed, file = parseFile src
                let tast = Pipeline.analyse provider src lexed file

                let errors =
                    tast.Diagnostics
                    |> Seq.filter (fun d -> d.Severity = Severity.Error)
                    |> Seq.toList

                Expect.isEmpty errors (sprintf "duck-typed for-in should type-check; got %A" errors)
            }
        ]
