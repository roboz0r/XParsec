module XParsec.FSharp.Codegen.Clr.Tests.SeqModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness
open XParsec.FSharp.Codegen.Clr.Tests.ModuleSuiteHarness

// `Vesper.Seq` over `seq<'T>` (= `IEnumerable<'T>`). Every function takes a real
// `seq<'T>`, so these run driver programs rather than reflecting members out of
// the PE.

[<Tests>]
let runtimeTests =
    // `Range(start, count)` yields `start, start+1, …`: a BCL `IEnumerable<int>`.
    let range1to (n: int) =
        sprintf "(System.Linq.Enumerable.Range(1, %d))" n

    testList
        "SeqModuleRuntime"
        [
            test "Seq.fold sums the elements (1+2+3)" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s x -> s + x) 0 %s)" (range1to 3))
            }

            // `s + 1` ignores the element values, so the total is the iteration count.
            test "Seq.fold counts the elements" {
                runsSeq
                    "4"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s x -> s + 1) 0 %s)" (range1to 4))
            }

            test "Seq.fold over the empty sequence returns the initial state" {
                runsSeq
                    "42"
                    (prelude
                     + "printfn \"%d\" (Seq.fold (fun s x -> s + x) 42 (System.Linq.Enumerable.Range(0, 0)))")
            }

            test "Seq.reduce folds from the first element (1+2+3)" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.reduce (fun a b -> a + b) %s)" (range1to 3))
            }

            test "Seq.reduce of a singleton is the element" {
                runsSeq
                    "1"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.reduce (fun a b -> a + b) %s)" (range1to 1))
            }

            // `toArray` grows a doubling buffer, then copies it to an exactly-sized
            // `'T[]`, so `.Length` is the element count and not the buffer capacity.
            test "Seq.toArray materialises the sequence (length)" {
                runsSeq "5" (prelude + sprintf "printfn \"%%d\" (Seq.toArray %s).Length" (range1to 5))
            }

            test "Seq.toArray preserves element values" {
                runsSeq
                    "3"
                    (prelude
                     + sprintf "let xs : int[] = Seq.toArray %s\n" (range1to 5)
                     + "printfn \"%d\" xs.[2]")
            }

            // `truncate` returns a lazy view, so folding it stops at `count`: 1+2 of 1..5.
            test "Seq.truncate keeps at most count elements (sum of first two)" {
                runsSeq
                    "3"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s x -> s + x) 0 (Seq.truncate 2 %s))" (range1to 5))
            }

            test "Seq.truncate past the end yields the whole sequence" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s x -> s + x) 0 (Seq.truncate 10 %s))" (range1to 3))
            }
        ]

// Type-checks only: no compile, no run.

[<Tests>]
let frontEndTests =
    testList
        "SeqFrontEnd"
        [
            test "Seq.fold with a lambda folder type-checks" {
                typeChecksSeq (prelude + "let sum (xs: seq<int>) : int = Seq.fold (fun s x -> s + x) 0 xs")
            }

            test "Seq.reduce type-checks" {
                typeChecksSeq (prelude + "let total (xs: seq<int>) : int = Seq.reduce (fun a b -> a + b) xs")
            }

            test "Seq.truncate type-checks (preserves the element type)" {
                typeChecksSeq (prelude + "let firstFew (xs: seq<int>) : seq<int> = Seq.truncate 4 xs")
            }

            test "Seq.toArray type-checks" {
                typeChecksSeq (prelude + "let arr (xs: seq<int>) : int[] = Seq.toArray xs")
            }
        ]
