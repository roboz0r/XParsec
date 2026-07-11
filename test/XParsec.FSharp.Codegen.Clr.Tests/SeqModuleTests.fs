module XParsec.FSharp.Codegen.Clr.Tests.SeqModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Seq` — the `Seq` module over
// `seq<'T>` (= `IEnumerable<'T>`). The four minimal reference impls are eager
// explicit-enumerator terminals (`fold`/`reduce`/`toArray`, each driving
// `source.GetEnumerator()` / `MoveNext` / `Current`) plus the lazy `truncate`
// (delegating to `System.Linq.Enumerable.Take<TSource>`).
//
// Both test groups use DRIVER PROGRAMS (the reflection route does not fit here —
// every function takes a real `seq<'T>`):
//   * `SeqModuleRuntime` — `fold` / `reduce` / `toArray` / `truncate` over a
//     concrete `seq<int>`.
//   * `SeqFrontEnd` — the cheap analysis-only regression guard.
//
// The `seq<'T>` source is `System.Linq.Enumerable.Range(start, count)` — a real
// BCL `IEnumerable<int>`. Folders / reducers are *curried* (`fun s -> fun x -> …`)
// because Elaborate emits multi-arg lambdas as curried.

// ---- driver programs: fold / reduce / toArray / truncate --------------------

[<Tests>]
let runtimeTests =
    let prelude = "open Vesper.Collections\n"
    // `Range(start, count)` yields `start, start+1, …` — a concrete `seq<int>`.
    let range1to (n: int) =
        sprintf "(System.Linq.Enumerable.Range(1, %d))" n

    testList
        "SeqModuleRuntime"
        [
            // `fold` threads the accumulator across every element: summing
            // 1..3 = 6 proves the enumerator loop visits each element and feeds
            // its value to the folder (`callvirt Fun::Invoke`).
            test "Seq.fold sums the elements (1+2+3)" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s -> fun x -> s + x) 0 %s)" (range1to 3))
            }

            // Counting with `s + 1` proves the loop runs exactly once per element
            // (independent of the element values).
            test "Seq.fold counts the elements" {
                runsSeq
                    "4"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s -> fun x -> s + 1) 0 %s)" (range1to 4))
            }

            // `fold` over the empty sequence returns the seed untouched (the
            // `while e.MoveNext()` loop never enters).
            test "Seq.fold over the empty sequence returns the initial state" {
                runsSeq
                    "42"
                    (prelude
                     + "printfn \"%d\" (Seq.fold (fun s -> fun x -> s + x) 42 (System.Linq.Enumerable.Range(0, 0)))")
            }

            // `reduce` seeds with the first element then folds the rest:
            // 1+2+3 = 6. Proves the `MoveNext`-before-seed + the empty-guard path.
            test "Seq.reduce folds from the first element (1+2+3)" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.reduce (fun a -> fun b -> a + b) %s)" (range1to 3))
            }

            // A single-element sequence reduces to that element (zero loop
            // iterations after the seed).
            test "Seq.reduce of a singleton is the element" {
                runsSeq
                    "1"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.reduce (fun a -> fun b -> a + b) %s)" (range1to 1))
            }

            // `toArray` materialises the sequence into a `'T[]`; `.Length` reads
            // the count back (`ldlen`) — proves the `ResizeArray` accumulation +
            // `ToArray()` round-trip.
            test "Seq.toArray materialises the sequence (length)" {
                runsSeq "5" (prelude + sprintf "printfn \"%%d\" (Seq.toArray %s).Length" (range1to 5))
            }

            // Reading an element of the materialised array (`.[i]` → `ldelem`)
            // proves `toArray` preserved the values in order.
            test "Seq.toArray preserves element values" {
                runsSeq
                    "3"
                    (prelude
                     + sprintf "let xs : int[] = Seq.toArray %s\n" (range1to 5)
                     + "printfn \"%d\" xs.[2]")
            }

            // `truncate` is lazy (`Enumerable.Take`): folding the truncated view
            // sums only the first `count` elements — 1+2 = 3 from 1..5.
            test "Seq.truncate keeps at most count elements (sum of first two)" {
                runsSeq
                    "3"
                    (prelude
                     + sprintf "printfn \"%%d\" (Seq.fold (fun s -> fun x -> s + x) 0 (Seq.truncate 2 %s))" (range1to 5))
            }

            // Truncating to more than the sequence holds yields the whole
            // sequence (1+2+3 = 6 from 1..3, `truncate 10`).
            test "Seq.truncate past the end yields the whole sequence" {
                runsSeq
                    "6"
                    (prelude
                     + sprintf
                         "printfn \"%%d\" (Seq.fold (fun s -> fun x -> s + x) 0 (Seq.truncate 10 %s))"
                         (range1to 3))
            }
        ]

// ---- front-end regression guard (analysis only) ------------------------------
// `Seq.fold` / `reduce` / `truncate` / `toArray` type-check through the Seq
// contract stack without running.

[<Tests>]
let frontEndTests =
    let prelude = "open Vesper.Collections\n"

    testList
        "SeqFrontEnd"
        [
            test "Seq.fold with a curried folder type-checks" {
                typeChecksSeq (
                    prelude
                    + "let sum (xs: seq<int>) : int = Seq.fold (fun s -> fun x -> s + x) 0 xs"
                )
            }

            test "Seq.reduce type-checks" {
                typeChecksSeq (
                    prelude
                    + "let total (xs: seq<int>) : int = Seq.reduce (fun a -> fun b -> a + b) xs"
                )
            }

            test "Seq.truncate type-checks (preserves the element type)" {
                typeChecksSeq (prelude + "let firstFew (xs: seq<int>) : seq<int> = Seq.truncate 4 xs")
            }

            test "Seq.toArray type-checks" {
                typeChecksSeq (prelude + "let arr (xs: seq<int>) : int[] = Seq.toArray xs")
            }
        ]
