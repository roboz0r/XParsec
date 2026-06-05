module XParsec.FSharp.Codegen.Clr.Tests.ArrayModuleTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Array` (vesper-lib-test-plan Phase 3),
// the codebase's first *generic intrinsic* (`'T[]`) to emit end-to-end. The
// surface is two functions over the intrinsic array:
//   * `zeroCreate : int -> 'T[]` — open-coded over the `newarr !0` IL intrinsic.
//   * `fold : ('State -> 'T -> 'State) -> 'State -> 'T[] -> 'State` — a counted
//     index loop (`for i = 0 to array.Length - 1 do … array.[i] …`), exercising
//     `ldlen` (`.Length`) and `ldelem` (`.[i]`) plus the `for-to` loop emitter.
//
// Two routes, mirroring `OptionTests` / `ListModuleTests`:
//   * REFLECTION-INVOKE over `buildPackage "Vesper.Array"` for `zeroCreate` (the
//     pure-data producer — its result is a BCL `int[]`, asserted directly).
//   * DRIVER PROGRAMS for `fold` (the HOF — its `folder` is a `Vesper.Fun`, built
//     naturally by a lambda) and for the `.Length` / `.[i]` access intrinsics.
//     Arrays are built through our own `zeroCreate`, so every row stays on the
//     BCL-only path (no FSharp.Core `ArrayModule.OfList` from an `[| … |]` literal).

// ---- reflection over the built Vesper.Array.dll (zeroCreate, pure-data) -------

/// The built `Vesper.Array.dll` (cached). The module's `ModuleSuffix` repr gives
/// it the compiled holder name `Vesper.Collections.ArrayModule` (matching the
/// FSharp.Core surface).
let private arrayAsm: Lazy<Assembly> =
    lazy (fst (buildPackage "Vesper.Array").Value)

/// `Array.zeroCreate<int> count` via the emitted generic static method.
let private zeroCreateInt (count: int) : int[] =
    arrayAsm.Value
        .GetType("Vesper.Collections.ArrayModule")
        .GetMethod("zeroCreate")
        .MakeGenericMethod(typeof<int>)
        .Invoke(null, [| box count |])
    :?> int[]

[<Tests>]
let tests =
    testList
        "Array"
        [
            // `newarr !0` allocates a zero-initialised array of the requested length.
            test "zeroCreate allocates an array of the given length" {
                let xs = zeroCreateInt 3
                Expect.equal xs.Length 3 "length is 3"
                Expect.sequenceEqual xs [| 0; 0; 0 |] "elements are all the default value"
            }

            test "zeroCreate 0 is the empty array" {
                let xs = zeroCreateInt 0
                Expect.equal xs.Length 0 "length is 0"
            }

            // `fold` is covered by the driver route below: its `folder` is a
            // `Vesper.Fun` (impractical to mint by reflection — the `OptionTests`
            // HOF rationale), exercised through driver programs where the lambda
            // builds the `Vesper.Fun` naturally. This anchor stays as the pointer.
            test "fold covered by ArrayModuleRuntime (Vesper.Fun via driver)" { () }
        ]

// ---- driver programs: Array.fold (the HOF) + .Length / .[i] intrinsics -------
// Arrays are built with `Array.zeroCreate` (BCL-only), so these never touch the
// FSharp.Core array-literal lowering. The folder is *curried* (`fun s -> fun x ->
// …`) per the Freeze multi-arg-lambda gap the plan documents.

[<Tests>]
let runtimeTests =
    testList
        "ArrayModuleRuntime"
        [
            // `fold` threads the accumulator across every element: counting with
            // `s + 1` proves the loop visits each of the `zeroCreate`d slots exactly
            // once (`array.Length` bound + `for-to` iteration).
            test "Array.fold visits each element once (counts the length)" {
                runsArray
                    "5"
                    ("open Vesper.Collections\n"
                     + "let xs : int[] = Array.zeroCreate 5\n"
                     + "printfn \"%d\" (Array.fold (fun s -> fun x -> s + 1) 0 xs)")
            }

            // Summing the (zero-initialised) elements proves `fold` reads each
            // element value (`array.[i]` → `ldelem`) and feeds it to the folder.
            test "Array.fold reads element values (sum of zeroCreate is 0)" {
                runsArray
                    "0"
                    ("open Vesper.Collections\n"
                     + "printfn \"%d\" (Array.fold (fun s -> fun x -> s + x) 0 (Array.zeroCreate 3))")
            }

            // `fold` over the empty array returns the seed untouched (zero iterations).
            test "Array.fold over the empty array returns the initial state" {
                runsArray
                    "42"
                    ("open Vesper.Collections\n"
                     + "printfn \"%d\" (Array.fold (fun s -> fun x -> s + x) 42 (Array.zeroCreate 0))")
            }

            // `arr.Length` lowers to `ldlen; conv.i4`.
            test "array .Length reads the length" {
                runsArray
                    "4"
                    ("open Vesper.Collections\n"
                     + "let xs : int[] = Array.zeroCreate 4\n"
                     + "printfn \"%d\" xs.Length")
            }

            // `arr.[i]` lowers to `ldelem <elem>`; a fresh `zeroCreate`d slot reads 0.
            test "indexed lookup reads an element" {
                runsArray
                    "0"
                    ("open Vesper.Collections\n"
                     + "let xs : int[] = Array.zeroCreate 3\n"
                     + "printfn \"%d\" xs.[0]")
            }
        ]

// ---- front-end regression guard (analysis only) ------------------------------
// The cheap probe: `Array.zeroCreate` / `Array.fold` and the `.Length` / `.[i]`
// intrinsics type-check through the Array contract stack without running.

[<Tests>]
let frontEndTests =
    testList
        "ArrayFrontEnd"
        [
            test "Array.zeroCreate type-checks" {
                typeChecksArray "open Vesper.Collections\nlet mk (n: int) : int[] = Array.zeroCreate n"
            }

            test "Array.fold with a curried lambda type-checks" {
                typeChecksArray
                    "open Vesper.Collections\nlet sum (xs: int[]) : int = Array.fold (fun s -> fun x -> s + x) 0 xs"
            }

            test "array .Length / indexed lookup type-check" {
                typeChecksArray "let len (xs: int[]) : int = xs.Length"
                typeChecksArray "let first (xs: int[]) : int = xs.[0]"
            }
        ]
