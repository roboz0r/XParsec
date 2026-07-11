module XParsec.FSharp.Codegen.Clr.Tests.ArrayModuleTests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Array` (vesper-lib-test-plan Phase 3),
// the codebase's first *generic intrinsic* (`'T[]`) to emit end-to-end. The
// surface has grown from the `zeroCreate`/`fold` starter to the FSharp.Core-shaped
// subset built only from counted index loops, indexed read (`arr.[i]` → `ldelem`)
// and write (`arr.[i] <- v` → `stelem`), `.Length` (`ldlen`), and `Vesper.Fun`
// application: length/isEmpty/get/set/create/init/copy/append/rev/map/mapi/iter/
// iteri/fold/foldBack. Three test groups:
//   * `Array` — REFLECTION-INVOKE over `buildPackage "Vesper.Array"` for the pure
//     data producer `zeroCreate` (its result is a BCL `int[]`, asserted directly).
//   * `ArrayModuleRuntime` — DRIVER PROGRAMS for `fold` (the HOF) and the
//     `.Length` / `.[i]` read + `.[i] <- v` write intrinsics.
//   * `ArrayModuleSurface` — DRIVER PROGRAMS for the grown module functions.
//
// Arrays are built through our own `zeroCreate` / `init` / `create`, so every row
// stays on the BCL-only path (no FSharp.Core `ArrayModule.OfList` from an
// `[| … |]` literal). Folders/mappers are *curried* (`fun s -> fun x -> …`) per
// the Elaborate multi-arg-lambda gap the plan documents.

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
// …`) per the Elaborate multi-arg-lambda gap the plan documents.

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

            // `arr.[i] <- v` lowers to `stelem <elem>` (the `SetArray` inline
            // body); writing then reading the slot proves the store landed.
            test "indexed assignment writes an element" {
                runsArray
                    "7"
                    ("open Vesper.Collections\n"
                     + "let xs : int[] = Array.zeroCreate 3\n"
                     + "xs.[1] <- 7\n"
                     + "printfn \"%d\" xs.[1]")
            }

            // The store mutates in place: other slots stay zero-initialised.
            test "indexed assignment leaves other slots untouched" {
                runsArray
                    "0"
                    ("open Vesper.Collections\n"
                     + "let xs : int[] = Array.zeroCreate 3\n"
                     + "xs.[0] <- 9\n"
                     + "printfn \"%d\" xs.[2]")
            }
        ]

// ---- expanded module surface: behavioural driver programs --------------------
// The grown `Array` surface (length/isEmpty/get/set/create/init/copy/append/rev/
// map/mapi/iter/iteri/foldBack). Each is built only from counted loops, indexed
// get/set, `.Length`, and `Vesper.Fun` application — so every row stays on the
// BCL-only path. Arrays are constructed via `Array.init` / `Array.create` /
// `Array.zeroCreate` (never `[| … |]` literals, which would route through
// FSharp.Core), and results read back with `Array.fold` / `Array.get` / `.[i]`.

[<Tests>]
let surfaceTests =
    let prelude = "open Vesper.Collections\n"
    // Sum an int[] — the standard "read back every element" probe.
    let sumDecl =
        "let sum (a: int[]) : int = Array.fold (fun s -> fun x -> s + x) 0 a\n"

    testList
        "ArrayModuleSurface"
        [
            test "length reports the element count" {
                runsArray "4" (prelude + "printfn \"%d\" (Array.length (Array.init 4 (fun i -> i)))")
            }

            test "isEmpty is true for the empty array" {
                runsArray
                    "true"
                    (prelude
                     + "let xs : int[] = Array.zeroCreate 0\n"
                     + "printfn \"%b\" (Array.isEmpty xs)")
            }

            test "isEmpty is false for a non-empty array" {
                runsArray "false" (prelude + "printfn \"%b\" (Array.isEmpty (Array.init 2 (fun i -> i)))")
            }

            test "get reads the element at an index" {
                runsArray "9" (prelude + "printfn \"%d\" (Array.get (Array.init 4 (fun i -> i * i)) 3)")
            }

            test "set writes the element at an index" {
                runsArray
                    "5"
                    (prelude
                     + "let xs : int[] = Array.zeroCreate 3\n"
                     + "Array.set xs 1 5\n"
                     + "printfn \"%d\" (Array.get xs 1)")
            }

            test "create fills every slot with the value" {
                runsArray "21" (prelude + sumDecl + "printfn \"%d\" (sum (Array.create 3 7))")
            }

            test "init builds from the index generator" {
                // [|0;1;2;3|] sums to 6.
                runsArray "6" (prelude + sumDecl + "printfn \"%d\" (sum (Array.init 4 (fun i -> i)))")
            }

            test "copy duplicates the elements" {
                runsArray
                    "3"
                    (prelude
                     + sumDecl
                     + "printfn \"%d\" (sum (Array.copy (Array.init 3 (fun i -> i))))")
            }

            test "copy is a distinct array (mutating the copy leaves the source)" {
                runsArray
                    "0"
                    (prelude
                     + "let xs : int[] = Array.zeroCreate 3\n"
                     + "let ys : int[] = Array.copy xs\n"
                     + "Array.set ys 0 9\n"
                     + "printfn \"%d\" (Array.get xs 0)")
            }

            test "append concatenates two arrays (length)" {
                runsArray
                    "4"
                    (prelude
                     + "printfn \"%d\" (Array.length (Array.append (Array.init 2 (fun i -> i)) (Array.init 2 (fun i -> i))))")
            }

            test "append concatenates two arrays (contents)" {
                // [|0;1|] ++ [|10;11|] sums to 22.
                runsArray
                    "22"
                    (prelude
                     + sumDecl
                     + "printfn \"%d\" (sum (Array.append (Array.init 2 (fun i -> i)) (Array.init 2 (fun i -> i + 10))))")
            }

            test "rev reverses the order" {
                // rev [|0;1;2|] = [|2;1;0|]; head is 2.
                runsArray "2" (prelude + "printfn \"%d\" (Array.get (Array.rev (Array.init 3 (fun i -> i))) 0)")
            }

            test "map applies the function to every element" {
                // map (+10) [|0;1;2|] = [|10;11;12|]; sums to 33.
                runsArray
                    "33"
                    (prelude
                     + sumDecl
                     + "printfn \"%d\" (sum (Array.map (fun x -> x + 10) (Array.init 3 (fun i -> i))))")
            }

            test "mapi feeds the index to the function" {
                // mapi (fun i _ -> i) over a length-3 array = [|0;1;2|]; sums to 3.
                runsArray
                    "3"
                    (prelude
                     + sumDecl
                     + "printfn \"%d\" (sum (Array.mapi (fun i -> fun x -> i) (Array.init 3 (fun i -> i))))")
            }

            test "iter visits every element in order" {
                runsArrayLines
                    [ "0"; "1"; "2" ]
                    (prelude + "Array.iter (fun x -> printfn \"%d\" x) (Array.init 3 (fun i -> i))")
            }

            test "iteri pairs each element with its index" {
                // print i for each slot of a length-3 array → 0,1,2.
                runsArrayLines
                    [ "0"; "1"; "2" ]
                    (prelude
                     + "Array.iteri (fun i -> fun x -> printfn \"%d\" i) (Array.init 3 (fun i -> i))")
            }

            test "foldBack threads right-to-left" {
                // foldBack (fun x acc -> x - acc) [|1;2;3|] 0 = 1-(2-(3-0)) = 2.
                runsArray
                    "2"
                    (prelude
                     + "printfn \"%d\" (Array.foldBack (fun x -> fun acc -> x - acc) (Array.init 3 (fun i -> i + 1)) 0)")
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

            test "indexed assignment type-checks" {
                typeChecksArray "let put (xs: int[]) (v: int) : unit = xs.[0] <- v"
            }

            test "Array.set / Array.get type-check" {
                typeChecksArray "open Vesper.Collections\nlet put (xs: int[]) (v: int) : unit = Array.set xs 0 v"
                typeChecksArray "open Vesper.Collections\nlet at (xs: int[]) : int = Array.get xs 0"
            }

            test "Array.map / Array.iter type-check" {
                typeChecksArray "open Vesper.Collections\nlet bump (xs: int[]) : int[] = Array.map (fun x -> x + 1) xs"

                typeChecksArray
                    "open Vesper.Collections\nlet shout (xs: int[]) : unit = Array.iter (fun x -> printfn \"%d\" x) xs"
            }
        ]
