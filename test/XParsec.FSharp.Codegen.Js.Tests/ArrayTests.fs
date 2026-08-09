module XParsec.FSharp.Codegen.Js.Tests.ArrayTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// The generated `Vesper.Array.mjs` source (library mode). The contract carries the
/// package's own prelude, which `zeroCreate` splices.
let private generated: Lazy<string> =
    lazy
        compileLibrary
            arrayDepsJsContract.Value
            "Vesper.Array"
            "array.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Array" "array.fs"))

/// Normalise line endings so a CRLF checkout still matches the printer's `\n` output.
let private lf (s: string) : string = s.Replace("\r\n", "\n")

[<Tests>]
let tests =
    testList
        "Codegen.Js Arrays"
        [
            test "the generated module exports every function array.fsi declares" {
                let src = generated.Value

                for name in
                    [
                        "zeroCreate"
                        "length"
                        "isEmpty"
                        "get"
                        "set"
                        "create"
                        "init"
                        "copy"
                        "append"
                        "rev"
                        "map"
                        "mapi"
                        "iter"
                        "iteri"
                        "fold"
                        "foldBack"
                    ] do
                    Expect.stringContains src (sprintf "export const %s = " name) (sprintf "exports %s" name)

                Expect.isFalse (src.Contains "import ") "the module imports nothing"
            }

            test "zeroCreate's `newarr` lowers to a dense Array(n).fill(null)" {
                let src = generated.Value
                Expect.stringContains src "Array(count).fill(null)" "newarr → a dense allocation"
                Expect.stringContains src ".length" "arr.Length → arr.length"
            }

            test "the committed Vesper.Array.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.Array" "Vesper.Array.mjs"

                if Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" = "1" then
                    IO.File.WriteAllText(path, generated.Value)

                Expect.equal
                    (lf generated.Value)
                    (lf (IO.File.ReadAllText path))
                    "committed asset is stale — rerun with UPDATE_SNAPSHOTS=1"
            }

            test "the generated module runs under Node" {
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { zeroCreate, length, isEmpty, get, set, create, init, copy, append, rev, map, mapi, iter, iteri, fold, foldBack } from \"./Vesper.Array.mjs\";"
                            // Every slot is a real `null`, not a hole: a sparse array would
                            // report length 3 but skip all three in `join`.
                            "const z = zeroCreate(3);"
                            "console.log(length(z), z.join(\",\"));"
                            "console.log(isEmpty(zeroCreate(0)), isEmpty(z));"
                            "const a = init(4, (i) => i + 1);"
                            "console.log(a.join(\",\"));"
                            "console.log(get(a, 2));"
                            "const s = copy(a); set(s, 0, 99);"
                            // `copy` is a real copy: mutating it leaves the source alone.
                            "console.log(s.join(\",\"), a.join(\",\"));"
                            "console.log(create(3, 7).join(\",\"));"
                            "console.log(append(a, create(2, 0)).join(\",\"));"
                            "console.log(rev(a).join(\",\"));"
                            // The exported functions are FLAT; the mapping/folder closures
                            // stay curried (they are first-class function args).
                            "console.log(map((x) => x * 10, a).join(\",\"));"
                            "console.log(mapi((i) => (x) => i * x, a).join(\",\"));"
                            "const seen = []; iter((x) => seen.push(x), a); console.log(seen.join(\",\"));"
                            "const idx = []; iteri((i) => (x) => idx.push(i + \":\" + x), a); console.log(idx.join(\",\"));"
                            "console.log(fold((acc) => (x) => acc + x, 0, a));"
                            // foldBack visits right-to-left: the string records the order.
                            "console.log(foldBack((x) => (acc) => acc + x, a, \"\"));"
                        ]

                match
                    runNodeFiles "array-module-node" [ "driver.mjs", driver; "Vesper.Array.mjs", generated.Value ]
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        out
                        (String.concat
                            "\n"
                            [
                                "3 ,,"
                                "true false"
                                "1,2,3,4"
                                "3"
                                "99,2,3,4 1,2,3,4"
                                "7,7,7"
                                "1,2,3,4,0,0"
                                "4,3,2,1"
                                "10,20,30,40"
                                "0,2,6,12"
                                "1,2,3,4"
                                "0:1,1:2,2:3,3:4"
                                "10"
                                "4321"
                            ])
                        "zeroCreate/length/isEmpty/init/get/set/copy/create/append/rev/map/mapi/iter/iteri/fold/foldBack"
            }

            // ---- consumer side: a use site imports from the runtime module ----

            test "an `Array.map` use site imports the function from Vesper.Array.mjs" {
                Expect.stringContains
                    (emitJs "let ys = Array.map (fun x -> x * 2) (Array.init 3 (fun i -> i))")
                    "from \"./Vesper.Array.mjs\""
                    "the module functions come from the runtime asset, not a local re-emit"
            }
        ]
