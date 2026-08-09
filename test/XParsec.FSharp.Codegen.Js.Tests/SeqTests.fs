module XParsec.FSharp.Codegen.Js.Tests.SeqTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// The generated `Vesper.Seq.mjs` source (library mode).
let private generated: Lazy<string> =
    lazy
        compileLibrary
            seqDepsJsContract.Value
            "Vesper.Seq"
            "seq.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Seq" "seq.fs"))

/// Normalise line endings so a CRLF checkout still matches the printer's `\n` output.
let private lf (s: string) : string = s.Replace("\r\n", "\n")

/// The dependency assets `Vesper.Seq.mjs` imports: `zeroCreate` for `toArray`'s buffer,
/// `enumeratorOf` for the cursor's `GetEnumerator`. A consumer ships these too.
let private depAssets: (string * string) list =
    [
        "Vesper.Array.mjs", IO.File.ReadAllText(srcFile "Vesper.Array" "Vesper.Array.mjs")
        "Vesper.Core.mjs", IO.File.ReadAllText(srcFile "Vesper.Core" "Vesper.Core.mjs")
    ]

[<Tests>]
let tests =
    testList
        "Codegen.Js Seqs"
        [
            test "the generated module exports every function seq.fsi declares" {
                let src = generated.Value

                for name in [ "fold"; "reduce"; "truncate"; "toArray" ] do
                    Expect.stringContains src (sprintf "export const %s = " name) (sprintf "exports %s" name)
            }

            test "the truncate cursor emits as a class pair with the capability slots" {
                // `interface seq<'T>` on a class lowers to `*[Symbol.iterator]()`, and the
                // cursor's `Dispose` to `[Symbol.dispose]()`. The same source compiles for
                // CLR against the real `IEnumerator\`1`.
                let src = generated.Value
                Expect.stringContains src "class TruncateSeq" "emits the truncating view"
                Expect.stringContains src "class TruncateEnumerator" "emits its cursor"
                Expect.stringContains src "[Symbol.iterator]" "the view IS iterable"
                Expect.stringContains src "[Symbol.dispose]" "the cursor forwards disposal"
            }

            test "toArray allocates through Vesper.Array, never a raw newarr" {
                // The shared body spells no allocation intrinsic; the buffer comes from the
                // Array package instead.
                Expect.stringContains
                    (generated.Value)
                    "from \"./Vesper.Array.mjs\""
                    "zeroCreate is imported, not inlined as an allocation intrinsic"
            }

            test "the committed Vesper.Seq.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.Seq" "Vesper.Seq.mjs"

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
                            "import { fold, reduce, truncate, toArray } from \"./Vesper.Seq.mjs\";"
                            // A plain JS array is already iterable, so it IS a `seq<'T>`.
                            "const xs = [1, 2, 3, 4, 5];"
                            "console.log(fold((acc) => (x) => acc + x, 0, xs));"
                            "console.log(reduce((a) => (b) => a * b, xs));"
                            "console.log(toArray(truncate(3, xs)).join(\",\"));"
                            // Truncating past the end yields the whole source, not padding.
                            "console.log(toArray(truncate(99, xs)).join(\",\"));"
                            "console.log(toArray(truncate(0, xs)).length);"
                            // Round-trips an empty source through the doubling buffer.
                            "console.log(toArray([]).length);"
                            // Crosses the initial capacity of 4, so the buffer grows twice.
                            "console.log(toArray([1,2,3,4,5,6,7,8,9,10]).join(\",\"));"
                            "try { reduce((a) => (b) => a + b, []); console.log(\"NO_THROW\"); }"
                            + " catch (e) { console.log(e.message); }"
                        ]

                match
                    runNodeFiles
                        "seq-module-node"
                        ([ "driver.mjs", driver; "Vesper.Seq.mjs", generated.Value ] @ depAssets)
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        out
                        (String.concat
                            "\n"
                            [
                                "15"
                                "120"
                                "1,2,3"
                                "1,2,3,4,5"
                                "0"
                                "0"
                                "1,2,3,4,5,6,7,8,9,10"
                                "The input sequence was empty. (Parameter 'source')"
                            ])
                        "fold/reduce/truncate/toArray, buffer growth, and the empty-source throw"
            }

            test "truncate is LAZY: it pulls only what was asked for" {
                // A generator counting its pulls shows the source is not run to exhaustion,
                // and that an INFINITE source terminates at all.
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { truncate, toArray } from \"./Vesper.Seq.mjs\";"
                            "let pulled = 0;"
                            "function* naturals() { let i = 1; while (true) { pulled++; yield i++; } }"
                            "console.log(toArray(truncate(3, naturals())).join(\",\"));"
                            // 3 pulls, not 4: MoveNext stops AT the limit without stepping past it.
                            "console.log(pulled);"
                        ]

                match
                    runNodeFiles
                        "seq-truncate-lazy"
                        ([ "driver.mjs", driver; "Vesper.Seq.mjs", generated.Value ] @ depAssets)
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)
                    Expect.equal out (String.concat "\n" [ "1,2,3"; "3" ]) "3 elements, and exactly 3 pulls"
            }
        ]
