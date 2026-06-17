module XParsec.FSharp.Codegen.Js.Tests.Step5bPhase3Tests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// The generated `Vesper.List.mjs` source (deps-only provider, library mode).
let private generated: Lazy<string> =
    lazy compileLibrary coreDepsJsProvider.Value "Vesper.List" (IO.File.ReadAllText(srcFile "Vesper.List" "list.js.fs"))

/// Normalise line endings so a CRLF checkout still matches the printer's `\n` output.
let private lf (s: string) : string = s.Replace("\r\n", "\n")

[<Tests>]
let tests =
    testList
        "Codegen.Js Step5b-Phase3"
        [
            test "library mode exports a top-level value binding" {
                Expect.equal
                    (emitJsLibrary "let answer = 42")
                    "export const answer = 42;\n"
                    "top-level `let` → `export const` (vs script mode's `const`)"
            }

            test "library mode exports a flat function binding" {
                Expect.equal
                    (emitJsLibrary "let add x y = x + y")
                    "export const add = (x, y) => (((x) + (y)) | 0);\n"
                    "the binding name is the source identifier, the value the flat multi-arg arrow"
            }

            test "script mode keeps top-level bindings as plain const" {
                Expect.equal
                    (emitJs "let answer = 42")
                    "const answer = 42;\n"
                    "Kind defaults to Script — no `export` (the runnable-program behaviour)"
            }

            test "the generated module exports the subset functions" {
                let src = generated.Value

                for name in
                    [
                        "fold"
                        "isEmpty"
                        "length"
                        "head"
                        "tail"
                        "map"
                        "filter"
                        "append"
                        "rev"
                    ] do
                    Expect.stringContains src (sprintf "export const %s = " name) (sprintf "exports %s" name)

                // The cons-list classes are emitted into the module itself — no runtime import.
                Expect.stringContains src "class List_Cons extends List" "emits the cons subclass"
                Expect.isFalse (src.Contains "import ") "the subset imports nothing"
            }

            test "the committed Vesper.List.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.List" "Vesper.List.mjs"

                if Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" = "1" then
                    IO.File.WriteAllText(path, generated.Value)

                Expect.equal
                    (lf generated.Value)
                    (lf (IO.File.ReadAllText path))
                    "committed asset is stale — rerun with UPDATE_SNAPSHOTS=1"
            }

            test "the generated module runs under Node (consumer builds plain cons cells)" {
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { fold, length, head, map, filter, rev, isEmpty, append } from \"./Vesper.List.mjs\";"
                            // Plain {tag,Head,Tail} consumer cells are interchangeable with List_Cons — no instanceof.
                            "const cons = (h, t) => ({ tag: 1, Head: h, Tail: t });"
                            "const empty = { tag: 0 };"
                            "const xs = cons(1, cons(2, cons(3, empty)));"
                            "console.log(length(xs));"
                            "console.log(head(xs));"
                            "console.log(isEmpty(empty));"
                            "console.log(isEmpty(xs));"
                            "console.log(head(rev(xs)));"
                            // The exported functions are FLAT (Fable-style); the folder/mapping
                            // closures stay curried (they are first-class function args).
                            "console.log(head(map((x) => x * 10, xs)));"
                            "console.log(length(append(xs, xs)));"
                            "console.log(fold((s) => (x) => (s + x), 0, xs));"
                            "console.log(length(filter((x) => x > 1, xs)));"
                            "try { head(empty); console.log(\"NO_THROW\"); } catch (e) { console.log(e.message); }"
                        ]

                match runNodeFiles "list-js-phase3" [ "driver.mjs", driver; "Vesper.List.mjs", generated.Value ] with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        out
                        (String.concat
                            "\n"
                            [
                                "3"
                                "1"
                                "true"
                                "false"
                                "3"
                                "10"
                                "6"
                                "6"
                                "2"
                                "The input list was empty."
                            ])
                        "length/head/isEmpty/rev/map/append/fold/filter + empty-list throw"
            }
        ]
