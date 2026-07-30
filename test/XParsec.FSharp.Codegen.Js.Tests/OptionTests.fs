module XParsec.FSharp.Codegen.Js.Tests.OptionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// The generated `Vesper.Option.mjs` source (deps-only provider, library mode).
let private generated: Lazy<string> =
    lazy
        compileLibrary
            coreDepsJsContract.Value
            "Vesper.Option"
            "option.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Option" "option.fs"))

let private lf (s: string) : string = s.Replace("\r\n", "\n")

[<Tests>]
let tests =
    testList
        "Codegen.Js Option"
        [
            // ---- consumer-side: `Some`/`None` import + match against the runtime module ----

            test "Some imports the case class from the Option runtime module" {
                Expect.equal
                    (emitJs "let x = Some 5")
                    ("import { Option_Some as $Vesper_Option_Option_Some } from \"./Vesper.Option.mjs\";\n"
                     + "const x = new $Vesper_Option_Option_Some(5);\n")
                    "external Option `Some` → import the case class from its home module, then `new` it (no local re-emit)"
            }

            test "Some binds its value in a match (Some 5 → 5)" {
                match
                    runJs
                        "option-some"
                        ("let x = Some 5\n"
                         + "match x with\n"
                         + "| Some n -> printfn \"%d\" n\n"
                         + "| None -> printfn \"%d\" 0")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "Some arm binds the Value field"
            }

            test "None matches its tag (None → 0)" {
                match
                    runJs
                        "option-none"
                        ("let x : int option = None\n"
                         + "match x with\n"
                         + "| Some n -> printfn \"%d\" n\n"
                         + "| None -> printfn \"%d\" 0")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0" "None matches tag 0"
            }

            test "Option threads through a function (toInt)" {
                match
                    runJs
                        "option-fn"
                        ("let toInt o =\n"
                         + "    match o with\n"
                         + "    | Some n -> n\n"
                         + "    | None -> -1\n"
                         + "printfn \"%d\" (toInt (Some 42))\n"
                         + "printfn \"%d\" (toInt None)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42\n-1" "Some/None both route through the match"
            }

            // ---- the Vesper.Option runtime module (generated in library mode) ----

            test "the generated module exports the Option module surface" {
                let src = generated.Value

                for name in
                    [
                        "isSome"
                        "isNone"
                        "defaultValue"
                        "defaultWith"
                        "orElse"
                        "orElseWith"
                        "get"
                        "count"
                        "fold"
                        "exists"
                        "forall"
                        "iter"
                        "map"
                        "bind"
                        "flatten"
                        "filter"
                    ] do
                    Expect.stringContains src (sprintf "export const %s = " name) (sprintf "exports %s" name)

                for memberName in [ "Option__get_Value"; "Option__get_IsSome"; "Option__get_IsNone" ] do
                    Expect.stringContains
                        src
                        (sprintf "export const %s = " memberName)
                        (sprintf "exports %s" memberName)

                Expect.stringContains src "new Error(" "Value raises a constructed exception → `new Error`"
                Expect.stringContains src "class Option_Some extends Option" "emits the Some subclass"
                Expect.isFalse (src.Contains "import ") "the Option module imports nothing"
            }

            test "the committed Vesper.Option.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.Option" "Vesper.Option.mjs"

                if Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" = "1" then
                    IO.File.WriteAllText(path, generated.Value)

                Expect.equal
                    (lf generated.Value)
                    (lf (IO.File.ReadAllText path))
                    "committed asset is stale — rerun with UPDATE_SNAPSHOTS=1"
            }

            test "the generated module runs under Node (consumer builds plain option cells)" {
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { isSome, isNone, defaultValue, defaultWith, orElse, get, count, fold, exists, forall, map, bind, flatten, filter } from \"./Vesper.Option.mjs\";"
                            // Plain {tag,Value} consumer cells (never instanceof) are interchangeable with Option_Some.
                            "const some = (x) => ({ tag: 1, Value: x });"
                            "const none = { tag: 0 };"
                            "const s5 = some(5);"
                            "console.log(isSome(s5));"
                            "console.log(isNone(none));"
                            // The exported functions are FLAT (Fable-style); the mapping /
                            // folder / predicate / thunk closures stay curried (first-class args).
                            "console.log(defaultValue(0, none));"
                            "console.log(defaultValue(0, s5));"
                            "console.log(get(s5));"
                            "console.log(count(s5));"
                            "console.log(count(none));"
                            "console.log(get(map((x) => x * 2, s5)));"
                            "console.log(fold((acc) => (x) => (acc + x), 100, s5));"
                            "console.log(isSome(filter((x) => x > 3, s5)));"
                            "console.log(isSome(filter((x) => x > 9, s5)));"
                            "console.log(get(bind((x) => some(x + 1), s5)));"
                            "console.log(get(flatten(some(s5))));"
                            "console.log(exists((x) => x > 3, s5));"
                            "console.log(forall((x) => x > 9, s5));"
                            "console.log(defaultWith(() => 42, none));"
                            "console.log(isSome(orElse(s5, none)));"
                            "try { get(none); console.log(\"NO_THROW\"); } catch (e) { console.log(e.message); }"
                        ]

                match
                    runNodeFiles "option-module-node" [ "driver.mjs", driver; "Vesper.Option.mjs", generated.Value ]
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        out
                        (String.concat
                            "\n"
                            [
                                "true"
                                "true"
                                "0"
                                "5"
                                "5"
                                "1"
                                "0"
                                "10"
                                "105"
                                "true"
                                "false"
                                "6"
                                "5"
                                "true"
                                "false"
                                "42"
                                "true"
                                "Option.get: the option value was None"
                            ])
                        "isSome/isNone/defaultValue/get/count/map/fold/filter/bind/flatten/exists/forall/defaultWith/orElse + get-None throw"
            }
        ]
