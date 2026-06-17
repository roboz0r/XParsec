module XParsec.FSharp.Codegen.Js.Tests.PrintfEmissionTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js printf emission"
        [
            test "`printfn \"hi\"` emits `console.log(\"hi\")`" {
                Expect.equal (emit "printfn \"hi\"") "console.log(\"hi\");\n" "the emitted ESM source"
            }

            test "`printfn \"hi\"` compiles to ESM that prints `hi` under Node" {
                let outDir = tmpDir "codegen-js-step0a"
                let jsPath = IO.Path.Combine(outDir, "hi.mjs")

                let artifact =
                    Codegen.compile
                        { JsProjectInfo.defaults "Hi" with
                            OutputPath = Some jsPath
                        }
                        (frozenOf "printfn \"hi\"")

                Codegen.materialise artifact
                Expect.isTrue (IO.File.Exists jsPath) "the .js was written"

                match runNode jsPath with
                | None -> skiptest "node not found on PATH"
                | Some(exitCode, output) ->
                    Expect.equal exitCode 0 (sprintf "node exits 0 (output: %s)" output)
                    Expect.equal (output.Replace("\r", "").Trim()) "hi" "Node prints hi"
            }
        ]
