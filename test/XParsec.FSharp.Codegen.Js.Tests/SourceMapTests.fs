module XParsec.FSharp.Codegen.Js.Tests.SourceMapTests

open System
open System.Text.Json
open Expecto
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// V3 source maps. The `loc` threaded onto each emitted `JsExpr` (from its `TExprG`
// node's `'tok`) lets the printer emit a `.js.map` that resolves the generated
// `console.log` back to the `printfn` call's source position.

/// Compile `input` with its own text as the source, so source-map emission is on.
let private compileWithMap (input: string) (outputPath: string option) : JsArtifact =
    let project =
        { JsProjectInfo.defaults "Hi" with
            OutputPath = outputPath
            Source = Some { Path = "hi.fsx"; Content = input }
        }

    Codegen.compile project (frozenOf input)

[<Tests>]
let tests =
    testList
        "Codegen.Js source maps"
        [
            test "supplying source emits a `sourceMappingURL` comment" {
                let artifact = compileWithMap "printfn \"hi\"" None
                // The mapped JS keeps the 0a body, then the trailing comment.
                Expect.equal
                    (Codegen.toSource artifact)
                    "console.log(\"hi\");\n//# sourceMappingURL=Hi.mjs.map\n"
                    "the ESM source plus the sourceMappingURL comment"
            }

            test "no source text → no map, unchanged 0a output" {
                let artifact =
                    Codegen.compile (JsProjectInfo.defaults "Hi") (frozenOf "printfn \"hi\"")

                Expect.equal (Codegen.toSource artifact) "console.log(\"hi\");\n" "0a output unchanged"
                Expect.isNone (Codegen.toSourceMap artifact) "no map without source"
            }

            test "the map is well-formed V3 and resolves `console.log` to source origin" {
                let artifact = compileWithMap "printfn \"hi\"" None

                match Codegen.toSourceMap artifact with
                | None -> failtest "expected a source map"
                | Some mapJson ->
                    use doc = JsonDocument.Parse mapJson
                    let root = doc.RootElement
                    Expect.equal (root.GetProperty("version").GetInt32()) 3 "version 3"

                    Expect.equal (root.GetProperty("sources").[0].GetString()) "hi.fsx" "the source file name"

                    Expect.equal
                        (root.GetProperty("sourcesContent").[0].GetString())
                        "printfn \"hi\""
                        "the embedded source content"

                    // `console.log` is generated at (line 0, col 0); `printfn` is
                    // the first source token, also at (0, 0). The single segment
                    // is therefore four zero deltas — VLQ `AAAA`.
                    Expect.equal (root.GetProperty("mappings").GetString()) "AAAA" "the VLQ mappings"
            }

            test "the `.js` and `.js.map` are written and Node still runs the `.js`" {
                let outDir = tmpDir "codegen-js-sourcemap"
                let jsPath = IO.Path.Combine(outDir, "hi.mjs")
                let mapPath = jsPath + ".map"

                let artifact = compileWithMap "printfn \"hi\"" (Some jsPath)
                Codegen.materialise artifact

                Expect.isTrue (IO.File.Exists jsPath) "the .js was written"
                Expect.isTrue (IO.File.Exists mapPath) "the .js.map was written"

                // The map filename in the comment matches the emitted basename.
                Expect.stringContains
                    (IO.File.ReadAllText jsPath)
                    "//# sourceMappingURL=hi.mjs.map"
                    "the sourceMappingURL references the emitted map basename"

                match runNode jsPath with
                | None -> skiptest "node not found on PATH"
                | Some(exitCode, output) ->
                    Expect.equal exitCode 0 (sprintf "node exits 0 (output: %s)" output)
                    Expect.equal (output.Replace("\r", "").Trim()) "hi" "Node prints hi"
            }
        ]
