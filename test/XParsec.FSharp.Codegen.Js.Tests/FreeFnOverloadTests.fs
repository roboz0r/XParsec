module XParsec.FSharp.Codegen.Js.Tests.FreeFnOverloadTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A TS module exporting an OVERLOADED free function `format` is modelled as a synthetic
// erased type `Util` with `format` as a static member (F# has no free-function overloading).
// `Util.format(x)` emits the bare export `format(x)`: no `Util_format` export exists.

/// A TS manifest for package `util`: the free function `format`, overloaded at `string`
/// and at `float`.
let private utilManifestJson =
    """{
  "schemaVersion": 2,
  "package": "util",
  "version": null,
  "exports": [
    {
      "export": "function",
      "name": "format",
      "signatures": [
        {
          "typeParams": 0,
          "params": [
            { "name": "x", "type": { "k": "named", "name": "string", "args": [] }, "optional": false, "rest": false }
          ],
          "returns": { "k": "named", "name": "string", "args": [] }
        },
        {
          "typeParams": 0,
          "params": [
            { "name": "x", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false }
          ],
          "returns": { "k": "named", "name": "string", "args": [] }
        }
      ],
      "import": "named"
    }
  ]
}"""

let private utilContract =
    match Codec.deserialize utilManifestJson with
    | Error e -> failwithf "util manifest does not parse: %s" e
    | Ok man -> contractTs man

/// Emit `input` through the `util` contract. The stub `util.mjs` stands in for a runtime
/// asset the synthetic package does not have, without which the import throws.
let private emitWithUtil (input: string) : string =
    emitWith utilContract (Map.ofList [ "util", JsRuntimeModule.ofSource "util.mjs" "" ]) false input

[<Tests>]
let tests =
    testList
        "FreeFnOverloads"
        [
            test "Util.format erases to the bare export (named import + bare call)" {
                let js = emitWithUtil "Util.format(\"hi\")"

                Expect.isTrue
                    (js.Contains "format as $_format")
                    (sprintf "expected a bare `format` named import (erased), got:\n%s" js)

                Expect.isTrue
                    (js.Contains "$_format(\"hi\")")
                    (sprintf "expected the call to erase to a bare `format(\"hi\")`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "Util_format")
                    (sprintf "the synthetic grouping type must ERASE — no mangled `Util_format`, got:\n%s" js)
            }
        ]
