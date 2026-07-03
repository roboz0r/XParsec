module XParsec.FSharp.Codegen.Js.Tests.FreeFnOverloadTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Tier 2 item 9b Phase 2 — the JS-emit ERASE branch for a synthetic free-function-
// overload grouping type. A TS module `util` exporting an OVERLOADED free function
// `format` is modelled by the provider as a synthetic erased type `Util` with `format`
// as a static member (F# has no free-function overloading). A call `Util.format(x)` must
// erase at JS emit to the BARE module export `format(x)` — NOT the mangled `Util_format`
// an ordinary external static member would import (no such export exists).

/// A synthetic TS manifest for package `util`: an OVERLOADED free function `format`
/// (`string | float`) — so the provider mints the synthetic erased grouping type `Util`.
let private utilManifestJson =
    """{
  "schemaVersion": 1,
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

/// The `util` package's overload provider, layered over the standard JS provider (so the
/// argument's primitive types still resolve). The synthetic `Util` type + its `format`
/// overloads come from the TS-manifest provider.
let private utilProvider: IExternalSymbolProvider =
    match Codec.deserialize utilManifestJson with
    | Error e -> failwithf "util manifest does not parse: %s" e
    | Ok man -> stackTs man

/// Emit `input` to JS through `provider`, injecting a fake `util` runtime module so the
/// erase branch's bare-export `addRef` import resolves (the synthetic package has no
/// `.toml`/`runtime-js` asset of its own — the erase contract is what this test pins).
let private emitWithUtil (input: string) : string =
    emitWith utilProvider (Map.ofList [ "util", { FileName = "util.mjs"; Source = "" } ]) false input

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
