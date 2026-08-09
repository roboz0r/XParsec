module XParsec.FSharp.Codegen.Js.Tests.MemberOverloadTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A TS method with N call signatures publishes N distinct keyed members, and selection by
// arity + argument type must resolve end-to-end (front end and JS emit). A real exported
// class's static member emits the mangled `Calc_add` import, never an erased bare export.

/// A `calc` package whose static `Calc.add` carries THREE overloads: `(float)`,
/// `(float, float)` and `(string, string)`.
let private calcManifestJson =
    """{
  "schemaVersion": 1,
  "package": "calc",
  "version": null,
  "exports": [
    {
      "export": "class",
      "name": "Calc",
      "typeParams": 0,
      "members": [
        {
          "name": "add",
          "kind": "method",
          "type": null,
          "signatures": [
            {
              "typeParams": 0,
              "params": [
                { "name": "x", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false }
              ],
              "returns": { "k": "named", "name": "float", "args": [] }
            },
            {
              "typeParams": 0,
              "params": [
                { "name": "x", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false },
                { "name": "y", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false }
              ],
              "returns": { "k": "named", "name": "float", "args": [] }
            },
            {
              "typeParams": 0,
              "params": [
                { "name": "x", "type": { "k": "named", "name": "string", "args": [] }, "optional": false, "rest": false },
                { "name": "y", "type": { "k": "named", "name": "string", "args": [] }, "optional": false, "rest": false }
              ],
              "returns": { "k": "named", "name": "string", "args": [] }
            }
          ],
          "static": true,
          "optional": false
        }
      ],
      "heritage": [],
      "import": "named"
    }
  ]
}"""

let private calcContract =
    match Codec.deserialize calcManifestJson with
    | Error e -> failwithf "calc manifest does not parse: %s" e
    | Ok man -> contractTs man

/// Emit `input` through the `calc` contract. The stub `calc.mjs` stands in for a runtime
/// asset the synthetic package does not have, without which the import throws.
let private emitWithCalc (input: string) : string =
    emitWith calcContract (Map.ofList [ "calc", { FileName = "calc.mjs"; Source = "" } ]) false input

[<Tests>]
let tests =
    testList
        "MemberOverloads"
        [
            test "arity selects the overload (add/1 and add/2 both resolve)" {
                // A provider keeping one signature per name would fail one of these two
                // arities, so both resolving is the assertion.
                let js1 = emitWithCalc "Calc.add(1.0)"
                let js2 = emitWithCalc "Calc.add(1.0, 2.0)"

                Expect.isTrue
                    (js1.Contains "Calc_add")
                    (sprintf "add/1 should emit a mangled static-member ref, got:\n%s" js1)

                Expect.isTrue
                    (js2.Contains "Calc_add")
                    (sprintf "add/2 should emit a mangled static-member ref, got:\n%s" js2)
            }

            test "argument TYPE selects the overload (string args pick the string/string overload)" {
                // Strings are not assignable to the `(float, float)` params, so this
                // type-checks only if selection reads argument TYPE and not just arity.
                let js = emitWithCalc "Calc.add(\"a\", \"b\")"

                // External static-method args arrive as one tupled array: `$Calc_add(["a", "b"])`.
                Expect.isTrue
                    (js.Contains "Calc_add([\"a\", \"b\"])")
                    (sprintf "the string overload should resolve + emit, got:\n%s" js)
            }

            test "a real class static member is NOT erased (mangled ref, never a bare export)" {
                let js = emitWithCalc "Calc.add(1.0, 2.0)"

                Expect.isTrue
                    (js.Contains "Calc_add([1, 2])")
                    (sprintf "expected a mangled `Calc_add([1, 2])` static-member call, got:\n%s" js)
            }
        ]
