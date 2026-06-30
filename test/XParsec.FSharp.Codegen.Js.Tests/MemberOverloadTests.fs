module XParsec.FSharp.Codegen.Js.Tests.MemberOverloadTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Tier 2 item 9a (member overloads), consumer half. A TS method with N call
// signatures is expanded by the provider into N distinct keyed `ExternalMember`s
// (`TsManifestProvider.expandMethod`), `TryLookupMembers` returns the full set, and
// `UnificationInferOverload.pickBestOverload` selects by arity + argument type. This
// pins that the selection RESOLVES end-to-end (front end + JS emit) and — unlike the
// synthetic free-function grouping type (9b) — a real class's static member emits the
// ordinary mangled `Calc_add` import, NOT an erased bare export.

/// A `calc` package whose `Calc.add` static method carries THREE overloads: arity-1
/// `(float)`, arity-2 `(float, float)`, and arity-2 `(string, string)` — exercising
/// both count- and type-based overload selection, with all argSigs distinct.
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

/// The `calc` provider, layered over the standard JS provider (so `float`/`string`
/// argument types still resolve). `Calc` + its overload set come from the manifest.
let private calcProvider: IExternalSymbolProvider =
    match Codec.deserialize calcManifestJson with
    | Error e -> failwithf "calc manifest does not parse: %s" e
    | Ok man -> ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest man; jsProvider.Value ]

/// Emit `input` to JS through the `calc` provider, injecting a fake `calc` runtime
/// module so the static-member `addMemberRef` import resolves.
let private emitWithCalc (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost calcProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime = Map.ofList [ "calc", { FileName = "calc.mjs"; Source = "" } ]

    let ctx: EmitJs.WalkCtx =
        {
            Resolver = ValueNone
            Source = ValueSome input
            Records = System.Collections.Generic.Dictionary()
            Unions = System.Collections.Generic.Dictionary()
            Classes = System.Collections.Generic.Dictionary()
            Enums = System.Collections.Generic.Dictionary()
            Provider = ValueSome calcProvider
            ExternalUnions = System.Collections.Generic.Dictionary()
            Imports = JsImports.create runtime
            ExportTopLevel = false
            CompiledFns = System.Collections.Generic.Dictionary()
            LocalInterfaces = System.Collections.Generic.HashSet()
        }

    (JsPrint.print (EmitJs.buildProgram ctx frozen)).Source

[<Tests>]
let tests =
    testList
        "MemberOverloads"
        [
            test "arity selects the overload (add/1 and add/2 both resolve)" {
                // If only one signature survived the provider's `Map.ofList`, one of these
                // two arities would fail to type-check. Both resolving proves the full
                // overload set is keyed and `pickBestOverload` selects by arity.
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
                // `Calc.add("a", "b")` only type-checks if the (string, string) overload is
                // a candidate — strings are not assignable to the (float, float) params. So
                // success here is proof of TYPE-based overload selection, not just arity.
                let js = emitWithCalc "Calc.add(\"a\", \"b\")"

                // External static-method args arrive as a single tupled array (the existing
                // external-member ABI), so the emitted call is `$Calc_add(["a", "b"])`.
                Expect.isTrue
                    (js.Contains "Calc_add([\"a\", \"b\"])")
                    (sprintf "the string overload should resolve + emit, got:\n%s" js)
            }

            test "a real class static member is NOT erased (mangled ref, never a bare export)" {
                // Contrast with the 9b synthetic free-function grouping type, which erases to
                // the bare export. A real exported class keeps the ordinary static-member path.
                let js = emitWithCalc "Calc.add(1.0, 2.0)"

                Expect.isTrue
                    (js.Contains "Calc_add([1, 2])")
                    (sprintf "expected a mangled `Calc_add([1, 2])` static-member call, got:\n%s" js)
            }
        ]
