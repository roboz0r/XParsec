module XParsec.FSharp.Codegen.Js.Tests.NullUndefinedTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Phase 0: `null` / `undefined` as first-class intrinsic types. A TS `T | null` /
// `T | undefined` rides in as a `TyOr`/`FTOr` member; this pins that such a type
// resolves through the provider, survives JS emit (no `PlatformTypes` reject), and
// runs under Node round-tripping present / absent values.
//
// `N` is a class with FOUR static methods (the class path stamps a real assembly
// origin onto the member key, unlike the v1 free-function path):
//   pickName   : bool -> (string | null)
//   pickMiddle : bool -> (string | undefined)
//   renderN    : (string | null)      -> string
//   renderU    : (string | undefined) -> string
// The renderers' parameter type EXACTLY matches each producer's return, so no
// union-assignability is exercised — only the type's end-to-end survival.

let private nullManifestJson =
    """{
  "schemaVersion": 1,
  "package": "nulllib",
  "version": null,
  "exports": [
    {
      "export": "class",
      "name": "N",
      "typeParams": 0,
      "members": [
        {
          "name": "pickName",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 0,
              "params": [ { "name": "present", "type": { "k": "named", "name": "bool", "args": [] }, "optional": false, "rest": false } ],
              "returns": { "k": "union", "members": [
                { "k": "named", "name": "null", "args": [] },
                { "k": "named", "name": "string", "args": [] } ] } }
          ],
          "static": true,
          "optional": false
        },
        {
          "name": "pickMiddle",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 0,
              "params": [ { "name": "present", "type": { "k": "named", "name": "bool", "args": [] }, "optional": false, "rest": false } ],
              "returns": { "k": "union", "members": [
                { "k": "named", "name": "undefined", "args": [] },
                { "k": "named", "name": "string", "args": [] } ] } }
          ],
          "static": true,
          "optional": false
        },
        {
          "name": "renderN",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 0,
              "params": [ { "name": "value", "type": { "k": "union", "members": [
                { "k": "named", "name": "null", "args": [] },
                { "k": "named", "name": "string", "args": [] } ] }, "optional": false, "rest": false } ],
              "returns": { "k": "named", "name": "string", "args": [] } }
          ],
          "static": true,
          "optional": false
        },
        {
          "name": "renderU",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 0,
              "params": [ { "name": "value", "type": { "k": "union", "members": [
                { "k": "named", "name": "undefined", "args": [] },
                { "k": "named", "name": "string", "args": [] } ] }, "optional": false, "rest": false } ],
              "returns": { "k": "named", "name": "string", "args": [] } }
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

let private nullProvider: IExternalSymbolProvider =
    match Codec.deserialize nullManifestJson with
    | Error e -> failwithf "nulllib manifest does not parse: %s" e
    | Ok man -> ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest man; jsProvider.Value ]

/// Hand-authored JS runtime backing the `nulllib` manifest. A single-argument
/// external static-member call passes its argument directly (`$N_renderN(value)`),
/// so each flat export takes one plain parameter.
let private nullRuntimeSource =
    """export function N_pickName(present) { return present ? "hi" : null; }
export function N_pickMiddle(present) { return present ? "yo" : undefined; }
export function N_renderN(value) { return value === null ? "NULL" : value; }
export function N_renderU(value) { return value === undefined ? "UNDEF" : value; }
"""

let private program =
    String.concat
        "\n"
        [
            "let r1 = N.renderN(N.pickName(true))"
            "let r2 = N.renderN(N.pickName(false))"
            "let r3 = N.renderU(N.pickMiddle(true))"
            "let r4 = N.renderU(N.pickMiddle(false))"
            "printfn \"%s %s %s %s\" r1 r2 r3 r4"
            ""
        ]

/// Emit `input` to JS through the `nulllib` provider, injecting the runtime module.
let private emitWithNull (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost nullProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "nulllib",
                {
                    FileName = "nulllib.mjs"
                    Source = nullRuntimeSource
                }
            ]

    let ctx: EmitJs.WalkCtx =
        {
            Resolver = ValueNone
            Source = ValueSome input
            Records = System.Collections.Generic.Dictionary()
            Unions = System.Collections.Generic.Dictionary()
            Classes = System.Collections.Generic.Dictionary()
            Enums = System.Collections.Generic.Dictionary()
            Provider = ValueSome nullProvider
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
        "NullUndefined"
        [
            test "`T | null` and `T | undefined` survive JS emit and round-trip under Node" {
                // The emit itself proves the types survive `PlatformTypes` (no
                // "no representation on the target" reject) and JS lowering.
                let js = emitWithNull program

                match runNodeFiles "null-undefined" [ "null-undefined.mjs", js; "nulllib.mjs", nullRuntimeSource ] with
                | None -> () // node absent — exec test skips, the emit above still ran
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    // r1 present (string), r2 absent (null), r3 present (string), r4 absent (undefined).
                    Expect.equal out "hi NULL yo UNDEF" (sprintf "round-trip output, got:\n%s" out)
            }
        ]
