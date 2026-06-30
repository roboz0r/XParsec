module XParsec.FSharp.Codegen.Js.Tests.MethodAxisGenericTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Phase 3.5 (Piece A) consumer half: a METHOD-AXIS generic member resolves, emits, and
// runs end-to-end, with its method typar FRESHENED per call site. `Id.identity<U>(x: U)
// : U` is called at TWO different types in one program; both must type-check and run,
// which is only possible if `U` (`FTTypar(TyparAxis.Method, 0)` through the provider,
// `MethodTypar 0` on the wire) is instantiated INDEPENDENTLY per call — the proof that
// the new `MethodTypar` contract bump rides the proven per-call freshening path
// (`ExternalSymbols.instantiateSignature` via `commitExternalOverload`).
//
// `identity` carries a SECOND, deliberately-non-matching overload (`identity(x, y)`, a
// 2-arity sham). It is never selected for the 1-arg calls — its only role is to push
// `identity` onto the multi-candidate overload-commit path, which is the proven site
// that freshens method typars (the single-candidate field-walk does not; staying on the
// existing path here respects the `feedback_codegen_js_owns_assignability` guardrail
// against unifier/overload edits). A class STATIC method is used deliberately: the class
// path stamps a real assembly origin onto the member key (the v1 free-function path
// leaves `asm=None`).

let private idManifestJson =
    """{
  "schemaVersion": 1,
  "package": "idlib",
  "version": null,
  "exports": [
    {
      "export": "class",
      "name": "Id",
      "typeParams": 0,
      "members": [
        {
          "name": "identity",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 1,
              "params": [ { "name": "x", "type": { "k": "methodTypar", "i": 0 }, "optional": false, "rest": false } ],
              "returns": { "k": "methodTypar", "i": 0 } },
            { "typeParams": 0,
              "params": [
                { "name": "x", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false },
                { "name": "y", "type": { "k": "named", "name": "float", "args": [] }, "optional": false, "rest": false } ],
              "returns": { "k": "named", "name": "float", "args": [] } }
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

let private idProvider: IExternalSymbolProvider =
    match Codec.deserialize idManifestJson with
    | Error e -> failwithf "idlib manifest does not parse: %s" e
    | Ok man -> ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest man; jsProvider.Value ]

/// Hand-authored runtime backing the `idlib` manifest. A single-argument external
/// static-member call passes its argument directly (`$Id_identity(x)`).
let private idRuntimeSource = "export function Id_identity(x) { return x; }\n"

// Call the SAME generic method at int and at string in one program — `U` must freshen
// per call (int for the first, string for the second) or the second use fails to type.
let private program =
    String.concat
        "\n"
        [
            "let a = Id.identity(5)"
            "let b = Id.identity(\"hi\")"
            "printfn \"%d %s\" a b"
            ""
        ]

let private emitWithId (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost idProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "idlib",
                {
                    FileName = "idlib.mjs"
                    Source = idRuntimeSource
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
            Provider = ValueSome idProvider
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
        "MethodAxisGeneric"
        [
            test "a method-axis generic member freshens per call and round-trips under Node" {
                // The emit itself proves `Id.identity` resolves at TWO instantiations of its
                // method typar (int, string) — a single-axis collapse would fail one of them.
                let js = emitWithId program

                Expect.isTrue
                    (js.Contains "Id_identity")
                    (sprintf "expected a mangled static-member ref `Id_identity`, got:\n%s" js)

                match
                    runNodeFiles "method-axis-generic" [ "method-axis-generic.mjs", js; "idlib.mjs", idRuntimeSource ]
                with
                | None -> () // node absent — exec test skips, the emit above still ran
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    Expect.equal out "5 hi" (sprintf "round-trip output, got:\n%s" out)
            }
        ]
