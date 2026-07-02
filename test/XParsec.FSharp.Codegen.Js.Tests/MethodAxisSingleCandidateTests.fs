module XParsec.FSharp.Codegen.Js.Tests.MethodAxisSingleCandidateTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// R4a STEP 3 item 5 (ts-provider plan): the SINGLE-candidate `TryLookupMember`
// field-walk must freshen a member's method typars per call site, exactly as the
// multi-candidate overload-commit path already does
// (`ExternalSymbols.instantiateSignature` via `commitExternalOverload`).
//
// The sibling `MethodAxisGenericTests` pins the MULTI-candidate path: `Id.identity`
// carries a sham second overload precisely to force `on`-style overload commit, the
// only site that historically freshened. Here `echo` has ONE signature, so
// `tryInferExternalInstanceMethodCall` DECLINES (candidates.Length <= 1) and the call
// falls to `resolveFieldStep`'s external-`TyClass` field-walk — the path that used
// `ExternalSymbols.openSignature` (method typars left as inert `TyTypar(Method,_)`
// markers a per-call solution can never touch). Calling `echo` at TWO instantiations
// (int, string) in one program is the discriminator: shared inert markers cross-
// contaminate (or fail rigid unification), independent fresh `TyVar`s type both.

let private echoManifestJson =
    """{
  "schemaVersion": 1,
  "package": "boxlib",
  "version": null,
  "exports": [
    {
      "export": "interface",
      "name": "Box",
      "typeParams": 0,
      "members": [
        {
          "name": "echo",
          "kind": "method",
          "type": null,
          "signatures": [
            { "typeParams": 1,
              "params": [ { "name": "x", "type": { "k": "methodTypar", "i": 0 }, "optional": false, "rest": false } ],
              "returns": { "k": "methodTypar", "i": 0 } }
          ],
          "static": false,
          "optional": false
        }
      ],
      "heritage": []
    },
    {
      "export": "function",
      "name": "makeBox",
      "signatures": [ { "typeParams": 0, "params": [], "returns": { "k": "named", "name": "Box", "args": [] } } ],
      "import": "named"
    }
  ]
}"""

let private echoProvider: IExternalSymbolProvider =
    match Codec.deserialize echoManifestJson with
    | Error e -> failwithf "boxlib manifest does not parse: %s" e
    | Ok man -> ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest man; jsProvider.Value ]

/// Hand-authored runtime backing `boxlib`: `makeBox()` yields an object whose
/// `echo` instance method is the identity (so R2's `receiver.member(args)`
/// lowering round-trips the argument unchanged).
let private echoRuntimeSource =
    "export function makeBox() { return { echo(x) { return x; } }; }\n"

// A SINGLE generic instance method, called at int then at string on one receiver:
// `U` must freshen per call, or one of the two uses fails to type.
let private program =
    String.concat
        "\n"
        [
            "let b = makeBox()"
            "let a = b.echo(5)"
            "let c = b.echo(\"hi\")"
            "printfn \"%d %s\" a c"
            ""
        ]

let private analyseErrors (input: string) : string list =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost echoProvider input lexed file

    tast.Diagnostics
    |> List.filter (fun d -> d.Severity = Severity.Error)
    |> List.map (fun d -> d.Message)

let private emitWithEcho (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost echoProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "boxlib",
                {
                    FileName = "boxlib.mjs"
                    Source = echoRuntimeSource
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
            Provider = ValueSome echoProvider
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
        "MethodAxisSingleCandidate"
        [
            test "a single-candidate instance generic member freshens per call (analysis)" {
                // The discriminator: with the field-walk leaving `U` an inert
                // `TyTypar(Method,0)` marker, calling `echo` at int and string shares one
                // marker and cannot solve both — analysis errors. With per-call freshening
                // both calls type cleanly.
                let errors = analyseErrors program

                Expect.isEmpty
                    errors
                    (sprintf "single-candidate method typar must freshen per call, got errors:\n%A" errors)
            }

            test "a single-candidate instance generic member freshens per call and round-trips under Node" {
                let js = emitWithEcho program

                Expect.isTrue
                    (js.Contains "echo")
                    (sprintf "expected the instance member call `.echo(...)`, got:\n%s" js)

                match
                    runNodeFiles "method-axis-single" [ "method-axis-single.mjs", js; "boxlib.mjs", echoRuntimeSource ]
                with
                | None -> () // node absent — exec test skips, the emit above still ran
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    Expect.equal out "5 hi" (sprintf "round-trip output, got:\n%s" out)
            }
        ]
