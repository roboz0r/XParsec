module XParsec.FSharp.Codegen.Js.Tests.OptionalParamTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A trailing run of optional parameters (`greet(name, title?)`) is recorded as the
// member's `OptionalDefaults`, so a call may omit the suffix and each omitted slot is
// synthesised as `undefined`. Overloads erasing to one `argSig` dedup, keeping the first.

/// `optlib`: an `Opts` config interface, and an `Api` whose `greet(name, title?)` and
/// `readFile(path, cb, opts?)` exercise the optional fill while `log(x: number)` is
/// declared TWICE. `api: Api` is the instance value the method calls dispatch on.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "optlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Opts",
                    0,
                    [ property' "retries" (named "number"); optProperty' "verbose" (named "bool") ],
                    [],
                    []
                )
                Schema.Export.Interface(
                    "Api",
                    0,
                    [
                        method'
                            "greet"
                            (sigG
                                0
                                [ param' "name" (named "string"); optParam' "title" (named "string") ]
                                (named "string"))
                        method'
                            "readFile"
                            (sigG
                                0
                                [
                                    param' "path" (named "string")
                                    param' "cb" (fn [ named "string"; named "string" ] (named "unit"))
                                    optParam' "opts" (named "Opts")
                                ]
                                (named "unit"))
                        // Two overloads erasing to the SAME argSig: the provider keeps the
                        // first and drops the twin.
                        methodOf
                            "log"
                            false
                            [
                                sig1 "x" (named "number") (named "unit")
                                sig1 "x" (named "number") (named "unit")
                            ]
                    ],
                    [],
                    []
                )
                Schema.Export.Variable("api", named "Api", true, Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

/// Provider construction is EAGER, expanding every member, so building this at module
/// load already exercises the `log` dedup; an abort there fails the whole list, not one test.
let private contract = contractTs manifest

let private provider: IExternalSymbolProvider = contract.Provider

/// Bind the external `api` to a LOCAL first: `a.greet …` dispatches on a local-binding
/// object argument, whereas a bare `api.greet` folds into an unresolvable qualified name.
let private withApi (body: string) : string = "let a = api\n" + body

let private analyseErrors (input: string) : string list =
    analyseWith provider (withApi input) |> List.map (fun d -> d.Message)

/// Emit through the `optlib` provider with a stub runtime module, so the `api` import
/// resolves (the synthetic package has no `.toml` asset).
let private emitApi (input: string) : string =
    emitWith contract (Map.ofList [ "optlib", JsRuntimeModule.ofSource "optlib.mjs" "" ]) false (withApi input)

[<Tests>]
let tests =
    testList
        "OptionalParam"
        [
            test "omitting a trailing optional parameter type-checks" {
                let errs = analyseErrors "a.greet(\"x\")\n"
                Expect.isEmpty errs (sprintf "omitting the trailing optional `title?` should be admitted, got: %A" errs)
            }

            test "supplying a trailing optional parameter type-checks" {
                let errs = analyseErrors "a.greet(\"x\", \"sir\")\n"
                Expect.isEmpty errs (sprintf "supplying the trailing optional `title?` should type-check, got: %A" errs)
            }

            test "an omitted trailing optional lowers to `undefined`" {
                // The fill synthesises the omitted `title?` slot as `undefined`, so codegen
                // sees a full call.
                let js = emitApi "a.greet(\"x\")\n"
                Expect.stringContains js "greet" (sprintf "expected a `greet` call, got:\n%s" js)

                Expect.stringContains
                    js
                    "undefined"
                    (sprintf "expected the omitted optional filled with `undefined`, got:\n%s" js)
            }

            test "a callback arg plus a supplied config-object optional widens by structure" {
                // `opts?` supplied as a Vesper record: structural-width admission at the
                // foreign-call arg position. The callback maps to a curried
                // `string -> string -> unit`.
                let errs =
                    analyseErrors
                        "type Cfg = { retries: int; verbose: bool }\na.readFile(\"p\", (fun (err: string) (data: string) -> ()), { retries = 1; verbose = true })\n"

                Expect.isEmpty errs (sprintf "callback + config-object width should type-check, got: %A" errs)
            }

            test "omitting a trailing optional INTERFACE arg (callback present) type-checks" {
                let errs =
                    analyseErrors "a.readFile(\"p\", (fun (err: string) (data: string) -> ()))\n"

                Expect.isEmpty errs (sprintf "omitting the trailing optional `opts?` should be admitted, got: %A" errs)
            }

            test "overloads erasing to the same argSig DEDUP rather than abort" {
                // Module load already exercised the dedup; this pins that the surviving
                // `log` member still resolves.
                let errs = analyseErrors "a.log(1)\n"
                Expect.isEmpty errs (sprintf "the deduped `log` overload should resolve, got: %A" errs)
            }
        ]
