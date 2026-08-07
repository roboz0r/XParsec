module XParsec.FSharp.Codegen.Js.Tests.OptionalParamTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// W3 (per-parameter optional/rest + dedup): the provider's member path now carries the
// extractor's per-param `Optional` flag through to the seam. A trailing run of optional
// parameters (`greet(name, title?)`, `readFile(path, cb, opts?)`) is recorded as the
// member's `OptionalDefaults`, so the SHARED optional-fill seam
// (`InferExternalCall.tryFillOptionalCall` + `ElaborateExpr.optionalDefaultNode` — the same
// machinery a .NET `[<Optional>]` rides) admits a call that omits the trailing suffix and
// synthesises each omitted slot as `undefined` (the `unit`→`undefined` JS value repr).
// Independently, two overloads that erase to the SAME `argSig` after numeric/structural
// degradation now DEDUP (keep-first) rather than abort — node's overload storms collapse
// pervasively, so the former `ErasedDistinction` throw was untenable.

/// `optlib`: an `Opts` config interface (a structural-width target) and an `Api` interface
/// whose methods exercise the optional-fill seam —
///   • `greet(name, title?)` — a trailing optional STRING, omittable;
///   • `readFile(path, cb, opts?)` — a callback param plus a trailing optional INTERFACE
///     arg (structural width when supplied, omittable when not);
///   • `log(x: number)` declared TWICE — a post-degradation duplicate that must dedup, not
///     throw, at provider construction.
/// `api: Api` is the instance value the method calls dispatch on.
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
                        // Two overloads erasing to the SAME argSig (a post-degradation
                        // duplicate). The former behaviour THREW here (`ErasedDistinction`);
                        // the provider must now keep the first and drop the twin.
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

/// Provider construction is EAGER (the `TsManifestSymbolProvider` ctor expands every
/// member): building this at module load already exercises the `log` dedup — a revived
/// `ErasedDistinction` abort would fail every test in the list, not just one.
let private contract = contractTs manifest

let private provider: IExternalSymbolProvider = contract.Provider

/// Bind the external `api` value to a LOCAL first: an instance-method call dispatches
/// on a local-binding receiver (`a.greet …`), the shape the dot-access / instance-probe
/// path resolves — a bare external value folds into an unresolvable qualified name.
let private withApi (body: string) : string = "let a = api\n" + body

let private analyseErrors (input: string) : string list =
    analyseWith provider (withApi input) |> List.map (fun d -> d.Message)

/// Emit through the `optlib` provider, injecting a stub runtime module so the `api`
/// value import resolves (the synthetic package has no `.toml` asset).
let private emitApi (input: string) : string =
    emitWith contract (Map.ofList [ "optlib", { FileName = "optlib.mjs"; Source = "" } ]) false (withApi input)

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
                // The fill synthesises the omitted `title?` slot as `undefined` (the
                // `unit`→`undefined` value repr), so codegen sees a full call.
                let js = emitApi "a.greet(\"x\")\n"
                Expect.stringContains js "greet" (sprintf "expected a `greet` call, got:\n%s" js)

                Expect.stringContains
                    js
                    "undefined"
                    (sprintf "expected the omitted optional filled with `undefined`, got:\n%s" js)
            }

            test "a callback arg plus a supplied config-object optional widens by structure" {
                // `opts?` supplied as a Vesper record — the structural-width admission at
                // the foreign-call arg position (gated on the interface). The callback maps
                // to a curried `string -> string -> unit`.
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
                // The module-level `provider` already constructed without throwing (the dedup
                // itself); this pins that the surviving `log` member still resolves.
                let errs = analyseErrors "a.log(1)\n"
                Expect.isEmpty errs (sprintf "the deduped `log` overload should resolve, got: %A" errs)
            }
        ]
