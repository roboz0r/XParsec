module XParsec.FSharp.Codegen.Js.Tests.BusE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A NON-GENERIC emitter-style external object, driven end to end under Node. A Vesper
// LAMBDA is passed to a native `objArg.member(args)` call, its parameter type inferred from
// `handler: int -> unit` in the manifest, and its own body calls back on a captured object.

// ─── Hand-built manifest (no JSON round-trip); builders from `SchemaDsl` ────────

let private intT = named "int"
let private unitT = named "unit"

let private handlerT = fn [ intT ] unitT

/// `buslib`: a NON-GENERIC stateful event bus. `record`/`last` let the handler stash the
/// payload it observed back through the bus, so a Vesper-only program can read what the
/// handler saw without capturing a mutable.
let private busManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "buslib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Bus",
                    0,
                    [
                        method' "on" (sig2 "name" (named "string") "handler" handlerT unitT)
                        method' "emit" (sig2 "name" (named "string") "payload" intT unitT)
                        method' "record" (sig1 "x" intT unitT)
                        method' "last" (sig0 intT)
                    ],
                    [],
                    []
                )
                Schema.Export.Function("makeBus", [ sig0 (named "Bus") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private busContract = contractTs busManifest

let private busRuntime =
    String.concat
        "\n"
        [
            "export function makeBus() {"
            "  return {"
            "    _handlers: {},"
            "    _last: 0,"
            "    on(name, handler) { (this._handlers[name] = this._handlers[name] || []).push(handler); },"
            "    emit(name, payload) { for (const h of (this._handlers[name] || [])) h(payload); },"
            "    record(x) { this._last = x; },"
            "    last() { return this._last; },"
            "  };"
            "}"
            ""
        ]

let private emitBus (input: string) : string =
    emitWith busContract (Map.ofList [ "buslib", JsPackageOutput.rootModule "buslib.mjs" busRuntime ]) true input

let private resultHarness =
    "import { result } from \"./bus-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "BusE2E"
        [
            test "a Vesper program registers a lambda handler, emits, and observes it fire" {
                // Effectful unit calls are bound (`let u = …`): a bare mid-sequence unit
                // call is a front-end parse gap unrelated to this e2e.
                let program =
                    String.concat
                        "\n"
                        [
                            "let bus = makeBus()"
                            "let u1 = bus.on(\"ping\", fun payload -> bus.record(payload))"
                            "let u2 = bus.emit(\"ping\", 7)"
                            "let result = bus.last()"
                            ""
                        ]

                let js = emitBus program

                Expect.isTrue (js.Contains ".on(") (sprintf "expected a native `.on(` call, got:\n%s" js)
                Expect.isTrue (js.Contains ".emit(") (sprintf "expected a native `.emit(` call, got:\n%s" js)
                Expect.isFalse (js.Contains "Bus__") (sprintf "unexpected mangled member import in:\n%s" js)

                // The handler observed the emitted payload (7) and stashed it.
                expectNodeOutput
                    "bus-e2e"
                    [
                        "harness.mjs", resultHarness
                        "bus-program.mjs", js
                        "buslib.mjs", busRuntime
                    ]
                    "7"
            }
        ]
