module XParsec.FSharp.Codegen.Js.Tests.BusE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// The real Vesper e2e that proves the external-object consumption machinery (type
// resolution + native member calls) end-to-end on the EASIEST honest path — a
// NON-GENERIC emitter-style external object. A Vesper program creates the object,
// REGISTERS A HANDLER (a Vesper lambda passed as a callback argument to a native member —
// new territory beyond plain int-only args), emits an event, and reads back the value the
// handler observed. Everything runs against a hand-authored stateful runtime under Node.
//
// The new machinery this exercises (vs plain member calls): a Vesper lambda flowing as an argument INTO a
// native `receiver.member(args)` call, with the lambda's parameter type inferred from the
// manifest member's function-typed parameter (`handler: int -> unit`), and the lambda body
// itself making a further native member call on a captured receiver.

// ─── Hand-built manifest (no JSON round-trip); builders from `SchemaDsl` ────────

let private intT = named "int"
let private unitT = named "unit"

/// `int -> unit`, the event-handler callback type.
let private handlerT = fn [ intT ] unitT

/// `buslib`: a NON-GENERIC stateful event bus. `on` takes a name and a `int -> unit`
/// handler; `emit` fires all handlers registered for a name with an int payload;
/// `record`/`last` let the handler stash the observed payload back through the bus so a
/// Vesper-only program can read what the handler saw (no mutable capture needed).
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

// State (registered handlers + last recorded payload) lives on `this`; `emit` invokes
// each handler with the payload, and the Vesper handler calls back through `this.record`.
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
    emitWith
        busContract
        (Map.ofList
            [
                "buslib",
                {
                    FileName = "buslib.mjs"
                    Source = busRuntime
                }
            ])
        true
        input

let private resultHarness =
    "import { result } from \"./bus-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "BusE2E"
        [
            test "a Vesper program registers a lambda handler, emits, and observes it fire" {
                // Every member call originates in Vesper source: `on` receives a Vesper
                // lambda (`fun payload -> bus.record(payload)`) whose param type is inferred
                // from the manifest `handler: int -> unit`; `emit` fires it; the handler
                // stashes the payload via `bus.record`; `bus.last` reads it back. Effectful
                // unit calls are bound (`let u = …`) — a bare mid-sequence unit call is a
                // front-end parse gap unrelated to this e2e.
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

                // The on/emit calls must be NATIVE prototype methods, not mangled
                // receiver-first free-fn imports.
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
