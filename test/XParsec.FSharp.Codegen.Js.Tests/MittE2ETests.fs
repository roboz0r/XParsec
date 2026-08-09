module XParsec.FSharp.Codegen.Js.Tests.MittE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// The mitt full-fidelity GATE. A Vesper program drives the whole public surface (the
// default-exported generic `mitt<Events>()`, `on`, `off`, `emit` with and without a
// payload, `all`) and RUNS against the real vendored `mitt.mjs` under Node.

// ─── auxiliary recorder (a stateful native object the handlers write to) ──────

// The handlers observe through this external object because a module-level `let mutable`
// lowers to a `const`. Each recorded call proves the matching mitt call fired against the
// real runtime.

let private intT = named "int"
let private strT = named "string"
let private unitT = named "unit"

let private recorderManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "recorder"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Recorder",
                    0,
                    [
                        method' "pinged" (sig1 "x" intT unitT)
                        method' "ponged" (sig1 "x" strT unitT)
                        method' "ticked" (sig0 unitT)
                        method' "lastPing" (sig0 intT)
                        method' "lastPong" (sig0 strT)
                        method' "tickCount" (sig0 intT)
                    ],
                    [],
                    []
                )
                Schema.Export.Function("makeRecorder", [ sig0 (named "Recorder") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private recorderRuntime =
    String.concat
        "\n"
        [
            "export function makeRecorder() {"
            "  return {"
            "    _ping: 0, _pong: \"\", _ticks: 0,"
            "    pinged(x) { this._ping = x; },"
            "    ponged(x) { this._pong = x; },"
            "    ticked() { this._ticks = this._ticks + 1; },"
            "    lastPing() { return this._ping; },"
            "    lastPong() { return this._pong; },"
            "    tickCount() { return this._ticks; },"
            "  };"
            "}"
            ""
        ]

// ─── mitt provider + fixtures (shared fixture via `TestHelpers.MittFixture`) ──────

let private mittRuntimeSource = MittFixture.runtimeSource

// es2015 is STACKED under mitt+recorder so mitt's `all: Map<…>` homed ref resolves as a
// real `Js.Map`, and a member call on `e.all` types and emits a native `.has(`.
let private mittContract =
    contractTsMany [ MittFixture.manifest; recorderManifest; es2015Manifest ]

// The full-surface Vesper program. Effectful unit member calls are bound (`let u = …`)
// per the front-end sequencing convention.
let private program =
    String.concat
        "\n"
        [
            "type MyEvents = { ping: int; pong: string; tick: undefined }"
            "let e : Emitter<MyEvents> = mitt()"
            "let r = makeRecorder()"
            // `h` is a NAMED handler so `off` can pass the SAME reference mitt removes by.
            "let h = fun (p: int) -> r.pinged(p)"
            "let u1 = e.on(\"ping\", h)"
            "let u2 = e.on(\"pong\", fun p -> r.ponged(p))"
            "let u3 = e.on(\"tick\", fun p -> r.ticked())"
            "let u4 = e.emit(\"ping\", 7)"
            "let u5 = e.emit(\"pong\", \"hi\")"
            // No-payload emit (the conditional-fold overload) on the undefined-typed event.
            "let u6 = e.emit(\"tick\")"
            // `off` unregisters the ping handler; the subsequent emit must NOT re-fire it.
            "let u7 = e.off(\"ping\", h)"
            "let u8 = e.emit(\"ping\", 99)"
            "let pingResult = r.lastPing()"
            "let pongResult = r.lastPong()"
            "let tickResult = r.tickCount()"
            // `all` is mitt's `Js.Map` of registered handlers. `pong` is registered and
            // never `off`'d, so `has("pong")` is deterministically true against the real
            // runtime Map.
            "let allMap = e.all"
            "let allHasPong = allMap.has(\"pong\")"
            ""
        ]

let private harness =
    String.concat
        "\n"
        [
            "import { pingResult, pongResult, tickResult, allMap, allHasPong } from \"./mitt-program.mjs\";"
            "console.log(`${pingResult},${pongResult},${tickResult},${allMap instanceof Map},${allHasPong}`);"
            ""
        ]

// The DEFAULT-import lowering needs no wiring here: it flows from the import form the
// provider stamps on mitt's factory.
let private emitWithMitt (input: string) : string =
    let runtime =
        Map.ofList
            [
                "mitt",
                {
                    FileName = "mitt.mjs"
                    Source = mittRuntimeSource
                }
                "recorder",
                {
                    FileName = "recorder.mjs"
                    Source = recorderRuntime
                }
            ]

    emitWith mittContract runtime true input

[<Tests>]
let tests =
    testList
        "MittE2E"
        [
            test "mitt's complete public surface runs end-to-end under Node" {
                let js = emitWithMitt program

                // The factory lowers to a genuine DEFAULT import of the real vendored runtime
                // (mitt has no named export; a `import { mitt }` would fail).
                Expect.isTrue
                    (js.Contains "import $_mitt from \"./mitt.mjs\"")
                    (sprintf "expected a DEFAULT import of the real mitt runtime, got:\n%s" js)

                // on/off/emit are NATIVE prototype calls, not type-prefixed imports.
                Expect.isTrue (js.Contains ".on(") (sprintf "expected native `.on(`:\n%s" js)
                Expect.isTrue (js.Contains ".emit(") (sprintf "expected native `.emit(`:\n%s" js)
                Expect.isTrue (js.Contains ".off(") (sprintf "expected native `.off(`:\n%s" js)

                Expect.isTrue (js.Contains ".has(") (sprintf "expected the graduated `all` member call `.has(`:\n%s" js)

                // `7,hi,1,true,true`: ping is STILL 7 after off+emit(99), so `off` removed
                // the handler; pong "hi"; tick fired once via the no-payload emit; `allMap`
                // is a real Map; `has("pong")` on it is true.
                expectNodeOutput
                    "mitt-e2e"
                    [
                        "harness.mjs", harness
                        "mitt-program.mjs", js
                        "mitt.mjs", mittRuntimeSource
                        "recorder.mjs", recorderRuntime
                    ]
                    "7,hi,1,true,true"
            }
        ]
