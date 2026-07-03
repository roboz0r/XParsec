module XParsec.FSharp.Codegen.Js.Tests.MittE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// R4a STEP 4 / R5 STEP 5 — the mitt full-fidelity GATE. A Vesper program drives mitt's
// COMPLETE public surface — the DEFAULT-exported generic factory `mitt<Events>()`, `on`,
// `off`, `emit` WITH a payload, `emit` WITHOUT a payload (the conditional-fold overload),
// and `all` — over a ≥2-key `Events` record with DIFFERENT payload types (`ping:int`,
// `pong:string`, `tick:undefined`), emitted via Codegen.Js and RUN against the real
// vendored `mitt.mjs` under Node. Reads ONLY committed files (the golden manifest, the
// vendored runtime, the program below); the Node extractor is NEVER run.
//
// R5: `all` is a full `Js.Map`, not reads-only. mitt's `all: Map<"*" | keyof Events, …>`
// carries a `Map` ref homed to `es2015` (`Refs` table); with the es2015 pack STACKED
// under mitt (`MittFixture.provider` / `stackTsMany [mitt; recorder; es2015]`) that homed
// ref resolves as a real `Js.Map`, so a MEMBER CALL on it (`e.all.has("pong")`) types and
// runs against the real runtime Map — the graduation from the old reads-only pin.
//
// The handlers observe through a tiny auxiliary `recorder` external object (the BusE2E
// stash idiom — a Vesper lambda calling a captured native member), NOT a module-level
// `let mutable`: mutable-captured-in-a-closure is a separate Vesper JS codegen gap
// (the cell is accessed as `x.contents` but declared as a bare value) orthogonal to this
// gate. The recorder proves each mitt call actually FIRED against the real runtime.
//
// ── walls status (was the R3 residue; now the gate) ──
//  1. UNANNOTATED `mitt()` leaves `Events` ungrounded. POLICY: annotation-required (an
//     external generic factory with nothing to solve `Events` from is ungrounded on
//     purpose, exactly as TS needs a use/`as` to infer it). Pinned by
//     `UnannotatedMittTests`; the gate program annotates `let e : Emitter<MyEvents>`.
//  2. annotation arity mismatch — CLOSED (arity law; `SymbolKeyOps.arityName`).
//  3. `on`/`emit`/`off`'s `type` = `Key extends keyof Events` — CLOSED: the extractor
//     carries the `keyof` bound and a syntactic string constant grounds `Key` at the
//     overload-commit seam (`admitLiteralMethodTypars`).
//  4. `on`/`off`'s `handler` = `Handler<Events[Key]>` — CLOSED: the extractor emits a
//     FAITHFUL `(Events[Key]) -> unit` (the `__type` anonymous-alias-application degrade
//     is gone) and the front end folds `Events[Key]` per key; `off`'s optional
//     `Handler<…> | undefined` union member is admitted at the commit seam.
//  5. `emit`'s payload = `Events[Key]` — CLOSED: the `T[K]` fold types the payload; the
//     no-payload `undefined extends Events[Key] ? Key : never` overload folds its
//     conditional once `Key` is grounded from the constant (`methodTyparConstantSeed`).
//
// ── documented residual precision gap (NOT a wall — orthogonal, deferred by design) ──
//  `undefined` is not yet a registered intrinsic type DISTINCT from `unit` (design
//  §"`null`/`undefined` as JS-intrinsic types — step 0 OUTSTANDING"): a `unit`-typed
//  event is wrongly ACCEPTED by the no-payload `emit` overload. The conditional-fold
//  machinery is correct; the imprecision is undefined-vs-unit identity. Pinned in
//  `UnannotatedMittTests`.

// ─── auxiliary recorder (a stateful native object the handlers write to) ──────

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
// real `Js.Map` (Step 5) — a member call on `e.all` types and emits native `.has(`.
let private mittProvider: IExternalSymbolProvider =
    stackTsMany [ MittFixture.manifest; recorderManifest; es2015Manifest ]

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
            // `all` is mitt's `Js.Map` of registered handlers (homed es2015 ref). A MEMBER
            // CALL on it — `has` a key the pong handler registered — exercises the graduated
            // (no-longer-reads-only) capability against the real runtime Map. `pong` is
            // registered and never `off`'d, so `has("pong")` is deterministically true.
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

// No default-import wiring is needed: the DEFAULT-import lowering flows purely from the
// provider-stamped `ExternalSymbol.ImportForm` on mitt's factory.
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

    emitWith mittProvider runtime true input

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

                // on/off/emit are NATIVE prototype calls, not mangled receiver-first imports.
                Expect.isTrue (js.Contains ".on(") (sprintf "expected native `.on(`:\n%s" js)
                Expect.isTrue (js.Contains ".emit(") (sprintf "expected native `.emit(`:\n%s" js)
                Expect.isTrue (js.Contains ".off(") (sprintf "expected native `.off(`:\n%s" js)

                // R5: the member call on the homed `Js.Map` `all` lowers to a native `.has(`.
                Expect.isTrue (js.Contains ".has(") (sprintf "expected the graduated `all` member call `.has(`:\n%s" js)

                // ping observed 7 (emit-with-payload) and STILL 7 after off+emit(99) (off
                // removed the handler); pong "hi"; tick fired once (no-payload emit); `all`
                // is the real Map AND a member call on it (`has("pong")`) returns true.
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
