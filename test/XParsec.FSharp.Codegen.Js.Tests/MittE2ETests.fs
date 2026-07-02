module XParsec.FSharp.Codegen.Js.Tests.MittE2ETests

open System.IO
open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// R4a STEP 4 — the mitt full-fidelity GATE. A Vesper program drives mitt's COMPLETE
// public surface — the DEFAULT-exported generic factory `mitt<Events>()`, `on`, `off`,
// `emit` WITH a payload, `emit` WITHOUT a payload (the conditional-fold overload), and a
// read of `all` — over a ≥2-key `Events` record with DIFFERENT payload types
// (`ping:int`, `pong:string`, `tick:undefined`), emitted via Codegen.Js and RUN against
// the real vendored `mitt.mjs` under Node. Reads ONLY committed files (the golden
// manifest, the vendored runtime, the program below); the Node extractor is NEVER run.
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

let private named n = Schema.TypeRef.Named(n, [])
let private intT = named "int"
let private strT = named "string"
let private unitT = named "unit"

let private param' (name: string) (ty: Schema.TypeRef) : Schema.Param =
    {
        Name = name
        Type = ty
        Optional = false
        Rest = false
    }

let private sig0 (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = []
        Returns = ret
    }

let private sig1 (pn: string) (pt: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param' pn pt ]
        Returns = ret
    }

let private method' (name: string) (sg: Schema.Signature) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Method
        Type = None
        Signatures = [ sg ]
        Static = false
        Optional = false
    }

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

// ─── mitt provider + fixtures ────────────────────────────────────────────────

let private fixtureDir =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures", "mitt")

let private mittManifestJson =
    File.ReadAllText(Path.Combine(fixtureDir, "mitt.manifest.json"))

let private mittRuntimeSource =
    File.ReadAllText(Path.Combine(fixtureDir, "dist", "mitt.mjs"))

let private manifest =
    match Codec.deserialize mittManifestJson with
    | Error e -> failwithf "mitt manifest does not parse: %s" e
    | Ok man -> man

let private mittProvider: IExternalSymbolProvider =
    ExternalSymbols.stack
        ValueNone
        []
        [
            TsManifestProvider.providerOfManifest manifest
            TsManifestProvider.providerOfManifest recorderManifest
            jsProvider.Value
        ]

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
            // Read `all` — mitt's `Map` of registered handlers (reads-only coverage).
            "let allMap = e.all"
            ""
        ]

let private harness =
    String.concat
        "\n"
        [
            "import { pingResult, pongResult, tickResult, allMap } from \"./mitt-program.mjs\";"
            "console.log(`${pingResult},${pongResult},${tickResult},${allMap instanceof Map}`);"
            ""
        ]

let private emitWithMitt (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost mittProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

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

    let ctx: EmitJs.WalkCtx =
        {
            Resolver = ValueNone
            Source = ValueSome input
            Records = System.Collections.Generic.Dictionary()
            Unions = System.Collections.Generic.Dictionary()
            Classes = System.Collections.Generic.Dictionary()
            Enums = System.Collections.Generic.Dictionary()
            Provider = ValueSome mittProvider
            ExternalUnions = System.Collections.Generic.Dictionary()
            // No default-import wiring: the DEFAULT-import lowering below flows purely
            // from the provider-stamped `ExternalSymbol.ImportForm` on mitt's factory.
            Imports = JsImports.create runtime
            ExportTopLevel = true
            CompiledFns = System.Collections.Generic.Dictionary()
            LocalInterfaces = System.Collections.Generic.HashSet()
        }

    (JsPrint.print (EmitJs.buildProgram ctx frozen)).Source

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

                match
                    runNodeFiles
                        "mitt-e2e"
                        [
                            "harness.mjs", harness
                            "mitt-program.mjs", js
                            "mitt.mjs", mittRuntimeSource
                            "recorder.mjs", recorderRuntime
                        ]
                with
                | None -> () // node absent — the emit + type-check above still ran
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    // ping observed 7 (emit-with-payload) and STILL 7 after off+emit(99)
                    // (off removed the handler); pong "hi"; tick fired once (no-payload
                    // emit); `all` is the real Map.
                    Expect.equal out "7,hi,1,true" (sprintf "full-surface round-trip, got:\n%s" out)
            }
        ]
