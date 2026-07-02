module XParsec.FSharp.Codegen.Js.Tests.BusE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// R3 (ts-provider plan): the real Vesper e2e that proves the R1+R2 consumption machinery
// end-to-end on the EASIEST honest path — a NON-GENERIC emitter-style external object.
// A Vesper program creates the object, REGISTERS A HANDLER (a Vesper lambda passed as a
// callback argument to a native member — new territory beyond R2's int-only args), emits
// an event, and reads back the value the handler observed. Everything runs against a
// hand-authored stateful runtime under Node.
//
// The new machinery this exercises (vs R2): a Vesper lambda flowing as an argument INTO a
// native `receiver.member(args)` call, with the lambda's parameter type inferred from the
// manifest member's function-typed parameter (`handler: int -> unit`), and the lambda body
// itself making a further native member call on a captured receiver.

// ─── Hand-built manifest (no JSON round-trip) ──────────────────────────────

let private named n = Schema.TypeRef.Named(n, [])
let private intT = named "int"
let private unitT = named "unit"

/// `int -> unit`, the event-handler callback type.
let private handlerT = Schema.TypeRef.Fun([ intT ], unitT)

let private sig0 (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = []
        Returns = ret
    }

let private param' (name: string) (ty: Schema.TypeRef) : Schema.Param =
    {
        Name = name
        Type = ty
        Optional = false
        Rest = false
    }

let private sig1 (pname: string) (pty: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param' pname pty ]
        Returns = ret
    }

let private sig2
    (p1n: string)
    (p1t: Schema.TypeRef)
    (p2n: string)
    (p2t: Schema.TypeRef)
    (ret: Schema.TypeRef)
    : Schema.Signature =
    {
        TypeParams = 0
        TypeParamBounds = []
        Params = [ param' p1n p1t; param' p2n p2t ]
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
                    []
                )
                Schema.Export.Function("makeBus", [ sig0 (named "Bus") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
    }

let private busProvider: IExternalSymbolProvider =
    ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest busManifest; jsProvider.Value ]

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
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost busProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "buslib",
                {
                    FileName = "buslib.mjs"
                    Source = busRuntime
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
            Provider = ValueSome busProvider
            ExternalUnions = System.Collections.Generic.Dictionary()
            Imports = JsImports.create runtime
            ExportTopLevel = true
            CompiledFns = System.Collections.Generic.Dictionary()
            LocalInterfaces = System.Collections.Generic.HashSet()
        }

    (JsPrint.print (EmitJs.buildProgram ctx frozen)).Source

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
                // front-end parse gap unrelated to R3.
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

                match
                    runNodeFiles
                        "bus-e2e"
                        [
                            "harness.mjs", resultHarness
                            "bus-program.mjs", js
                            "buslib.mjs", busRuntime
                        ]
                with
                | None -> () // node absent — the emit above still ran + asserted
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    // The handler observed the emitted payload (7) and stashed it.
                    Expect.equal out "7" (sprintf "round-trip output, got:\n%s" out)
            }
        ]
