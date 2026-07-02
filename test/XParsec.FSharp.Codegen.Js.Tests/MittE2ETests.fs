module XParsec.FSharp.Codegen.Js.Tests.MittE2ETests

open System.IO
open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Real-package e2e: a Vesper program that USES the real `mitt` package emits JS that RUNS
// against the real vendored mitt runtime under Node — the semantic oracle proving
// `manifest → provider → emit → run` is behaviourally correct on a real package.
//
// Reads ONLY committed files: the committed `mitt.manifest.json`, the vendored
// `dist/mitt.mjs`, and the program below. The Node extractor is NEVER run here.
//
// ── What is VESPER-DRIVEN (proven here) ──
// mitt's primary export is the DEFAULT-exported generic factory
// `export default function mitt<Events>(all?): Emitter<Events>`. The Vesper program binds
// it as a VALUE (`let factory = mitt`); the front end resolves it through the manifest
// provider (with a real module-spec origin), and emit lowers it to a genuine DEFAULT
// import `import $_mitt from "./mitt.mjs"` bound to the REAL vendored runtime.
//
// ── What is RESIDUAL HARNESS GLUE (NOT yet Vesper-drivable — R4's gate) ──
// The `emitter.on`/`emitter.emit` member calls that drive the emitter still live in the
// hand-written harness, because mitt's generic surface cannot yet type-check from Vesper
// source. This is the honest R3 residue; the NON-GENERIC Vesper-driven member-call e2e
// (create + register a lambda handler + emit + observe) is proven end-to-end in
// `BusE2ETests` instead — that is R3's real proof. The precise mitt walls (all R4 scope,
// probed against this manifest + provider stack) are:
//   1. Wall #3 — even the bare factory CALL `let e = mitt()` fails: the return
//      `Emitter<Events>` leaves `Events` ungrounded → "TAST contains 1 unresolved TyVar".
//   2. An explicit annotation `let e : Emitter<int> = mitt()` does NOT rescue it: the
//      annotation mints `TyClass(Emitter)` but the provider return is `TyClass(Emitter`1)`
//      — an arity-suffix key mismatch — and the TyVar stays unresolved.
//   3. `on`/`emit`'s first `type` parameter is a raw `TyTypar(Method, 0)` marker a string
//      literal cannot unify with ("Type mismatch: string vs TyTypar(Method,0)") — the
//      single-overload method-typar freshening / string-literal-key gap.
//   4. `on`'s `handler` parameter degraded to the opaque `__type` (`TyConst "__type"`), so
//      a Vesper lambda `TyFun(_, unit)` cannot unify with it.
//   5. `emit`'s payload is the `Events[Key]` structural stub — "Type 'structural:Events[Key]'
//      could not be resolved during contract extraction".
// Items 3–5 need R4a's faithful keyof / indexed-access / string-literal-type work; items
// 1–2 need wall #3 grounding + the generic-external-key arity reconciliation. None is
// fixable without touching inference/the extractor, so they are NOT forced here.

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
    ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest manifest; jsProvider.Value ]

let private mittDefaultKeys = TsManifestProvider.defaultValueKeys manifest

// The smallest program that genuinely reaches mitt: bind the default-exported factory.
// Emitted in library mode so the binding is `export const factory`, callable by the
// harness below.
let private program = "let factory = mitt\n"

// A JS harness that drives the REAL mitt emitter through the Vesper-emitted `factory`
// export: create an emitter, register a handler, emit an event, and print what the
// handler observed. `factory` is the eta-reified Vesper binding `(_s0) => $_mitt(_s0)`
// that imports mitt's real default export — so this exercises genuine mitt code.
let private harness =
    String.concat
        "\n"
        [
            "import { factory } from \"./mitt-program.mjs\";"
            "const emitter = factory();"
            "let observed = 0;"
            "emitter.on(\"ping\", (e) => { observed = e; });"
            "emitter.emit(\"ping\", 7);"
            "console.log(observed);"
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
            Imports = JsImports.createWithDefaults runtime mittDefaultKeys
            // Library mode: top-level `let factory` emits `export const factory`, so the
            // harness can import it.
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
            test "the real mitt default-export factory imports + runs end-to-end under Node" {
                let js = emitWithMitt program

                // The Step-1 proof: a DEFAULT import of the REAL vendored mitt runtime, not
                // a named `import { mitt as … }` (which would fail — mitt has no named export).
                Expect.isTrue
                    (js.Contains "import $_mitt from \"./mitt.mjs\"")
                    (sprintf "expected a DEFAULT import of the real mitt runtime, got:\n%s" js)

                match
                    runNodeFiles
                        "mitt-e2e"
                        [
                            // Harness is the entry point; it imports the emitted factory.
                            "harness.mjs", harness
                            "mitt-program.mjs", js
                            "mitt.mjs", mittRuntimeSource
                        ]
                with
                | None -> () // node absent — exec test skips, the emit above still ran
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    // The handler observed the emitted event (7), proving the Vesper-emitted
                    // factory binding produced a working REAL mitt emitter.
                    Expect.equal out "7" (sprintf "round-trip output, got:\n%s" out)
            }
        ]
