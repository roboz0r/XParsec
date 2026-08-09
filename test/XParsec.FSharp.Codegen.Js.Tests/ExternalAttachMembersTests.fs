module XParsec.FSharp.Codegen.Js.Tests.ExternalAttachMembersTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// An instance-member call on an external TS-manifest object lowers to a NATIVE
// `objArg.member(args)`, because the object has genuine own methods; it is not the
// type-prefixed `$Box_get`-style free-fn import Vesper's own runtimes emit.

// ─── Hand-built manifest (no JSON round-trip); builders from `SchemaDsl` ────────

let private intT = named "int"
let private unitT = named "unit"

/// `boxlib`: a stateful `Box { get(): int; set(x: int): unit; addTo(a: int, b: int): unit;
/// value: int }` plus a `makeBox(): Box` factory whose RETURN freezes to `FTClass`, so a
/// Vesper value returned by it admits native member calls.
let private boxManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "boxlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Box",
                    0,
                    [
                        method' "get" (sig0 intT)
                        method' "set" (sig1 "x" intT unitT)
                        method' "addTo" (sig2 "a" intT "b" intT unitT)
                        property' "value" intT
                    ],
                    [],
                    []
                )
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private boxContract = contractTs boxManifest

// The factory returns a STATEFUL object: state lives in `this._v` and every method
// reads `this`, so a lowering that detached the method would observe the wrong `this`.
let private boxRuntime =
    String.concat
        "\n"
        [
            "export function makeBox() {"
            "  return {"
            "    _v: 0,"
            "    get() { return this._v; },"
            "    set(x) { this._v = x; },"
            "    addTo(a, b) { this._v = a + b; },"
            "    get value() { return this._v; },"
            "  };"
            "}"
            ""
        ]

/// Emit `input` in library mode with `boxlib` resolved to the runtime above.
let private emitBox (input: string) : string =
    emitWith boxContract (Map.ofList [ "boxlib", JsRuntimeModule.ofSource "boxlib.mjs" boxRuntime ]) true input

// Prints the library-mode `export const result` the emitted program exports, so Node's
// stdout carries the observed value.
let private resultHarness =
    "import { result } from \"./box-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "ExternalAttachMembers"
        [
            test "instance member calls lower to NATIVE objArg.member(args), no mangled import" {
                // The unit result of `set` is bound to a named `u` because a bare
                // mid-sequence `b.set(5)` parses as `Expr.Missing`, and a top-level
                // `let _ =` is not emit-supported.
                let program =
                    String.concat "\n" [ "let b = makeBox()"; "let u = b.set(5)"; "let result = b.get()"; "" ]

                let js = emitBox program

                Expect.isTrue (js.Contains ".set(") (sprintf "expected a native `.set(` call, got:\n%s" js)
                Expect.isTrue (js.Contains ".get(") (sprintf "expected a native `.get(` call, got:\n%s" js)

                Expect.isFalse (js.Contains "Box__") (sprintf "unexpected mangled member import in:\n%s" js)

                // `set(5)` then `get()` reading `this._v` returns 5.
                expectNodeOutput
                    "attach-members"
                    [
                        "harness.mjs", resultHarness
                        "box-program.mjs", js
                        "boxlib.mjs", boxRuntime
                    ]
                    "5"
            }

            test "a zero-arg method emits objArg.get() with the lone unit dropped" {
                let js =
                    emitBox (String.concat "\n" [ "let b = makeBox()"; "let result = b.get()"; "" ])
                // The `()` argument has no JS value, so it emits `objArg.get()` and never
                // `objArg.get(undefined)`.
                Expect.isTrue
                    (js.Contains ".get()")
                    (sprintf "expected `objArg.get()` (lone unit dropped), got:\n%s" js)
            }

            test "a method extracted as a VALUE eta-wraps (this bound at call) and runs" {
                // `let f = b.get` extracts the method as a value; a detached `b.get` would
                // lose `this` in JS. The eta-wrap `(_a) => b.get()` binds `this` at the call.
                let program =
                    String.concat
                        "\n"
                        [
                            "let b = makeBox()"
                            "let u = b.set(9)"
                            "let f = b.get"
                            "let result = f()"
                            ""
                        ]

                let js = emitBox program
                Expect.isFalse (js.Contains "Box__") (sprintf "unexpected mangled member import in:\n%s" js)

                expectNodeOutput
                    "attach-escape"
                    [
                        "harness.mjs", resultHarness
                        "box-program.mjs", js
                        "boxlib.mjs", boxRuntime
                    ]
                    "9"
            }

            // Overload resolution reads the argument's TYPE, so a tuple-valued expression
            // reaches the same 2-parameter member a literal `(11, 20)` does. Spread either
            // way, or `addTo` sees one array where it declares two numbers.
            test "a 2-param attached member spreads a tuple VALUE as well as a literal" {
                let program =
                    String.concat
                        "\n"
                        [
                            "let b = makeBox()"
                            "let t = (11, 20)"
                            "let u = b.addTo t"
                            "let result = b.get()"
                            ""
                        ]

                let js = emitBox program

                Expect.isFalse
                    (js.Contains ".addTo(t)")
                    (sprintf "the tuple must not be passed as ONE argument, got:\n%s" js)

                expectNodeOutput
                    "attach-tuple-value"
                    [
                        "harness.mjs", resultHarness
                        "box-program.mjs", js
                        "boxlib.mjs", boxRuntime
                    ]
                    "31"
            }

            test "a Property member lowers to a plain objArg.prop READ (no call)" {
                // A manifest Property is a JS data property, so `b.value` is a member READ,
                // not the zero-arg-method shape a local interface-impl property emits.
                let js =
                    emitBox (String.concat "\n" [ "let b = makeBox()"; "let result = b.value"; "" ])

                Expect.isTrue (js.Contains ".value") (sprintf "expected a `.value` property read, got:\n%s" js)

                Expect.isFalse
                    (js.Contains ".value()")
                    (sprintf "property must NOT emit a call `.value()`, got:\n%s" js)
            }
        ]
