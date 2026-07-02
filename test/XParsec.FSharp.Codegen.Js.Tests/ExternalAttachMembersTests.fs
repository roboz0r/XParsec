module XParsec.FSharp.Codegen.Js.Tests.ExternalAttachMembersTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// R2 (ts-provider plan): an instance-member call on an external TS-manifest object
// lowers to a NATIVE `receiver.member(args)` — the object has genuine prototype/own
// methods, NOT the receiver-first `$Box_get`-style free-fn import Vesper's OWN runtimes
// emit (a tree-shaking optimisation). The signal is `ExternalClassFlags.AttachMembers`,
// which `TsManifestProvider` stamps on every real Interface/Class shape; EmitJs reads it
// through the declaring type's shape.
//
// The runtime object below holds state in a `this._v` field and its methods READ `this`,
// so a detached-function bug (which loses `this` in JS) cannot hide behind a closure.

// ─── Hand-built manifest (no JSON round-trip) ──────────────────────────────

let private named n = Schema.TypeRef.Named(n, [])
let private intT = named "int"
let private unitT = named "unit"

let private sig0 (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        Params = []
        Returns = ret
    }

let private sig1 (pname: string) (pty: Schema.TypeRef) (ret: Schema.TypeRef) : Schema.Signature =
    {
        TypeParams = 0
        Params =
            [
                {
                    Name = pname
                    Type = pty
                    Optional = false
                    Rest = false
                }
            ]
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

let private property' (name: string) (ty: Schema.TypeRef) : Schema.Member =
    {
        Name = name
        Kind = Schema.MemberKind.Property
        Type = Some ty
        Signatures = []
        Static = false
        Optional = false
    }

/// `boxlib`: a NON-GENERIC stateful interface `Box { get(): int; set(x: int): unit;
/// value: int }` plus a `makeBox(): Box` factory whose RETURN freezes to `FTClass`
/// (R1), so a Vesper value flowing from it admits native member calls (R2).
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
                        property' "value" intT
                    ],
                    []
                )
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
    }

let private boxProvider: IExternalSymbolProvider =
    ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest boxManifest; jsProvider.Value ]

// A hand-authored runtime whose factory returns a STATEFUL object: state lives in
// `this._v` and every method reads/writes `this`, so a lowering that detached the
// method from its receiver would observe the wrong (or undefined) `this`.
let private boxRuntime =
    String.concat
        "\n"
        [
            "export function makeBox() {"
            "  return {"
            "    _v: 0,"
            "    get() { return this._v; },"
            "    set(x) { this._v = x; },"
            "    get value() { return this._v; },"
            "  };"
            "}"
            ""
        ]

/// Emit `input` as a JS module (library mode) resolving `boxlib` to the hand-authored
/// runtime above — mirrors `MittE2ETests.emitWithMitt`.
let private emitBox (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost boxProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "boxlib",
                {
                    FileName = "boxlib.mjs"
                    Source = boxRuntime
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
            Provider = ValueSome boxProvider
            ExternalUnions = System.Collections.Generic.Dictionary()
            Imports = JsImports.create runtime
            ExportTopLevel = true
            CompiledFns = System.Collections.Generic.Dictionary()
            LocalInterfaces = System.Collections.Generic.HashSet()
        }

    (JsPrint.print (EmitJs.buildProgram ctx frozen)).Source

// A tiny harness: import the library-mode `export const result` the Vesper program emits
// and print it, so Node's stdout carries the observed value.
let private resultHarness =
    "import { result } from \"./box-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "ExternalAttachMembers"
        [
            test "instance member calls lower to NATIVE receiver.member(args), no mangled import" {
                // `b.set(5)` / `b.get()` must be genuine object methods on the runtime the
                // factory returns — NOT the mangled `$Box__get` free-fn import.
                // `set` (unit) is bound to a named `u`: a BARE `b.set(5)` mid-sequence is a
                // front-end parse gap (`Expr.Missing`) and a top-level `let _ =` is not an
                // emit-supported declaration — both unrelated to R2. A named unit binding
                // runs the call for its effect and emits as `const u = recv.set(5)`.
                let program =
                    String.concat "\n" [ "let b = makeBox()"; "let u = b.set(5)"; "let result = b.get()"; "" ]

                let js = emitBox program

                Expect.isTrue (js.Contains ".set(") (sprintf "expected a native `.set(` call, got:\n%s" js)
                Expect.isTrue (js.Contains ".get(") (sprintf "expected a native `.get(` call, got:\n%s" js)

                // The receiver-first free-fn form Vesper's own runtimes use must be ABSENT
                // for a native manifest member: no `Box__get` / `Box__set` mangled export.
                Expect.isFalse (js.Contains "Box__") (sprintf "unexpected mangled member import in:\n%s" js)

                match
                    runNodeFiles
                        "attach-members"
                        [
                            "harness.mjs", resultHarness
                            "box-program.mjs", js
                            "boxlib.mjs", boxRuntime
                        ]
                with
                | None -> () // node absent — the emit above still ran + asserted
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    // `set(5)` then `get()` reading `this._v` returns 5.
                    Expect.stringContains out "5" (sprintf "round-trip output, got:\n%s" out)
            }

            test "a zero-arg method emits recv.get() with the lone unit dropped" {
                let js =
                    emitBox (String.concat "\n" [ "let b = makeBox()"; "let result = b.get()"; "" ])
                // The `()` argument has no JS value — `recv.get()`, never `recv.get(undefined)`.
                Expect.isTrue (js.Contains ".get()") (sprintf "expected `recv.get()` (lone unit dropped), got:\n%s" js)
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

                match
                    runNodeFiles
                        "attach-escape"
                        [
                            "harness.mjs", resultHarness
                            "box-program.mjs", js
                            "boxlib.mjs", boxRuntime
                        ]
                with
                | None -> ()
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
                    Expect.stringContains out "9" (sprintf "escaped-method round-trip, got:\n%s" out)
            }

            test "a Property member lowers to a plain receiver.prop READ (no call)" {
                // A manifest Property is a JS data property — `b.value` is a member READ,
                // not the zero-arg-method shape a local interface-impl property emits.
                let js =
                    emitBox (String.concat "\n" [ "let b = makeBox()"; "let result = b.value"; "" ])

                Expect.isTrue (js.Contains ".value") (sprintf "expected a `.value` property read, got:\n%s" js)

                Expect.isFalse
                    (js.Contains ".value()")
                    (sprintf "property must NOT emit a call `.value()`, got:\n%s" js)
            }
        ]
