module XParsec.FSharp.Codegen.Js.Tests.ExternalHeritageTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS provider — external INTERFACE heritage resolution. The TS-manifest
// provider stores an interface's `extends`-chain UN-flattened (`FrozenInterfaces` /
// `FrozenBaseType`) and does NOT copy inherited members onto the subtype's `Members`
// (unlike the CLR metadata layer, where `GetInterfaces()` pre-flattens the transitive
// set). Two capabilities therefore have to be walked at the consumer:
//
//   Gap 1 — interface→interface supertype ASSIGNABILITY at the foreign-arg seam:
//           passing a `Child` where a super-interface `Base<int>` is expected.
//   Gap 2 — inherited MEMBER READS: reading a member declared on a base interface off a
//           subtype object argument.
//
// Both must resolve DIRECTLY (Child extends Base) AND TRANSITIVELY (grandparent, via an
// intermediate interface with no members of the target).

// ─── Hand-built manifest: A<T> ⊃ B ⊃ C (generic root, two-deep chain) ───────────

let private intT = named "int"
let private unitT = named "unit"
let private stringT = named "string"

/// `heritlib`:
///   interface A<T> { value: T; m(): T }
///   interface B extends A<int> { bTag: string }
///   interface C extends B      { cTag: string }
/// plus factories `makeA(): A<int>`, `makeB(): B`, `makeC(): C` and a consumer
/// `useA(a: A<int>): unit`.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "heritlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface("A", 1, [ property' "value" (typar 0); method' "m" (sig0 (typar 0)) ], [], [])
                Schema.Export.Interface("B", 0, [ property' "bTag" stringT ], [ namedG "A" [ intT ] ], [])
                Schema.Export.Interface("C", 0, [ property' "cTag" stringT ], [ named "B" ], [])
                Schema.Export.Function("makeA", [ sig0 (namedG "A" [ intT ]) ], Schema.ImportShape.Named)
                Schema.Export.Function("makeB", [ sig0 (named "B") ], Schema.ImportShape.Named)
                Schema.Export.Function("makeC", [ sig0 (named "C") ], Schema.ImportShape.Named)
                Schema.Export.Function("useA", [ sig1 "a" (namedG "A" [ intT ]) unitT ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private provider: IExternalSymbolProvider = stackTs manifest

let private analyse (input: string) : Diagnostic list = analyseWith provider input

// ─── E2E: an inherited member READ lowers to native `objArg.member` and runs ────

/// `chainlib`: a non-generic two-deep chain through an EMPTY intermediate interface,
///   interface Base { value: int; describe(): string }
///   interface Mid extends Base {}          // no own members — a pure relay
///   interface Leaf extends Mid { tag: int }
/// with `makeLeaf(): Leaf`. Reading `leaf.value` / `leaf.describe()` off a `Leaf` must
/// resolve through the grandparent `Base` and lower to a native `objArg.member`.
let private chainManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "chainlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Base",
                    0,
                    [ property' "value" intT; method' "describe" (sig0 stringT) ],
                    [],
                    []
                )
                Schema.Export.Interface("Mid", 0, [], [ named "Base" ], [])
                Schema.Export.Interface("Leaf", 0, [ property' "tag" intT ], [ named "Mid" ], [])
                Schema.Export.Function("makeLeaf", [ sig0 (named "Leaf") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private chainContract = contractTs chainManifest

/// A runtime `Leaf` whose inherited `value`/`describe` live on the object itself; a
/// lowering that failed to home the inherited member would read `undefined`.
let private chainRuntime =
    String.concat
        "\n"
        [
            "export function makeLeaf() {"
            "  return {"
            "    value: 42,"
            "    tag: 7,"
            "    describe() { return \"leaf\"; },"
            "  };"
            "}"
            ""
        ]

let private emitChain (input: string) : string =
    emitWith
        chainContract
        (Map.ofList
            [
                "chainlib",
                {
                    FileName = "chainlib.mjs"
                    Source = chainRuntime
                }
            ])
        true
        input

let private chainHarness =
    "import { result } from \"./chain-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "ExternalHeritage"
        [
            // ── Gap 2: inherited member reads ──────────────────────────────────────
            test "DIRECT inherited member read: B sees A's `value`" {
                // `value` is declared on `A<T>`; `B extends A<int>` inherits it as `int`.
                let errors =
                    analyse (String.concat "\n" [ "let b = makeB()"; "let n : int = b.value"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "DIRECT inherited method read: B sees A's `m`" {
                let errors =
                    analyse (String.concat "\n" [ "let b = makeB()"; "let n : int = b.m()"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "TRANSITIVE inherited member read: C sees A's `value` through B" {
                // `C extends B extends A<int>`; the walk must reach the grandparent.
                let errors =
                    analyse (String.concat "\n" [ "let c = makeC()"; "let n : int = c.value"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "own members still resolve alongside inherited ones" {
                let errors =
                    analyse (String.concat "\n" [ "let c = makeC()"; "let s : string = c.cTag"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "NEGATIVE: a member on NEITHER self nor any supertype still errors" {
                let errors =
                    analyse (String.concat "\n" [ "let c = makeC()"; "let n = c.nope"; "" ])

                Expect.isNonEmpty errors "reading a nonexistent member must still error"
            }

            // ── Gap 1: interface→interface supertype assignability ─────────────────
            test "DIRECT super-interface assignability: B flows into an A<int> param" {
                let errors = analyse (String.concat "\n" [ "let b = makeB()"; "useA(b)"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "TRANSITIVE super-interface assignability: C flows into an A<int> param" {
                let errors = analyse (String.concat "\n" [ "let c = makeC()"; "useA(c)"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%A" (errors |> List.map (fun d -> d.Message)))
            }

            test "NEGATIVE: an unrelated value does not flow into an A<int> param" {
                let errors = analyse (String.concat "\n" [ "let x = 5"; "useA(x)"; "" ])
                Expect.isNonEmpty errors "an int must not be assignable to an A<int> parameter"
            }

            // ── E2E: transitively-inherited read lowers native and round-trips ─────
            test "an inherited member READ lowers to native objArg.member and runs under Node" {
                // `leaf.value` is declared on `Base`, reached from `Leaf` THROUGH the empty
                // `Mid` — the transitive inherited-member walk. It must lower to a plain
                // `objArg.value` (the declaring `Base` interface is AttachedNative), never a
                // mangled `Base__value` free-fn import.
                let program =
                    String.concat "\n" [ "let leaf = makeLeaf()"; "let result = leaf.value"; "" ]

                let js = emitChain program

                Expect.isTrue (js.Contains ".value") (sprintf "expected a native `.value` read, got:\n%s" js)
                Expect.isFalse (js.Contains "Base__") (sprintf "unexpected mangled member import in:\n%s" js)

                expectNodeOutput
                    "heritage-inherited-read"
                    [
                        "harness.mjs", chainHarness
                        "chain-program.mjs", js
                        "chainlib.mjs", chainRuntime
                    ]
                    "42"
            }
        ]
