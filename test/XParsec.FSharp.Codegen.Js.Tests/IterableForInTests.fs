module XParsec.FSharp.Codegen.Js.Tests.IterableForInTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A TS `[Symbol.iterator]()` member (carried as `__@iterator@N`) is homed as `seq<'T>` by
// the provider, so `for x in src` lowers to a native `for (const x of src)` with no
// front-end or emit change. Both packs here are Node globals, so no import is emitted.

/// A parameterless `.ctor` returning the declaring class applied to its own typar
/// (`Set<T>`), so `new Js.Set<int>()` grounds and emits the bare global `new Set()`.
let private setCtor: Schema.Member =
    methodOf ".ctor" false [ sig0 (namedG "Set" [ typar 0 ]) ]

/// `es2015`: a minimal GLOBAL pack declaring a constructible, iterable `Set<T>`. Only the
/// first type argument of the `[Symbol.iterator]` return is peeled, so `SetIterator`
/// itself is never resolved and needs no export.
let private setManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "es2015"
        Version = None
        Exports =
            [
                Schema.Export.Class(
                    "Set",
                    1,
                    [
                        setCtor
                        method' "add" (sig1 "value" (typar 0) (namedG "Set" [ typar 0 ]))
                        method' "__@iterator@1" (sig0 (namedG "SetIterator" [ typar 0 ]))
                    ],
                    [],
                    Schema.ImportShape.Named,
                    []
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private setContract = contractTs setManifest

let private setProvider: IExternalSymbolProvider = setContract.Provider

// The REAL vendored es2015 pack. `Map<K,V>`'s `[Symbol.iterator](): MapIterator<[K,V]>`
// entry is a genuine `(K, V)` TUPLE, so the provider homes `Map` as `seq<K*V>` and
// `for (k, v) in m` destructures.
let private mapContract = contractTs es2015Manifest

let private mapProvider: IExternalSymbolProvider = mapContract.Provider

// The accumulator is function-local because a module-level `let mutable` lowers to a
// `const`. `sum` is a top-level `let`, so library-mode emit exports it for the harness.
let private program =
    String.concat
        "\n"
        [
            "let sumSet (xs: Js.Set<int>) ="
            "    let mutable total = 0"
            "    for x in xs do"
            "        total <- total + x"
            "    total"
            "let s = new Js.Set<int>()"
            "let a1 = s.add(1)"
            "let a2 = s.add(2)"
            "let a3 = s.add(3)"
            "let sum = sumSet s"
            ""
        ]

let private harness =
    String.concat
        "\n"
        [
            "import { sum } from \"./set-forin-program.mjs\";"
            "console.log(`${sum}`);"
            ""
        ]

let private analyseErrors (input: string) : string list =
    analyseWith setProvider input |> List.map (fun d -> d.Message)

let private emitSet (input: string) : string =
    emitWith setContract Map.empty true input

let private mapProgram =
    String.concat
        "\n"
        [
            "let sumKV (m: Js.Map<int, int>) ="
            "    let mutable total = 0"
            "    for (k, v) in m do"
            "        total <- total + k + v"
            "    total"
            "let m = new Js.Map<int, int>()"
            "let s1 = m.set(1, 10)"
            "let s2 = m.set(2, 20)"
            "let s3 = m.set(3, 30)"
            "let sum = sumKV m"
            ""
        ]

let private mapHarness =
    String.concat
        "\n"
        [
            "import { sum } from \"./map-forin-program.mjs\";"
            "console.log(`${sum}`);"
            ""
        ]

let private analyseMapErrors (input: string) : string list =
    analyseWith mapProvider input |> List.map (fun d -> d.Message)

let private emitMap (input: string) : string =
    emitWith mapContract Map.empty true input

[<Tests>]
let tests =
    testList
        "IterableForIn"
        [
            test "a `for x in Js.Set` program type-checks against the mounted pack" {
                let errors = analyseErrors program

                Expect.isEmpty errors (sprintf "the for-in-over-Set program must type-check, got:\n%A" errors)
            }

            test "a `for x in Js.Set` lowers to a native `for..of` and round-trips under Node" {
                let js = emitSet program

                Expect.stringContains js "for (" (sprintf "expected a `for` loop, got:\n%s" js)
                Expect.stringContains js " of " (sprintf "expected a `for..of`, got:\n%s" js)
                Expect.stringContains js "new Set(" (sprintf "expected bare `new Set(`, got:\n%s" js)
                Expect.isFalse (js.Contains "import") (sprintf "a global Set program emits NO import, got:\n%s" js)

                expectNodeOutput "set-forin-e2e" [ "harness.mjs", harness; "set-forin-program.mjs", js ] "6"
            }

            test "a `for (k, v) in Js.Map` program type-checks (tuple element bound variable)" {
                let errors = analyseMapErrors mapProgram

                Expect.isEmpty errors (sprintf "the for-(k,v)-in-Map program must type-check, got:\n%A" errors)
            }

            test "a `for (k, v) in Js.Map` destructures each pair and round-trips under Node" {
                let js = emitMap mapProgram

                // The pair binds via a fresh loop temp, deconstructed positionally
                // (`t[0]`/`t[1]`) into the body's first statements.
                Expect.stringContains js " of " (sprintf "expected a `for..of`, got:\n%s" js)
                Expect.stringContains js "[0]" (sprintf "expected positional `[0]` destructure, got:\n%s" js)
                Expect.stringContains js "[1]" (sprintf "expected positional `[1]` destructure, got:\n%s" js)
                Expect.stringContains js "new Map(" (sprintf "expected bare `new Map(`, got:\n%s" js)

                expectNodeOutput "map-forin-e2e" [ "harness.mjs", mapHarness; "map-forin-program.mjs", js ] "66"
            }
        ]
