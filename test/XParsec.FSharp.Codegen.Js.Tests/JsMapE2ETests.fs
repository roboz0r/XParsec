module XParsec.FSharp.Codegen.Js.Tests.JsMapE2ETests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The `Js.Map` consumer GATE. A Vesper program CONSTRUCTS and DRIVES a real
// `Js.Map<string, int>` directly against the VENDORED es2015 ref pack
// (`../ts-fixtures/es2015`, mounted under `Js`), emitted via Codegen.Js and RUN under
// Node. `Map` is a Node GLOBAL: `new Map()`/`.set()`/`.get()`/`.has()`/`.size`/`.delete()`
// run intrinsically, so NO runtime `.mjs` module is emitted and NO import appears.
//
// Iteration lives in its own gate now (`IterableForInTests`): the provider homes a
// `[Symbol.iterator]` type as `seq<'T>`, the extractor carries `Map`'s `[K,V]` entry as a
// real tuple, and `for (k,v) in (m: Js.Map<_,_>)` lowers to a native `for..of` with a tuple
// binder — driven END-TO-END against THIS vendored pack. This gate stays focused on the
// direct member surface (construct / set / get / has / size / delete).
//
// The es2015 pack stacks over the JS-native provider (`stackTs es2015Manifest`), the
// same shape `JsNamespaceTests` pins on a hand-built global manifest — this is the REAL
// pack, so it also proves the ctor-argSig dedup (`Map`'s cross-file no-arg ctor merge)
// loads cleanly.

let private mapContract = contractTs es2015Manifest

let private mapProvider: IExternalSymbolProvider = mapContract.Provider

// The gate program. Every observed value is a TOP-LEVEL `let` so library-mode emit
// exports it (`export const …`) and the harness below reads it — the MittE2E idiom, so
// no `printfn`/`%A` over the `get` result's `undefined | int` union is needed. `set`
// returns the Map (bound per the effectful-call sequencing convention); `size` is a
// PROPERTY (emits `.size`, no call); `get` returns `undefined | int` (at runtime the set
// value); `has`/`delete` return bool. `n` is read BEFORE the delete, so it observes 2.
let private program =
    String.concat
        "\n"
        [
            "let m = new Js.Map<string, int>()"
            "let s1 = m.set(\"a\", 1)"
            "let s2 = m.set(\"b\", 2)"
            "let v = m.get(\"a\")"
            "let hasA = m.has(\"a\")"
            "let hasZ = m.has(\"z\")"
            "let n = m.size"
            "let removed = m.delete(\"a\")"
            "let hasAfter = m.has(\"a\")"
            ""
        ]

// Imports the exported observations and prints them in a stable order. `v` is `1` at
// runtime; `n` (size before delete) is `2`; `removed` true; `hasAfter` false.
let private harness =
    String.concat
        "\n"
        [
            "import { v, hasA, hasZ, n, removed, hasAfter } from \"./js-map-program.mjs\";"
            "console.log(`${v},${hasA},${hasZ},${n},${removed},${hasAfter}`);"
            ""
        ]

let private analyseErrors (input: string) : string list =
    analyseWith mapProvider input |> List.map (fun d -> d.Message)

// A Global pack needs no runtime modules — that is the whole point (Map is intrinsic).
let private emitMap (input: string) : string =
    emitWith mapContract Map.empty true input

[<Tests>]
let tests =
    testList
        "JsMapE2E"
        [
            test "a Js.Map program constructs and drives the global Map (analysis)" {
                let errors = analyseErrors program

                Expect.isEmpty
                    errors
                    (sprintf "the Js.Map gate program must type-check against the mounted pack, got:\n%A" errors)
            }

            test "a Js.Map program emits bare global Map calls and round-trips under Node" {
                let js = emitMap program

                // Construction and every member drive the BARE global names — no import,
                // no `es2015`/`Js.` wire home leaks into the output.
                Expect.stringContains js "new Map(" (sprintf "expected bare `new Map(`, got:\n%s" js)
                Expect.stringContains js ".set(" (sprintf "expected `.set(`, got:\n%s" js)
                Expect.stringContains js ".get(" (sprintf "expected `.get(`, got:\n%s" js)
                Expect.stringContains js ".has(" (sprintf "expected `.has(`, got:\n%s" js)
                Expect.stringContains js ".size" (sprintf "expected the PROPERTY read `.size`, got:\n%s" js)
                Expect.stringContains js ".delete(" (sprintf "expected `.delete(`, got:\n%s" js)

                // `size` is a property, not a call — no `.size(`.
                Expect.isFalse
                    (js.Contains ".size(")
                    (sprintf "`size` must read as a property, not a call, got:\n%s" js)

                Expect.isFalse (js.Contains "import") (sprintf "a global Map program must emit NO import, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "es2015")
                    (sprintf "the wire home `es2015` must not leak into the output, got:\n%s" js)

                expectNodeOutput
                    "js-map-e2e"
                    [ "harness.mjs", harness; "js-map-program.mjs", js ]
                    "1,true,false,2,true,false"
            }
        ]
