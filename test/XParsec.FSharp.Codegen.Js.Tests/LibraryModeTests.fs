module XParsec.FSharp.Codegen.Js.Tests.LibraryModeTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// `core-types.fs` compiled in library mode, AS `Vesper.Core`. It needs `Fun` and the two
/// compiler attributes from its own package's contract, which also declares the three
/// types the file itself defines — reachable only because the home assembly is named.
let private coreTypes: Lazy<string> =
    lazy
        compileOwnLibrary
            coreDepsJsContract.Value
            "Vesper.Core"
            "core-types.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Core" "core-types.fs"))

// Script mode (a runnable program) emits top-level bindings as plain `const`;
// library mode emits them as `export const` so other modules can import them.
// This is the mode primitive the runtime library modules (Vesper.List, Vesper.Option)
// are generated under.
[<Tests>]
let tests =
    testList
        "Codegen.Js Library vs Script Mode"
        [
            test "library mode exports a top-level value binding" {
                Expect.equal
                    (emitJsLibrary "let answer = 42")
                    "export const answer = 42;\n"
                    "top-level `let` → `export const` (vs script mode's `const`)"
            }

            test "library mode exports a flat function binding" {
                Expect.equal
                    (emitJsLibrary "let add x y = x + y")
                    "export const add = (x, y) => (((x) + (y)) | 0);\n"
                    "the binding name is the source identifier, the value the flat multi-arg arrow"
            }

            test "script mode keeps top-level bindings as plain const" {
                Expect.equal
                    (emitJs "let answer = 42")
                    "const answer = 42;\n"
                    "Kind defaults to Script — no `export` (the runnable-program behaviour)"
            }

            // ---- a Vesper.Core body compiled as a module ----
            //
            // Vesper.Core has no JS module of its own — an eighteen-file package, and a JS
            // module is compiled from one file. `core-types.fs` is the exception: it needs
            // only `Fun` and two erased attributes, so it stands alone.

            test "core-types.fs emits the ref cell, the two Fun adapters, and the adapter functions" {
                let src = coreTypes.Value

                Expect.stringContains src "class Ref {" "the reference-equality record with the mutable field"
                Expect.stringContains src "class Curried {" "the partial-application adapter"
                Expect.stringContains src "class Flattened {" "the curried→flat adapter"
                Expect.stringContains src "export const curryFun = " "the upcasting adapter functions export"
                Expect.stringContains src "export const flatten = " "both of them"
            }

            // Why there is no Node round-trip beside the emit check: `f.Invoke(a, b)` on a
            // `Fun`-typed receiver lowers to a receiver-first `Fun__Invoke` imported from
            // `Vesper.Core.mjs`, and the committed asset exports no such name — so the module
            // does not load. No other library body calls an interface method on an external
            // interface-typed value, which is why nothing caught it before.
            test "an external interface's method call imports a free function the runtime asset lacks" {
                Expect.stringContains
                    coreTypes.Value
                    "import { Fun__Invoke as $Fun__Invoke } from \"./Vesper.Core.mjs\";"
                    "the import that blocks execution — delete this test and run it under Node once it resolves"
            }
        ]
