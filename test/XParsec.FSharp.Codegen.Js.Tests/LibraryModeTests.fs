module XParsec.FSharp.Codegen.Js.Tests.LibraryModeTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

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
        ]
