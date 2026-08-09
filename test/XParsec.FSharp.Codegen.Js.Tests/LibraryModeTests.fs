module XParsec.FSharp.Codegen.Js.Tests.LibraryModeTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// `core-types.fs` compiled in library mode, AS `Vesper.Core`. Naming the home assembly is
/// what makes its two compiler attributes, and its own `Ref<'T>` declaration, resolvable out
/// of the package's own contract.
let private coreTypes: Lazy<string> =
    lazy
        compileOwnLibrary
            coreDepsJsContract.Value
            "Vesper.Core"
            "core-types.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Core" "core-types.fs"))

// Script mode (a runnable program) emits top-level bindings as plain `const`; library mode
// emits them as `export const` so other modules can import them. The runtime library packages
// (Vesper.List, Vesper.Option) are generated in library mode.
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

            // Vesper.Core has no JS module of its own, because a JS module compiles from one
            // file and the package is many. `core-types.fs` is the exception: the ref cell
            // needs only two erased attributes, so it stands alone.
            test "core-types.fs emits the ref cell and imports nothing" {
                let src = coreTypes.Value

                Expect.stringContains src "class Ref {" "the reference-equality record with the mutable field"

                // The flat<->curried adapters are CLR-only, so nothing here reaches `Fun` and
                // no import is owed. An `import` would name the package's own asset, which
                // exports runtime entries and no compiled member.
                Expect.isFalse (src.Contains "import ") "a self-contained module"
            }

            test "core-types.fs loads under node and its ref cell round-trips" {
                match
                    runNodeFiles
                        "core-types-lib"
                        [ "entry.mjs", coreTypes.Value + "\nconsole.log(new Ref(3).contents);\n" ]
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3" "the emitted module is loadable and its class constructs"
            }
        ]
