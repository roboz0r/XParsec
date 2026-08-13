module XParsec.FSharp.Codegen.Js.Tests.ConformanceByteIdentityTests

open System.IO
open Expecto
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The JS byte-identity gate: for every conformance program the JS backend actually COMPILES,
// pin the emitted JS source text as a golden and assert equality on every later run.

// That includes an `accept` program: the manifest declines to assert its OUTPUT, not its
// emission.

/// Committed goldens, one `.js` per program, beside this test.
let private goldensDir = Path.Combine(__SOURCE_DIRECTORY__, "goldens")

/// The exact source `runJs` would materialise, minus the Node run: compiled under the assembly
/// name `conformance-<program>`, with the trailing `//# sourceMappingURL` line stripped.
let private emitConformanceJs (name: string) (src: string) : string = emitFrozenJs name src (frozenOfJs src)

let private gated = compiledBy "js"

[<Tests>]
let tests =
    testList
        "JS byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let js = emitConformanceJs ("conformance-" + p.Name) p.Source
                    Goldens.check (Path.Combine(goldensDir, p.Name + ".js")) (p.Name + ".js") js
                }

            test "no golden outlives the program it pins" {
                Goldens.checkNoOrphans goldensDir "*.js" [ for p in gated -> p.Name + ".js" ]
            }

            // Identical input yields identical source text.
            match gated with
            | p :: _ ->
                test "JS emission is deterministic for identical input" {
                    let name = "conformance-" + p.Name

                    Expect.equal
                        (emitConformanceJs name p.Source)
                        (emitConformanceJs name p.Source)
                        "same input, same JS"
                }
            | [] -> ()
        ]
