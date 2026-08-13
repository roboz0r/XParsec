module XParsec.FSharp.Codegen.Js.Tests.ConformanceRoundTripByteIdentityTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The frozen-cache round-trip is codegen-INVARIANT for JS: emitting from the direct frozen
// tree and from `thaw (flatten direct)` must yield byte-identical JS. Structural equality does
// not guarantee identical text, so a divergence here is a defect, not a golden to update.

/// Filtered, so `frozenOfJs` never trips on a `Diagnose` program's error diagnostics.
let private gated = compiledBy "js"

[<Tests>]
let tests =
    testList
        "JS round-trip byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let name = "conformance-" + p.Name
                    let direct = frozenOfJs p.Source
                    let rt = FrozenCodec.thaw (FrozenCodec.flatten direct)

                    Expect.equal
                        (emitFrozenJs name p.Source rt)
                        (emitFrozenJs name p.Source direct)
                        "round-tripped frozen tree emits byte-identical JS"
                }

            // The gate must exercise a non-trivial corpus, else an empty run would
            // pass vacuously.
            test "the gated corpus is non-empty" { Expect.isGreaterThan (List.length gated) 0 "JS-gated programs" }
        ]
