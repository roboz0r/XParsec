module XParsec.FSharp.Codegen.Js.Tests.ConformanceRoundTripByteIdentityTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The frozen-cache round-trip is codegen-INVARIANT for JS: emitting from the direct
// frozen tree and from `thaw (flatten direct)` must yield byte-identical JS. This is
// the end-to-end strengthening of the structural round-trip gate — structural
// equality does not by itself guarantee identical emitted text, so codegen is the
// judge. A divergence here means flatten/thaw dropped or perturbed something codegen
// reads, which is a real defect, not a golden to update.
//
// Gated set = programs the JS backend actually COMPILES (`Run`/`Fault` for "js"),
// mirroring the direct byte-identity gate so `frozenOfJs` never trips on a `Diagnose`
// program's error diagnostics.

/// Programs the JS backend compiles (see the header).
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "js" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

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
            test "the gated corpus is non-empty" {
                Expect.isGreaterThan (List.length gated) 0 "JS-gated programs"
            }
        ]
