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

/// Emit JS through the exact project/pipeline shape the direct byte-identity gate
/// (`emitConformanceJs`) uses, but from an already-frozen tree — so the round-tripped
/// emit is judged against the identical pipeline and differs only by the round-trip.
/// Strips the trailing `//# sourceMappingURL` line as the direct gate does.
let private emitFrom (name: string) (src: string) (frozen: Frozen.TastFile) : string =
    let project =
        { JsProjectInfo.defaults name with
            Source = Some { Path = name + ".fsx"; Content = src }
        }

    let source =
        Codegen.compileWith jsProvider.Value jsManifests project frozen
        |> Codegen.toSource

    let idx = source.IndexOf "//# sourceMappingURL"
    if idx >= 0 then source.Substring(0, idx) else source

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
                        (emitFrom name p.Source rt)
                        (emitFrom name p.Source direct)
                        "round-tripped frozen tree emits byte-identical JS"
                }

            // The gate must exercise a non-trivial corpus, else an empty run would
            // pass vacuously.
            test "the gated corpus is non-empty" {
                Expect.isGreaterThan (List.length gated) 0 "JS-gated programs"
            }
        ]
