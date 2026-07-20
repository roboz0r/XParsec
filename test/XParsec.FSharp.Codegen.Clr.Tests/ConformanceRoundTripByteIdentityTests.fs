module XParsec.FSharp.Codegen.Clr.Tests.ConformanceRoundTripByteIdentityTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The frozen-cache round-trip is codegen-INVARIANT for the CLR: the assembly emitted
// from the direct frozen tree and from `thaw (flatten direct)` must be structurally
// identical. This is the end-to-end strengthening of the structural round-trip gate —
// structural TAST equality does not by itself guarantee identical emitted IL, so
// codegen is the judge. A divergence means flatten/thaw perturbed something the
// backend reads.
//
// The comparison is the STRUCTURAL DIGEST, not raw bytes: `Metadata.fs:64` mints a
// fresh MVID per compile, so raw PE bytes differ even for the same tree — exactly why
// the direct byte-identity gate digests structure. See `ClrStructuralDigest`.
//
// Gated set = programs the CLR backend compiles (`Run`/`Fault` for "clr"); `Diagnose`
// programs emit nothing, so skipped.

/// Programs the CLR backend compiles (see the header).
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "clr" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

[<Tests>]
let tests =
    testList
        "CLR round-trip byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let direct, roundTripped =
                        compileConformanceDirectAndRoundTripped (conformanceAssemblyName p.Name) p.Source

                    Expect.equal
                        (ClrStructuralDigest.ofBytes (Codegen.toBytes roundTripped))
                        (ClrStructuralDigest.ofBytes (Codegen.toBytes direct))
                        "round-tripped frozen tree emits a structurally identical assembly"
                }

            // The gate must exercise a non-trivial corpus, else an empty run would
            // pass vacuously.
            test "the gated corpus is non-empty" {
                Expect.isGreaterThan (List.length gated) 0 "CLR-gated programs"
            }
        ]
