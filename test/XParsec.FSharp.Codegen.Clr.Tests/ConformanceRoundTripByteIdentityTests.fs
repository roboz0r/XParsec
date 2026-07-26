module XParsec.FSharp.Codegen.Clr.Tests.ConformanceRoundTripByteIdentityTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A frozen-tree round-trip is codegen-INVARIANT for the CLR: the assembly emitted from
// the direct frozen tree and from a round-tripped copy must be structurally identical.
// This is the end-to-end strengthening of the structural round-trip gate — structural
// TAST equality does not by itself guarantee identical emitted IL, so codegen is the
// judge. A divergence means the round-trip perturbed something the backend reads.
//
// Two round-trips ride the same gate, sharing one freeze per program:
//   * `thaw (flatten frozen)` — the frozen-cache serialization round-trip.
//   * `toPools (ofPools frozen)` — the id-pool round-trip. The freeze yields POOLS, so
//     this is the composition that exists: drain the columns to the DU, re-derive every
//     column from THAT (`TestHelpers.compileConformanceDirectAndRoundTripped`). It is the
//     corpus-wide
//     proof that the pools are interconvertible with the DU over EVERY shape the CLR
//     backend exercises (`ILIntrinsic`, `StaticOptimization`, `TraitCall`, the casts,
//     `TryWith`/`TryFinally`, `MethodCall`/`PropertyGet`, …), which the small in-project
//     `TastPoolsTests` smoke set does not reach.
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

let private digest (artifact: ClrArtifact) =
    ClrStructuralDigest.ofBytes (Codegen.toBytes artifact)

[<Tests>]
let tests =
    testList
        "CLR round-trip byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let arts =
                        compileConformanceDirectAndRoundTripped (conformanceAssemblyName p.Name) p.Source

                    let directDigest = digest arts.Direct

                    Expect.equal
                        (digest arts.ThawRoundTripped)
                        directDigest
                        "thaw (flatten frozen) emits a structurally identical assembly"

                    Expect.equal
                        (digest arts.PoolRoundTripped)
                        directDigest
                        "toPools (ofPools frozen) emits a structurally identical assembly"
                }

            // The gate must exercise a non-trivial corpus, else an empty run would
            // pass vacuously.
            test "the gated corpus is non-empty" { Expect.isGreaterThan (List.length gated) 0 "CLR-gated programs" }
        ]
