module XParsec.FSharp.Codegen.Clr.Tests.ConformanceRoundTripByteIdentityTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A frozen-tree round-trip must be codegen-invariant: structural TAST equality does not
// by itself guarantee identical IL, so the emitted assembly is the judge. Compared by
// structural digest rather than raw bytes, since each compile mints a fresh MVID.

let private gated = compiledBy "clr"

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

            test "the gated corpus is non-empty" { Expect.isGreaterThan (List.length gated) 0 "CLR-gated programs" }
        ]
