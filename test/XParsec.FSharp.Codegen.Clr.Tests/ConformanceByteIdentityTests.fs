module XParsec.FSharp.Codegen.Clr.Tests.ConformanceByteIdentityTests

open System
open System.IO
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Pins a structural digest of every conformance assembly the CLR compiles as a golden.
// Structural rather than a byte hash: each compile mints a fresh MVID, so identical
// source yields different bytes. A `Fault` or `accept` program compiles too; a `Diagnose`
// one does not.

let private goldensDir = Path.Combine(__SOURCE_DIRECTORY__, "goldens")

let private gated = compiledBy "clr"

[<Tests>]
let tests =
    testList
        "CLR byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let _, artifact = compileSource (conformanceAssemblyName p.Name) p.Source
                    let digest = ClrStructuralDigest.ofBytes (Codegen.toBytes artifact)
                    Goldens.check (Path.Combine(goldensDir, p.Name + ".clr.txt")) (p.Name + ".clr.txt") digest
                }

            test "no golden outlives the program it pins" {
                Goldens.checkNoOrphans goldensDir "*.clr.txt" [ for p in gated -> p.Name + ".clr.txt" ]
            }

            match gated with
            | p :: _ ->
                test "structural digest is MVID-invariant" {
                    let _, a1 = compileSource (conformanceAssemblyName p.Name) p.Source
                    let _, a2 = compileSource (conformanceAssemblyName p.Name) p.Source
                    let b1 = Codegen.toBytes a1
                    let b2 = Codegen.toBytes a2

                    Expect.notEqual
                        (Convert.ToHexString b1)
                        (Convert.ToHexString b2)
                        "raw PE bytes differ per compile (fresh MVID), which is why the gate digests structure, not bytes"

                    Expect.equal
                        (ClrStructuralDigest.ofBytes b2)
                        (ClrStructuralDigest.ofBytes b1)
                        "structural digest is stable across recompiles of identical source"
                }
            | [] -> ()
        ]
