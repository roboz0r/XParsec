module XParsec.FSharp.Codegen.Clr.Tests.ConformanceByteIdentityTests

open System
open System.IO
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Two goldens per conformance assembly the CLR compiles. `<name>.clr.cs` is the program's
// source over the whole module decompiled to C#, so an emission change reads as a source
// diff; it is compared first, and a failure reports that diff ahead of the digest.
// `<name>.clr.txt` is a structural digest, sensitive to the IL detail a C# rendering elides
// (opcode choice, prefixes, local signatures). The digest is structural rather than a byte
// hash: each compile mints a fresh MVID, so identical source yields different bytes. A
// `Fault` or `accept` program compiles too; a `Diagnose` one does not.
//
// Regenerate both with `-UpdateSnapshots`. The C# will not always compile: an emitted name
// can be unspellable in C#, and the rendering is for review, not for a round trip.

let private goldensDir = Path.Combine(__SOURCE_DIRECTORY__, "goldens")

let private gated = compiledBy Target.Clr

[<Tests>]
let tests =
    testList
        "CLR byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let artifact = compileSource (conformanceAssemblyName p.Name) p.Source

                    let rendered =
                        sprintf
                            "/*\n%s\n*/\n\n%s"
                            (p.Source.Replace("\r", "").Trim())
                            (Decompile.moduleAsCSharp artifact)

                    Goldens.check (Path.Combine(goldensDir, p.Name + ".clr.cs")) (p.Name + ".clr.cs") rendered

                    let digest = ClrStructuralDigest.ofBytes (Codegen.toBytes artifact)
                    Goldens.check (Path.Combine(goldensDir, p.Name + ".clr.txt")) (p.Name + ".clr.txt") digest
                }

            test "no golden outlives the program it pins" {
                Goldens.checkNoOrphans goldensDir "*.clr.cs" [ for p in gated -> p.Name + ".clr.cs" ]
                Goldens.checkNoOrphans goldensDir "*.clr.txt" [ for p in gated -> p.Name + ".clr.txt" ]
            }

            match gated with
            | p :: _ ->
                test "structural digest is MVID-invariant" {
                    let a1 = compileSource (conformanceAssemblyName p.Name) p.Source
                    let a2 = compileSource (conformanceAssemblyName p.Name) p.Source
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
