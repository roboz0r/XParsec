module XParsec.FSharp.Codegen.Clr.Tests.ConformanceByteIdentityTests

open System
open System.IO
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CLR byte-identity gate: for every conformance program the CLR COMPILES, pin a
// deterministic structural digest of the emitted assembly as a golden and assert
// equality on every later run — a regression tripwire for the frozen-cache work. No
// emitter is touched.
//
// The digest is structural, NOT hash(toBytes …): `Metadata.fs:64` mints a fresh MVID
// per compile, so raw PE bytes differ run-to-run for identical source. See
// `ClrStructuralDigest` for exactly what is folded in and left out.
//
// Gated set = programs with a `Run` or `Fault` obligation for "clr" (a `Fault` program
// still compiles; it faults at RUNTIME). `Diagnose` programs emit nothing, so skipped.

/// Committed goldens, one `.clr.txt` (the hex digest) per program, beside this test.
let private goldensDir = Path.Combine(__SOURCE_DIRECTORY__, "goldens")

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
        "CLR byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let _, artifact = compileSource (conformanceAssemblyName p.Name) p.Source
                    let digest = ClrStructuralDigest.ofBytes (Codegen.toBytes artifact)
                    Goldens.check (Path.Combine(goldensDir, p.Name + ".clr.txt")) (p.Name + ".clr.txt") digest
                }

            // The mechanism, pinned once: recompiling identical source yields RAW PE
            // bytes that DIFFER (fresh MVID) yet the SAME structural digest — the whole
            // reason the gate hashes structure rather than bytes.
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
                        "raw PE bytes differ per compile (fresh MVID) — why the gate digests structure, not bytes"

                    Expect.equal
                        (ClrStructuralDigest.ofBytes b2)
                        (ClrStructuralDigest.ofBytes b1)
                        "structural digest is stable across recompiles of identical source"
                }
            | [] -> ()
        ]
