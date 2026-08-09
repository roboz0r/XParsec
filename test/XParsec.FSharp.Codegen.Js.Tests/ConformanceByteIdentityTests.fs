module XParsec.FSharp.Codegen.Js.Tests.ConformanceByteIdentityTests

open System.IO
open Expecto
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The JS byte-identity gate: for every conformance program the JS backend actually COMPILES,
// pin the emitted JS source text as a golden and assert equality on every later run.

// Gated set = programs with a `Run` or `Fault` obligation for "js" (a `Fault` program still
// compiles; it faults at RUNTIME). `Diagnose` programs are rejected at compile time and emit
// nothing, so they are skipped.

/// Committed goldens, one `.js` per program, beside this test.
let private goldensDir = Path.Combine(__SOURCE_DIRECTORY__, "goldens")

/// The exact source `runJs` would materialise, minus the Node run: compiled under the assembly
/// name `conformance-<program>`, with the trailing `//# sourceMappingURL` line stripped.
let private emitConformanceJs (name: string) (src: string) : string = emitFrozenJs name src (frozenOfJs src)

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
        "JS byte-identity gate"
        [
            for p in gated do
                test p.Name {
                    let js = emitConformanceJs ("conformance-" + p.Name) p.Source
                    Goldens.check (Path.Combine(goldensDir, p.Name + ".js")) (p.Name + ".js") js
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
