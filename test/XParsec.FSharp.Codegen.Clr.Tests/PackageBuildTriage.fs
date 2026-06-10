module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTriage

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Pre-2 / Phase 0 (vesper-lib-test-plan.md): the triage smoke pass. Run the
// `buildPackage` harness against every Candidate / At-risk package and let the
// green/pending split *be* the readiness map — converting the readiness
// hypothesis into a fact and producing the work-list for the B/R suites
// (Phase 2/3). A green row = the package compiles to a BCL-only DLL today; a
// `ptest` (pending) row = it does not yet, with the blocking diagnostic quoted in
// the test name + comment so the gap is legible without re-running. Each pending
// body still calls `buildsBclOnly`, so flipping `ptest`→`test` re-runs the build
// once the gap is closed.
//
// Triage result (2026-05-30, Gap 1 closed):
//   Vesper.Choice  — BUILDS (BCL-only).
//   Vesper.Option  — BUILDS (BCL-only) after the front-end + `unit`-repr fixes.
//   Vesper.Result  — BUILDS (BCL-only) after the `unit`-repr fix.
//   Vesper.Array   — BUILDS (BCL-only) after the `'T[]` intrinsic codegen landed
//                    (`arr.[i]`/`arr.Length`/`newarr` → `ldelem`/`ldlen`/`newarr`).
//   Vesper.Seq     — BUILDS (BCL-only)
//
// Out of scope here:
//   - Vesper.Core / Vesper.List — proven (PackageBuildTests).
//   - Vesper.Comparison — `impl = []` (the ordering operators are signature-only
//     inline bodies, no DLL); exercised at use sites in Phase 2, not built here.
//   - Vesper.Printf — a C# DLL (PrintfHappyPathTests).
//   - Vesper.Set — Blocked on class/interface/object-expr backend features
//     (vesper-set-gaps.md §B); do not attempt B/R.

/// Assert a package builds to a BCL-only DLL (empty `FSharpCoreDependencies`).
let private buildsBclOnly (package: string) : unit =
    let _, artifact = (buildPackage package).Value

    Expect.isEmpty
        artifact.FSharpCoreDependencies
        (sprintf "%s must compile to a BCL-only DLL (FSharp.Core deps: %A)" package artifact.FSharpCoreDependencies)

[<Tests>]
let tests =
    testList
        "PackageBuildTriage"
        [

            test "Vesper.Choice builds BCL-only" { buildsBclOnly "Vesper.Choice" }

            test "Vesper.Option builds BCL-only" { buildsBclOnly "Vesper.Option" }

            test "Vesper.Result builds BCL-only" { buildsBclOnly "Vesper.Result" }

            test "Vesper.Array builds BCL-only" { buildsBclOnly "Vesper.Array" }

            test "Vesper.Seq builds BCL-only" { buildsBclOnly "Vesper.Seq" }

            ptest "Vesper.Set builds BCL-only" { buildsBclOnly "Vesper.Set" }
        ]
