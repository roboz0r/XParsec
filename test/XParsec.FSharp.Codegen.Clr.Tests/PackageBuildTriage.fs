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
// Triage result (2026-05-30):
//   Vesper.Choice  — BUILDS (BCL-only). The only Candidate that links end-to-end.
//   Vesper.Option  — front-end: `Some x` / generic `option` don't type-check.
//   Vesper.Result  — builds, but NOT BCL-only (emits FSharp.Core `unit`).
//   Vesper.Array   — front-end: indexed `arr.[i]` lookup unhandled in CstKeys.
//   Vesper.Seq     — front-end: a `while`-loop body trips a CstKeys TODO.
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
            // ---- Candidates: no known structural blocker (Core dep only) ------

            // Choice links end-to-end: `Choice<'T1,'T2>` + its constructors compile
            // to a BCL-only DLL. Real assertion — a regression turns this red.
            test "Vesper.Choice builds BCL-only" { buildsBclOnly "Vesper.Choice" }

            // PENDING — `option.fs` does not type-check (33 errors). The union-case
            // `Some x` resolves to a 0-arg constructor ("Constructor 'Some' expects
            // 0 argument(s) but got 1") and the generic `'T option` head unifies
            // against an arrow ("Type mismatch: TyUnion Option vs TyFun" / "Free
            // type parameter 'T is not declared in the enclosing type's
            // type-parameter list"). A front-end gap in single-case-arg union
            // construction / generic-union inference, not a codegen one.
            ptest "Vesper.Option builds BCL-only (front-end: Some/generic option)" { buildsBclOnly "Vesper.Option" }

            // PENDING — `result.fs` compiles, but the emitted DLL is NOT BCL-only:
            // `FSharpCoreDependencies = ["Microsoft.FSharp.Core.Unit"]`. Something in
            // the impl lowers `unit` to FSharp.Core's `unit` instead of Vesper's,
            // pulling an FSharp.Core reference into a package that must stand alone.
            ptest "Vesper.Result builds BCL-only (emits FSharp.Core unit)" { buildsBclOnly "Vesper.Result" }

            // ---- At-risk: documented feature gap ------------------------------

            // PENDING — gated on `'T[]` intrinsic codegen, but the first wall is
            // earlier (front-end): an indexed `arr.[i]` lookup trips
            // "CstKeys.firstTokenOfExpr: TODO IndexedLookup" during Unification.
            ptest "Vesper.Array builds BCL-only (front-end: IndexedLookup CstKey)" { buildsBclOnly "Vesper.Array" }

            // PENDING — gated on enumeration over a project-local `seq<'T>`
            // (get-enumerator-gaps.md Gap 2); pulls in Vesper.List. The first wall
            // is a `while`-loop body hitting "CstKeys.firstTokenOfExpr: TODO Error:
            // Multiple CompilationMappingAttributes, expected at most one".
            ptest "Vesper.Seq builds BCL-only (front-end: CstKeys TODO in while)" { buildsBclOnly "Vesper.Seq" }
        ]
