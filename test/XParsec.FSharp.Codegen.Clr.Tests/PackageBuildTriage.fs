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

            // `option.fs` type-checks, compiles end-to-end, AND is now BCL-only.
            // The front-end gaps were fixed earlier (`Some` carries its `Value`
            // field; external-ctor-without-`new` resolution lets `get` /
            // `member Value`'s `raise (InvalidOperationException …)` type-check);
            // Gap 1 (vesper-lib-test-plan.md) then removed the last wall — `iter` /
            // `defaultWith` / `orElseWith` no longer pull `Microsoft.FSharp.Core.Unit`
            // now that `unit` encodes off its `prim-types-min` `System.ValueTuple`
            // binding. Behavioural coverage rides reflection-invoke (OptionTests.fs).
            test "Vesper.Option builds BCL-only" { buildsBclOnly "Vesper.Option" }

            // `result.fs` compiles to a BCL-only DLL — same story as Option: the
            // `unit`→`System.ValueTuple` repr (Gap 1) dropped the lone
            // `Microsoft.FSharp.Core.Unit` dependency the impl's `unit`-typed
            // functions used to pin.
            test "Vesper.Result builds BCL-only" { buildsBclOnly "Vesper.Result" }

            // ---- At-risk: documented feature gap ------------------------------

            // PENDING — gated on `'T[]` intrinsic codegen, but the first wall is
            // earlier (front-end): an indexed `arr.[i]` lookup trips
            // "CstKeys.firstTokenOfExpr: TODO IndexedLookup" during Unification.
            ptest "Vesper.Array builds BCL-only (front-end: IndexedLookup CstKey)" { buildsBclOnly "Vesper.Array" }

            // PENDING — the handoff's two gaps (vesper-seq-handoff.md) are CLOSED:
            // the explicit-enumerator terminals `fold` / `reduce` / `toArray` now
            // compile BCL-only. The metadata provider surfaces interface members
            // through base interfaces (so `source.GetEnumerator()` / `e.MoveNext()` /
            // `e.Current` / the `use` `IDisposable.Dispose` resolve against
            // `IEnumerator`1`/`IEnumerator`/`IDisposable`), `not` is in the Core
            // `Operators` contract, and the supporting backend gaps closed along the
            // way (value restriction on expansive lets, `while`/local-assignment IL,
            // `'T[]` type translation + SZArray encoding + array-return members). The
            // lone remaining blocker is `truncate`'s `Enumerable.Take<TSource>(…)` — a
            // *generic external static method* (method-owned typars), still deferred
            // in P2. Flip to `test` once that lands.
            ptest "Vesper.Seq builds BCL-only (only `truncate`'s generic Enumerable.Take`<T>` remains)" {
                buildsBclOnly "Vesper.Seq"
            }
        ]
