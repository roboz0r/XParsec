module XParsec.FSharp.Codegen.Clr.Tests.SetModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The behavioral runtime suite for `Vesper.Set` — the §9.7 round-trip gate +
// the Phase-9-exit operation table (vesper-set-sprint-phase-9.md). The DLL
// builds + links + loads BCL-only (`PackageBuildTriage`); this suite is the
// *runtime* round-trip the §9.7 gate calls for, driven through `runsSet`.
//
// Status (the consumption path — a program that *uses* `Set` via its `.fsi`
// contract — was never exercised before; standing it up surfaced a chain of
// gaps, two now fixed and one still open):
//   1. ✅ FIXED — the `Set` module resolved to nothing because the source-name
//      alias for a `[<CompiledName>]`'d module val was built *with* the compiled
//      name (`Set.empty` ⇒ wrongly `Set.Empty`). `VesperLib.sourceNameForVal`
//      now keys the alias on the written ident, so `Set.empty`/`Set.add`/… all
//      resolve to `SetModule.Empty`/`.Add` (the module's `ModuleSuffix` compiled
//      class), matching the user's "module Set ⇒ static class SetModule" note.
//   2. ✅ FIXED — a bare zero-arg external module value (`Set.empty`, a
//      `[<GeneralizableValue>]` generic value ⇒ generic static method) reached
//      `EmitExpr.buildExpr` as an unhandled `External` leaf; it now routes
//      through `buildAppCall` as an empty-spine call.
//   3. ✅ FIXED — `Set\`1` now type-loads. The real cause was NOT a single-param
//      `Equals` (that diagnosis was wrong): both the `override Equals`/`GetHashCode`
//      and the `IStructuralEquatable` members emitted as spurious *generic* methods
//      (a leaked method typar) and the `override`s emitted *non-virtual*. Fixes:
//      (a) `IsOverride` now flows Tast→codegen so an `override` of an Object virtual
//      emits `overrideMethodAttrs` (virtual, reuse base slot); (b)
//      `checkObjectOverrideConformance` pins an override's unannotated params to the
//      Object slot (`Equals(obj)`, `GetHashCode()`, `ToString()`); (c)
//      `generaliseMemberTypars` is skipped for override + interface-impl members
//      (their slot fixes the signature) so no phantom method typar survives.
//      Verified: `Set\`1`'s `Equals`/`GetHashCode`/`CompareTo` emit non-generic +
//      virtual with the right `obj`/`IEqualityComparer` params.
//
//   4. ❌ OPEN — the round-trip now runs deep: `Set.empty`/`.add` resolve + call
//      and `Set\`1` initialises, blocked on a *pre-existing producer* inference bug
//      — `member s.Add value : Set<'T>`'s unannotated `value` grounds to `obj`
//      (encoded `Set<obj>::Add(obj)`), not the declaring typar `!0`, so a consumer
//      `Set.add 2 e` can't bind the `Add(!0):Set<!0>` member ref → an
//      `InvalidProgramException` JIT-ing `Set.Add`. NOT reproducible in a minimal
//      generic-class + module-fn shape (those all run correctly); specific to
//      `set.fs`'s exact `SetTree.add`-flow. Chain of consumption-path gaps closed
//      to get here: contract-only-package harness wiring (Printf), `[<CompiledName>]`
//      on module functions (producer), static-member instantiation from the result
//      type (handoff deferred gap 1), generic-class ctor param count (exclude
//      `val`/`static let` backing fields), and class-vs-interface member name
//      collision (class member wins name resolution).
//
// When #4 lands, flip `ptest`→`test` to re-run the round-trip. `runsSet` routes
// a driver through `packageAlc` (where `Vesper.Set` + its eight transitive deps
// resolve); HOF arguments are written *curried* per the Freeze posture.

[<Tests>]
let tests =
    let prelude = "open Vesper.Collections\n"

    testList
        "SetModule"
        [
            // §9.7 round-trip: empty → add → contains → union → intersect → fold.
            // Body is the gate that re-runs once the round-trip clears (see header
            // gap #4). `Set\`1` now type-loads (gap #3 closed) and `Set.empty`/`.add`
            // resolve + call; the residual block is `Set.Add`'s unannotated `value`
            // param grounding to `obj` (not the declaring typar) in the *producer*
            // build — a pre-existing front-end inference bug surfaced by the
            // round-trip, see header.
            ptest "Set round-trip smoke (add/contains/union/intersect/fold)" {
                runsSetLines
                    [ "3"; "true"; "false"; "4"; "1"; "6" ]
                    (prelude
                     + "let s = Set.add 3 (Set.add 1 (Set.add 2 Set.empty))\n"
                     + "printfn \"%d\" (Set.count s)\n"
                     + "printfn \"%b\" (Set.contains 2 s)\n"
                     + "printfn \"%b\" (Set.contains 9 s)\n"
                     + "let u = Set.union s (Set.add 5 Set.empty)\n"
                     + "printfn \"%d\" (Set.count u)\n"
                     + "let i = Set.intersect s (Set.add 2 Set.empty)\n"
                     + "printfn \"%d\" (Set.count i)\n"
                     + "printfn \"%d\" (Set.fold (fun acc -> fun x -> acc + x) 0 s)")
            }
        ]
