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
//   4. ✅ FIXED — the producer-grounding wall. The real cause was NOT `Set.Add`'s
//      `value` alone but the *whole* class typar `'T` grounding to `obj`: the
//      `IStructuralEquatable.Equals`/`GetHashCode` members pass a `'T`-typed set
//      element into a BCL `obj` parameter (`comparer.GetHashCode(x)` /
//      `comparer.Equals(e1.Current, e2.Current)`), and the deferred dot-access drain
//      *unified* `'T := obj` rather than treating it as F#'s implicit upcast — so
//      every `Set\`1` member emitted `obj` for `'T`. Fix (general, not Set-specific):
//      `obj` is now the universal supertype at argument-coercion sites
//      (`Engine.isObjType` + the `obj` rule in `tryCoerceUpcast`; `unifyAppliedSig`
//      coerces each parameter position of a whole-signature unify, used by the
//      deferred drain + the overload-commit), so a typar/value-type argument flows
//      into an `obj` slot WITHOUT grounding; `EmitCall` materialises the implied box
//      (`box <T>` / `box !i`). `Set\`1` now emits `Add(!0):Set<!0>` etc. and the
//      add/count/contains/fold round-trip runs end-to-end.
//
//   5. ❌ OPEN (separate, pre-existing — never reached before #4) — `Set.union` /
//      `Set.intersect` go through the `Set.(+)` / `Set.Intersection` *static
//      operator* members, which produce a CORRUPT `Set`: building succeeds but any
//      read (`Set.count`/`Set.contains`) `AccessViolation`s walking the tree. The
//      identical result built via the instance `Set.add` path reads back fine, so
//      the corruption is specific to the static-operator member's
//      `Set(set1.Comparer, SetTree.union …)` construction / its call from generic
//      `SetModule.union<'T>` (handoff deferred gap #1 family). See the
//      `ptest "… union/intersect (static-operator wall)"` row.
//
// `runsSet` routes a driver through `packageAlc` (where `Vesper.Set` + its eight
// transitive deps resolve); HOF arguments are written *curried* per the Freeze
// posture.

[<Tests>]
let tests =
    let prelude = "open Vesper.Collections\n"

    testList
        "SetModule"
        [
            // §9.7 round-trip — the producer-grounding wall (gap #4) is CLOSED.
            // `Set\`1` now emits its members generic in the declaring typar `'T`
            // (`Add(!0) : Set<!0>`, `get_Comparer() : IComparer<!0>`, …) — the
            // previous whole-class grounding to `obj` (`Add(obj) : Set<obj>`) came
            // from the `IStructuralEquatable` members passing a `'T`-typed element
            // into a BCL `obj` parameter (`comparer.GetHashCode(x)` /
            // `comparer.Equals(e1.Current, e2.Current)`), which the unifier *ground*
            // `'T := obj` instead of treating as the implicit upcast it is. Fix:
            // `Engine.unifyAppliedSig` / the `obj` rule in `tryCoerceUpcast` make
            // `obj` the universal supertype at argument-coercion sites (no grounding),
            // and `EmitCall` boxes the typar/value-type argument into the `obj` slot.
            // This round-trip (add → count → contains → fold) now runs end-to-end.
            test "Set round-trip (add/count/contains/fold)" {
                runsSetLines
                    [ "3"; "true"; "false"; "6" ]
                    (prelude
                     + "let s = Set.add 3 (Set.add 1 (Set.add 2 Set.empty))\n"
                     + "printfn \"%d\" (Set.count s)\n"
                     + "printfn \"%b\" (Set.contains 2 s)\n"
                     + "printfn \"%b\" (Set.contains 9 s)\n"
                     + "printfn \"%d\" (Set.fold (fun acc -> fun x -> acc + x) 0 s)")
            }

            // NEXT WALL (separate, pre-existing — never reached before gap #4 closed):
            // `Set.union` / `Set.intersect` route through the `Set.(+)` /
            // `Set.Intersection` *static operator* members, which produce a CORRUPT
            // `Set` — building `u`/`i` succeeds, but any subsequent read
            // (`Set.count`/`Set.contains`) `AccessViolation`s reading the tree.
            // Isolation: the identical 4-element result built via `Set.add 5 s` (the
            // instance-member path) reads back fine, so the corruption is specific to
            // the static-operator member's `Set(set1.Comparer, SetTree.union …)`
            // construction / its call from the generic `SetModule.union<'T>` (the
            // handoff's deferred gap #1 family — a static call on a generic class from
            // a concrete/non-declaring context). Flip → `test` when that lands.
            ptest "Set round-trip union/intersect (static-operator wall)" {
                runsSetLines
                    [ "4"; "1" ]
                    (prelude
                     + "let s = Set.add 3 (Set.add 1 (Set.add 2 Set.empty))\n"
                     + "let u = Set.union s (Set.add 5 Set.empty)\n"
                     + "printfn \"%d\" (Set.count u)\n"
                     + "let i = Set.intersect s (Set.add 2 Set.empty)\n"
                     + "printfn \"%d\" (Set.count i)")
            }
        ]
