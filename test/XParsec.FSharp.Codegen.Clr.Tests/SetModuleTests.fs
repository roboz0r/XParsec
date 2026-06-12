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
//   5. ✅ FIXED — the static-operator wall. `Set.union` / `Set.intersect` go through
//      `set1 + set2` / `Set<'T>.Intersection(...)`. The infix `set1 + set2` reached
//      codegen as a saturated `External("op_Addition")` and the closing
//      `expandBuiltinOps` collapsed it to the primitive IL `add` opcode — INTEGER
//      addition of two `Set` object references, yielding a garbage pointer that
//      `AccessViolation`s on the next read. The real `(+)` must dispatch to the type's
//      OWN static operator member (F#'s SRTP rule). Fix (ops-platform.fs + compiler):
//      the arithmetic operators gained FSharp.Core's final
//      `when ^T : ^T = (^T: (static member (+): ^T*^T->^T) (x,y))` static-opt clause —
//      an SRTP member-trait call — now supported end-to-end (`Expr.StaticMemberInvocation`
//      → `TExpr.TraitCall` → resolved to a `StaticMethodCall` at inline expansion when
//      `^T` substitutes to a nominal; primitives keep the inline-IL base). `SetModule.Union`
//      now emits `call Set\`1::op_Addition` and the union/intersect round-trip reads back
//      correctly. See the `test "… union/intersect (static-operator wall)"` row.
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

            // The static-operator wall (gap #5) is CLOSED. `Set.union` is `set1 + set2`,
            // whose `(+)` now dispatches to `Set<'T>.op_Addition` via the
            // `when ^T : ^T` SRTP member-trait clause in `ops-platform.fs` (`set1 + set2`
            // previously collapsed to the primitive IL `add` opcode — integer addition of
            // two object references — corrupting the `Set`). `SetModule.Union` emits a
            // `call Set\`1::op_Addition`; the union/intersect round-trip reads back fine.
            test "Set round-trip union/intersect (static-operator wall)" {
                runsSetLines
                    [ "4"; "1" ]
                    (prelude
                     + "let s = Set.add 3 (Set.add 1 (Set.add 2 Set.empty))\n"
                     + "let u = Set.union s (Set.add 5 Set.empty)\n"
                     + "printfn \"%d\" (Set.count u)\n"
                     + "let i = Set.intersect s (Set.add 2 Set.empty)\n"
                     + "printfn \"%d\" (Set.count i)")
            }

            // `Set<'T>`'s `IStructuralEquatable` members (`set.fs:905`) pass a `'T`-typed
            // element into the *non-generic* `System.Collections.IEqualityComparer`'s
            // `GetHashCode(obj)` / `Equals(obj, obj)`. That is the gap-#4 boxing shape:
            // codegen must resolve the non-generic overload and `box` the typar/value-type
            // argument (an early draft emitted a `GetHashCode('T)` ref → MissingMethod).
            // The round-trip gates never *call* the structural path, so exercise it here
            // by reflectively invoking the interface slots on a loaded `Set\`1<int>` with a
            // real `StructuralEqualityComparer` — proving the boxed `IEqualityComparer`
            // member-refs resolve and run.
            test "Set IStructuralEquatable GetHashCode/Equals run (boxed IEqualityComparer member-refs)" {
                let asm = (buildPackage "Vesper.Set").Value |> fst
                let setModule = asm.GetType("Vesper.Collections.SetModule", true)

                let ofArray = setModule.GetMethod("OfArray").MakeGenericMethod(typeof<int>)

                let build (xs: int[]) = ofArray.Invoke(null, [| box xs |])

                let s1 = build [| 1; 2; 3 |]
                let s2 = build [| 3; 2; 1 |] // same set, reversed insertion order
                let s3 = build [| 1; 2; 9 |] // different element

                let comparer: System.Collections.IEqualityComparer =
                    System.Collections.StructuralComparisons.StructuralEqualityComparer

                let ise = typeof<System.Collections.IStructuralEquatable>

                let getHash =
                    ise.GetMethod("GetHashCode", [| typeof<System.Collections.IEqualityComparer> |])

                let equals =
                    ise.GetMethod("Equals", [| typeof<obj>; typeof<System.Collections.IEqualityComparer> |])

                // Reflective invoke of an interface method virtual-dispatches to `Set`'s
                // explicit impl — the path that emits the boxed `IEqualityComparer` calls.
                // Unwrap reflection's `TargetInvocationException` so a runtime failure in
                // the member body surfaces its real type/message.
                let invoke (m: System.Reflection.MethodInfo) (target: obj) (args: obj[]) : obj =
                    try
                        m.Invoke(target, args)
                    with :? System.Reflection.TargetInvocationException as e ->
                        raise e.InnerException

                let h1 = invoke getHash s1 [| comparer |] :?> int
                let h2 = invoke getHash s2 [| comparer |] :?> int
                let eq12 = invoke equals s1 [| s2; comparer |] :?> bool
                let eq13 = invoke equals s1 [| s3; comparer |] :?> bool

                Expect.equal h1 h2 "equal sets hash equally through the structural comparer (GetHashCode(obj) ran)"
                Expect.isTrue eq12 "structurally equal sets compare equal (Equals(obj, obj) ran)"
                Expect.isFalse eq13 "structurally different sets compare unequal"
            }
        ]
