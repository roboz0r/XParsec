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
//   6. ✅ FIXED — the `'T array` contract gap (`Set.ofArray` / `Set.toArray`). The
//      `.fsi` `'T array` parsed as a `Type.SuffixedType` (`array` as a postfix type
//      name) that `resolveTypeName` failed → `FTUnknown "array"` →
//      "Type 'array' could not be resolved during contract extraction".
//      `VesperLibTypeTranslate.translateType` now routes the `array` suffix to the
//      rank-1 array intrinsic (`RuntimeNames.arrayName`), matching the `'T[]`
//      (`ArrayType`) form. The `[| … |]` literal then needed a BCL-only emission:
//      it lowers (`FreezeExpr`) to `ArrayModule.OfList <cons-chain>` — FSharp.Core's,
//      absent here — so `EmitCall.tryEmitArrayLiteral` recognises that exact head
//      (`RuntimeNames.arrayOfListName`) and emits `newarr` + `dup`/`stelem` directly.
//      See the `ofArray deduplicates` / `toArray round-trips` rows.
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

            // ---- Phase-9-exit operation table -------------------------------
            // The golden round-trip table over the `Set` module's `.fsi` surface
            // (vesper-set-phase-9-handoff §"Remaining to ship (G8)"). Each row
            // drives one or more `Set.*` operations through `runsSet` and pins the
            // observed stdout. Sets are built via `Set.add` chains / `Set.ofArray`
            // (the proven construction paths) and rendered to deterministic
            // `int`/`bool` output through `count`/`contains`/`minElement`/etc. —
            // never `%A`/`string`, so a row asserts the *operation*, not the
            // printer. A `ptest` row is one whose operation is not yet supported,
            // with the blocking diagnostic quoted in its comment (the
            // `PackageBuildTriage` convention); flip → `test` when it lands.
            //
            // `s` = {1; 2; 3}, `t` = {2; 3; 4} throughout (built fresh per driver).
            let s123 = "let s = Set.add 1 (Set.add 2 (Set.add 3 Set.empty))\n"
            let t234 = "let t = Set.add 2 (Set.add 3 (Set.add 4 Set.empty))\n"

            testList
                "construction & cardinality"
                [
                    test "singleton" {
                        runsSetLines
                            [ "1"; "true" ]
                            (prelude
                             + "let s = Set.singleton 7\n"
                             + "printfn \"%d\" (Set.count s)\n"
                             + "printfn \"%b\" (Set.contains 7 s)")
                    }

                    test "add is idempotent (duplicate is absorbed)" {
                        runsSet
                            "2"
                            (prelude
                             + "let s = Set.add 1 (Set.add 1 (Set.add 2 Set.empty))\n"
                             + "printfn \"%d\" (Set.count s)")
                    }

                    test "remove" {
                        runsSetLines
                            [ "2"; "false" ]
                            (prelude
                             + s123
                             + "let r = Set.remove 2 s\n"
                             + "printfn \"%d\" (Set.count r)\n"
                             + "printfn \"%b\" (Set.contains 2 r)")
                    }

                    test "isEmpty" {
                        // `e` is built empty from a pinned element (`add 1` then
                        // `remove 1`) rather than a bare `Set.empty` — an unannotated
                        // `Set.isEmpty Set.empty` leaves the element typar unresolved
                        // (the value-restriction shape F# itself rejects), which is a
                        // test-authoring concern, not a Set gap.
                        runsSetLines
                            [ "true"; "false" ]
                            (prelude
                             + s123
                             + "let e = Set.remove 1 (Set.add 1 Set.empty)\n"
                             + "printfn \"%b\" (Set.isEmpty e)\n"
                             + "printfn \"%b\" (Set.isEmpty s)")
                    }

                    // The `'T array` contract gap (gap #6) is CLOSED. `Set.ofArray`'s
                    // `'T array` parameter parsed as a `Type.SuffixedType` (`array` as a
                    // postfix type name) that resolved to no shape → `FTUnknown "array"`;
                    // `VesperLibTypeTranslate.translateType` now routes the `array` suffix to
                    // the rank-1 array intrinsic (`RuntimeNames.arrayName`), the same repr the
                    // `'T[]` (`ArrayType`) form bakes. The `[| … |]` literal then needed a
                    // BCL-only emission: it lowers to `ArrayModule.OfList <cons-chain>`, which
                    // FSharp.Core owns — `EmitCall.tryEmitArrayLiteral` now recognises that
                    // exact head and emits `newarr` + `dup`/`stelem` directly.
                    test "ofArray deduplicates" {
                        runsSet
                            "3"
                            (prelude
                             + "let s = Set.ofArray [| 3; 1; 2; 2; 1 |]\n"
                             + "printfn \"%d\" (Set.count s)")
                    }
                ]

            testList
                "ordering & set algebra"
                [
                    test "minElement / maxElement" {
                        runsSetLines
                            [ "1"; "3" ]
                            (prelude
                             + s123
                             + "printfn \"%d\" (Set.minElement s)\n"
                             + "printfn \"%d\" (Set.maxElement s)")
                    }

                    test "difference" {
                        runsSetLines
                            [ "1"; "1" ]
                            (prelude
                             + s123
                             + t234
                             + "let d = Set.difference s t\n"
                             + "printfn \"%d\" (Set.count d)\n"
                             + "printfn \"%d\" (Set.minElement d)")
                    }

                    test "union (function form)" {
                        runsSet "4" (prelude + s123 + t234 + "printfn \"%d\" (Set.count (Set.union s t))")
                    }

                    test "intersect (function form)" {
                        runsSetLines
                            [ "2"; "2"; "3" ]
                            (prelude
                             + s123
                             + t234
                             + "let i = Set.intersect s t\n"
                             + "printfn \"%d\" (Set.count i)\n"
                             + "printfn \"%d\" (Set.minElement i)\n"
                             + "printfn \"%d\" (Set.maxElement i)")
                    }

                    // PENDING — a *driver-level* `s + t` / `s - t` leaves the SRTP
                    // operator unresolved: "ResolvedTypes: TAST contains 2 unresolved
                    // TyVar(s) — inference bug". NB the *internal* use of the same
                    // operator (inside `Set.union`/`Set.intersect`, gap #5) is CLOSED
                    // and gated by the `union (function form)` / `intersect` rows above
                    // — this row pins the remaining consumer-side resolution gap. Flip
                    // → `test` when driver-level set-operator dispatch resolves.
                    ptest "(+) / (-) operators dispatch to the static members" {
                        runsSetLines
                            [ "4"; "1" ]
                            (prelude
                             + s123
                             + t234
                             + "printfn \"%d\" (Set.count (s + t))\n"
                             + "printfn \"%d\" (Set.count (s - t))")
                    }
                ]

            testList
                "predicates"
                [
                    test "isSubset / isSuperset" {
                        runsSetLines
                            [ "true"; "false"; "true"; "false" ]
                            (prelude
                             + s123
                             + t234
                             + "let sub = Set.add 1 (Set.add 2 Set.empty)\n"
                             + "printfn \"%b\" (Set.isSubset sub s)\n"
                             + "printfn \"%b\" (Set.isSubset t s)\n"
                             + "printfn \"%b\" (Set.isSuperset s sub)\n"
                             + "printfn \"%b\" (Set.isSuperset sub s)")
                    }

                    test "isProperSubset / isProperSuperset" {
                        runsSetLines
                            [ "true"; "false"; "true"; "false" ]
                            (prelude
                             + s123
                             + "let sub = Set.add 1 (Set.add 2 Set.empty)\n"
                             + "printfn \"%b\" (Set.isProperSubset sub s)\n"
                             + "printfn \"%b\" (Set.isProperSubset s s)\n"
                             + "printfn \"%b\" (Set.isProperSuperset s sub)\n"
                             + "printfn \"%b\" (Set.isProperSuperset s s)")
                    }

                    test "exists / forall" {
                        runsSetLines
                            [ "true"; "false"; "true"; "false" ]
                            (prelude
                             + s123
                             + "printfn \"%b\" (Set.exists (fun x -> x = 2) s)\n"
                             + "printfn \"%b\" (Set.exists (fun x -> x = 9) s)\n"
                             + "printfn \"%b\" (Set.forall (fun x -> x > 0) s)\n"
                             + "printfn \"%b\" (Set.forall (fun x -> x > 1) s)")
                    }
                ]

            testList
                "transforms"
                [
                    // PENDING — `Set.map`'s emitted `Set`1::Map` member-ref carries an
                    // open `!0` in the mapping-function parameter: "MissingMethodException:
                    // Method not found: 'Set`1<Int32> Set`1.Map(Vesper.Fun`2<!0,Int32>)'".
                    // The mapping `'T -> 'U`'s source typar leaks unground at the member-ref
                    // site (Outstanding 2 gap A — a generic member-ref minted from a
                    // non-declaring context emits an open typar). Flip → `test` when the
                    // map member-ref instantiates its `'T` from the receiver.
                    ptest "map" {
                        runsSetLines
                            [ "3"; "12" ]
                            (prelude
                             + s123
                             + "let m = Set.map (fun x -> x * 2) s\n"
                             + "printfn \"%d\" (Set.count m)\n"
                             + "printfn \"%d\" (Set.fold (fun acc -> fun x -> acc + x) 0 m)")
                    }

                    test "filter" {
                        runsSetLines
                            [ "1"; "2" ]
                            (prelude
                             + s123
                             + "let f = Set.filter (fun x -> x % 2 = 0) s\n"
                             + "printfn \"%d\" (Set.count f)\n"
                             + "printfn \"%d\" (Set.fold (fun acc -> fun x -> acc + x) 0 f)")
                    }

                    test "fold (sum) / foldBack (sum)" {
                        runsSetLines
                            [ "6"; "6" ]
                            (prelude
                             + s123
                             + "printfn \"%d\" (Set.fold (fun acc -> fun x -> acc + x) 0 s)\n"
                             + "printfn \"%d\" (Set.foldBack (fun x -> fun acc -> x + acc) s 0)")
                    }

                    test "iter walks elements in order" {
                        runsSetLines [ "1"; "2"; "3" ] (prelude + s123 + "Set.iter (fun x -> printfn \"%d\" x) s")
                    }

                    // `SetTree.partition1 comparer f k (acc1, acc2)` has a tuple-destructured
                    // static-method parameter. `EmitLower.peelLambda` now peels the tuple
                    // param (synthetic `Slot` + carried `Pat`); `StaticFn.Params` carries the
                    // pattern so `Emit.buildStaticMethod` spills the `ldarg` `ValueTuple` to a
                    // local and `bindPattern`s its leaves — mirroring `buildClosureInvoke`.
                    test "partition" {
                        // {1,2,3,4} → evens {2,4} (count 2), odds {1,3} (count 2).
                        runsSetLines
                            [ "2"; "2" ]
                            (prelude
                             + "let s = Set.add 1 (Set.add 2 (Set.add 3 (Set.add 4 Set.empty)))\n"
                             + "let (evens, odds) = Set.partition (fun x -> x % 2 = 0) s\n"
                             + "printfn \"%d\" (Set.count evens)\n"
                             + "printfn \"%d\" (Set.count odds)")
                    }
                ]

            testList
                "conversions"
                [
                    // Same `'T array` contract fix as `ofArray deduplicates` (gap #6, CLOSED):
                    // `Set.toArray`'s `'T array` return now resolves to the array intrinsic.
                    // (This row uses no `[| … |]` literal — `Set.toArray s` is a real array —
                    // so it exercises the contract fix alone, not the literal-emission half.)
                    test "toArray round-trips through ofArray" {
                        runsSet "3" (prelude + s123 + "printfn \"%d\" (Set.count (Set.ofArray (Set.toArray s)))")
                    }

                    // PENDING — `Set.ofList` calls `ListModule.toSeq` internally, which
                    // Vesper.List does not emit: "MissingMethodException: Method not found:
                    // 'IEnumerable`1<Int32> Vesper.Collections.ListModule.toSeq(List`1<Int32>)'".
                    // A Vesper.List dependency gap, not a Set one. Flip → `test` when
                    // `ListModule.toSeq` ships.
                    ptest "toList round-trips through ofList" {
                        runsSet "3" (prelude + s123 + "printfn \"%d\" (Set.count (Set.ofList (Set.toList s)))")
                    }
                ]
        ]
