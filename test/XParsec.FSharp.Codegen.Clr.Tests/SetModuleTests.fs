module XParsec.FSharp.Codegen.Clr.Tests.SetModuleTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

[<Tests>]
let tests =
    let prelude = "open Vesper.Collections\n"

    testList
        "SetModule"
        [
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

                    test "(+) / (-) operators dispatch to the static members" {
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
                    // `set.Map` is a generic instance method (`member s.Map<'U> f : Set<'U>`);
                    // the member-ref carries a generic header and the call site wraps it in a
                    // `MethodSpec` with `'U` recovered by matching declared vs actual types.
                    test "map" {
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

                    // `SetTree.partition1` takes a tuple-destructured static-method parameter;
                    // the lambda peeler handles the synthetic slot + carried pattern, mirroring
                    // `buildClosureInvoke`.
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
                    test "toArray round-trips through ofArray" {
                        runsSet "3" (prelude + s123 + "printfn \"%d\" (Set.count (Set.ofArray (Set.toArray s)))")
                    }

                    // `Set.ofList` calls `List.toSeq` internally; the `IEnumerable` surface
                    // rides a `ListSeq` wrapper class + `[<Struct>] ListEnumerator` because
                    // interface impls on union types are not yet supported.
                    test "toList round-trips through ofList" {
                        runsSet "3" (prelude + s123 + "printfn \"%d\" (Set.count (Set.ofList (Set.toList s)))")
                    }
                ]
        ]
