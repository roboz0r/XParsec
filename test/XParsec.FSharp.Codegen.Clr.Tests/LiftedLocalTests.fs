module XParsec.FSharp.Codegen.Clr.Tests.LiftedLocalTests

open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

// A generalised body-local `let` is lifted to a generic static method on the Program
// class, as `fsc` emits it: its captures lead its parameters, and its method typars are
// every scope visible at its `let` followed by its own. A non-saturated reference is a
// bridge closure over a saturated call.

let private lines (xs: string list) : string = String.concat "\n" xs

let private bytesOf (name: string) (src: string) : byte[] =
    Codegen.toBytes (compileSource name src)

let private none = GenericParameterAttributes.None

[<Tests>]
let tests =
    testList
        "Lifted locals"
        [
            test "a local used at two types in a monomorphic function is one generic method" {
                let src =
                    lines
                        [
                            "let twoUses (x: int) ="
                            "    let g y = y"
                            "    (g x, g \"a\")"
                            "match twoUses 1 with"
                            "| a, b -> printfn \"%d %s\" a b"
                        ]

                runs "1 a" src

                Expect.equal
                    (methodGenericParamsOf (bytesOf "LiftedTwoUses" src) "Program" "g@0")
                    [ "T0", none ]
                    "g's own typar"
            }

            test "a local capturing the enclosing function's typar takes it as a leading parameter and typar" {
                let src =
                    lines
                        [
                            "let entangled (x: 'b) ="
                            "    let g y = (y, x)"
                            "    (g 1, g \"c\")"
                            "match entangled \"outer\" with"
                            "| (c1, c1x), (c2, c2x) -> printfn \"%d %s %s %s\" c1 c1x c2 c2x"
                        ]

                runs "1 outer c outer" src

                Expect.equal
                    (methodGenericParamsOf (bytesOf "LiftedEntangled" src) "Program" "g@0")
                    [ "T0", none; "T1", none ]
                    "entangled's `'b` leads, g's own follows"
            }

            test "a local of a member of a generic type lifts the type's typar too" {
                runs
                    "1 s"
                    (lines
                        [
                            "type C<'a>(v: 'a) ="
                            "    member _.M() ="
                            "        let idc z = (z, v)"
                            "        (idc 1, idc \"s\")"
                            "match C(\"v\").M() with"
                            "| (a, _), (b, _) -> printfn \"%d %s\" a b"
                        ])
            }

            test "a local nested in a lifted local lifts under both" {
                runs
                    "1 s 2"
                    (lines
                        [
                            "let f (a: int) ="
                            "    let h c ="
                            "        let inner d = (d, c)"
                            "        (inner 1, inner \"s\")"
                            "    match h a with"
                            "    | (x, _), (y, xa) -> printfn \"%d %s %d\" x y xa"
                            "f 2"
                        ])
            }

            test "a local escaping as a value is bridged to a closure over a saturated call" {
                runs
                    "5 a"
                    (lines
                        [
                            "let apply (f: int -> int) (x: int) = f x"
                            "let escape (x: int) ="
                            "    let g y = y"
                            "    (apply g x, g \"a\")"
                            "match escape 5 with"
                            "| a, b -> printfn \"%d %s\" a b"
                        ])
            }

            test "a recursive local referenced from a closure in its body is called through the closure's captures" {
                // `count` is lifted with `n` as its capture. The closure inside its body
                // references `count`, so the closure captures `n`, not `count`, and its body
                // `call`s the lifted method.
                runs
                    "13 12"
                    (lines
                        [
                            "let apply (g: unit -> int) = g ()"
                            "let f (n: int) ="
                            "    let rec count xs ="
                            "        match xs with"
                            "        | [] -> n"
                            "        | _ :: rest -> apply (fun () -> 1 + count rest)"
                            "    (count [ 1; 2; 3 ], count [ \"a\"; \"b\" ])"
                            "match f 10 with"
                            "| a, b -> printfn \"%d %d\" a b"
                        ])
            }

            test "a generalised local bound to a value is a parameterless generic method" {
                runs
                    "3 z"
                    (lines
                        [
                            "let id' (v: 'a) = v"
                            "let viaValue (x: int) ="
                            "    let g = id'"
                            "    (g x, g \"z\")"
                            "match viaValue 3 with"
                            "| a, b -> printfn \"%d %s\" a b"
                        ])
            }

            // The same-file cases of `inline/inline-local-poly`: a spliced body's local is a
            // generalised local of the host under the bound variable the splice minted.
            test "a generalised local inside a same-file inline body is lifted at the splice" {
                runs
                    "1 a\n2 b\n1 outer c outer"
                    (lines
                        [
                            "let inline twoUses (x: int) ="
                            "    let g y = y"
                            "    (g x, g \"a\")"
                            "let inline annotated (x: int) ="
                            "    let g (y: 'a) : 'a = y"
                            "    (g x, g \"b\")"
                            "let inline entangled (x: 'b) ="
                            "    let g y = (y, x)"
                            "    (g 1, g \"c\")"
                            "match twoUses 1 with"
                            "| a1, a2 -> printfn \"%d %s\" a1 a2"
                            "match annotated 2 with"
                            "| b1, b2 -> printfn \"%d %s\" b1 b2"
                            "match entangled \"outer\" with"
                            "| (c1, c1x), (c2, c2x) -> printfn \"%d %s %s %s\" c1 c1x c2 c2x"
                        ])
            }

            test "a lifted local's method sits on the Program class as assembly-visible" {
                let bytes =
                    bytesOf
                        "LiftedRows"
                        (lines [ "let f (x: int) ="; "    let g y = y"; "    (g x, g \"a\")"; "ignore (f 1)" ])

                let program = emittedTypes bytes |> List.find (fun t -> t.Name = "Program")

                Expect.contains program.Methods "g@0" "the lifted local is a Program-class method row"
                assertWellFormed "lifted local" bytes
            }
        ]
