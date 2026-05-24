module XParsec.FSharp.Codegen.Clr.Tests.CilTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Cil"
        [
            test "hand-written `ldc; ldc; add; ret` returns the sum as exit code" {
                let body =
                    cil {
                        yield Cil.ldcI4 20
                        yield Cil.ldcI4 22
                        yield Cil.add
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilAddCe") body |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "20 + 22 = 42"
            }

            test "the `combine` form composes the same body" {
                let body =
                    Cil.combine
                        (Cil.ldcI4 1)
                        (fun () -> Cil.combine (Cil.ldcI4 2) (fun () -> Cil.combine Cil.add (fun () -> Cil.ret)))

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilAddCombine") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 3 "1 + 2 = 3"
            }

            test "typed `sub` returns the difference as exit code" {
                let body =
                    cil {
                        yield Cil.ldcI4 50
                        yield Cil.ldcI4 8
                        yield Cil.sub
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilSub") body |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "50 - 8 = 42"
            }

            test "typed `mul` returns the product as exit code" {
                let body =
                    cil {
                        yield Cil.ldcI4 6
                        yield Cil.ldcI4 7
                        yield Cil.mul
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilMul") body |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "6 * 7 = 42"
            }

            test "a value round-trips through a declared local slot" {
                // `let!` threads the slot index from `declareLocal` into the
                // `stloc`/`ldloc` that use it, so the body stays statically
                // stack-checked rather than dropping to the untyped `Il` surface.
                let body =
                    cil {
                        let! slot = Cil.declareLocal MockBuiltins.tyInt
                        yield Cil.ldcI4 20
                        yield Cil.ldcI4 22
                        yield Cil.add
                        yield Cil.stloc slot
                        yield Cil.ldloc slot
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilLocals") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "20 + 22 stored to and reloaded from a local = 42"
            }

            test "ifThen runs the body when the condition holds" {
                // acc = 7; if 1 then acc <- 42; acc
                let body =
                    cil {
                        let! acc = Cil.declareLocal MockBuiltins.tyInt
                        yield Cil.ldcI4 7
                        yield Cil.stloc acc

                        yield
                            Cil.ifThen
                                (cil { yield Cil.ldcI4 1 })
                                (cil {
                                    yield Cil.ldcI4 42
                                    yield Cil.stloc acc
                                })

                        yield Cil.ldloc acc
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilIfThenRun") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "cond true → body assigns 42"
            }

            test "ifThen skips the body when the condition fails" {
                // acc = 42; if 0 then acc <- 7; acc
                let body =
                    cil {
                        let! acc = Cil.declareLocal MockBuiltins.tyInt
                        yield Cil.ldcI4 42
                        yield Cil.stloc acc

                        yield
                            Cil.ifThen
                                (cil { yield Cil.ldcI4 0 })
                                (cil {
                                    yield Cil.ldcI4 7
                                    yield Cil.stloc acc
                                })

                        yield Cil.ldloc acc
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilIfThenSkip") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "cond false → body skipped, acc stays 42"
            }

            test "ifThenElse takes the then-arm when the condition holds" {
                let body =
                    cil {
                        yield
                            Cil.ifThenElse
                                (cil { yield Cil.ldcI4 1 })
                                (cil { yield Cil.ldcI4 42 })
                                (cil { yield Cil.ldcI4 7 })

                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilIfThen") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "cond true → then-arm value"
            }

            test "ifThenElse takes the else-arm when the condition fails" {
                let body =
                    cil {
                        yield
                            Cil.ifThenElse
                                (cil { yield Cil.ldcI4 0 })
                                (cil { yield Cil.ldcI4 7 })
                                (cil { yield Cil.ldcI4 42 })

                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilIfElse") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "cond false → else-arm value"
            }

            test "a while-loop accumulates across iterations" {
                // acc = 0; i = 7; while i do (acc <- acc + 6; i <- i - 1); acc
                // The loop body is stack-neutral and `i` (truthy until 0) is the
                // condition, so 6 is added seven times → 42.
                let body =
                    cil {
                        let! acc = Cil.declareLocal MockBuiltins.tyInt
                        let! i = Cil.declareLocal MockBuiltins.tyInt
                        yield Cil.ldcI4 0
                        yield Cil.stloc acc
                        yield Cil.ldcI4 7
                        yield Cil.stloc i

                        yield
                            Cil.whileLoop
                                (cil { yield Cil.ldloc i })
                                (cil {
                                    yield Cil.ldloc acc
                                    yield Cil.ldcI4 6
                                    yield Cil.add
                                    yield Cil.stloc acc
                                    yield Cil.ldloc i
                                    yield Cil.ldcI4 1
                                    yield Cil.sub
                                    yield Cil.stloc i
                                })

                        yield Cil.ldloc acc
                        yield Cil.ret
                    }

                let bytes =
                    Codegen.assembleMainOp (ProjectInfo.defaults "CilWhile") body |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "6 added across 7 iterations = 42"
            }
        ]
