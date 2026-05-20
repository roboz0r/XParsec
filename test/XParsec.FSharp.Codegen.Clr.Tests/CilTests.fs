module XParsec.FSharp.Codegen.Clr.Tests.CilTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Standalone exercise of the `Cil` body DSL + metadata/PE path, independent of
// any TAST — the roadmap's "Cil tested by emitting a hand-written body and
// asserting behaviour". The body computes an arithmetic result and returns it
// as the process exit code, so a round-trip through load + invoke proves both
// the typed `Op` surface and the assembler.

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
                // Same program, built with explicit `combine` rather than the CE.
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
                // Exercises `DeclareLocal` + the local-variable signature +
                // typed `stloc` / `ldloc`, driven imperatively over `Il` since
                // the typed `Op` CE can't thread a slot index without `Bind`.
                let body (il: Il) =
                    let slot = il.DeclareLocal MockBuiltins.tyInt
                    Cil.ldcI4 20 null null il
                    Cil.ldcI4 22 null null il
                    Cil.add null null il
                    Cil.stloc slot null null il
                    Cil.ldloc slot null null il
                    Cil.ret null null il

                let bytes =
                    Codegen.assembleMainEmit (ProjectInfo.defaults "CilLocals") body
                    |> Codegen.toBytes

                let exitCode, _ = runEntryPoint bytes
                Expect.equal exitCode 42 "20 + 22 stored to and reloaded from a local = 42"
            }
        ]
