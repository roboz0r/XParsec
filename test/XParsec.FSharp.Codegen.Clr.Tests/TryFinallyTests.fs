module XParsec.FSharp.Codegen.Clr.Tests.TryFinallyTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `try body finally cleanup` lowers to the same IL-IR exception region the `use`
// desugaring uses (`EmitBindings.buildTryFinallyRegion`): the body's value is parked in
// a local inside the `try` and reloaded after the `finally`, and `cleanup` runs on every
// exit — normal completion or exception unwind. Asserting on captured stdout proves both
// the ordering (cleanup after the body, before the value is consumed) and that the
// cleanup still runs when the body raises.

[<Tests>]
let tryFinallyTests =
    testList
        "TryFinally"
        [
            test "the finally runs after the body on normal completion" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let run () ="
                            "    try"
                            "        printfn \"body\""
                            "    finally"
                            "        printfn \"cleanup\""
                            "run ()"
                        ]

                let _, artifact = compileSource "TryFinallyOrder" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body\ncleanup"
                    "the body runs, then the cleanup in the finally"
            }

            test "the body's result survives the finally and is the try/finally's value" {
                // `compute ()` returns the body value (42); the finally still runs before the
                // value is consumed — cleanup prints first, then the caller prints 42. The
                // region parks the body's 42 in a local and reloads it after the finally.
                let src =
                    String.concat
                        "\n"
                        [
                            "let compute () ="
                            "    try"
                            "        42"
                            "    finally"
                            "        printfn \"cleanup\""
                            "printfn \"%d\" (compute ())"
                        ]

                let _, artifact = compileSource "TryFinallyResult" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "cleanup\n42"
                    "the finally runs, then the parked 42 is reloaded as the result"
            }

            test "the finally runs when the body raises, and the exception still propagates" {
                // The finally must run on the exception path too. `1 / z` (z = 0) faults with
                // a DivideByZeroException; the finally prints during unwind, then the exception
                // propagates uncaught. `runEntryPoint` surfaces the captured stdout in its
                // failure message, so the cleanup line proves the finally ran before the throw
                // escaped the region.
                let src =
                    String.concat
                        "\n"
                        [
                            "let run (z: int) ="
                            "    try"
                            "        printfn \"body\""
                            "        1 / z"
                            "    finally"
                            "        printfn \"cleanup\""
                            "printfn \"%d\" (run 0)"
                        ]

                let _, artifact = compileSource "TryFinallyThrow" src

                let thrown =
                    try
                        runEntryPoint (Codegen.toBytes artifact) |> ignore
                        None
                    with ex ->
                        Some ex.Message

                match thrown with
                | Some msg ->
                    Expect.stringContains msg "cleanup" "the finally ran during exception unwind"
                    Expect.stringContains msg "DivideByZero" "the original exception propagated out of the finally"
                | None -> failtest "expected the body's DivideByZero to propagate out of the finally"
            }
        ]
