module XParsec.FSharp.Codegen.Clr.Tests.TryFinallyTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `try body finally cleanup` lowers to the same IL-IR exception region `use` does:
// the body's value is parked in a local inside the `try` and reloaded after the
// `finally`, which runs on normal completion and on exception unwind alike.

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

                let artifact = compileSource "TryFinallyOrder" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body\ncleanup"
                    "the body runs, then the cleanup in the finally"
            }

            test "the body's result survives the finally and is the try/finally's value" {
                // Print order is the assertion: `cleanup` before `42`, so the finally
                // ran before the parked value was reloaded and consumed.
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

                let artifact = compileSource "TryFinallyResult" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "cleanup\n42"
                    "the finally runs, then the parked 42 is reloaded as the result"
            }

            test "the finally runs when the body raises, and the exception still propagates" {
                // `1 / z` (z = 0) faults, so nothing returns normally. The assertions read
                // `runEntryPoint`'s failure message, which carries the captured stdout, so
                // a "cleanup" line in it means the finally ran during unwind.
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

                let artifact = compileSource "TryFinallyThrow" src

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
