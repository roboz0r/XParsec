module XParsec.FSharp.Codegen.Clr.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    match argv with
    // Real-IL `%A` structural-printer benchmark (not an Expecto test). See
    // `StructuralFormatBench`. Off the test path so it never runs in CI.
    | [| "--bench-structural" |] -> StructuralFormatBench.run ()
    | _ -> runTestsInAssemblyWithCLIArgs [] argv
