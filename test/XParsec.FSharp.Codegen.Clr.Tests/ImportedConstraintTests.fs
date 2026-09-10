module XParsec.FSharp.Codegen.Clr.Tests.ImportedConstraintTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Constraints imported from BCL metadata refuse a written type argument and a generic call
// the way `fsc` refuses them.

let private errorsOf (src: string) : Diagnostic list =
    let provider = ClrSymbolProviders.buildContract defaultPackages
    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

    tast.Diagnostics |> Diagnostic.errors

let private clean (label: string) (src: string) : unit =
    let errs = errorsOf src
    Expect.isEmpty errs (sprintf "%s: expected clean, got: %A" label (errs |> List.map (fun d -> d.Message)))

let private refuses (label: string) (fragment: string) (src: string) : unit =
    let messages = errorsOf src |> List.map (fun d -> d.Message)

    Expect.isTrue
        (messages |> List.exists (fun m -> m.Contains fragment))
        (sprintf "%s: expected a message containing %s, got: %A" label fragment messages)

[<Tests>]
let tests =
    testList
        "ImportedConstraint"
        [
            testList
                "TypeArgument"
                [
                    test "Nullable<int> satisfies the imported 'struct' constraint" {
                        clean "nullable-int" "let f (x: System.Nullable<int>) = x"
                    }

                    test "Nullable<string> is refused by the imported 'struct' constraint" {
                        refuses
                            "nullable-string"
                            "does not support the 'struct' constraint"
                            "let f (x: System.Nullable<string>) = x"
                    }

                    test "a still-free type argument defers rather than refusing" {
                        clean "nullable-generic" "let f<'a when 'a: struct> (x: System.Nullable<'a>) = x"
                    }
                ]

            testList
                "MethodTypar"
                [
                    test "Enum.GetName refuses an argument that is not an enum" {
                        refuses "getname-int" "does not support the 'subtype of " "let n = System.Enum.GetName 1"
                    }
                ]
        ]
