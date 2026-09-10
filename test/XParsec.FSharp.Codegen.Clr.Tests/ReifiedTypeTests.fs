module XParsec.FSharp.Codegen.Clr.Tests.ReifiedTypeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `typeof<T>` / `typedefof<T>` in ORDINARY expression position: the front end lowers the
// `Vesper.Core` `reflect` binding to its `ldtoken` / `ldtokendef` inline-IL node, which the
// emitter writes as `ldtoken` + `System.Type::GetTypeFromHandle`.

[<Tests>]
let tests =
    testList
        "Reified types"
        [
            test "typeof<T> emits the runtime type of its operand" {
                let src =
                    String.concat "\n" [ "printfn \"%s\" typeof<int>.Name"; "printfn \"%s\" typeof<string>.Name" ]

                let artifact = compileSource "ReifiedTypeof" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "the program runs"
                Expect.equal (output.Replace("\r", "").Trim()) "Int32\nString" "each typeof yields its own type"
            }

            test "typedefof<T> takes a constructed generic's definition and leaves other types alone" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Box<'T> = { Item: 'T }"
                            ""
                            "printfn \"%s\" typedefof<Box<int>>.Name"
                            "printfn \"%s\" typedefof<int>.Name"
                        ]

                let artifact = compileSource "ReifiedTypedefof" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "the program runs"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "Box`1\nInt32"
                    "a constructed generic reifies its definition; a non-generic type reifies itself"
            }

            // A reification takes exactly one written type; two reports the arity.
            test "typeof applied to two types reports the written arity" {
                let diagnostics = diagnoseSourceErrors "ReifiedArity" "let t = typeof<int, string>"

                Expect.equal
                    [ for d in diagnostics -> d.Diagnostic.Kind ]
                    [ Kind.TypeArgArity("typeof", 1, 2) ]
                    "one diagnostic, the arity"
            }
        ]
