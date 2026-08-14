module XParsec.FSharp.Codegen.Clr.Tests.ConformanceTyparsTests

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Typar-order conformance against a contract EXTRACTED from a real `.fsi`, the half a
// stub provider cannot reach: a declared scheme's positional typars land on the
// `Declaring` axis and an inferred scheme's on `Method`, and the two must line up.

[<Tests>]
let tests =
    testList
        "ConformanceTypars.Clr"
        [
            test "Vesper.List: list.fs generic module functions conform to list.fsi typar order" {
                // Analysis sees the DEPENDENCY contract only (Core): the package is here
                // defining the types its own manifest publishes.
                let src = File.ReadAllText(vesperListSource "list.fs")
                let analysisProvider = ClrSymbolProviders.buildContract [ vesperCorePackage ]
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseFor "Vesper.List" analysisProvider (Hashing.originSourceOfText lexed) file

                Expect.isEmpty tast.Residue.Diagnostics "list.fs analyses cleanly"

                // The conformance contract DOES include `list.fsi`, so each module function
                // resolves to the declared scheme its inferred one is checked against.
                let contract =
                    ClrSymbolProviders.buildContract [ vesperCorePackage; vesperListPackage ]

                let mismatches = ConformanceTypars.checkFile contract tast

                Expect.isEmpty mismatches (sprintf "list.fs conforms to list.fsi typar order; got %A" mismatches)
            }

            // The member half: a generic member (`AppendFormatted: 'T -> unit`) must carry
            // its own method-owned typar through the extracted `.fsi` and back.
            test "Vesper.Printf: formatter.clr.fs generic members conform to formatter.fsi" {
                // The package's files are analysed as one concatenated, declaration-ordered
                // source: `formatter.clr.fs` calls its sibling, so it cannot stand alone.
                let src =
                    [ "structural-printer.clr.fs"; "formatter.clr.fs" ]
                    |> List.map (fun f -> File.ReadAllText(vesperPrintfSource f))
                    |> String.concat "\n\n"

                let analysisProvider =
                    ClrSymbolProviders.buildContract [ vesperCorePackage; vesperListPackage ]

                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseFor "Vesper.Printf" analysisProvider (Hashing.originSourceOfText lexed) file

                let analysisErrors = tast.Residue.Diagnostics |> Diagnostic.errors

                Expect.isEmpty analysisErrors (sprintf "Vesper.Printf impl analyses cleanly; got %A" analysisErrors)

                let contract =
                    ClrSymbolProviders.buildContract [ vesperCorePackage; vesperListPackage; vesperPrintfPackage ]

                let appendFormatted =
                    contract.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey "Vesper.Formatter" 0, "AppendFormatted")

                Expect.isNonEmpty appendFormatted "formatter.fsi publishes AppendFormatted overloads"

                Expect.isTrue
                    (appendFormatted |> EqArray.forall (fun m -> m.MethodTyparArity = 1))
                    (sprintf
                        "every AppendFormatted overload carries its own typar (MethodTyparArity = 1); got %A"
                        (appendFormatted |> EqArray.map (fun m -> m.MethodTyparArity)))

                let memberMismatches = ConformanceTypars.checkMembers contract tast

                Expect.isEmpty
                    memberMismatches
                    (sprintf "formatter.clr.fs members conform to formatter.fsi; got %A" memberMismatches)
            }
        ]
