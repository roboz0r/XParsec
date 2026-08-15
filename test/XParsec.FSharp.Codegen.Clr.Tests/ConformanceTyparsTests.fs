module XParsec.FSharp.Codegen.Clr.Tests.ConformanceTyparsTests

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
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

            test "Vesper.Core: every module binding, `inline` included, conforms to its `.fsi`" {
                // The whole package through the multi-file route, so `ops-platform.clr.fs`'s
                // `let inline (+) (x: ^T1) (y: ^T2) : ^T3` is checked against the contract's
                // three typars. Those are the slots a splice fills, one per typar: a body
                // folding them into one `^T` binds `y` at `x`'s type and emits a program that
                // fails at runtime, having type-checked with no diagnostic.
                let units =
                    manifestImplFiles vesperCorePackage
                    |> List.map (
                        AssemblyFiles.SourceFile.read vesperCorePackage
                        >> AssemblyFiles.SourceUnit.ofImplementation
                    )

                let analysed =
                    AssemblyFiles.analyseAssembly
                        {
                            Name = "Vesper.Core"
                            Target = Target.Clr
                        }
                        (ClrSymbolProviders.buildContractForSelf (Some vesperCorePackage) [])
                        units

                let contract = ClrSymbolProviders.buildContract [ vesperCorePackage ]

                let sweep (provider: IExternalSymbolProvider) =
                    [
                        for result in analysed do
                            match result with
                            | Ok file -> yield! ConformanceTypars.checkFile provider file.Frozen
                            | Error e -> failwithf "Vesper.Core: %s did not parse" e.Id.Name
                    ]

                let mismatches = sweep contract

                Expect.isEmpty
                    mismatches
                    (sprintf
                        "Vesper.Core conforms to its own contract's typar order; got %s"
                        (mismatches |> List.map ConformanceTypars.describe |> String.concat "\n"))

                // The sweep above is only evidence if it REACHED `(+)`. Shadow the contract's
                // `^T1 -> ^T2 -> ^T3` with its own reversal and the inline binding must be
                // reported — a conforming corpus and a skipped one look alike otherwise.
                let addName = "Vesper.ArithmeticOperators.op_Addition"

                let add =
                    match contract.TryLookup addName with
                    | ValueSome s -> s
                    | ValueNone -> failtestf "the Vesper.Core contract publishes %s" addName

                Expect.equal add.TyparArity 3 "(+) is published with its three typars"

                let rec reverseTypars (t: FrozenType) : FrozenType =
                    match t with
                    | FTTypar(axis, i) -> FTTypar(axis, add.TyparArity - 1 - i)
                    | t -> FrozenType.mapChildren reverseTypars t

                let reversed =
                    ExternalSymbolProviders.composite
                        [
                            ExternalSymbolProviders.ofNamedChannels
                                { ExternalSymbolProviders.NamedChannels.empty with
                                    TryLookup =
                                        fun name ->
                                            if name = addName then
                                                ValueSome
                                                    { add with
                                                        Scheme = reverseTypars add.Scheme
                                                    }
                                            else
                                                ValueNone
                                }
                            contract
                        ]

                Expect.contains
                    (sweep reversed |> List.map (fun m -> m.Name))
                    addName
                    "an inline binding is compared, not exempt"
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
                    contract.TryLookupMembers(SymbolKeyOps.qualifiedTypeKeyOf "Vesper.Formatter" 0, "AppendFormatted")

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
