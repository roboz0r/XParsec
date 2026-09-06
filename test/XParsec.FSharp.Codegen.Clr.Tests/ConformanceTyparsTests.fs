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
                    Pipeline.analyseFor
                        {
                            Name = AssemblyName "Vesper.List"
                            Target = Target.Clr
                        }
                        analysisProvider
                        (LexedFile.ofText lexed)
                        file

                Expect.isEmpty tast.Residue.Diagnostics "list.fs analyses cleanly"

                // The contract DOES cover what `list.fsi` declares, so each module function
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
                    AnalysedAssembly.analyse
                        Pipeline.analyseFileFor
                        (ClrSymbolProviders.buildContractForSelf (Some vesperCorePackage) [])
                        (AssemblySources.synthetic "Vesper.Core" Target.Clr Set.empty units)

                let contract = ClrSymbolProviders.buildContract [ vesperCorePackage ]

                let sweep (provider: IExternalSymbolProvider) =
                    [
                        for outcome in analysed.Units do
                            match outcome with
                            | AssemblyAnalysis.UnitOutcome.Analysed u ->
                                yield! ConformanceTypars.checkFile provider u.File.Frozen
                            | AssemblyAnalysis.UnitOutcome.Failed(leading, rest) ->
                                failwithf
                                    "Vesper.Core: %s did not parse"
                                    (leading :: rest |> List.map (fun e -> e.Id.Name) |> String.concat ", ")
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
                    match ScopeContents.tryValueAt contract.Scope addName with
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
                            PublishedSurface.build (fun b ->
                                PublishedSurfaceBuilder.addValue
                                    b
                                    { add with
                                        Scheme = reverseTypars add.Scheme
                                    }
                            )
                            |> PublishedSurface.toProvider
                            contract
                        ]

                Expect.contains
                    (sweep reversed |> List.map (fun m -> m.Name))
                    (SymbolKeyOps.qualifiedName (SymbolKey.Binding add.Key))
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
                    Pipeline.analyseFor
                        {
                            Name = AssemblyName "Vesper.Printf"
                            Target = Target.Clr
                        }
                        analysisProvider
                        (LexedFile.ofText lexed)
                        file

                Expect.isEmpty
                    tast.Residue.Diagnostics
                    (sprintf "Vesper.Printf impl analyses cleanly; got %A" tast.Residue.Diagnostics)

                let contract =
                    ClrSymbolProviders.buildContract [ vesperCorePackage; vesperListPackage; vesperPrintfPackage ]

                let appendFormatted =
                    contract.TryLookupMembers(SymbolKeyOps.qualifiedTypeKeyOf "Vesper.Formatter" 0, "AppendFormatted")

                Expect.isNonEmpty appendFormatted "formatter.fsi publishes AppendFormatted overloads"

                Expect.isTrue
                    (appendFormatted |> EqArray.forall (fun m -> m.Signature.MethodTyparArity = 1))
                    (sprintf
                        "every AppendFormatted overload carries its own typar (MethodTyparArity = 1); got %A"
                        (appendFormatted |> EqArray.map (fun m -> m.Signature.MethodTyparArity)))

                let memberMismatches = ConformanceTypars.checkMembers contract tast

                Expect.isEmpty
                    memberMismatches
                    (sprintf "formatter.clr.fs members conform to formatter.fsi; got %A" memberMismatches)
            }
        ]
