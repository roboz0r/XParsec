module XParsec.FSharp.Codegen.Clr.Tests.ConformanceTyparsTests

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// T8 Step 4.2 — SEMANTIC typar-order conformance against a REAL extracted `.fsi`.
//
// `ConformanceTypars.checkFile` is unit-tested over the frozen pipeline in
// `SemanticAnalysis.Tests/ConformanceTests.fs` with a STUB contract provider (which
// pins an exact declared typar order). This suite is the other half: it drives the
// SAME check against a contract provider EXTRACTED from a package's real `.fsi`
// (`ClrSymbolProviders.buildContract`, available only here), proving that a genuinely
// authored, paired `.fsi`/`.fs` agrees on generic typar order end-to-end — the
// extracted scheme's positional typars (`FTTypar(Declaring, i)`, via
// `translateCurriedSig`) line up with the inferred scheme's (`FTTypar(Method, i)`, via
// `GeneralizedTypars.canonical`).

[<Tests>]
let tests =
    testList
        "ConformanceTypars.Clr"
        [
            test "Vesper.List: list.fs generic module functions conform to list.fsi typar order" {
                // Analyse `list.fs` through the real frozen self-host pipeline against its
                // DEPENDENCY contract only (Core; the self-manifest is excluded — the
                // package is defining its own types here), exactly as `buildPackage` /
                // `vesperListDll` do.
                let src = File.ReadAllText(vesperListSource "list.fs")
                let analysisProvider = ClrSymbolProviders.buildContract [ vesperCoreManifest ]
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseForSelfHost
                        "Vesper.List"
                        analysisProvider
                        (Hashing.originSourceOfText src lexed)
                        file

                Expect.isEmpty tast.Residue.Diagnostics "list.fs analyses cleanly"

                // The LOOKUP provider DOES include `list.fsi` (the published contract), so
                // `List.fold` / `List.map` / `List.append` / … resolve to their declared
                // schemes. Generic functions are `fold` (`'State`,`'T`), `map` (`'T`,`'U`),
                // `append`/`rev`/`head`/`tail`/`length`/`filter`/`isEmpty`/`ofSeq`/`toSeq`.
                let contract =
                    ClrSymbolProviders.buildContract [ vesperCoreManifest; vesperListManifest ]

                let mismatches = ConformanceTypars.checkFile contract tast

                Expect.isEmpty mismatches (sprintf "list.fs conforms to list.fsi typar order; got %A" mismatches)
            }

            // T8 Step 6 — generic type MEMBER conformance against a REAL extracted `.fsi`.
            // `Formatter.AppendFormatted: 'T -> unit` (+ its overloads and
            // `AppendStructured`) are generic members the `.fsi` extractor now publishes
            // with a method-owned typar (`MethodTyparArity = 1`, `FTTypar(Method, 0)`), no
            // longer dropped. This drives BOTH halves: the published contract surface is
            // present + correctly typed, and `checkMembers` confirms the real
            // `formatter.clr.fs` member signatures agree with it end-to-end.
            test "Vesper.Printf: formatter.clr.fs generic members conform to formatter.fsi" {
                // The whole package is analysed as one concatenated `impl` source (a
                // single declaration-ordered compile), exactly as `buildPackage` does —
                // `formatter.clr.fs` calls its sibling `StructuralPrinter` so it can't be
                // analysed alone. Dependency contract = Core + List (the `depends-on`).
                let src =
                    [ "structural-printer.clr.fs"; "formatter.clr.fs" ]
                    |> List.map (fun f -> File.ReadAllText(vesperPrintfSource f))
                    |> String.concat "\n\n"

                let analysisProvider =
                    ClrSymbolProviders.buildContract [ vesperCoreManifest; vesperListManifest ]

                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseForSelfHost
                        "Vesper.Printf"
                        analysisProvider
                        (Hashing.originSourceOfText src lexed)
                        file

                let analysisErrors = tast.Residue.Diagnostics |> Diagnostic.errors

                Expect.isEmpty analysisErrors (sprintf "Vesper.Printf impl analyses cleanly; got %A" analysisErrors)

                // Lookup provider includes the published `formatter.fsi`, so
                // `Formatter.AppendFormatted` / `AppendStructured` resolve to their
                // generic declared signatures.
                let contract =
                    ClrSymbolProviders.buildContract [ vesperCoreManifest; vesperListManifest; vesperPrintfManifest ]

                // The contract surface is published with the method-owned typar.
                let appendFormatted =
                    contract.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey "Vesper.Formatter" 0, "AppendFormatted")

                Expect.isNonEmpty appendFormatted "formatter.fsi publishes AppendFormatted overloads"

                Expect.isTrue
                    (appendFormatted |> Array.forall (fun m -> m.MethodTyparArity = 1))
                    (sprintf
                        "every AppendFormatted overload carries its own typar (MethodTyparArity = 1); got %A"
                        (appendFormatted |> Array.map (fun m -> m.MethodTyparArity)))

                // And the real `formatter.clr.fs` members agree with that published surface.
                let memberMismatches = ConformanceTypars.checkMembers contract tast

                Expect.isEmpty
                    memberMismatches
                    (sprintf "formatter.clr.fs members conform to formatter.fsi; got %A" memberMismatches)
            }
        ]
