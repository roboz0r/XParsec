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

                let tast = Pipeline.analyseForSelfHost "Vesper.List" analysisProvider src lexed file

                Expect.isEmpty tast.Diagnostics "list.fs analyses cleanly"

                // The LOOKUP provider DOES include `list.fsi` (the published contract), so
                // `List.fold` / `List.map` / `List.append` / … resolve to their declared
                // schemes. Generic functions are `fold` (`'State`,`'T`), `map` (`'T`,`'U`),
                // `append`/`rev`/`head`/`tail`/`length`/`filter`/`isEmpty`/`ofSeq`/`toSeq`.
                let contract =
                    ClrSymbolProviders.buildContract [ vesperCoreManifest; vesperListManifest ]

                let mismatches = ConformanceTypars.checkFile contract tast

                Expect.isEmpty mismatches (sprintf "list.fs conforms to list.fsi typar order; got %A" mismatches)
            }
        ]
