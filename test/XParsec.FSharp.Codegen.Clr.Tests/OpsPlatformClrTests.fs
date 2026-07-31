module XParsec.FSharp.Codegen.Clr.Tests.OpsPlatformClrTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CROSS-target / CLR-repr half of the ops-platform harvest: a JS template absent
// on CLR, and the CLR BCL primitive repr. Both need the BCL metadata leaf
// (`ClrSymbolProviders`, which the JS-native contract has no equivalent of), so they
// live here rather than in `Codegen.Js.Tests.OpsPlatformJsTests` (which keeps the
// JS-target-only assertions and stays off the CLR backend).
//
// The body harvest itself (`InlineBodies.ilOpCodes`) is shared with that suite: both read
// the same contract, only through a different symbol leaf.

[<Tests>]
let tests =
    testList
        "OpsPlatformClr"
        [
            // The declared operator surface IS the CLR's arithmetic-support definition;
            // the manifest states the same matrix. The contract is the one a CLR build
            // resolves against — target `None`.
            OperatorSurfaceParity.tests
                "clr"
                (ClrSymbolProviders.buildContractFor None [ vesperCoreManifest ])
                (ClrSymbolProviders.contractInlineBodiesFor None [ vesperCoreManifest ])

            test "target selection swaps in the JS bodies (Math.imul present for js, absent for clr)" {
                let js = ClrSymbolProviders.buildContractFor (Some Target.Js) [ vesperCoreManifest ]

                let clr = ClrSymbolProviders.buildContractFor None [ vesperCoreManifest ]

                // `int`'s own `( * )` — the per-width body that used to be the operator's
                // int32 clause, now served by member key off the primitive.
                let jsMul =
                    InlineBodies.ilOpCodes (InlineBodies.operatorBody js "int" "op_Multiply")

                let clrMul =
                    InlineBodies.ilOpCodes (InlineBodies.operatorBody clr "int" "op_Multiply")

                Expect.contains jsMul "Math.imul($0, $1)" "js int `*` is the Math.imul template"
                Expect.contains clrMul "mul" "clr int `*` is the CIL `mul` mnemonic"

                Expect.isFalse
                    (clrMul |> List.exists (fun s -> s.Contains "Math.imul"))
                    "the CLR collection of the same manifest does NOT pick up the JS template"
            }

            test "CLR target: canon is the `.fsi` name, platform is the BCL repr" {
                let clr = ClrSymbolProviders.buildContractFor None [ vesperCoreManifest ]

                match clr.TryLookupType "Vesper.int" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = Some platform
                                                                 }
                                                        }) ->
                    Expect.equal (SymbolKey.Type canon) (RuntimeNames.intKey) "int canon on CLR is the `.fsi` name"
                    Expect.equal platform "System.Int32" "int platform face on CLR is the BCL repr"

                    Expect.notEqual canon.Name platform "the two faces diverge on CLR too (identity ≠ runtime repr)"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }
        ]
