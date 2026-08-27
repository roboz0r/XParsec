module XParsec.FSharp.Codegen.Clr.Tests.OpsPlatformClrTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CROSS-target / CLR-repr half of the ops-platform contract: a JS template absent on
// CLR, and the CLR BCL primitive repr. Both need the .NET metadata reader, so they live here
// rather than beside the JS-target-only assertions, which stay off the CLR backend.

[<Tests>]
let tests =
    testList
        "OpsPlatformClr"
        [
            // The declared operator surface IS the CLR's arithmetic-support definition.
            // This is the contract a CLR build resolves against.
            OperatorSurfaceParity.tests Target.Clr (ClrSymbolProviders.buildContract [ vesperCorePackage ])

            test "target selection swaps in the JS bodies (Math.imul present for js, absent for clr)" {
                let js = ClrSymbolProviders.buildContractFor Target.Js [ vesperCorePackage ]

                let clr = ClrSymbolProviders.buildContract [ vesperCorePackage ]

                // `int`'s own `( * )`: a per-width body served by member key off the primitive.
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
                let clr = ClrSymbolProviders.buildContract [ vesperCorePackage ]

                match ExternalSymbols.tryReprType clr "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = IntrinsicPlatform.Repr platform
                                                                 }
                                                        }) ->
                    Expect.equal canon (RuntimeNames.intKey) "int canon on CLR is the `.fsi` name"
                    Expect.equal platform "System.Int32" "int platform name on CLR is the BCL repr"

                    Expect.notEqual canon.Name platform "the two names diverge on CLR too (identity ≠ runtime repr)"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }
        ]
