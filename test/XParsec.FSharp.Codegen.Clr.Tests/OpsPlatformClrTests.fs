module XParsec.FSharp.Codegen.Clr.Tests.OpsPlatformClrTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CROSS-target / CLR-repr half of the ops-platform harvest: a JS template absent
// on CLR, and the CLR BCL primitive repr. Both need the BCL metadata leaf
// (`ClrSymbolProviders`, which the JS-native contract has no equivalent of), so they
// live here rather than in `Codegen.Js.Tests.OpsPlatformJsTests` (which keeps the
// JS-target-only assertions and stays off the CLR backend).

/// All `ILIntrinsic` opCode (template) strings reachable in an inline body.
let private ilOpCodes (body: InlineBody) : string list =
    let acc = ResizeArray<string>()

    let rec walkExpr (e: TExpr) =
        match e with
        | TExpr.ILIntrinsic(opCode, _, args, _, _) ->
            acc.Add opCode

            for a in args do
                walkExpr a
        | TExpr.StaticOptimization(clauses, dflt, _, _) ->
            for c in clauses do
                walkExpr c.Body

            walkExpr dflt
        | TExpr.Lambda(_, b, _, _) -> walkExpr b
        | _ -> ()

    match body.Decl with
    | TDecl.Let(_, v, _, _) -> walkExpr v
    | _ -> ()

    List.ofSeq acc

[<Tests>]
let tests =
    testList
        "OpsPlatformClr"
        [
            test "target selection swaps in the JS bodies (Math.imul present for js, absent for clr)" {
                let js =
                    ClrSymbolProviders.contractInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                let clr = ClrSymbolProviders.contractInlineBodiesFor None [ vesperCoreManifest ]

                let jsMul = ilOpCodes js.["op_Multiply"]
                let clrMul = ilOpCodes clr.["op_Multiply"]

                Expect.contains jsMul "Math.imul($0, $1)" "js `*` int32 clause is the Math.imul template"
                Expect.contains clrMul "mul" "clr `*` base is the CIL `mul` mnemonic"

                Expect.isFalse
                    (clrMul |> List.exists (fun s -> s.Contains "Math.imul"))
                    "the CLR collection of the same manifest does NOT pick up the JS template"
            }

            test "CLR target: canon is the `.fsi` name, platform is the BCL repr" {
                let clr = ClrSymbolProviders.buildContractFor None [ vesperCoreManifest ]

                match clr.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = Some platform
                                                                 }
                                                        }) ->
                    Expect.equal canon (RuntimeNames.intKey) "int canon on CLR is the `.fsi` name"
                    Expect.equal platform "System.Int32" "int platform face on CLR is the BCL repr"

                    Expect.notEqual
                        (SymbolKeyOps.intrinsicName canon)
                        platform
                        "the two faces diverge on CLR too (identity ≠ runtime repr)"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }
        ]
