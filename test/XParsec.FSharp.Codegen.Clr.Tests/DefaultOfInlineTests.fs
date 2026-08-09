module XParsec.FSharp.Codegen.Clr.Tests.DefaultOfInlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `Unchecked.defaultof`, bare or type-applied, must splice to the `ilzero` intrinsic
// during semantic analysis. A surviving call would target the inline-only
// `Vesper.Unchecked` module, which emits no method, and `TypeLoadException` at runtime.

/// True when the body reduces to `ilzero`. A `defaultof` reference leaves a
/// specialization edge, so the resolved body is reached through it rather than in place.
let rec private findIlzero (tast: TastFile) (e: TExpr) : bool =
    match throughEdge tast e with
    | TExpr.ILIntrinsic("ilzero", _, _, _, _) -> true
    | TExpr.Lambda(_, b, _, _) -> findIlzero tast b
    | _ -> false

[<Tests>]
let tests =
    testList
        "DefaultOfInline"
        [
            yield!
                [
                    for label, src in
                        [
                            "qualified", "let f () : int = Unchecked.defaultof"
                            "qualified type-applied", "let f () : int = Unchecked.defaultof<int>"
                        ] ->
                        test (sprintf "%s `defaultof` splices to `ilzero` (no surviving call)" label) {
                            let tast = analyse src
                            Expect.isEmpty tast.Diagnostics "no diagnostics"

                            let spliced =
                                match tast.Decls with
                                | EqList [ TDecl.Let(_, v, _, _) ] -> findIlzero tast v
                                | _ -> false

                            Expect.isTrue spliced (sprintf "%s splices to the ilzero intrinsic" label)
                        }
                ]

            // The spliced `ilzero` yields `default(int)`, so the program prints 0.
            yield
                test "`Unchecked.defaultof<int>` runs (no phantom call into Vesper.Unchecked)" {
                    runs "0" "printfn \"%d\" (Unchecked.defaultof<int>)"
                }
        ]
