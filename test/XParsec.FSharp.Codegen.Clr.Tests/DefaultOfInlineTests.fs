module XParsec.FSharp.Codegen.Clr.Tests.DefaultOfInlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Every reference to the `inline` nullary intrinsic `defaultof` —
// `Unchecked.defaultof` and its `<'T>`-type-applied form — splices to the zero-operand
// `ilzero` intrinsic during semantic analysis and NEVER emits a `call` into the
// inline-only `Vesper.Unchecked` holder (which emits no method). A qualified /
// type-applied spelling that reached codegen as a member read/call would
// `TypeLoadException` at runtime. This suite drives the real contract stack
// (`ClrSymbolProviders.buildContract`), whose provider serves the cross-package inline
// body, so the splice actually fires and the program runs.

/// The frozen body of the single top-level `let f () = <body>` reduces to the
/// zero-operand `ilzero` intrinsic — no member-read / call survives. Read THROUGH the
/// specialization edge the reference now leaves: the resolved body is an entry, and a
/// nullary intrinsic alias is an entry like any other.
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

            // Execution proof: the idiomatic qualified, type-applied spelling must
            // run. A surviving `call Vesper.Unchecked::DefaultOf<int>()` would
            // `TypeLoadException` (the inline-only holder emits no method); the
            // spliced `ilzero` yields `default(int)` = 0.
            yield
                test "`Unchecked.defaultof<int>` runs (no phantom call into Vesper.Unchecked)" {
                    runs "0" "printfn \"%d\" (Unchecked.defaultof<int>)"
                }
        ]
