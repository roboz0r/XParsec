module XParsec.FSharp.Codegen.Clr.Tests.DefaultOfInlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Half 1 of unchecked-defaultof-plan: every reference to the `inline` nullary
// intrinsic `defaultof` — bare, `Unchecked.`-qualified, and `<'T>`-type-applied —
// splices to the zero-operand `ilzero` intrinsic during semantic analysis and
// NEVER emits a `call` into the never-materialised `Vesper.Unchecked` holder.
// Before the fix the qualified / type-applied spellings reached codegen as a
// member read/call and crashed with a `TypeLoadException` at runtime; only the
// bare form (the `[<AutoOpen>]` workaround) spliced. This suite drives the real
// contract stack (`ClrSymbolProviders.buildContract`), whose provider serves the
// cross-package inline body, so the splice actually fires and the program runs.

/// The frozen body of the single top-level `let f () = <body>` reduces to the
/// zero-operand `ilzero` intrinsic — no member-read / call head survives.
let rec private findIlzero (e: TExpr) : bool =
    match e with
    | TExpr.ILIntrinsic("ilzero", _, _, _, _) -> true
    | TExpr.Lambda(_, b, _, _) -> findIlzero b
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
                            "bare", "let f () : int = defaultof"
                            "bare type-applied", "let f () : int = defaultof<int>"
                            "qualified", "let f () : int = Unchecked.defaultof"
                            "qualified type-applied", "let f () : int = Unchecked.defaultof<int>"
                        ] ->
                        test (sprintf "%s `defaultof` splices to `ilzero` (no surviving call head)" label) {
                            let tast = analyse src
                            Expect.isEmpty tast.Diagnostics "no diagnostics"

                            let spliced =
                                match tast.Decls with
                                | EqList [ TDecl.Let(_, v, _, _) ] -> findIlzero v
                                | _ -> false

                            Expect.isTrue spliced (sprintf "%s splices to the ilzero intrinsic" label)
                        }
                ]

            // Execution proof: the idiomatic qualified, type-applied spelling must
            // run. A surviving `call Vesper.Unchecked::DefaultOf<int>()` would
            // `TypeLoadException` (the holder is never materialised in Half 1); the
            // spliced `ilzero` yields `default(int)` = 0.
            yield
                test "`Unchecked.defaultof<int>` runs (no phantom call into Vesper.Unchecked)" {
                    runs "0" "printfn \"%d\" (Unchecked.defaultof<int>)"
                }
        ]
