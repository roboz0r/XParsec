module XParsec.FSharp.SemanticAnalysis.Tests.DefaultOfInlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `Unchecked.defaultof`, however spelled, must lower to the served intrinsic — under the
// served contract, an `InlineCall` of its template. A member-read instead reaches codegen as
// a `call` into `Unchecked`, a module every reference splices in place — so it emits no
// method to call.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

/// The body of the single top-level `let f () = _`, asserting the program froze clean.
let private bodyOf (input: string) : TExpr =
    let tast = analyse input
    Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics for %s" input)

    match tast.Decls with
    | EqList [ TDecl.Let(_, TExpr.Lambda(_, body, _, _), _, _) ] -> body
    | other -> failtestf "expected a single `let f () = _` decl, got %A" other

[<Tests>]
let tests =
    testList
        "DefaultOfInline"
        [
            for label, src in
                [
                    "qualified", "let f () : int = Unchecked.defaultof"
                    "qualified type-applied", "let f () : int = Unchecked.defaultof<int>"
                ] ->
                test (sprintf "%s `defaultof` lowers to the served intrinsic, not a member-read" label) {
                    match bodyOf src with
                    | TExpr.External _
                    | TExpr.InlineCall _ -> ()
                    | (TExpr.StaticPropertyGet _ | TExpr.StaticMethodCall _ | TExpr.ExternalMember _) as other ->
                        failtestf "%s lowered to a member-read (%A) — the splice arm would miss it" label other
                    | other -> failtestf "%s lowered to %A, expected an External or its InlineCall" label other
                }
        ]
