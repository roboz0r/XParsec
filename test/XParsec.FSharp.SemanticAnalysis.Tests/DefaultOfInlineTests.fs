module XParsec.FSharp.SemanticAnalysis.Tests.DefaultOfInlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Every module-qualified spelling of a reference to the `inline` nullary intrinsic
// `defaultof` — `Unchecked.defaultof` and its `<'T>`-type-applied form — must lower to
// a `TExpr.External` splice-eligible head: never a member-read (`StaticPropertyGet` /
// `StaticMethodCall` / `ExternalMember`, which the splice arm skips), and never the
// `Expr.TypeApp` `failwith` fallthrough. A member-read / call head reaches codegen as a
// `call` into the inline-only `Unchecked` holder — which emits no method — and would
// `TypeLoadException` at runtime.
//
// This front-end-only harness resolves the `Vesper.Core` contract from its `.fsi`
// alone, so it serves no cross-package inline body and the `External` head does not
// splice HERE; the `ilzero` splice + the no-`call`-into-`Unchecked` guarantee are
// exercised end-to-end (with bodies + emitted IL) by the CLR codegen twin.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

/// The body node of the single top-level `let f () = <body>`, asserting the
/// program froze without diagnostics.
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
                test (sprintf "%s `defaultof` freezes to a splice-eligible External head" label) {
                    match bodyOf src with
                    | TExpr.External _ -> ()
                    | (TExpr.StaticPropertyGet _ | TExpr.StaticMethodCall _ | TExpr.ExternalMember _) as other ->
                        failtestf "%s lowered to a member-read (%A) — the splice arm would miss it" label other
                    | other -> failtestf "%s lowered to %A, expected TExpr.External" label other
                }
        ]
