module XParsec.FSharp.SemanticAnalysis.Tests.InlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Covers front-end-gaps-plan §C: the `Inline` marker on `TDecl.Let` and the
// codegen-facing `Inline.inlineExpand` body-substitution helper.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyse MockBuiltins.provider input lexed file

let private firstDecl (input: string) : TDecl =
    match (analyse input).Decls with
    | d :: _ -> d
    | [] -> failwithf "no decls for %s" input

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | [ TDecl.Let(_, _, _, ty) ] -> ty
    | other -> failwithf "expected single TDecl.Let, got %A" other

[<Tests>]
let tests =
    testList
        "Inline"
        [
            // ---- The Inline marker ----

            test "`let inline` sets isInline on the TDecl.Let" {
                match firstDecl "let inline succ x = x + 1" with
                | TDecl.Let(_, _, true, _) -> ()
                | other -> failtestf "expected inline TDecl.Let, got %A" other
            }

            test "a plain `let` leaves isInline clear" {
                match firstDecl "let succ x = x + 1" with
                | TDecl.Let(_, _, false, _) -> ()
                | other -> failtestf "expected non-inline TDecl.Let, got %A" other
            }

            test "prettyDecl renders the inline keyword" {
                let tast = analyse "let inline succ x = x + 1"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let inline v0 = fun v1 -> (v1 + 1)"
                    "TAST shape includes inline"
            }

            test "inline binding still types and freezes its body verbatim" {
                let tast = analyse "let inline succ x = x + 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) (TyFun(MockBuiltins.tyInt, MockBuiltins.tyInt)) "succ : int -> int"
            }

            // ---- quantifiedTypars ----

            test "monomorphic inline binding has no quantified typars" {
                match firstDecl "let inline succ x = x + 1" with
                | TDecl.Let(_, _, _, declTy) -> Expect.isEmpty (Inline.quantifiedTypars declTy) "no typars"
                | other -> failtestf "unexpected %A" other
            }

            test "polymorphic inline binding exposes one quantified typar" {
                match firstDecl "let inline id x = x" with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal (List.length (Inline.quantifiedTypars declTy)) 1 "id has a single typar"
                | other -> failtestf "unexpected %A" other
            }

            // ---- inlineExpand ----

            test "expanding a monomorphic binding returns its body unchanged" {
                let decl = firstDecl "let inline succ x = x + 1"

                let body =
                    match decl with
                    | TDecl.Let(_, v, _, _) -> v
                    | other -> failtestf "unexpected %A" other

                let expanded = Inline.inlineExpand decl [||]
                // No typars to substitute → the retained body round-trips by
                // reference, confirming v1's "retain bodies verbatim" decision.
                Expect.isTrue (System.Object.ReferenceEquals(expanded, body)) "body returned verbatim"
            }

            test "expanding a polymorphic binding substitutes the typar through the body" {
                let decl = firstDecl "let inline id x = x"
                let expanded = Inline.inlineExpand decl [| MockBuiltins.tyInt |]

                // `id`'s body is `fun x -> x`; instantiating 'a := int makes
                // every position concrete int.
                match expanded with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst "int"),
                               TExpr.Var(_, TyConst "int"),
                               TyFun(TyConst "int", TyConst "int")) -> ()
                | other -> failtestf "expected fully-int `fun x -> x`, got %A" other
            }

            test "inlineExpand does not mutate the original decl" {
                let decl = firstDecl "let inline id x = x"
                // Expand once at int…
                Inline.inlineExpand decl [| MockBuiltins.tyInt |] |> ignore

                // …the decl's own type must still carry a free typar so a
                // second call-site can instantiate it independently.
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal (List.length (Inline.quantifiedTypars declTy)) 1 "typar still free after expansion"

                    let again = Inline.inlineExpand decl [| MockBuiltins.tyBool |]

                    match again with
                    | TExpr.Lambda(TPat.NamedSimple(_, TyConst "bool"), _, _) -> ()
                    | other -> failtestf "second expansion at bool failed: %A" other
                | other -> failtestf "unexpected %A" other
            }

            test "inlineExpand on a TDecl.Expression raises" {
                // A top-level expression has no binding to expand.
                let decl =
                    TDecl.Expression(TExpr.Const(TConstValue.Unit, MockBuiltins.tyUnit), MockBuiltins.tyUnit)

                Expect.throws (fun () -> Inline.inlineExpand decl [||] |> ignore) "expects a TDecl.Let"
            }

            // ---- The §C test-gate example end to end ----

            test "`let inline succ x = x + 1 in succ 41` splits into an inline decl + use site" {
                // At module level the parser lifts `let inline succ … in body`
                // into a top-level inline binding followed by the body as its
                // own expression — so the §C marker lands on a TDecl.Let.
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, true, TyFun(TyConst "int", TyConst "int"))
                    TDecl.Expression(TExpr.App(TExpr.Var _, TExpr.Const(TConstValue.Int 41, _), _), _) ] -> ()
                | other -> failtestf "unexpected shape: %A" other
            }

            test "expanding the §C inline succ at its use site yields its int-typed body" {
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                let succDecl = tast.Decls.[0]

                // succ is monomorphic (int -> int) — expansion is a no-op
                // substitution returning the retained `fun x -> x + 1` body.
                match Inline.inlineExpand succDecl [||] with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst "int"),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _), TExpr.Var(_, TyConst "int"), _),
                                         TExpr.Const(TConstValue.Int 1, _),
                                         _),
                               TyFun(TyConst "int", TyConst "int")) -> ()
                | other -> failtestf "expected `fun x -> x + 1` body, got %A" other
            }
        ]
