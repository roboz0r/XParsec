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
                Expect.equal (declType tast) (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)) "succ : int -> int"
            }

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
                let expanded = Inline.inlineExpand decl [| BuiltinTypes.tyInt |]

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
                Inline.inlineExpand decl [| BuiltinTypes.tyInt |] |> ignore

                // …the decl's own type must still carry a free typar so a
                // second call-site can instantiate it independently.
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal (List.length (Inline.quantifiedTypars declTy)) 1 "typar still free after expansion"

                    let again = Inline.inlineExpand decl [| BuiltinTypes.tyBool |]

                    match again with
                    | TExpr.Lambda(TPat.NamedSimple(_, TyConst "bool"), _, _) -> ()
                    | other -> failtestf "second expansion at bool failed: %A" other
                | other -> failtestf "unexpected %A" other
            }

            test "inlineExpand on a TDecl.Expression raises" {
                // A top-level expression has no binding to expand.
                let decl =
                    TDecl.Expression(TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit), BuiltinTypes.tyUnit)

                Expect.throws (fun () -> Inline.inlineExpand decl [||] |> ignore) "expects a TDecl.Let"
            }

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
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _), TExpr.Var(_, TyConst "int"), _),
                                         TExpr.Const(TConstValue.Int 1, _),
                                         _),
                               TyFun(TyConst "int", TyConst "int")) -> ()
                | other -> failtestf "expected `fun x -> x + 1` body, got %A" other
            }

            // A minter mirroring the one codegen owns: a monotone counter
            // packed into synthetic inline-expansion keys, shared across calls.
            let sharedMinter () =
                let mutable n = 0

                fun () ->
                    let k = NodeKey.ofSynthetic n NodeKind.SynthInlineExpansion
                    n <- n + 1
                    k

            // Pull (binder key, body-Var key) out of the frozen `succ` body
            // shape `fun x -> x + 1`.
            let succBinderAndVar (e: TExpr) =
                match e with
                | TExpr.Lambda(TPat.NamedSimple(kb, _),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _), TExpr.Var(kv, _), _),
                                         TExpr.Const(TConstValue.Int 1, _),
                                         _),
                               _) -> kb, kv
                | other -> failtestf "unexpected succ body: %A" other

            test "freshen renames binders and rewires their references" {
                let body =
                    match firstDecl "let inline succ x = x + 1" with
                    | TDecl.Let(_, v, _, _) -> v
                    | other -> failtestf "unexpected %A" other

                let kb0, kv0 = succBinderAndVar body
                Expect.equal kv0 kb0 "original body Var references the original binder"

                let mint = sharedMinter ()
                let f1 = Inline.freshen mint body
                let f2 = Inline.freshen mint body

                let kb1, kv1 = succBinderAndVar f1
                let kb2, kv2 = succBinderAndVar f2

                // (a) every binder key differs from the original and between results.
                Expect.notEqual kb1 kb0 "first expansion's binder is fresh"
                Expect.notEqual kb2 kb0 "second expansion's binder is fresh"
                Expect.notEqual kb1 kb2 "the two expansions don't share a binder"
                // (b) the internal Var is rewired to the new binder.
                Expect.equal kv1 kb1 "first expansion's Var follows its fresh binder"
                Expect.equal kv2 kb2 "second expansion's Var follows its fresh binder"
            }

            test "freshen leaves a free Var untouched" {
                // `let bound = <free> in bound`: `free`'s key is never bound
                // inside the body, so it must pass through; `bound` is rebound
                // and its reference rewired.
                let tyInt = BuiltinTypes.tyInt
                let freeKey = NodeKey.ofSource 999 NodeKind.ExprIdent
                let boundKey = NodeKey.ofSource 1 NodeKind.PatIdent

                let body =
                    TExpr.Let(
                        TPat.NamedSimple(boundKey, tyInt),
                        TExpr.Var(freeKey, tyInt),
                        TExpr.Var(boundKey, tyInt),
                        tyInt
                    )

                match Inline.freshen (sharedMinter ()) body with
                | TExpr.Let(TPat.NamedSimple(kb, _), TExpr.Var(kFree, _), TExpr.Var(kRef, _), _) ->
                    Expect.equal kFree freeKey "free Var passes through unchanged"
                    Expect.notEqual kb boundKey "the bound name is freshened"
                    Expect.equal kRef kb "the bound reference follows the fresh binder"
                | other -> failtestf "unexpected freshened shape: %A" other
            }
        ]
