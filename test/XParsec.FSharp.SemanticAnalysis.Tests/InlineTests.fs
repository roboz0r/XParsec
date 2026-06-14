module XParsec.FSharp.SemanticAnalysis.Tests.InlineTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Covers front-end-gaps-plan §C: the `Inline` marker on `TDecl.Let` and the
// codegen-facing `Inline.inlineExpand` body-substitution helper.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem MockBuiltins.provider input lexed file

let private firstDecl (input: string) : TDecl =
    let tast = analyse input

    if tast.Decls.IsEmpty then
        failwithf "no decls for %s" input
    else
        tast.Decls.[0]

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

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
                    Expect.equal (Inline.quantifiedTypars declTy).Length 1 "id has a single typar"
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
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst("int", _), _),
                               TExpr.Var(_, TyConst("int", _), _),
                               TyFun(TyConst("int", _), TyConst("int", _)),
                               _) -> ()
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
                    Expect.equal (Inline.quantifiedTypars declTy).Length 1 "typar still free after expansion"

                    let again = Inline.inlineExpand decl [| BuiltinTypes.tyBool |]

                    match again with
                    | TExpr.Lambda(TPat.NamedSimple(_, TyConst("bool", _), _), _, _, _) -> ()
                    | other -> failtestf "second expansion at bool failed: %A" other
                | other -> failtestf "unexpected %A" other
            }

            test "inlineExpand on a TDecl.Expression raises" {
                // A top-level expression has no binding to expand.
                let decl =
                    TDecl.Expression(TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit, dummyTok), BuiltinTypes.tyUnit)

                Expect.throws (fun () -> Inline.inlineExpand decl [||] |> ignore) "expects a TDecl.Let"
            }

            test "`let inline succ x = x + 1 in succ 41` keeps the inline template and expands its use site" {
                // At module level the parser lifts `let inline succ … in body`
                // into a top-level inline binding followed by the body as its
                // own expression — so the §C marker lands on a TDecl.Let. The
                // template (decl 0) is retained verbatim, but the use site `succ
                // 41` is now expanded *pre-freeze* by `InlineExpansion`
                // the call beta-reduces to a `Let`
                // binding the argument, with `succ`'s `x + 1` body inlined (the
                // `op_Addition` head is left for codegen's `BuiltinOps`).
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(TPat.NamedSimple _,
                                     TExpr.Lambda _,
                                     true,
                                     TyFun(TyConst("int", _), TyConst("int", _)))
                           TDecl.Expression(TExpr.Let(TPat.NamedSimple _,
                                                      TExpr.Const(TConstValue.Int 41, _, _),
                                                      TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _),
                                                                          TExpr.Var _,
                                                                          _,
                                                                          _),
                                                                TExpr.Const(TConstValue.Int 1, _, _),
                                                                _,
                                                                _),
                                                      _,
                                                      _),
                                            _) ] -> ()
                | _ -> failtestf "unexpected shape: %A" tast.Decls
            }

            test "expanding the §C inline succ at its use site yields its int-typed body" {
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                let succDecl = tast.Decls.[0]

                // succ is monomorphic (int -> int) — expansion is a no-op
                // substitution returning the retained `fun x -> x + 1` body.
                match Inline.inlineExpand succDecl [||] with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst("int", _), _),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _),
                                                   TExpr.Var(_, TyConst("int", _), _),
                                                   _,
                                                   _),
                                         TExpr.Const(TConstValue.Int 1, _, _),
                                         _,
                                         _),
                               TyFun(TyConst("int", _), TyConst("int", _)),
                               _) -> ()
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
                | TExpr.Lambda(TPat.NamedSimple(kb, _, _),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _), TExpr.Var(kv, _, _), _, _),
                                         TExpr.Const(TConstValue.Int 1, _, _),
                                         _,
                                         _),
                               _,
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
                        TPat.NamedSimple(boundKey, tyInt, dummyTok),
                        TExpr.Var(freeKey, tyInt, dummyTok),
                        TExpr.Var(boundKey, tyInt, dummyTok),
                        tyInt,
                        dummyTok
                    )

                match Inline.freshen (sharedMinter ()) body with
                | TExpr.Let(TPat.NamedSimple(kb, _, _), TExpr.Var(kFree, _, _), TExpr.Var(kRef, _, _), _, _) ->
                    Expect.equal kFree freeKey "free Var passes through unchanged"
                    Expect.notEqual kb boundKey "the bound name is freshened"
                    Expect.equal kRef kb "the bound reference follows the fresh binder"
                | other -> failtestf "unexpected freshened shape: %A" other
            }

            // inline-first soundness, beta-reduction half:
            // a lambda argument bound to an inline parameter and FULLY APPLIED in
            // the body is inlined away — its closure never exists. A stored /
            // partially-applied lambda parameter survives as a real closure.

            // The parameter annotations keep these inlines monomorphic so the
            // expanded use grounds fully (a polymorphic inline use leaves the
            // template's typars free, an orthogonal front-end limitation). The
            // lambda-elimination logic under test is independent of polymorphism.

            test "a fully-applied inline lambda parameter is eliminated (no surviving closure)" {
                // `apply` saturates `f` (one arg, arity 1), so the lambda is
                // spliced in and beta-reduced — the expanded use has no `fun`.
                let tast =
                    analyse "let inline apply (f: int -> int) (x: int) = f x in apply (fun y -> y + 1) 41"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "no surviving closure: %s" body)
                Expect.stringContains body "+ 1" "the inlined lambda body survives"
            }

            test "a doubly-applied inline lambda parameter is eliminated at both sites" {
                // `twice f x = f (f x)`: both uses are saturated, so both copies
                // of the lambda are spliced (each freshened) and no closure remains.
                let tast =
                    analyse "let inline twice (f: int -> int) (x: int) = f (f x) in twice (fun y -> y + 1) 10"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "both closures eliminated: %s" body)
            }

            test "a stored inline lambda parameter survives as a closure" {
                // `pick f = f` returns its parameter rather than applying it, so
                // the lambda cannot be inlined away — it stays a real closure (the
                // 3A-3 fallback; the byref-capture reject of such a survivor is the
                // deferred half).
                let tast = analyse "let inline pick (f: int -> int) = f in pick (fun y -> y + 1)"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.stringContains body "fun" (sprintf "closure survives: %s" body)
            }
        ]
